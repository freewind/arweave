%%====================================================================
%% 文件功能描述：
%% 本模块实现了区块传播工作进程的功能，负责将新区块传播给其他节点。
%% 
%% 关键概念：
%% 1. 区块传播 - 确保新区块能快速传播到整个网络
%% 2. 节点选择 - 智能选择传播目标节点
%% 3. 失败重试 - 处理传播失败的情况
%%
%% 重要说明：
%% - 使用gen_server行为模式
%% - 支持异步传播
%% - 包含重试机制
%%====================================================================

%% 声明这是一个gen_server行为模式的模块
-module(ar_block_propagation_worker).

%% 使用gen_server行为模式
-behaviour(gen_server).

%% 导出公共API函数
-export([start_link/2]).

%% 导出gen_server回调函数
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 引入必要的头文件
-include_lib("arweave/include/ar.hrl").
-include_lib("arweave/include/ar_config.hrl").
-include_lib("arweave/include/ar_consensus.hrl").

%% 定义服务器状态记录
-record(state, {
    %% 要传播的区块
    block,
    %% 目标节点列表
    peers,
    %% 已经成功传播到的节点集合(set)
    successful_peers = sets:new(),
    %% 传播失败的节点到重试次数的映射(map)
    failed_peers = #{},
    %% 最大重试次数
    max_retries
}).

%% @doc 启动区块传播工作进程
%% Block: 要传播的区块
%% Peers: 目标节点列表
start_link(Block, Peers) ->
    %% 使用gen_server启动进程，进程名使用随机引用(reference)
    gen_server:start_link({local, make_ref()}, ?MODULE, [Block, Peers], []).

%% @doc 初始化函数，由gen_server在启动时调用
%% Args: [Block, Peers] - 区块和目标节点列表
init([Block, Peers]) ->
    %% 获取配置中的最大重试次数
    {ok, Config} = application:get_env(arweave, config),
    MaxRetries = Config#config.max_propagation_retries,
    
    %% 启动传播过程
    gen_server:cast(self(), propagate),
    
    %% 返回初始状态
    {ok, #state{
        block = Block,
        peers = Peers,
        max_retries = MaxRetries
    }}.

%% @doc 处理同步调用请求（本模块未使用）
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc 处理异步传播请求
handle_cast(propagate, #state{
        block = Block,
        peers = Peers,
        successful_peers = SuccessfulPeers,
        failed_peers = FailedPeers,
        max_retries = MaxRetries
    } = State) ->
    
    %% 获取所有需要传播的节点（排除已成功和超过重试次数的节点）
    PeersToPropagate = [
        Peer || Peer <- Peers,
        not sets:is_element(Peer, SuccessfulPeers) andalso
        (not maps:is_key(Peer, FailedPeers) orelse
         maps:get(Peer, FailedPeers) < MaxRetries)
    ],
    
    %% 如果没有需要传播的节点，终止进程
    case PeersToPropagate of
        [] ->
            {stop, normal, State};
        _ ->
            %% 向每个节点发送区块
            lists:foreach(
                fun(Peer) ->
                    spawn(fun() -> propagate_to_peer(Block, Peer, self()) end)
                end,
                PeersToPropagate
            ),
            %% 设置传播超时定时器
            erlang:send_after(30000, self(), check_propagation),
            {noreply, State}
    end;

%% @doc 处理其他异步请求（用于扩展）
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc 处理传播结果消息
handle_info({propagation_result, Peer, Result}, State) ->
    #state{
        successful_peers = SuccessfulPeers,
        failed_peers = FailedPeers
    } = State,
    
    %% 根据传播结果更新状态
    case Result of
        success ->
            %% 传播成功，将节点添加到成功集合
            NewState = State#state{
                successful_peers = sets:add_element(Peer, SuccessfulPeers)
            },
            {noreply, NewState};
        failure ->
            %% 传播失败，更新失败次数
            RetryCount = maps:get(Peer, FailedPeers, 0) + 1,
            NewState = State#state{
                failed_peers = maps:put(Peer, RetryCount, FailedPeers)
            },
            %% 触发新一轮传播
            gen_server:cast(self(), propagate),
            {noreply, NewState}
    end;

%% @doc 处理传播超时检查
handle_info(check_propagation, State) ->
    %% 触发新一轮传播尝试
    gen_server:cast(self(), propagate),
    {noreply, State};

%% @doc 处理其他消息（用于扩展）
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc 进程终止时的清理函数
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 向单个节点传播区块
%% Block: 要传播的区块
%% Peer: 目标节点
%% Parent: 父进程PID（用于发送结果）
propagate_to_peer(Block, Peer, Parent) ->
    %% 构建传播请求
    Request = {<<"POST">>, <<"/block">>, Block},
    
    %% 发送请求并处理结果
    Result = case ar_http_iface_client:send_block_request(Peer, Request) of
        ok ->
            success;
        {error, _} ->
            failure
    end,
    
    %% 将结果发送给父进程
    Parent ! {propagation_result, Peer, Result}.
