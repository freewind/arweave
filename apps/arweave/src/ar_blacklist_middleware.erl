%%====================================================================
%% 文件功能描述：
%% 本模块实现了HTTP请求的黑名单中间件，用于限制和控制API访问。
%%
%% 主要功能：
%% 1. IP地址访问频率限制
%% 2. IP黑名单管理
%% 3. 本地节点白名单
%% 4. 自动清理过期封禁
%%
%% 实现方式：
%% - 使用ETS表存储访问计数和封禁信息
%% - 支持按路径配置不同的访问限制
%% - 定期清理过期的封禁记录
%%====================================================================

-module(ar_blacklist_middleware).

%% Cowboy中间件行为规范
-behaviour(cowboy_middleware).

%% API导出
-export([start/0, execute/2, reset/0, reset_rate_limit/3,
        ban_peer/2, is_peer_banned/1, cleanup_ban/1, decrement_ip_addr/2]).

%% 引入必要的头文件
-include_lib("arweave/include/ar.hrl").
-include_lib("arweave/include/ar_config.hrl").
-include_lib("arweave/include/ar_blacklist_middleware.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Cowboy中间件执行函数
execute(Req, Env) ->
    IPAddr = requesting_ip_addr(Req),
    {ok, Config} = application:get_env(arweave, config),
    case lists:member(blacklist, Config#config.disable) of
        true ->
            %% 黑名单功能已禁用
            {ok, Req, Env};
        _ ->
            %% 检查是否为本地节点
            LocalIPs = [peer_to_ip_addr(Peer) || Peer <- Config#config.local_peers],
            case lists:member(IPAddr, LocalIPs) of
                true ->
                    %% 本地节点直接放行
                    {ok, Req, Env};
                false ->
                    %% 检查访问限制
                    case increment_ip_addr(IPAddr, Req) of
                        block -> {stop, blacklisted(Req)};
                        pass -> {ok, Req, Env}
                    end
            end
    end.

%% @doc 启动黑名单中间件
start() ->
    ?LOG_INFO([{event, ar_blacklist_middleware_start}]),
    %% 启动定期清理任务
    {ok, _} =
        timer:apply_after(
            ?BAN_CLEANUP_INTERVAL,
            ?MODULE,
            cleanup_ban,
            [ets:whereis(?MODULE)]
        ),
    ok.

%% @doc 封禁指定节点
%% Peer - 要封禁的节点
%% TTLSeconds - 封禁时长(秒)
ban_peer(Peer, TTLSeconds) ->
    ?LOG_DEBUG([{event, ban_peer}, {peer, ar_util:format_peer(Peer)}, {seconds, TTLSeconds}]),
    Key = {ban, peer_to_ip_addr(Peer)},
    Expires = os:system_time(seconds) + TTLSeconds,
    ets:insert(?MODULE, {Key, Expires}).

%% @doc 检查节点是否被封禁
is_peer_banned(Peer) ->
    Key = {ban, peer_to_ip_addr(Peer)},
    case ets:lookup(?MODULE, Key) of
        [] -> not_banned;
        [_] -> banned
    end.

%% @doc 清理过期的封禁记录
cleanup_ban(TableID) ->
    case ets:whereis(?MODULE) of
        TableID ->
            Now = os:system_time(seconds),
            %% 查找所有过期的封禁记录
            Folder = fun
                ({{ban, _} = Key, Expires}, Acc) when Expires < Now ->
                    [Key | Acc];
                (_, Acc) ->
                    Acc
            end,
            RemoveKeys = ets:foldl(Folder, [], ?MODULE),
            %% 删除过期记录
            Delete = fun(Key) -> ets:delete(?MODULE, Key) end,
            lists:foreach(Delete, RemoveKeys),
            %% 调度下一次清理
            {ok, _} =
                timer:apply_after(
                    ?BAN_CLEANUP_INTERVAL,
                    ?MODULE,
                    cleanup_ban,
                    [TableID]
                );
        _ ->
            table_owner_died
    end.

%% @doc 返回HTTP 429错误响应
blacklisted(Req) ->
    cowboy_req:reply(
        429,
        #{<<"connection">> => <<"close">>},
        <<"Too Many Requests">>,
        Req
    ).

%% @doc 重置黑名单状态
reset() ->
    true = ets:delete_all_objects(?MODULE),
    ok.

%% @doc 重置指定IP地址的访问计数
reset_rate_limit(TableID, IPAddr, Path) ->
    case ets:whereis(?MODULE) of
        TableID ->
            ets:delete(?MODULE, {rate_limit, IPAddr, Path});
        _ ->
            table_owner_died
    end.

%% @doc 增加IP地址的访问计数
increment_ip_addr(IPAddr, Req) ->
    case ets:whereis(?MODULE) of 
        undefined -> pass;
        _ -> update_ip_addr(IPAddr, Req, 1)
    end.

%% @doc 减少IP地址的访问计数
decrement_ip_addr(IPAddr, Req) ->
    case ets:whereis(?MODULE) of 
        undefined -> pass;
        _ -> update_ip_addr(IPAddr, Req, -1)
    end.

%% @doc 更新IP地址的访问计数
update_ip_addr(IPAddr, Req, Delta) ->
    {PathKey, Limit}  = get_key_limit(IPAddr, Req),
    %% 访问限制为30秒内的一半
    RequestLimit = Limit div 2,
    Key = {rate_limit, IPAddr, PathKey},
    case ets:update_counter(?MODULE, Key, {2, Delta}, {Key, 0}) of
        1 ->
            %% 第一次访问,启动重置定时器
            timer:apply_after(
                ?THROTTLE_PERIOD,
                ?MODULE,
                reset_rate_limit,
                [ets:whereis(?MODULE), IPAddr, PathKey]
            ),
            pass;
        Count when Count =< RequestLimit ->
            pass;
        _ ->
            block
    end.

%% @doc 获取请求的IP地址
requesting_ip_addr(Req) ->
    {IPAddr, _} = cowboy_req:peer(Req),
    IPAddr.

%% @doc 从节点地址中提取IP地址
peer_to_ip_addr({A, B, C, D, _}) -> {A, B, C, D}.

%% @doc 获取请求路径对应的访问限制
get_key_limit(IPAddr, Req) ->
    Path = ar_http_iface_server:split_path(cowboy_req:path(Req)),
    {ok, Config} = application:get_env(arweave, config),
    Map = maps:get(IPAddr, Config#config.requests_per_minute_limit_by_ip, #{}),
    ?RPM_BY_PATH(Path, Map)().
