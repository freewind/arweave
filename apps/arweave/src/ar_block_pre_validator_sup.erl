%%====================================================================
%% 文件功能描述：
%% 本模块实现了区块预验证器的监督者(Supervisor)功能。
%%
%% 主要职责:
%% 1. 启动和监控区块预验证器进程(ar_block_pre_validator)
%% 2. 当预验证器进程崩溃时自动重启
%% 3. 维护一对一(one_for_one)的重启策略
%%
%% 重要说明:
%% - 使用OTP的supervisor行为模式
%% - 只监督一个子进程(ar_block_pre_validator)
%% - 在5秒内最多重启10次,超过则supervisor本身会终止
%%====================================================================

-module(ar_block_pre_validator_sup).

%% 声明这是一个supervisor行为模式的模块
-behaviour(supervisor).

%% 引入必要的头文件,包含一些预定义的宏和类型
-include_lib("arweave/include/ar_sup.hrl").
-include_lib("arweave/include/ar_config.hrl").

%% 导出supervisor启动函数
-export([start_link/0]).

%% 导出supervisor必需的回调函数
-export([init/1]).

%%%===================================================================
%%% Public API (公共接口)
%%%===================================================================

%% @doc 启动supervisor进程
%% 使用start_link/0函数启动supervisor,并注册为本模块名
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks (supervisor回调函数)
%%%===================================================================

%% @doc 初始化supervisor
%% 配置子进程规格和重启策略
init([]) ->
	%% 使用?CHILD宏定义子进程规格
	%% ar_block_pre_validator作为worker类型的子进程
	Children = [?CHILD(ar_block_pre_validator, worker)],
	
	%% 返回supervisor的配置
	%% one_for_one: 一个子进程崩溃时只重启该进程
	%% 5: 重启频率计数的时间窗口(秒)
	%% 10: 在时间窗口内允许的最大重启次数
	{ok, {{one_for_one, 5, 10}, Children}}.