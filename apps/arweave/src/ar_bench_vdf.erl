%%====================================================================
%% 文件功能描述：
%% 本模块实现了VDF(可验证延迟函数)的性能基准测试。
%%
%% VDF是一种需要顺序计算步骤的函数，无法并行加速。
%% 该测试用于评估系统的VDF计算性能，并提供性能建议。
%%
%% 主要功能：
%% 1. 测试单步VDF计算的性能
%% 2. 评估计算时间是否在可接受范围内
%% 3. 提供使用外部VDF服务的建议
%%====================================================================

-module(ar_bench_vdf).

-export([run_benchmark/0]).

-include_lib("arweave/include/ar_vdf.hrl").

%% @doc 运行VDF基准测试
%% 返回: 执行时间(微秒)
run_benchmark() ->
	%% 生成随机输入数据
	Input = crypto:strong_rand_bytes(32),
	
	%% 测量VDF计算时间
	{Time, _} = timer:tc(fun() -> ar_vdf:compute2(1, Input, ?VDF_DIFFICULTY) end),
	
	%% 打印结果(转换为秒)
	io:format("~n~nVDF step computed in ~.2f seconds.~n~n", [Time / 1000000]),
	
	%% 如果计算时间超过1.15秒,建议使用外部VDF服务
	case Time > 1150000 of
		true ->
			io:format("WARNING: your VDF computation speed is low - consider fetching "
					"VDF outputs from an external source (see vdf_server_trusted_peer "
					"and vdf_client_peer command line parameters).~n~n");
		false ->
			ok
	end,
	Time.