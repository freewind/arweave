%%====================================================================
%% 文件功能描述：
%% 本模块实现了哈希性能基准测试功能，用于测试系统的RandomX哈希计算性能。
%% 主要测试两种类型的哈希:
%% 1. H0 - RandomX哈希计算
%% 2. H1/H2 - SHA256哈希计算
%%
%% 可配置参数包括:
%% - RandomX模式(512/4096)
%% - JIT编译开关
%% - 大页内存开关
%% - 硬件AES指令开关
%%====================================================================

-module(ar_bench_hash).

-export([run_benchmark_from_cli/1, run_benchmark/1]).

%% 引入必要的头文件
-include_lib("arweave/include/ar_consensus.hrl").
-include_lib("arweave/include/ar_config.hrl").

%% @doc 从命令行运行基准测试
%% Args - 命令行参数列表
run_benchmark_from_cli(Args) ->
	%% 获取各项配置参数,使用默认值
	RandomX = get_flag_value(Args, "randomx", "512"),  % RandomX模式
	JIT = list_to_integer(get_flag_value(Args, "jit", "1")), % JIT编译开关
	LargePages = list_to_integer(get_flag_value(Args, "large_pages", "1")), % 大页内存开关
	HardwareAES = list_to_integer(get_flag_value(Args, "hw_aes", "1")), % 硬件AES开关

	%% 确定RandomX模式
	RandomXMode = case RandomX of
		"512" -> rx512;
		"4096" -> rx4096;
		_ -> show_help()
	end,

	%% 获取CPU调度器数量
	Schedulers = erlang:system_info(dirty_cpu_schedulers_online),
	
	%% 初始化RandomX状态
	RandomXState = ar_mine_randomx:init_fast2(
		RandomXMode, ?RANDOMX_PACKING_KEY, JIT, LargePages, Schedulers),
		
	%% 运行基准测试并获取结果
	{H0, H1} = run_benchmark(RandomXState, JIT, LargePages, HardwareAES),
	
	%% 格式化输出结果(转换为毫秒)
	H0String = io_lib:format("~.3f", [H0 / 1000]),
	H1String = io_lib:format("~.3f", [H1 / 1000]),
	ar:console("Hashing benchmark~nH0: ~s ms~nH1/H2: ~s ms~n", [H0String, H1String]).

%% @doc 从参数列表中获取指定标志的值
%% 如果找不到则返回默认值
get_flag_value([], _, DefaultValue) ->
	DefaultValue;
get_flag_value([Flag | [Value | _Tail]], TargetFlag, _DefaultValue) when Flag == TargetFlag ->
	Value;
get_flag_value([_ | Tail], TargetFlag, DefaultValue) ->
	get_flag_value(Tail, TargetFlag, DefaultValue).

%% @doc 显示帮助信息并退出程序
show_help() ->
	io:format("~nUsage: benchmark-hash [options]~n"),
	io:format("Options:~n"),
	io:format("  randomx <512|4096> (default: 512)~n"),
	io:format("  jit <0|1> (default: 1)~n"), 
	io:format("  large_pages <0|1> (default: 1)~n"),
	io:format("  hw_aes <0|1> (default: 1)~n"),
	erlang:halt().

%% @doc 使用默认参数运行基准测试
run_benchmark(RandomXState) ->
	run_benchmark(RandomXState, ar_mine_randomx:jit(),
		ar_mine_randomx:large_pages(), ar_mine_randomx:hardware_aes()).

%% @doc 运行基准测试的主函数
run_benchmark(RandomXState, JIT, LargePages, HardwareAES) ->
	%% 生成随机测试数据
	NonceLimiterOutput = crypto:strong_rand_bytes(32),
	Seed = crypto:strong_rand_bytes(32),
	MiningAddr = crypto:strong_rand_bytes(32),
	Iterations = 1000,

	%% 测试H0哈希性能(RandomX哈希)
	{H0Time, _} = timer:tc(fun() ->
		lists:foreach(
			fun(_) ->
				PartitionNumber = rand:uniform(1000),
				Data = << NonceLimiterOutput:32/binary,
					PartitionNumber:256, Seed:32/binary, MiningAddr/binary >>,
				ar_mine_randomx:hash(RandomXState, Data, JIT, LargePages, HardwareAES)
			end,
			lists:seq(1, Iterations))
		end),
	H0Microseconds = H0Time / Iterations,

	%% 测试H1哈希性能(SHA256哈希)
	H0 = crypto:strong_rand_bytes(32),
	Chunk = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE),
	{H1Time, _} = timer:tc(fun() ->
		lists:foreach(
			fun(_) ->
				Nonce = rand:uniform(1000),
				Preimage = crypto:hash(sha256, << H0:32/binary, Nonce:64, Chunk/binary >>),
				crypto:hash(sha256, << H0:32/binary, Preimage/binary >>)
			end,
			lists:seq(1, Iterations))
		end),
	H1Microseconds = H1Time / Iterations,

	%% 返回每次迭代的平均时间(微秒)
	{H0Microseconds, H1Microseconds}.