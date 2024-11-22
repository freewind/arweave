%%====================================================================
%% 文件功能描述：
%% 本模块实现了数据打包性能基准测试功能，用于评估系统的数据打包和重打包性能。
%%
%% 主要功能：
%% 1. 测试不同打包模式的性能:
%%    - 传统打包(pack_legacy)
%%    - 组合打包(pack_composite) 
%%    - Erlang重打包(erl_repack_legacy)
%%    - NIF重打包(nif_repack_legacy/composite)
%%
%% 2. 支持的配置参数:
%%    - RandomX模式
%%    - JIT编译开关
%%    - 大页内存开关
%%    - 硬件AES指令开关
%%    - 打包难度
%%    - 打包轮数
%%
%% 3. 性能指标统计:
%%    - 总执行时间
%%    - 初始化时间
%%    - 每个块的处理时间
%%    - 每核心的处理能力
%%====================================================================

-module(ar_bench_packing).

%% API导出
-export([run_benchmark_from_cli/1]).

%% 引入必要的头文件
-include_lib("arweave/include/ar.hrl").
-include_lib("arweave/include/ar_consensus.hrl").
-include_lib("kernel/include/file.hrl").

%% 测试配置记录(record)定义
%% test: 测试类型
%% num_workers: 工作进程数量
%% total_megabytes: 总测试数据大小(MB)
%% jit: JIT编译开关
%% large_pages: 大页内存开关
%% hardware_aes: 硬件AES指令开关
%% packing_difficulty: 打包难度
%% rounds: 打包轮数
%% root: 根哈希
%% src_address: 源地址
%% dst_address: 目标地址
%% randomx_state: RandomX状态
%% input_file: 输入文件句柄
%% output_file: 输出文件句柄
-record(test_config, {
	test,
	num_workers,
	total_megabytes,
	jit,
	large_pages,
	hardware_aes,
	packing_difficulty,
	rounds,
	root,
	src_address,
	dst_address,
	randomx_state,
	input_file,
	output_file
}).

%% 定义有效的测试类型和对应的处理函数
%% 第一个元素表示是否为重打包测试
%% 第二个元素是实际的处理函数
-define(VALID_TESTS, #{
	pack_legacy => {false, fun pack_legacy_chunks/4},
	pack_composite => {false, fun pack_composite_chunks/4},
	erl_repack_legacy => {true, fun erl_repack_legacy_chunks/4},
	nif_repack_legacy => {true, fun nif_repack_legacy_chunks/4},
	nif_repack_composite => {true, fun nif_repack_composite_chunks/4}
}).

%% @doc 从命令行运行基准测试
%% Args - 命令行参数列表
run_benchmark_from_cli(Args) ->
	%% 获取测试类型参数,默认为pack_legacy
	Test = list_to_atom(get_flag_value(Args, "test", "pack_legacy")),
	%% 获取JIT编译开关,默认启用
	JIT = list_to_integer(get_flag_value(Args, "jit", "1")),
	%% 获取大页内存开关,默认启用
	LargePages = list_to_integer(get_flag_value(Args, "large_pages", "1")),
	%% 获取硬件AES开关,默认启用
	HardwareAES = list_to_integer(get_flag_value(Args, "hw_aes", "1")),
	%% 获取打包难度,默认为1
	PackingDifficulty = list_to_integer(get_flag_value(Args, "pdiff", "1")),
	%% 获取打包轮数,默认为COMPOSITE_PACKING_ROUND_COUNT
	Rounds = list_to_integer(get_flag_value(Args, "rounds",
		integer_to_list(?COMPOSITE_PACKING_ROUND_COUNT))),
	%% 运行基准测试
	run_benchmark(Test, JIT, LargePages, HardwareAES, PackingDifficulty, Rounds).

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
	io:format("~nUsage: benchmark-packing [options]~n"),
	io:format("Options:~n"),
	io:format("  test <test> (default: pack_legacy)~n"),
	io:format("  mb <megabytes> (default: 16)~n"),
	io:format("  jit <0|1> (default: 1)~n"),
	io:format("  large_pages <0|1> (default: 1)~n"),
	io:format("  hw_aes <0|1> (default: 1)~n"),
	io:format("  pdiff <number> (default: 1)~n"),
	io:format("  rounds <number> (default: 10)~n"),
	lists:foreach(fun(Test) -> io:format("  ~p~n", [Test]) end, maps:keys(?VALID_TESTS)),
	erlang:halt().

%% @doc 运行基准测试
%% 参数:
%% - Test: 测试类型
%% - JIT: JIT编译开关
%% - LargePages: 大页内存开关
%% - HardwareAES: 硬件AES开关
%% - PackingDifficulty: 打包难度
%% - Rounds: 打包轮数
run_benchmark(Test, JIT, LargePages, HardwareAES, PackingDifficulty, Rounds) ->
	%% 等待3秒,让系统稳定
	timer:sleep(3000),
	%% 创建offsets ETS表
	ets:new(offsets, [set, named_table, public]),
	%% 初始化测试数据
	EncodedRoot = <<"OIgTTxuEPklMR47Ho8VWnNr1Uh6TNjzxwIs38yuqBK0">>,
	Root = ar_util:decode(EncodedRoot),
	EncodedSrcAddress = <<"mvK6e65dcD6XNYDHUVxMa7-d6wVP535Ummtvb8OCUtQ">>,
	SrcAddress = ar_util:decode(EncodedSrcAddress),
	EncodedDstAddress = <<"ymvkTAt6DVo0LaV3SH4TPLvzCmn5TIqvCcv1pHWt2Zs">>,
	DstAddress = ar_util:decode(EncodedDstAddress),

	%% 获取CPU核心数
	NumWorkers = erlang:system_info(dirty_cpu_schedulers_online),

	%% 计算总测试数据大小(MB),确保能被核心数整除
	TotalMegaBytes = (1024 div NumWorkers) * NumWorkers,

	%% 创建测试配置
	Config = #test_config{
		test = Test,
		num_workers = NumWorkers,
		total_megabytes = TotalMegaBytes,
		jit = JIT,
		large_pages = LargePages,
		hardware_aes = HardwareAES,
		packing_difficulty = PackingDifficulty,
		rounds = Rounds,
		root = Root,
		src_address = SrcAddress,
		dst_address = DstAddress
	},

	%% 打印测试配置信息
	io:format("~nBenchmark settings:~n"),
	io:format("~12s: ~p~n", ["Test", Test]),
	io:format("~12s: ~p~n", ["Data (MB)", TotalMegaBytes]),
	io:format("~12s: ~p~n", ["Cores Used", NumWorkers]),
	io:format("~12s: ~p~n", ["JIT", JIT]),
	io:format("~12s: ~p~n", ["Large Pages", LargePages]),
	io:format("~12s: ~p~n", ["HW AES", HardwareAES]),
	io:format("~nBenchmark settings (composite only):~n"),
	io:format("~12s: ~p~n", ["pdiff", PackingDifficulty]),
	io:format("~12s: ~p~n", ["rounds", Rounds]),
	io:format("~n"),

	%% 生成测试输入数据
	generate_input(Config),

	%% 检查测试类型是否有效
	case lists:member(Test, maps:keys(?VALID_TESTS)) of
		true ->
			run_benchmark(Config);
		false ->
			show_help()
	end,

	%% 获取初始化时间和总时间(转换为秒)
	Init = ar_bench_timer:get_total({init}) / 1000000,
	Total = ar_bench_timer:get_total({wall}) / 1000000,

	%% 打开结果文件
	File = open_file("benchmark.results.csv", [append]),

	%% 写入CSV格式的测试结果
	Output = io_lib:format("~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p~n", [
		erlang:system_time() div 1000000000,
		Test, TotalMegaBytes, JIT, LargePages, HardwareAES,
		PackingDifficulty, Rounds,
		Init, Total]),
	
	file:write(File, Output),
	file:close(File),

	%% 计算性能指标
	Label = "Chunks Processed",
	Chunks = (TotalMegaBytes * ?MiB) div ?DATA_CHUNK_SIZE,
	TimePerChunk = (Total / Chunks) * 1000,
	TimePerChunkPerCore = TimePerChunk * NumWorkers,
	ChunksPerSecond = Chunks / Total,
	ChunksPerSecondPerCore = ChunksPerSecond / NumWorkers,

	%% 打印测试结果
	io:format("~nBenchmark results:~n"),
	io:format("~28s: ~p~n", [Label, Chunks]),
	io:format("~28s: ~.2f~n", ["Total Time (s)", Total]),
	io:format("~28s: ~.2f~n", ["Init Time (s)", Init]),
	io:format("~28s: ~.2f~n", ["Time Per Chunk (ms)", TimePerChunk]),
	io:format("~28s: ~.2f~n", ["Time Per Chunk Per Core (ms)", TimePerChunkPerCore]),
	io:format("~28s: ~p~n", ["Chunks Per Second", floor(ChunksPerSecond)]),
	io:format("~28s: ~p~n", ["Chunks Per Second Per Core", floor(ChunksPerSecondPerCore)]).

%% @doc 生成测试输入数据
%% Config - 测试配置
generate_input(Config) ->
	#test_config{ total_megabytes = TotalMegaBytes } = Config,
	TotalBytes = TotalMegaBytes * ?MiB,

	%% 生成未打包数据文件
	UnpackedFilename = unpacked_filename(TotalMegaBytes),
	case file:read_file_info(UnpackedFilename) of
		{ok, FileInfo1} ->
			if
				FileInfo1#file_info.size == TotalBytes ->
					ok;
				true ->
					file:delete(UnpackedFilename),
					write_random_data(UnpackedFilename, TotalBytes)
			end;
		{error, _} ->
			write_random_data(UnpackedFilename, TotalBytes)
	end,

	%% 生成打包数据文件
	PackedFilename = packed_filename(TotalMegaBytes),
	case file:read_file_info(PackedFilename) of
		{ok, FileInfo2} ->
			if
				FileInfo2#file_info.size == TotalBytes ->
					ok;
				true ->
					file:delete(PackedFilename),
					write_packed_data(Config, UnpackedFilename, PackedFilename)
			end;
		{error, _} ->
			write_packed_data(Config, UnpackedFilename, PackedFilename)
	end.

%% @doc 写入随机测试数据
write_random_data(UnpackedFilename, TotalBytes) ->
	io:format("Generating input file: ~s~n", [UnpackedFilename]),
	File = open_file(UnpackedFilename, [write, binary, raw]),
	write_chunks(File, TotalBytes),
	file:close(File).

%% @doc 分块写入数据
write_chunks(File, TotalBytes) ->
	ChunkSize = 1024*1024, % 1MB
	RemainingBytes = TotalBytes,
	write_chunks_loop(File, RemainingBytes, ChunkSize).

%% @doc 循环写入数据块
write_chunks_loop(_File, 0, _) ->
	ok;
write_chunks_loop(File, RemainingBytes, ChunkSize) ->
	BytesToWrite = min(RemainingBytes, ChunkSize),
	Data = crypto:strong_rand_bytes(BytesToWrite),
	file:write(File, Data),
	write_chunks_loop(File, RemainingBytes - BytesToWrite, ChunkSize).

%% @doc 写入打包数据
write_packed_data(Config, UnpackedFilename, PackedFilename) ->
	io:format("Generating input file: ~s~n", [PackedFilename]),
	{ok, RandomXState} = init_randomx_state(Config),

	UnpackedFileHandle = open_file(UnpackedFilename, [read, binary]),
	PackedFileHandle = open_file(PackedFilename, [write, binary]),

	test(Config#test_config{
		test = pack_legacy,
		randomx_state = RandomXState,
		input_file = UnpackedFileHandle,
		output_file = PackedFileHandle
	}),
	
	file:close(PackedFileHandle),
	file:close(UnpackedFileHandle).

%% @doc 运行基准测试
run_benchmark(Config) ->
	#test_config{
		test = Test,
		total_megabytes = TotalMegaBytes
	} = Config,
	
	%% 根据测试类型打开相应的输入文件
	Config2 = case is_repack_test(Test) of
		true ->
			Config#test_config{
				input_file = open_file(packed_filename(TotalMegaBytes), [read, binary]),
				output_file = open_file(output_filename(Config), [write, binary])
			};
		false ->
			Config#test_config{
				input_file = open_file(unpacked_filename(TotalMegaBytes), [read, binary]),
				output_file = open_file(output_filename(Config), [write, binary])
			}
	end,

	%% 初始化RandomX状态
	{ok, RandomXState} = init_randomx_state(Config),

	%% 运行测试
	run_test(Config2#test_config{randomx_state = RandomXState}).

%% @doc 初始化RandomX状态
init_randomx_state(Config) ->
	#test_config{
		test = Test,
		num_workers = NumWorkers,
		jit = JIT,
		large_pages = LargePages
	} = Config,
	case lists:member(Test, [pack_composite, nif_repack_composite]) of
		true ->
			ar_bench_timer:record({init},
				fun ar_rx4096_nif:rx4096_init_nif/5,
					[?RANDOMX_PACKING_KEY, ?RANDOMX_HASHING_MODE_FAST, 
						JIT, LargePages, NumWorkers]);
		false ->
			ar_bench_timer:record({init},
				fun ar_rx512_nif:rx512_init_nif/5,
					[?RANDOMX_PACKING_KEY, ?RANDOMX_HASHING_MODE_FAST,
						JIT, LargePages, NumWorkers])
	end.

%% @doc 运行测试
run_test(Config) ->
	#test_config{
		input_file = InputFileHandle,
		output_file = OutputFileHandle
	} = Config,

	io:format("packing..."),
	ar_bench_timer:record({wall}, fun test/1, [Config]),

	file:close(InputFileHandle),
	file:close(OutputFileHandle).

%% @doc 执行测试
test(Config) ->
	#test_config{
		test = Test,
		total_megabytes = TotalMegaBytes,
		num_workers = NumWorkers
	} = Config,
	TotalBytes = TotalMegaBytes * ?MiB,
	%% 启动多个工作进程,每个负责处理一部分数据
	WorkerSize = TotalBytes div NumWorkers,
	{_, WorkerFun} = maps:get(Test, ?VALID_TESTS),
	Workers = [spawn_monitor(
		fun() -> worker(
			N,
			Config, 
			WorkerFun,
			WorkerSize * (N - 1),
			WorkerSize
		) end) || N <- lists:seq(1, NumWorkers)],
	%% 等待所有工作进程完成
	[
		receive
			{'DOWN', Ref, process, Pid, _Result} -> erlang:demonitor(Ref), ok
		after 
			60000 -> timeout
		end || {Pid, Ref} <- Workers
	],
	io:format("~n").

%% @doc 工作进程函数
worker(WorkerID, Config, WorkerFun, Offset, Size) ->
	ar_bench_timer:record({total, WorkerID}, WorkerFun, [
			WorkerID,
			Config,
			Offset,
			Size
			]),
	exit(normal).

%% @doc 检查是否为重打包测试
is_repack_test(Test) ->
	{IsRepackTest, _} = maps:get(Test, ?VALID_TESTS),
	IsRepackTest.
