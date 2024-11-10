%% ar_process_sampler模块
%% 这是一个进程采样器，用于收集Erlang虚拟机中进程的运行状态数据
%% 采用gen_server行为模式实现，这是Erlang中最常用的进程行为模式之一

%% 声明模块名
-module(ar_process_sampler).
%% 声明这个模块遵循gen_server行为模式
%% gen_server是Erlang/OTP中的一个通用服务器行为模式，用于实现客户端-服务器关系
-behaviour(gen_server).

%% 包含arweave项目的头文件，其中定义了一些常量和类型
-include_lib("arweave/include/ar.hrl").

%% 导出函数列表
%% start_link/0 用于启动服务
%% init/1等是gen_server行为模式要求必须实现的回调函数
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 定义常量
%% 采样进程的时间间隔：15000毫秒（15秒）
-define(SAMPLE_PROCESSES_INTERVAL, 15000).
%% 采样调度器的时间间隔：30000毫秒（30秒）
-define(SAMPLE_SCHEDULERS_INTERVAL, 30000).
%% 采样调度器的持续时间：5000毫秒（5秒）
-define(SAMPLE_SCHEDULERS_DURATION, 5000).

%% 定义状态记录
%% scheduler_samples用于存储调度器采样数据
%% undefined是其初始值
-record(state, {
	scheduler_samples = undefined
}).

%% API函数
%% 启动服务器并注册为本地名称（使用模块名作为进程名）
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server回调函数
%% 初始化函数，当服务器启动时调用
init([]) ->
	%% 设置定时器，每隔SAMPLE_PROCESSES_INTERVAL毫秒发送sample_processes消息
	timer:send_interval(?SAMPLE_PROCESSES_INTERVAL, sample_processes),
	%% 延迟SAMPLE_SCHEDULERS_INTERVAL毫秒后发送sample_schedulers消息
	ar_util:cast_after(?SAMPLE_SCHEDULERS_INTERVAL, ?MODULE, sample_schedulers),
	%% 返回初始状态
	{ok, #state{}}.

%% 处理同步调用的回调函数
%% _Request: 请求内容
%% _From: 请求方的进程标识
%% State: 当前状态
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% 处理异步消息的回调函数
handle_cast(sample_schedulers, State) ->
	%% 当收到sample_schedulers消息时，执行采样
	State2 = sample_schedulers(State),
	{noreply, State2};
	
handle_cast(_Msg, State) ->
	{noreply, State}.

%% 处理其他消息的回调函数
handle_info(sample_processes, State) ->
	%% 获取单调递增的时间戳（monotonic：单调的，永远递增不会倒退的时间）
	StartTime = erlang:monotonic_time(),
	%% 获取系统中所有进程的PID列表
	Processes = erlang:processes(),
	%% 过滤并映射进程信息
	ProcessData = lists:filtermap(fun(Pid) -> process_function(Pid) end, Processes),

	%% 处理进程数据，按进程名称聚合信息
	ProcessMetrics =
		lists:foldl(fun({_Status, ProcessName, Memory, Reductions, MsgQueueLen}, Acc) ->
			%% 对每个进程名称，累加其内存使用、归约次数和消息队列长度
			%% 这样做是为了处理未注册的进程，将相同函数的多个实例数据合并
			%% 例如：矿工IO线程或哈希计算线程的聚合数据
			{MemoryTotal, ReductionsTotal, MsgQueueLenTotal} =
				maps:get(ProcessName, Acc, {0, 0, 0}),
			Metrics = {
				MemoryTotal + Memory, 
				ReductionsTotal + Reductions, 
				MsgQueueLenTotal + MsgQueueLen},
			maps:put(ProcessName, Metrics, Acc)
		end, 
		#{},
		ProcessData),

	%% 重置进程信息指标
	%% 注销现有的process_info指标，以清除已退出进程的数据
	prometheus_gauge:deregister(process_info),
	%% 重新注册process_info指标
	prometheus_gauge:new([{name, process_info},
		{labels, [process, type]},
		{help, "采样活动进程的信息。仅在debug=true时设置。"}]),

	%% 更新每个进程的指标
	maps:foreach(fun(ProcessName, Metrics) ->
		{Memory, Reductions, MsgQueueLen} = Metrics,
		%% 设置内存使用量指标
		prometheus_gauge:set(process_info, [ProcessName, memory], Memory),
		%% 设置归约次数指标（归约是Erlang中的计算步骤单位）
		prometheus_gauge:set(process_info, [ProcessName, reductions], Reductions),
		%% 设置消息队列长度指标
		prometheus_gauge:set(process_info, [ProcessName, message_queue], MsgQueueLen)
	end, ProcessMetrics),

	%% 设置系统内存相关指标
	prometheus_gauge:set(process_info, [total, memory], erlang:memory(total)),
	prometheus_gauge:set(process_info, [processes, memory], erlang:memory(processes)),
	prometheus_gauge:set(process_info, [processes_used, memory], erlang:memory(processes_used)),
	prometheus_gauge:set(process_info, [system, memory], erlang:memory(system)),
	prometheus_gauge:set(process_info, [atom, memory], erlang:memory(atom)),
	prometheus_gauge:set(process_info, [atom_used, memory], erlang:memory(atom_used)),
	prometheus_gauge:set(process_info, [binary, memory], erlang:memory(binary)),
	prometheus_gauge:set(process_info, [code, memory], erlang:memory(code)),
	prometheus_gauge:set(process_info, [ets, memory], erlang:memory(ets)),

	%% 记录二进制内存分配器的信息
	log_binary_alloc(),

	%% 计算采样耗时并记录日志
	EndTime = erlang:monotonic_time(),
	ElapsedTime = erlang:convert_time_unit(EndTime-StartTime, native, microsecond),
	?LOG_DEBUG([{event, sample_processes}, {elapsed_ms, ElapsedTime / 1000}]),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

%% 终止回调函数
terminate(_Reason, _State) ->
	ok.

%% 内部函数

%% 采样调度器状态
%% 当scheduler_samples为undefined时开始采样
sample_schedulers(#state{ scheduler_samples = undefined } = State) ->
	%% 启用调度器墙钟时间测量
	erlang:system_flag(scheduler_wall_time,true),
	Samples = scheduler:sample_all(),
	%% 设置下一次采样的定时器
	ar_util:cast_after(?SAMPLE_SCHEDULERS_INTERVAL, ?MODULE, sample_schedulers),
	ar_util:cast_after(?SAMPLE_SCHEDULERS_DURATION, ?MODULE, sample_schedulers),
	State#state{ scheduler_samples = Samples };

%% 当有前次采样数据时，计算使用率
sample_schedulers(#state{ scheduler_samples = Samples1 } = State) ->
	Samples2 = scheduler:sample_all(),
	Util = scheduler:utilization(Samples1, Samples2),
	erlang:system_flag(scheduler_wall_time,false),
	average_utilization(Util),
	State#state{ scheduler_samples = undefined }.

%% 计算平均使用率并更新指标
average_utilization(Util) ->
	Averages = lists:foldl(
		fun
		({Type, Value, _}, Acc) ->
			maps:put(Type, {Value, 1}, Acc);
		({Type, _, Value, _}, Acc) ->
			case (Type == io andalso Value > 0) orelse (Type /= io) of
				true ->
					{Sum, Count} = maps:get(Type, Acc, {0, 0}),
					maps:put(Type, {Sum+Value, Count+1}, Acc);
				false ->
					Acc
			end
		end,
		#{},
		Util),
	maps:foreach(
		fun(Type, {Sum, Count}) ->
			prometheus_gauge:set(scheduler_utilization, [Type], Sum / Count)
		end,
		Averages).
	
%% 获取进程信息
process_function(Pid) ->
	case process_info(Pid, [current_function, current_stacktrace, registered_name,
		status, memory, reductions, message_queue_len]) of
	%% 跳过正在执行process_info的进程
	[{current_function, {erlang, process_info, _A}}, _, _, _, _, _, _] ->
		false;
	%% 收集进程信息
	[{current_function, _CurrentFunction}, {current_stacktrace, Stack},
			{registered_name, Name}, {status, Status},
			{memory, Memory}, {reductions, Reductions},
			{message_queue_len, MsgQueueLen}] ->
		ProcessName = process_name(Name, Stack),
		{true, {Status, ProcessName, Memory, Reductions, MsgQueueLen}};
	_ ->
		false
	end.

%% 记录二进制内存分配器信息
log_binary_alloc() ->
	[Instance0 | _Rest] = erlang:system_info({allocator, binary_alloc}),
	log_binary_alloc_instances([Instance0]).

%% 处理二进制分配器实例
log_binary_alloc_instances([]) ->
	ok;
log_binary_alloc_instances([Instance | _Rest]) ->
	{instance, Id, [
		_Versions,
		_Options,
		MBCS,
		SBCS,
		Calls
	]} = Instance,
	{calls, [
		{binary_alloc, AllocGigaCount, AllocCount},
		{binary_free, FreeGigaCount, FreeCount},
		{binary_realloc, ReallocGigaCount, ReallocCount},
		_MsegAllocCount, _MsegDeallocCount, _MsegReallocCount,
		_SysAllocCount, _SysDeallocCount, _SysReallocCount
	]} = Calls,

	%% 记录载体信息
	log_binary_alloc_carrier(Id, MBCS),
	log_binary_alloc_carrier(Id, SBCS),

	%% 更新分配计数指标
	prometheus_gauge:set(allocator, [binary, Id, calls, binary_alloc_count],
		(AllocGigaCount * 1000000000) + AllocCount),
	prometheus_gauge:set(allocator, [binary, Id, calls, binary_free_count],
		(FreeGigaCount * 1000000000) + FreeCount),
	prometheus_gauge:set(allocator, [binary, Id, calls, binary_realloc_count],
		(ReallocGigaCount * 1000000000) + ReallocCount).

%% 记录二进制分配器载体信息
log_binary_alloc_carrier(Id, Carrier) ->
	{CarrierType, [
		{blocks, Blocks},
		{carriers, _, CarrierCount, _},
		_MsegCount, _SysCount,
		{carriers_size, _, CarrierSize, _},
		_MsegSize, _SysSize
	]} = Carrier,

	%% 更新块信息指标
	case Blocks of
		[{binary_alloc, [{count, _, BlockCount, _}, {size, _, BlockSize, _}]}] ->
			prometheus_gauge:set(allocator, [binary, Id, CarrierType, binary_block_count],
				BlockCount),
			prometheus_gauge:set(allocator, [binary, Id, CarrierType, binary_block_size],
				BlockSize);
		_ ->
			prometheus_gauge:set(allocator, [binary, Id, CarrierType, binary_block_count],
				0),
			prometheus_gauge:set(allocator, [binary, Id, CarrierType, binary_block_size],
				0)
	end,

	%% 更新载体信息指标
	prometheus_gauge:set(allocator, [binary, Id, CarrierType, binary_carrier_count],
		CarrierCount),
	prometheus_gauge:set(allocator, [binary, Id, CarrierType, binary_carrier_size], 
		CarrierSize).

%% 获取进程名称
%% 对于匿名进程，使用其模块、函数和参数数量作为名称
process_name([], []) ->
	"unknown";
process_name([], Stack) ->
	InitialCall = initial_call(lists:reverse(Stack)),
	M = element(1, InitialCall),
	F = element(2, InitialCall),
	A = element(3, InitialCall),
	atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A);
process_name(Name, _Stack) ->
	atom_to_list(Name).

%% 获取初始调用信息
initial_call([]) ->
	"unknown";
initial_call([{proc_lib, init_p_do_apply, _A, _Location} | Stack]) ->
	initial_call(Stack);
initial_call([InitialCall | _Stack]) ->
	InitialCall.
