%%====================================================================
%% 文件功能描述：
%% 本模块提供基准测试的计时功能，用于记录和统计各种操作的执行时间。
%%
%% 主要功能：
%% 1. 记录操作的执行时间
%% 2. 计算总时间、最大时间、最小时间和平均时间
%% 3. 支持多进程并发记录时间
%% 4. 提供时间数据的格式化输出
%%
%% 实现方式：
%% - 使用ETS表存储时间数据
%% - 支持按Key分类统计不同操作的时间
%% - 提供微秒级的精确计时
%%====================================================================

-module(ar_bench_timer).

%% API导出
-export([initialize/0, reset/0, record/3, start/1, stop/1, 
         get_timing_data/0, print_timing_data/0,
         get_total/1, get_max/1, get_min/1, get_avg/1]).

%% 引入必要的头文件
-include_lib("arweave/include/ar.hrl").
-include_lib("arweave/include/ar_vdf.hrl").
-include_lib("arweave/include/ar_consensus.hrl").

%% @doc 记录函数执行时间
%% Key - 操作标识
%% Fun - 要执行的函数
%% Args - 函数参数
record(Key, Fun, Args) ->
    {Time, Result} = timer:tc(Fun, Args), % 使用timer:tc测量执行时间(微秒)
    update_total(Key, Time),              % 更新总时间
    Result.                               % 返回函数执行结果

%% @doc 开始计时
%% Key - 操作标识
start(Key) ->
    StartTime = erlang:timestamp(),
    ets:insert(start_time, {real_key(Key), StartTime}).

%% @doc 停止计时并记录时间
%% Key - 操作标识
stop(Key) ->
    case ets:lookup(start_time, real_key(Key)) of
        [{_, StartTime}] ->
            EndTime = erlang:timestamp(),
            ElapsedTime = timer:now_diff(EndTime, StartTime), % 计算时间差(微秒)
            update_total(Key, ElapsedTime),
            ElapsedTime;
        [] ->
            {error, {not_started, Key}}
    end.

%% @doc 更新操作的总执行时间
update_total(Key, ElapsedTime) ->
    ets:update_counter(total_time, real_key(Key), {2, ElapsedTime}, {real_key(Key), 0}).

%% @doc 获取操作的总执行时间
get_total([]) ->
    0;
get_total(Times) when is_list(Times) ->
    lists:sum(Times);
get_total(Key) ->
    get_total(get_times(Key)).

%% @doc 获取操作的最大执行时间
get_max([]) ->
    0;
get_max(Times) when is_list(Times) ->
    lists:max(Times);
get_max(Key) ->
    get_max(get_times(Key)).

%% @doc 获取操作的最小执行时间
get_min([]) ->
    0;
get_min(Times) when is_list(Times) ->
    lists:min(Times);
get_min(Key) ->
    get_min(get_times(Key)).

%% @doc 获取操作的平均执行时间
get_avg([]) ->
    0;
get_avg(Times) when is_list(Times) ->
    TotalTime = lists:sum(Times),
    case length(Times) of
        0 -> 0;
        N -> TotalTime / N
    end;
get_avg(Key) ->
    get_avg(get_times(Key)).

%% @doc 获取指定Key的所有执行时间记录
get_times(Key) ->
    [Match || [Match] <- ets:match(total_time, {{Key, '_'}, '$1'})].

%% @doc 获取所有计时Key
get_timing_keys() ->
    Keys = [Key || {{Key, _PID}, _Value} <- get_timing_data()],
    UniqueKeys = sets:to_list(sets:from_list(Keys)),
    UniqueKeys.

%% @doc 获取所有计时数据
get_timing_data() ->
    ets:tab2list(total_time).

%% @doc 打印所有计时数据(转换为秒)
print_timing_data() ->
    lists:foreach(fun(Key) ->
            Seconds = get_total(Key) / 1000000,
            ?LOG_ERROR("~p: ~p", [Key, Seconds])
        end, get_timing_keys()).

%% @doc 重置所有计时数据
reset() ->
    ets:delete_all_objects(total_time),
    ets:delete_all_objects(start_time).

%% @doc 初始化计时器
%% 创建两个ETS表:
%% - total_time: 存储累计时间
%% - start_time: 存储开始时间
initialize() ->
    ets:new(total_time, [set, named_table, public]),
    ets:new(start_time, [set, named_table, public]).

%% @doc 生成实际的Key(包含进程ID)
%% 用于区分不同进程的同名操作
real_key(Key) ->
    {Key, self()}.


