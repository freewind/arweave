%%====================================================================
%% 文件功能描述：
%% 本模块实现了区块时间历史记录功能，用于追踪和分析区块生成时间。
%% 主要用于难度调整算法，通过分析历史区块时间来调整挖矿难度。
%%
%% 关键概念：
%% 1. 区块时间 - 相邻区块的时间差
%% 2. 时间窗口 - 用于计算的历史区块数量
%% 3. 难度调整 - 基于历史时间的难度计算
%%
%% 重要说明：
%% - 使用ETS表存储时间历史
%% - 支持多种统计计算
%% - 线程安全的实现
%%====================================================================

%% 声明模块名
-module(ar_block_time_history).

%% 导出公共API函数
-export([
    init/0,
    init/1,
    add/4,
    get_average_block_time/1,
    get_average_block_time_for_range/3,
    get_median_block_time/1,
    get_median_block_time_for_range/3,
    get_block_period_for_range/3
]).

%% 引入必要的头文件
-include_lib("arweave/include/ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc 初始化时间历史记录
%% 创建一个名为'block_time_history'的ETS表
init() ->
    %% 创建ETS表，使用ordered_set类型确保按键排序
    %% public选项允许其他进程访问此表
    ets:new(block_time_history, [ordered_set, public, named_table]).

%% @doc 使用给定的区块列表初始化时间历史
%% Blocks: 区块列表，按时间倒序排列
init(Blocks) ->
    %% 首先初始化ETS表
    init(),
    %% 将区块列表转换为时间记录并插入ETS表
    lists:foldl(
        fun(B, _) ->
            add(B#block.indep_hash, B#block.height,
                B#block.timestamp, B#block.previous_block)
        end,
        ok,
        Blocks
    ).

%% @doc 添加新的区块时间记录
%% H: 区块哈希(Hash)
%% Height: 区块高度
%% TS: 时间戳(Timestamp)
%% PrevH: 前一个区块的哈希
add(H, Height, TS, PrevH) ->
    %% 插入新记录到ETS表
    %% 键为区块高度，值为包含所有信息的元组
    ets:insert(block_time_history, {Height, {H, TS, PrevH}}).

%% @doc 获取指定数量区块的平均时间
%% N: 要计算的区块数量
get_average_block_time(N) ->
    %% 获取最新的区块高度
    case ets:last(block_time_history) of
        '$end_of_table' ->
            %% 如果表为空，返回默认目标时间
            ?TARGET_TIME;
        Height ->
            %% 计算指定范围内的平均区块时间
            get_average_block_time_for_range(Height - N + 1, Height, N)
    end.

%% @doc 计算指定高度范围内的平均区块时间
%% StartHeight: 起始高度
%% EndHeight: 结束高度
%% N: 预期的区块数量
get_average_block_time_for_range(StartHeight, EndHeight, N) ->
    %% 获取范围内的所有区块时间
    case get_block_times_for_range(StartHeight, EndHeight) of
        [] ->
            %% 如果没有数据，返回默认目标时间
            ?TARGET_TIME;
        BlockTimes ->
            %% 计算平均值
            lists:sum(BlockTimes) / min(length(BlockTimes), N)
    end.

%% @doc 获取指定数量区块的中位数时间
%% N: 要计算的区块数量
get_median_block_time(N) ->
    case ets:last(block_time_history) of
        '$end_of_table' ->
            ?TARGET_TIME;
        Height ->
            get_median_block_time_for_range(Height - N + 1, Height, N)
    end.

%% @doc 计算指定高度范围内的中位数区块时间
%% StartHeight: 起始高度
%% EndHeight: 结束高度
%% N: 预期的区块数量
get_median_block_time_for_range(StartHeight, EndHeight, N) ->
    case get_block_times_for_range(StartHeight, EndHeight) of
        [] ->
            ?TARGET_TIME;
        BlockTimes ->
            %% 对时间列表排序
            SortedTimes = lists:sort(BlockTimes),
            Length = length(SortedTimes),
            %% 取中位数
            case Length rem 2 of
                0 ->
                    %% 偶数个元素，取中间两个值的平均
                    (lists:nth(Length div 2, SortedTimes) +
                     lists:nth(Length div 2 + 1, SortedTimes)) / 2;
                1 ->
                    %% 奇数个元素，直接取中间值
                    lists:nth(Length div 2 + 1, SortedTimes)
            end
    end.

%% @doc 获取指定高度范围内的总时间周期
%% StartHeight: 起始高度
%% EndHeight: 结束高度
%% N: 预期的区块数量
get_block_period_for_range(StartHeight, EndHeight, N) ->
    case get_timestamps_for_range(StartHeight, EndHeight) of
        [] ->
            %% 如果没有数据，返回默认目标时间乘以区块数
            ?TARGET_TIME * N;
        [_] ->
            %% 只有一个时间戳时，返回默认值
            ?TARGET_TIME * N;
        Timestamps ->
            %% 计算最早和最晚时间戳的差值
            lists:max(Timestamps) - lists:min(Timestamps)
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 获取指定高度范围内的区块时间列表
%% StartHeight: 起始高度
%% EndHeight: 结束高度
%% 返回：区块时间列表
get_block_times_for_range(StartHeight, EndHeight) ->
    case get_timestamps_for_range(StartHeight, EndHeight) of
        [] ->
            [];
        [_] ->
            [];
        Timestamps ->
            %% 计算相邻时间戳的差值
            lists:zipwith(
                fun(T1, T2) -> T2 - T1 end,
                lists:droplast(Timestamps),
                tl(Timestamps)
            )
    end.

%% @doc 获取指定高度范围内的时间戳列表
%% StartHeight: 起始高度
%% EndHeight: 结束高度
%% 返回：时间戳列表，按时间升序排列
get_timestamps_for_range(StartHeight, EndHeight) ->
    %% 使用列表推导式从ETS表中获取时间戳
    [TS || {Height, {_H, TS, _PrevH}} <- 
           ets:select(block_time_history,
                     [{{'$1', {'$2', '$3', '$4'}},
                       [{'>=', '$1', StartHeight},
                        {'=<', '$1', EndHeight}],
                       ['$_']}])].

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% @doc 测试初始化功能
init_test() ->
    init(),
    ?assert(ets:info(block_time_history) /= undefined).

%% @doc 测试添加和获取功能
add_get_test() ->
    init(),
    add(<<"hash1">>, 1, 100, <<"prev_hash1">>),
    add(<<"hash2">>, 2, 200, <<"hash1">>),
    add(<<"hash3">>, 3, 300, <<"hash2">>),
    ?assertEqual(100, element(2, element(2, hd(ets:lookup(block_time_history, 1))))).

%% @doc 测试平均时间计算
average_time_test() ->
    init(),
    add(<<"hash1">>, 1, 100, <<"prev_hash1">>),
    add(<<"hash2">>, 2, 200, <<"hash1">>),
    add(<<"hash3">>, 3, 300, <<"hash2">>),
    ?assertEqual(100.0, get_average_block_time(2)).

%% @doc 测试中位数时间计算
median_time_test() ->
    init(),
    add(<<"hash1">>, 1, 100, <<"prev_hash1">>),
    add(<<"hash2">>, 2, 300, <<"hash1">>),
    add(<<"hash3">>, 3, 400, <<"hash2">>),
    ?assertEqual(100.0, get_median_block_time(2)).
