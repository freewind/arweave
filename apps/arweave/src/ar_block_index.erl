%%====================================================================
%% 文件功能描述：
%% 本模块实现了区块索引功能，用于维护和管理区块链的索引信息。
%%
%% 主要功能：
%% 1. 存储和管理区块索引信息
%% 2. 提供区块查询接口
%% 3. 处理区块链分叉和重组
%% 4. 维护区块高度到区块哈希的映射
%% 5. 提供区块范围查询功能
%%
%% 数据结构：
%% - 使用ETS表存储区块索引
%% - 每个索引项包含: {WeaveSize, Height, Hash, TXRoot}
%% - 按区块高度排序存储
%%====================================================================

-module(ar_block_index).

-export([init/1, update/2, member/1, get_list/1, get_list_by_hash/1, get_element_by_height/1,
		get_block_bounds/1, get_intersection/2, get_intersection/1, get_range/2, get_last/0]).

%%%===================================================================
%%% 公共API
%%%===================================================================

%% @doc 将给定的区块索引存储到ETS表中
init(BI) ->
	init(lists:reverse(BI), 0).

%% @doc 从BI插入新的区块索引元素并移除N个孤立的元素
update([], 0) ->
	ok;
update(BI, 0) ->
	{_WeaveSize, Height, _H, _TXRoot} = ets:last(block_index),
	update2(BI, Height + 1);
update(BI, N) ->
	ets:delete(block_index, ets:last(block_index)),
	update(BI, N - 1).

%% @doc 检查给定的区块哈希是否在索引中
member(H) ->
	member(H, ets:last(block_index)).

%% @doc 返回到给定高度(包含)的{H, WeaveSize, TXRoot}三元组列表
%% 按从最新到最旧排序
get_list(Height) ->
	get_list([], ets:first(block_index), -1, Height).

%% @doc 返回到具有给定哈希H的区块(包含)的{H, WeaveSize, TXRoot}三元组列表
%% 按从最新到最旧排序
get_list_by_hash(H) ->
	get_list_by_hash([], ets:first(block_index), -1, H).

%% @doc 返回给定高度的{H, WeaveSize, TXRoot}三元组或not_found
get_element_by_height(Height) ->
	case catch ets:slot(block_index, Height) of
		{'EXIT', _} ->
			not_found;
		'$end_of_table' ->
			not_found;
		[{{WeaveSize, Height, H, TXRoot}}] ->
			{H, WeaveSize, TXRoot}
	end.

%% @doc 返回{BlockStartOffset, BlockEndOffset, TXRoot}
%% 其中Offset >= BlockStartOffset, Offset < BlockEndOffset
get_block_bounds(Offset) ->
	{WeaveSize, Height, _H, TXRoot} = Key = ets:next(block_index, {Offset, n, n, n}),
	case Height of
		0 ->
			{0, WeaveSize, TXRoot};
		_ ->
			{PrevWeaveSize, _, _, _} = ets:prev(block_index, Key),
			{PrevWeaveSize, WeaveSize, TXRoot}
	end.

%% @doc 返回{Height, {H, WeaveSize, TXRoot}}
%% 其中三元组同时存在于缓存的区块索引和给定的BI中
%% 如果没有交集则返回no_intersection
get_intersection(Height, _BI) when Height < 0 ->
	no_intersection;
get_intersection(_Height, []) ->
	no_intersection;
get_intersection(Height, BI) ->
	ReverseBI = lists:reverse(BI),
	[{H, _, _} = Elem | ReverseBI2] = ReverseBI,
	case catch ets:slot(block_index, Height) of
		[{{_, Height, H, _} = Entry}] ->
			get_intersection(Height + 1, Elem, ReverseBI2, ets:next(block_index, Entry));
		_ ->
			no_intersection
	end.

%% @doc 返回同时存在于缓存的区块索引和给定的BI中的{H, WeaveSize, TXRoot}三元组
%% 如果没有交集则返回no_intersection
get_intersection([]) ->
	no_intersection;
get_intersection(BI) ->
	{H, WeaveSize, _TXRoot} = lists:last(BI),
	get_intersection2({H, WeaveSize}, tl(lists:reverse(BI)),
			ets:next(block_index, {WeaveSize - 1, n, n, n})).

%% @doc 返回高度在Start和End之间(包含)的{H, WeaveSize, TXRoot}列表
%% 按从高度大到小排序
get_range(Start, End) when Start > End ->
	[];
get_range(Start, End) ->
	case catch ets:slot(block_index, Start) of
		[{{WeaveSize, _Height, H, TXRoot} = Entry}] ->
			lists:reverse([{H, WeaveSize, TXRoot}
				| get_range2(Start + 1, End, ets:next(block_index, Entry))]);
		_ ->
			{error, invalid_start}
	end.

%% @doc 返回区块索引中的最后一个元素
get_last() ->
	ets:last(block_index).

%%%===================================================================
%%% 私有函数
%%%===================================================================

%% @doc 初始化区块索引
init([], _Height) ->
	ok;
init([{H, WeaveSize, TXRoot} | BI], Height) ->
	ets:insert(block_index, {{WeaveSize, Height, H, TXRoot}}),
	init(BI, Height + 1).

%% @doc 更新区块索引
update2([], _Height) ->
	ok;
update2([{H, WeaveSize, TXRoot} | BI], Height) ->
	ets:insert(block_index, {{WeaveSize, Height, H, TXRoot}}),
	update2(BI, Height + 1).

%% @doc 检查区块哈希是否在索引中
member(H, {_, _, H, _}) ->
	true;
member(_H, '$end_of_table') ->
	false;
member(H, Key) ->
	member(H, ets:prev(block_index, Key)).

%% @doc 获取区块列表
get_list(BI, '$end_of_table', _Height, _MaxHeight) ->
	BI;
get_list(BI, _Elem, Height, MaxHeight) when Height >= MaxHeight ->
	BI;
get_list(BI, {WeaveSize, NextHeight, H, TXRoot} = Key, Height, MaxHeight)
		when NextHeight == Height + 1 ->
	get_list([{H, WeaveSize, TXRoot} | BI], ets:next(block_index, Key), Height + 1, MaxHeight);
get_list(_BI, _Key, _Height, MaxHeight) ->
	%% 极不可能发生的竞争条件:我们传递了一些区块后,它们被孤立了,
	%% 在我们到达表尾之前又添加了新的区块
	get_list(MaxHeight).

%% @doc 按哈希获取区块列表
get_list_by_hash(BI, '$end_of_table', _Height, _H) ->
	BI;
get_list_by_hash(BI, {WeaveSize, NextHeight, H, TXRoot}, Height, H)
		when NextHeight == Height + 1 ->
	[{H, WeaveSize, TXRoot} | BI];
get_list_by_hash(BI, {WeaveSize, NextHeight, H, TXRoot} = Key, Height, H2)
		when NextHeight == Height + 1 ->
	get_list_by_hash([{H, WeaveSize, TXRoot} | BI], ets:next(block_index, Key), Height + 1,
			H2);
get_list_by_hash(_BI, _Key, _Height, H) ->
	%% 极不可能发生的竞争条件:我们传递了一些区块后,它们被孤立了,
	%% 在我们到达表尾之前又添加了新的区块
	get_list_by_hash(H).

%% @doc 获取交集
get_intersection(Height, Entry, _ReverseBI, '$end_of_table') ->
	{Height - 1, Entry};
get_intersection(Height, Entry, [], _Entry) ->
	{Height - 1, Entry};
get_intersection(Height, _Entry, [{H, _, _} = Elem | ReverseBI], {_, Height, H, _} = Entry) ->
	get_intersection(Height + 1, Elem, ReverseBI, ets:next(block_index, Entry));
get_intersection(Height, Entry, _ReverseBI, _TableEntry) ->
	{Height - 1, Entry}.

get_intersection2(_, _, '$end_of_table') ->
	no_intersection;
get_intersection2({_, WeaveSize}, _, {WeaveSize2, _, _, _}) when WeaveSize2 > WeaveSize ->
	no_intersection;
get_intersection2({H, WeaveSize}, BI, {WeaveSize, _, H, TXRoot} = Elem) ->
	get_intersection3(ets:next(block_index, Elem), BI, {H, WeaveSize, TXRoot});
get_intersection2({H, WeaveSize}, BI, {WeaveSize, _, _, _} = Elem) ->
	get_intersection2({H, WeaveSize}, BI, ets:next(block_index, Elem)).

get_intersection3({WeaveSize, _, H, TXRoot} = Key, [{H, WeaveSize, TXRoot} | BI], _Elem) ->
	get_intersection3(ets:next(block_index, Key), BI, {H, WeaveSize, TXRoot});
get_intersection3(_, _, {H, WeaveSize, TXRoot}) ->
	{H, WeaveSize, TXRoot}.

%% @doc 获取区块范围
get_range2(Start, End, _Elem) when Start > End ->
	[];
get_range2(_Start, _End, '$end_of_table') ->
	[];
get_range2(Start, End, {WeaveSize, _Height, H, TXRoot} = Elem) ->
	[{H, WeaveSize, TXRoot} | get_range2(Start + 1, End, ets:next(block_index, Elem))].
