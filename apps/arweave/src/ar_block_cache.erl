%%====================================================================
%% 文件功能描述：
%% 本模块维护一个已通过工作量证明(PoW)验证的区块有向无环图(DAG)，使用ETS表存储。
%%
%% 主要功能：
%% 1. 管理区块缓存
%% 2. 跟踪区块验证状态
%% 3. 处理区块分叉
%% 4. 维护最长链
%%
%% 重要说明：
%% - 不建议从不同进程调用修改状态的函数，可能导致数据不一致
%% - 使用ETS表提供高效的内存存储和快速访问
%% - 支持区块分叉的检测和处理
%%====================================================================

-module(ar_block_cache).

%% API导出
-export([new/2, initialize_from_list/2, add/2, mark_nonce_limiter_validated/2,
        add_validated/2,
        mark_tip/2, get/2, get_earliest_not_validated_from_longest_chain/1,
        get_longest_chain_cache/1,
        get_block_and_status/2, remove/2, get_checkpoint_block/1, prune/2,
        get_by_solution_hash/5, is_known_solution_hash/2,
        get_siblings/2, get_fork_blocks/2, update_timestamp/3]).

%% 引入必要的头文件
-include_lib("arweave/include/ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%% 常量定义
%% 具有相同解决方案的"替代"区块的过期时间(秒)
-define(ALTERNATIVE_BLOCK_EXPIRATION_TIME_SECONDS, 5).

%% @doc 区块验证状态
%% on_chain: 区块已验证且属于当前最长链
%% validated: 区块已验证但不在当前最长链上
%% not_validated: 区块尚未验证
%% none: 空状态

%% @doc ETS表结构说明
%% {block, BlockHash} => {#block{}, block_status(), Timestamp, set(Children)}
%%   - Children是将此区块作为前一个区块的所有区块集合
%%   - 用于跟踪从该区块分叉出的分支(即DAG子节点)
%%
%% max_cdiff => {CDiff, BlockHash}
%%   - 遇到的最大累积难度及其区块哈希
%%   - 用于确定是否需要从当前链尖切换到分叉链尖
%%
%% {solution, SolutionHash} => set(BlockHash)
%%   - 具有相同解决方案哈希的所有区块集合
%%
%% longest_chain => [{BlockHash, [TXIDs]}]
%%   - 最长链上最新的?STORE_BLOCKS_BEHIND_CURRENT个区块
%%
%% tip -> BlockHash
%%   - 当前区块链尖
%%
%% links -> gb_set({Height, BlockHash})
%%   - 缓存中按高度排序的所有区块
%%   - 用于裁剪缓存时丢弃特定高度以下的所有区块
%%   - 同时丢弃这些区块的所有链下子区块(不考虑其高度)

%% @doc 创建缓存，使用给定区块初始化
%% 该区块被标记为on-chain状态和链尖
new(Tab, B) ->
    #block{ indep_hash = H, hash = SolutionH, cumulative_diff = CDiff, height = Height } =     B,
    ets:delete_all_objects(Tab),
    ar_ignore_registry:add(H),
    insert(Tab, [
        {max_cdiff, {CDiff, H}},
        {links, gb_sets:from_list([{Height, H}])},
        {{solution, SolutionH}, sets:from_list([H])},
        {tip, H},
        {{block, H}, {B, on_chain, erlang:timestamp(), sets:new()}}
    ]).

%% @doc 从给定的已验证区块列表初始化缓存
%% 将最新的区块标记为链尖
%% 给定的区块必须按从新到旧排序
initialize_from_list(Tab, [B]) ->
    new(Tab, B);
initialize_from_list(Tab, [#block{ indep_hash = H } = B | Blocks]) ->
    initialize_from_list(Tab, Blocks),
    add_validated(Tab, B),
    mark_tip(Tab, H).

%% @doc 将区块添加到缓存
%% 区块被标记为尚未验证
%% 如果区块已存在且未验证，则覆盖
%% 如果区块已验证，则不做任何操作并发出警告
add(Tab,
        #block{
            indep_hash = H,
            hash = SolutionH,
            previous_block = PrevH,
            height = Height
        } = B) ->
    case ets:lookup(Tab, {block, H}) of
        [] ->
            ar_ignore_registry:add(H),
            RemainingHs = remove_expired_alternative_blocks(Tab, SolutionH),
            SolutionSet = sets:from_list([H | RemainingHs]),
            [{_, Set}] = ets:lookup(Tab, links),
            [{_, {PrevB, PrevStatus, PrevTimestamp, Children}}] = ets:lookup(Tab, {block, PrevH}),
            Set2 = gb_sets:insert({Height, H}, Set),
            Status = {not_validated, awaiting_nonce_limiter_validation},
            %% 如果CDiff > MaxCDiff，说明此区块属于我们已知的最重分叉
            %% 如果当前链尖不在此分叉上，ar_node_worker可能会切换到此分叉
            insert(Tab, [
                {max_cdiff, maybe_increase_max_cdiff(Tab, B, Status)},
                {links, Set2},
                {{solution, SolutionH}, SolutionSet},
                {{block, H}, {B, Status, erlang:timestamp(), sets:new()}},
                {{block, PrevH},
                        {PrevB, PrevStatus, PrevTimestamp, sets:add_element(H, Children)}}
            ]);
        [{_, {_B, {not_validated, _} = CurrentStatus, CurrentTimestamp, Children}}] ->
            insert(Tab, {{block, H}, {B, CurrentStatus, CurrentTimestamp, Children}});
        _ ->
            ?LOG_WARNING([{event, attempt_to_update_already_validated_cached_block},
                    {h, ar_util:encode(H)}, {height, Height},
                    {previous_block, ar_util:encode(PrevH)}]),
            ok
    end.

%% @doc 检查所有共享相同解决方案的区块并移除过期的区块
remove_expired_alternative_blocks(Tab, SolutionH) ->
    SolutionSet =
        case ets:lookup(Tab, {solution, SolutionH}) of
            [] ->
                sets:new();
            [{_, SolutionSet2}] ->
                SolutionSet2
        end,
    remove_expired_alternative_blocks2(Tab, sets:to_list(SolutionSet)).

%% @doc 递归检查和移除过期的替代区块
remove_expired_alternative_blocks2(_Tab, []) ->
    [];
remove_expired_alternative_blocks2(Tab, [H | Hs]) ->
    [{_, {_B, Status, Timestamp, Children}}] = ets:lookup(Tab, {block, H}),
    case Status of
        on_chain ->
            [H | remove_expired_alternative_blocks2(Tab, Hs)];
        _ ->
            LifetimeSeconds = get_alternative_block_lifetime(Tab, Children),
            {MegaSecs, Secs, MicroSecs} = Timestamp,
            ExpirationTimestamp = {MegaSecs, Secs + LifetimeSeconds, MicroSecs},
            case timer:now_diff(erlang:timestamp(), ExpirationTimestamp) >= 0 of
                true ->
                    remove(Tab, H),
                    remove_expired_alternative_blocks2(Tab, Hs);
                false ->
                    [H | remove_expired_alternative_blocks2(Tab, Hs)]
            end
    end.

%% @doc 获取替代区块的生命周期
%% 基于分叉长度计算过期时间
get_alternative_block_lifetime(Tab, Children) ->
    ForkLen = get_fork_length(Tab, sets:to_list(Children)),
    (?ALTERNATIVE_BLOCK_EXPIRATION_TIME_SECONDS) * ForkLen.

%% @doc 计算分叉长度
%% 返回最长分叉路径的长度
get_fork_length(Tab, Branches) when is_list(Branches) ->
    1 + lists:max([0 | [get_fork_length(Tab, Branch) || Branch <- Branches]]);
get_fork_length(Tab, Branch) ->
    [{_, {_B, _Status, _Timestamp, Children}}] = ets:lookup(Tab, {block, Branch}),
    case sets:size(Children) == 0 of
        true ->
            1;
        false ->
            1 + get_fork_length(Tab, sets:to_list(Children))
    end.

%% @doc 将给定区块的状态更新为'nonce_limiter_validated'
%% 如果区块不在缓存中或其状态不是'awaiting_nonce_limiter_validation'，则不做任何操作
mark_nonce_limiter_validated(Tab, H) ->
    case ets:lookup(Tab, {block, H}) of
        [{_, {B, {not_validated, awaiting_nonce_limiter_validation}, Timestamp, Children}}] ->
            insert(Tab, {{block, H}, {B,
                    {not_validated, nonce_limiter_validated}, Timestamp, Children}});
        _ ->
            ok
    end.

%% @doc 将已验证的区块添加到缓存
%% 如果区块已在缓存中则覆盖，但假设高度、哈希、前一个哈希和累积难度不变
%% 如果前一个区块不在缓存中则抛出previous_block_not_found错误
%% 如果前一个区块未验证则抛出previous_block_not_validated错误
add_validated(Tab, B) ->
    #block{ indep_hash = H, hash = SolutionH, previous_block = PrevH, height = Height } = B,
    case ets:lookup(Tab, {block, PrevH}) of
        [] ->
            error(previous_block_not_found);
        [{_, {_PrevB, {not_validated, _}, _Timestamp, _Children}}] ->
            error(previous_block_not_validated);
        [{_, {PrevB, PrevStatus, PrevTimestamp, PrevChildren}}] ->
            case ets:lookup(Tab, {block, H}) of
                [] ->
                    RemainingHs = remove_expired_alternative_blocks(Tab, SolutionH),
                    SolutionSet = sets:from_list([H | RemainingHs]),
                    [{_, Set}] = ets:lookup(Tab, links),
                    Status = validated,
                    insert(Tab, [
                        {{block, PrevH}, {PrevB, PrevStatus, PrevTimestamp,
                                sets:add_element(H, PrevChildren)}},
                        {{block, H}, {B, Status, erlang:timestamp(), sets:new()}},
                        {max_cdiff, maybe_increase_max_cdiff(Tab, B, Status)},
                        {links, gb_sets:insert({Height, H}, Set)},
                        {{solution, SolutionH}, SolutionSet}
                    ]);
                [{_, {_B, on_chain, Timestamp, Children}}] ->
                    insert(Tab, [
                        {{block, PrevH}, {PrevB, PrevStatus, PrevTimestamp,
                                sets:add_element(H, PrevChildren)}},
                        {{block, H}, {B, on_chain, Timestamp, Children}}
                    ]);
                [{_, {_B, _Status, Timestamp, Children}}] ->
                    insert(Tab, [
                        {{block, PrevH}, {PrevB, PrevStatus, PrevTimestamp,
                                sets:add_element(H, PrevChildren)}},
                        {{block, H}, {B, validated, Timestamp, Children}}
                    ])
            end
    end.

%% @doc 从缓存获取区块
%% 如果区块不在缓存中返回not_found
get(Tab, H) ->
    case ets:lookup(Tab, {block, H}) of
        [] ->
            not_found;
        [{_, {B, _Status, _Timestamp, _Children}}] ->
            B
    end.

%% @doc 从缓存获取区块及其状态
%% 如果区块不在缓存中返回not_found
get_block_and_status(Tab, H) ->
    case ets:lookup(Tab, {block, H}) of
        [] ->
            not_found;
        [{_, {B, Status, Timestamp, _Children}}] ->
            {B, {Status, Timestamp}}
    end.

%% @doc 获取最长链上最早的未验证区块
%% 返回{区块, 前序区块列表, 状态}元组
%% 前序区块按从新到旧排序，最后一个是当前分叉上的区块
%% 状态表示区块在验证过程中的位置
get_earliest_not_validated_from_longest_chain(Tab) ->
    [{_, Tip}] = ets:lookup(Tab, tip),
    [{_, {CDiff, H}}] = ets:lookup(Tab, max_cdiff),
    [{_, {#block{ cumulative_diff = TipCDiff }, _, _, _}}] = ets:lookup(Tab, {block, Tip}),
    case TipCDiff >= CDiff of
        true ->
            %% 当前链尖是最长链的链尖
            not_found;
        false ->
            [{_, {B, Status, Timestamp, _Children}}] = ets:lookup(Tab, {block, H}),
            case Status of
                {not_validated, _} ->
                    get_earliest_not_validated(Tab, B, Status, Timestamp);
                _ ->
                    not_found
            end
    end.

%% @doc 返回最长链上最新的?STORE_BLOCKS_BEHIND_CURRENT个区块的{BH, TXIDs}对列表
%% 以及这个列表中不在链上的区块数量
%%
%% 缓存通过update_longest_chain_cache/1更新
%% 该函数调用get_longest_chain_block_txs_pairs/7
get_longest_chain_cache(Tab) ->
    [{longest_chain, LongestChain}] = ets:lookup(Tab, longest_chain),
    LongestChain.

%% @doc 获取最长链上的区块和交易ID对
get_longest_chain_block_txs_pairs(_Tab, _H, 0, _PrevStatus, _PrevH, Pairs, NotOnChainCount) ->
    {lists:reverse(Pairs), NotOnChainCount};
get_longest_chain_block_txs_pairs(Tab, H, N, PrevStatus, PrevH, Pairs, NotOnChainCount) ->
    case ets:lookup(Tab, {block, H}) of
        [{_, {B, {not_validated, awaiting_nonce_limiter_validation}, _Timestamp,
                _Children}}] ->
            get_longest_chain_block_txs_pairs(Tab, B#block.previous_block,
                    ?STORE_BLOCKS_BEHIND_CURRENT, none, none, [], 0);
        [{_, {B, Status, _Timestamp, _Children}}] ->
            case PrevStatus == on_chain andalso Status /= on_chain of
                true ->
                    %% 在此期间应该发生了重组 - 不太可能发生的事件，重试
                    get_longest_chain_cache(Tab);
                false ->
                    NotOnChainCount2 =
                        case Status of
                            on_chain ->
                                NotOnChainCount;
                            _ ->
                                NotOnChainCount + 1
                        end,
                    Pairs2 = [{B#block.indep_hash, [tx_id(TX) || TX <- B#block.txs]} | Pairs],
                    get_longest_chain_block_txs_pairs(Tab, B#block.previous_block, N - 1,
                            Status, H, Pairs2, NotOnChainCount2)
            end;
        [] ->
            case PrevStatus of
                on_chain ->
                    case ets:lookup(Tab, {block, PrevH}) of
                        [] ->
                            %% 区块已被裁剪 - 
                            %% 不太可能的竞争条件，所以我们重试
                            get_longest_chain_cache(Tab);
                        [_] ->
                            %% Pairs已经包含缓存中最深的区块
                            {lists:reverse(Pairs), NotOnChainCount}
                    end;
                _ ->
                    %% 区块已被验证无效 - 
                    %% 不太可能的竞争条件，所以我们重试
                    get_longest_chain_cache(Tab)
            end
    end.

%% @doc 获取交易ID
tx_id(#tx{ id = ID }) ->
    ID;
tx_id(TXID) ->
    TXID.

%% @doc 将给定区块标记为链尖
%% 将前序区块标记为on_chain
%% 将其他分叉上的on_chain区块标记为validated
%% 如果前序区块之一未验证则抛出invalid_tip错误
%% 如果区块不存在则抛出not_found错误
%%
%% 设置新的链尖可能导致一些分支因检查点而失效
%% 所以我们需要重新计算max_cdiff
mark