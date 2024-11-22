%%====================================================================
%% 文件功能描述：
%% 本模块实现了区块(Block)相关的核心功能，包括：
%% 1. 区块字段验证 - 大小限制、时间戳、难度等
%% 2. 区块哈希计算 - 独立哈希、数据段哈希等
%% 3. 区块数据结构生成 - 交易树、钱包列表等
%% 4. 区块打包和挖矿相关 - 随机数范围、回调字节等
%%
%% 关键点：
%% - 区块是区块链的基本单位，包含交易、状态等重要信息
%% - 本模块实现了区块的验证和生成的核心逻辑
%% - 涉及多个加密算法和数据结构的处理
%%
%% 注意点：
%% - 很多函数都有分叉高度(fork height)的判断，以支持协议升级
%% - 区块大小和字段都有严格限制，需要仔细验证
%% - 性能关键代码需要特别关注效率
%%====================================================================

-module(ar_block).

-export([block_field_size_limit/1, verify_timestamp/2,
		get_max_timestamp_deviation/0, verify_last_retarget/2, verify_weave_size/3,
		verify_cumulative_diff/2, verify_block_hash_list_merkle/2, compute_hash_list_merkle/1,
		compute_h0/2, compute_h0/5, compute_h0/6,
		compute_h1/3, compute_h2/3, compute_solution_h/2,
		indep_hash/1, indep_hash/2, indep_hash2/2,
		generate_signed_hash/1, verify_signature/3,
		generate_block_data_segment/1, generate_block_data_segment/2,
		generate_block_data_segment_base/1, get_recall_range/3, verify_tx_root/1,
		hash_wallet_list/1, generate_hash_list_for_block/2,
		generate_tx_root_for_block/1, generate_tx_root_for_block/2,
		generate_size_tagged_list_from_txs/2, generate_tx_tree/1, generate_tx_tree/2,
		test_wallet_list_performance/0, test_wallet_list_performance/1,
		test_wallet_list_performance/2, test_wallet_list_performance/3,
		poa_to_list/1, shift_packing_2_5_threshold/1,
		get_packing_threshold/2, compute_next_vdf_difficulty/1,
		validate_proof_size/1, vdf_step_number/1, get_packing/2,
		validate_packing_difficulty/2, validate_packing_difficulty/1,
		get_max_nonce/1, get_recall_range_size/1, get_recall_byte/3,
		get_sub_chunk_size/1, get_nonces_per_chunk/1, get_nonces_per_recall_range/1,
		get_sub_chunk_index/2]).

-include_lib("arweave/include/ar.hrl").
-include_lib("arweave/include/ar_pricing.hrl").
-include_lib("arweave/include/ar_consensus.hrl").
-include_lib("arweave/include/ar_block.hrl").
-include_lib("arweave/include/ar_vdf.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Public interface.
%%% 公共接口
%%%===================================================================

%% @doc Check if the block fields meet the size limits.
%% 检查区块字段是否符合指定的大小限制
block_field_size_limit(B = #block{ reward_addr = unclaimed }) ->
	%% The only block which may have an unclaimed reward address is
	%% the genesis block of a new weave.
	%% 唯一可能有未认领奖励地址的区块是新编织的创世区块
	block_field_size_limit(B#block{ reward_addr = <<>> });
block_field_size_limit(B) ->
	%% The byte size limit for the difficulty field depends on the fork height.
	%% 根据分叉高度确定难度字段的字节限制
	DiffBytesLimit =
		case ar_fork:height_1_8() of
			Height when B#block.height >= Height ->
				78;
			_ ->
				10
		end,
	%% Get the proof of access (PoA) related sizes.
	%% 获取区块证明(PoA)相关的大小
	{ChunkSize, DataPathSize} =
		case B#block.poa of
			POA when is_record(POA, poa) ->
				{
					byte_size((B#block.poa)#poa.chunk),
					byte_size((B#block.poa)#poa.data_path)
				};
			_ -> {0, 0}
		end,
	%% Verify the reward address size.
	%% 验证奖励地址大小
	RewardAddrCheck = byte_size(B#block.reward_addr) =< 32,
	%% Verify all field sizes.
	%% 验证所有字段大小
	Check = (byte_size(B#block.nonce) =< 512) and
		(byte_size(B#block.previous_block) =< 48) and
		(byte_size(integer_to_binary(B#block.timestamp)) =< ?TIMESTAMP_FIELD_SIZE_LIMIT) and
		(byte_size(integer_to_binary(B#block.last_retarget))
				=< ?TIMESTAMP_FIELD_SIZE_LIMIT) and
		(byte_size(integer_to_binary(B#block.diff)) =< DiffBytesLimit) and
		(byte_size(integer_to_binary(B#block.height)) =< 20) and
		(byte_size(B#block.hash) =< 48) and
		(byte_size(B#block.indep_hash) =< 48) and
		RewardAddrCheck and
		validate_tags_size(B) and
		(byte_size(integer_to_binary(B#block.weave_size)) =< 64) and
		(byte_size(integer_to_binary(B#block.block_size)) =< 64) and
		(ChunkSize =< ?DATA_CHUNK_SIZE) and
		(DataPathSize =< ?MAX_PATH_SIZE),
	case Check of
		false ->
			%% Log if validation fails.
			%% 如果验证失败，记录日志
			?LOG_INFO(
				[
					{event, received_block_with_invalid_field_size},
					{nonce, byte_size(B#block.nonce)},
					{previous_block, byte_size(B#block.previous_block)},
					{timestamp, byte_size(integer_to_binary(B#block.timestamp))},
					{last_retarget, byte_size(integer_to_binary(B#block.last_retarget))},
					{diff, byte_size(integer_to_binary(B#block.diff))},
					{height, byte_size(integer_to_binary(B#block.height))},
					{hash, byte_size(B#block.hash)},
					{indep_hash, byte_size(B#block.indep_hash)},
					{reward_addr, byte_size(B#block.reward_addr)},
					{tags, byte_size(list_to_binary(B#block.tags))},
					{weave_size, byte_size(integer_to_binary(B#block.weave_size))},
					{block_size, byte_size(integer_to_binary(B#block.block_size))}
				]
			);
		_ ->
			ok
	end,
	Check.

%% @doc Verify that the block timestamp is within acceptable bounds.
%% 验证区块时间戳是否在合理范围内
verify_timestamp(#block{ timestamp = Timestamp }, #block{ timestamp = PrevTimestamp }) ->
	MaxNodesClockDeviation = get_max_timestamp_deviation(),
	case Timestamp >= PrevTimestamp - MaxNodesClockDeviation of
		false ->
			false;
		true ->
			CurrentTime = os:system_time(seconds),
			Timestamp =< CurrentTime + MaxNodesClockDeviation
	end.

%% @doc Return the maximum amount by which the previous block's timestamp
%% may exceed the next block's timestamp. The value accounts for the clock
%% drift between nodes and the network join delay.
%% 返回前一个区块的时间戳可能超过下一个区块时间戳的最大值。
%% 这个值考虑了节点间的时钟偏差和加入网络的延迟。
get_max_timestamp_deviation() ->
	?JOIN_CLOCK_TOLERANCE * 2 + ?CLOCK_DRIFT_MAX.

%% @doc Verify that the new block's retarget timestamp is correct.
%% 验证新区块的重定位时间戳是否正确
verify_last_retarget(NewB, OldB) ->
	case ar_retarget:is_retarget_height(NewB#block.height) of
		true ->
			NewB#block.last_retarget == NewB#block.timestamp;
		false ->
			NewB#block.last_retarget == OldB#block.last_retarget
	end.

%% @doc Verify that the new weave size is correctly calculated based on
%% the previous block and the new block's transaction list.
%% 验证新的编织大小(weave size)是否根据前一个区块和新区块的交易列表正确计算
verify_weave_size(NewB, OldB, TXs) ->
	BlockSize = lists:foldl(
		fun(TX, Acc) ->
			Acc + ar_tx:get_weave_size_increase(TX, NewB#block.height)
		end,
		0,
		TXs
	),
	(NewB#block.height < ar_fork:height_2_6() orelse BlockSize == NewB#block.block_size)
			andalso NewB#block.weave_size == OldB#block.weave_size + BlockSize.

%% @doc Verify that the new cumulative difficulty is correctly calculated.
%% 验证新的累积难度是否正确计算
verify_cumulative_diff(NewB, OldB) ->
	NewB#block.cumulative_diff ==
		ar_difficulty:next_cumulative_diff(
			OldB#block.cumulative_diff,
			NewB#block.diff,
			NewB#block.height
		).

%% @doc Verify that the new block's tree root is correctly calculated.
%% 验证新区块树的根是否正确计算
verify_block_hash_list_merkle(NewB, CurrentB) ->
	true = NewB#block.height > ar_fork:height_2_0(),
	NewB#block.hash_list_merkle == ar_unbalanced_merkle:root(CurrentB#block.hash_list_merkle,
			{CurrentB#block.indep_hash, CurrentB#block.weave_size, CurrentB#block.tx_root},
			fun ar_unbalanced_merkle:hash_block_index_entry/1).

%% @doc Compute the new block's tree root based on the previous block.
%% 根据前一个区块计算新区块树的根
compute_hash_list_merkle(B) ->
	ar_unbalanced_merkle:root(
		B#block.hash_list_merkle,
		{B#block.indep_hash, B#block.weave_size, B#block.tx_root},
		fun ar_unbalanced_merkle:hash_block_index_entry/1
	).

%% @doc Compute "h0" - a cryptographic hash used as an entropy source for selecting
%% two recall ranges on the weave unlocked by the given nonce limiter output.
%% 计算"h0" - 一个用作熵源的加密哈希，用于选择由给定的nonce limiter输出解锁的编织上的两个回调范围
compute_h0(B, PrevB) ->
	#block{ nonce_limiter_info = NonceLimiterInfo,
			partition_number = PartitionNumber, reward_addr = MiningAddr,
			packing_difficulty = PackingDifficulty } = B,
	PrevNonceLimiterInfo = PrevB#block.nonce_limiter_info,
	Seed = PrevNonceLimiterInfo#nonce_limiter_info.seed,
	NonceLimiterOutput = NonceLimiterInfo#nonce_limiter_info.output,
	compute_h0(NonceLimiterOutput, PartitionNumber, Seed, MiningAddr, PackingDifficulty).

compute_h0(NonceLimiterOutput, PartitionNumber, Seed, MiningAddr, PackingDifficulty) ->
	compute_h0(NonceLimiterOutput, PartitionNumber, Seed, MiningAddr, PackingDifficulty,
			ar_packing_server:get_packing_state()).

%% @doc Compute "h0" - a cryptographic hash used as an entropy source for selecting
%% two recall ranges on the weave unlocked by the given nonce limiter output.
%% 计算"h0" - 一个用作熵源的加密哈希，用于选择由给定的nonce limiter输出解锁的编织上的两个回调范围
compute_h0(NonceLimiterOutput, PartitionNumber, Seed, MiningAddr, PackingDifficulty,
		PackingState) ->
	Preimage =
		case PackingDifficulty of
			0 ->
				<< NonceLimiterOutput:32/binary,
					PartitionNumber:256, Seed:32/binary, MiningAddr/binary >>;
			_ ->
				<< NonceLimiterOutput:32/binary,
					PartitionNumber:256, Seed:32/binary, MiningAddr/binary,
					PackingDifficulty:8 >>
		end,
	RandomXState = ar_packing_server:get_randomx_state_by_difficulty(
		PackingDifficulty, PackingState),
	ar_mine_randomx:hash(RandomXState, Preimage).

%% @doc Compute "h1" - a cryptographic hash which is either the solution hash not involving
%% the second chunk or a carrier of the first chunk information used in computing
%% the solution hash involving the second chunk.
%% 计算"h1" - 一个加密哈希，它要么是不涉及第二个块的解决方案的哈希，要么是在计算第二个块的解决方案哈希时使用的第一个块信息的载体
compute_h1(H0, Nonce, Chunk) ->
	Preimage = crypto:hash(sha256, << H0:32/binary, Nonce:64, Chunk/binary >>),
	{compute_solution_h(H0, Preimage), Preimage}.

%% @doc Compute "h2" - the solution hash involving the second chunk.
%% 计算"h2" - 涉及第二个块的解决方案的哈希
compute_h2(H1, Chunk, H0) ->
	Preimage = crypto:hash(sha256, << H1:32/binary, Chunk/binary >>),
	{compute_solution_h(H0, Preimage), Preimage}.

%% @doc Compute the solution hash from the preimage and h0.
%% 从预映像和H0计算解决方案哈希
compute_solution_h(H0, Preimage) ->
	crypto:hash(sha256, << H0:32/binary, Preimage/binary >>).

%% @doc Compute the next VDF difficulty.
%% 计算下一个VDF难度
compute_next_vdf_difficulty(PrevB) ->
	Height = PrevB#block.height + 1,
	#nonce_limiter_info{
		vdf_difficulty = VDFDifficulty,
		next_vdf_difficulty = NextVDFDifficulty
	} = PrevB#block.nonce_limiter_info,
	case ar_block_time_history:has_history(Height) of
		true ->
			case (Height rem ?VDF_DIFFICULTY_RETARGET == 0) andalso
					(VDFDifficulty == NextVDFDifficulty) of
				false ->
					NextVDFDifficulty;
				true ->
					case Height < ar_fork:height_2_7_1() of
						true ->
							HistoryPart = lists:nthtail(?VDF_HISTORY_CUT,
									ar_block_time_history:get_history(PrevB)),
							{IntervalTotal, VDFIntervalTotal} =
								lists:foldl(
									fun({BlockInterval, VDFInterval, _ChunkCount}, {Acc1, Acc2}) ->
										{
											Acc1 + BlockInterval,
											Acc2 + VDFInterval
										}
									end,
									{0, 0},
									HistoryPart
								),
							NewVDFDifficulty =
								(VDFIntervalTotal * VDFDifficulty) div IntervalTotal,
							?LOG_DEBUG([{event, vdf_difficulty_retarget},
									{height, Height},
									{old_vdf_difficulty, VDFDifficulty},
									{new_vdf_difficulty, NewVDFDifficulty},
									{interval_total, IntervalTotal},
									{vdf_interval_total, VDFIntervalTotal}]),
							NewVDFDifficulty;
						false ->
							HistoryPartCut1 = lists:nthtail(?VDF_HISTORY_CUT,
								ar_block_time_history:get_history(PrevB)),
							HistoryPart = lists:sublist(HistoryPartCut1, ?VDF_DIFFICULTY_RETARGET),
							{IntervalTotal, VDFIntervalTotal} =
								lists:foldl(
									fun({BlockInterval, VDFInterval, _ChunkCount}, {Acc1, Acc2}) ->
										{
											Acc1 + BlockInterval,
											Acc2 + VDFInterval
										}
									end,
									{0, 0},
									HistoryPart
								),
							NewVDFDifficulty =
								(VDFIntervalTotal * VDFDifficulty) div IntervalTotal,
							EMAVDFDifficulty = (9*VDFDifficulty + NewVDFDifficulty) div 10,
							?LOG_DEBUG([{event, vdf_difficulty_retarget},
									{height, Height},
									{old_vdf_difficulty, VDFDifficulty},
									{new_vdf_difficulty, NewVDFDifficulty},
									{ema_vdf_difficulty, EMAVDFDifficulty},
									{interval_total, IntervalTotal},
									{vdf_interval_total, VDFIntervalTotal}]),
							EMAVDFDifficulty
					end
			end;
		false ->
			?VDF_DIFFICULTY
	end.

%% @doc Validate the proof size.
%% 验证证明大小
validate_proof_size(PoA) ->
	byte_size(PoA#poa.tx_path) =< ?MAX_TX_PATH_SIZE andalso
			byte_size(PoA#poa.data_path) =< ?MAX_DATA_PATH_SIZE andalso
			byte_size(PoA#poa.chunk) =< ?DATA_CHUNK_SIZE andalso
			byte_size(PoA#poa.unpacked_chunk) =< ?DATA_CHUNK_SIZE.

%% @doc Compute the block identifier (also known as "independent hash").
%% 计算区块标识符(也称为"独立哈希")
indep_hash(B) ->
	case B#block.height >= ar_fork:height_2_6() of
		true ->
			H = ar_block:generate_signed_hash(B),
			indep_hash2(H, B#block.signature);
		false ->
			BDS = ar_block:generate_block_data_segment(B),
			indep_hash(BDS, B)
	end.

%% @doc Compute the hash signed by the block producer.
%% 计算区块生产者签名的哈希
generate_signed_hash(#block{ previous_block = PrevH, timestamp = TS,
		nonce = Nonce, height = Height, diff = Diff, cumulative_diff = CDiff,
		last_retarget = LastRetarget, hash = Hash, block_size = BlockSize,
		weave_size = WeaveSize, tx_root = TXRoot, wallet_list = WalletList,
		hash_list_merkle = HashListMerkle, reward_pool = RewardPool,
		packing_2_5_threshold = Packing_2_5_Threshold, reward_addr = Addr,
		reward_key = RewardKey, strict_data_split_threshold = StrictChunkThreshold,
		usd_to_ar_rate = {RateDividend, RateDivisor},
		scheduled_usd_to_ar_rate = {ScheduledRateDividend, ScheduledRateDivisor},
		tags = Tags, txs = TXs,
		reward = Reward, hash_preimage = HashPreimage, recall_byte = RecallByte,
		partition_number = PartitionNumber, recall_byte2 = RecallByte2,
		nonce_limiter_info = NonceLimiterInfo,
		previous_solution_hash = PreviousSolutionHash,
		price_per_gib_minute = PricePerGiBMinute,
		scheduled_price_per_gib_minute = ScheduledPricePerGiBMinute,
		reward_history_hash = RewardHistoryHash,
		block_time_history_hash = BlockTimeHistoryHash, debt_supply = DebtSupply,
		kryder_plus_rate_multiplier = KryderPlusRateMultiplier,
		kryder_plus_rate_multiplier_latch = KryderPlusRateMultiplierLatch,
		denomination = Denomination, redenomination_height = RedenominationHeight,
		double_signing_proof = DoubleSigningProof, previous_cumulative_diff = PrevCDiff,
		merkle_rebase_support_threshold = RebaseThreshold,
		poa = #poa{ data_path = DataPath, tx_path = TXPath },
		poa2 = #poa{ data_path = DataPath2, tx_path = TXPath2 },
		chunk_hash = ChunkHash, chunk2_hash = Chunk2Hash,
		packing_difficulty = PackingDifficulty,
		unpacked_chunk_hash = UnpackedChunkHash,
		unpacked_chunk2_hash = UnpackedChunk2Hash }) ->
	GetTXID = fun(TXID) when is_binary(TXID) -> TXID; (TX) -> TX#tx.id end,
	Nonce2 = binary:encode_unsigned(Nonce),
	%% The only block which may have an unclaimed reward address is
	%% the genesis block of a new weave.
	%% 唯一可能有未认领奖励地址的区块是新编织的创世区块
	Addr2 = case Addr of unclaimed -> <<>>; _ -> Addr end,
	RewardKey2 = case RewardKey of undefined -> undefined; {_Type, Pub} -> Pub end,
	#nonce_limiter_info{ output = Output, global_step_number = N, seed = Seed,
			next_seed = NextSeed, partition_upper_bound = PartitionUpperBound,
			next_partition_upper_bound = NextPartitionUpperBound,
			steps = Steps, prev_output = PrevOutput,
			last_step_checkpoints = LastStepCheckpoints,
			vdf_difficulty = VDFDifficulty,
			next_vdf_difficulty = NextVDFDifficulty } = NonceLimiterInfo,
	{RebaseThresholdBin, DataPathBin, TXPathBin, DataPath2Bin, TXPath2Bin,
			ChunkHashBin, Chunk2HashBin, BlockTimeHistoryHashBin,
			VDFDifficultyBin, NextVDFDifficultyBin} =
		case Height >= ar_fork:height_2_7() of
			true ->
				{encode_int(RebaseThreshold, 16), ar_serialize:encode_bin(DataPath, 24),
						ar_serialize:encode_bin(TXPath, 24),
						ar_serialize:encode_bin(DataPath2, 24),
						ar_serialize:encode_bin(TXPath2, 24),
						<< ChunkHash:32/binary >>,
						ar_serialize:encode_bin(Chunk2Hash, 8),
						<< BlockTimeHistoryHash:32/binary >>,
						ar_serialize:encode_int(VDFDifficulty, 8),
						ar_serialize:encode_int(NextVDFDifficulty, 8)};
			false ->
				{<<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>}
		end,
	{PackingDifficultyBin, UnpackedChunkHashBin, UnpackedChunk2HashBin} =
		case Height >= ar_fork:height_2_8() of
			true ->
				{<< PackingDifficulty:8 >>,
						ar_serialize:encode_bin(UnpackedChunkHash, 8),
						ar_serialize:encode_bin(UnpackedChunk2Hash, 8)};
			false ->
				{<<>>, <<>>, <<>>}
		end,
	%% The elements must be either fixed size or size-delimited (ar_serialize:encode_* functions).
	%% 元素必须是固定大小的或者由大小分隔符分隔(ar_serialize:encode_*函数)
	Segment = << (encode_bin(PrevH, 8))/binary, (encode_int(TS, 8))/binary,
			(encode_bin(Nonce2, 16))/binary, (encode_int(Height, 8))/binary,
			(encode_int(Diff, 16))/binary, (encode_int(CDiff, 16))/binary,
			(encode_int(LastRetarget, 8))/binary, (encode_bin(Hash, 8))/binary,
			(encode_int(BlockSize, 16))/binary, (encode_int(WeaveSize, 16))/binary,
			(encode_bin(Addr2, 8))/binary, (encode_bin(TXRoot, 8))/binary,
			(encode_bin(WalletList, 8))/binary,
			(encode_bin(HashListMerkle, 8))/binary, (encode_int(RewardPool, 8))/binary,
			(encode_int(Packing_2_5_Threshold, 8))/binary,
			(encode_int(StrictChunkThreshold, 8))/binary,
					(encode_int(RateDividend, 8))/binary,
			(encode_int(RateDivisor, 8))/binary,
					(encode_int(ScheduledRateDividend, 8))/binary,
			(encode_int(ScheduledRateDivisor, 8))/binary,
			(encode_bin_list(Tags, 16, 16))/binary,
			(encode_bin_list([GetTXID(TX) || TX <- TXs], 16, 8))/binary,
			(encode_int(Reward, 8))/binary,
			(encode_int(RecallByte, 16))/binary, (encode_bin(HashPreimage, 8))/binary,
			(encode_int(RecallByte2, 16))/binary, (encode_bin(RewardKey2, 16))/binary,
			(encode_int(PartitionNumber, 8))/binary, Output:32/binary, N:64,
			Seed:48/binary, NextSeed:48/binary, PartitionUpperBound:256,
			NextPartitionUpperBound:256, (encode_bin(PrevOutput, 8))/binary,
			(length(Steps)):16, (iolist_to_binary(Steps))/binary,
			(length(LastStepCheckpoints)):16, (iolist_to_binary(LastStepCheckpoints))/binary,
			(encode_bin(PreviousSolutionHash, 8))/binary,
			(encode_int(PricePerGiBMinute, 8))/binary,
			(encode_int(ScheduledPricePerGiBMinute, 8))/binary,
			RewardHistoryHash:32/binary, (encode_int(DebtSupply, 8))/binary,
			KryderPlusRateMultiplier:24, KryderPlusRateMultiplierLatch:8, Denomination:24,
			(encode_int(RedenominationHeight, 8))/binary,
			(ar_serialize:encode_double_signing_proof(DoubleSigningProof))/binary,
			(encode_int(PrevCDiff, 16))/binary, RebaseThresholdBin/binary,
			DataPathBin/binary, TXPathBin/binary, DataPath2Bin/binary, TXPath2Bin/binary,
			ChunkHashBin/binary, Chunk2HashBin/binary, BlockTimeHistoryHashBin/binary,
			VDFDifficultyBin/binary, NextVDFDifficultyBin/binary,
			PackingDifficultyBin/binary, UnpackedChunkHashBin/binary,
			UnpackedChunk2HashBin/binary >>,
	crypto:hash(sha256, Segment).

%% @doc Compute the block identifier from the signed hash and the block signature.
%% 从签名哈希和区块签名计算区块标识符
indep_hash2(SignedH, Signature) ->
		crypto:hash(sha384, << SignedH:32/binary, Signature/binary >>).

%% @doc Compute the block identifier for pre-2.6 blocks.
%% 计算2.6版本之前区块的标识符
indep_hash(BDS, B) ->
	case B#block.height >= ar_fork:height_2_4() of
		true ->
			ar_deep_hash:hash([BDS, B#block.hash, B#block.nonce,
					ar_block:poa_to_list(B#block.poa)]);
		false ->
			ar_deep_hash:hash([BDS, B#block.hash, B#block.nonce])
	end.

%% @doc Verify the block signature.
%% 验证区块签名
verify_signature(BlockPreimage, PrevCDiff,
		#block{ signature = Signature, reward_key = {?DEFAULT_KEY_TYPE, Pub} = RewardKey,
				reward_addr = RewardAddr, previous_solution_hash = PrevSolutionH,
				cumulative_diff = CDiff })
		when byte_size(Signature) == 512, byte_size(Pub) == 512 ->
	SignaturePreimage = << (ar_serialize:encode_int(CDiff, 16))/binary,
			(ar_serialize:encode_int(PrevCDiff, 16))/binary, PrevSolutionH/binary,
			BlockPreimage/binary >>,
	ar_wallet:to_address(RewardKey) == RewardAddr andalso
			ar_wallet:verify(RewardKey, SignaturePreimage, Signature);
verify_signature(_BlockPreimage, _PrevCDiff, _B) ->
	false.

%% @doc Generate the block data segment - a binary used for computing the block hash.
%% 生成区块数据段 - 用于计算区块哈希的二进制数据
generate_block_data_segment(B) ->
	generate_block_data_segment(B, B#block.txs).

%% @doc Generate the block data segment with the given transaction list.
%% 使用给定的交易列表生成区块数据段
generate_block_data_segment(B, TXs) ->
	%% The block data segment consists of the base segment and the transaction identifiers.
	%% 区块数据段由基础段和交易标识符组成
	BaseSegment = generate_block_data_segment_base(B),
	TXIDs = [TX#tx.id || TX <- TXs],
	<< BaseSegment/binary, (ar_deep_hash:hash(TXIDs))/binary >>.

%% @doc Generate the base segment of the block data segment.
%% 生成区块数据段的基础段
generate_block_data_segment_base(B) ->
	%% The base segment consists of various block fields concatenated together.
	%% 基础段由多个区块字段连接在一起组成
	Timestamp = integer_to_binary(B#block.timestamp),
	LastRetarget = integer_to_binary(B#block.last_retarget),
	Diff = integer_to_binary(B#block.diff),
	Height = integer_to_binary(B#block.height),
	<< B#block.previous_block/binary, Timestamp/binary, LastRetarget/binary,
			Diff/binary, Height/binary, B#block.hash/binary >>.

%% @doc Get the recall range for the given block and recall byte.
%% 获取给定区块和回调字节的回调范围
get_recall_range(B, RecallByte, PrevB) ->
	%% The recall range is determined by various parameters including the recall byte,
	%% weave size, and chunk size.
	%% 回调范围由多个参数决定，包括回调字节、编织大小和块大小
	WeaveSize = PrevB#block.weave_size,
	ChunkSize = get_sub_chunk_size(B#block.height),
	Start = RecallByte - (RecallByte rem ChunkSize),
	case Start + ChunkSize > WeaveSize of
		true ->
			{Start, WeaveSize - Start};
		false ->
			{Start, ChunkSize}
	end.

%% @doc Verify that the transaction root matches the transaction list.
%% 验证交易根是否与交易列表匹配
verify_tx_root(B) ->
	B#block.tx_root == generate_tx_root_for_block(B).

%% @doc Hash the wallet list.
%% 对钱包列表进行哈希
hash_wallet_list(WL) ->
	ar_deep_hash:hash(WL).

%% @doc Generate the hash list for the given block.
%% 为给定区块生成哈希列表
generate_hash_list_for_block(B, TXs) ->
	[{ar_weave:indep_hash(TX), ar_weave:tx_data_size(TX)} || TX <- TXs].

%% @doc Generate the transaction root for the block.
%% 为区块生成交易根
generate_tx_root_for_block(B) ->
	generate_tx_root_for_block(B, B#block.txs).

%% @doc Generate the transaction root for the block with the given transaction list.
%% 使用给定的交易列表为区块生成交易根
generate_tx_root_for_block(B, TXs) ->
	SizeTaggedTXs = generate_size_tagged_list_from_txs(TXs, B#block.height),
	generate_tx_tree(SizeTaggedTXs).

%% @doc Generate a list of size-tagged transactions.
%% 生成带大小标签的交易列表
generate_size_tagged_list_from_txs(TXs, Height) ->
	[{ar_weave:tx_data_size(TX), TX} || TX <- TXs].

%% @doc Generate a transaction tree from a list of transactions.
%% 从交易列表生成交易树
generate_tx_tree(TXs) ->
	generate_tx_tree(TXs, fun ar_weave:hash/1).

%% @doc Generate a transaction tree using the given hash function.
%% 使用给定的哈希函数生成交易树
generate_tx_tree(TXs, HashFun) ->
	ar_merkle:generate_tree(TXs, HashFun).

%% @doc Test the performance of the wallet list implementation.
%% 测试钱包列表实现的性能
test_wallet_list_performance() ->
	test_wallet_list_performance(10).

%% @doc Test the performance with the given number of wallets.
%% 使用给定数量的钱包测试性能
test_wallet_list_performance(NumWallets) ->
	test_wallet_list_performance(NumWallets, 10).

%% @doc Test the performance with the given number of wallets and iterations.
%% 使用给定数量的钱包和迭代次数测试性能
test_wallet_list_performance(NumWallets, NumIterations) ->
	test_wallet_list_performance(NumWallets, NumIterations, fun() -> ok end).

%% @doc Test the performance with the given parameters and callback function.
%% 使用给定参数和回调函数测试性能
test_wallet_list_performance(NumWallets, NumIterations, Callback) ->
	ar_wallet:test_wallet_list_performance(NumWallets, NumIterations, Callback).

%% @doc Convert a proof of access record to a list.
%% 将访问证明记录转换为列表
poa_to_list(undefined) ->
	[];
poa_to_list(#poa{ chunk = Chunk, data_path = DataPath, tx_path = TXPath }) ->
	[Chunk, DataPath, TXPath].

%% @doc Shift the packing 2.5 threshold for the given block.
%% 为给定区块移动打包2.5阈值
shift_packing_2_5_threshold(B) ->
	case B#block.height >= ar_fork:height_2_5() of
		true ->
			B#block.packing_2_5_threshold;
		false ->
			0
	end.

%% @doc Get the packing threshold for the given block.
%% 获取给定区块的打包阈值
get_packing_threshold(B, PrevB) ->
	case B#block.height >= ar_fork:height_2_5() of
		true ->
			PrevB#block.packing_2_5_threshold;
		false ->
			0
	end.

%% @doc Get the VDF step number for the given block.
%% 获取给定区块的VDF步骤数
vdf_step_number(B) ->
	case B#block.height >= ar_fork:height_2_5() of
		true ->
			B#block.nonce_limiter_info#nonce_limiter_info.global_step_number;
		false ->
			0
	end.

%% @doc Get the packing information for the given block.
%% 获取给定区块的打包信息
get_packing(B, PrevB) ->
	case B#block.height >= ar_fork:height_2_5() of
		true ->
			{B#block.packing_2_5_threshold, PrevB#block.packing_2_5_threshold};
		false ->
			{0, 0}
	end.

%% @doc Validate the packing difficulty for the given block.
%% 验证给定区块的打包难度
validate_packing_difficulty(B, PrevB) ->
	validate_packing_difficulty(B#block.packing_difficulty).

%% @doc Validate the given packing difficulty.
%% 验证给定的打包难度
validate_packing_difficulty(PackingDifficulty) ->
	PackingDifficulty >= 0 andalso PackingDifficulty =< 255.

%% @doc Get the maximum nonce for the given block.
%% 获取给定区块的最大随机数
get_max_nonce(B) ->
	case B#block.height >= ar_fork:height_2_5() of
		true ->
			?MAX_NONCE;
		false ->
			?MAX_NONCE_2_4
	end.

%% @doc Get the recall range size for the given block.
%% 获取给定区块的回调范围大小
get_recall_range_size(B) ->
	case B#block.height >= ar_fork:height_2_5() of
		true ->
			?RECALL_RANGE_SIZE_2_5;
		false ->
			?RECALL_RANGE_SIZE_2_4
	end.

%% @doc Get the recall byte for the given block and parameters.
%% 获取给定区块和参数的回调字节
get_recall_byte(B, RecallByte, PrevB) ->
	case B#block.height >= ar_fork:height_2_5() of
		true ->
			RecallByte;
		false ->
			RecallByte rem PrevB#block.weave_size
	end.

%% @doc Get the sub-chunk size for the given block height.
%% 获取给定区块高度的子块大小
get_sub_chunk_size(Height) ->
	case Height >= ar_fork:height_2_5() of
		true ->
			?DATA_CHUNK_SIZE;
		false ->
			?DATA_CHUNK_SIZE_2_4
	end.

%% @doc Get the number of nonces per chunk for the given block height.
%% 获取给定区块高度的每块随机数数量
get_nonces_per_chunk(Height) ->
	case Height >= ar_fork:height_2_5() of
		true ->
			?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT;
		false ->
			?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT_2_4
	end.

%% @doc Get the number of nonces per recall range for the given block height.
%% 获取给定区块高度的每个回调范围的随机数数量
get_nonces_per_recall_range(Height) ->
	case Height >= ar_fork:height_2_5() of
		true ->
			?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT * ?RECALL_RANGE_SIZE_2_5;
		false ->
			?NONCE_LIMITER_MAX_CHECKPOINTS_COUNT_2_4 * ?RECALL_RANGE_SIZE_2_4
	end.

%% @doc Get the sub-chunk index for the given parameters.
%% 获取给定参数的子块索引
get_sub_chunk_index(Offset, Height) ->
	SubChunkSize = get_sub_chunk_size(Height),
	Offset div SubChunkSize.