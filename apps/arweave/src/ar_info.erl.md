### 文件功能和目的
该文件 `ar_info.erl` 的主要功能是为 `/info` 和 `/recent` 两个端点收集和提供数据。这些端点通常用于提供区块链网络的当前状态信息和最近的活动记录，如区块和分叉信息。

### 主要逻辑
1. **get_info/0**: 
   - 获取当前区块的哈希值、高度、区块数量、节点状态延迟等信息。
   - 从 `ar_node` 模块获取当前区块哈希和高度，并计算获取这些信息所需的时间。
   - 从 `ets` 表中获取已同步的区块数量。
   - 从 `prometheus_gauge` 中获取当前的节点数量。
   - 获取节点工作进程的消息队列长度。
   - 返回一个包含上述信息的映射（map）。

2. **get_recent/0**:
   - 获取最近的区块和分叉信息。
   - 调用 `get_recent_blocks/0` 获取最近的区块信息。
   - 调用 `get_recent_forks/0` 获取最近的分叉信息。
   - 返回一个包含上述信息的映射（map）。

3. **get_recent_blocks/0**:
   - 获取最近的区块信息，并按逆时间顺序排列。
   - 从 `ar_node` 模块获取区块锚点，并截取最近的 `?CHECKPOINT_DEPTH` 个区块。
   - 遍历这些区块，获取每个区块的详细信息，并将其添加到列表中。
   - 最终将列表反转，以确保最新的区块在最前面。

4. **get_recent_forks/0**:
   - 获取最近的分叉信息，并按逆时间顺序排列。
   - 计算分叉信息的截止时间。
   - 从 `ar_chain_stats` 模块获取分叉信息。
   - 遍历这些分叉信息，获取每个分叉的详细信息，并将其添加到列表中。
   - 返回最近的分叉信息列表。

5. **get_block_timestamp/2**:
   - 获取区块的接收时间戳。
   - 如果区块的接收时间戳未定义或深度小于 `?RECENT_BLOCKS_WITHOUT_TIMESTAMP`，则返回 `"pending"`。
   - 否则，将时间戳转换为秒并返回。

### 关键点
- **时间测量**: 使用 `timer:tc/1` 函数测量获取当前区块哈希和高度所需的时间，并计算节点状态延迟。
- **ETS 表**: 使用 `ets:lookup/2` 从 `ar_header_sync` 表中获取已同步的区块数量。
- **Prometheus 监控**: 使用 `prometheus_gauge:value/1` 获取当前的节点数量。
- **消息队列长度**: 使用 `erlang:process_info/2` 获取节点工作进程的消息队列长度。
- **区块和分叉信息**: 通过 `ar_node` 和 `ar_chain_stats` 模块获取最近的区块和分叉信息。

### 潜在的坑
- **性能问题**: 频繁调用 `timer:tc/1` 和 `erlang:process_info/2` 可能会影响性能，尤其是在高并发情况下。
- **ETS 表依赖**: 依赖 `ets:lookup/2` 获取数据，如果 `ar_header_sync` 表不存在或数据不完整，可能会导致错误。
- **Prometheus 依赖**: 依赖 `prometheus_gauge:value/1` 获取节点数量，如果 Prometheus 监控未正确配置，可能会返回错误值。
- **时间戳处理**: `get_block_timestamp/2` 函数中的时间戳处理逻辑可能需要根据具体业务需求进行调整。

### 隐藏信息
- **区块锚点**: `ar_node:get_block_anchors/0` 返回的区块锚点是按逆时间顺序排列的，但在处理过程中进行了多次反转，最终结果仍然是逆时间顺序。
- **分叉信息**: `ar_chain_stats:get_forks/1` 返回的分叉信息可能包含多个分叉，代码中只处理了最近的 `?RECENT_FORKS_LENGTH` 个分叉。

### 假设前提
- **模块依赖**: 假设 `ar_node`、`ar_block_cache`、`ar_chain_stats` 和 `ar_util` 模块已经正确实现并可用。
- **ETS 表存在**: 假设 `ar_header_sync` 表已经存在，并且包含 `synced_blocks` 键。
- **Prometheus 配置**: 假设 Prometheus 监控已经正确配置，并且 `arweave_peer_count` 指标可用。

### 历史背景
该代码文件可能是 Arweave 区块链项目的一部分，用于提供节点状态和最近活动信息的 API 端点。Arweave 是一个去中心化的存储网络，旨在永久保存数据。

### 数学或算法原理
- **时间测量**: 使用 `timer:tc/1` 函数测量函数执行时间，这是一种常见的性能测量方法。
- **列表反转**: 在 `get_recent_blocks/0` 中，通过多次反转列表来确保最新的区块在最前面，这是一种常见的列表处理技巧。