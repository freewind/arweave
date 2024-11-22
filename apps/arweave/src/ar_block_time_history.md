# ar_block_time_history 模块详解

## 模块概述

本模块实现了区块时间历史记录功能，用于追踪和分析区块生成时间间隔。这对于调整挖矿难度非常重要，通过分析历史区块时间来保持网络的稳定性。

## 核心概念

### 1. 区块时间
- 定义：相邻两个区块的时间戳差值
- 用途：反映网络的挖矿速度
- 重要性：用于难度调整算法

### 2. 时间窗口
- 概念：用于计算的历史区块数量
- 作用：平滑短期波动
- 配置：可调整窗口大小

### 3. 统计方法
- 平均值：反映整体趋势
- 中位数：避免极端值影响
- 时间周期：计算总体时间跨度

## 数据结构

### ETS表设计
- 表名：block_time_history
- 类型：ordered_set
- 键：区块高度
- 值：{区块哈希, 时间戳, 前区块哈希}

## 主要函数

### 初始化函数
- init/0：创建空的时间历史记录
- init/1：使用已有区块列表初始化

### 数据操作
- add/4：添加新的时间记录
- get_timestamps_for_range/2：获取时间戳列表

### 统计计算
- get_average_block_time/1：计算平均时间
- get_median_block_time/1：计算中位数时间
- get_block_period_for_range/3：计算时间周期

## 性能考虑

### 1. 数据存储
- 使用ETS表提高访问效率
- ordered_set保证数据有序
- public访问权限支持并发

### 2. 计算优化
- 批量数据获取
- 高效的统计算法
- 缓存中间结果

## 使用示例

    # 初始化时间历史
    ar_block_time_history:init().
    
    # 添加区块时间记录
    ar_block_time_history:add(Hash, Height, Timestamp, PrevHash).
    
    # 获取最近10个区块的平均时间
    AverageTime = ar_block_time_history:get_average_block_time(10). 