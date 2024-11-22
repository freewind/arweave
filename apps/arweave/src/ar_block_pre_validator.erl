%%====================================================================
%% 文件功能描述：
%% 本模块实现了区块的预验证功能，是Arweave区块验证的第一道防线。
%% 主要用于防止DDoS攻击，通过快速验证来过滤掉明显无效的区块。
%%
%% 关键概念：
%% 1. 优先级队列 - 使用gb_sets实现，按区块高度和节点评分排序
%% 2. 节流控制 - 限制单个IP和解决方案哈希的验证频率
%% 3. 分阶段验证 - 从最快的检查开始，逐步深入验证
%%
%% 重要说明：
%% - 使用gen_server行为模式
%% - 队列大小限制为200MB
%% - 支持异步验证
%%====================================================================

%% 声明这是一个gen_server行为模式的模块
-module(ar_block_pre_validator).

%% 使用gen_server行为模式，这是Erlang/OTP的标准服务器行为模式
-behaviour(gen_server).

%% 导出公共API函数
-export([start_link/0, pre_validate/3]).

%% 导出gen_server回调函数
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% 引入必要的头文件，包含各种宏定义和记录定义
-include_lib("arweave/include/ar.hrl").
-include_lib("arweave/include/ar_config.hrl").
-include_lib("arweave/include/ar_consensus.hrl").

%% 定义服务器状态记录
-record(state, {
    %% 存储验证请求的优先级队列(priority queue)
    pqueue = gb_sets:new(),
    %% 优先级队列的总大小(字节)
    size = 0,
    %% IP地址到最后一个区块时间戳的映射，用于IP节流
    ip_timestamps = #{},
    throttle_by_ip_interval,
    %% 解决方案哈希到最后一个区块时间戳的映射，用于解决方案节流
    hash_timestamps = #{},
    throttle_by_solution_interval
}).

%% 预验证队列的最大大小(200MB)
-define(MAX_PRE_VALIDATION_QUEUE_SIZE, (200 * 1024 * 1024)).
