%%====================================================================
%% 文件功能描述：
%% 本模块提供Base32编码功能，主要用于将二进制数据编码为小写的、不带填充的RFC 4648 base32字母表格式。
%% 该实现非常强烈地借鉴了OTP的base64源代码。
%% 
%% 主要特点：
%% 1. 只支持编码功能,不支持解码
%% 2. 使用标准的RFC 4648 base32字母表
%% 3. 输出结果为小写字母,不带填充字符
%% 4. 高效的位操作实现
%%====================================================================

%% @doc 这个模块的实现非常强烈地借鉴了OTP的base64源代码
%% (中文翻译: 本模块参考了OTP标准库中base64模块的实现方式)
%% 参考源码: https://github.com/erlang/otp/blob/93ec8bb2dbba9456395a54551fe9f1e0f86184b1/lib/stdlib/src/base64.erl#L66-L80
-module(ar_base32).

%% 导出encode/1函数作为公共API
-export([encode/1]).

%%%===================================================================
%%% 公共接口
%%%===================================================================

%% @doc 将二进制数据编码为小写的、不带填充的RFC 4648 base32字母表格式
%% 参数Bin: 输入的二进制数据
%% 返回: Base32编码后的二进制数据
encode(Bin) when is_binary(Bin) ->
    encode_binary(Bin, <<>>).

%%%===================================================================
%%% 私有函数
%%%===================================================================

%% 处理空二进制数据的情况
encode_binary(<<>>, A) ->
    A;

%% 处理剩余1个字节的情况
encode_binary(<<B1:8>>, A) ->
    %% bsr: 位右移操作符, band: 位与操作符, bsl: 位左移操作符
    %% 将1个字节拆分成5位一组进行编码
    <<A/bits, (b32e(B1 bsr 3)):8, (b32e((B1 band 7) bsl 2)):8>>;

%% 处理剩余2个字节的情况
encode_binary(<<B1:8, B2:8>>, A) ->
    %% bor: 位或操作符
    %% 将2个字节组合后按5位拆分进行编码
    BB = (B1 bsl 8) bor B2,
    <<A/bits,
        (b32e(BB bsr 11)):8,
        (b32e((BB bsr 6) band 31)):8,
        (b32e((BB bsr 1) band 31)):8,
        (b32e((BB bsl 4) band 31)):8>>;

%% 处理剩余3个字节的情况
encode_binary(<<B1:8, B2:8, B3:8>>, A) ->
    BB = (B1 bsl 16) bor (B2 bsl 8) bor B3,
    <<A/bits,
        (b32e(BB bsr 19)):8,
        (b32e((BB bsr 14) band 31)):8,
        (b32e((BB bsr 9) band 31)):8,
        (b32e((BB bsr 4) band 31)):8,
        (b32e((BB bsl 1) band 31)):8>>;

%% 处理剩余4个字节的情况
encode_binary(<<B1:8, B2:8, B3:8, B4:8>>, A) ->
    BB = (B1 bsl 24) bor (B2 bsl 16) bor (B3 bsl 8) bor B4,
    <<A/bits,
        (b32e(BB bsr 27)):8,
        (b32e((BB bsr 22) band 31)):8,
        (b32e((BB bsr 17) band 31)):8,
        (b32e((BB bsr 12) band 31)):8,
        (b32e((BB bsr 7) band 31)):8,
        (b32e((BB bsr 2) band 31)):8,
        (b32e((BB bsl 3) band 31)):8>>;

%% 处理5个及以上字节的情况
encode_binary(<<B1:8, B2:8, B3:8, B4:8, B5:8, Ls/bits>>, A) ->
    BB = (B1 bsl 32) bor (B2 bsl 24) bor (B3 bsl 16) bor (B4 bsl 8) bor B5,
    encode_binary(
        Ls,
        <<A/bits,
            (b32e(BB bsr 35)):8,
            (b32e((BB bsr 30) band 31)):8,
            (b32e((BB bsr 25) band 31)):8,
            (b32e((BB bsr 20) band 31)):8,
            (b32e((BB bsr 15) band 31)):8,
            (b32e((BB bsr 10) band 31)):8,
            (b32e((BB bsr 5) band 31)):8,
            (b32e(BB band 31)):8>>
    ).

%% 编译指令:将b32e/1函数内联展开,提高性能
-compile({inline, [{b32e, 1}]}).

%% Base32字母表映射函数
%% 将5位二进制数(0-31)映射到对应的Base32字符
%% 使用element从元组中快速查找对应字符
b32e(X) ->
    element(X+1, {
        $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
        $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
        $2, $3, $4, $5, $6, $7, $8, $9
    }).
