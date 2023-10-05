%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: int_emu.m.
% Main author: wangp.
%
% Emulate `int' operations for a given number of bits per int. These predicates
% succeed only if the result is defined for the given arguments, and the result
% can be represented by the `int' type of the host compiler.
%
%---------------------------------------------------------------------------%

:- module parse_tree.int_emu.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.prog_data.

:- import_module integer.

%---------------------------------------------------------------------------%

:- type op_type
    --->    op_int(op_num_bits)
    ;       op_uint(op_num_bits).

:- type op_num_bits
    --->    word_bits(int)
    ;       bits_8
    ;       bits_16
    ;       bits_32
    ;       bits_64.

    % Return the number of bits per word (and therefore per int and per uint)
    % for the selected compilation target, optionally wrapped in word_bits().
    %
:- func target_word_bits(globals) = int.
:- func target_op_type(globals) = op_num_bits.

%---------------------------------------------------------------------------%

:- pred emu_plus(op_type::in, integer::in, integer::in,
    some_int_const::out) is semidet.
:- pred emu_minus(op_type::in, integer::in, integer::in,
    some_int_const::out) is semidet.
:- pred emu_times(op_type::in, integer::in, integer::in,
    some_int_const::out) is semidet.
:- pred emu_quotient(op_type::in, integer::in, integer::in,
    some_int_const::out) is semidet.
:- pred emu_mod(op_type::in, integer::in, integer::in,
    some_int_const::out) is semidet.
:- pred emu_rem(op_type::in, integer::in, integer::in,
    some_int_const::out) is semidet.

%---------------------------------------------------------------------------%

:- pred emu_left_shift(op_type::in, integer::in, int::in,
    some_int_const::out) is semidet.
:- pred emu_right_shift(op_type::in, integer::in, int::in,
    some_int_const::out) is semidet.

%---------------------------------------------------------------------------%

:- pred emu_int_floor_to_multiple_of_bits_per_int(op_type::in,
    some_int_const::in, some_int_const::out) is semidet.

:- pred emu_int_quot_bits_per_int(op_type::in, some_int_const::in,
    some_int_const::out) is semidet.

:- pred emu_int_times_bits_per_int(op_type::in, some_int_const::in,
    some_int_const::out) is semidet.

:- pred emu_int_rem_bits_per_int(op_type::in, some_int_const::in,
    some_int_const::out) is semidet.

%---------------------------------------------------------------------------%

:- pred emu_not(op_type::in, cons_id::in,
    some_int_const::out) is semidet.
:- pred emu_and(op_type::in, cons_id::in, cons_id::in,
    some_int_const::out) is semidet.
:- pred emu_or(op_type::in, cons_id::in, cons_id::in,
    some_int_const::out) is semidet.
:- pred emu_xor(op_type::in, cons_id::in, cons_id::in,
    some_int_const::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.

:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module int8.
:- import_module uint.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.
:- import_module uint8.

%---------------------------------------------------------------------------%

target_word_bits(Globals) = WordBits :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.lookup_int_option(Globals, bits_per_word, WordBits)
    ;
        ( Target = target_csharp
        ; Target = target_java
        ),
        WordBits = 32
    ).

target_op_type(Globals) = word_bits(WordBits) :-
    WordBits = target_word_bits(Globals).

%---------------------------------------------------------------------------%

emu_plus(WordBits, X, Y, Z) :-
    to_some_int_const_if_in_in_range(WordBits, X + Y, Z).

emu_minus(WordBits, X, Y, Z) :-
    to_some_int_const_if_in_in_range(WordBits, X - Y, Z).

emu_times(WordBits, X, Y, Z) :-
    to_some_int_const_if_in_in_range(WordBits, X * Y, Z).

emu_quotient(WordBits, X, Y, Z) :-
    not is_zero(Y),
    to_some_int_const_if_in_in_range(WordBits, X // Y, Z).

emu_mod(WordBits, X, Y, Z) :-
    not is_zero(Y),
    to_some_int_const_if_in_in_range(WordBits, X mod Y, Z).

emu_rem(WordBits, X, Y, Z) :-
    not is_zero(Y),
    to_some_int_const_if_in_in_range(WordBits, X rem Y, Z).

%---------------------------------------------------------------------------%

emu_left_shift(OpType, X, Y, Z) :-
    Y >= 0,
    Y < get_op_type_bits(OpType),
    to_some_int_const_if_in_in_range(OpType, X << Y, Z).

emu_right_shift(OpType, X, Y, Z) :-
    Y >= 0,
    Y < get_op_type_bits(OpType),
    to_some_int_const_if_in_in_range(OpType, X >> Y, Z).

%---------------------------------------------------------------------------%

emu_int_floor_to_multiple_of_bits_per_int(OpType, ConstX, FloorConst) :-
    % This operation is defined only for word-size signed integers.
    OpType = op_int(word_bits(BitsPerInt)),
    ConstX = int_const(IntX),
    Trunc = integer(IntX) // integer(BitsPerInt),
    Floor0 = Trunc * integer(BitsPerInt),
    ( if Floor0 > integer(IntX) then
        Floor = Floor0 - integer(BitsPerInt)
    else
        Floor = Floor0
    ),
    to_some_int_const_if_in_in_range(OpType, Floor, FloorConst).

emu_int_quot_bits_per_int(OpType, ConstX, ConstZ) :-
    % This operation is defined only for word-size signed integers.
    OpType = op_int(word_bits(BitsPerInt)),
    ConstX = int_const(IntX),
    emu_quotient(OpType, integer(IntX), integer(BitsPerInt), ConstZ).

emu_int_times_bits_per_int(OpType, ConstX, ConstZ) :-
    % This operation is defined only for word-size signed integers.
    OpType = op_int(word_bits(BitsPerInt)),
    ConstX = int_const(IntX),
    emu_times(OpType, integer(IntX), integer(BitsPerInt), ConstZ).

emu_int_rem_bits_per_int(OpType, ConstX, ConstZ) :-
    % This operation is defined only for word-size signed integers.
    OpType = op_int(word_bits(BitsPerInt)),
    ConstX = int_const(IntX),
    emu_rem(OpType, integer(IntX), integer(BitsPerInt), ConstZ).

%---------------------------------------------------------------------------%
%
% The bitwise logical operations NOT, AND, OR and XOR *could* be implemented
% using the corresponding operations in library/integer.m, and such an
% implementation would be much shorter and simpler than the implementation
% here. The reason why we use the case-analysis-based implementation we do
% is because it is not clear how well those operations in integer.m have 
% been tested. Once that testing happens, and those operations are found to be
% correct and reliable, we should switch to using them.
% 

emu_not(OpType, FunctorX, ConstY) :-
    FunctorX = some_int_const(ConstX),
    require_complete_switch [OpType]
    (
        OpType = op_int(OpNumBits),
        require_complete_switch [OpNumBits]
        (
            OpNumBits = word_bits(TargetNumBits),
            ConstX = int_const(IntX),
            % Evaluate bitwise negations at compile time ONLY if the host
            % has the same number of bits per int as the target. Otherwise,
            % the result we compute here may differ from the result that
            % would be computed at runtime. This test was half of the code
            % added by commit a9d13d5fa2b871b3888de505c9c36425cd6ea3ed,
            % which fixed this bug; the other half is in our caller.
            int.bits_per_int = TargetNumBits,
            ConstY = int_const(int.(\ IntX))
        ;
            OpNumBits = bits_8,
            ConstX = int8_const(Int8X),
            ConstY = int8_const(int8.(\ Int8X))
        ;
            OpNumBits = bits_16,
            ConstX = int16_const(Int16X),
            ConstY = int16_const(int16.(\ Int16X))
        ;
            OpNumBits = bits_32,
            ConstX = int32_const(Int32X),
            ConstY = int32_const(int32.(\ Int32X))
        ;
            OpNumBits = bits_64,
            ConstX = int64_const(Int64X),
            ConstY = int64_const(int64.(\ Int64X))
        )
    ;
        OpType = op_uint(OpNumBits),
        require_complete_switch [OpNumBits]
        (
            OpNumBits = word_bits(TargetNumBits),
            ConstX = uint_const(UIntX),
            % The comment for the signed case above also applies here.
            uint.bits_per_uint = TargetNumBits,
            ConstY = uint_const(uint.(\ UIntX))
        ;
            OpNumBits = bits_8,
            ConstX = uint8_const(UInt8X),
            ConstY = uint8_const(uint8.(\ UInt8X))
        ;
            OpNumBits = bits_16,
            ConstX = uint16_const(UInt16X),
            ConstY = uint16_const(uint16.(\ UInt16X))
        ;
            OpNumBits = bits_32,
            ConstX = uint32_const(UInt32X),
            ConstY = uint32_const(uint32.(\ UInt32X))
        ;
            OpNumBits = bits_64,
            ConstX = uint64_const(UInt64X),
            ConstY = uint64_const(uint64.(\ UInt64X))
        )
    ).

%---------------------%

emu_and(OpType, FunctorX, FunctorY, ConstZ) :-
    FunctorX = some_int_const(ConstX),
    FunctorY = some_int_const(ConstY),
    require_complete_switch [OpType]
    (
        OpType = op_int(OpNumBits),
        require_complete_switch [OpNumBits]
        (
            OpNumBits = word_bits(_),
            ConstX = int_const(IntX),
            ConstY = int_const(IntY),
            ConstZ = int_const(int.(IntX /\ IntY))
        ;
            OpNumBits = bits_8,
            ConstX = int8_const(Int8X),
            ConstY = int8_const(Int8Y),
            ConstZ = int8_const(int8.(Int8X /\ Int8Y))
        ;
            OpNumBits = bits_16,
            ConstX = int16_const(Int16X),
            ConstY = int16_const(Int16Y),
            ConstZ = int16_const(int16.(Int16X /\ Int16Y))
        ;
            OpNumBits = bits_32,
            ConstX = int32_const(Int32X),
            ConstY = int32_const(Int32Y),
            ConstZ = int32_const(int32.(Int32X /\ Int32Y))
        ;
            OpNumBits = bits_64,
            ConstX = int64_const(Int64X),
            ConstY = int64_const(Int64Y),
            ConstZ = int64_const(int64.(Int64X /\ Int64Y))
        )
    ;
        OpType = op_uint(OpNumBits),
        require_complete_switch [OpNumBits]
        (
            OpNumBits = word_bits(_),
            ConstX = uint_const(UIntX),
            ConstY = uint_const(UIntY),
            ConstZ = uint_const(uint.(UIntX /\ UIntY))
        ;
            OpNumBits = bits_8,
            ConstX = uint8_const(UInt8X),
            ConstY = uint8_const(UInt8Y),
            ConstZ = uint8_const(uint8.(UInt8X /\ UInt8Y))
        ;
            OpNumBits = bits_16,
            ConstX = uint16_const(UInt16X),
            ConstY = uint16_const(UInt16Y),
            ConstZ = uint16_const(uint16.(UInt16X /\ UInt16Y))
        ;
            OpNumBits = bits_32,
            ConstX = uint32_const(UInt32X),
            ConstY = uint32_const(UInt32Y),
            ConstZ = uint32_const(uint32.(UInt32X /\ UInt32Y))
        ;
            OpNumBits = bits_64,
            ConstX = uint64_const(UInt64X),
            ConstY = uint64_const(UInt64Y),
            ConstZ = uint64_const(uint64.(UInt64X /\ UInt64Y))
        )
    ).

%---------------------%

emu_or(OpType, FunctorX, FunctorY, ConstZ) :-
    FunctorX = some_int_const(ConstX),
    FunctorY = some_int_const(ConstY),
    require_complete_switch [OpType]
    (
        OpType = op_int(OpNumBits),
        require_complete_switch [OpNumBits]
        (
            OpNumBits = word_bits(_),
            ConstX = int_const(IntX),
            ConstY = int_const(IntY),
            ConstZ = int_const(int.(IntX \/ IntY))
        ;
            OpNumBits = bits_8,
            ConstX = int8_const(Int8X),
            ConstY = int8_const(Int8Y),
            ConstZ = int8_const(int8.(Int8X \/ Int8Y))
        ;
            OpNumBits = bits_16,
            ConstX = int16_const(Int16X),
            ConstY = int16_const(Int16Y),
            ConstZ = int16_const(int16.(Int16X \/ Int16Y))
        ;
            OpNumBits = bits_32,
            ConstX = int32_const(Int32X),
            ConstY = int32_const(Int32Y),
            ConstZ = int32_const(int32.(Int32X \/ Int32Y))
        ;
            OpNumBits = bits_64,
            ConstX = int64_const(Int64X),
            ConstY = int64_const(Int64Y),
            ConstZ = int64_const(int64.(Int64X \/ Int64Y))
        )
    ;
        OpType = op_uint(OpNumBits),
        require_complete_switch [OpNumBits]
        (
            OpNumBits = word_bits(_),
            ConstX = uint_const(UIntX),
            ConstY = uint_const(UIntY),
            ConstZ = uint_const(uint.(UIntX \/ UIntY))
        ;
            OpNumBits = bits_8,
            ConstX = uint8_const(UInt8X),
            ConstY = uint8_const(UInt8Y),
            ConstZ = uint8_const(uint8.(UInt8X \/ UInt8Y))
        ;
            OpNumBits = bits_16,
            ConstX = uint16_const(UInt16X),
            ConstY = uint16_const(UInt16Y),
            ConstZ = uint16_const(uint16.(UInt16X \/ UInt16Y))
        ;
            OpNumBits = bits_32,
            ConstX = uint32_const(UInt32X),
            ConstY = uint32_const(UInt32Y),
            ConstZ = uint32_const(uint32.(UInt32X \/ UInt32Y))
        ;
            OpNumBits = bits_64,
            ConstX = uint64_const(UInt64X),
            ConstY = uint64_const(UInt64Y),
            ConstZ = uint64_const(uint64.(UInt64X \/ UInt64Y))
        )
    ).

%---------------------%

emu_xor(OpType, FunctorX, FunctorY, ConstZ) :-
    FunctorX = some_int_const(ConstX),
    FunctorY = some_int_const(ConstY),
    require_complete_switch [OpType]
    (
        OpType = op_int(OpNumBits),
        require_complete_switch [OpNumBits]
        (
            OpNumBits = word_bits(_),
            ConstX = int_const(IntX),
            ConstY = int_const(IntY),
            ConstZ = int_const(int.xor(IntX, IntY))
        ;
            OpNumBits = bits_8,
            ConstX = int8_const(Int8X),
            ConstY = int8_const(Int8Y),
            ConstZ = int8_const(int8.xor(Int8X, Int8Y))
        ;
            OpNumBits = bits_16,
            ConstX = int16_const(Int16X),
            ConstY = int16_const(Int16Y),
            ConstZ = int16_const(int16.xor(Int16X, Int16Y))
        ;
            OpNumBits = bits_32,
            ConstX = int32_const(Int32X),
            ConstY = int32_const(Int32Y),
            ConstZ = int32_const(int32.xor(Int32X, Int32Y))
        ;
            OpNumBits = bits_64,
            ConstX = int64_const(Int64X),
            ConstY = int64_const(Int64Y),
            ConstZ = int64_const(int64.xor(Int64X, Int64Y))
        )
    ;
        OpType = op_uint(OpNumBits),
        require_complete_switch [OpNumBits]
        (
            OpNumBits = word_bits(_),
            ConstX = uint_const(UIntX),
            ConstY = uint_const(UIntY),
            ConstZ = uint_const(uint.xor(UIntX, UIntY))
        ;
            OpNumBits = bits_8,
            ConstX = uint8_const(UInt8X),
            ConstY = uint8_const(UInt8Y),
            ConstZ = uint8_const(uint8.xor(UInt8X, UInt8Y))
        ;
            OpNumBits = bits_16,
            ConstX = uint16_const(UInt16X),
            ConstY = uint16_const(UInt16Y),
            ConstZ = uint16_const(uint16.xor(UInt16X, UInt16Y))
        ;
            OpNumBits = bits_32,
            ConstX = uint32_const(UInt32X),
            ConstY = uint32_const(UInt32Y),
            ConstZ = uint32_const(uint32.xor(UInt32X, UInt32Y))
        ;
            OpNumBits = bits_64,
            ConstX = uint64_const(UInt64X),
            ConstY = uint64_const(UInt64Y),
            ConstZ = uint64_const(uint64.xor(UInt64X, UInt64Y))
        )
    ).

%---------------------------------------------------------------------------%

:- pred to_some_int_const_if_in_in_range(op_type::in, integer::in,
    some_int_const::out) is semidet.

to_some_int_const_if_in_in_range(OpType, Integer, Const) :-
    (
        OpType = op_int(OpNumBits),
        NumBits = get_op_num_bits(OpNumBits),
        Integer >= -pow(integer.two, integer(NumBits - 1)),
        Integer < pow(integer.two, integer(NumBits - 1)),
        (
            OpNumBits = word_bits(_),
            % Fails if Integer is representable on the target machine,
            % but not on the host.
            integer.to_int(Integer, Int),
            Const = int_const(Int)
        ;
            OpNumBits = bits_8,
            integer.to_int8(Integer, Int8),
            Const = int8_const(Int8)
        ;
            OpNumBits = bits_16,
            integer.to_int16(Integer, Int16),
            Const = int16_const(Int16)
        ;
            OpNumBits = bits_32,
            integer.to_int32(Integer, Int32),
            Const = int32_const(Int32)
        ;
            OpNumBits = bits_64,
            integer.to_int64(Integer, Int64),
            Const = int64_const(Int64)
        )
    ;
        OpType = op_uint(OpNumBits),
        NumBits = get_op_num_bits(OpNumBits),
        Integer >= integer.zero,
        Integer < pow(integer.two, integer(NumBits)),
        (
            OpNumBits = word_bits(_),
            % Fails if Integer is representable on the target machine,
            % but not on the host.
            integer.to_uint(Integer, UInt),
            Const = uint_const(UInt)
        ;
            OpNumBits = bits_8,
            integer.to_uint8(Integer, UInt8),
            Const = uint8_const(UInt8)
        ;
            OpNumBits = bits_16,
            integer.to_uint16(Integer, UInt16),
            Const = uint16_const(UInt16)
        ;
            OpNumBits = bits_32,
            integer.to_uint32(Integer, UInt32),
            Const = uint32_const(UInt32)
        ;
            OpNumBits = bits_64,
            integer.to_uint64(Integer, UInt64),
            Const = uint64_const(UInt64)
        )
    ).

%---------------------------------------------------------------------------%

:- func get_op_type_bits(op_type) = int.

get_op_type_bits(OpType) = NumBits :-
    ( OpType = op_int(OpNumBits)
    ; OpType = op_uint(OpNumBits)
    ),
    NumBits = get_op_num_bits(OpNumBits).

:- func get_op_num_bits(op_num_bits) = int.

get_op_num_bits(OpNumBits) = NumBits :-
    ( OpNumBits = word_bits(NumBits)
    ; OpNumBits = bits_8,  NumBits = 8
    ; OpNumBits = bits_16, NumBits = 16
    ; OpNumBits = bits_32, NumBits = 32
    ; OpNumBits = bits_64, NumBits = 64
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.int_emu.
%---------------------------------------------------------------------------%
