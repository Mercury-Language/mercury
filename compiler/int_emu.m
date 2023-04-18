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

%---------------------------------------------------------------------------%

:- type word_bits
    --->    word_bits(int).

    % Return the number of bits per int for the selected compilation target.
    %
:- pred target_word_bits(globals::in, word_bits::out) is det.

%---------------------------------------------------------------------------%

:- pred int_plus(word_bits::in, int::in, int::in, int::out) is semidet.
:- pred uint_plus(word_bits::in, uint::in, uint::in, uint::out) is semidet.

:- pred int_minus(word_bits::in, int::in, int::in, int::out) is semidet.
:- pred uint_minus(word_bits::in, uint::in, uint::in, uint::out)
    is semidet.

:- pred int_times(word_bits::in, int::in, int::in, int::out) is semidet.
:- pred uint_times(word_bits::in, uint::in, uint::in, uint::out)
    is semidet.

:- pred int_quotient(word_bits::in, int::in, int::in, int::out) is semidet.
:- pred uint_quotient(word_bits::in, uint::in, uint::in, uint::out)
    is semidet.

:- pred int_mod(word_bits::in, int::in, int::in, int::out) is semidet.
:- pred uint_mod(word_bits::in, uint::in, uint::in, uint::out) is semidet.

:- pred int_rem(word_bits::in, int::in, int::in, int::out) is semidet.
:- pred uint_rem(word_bits::in, uint::in, uint::in, uint::out) is semidet.

%---------------------------------------------------------------------------%

:- pred int_left_shift(word_bits::in, int::in, int::in, int::out)
    is semidet.
:- pred int_left_ushift(word_bits::in, int::in, uint::in, int::out)
    is semidet.
:- pred uint_left_shift(word_bits::in, uint::in, int::in, uint::out)
    is semidet.
:- pred uint_left_ushift(word_bits::in, uint::in, uint::in, uint::out)
    is semidet.

:- pred int_right_shift(word_bits::in, int::in, int::in, int::out)
    is semidet.
:- pred int_right_ushift(word_bits::in, int::in, uint::in, int::out)
    is semidet.
:- pred uint_right_shift(word_bits::in, uint::in, int::in, uint::out)
    is semidet.
:- pred uint_right_ushift(word_bits::in, uint::in, uint::in, uint::out)
    is semidet.

%---------------------------------------------------------------------------%

:- pred int_floor_to_multiple_of_bits_per_int(int::in, word_bits::in,
    int::out) is semidet.

:- pred int_quot_bits_per_int(int::in, word_bits::in, int::out) is semidet.

:- pred int_times_bits_per_int(int::in, word_bits::in, int::out)
    is semidet.

:- pred int_rem_bits_per_int(int::in, word_bits::in, int::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.

:- import_module int.
:- import_module integer.
:- import_module uint.

%---------------------------------------------------------------------------%

target_word_bits(Globals, word_bits(WordBits)) :-
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

%---------------------------------------------------------------------------%

int_plus(WordBits, X, Y, Z) :-
    to_int_in_range(WordBits, integer(X) + integer(Y), Z).

uint_plus(WordBits, X, Y, Z) :-
    to_uint_in_range(WordBits,
        integer.from_uint(X) + integer.from_uint(Y), Z).

%---------------------%

int_minus(WordBits, X, Y, Z) :-
    to_int_in_range(WordBits, integer(X) - integer(Y), Z).

uint_minus(WordBits, X, Y, Z) :-
    to_uint_in_range(WordBits,
        integer.from_uint(X) - integer.from_uint(Y), Z).

%---------------------%

int_times(WordBits, X, Y, Z) :-
    to_int_in_range(WordBits, integer(X) * integer(Y), Z).

uint_times(WordBits, X, Y, Z) :-
    to_uint_in_range(WordBits,
        integer.from_uint(X) * integer.from_uint(Y), Z).

%---------------------%

int_quotient(WordBits, X, Y, Z) :-
    Y \= 0,
    to_int_in_range(WordBits, integer(X) // integer(Y), Z).

uint_quotient(WordBits, X, Y, Z) :-
    Y \= 0u,
    to_uint_in_range(WordBits,
        integer.from_uint(X) // integer.from_uint(Y), Z).

%---------------------%

int_mod(WordBits, X, Y, Z) :-
    Y \= 0,
    to_int_in_range(WordBits, integer(X) mod integer(Y), Z).

uint_mod(WordBits, X, Y, Z) :-
    Y \= 0u,
    to_uint_in_range(WordBits,
        integer.from_uint(X) mod integer.from_uint(Y), Z).

%---------------------%

int_rem(WordBits, X, Y, Z) :-
    Y \= 0,
    to_int_in_range(WordBits, integer(X) rem integer(Y), Z).

uint_rem(WordBits, X, Y, Z) :-
    Y \= 0u,
    to_uint_in_range(WordBits,
        integer.from_uint(X) rem integer.from_uint(Y), Z).

%---------------------------------------------------------------------------%

int_left_shift(WordBits, X, Y, Z) :-
    WordBits = word_bits(N),
    Y >= 0,
    Y < N,
    to_int_in_range(WordBits, integer(X) << Y, Z).

int_left_ushift(WordBits, X, UY, Z) :-
    WordBits = word_bits(N),
    Y = uint.cast_to_int(UY),
    % While UY cannot be negative, Y can be.
    Y >= 0,
    Y < N,
    to_int_in_range(WordBits, integer(X) << Y, Z).

uint_left_shift(WordBits, X, Y, Z) :-
    WordBits = word_bits(N),
    Y >= 0,
    Y < N,
    to_uint_in_range(WordBits, integer.from_uint(X) << Y, Z).

uint_left_ushift(WordBits, X, UY, Z) :-
    WordBits = word_bits(N),
    Y = uint.cast_to_int(UY),
    % While UY cannot be negative, Y can be.
    Y >= 0,
    Y < N,
    to_uint_in_range(WordBits, integer.from_uint(X) << Y, Z).

%---------------------%

int_right_shift(WordBits, X, Y, Z) :-
    WordBits = word_bits(N),
    Y >= 0,
    Y < N,
    to_int_in_range(WordBits, integer(X) >> Y, Z).

int_right_ushift(WordBits, X, UY, Z) :-
    WordBits = word_bits(N),
    Y = uint.cast_to_int(UY),
    % While UY cannot be negative, Y can be.
    Y >= 0,
    Y < N,
    to_int_in_range(WordBits, integer(X) >> Y, Z).

uint_right_shift(WordBits, X, Y, Z) :-
    WordBits = word_bits(N),
    Y >= 0,
    Y < N,
    to_uint_in_range(WordBits, integer.from_uint(X) >> Y, Z).

uint_right_ushift(WordBits, X, UY, Z) :-
    WordBits = word_bits(N),
    Y = uint.cast_to_int(UY),
    % While UY cannot be negative, Y can be.
    Y >= 0,
    Y < N,
    to_uint_in_range(WordBits, integer.from_uint(X) >> Y, Z).

%---------------------------------------------------------------------------%

int_floor_to_multiple_of_bits_per_int(X, WordBits, FloorInt) :-
    WordBits = word_bits(N),
    Trunc = integer(X) // integer(N),
    Floor0 = Trunc * integer(N),
    ( if Floor0 > integer(X) then
        Floor = Floor0 - integer(N)
    else
        Floor = Floor0
    ),
    to_int_in_range(WordBits, Floor, FloorInt).

int_quot_bits_per_int(X, WordBits, Z) :-
    WordBits = word_bits(Y),
    int_quotient(WordBits, X, Y, Z).

int_times_bits_per_int(X, WordBits, Z) :-
    WordBits = word_bits(Y),
    int_times(WordBits, X, Y, Z).

int_rem_bits_per_int(X, WordBits, Z) :-
    WordBits = word_bits(Y),
    int_rem(WordBits, X, Y, Z).

%---------------------------------------------------------------------------%

:- pred to_int_in_range(word_bits::in, integer::in, int::out) is semidet.

to_int_in_range(word_bits(WordBits), Integer, Int) :-
    Integer >= -pow(integer(2), integer(WordBits - 1)),
    Integer =< pow(integer(2), integer(WordBits - 1)) - one,
    integer.to_int(Integer, Int).

:- pred to_uint_in_range(word_bits::in, integer::in, uint::out) is semidet.

to_uint_in_range(word_bits(WordBits), Integer, UInt) :-
    Integer >= integer.zero,
    Integer =< pow(integer(2), integer(WordBits)),
    integer.to_uint(Integer, UInt).

%---------------------------------------------------------------------------%
:- end_module parse_tree.int_emu.
%---------------------------------------------------------------------------%
