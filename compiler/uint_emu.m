%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: uint_emu.m.
% Main author: juliensf.
%
% Emulate `uint' operations for a given number of bits per int. These
% predicates succeed only if the result is defined for the given arguments, and
% the result can be represented by the `uint' type of the host compiler.
%
%----------------------------------------------------------------------------%

:- module libs.uint_emu.
:- interface.

:- import_module libs.globals.

%----------------------------------------------------------------------------%

:- type bits_per_uint
    --->    bits_per_uint(int).

    % Return the number of bits per int for the selected compilation target.
    %
:- pred target_bits_per_uint(globals::in, bits_per_uint::out) is det.

%----------------------------------------------------------------------------%

:- pred plus(bits_per_uint::in, uint::in, uint::in, uint::out) is semidet.

:- pred minus(bits_per_uint::in, uint::in, uint::in, uint::out) is semidet.

:- pred times(bits_per_uint::in, uint::in, uint::in, uint::out) is semidet.

:- pred quotient(bits_per_uint::in, uint::in, uint::in, uint::out) is semidet.

:- pred mod(bits_per_uint::in, uint::in, uint::in, uint::out) is semidet.

:- pred rem(bits_per_uint::in, uint::in, uint::in, uint::out) is semidet.

:- pred left_shift(bits_per_uint::in, uint::in, int::in, uint::out)
    is semidet.
:- pred left_ushift(bits_per_uint::in, uint::in, uint::in, uint::out)
    is semidet.

:- pred right_shift(bits_per_uint::in, uint::in, int::in, uint::out)
    is semidet.
:- pred right_ushift(bits_per_uint::in, uint::in, uint::in, uint::out)
    is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.

:- import_module int.
:- import_module integer.
:- import_module uint.

%----------------------------------------------------------------------------%

target_bits_per_uint(Globals, bits_per_uint(BitsPerUInt)) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.lookup_int_option(Globals, bits_per_word, BitsPerUInt)
    ;
        ( Target = target_csharp
        ; Target = target_java
        ),
        BitsPerUInt = 32
    ).

%----------------------------------------------------------------------------%

plus(BitsPerUInt, X, Y, Z) :-
    to_uint_in_range(BitsPerUInt,
        integer.from_uint(X) + integer.from_uint(Y), Z).

minus(BitsPerUInt, X, Y, Z) :-
    to_uint_in_range(BitsPerUInt,
        integer.from_uint(X) - integer.from_uint(Y), Z).

times(BitsPerUInt, X, Y, Z) :-
    to_uint_in_range(BitsPerUInt,
        integer.from_uint(X) * integer.from_uint(Y), Z).

quotient(BitsPerUInt, X, Y, Z) :-
    Y \= 0u,
    to_uint_in_range(BitsPerUInt,
        integer.from_uint(X) // integer.from_uint(Y), Z).

mod(BitsPerUInt, X, Y, Z) :-
    Y \= 0u,
    to_uint_in_range(BitsPerUInt,
        integer.from_uint(X) mod integer.from_uint(Y), Z).

rem(BitsPerUInt, X, Y, Z) :-
    Y \= 0u,
    to_uint_in_range(BitsPerUInt,
        integer.from_uint(X) rem integer.from_uint(Y), Z).

%----------------------------------------------------------------------------%

left_shift(BitsPerUInt, X, Y, Z) :-
    BitsPerUInt = bits_per_uint(N),
    Y >= 0,
    Y < N,
    to_uint_in_range(BitsPerUInt, integer.from_uint(X) << Y, Z).

left_ushift(BitsPerUInt, X, UY, Z) :-
    BitsPerUInt = bits_per_uint(N),
    Y = uint.cast_to_int(UY),
    % While UY cannot be negative, Y can be.
    Y >= 0,
    Y < N,
    to_uint_in_range(BitsPerUInt, integer.from_uint(X) << Y, Z).

right_shift(BitsPerUInt, X, Y, Z) :-
    BitsPerUInt = bits_per_uint(N),
    Y >= 0,
    Y < N,
    to_uint_in_range(BitsPerUInt, integer.from_uint(X) >> Y, Z).

right_ushift(BitsPerUInt, X, UY, Z) :-
    BitsPerUInt = bits_per_uint(N),
    Y = uint.cast_to_int(UY),
    % While UY cannot be negative, Y can be.
    Y >= 0,
    Y < N,
    to_uint_in_range(BitsPerUInt, integer.from_uint(X) >> Y, Z).

%----------------------------------------------------------------------------%

:- pred to_uint_in_range(bits_per_uint::in, integer::in, uint::out) is semidet.

to_uint_in_range(bits_per_uint(BitsPerUInt), Integer, UInt) :-
    Integer >= integer.zero,
    Integer =< pow(integer(2), integer(BitsPerUInt)),
    integer.to_uint(Integer, UInt).

%----------------------------------------------------------------------------%
:- end_module libs.uint_emu.
%----------------------------------------------------------------------------%
