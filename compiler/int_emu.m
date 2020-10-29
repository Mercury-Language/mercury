%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: int_emu.m.
% Main author: wangp.
%
% Emulate `int' operations for a given number of bits per int. These predicates
% succeed only if the result is defined for the given arguments, and the result
% can be represented by the `int' type of the host compiler.
%
%----------------------------------------------------------------------------%

:- module libs.int_emu.
:- interface.

:- import_module libs.globals.

%----------------------------------------------------------------------------%

:- type bits_per_int
    --->    bits_per_int(int).

    % Return the number of bits per int for the selected compilation target.
    %
:- pred target_bits_per_int(globals::in, bits_per_int::out) is det.

%----------------------------------------------------------------------------%

:- pred plus(bits_per_int::in, int::in, int::in, int::out) is semidet.

:- pred minus(bits_per_int::in, int::in, int::in, int::out) is semidet.

:- pred times(bits_per_int::in, int::in, int::in, int::out) is semidet.

:- pred quotient(bits_per_int::in, int::in, int::in, int::out) is semidet.

:- pred unchecked_quotient(bits_per_int::in, int::in, int::in, int::out)
    is semidet.

:- pred mod(bits_per_int::in, int::in, int::in, int::out) is semidet.

:- pred rem(bits_per_int::in, int::in, int::in, int::out) is semidet.

:- pred unchecked_rem(bits_per_int::in, int::in, int::in, int::out)
    is semidet.

:- pred left_shift(bits_per_int::in, int::in, int::in, int::out) is semidet.

:- pred unchecked_left_shift(bits_per_int::in, int::in, int::in, int::out)
    is semidet.

:- pred right_shift(bits_per_int::in, int::in, int::in, int::out) is semidet.

:- pred unchecked_right_shift(bits_per_int::in, int::in, int::in, int::out)
    is semidet.

:- pred floor_to_multiple_of_bits_per_int(int::in, bits_per_int::in, int::out)
    is semidet.

:- pred quot_bits_per_int(int::in, bits_per_int::in, int::out) is semidet.

:- pred times_bits_per_int(int::in, bits_per_int::in, int::out) is semidet.

:- pred rem_bits_per_int(int::in, bits_per_int::in, int::out) is semidet.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module integer.

:- import_module libs.options.

%----------------------------------------------------------------------------%

target_bits_per_int(Globals, bits_per_int(BitsPerInt)) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.lookup_int_option(Globals, bits_per_word, BitsPerInt)
    ;
        ( Target = target_csharp
        ; Target = target_java
        ),
        BitsPerInt = 32
    ).

%----------------------------------------------------------------------------%

plus(BitsPerInt, X, Y, Z) :-
    to_int_in_range(BitsPerInt, integer(X) + integer(Y), Z).

minus(BitsPerInt, X, Y, Z) :-
    to_int_in_range(BitsPerInt, integer(X) - integer(Y), Z).

times(BitsPerInt, X, Y, Z) :-
    to_int_in_range(BitsPerInt, integer(X) * integer(Y), Z).

quotient(BitsPerInt, X, Y, Z) :-
    to_int_in_range(BitsPerInt, integer(X) // integer(Y), Z).

unchecked_quotient(BitsPerInt, X, Y, Z) :-
    Y \= 0,
    quotient(BitsPerInt, X, Y, Z).

mod(BitsPerInt, X, Y, Z) :-
    to_int_in_range(BitsPerInt, integer(X) mod integer(Y), Z).

rem(BitsPerInt, X, Y, Z) :-
    to_int_in_range(BitsPerInt, integer(X) rem integer(Y), Z).

unchecked_rem(BitsPerInt, X, Y, Z) :-
    Y \= 0,
    rem(BitsPerInt, X, Y, Z).

left_shift(BitsPerInt, X, Y, Z) :-
    BitsPerInt = bits_per_int(N),
    to_int_in_range(BitsPerInt, integer(X) << min(Y, N), Z).

unchecked_left_shift(BitsPerInt, X, Y, Z) :-
    BitsPerInt = bits_per_int(N),
    Y >= 0,
    Y < N,
    left_shift(BitsPerInt, X, Y, Z).

right_shift(BitsPerInt, X, Y, Z) :-
    ( if Y < 0 then
        left_shift(BitsPerInt, X, -Y, Z)
    else
        to_int_in_range(BitsPerInt, integer(X) >> Y, Z)
    ).

unchecked_right_shift(BitsPerInt, X, Y, Z) :-
    BitsPerInt = bits_per_int(N),
    Y >= 0,
    Y < N,
    right_shift(BitsPerInt, X, Y, Z).

floor_to_multiple_of_bits_per_int(X, BitsPerInt, FloorInt) :-
    BitsPerInt = bits_per_int(N),
    Trunc = integer(X) // integer(N),
    Floor0 = Trunc * integer(N),
    ( if Floor0 > integer(X) then
        Floor = Floor0 - integer(N)
    else
        Floor = Floor0
    ),
    to_int_in_range(BitsPerInt, Floor, FloorInt).

quot_bits_per_int(X, BitsPerInt, Z) :-
    BitsPerInt = bits_per_int(Y),
    quotient(BitsPerInt, X, Y, Z).

times_bits_per_int(X, BitsPerInt, Z) :-
    BitsPerInt = bits_per_int(Y),
    times(BitsPerInt, X, Y, Z).

rem_bits_per_int(X, BitsPerInt, Z) :-
    BitsPerInt = bits_per_int(Y),
    rem(BitsPerInt, X, Y, Z).

:- pred to_int_in_range(bits_per_int::in, integer::in, int::out) is semidet.

to_int_in_range(bits_per_int(BitsPerInt), Integer, Int) :-
    Integer >= -pow(integer(2), integer(BitsPerInt - 1)),
    Integer =< pow(integer(2), integer(BitsPerInt - 1)) - one,
    integer.to_int(Integer, Int).

%----------------------------------------------------------------------------%
:- end_module libs.int_emu.
%----------------------------------------------------------------------------%
