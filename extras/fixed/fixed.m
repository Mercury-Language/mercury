%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006, 2011 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: fixed.m
% Author: Peter Ross <pro@missioncriticalit.com>
%
% Implementation of fixed point arithmetic which is equivalent to cobol
% fixed point arithmetic.
%
%-----------------------------------------------------------------------------%

:- module fixed.
:- interface.

%-----------------------------------------------------------------------------%

    % Represents a fixed point number at some given precision.
    %
:- type fixed.

%-----------------------------------------------------------------------------%

:- func - fixed = fixed.
:- func fixed + fixed = fixed.
:- func fixed - fixed = fixed.
:- func fixed * fixed = fixed.

    % div(MinP, A, B) is A / B where the result has
    % to have at least a precision MinP.
    %
:- func div(int, fixed, fixed) = fixed.

    % Increase or decrease the precision of the given fixed.
    % We decrease the precision by truncating the result.
    %
:- func precision(int, fixed) = fixed.

    % Return the integer part of the fixed point number.
    %
:- func to_int(fixed) = int.

    % Given a fixed return the floating point number
    % which represents that fixed point number.
    %
:- func to_float(fixed) = float.

    % truncate(P, F) truncates the number, F, to precision, P.
    %
:- func truncate(int, fixed) = fixed.

    % round(P, F) rounds the number, F, to precision, P.
    %
:- func round(int, fixed) = fixed.

    % Is the given floating point number equal to zero?
    %
:- pred is_zero(fixed::in) is semidet.

    % Determine the precision with which the given number
    % is stored.
    %
:- func fixed_precision(fixed) = int.

    % Compare two fixed point numbers.
    %
:- func compare_fixed(fixed::in, fixed::in) = (comparison_result::uo) is det.

    % Get the fractional part of a fixed point number as a string.
    %
:- func get_fraction_part_string(fixed) = string.

    % Get the integral part of a fixed point number as % a string.
    %
:- func get_whole_part_string(fixed) = string.

%------------------------------------------------------------------------------%
%
% Comparison operators for fixed point numbers.
%

:- pred (fixed::in) == (fixed::in) is semidet.
:- pred (fixed::in) \== (fixed::in) is semidet.

:- pred (fixed::in) < (fixed::in) is semidet.
:- pred (fixed::in) > (fixed::in) is semidet.
:- pred (fixed::in) =< (fixed::in) is semidet.
:- pred (fixed::in) >= (fixed::in) is semidet.

%------------------------------------------------------------------------------%

:- typeclass fixed(T) where [
        % Return the fixed point representation of T, with the supplied
        % precision
    func to_fixed(int, T) = fixed
].

:- instance fixed(int).

    % The float is rounded.
:- instance fixed(float).

    % The string is truncated.
:- instance fixed(string).

    % Output a fixed point number as a string.
    %
:- func to_string(fixed) = string.

    % Given a string, return the fixed which represents that string.
    %
:- func to_fixed(string) = fixed.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module require.
:- import_module string.

%------------------------------------------------------------------------------%

:- type fixed
    --->    fixed(
                precision   :: int,
                number      :: integer
            ).

%------------------------------------------------------------------------------%

- fixed(P, N) = fixed(P, integer(-1) * N).

X + Y = fixed(P, A + B) :-
    compare(Result, X ^ precision, Y ^ precision),
    (
        Result = (<),
        P = Y ^ precision,
        A = precision(P, X) ^ number,
        B = Y ^ number
    ;
        Result = (=),
        P = X ^ precision,
        A = X ^ number,
        B = Y ^ number
    ;
        Result = (>),
        P = X ^ precision,
        A = X ^ number,
        B = precision(P, Y) ^ number
    ).

X - Y = fixed(P, A - B) :-
    compare(Result, X ^ precision, Y ^ precision),
    (
        Result = (<),
        P = Y ^ precision,
        A = precision(P, X) ^ number,
        B = Y ^ number
    ;
        Result = (=),
        P = X ^ precision,
        A = X ^ number,
        B = Y ^ number
    ;
        Result = (>),
        P = X ^ precision,
        A = X ^ number,
        B = precision(P, Y) ^ number
    ).

X * Y = fixed(X ^ precision + Y ^ precision, X ^ number * Y ^ number).

div(MinP, X, Y) = fixed(P, N) :-
    Diff = X ^ precision - Y ^ precision,
    ( Diff < MinP ->
        P = MinP,
        N = (X ^ number * scale(MinP - Diff)) // Y ^ number
    ;
        P = Diff,
        N = X ^ number // Y ^ number
    ).

precision(DesiredP, fixed(ActualP, N0)) = fixed(DesiredP, N) :-
    compare(Result, DesiredP, ActualP),
    ( Result = (<),
        N = N0 // scale(ActualP - DesiredP)
    ; Result = (=),
        N = N0
    ; Result = (>),
        N = N0 * scale(DesiredP - ActualP)
    ).
    
:- func scale(int) = integer.

scale(X) = integer(10) `pow` integer(X).

truncate(DesiredP, F) = precision(DesiredP, F).

round(DesiredP, fixed(ActualP, N0)) = fixed(DesiredP, N) :-
    compare(Result, DesiredP, ActualP),
    (
        Result = (<),
        Scale = scale(ActualP - DesiredP),
        Rem = N0 rem Scale,
        ( Rem << 1 >= Scale ->
            N = N0 // Scale + integer.one
        ;
            N = N0 // Scale
        )
    ;
        Result = (=),
        N = N0
    ;
        Result = (>),
        N = N0 * scale(DesiredP - ActualP)
    ).
    
is_zero(N) :-
    N ^ number = integer.zero.

fixed_precision(N) = N ^ precision.

%------------------------------------------------------------------------------%

X == Y :-
    Result = compare_fixed(X, Y),
    Result = (=).

X \== Y :-
    Result = compare_fixed(X, Y),
    ( Result = (>)
    ; Result = (<)
    ).

X < Y :-
    Result = compare_fixed(X, Y),
    Result = (<).

X > Y :-
    Result = compare_fixed(X, Y),
    Result = (>).

X =< Y :-
    Result = compare_fixed(X, Y),
    ( Result = (<)
    ; Result = (=)
    ).

X >= Y :-
    Result = compare_fixed(X, Y),
    ( Result = (>)
    ; Result = (=)
    ).

compare_fixed(X, Y) = Result :-
    Z = (X - Y) ^ number,
    ( Z < integer.zero ->
        Result = (<)
    ; Z = integer.zero ->
        Result = (=)
    ;
        Result = (>)
    ).

%-----------------------------------------------------------------------------%

:- instance fixed(int) where [
    to_fixed(N, I) = fixed(N, integer(I) * (integer(10) `pow` integer(N)))
].

:- instance fixed(float) where [
    to_fixed(N, F) = to_fixed(N, string.format(Spec, [f(F)])) :-
    Spec = string.format("%%.%df", [i(N)])
].

:- instance fixed(string) where [
    to_fixed(N, S) = fixed(N, scaled_integer(N, S))
].

%-----------------------------------------------------------------------------%

to_string(fixed(N, Int)) = Str :-
    ( N = 0 ->
        Str = integer.to_string(Int)
    ;
        Cs0 = to_char_list(integer.to_string(Int)),
        insert_decimal_point(N, Cs0, P, Cs1),
        ( N >= P ->
            Cs = ['0', '.'] ++ list.duplicate(N - P, '0') ++ Cs1
        ;
            Cs = Cs1
        ),
        Str = from_char_list(Cs)
    ).

:- pred insert_decimal_point(int::in, list(char)::in,
    int::out, list(char)::out) is det.

insert_decimal_point(_, [], 0, []).
insert_decimal_point(N, [C|Cs], P+1, L) :-
    insert_decimal_point(N, Cs, P, L0),
    ( N = P ->
        L = [C, '.' | L0]
    ;
        L = [C | L0]
    ).

%-----------------------------------------------------------------------------%

to_int(F) = det_to_int(I) :- fixed(_, I) = precision(0, F).

to_float(fixed(P, N)) = float(N) / pow(10.0, P).

%-----------------------------------------------------------------------------%

    % Deterministic version of scaled_integer which throws an error, instead
    % of failing.
    %
:- func scaled_integer(int, string) = integer.

scaled_integer(N, Str) =
    ( scaled_integer(N, Str, ScaledInteger) ->
        ScaledInteger
    ;
        func_error("scaled_integer: " ++ Str)
    ).

    % scaled_integer(N, S, SI) is true iff
    % SI is a scaled integer which represents the string, S, as a fixed
    % point number of order N.
    %
    % Fails if S doesn't represent a number.
    %
    % Note that SI is a truncated version of S, if S has greater precision
    % than N. eg fixed(1, "1.36", integer(13)) is true, there is no rounding.
    %
:- pred scaled_integer(int::in, string::in, integer::out) is semidet.

scaled_integer(N, Str, ScaledInteger) :-
    Str \= "",
    L = to_char_list(Str),
    ( L = ['-' | Cs] ->
        scaled_integer(N, Cs, integer(0), ScaledInteger0),
        ScaledInteger = integer(-1) * ScaledInteger0
    ; L = ['+' | Cs] ->
        scaled_integer(N, Cs, integer(0), ScaledInteger)
    ;
        scaled_integer(N, L, integer(0), ScaledInteger)
    ).


:- pred scaled_integer(int::in, list(char)::in,
    integer::in, integer::out) is semidet.

scaled_integer(N, [], A0, A) :-
    A = A0 * scale(N).
scaled_integer(N, [C|Cs], A0, A) :-
    ( C = ('.') ->
        L = list.take_upto(N, Cs),
        fraction(L, A0, A1),
        A = A1 * (integer(10) `pow` integer(N - length(L)))
    ;
        char_to_int(C, I),
        scaled_integer(N, Cs, A0 * integer(10) + integer(I), A)
    ).

:- pred fraction(list(char)::in, integer::in, integer::out) is semidet.

fraction([], A, A).
fraction([C|Cs], A0, A) :-
    char_to_int(C, I),
    fraction(Cs, A0 * integer(10) + integer(I), A).

:- pred char_to_int(char::in, int::out) is semidet.

char_to_int('0', 0).
char_to_int('1', 1).
char_to_int('2', 2).
char_to_int('3', 3).
char_to_int('4', 4).
char_to_int('5', 5).
char_to_int('6', 6).
char_to_int('7', 7).
char_to_int('8', 8).
char_to_int('9', 9).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- import_module maybe.

to_fixed(Str) = Fixed :-
    L = to_char_list(Str),
    ( L = ['-' | Cs] ->
        Factor = integer(-1),
        List = Cs
    ; L = ['+' | Cs] ->
        Factor = integer(+1),
        List = Cs
    ; L = [_|_] ->
        Factor = integer(+1),
        List = L
    ;
        error("to_fixed: empty string")
    ),
    ( parse_fixed(List, integer(0), N0, no, P) ->
        N = Factor * N0,
        Fixed = fixed(P, N)
    ;
        error("to_fixed: " ++ Str)
    ).

:- pred parse_fixed(list(char)::in, integer::in, integer::out,
    maybe(int)::in, int::out) is semidet.

parse_fixed([], I, I, no, 0).
parse_fixed([], I, I, yes(P), P).
parse_fixed([C | Cs], I0, I, no, P) :-
    ( C = ('.') ->
        parse_fixed(Cs, I0, I, yes(0), P)
    ;
        char_to_int(C, CInt),
        parse_fixed(Cs, integer(10) * I0 + integer(CInt), I, no, P)
    ).
parse_fixed([C | Cs], I0, I, yes(P0), P) :-
    char_to_int(C, CInt),
    parse_fixed(Cs, integer(10) * I0 + integer(CInt), I, yes(P0 + 1), P).
    
%-----------------------------------------------------------------------------%

get_fraction_part_string(fixed(Precision, N)) = FracStr :-
    FracPart = N mod pow(integer(10), integer(Precision)),
    FracStr = to_string(FracPart).

get_whole_part_string(fixed(Precision, N)) = WholeStr :-
    WholePart = N div pow(integer(10), integer(Precision)),
    WholeStr = to_string(WholePart).

%-----------------------------------------------------------------------------%
:- end_module fixed.
%-----------------------------------------------------------------------------%
