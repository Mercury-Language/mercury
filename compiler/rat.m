%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003, 2005-2006, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rat.m.
% Authors: vjteag, juliensf.
%
% Implements a rational number type using fixed precision integers.
% The functionality here is limited to that which is used in the
% lp_rational module.
%
% NOTE: if you actually want a general purpose rational number type,
% then use the rational module in the standard library. This module
% is pretty heavily geared towards a few specific tasks that are part of
% the termination analysis.
%
% TODO:
%   - overflow checking would be nice
%
%-----------------------------------------------------------------------------%

:- module libs.rat.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type rat.

:- func one = rat.
:- func zero = rat.

:- pred '<'(rat::in, rat::in) is semidet.
:- pred '>'(rat::in, rat::in) is semidet.
:- pred '=<'(rat::in, rat::in) is semidet.
:- pred '>='(rat::in, rat::in) is semidet.

:- func rat(int) = rat.
:- func rat(int, int) = rat.

:- func '+'(rat) = rat.
:- func '-'(rat) = rat.
:- func rat + rat = rat.
:- func rat - rat = rat.
:- func rat * rat = rat.
:- func rat / rat = rat.

:- func numer(rat) = int.
:- func denom(rat) = int.

:- func abs(rat) = rat.

    % Convert a rational to a string of the form: "(<Num>/<Denom>)".
    %
:- func to_string(rat) = string.

    % Write a rat in the form: r(<Numerator>, <Denominator>).
    %
:- pred write_rat(rat::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

    % The normal form of a rat number has the following properties:
    %
    %   - numerator and denominator have no common factors.
    %   - denominator is positive.
    %   - denominator is not zero.
    %   - if numerator is zero, then denominator is one.
    %
    % These invariants must be preserved by any rat number
    % constructed using this module since the equality predicate
    % on rats is simply Mercury's default unification
    % predicate =/2. If the invariants were not maintained,
    % we would have pathologies like r(-1,2) \= r(1,-2).
    %
    % The rat_norm/2 function generates rationals in this normal form.
    %
:- type rat
    --->    r(int, int).

one = r(1, 1).

zero = r(0, 1).

'<'(X, Y) :- cmp(X, Y) = (<).

'>'(X, Y) :- cmp(X, Y) = (>).

'=<'(X, Y) :- cmp(X, Y) \= (>).

'>='(X, Y) :- cmp(X, Y) \= (<).

rat(Int) = r(Int, 1).

rat(Num, Den) = rat_norm(Num, Den).

'+'(Rat) = Rat.

'-'(r(Num, Den)) = r(-Num, Den).

r(An, Ad) + r(Bn, Bd) = rat_norm(Numer, M) :-
    M = lcm(Ad, Bd),
    CA = M // Ad,
    CB = M // Bd,
    Numer = An * CA + Bn * CB.

X - Y = X + (-Y).

    % XXX: need we call rat_norm here?
r(An, Ad) * r(Bn, Bd) = rat_norm(Numer, Denom) :-
    G1 = gcd(An, Bd),
    G2 = gcd(Ad, Bn),
    Numer = (An // G1) * (Bn // G2),
    Denom = (Ad // G2) * (Bd // G1).

X / Y = X * rat.reciprocal(Y).

:- func rat.reciprocal(rat) = rat.

reciprocal(r(Num, Den)) =
    ( if Num = 0 then
        unexpected($pred, "division by zero")
    else
        r(signum(Num) * Den, int.abs(Num))
    ).

numer(r(Num, _)) = Num.

denom(r(_, Den)) = Den.

abs(r(Num, Den)) = r(int.abs(Num), Den).

:- func rat_norm(int, int) = rat.

rat_norm(Num, Den) = Rat :-
    ( if Den = 0 then
        unexpected($pred, "division by zero")
    else if Num = 0 then
        Rat = r(0, 1)
    else
        G = gcd(Num, Den),
        Num2 = Num * signum(Den),
        Den2 = int.abs(Den),
        Rat  = r(Num2 // G, Den2 // G)
    ).

:- func gcd(int, int) = int.

gcd(A, B) = gcd_2(int.abs(A), int.abs(B)).

:- func gcd_2(int, int) = int.

gcd_2(A, B) = ( if B = 0 then A else gcd_2(B, A rem B) ).

:- func lcm(int, int) = int.

lcm(A, B) =
    ( if A = 0 then
        0
    else if B = 0 then
        0
    else
        int.abs((A // gcd(A, B)) * B)
    ).

:- func signum(int) = int.

signum(N) = ( if N = 0 then 0 else if N < 0 then -1 else 1 ).

    % Builtin comparison does not give a natural ordering on rats.
    %
:- func cmp(rat, rat) = comparison_result.

cmp(X, Y) = Cmp :-
    Diff = X - Y,
    ( if is_zero(Diff) then
        Cmp = (=)
    else if is_negative(Diff) then
        Cmp = (<)
    else
        Cmp = (>)
    ).

:- pred is_zero(rat::in) is semidet.

is_zero(r(0, _)).

:- pred is_negative(rat::in) is semidet.

is_negative(r(Num, _)) :- Num < 0.

to_string(r(Num, Denom)) =
    ( if Num = 0 then
        "0"
    else
        "(" ++ string.int_to_string(Num) ++
            ( if Denom = 1 then
                ""
            else
                "/" ++ string.int_to_string(Denom)
            )
        ++ ")"
    ).

write_rat(r(Numerator, Denominator), !IO) :-
    io.write_string("r(", !IO),
    io.write_int(Numerator, !IO),
    io.write_string(", ", !IO),
    io.write_int(Denominator, !IO),
    io.write_char(')', !IO).

%-----------------------------------------------------------------------------%
:- end_module libs.rat.
%-----------------------------------------------------------------------------%
