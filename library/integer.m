%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2000, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: integer.m
% main authors: aet Mar 1998. 
%               Dan Hazel <odin@svrc.uq.edu.au> Oct 1999.
% 
% Implements an arbitrary precision integer type and basic 
% operations on it. (An arbitrary precision integer may have
% any number of digits, unlike an int, which is limited to the
% precision of the machine's int type, which is typically 32 bits.)
%
% Note that all operators behave as the equivalent operators on ints do.
% This includes the division operators: / // rem div mod.
%
% Possible improvements:
%
%	1) allow negative digits (-base+1 .. base-1) in lists of
%	  digits and normalise only when printing. This would
%	  probably simplify the division algorithm, also.
%       (djh: this is not really done although -ve integers include a list
%             of -ve digits for faster comparison and so that normal mercury
%             sorting produces an intuitive order)
%
%	2) alternatively, instead of using base=10000, use *all* the
%	  bits in an int and make use of the properties of machine
%	  arithmetic. Base 10000 doesn't use even half the bits
%	  in an int, which is inefficient. (Base 2^14 would be
%	  a little better but would require a slightly more
%	  complex case conversion on reading and printing.)
%       (djh: this is done)
%
%	3) Use an O(n^(3/2)) algorithm for multiplying large
%	  integers, rather than the current O(n^2) method.
%	  There's an obvious divide-and-conquer technique,
%	  Karatsuba multiplication.
%
%	4) We could overload operators so that we can have mixed operations
%	  on ints and integers. For example, "integer(1)+3". This
%	  would obviate most calls of integer().
%
%	5) Use double-ended lists rather than simple lists. This
%	  would improve the efficiency of the division algorithm,
%	  which reverse lists.
%       (djh: this is obsolete - digits lists are now in normal order)
%
%	6) Add bit operations (XOR, AND, OR, etc). We would treat
%	  the integers as having a 2's complement bit representation.
%	  This is easier to do if we use base 2^14 as mentioned above.
%       (djh: this is done:  /\ \/ << >> xor \)
%
%	7) The implementation of `div' is slower than it need be.
%       (djh: this is much improved)
%
%	8) Fourier methods such as Schoenhage-Strassen and
%	  multiplication via modular arithmetic are left as
%	  exercises to the reader. 8^)
%
%
%	Of the above, 1) would have the best bang-for-buck, 5) would
%	benefit division and remainder operations quite a lot, and 3)
%	would benefit large multiplications (thousands of digits)
%	and is straightforward to implement.
%       (djh:
%            I'd like to see 1) done.
%            integers are now represented as
%                    i(Length, Digits)
%                where Digits are no longer reversed.
%            The only penalty for not reversing is in multiplication
%            by the base which now entails walking to the end of the list
%            to append a 0.
%            Therefore I'd like to see:
%       9) Allow empty tails for low end zeros.
%          Base multiplication is then an increment to Length.


:- module integer.

:- interface.

:- import_module string, float.

:- type integer.

:- pred '<'(integer, integer).
:- mode '<'(in, in) is semidet.

:- pred '>'(integer, integer).
:- mode '>'(in, in) is semidet.

:- pred '=<'(integer, integer).
:- mode '=<'(in, in) is semidet.

:- pred '>='(integer, integer).
:- mode '>='(in, in) is semidet.

:- func integer(int) = integer.

:- func integer__to_string(integer) = string.

:- func integer__from_string(string) = integer.
:- mode integer__from_string(in) = out is semidet.

:- func '+'(integer) = integer.

:- func '-'(integer) = integer.

:- func integer + integer = integer.

:- func integer - integer = integer.

:- func integer * integer = integer.

:- func integer // integer = integer.

:- func integer div integer = integer.

:- func integer rem integer = integer.

:- func integer mod integer = integer.

:- func integer << int = integer.

:- func integer >> int = integer.

:- func integer /\ integer = integer.

:- func integer \/ integer = integer.

:- func integer `xor` integer = integer.

:- func \ integer = integer.

:- func integer__abs(integer) = integer.

:- pred integer__pow(integer, integer, integer).
:- mode integer__pow(in, in, out) is det.

:- func integer__float(integer) = float.
:- func integer__int(integer) = int.

:- implementation.

:- import_module require, list, char, std_util, int.

:- type sign == int.	% sign of integer and length of digit list
:- type digit == int.	% base 2^14 digit

:- type integer
	--->	i(sign, list(digit)).

:- func base = int.
base = 16384. % 2^14
:- func basediv2 = int.
basediv2 = 8192.

:- func log2base = int.
log2base = 14.
:- func basemask = int.
basemask = 16383.
:- func highbitmask = int.
highbitmask = basediv2.
:- func lowbitmask = int.
lowbitmask = 1.
:- func evenmask = int.
evenmask = 16382.

'<'(X1, X2) :- 
    big_cmp(X1, X2) = C,
    C = (<).

'>'(X1, X2) :-
    big_cmp(X1, X2) = C,
    C = (>).

'=<'(X1, X2) :-
    big_cmp(X1, X2) = C,
    ( C = (<) ; C = (=)).

'>='(X1, X2) :-
    big_cmp(X1, X2) = C,
    ( C = (>) ; C = (=)).

'+'(X1) =
    X1.

'-'(N) =
    big_neg(N).

X1 + X2 = big_plus(X1, X2).

X1 - X2 = big_plus(X1, big_neg(X2)).

X1 * X2 = big_mul(X1, X2).

X1 div X2 = big_div(X1, X2).

X1 // X2 = big_quot(X1, X2).

X1 rem X2 = big_rem(X1, X2).

X1 mod X2 = big_mod(X1, X2).

X << I =
    (
        I > 0 ->
            big_left_shift(X, I)
    ;
        I < 0 ->
            X >> -I
    ;
        X
    ).

X >> I = 
    (
        I < 0 ->
            X << -I
    ;
        I > 0 ->
            big_right_shift(X, I)
    ;
        X
    ).

X1 /\ X2 =
    (
        big_isnegative(X1) ->
            (
                big_isnegative(X2) ->
                    \ big_or(\ X1, \ X2)
            ;
                big_and_not(X2, \ X1) 
            )
    ;
        big_isnegative(X2) ->
            big_and_not(X1, \ X2) 
    ;
        big_and(X1, X2)
    ).


X1 \/ X2 =
    (
        big_isnegative(X1) ->
            (
                big_isnegative(X2) ->
                    \ big_and(\ X1, \ X2)
            ;
                \ big_and_not(\ X1, X2) 
            )
    ;
        big_isnegative(X2) ->
            \ big_and_not(\ X2, X1)
    ;
        big_or(X1, X2)
    ).

X1 `xor` X2 = 
    (
        big_isnegative(X1) ->
            (
                big_isnegative(X2) ->
                    big_xor(\ X1, \ X2)
            ;
                big_xor_not(X2, \X1)
            )
    ;
        big_isnegative(X2) ->
            big_xor_not(X1, \X2)
    ;
        big_xor(X1, X2)
    ).

\ X = big_neg(big_plus(X, integer__one)).

integer__abs(N) = big_abs(N).


:- func big_abs(integer) = integer.
big_abs(i(Sign, Ds)) =
    (Sign < 0 ->
        big_neg(i(Sign, Ds))
    ;
	i(Sign, Ds)
    ).

:- pred neg_list(list(int)::in, list(int)::out, list(int)::in) is det.
neg_list([]) -->
    [].
neg_list([H | T]) -->
    [-H],
    neg_list(T).

:- pred big_isnegative(integer::in) is semidet.
big_isnegative(i(Sign, _)) :-
    Sign < 0.

:- pred big_iszero(integer::in) is semidet.
big_iszero(i(0, [])).


:- func big_neg(integer) = integer.
big_neg(i(S, Ds)) = i(-S, NewDs) :-
    neg_list(Ds, NewDs, []).

:- func big_mul(integer, integer) = integer.
big_mul(I1, I2) =
    big_sign(integer_signum(I1) * integer_signum(I2),
             pos_mul(big_abs(I1), big_abs(I2))).

:- func big_sign(int, integer) = integer.
big_sign(Sign, In) =
    (Sign < 0 ->
        big_neg(In)
    ;
        In
    ).

:- func big_quot(integer, integer) = integer.
big_quot(X1, X2) = Q :-
    big_quot_rem(X1, X2, Q, _R).

:- func big_rem(integer, integer) = integer.
big_rem(X1, X2) = R :-
    big_quot_rem(X1, X2, _Q, R).

:- func big_div(integer, integer) = integer.
big_div(X, Y) = Div :-
    big_quot_rem(X, Y, Trunc, Rem),
    (integer_signum(Y) * integer_signum(Rem) < 0 ->
        Div = Trunc - integer__one
    ;
        Div = Trunc
    ).

:- func big_mod(integer, integer) = integer.
big_mod(X, Y) = Mod :-
    big_quot_rem(X, Y, _Trunc, Rem),
    (integer_signum(Y) * integer_signum(Rem) < 0 ->
        Mod = Rem + Y
    ;
        Mod = Rem
    ).


:- func big_right_shift(integer, int) = integer.
big_right_shift(X, I) =
    (
        big_iszero(X) ->
            X
    ;
        big_isnegative(X) ->
            \ pos_right_shift(\ X, I)
    ;
        pos_right_shift(X, I)
    ).

:- func pos_right_shift(integer, int) = integer.
pos_right_shift(i(Len, Digits), I) = Integer :-
    Div = I div log2base,
    (Div < Len ->
        Mod = I mod log2base,
        Integer = decap(rightshift(Mod, log2base - Mod,
                                   i(Len - Div, Digits), 0))
    ;
        Integer = integer__zero
    ).

:- func rightshift(int, int, integer, int) = integer.
rightshift(_Mod, _InvMod, i(_Len, []), _Carry) = integer__zero.
rightshift(Mod, InvMod, i(Len, [H | T]), Carry) = Integer :-
    (Len =< 0 ->
        Integer = integer__zero
    ;
        NewH = Carry \/ (H >> Mod),
        NewCarry = (H /\ (basemask >> InvMod)) << InvMod,
        i(TailLen, NewTail) = rightshift(Mod, InvMod, i(Len - 1, T), NewCarry),
        Integer = i(TailLen + 1, [NewH | NewTail])
    ).


:- func big_left_shift(integer, int) = integer.
big_left_shift(X, I) =
    (
        big_iszero(X) ->
            X
    ;
        big_isnegative(X) ->
            big_neg(pos_left_shift(big_neg(X), I))
    ;
        pos_left_shift(X, I)
    ).


:- func pos_left_shift(integer, int) = integer.
pos_left_shift(i(Len, Digits), I) = Integer :-
    Div = I div log2base,
    Mod = I mod log2base,
    NewLen = Len + Div,
    leftshift(Mod, log2base - Mod, NewLen, Digits, Carry, NewDigits),
    (Carry = 0 ->
        Integer = i(NewLen, NewDigits)
    ;
        Integer = i(NewLen + 1, [Carry | NewDigits])
    ).


:- pred leftshift(int::in, int::in, int::in, list(digit)::in,
                  int::out, list(digit)::out) is det.
leftshift(_Mod, _InvMod, Len, [], Carry, DigitsOut) :-
    Carry = 0,
    zeros(Len, DigitsOut, []).
leftshift(Mod, InvMod, Len, [H | T], Carry, DigitsOut) :-
    (Len =< 0 ->
        Carry = 0,
        DigitsOut = []
    ;
        Carry = (H /\ (basemask << InvMod)) >> InvMod,
        leftshift(Mod, InvMod, Len - 1, T, TailCarry, Tail),
        DigitsOut = [TailCarry \/ ((H << Mod) /\ basemask) | Tail]
    ).

:- pred zeros(int::in, list(digit)::out, list(digit)::in) is det.
zeros(Len) -->
    ({ Len > 0 } ->
        [0],
        zeros(Len - 1)
    ;
        []
    ).

:- func big_or(integer, integer) = integer.
big_or(X1, X2) = decap(or_pairs(X1, X2)).

:- func or_pairs(integer, integer) = integer.
or_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    (
        L1 = L2 ->
            Integer = i(L1, or_pairs_equal(D1, D2))
    ;
        L1 < L2, D2 = [H2 | T2] ->
            i(_, DsT) = or_pairs(i(L1, D1), i(L2 - 1, T2)),
            Integer = i(L2, [H2 | DsT])
    ;
        L1 > L2, D1 = [H1 | T1] ->
            i(_, DsT) = or_pairs(i(L1 - 1, T1), i(L2, D2)),
            Integer = i(L1, [H1 | DsT])
    ;
	error("integer__or_pairs")
    ).

:- func or_pairs_equal(list(digit), list(digit)) = list(digit).
or_pairs_equal([], _) = [].
or_pairs_equal([X | Xs], [Y | Ys]) = [X \/ Y | or_pairs_equal(Xs, Ys)].
or_pairs_equal([_ | _], []) = [].



:- func big_xor(integer, integer) = integer.
big_xor(X1, X2) = decap(xor_pairs(X1, X2)).

:- func xor_pairs(integer, integer) = integer.
xor_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    (
        L1 = L2 ->
            Integer = i(L1, xor_pairs_equal(D1, D2))
    ;
        L1 < L2, D2 = [H2 | T2] ->
            i(_, DsT) = xor_pairs(i(L1, D1), i(L2 - 1, T2)),
            Integer = i(L2, [H2 | DsT])
    ;
        L1 > L2, D1 = [H1 | T1] ->
            i(_, DsT) = xor_pairs(i(L1 - 1, T1), i(L2, D2)),
            Integer = i(L1, [H1 | DsT])
    ;
	error("integer__xor_pairs")
    ).

:- func xor_pairs_equal(list(digit), list(digit)) = list(digit).
xor_pairs_equal([], _) = [].
xor_pairs_equal([X | Xs], [Y | Ys]) =
	[int__xor(X, Y) | xor_pairs_equal(Xs, Ys)].
xor_pairs_equal([_ | _], []) = [].



:- func big_and(integer, integer) = integer.
big_and(X1, X2) = decap(and_pairs(X1, X2)).

:- func and_pairs(integer, integer) = integer.
and_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    (
        L1 = L2 ->
            Integer = i(L1, and_pairs_equal(D1, D2))
    ;
        L1 < L2, D2 = [_ | T2] ->
            i(_, DsT) = and_pairs(i(L1, D1), i(L2 - 1, T2)),
            Integer = i(L1, DsT)
    ;
        L1 > L2, D1 = [_ | T1] ->
            i(_, DsT) = and_pairs(i(L1 - 1, T1), i(L2, D2)),
            Integer = i(L2, DsT)
    ;
	error("integer__and_pairs")
    ).

:- func and_pairs_equal(list(digit), list(digit)) = list(digit).
and_pairs_equal([], _) = [].
and_pairs_equal([X | Xs], [Y | Ys]) = [X /\ Y | and_pairs_equal(Xs, Ys)].
and_pairs_equal([_ | _], []) = [].

:- func big_and_not(integer, integer) = integer.
big_and_not(X1, X2) = decap(and_not_pairs(X1, X2)).

:- func and_not_pairs(integer, integer) = integer.
and_not_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    (
        L1 = L2 ->
            Integer = i(L1, and_not_pairs_equal(D1, D2))
    ;
        L1 < L2, D2 = [_ | T2] ->
            i(_, DsT) = and_not_pairs(i(L1, D1), i(L2 - 1, T2)),
            Integer = i(L1, DsT)
    ;
        L1 > L2, D1 = [H1 | T1] ->
            i(_, DsT) = and_not_pairs(i(L1 - 1, T1), i(L2, D2)),
            Integer = i(L1, [H1 | DsT])
    ;
	error("integer__and_not_pairs")
    ).

:- func and_not_pairs_equal(list(digit), list(digit)) = list(digit).
and_not_pairs_equal([], _) = [].
and_not_pairs_equal([X | Xs], [Y | Ys]) =
    [X /\ \ Y | and_not_pairs_equal(Xs, Ys)].
and_not_pairs_equal([_ | _], []) = [].


:- func big_xor_not(integer, integer) = integer.
big_xor_not(X1, NotX2) =
    \ big_and_not(big_or(X1, NotX2), big_and(X1, NotX2)).


:- func big_cmp(integer, integer) = comparison_result.
big_cmp(I1, I2) = Result :-
    compare(Result, I1, I2).


:- func pos_cmp(integer, integer) = comparison_result.
pos_cmp(Xs, Ys) = Result :-
    compare(Result, Xs, Ys).

:- func big_plus(integer, integer) = integer.
big_plus(I1, I2) = Sum :-
    (
        I1 = integer__zero ->
            Sum = I2
    ;
        I2 = integer__zero ->
            Sum = I1
    ;
        Abs1 = big_abs(I1),
        Abs2 = big_abs(I2),
        S1 = integer_signum(I1),
        S2 = integer_signum(I2),
        (
            S1 = S2 ->
                Sum = big_sign(S1, pos_plus(Abs1, Abs2))
        ;
            C = pos_cmp(Abs1, Abs2),
            (
                C = (<) ->
                    Sum = big_sign(S2, pos_minus(Abs2, Abs1))
            ;
                C = (>) ->
                    Sum = big_sign(S1, pos_minus(Abs1, Abs2))
            ;
                Sum = integer__zero
            )
        )
    ).

integer(N) =
    int_to_integer(N).

% Note: Since most machines use 2's complement arithmetic,
% INT_MIN is usually -INT_MAX-1, hence -INT_MIN will
% cause int overflow. We handle overflow below.
% We don't check for a negative result from abs(), which
% would indicate overflow, since we may trap int overflow
% instead.
%
% XXX: What about machines that aren't 2's complement?
:- func int_to_integer(int) = integer.
int_to_integer(D) = Int :-
    (
        D = 0 ->
            Int = integer__zero
    ;
        D > 0, D < base ->
            Int = i(1, [D])
    ;
        D < 0, D > -base ->
            Int = i(-1, [D])
    ;
	( int__min_int(D) ->
	    % were we to call int__abs, int overflow might occur.
	    Int = integer(D + 1) - integer__one
	;
	    int__abs(D, AD),
	    Int = big_sign(D, pos_int_to_digits(AD))
	)
    ).

:- func shortint_to_integer(int) = integer.
shortint_to_integer(D) =
    (
        D = 0 ->
            integer__zero
    ;
        D > 0 ->
            i(1, [D])
    ;
        i(-1, [D])
    ).

:- func signum(int) = int.
signum(N) = SN :-
    ( N < 0 ->
	SN = -1
    ; N = 0 ->
	SN = 0
    ;
	SN = 1
    ).

:- func integer_signum(integer) = int.
integer_signum(i(Sign, _)) = signum(Sign).

:- func pos_int_to_digits(int) = integer.
pos_int_to_digits(D) =
    pos_int_to_digits_2(D, integer__zero).

:- func pos_int_to_digits_2(int, integer) = integer.
pos_int_to_digits_2(D, Tail) = Result :-
    (D = 0 ->
	Result = Tail
    ;
        Tail = i(Length, Digits),
        chop(D, Div, Mod),
        Result = pos_int_to_digits_2(Div, i(Length + 1, [Mod | Digits]))
    ).

:- func mul_base(integer) = integer.
mul_base(i(L, Ds)) = Result :-
    (Ds = [] ->
        Result = integer__zero
    ;
        Result = i(L + 1, mul_base_2(Ds))
    ).

:- func mul_base_2(list(digit)) = list(digit).
mul_base_2([]) = [0].
mul_base_2([H | T]) = [H | mul_base_2(T)].

:- func mul_by_digit(digit, integer) = integer.
mul_by_digit(D, i(Len, Ds)) = Out :-
    mul_by_digit_2(D, Mod, Ds, DsOut),
    (Mod = 0 ->
	Out = i(Len, DsOut)
    ;
	Out = i(Len + 1, [Mod | DsOut])
    ).

:- pred mul_by_digit_2(digit::in, digit::out,
                       list(digit)::in, list(digit)::out)
   is det.
mul_by_digit_2(_, 0, [], []).
mul_by_digit_2(D, Div, [X | Xs], [Mod | NewXs]) :-
    mul_by_digit_2(D, DivXs, Xs, NewXs),
    chop(D * X + DivXs, Div, Mod).


:- pred chop(int, digit, digit).
:- mode chop(in, out, out) is det.
chop(N, Div, Mod) :-
    %Div = N div base,
    %Mod = N mod base.
    Div = N >> log2base,
    Mod = N /\ basemask.

:- func pos_plus(integer, integer) = integer.
pos_plus(i(L1, D1), i(L2, D2)) = Out :-
    add_pairs(Div, i(L1, D1), i(L2, D2), Ds),
    (L1 > L2 ->
        Len = L1
    ;
        Len = L2
    ),
    (Div = 0 ->
	Out = i(Len, Ds)
    ;
	Out = i(Len + 1, [Div | Ds])
    ).

:- pred add_pairs(digit::out, integer::in, integer::in,
                  list(digit)::out)
   is det.
add_pairs(Div, i(L1, D1), i(L2, D2), Ds) :-
    (
        L1 = L2 ->
            add_pairs_equal(Div, D1, D2, Ds)
    ;
        L1 < L2, D2 = [H2 | T2] ->
            add_pairs(Div1, i(L1, D1), i(L2 - 1, T2), Ds1),
            chop(H2 + Div1, Div, Mod),
            Ds = [Mod | Ds1]
    ;
        L1 > L2, D1 = [H1 | T1] ->
            add_pairs(Div1, i(L1 - 1, T1), i(L2, D2), Ds1),
            chop(H1 + Div1, Div, Mod),
            Ds = [Mod | Ds1]
    ;
	error("integer__add_pairs")
    ).

:- pred add_pairs_equal(digit::out, list(digit)::in, list(digit)::in,
                        list(digit)::out)
   is det.
add_pairs_equal(0, [], _, []).
add_pairs_equal(Div, [X | Xs], [Y | Ys], [Mod | TailDs]) :-
    add_pairs_equal(DivTail, Xs, Ys, TailDs),
    chop(X + Y + DivTail, Div, Mod).
add_pairs_equal(0, [_ | _], [], []).


:- func pos_minus(integer, integer) = integer.
pos_minus(i(L1, D1), i(L2, D2)) = Out :-
    diff_pairs(Mod, i(L1, D1), i(L2, D2), Ds),
    (L1 > L2 ->
        Len = L1
    ;
        Len = L2
    ),
    (Mod = 0 ->
	Out = decap(i(Len, Ds))
    ;
	Out = i(Len + 1, [Mod | Ds])
    ).

:- pred diff_pairs(digit::out, integer::in, integer::in,
                  list(digit)::out)
   is det.
diff_pairs(Div, i(L1, D1), i(L2, D2), Ds) :-
    (
        L1 = L2 ->
            diff_pairs_equal(Div, D1, D2, Ds)
    ;
        L1 > L2, D1 = [H1 | T1] ->
            diff_pairs(Div1, i(L1 - 1, T1), i(L2, D2), Ds1),
            chop(H1 + Div1, Div, Mod),
            Ds = [Mod | Ds1]
    ;
	error("integer__diff_pairs")
    ).

:- pred diff_pairs_equal(digit::out, list(digit)::in, list(digit)::in,
                        list(digit)::out)
   is det.
diff_pairs_equal(0, [], _, []).
diff_pairs_equal(Div, [X | Xs], [Y | Ys], [Mod | TailDs]) :-
    diff_pairs_equal(DivTail, Xs, Ys, TailDs),
    chop(X - Y + DivTail, Div, Mod).
diff_pairs_equal(0, [_ | _], [], []).


:- func pos_mul(integer, integer) = integer.
pos_mul(i(L1, Ds1), i(L2, Ds2)) =
    (L1 < L2 ->
        pos_mul_list(Ds1, integer__zero, i(L2, Ds2))
    ;
        pos_mul_list(Ds2, integer__zero, i(L1, Ds1))
    ).

:- func pos_mul_list(list(digit), integer, integer) = integer.
pos_mul_list([], Carry, _Y) = Carry.
pos_mul_list([X|Xs], Carry, Y) =
    pos_mul_list(Xs, pos_plus(mul_base(Carry), mul_by_digit(X, Y)), Y).





:- pred big_quot_rem(integer, integer, integer, integer).
:- mode big_quot_rem(in, in, out, out) is det.
big_quot_rem(N1, N2, Qt, Rm) :-
    (
        big_iszero(N2) ->
            error("integer__big_quot_rem: division by zero")
    ;
        big_iszero(N1) ->
            Qt = integer__zero,
            Rm = integer__zero
    ;
	N1 = i(S1, _),
	N2 = i(S2, _),
	quot_rem(big_abs(N1), big_abs(N2), Q, R),
        Qt = big_sign(S1*S2, Q),
        Rm = big_sign(S1, R)
    ).

% Algorithm: We take digits from the start of U (call them Ur)
% and divide by V to get a digit Q of the ratio.
% Essentially the usual long division algorithm.
% Qhat is an approximation to Q. It may be at most 2 too big.
%
% If the first digit of V is less than base/2, then
% we scale both the numerator and denominator. This
% way, we can use Knuth's[*] nifty trick for finding
% an accurate approximation to Q. That's all we use from
% Knuth; his MIX algorithm is fugly.
%
% [*] Knuth, Semi-numerical algorithms.
%
:- pred quot_rem(integer, integer, integer, integer).
:- mode quot_rem(in, in, out, out) is det.
quot_rem(U, V, Qt, Rm) :-
    (U = i(_, [UI]), V = i(_, [VI]) ->
        Qt = shortint_to_integer(UI // VI),
        Rm = shortint_to_integer(UI rem VI)
    ;
        V0 = head(V),
        ( V0 < basediv2 ->
            M = base div (V0 + 1),
            quot_rem_2(integer__zero,
        	       mul_by_digit(M, U),
        	       mul_by_digit(M, V), QtZeros, R),
            Rm = div_by_digit(M, R)
        ;
            quot_rem_2(integer__zero, U, V, QtZeros, Rm)
        ),
        Qt = decap(QtZeros)
    ).

:- pred quot_rem_2(integer, integer, integer, integer, integer).
:- mode quot_rem_2(in, in, in, out, out) is det.
quot_rem_2(Ur, U, V, Qt, Rm) :-
    (pos_lt(Ur, V) ->
	(
            U = i(_, [Ua | _]) ->
                quot_rem_2(integer_append(Ur, Ua), tail(U), V, Quot, Rem),
                Qt = integer_prepend(0, Quot),
                Rm = Rem
        ;
            Qt = i(1, [0]),
            Rm = Ur
	)
    ;
	(length(Ur) > length(V) ->
	    Qhat = (head(Ur) * base + head_tail(Ur)) div head(V)
        ;
            Qhat = head(Ur) div head(V)
	),
        QhatByV = mul_by_digit(Qhat, V),
	(pos_geq(Ur, QhatByV) ->
            Q = Qhat,
            QByV = QhatByV
	;
            QhatMinus1ByV = pos_minus(QhatByV, V),
            (pos_geq(Ur, QhatMinus1ByV) ->
        	Q = Qhat - 1,
                QByV = QhatMinus1ByV
            ;
        	Q = Qhat - 2,
                QByV = pos_minus(QhatMinus1ByV, V)
            )
	),
	NewUr = pos_minus(Ur, QByV),
	(
            U = i(_, [Ua | _]) ->
                quot_rem_2(integer_append(NewUr, Ua), tail(U), V, Quot, Rem),
                Qt = integer_prepend(Q, Quot),
                Rm = Rem
        ;
            Qt = i(1, [Q]),
            Rm = NewUr
	)
    ).

:- func length(integer) = int.
length(i(L, _)) = L.

:- func decap(integer) = integer.
decap(i(_, [])) = integer__zero.
decap(i(L, [H | T])) =
    (H = 0 ->
        decap(i(L - 1, T))
    ;
        i(L, [H | T])
    ).

:- func head(integer) = digit.
head(I) = H :-
    (I = i(_, [Hd|_T]) ->
	H = Hd
    ;
	error("integer__head: []")
    ).

:- func head_tail(integer) = digit.
head_tail(I) = HT :-
    (I = i(_, [_ | [HT1 | _]]) ->
	HT = HT1
    ;
	error("integer__tail: []")
    ).

:- func tail(integer) = integer.
tail(I) = T :-
    (I = i(L, [_ | Tail]) ->
	T = i(L - 1, Tail)
    ;
	error("integer__tail: []")
    ).

:- func integer_append(integer, digit) = integer.
integer_append(i(L, List), Digit) = i(L + 1, NewList) :-
    list__append(List, [Digit], NewList).


:- func integer_prepend(digit, integer) = integer.
integer_prepend(Digit, i(L, List)) = i(L + 1, [Digit | List]).


:- func div_by_digit(digit, integer) = integer.
div_by_digit(_D, i(_, [])) = integer__zero.
div_by_digit(D, i(_, [X|Xs])) = div_by_digit_1(X, Xs, D).

:- func div_by_digit_1(digit, list(digit), digit) = integer.
div_by_digit_1(X, [], D) = Integer :-
    Q = X div D,
    (Q = 0 ->
        Integer = integer__zero
    ;
        Integer = i(1, [Q])
    ).
div_by_digit_1(X, [H|T], D) = Integer :-
    Q = X div D,
    (Q = 0 ->
        Integer = div_by_digit_1((X rem D) * base + H, T, D)
    ;
        i(L, Ds) = div_by_digit_2((X rem D) * base + H, T, D),
        Integer = i(L + 1, [Q | Ds])
    ).

:- func div_by_digit_2(digit, list(digit), digit) = integer.
div_by_digit_2(X, [], D) = i(1, [X div D]).
div_by_digit_2(X, [H|T], D) = i(Len + 1, [X div D | Tail]) :-
    i(Len, Tail) = div_by_digit_2((X rem D) * base + H, T, D).


:- pred pos_lt(integer, integer).
:- mode pos_lt(in, in) is semidet.
pos_lt(Xs, Ys) :-
    (<) = pos_cmp(Xs, Ys).

:- pred pos_geq(integer, integer).
:- mode pos_geq(in, in) is semidet.
pos_geq(Xs, Ys) :-
    C = pos_cmp(Xs, Ys),
    ( C = (>) ; C = (=) ).


integer__pow(A, N, P) :-
    ( big_isnegative(N) ->
	error("integer__pow: negative exponent")
    ;
	P = big_pow(A, N)
    ).

:- func big_pow(integer, integer) = integer.
big_pow(A, N) =
    (
        N = integer__zero ->
            integer__one
    ;
        N = integer__one ->
            A
    ;
        A = integer__one ->
            integer__one
    ;
        A = integer__zero ->
            integer__zero
    ;
        N = i(_, [Head | Tail]) ->
            bits_pow_list(Tail, A, bits_pow_head(Head, A))
    ;
        integer__zero
    ).

:- func bits_pow_head(int, integer) = integer.
bits_pow_head(H, A) =
    (
        H = 0 ->
            integer__one
    ;
        H /\ lowbitmask = 1 ->
            A * bits_pow_head(H /\ evenmask, A)
    ;
        big_sqr(bits_pow_head(H >> 1, A))
    ).

:- func bits_pow_list(list(int), integer, integer) = integer.
bits_pow_list([], _, Accum) = Accum.
bits_pow_list([H | T], A, Accum) =
    bits_pow_list(T, A, bits_pow(log2base, H, A, Accum)).

:- func bits_pow(int, int, integer, integer) = integer.
bits_pow(Shifts, H, A, Accum) =
    (
        Shifts =< 0 ->
            Accum
    ;
        H /\ lowbitmask = 1 ->
            A * bits_pow(Shifts, H /\ evenmask, A, Accum)
    ;
        big_sqr(bits_pow(Shifts - 1, H >> 1, A, Accum))
    ).


:- func big_sqr(integer) = integer.
big_sqr(A) = A * A.


%:- func integer__float(integer) = float.
integer__float(i(_, List)) = float_list(FBase, 0.0, List) :-
    int__to_float(base, FBase).

:- func float_list(float, float, list(int)) = float.
float_list(_, Accum, []) = Accum.
float_list(FBase, Accum, [H | T]) =
    float_list(FBase, Accum * FBase + FH, T) :-
        int__to_float(H, FH).

 
%:- func integer__int(integer) = int.
integer__int(Integer) = Int :-
    ( Integer >= integer(int__min_int), Integer =< integer(int__max_int) ->
    	Integer = i(_Sign, List),
	Int = int_list(List, 0)
    ;
        error("integer__int: domain error (conversion would overflow)")
    ).


:- func int_list(list(int), int) = int.
int_list([], Accum) = Accum.
int_list([H|T], Accum) = int_list(T, Accum * base + H).

:- func integer__zero = integer.
integer__zero = i(0, []).

:- func integer__one = integer.
integer__one = i(1, [1]).

%===========================================================================
% Reading

%:- func integer__from_string(string) = integer.
%:- mode integer__from_string(in) = out is semidet.
integer__from_string(S) = Big :-
    string__to_char_list(S, Cs),
    string_to_integer(Cs) = Big.

:- func string_to_integer(list(char)) = integer.
:- mode string_to_integer(in) = out is semidet.
string_to_integer(CCs) = Result :-
    CCs = [C|Cs],
    (C = ('-') ->
	Result = big_sign(-1, string_to_integer(Cs))
    ;
        Result = string_to_integer_acc(CCs, integer__zero)
    ).


:- func string_to_integer_acc(list(char), integer) = integer.
:- mode string_to_integer_acc(in, in) = out is semidet.
string_to_integer_acc([], Acc) = Acc.
string_to_integer_acc([C|Cs], Acc) = Result :-
    (char__is_digit(C) ->
	char__to_int(C, D1),
	char__to_int('0', Z),
	Dig = pos_int_to_digits(D1 - Z),
	NewAcc = pos_plus(Dig, mul_by_digit(10, Acc)),
	Result = string_to_integer_acc(Cs, NewAcc)
    ;
	fail
    ).


%===========================================================================
% Writing

%:- func integer__to_string(integer) = string.
integer__to_string(i(Sign, Ds)) = Str :-
    (Sign < 0 ->
	Sgn = "-",
        neg_list(Ds, AbsDs, [])
    ;
	Sgn = "",
        Ds = AbsDs
    ),
    string__append(Sgn, digits_to_string(AbsDs), Str).



:- func digits_to_string(list(digit)) = string.
digits_to_string(DDs) = Str :-
    (DDs = [] ->
        Str = "0"
    ;
        printbase_rep(printbase_pos_int_to_digits(base),
                      DDs, i(_, DDsInPrintBase)),
        (DDsInPrintBase = [Head | Tail] ->
            string__int_to_string(Head, SHead),
            digits_to_strings(Tail, Ss, []),
            string__append_list([SHead | Ss], Str)
        ;
            Str = "Woops"
        )
    ).

:- pred digits_to_strings(list(digit)::in,
                          list(string)::out, list(string)::in)
   is det.
digits_to_strings([]) -->
    [].
digits_to_strings([H | T]) -->
    { digit_to_string(H, S) },
    [ S ],
    digits_to_strings(T).

:- pred printbase_rep(integer::in, list(digit)::in, integer::out)
   is det.
printbase_rep(Base, Digits, printbase_rep_1(Digits, Base, integer__zero)).

:- func printbase_rep_1(list(digit), integer, integer) = integer.
printbase_rep_1([], _Base, Carry) = Carry.
printbase_rep_1([X|Xs], Base, Carry) =
    printbase_rep_1(Xs, Base,
                    printbase_pos_plus(printbase_pos_mul(Base, Carry),
                                       printbase_pos_int_to_digits(X))).


:- pred digit_to_string(digit, string).
:- mode digit_to_string(in, out) is det.
digit_to_string(D, S) :-
    string__int_to_string(D, S1),
    string__pad_left(S1, '0', log10printbase, S).


%=========================================================
% Essentially duplicated code to work in base `printbase' follows

:- func printbase = int.
printbase = 10000.

:- func log10printbase = int.
log10printbase = 4.

:- func printbase_pos_int_to_digits(int) = integer.
printbase_pos_int_to_digits(D) =
    printbase_pos_int_to_digits_2(D, integer__zero).

:- func printbase_pos_int_to_digits_2(int, integer) = integer.
printbase_pos_int_to_digits_2(D, Tail) = Result :-
    (D = 0 ->
	Result = Tail
    ;
        Tail = i(Length, Digits),
        printbase_chop(D, Div, Mod),
        Result = printbase_pos_int_to_digits_2(Div,
                                               i(Length + 1, [Mod | Digits]))
    ).


:- pred printbase_chop(int, digit, digit).
:- mode printbase_chop(in, out, out) is det.
printbase_chop(N, Div, Mod) :-
    Mod = N mod printbase,
    Div = N div printbase.


:- func printbase_mul_by_digit(digit, integer) = integer.
printbase_mul_by_digit(D, i(Len, Ds)) = Out :-
    printbase_mul_by_digit_2(D, Div, Ds, DsOut),
    (Div = 0 ->
	Out = i(Len, DsOut)
    ;
	Out = i(Len + 1, [Div | DsOut])
    ).

:- pred printbase_mul_by_digit_2(digit::in, digit::out,
                                 list(digit)::in, list(digit)::out)
   is det.
printbase_mul_by_digit_2(_, 0, [], []).
printbase_mul_by_digit_2(D, Div, [X | Xs], [Mod | NewXs]) :-
    printbase_mul_by_digit_2(D, DivXs, Xs, NewXs),
    printbase_chop(D * X + DivXs, Div, Mod).


:- func printbase_pos_plus(integer, integer) = integer.
printbase_pos_plus(i(L1, D1), i(L2, D2)) = Out :-
    printbase_add_pairs(Div, i(L1, D1), i(L2, D2), Ds),
    (L1 > L2 ->
        Len = L1
    ;
        Len = L2
    ),
    (Div = 0 ->
	Out = i(Len, Ds)
    ;
	Out = i(Len + 1, [Div | Ds])
    ).

:- pred printbase_add_pairs(digit::out, integer::in, integer::in,
                            list(digit)::out)
   is det.
printbase_add_pairs(Div, i(L1, D1), i(L2, D2), Ds) :-
    (
        L1 = L2 ->
            printbase_add_pairs_equal(Div, D1, D2, Ds)
    ;
        L1 < L2, D2 = [H2 | T2] ->
            printbase_add_pairs(Div1, i(L1, D1), i(L2 - 1, T2), Ds1),
            printbase_chop(H2 + Div1, Div, Mod),
            Ds = [Mod | Ds1]
    ;
        L1 > L2, D1 = [H1 | T1] ->
            printbase_add_pairs(Div1, i(L1 - 1, T1), i(L2, D2), Ds1),
            printbase_chop(H1 + Div1, Div, Mod),
            Ds = [Mod | Ds1]
    ;
	error("integer__printbase_add_pairs")
    ).

:- pred printbase_add_pairs_equal(digit::out, list(digit)::in, list(digit)::in,
                                  list(digit)::out)
   is det.
printbase_add_pairs_equal(0, [], _, []).
printbase_add_pairs_equal(Div, [X | Xs], [Y | Ys], [Mod | TailDs]) :-
    printbase_add_pairs_equal(DivTail, Xs, Ys, TailDs),
    printbase_chop(X + Y + DivTail, Div, Mod).
printbase_add_pairs_equal(0, [_ | _], [], []).


:- func printbase_pos_mul(integer, integer) = integer.
printbase_pos_mul(i(L1, Ds1), i(L2, Ds2)) =
    (L1 < L2 ->
        printbase_pos_mul_list(Ds1, integer__zero, i(L2, Ds2))
    ;
        printbase_pos_mul_list(Ds2, integer__zero, i(L1, Ds1))
    ).

:- func printbase_pos_mul_list(list(digit), integer, integer) = integer.
printbase_pos_mul_list([], Carry, _Y) = Carry.
printbase_pos_mul_list([X|Xs], Carry, Y) =
    printbase_pos_mul_list(Xs,
                           printbase_pos_plus(mul_base(Carry),
                                              printbase_mul_by_digit(X, Y)),
                           Y).

