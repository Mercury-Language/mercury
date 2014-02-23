%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2000, 2003-2007, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: integer.m.
% Main authors: aet, Dan Hazel <odin@svrc.uq.edu.au>.
% Stability: high.
%
% Implements an arbitrary precision integer type and basic
% operations on it. (An arbitrary precision integer may have
% any number of digits, unlike an int, which is limited to the
% precision of the machine's int type, which is typically 32 bits.)
%
% NOTE: All operators behave as the equivalent operators on ints do.
% This includes the division operators: / // rem div mod.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module integer.
:- interface.

:- type integer.

:- pred '<'(integer::in, integer::in) is semidet.

:- pred '>'(integer::in, integer::in) is semidet.

:- pred '=<'(integer::in, integer::in) is semidet.

:- pred '>='(integer::in, integer::in) is semidet.

:- func integer.integer(int) = integer.

:- func integer.to_string(integer) = string.

:- func integer.from_string(string::in) = (integer::out) is semidet.

:- func integer.det_from_string(string) = integer.

    % Convert a string in the specified base (2-36) to an integer.
    % The string must contain one or more digits in the specified base,
    % optionally preceded by a plus or minus sign.  For bases > 10, digits
    % 10 to 35 are represented by the letters A-Z or a-z.  If the string
    % does not match this syntax then the function fails.
    %
:- func integer.from_base_string(int, string) = integer is semidet.

    % As above but throws an exception rather than failing.
    %
:- func integer.det_from_base_string(int, string) = integer.

:- func '+'(integer) = integer.

:- func '-'(integer) = integer.

:- func integer + integer = integer.

:- func integer - integer = integer.

:- func integer * integer = integer.

:- func integer // integer = integer.

:- func integer div integer = integer.

:- func integer rem integer = integer.

:- func integer mod integer = integer.

    % divide_with_rem(X, Y, Q, R) where Q = X // Y and R = X rem Y
    % where both answers are calculated at the same time.
    %
:- pred divide_with_rem(integer::in, integer::in,
    integer::out, integer::out) is det.

:- func integer << int = integer.

:- func integer >> int = integer.

:- func integer /\ integer = integer.

:- func integer \/ integer = integer.

:- func integer `xor` integer = integer.

:- func \ integer = integer.

:- func integer.abs(integer) = integer.

:- func integer.pow(integer, integer) = integer.

:- func integer.float(integer) = float.
:- func integer.int(integer) = int.

:- func integer.zero = integer.

:- func integer.one = integer.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

% Possible improvements:
%
% 1) allow negative digits (-base+1 .. base-1) in lists of
%    digits and normalise only when printing. This would
%    probably simplify the division algorithm, also.
%    (djh: this is not really done although -ve integers include a list
%    of -ve digits for faster comparison and so that normal mercury
%    sorting produces an intuitive order)
%
% 2) alternatively, instead of using base=10000, use *all* the
%    bits in an int and make use of the properties of machine
%    arithmetic. Base 10000 doesn't use even half the bits
%    in an int, which is inefficient. (Base 2^14 would be
%    a little better but would require a slightly more
%    complex case conversion on reading and printing.)
%    (djh: this is done)
%
% 3) Use an O(n^(3/2)) algorithm for multiplying large
%    integers, rather than the current O(n^2) method.
%    There's an obvious divide-and-conquer technique,
%    Karatsuba multiplication. (this is done)
%
% 4) We could overload operators so that we can have mixed operations
%    on ints and integers. For example, "integer(1)+3". This
%    would obviate most calls of integer().
%
% 5) Use double-ended lists rather than simple lists. This
%    would improve the efficiency of the division algorithm,
%    which reverse lists.
%    (djh: this is obsolete - digits lists are now in normal order)
%
% 6) Add bit operations (XOR, AND, OR, etc). We would treat
%    the integers as having a 2's complement bit representation.
%    This is easier to do if we use base 2^14 as mentioned above.
%    (djh: this is done:  /\ \/ << >> xor \)
%
% 7) The implementation of `div' is slower than it need be.
%    (djh: this is much improved)
%
% 8) Fourier methods such as Schoenhage-Strassen and
%    multiplication via modular arithmetic are left as
%    exercises to the reader. 8^)
%
% Of the above, 1) would have the best bang-for-buck, 5) would
% benefit division and remainder operations quite a lot, and 3)
% would benefit large multiplications (thousands of digits)
% and is straightforward to implement.
%
% (djh: I'd like to see 1) done. integers are now represented as
% i(Length, Digits) where Digits are no longer reversed.
% The only penalty for not reversing is in multiplication
% by the base which now entails walking to the end of the list
% to append a 0. Therefore I'd like to see:
%
% 9) Allow empty tails for low end zeros.
%    Base multiplication is then an increment to Length.

:- type sign == int.    % sign of integer and length of digit list
:- type digit == int.   % base 2^14 digit

:- type integer
    --->    i(sign, list(digit)).

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

'<'(X, Y) :-
    big_cmp(X, Y) = C,
    C = (<).

'>'(X, Y) :-
    big_cmp(X, Y) = C,
    C = (>).

'=<'(X, Y) :-
    big_cmp(X, Y) = C,
    ( C = (<) ; C = (=)).

'>='(X, Y) :-
    big_cmp(X, Y) = C,
    ( C = (>) ; C = (=)).

'+'(X) = X.

'-'(X) = big_neg(X).

X + Y = big_plus(X, Y).

X - Y = big_plus(X, big_neg(Y)).

X * Y = big_mul(X, Y).

X div Y = big_div(X, Y).

X // Y = big_quot(X, Y).

X rem Y = big_rem(X, Y).

X mod Y = big_mod(X, Y).

divide_with_rem(X, Y, Quotient, Remainder) :-
    big_quot_rem(X, Y, Quotient, Remainder).

X << I = ( I > 0 -> big_left_shift(X, I) ; I < 0 -> X >> -I ; X ).

X >> I = ( I < 0 -> X << -I ; I > 0 -> big_right_shift(X, I) ; X ).

X /\ Y =
    ( big_isnegative(X) ->
        ( big_isnegative(Y) ->
            \ big_or(\ X, \ Y)
        ;
            big_and_not(Y, \ X)
        )
    ; big_isnegative(Y) ->
        big_and_not(X, \ Y)
    ;
        big_and(X, Y)
    ).

X \/ Y =
    ( big_isnegative(X) ->
        ( big_isnegative(Y) ->
            \ big_and(\ X, \ Y)
        ;
            \ big_and_not(\ X, Y)
        )
    ; big_isnegative(Y) ->
        \ big_and_not(\ Y, X)
    ;
        big_or(X, Y)
    ).

X `xor` Y =
    ( big_isnegative(X) ->
        ( big_isnegative(Y) ->
            big_xor(\ X, \ Y)
        ;
            big_xor_not(Y, \ X)
        )
    ; big_isnegative(Y) ->
        big_xor_not(X, \ Y)
    ;
        big_xor(X, Y)
    ).

\ X = big_neg(big_plus(X, integer.one)).

integer.abs(N) = big_abs(N).

:- func big_abs(integer) = integer.

big_abs(i(Sign, Ds)) = ( Sign < 0 -> big_neg(i(Sign, Ds)) ; i(Sign, Ds) ).

:- pred neg_list(list(int)::in, list(int)::out) is det.

neg_list([], []).
neg_list([H | T], [-H | NT]) :-
    neg_list(T, NT).

:- pred big_isnegative(integer::in) is semidet.

big_isnegative(i(Sign, _)) :- Sign < 0.

:- pred big_iszero(integer::in) is semidet.

big_iszero(i(0, [])).

:- func big_neg(integer) = integer.

big_neg(i(S, Digits0)) = i(-S, Digits) :-
    neg_list(Digits0, Digits).

:- func big_mul(integer, integer) = integer.

big_mul(X, Y) =
    big_sign(integer_signum(X) * integer_signum(Y),
        pos_mul(big_abs(X), big_abs(Y))).

:- func big_sign(int, integer) = integer.

big_sign(Sign, In) = ( Sign < 0 -> big_neg(In) ; In ).

:- func big_quot(integer, integer) = integer.

big_quot(X, Y) = Quot :-
    big_quot_rem(X, Y, Quot, _Rem).

:- func big_rem(integer, integer) = integer.

big_rem(X, Y) = Rem :-
    big_quot_rem(X, Y, _Quot, Rem).

:- func big_div(integer, integer) = integer.

big_div(X, Y) = Div :-
    big_quot_rem(X, Y, Trunc, Rem),
    ( integer_signum(Y) * integer_signum(Rem) < 0 ->
        Div = Trunc - integer.one
    ;
        Div = Trunc
    ).

:- func big_mod(integer, integer) = integer.

big_mod(X, Y) = Mod :-
    big_quot_rem(X, Y, _Trunc, Rem),
    ( integer_signum(Y) * integer_signum(Rem) < 0 ->
        Mod = Rem + Y
    ;
        Mod = Rem
    ).

:- func big_right_shift(integer, int) = integer.

big_right_shift(X, I) =
    ( big_iszero(X) ->
        X
    ; big_isnegative(X) ->
        \ pos_right_shift(\ X, I)
    ;
        pos_right_shift(X, I)
    ).

:- func pos_right_shift(integer, int) = integer.

pos_right_shift(i(Len, Digits), I) = Integer :-
    Div = I div log2base,
    ( Div < Len ->
        Mod = I mod log2base,
        Integer = decap(rightshift(Mod, log2base - Mod,
            i(Len - Div, Digits), 0))
    ;
        Integer = integer.zero
    ).

:- func rightshift(int, int, integer, int) = integer.

rightshift(_Mod, _InvMod, i(_Len, []), _Carry) = integer.zero.
rightshift(Mod, InvMod, i(Len, [H | T]), Carry) = Integer :-
    ( Len =< 0 ->
        Integer = integer.zero
    ;
        NewH = Carry \/ (H >> Mod),
        NewCarry = (H /\ (basemask >> InvMod)) << InvMod,
        i(TailLen, NewTail) = rightshift(Mod, InvMod, i(Len - 1, T),
            NewCarry),
        Integer = i(TailLen + 1, [NewH | NewTail])
    ).

:- func big_left_shift(integer, int) = integer.

big_left_shift(X, I) =
    ( big_iszero(X) ->
        X
    ; big_isnegative(X) ->
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
    ( Carry = 0 ->
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
    ( Len =< 0 ->
        Carry = 0,
        DigitsOut = []
    ;
        Carry = (H /\ (basemask << InvMod)) >> InvMod,
        leftshift(Mod, InvMod, Len - 1, T, TailCarry, Tail),
        DigitsOut = [TailCarry \/ ((H << Mod) /\ basemask) | Tail]
    ).

:- pred zeros(int::in, list(digit)::out, list(digit)::in) is det.

zeros(Len) -->
    ( { Len > 0 } ->
        [0],
        zeros(Len - 1)
    ;
        []
    ).

:- func big_or(integer, integer) = integer.

big_or(X, Y) = decap(or_pairs(X, Y)).

:- func or_pairs(integer, integer) = integer.

or_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    ( L1 = L2 ->
        Integer = i(L1, or_pairs_equal(D1, D2))
    ; L1 < L2, D2 = [H2 | T2] ->
        i(_, DsT) = or_pairs(i(L1, D1), i(L2 - 1, T2)),
        Integer = i(L2, [H2 | DsT])
    ; L1 > L2, D1 = [H1 | T1] ->
        i(_, DsT) = or_pairs(i(L1 - 1, T1), i(L2, D2)),
        Integer = i(L1, [H1 | DsT])
    ;
        error("integer.or_pairs")
    ).

:- func or_pairs_equal(list(digit), list(digit)) = list(digit).

or_pairs_equal([], _) = [].
or_pairs_equal([_ | _], []) = [].
or_pairs_equal([X | Xs], [Y | Ys]) = [X \/ Y | or_pairs_equal(Xs, Ys)].

:- func big_xor(integer, integer) = integer.

big_xor(X, Y) = decap(xor_pairs(X, Y)).

:- func xor_pairs(integer, integer) = integer.

xor_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    ( L1 = L2 ->
        Integer = i(L1, xor_pairs_equal(D1, D2))
    ; L1 < L2, D2 = [H2 | T2] ->
        i(_, DsT) = xor_pairs(i(L1, D1), i(L2 - 1, T2)),
        Integer = i(L2, [H2 | DsT])
    ; L1 > L2, D1 = [H1 | T1] ->
        i(_, DsT) = xor_pairs(i(L1 - 1, T1), i(L2, D2)),
        Integer = i(L1, [H1 | DsT])
    ;
        error("integer.xor_pairs")
    ).

:- func xor_pairs_equal(list(digit), list(digit)) = list(digit).

xor_pairs_equal([], _) = [].
xor_pairs_equal([_ | _], []) = [].
xor_pairs_equal([X | Xs], [Y | Ys]) =
    [int.xor(X, Y) | xor_pairs_equal(Xs, Ys)].

:- func big_and(integer, integer) = integer.

big_and(X, Y) = decap(and_pairs(X, Y)).

:- func and_pairs(integer, integer) = integer.

and_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    ( L1 = L2 ->
        Integer = i(L1, and_pairs_equal(D1, D2))
    ; L1 < L2, D2 = [_ | T2] ->
        i(_, DsT) = and_pairs(i(L1, D1), i(L2 - 1, T2)),
        Integer = i(L1, DsT)
    ; L1 > L2, D1 = [_ | T1] ->
        i(_, DsT) = and_pairs(i(L1 - 1, T1), i(L2, D2)),
        Integer = i(L2, DsT)
    ;
        error("integer.and_pairs")
    ).

:- func and_pairs_equal(list(digit), list(digit)) = list(digit).

and_pairs_equal([], _) = [].
and_pairs_equal([_ | _], []) = [].
and_pairs_equal([X | Xs], [Y | Ys]) = [X /\ Y | and_pairs_equal(Xs, Ys)].

:- func big_and_not(integer, integer) = integer.

big_and_not(X, Y) = decap(and_not_pairs(X, Y)).

:- func and_not_pairs(integer, integer) = integer.

and_not_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    ( L1 = L2 ->
        Integer = i(L1, and_not_pairs_equal(D1, D2))
    ; L1 < L2, D2 = [_ | T2] ->
        i(_, DsT) = and_not_pairs(i(L1, D1), i(L2 - 1, T2)),
        Integer = i(L1, DsT)
    ; L1 > L2, D1 = [H1 | T1] ->
        i(_, DsT) = and_not_pairs(i(L1 - 1, T1), i(L2, D2)),
        Integer = i(L1, [H1 | DsT])
    ;
        error("integer.and_not_pairs")
    ).

:- func and_not_pairs_equal(list(digit), list(digit)) = list(digit).

and_not_pairs_equal([], _) = [].
and_not_pairs_equal([_ | _], []) = [].
and_not_pairs_equal([X | Xs], [Y | Ys]) =
    [X /\ \ Y | and_not_pairs_equal(Xs, Ys)].

:- func big_xor_not(integer, integer) = integer.

big_xor_not(X1, NotX2) =
    \ big_and_not(big_or(X1, NotX2), big_and(X1, NotX2)).

:- func big_cmp(integer, integer) = comparison_result.

big_cmp(X, Y) = Result :-
    compare(Result, X, Y).

:- func pos_cmp(integer, integer) = comparison_result.

pos_cmp(X, Y) = Result :-
    compare(Result, X, Y).

:- func big_plus(integer, integer) = integer.

big_plus(X, Y) = Sum :-
    ( X = integer.zero ->
        Sum = Y
    ; Y = integer.zero ->
        Sum = X
    ;
        AbsX = big_abs(X),
        AbsY = big_abs(Y),
        SignX = integer_signum(X),
        SignY = integer_signum(Y),
        ( SignX = SignY ->
            Sum = big_sign(SignX, pos_plus(AbsX, AbsY))
        ;
            C = pos_cmp(AbsX, AbsY),
            (
                C = (<),
                Sum = big_sign(SignY, pos_minus(AbsY, AbsX))
            ;
                C = (>),
                Sum = big_sign(SignX, pos_minus(AbsX, AbsY))
            ;
                C = (=),
                Sum = integer.zero
            )
        )
    ).

integer(N) = int_to_integer(N).

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
    ( D = 0 ->
        Int = integer.zero
    ; D > 0, D < base ->
        Int = i(1, [D])
    ; D < 0, D > -base ->
        Int = i(-1, [D])
    ;
        ( int.min_int(D) ->
            % were we to call int.abs, int overflow might occur.
            Int = integer(D + 1) - integer.one
        ;
            Int = big_sign(D, pos_int_to_digits(int.abs(D)))
        )
    ).

:- func shortint_to_integer(int) = integer.

shortint_to_integer(D) =
    ( D = 0 -> integer.zero ; D > 0 -> i(1, [D]) ; i(-1, [D]) ).

:- func signum(int) = int.

signum(N) = ( N < 0 -> -1 ; N = 0 -> 0 ; 1 ).

:- func integer_signum(integer) = int.

integer_signum(i(Sign, _)) = signum(Sign).

:- func pos_int_to_digits(int) = integer.

pos_int_to_digits(D) = pos_int_to_digits_2(D, integer.zero).

:- func pos_int_to_digits_2(int, integer) = integer.

pos_int_to_digits_2(D, Tail) = Result :-
    ( D = 0 ->
        Result = Tail
    ;
        Tail = i(Length, Digits),
        chop(D, Div, Mod),
        Result = pos_int_to_digits_2(Div,
            i(Length + 1, [Mod | Digits]))
    ).

:- func mul_base(integer) = integer.

mul_base(i(Len, Digits)) = Result :-
    (
        Digits = [],
        Result = integer.zero
    ;
        Digits = [_ | _],
        Result = i(Len + 1, mul_base_2(Digits))
    ).

:- func mul_base_2(list(digit)) = list(digit).

mul_base_2([]) = [0].
mul_base_2([H | T]) = [H | mul_base_2(T)].

:- func mul_by_digit(digit, integer) = integer.

mul_by_digit(Digit, i(Len, Digits0)) = Out :-
    mul_by_digit_2(Digit, Mod, Digits0, Digits),
    Out = ( Mod = 0 -> i(Len, Digits) ; i(Len + 1, [Mod | Digits]) ).

:- pred mul_by_digit_2(digit::in, digit::out, list(digit)::in,
    list(digit)::out) is det.

mul_by_digit_2(_, 0, [], []).
mul_by_digit_2(D, Div, [X | Xs], [Mod | NewXs]) :-
    mul_by_digit_2(D, DivXs, Xs, NewXs),
    chop(D * X + DivXs, Div, Mod).

:- pred chop(int::in, digit::out, digit::out) is det.

chop(N, Div, Mod) :-
    Div = N >> log2base,    % i.e. Div = N div base
    Mod = N /\ basemask.    % i.e. Mod = N mod base

:- func pos_plus(integer, integer) = integer.

pos_plus(i(L1, D1), i(L2, D2)) = Out :-
    add_pairs(Div, i(L1, D1), i(L2, D2), Ds),
    Len = ( L1 > L2 -> L1 ; L2 ),
    Out = ( Div = 0 -> i(Len, Ds) ; i(Len + 1, [Div | Ds]) ).

:- pred add_pairs(digit::out, integer::in, integer::in,
    list(digit)::out) is det.

add_pairs(Div, i(L1, D1), i(L2, D2), Ds) :-
    ( L1 = L2 ->
        add_pairs_equal(Div, D1, D2, Ds)
    ; L1 < L2, D2 = [H2 | T2] ->
        add_pairs(Div1, i(L1, D1), i(L2 - 1, T2), Ds1),
        chop(H2 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    ; L1 > L2, D1 = [H1 | T1] ->
        add_pairs(Div1, i(L1 - 1, T1), i(L2, D2), Ds1),
        chop(H1 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    ;
        error("integer.add_pairs")
    ).

:- pred add_pairs_equal(digit::out, list(digit)::in, list(digit)::in,
    list(digit)::out) is det.

add_pairs_equal(0, [], _, []).
add_pairs_equal(0, [_ | _], [], []).
add_pairs_equal(Div, [X | Xs], [Y | Ys], [Mod | TailDs]) :-
    add_pairs_equal(DivTail, Xs, Ys, TailDs),
    chop(X + Y + DivTail, Div, Mod).

:- func pos_minus(integer, integer) = integer.

pos_minus(i(L1, D1), i(L2, D2)) = Out :-
    diff_pairs(Mod, i(L1, D1), i(L2, D2), Ds),
    Len = ( L1 > L2 -> L1 ; L2 ),
    Out = ( Mod = 0 -> decap(i(Len, Ds)) ; i(Len + 1, [Mod | Ds]) ).

:- pred diff_pairs(digit::out, integer::in, integer::in,
    list(digit)::out) is det.

diff_pairs(Div, i(L1, D1), i(L2, D2), Ds) :-
    ( L1 = L2 ->
        diff_pairs_equal(Div, D1, D2, Ds)
    ; L1 > L2, D1 = [H1 | T1] ->
        diff_pairs(Div1, i(L1 - 1, T1), i(L2, D2), Ds1),
        chop(H1 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    ;
        error("integer.diff_pairs")
    ).

:- pred diff_pairs_equal(digit::out, list(digit)::in, list(digit)::in,
    list(digit)::out) is det.

diff_pairs_equal(0, [], _, []).
diff_pairs_equal(0, [_ | _], [], []).
diff_pairs_equal(Div, [X | Xs], [Y | Ys], [Mod | TailDs]) :-
    diff_pairs_equal(DivTail, Xs, Ys, TailDs),
    chop(X - Y + DivTail, Div, Mod).

:- func pos_mul(integer, integer) = integer.

pos_mul(i(L1, Ds1), i(L2, Ds2)) =
    ( L1 < L2 ->
        pos_mul_karatsuba(i(L1, Ds1), i(L2, Ds2))
    ;
        pos_mul_karatsuba(i(L2, Ds2), i(L1, Ds1))
    ).

% Use quadratic multiplication for less than threshold digits.
:- func karatsuba_threshold = int.

karatsuba_threshold = 35.

% Use parallel execution if number of digits of split numbers is larger
% then this threshold.
:- func karatsuba_parallel_threshold = int.

karatsuba_parallel_threshold = karatsuba_threshold * 10.

% Karatsuba / Toom-2 multiplication in O(n^1.585)
:- func pos_mul_karatsuba(integer, integer) = integer.

pos_mul_karatsuba(i(L1, Ds1), i(L2, Ds2)) = Res :-
    ( L1 < karatsuba_threshold ->
        Res = pos_mul_list(Ds1, integer.zero, i(L2, Ds2))
    ;
        ( L2 < L1 ->
            error("integer.pos_mul_karatsuba: second factor smaller")
        ;
            Middle = L2 div 2,
            HiDigits = L2 - Middle,
            HiDigitsSmall = max(0, L1 - Middle),
            % Split Ds1 in [L1 - Middle];[Middle] digits if L1 > Middle
            % or leave as [L1] digits.
            list.split_upto(HiDigitsSmall, Ds1, Ds1Upper, Ds1Lower),
            % Split Ds2 in [L2 - Middle; Middle] digits.
            list.split_upto(HiDigits, Ds2, Ds2Upper, Ds2Lower),
            LoDs1 = i(min(L1, Middle), Ds1Lower),
            LoDs2 = i(Middle, Ds2Lower),
            HiDs1 = i(HiDigitsSmall, Ds1Upper),
            HiDs2 = i(HiDigits, Ds2Upper),
            ( Middle > karatsuba_parallel_threshold ->
                Res0 = pos_mul(LoDs1, LoDs2) &
                Res1 = pos_mul(LoDs1 + HiDs1, LoDs2 + HiDs2) &
                Res2 = pos_mul(HiDs1, HiDs2)
            ;
                Res0 = pos_mul(LoDs1, LoDs2),
                Res1 = pos_mul(LoDs1 + HiDs1, LoDs2 + HiDs2),
                Res2 = pos_mul(HiDs1, HiDs2)
            )
        ),
        Res = big_left_shift(Res2, 2*Middle*log2base) +
              big_left_shift(Res1 - (Res2 + Res0), Middle*log2base) + Res0
    ).


:- func pos_mul_list(list(digit), integer, integer) = integer.

pos_mul_list([], Carry, _Y) = Carry.
pos_mul_list([X | Xs], Carry, Y) =
    pos_mul_list(Xs, pos_plus(mul_base(Carry), mul_by_digit(X, Y)), Y).

:- pred big_quot_rem(integer::in, integer::in, integer::out,
    integer::out) is det.

big_quot_rem(X, Y, Quot, Rem) :-
    ( big_iszero(Y) ->
        error("integer.big_quot_rem: division by zero")
    ; big_iszero(X) ->
        Quot = integer.zero,
        Rem  = integer.zero
    ;
        X = i(SignX, _),
        Y = i(SignY, _),
        quot_rem(big_abs(X), big_abs(Y), Quot0, Rem0),
        Quot = big_sign(SignX * SignY, Quot0),
        Rem  = big_sign(SignX, Rem0)
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

:- pred quot_rem(integer::in, integer::in, integer::out, integer::out) is det.

quot_rem(U, V, Quot, Rem) :-
    ( U = i(_, [UI]), V = i(_, [VI]) ->
        Quot = shortint_to_integer(UI // VI),
        Rem  = shortint_to_integer(UI rem VI)
    ;
        V0 = head(V),
        ( V0 < basediv2 ->
            M = base div (V0 + 1),
            quot_rem_2(integer.zero, mul_by_digit(M, U),
                mul_by_digit(M, V), QuotZeros, R),
            Rem = div_by_digit(M, R)
        ;
            quot_rem_2(integer.zero, U, V, QuotZeros, Rem)
        ),
        Quot = decap(QuotZeros)
    ).

:- pred quot_rem_2(integer::in, integer::in, integer::in, integer::out,
    integer::out) is det.

quot_rem_2(Ur, U, V, Quot, Rem) :-
    ( pos_lt(Ur, V) ->
        ( U = i(_, [Ua | _]) ->
            quot_rem_2(integer_append(Ur, Ua), tail(U), V,
                Quot0, Rem0),
            Quot = integer_prepend(0, Quot0),
            Rem = Rem0
        ;
            Quot = i(1, [0]),
            Rem = Ur
        )
    ;
        ( length(Ur) > length(V) ->
            Qhat = (head(Ur) * base + head_tail(Ur)) div head(V)
        ;
            Qhat = head(Ur) div head(V)
        ),
        QhatByV = mul_by_digit(Qhat, V),
        ( pos_geq(Ur, QhatByV) ->
            Q = Qhat,
            QByV = QhatByV
        ;
            QhatMinus1ByV = pos_minus(QhatByV, V),
            ( pos_geq(Ur, QhatMinus1ByV) ->
                Q = Qhat - 1,
                QByV = QhatMinus1ByV
            ;
                Q = Qhat - 2,
                QByV = pos_minus(QhatMinus1ByV, V)
            )
        ),
        NewUr = pos_minus(Ur, QByV),
        ( U = i(_, [Ua | _]) ->
            quot_rem_2(integer_append(NewUr, Ua), tail(U), V,
                Quot0, Rem0),
            Quot = integer_prepend(Q, Quot0),
            Rem = Rem0
        ;
            Quot = i(1, [Q]),
            Rem = NewUr
        )
    ).

:- func length(integer) = int.

length(i(L, _)) = L.

:- func decap(integer) = integer.

decap(i(_, [])) = integer.zero.
decap(i(L, [H | T])) = ( H = 0 -> decap(i(L - 1, T)) ; i(L, [H | T]) ).

:- func head(integer) = digit.

head(I) = (I = i(_, [Hd|_T]) ->  Hd ; func_error("integer.head: []") ).

:- func head_tail(integer) = digit.

head_tail(I) =
    ( I = i(_, [_ | [HT | _]]) ->
        HT
    ;
        func_error("integer.head_tail: []")
    ).

:- func tail(integer) = integer.

tail(i(_, [])) = func_error("integer.tail: []").
tail(i(Len, [_ | Tail])) = i(Len - 1, Tail).

:- func integer_append(integer, digit) = integer.

integer_append(i(L, List), Digit) = i(L + 1, NewList) :-
    list.append(List, [Digit], NewList).

:- func integer_prepend(digit, integer) = integer.

integer_prepend(Digit, i(L, List)) = i(L + 1, [Digit | List]).

:- func div_by_digit(digit, integer) = integer.

div_by_digit(_, i(_, [])) = integer.zero.
div_by_digit(Digit, i(_, [X | Xs])) = div_by_digit_1(X, Xs, Digit).

:- func div_by_digit_1(digit, list(digit), digit) = integer.

div_by_digit_1(X, [], D) = ( Q = 0 -> integer.zero ; i(1, [Q]) ) :-
    Q = X div D.
div_by_digit_1(X, [H | T], D) = Integer :-
    Q = X div D,
    ( Q = 0 ->
        Integer = div_by_digit_1((X rem D) * base + H, T, D)
    ;
        i(L, Ds) = div_by_digit_2((X rem D) * base + H, T, D),
        Integer = i(L + 1, [Q | Ds])
    ).

:- func div_by_digit_2(digit, list(digit), digit) = integer.

div_by_digit_2(X, [], D) = i(1, [X div D]).
div_by_digit_2(X, [H | T], D) = i(Len + 1, [X div D | Tail]) :-
    i(Len, Tail) = div_by_digit_2((X rem D) * base + H, T, D).

:- pred pos_lt(integer::in, integer::in) is semidet.

pos_lt(Xs, Ys) :-
    (<) = pos_cmp(Xs, Ys).

:- pred pos_geq(integer::in, integer::in) is semidet.

pos_geq(Xs, Ys) :-
    C = pos_cmp(Xs, Ys),
    ( C = (>) ; C = (=) ).

integer.pow(A, N) = P :-
    ( big_isnegative(N) ->
        error("integer.pow: negative exponent")
    ;
        P = big_pow(A, N)
    ).

:- func big_pow(integer, integer) = integer.

big_pow(A, N) =
    ( N = integer.zero ->
        integer.one
    ; N = integer.one ->
        A
    ; A = integer.one ->
        integer.one
    ; A = integer.zero ->
        integer.zero
    ; N = i(_, [Head | Tail]) ->
        bits_pow_list(Tail, A, bits_pow_head(Head, A))
    ;
        integer.zero
    ).

:- func bits_pow_head(int, integer) = integer.

bits_pow_head(H, A) =
    ( H = 0 ->
        integer.one
    ; H /\ lowbitmask = 1 ->
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
    ( Shifts =< 0 ->
        Accum
    ; H /\ lowbitmask = 1 ->
        A * bits_pow(Shifts, H /\ evenmask, A, Accum)
    ;
        big_sqr(bits_pow(Shifts - 1, H >> 1, A, Accum))
    ).

:- func big_sqr(integer) = integer.

big_sqr(A) = A * A.

integer.float(i(_, List)) = float_list(float.float(base), 0.0, List).

:- func float_list(float, float, list(int)) = float.

float_list(_, Accum, []) = Accum.
float_list(FBase, Accum, [H | T]) =
    float_list(FBase, Accum * FBase + float.float(H), T).

integer.int(Integer) = Int :-
    (
        Integer >= integer(int.min_int),
        Integer =< integer(int.max_int)
    ->
        Integer = i(_Sign, Digits),
        Int = int_list(Digits, 0)
    ;
        error("integer.int: domain error (conversion would overflow)")
    ).

:- func int_list(list(int), int) = int.

int_list([], Accum) = Accum.
int_list([H | T], Accum) = int_list(T, Accum * base + H).

integer.zero = i(0, []).

integer.one = i(1, [1]).

%-----------------------------------------------------------------------------%
%
% Converting strings to integers.
%

integer.from_string(S) = Big :-
    string.to_char_list(S, Cs),
    string_to_integer(Cs) = Big.

integer.det_from_string(S) =
    ( I = integer.from_string(S) ->
        I
    ;
        func_error(
        "integer.det_from_string/1: cannot convert to integer.")
    ).

:- func string_to_integer(list(char)::in) = (integer::out) is semidet.

string_to_integer(CCs0 @ [C | Cs]) = Integer :-
    ( C = ('-') ->
        Integer = big_sign(-1, string_to_integer(Cs))
    ;
        CCs = ( C = ('+') -> Cs ; CCs0),
        Integer = string_to_integer_acc(CCs, integer.zero)
    ).

:- func string_to_integer_acc(list(char)::in, integer::in) = (integer::out)
    is semidet.

string_to_integer_acc([], Acc) = Acc.
string_to_integer_acc([C | Cs], Acc) = Result :-
        % The if-then-else here is acting as a sequential conjunction.
        % It is needed to guarantee termination with --reorder-conj.
        % Without it, the value of `Digit0 - Z' might be negative and
        % then the call to pos_int_to_digits/1 may not terminate.
    ( char.is_digit(C) ->
        Digit0 = char.to_int(C),
        Z = char.to_int('0'),
        Digit = pos_int_to_digits(Digit0 - Z),
        NewAcc = pos_plus(Digit, mul_by_digit(10, Acc)),
        Result = string_to_integer_acc(Cs, NewAcc)
    ;
        fail
    ).

%-----------------------------------------------------------------------------%
%
% Converting integers to strings.
%

integer.to_string(i(Sign, Digits)) = SignStr ++ digits_to_string(AbsDigits) :-
    ( Sign < 0 ->
        SignStr = "-",
        neg_list(Digits, AbsDigits)
    ;
        SignStr = "",
        Digits = AbsDigits
    ).

:- func digits_to_string(list(digit)) = string.

digits_to_string([]) = "0".
digits_to_string(Digits @ [_ | _]) = Str :-
    printbase_rep(printbase_pos_int_to_digits(base),
        Digits, i(_, DigitsInPrintBase)),
    (
        DigitsInPrintBase = [Head | Tail],
        string.int_to_string(Head, SHead),
        digits_to_strings(Tail, Ss, []),
        string.append_list([SHead | Ss], Str)
    ;
        DigitsInPrintBase = [],
        error("integer.digits_to_string/1: empty list")
    ).

:- pred digits_to_strings(list(digit)::in, list(string)::out,
    list(string)::in) is det.

digits_to_strings([]) --> [].
digits_to_strings([H | T]) -->
    { digit_to_string(H, S) },
    [ S ],
    digits_to_strings(T).

:- pred printbase_rep(integer::in, list(digit)::in, integer::out) is det.

printbase_rep(Base, Digits, printbase_rep_1(Digits, Base, integer.zero)).

:- func printbase_rep_1(list(digit), integer, integer) = integer.

printbase_rep_1([], _Base, Carry) = Carry.
printbase_rep_1([X|Xs], Base, Carry) =
    printbase_rep_1(Xs, Base,
        printbase_pos_plus(printbase_pos_mul(Base, Carry),
        printbase_pos_int_to_digits(X))).

:- pred digit_to_string(digit::in, string::out) is det.

digit_to_string(D, S) :-
    string.int_to_string(D, S1),
    string.pad_left(S1, '0', log10printbase, S).

%-----------------------------------------------------------------------------%
%
% Essentially duplicated code to work in base `printbase' follows
%

:- func printbase = int.

printbase = 10000.

:- func log10printbase = int.

log10printbase = 4.

:- func printbase_pos_int_to_digits(int) = integer.

printbase_pos_int_to_digits(D) =
    printbase_pos_int_to_digits_2(D, integer.zero).

:- func printbase_pos_int_to_digits_2(int, integer) = integer.

printbase_pos_int_to_digits_2(D, Tail) = Result :-
    ( D = 0 ->
        Result = Tail
    ;
        Tail = i(Length, Digits),
        printbase_chop(D, Div, Mod),
        Result = printbase_pos_int_to_digits_2(Div,
            i(Length + 1, [Mod | Digits]))
    ).

:- pred printbase_chop(int::in, digit::out, digit::out) is det.

printbase_chop(N, Div, Mod) :-
    Mod = N mod printbase,
    Div = N div printbase.

:- func printbase_mul_by_digit(digit, integer) = integer.

printbase_mul_by_digit(D, i(Len, Ds)) = Out :-
    printbase_mul_by_digit_2(D, Div, Ds, DsOut),
    Out = ( Div = 0 -> i(Len, DsOut) ; i(Len + 1, [Div | DsOut]) ).

:- pred printbase_mul_by_digit_2(digit::in, digit::out,
    list(digit)::in, list(digit)::out) is det.

printbase_mul_by_digit_2(_, 0, [], []).
printbase_mul_by_digit_2(D, Div, [X | Xs], [Mod | NewXs]) :-
    printbase_mul_by_digit_2(D, DivXs, Xs, NewXs),
    printbase_chop(D * X + DivXs, Div, Mod).

:- func printbase_pos_plus(integer, integer) = integer.

printbase_pos_plus(i(L1, D1), i(L2, D2)) = Out :-
    printbase_add_pairs(Div, i(L1, D1), i(L2, D2), Ds),
    Len = ( L1 > L2 -> L1 ; L2 ),
    Out = ( Div = 0 -> i(Len, Ds) ; i(Len + 1, [Div | Ds]) ).

:- pred printbase_add_pairs(digit::out, integer::in, integer::in,
    list(digit)::out) is det.

printbase_add_pairs(Div, i(L1, D1), i(L2, D2), Ds) :-
    ( L1 = L2 ->
        printbase_add_pairs_equal(Div, D1, D2, Ds)
    ; L1 < L2, D2 = [H2 | T2] ->
        printbase_add_pairs(Div1, i(L1, D1), i(L2 - 1, T2), Ds1),
        printbase_chop(H2 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    ; L1 > L2, D1 = [H1 | T1] ->
        printbase_add_pairs(Div1, i(L1 - 1, T1), i(L2, D2), Ds1),
        printbase_chop(H1 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    ;
        error("integer.printbase_add_pairs")
    ).

:- pred printbase_add_pairs_equal(digit::out, list(digit)::in, list(digit)::in,
    list(digit)::out) is det.

printbase_add_pairs_equal(0, [], _, []).
printbase_add_pairs_equal(0, [_ | _], [], []).
printbase_add_pairs_equal(Div, [X | Xs], [Y | Ys], [Mod | TailDs]) :-
    printbase_add_pairs_equal(DivTail, Xs, Ys, TailDs),
    printbase_chop(X + Y + DivTail, Div, Mod).

:- func printbase_pos_mul(integer, integer) = integer.

printbase_pos_mul(i(L1, Ds1), i(L2, Ds2)) =
    ( L1 < L2 ->
        printbase_pos_mul_list(Ds1, integer.zero, i(L2, Ds2))
    ;
        printbase_pos_mul_list(Ds2, integer.zero, i(L1, Ds1))
    ).

:- func printbase_pos_mul_list(list(digit), integer, integer) = integer.

printbase_pos_mul_list([], Carry, _Y) = Carry.
printbase_pos_mul_list([X|Xs], Carry, Y) =
    printbase_pos_mul_list(Xs, printbase_pos_plus(mul_base(Carry),
        printbase_mul_by_digit(X, Y)), Y).

%-----------------------------------------------------------------------------%

integer.from_base_string(Base0, String) = Integer :-
    string.index(String, 0, Char),
    Base = integer(Base0),
    Len = string.length(String),
    ( Char = ('-') ->
        Len > 1,
        string.foldl_between(accumulate_integer(Base), String, 1, Len,
            integer.zero, N),
        Integer = -N
    ; Char = ('+') ->
        Len > 1,
        string.foldl_between(accumulate_integer(Base), String, 1, Len,
            integer.zero, N),
        Integer = N
    ;
        string.foldl_between(accumulate_integer(Base), String, 0, Len,
            integer.zero, N),
        Integer = N
    ).

:- pred accumulate_integer(integer::in, char::in, integer::in, integer::out)
    is semidet.

accumulate_integer(Base, Char, !N) :-
    char.digit_to_int(Char, Digit0),
    Digit = integer(Digit0),
    Digit < Base,
    !:N = (Base * !.N) + Digit.

integer.det_from_base_string(Base, String) = Integer :-
    ( Integer0 = integer.from_base_string(Base, String) ->
        Integer = Integer0
    ;
        error("integer.det_from_base_string")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
