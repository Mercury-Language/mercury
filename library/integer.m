%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2000, 2003-2007, 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: integer.m.
% Main authors: aet, Dan Hazel <odin@svrc.uq.edu.au>.
% Stability: high.
%
% This modules defines an arbitrary precision integer type (named "integer")
% and basic arithmetic operations on it.
%
% The builtin Mercury type "int" is implemented as machine integers,
% which on virtually all modern machines will be 32 or 64 bits in size.
% If you need to manipulate integers that may not fit into this many bits,
% you will want to use "integer"s instead of "int"s.
%
% NOTE: All the operators we define on "integers" behave the same as the
% corresponding operators on "int"s. This includes the operators related
% to division: /, //, rem, div, and mod.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module integer.
:- interface.

:- type integer.

%---------------------------------------------------------------------------%
%
% Constants.
%

    % Equivalent to integer(-1).
    %
:- func negative_one = integer.

    % Equivalent to integer(0).
    %
:- func zero = integer.

    % Equivalent to integer(1).
    %
:- func one = integer.

    % Equivalent to integer(2).
    %
:- func two = integer.

    % Equivalent to integer(8).
    %
:- func eight = integer.

    % Equivalent to integer(10).
    %
:- func ten = integer.

    % Equivalent to integer(16).
    %
:- func sixteen = integer.

%---------------------------------------------------------------------------%

    % X < Y: Succeed if and only if X is less than Y.
    %
:- pred '<'(integer::in, integer::in) is semidet.

    % X > Y: Succeed if and only if X is greater than Y.
    %
:- pred '>'(integer::in, integer::in) is semidet.

    % X =< Y: Succeed if and only if X is less than or equal to Y.
    %
:- pred '=<'(integer::in, integer::in) is semidet.

    % X >= Y: Succeed if and only if X is greater than or equal to Y.
    %
:- pred '>='(integer::in, integer::in) is semidet.

    % Absolute value.
    %
:- func abs(integer) = integer.

    % True if the argument is equal to integer.zero.
    %
:- pred is_zero(integer::in) is semidet.

%---------------------------------------------------------------------------%

    % Unary plus.
    %
:- func '+'(integer) = integer.

    % Unary minus.
    %
:- func '-'(integer) = integer.

    % Addition.
    %
:- func integer + integer = integer.

    % Subtraction.
    %
:- func integer - integer = integer.

    % Multiplication.
    %
:- func integer * integer = integer.

    % Flooring integer division.
    % Behaves as int.div.
    %
:- func integer div integer = integer.

    % Truncating integer division.
    % Behaves as int.(//).
    %
:- func integer // integer = integer.

    % Modulus.
    % Behaves as int.mod.
    %
:- func integer mod integer = integer.

    % Remainder.
    % Behaves as int.rem.
    %
:- func integer rem integer = integer.

    % divide_with_rem(X, Y, Q, R) where Q = X // Y and R = X rem Y
    % where both answers are calculated at the same time.
    %
:- pred divide_with_rem(integer::in, integer::in,
    integer::out, integer::out) is det.

    % Exponentiation.
    % pow(X, Y) = Z: Z is X raised to the Yth power.
    % Throws a `domain_error' exception if Y is negative.
    %
:- func pow(integer, integer) = integer.

%---------------------------------------------------------------------------%

    % Left shift.
    % Behaves as int.(<<).
    %
:- func integer << int = integer.

    % Right shift.
    % Behaves as int.(>>).
    %
:- func integer >> int = integer.

%---------------------------------------------------------------------------%

    % Bitwise complement.
    %
:- func \ integer = integer.

    % Bitwise and.
    %
:- func integer /\ integer = integer.

    % Bitwise or.
    %
:- func integer \/ integer = integer.

    % Bitwise exclusive or (xor).
    %
:- func integer `xor` integer = integer.

%---------------------------------------------------------------------------%

    % Convert an integer to an int.
    % Fails if the integer is not in the range [min_int, max_int].
    %
:- pred to_int(integer::in, int::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_int(integer) = int.

%---------------------%

    % Convert an integer to a uint.
    % Fails if the integer is not in the range [0, max_uint].
    %
:- pred to_uint(integer::in, uint::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_uint(integer) = uint.

    % Convert an integer to an int8.
    % Fails if the integer is not in the range [-128, 127].
    %
:- pred to_int8(integer::in, int8::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_int8(integer) = int8.

    % Convert an integer to a uint8.
    % Fails if the integer is not in the range [0, 255].
    %
:- pred to_uint8(integer::in, uint8::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_uint8(integer) = uint8.

    % Convert an integer to an int16.
    % Fails if the integer is not in the range [-32768, 32767].
    %
:- pred to_int16(integer::in, int16::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_int16(integer) = int16.

    % Convert an integer to a uint16.
    % Fails if the integer is not in the range [0, 65535].
    %
:- pred to_uint16(integer::in, uint16::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_uint16(integer) = uint16.

    % Convert an integer to an int32.
    % Fails if the integer is not in the range [-2147483648, 2147483647].
    %
:- pred to_int32(integer::in, int32::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_int32(integer) = int32.

    % Convert an integer to a uint32.
    % Fails if the integer is not in range [0, 4294967295].
    %
:- pred to_uint32(integer::in, uint32::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_uint32(integer) = uint32.

    % Convert an integer to an int64.
    % Fails if the integer is not in the range
    % [-9223372036854775808, 9223372036854775807].
    %
:- pred to_int64(integer::in, int64::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_int64(integer) = int64.

    % Convert an integer to a uint64.
    % Fails if the integer is not in range [0, 18446744073709551615].
    %
:- pred to_uint64(integer::in, uint64::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_to_uint64(integer) = uint64.

%---------------------%

    % Convert an integer to a float.
    %
:- func float(integer) = float.

%---------------------%

    % Convert an integer to a string (in base 10).
    %
:- func to_string(integer) = string.

    % to_base_string(Integer, Base) = String:
    %
    % Convert an integer to a string in a given Base.
    %
    % Base must be between 2 and 36, both inclusive; if it is not,
    % the predicate will throw an exception.
    %
:- func to_base_string(integer, int) = string.

%---------------------------------------------------------------------------%

    % Convert an int to integer.
    %
:- func integer(int) = integer.

    % Convert a uint to an integer.
    %
:- func from_uint(uint) = integer.

    % Convert an int8 to an integer.
    %
:- func from_int8(int8) = integer.

    % Convert a uint8 to an integer.
    %
:- func from_uint8(uint8) = integer.

    % Convert an int16 to an integer.
    %
:- func from_int16(int16) = integer.

    % Convert a uint16 to an integer.
    %
:- func from_uint16(uint16) = integer.

    % Convert an int32 to an integer.
    %
:- func from_int32(int32) = integer.

    % Convert a uint32 to an integer.
    %
:- func from_uint32(uint32) = integer.

    % Convert an int64 to an integer.
    %
:- func from_int64(int64) = integer.

    % Convert a uint64 to an integer.
    %
:- func from_uint64(uint64) = integer.

    % Convert a string to an integer. The string must contain only digits
    % [0-9], optionally preceded by a plus or minus sign. If the string does
    % not match this syntax, then the predicate fails.
    %
:- pred from_string(string::in, integer::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_from_string(string) = integer.

    % Convert a string in the specified base (2-36) to an integer.
    % The string must contain one or more digits in the specified base,
    % optionally preceded by a plus or minus sign. For bases > 10, digits
    % 10 to 35 are represented by the letters A-Z or a-z. If the string
    % does not match this syntax, then the predicate fails.
    %
:- pred from_base_string(int::in, string::in, integer::out) is semidet.

    % As above, but throws an exception rather than failing.
    %
:- func det_from_base_string(int, string) = integer.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

%---------------------------------------------------------------------------%

:- interface.

    % Exported for use by mercury_term_lexer.m.
    %
:- pred from_base_string_underscore(int::in, string::in, integer::out)
    is semidet.

%---------------------------------------------------------------------------%

:- implementation.

% Possible improvements:
%
% 1) allow negative digits (-base+1 .. base-1) in lists of digits
%    and normalise only when printing. This would probably simplify
%    the division algorithm, also.
%    (djh: this is not really done although -ve integers include a list
%    of -ve digits for faster comparison and so that normal mercury
%    sorting produces an intuitive order)
%
% 2) alternatively, instead of using base=10000, use *all* the bits in an int
%    and make use of the properties of machine arithmetic. Base 10000 doesn't
%    use even half the bits in an int, which is inefficient. (Base 2^14
%    would be a little better but would require a slightly more complex
%    case conversion on reading and printing.)
%    (djh: this is done)
%
% 3) Use an O(n^(3/2)) algorithm for multiplying large integers, rather than
%    the current O(n^2) method. There is an obvious divide-and-conquer
%    technique, Karatsuba multiplication. (this is done)
%
% 4) We could overload operators so that we can have mixed operations
%    on ints and integers. For example, "integer(1)+3". This
%    would obviate most calls of integer().
%
% 5) Use double-ended lists rather than simple lists. This would improve
%    the efficiency of the division algorithm, which reverse lists.
%    (djh: this is obsolete - digits lists are now in normal order)
%
% 6) Add bit operations (XOR, AND, OR, etc). We would treat the integers
%    as having a 2's complement bit representation. This is easier to do
%    if we use base 2^14 as mentioned above.
%    (djh: this is done:  /\ \/ << >> xor \)
%
% 7) The implementation of `div' is slower than it need be.
%    (djh: this is much improved)
%
% 8) Fourier methods such as Schoenhage-Strassen and multiplication via
%    modular arithmetic are left as exercises to the reader. 8^)
%
% Of the above, 1) would have the best bang-for-buck, 5) would benefit
% division and remainder operations quite a lot, and 3) would benefit
% large multiplications (thousands of digits) and is straightforward
% to implement.
%
% (djh: I'd like to see 1) done. integers are now represented as
% i(Length, Digits) where Digits are no longer reversed.
% The only penalty for not reversing is in multiplication by the base
% which now entails walking to the end of the list to append a 0.
% Therefore I'd like to see:
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

%---------------------------------------------------------------------------%

:- func length(integer) = int.

length(i(L, _)) = L.

:- func integer_signum(integer) = int.

integer_signum(i(Sign, _)) = signum(Sign).

:- func signum(int) = int.

signum(N) = Sign :-
    ( if N < 0 then
        Sign = -1
    else if N = 0 then
        Sign = 0
    else
        Sign = 1
    ).

:- pred big_isnegative(integer::in) is semidet.

big_isnegative(i(Sign, _)) :-
    Sign < 0.

%---------------------%

:- func decap(integer) = integer.

decap(i(_, [])) = integer.zero.
decap(i(L, [H | T])) = Result :-
    ( if H = 0 then
        Result = decap(i(L - 1, T))
    else
        Result = i(L, [H | T])
    ).

%---------------------%

:- pred chop(int::in, digit::out, digit::out) is det.

chop(N, Div, Mod) :-
    % The unchecked shifts here and in the uint case below
    % are safe since log2base is 14.
    Div = N `int.unchecked_right_shift` log2base,    % i.e. Div = N div base
    Mod = N /\ basemask.                             % i.e. Mod = N mod base

:- pred chop_uint(uint::in, uint::out, uint::out) is det.

chop_uint(N, Div, Mod) :-
    % See the comments in chop/3.
    Div = N `uint.unchecked_right_shift` log2base,
    Mod = N /\ cast_from_int(basemask).

:- pred chop_uint32(uint32::in, uint32::out, uint32::out) is det.

chop_uint32(N, Div, Mod) :-
    % See the comments in chop/3.
    Div = N `uint32.unchecked_right_shift` log2base,
    Mod = N /\ cast_from_int(basemask).

:- pred chop_int64(int64::in, int64::out, int64::out) is det.

chop_int64(N, Div, Mod) :-
    % See the comments in chop/3.
    Div = N `int64.unchecked_right_shift` log2base,
    Mod = N /\ from_int(basemask).

:- pred chop_uint64(uint64::in, uint64::out, uint64::out) is det.

chop_uint64(N, Div, Mod) :-
    % See the comments in chop/3.
    Div = N `uint64.unchecked_right_shift` log2base,
    Mod = N /\ cast_from_int(basemask).

%---------------------%

:- func det_first(integer) = digit.

det_first(i(_, Digits)) = First :-
    (
        Digits = [],
        unexpected($pred, "empty list")
    ;
        Digits = [First | _]
    ).

:- func det_second(integer) = digit.

det_second(i(_, Digits)) = Second :-
    (
        Digits = [],
        unexpected($pred, "empty list")
    ;
        Digits = [_],
        unexpected($pred, "short list")
    ;
        Digits = [_, Second | _]
    ).

:- func det_tail(integer) = integer.

det_tail(i(Len, Digits)) = I :-
    (
        Digits = [],
        unexpected($pred, "empty list")
    ;
        Digits = [_ | T],
        I = i(Len - 1, T)
    ).

%---------------------%

    % XXX What is the intended semantic difference between
    % big_cmp and res_cmp?
    %
:- func big_cmp(integer, integer) = comparison_result.

big_cmp(X, Y) = Result :-
    compare(Result, X, Y).

:- func pos_cmp(integer, integer) = comparison_result.

pos_cmp(X, Y) = Result :-
    compare(Result, X, Y).

:- pred pos_lt(integer::in, integer::in) is semidet.

pos_lt(X, Y) :-
    Result = pos_cmp(X, Y),
    Result = (<).

:- pred pos_geq(integer::in, integer::in) is semidet.

pos_geq(X, Y) :-
    Result = pos_cmp(X, Y),
    ( Result = (>)
    ; Result = (=)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

negative_one = i(-1, [-1]).

zero = i(0, []).

one = i(1, [1]).

two = i(1, [2]).

eight = i(1, [8]).

ten = i(1, [10]).

sixteen = i(1, [16]).

%---------------------------------------------------------------------------%

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

abs(N) = big_abs(N).

is_zero(i(0, [])).

%---------------------%

:- func big_abs(integer) = integer.

big_abs(i(Sign, Ds)) = Result :-
    ( if Sign < 0 then
        Result = big_neg(i(Sign, Ds))
    else
        Result = i(Sign, Ds)
    ).

:- func big_neg(integer) = integer.

big_neg(i(S, Digits0)) = i(-S, Digits) :-
    neg_list(Digits0, Digits).

:- pred neg_list(list(int)::in, list(int)::out) is det.

neg_list([], []).
neg_list([H | T], [-H | NT]) :-
    neg_list(T, NT).

%---------------------------------------------------------------------------%

'+'(X) = X.

'-'(X) = big_neg(X).

X + Y = big_plus(X, Y).

X - Y = big_plus(X, big_neg(Y)).

X * Y = big_mul(X, Y).

X div Y = big_div(X, Y).

X // Y = big_quot(X, Y).

X mod Y = big_mod(X, Y).

X rem Y = big_rem(X, Y).

divide_with_rem(X, Y, Quotient, Remainder) :-
    big_quot_rem(X, Y, Quotient, Remainder).

pow(A, N) = P :-
    ( if big_isnegative(N) then
        throw(domain_error("integer.pow: negative exponent"))
    else
        P = big_pow(A, N)
    ).

%---------------------%

:- func big_plus(integer, integer) = integer.

big_plus(X, Y) = Sum :-
    ( if is_zero(X) then
        Sum = Y
    else if is_zero(Y) then
        Sum = X
    else
        AbsX = big_abs(X),
        AbsY = big_abs(Y),
        SignX = integer_signum(X),
        SignY = integer_signum(Y),
        ( if SignX = SignY then
            Sum = big_sign(SignX, pos_plus(AbsX, AbsY))
        else
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

:- func pos_plus(integer, integer) = integer.

pos_plus(i(L1, D1), i(L2, D2)) = Out :-
    add_pairs(Div, i(L1, D1), i(L2, D2), Ds),
    ( if L1 > L2 then
        Len = L1
    else
        Len = L2
    ),
    ( if Div = 0  then
        Out = i(Len, Ds)
    else
        Out = i(Len + 1, [Div | Ds])
    ).

:- pred add_pairs(digit::out, integer::in, integer::in,
    list(digit)::out) is det.

add_pairs(Div, i(L1, D1), i(L2, D2), Ds) :-
    ( if L1 = L2 then
        add_pairs_equal(Div, D1, D2, Ds)
    else if L1 < L2, D2 = [H2 | T2] then
        add_pairs(Div1, i(L1, D1), i(L2 - 1, T2), Ds1),
        chop(H2 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    else if L1 > L2, D1 = [H1 | T1] then
        add_pairs(Div1, i(L1 - 1, T1), i(L2, D2), Ds1),
        chop(H1 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    else
        unexpected($pred, "invalid integer")
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
    ( if L1 > L2 then
        Len = L1
    else
        Len = L2
    ),
    ( if Mod = 0 then
        Out = decap(i(Len, Ds))
    else
        Out = i(Len + 1, [Mod | Ds])
    ).

:- pred diff_pairs(digit::out, integer::in, integer::in,
    list(digit)::out) is det.

diff_pairs(Div, i(L1, D1), i(L2, D2), Ds) :-
    ( if L1 = L2 then
        diff_pairs_equal(Div, D1, D2, Ds)
    else if L1 > L2, D1 = [H1 | T1] then
        diff_pairs(Div1, i(L1 - 1, T1), i(L2, D2), Ds1),
        chop(H1 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    else
        unexpected($pred, "invalid integer")
    ).

:- pred diff_pairs_equal(digit::out, list(digit)::in, list(digit)::in,
    list(digit)::out) is det.

diff_pairs_equal(0, [], _, []).
diff_pairs_equal(0, [_ | _], [], []).
diff_pairs_equal(Div, [X | Xs], [Y | Ys], [Mod | TailDs]) :-
    diff_pairs_equal(DivTail, Xs, Ys, TailDs),
    chop(X - Y + DivTail, Div, Mod).

%---------------------%

:- func big_mul(integer, integer) = integer.

big_mul(X, Y) = Result :-
    Sign = integer_signum(X) * integer_signum(Y),
    Value = pos_mul(big_abs(X), big_abs(Y)),
    Result = big_sign(Sign, Value).

:- func pos_mul(integer, integer) = integer.

pos_mul(i(L1, Ds1), i(L2, Ds2)) =
    ( if L1 < L2 then
        pos_mul_karatsuba(i(L1, Ds1), i(L2, Ds2))
    else
        pos_mul_karatsuba(i(L2, Ds2), i(L1, Ds1))
    ).

% Use quadratic multiplication for less than threshold digits.
:- func karatsuba_threshold = int.

karatsuba_threshold = 35.

% Use parallel execution if number of digits of split numbers is larger
% than this threshold.
:- func karatsuba_parallel_threshold = int.

karatsuba_parallel_threshold = karatsuba_threshold * 10.

% Karatsuba / Toom-2 multiplication in O(n^1.585)
:- func pos_mul_karatsuba(integer, integer) = integer.

pos_mul_karatsuba(i(L1, Ds1), i(L2, Ds2)) = Res :-
    ( if L1 < karatsuba_threshold then
        Res = pos_mul_list(Ds1, integer.zero, i(L2, Ds2))
    else
        ( if L2 < L1 then
            unexpected($pred, "second factor smaller")
        else
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
            ( if Middle > karatsuba_parallel_threshold then
                Res0 = pos_mul(LoDs1, LoDs2) &
                Res1 = pos_mul(LoDs1 + HiDs1, LoDs2 + HiDs2) &
                Res2 = pos_mul(HiDs1, HiDs2)
            else
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
    ( if Mod = 0 then
        Out = i(Len, Digits)
    else
        Out = i(Len + 1, [Mod | Digits])
    ).

:- pred mul_by_digit_2(digit::in, digit::out, list(digit)::in,
    list(digit)::out) is det.

mul_by_digit_2(_, 0, [], []).
mul_by_digit_2(D, Div, [X | Xs], [Mod | NewXs]) :-
    mul_by_digit_2(D, DivXs, Xs, NewXs),
    chop(D * X + DivXs, Div, Mod).

%---------------------%

:- func big_div(integer, integer) = integer.

big_div(X, Y) = Div :-
    big_quot_rem(X, Y, Trunc, Rem),
    ( if integer_signum(Y) * integer_signum(Rem) < 0 then
        Div = Trunc - integer.one
    else
        Div = Trunc
    ).

:- func big_quot(integer, integer) = integer.

big_quot(X, Y) = Quot :-
    big_quot_rem(X, Y, Quot, _Rem).

:- func big_rem(integer, integer) = integer.

big_rem(X, Y) = Rem :-
    big_quot_rem(X, Y, _Quot, Rem).

:- func big_mod(integer, integer) = integer.

big_mod(X, Y) = Mod :-
    big_quot_rem(X, Y, _Trunc, Rem),
    ( if integer_signum(Y) * integer_signum(Rem) < 0 then
        Mod = Rem + Y
    else
        Mod = Rem
    ).

%---------------------%

:- pred big_quot_rem(integer::in, integer::in, integer::out, integer::out)
    is det.

big_quot_rem(X, Y, Quot, Rem) :-
    ( if is_zero(Y) then
        throw(domain_error("integer.big_quot_rem: division by zero"))
    else if is_zero(X) then
        Quot = integer.zero,
        Rem  = integer.zero
    else
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
% If the first digit of V is less than base/2, then we scale both
% the numerator and denominator. This way, we can use Knuth's[*] nifty trick
% for finding an accurate approximation to Q. That is all we use from Knuth;
% his MIX algorithm is fugly.
%
% [*] Knuth, Semi-numerical algorithms.

:- pred quot_rem(integer::in, integer::in, integer::out, integer::out) is det.

quot_rem(U, V, Quot, Rem) :-
    ( if U = i(_, [UI]), V = i(_, [VI]) then
        Quot = shortint_to_integer(UI // VI),
        Rem  = shortint_to_integer(UI rem VI)
    else
        V0 = det_first(V),
        ( if V0 < basediv2 then
            M = base div (V0 + 1),
            quot_rem_2(integer.zero, mul_by_digit(M, U),
                mul_by_digit(M, V), QuotZeros, R),
            Rem = div_by_digit(M, R)
        else
            quot_rem_2(integer.zero, U, V, QuotZeros, Rem)
        ),
        Quot = decap(QuotZeros)
    ).

:- pred quot_rem_2(integer::in, integer::in, integer::in, integer::out,
    integer::out) is det.

quot_rem_2(Ur, U, V, Quot, Rem) :-
    ( if pos_lt(Ur, V) then
        ( if U = i(_, [Ua | _]) then
            quot_rem_2(integer_append(Ur, Ua), det_tail(U), V,
                Quot0, Rem0),
            Quot = integer_prepend(0, Quot0),
            Rem = Rem0
        else
            Quot = i(1, [0]),
            Rem = Ur
        )
    else
        ( if length(Ur) > length(V) then
            Qhat = (det_first(Ur) * base + det_second(Ur)) div det_first(V)
        else
            Qhat = det_first(Ur) div det_first(V)
        ),
        QhatByV = mul_by_digit(Qhat, V),
        ( if pos_geq(Ur, QhatByV) then
            Q = Qhat,
            QByV = QhatByV
        else
            QhatMinus1ByV = pos_minus(QhatByV, V),
            ( if pos_geq(Ur, QhatMinus1ByV) then
                Q = Qhat - 1,
                QByV = QhatMinus1ByV
            else
                Q = Qhat - 2,
                QByV = pos_minus(QhatMinus1ByV, V)
            )
        ),
        NewUr = pos_minus(Ur, QByV),
        ( if U = i(_, [Ua | _]) then
            quot_rem_2(integer_append(NewUr, Ua), det_tail(U), V, Quot0, Rem0),
            Quot = integer_prepend(Q, Quot0),
            Rem = Rem0
        else
            Quot = i(1, [Q]),
            Rem = NewUr
        )
    ).

:- func div_by_digit(digit, integer) = integer.

div_by_digit(_, i(_, [])) = integer.zero.
div_by_digit(Digit, i(_, [X | Xs])) = div_by_digit_1(X, Xs, Digit).

:- func div_by_digit_1(digit, list(digit), digit) = integer.

div_by_digit_1(X, [], D) = Integer :-
    Q = X div D,
    ( if Q = 0 then
        Integer = integer.zero
    else
        Integer = i(1, [Q])
    ).
div_by_digit_1(X, [H | T], D) = Integer :-
    Q = X div D,
    ( if Q = 0 then
        Integer = div_by_digit_1((X rem D) * base + H, T, D)
    else
        i(L, Ds) = div_by_digit_2((X rem D) * base + H, T, D),
        Integer = i(L + 1, [Q | Ds])
    ).

:- func div_by_digit_2(digit, list(digit), digit) = integer.

div_by_digit_2(X, [], D) = i(1, [X div D]).
div_by_digit_2(X, [H | T], D) = i(Len + 1, [X div D | Tail]) :-
    i(Len, Tail) = div_by_digit_2((X rem D) * base + H, T, D).

:- func integer_append(integer, digit) = integer.

integer_append(i(L, List), Digit) = i(L + 1, NewList) :-
    list.append(List, [Digit], NewList).

:- func integer_prepend(digit, integer) = integer.

integer_prepend(Digit, i(L, List)) = i(L + 1, [Digit | List]).

%---------------------%

:- func shortint_to_integer(int) = integer.

shortint_to_integer(D) = Result :-
    ( if D = 0 then
        Result = integer.zero
    else if D > 0 then
        Result = i(1, [D])
    else
        Result = i(-1, [D])
    ).

:- func big_sign(int, integer) = integer.

big_sign(Sign, In) = Result :-
    ( if Sign < 0 then
        Result = big_neg(In)
    else
        Result = In
    ).

%---------------------%

:- func big_pow(integer, integer) = integer.

big_pow(A, N) = Result :-
    ( if N = integer.zero then
        Result = integer.one
    else if N = integer.one then
        Result = A
    else if A = integer.one then
        Result = integer.one
    else if A = integer.zero then
        Result = integer.zero
    else if N = i(_, [_ | _]) then
        Result = big_pow_sqmul(A, N)
    else
        Result = integer.zero
    ).

:- func big_pow_sqmul(integer, integer) = integer.

big_pow_sqmul(A, N) = Result :-
    ( if N = integer.zero then
        Result = integer.one
    else if N = integer.one then
        Result = A
    else
        ( if (N mod integer.two) = integer.zero then
            % if exponent N is even -> Result = A^(N//2) * A^(N//2)
            HalfResult = big_pow_sqmul(A, N // integer.two),
            Result = HalfResult * HalfResult
        else
            % if odd, then Result = A * A^(N - 1)
            SubResult = big_pow_sqmul(A, N - integer.one),
            Result = A * SubResult
        )
    ).

%---------------------------------------------------------------------------%

X << I = Result :-
    ( if I > 0 then
        Result = big_left_shift(X, I)
    else if I < 0 then
        Result = X >> -I
    else
        Result = X
    ).

:- func big_left_shift(integer, int) = integer.

big_left_shift(X, I) = Result :-
    ( if is_zero(X) then
        Result = X
    else if big_isnegative(X) then
        Result = big_neg(pos_left_shift(big_neg(X), I))
    else
        Result = pos_left_shift(X, I)
    ).

:- func pos_left_shift(integer, int) = integer.

pos_left_shift(i(Len, Digits), I) = Integer :-
    Div = I div log2base,
    Mod = I mod log2base,
    NewLen = Len + Div,
    leftshift(Mod, log2base - Mod, NewLen, Digits, Carry, NewDigits),
    ( if Carry = 0 then
        Integer = i(NewLen, NewDigits)
    else
        Integer = i(NewLen + 1, [Carry | NewDigits])
    ).

:- pred leftshift(int::in, int::in, int::in, list(digit)::in,
    int::out, list(digit)::out) is det.

leftshift(_Mod, _InvMod, Len, [], Carry, DigitsOut) :-
    Carry = 0,
    zeros(Len, [], DigitsOut).
leftshift(Mod, InvMod, Len, [H | T], Carry, DigitsOut) :-
    ( if Len =< 0 then
        Carry = 0,
        DigitsOut = []
    else
        Carry = (H /\ (basemask << InvMod)) >> InvMod,
        leftshift(Mod, InvMod, Len - 1, T, TailCarry, Tail),
        DigitsOut = [TailCarry \/ ((H << Mod) /\ basemask) | Tail]
    ).

:- pred zeros(int::in, list(digit)::in, list(digit)::out) is det.

zeros(Len, Digits0, Digits) :-
    ( if Len > 0 then
        zeros(Len - 1, Digits0, Digits1),
        Digits = [0 | Digits1]
    else
        Digits = Digits0
    ).

%---------------------%

X >> I = Result :-
    ( if I < 0 then
        Result = X << -I
    else if I > 0 then
        Result = big_right_shift(X, I)
    else
        Result = X
    ).

:- func big_right_shift(integer, int) = integer.

big_right_shift(X, I) = Result :-
    ( if is_zero(X) then
        Result = X
    else if big_isnegative(X) then
        Result = \ pos_right_shift(\ X, I)
    else
        Result = pos_right_shift(X, I)
    ).

:- func pos_right_shift(integer, int) = integer.

pos_right_shift(i(Len, Digits), I) = Integer :-
    Div = I div log2base,
    ( if Div < Len then
        Mod = I mod log2base,
        Integer = decap(rightshift(Mod, log2base - Mod,
            i(Len - Div, Digits), 0))
    else
        Integer = integer.zero
    ).

:- func rightshift(int, int, integer, int) = integer.

rightshift(_Mod, _InvMod, i(_Len, []), _Carry) = integer.zero.
rightshift(Mod, InvMod, i(Len, [H | T]), Carry) = Integer :-
    ( if Len =< 0 then
        Integer = integer.zero
    else
        NewH = Carry \/ (H >> Mod),
        NewCarry = (H /\ (basemask >> InvMod)) << InvMod,
        i(TailLen, NewTail) = rightshift(Mod, InvMod, i(Len - 1, T),
            NewCarry),
        Integer = i(TailLen + 1, [NewH | NewTail])
    ).

%---------------------------------------------------------------------------%

\ X = big_neg(big_plus(X, integer.one)).

%---------------------%

X /\ Y = Result :-
    ( if big_isnegative(X) then
        ( if big_isnegative(Y) then
            Result = \ big_or(\ X, \ Y)
        else
            Result = big_and_not(Y, \ X)
        )
    else if big_isnegative(Y) then
        Result = big_and_not(X, \ Y)
    else
        Result = big_and(X, Y)
    ).

X \/ Y = Result :-
    ( if big_isnegative(X) then
        ( if big_isnegative(Y) then
            Result = \ big_and(\ X, \ Y)
        else
            Result = \ big_and_not(\ X, Y)
        )
    else if big_isnegative(Y) then
        Result = \ big_and_not(\ Y, X)
    else
        Result = big_or(X, Y)
    ).

X `xor` Y = Result :-
    ( if big_isnegative(X) then
        ( if big_isnegative(Y) then
            Result = big_xor(\ X, \ Y)
        else
            Result = big_xor_not(Y, \ X)
        )
    else if big_isnegative(Y) then
        Result = big_xor_not(X, \ Y)
    else
        Result = big_xor(X, Y)
    ).

%---------------------%

:- func big_and(integer, integer) = integer.

big_and(X, Y) = decap(and_pairs(X, Y)).

:- func and_pairs(integer, integer) = integer.

and_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    ( if L1 = L2 then
        Integer = i(L1, and_pairs_equal(D1, D2))
    else if L1 < L2, D2 = [_ | T2] then
        i(_, DsT) = and_pairs(i(L1, D1), i(L2 - 1, T2)),
        Integer = i(L1, DsT)
    else if L1 > L2, D1 = [_ | T1] then
        i(_, DsT) = and_pairs(i(L1 - 1, T1), i(L2, D2)),
        Integer = i(L2, DsT)
    else
        unexpected($pred, "invalid integer")
    ).

:- func and_pairs_equal(list(digit), list(digit)) = list(digit).

and_pairs_equal([], _) = [].
and_pairs_equal([_ | _], []) = [].
and_pairs_equal([X | Xs], [Y | Ys]) = [X /\ Y | and_pairs_equal(Xs, Ys)].

%---------------------%

:- func big_or(integer, integer) = integer.

big_or(X, Y) = decap(or_pairs(X, Y)).

:- func or_pairs(integer, integer) = integer.

or_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    ( if L1 = L2 then
        Integer = i(L1, or_pairs_equal(D1, D2))
    else if L1 < L2, D2 = [H2 | T2] then
        i(_, DsT) = or_pairs(i(L1, D1), i(L2 - 1, T2)),
        Integer = i(L2, [H2 | DsT])
    else if L1 > L2, D1 = [H1 | T1] then
        i(_, DsT) = or_pairs(i(L1 - 1, T1), i(L2, D2)),
        Integer = i(L1, [H1 | DsT])
    else
        unexpected($pred, "invalid integer")
    ).

:- func or_pairs_equal(list(digit), list(digit)) = list(digit).

or_pairs_equal([], _) = [].
or_pairs_equal([_ | _], []) = [].
or_pairs_equal([X | Xs], [Y | Ys]) = [X \/ Y | or_pairs_equal(Xs, Ys)].

%---------------------%

:- func big_xor(integer, integer) = integer.

big_xor(X, Y) = decap(xor_pairs(X, Y)).

:- func xor_pairs(integer, integer) = integer.

xor_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    ( if L1 = L2 then
        Integer = i(L1, xor_pairs_equal(D1, D2))
    else if L1 < L2, D2 = [H2 | T2] then
        i(_, DsT) = xor_pairs(i(L1, D1), i(L2 - 1, T2)),
        Integer = i(L2, [H2 | DsT])
    else if L1 > L2, D1 = [H1 | T1] then
        i(_, DsT) = xor_pairs(i(L1 - 1, T1), i(L2, D2)),
        Integer = i(L1, [H1 | DsT])
    else
        unexpected($pred, "invalid integer")
    ).

:- func xor_pairs_equal(list(digit), list(digit)) = list(digit).

xor_pairs_equal([], _) = [].
xor_pairs_equal([_ | _], []) = [].
xor_pairs_equal([X | Xs], [Y | Ys]) =
    [int.xor(X, Y) | xor_pairs_equal(Xs, Ys)].

%---------------------%

:- func big_and_not(integer, integer) = integer.

big_and_not(X, Y) = decap(and_not_pairs(X, Y)).

:- func and_not_pairs(integer, integer) = integer.

and_not_pairs(i(L1, D1), i(L2, D2)) = Integer :-
    ( if L1 = L2 then
        Integer = i(L1, and_not_pairs_equal(D1, D2))
    else if L1 < L2, D2 = [_ | T2] then
        i(_, DsT) = and_not_pairs(i(L1, D1), i(L2 - 1, T2)),
        Integer = i(L1, DsT)
    else if L1 > L2, D1 = [H1 | T1] then
        i(_, DsT) = and_not_pairs(i(L1 - 1, T1), i(L2, D2)),
        Integer = i(L1, [H1 | DsT])
    else
        unexpected($pred, "invalid integer")
    ).

:- func and_not_pairs_equal(list(digit), list(digit)) = list(digit).

and_not_pairs_equal([], _) = [].
and_not_pairs_equal([_ | _], []) = [].
and_not_pairs_equal([X | Xs], [Y | Ys]) =
    [X /\ \ Y | and_not_pairs_equal(Xs, Ys)].

%---------------------%

:- func big_xor_not(integer, integer) = integer.

big_xor_not(X1, NotX2) =
    \ big_and_not(big_or(X1, NotX2), big_and(X1, NotX2)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%

to_int(Integer, Int) :-
    Integer >= integer(int.min_int),
    Integer =< integer(int.max_int),
    Integer = i(_Sign, Digits),
    Int = int_list(Digits, 0).

:- func int_list(list(int), int) = int.

int_list([], Accum) = Accum.
int_list([H | T], Accum) = int_list(T, Accum * base + H).

det_to_int(Integer) = Int :-
    ( if integer.to_int(Integer, IntPrime) then
        Int = IntPrime
    else
        throw(domain_error(
            "integer.det_to_int: domain error (conversion would overflow)"))
    ).

%---------------------------------------------------------------------------%

to_uint(Integer, UInt) :-
    Integer >= integer.zero,
    Integer =< integer.from_uint(uint.max_uint),
    Integer = i(_Sign, Digits),
    UInt = uint_list(Digits, 0u).

:- func uint_list(list(int), uint) = uint.

uint_list([], Accum) = Accum.
uint_list([H | T], Accum) =
    uint_list(T, Accum * cast_from_int(base) + cast_from_int(H)).

det_to_uint(Integer) = UInt :-
    ( if integer.to_uint(Integer, UIntPrime) then
        UInt = UIntPrime
    else
        throw(domain_error(
            "integer.det_to_uint: domain error (conversion would overflow)"))
    ).

%---------------------------------------------------------------------------%

to_int8(Integer, Int8) :-
    ( if is_zero(Integer) then
        Int8 = 0i8
    else
        Integer = i(_, [Digit]),
        int8.from_int(Digit, Int8)
    ).

det_to_int8(Integer) = Int8 :-
    ( if integer.to_int8(Integer, Int8Prime) then
        Int8 = Int8Prime
    else
        throw(domain_error(
            "integer.det_to_int8: domain error (conversion would overflow)"))
    ).

%---------------------------------------------------------------------------%

to_uint8(Integer, UInt8) :-
    ( if is_zero(Integer) then
        UInt8 = 0u8
    else
        Integer = i(_, [Digit]),
        uint8.from_int(Digit, UInt8)
    ).

det_to_uint8(Integer) = UInt8 :-
    ( if integer.to_uint8(Integer, UInt8Prime) then
        UInt8 = UInt8Prime
    else
        throw(domain_error(
            "integer.det_to_uint8: domain error (conversion would overflow)"))
    ).

%---------------------------------------------------------------------------%

to_int16(Integer, Int16) :-
    integer.to_int(Integer, Int),
    int16.from_int(Int, Int16).

det_to_int16(Integer) = Int16 :-
    ( if integer.to_int16(Integer, Int16Prime) then
        Int16 = Int16Prime
    else
        throw(domain_error(
            "integer.det_to_int16: domain error (conversion would overflow)"))
    ).

%---------------------------------------------------------------------------%

to_uint16(Integer, UInt16) :-
    integer.to_int(Integer, Int),
    uint16.from_int(Int, UInt16).

det_to_uint16(Integer) = UInt16 :-
    ( if integer.to_uint16(Integer, UInt16Prime) then
        UInt16 = UInt16Prime
    else
        throw(domain_error(
            "integer.det_to_uint16: domain error (conversion would overflow)"))
    ).

%---------------------------------------------------------------------------%

to_int32(Integer, Int32) :-
    integer.to_int(Integer, Int),
    int32.from_int(Int, Int32).

det_to_int32(Integer) = Int32 :-
    ( if integer.to_int32(Integer, Int32Prime) then
        Int32 = Int32Prime
    else
        throw(domain_error(
            "integer.det_to_int32: domain error (conversion would overflow"))
    ).

%---------------------------------------------------------------------------%

to_uint32(Integer, UInt32) :-
    Integer = i(Sign, Digits),
    Sign >= 0,     % i.e. Integer >= 0.
    Integer =< integer_max_uint32,
    UInt32 = uint32_list(Digits, 0u32).

    % Return max_uint32 as an integer.
    %
:- func integer_max_uint32 = integer.

integer_max_uint32 = i(3, [15, 16383, 16383]).

:- func uint32_list(list(int), uint32) = uint32.

uint32_list([], Accum) = Accum.
uint32_list([H | T], Accum) =
    uint32_list(T, Accum * cast_from_int(base) + cast_from_int(H)).

det_to_uint32(Integer) = UInt32 :-
    ( if integer.to_uint32(Integer, UInt32Prime) then
        UInt32 = UInt32Prime
    else
        throw(domain_error(
            "integer.det_to_uint32: domain error (conversion would overflow"))
    ).

%---------------------------------------------------------------------------%

to_int64(Integer, Int64) :-
    Integer = i(Sign, Digits),
    compare(SignRes, Sign, 0),
    (
        SignRes = (<),
        Integer >= integer_min_int64,
        Int64 = int64_list(Digits, from_int(0))
    ;
        SignRes = (=),
        Int64 = from_int(0)
    ;
        SignRes = (>),
        Integer =< integer_max_int64,
        Int64 = int64_list(Digits, from_int(0))
    ).

    % Return min_int64 as an integer.
    %
:- func integer_min_int64 = integer.

    %  128 * 2^(14*4) + 0 * 2^(14*3) + ... + 0 * 2^(14*0)
    %  = 2^7 * 2^56 + 0 ... + 0
    %  = 2^63.
integer_min_int64 = i(-5, [-128, 0, 0, 0, 0]).

    % Return max_int64 as an integer.
    %
:- func integer_max_int64 = integer.

    % 127 * 2^(14*4) + 16383 * 2^(14*3) + ... + 16383 * 2^(14*0)
    % = (2^7  - 1) * 2^56 + (2^14 - 1) * 2^42 + ... + (2^14 - 1) * 1
    % = 2^63 - 1.
integer_max_int64 = i(5, [127, 16383, 16383, 16383, 16383]).

:- func int64_list(list(int), int64) = int64.

int64_list([], Accum) = Accum.
int64_list([H | T], Accum) =
    int64_list(T, Accum * from_int(base) + from_int(H)).

det_to_int64(Integer) = Int64 :-
    ( if integer.to_int64(Integer, Int64Prime) then
        Int64 = Int64Prime
    else
        throw(domain_error(
            "integer.det_to_int64: domain error (conversion would overflow"))
    ).

%---------------------------------------------------------------------------%

to_uint64(Integer, UInt64) :-
    Integer = i(Sign, Digits),
    Sign >= 0, % i.e. Integer >= 0.
    Integer =< integer_max_uint64,
    UInt64 = uint64_list(Digits, uint64.cast_from_int(0)).

    % Return max_uint64 as an integer.
    %
:- func integer_max_uint64 = integer.

    % 255 * 2^(14*4) + 16383 * 2^(14*3) + ... + 16383 * 2(14*0)
    % = (2^8 - 1) * 2^56 + (2^14 - 1) * 2^42 + ... + (2^14 - 1) * 2^0
    % = 2^64 - 2^56 + 2^56 - 2^42 + ... + 2^14 - 1
    % = 2^64 - 1.
integer_max_uint64 = i(5, [255, 16383, 16383, 16383, 16383]).

:- func uint64_list(list(int), uint64) = uint64.

uint64_list([], Accum) = Accum.
uint64_list([H | T], Accum) =
    uint64_list(T, Accum * cast_from_int(base) + cast_from_int(H)).

det_to_uint64(Integer) = UInt64 :-
    ( if integer.to_uint64(Integer, UInt64Prime) then
        UInt64 = UInt64Prime
    else
        throw(domain_error(
            "integer.det_to_uint64: domain error (conversion would overflow"))
    ).

%---------------------------------------------------------------------------%

float(i(_, List)) = float_list(float.float(base), 0.0, List).

:- func float_list(float, float, list(int)) = float.

float_list(_, Accum, []) = Accum.
float_list(FBase, Accum, [H | T]) =
    float_list(FBase, Accum * FBase + float.float(H), T).

%---------------------------------------------------------------------------%
%
% Converting integers to strings.
%

to_string(Integer) = to_base_string(Integer, 10).

to_base_string(Integer, Base) = String :-
    ( if 2 =< Base, Base =< 36 then
        true
    else
        unexpected($pred, "invalid base")
    ),
    PrintBase = printbase(pow(Base, printbase_exponent)),
    Integer = i(Sign, Digits),
    ( if Sign < 0 then
        neg_list(Digits, AbsDigits),
        String = "-" ++ digits_to_string(Base, PrintBase, AbsDigits)
    else
        String = digits_to_string(Base, PrintBase, Digits)
    ).

:- func digits_to_string(int, printbase, list(digit)) = string.

digits_to_string(_Base, _PrintBase, []) = "0".
digits_to_string(Base, PrintBase, Digits) = Str :-
    Digits = [_ | _],
    printbase_rep(PrintBase, printbase_pos_int_to_digits(PrintBase, base),
        Digits, i(_, DigitsInPrintBase)),
    (
        DigitsInPrintBase = [Head | Tail],
        string.int_to_base_string(Head, Base, HeadStr),
        digits_to_strings(Base, Tail, [], TailStrs),
        string.append_list([HeadStr | TailStrs], Str)
    ;
        DigitsInPrintBase = [],
        unexpected($pred, "empty list")
    ).

:- pred printbase_rep(printbase::in, integer::in, list(digit)::in,
    integer::out) is det.

printbase_rep(PrintBase, Base, Digits, Result) :-
    Result = printbase_rep_1(PrintBase, Digits, Base, integer.zero).

:- func printbase_rep_1(printbase, list(digit), integer, integer) = integer.

printbase_rep_1(_PrintBase, [], _Base, Carry) = Carry.
printbase_rep_1(PrintBase, [X | Xs], Base, Carry) =
    printbase_rep_1(PrintBase, Xs, Base,
        printbase_pos_plus(PrintBase,
            printbase_pos_mul(PrintBase, Base, Carry),
            printbase_pos_int_to_digits(PrintBase, X))).

:- pred digits_to_strings(int::in, list(digit)::in,
    list(string)::in, list(string)::out) is det.

digits_to_strings(_Base, [], !Strs).
digits_to_strings(Base, [H | T], !Strs) :-
    digit_to_string(Base, H, Str),
    digits_to_strings(Base, T, !Strs),
    !:Strs = [Str | !.Strs].

:- pred digit_to_string(int::in, digit::in, string::out) is det.

digit_to_string(Base, D, S) :-
    string.int_to_base_string(D, Base, S1),
    string.pad_left(S1, '0', printbase_exponent, S).

%---------------------------------------------------------------------------%
%
% Essentially duplicated code to work in base `printbase' follows.
%

:- type printbase
    --->    printbase(int). % base^printbase_exponent

:- func printbase_exponent = int.

printbase_exponent = 3.

:- func printbase_pos_int_to_digits(printbase, int) = integer.

printbase_pos_int_to_digits(Base, D) =
    printbase_pos_int_to_digits_2(Base, D, integer.zero).

:- func printbase_pos_int_to_digits_2(printbase, int, integer) = integer.

printbase_pos_int_to_digits_2(Base, D, Tail) = Result :-
    ( if D = 0 then
        Result = Tail
    else
        Tail = i(Length, Digits),
        printbase_chop(Base, D, Div, Mod),
        Result = printbase_pos_int_to_digits_2(Base, Div,
            i(Length + 1, [Mod | Digits]))
    ).

:- pred printbase_chop(printbase::in, int::in, digit::out, digit::out) is det.

printbase_chop(printbase(Base), N, Div, Mod) :-
    Mod = N mod Base,
    Div = N div Base.

:- func printbase_mul_by_digit(printbase, digit, integer) = integer.

printbase_mul_by_digit(Base, D, i(Len, Ds)) = Out :-
    printbase_mul_by_digit_2(Base, D, Div, Ds, DsOut),
    ( if Div = 0 then
        Out = i(Len, DsOut)
    else
        Out = i(Len + 1, [Div | DsOut])
    ).

:- pred printbase_mul_by_digit_2(printbase::in, digit::in, digit::out,
    list(digit)::in, list(digit)::out) is det.

printbase_mul_by_digit_2(_Base, _, 0, [], []).
printbase_mul_by_digit_2(Base, D, Div, [X | Xs], [Mod | NewXs]) :-
    printbase_mul_by_digit_2(Base, D, DivXs, Xs, NewXs),
    printbase_chop(Base, D * X + DivXs, Div, Mod).

:- func printbase_pos_plus(printbase, integer, integer) = integer.

printbase_pos_plus(Base, i(L1, D1), i(L2, D2)) = Out :-
    printbase_add_pairs(Base, Div, i(L1, D1), i(L2, D2), Ds),
    ( if L1 > L2 then
        Len = L1
    else
        Len = L2
    ),
    ( if Div = 0 then
        Out = i(Len, Ds)
    else
        Out = i(Len + 1, [Div | Ds])
    ).

:- pred printbase_add_pairs(printbase::in, digit::out,
    integer::in, integer::in, list(digit)::out) is det.

printbase_add_pairs(Base, Div, i(L1, D1), i(L2, D2), Ds) :-
    ( if L1 = L2 then
        printbase_add_pairs_equal(Base, Div, D1, D2, Ds)
    else if L1 < L2, D2 = [H2 | T2] then
        printbase_add_pairs(Base, Div1, i(L1, D1), i(L2 - 1, T2), Ds1),
        printbase_chop(Base, H2 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    else if L1 > L2, D1 = [H1 | T1] then
        printbase_add_pairs(Base, Div1, i(L1 - 1, T1), i(L2, D2), Ds1),
        printbase_chop(Base, H1 + Div1, Div, Mod),
        Ds = [Mod | Ds1]
    else
        unexpected($pred, "integer.printbase_add_pairs")
    ).

:- pred printbase_add_pairs_equal(printbase::in, digit::out,
    list(digit)::in, list(digit)::in, list(digit)::out) is det.

printbase_add_pairs_equal(_, 0, [], _, []).
printbase_add_pairs_equal(_, 0, [_ | _], [], []).
printbase_add_pairs_equal(Base, Div, [X | Xs], [Y | Ys], [Mod | TailDs]) :-
    printbase_add_pairs_equal(Base, DivTail, Xs, Ys, TailDs),
    printbase_chop(Base, X + Y + DivTail, Div, Mod).

:- func printbase_pos_mul(printbase, integer, integer) = integer.

printbase_pos_mul(Base, i(L1, Ds1), i(L2, Ds2)) =
    ( if L1 < L2 then
        printbase_pos_mul_list(Base, Ds1, integer.zero, i(L2, Ds2))
    else
        printbase_pos_mul_list(Base, Ds2, integer.zero, i(L1, Ds1))
    ).

:- func printbase_pos_mul_list(printbase, list(digit), integer, integer)
    = integer.

printbase_pos_mul_list(_Base, [], Carry, _Y) = Carry.
printbase_pos_mul_list(Base, [X | Xs], Carry, Y) =
    printbase_pos_mul_list(Base, Xs,
        printbase_pos_plus(Base, mul_base(Carry),
            printbase_mul_by_digit(Base, X, Y)), Y).

%---------------------------------------------------------------------------%
%
% Converting ints to integers.
%

integer(N) = int_to_integer(N).

% Note: Since most machines use 2's complement arithmetic,
% INT_MIN is usually -INT_MAX-1, hence -INT_MIN will cause int overflow.
% We handle overflow below.
% We don't check for a negative result from abs(), which would indicate
% overflow, since we may trap int overflow instead.
%
% XXX: What about machines that aren't 2's complement?

:- func int_to_integer(int) = integer.

int_to_integer(D) = Int :-
    ( if D = 0 then
        Int = integer.zero
    else if D > 0, D < base then
        Int = i(1, [D])
    else if D < 0, D > -base then
        Int = i(-1, [D])
    else
        ( if int.min_int(D) then
            % Were we to call int.abs, int overflow might occur.
            Int = integer(D + 1) - integer.one
        else
            Int = big_sign(D, pos_int_to_digits(int.abs(D)))
        )
    ).

:- func pos_int_to_digits(int) = integer.

pos_int_to_digits(D) = pos_int_to_digits_2(D, integer.zero).

:- func pos_int_to_digits_2(int, integer) = integer.

pos_int_to_digits_2(D, Tail) = Result :-
    ( if D = 0 then
        Result = Tail
    else
        Tail = i(Length, Digits),
        chop(D, Div, Mod),
        Result = pos_int_to_digits_2(Div, i(Length + 1, [Mod | Digits]))
    ).

%---------------------------------------------------------------------------%
%
% Converting uints to integers.
%

from_uint(U) = Integer :-
    ( if U = 0u then
        Integer = integer.zero
    else if U < cast_from_int(base) then
        Integer = i(1, [cast_to_int(U)])
    else
        Integer = uint_to_digits(U)
    ).

:- func uint_to_digits(uint) = integer.

uint_to_digits(U) = uint_to_digits_2(U, integer.zero).

:- func uint_to_digits_2(uint, integer) = integer.

uint_to_digits_2(U, Tail) = Result :-
    ( if U = 0u then
        Result = Tail
    else
        Tail = i(Length, Digits),
        chop_uint(U, Div, Mod),
        Result = uint_to_digits_2(Div,
            i(Length + 1, [cast_to_int(Mod) | Digits]))
    ).

%---------------------------------------------------------------------------%

from_int8(I8) = Integer :-
    I = int8.to_int(I8),
    Integer = integer(I).

from_uint8(U8) = Integer :-
    I = uint8.to_int(U8),
    Integer = integer(I).

from_int16(I16) = Integer :-
    I = int16.to_int(I16),
    Integer = integer(I).

from_uint16(U16) = Integer :-
    I = uint16.to_int(U16),
    Integer = integer(I).

from_int32(I32) = Integer :-
    I = int32.to_int(I32),
    Integer = integer(I).

from_uint32(U32) = Integer :-
    ( if U32 = 0u32 then
        Integer = integer.zero
    else if U32 < cast_from_int(base) then
        Integer = i(1, [cast_to_int(U32)])
    else
        Integer = uint32_to_digits(U32)
    ).

:- func uint32_to_digits(uint32) = integer.

uint32_to_digits(U) = uint32_to_digits_2(U, integer.zero).

:- func uint32_to_digits_2(uint32, integer) = integer.

uint32_to_digits_2(U, Tail) = Result :-
    ( if U = 0u32 then
        Result = Tail
    else
        Tail = i(Length, Digits),
        chop_uint32(U, Div, Mod),
        Result = uint32_to_digits_2(Div,
            i(Length + 1, [cast_to_int(Mod) | Digits]))
    ).

from_int64(I64) = Integer :-
    ( if
        I64 = from_int(0)
    then
        Integer = integer.zero
    else if
        I64 > from_int(0),
        I64 < from_int(base)
    then
        Integer = i(1, [cast_to_int(I64)])
    else if
        I64 < from_int(0),
        I64 > from_int(-base)
    then
        Integer = i(-1, [cast_to_int(I64)])
    else if I64 = int64.min_int64 then
        % If we were to call int64.abs with mint_int64 as an argument it would
        % overflow.
        Integer = integer.from_int64(I64 + from_int(1)) - integer.one
    else
        Magnitude = pos_int64_to_digits(int64.abs(I64)),
        ( if I64 < int64.from_int(0) then
            Integer = big_neg(Magnitude)
        else
            Integer = Magnitude
        )
    ).

:- func pos_int64_to_digits(int64) = integer.

pos_int64_to_digits(D) = pos_int64_to_digits_2(D, integer.zero).

:- func pos_int64_to_digits_2(int64, integer) = integer.

pos_int64_to_digits_2(D, Tail) = Result :-
    ( if D = int64.from_int(0) then
        Result = Tail
    else
        Tail = i(Length, Digits),
        chop_int64(D, Div, Mod),
        Result = pos_int64_to_digits_2(Div,
            i(Length + 1, [cast_to_int(Mod) | Digits]))
    ).

from_uint64(U64) = Integer :-
    ( if U64 = cast_from_int(0) then
        Integer = integer.zero
    else if U64 < cast_from_int(base) then
        Integer = i(1, [cast_to_int(U64)])
    else
        Integer = uint64_to_digits(U64)
    ).

:- func uint64_to_digits(uint64) = integer.

uint64_to_digits(U) = uint64_to_digits_2(U, integer.zero).

:- func uint64_to_digits_2(uint64, integer) = integer.

uint64_to_digits_2(U, Tail) = Result :-
    ( if U = cast_from_int(0) then
        Result = Tail
    else
        Tail = i(Length, Digits),
        chop_uint64(U, Div, Mod),
        Result = uint64_to_digits_2(Div,
            i(Length + 1, [cast_to_int(Mod) | Digits]))
    ).

%---------------------------------------------------------------------------%
%
% Converting strings to integers.
%

from_string(S, Big) :-
    string.to_char_list(S, Cs),
    string_to_integer(Cs, Big).

:- pred string_to_integer(list(char)::in, integer::out) is semidet.

string_to_integer(Chars, Integer) :-
    Chars = [HeadChar | TailChars],
    ( if HeadChar = ('-') then
        TailChars = [_ | _], % Don't accept just "-" as an integer.
        string_to_integer_acc(TailChars, integer.zero, PosInteger),
        Integer = big_sign(-1, PosInteger)
    else if HeadChar = ('+') then
        TailChars = [_ | _], % Don't accept just "+" as an integer.
        string_to_integer_acc(TailChars, integer.zero, Integer)
    else
        string_to_integer_acc(Chars, integer.zero, Integer)
    ).

:- pred string_to_integer_acc(list(char)::in, integer::in, integer::out)
    is semidet.

string_to_integer_acc([], !Integer).
string_to_integer_acc([C | Cs], !Integer) :-
    % The if-then-else here is acting as a sequential conjunction.
    % It is needed to guarantee termination with --reorder-conj.
    % Without it, the value of `Digit0 - Z' might be negative and
    % then the call to pos_int_to_digits/1 may not terminate.
    ( if char.is_digit(C) then
        Digit0 = char.to_int(C),
        Z = char.to_int('0'),
        Digit = pos_int_to_digits(Digit0 - Z),
        !:Integer = pos_plus(Digit, mul_by_digit(10, !.Integer)),
        string_to_integer_acc(Cs, !Integer)
    else
        fail
    ).

det_from_string(S) = I :-
    ( if integer.from_string(S, IPrime) then
        I = IPrime
    else
        unexpected($pred, "conversion failed")
    ).

%---------------------------------------------------------------------------%
%
% Converting base strings to integers.
%

from_base_string(Base, String, Integer) :-
    string.index(String, 0, Char),
    Len = string.length(String),
    ( if Char = ('-') then
        Len > 1,
        string.foldl_between(accumulate_integer(Base), String, 1, Len,
            integer.zero, PosInteger),
        Integer = -PosInteger
    else if Char = ('+') then
        Len > 1,
        string.foldl_between(accumulate_integer(Base), String, 1, Len,
            integer.zero, Integer)
    else
        string.foldl_between(accumulate_integer(Base), String, 0, Len,
            integer.zero, Integer)
    ).

:- pred accumulate_integer(int::in, char::in, integer::in, integer::out)
    is semidet.

accumulate_integer(Base, Char, !N) :-
    char.base_digit_to_int(Base, Char, Digit0),
    Digit = integer(Digit0),
    !:N = (integer(Base) * !.N) + Digit.

det_from_base_string(Base, String) = Integer :-
    ( if integer.from_base_string(Base, String, IntegerPrime) then
        Integer = IntegerPrime
    else
        unexpected($pred, "conversion failed")
    ).

from_base_string_underscore(Base, String, Integer) :-
    string.index(String, 0, Char),
    Len = string.length(String),
    ( if Char = ('-') then
        Len > 1,
        string.foldl_between(accumulate_integer_underscore(Base), String,
            1, Len, integer.zero, PosInteger),
        Integer = -PosInteger
    else if Char = ('+') then
        Len > 1,
        string.foldl_between(accumulate_integer_underscore(Base), String,
            1, Len, integer.zero, Integer)
    else
        string.foldl_between(accumulate_integer_underscore(Base), String,
            0, Len, integer.zero, Integer)
    ).

:- pred accumulate_integer_underscore(int::in, char::in,
    integer::in, integer::out) is semidet.

accumulate_integer_underscore(Base, Char, !N) :-
    ( if char.base_digit_to_int(Base, Char, Digit0) then
        Digit = integer(Digit0),
        !:N = (integer(Base) * !.N) + Digit
    else if Char = '_' then
        true
    else
        false
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
