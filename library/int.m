%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: int.m.
% Main authors: conway, fjh.
% Stability: medium.
%
% Predicates and functions for dealing with machine-size integer numbers.
%
% The behaviour of a computation for which overflow occurs is undefined.
% (In the current implementation, the predicates and functions in this
% module do not check for overflow, and the results you get are those
% delivered by the C compiler.  However, future implementations
% might check for overflow.)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module int.
:- interface.

:- import_module array.
:- import_module enum.
:- import_module pretty_printer.

%-----------------------------------------------------------------------------%

:- instance enum(int).

    % Less than.
    %
:- pred (int::in) < (int::in) is semidet.

    % Greater than.
    %
:- pred (int::in) > (int::in) is semidet.

    % Less than or equal.
    %
:- pred (int::in) =< (int::in) is semidet.

    % Greater than or equal.
    %
:- pred (int::in) >= (int::in) is semidet.

    % Absolute value.
    %
:- func int.abs(int) = int.
:- pred int.abs(int::in, int::out) is det.

    % Maximum.
    %
:- func int.max(int, int) = int.
:- pred int.max(int::in, int::in, int::out) is det.

    % Minimum.
    %
:- func int.min(int, int) = int.
:- pred int.min(int::in, int::in, int::out) is det.

    % Exponentiation.
    % int.pow(X, Y, Z): Z is X raised to the Yth power.
    % Throws a `math.domain_error' exception if Y is negative.
    %
:- func int.pow(int, int) = int.
:- pred int.pow(int::in, int::in, int::out) is det.

    % Base 2 logarithm.
    % int.log2(X) = N is the least integer such that 2 to the power N
    % is greater than or equal to X.
    % Throws a `math.domain_error' exception if X is not positive.
    %
:- func int.log2(int) = int.
:- pred int.log2(int::in, int::out) is det.

    % addition
    %
:- func int + int = int.
:- mode in  + in  = uo  is det.
:- mode uo  + in  = in  is det.
:- mode in  + uo  = in  is det.

:- func int.plus(int, int) = int.

    % Multiplication.
    %
:- func (int::in) * (int::in) = (int::uo) is det.
:- func int.times(int, int) = int.

    % Subtraction.
    %
:- func int - int = int.
:- mode in  - in  = uo  is det.
:- mode uo  - in  = in  is det.
:- mode in  - uo  = in  is det.

:- func int.minus(int, int) = int.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10) div 3 = (-4).
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    % See the comments at the top of math.m to find out how to disable
    % domain checks.
    %
:- func div(int::in, int::in) = (int::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10) // 3 = (-3).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    % See the comments at the top of math.m to find out how to disable
    % domain checks.
    %
:- func (int::in) // (int::in) = (int::uo) is det.

    % (/)/2 is a synonym for (//)/2 to bring Mercury into line with
    % the common convention for naming integer division.
    %
:- func (int::in) / (int::in) = (int::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(int::in, int::in) = (int::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
:- func (int::in) mod (int::in) = (int::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y
    % `mod' has nicer mathematical properties for negative X,
    % but `rem' is typically more efficient.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    % See the comments at the top of math.m to find out how to disable
    % domain checks.
    %
:- func (int::in) rem (int::in) = (int::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_rem(int::in, int::in) = (int::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % To be precise, if Y is negative, the result is
    % X div (2^(-Y)), otherwise the result is X * (2^Y).
    %
:- func (int::in) << (int::in) = (int::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y
    % except that the behaviour is undefined if Y is negative,
    % or greater than or equal to the result of `int.bits_per_int/1'.
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(int::in, int::in) = (int::uo) is det.

    % Right shift.
    % X >> Y returns X "arithmetic right shifted" by Y bits.
    % To be precise, if Y is negative, the result is
    % X * (2^(-Y)), otherwise the result is X div (2^Y).
    %
:- func (int::in) >> (int::in) = (int::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y
    % except that the behaviour is undefined if Y is negative,
    % or greater than or equal to the result of `int.bits_per_int/1'.
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(int::in, int::in) = (int::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(int::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(int::in) is semidet.

    % Bitwise and.
    %
:- func (int::in) /\ (int::in) = (int::uo) is det.

    % Bitwise or.
    %
:- func (int::in) \/ (int::in) = (int::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func int.xor(int, int) = int.
:- mode int.xor(in, in) = uo is det.
:- mode int.xor(in, uo) = in is det.
:- mode int.xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (int::in) = (int::uo) is det.

    % Unary plus.
    %
:- func + (int::in) = (int::uo) is det.

    % Unary minus.
    %
:- func - (int::in) = (int::uo) is det.

    % is/2, for backwards compatibility with Prolog.
    %
:- pred is(T, T) is det.
:- mode is(uo, di) is det.
:- mode is(out, in) is det.

    % int.max_int is the maximum value of an int on this machine.
    %
:- func int.max_int = int.
:- pred int.max_int(int::out) is det.

    % int.min_int is the minimum value of an int on this machine.
    %
:- func int.min_int = int.
:- pred int.min_int(int::out) is det.

    % int.bits_per_int is the number of bits in an int on this machine.
    %
:- func int.bits_per_int = int.
:- pred int.bits_per_int(int::out) is det.

    % fold_up(F, Low, High, !Acc) <=> list.foldl(F, Low .. High, !Acc)
    %
    % NOTE: fold_up/5 is undefined if High = int.max_int.
    %
:- pred int.fold_up(pred(int, T, T), int, int, T, T).
:- mode int.fold_up(pred(in, in, out) is det, in, in, in, out) is det.
:- mode int.fold_up(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode int.fold_up(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode int.fold_up(pred(in, array_di, array_uo) is det, in, in,
    array_di, array_uo) is det.
:- mode int.fold_up(pred(in, in, out) is semidet, in, in, in, out)
    is semidet.
:- mode int.fold_up(pred(in, mdi, muo) is semidet, in, in, mdi, muo)
    is semidet.
:- mode int.fold_up(pred(in, di, uo) is semidet, in, in, di, uo)
    is semidet.
:- mode int.fold_up(pred(in, in, out) is nondet, in, in, in, out)
    is nondet.
:- mode int.fold_up(pred(in, mdi, muo) is nondet, in, in, mdi, muo) 
    is nondet.
:- mode int.fold_up(pred(in, di, uo) is cc_multi, in, in, di, uo)
    is cc_multi.
:- mode int.fold_up(pred(in, in, out) is cc_multi, in, in, in, out)
    is cc_multi.

    % fold_up(F, Low, High, Acc) <=> list.foldl(F, Low .. High, Acc)
    %
    % NOTE: fold_up/4 is undefined if High = int.max_int.
    %
:- func int.fold_up(func(int, T) = T, int, int, T) = T.

    % fold_down(F, Low, High, !Acc) <=> list.foldr(F, Low .. High, !Acc)
    %
    % NOTE: fold_down/5 is undefined if Low int.min_int.
    %
:- pred int.fold_down(pred(int, T, T), int, int, T, T).
:- mode int.fold_down(pred(in, in, out) is det, in, in, in, out) is det.
:- mode int.fold_down(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode int.fold_down(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode int.fold_down(pred(in, array_di, array_uo) is det, in, in,
    array_di, array_uo) is det.
:- mode int.fold_down(pred(in, in, out) is semidet, in, in, in, out)
    is semidet.
:- mode int.fold_down(pred(in, mdi, muo) is semidet, in, in, mdi, muo)
    is semidet.
:- mode int.fold_down(pred(in, di, uo) is semidet, in, in, di, uo)
    is semidet.
:- mode int.fold_down(pred(in, in, out) is nondet, in, in, in, out)
    is nondet.
:- mode int.fold_down(pred(in, mdi, muo) is nondet, in, in, mdi, muo)
    is nondet.
:- mode int.fold_down(pred(in, in, out) is cc_multi, in, in, in, out)
    is cc_multi.
:- mode int.fold_down(pred(in, di, uo) is cc_multi, in, in, di, uo)
    is cc_multi.

    % fold_down(F, Low, High, Acc) <=> list.foldr(F, Low .. High, Acc)
    %
    % NOTE: fold_down/4 is undefined if Low = int.min_int.
    %
:- func int.fold_down(func(int, T) = T, int, int, T) = T.

    % fold_up2(F, Low, High, !Acc1, Acc2) <=>
    %   list.foldl2(F, Low .. High, !Acc1, !Acc2)
    %
    % NOTE: fold_up2/7 is undefined if High = int.max_int.
    %
:- pred int.fold_up2(pred(int, T, T, U, U), int, int, T, T, U, U).
:- mode int.fold_up2(pred(in, in, out, in, out) is det, in, in, in, out,
    in, out) is det.
:- mode int.fold_up2(pred(in, in, out, mdi, muo) is det, in, in, in, out,
    mdi, muo) is det.
:- mode int.fold_up2(pred(in, in, out, di, uo) is det, in, in, in, out,
    di, uo) is det.
:- mode int.fold_up2(pred(in, di, uo, di, uo) is det, in, in, di, uo,
    di, uo) is det.
:- mode int.fold_up2(pred(in, in, out, array_di, array_uo) is det, in, in,
    in, out, array_di, array_uo) is det.
:- mode int.fold_up2(pred(in, in, out, in, out) is semidet, in, in,
    in, out, in, out) is semidet.
:- mode int.fold_up2(pred(in, in, out, mdi, muo) is semidet, in, in,
    in, out, mdi, muo) is semidet.
:- mode int.fold_up2(pred(in, in, out, di, uo) is semidet, in, in,
    in, out, di, uo) is semidet.
:- mode int.fold_up2(pred(in, in, out, in, out) is nondet, in, in,
    in, out, in, out) is nondet.
:- mode int.fold_up2(pred(in, in, out, mdi, muo) is nondet, in, in,
    in, out, mdi, muo) is nondet.

    % fold_down2(F, Low, High, !Acc1, !Acc2) <=>
    %   list.foldr2(F, Low .. High, !Acc1, Acc2).
    %
    % NOTE: fold_down2/7 is undefined if Low = int.min_int.
    %
:- pred int.fold_down2(pred(int, T, T, U, U), int, int, T, T, U, U).
:- mode int.fold_down2(pred(in, in, out, in, out) is det, in, in, in, out,
    in, out) is det.
:- mode int.fold_down2(pred(in, in, out, mdi, muo) is det, in, in, in, out,
    mdi, muo) is det.
:- mode int.fold_down2(pred(in, in, out, di, uo) is det, in, in, in, out,
    di, uo) is det.
:- mode int.fold_down2(pred(in, di, uo, di, uo) is det, in, in, di, uo,
    di, uo) is det.
:- mode int.fold_down2(pred(in, in, out, array_di, array_uo) is det, in, in,
    in, out, array_di, array_uo) is det.
:- mode int.fold_down2(pred(in, in, out, in, out) is semidet, in, in,
    in, out, in, out) is semidet.
:- mode int.fold_down2(pred(in, in, out, di, uo) is semidet, in, in,
    in, out, di, uo) is semidet.
:- mode int.fold_down2(pred(in, in, out, in, out) is nondet, in, in,
    in, out, in, out) is nondet.
:- mode int.fold_down2(pred(in, in, out, mdi, muo) is nondet, in, in,
    in, out, mdi, muo) is nondet.
    
    % fold_up3(F, Low, High, !Acc1, Acc2, !Acc3) <=>
    %   list.foldl3(F, Low .. High, !Acc1, !Acc2, !Acc3)
    %
    % NOTE: fold_up3/9 is undefined if High = int.max_int.
    %
:- pred int.fold_up3(pred(int, T, T, U, U, V, V), int, int, T, T, U, U, V, V).
:- mode int.fold_up3(pred(in, in, out, in, out, in, out) is det,
    in, in, in, out, in, out, in, out) is det.
:- mode int.fold_up3(pred(in, in, out, in, out, mdi, muo) is det,
    in, in, in, out, in, out, mdi, muo) is det.
:- mode int.fold_up3(pred(in, in, out, in, out, di, uo) is det,
    in, in, in, out, in, out, di, uo) is det.
:- mode int.fold_up3(pred(in, in, out, di, uo, di, uo) is det,
    in, in, in, out, di, uo, di, uo) is det.
:- mode int.fold_up3(pred(in, in, out, in, out, array_di, array_uo) is det,
    in, in, in, out, in, out, array_di, array_uo) is det.
:- mode int.fold_up3(pred(in, in, out, in, out, in, out) is semidet,
    in, in, in, out, in, out, in, out) is semidet.
:- mode int.fold_up3(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, in, in, out, in, out, mdi, muo) is semidet.
:- mode int.fold_up3(pred(in, in, out, in, out, di, uo) is semidet,
    in, in, in, out, in, out, di, uo) is semidet.
:- mode int.fold_up3(pred(in, in, out, in, out, in, out) is nondet,
    in, in, in, out, in, out, in, out) is nondet.
:- mode int.fold_up3(pred(in, in, out, in, out, mdi, muo) is nondet,
    in, in, in, out, in, out, mdi, muo) is nondet.
    
    % fold_up3(F, Low, High, !Acc1, Acc2, !Acc3) <=>
    %   list.foldr3(F, Low .. High, !Acc1, !Acc2, !Acc3)
    %
    % NOTE: fold_down3/9 is undefined if Low = int.min_int.
    %
:- pred int.fold_down3(pred(int, T, T, U, U, V, V), int, int, T, T, U, U, V, V).
:- mode int.fold_down3(pred(in, in, out, in, out, in, out) is det,
    in, in, in, out, in, out, in, out) is det.
:- mode int.fold_down3(pred(in, in, out, in, out, mdi, muo) is det,
    in, in, in, out, in, out, mdi, muo) is det.
:- mode int.fold_down3(pred(in, in, out, in, out, di, uo) is det,
    in, in, in, out, in, out, di, uo) is det.
:- mode int.fold_down3(pred(in, in, out, di, uo, di, uo) is det,
    in, in, in, out, di, uo, di, uo) is det.
:- mode int.fold_down3(pred(in, in, out, in, out, array_di, array_uo) is det,
    in, in, in, out, in, out, array_di, array_uo) is det.
:- mode int.fold_down3(pred(in, in, out, in, out, in, out) is semidet,
    in, in, in, out, in, out, in, out) is semidet.
:- mode int.fold_down3(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, in, in, out, in, out, mdi, muo) is semidet.
:- mode int.fold_down3(pred(in, in, out, in, out, di, uo) is semidet,
    in, in, in, out, in, out, di, uo) is semidet.
:- mode int.fold_down3(pred(in, in, out, in, out, in, out) is nondet,
    in, in, in, out, in, out, in, out) is nondet.
:- mode int.fold_down3(pred(in, in, out, in, out, mdi, muo) is nondet,
    in, in, in, out, in, out, mdi, muo) is nondet.

    % nondet_int_in_range(Lo, Hi, I):
    %
    % On successive successes, set I to every integer from Lo to Hi.
    %
:- pred nondet_int_in_range(int::in, int::in, int::out) is nondet.

    % Convert an int to a pretty_printer.doc for formatting.
    %
:- func int.int_to_doc(int) = pretty_printer.doc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- interface.

    % Everything below here will not appear in the
    % Mercury Library Reference Manual.

%-----------------------------------------------------------------------------%

    % commutativity and associativity of +
:- promise all [A, B, C]        ( C = B + A <=> C = A + B ).
:- promise all [A, B, C, ABC]   ( ABC = (A + B) + C <=> ABC = A + (B + C) ).

    % commutativity and associativity of *
:- promise all [A, B, C]        ( C = B * A <=> C = A * B ).
:- promise all [A, B, C, ABC]   ( ABC = (A * B) * C <=> ABC = A * (B * C) ).

%-----------------------------------------------------------------------------%

    % floor_to_multiple_of_bits_per_int(Int):
    %
    % Returns the largest multiple of bits_per_int which is less than or
    % equal to `Int'.
    %
    % Used by sparse_bitset.m. Makes it clearer to gcc that parts of this
    % operation can be optimized into shifts, without turning up the
    % optimization level.
    %
:- func floor_to_multiple_of_bits_per_int(int) = int.

    % Used by floor_to_multiple_of_bits_per_int, placed here to make sure
    % they go in the `.opt' file.

    % int.quot_bits_per_int(X) = X // bits_per_int.
    %
:- func int.quot_bits_per_int(int) = int.

    % int.times_bits_per_int(X) = X * bits_per_int.
    %
:- func int.times_bits_per_int(int) = int.

    % Used by bitmap.m.  Like the ones above, the purpose of defining this in C
    % is to make it clearer to gcc that this can be optimized.

    % int.rem_bits_per_int(X) = X `rem` bits_per_int.
    %
:- func int.rem_bits_per_int(int) = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module math.
:- import_module string.

%-----------------------------------------------------------------------------%

:- instance enum(int) where [
    to_int(X) = X,
    from_int(X) = X
].

% Most of the arithmetic and comparison operators are recognized by
% the compiler as builtins, so we don't need to define them here.

X div Y = Div :-
    Trunc = X // Y,
    (
        ( X >= 0, Y >= 0
        ; X < 0, Y < 0
        ; X rem Y = 0
        )
    ->
        Div = Trunc
    ;
        Div = Trunc - 1
    ).

:- pragma inline('//'/2).
X // Y = Div :-
    (
        int_domain_checks,
        Y = 0
    ->
        throw(math.domain_error("int.'//': division by zero"))
    ;
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    (
        int_domain_checks,
        Y = 0
    ->
        throw(math.domain_error("int.rem: division by zero"))
    ;
        Rem = unchecked_rem(X, Y)
    ).

    % This code is included here rather than just calling the version
    % in math.m because we currently don't do transitive inter-module
    % inlining, so code which uses `//'/2 but doesn't import math.m
    % couldn't have the domain check optimized away.
:- pred int_domain_checks is semidet.
:- pragma inline(int_domain_checks/0).

:- pragma foreign_proc("C",
    int_domain_checks,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#ifdef ML_OMIT_MATH_DOMAIN_CHECKS
    SUCCESS_INDICATOR = MR_FALSE;
#else
    SUCCESS_INDICATOR = MR_TRUE;
#endif
").

:- pragma foreign_proc("C#",
    int_domain_checks,
    [thread_safe, promise_pure],
"
#if ML_OMIT_MATH_DOMAIN_CHECKS
    SUCCESS_INDICATOR = false;
#else
    SUCCESS_INDICATOR = true;
#endif
").
:- pragma foreign_proc("Java",
    int_domain_checks,
    [thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = true;
").
:- pragma foreign_proc("Erlang",
    int_domain_checks,
    [thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = true
").

:- pragma inline(floor_to_multiple_of_bits_per_int/1).

floor_to_multiple_of_bits_per_int(X) = Floor :-
    Trunc = quot_bits_per_int(X),
    Floor0 = times_bits_per_int(Trunc),
    ( Floor0 > X ->
        Floor = Floor0 - bits_per_int
    ;
        Floor = Floor0
    ).

X mod Y = X - (X div Y) * Y.

X << Y = Z :-
    int.bits_per_int(IntBits),
    ( Y >= 0 ->
        ( Y >= IntBits ->
            Z = 0
        ;
            Z = unchecked_left_shift(X, Y)
        )
    ;
        ( Y =< -IntBits ->
            Z = (if X >= 0 then 0 else -1)
        ;
            Z = unchecked_right_shift(X, -Y)
        )
    ).

    % Note: this assumes two's complement arithmetic.
    % tests/hard_coded/shift_test.m will fail if this is not the case.
X >> Y = Z :-
    int.bits_per_int(IntBits),
    ( Y >= 0 ->
        ( Y >= IntBits ->
            Z = (if X >= 0 then 0 else -1)
        ;
            Z = unchecked_right_shift(X, Y)
        )
    ;
        ( Y =< -IntBits ->
            Z = 0
        ;
            Z = unchecked_left_shift(X, -Y)
        )
    ).

:- pragma inline(even/1).
even(X):-
    (X /\ 1) = 0.

:- pragma inline(odd/1).
odd(X):-
    (X /\ 1) \= 0.

int.abs(Num) = Abs :-
    int.abs(Num, Abs).

int.abs(Num, Abs) :-
    ( Num < 0 ->
        Abs = 0 - Num
    ;
        Abs = Num
    ).

int.max(X, Y) = Max :-
    int.max(X, Y, Max).

int.max(X, Y, Max) :-
    ( X > Y ->
        Max = X
    ;
        Max = Y
    ).

int.min(X, Y) = Min :-
    int.min(X, Y, Min).

int.min(X, Y, Min) :-
    ( X < Y ->
        Min = X
    ;
        Min = Y
    ).

int.pow(Base, Exp) = Result :-
    int.pow(Base, Exp, Result).

int.pow(Base, Exp, Result) :-
    ( int_domain_checks, Exp < 0 ->
        throw(math.domain_error("int.pow: zero base"))
    ;
        Result = int.multiply_by_pow(1, Base, Exp)
    ).

    % Returns Scale0 * (Base ** Exp).
    % Requires that Exp >= 0.
    %
:- func int.multiply_by_pow(int, int, int) = int.

int.multiply_by_pow(Scale0, Base, Exp) = Result :-
    ( Exp = 0 ->
        Result = Scale0
    ;
        ( odd(Exp) ->
            Scale1 = Scale0 * Base
        ;
            Scale1 = Scale0
        ),
        Result = int.multiply_by_pow(Scale1, Base * Base, Exp div 2)
    ).

int.log2(X) = N :-
    int.log2(X, N).

int.log2(X, N) :-
    ( int_domain_checks, X =< 0 ->
        throw(math.domain_error("int.log2: taking logarithm of zero"))
    ;
        int.log2_2(X, 0, N)
    ).

:- pred int.log2_2(int, int, int).
:- mode int.log2_2(in, in, out) is det.

int.log2_2(X, N0, N) :-
    ( X = 1 ->
        N = N0
    ;
        X1 = X + 1,
        X2 = X1 // 2,
        N1 = N0 + 1,
        int.log2_2(X2, N1, N)
    ).

%-----------------------------------------------------------------------------%

% is/2 is replaced with `=' in the parser, but the following is useful
% in case you should take the address of `is' or something weird like that.

is(X, X).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <limits.h>

    #define ML_BITS_PER_INT     (sizeof(MR_Integer) * CHAR_BIT)
").

int.max_int = X :-
    int.max_int(X).

:- pragma foreign_proc("C",
    int.max_int(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    if (sizeof(MR_Integer) == sizeof(int)) {
        Max = INT_MAX;
    } else if (sizeof(MR_Integer) == sizeof(long)) {
        Max = (MR_Integer) LONG_MAX;
    #if defined(LLONG_MAX)
    } else if (sizeof(MR_Integer) == sizeof(long long)) {
        Max = (MR_Integer) LLONG_MAX;
    #endif
    } else {
        MR_fatal_error(""Unable to figure out max integer size"");
    }
").

int.min_int = X :-
    int.min_int(X).

:- pragma foreign_proc("C",
    int.min_int(Min::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    if (sizeof(MR_Integer) == sizeof(int)) {
        Min = INT_MIN;
    } else if (sizeof(MR_Integer) == sizeof(long)) {
        Min = (MR_Integer) LONG_MIN;
    #if defined(LLONG_MIN)
    } else if (sizeof(MR_Integer) == sizeof(long long)) {
        Min = (MR_Integer) LLONG_MIN;
    #endif
    } else {
        MR_fatal_error(""Unable to figure out min integer size"");
    }
").

int.bits_per_int = X :-
    int.bits_per_int(X).

:- pragma foreign_proc("C",
    int.bits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Bits = ML_BITS_PER_INT;
").

:- pragma foreign_proc("C",
    int.quot_bits_per_int(Int::in) = (Div::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Div = Int / ML_BITS_PER_INT;
").

:- pragma foreign_proc("C",
    int.times_bits_per_int(Int::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Result = Int * ML_BITS_PER_INT;
").

:- pragma foreign_proc("C",
    int.rem_bits_per_int(Int::in) = (Rem::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Rem = Int % ML_BITS_PER_INT;
").

:- pragma foreign_proc("C#",
    int.max_int(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = System.Int32.MaxValue;
").

:- pragma foreign_proc("C#",
    int.min_int(Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Min = System.Int32.MinValue;
").

:- pragma foreign_proc("C#",
    int.bits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // we are using int32 in the compiler.
    // XXX would be better to avoid hard-coding this here.
    Bits = 32;
").

:- pragma foreign_proc("Java",
    int.max_int(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = java.lang.Integer.MAX_VALUE;
").

:- pragma foreign_proc("Java",
    int.min_int(Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Min = java.lang.Integer.MIN_VALUE;
").

:- pragma foreign_proc("Java",
    int.bits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Java ints are 32 bits.
    Bits = 32;
").

:- pragma foreign_proc("Erlang",
    int.max_int(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX Erlang ints are actually arbitrary precision.
    Max = (1 bsl 31) - 1
").

:- pragma foreign_proc("Erlang",
    int.min_int(Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX Erlang ints are actually arbitrary precision.
    Min = -(1 bsl 31)
").

:- pragma foreign_proc("Erlang",
    int.bits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX Erlang ints are actually arbitrary precision.
    Bits = 32
").

int.quot_bits_per_int(Int::in) = (Result::out) :-
    Result = Int // int.bits_per_int.

int.times_bits_per_int(Int::in) = (Result::out) :-
    Result = Int * int.bits_per_int.

int.rem_bits_per_int(Int::in) = (Result::out) :-
    Result = Int rem int.bits_per_int.

%-----------------------------------------------------------------------------%

int.fold_up(P, Lo, Hi, !A) :-
    ( if    Lo =< Hi
      then  P(Lo, !A), int.fold_up(P, Lo + 1, Hi, !A)
      else  true
    ).

int.fold_up(F, Lo, Hi, A) =
    ( if Lo =< Hi then int.fold_up(F, Lo + 1, Hi, F(Lo, A)) else A ).

int.fold_up2(P, Lo, Hi, !A, !B) :-
    ( if    Lo =< Hi
      then  P(Lo, !A, !B), int.fold_up2(P, Lo + 1, Hi, !A, !B)
      else  true
    ).

int.fold_up3(P, Lo, Hi, !A, !B, !C) :-
    ( if    Lo =< Hi
      then  P(Lo, !A, !B, !C), int.fold_up3(P, Lo + 1, Hi, !A, !B, !C)
      else  true
    ).

%-----------------------------------------------------------------------------%

int.fold_down(P, Lo, Hi, !A) :-
    ( if    Lo =< Hi
      then  P(Hi, !A), int.fold_down(P, Lo, Hi - 1, !A)
      else  true
    ).

int.fold_down(F, Lo, Hi, A) =
    ( if Lo =< Hi then int.fold_down(F, Lo, Hi - 1, F(Hi, A)) else A ).

int.fold_down2(P, Lo, Hi, !A, !B) :-
    ( if    Lo =< Hi
      then  P(Hi, !A, !B), int.fold_down2(P, Lo, Hi - 1, !A, !B)
      else  true
    ).

int.fold_down3(P, Lo, Hi, !A, !B, !C) :-
    ( if    Lo =< Hi
      then  P(Hi, !A, !B, !C), int.fold_down3(P, Lo, Hi - 1, !A, !B, !C)
      else  true
    ).

%-----------------------------------------------------------------------------%

nondet_int_in_range(Lo, Hi, I) :-
    % Leave a choice point only if there is at least one solution
    % to find on backtracking.
    ( Lo < Hi ->
        (
            I = Lo
        ;
            nondet_int_in_range(Lo + 1, Hi, I)
        )
    ;
        Lo = Hi,
        I = Lo
    ).

%-----------------------------------------------------------------------------%

int.int_to_doc(X) = str(string.int_to_string(X)).

%-----------------------------------------------------------------------------%
:- end_module int.
%-----------------------------------------------------------------------------%
