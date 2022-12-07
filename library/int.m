%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2018, 2020-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
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
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module int.
:- interface.

:- import_module array.
:- import_module enum.
:- import_module pretty_printer.

%---------------------------------------------------------------------------%

:- instance enum(int).
:- instance uenum(int).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

    % abs(X) returns the absolute value of X.
    % Throws an exception if X = int.min_int.
    %
:- func abs(int) = int.
:- pred abs(int::in, int::out) is det.

    % unchecked_abs(X) returns the absolute value of X, except that the result
    % is undefined if X = int.min_int.
    %
:- func unchecked_abs(int) = int.

    % nabs(X) returns the negative absolute value of X.
    % Unlike abs/1 this function is defined for X = int.min_int.
    %
:- func nabs(int) = int.

%---------------------------------------------------------------------------%

    % Maximum.
    %
:- func max(int, int) = int.
:- pred max(int::in, int::in, int::out) is det.

    % Minimum.
    %
:- func min(int, int) = int.
:- pred min(int::in, int::in, int::out) is det.

%---------------------------------------------------------------------------%

    % Unary plus.
    %
:- func + (int::in) = (int::uo) is det.

    % Unary minus.
    %
:- func - (int::in) = (int::uo) is det.

    % Addition.
    %
:- func int + int = int.
:- mode in  + in  = uo  is det.
:- mode uo  + in  = in  is det.
:- mode in  + uo  = in  is det.

:- func plus(int, int) = int.

    % Subtraction.
    %
:- func int - int = int.
:- mode in  - in  = uo  is det.
:- mode uo  - in  = in  is det.
:- mode in  - uo  = in  is det.

:- func minus(int, int) = int.

    % Multiplication.
    %
:- func (int::in) * (int::in) = (int::uo) is det.
:- func times(int, int) = int.

    % Flooring integer division.
    % Truncates towards minus infinity, e.g. (-10) div 3 = (-4).
    %
    % Throws a `domain_error' exception if the right operand is zero.
    % See the comments at the top of math.m to find out how to disable
    % domain checks.
    %
:- func div(int::in, int::in) = (int::uo) is det.

    % Truncating integer division.
    % Truncates towards zero, e.g. (-10) // 3 = (-3).
    % `div' has nicer mathematical properties for negative operands,
    % but `//' is typically more efficient.
    %
    % Throws a `domain_error' exception if the right operand is zero.
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
    % Throws a `domain_error' exception if the right operand is zero.
    % See the comments at the top of math.m to find out how to disable
    % domain checks.
    %
:- func (int::in) rem (int::in) = (int::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_rem(int::in, int::in) = (int::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(int::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(int::in) is semidet.

    % Exponentiation.
    % pow(X, Y, Z): Z is X raised to the Yth power.
    % Throws a `domain_error' exception if Y is negative.
    %
:- func pow(int, int) = int.
:- pred pow(int::in, int::in, int::out) is det.

    % Base 2 logarithm.
    % log2(X) = N is the least integer such that 2 to the power N
    % is greater than or equal to X.
    % Throws a `domain_error' exception if X is not positive.
    %
:- func log2(int) = int.
:- pred log2(int::in, int::out) is det.

%---------------------------------------------------------------------------%

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, bits_per_int).
    %
:- func (int::in) << (int::in) = (int::uo) is det.
:- func (int::in) <<u (uint::in) = (int::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y
    % except that the behaviour is undefined if Y is negative,
    % or greater than or equal to the result of `bits_per_int/1'.
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(int::in, int::in) = (int::uo) is det.
:- func unchecked_left_ushift(int::in, uint::in) = (int::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by the sign bit.
    % Throws an exception if Y is not in [0, bits_per_int).
    %
:- func (int::in) >> (int::in) = (int::uo) is det.
:- func (int::in) >>u (uint::in) = (int::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y
    % except that the behaviour is undefined if Y is negative,
    % or greater than or equal to the result of `bits_per_int/1'.
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(int::in, int::in) = (int::uo) is det.
:- func unchecked_right_ushift(int::in, uint::in) = (int::uo) is det.

%---------------------------------------------------------------------------%

    % Bitwise complement.
    %
:- func \ (int::in) = (int::uo) is det.

    % Bitwise and.
    %
:- func (int::in) /\ (int::in) = (int::uo) is det.

    % Bitwise or.
    %
:- func (int::in) \/ (int::in) = (int::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(int, int) = int.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

%---------------------------------------------------------------------------%

    % max_int is the maximum value of an int on this machine.
    %
:- func max_int = int.
:- pred max_int(int::out) is det.

    % min_int is the minimum value of an int on this machine.
    %
:- func min_int = int.
:- pred min_int(int::out) is det.

    % bits_per_int and ubits_per_int both return the number of bits
    % in an int on this machine, as an int and as a uint respectively.
    %
:- func bits_per_int = int.
:- pred bits_per_int(int::out) is det.
:- func ubits_per_int = uint.
:- pred ubits_per_int(uint::out) is det.

%---------------------------------------------------------------------------%

    % fold_up(F, Low, High, Acc) <=> list.foldl(F, Low .. High, Acc)
    %
    % NOTE: fold_up/4 is undefined if High = max_int.
    %
:- func fold_up(func(int, T) = T, int, int, T) = T.

    % fold_up(F, Low, High, !Acc) <=> list.foldl(F, Low .. High, !Acc)
    %
    % NOTE: fold_up/5 is undefined if High = max_int.
    %
:- pred fold_up(pred(int, T, T), int, int, T, T).
:- mode fold_up(pred(in, in, out) is det, in, in, in, out) is det.
:- mode fold_up(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode fold_up(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode fold_up(pred(in, array_di, array_uo) is det, in, in,
    array_di, array_uo) is det.
:- mode fold_up(pred(in, in, out) is semidet, in, in, in, out)
    is semidet.
:- mode fold_up(pred(in, mdi, muo) is semidet, in, in, mdi, muo)
    is semidet.
:- mode fold_up(pred(in, di, uo) is semidet, in, in, di, uo)
    is semidet.
:- mode fold_up(pred(in, in, out) is nondet, in, in, in, out)
    is nondet.
:- mode fold_up(pred(in, mdi, muo) is nondet, in, in, mdi, muo)
    is nondet.
:- mode fold_up(pred(in, di, uo) is cc_multi, in, in, di, uo)
    is cc_multi.
:- mode fold_up(pred(in, in, out) is cc_multi, in, in, in, out)
    is cc_multi.

    % fold_up2(F, Low, High, !Acc1, Acc2) <=>
    %   list.foldl2(F, Low .. High, !Acc1, !Acc2)
    %
    % NOTE: fold_up2/7 is undefined if High = max_int.
    %
:- pred fold_up2(pred(int, T, T, U, U), int, int, T, T, U, U).
:- mode fold_up2(pred(in, in, out, in, out) is det, in, in, in, out,
    in, out) is det.
:- mode fold_up2(pred(in, in, out, mdi, muo) is det, in, in, in, out,
    mdi, muo) is det.
:- mode fold_up2(pred(in, in, out, di, uo) is det, in, in, in, out,
    di, uo) is det.
:- mode fold_up2(pred(in, di, uo, di, uo) is det, in, in, di, uo,
    di, uo) is det.
:- mode fold_up2(pred(in, in, out, array_di, array_uo) is det, in, in,
    in, out, array_di, array_uo) is det.
:- mode fold_up2(pred(in, in, out, in, out) is semidet, in, in,
    in, out, in, out) is semidet.
:- mode fold_up2(pred(in, in, out, mdi, muo) is semidet, in, in,
    in, out, mdi, muo) is semidet.
:- mode fold_up2(pred(in, in, out, di, uo) is semidet, in, in,
    in, out, di, uo) is semidet.
:- mode fold_up2(pred(in, in, out, in, out) is nondet, in, in,
    in, out, in, out) is nondet.
:- mode fold_up2(pred(in, in, out, mdi, muo) is nondet, in, in,
    in, out, mdi, muo) is nondet.

    % fold_up3(F, Low, High, !Acc1, Acc2, !Acc3) <=>
    %   list.foldl3(F, Low .. High, !Acc1, !Acc2, !Acc3)
    %
    % NOTE: fold_up3/9 is undefined if High = max_int.
    %
:- pred fold_up3(pred(int, T, T, U, U, V, V), int, int, T, T, U, U, V, V).
:- mode fold_up3(pred(in, in, out, in, out, in, out) is det,
    in, in, in, out, in, out, in, out) is det.
:- mode fold_up3(pred(in, in, out, in, out, mdi, muo) is det,
    in, in, in, out, in, out, mdi, muo) is det.
:- mode fold_up3(pred(in, in, out, in, out, di, uo) is det,
    in, in, in, out, in, out, di, uo) is det.
:- mode fold_up3(pred(in, in, out, di, uo, di, uo) is det,
    in, in, in, out, di, uo, di, uo) is det.
:- mode fold_up3(pred(in, in, out, in, out, array_di, array_uo) is det,
    in, in, in, out, in, out, array_di, array_uo) is det.
:- mode fold_up3(pred(in, in, out, in, out, in, out) is semidet,
    in, in, in, out, in, out, in, out) is semidet.
:- mode fold_up3(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, in, in, out, in, out, mdi, muo) is semidet.
:- mode fold_up3(pred(in, in, out, in, out, di, uo) is semidet,
    in, in, in, out, in, out, di, uo) is semidet.
:- mode fold_up3(pred(in, in, out, in, out, in, out) is nondet,
    in, in, in, out, in, out, in, out) is nondet.
:- mode fold_up3(pred(in, in, out, in, out, mdi, muo) is nondet,
    in, in, in, out, in, out, mdi, muo) is nondet.

    % fold_down(F, Low, High, Acc) <=> list.foldr(F, Low .. High, Acc)
    %
    % NOTE: fold_down/4 is undefined if Low = min_int.
    %
:- func fold_down(func(int, T) = T, int, int, T) = T.

    % fold_down(F, Low, High, !Acc) <=> list.foldr(F, Low .. High, !Acc)
    %
    % NOTE: fold_down/5 is undefined if Low min_int.
    %
:- pred fold_down(pred(int, T, T), int, int, T, T).
:- mode fold_down(pred(in, in, out) is det, in, in, in, out) is det.
:- mode fold_down(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode fold_down(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode fold_down(pred(in, array_di, array_uo) is det, in, in,
    array_di, array_uo) is det.
:- mode fold_down(pred(in, in, out) is semidet, in, in, in, out)
    is semidet.
:- mode fold_down(pred(in, mdi, muo) is semidet, in, in, mdi, muo)
    is semidet.
:- mode fold_down(pred(in, di, uo) is semidet, in, in, di, uo)
    is semidet.
:- mode fold_down(pred(in, in, out) is nondet, in, in, in, out)
    is nondet.
:- mode fold_down(pred(in, mdi, muo) is nondet, in, in, mdi, muo)
    is nondet.
:- mode fold_down(pred(in, in, out) is cc_multi, in, in, in, out)
    is cc_multi.
:- mode fold_down(pred(in, di, uo) is cc_multi, in, in, di, uo)
    is cc_multi.

    % fold_down2(F, Low, High, !Acc1, !Acc2) <=>
    %   list.foldr2(F, Low .. High, !Acc1, Acc2).
    %
    % NOTE: fold_down2/7 is undefined if Low = min_int.
    %
:- pred fold_down2(pred(int, T, T, U, U), int, int, T, T, U, U).
:- mode fold_down2(pred(in, in, out, in, out) is det, in, in, in, out,
    in, out) is det.
:- mode fold_down2(pred(in, in, out, mdi, muo) is det, in, in, in, out,
    mdi, muo) is det.
:- mode fold_down2(pred(in, in, out, di, uo) is det, in, in, in, out,
    di, uo) is det.
:- mode fold_down2(pred(in, di, uo, di, uo) is det, in, in, di, uo,
    di, uo) is det.
:- mode fold_down2(pred(in, in, out, array_di, array_uo) is det, in, in,
    in, out, array_di, array_uo) is det.
:- mode fold_down2(pred(in, in, out, in, out) is semidet, in, in,
    in, out, in, out) is semidet.
:- mode fold_down2(pred(in, in, out, di, uo) is semidet, in, in,
    in, out, di, uo) is semidet.
:- mode fold_down2(pred(in, in, out, in, out) is nondet, in, in,
    in, out, in, out) is nondet.
:- mode fold_down2(pred(in, in, out, mdi, muo) is nondet, in, in,
    in, out, mdi, muo) is nondet.

    % fold_up3(F, Low, High, !Acc1, Acc2, !Acc3) <=>
    %   list.foldr3(F, Low .. High, !Acc1, !Acc2, !Acc3)
    %
    % NOTE: fold_down3/9 is undefined if Low = min_int.
    %
:- pred fold_down3(pred(int, T, T, U, U, V, V), int, int, T, T, U, U, V, V).
:- mode fold_down3(pred(in, in, out, in, out, in, out) is det,
    in, in, in, out, in, out, in, out) is det.
:- mode fold_down3(pred(in, in, out, in, out, mdi, muo) is det,
    in, in, in, out, in, out, mdi, muo) is det.
:- mode fold_down3(pred(in, in, out, in, out, di, uo) is det,
    in, in, in, out, in, out, di, uo) is det.
:- mode fold_down3(pred(in, in, out, di, uo, di, uo) is det,
    in, in, in, out, di, uo, di, uo) is det.
:- mode fold_down3(pred(in, in, out, in, out, array_di, array_uo) is det,
    in, in, in, out, in, out, array_di, array_uo) is det.
:- mode fold_down3(pred(in, in, out, in, out, in, out) is semidet,
    in, in, in, out, in, out, in, out) is semidet.
:- mode fold_down3(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, in, in, out, in, out, mdi, muo) is semidet.
:- mode fold_down3(pred(in, in, out, in, out, di, uo) is semidet,
    in, in, in, out, in, out, di, uo) is semidet.
:- mode fold_down3(pred(in, in, out, in, out, in, out) is nondet,
    in, in, in, out, in, out, in, out) is nondet.
:- mode fold_down3(pred(in, in, out, in, out, mdi, muo) is nondet,
    in, in, in, out, in, out, mdi, muo) is nondet.

%---------------------------------------------------------------------------%

    % nondet_int_in_range(Low, High, I):
    %
    % On successive successes, set I to every integer from Low to High.
    %
:- pred nondet_int_in_range(int::in, int::in, int::out) is nondet.

    % all_true_in_range(P, Low, High):
    % True iff P is true for every integer in Low to High.
    %
    % NOTE: all_true_in_range/3 is undefined if High = max_int.
    %
:- pred all_true_in_range(pred(int)::in(pred(in) is semidet),
    int::in, int::in) is semidet.

%---------------------------------------------------------------------------%

    % Convert an int to a pretty_printer.doc for formatting.
    %
:- func int_to_doc(int) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%
% Computing hashes of ints.
%

    % Compute a hash value for an int.
    %
:- func hash(int) = int.
:- pred hash(int::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.
:- interface.

    % Everything below here will not appear in the
    % Mercury Library Reference Manual.

%---------------------------------------------------------------------------%

% XXX the module qualification in these promises is necessary because
% otherwise the compiler gets confused by the import of the uint module
% in the implementation section of this one.

    % commutativity and associativity of +
:- promise all [A, B, C] ( C = int.(B + A) <=> C = int.(A + B) ).
:- promise all [A, B, C, ABC] ( ABC = int.(A + B) + C <=>
    ABC = A + int.(B + C) ).

    % commutativity and associativity of *
:- promise all [A, B, C] ( C = int.(B * A) <=> C = A * B ).
:- promise all [A, B, C, ABC] ( ABC = int.(A * B) * C <=> ABC = A * (B * C) ).

%---------------------------------------------------------------------------%

    % floor_to_multiple_of_bits_per_int(Int):
    %
    % Returns the largest multiple of bits_per_int which is less than or
    % equal to Int.
    %
    % Used by sparse_bitset.m. Makes it clearer to gcc that parts of this
    % operation can be optimized into shifts, without turning up the
    % optimization level.
    %
:- func floor_to_multiple_of_bits_per_int(int) = int.

    % Used by floor_to_multiple_of_bits_per_int, placed here to make sure
    % they go in the `.opt' file.

    % quot_bits_per_int(X) = X // bits_per_int.
    %
:- func quot_bits_per_int(int) = int.

    % times_bits_per_int(X) = X * bits_per_int.
    %
:- func times_bits_per_int(int) = int.

    % Used by bitmap.m.  Like the ones above, the purpose of defining this in C
    % is to make it clearer to gcc that this can be optimized.

    % rem_bits_per_int(X) = X `rem` bits_per_int.
    %
:- func rem_bits_per_int(int) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

:- instance enum(int) where [
    to_int(X) = X,
    from_int(X) = X
].

:- instance uenum(int) where [
    % This maps non-negative numbers like this:
    %
    %   0->0u, 1->2u, 2->4u, ... (2^31)-1 -> (2^32)-2u
    %
    % It maps negative numbers like this:
    %
    %   -1->1u, -2->3u, -3->5u, ... (2^31) -> (2^32)-1u
    %
    to_uint(I) = U :-
        ( if I >= 0 then
            U = cast_from_int(I `unchecked_left_shift` 1)
        else
            U = cast_from_int((-I) `unchecked_left_shift` 1) - 1u
        ),
    from_uint(U, I) :-
        ( if even(U) then
            I = cast_to_int(U `unchecked_right_shift` 1)
        else
            I = -cast_to_int(U `unchecked_right_shift` 1) - 1
        )
].

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <limits.h>

    #define ML_BITS_PER_INT     (sizeof(MR_Integer) * CHAR_BIT)
").

%---------------------------------------------------------------------------%

    % This code is included here rather than just calling the version
    % in math.m because we currently don't do transitive inter-module
    % inlining, so code which uses `//'/2 but doesn't import math.m
    % couldn't have the domain check optimized away.
:- pred int_domain_checks is semidet.
:- pragma inline(pred(int_domain_checks/0)).

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

abs(Num) = Abs :-
    abs(Num, Abs).

abs(Num, Abs) :-
    ( if Num = int.min_int then
        throw(software_error("int.abs: abs(min_int) would overflow"))
    else
        Abs = unchecked_abs(Num)
    ).

unchecked_abs(Num) =
    ( if Num < 0 then
        0 - Num
    else
        Num
    ).

nabs(Num) =
    ( if Num > 0 then
        -Num
    else
        Num
    ).

%---------------------------------------------------------------------------%

max(X, Y) = Max :-
    max(X, Y, Max).

max(X, Y, Max) :-
    ( if X > Y then
        Max = X
    else
        Max = Y
    ).

min(X, Y) = Min :-
    min(X, Y, Min).

min(X, Y, Min) :-
    ( if X < Y then
        Min = X
    else
        Min = Y
    ).

%---------------------------------------------------------------------------%

% Most of the arithmetic and comparison operators are recognized by
% the compiler as builtins, so we don't need to define them here.

X div Y = Div :-
    Trunc = X // Y,
    ( if
        ( X >= 0, Y >= 0
        ; X < 0, Y < 0
        ; X rem Y = 0
        )
    then
        Div = Trunc
    else
        Div = Trunc - 1
    ).

:- pragma inline(func('//'/2)).
X // Y = Div :-
    ( if
        int_domain_checks,
        Y = 0
    then
        throw(domain_error("int.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline(func('/'/2)).
X / Y = X // Y.

X mod Y = X - (X div Y) * Y.

:- pragma inline(func(rem/2)).
X rem Y = Rem :-
    ( if
        int_domain_checks,
        Y = 0
    then
        throw(domain_error("int.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

:- pragma inline(pred(even/1)).
even(X) :-
    (X /\ 1) = 0.

:- pragma inline(pred(odd/1)).
odd(X) :-
    (X /\ 1) \= 0.

pow(Base, Exp) = Result :-
    pow(Base, Exp, Result).

pow(Base, Exp, Result) :-
    ( if int_domain_checks, Exp < 0 then
        throw(domain_error("int.pow: zero base"))
    else
        Result = multiply_by_pow(1, Base, Exp)
    ).

    % Returns Scale0 * (Base ** Exp).
    % Requires that Exp >= 0.
    %
:- func multiply_by_pow(int, int, int) = int.

multiply_by_pow(Scale0, Base, Exp) = Result :-
    ( if Exp = 0 then
        Result = Scale0
    else
        ( if odd(Exp) then
            Scale1 = Scale0 * Base
        else
            Scale1 = Scale0
        ),
        Result = multiply_by_pow(Scale1, Base * Base, Exp div 2)
    ).

log2(X) = CeilLogX :-
    log2(X, CeilLogX).

log2(X, CeilLogX) :-
    ( if int_domain_checks, X =< 0 then
        throw(domain_error("int.log2: taking logarithm of zero"))
    else
        log2_loop(X, 0, CeilLogX)
    ).

:- pred log2_loop(int::in, int::in, int::out) is det.

log2_loop(CurX, CurLogXSoFar, CeilLogX) :-
    ( if CurX = 1 then
        CeilLogX = CurLogXSoFar
    else
        NextX = (CurX + 1) // 2,
        NextLogXSoFar = CurLogXSoFar + 1,
        log2_loop(NextX, NextLogXSoFar, CeilLogX)
    ).

%---------------------------------------------------------------------------%

% The unchecked shift operations are builtins.

X << Y = Z :-
    ( if Y `private_builtin.unsigned_lt` bits_per_int then
        Z = unchecked_left_shift(X, Y)
    else
        Msg = "int.(<<): second operand is out of range",
        throw(domain_error(Msg))
    ).

X <<u Y = Z :-
    ( if Y < ubits_per_int then
        Z = unchecked_left_ushift(X, Y)
    else
        Msg = "int.(<<u): second operand is out of range",
        throw(domain_error(Msg))
    ).

X >> Y = Z :-
    ( if Y `private_builtin.unsigned_lt` bits_per_int then
        Z = unchecked_right_shift(X, Y)
    else
        Msg = "int.(>>): second operand is out of range",
        throw(domain_error(Msg))
    ).

X >>u Y = Z :-
    ( if Y < ubits_per_int then
        Z = unchecked_right_ushift(X, Y)
    else
        Msg = "int.(>>u): second operand is out of range",
        throw(domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_int(Max::out),
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

:- pragma foreign_proc("C#",
    max_int(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = System.Int32.MaxValue;
").

:- pragma foreign_proc("Java",
    max_int(Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Max = java.lang.Integer.MAX_VALUE;
").

max_int = X :-
    max_int(X).

%---------------------%

:- pragma foreign_proc("C",
    min_int(Min::out),
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

:- pragma foreign_proc("C#",
    min_int(Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Min = System.Int32.MinValue;
").

:- pragma foreign_proc("Java",
    min_int(Min::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Min = java.lang.Integer.MIN_VALUE;
").

min_int = X :-
    min_int(X).

%---------------------%

bits_per_int = X :-
    bits_per_int(X).

:- pragma foreign_proc("C",
    bits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Bits = ML_BITS_PER_INT;
").
:- pragma foreign_proc("C#",
    bits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // we are using int32 in the compiler.
    // XXX would be better to avoid hard-coding this here.
    Bits = 32;
").
:- pragma foreign_proc("Java",
    bits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Java ints are 32 bits.
    Bits = 32;
").

ubits_per_int = X :-
    ubits_per_int(X).

:- pragma foreign_proc("C",
    ubits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Bits = (MR_Unsigned) ML_BITS_PER_INT;
").
:- pragma foreign_proc("C#",
    ubits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // we are using int32 in the compiler.
    // XXX would be better to avoid hard-coding this here.
    Bits = 32;
").
:- pragma foreign_proc("Java",
    ubits_per_int(Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Java ints are 32 bits.
    Bits = 32;
").

%---------------------------------------------------------------------------%

fold_up(F, Lo, Hi, A) =
    ( if Lo =< Hi then
        fold_up(F, Lo + 1, Hi, F(Lo, A))
    else
        A
    ).

fold_up(P, Lo, Hi, !A) :-
    ( if Lo =< Hi then
        P(Lo, !A),
        fold_up(P, Lo + 1, Hi, !A)
    else
        true
    ).

fold_up2(P, Lo, Hi, !A, !B) :-
    ( if Lo =< Hi then
        P(Lo, !A, !B),
        fold_up2(P, Lo + 1, Hi, !A, !B)
    else
        true
    ).

fold_up3(P, Lo, Hi, !A, !B, !C) :-
    ( if Lo =< Hi then
        P(Lo, !A, !B, !C),
        fold_up3(P, Lo + 1, Hi, !A, !B, !C)
    else
        true
    ).

%---------------------------------------------------------------------------%

fold_down(F, Lo, Hi, A) =
    ( if Lo =< Hi then
        fold_down(F, Lo, Hi - 1, F(Hi, A))
    else
        A
    ).

fold_down(P, Lo, Hi, !A) :-
    ( if Lo =< Hi then
        P(Hi, !A),
        fold_down(P, Lo, Hi - 1, !A)
    else
        true
    ).

fold_down2(P, Lo, Hi, !A, !B) :-
    ( if Lo =< Hi then
        P(Hi, !A, !B),
        fold_down2(P, Lo, Hi - 1, !A, !B)
    else
        true
    ).

fold_down3(P, Lo, Hi, !A, !B, !C) :-
    ( if Lo =< Hi then
        P(Hi, !A, !B, !C),
        fold_down3(P, Lo, Hi - 1, !A, !B, !C)
    else
        true
    ).

%---------------------------------------------------------------------------%

nondet_int_in_range(Lo, Hi, I) :-
    % Leave a choice point only if there is at least one solution
    % to find on backtracking.
    ( if Lo < Hi then
        (
            I = Lo
        ;
            nondet_int_in_range(Lo + 1, Hi, I)
        )
    else
        Lo = Hi,
        I = Lo
    ).

%---------------------------------------------------------------------------%

all_true_in_range(P, Lo, Hi) :-
    ( if Lo =< Hi then
        P(Lo),
        all_true_in_range(P, Lo + 1, Hi)
    else
        true
    ).

%---------------------------------------------------------------------------%

int_to_doc(X) = str(string.int_to_string(X)).

%---------------------------------------------------------------------------%

hash(Int) = Hash :-
    UInt = uint.cast_from_int(Int),
    Hash = uint.hash(UInt).

hash(Int, Hash) :-
    Hash = int.hash(Int).

%---------------------------------------------------------------------------%

:- pragma inline(func(floor_to_multiple_of_bits_per_int/1)).

floor_to_multiple_of_bits_per_int(X) = Floor :-
    Trunc = quot_bits_per_int(X),
    Floor0 = times_bits_per_int(Trunc),
    ( if Floor0 > X then
        Floor = Floor0 - bits_per_int
    else
        Floor = Floor0
    ).

:- pragma foreign_proc("C",
    quot_bits_per_int(Int::in) = (Div::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Div = Int / ML_BITS_PER_INT;
").

quot_bits_per_int(Int) = Result :-
    Result = Int // bits_per_int.

:- pragma foreign_proc("C",
    times_bits_per_int(Int::in) = (Result::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Result = Int * ML_BITS_PER_INT;
").

times_bits_per_int(Int) = Result :-
    Result = Int * bits_per_int.

:- pragma foreign_proc("C",
    rem_bits_per_int(Int::in) = (Rem::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Rem = Int % ML_BITS_PER_INT;
").

rem_bits_per_int(Int) = Result :-
    Result = Int rem bits_per_int.

%---------------------------------------------------------------------------%
:- end_module int.
%---------------------------------------------------------------------------%
