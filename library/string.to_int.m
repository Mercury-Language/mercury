%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2025-2026 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: string.to_int.m.
% Main author: juliensf.
%
% This module includes the non-interface parts of operations
% that convert from strings to integers.
%
%---------------------------------------------------------------------------%

:- module string.to_int.
:- interface.

%---------------------------------------------------------------------------%

    % do_base_string_to_int(Base, String, Int):
    %
:- pred do_base_string_to_int(int::in, string::in, int::out) is semidet.

    % do_base_string_to_uint(Base, String, UInt):
    %
:- pred do_base_string_to_uint(int::in, string::in, uint::out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module uint.

%---------------------------------------------------------------------------%

:- type int_sign
    --->    positive
    ;       negative.

%---------------------------------------------------------------------------%

do_base_string_to_int(Base, String, Int) :-
    string.index(String, 0, Char),
    End = string.count_code_units(String),
    ( if
        ( Char = ('-'), Sign0 = negative
        ; Char = ('+'), Sign0 = positive
        )
    then
        Sign = Sign0,
        % Start at the first digit, which *should* be just after the sign.
        End > 1,
        Start = 1
    else
        Sign = positive,
        Start = 0
    ),

    % Do not include the sign bit, if any, in our initial digit count.
    NumStringDigits = End - Start,

    % The divisions below are all safe since our callers set Base
    % to be in 2..36.
    (
        Sign = positive,
        ( if can_use_to_int_fast_path(Base, NumStringDigits) then
            do_unsafe_base_string_to_positive_int_loop(Base, String,
                Start, End, 0, Int)
        else
            CutOff = max_int `unchecked_quotient` Base,
            CutLimit = max_int `unchecked_rem` Base,
            do_base_string_to_positive_int_loop(Base, CutOff, CutLimit, String,
                Start, End, 0, Int)
        )
    ;
        Sign = negative,
        ( if can_use_to_int_fast_path(Base, NumStringDigits) then
            do_unsafe_base_string_to_negative_int_loop(Base, String,
                Start, End, 0, Int)
        else
            CutOff = min_int `unchecked_quotient` Base,
            CutLimit = -(min_int `unchecked_rem` Base),
            do_base_string_to_negative_int_loop(Base, CutOff, CutLimit, String,
                Start, End, 0, Int)
        )
    ).

%---------------------%

    % do_base_string_to_positive_int_loop(Base, CutOff, CutLimit, String,
    %     CurOffset, EndOffset, !Int):
    %
    % Convert the base Base digits of String between CurOffset and EndOffset
    % into an int, reading a digit M from the string in each iteration and
    % accumulating the result in !Int. Fail if the value being accumulated
    % would exceed max_int.
    %
    % We must detect the overflow *before* it happens. Computing
    % (Base * !.Int) + M and then testing the result does not work,
    % because the multiplication may overflow by more than the range of
    % an int. In that case, the wrapped-around result is again greater
    % than !.Int, and so is indistinguishable from a result that did not
    % overflow.
    %
    % Requiring that !.Int =< (max_int - M) // Base be true at each iteration
    % of the loop does detect overflow, but at the cost of having a division in
    % each iteration.
    %
    % Instead, we can hoist the division out of the loop body by observing that
    % the above check depends on the digit M only through a comparison that
    % can be split into cases. Specifically, we can write max_int as:
    %
    %   max_int = (Base * CutOff) + CutLimit
    %
    % CutOff and CutLimit are invariant and our caller will compute them as:
    %
    %   CutOff   = max_int // Base
    %   CutLimit = max_int rem Base  (implying 0 =< CutLimit < Base)
    %
    % Given these, (Base * !.Int) + M does not exceed max_int if and only
    % if either:
    %
    % - !.Int < CutOff, in which case, since M < Base,
    %
    %      (Base * !.Int) + M =< (Base * (CutOff - 1)) + (Base - 1)
    %                           = (Base * CutOff) - 1
    %                           =< max_int
    %
    %   whatever the digit M is; or
    %
    % - !.Int = CutOff and M =< CutLimit, in which case
    %
    %      (Base * !.Int) + M =< (Base * CutOff) + CutLimit = max_int.
    %
    % If !.Int > CutOff, then
    %
    %      (Base * !.Int) >= (Base * CutOff) + Base,
    %
    % which exceeds max_int whatever the digit is.
    %
:- pred do_base_string_to_positive_int_loop(int::in, int::in, int::in,
    string::in, int::in, int::in, int::in, int::out) is semidet.

do_base_string_to_positive_int_loop(Base, CutOff, CutLimit, String,
        CurOffset, EndOffset, !Int) :-
    ( if CurOffset < EndOffset then
        unsafe_index_next(String, CurOffset, NextOffset, Char),
        char.unsafe_base_digit_to_int(Base, Char, M),
        % Fail if (Base * !.Int) + M would exceed max_int.
        ( !.Int < CutOff
        ; !.Int = CutOff, M =< CutLimit
        ),
        !:Int = (Base * !.Int) + M,
        do_base_string_to_positive_int_loop(Base, CutOff, CutLimit, String,
            NextOffset, EndOffset, !Int)
    else
        true
    ).

    % do_base_string_to_negative_int_loop(Base, CutOff, CutLimit, String,
    %     CurOffset, EndOffset, !Int):
    %
    % This predicate is similar to do_base_string_to_positive_int_loop above,
    % but accumulates a negative value, and fail if it would be less than
    % min_int. Here our caller gives us
    %
    %   CutOff   = min_int // Base
    %   CutLimit = -(min_int rem Base)
    %
    % so that min_int = (Base * CutOff) - CutLimit, where CutLimit is
    % again in 0 .. (Base - 1). The test mirrors the positive case: the
    % step is safe if !.Int > CutOff, whatever the digit M is, or if
    % !.Int = CutOff and M =< CutLimit.
    %
    % Note that we must be the truncating quotient and remainder.
    % For a negative dividend, truncation rounds towards zero, which makes
    % CutOff the ceiling of the exact quotient min_int / Base; that is the
    % tight bound on !.Int. Flooring division yields a CutOff one lower
    % whenever Base does not divide min_int exactly, and the test
    % !.Int > CutOff would then accept an accumulator value whose next step
    % overflows. With 32-bit ints and Base = 10, for example, it would
    % convert "-2147483649" to 2147483647 instead of failing.
    %
    % Note also that we cannot avoid the issue by accumulating a
    % positive value and negating it at the end, since the magnitude of
    % min_int is not representable as a positive int.
    %
:- pred do_base_string_to_negative_int_loop(int::in, int::in, int::in,
    string::in, int::in, int::in, int::in, int::out) is semidet.

do_base_string_to_negative_int_loop(Base, CutOff, CutLimit, String,
        CurOffset, EndOffset, !Int) :-
    ( if CurOffset < EndOffset then
        unsafe_index_next(String, CurOffset, NextOffset, Char),
        char.unsafe_base_digit_to_int(Base, Char, M),
        % Fail if (Base * !.Int) - M would be less than min_int.
        ( !.Int > CutOff
        ; !.Int = CutOff, M =< CutLimit
        ),
        !:Int = (Base * !.Int) - M,
        do_base_string_to_negative_int_loop(Base, CutOff, CutLimit, String,
            NextOffset, EndOffset, !Int)
    else
        true
    ).

%---------------------%

    % A version of do_base_string_to_positive_int_loop that omits the
    % overflow check. This is faster, but can only be used safely when the
    % number of digits in the string is below that which potentially overflow.
    %
:- pred do_unsafe_base_string_to_positive_int_loop(int::in, string::in,
    int::in, int::in, int::in, int::out) is semidet.

do_unsafe_base_string_to_positive_int_loop(Base, String, CurOffset,
        EndOffset, !Int) :-
    ( if CurOffset < EndOffset then
        unsafe_index_next(String, CurOffset, NextOffset, Char),
        char.unsafe_base_digit_to_int(Base, Char, M),
        !:Int = (Base * !.Int) + M,
        do_unsafe_base_string_to_positive_int_loop(Base, String, NextOffset,
            EndOffset, !Int)
    else
        true
    ).

    % As above, but for the negative case.
    %
:- pred do_unsafe_base_string_to_negative_int_loop(int::in, string::in,
    int::in, int::in, int::in, int::out) is semidet.

do_unsafe_base_string_to_negative_int_loop(Base, String, CurOffset,
        EndOffset, !Int) :-
    ( if CurOffset < EndOffset then
        unsafe_index_next(String, CurOffset, NextOffset, Char),
        char.unsafe_base_digit_to_int(Base, Char, M),
        !:Int = (Base * !.Int) - M,
        do_unsafe_base_string_to_negative_int_loop(Base, String,
            NextOffset, EndOffset, !Int)
    else
        true
    ).

%---------------------------------------------------------------------------%

do_base_string_to_uint(Base, String, UInt) :-
    End = string.count_code_units(String),
    End > 0, % Fail if we have the empty string.
    UBase = uint.cast_from_int(Base),
    % The divisions below are safe since our callers set Base
    % to be in 2..36.
    ( if can_use_to_uint_fast_path(Base, End) then
        do_unsafe_base_string_to_uint_loop(UBase, Base, String, 0, End,
            0u, UInt)
    else
        CutOff = max_uint `unchecked_quotient` UBase,
        CutLimit = max_uint `unchecked_rem` UBase,
        do_base_string_to_uint_loop(UBase, Base, CutOff, CutLimit, String,
            0, End, 0u, UInt)
    ).

%---------------------%

    % do_base_string_to_uint_loop(UBase, Base, CutOff, CutLimit, String,
    %    CurOffset, EndOffset, !UInt):
    %
    % This predicate is similar to do_base_string_to_positive_int_loop above.
    %
:- pred do_base_string_to_uint_loop(uint::in, int::in, uint::in, uint::in,
    string::in, int::in, int::in, uint::in, uint::out) is semidet.

do_base_string_to_uint_loop(UBase, Base, CutOff, CutLimit, String,
        CurOffset, EndOffset, !UInt) :-
    ( if CurOffset < EndOffset then
        unsafe_index_next(String, CurOffset, NextOffset, Char),
        char.unsafe_base_digit_to_int(Base, Char, M),
        MU = uint.cast_from_int(M),
        % Fail if (UBase * !.UInt) + MU would exceed max_uint.
        ( !.UInt < CutOff
        ; !.UInt = CutOff, MU =< CutLimit
        ),
        !:UInt = (UBase * !.UInt) + MU,
        do_base_string_to_uint_loop(UBase, Base, CutOff, CutLimit, String,
            NextOffset, EndOffset, !UInt)
    else
        true
    ).

    % do_unsafe_base_string_to_uint_loop(UBase, Base, String, CurOffset,
    %   EndOffset, !UInt):
    %
    % A version of do_base_string_to_uint_loop that omits the overflow
    % check. This is faster, but can only be used safely when the number of
    % digits in the string is below that which potentially overflow.
    %
:- pred do_unsafe_base_string_to_uint_loop(uint::in, int::in, string::in,
    int::in, int::in, uint::in, uint::out) is semidet.

do_unsafe_base_string_to_uint_loop(UBase, Base, String, CurOffset, EndOffset,
        !UInt) :-
    ( if CurOffset < EndOffset then
        unsafe_index_next(String, CurOffset, NextOffset, Char),
        char.unsafe_base_digit_to_int(Base, Char, M),
        MU = uint.cast_from_int(M),
        !:UInt = (UBase * !.UInt) + MU,
        do_unsafe_base_string_to_uint_loop(UBase, Base, String,
            NextOffset, EndOffset, !UInt)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % can_use_to_int_fast_path(Base, NumDigits):
    %
    % Can we safely use the fast path string-to-int conversion for a string of
    % base Base digits of length NumDigits?
    %
:- pred can_use_to_int_fast_path(int::in, int::in) is semidet.

can_use_to_int_fast_path(Base, NumDigits) :-
    WordSize = bits_per_int,
    (
        WordSize = 32,
        safe_num_digits_for_base_i32_u32_i64_u64(Base, SafeDigits, _, _, _)
    ;
        WordSize = 64,
        safe_num_digits_for_base_i32_u32_i64_u64(Base, _, _, SafeDigits, _)
    ),
    NumDigits =< SafeDigits.

%---------------------%

    % can_use_to_uint_fast_path(Base, NumDigits):
    %
    % Can we safely use the fast path string-to-uint conversion for a string of
    % base Base digits of length NumDigits?
    %
:- pred can_use_to_uint_fast_path(int::in, int::in) is semidet.

can_use_to_uint_fast_path(Base, NumDigits) :-
    WordSize = bits_per_uint,
    (
        WordSize = 32,
        safe_num_digits_for_base_i32_u32_i64_u64(Base, _, SafeDigits, _, _)
    ;
        WordSize = 64,
        safe_num_digits_for_base_i32_u32_i64_u64(Base, _, _, _, SafeDigits)
    ),
    NumDigits =< SafeDigits.

%---------------------%

    % safe_num_digits_for_base_i32_u32_i64_u64(Base,
    %   SafeDigitsI32, SafeDigitsU32, SafeDigitsI64, SafeDigitsU64):
    %
    % For a given base Base, SafeDigitsI32 is the largest number of digits
    % such that every string of SafeDigits base Base digits denotes a value
    % that fits in a 32-bit signed int. That is, SafeDigits is the
    % largest number such that:
    %
    %   Base ^ SafeDigits - 1 =< max_int, where max_int = 2 ^ (32 - 1) - 1.
    %
    % We use the same value of SafeDigits for negative ints. That is safe,
    % since abs(min_int) = max_int + 1 (i.e., a bound that holds for max_int
    % also holds for min_int).
    %
    % For a given base Base, SafeDigitsU32 is the largest number of digits
    % such that every string of SafeDigits base Base digits denotes a value
    % that fits in a 32-bit unsigned int. That is, SafeDigits is the
    % largest number such that:
    %
    %   Base ^ SafeDigits - 1 =< max_uint, where max_uint = 2 ^ 32 - 1.
    %
    % SafeDigitsI64 and SafeDigitsU64 are their 64-bit equivalents.
    %
:- pred safe_num_digits_for_base_i32_u32_i64_u64(int::in,
    int::out, int::out, int::out, int::out) is semidet.
:- pragma inline(pred(safe_num_digits_for_base_i32_u32_i64_u64/5)).

safe_num_digits_for_base_i32_u32_i64_u64( 2, 31, 32, 63, 64).
safe_num_digits_for_base_i32_u32_i64_u64( 3, 19, 20, 39, 40).
safe_num_digits_for_base_i32_u32_i64_u64( 4, 15, 16, 31, 32).
safe_num_digits_for_base_i32_u32_i64_u64( 5, 13, 13, 27, 27).
safe_num_digits_for_base_i32_u32_i64_u64( 6, 11, 12, 24, 24).
safe_num_digits_for_base_i32_u32_i64_u64( 7, 11, 11, 22, 22).
safe_num_digits_for_base_i32_u32_i64_u64( 8, 10, 10, 21, 21).
safe_num_digits_for_base_i32_u32_i64_u64( 9,  9, 10, 19, 20).
safe_num_digits_for_base_i32_u32_i64_u64(10,  9,  9, 18, 19).
safe_num_digits_for_base_i32_u32_i64_u64(11,  8,  9, 18, 18).
safe_num_digits_for_base_i32_u32_i64_u64(12,  8,  8, 17, 17).
safe_num_digits_for_base_i32_u32_i64_u64(13,  8,  8, 17, 17).
safe_num_digits_for_base_i32_u32_i64_u64(14,  8,  8, 16, 16).
safe_num_digits_for_base_i32_u32_i64_u64(15,  7,  8, 16, 16).
safe_num_digits_for_base_i32_u32_i64_u64(16,  7,  8, 15, 16).
safe_num_digits_for_base_i32_u32_i64_u64(17,  7,  7, 15, 15).
safe_num_digits_for_base_i32_u32_i64_u64(18,  7,  7, 15, 15).
safe_num_digits_for_base_i32_u32_i64_u64(19,  7,  7, 14, 15).
safe_num_digits_for_base_i32_u32_i64_u64(20,  7,  7, 14, 14).
safe_num_digits_for_base_i32_u32_i64_u64(21,  7,  7, 14, 14).
safe_num_digits_for_base_i32_u32_i64_u64(22,  6,  7, 14, 14).
safe_num_digits_for_base_i32_u32_i64_u64(23,  6,  7, 13, 14).
safe_num_digits_for_base_i32_u32_i64_u64(24,  6,  6, 13, 13).
safe_num_digits_for_base_i32_u32_i64_u64(25,  6,  6, 13, 13).
safe_num_digits_for_base_i32_u32_i64_u64(26,  6,  6, 13, 13).
safe_num_digits_for_base_i32_u32_i64_u64(27,  6,  6, 13, 13).
safe_num_digits_for_base_i32_u32_i64_u64(28,  6,  6, 13, 13).
safe_num_digits_for_base_i32_u32_i64_u64(29,  6,  6, 12, 13).
safe_num_digits_for_base_i32_u32_i64_u64(30,  6,  6, 12, 13).
safe_num_digits_for_base_i32_u32_i64_u64(31,  6,  6, 12, 12).
safe_num_digits_for_base_i32_u32_i64_u64(32,  6,  6, 12, 12).
safe_num_digits_for_base_i32_u32_i64_u64(33,  6,  6, 12, 12).
safe_num_digits_for_base_i32_u32_i64_u64(34,  6,  6, 12, 12).
safe_num_digits_for_base_i32_u32_i64_u64(35,  6,  6, 12, 12).
safe_num_digits_for_base_i32_u32_i64_u64(36,  5,  6, 12, 12).

%---------------------------------------------------------------------------%
:- end_module string.to_int.
%---------------------------------------------------------------------------%
