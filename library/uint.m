%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016-2021 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: uint.m
% Main author: juliensf
% Stability: low.
%
% Predicates and functions for dealing with unsigned machine sized integer
% numbers.
%
%---------------------------------------------------------------------------%

:- module uint.
:- interface.

:- import_module pretty_printer.

%---------------------------------------------------------------------------%

    % Convert an int to a uint.
    % Fails if the int is less than zero.
    %
:- pred from_int(int::in, uint::out) is semidet.

    % As above, but throw an exception instead of failing.
    %
:- func det_from_int(int) = uint.

:- func cast_from_int(int) = uint.

:- func cast_to_int(uint) = int.

    % Less than.
    %
:- pred (uint::in) < (uint::in) is semidet.

    % Greater than.
    %
:- pred (uint::in) > (uint::in) is semidet.

    % Less than or equal.
    %
:- pred (uint::in) =< (uint::in) is semidet.

    % Greater than or equal.
    %
:- pred (uint::in) >= (uint::in) is semidet.

    % Maximum.
    %
:- func max(uint, uint) = uint.

    % Minimum.
    %
:- func min(uint, uint) = uint.

    % Addition.
    %
:- func uint + uint = uint.
:- mode in   + in  = uo is det.
:- mode uo   + in  = in is det.
:- mode in   + uo  = in is det.

:- func plus(uint, uint) = uint.

    % Subtraction.
    %
:- func uint - uint = uint.
:- mode in   - in   = uo is det.
:- mode uo   - in   = in is det.
:- mode in   - uo   = in is det.

:- func minus(uint, uint) = uint.

    % Multiplication.
    %
:- func (uint::in) * (uint::in) = (uint::uo) is det.
:- func times(uint, uint) = uint.

    % Truncating integer division.
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (uint::in) div (uint::in) = (uint::uo) is det.

    % Truncating integer division.
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (uint::in) // (uint::in) = (uint::uo) is det.

    % (/)/2 is a synonym for (//)/2.
    %
:- func (uint::in) / (uint::in) = (uint::uo) is det.

    % unchecked_quotient(X, Y) is the same as X // Y, but the behaviour
    % is undefined if the right operand is zero.
    %
:- func unchecked_quotient(uint::in, uint::in) = (uint::uo) is det.

    % Modulus.
    % X mod Y = X - (X div Y) * Y
    %
    % Throws a `domain_error' exception if the right operand is zero.
    %
:- func (uint::in) mod (uint::in) = (uint::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `domain_error/` exception if the right operand is zero.
    %
:- func (uint::in) rem (uint::in) = (uint::uo) is det.

    % unchecked_rem(X, Y) is the same as X rem Y, but the behaviour is
    % undefined if the right operand is zero.
    %
:- func unchecked_rem(uint::in, uint::in) = (uint::uo) is det.

    % Left shift.
    % X << Y returns X "left shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, bits_per_uint).
    %
:- func (uint::in) << (int::in) = (uint::uo) is det.

    % unchecked_left_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, bits_per_uint).
    % It will typically be implemented more efficiently than X << Y.
    %
:- func unchecked_left_shift(uint::in, int::in) = (uint::uo) is det.

    % Right shift.
    % X >> Y returns X "right shifted" by Y bits.
    % The bit positions vacated by the shift are filled by zeros.
    % Throws an exception if Y is not in [0, bits_per_uint).
    %
:- func (uint::in) >> (int::in) = (uint::uo) is det.

    % unchecked_right_shift(X, Y) is the same as X >> Y except that the
    % behaviour is undefined if Y is not in [0, bits_per_uint).
    % It will typically be implemented more efficiently than X >> Y.
    %
:- func unchecked_right_shift(uint::in, int::in) = (uint::uo) is det.

    % even(X) is equivalent to (X mod 2 = 0).
    %
:- pred even(uint::in) is semidet.

    % odd(X) is equivalent to (not even(X)), i.e. (X mod 2 = 1).
    %
:- pred odd(uint::in) is semidet.

    % Bitwise and.
    %
:- func (uint::in) /\ (uint::in) = (uint::uo) is det.

    % Bitwise or.
    %
:- func (uint::in) \/ (uint::in) = (uint::uo) is det.

    % Bitwise exclusive or (xor).
    %
:- func xor(uint, uint) = uint.
:- mode xor(in, in) = uo is det.
:- mode xor(in, uo) = in is det.
:- mode xor(uo, in) = in is det.

    % Bitwise complement.
    %
:- func \ (uint::in) = (uint::uo) is det.

    % max_uint is the maximum value of a uint on this machine.
    %
:- func max_uint = uint.

    % bits_per_uint is the number of bits in a uint on this machine.
    %
:- func bits_per_uint = int.

    % Convert a uint to a pretty_printer.doc for formatting.
    %
:- func uint_to_doc(uint) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%
% Computing hashes of uints.
%

    % Compute a hash value for a uint.
    %
:- func hash(uint) = int.
:- pred hash(uint::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I < 0) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U = (MR_Unsigned) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (uint) I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

:- pragma foreign_proc("Java",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

det_from_int(I) = U :-
    ( if from_int(I, UPrime) then
        U = UPrime
    else
        error($pred, "cannot convert int to uint")
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_from_int(I::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    U = (MR_Unsigned) I;
").

:- pragma foreign_proc("C#",
    cast_from_int(I::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (uint) I;
").

:- pragma foreign_proc("Java",
    cast_from_int(I::in) = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = I;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cast_to_int(U::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I = (MR_Integer) U;
").

:- pragma foreign_proc("C#",
    cast_to_int(U::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) U;
").

:- pragma foreign_proc("Java",
    cast_to_int(U::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = U;
").

%---------------------------------------------------------------------------%

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

X div Y = X // Y.

:- pragma inline(func('//'/2)).
X // Y = Div :-
    ( if Y = 0u then
        throw(domain_error("uint.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline(func('/'/2)).
X / Y = X // Y.

X mod Y = X rem Y.

:- pragma inline(func(rem/2)).
X rem Y = Rem :-
    ( if Y = 0u then
        throw(domain_error("uint.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < cast_from_int(bits_per_uint) then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint.(<<): second operand is out of range",
        throw(domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < cast_from_int(bits_per_uint) then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint.(>>): second operand is out of range",
        throw(domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

:- pragma inline(pred(even/1)).
even(X) :-
    (X /\ 1u) = 0u.

:- pragma inline(pred(odd/1)).
odd(X) :-
    (X /\ 1u) \= 0u.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <limits.h>

    #define ML_BITS_PER_UINT     (sizeof(MR_Unsigned) * CHAR_BIT)
").

:- pragma foreign_proc("C",
    max_uint = (Max::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (sizeof(MR_Unsigned) == sizeof(unsigned int)) {
        Max = UINT_MAX;
    } else if (sizeof(MR_Unsigned) == sizeof(unsigned long)) {
        Max = (MR_Unsigned) ULONG_MAX;
    #if defined(ULLONG_MAX)
    } else if (sizeof(MR_Unsigned) == sizeof(unsigned long long)) {
        Max = (MR_Unsigned) ULLONG_MAX;
    #endif
    } else {
        MR_fatal_error(""Unable to figure out max uint size"");
    }
").

:- pragma foreign_proc("C#",
    max_uint = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = uint.MaxValue;
").

:- pragma foreign_proc("Java",
    max_uint = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 0xffffffff;
").

:- pragma foreign_proc("C",
    bits_per_uint = (Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    Bits = ML_BITS_PER_UINT;
").

:- pragma foreign_proc("Java",
    bits_per_uint = (Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Bits = 32;
").

:- pragma foreign_proc("C#",
    bits_per_uint = (Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Bits = 32;
").

%---------------------------------------------------------------------------%

uint_to_doc(X) = str(string.uint_to_string(X)).

%---------------------------------------------------------------------------%

% The integer hash functions below are originally from:
%
%  http://www.concentric.net/~Ttwang/tech/inthash.htm
%
% The above link is now dead; the last version can be found at:
%
%  https://web.archive.org/web/20121102023700/http://www.concentric.net/~Ttwang/tech/inthash.htm
%
% The algorithms from that page that we use are:
%
%   public int hash32shiftmult(int key)
%   public long hash64shift(long key)

hash(!.Key) = Hash :-
    C2 = 0x_27d4_eb2d_u, % A prime or odd constant.
    ( if bits_per_uint = 32 then
        !:Key = (!.Key `xor` 61_u) `xor` (!.Key >> 16),
        !:Key = !.Key + (!.Key << 3),
        !:Key = !.Key `xor` (!.Key >> 4),
        !:Key = !.Key * C2,
        !:Key = !.Key `xor` (!.Key >> 15)
    else
        !:Key = (\ !.Key) + (!.Key << 21), % !:Key = (!.Key << 21) - !.Key - 1
        !:Key = !.Key `xor` (!.Key >> 24),
        !:Key = (!.Key + (!.Key << 3)) + (!.Key << 8), % !.Key * 265
        !:Key = !.Key `xor` (!.Key >> 14),
        !:Key = (!.Key + (!.Key << 2)) + (!.Key << 4), % !.Key * 21
        !:Key = !.Key `xor` (!.Key >> 28),
        !:Key = !.Key + (!.Key << 31)
    ),
    Hash = uint.cast_to_int(!.Key).

hash(UInt, Hash) :-
    Hash = hash(UInt).

%---------------------------------------------------------------------------%
:- end_module uint.
%---------------------------------------------------------------------------%
