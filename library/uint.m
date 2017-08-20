%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

    % Maximum.
    %
:- func max(uint, uint) = uint.

    % Minimum.
    %
:- func min(uint, uint) = uint.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint::in) div (uint::in) = (uint::uo) is det.

    % Truncating integer division.
    %
    % Throws a `math.domain_error' exception if the right operand is zero.
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
    % Throws a `math.domain_error' exception if the right operand is zero.
    %
:- func (uint::in) mod (uint::in) = (uint::uo) is det.

    % Remainder.
    % X rem Y = X - (X // Y) * Y.
    %
    % Throws a `math.domain_error/` exception if the right operand is zero.
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

    % unchecked_lift_shift(X, Y) is the same as X << Y except that the
    % behaviour is undefined if Y is not in [0, bits_per_uint).
    % It will typically be be implemented more efficiently than X << Y.
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
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module math.
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
        error("uint.det_from_int: cannot convert int to uint")
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

cast_from_int(_) = _ :-
    sorry($module, "uint.cast_from_int/1 NYI for Erlang").

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

cast_to_int(_) = _ :-
    sorry($module, "uint.cast_to_int/1 NYI for Erlang").

%---------------------------------------------------------------------------%

X div Y = X // Y.

:- pragma inline('//'/2).
X // Y = Div :-
    ( if Y = 0u then
        throw(math.domain_error("uint.'//': division by zero"))
    else
        Div = unchecked_quotient(X, Y)
    ).

:- pragma inline('/'/2).
X / Y = X // Y.

X mod Y = X rem Y.

:- pragma inline(rem/2).
X rem Y = Rem :-
    ( if Y = 0u then
        throw(math.domain_error("uint.rem: division by zero"))
    else
        Rem = unchecked_rem(X, Y)
    ).

%---------------------------------------------------------------------------%

X << Y = Result :-
    ( if cast_from_int(Y) < cast_from_int(bits_per_uint) then
        Result = unchecked_left_shift(X, Y)
    else
        Msg = "uint.(<<): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

X >> Y = Result :-
    ( if cast_from_int(Y) < cast_from_int(bits_per_uint) then
        Result = unchecked_right_shift(X, Y)
    else
        Msg = "uint.(>>): second operand is out of range",
        throw(math.domain_error(Msg))
    ).

%---------------------------------------------------------------------------%

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

:- pragma inline(even/1).
even(X) :-
    (X /\ 1u) = 0u.

:- pragma inline(odd/1).
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

:- pragma foreign_proc("Erlang",
    bits_per_uint = (Bits::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    % XXX Erlang ints are actually arbitrary precision.
    Bits = 32
").

%---------------------------------------------------------------------------%

uint_to_doc(X) = str(string.uint_to_string(X)).

%---------------------------------------------------------------------------%
:- end_module uint.
%---------------------------------------------------------------------------%
