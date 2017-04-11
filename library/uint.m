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
:- func uint + int = uint.
:- mode in   + in  = uo is det.
%:- mode uo   + in  = in is det.
%:- mode in   + uo  = in is det.

    % Subtraction.
    %
:- func uint - uint = uint.
:- mode in   - in   = uo is det.
%:- mode uo   - in   = in is det.
%:- mode in   - uo   = in is det.

    % Multiplication.
    %
:- func (uint::in) * (uint::in) = (uint::uo) is det.

    % Maximum.
    %
:- func max(uint, uint) = uint.

    % Minimum.
    %
:- func min(uint, uint) = uint.

:- func unchecked_quotient(uint::in, uint::in) = (uint::uo) is det.

:- func unchecked_rem(uint::in, uint::in) = (uint::uo) is det.

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
:- func xor(uint::in, uint::in) = (uint::uo) is det.

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
    ( if from_int(I, U0)
    then U = U0
    else error("uint.det_from_int: cannot convert int to uint")
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

max(X, Y) =
    ( if X > Y then X else Y ).

min(X, Y) =
    ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

:- pragma inline(even/1).
even(X) :-
    (X /\ cast_from_int(1)) = cast_from_int(0).

:- pragma inline(odd/1).
odd(X) :-
    (X /\ cast_from_int(1)) \= cast_from_int(0).

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
