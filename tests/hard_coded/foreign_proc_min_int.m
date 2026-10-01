%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_proc_min_int.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.

main(!IO) :-
    I8 = cast_int8(-128_i8),
    io.print_line(I8, !IO),

    I16 = cast_int16(-32768_i16),
    io.print_line(I16, !IO),

    I32 = cast_int32(-2147483648_i32),
    io.print_line(I32, !IO).

%---------------------------------------------------------------------------%

:- func cast_int8(int8) = int64.

:- pragma foreign_proc("C",
    cast_int8(I8::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I64 = (int64_t) I8;
").

cast_int8(I) = int64.from_int(int8.cast_to_int(I)).

%---------------------------------------------------------------------------%

:- func cast_int16(int16) = int64.

:- pragma foreign_proc("C",
    cast_int16(I16::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I64 = (int64_t) I16;
").

cast_int16(I) = int64.from_int(int16.cast_to_int(I)).

%---------------------------------------------------------------------------%

:- func cast_int32(int32) = int64.

:- pragma foreign_proc("C",
    cast_int32(I32::in) = (I64::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
    I64 = (int64_t) I32;
").

cast_int32(I) = int64.from_int(int32.cast_to_int(I)).

%---------------------------------------------------------------------------%
