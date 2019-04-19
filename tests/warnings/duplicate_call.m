%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the warning for duplicate calls.
%
%---------------------------------------------------------------------------%

:- module duplicate_call.

:- interface.

:- pred dup_call(int::in, int::in, int::out) is det.

:- pred called(int::in, int::in, int::out) is det.

:- implementation.
:- import_module int.

dup_call(Int1, Int2, Int) :-
    called(Int1, Int2, Int3),
    called(Int1, Int2, Int4),
    Int = Int3 + Int4.

called(Int1, Int2, Int) :-
    Int = Int1 + Int2.
