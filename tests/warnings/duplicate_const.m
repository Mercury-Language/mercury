%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the warning for duplicate calls where some of the arguments are
% duplicated constants.
%
%---------------------------------------------------------------------------%

:- module duplicate_const.

:- interface.

:- pred dup_call(int::in, int::in, int::out) is det.

:- pred called(T::in, int::in, int::in, int::out) is det.

:- implementation.
:- import_module int.

dup_call(Int1, Int2, Int) :-
    called(1, Int1, Int2, Int3),
    called(1, Int1, Int2, Int4),
    Int is Int3 + Int4.

called(_, Int1, Int2, Int) :-
    Int is Int1 + Int2.
