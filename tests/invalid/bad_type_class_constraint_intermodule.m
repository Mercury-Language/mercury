%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_type_class_constraint_intermodule.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bad_type_class_constraint_intermodule_2.

main(!IO) :-
    bad_pred(io.stdout_stream, 42, !IO).

%---------------------------------------------------------------------------%

