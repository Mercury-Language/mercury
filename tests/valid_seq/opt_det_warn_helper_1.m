%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module opt_det_warn_helper_1.
:- interface.
:- import_module io.

:- pred foo(io::di, io::uo) is det.

:- implementation.
:- import_module exception.

foo(_, _) :-
    throw("help!").
