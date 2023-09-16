%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unused_args_analysis.

:- interface.

:- pred p(int::in, int::out) is det.

:- implementation.

:- import_module unused_args_analysis_helper_1.

p(X, Y) :-
    p2(X, Y).
