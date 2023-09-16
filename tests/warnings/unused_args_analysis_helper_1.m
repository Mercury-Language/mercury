%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unused_args_analysis_helper_1.

:- interface.

:- pred p2(int::in, int::out) is det.

:- implementation.

p2(_, 1).
