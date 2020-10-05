%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_unused_args2.
:- interface.

:- pred callee(int::in, int::in, int::in, int::out) is det.

:- implementation.

:- pragma no_inline(callee/4).
callee(_, _, _, 4).
