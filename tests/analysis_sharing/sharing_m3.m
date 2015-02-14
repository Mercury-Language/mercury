%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module sharing_m3.
:- interface.

:- import_module sharing_m2.

:- pred ccc(foo::in, foo::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module sharing_m1.

:- pragma no_inline(ccc/2).

ccc(N, M) :-
    aaa2(N, M).
