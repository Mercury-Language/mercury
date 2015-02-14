%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module sharing_m2.
:- interface.

:- type foo
    --->    foo(int, int).

:- pred bbb(foo::in, foo::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module sharing_m3.

:- pragma no_inline(bbb/2).

bbb(N, M) :-
    ccc(N, M).

