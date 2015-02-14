%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module reuse_m2.
:- interface.

:- type foo
    --->    foo(int, int).

:- pred fiddle2(foo::in, foo::in, foo::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module reuse_m3.

:- pragma no_inline(fiddle2/3).

fiddle2(X, Y, Z) :-
    fiddle3(X, Y, Z).
