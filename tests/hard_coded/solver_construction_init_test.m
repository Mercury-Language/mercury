%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% solver_construction_init_test.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Mar  9 12:24:52 EST 2005
%
% This test is disabled, because automatic initialization of solver variables
% is no longer supported.
%
%---------------------------------------------------------------------------%

:- module solver_construction_init_test.

:- interface.

:- import_module io.
:- import_module list.

:- solver type t.

:- func f = (list(t)::oa) is det.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- solver type t
    where   representation is int,
            initialisation is init,
            ground is ground,
            any is ground.

f = [_].

:- pred init(t::oa) is det.
:- pragma promise_pure(init/1).
init(X) :-
    impure X = 'representation to any t/0'(123).

main(!IO) :-
    io.print("Hello, World!\n", !IO).
