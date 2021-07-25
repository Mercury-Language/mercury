%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% unsafe_uniqueness.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Dec 19 13:42:42 EST 2001
%
% Test of built-in unsafe_promise_unique/1. This shouldn't compile
% if there is a problem with unsafe_promise_unique/1.
%
%---------------------------------------------------------------------------%

:- module unsafe_uniqueness.

:- interface.

:- import_module io.

:- type wrapper(T)
    --->    wrapper(T).

:- func unwrap(wrapper(T)) = T.
:- mode unwrap(di) = uo is det.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

unwrap(wrapper(X)) = unsafe_promise_unique(X).

%---------------------------------------------------------------------------%

main(!IO) :-
    io.print("I must have compiled!\n", !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
