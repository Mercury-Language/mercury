%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The compiler of 2009-06-11 aborted on this test case.

:- module implied_mode_no_tag.
:- interface.

:- pred p(string::in) is semidet.

:- implementation.

:- type nt
    --->    nt(string).

p(S) :-
    q(S, nt(_)).

:- pred q(string::in, nt::out) is det.
:- pragma no_inline(q/2).

q(S, nt(S)).
