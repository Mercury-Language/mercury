% Was trying to copy dummy outputs back to stack of parent thread.
%
% Uncaught Mercury exception:
% Software Error: llds_out.m: Unexpected: stack var out of range

:- module par_dummy.
:- interface.

:- type t ---> t.
:- pred p(t::out) is det.

:- implementation.

p(X) :-
    ( true
    & X = t
    ).
