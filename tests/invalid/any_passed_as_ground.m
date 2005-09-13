%-----------------------------------------------------------------------------%
% any_passed_as_ground.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Sep  5 15:28:33 EST 2005
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module any_passed_as_ground.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, std_util.

:- solver type st where representation is int, initialisation is i.

%-----------------------------------------------------------------------------%

% We shouldn't be able to pass Xs to member/2 because
% member/2 expects Xs to be ground.  The compiler should
% report an error.
%
main(!IO) :-
    p(Xs),
    ( if member((X - _), Xs) then Y = X else Y = 0 ),
    io.write_int(Y, !IO).

:- pred i(st::oa) is det.

i(X) :- promise_pure(impure X = 'representation to any st/0'(42)).

:- pred p(list(pair(int, st))::oa) is det.

p([1 - _, 2 - _, 3 - _]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
