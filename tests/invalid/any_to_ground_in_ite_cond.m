%-----------------------------------------------------------------------------%
% any_to_ground_in_ite_cond.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Sep  5 15:28:33 EST 2005
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module any_to_ground_in_ite_cond.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, std_util.

:- solver type st where representation is int, initialisation is i.

%-----------------------------------------------------------------------------%

% We shouldn't be able to ground a solver variable in the
% condition of an if-then-else.
%
main(!IO) :-
    i(X),
    promise_pure (
      if p(X) then
        io.write_string("aye\n", !IO)
      else
        io.write_string("nay\n", !IO)
    ).

:- pred i(st::oa) is det.

i(X) :- promise_pure(impure X = 'representation to any st/0'(42)).

:- pred p(st::(any >> ground)) is semidet.

:- pragma foreign_proc("C", p(_X::(any >> ground)), [promise_pure],
    "SUCCESS_INDICATOR = MR_TRUE;").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
