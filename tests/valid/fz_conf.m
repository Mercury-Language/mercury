% This program causes the following error in rotd-2007-10-22:
% (The code is derived from the G12 FlatZinc interpreter.)
%
% fz_conf.m:020: In clause for initialisation predicate for type
% fz_conf.m:020:   `zinc_sat_literal':
% fz_conf.m:020:   in argument 1 of call to predicate
% fz_conf.m:020:   `fz_conf.new_msat_literal'/1:
% fz_conf.m:020:   type error: variable `HeadVar__1' has type
% fz_conf.m:020:   `(fz_conf.zinc_sat_literal)',
% fz_conf.m:020:   expected type was `(fz_conf.msat_literal)'.
%
% Compile with `mmc -C fz_conf.m' to reproduce.
%
:- module fz_conf.
:- interface.

:- solver type msat_literal.

:- pred new_msat_literal(msat_literal::oa) is det.

:- type zinc_sat_literal == msat_literal.

:- implementation.

:- solver type msat_literal
    where   representation  is int,
            initialisation  is new_msat_literal.

new_msat_literal(A) :-
	promise_pure (
		impure A = 'representation to any msat_literal/0'(1)
	).
