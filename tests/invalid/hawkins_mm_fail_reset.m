% $ mmc --make foo --grade reg.mmsc.gc --no-warn-singleton-variables
% Making Mercury/int3s/foo.int3
% Making Mercury/cs/foo.c
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%          Key Type: term.var(parse_tree.prog_data.prog_var_type)
%          Key Value: var(5)
%          Value Type: ll_backend.var_locn.var_state
% Stack dump not available in this grade.
% foo.m:003: In `entry(out)':
% foo.m:003:   warning: determinism declaration could be tighter.
% foo.m:003:   Declared `nondet', inferred `failure'.
% foo.m:014: Warning: this disjunct will never have any solutions.
% foo.m:017: Warning: this disjunct will never have any solutions.
% ** Error making `Mercury/cs/foo.c'.

:- module hawkins_mm_fail_reset.
:- interface.

:- pred entry(int::out) is nondet.

:- implementation.
:- import_module int.

entry(0) :- pred1(_).

:- pred pred1(int::out).
:- pragma minimal_model(pred1/1, [allow_reset]).

pred1(2) :-
	pred2(_P).

pred1(1) :-
	pred2(_P).

:- pred pred2(int::out).
:- pragma minimal_model(pred2/1, [allow_reset]).

pred2(_) :-
	fail.
