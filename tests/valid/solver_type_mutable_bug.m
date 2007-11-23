% Since rotd-2007-11-22 we have been unable to successfully compile this 
% test case due to the conjunction of (a) the removal of automatic solver
% type initialisation and (b) the change to inst matching in r1.82 of
% compiler/inst_match.m.  Consequently, this test case is now disabled.
% If some form of automatic solver type initialisation is added back to the
% language in the future it should be re-enabled.
%
% The following program, derived from msat.m by Ralph Becket, caused
% rotd-2006-03-19 to abort in deep profiling grades.  The problem was that the
% varset attached to the mutable initialisation procedure was incorrect and
% the deep profiling transformation was clobbering existing variables when it
% tried to introduce new ones.  The bug eventually showed up as an abort in
% the code generator.

:- module solver_type_mutable_bug.
:- interface.

:- solver type sat_literal.
:- pred new_raw_sat_literal(sat_literal::oa) is det.

:- implementation.

:- solver type sat_literal
    where   representation  is int,
            initialisation  is new_raw_sat_literal.

:- pragma foreign_proc("C",
    new_raw_sat_literal(A::oa),
    [promise_pure, will_not_call_mercury],
"
    A = 3;
").

:- mutable(global, sat_literal, _, any,    [untrailed]).

