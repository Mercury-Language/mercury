%-----------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: program_representation.m
% Authors: zs, dougl
%
% This module defines the representation of procedure bodies
% used by the declarative debugger.
%
% One of the things we want the declarative debugger to be able to do
% is to let the user specify which part of which output argument of an
% incorrect or inadmissible atom is suspicious, and then find out where
% that particular subterm came from, i.e. where it was bound. Doing this
% requires knowing what the bodies of that procedure and its descendants are.
%
% If the Mercury compiler is invoked with options requesting declarative
% debugging, it will include in each procedure layout a pointer to a simplified
% representation of the goal that is the body of the corresponding procedure.
% We use a simplified representation partly because we want to insulate the
% code of the declarative debugger from irrelevant changes in HLDS types,
% and partly because we want to minimize the space taken in up in executables
% by these representations.
%
% The current representation is intended to contain all the information
% we are pretty sure can be usefully exploited by the declarative debugger.

%-----------------------------------------------------------------------------%

:- module mdb__program_representation.

:- interface.

:- import_module list.

	% A representation of the goal we execute.  These need to be
	% generated statically and stored inside the executable.
	%
	% Each element of this structure will correspond one-to-one
	% to the original stage 90 HLDS, with the exception of conj
	% goal_reps, which are stored in reversed order.

:- type goal_rep
	--->	conj_rep(
			list(goal_rep)		% The conjuncts in reverse
						% order.
		)
	;	disj_rep(
			list(goal_rep)		% The disjuncts in the original
						% order.
		)
	;	switch_rep(
			list(goal_rep)		% The switch arms in the
						% original order.
		)
	;	ite_rep(
			goal_rep,		% Condition.
			goal_rep,		% Then branch.
			goal_rep		% Else branch.
		)
	;	negation_rep(
			goal_rep		% The negated goal.
		)
	;	some_rep(
			goal_rep		% The quantified goal.
		)
	;	atomic_goal_rep(
			detism_rep,
			string,			% Filename of context.
			int,			% Line number of context.
			list(var_rep),		% The sorted list of the
						% variables bound by the
						% atomic goal.
			atomic_goal_rep
		).

:- type atomic_goal_rep
	--->	unify_construct_rep(
			var_rep,
			cons_id_rep,
			list(var_rep)
		)
	;	unify_deconstruct_rep(
			var_rep,
			cons_id_rep,
			list(var_rep)
		)
	;	unify_assign_rep(
			var_rep,		% target
			var_rep			% source
		)
	;	unify_simple_test_rep(
			var_rep,
			var_rep
		)
	;	pragma_foreign_code_rep(
			list(var_rep)		% arguments
		)
	;	higher_order_call_rep(
			var_rep,		% the closure to call
			list(var_rep)		% arguments
		)
	;	method_call_rep(
			var_rep,		% typeclass info var
			int,			% method number
			list(var_rep)		% arguments
		)
	;	plain_call_rep(
			string,			% name of called pred
			list(var_rep)		% arguments
		).

:- type var_rep	==	int.

:- type cons_id_rep ==	string.

:- type detism_rep	
	--->	det_rep
	;	semidet_rep
	;	nondet_rep
	;	multidet_rep
	;	cc_nondet_rep
	;	cc_multidet_rep
	;	erroneous_rep
	;	failure_rep.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
