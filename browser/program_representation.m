%-----------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: program_representation.m
% Authors: zs, dougl
%
% This module defines the representation of procedure bodies used by
% the declarative debugger.
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
	--->	conj(
			list(goal_rep)		% The conjuncts in reverse
						% order.
		)
	;	disj(
			list(goal_rep)		% The disjuncts in the original
						% order.
		)
	;	switch(
			list(goal_rep)		% The switch arms in the
						% original order.
		)
	;	ite(
			goal_rep,		% Condition.
			goal_rep,		% Then branch.
			goal_rep		% Else branch.
		)
	;	negation(
			goal_rep		% The negated goal.
		)
	;	atomic_goal(
			detism_rep,
			string,			% Filename of context.
			int,			% Line number of context.
			list(var_rep),		% The sorted list of the
						% variables bound by the
						% atomic goal.
			atomic_goal_rep
		).

:- type atomic_goal_rep
	--->	unify_construct(
			var_rep,
			list(var_rep)
		)
	;	unify_deconstruct(
			var_rep,
			list(var_rep)
		)
	;	unify_assign(
			var_rep,
			var_rep
		)
	;	unify_simple_test(
			var_rep,
			var_rep
		)
	;	pragma_c_code(
			list(var_rep)
		)
	;	higher_order_call(
			var_rep,
			list(var_rep)
		)
	;	method_call(
			var_rep,
			int,
			list(var_rep)
		)
	;	plain_call(
			string,
			list(var_rep)
		).

:- type var_rep	==	int.

:- type detism_rep	
	--->	det
	;	semidet
	;	nondet
	;	multidet
	;	cc_nondet
	;	cc_multidet
	;	erroneous
	;	failure.
