%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
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

:- import_module list, std_util.
:- import_module mdb__browser_info.

	% A representation of the goal we execute.  These need to be
	% generated statically and stored inside the executable.
	%
	% Each element of this structure will correspond one-to-one
	% to the original stage 90 HLDS.

:- type proc_rep
	--->	proc_rep(
			list(var_rep),		% The head variables, in order,
						% including the ones introduced
						% by the compiler.
			goal_rep		% The procedure body.
		).

:- type goal_rep
	--->	conj_rep(
			list(goal_rep)		% The conjuncts in the original
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
			goal_rep,		% The quantified goal.
			maybe_cut
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

	% If the given atomic goal is a call to a predicate or function
	% (not including the special predicates `unify' and `compare'),
	% then return the list of variables that are passed as arguments.
	%
:- pred atomic_goal_rep_is_call(atomic_goal_rep, list(var_rep)).
:- mode atomic_goal_rep_is_call(in, out) is semidet.

%-----------------------------------------------------------------------------%

	% The following three types are derived from compiler/hlds_goal.m.

:- type goal_path == list(goal_path_step).

:- type goal_path_step  --->    conj(int)
                        ;       disj(int)
                        ;       switch(int)
                        ;       ite_cond
                        ;       ite_then
                        ;       ite_else
                        ;       neg
                        ;       exist(maybe_cut)
                        ;       first
                        ;       later.

	% Does `some G' have a different determinism from plain `G'?
:- type maybe_cut       --->    cut ; no_cut.

:- pred path_step_from_string(string, goal_path_step).
:- mode path_step_from_string(in, out) is semidet.

	% Head variables are represented by a number from 1..N,
	% where N is the arity.
	
:- type arg_pos ==	var_rep.

	% A particular subterm within a term is represented by a term_path.
	% This is the list of argument positions that need to be followed
	% in order to travel from the root to the subterm.  In contrast to
	% goal_paths, this list is in top-down order.

:- type term_path ==	list(arg_pos).

:- pred convert_dirs_to_term_path(list(dir), term_path).
:- mode convert_dirs_to_term_path(in, out) is det.

	% Returns type_of(_ `with_type` goal_rep), for use in C code.
:- func goal_rep_type = type_desc.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, char, require.

atomic_goal_rep_is_call(pragma_foreign_code_rep(Args), Args).
atomic_goal_rep_is_call(higher_order_call_rep(_, Args), Args).
atomic_goal_rep_is_call(method_call_rep(_, _, Args), Args).
atomic_goal_rep_is_call(plain_call_rep(Name, Args), Args) :-
	Name \= "unify",
	Name \= "compare".

path_step_from_string(String, Step) :-
	string__first_char(String, First, Rest),
	path_step_from_string_2(First, Rest, Step).

:- pred path_step_from_string_2(char, string, goal_path_step).
:- mode path_step_from_string_2(in, in, out) is semidet.

path_step_from_string_2('c', NStr, conj(N)) :-
	string__to_int(NStr, N).
path_step_from_string_2('d', NStr, disj(N)) :-
	string__to_int(NStr, N).
path_step_from_string_2('s', NStr, switch(N)) :-
	string__to_int(NStr, N).
path_step_from_string_2('?', "", ite_cond).
path_step_from_string_2('t', "", ite_then).
path_step_from_string_2('e', "", ite_else).
path_step_from_string_2('~', "", neg).
path_step_from_string_2('q', "!", exist(cut)).
path_step_from_string_2('q', "", exist(no_cut)).
path_step_from_string_2('f', "", first).
path_step_from_string_2('l', "", later).

convert_dirs_to_term_path([], []).
convert_dirs_to_term_path([child_num(N) | Dirs], [N | TermPath]) :-
	convert_dirs_to_term_path(Dirs, TermPath).
convert_dirs_to_term_path([child_name(_) | _], _) :-
	error("convert_dirs_to_term_path: not in canonical form").
convert_dirs_to_term_path([parent | _], _) :-
	error("convert_dirs_to_term_path: not in canonical form").

:- pragma export(goal_rep_type = out, "ML_goal_rep_type").
goal_rep_type = type_of(_ `with_type` goal_rep).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
