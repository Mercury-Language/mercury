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

:- import_module mdb__browser_info.
:- import_module char, list, std_util.

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
			list(var_rep)		% the call's plain arguments
		)
	;	method_call_rep(
			var_rep,		% typeclass info var
			int,			% method number
			list(var_rep)		% the call's plain arguments
		)
	;	plain_call_rep(
			string,			% name of called pred's module
			string,			% name of the called pred
			list(var_rep)		% the call's arguments
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

	% If the given atomic goal behaves like a call in the sense that it
	% generates events, then return the list of variables that are passed
	% as arguments.
	%
:- func atomic_goal_generates_event(atomic_goal_rep) = maybe(list(var_rep)).

	% call_is_primitive(ModuleName, PredName): succeeds iff a call to the
	% named predicate behaves like a primitive operation, in the sense that
	% it does not generate events.
:- pred call_is_primitive(string::in, string::in) is semidet.

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

:- pred path_from_string_det(string, goal_path).
:- mode path_from_string_det(in, out) is det.

:- pred path_from_string(string, goal_path).
:- mode path_from_string(in, out) is semidet.

:- pred path_step_from_string(string, goal_path_step).
:- mode path_step_from_string(in, out) is semidet.

:- pred is_path_separator(char).
:- mode is_path_separator(in) is semidet.

	% User-visible head variables are represented by a number from 1..N,
	% where N is the user-visible arity.
	%
	% Both user-visible and compiler-generated head variables can be
	% referred to via their position in the full list of head variables;
	% the first head variable is at position 1.
	
:- type arg_pos
	--->	user_head_var(int)	% Nth in the list of arguments after
					% filtering out non-user-visible vars.
	;	any_head_var(int).	% Nth in the list of all arguments.

	% A particular subterm within a term is represented by a term_path.
	% This is the list of argument positions that need to be followed
	% in order to travel from the root to the subterm.  In contrast to
	% goal_paths, this list is in top-down order.

:- type term_path ==	list(int).

:- pred convert_dirs_to_term_path(list(dir), term_path).
:- mode convert_dirs_to_term_path(in, out) is det.

	% Returns type_of(_ `with_type` proc_rep), for use in C code.
:- func proc_rep_type = type_desc.

	% Returns type_of(_ `with_type` goal_rep), for use in C code.
:- func goal_rep_type = type_desc.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, char, require.

atomic_goal_generates_event(unify_construct_rep(_, _, _)) = no.
atomic_goal_generates_event(unify_deconstruct_rep(_, _, _)) = no.
atomic_goal_generates_event(unify_assign_rep(_, _)) = no.
atomic_goal_generates_event(unify_simple_test_rep(_, _)) = no.
atomic_goal_generates_event(pragma_foreign_code_rep(_)) = no.
atomic_goal_generates_event(higher_order_call_rep(_, Args)) = yes(Args).
atomic_goal_generates_event(method_call_rep(_, _, Args)) = yes(Args).
atomic_goal_generates_event(plain_call_rep(ModuleName, PredName, Args)) =
	( call_is_primitive(ModuleName, PredName) ->
		% These calls behave as primitives and do not generate events.
		no
	;
		yes(Args)
	).

call_is_primitive(ModuleName, PredName) :-
	ModuleName = "builtin",
	( PredName = "unify"
	; PredName = "compare"
	).

convert_dirs_to_term_path([], []).
convert_dirs_to_term_path([child_num(N) | Dirs], [N | TermPath]) :-
	convert_dirs_to_term_path(Dirs, TermPath).
convert_dirs_to_term_path([child_name(_) | _], _) :-
	error("convert_dirs_to_term_path: not in canonical form").
convert_dirs_to_term_path([parent | _], _) :-
	error("convert_dirs_to_term_path: not in canonical form").

:- pragma export(proc_rep_type = out, "ML_proc_rep_type").

proc_rep_type = type_of(_ `with_type` proc_rep).

:- pragma export(goal_rep_type = out, "ML_goal_rep_type").

goal_rep_type = type_of(_ `with_type` goal_rep).

%-----------------------------------------------------------------------------%

path_from_string_det(GoalPathStr, GoalPath) :-
	( path_from_string(GoalPathStr, GoalPathPrime) ->
		GoalPath = GoalPathPrime
	;
		error("path_from_string_det: path_from_string failed")
	).

path_from_string(GoalPathStr, GoalPath) :-
	StepStrs = string__words(is_path_separator, GoalPathStr),
	list__map(path_step_from_string, StepStrs, GoalPath).

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

is_path_separator(';').

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
