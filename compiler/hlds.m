%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% HLDS - The High-Level Data Structure.

:- module hlds.
:- import_module integer, string, list, varset, term, map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- adt module_info.

:- adt pred_id.

:- adt mode_id.

:- type category	--->	deterministic		% functional & total
			;	semideterministic	% just functional
			;	nondeterministic.	% neither

:- type goal		--->	goal_expr - liveness_info.

:- type goal_expr    	--->	conj(goals)

				% Initially only the pred_id and arguments
				% are filled in.  Mode analysis fills in the
				% mode_id.  Just before code generation,
				% we do a pass over the hlds which recognizes
				% the builtins and fills in the is_builtin
				% field.
			;	call(pred_id, mode_id, list(term), is_builtin)

				% Deterministic disjunctions are converted
				% into case statements by the determinism
				% analysis.
				% Variable, functor-args-goal, followvars
			;	switch(var_id, list(case), vars)

				% Initially only the two terms are filled
				% in.  Mode analysis fills in the last
				% two fields.
			;	unify(term, term, unify_mode, unification)

			% The remainder aren't used as yet, since
			% we only handle deterministic code.

			;	disj(goals)
			;	not(vars,goal)
					% could use if_then_else instead
			;	all(vars,goal)
			;	some(vars,goal)
			;	if_then_else(vars,goal,goal,goal)
			;	error.

	% Record whether a call is a builtin or not, and if so, which one.
:- type is_builtin	--->	not_builtin
			;	is_builtin(builtin).

:- type builtin		--->	plus
			;	times.
				% etc.

:- type case		--->	case(const, list(var), goal).
			%	functor to match with, arguments to extract,
			%	goal to execute if match succeeds.

	% Initially all unifications are represented as
	% unify(term = term), but mode analysis replaces
	% these with various special cases.

:- type unification	--->	(term = term)	% before mode analysis

				% Y = f(X) where the top node of Y is output,
				% written as Y := f(X).
			;	construct(var, const, list(var), list(mode))

				% Y = f(X) where the top node of Y is input,
				% written Y == f(X).
			;	deconstruct(var, const, list(var), list(mode))

				% Y = X where the top node of Y is output,
				% written Y := X.
			;	assign(var, var)

				% Y = X where the type of X and Y is an atomic
				% type and they are both input, written X == Y.
			;	simple_test(var, var)	

				% Y = X where the type of Y and X is not an
				% atomic type, and where the top-level node
				% of both Y and X is input.  May involve
				% bi-directional data flow.  Implemented
				% using out-of-line call to  a compiler
				% generated unification predicate for that
				% type & mode.
			;	complicated_unify(mode_id, var, var).

:- type goals		==	list(goal).
:- type vars		==	list(variable).

%-----------------------------------------------------------------------------%

	% This is how types are represented.  XXX

:- type type_defn	--->	du_type(string, list(term), list(constructor))
			;	%%% uu_type(string, list(term), list(type_body))
			;	eqv_type(string, list(term), type_body).
:- type constructor	==	term.
:- type type_head	==	term.
:- type type_body	==	term.
:- type (type)		==	term.

:- type condition	--->	true
			;	where(term).

%-----------------------------------------------------------------------------%

:- type int		==	integer.

%-----------------------------------------------------------------------------%
:- pred module_name(module_info, string).
:- mode module_name(input,output).

:- pred module_predicates(module_info, list(pred_id)).
:- mode module_predicates(input, output).

:- pred predicate_module(module_info, pred_id, string).
:- mode predicate_module(input, input, output).

:- pred predicate_name(module_info, pred_id, string).
:- mode predicate_name(input, input, output).

:- pred predicate_arity(module_info, pred_id, int).
:- mode predicate_arity(input, input, output).

:- pred predicate_modes(module_info, pred_id, list(mode_id)).
:- mode predicate_modes(input, input, output).

:- pred predicate_clauses(module_info, pred_id, mode_id, list(clause)).
:- mode predicate_clauses(input, input, output).

:- pred predicate_category(module_info, pred_id, mode_id, category).
:- mode predicate_category(input, input, input, output).

:- pred var_is_live(module_info, liveness_info, var_is, is_live).
:- mode var_is_live(input, input, input, output).

:- pred var_type(module_info, var_info, var_id, type).
:- mode var_type(input, input, input, output).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- type module_info	--->	module(
					string,		% module name
					map(pred_id, pred_info),
					map(type_id, type_body),
					map(inst_id, inst_body),
					map(mode_id, mode_body),
					map(pair(pred_id, mode_id), category)
				).

:- type pred_info	--->	predicate(
					list(proc_id),	% usually [1,2,...]
					map(proc_id, proc_info),
					% not needed: list(type) for args
				).

:- type proc_info	--->	procedure(
					category,
					list(var_id),	% usually [1,2,...]
					map(var_id, var_info), % all vars
					list(clause)  % singleton for det...
				).

:- type pred_id 	=	pred(string, string, int).
			%	module, predname, arity

:- type proc_id		=	proc(pred_id, mode_id).

:- type mode_id		=	int.

:- type clause		--->	clause(
					list(var_id),
					mode_info,
					goal
				).

			%	head variables, body

:- type var_info	--->	var_info(
					string, % name
					type
				).

:- type goalinfo	=	goalinfo (
					map(var_id, is_live)
				).

:- type is_live		--->	live ; dead.

:- type mode_info	=	map(var_id, instantiatedness).

%-----------------------------------------------------------------------------%

	% Various access predicates for the module_info data structure

module_get_modes(...) :-

module_name(module(Name,_,_,_,_),Name).

module_predicates(module(_,PredInfo,_,_,_), PredIds) :-
	map__keys(PredInfo,PredIds).

	% Various access predicates for the module_info and/or pred_id
	% data structures

predicate_module(_, pred(Module,_Name,_Arity), Module).

predicate_name(_, pred(_Module,Name,_Arity), Name).

predicate_arity(_, pred(_Module,_Name,Arity), Arity).

predicate_modes(module(_,PredInfo,_,_,_), PredId, Modes) :-
	map__search(PredInfo, PredId, Modes).

predicate_clauses(module(_,_,ClauseInfo,_,_), PredId, ModeId, Clauses) :-
	map__search(ClauseInfo, PredId - ModeId, Clauses).

predicate_category(module(_,_,_,CategoryInfo,_), PredId, ModeId, Category) :-
	map__search(CategoryInfo, PredId - ModeId, Category).

mode_is_input(...) :-

	% 


mode_id_to_int(X, X).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
