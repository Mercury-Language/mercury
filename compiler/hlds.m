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

:- type goal		--->	goal2 - liveness_info.

:- type goal2		--->	conj(goals)
					% could use conj(goals) instead 
				
				% The front-end annotates each call with
				% its mode
			;	call(pred_id, mode_id, list(var_id)) XXX

				% Deterministic disjunctions are converted
				% into case statements by the compiler
				% front-end.
			;	case(var_id, list(pair(term, goal)))

			% The remainder aren't used as yet, since
			% we only handle deterministic code.

			;	disj(goals)
			;	not(vars,goal)
					% could use if_then_else instead
			;	all(vars,goal)
			;	some(vars,goal)
			;	if_then_else(vars,goal,goal,goal)
			;	error.

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
					map(pred_id, list(mode_id)),
					map(pair(pred_id, mode_id), clause),
					map(pair(pred_id, mode_id), category),
					map(type_id, type_body)
				).

:- type pred_id 	=	pred(string, string, int).
			%	module, predname, arity

:- type mode_id		=	int.

:- type clause		--->	clause(var_info, list(var), goal).
			%	variable types, head variables, body

:- type var_info	--->	var_info(
					map(var, string),	% var names
					map(var, type)		% var types
				).

:- type liveness_info	=	map(var_id, is_live)
:- type is_live		--->	live ; dead.

:- type mode_info	=	map(var, instantiatedness).

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
