%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% HLDS - The High-Level Data Structure.

% Main authors: fjh, conway.

% This file contains the data types for the high-level data structure.
% The file is arranged as follows: first all the data structures are
% listed.  Most of these are private to this module, with access
% predicates provided.  Those that are not are explicitly exported
% using a `:- export_type' directive.
% Then for each data structure, we give the interface and then
% the implementation for the access predicates for that data structure.
%
% Although most of the data structures are private, it is a quite thin
% layer of abstraction.
%
% WARNING: changes here will probably require changes in make_hlds.nl
% and elsewhere.

:- module hlds.
:- interface.
:- import_module float, int, string, list, set, map, std_util.
:- import_module varset, term.
:- import_module prog_io, llds.

:- implementation.
:- import_module prog_util, mode_util, unify_proc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- type module_info.

:- type predicate_table.
:- type pred_table	==	map(pred_id, pred_info).
:- type pred_id.
:- type pred_info.

:- type pred_call_id	--->	sym_name / arity.

:- type proc_table	==	map(proc_id, proc_info).
	% a proc_id is a mode number within a particular predicate -
	% not to be confused with a mode_id, which is the name of a
	% user-defined mode.
:- type proc_id		==	int.
:- type proc_info.

%-----------------------------------------------------------------------------%

:- implementation.

:- type module_info	--->	module(
					string,		% module name
					predicate_table,
					unify_requests,
					unify_pred_map,
					type_table,
					inst_table,
					mode_table,
					cons_table,
					int,		% number of errors
					int		% number of warnings
				).

:- interface.

:- type unify_pred_map	==	map(type_id, pred_id).

:- type unify_requests.

:- type clauses_info	--->	clauses_info(
					varset,		% variable names
					map(var, type), % variable types
					list(var),	% head vars
					list(clause)
				).

:- type clause		--->	clause(
					list(proc_id),  % modes for which
							% this clause applies
					hlds__goal,	% Body
					term__context
				).

:- implementation.

:- type proc_info	--->	procedure(
					determinism,	% _declared_ determism
					varset,		% variable names
					map(var, type),	% variable types
					list(var),	% head vars
					list(mode), 	% modes of args
					hlds__goal,	% Body
					term__context,	% The context of
							% the :- mode decl,
							% not the clause.	
					call_info,	% stack allocations
					category,	% _inferred_ det'ism
					list(arg_info),	% information about
							% the arguments
							% derived from the
							% modes etc
					liveness_info,	% the initial liveness
					follow_vars	% initial followvars
				).

:- interface.

:- type category	--->	deterministic		% functional & total
			;	semideterministic	% just functional
			;	nondeterministic.	% neither

:- type procedure_id	--->	proc(pred_id, proc_id).

:- type liveness_info   ==      set(var).	% The live variables

:- type liveness        --->    live
                        ;       dead.

%-----------------------------------------------------------------------------%

	% The symbol table for types.

:- type type_id		== 	pair(sym_name, arity).
				% name, arity

:- type type_table	==	map(type_id, hlds__type_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for modes.

:- type mode_id		==	pair(sym_name, arity).
				% name, arity

:- type mode_table	==	map(mode_id, hlds__mode_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for insts.

:- type inst_id		==	pair(sym_name, arity).
				% name, arity.

:- type inst_table.

:- type user_inst_table	==	map(inst_id, hlds__inst_defn).

:- type unify_inst_table ==	map(unify_inst_pair, maybe_inst_det).

:- type unify_inst_pair	--->	unify_inst_pair(is_live, inst, inst).

:- type merge_inst_table ==	map(pair(inst), maybe_inst).

:- type ground_inst_table == 	map(inst_name, maybe_inst).

:- type maybe_inst	--->	unknown
			;	known(inst).

:- type maybe_inst_det	--->	unknown
			;	known(inst, determinism).

:- pred inst_table_init(inst_table).
:- mode inst_table_init(out) is det.

:- pred inst_table_get_user_insts(inst_table, user_inst_table).
:- mode inst_table_get_user_insts(in, out) is det.

:- pred inst_table_get_unify_insts(inst_table, unify_inst_table).
:- mode inst_table_get_unify_insts(in, out) is det.

:- pred inst_table_get_merge_insts(inst_table, merge_inst_table).
:- mode inst_table_get_merge_insts(in, out) is det.

:- pred inst_table_get_ground_insts(inst_table, ground_inst_table).
:- mode inst_table_get_ground_insts(in, out) is det.

:- pred inst_table_set_user_insts(inst_table, user_inst_table, inst_table).
:- mode inst_table_set_user_insts(in, in, out) is det.

:- pred inst_table_set_unify_insts(inst_table, unify_inst_table, inst_table).
:- mode inst_table_set_unify_insts(in, in, out) is det.

:- pred inst_table_set_merge_insts(inst_table, merge_inst_table, inst_table).
:- mode inst_table_set_merge_insts(in, in, out) is det.

:- pred inst_table_set_ground_insts(inst_table, ground_inst_table, inst_table).
:- mode inst_table_set_ground_insts(in, in, out) is det.

:- implementation.

:- type inst_table
	--->	inst_table(
			user_inst_table,
			unify_inst_table,
			merge_inst_table,
			ground_inst_table
		).

inst_table_init(inst_table(UserInsts, UnifyInsts, MergeInsts, GroundInsts)) :-
	map__init(UserInsts),
	map__init(UnifyInsts),
	map__init(MergeInsts),
	map__init(GroundInsts).

inst_table_get_user_insts(inst_table(UserInsts, _, _, _), UserInsts).

inst_table_get_unify_insts(inst_table(_, UnifyInsts, _, _), UnifyInsts).

inst_table_get_merge_insts(inst_table(_, _, MergeInsts, _), MergeInsts).

inst_table_get_ground_insts(inst_table(_, _, _, GroundInsts), GroundInsts).

inst_table_set_user_insts(inst_table(_, B, C, D), UserInsts,
			inst_table(UserInsts, B, C, D)).

inst_table_set_unify_insts(inst_table(A, _, C, D), UnifyInsts,
			inst_table(A, UnifyInsts, C, D)).

inst_table_set_merge_insts(inst_table(A, B, _, D), MergeInsts,
			inst_table(A, B, MergeInsts, D)).

inst_table_set_ground_insts(inst_table(A, B, C, _), GroundInsts,
			inst_table(A, B, C, GroundInsts)).

%-----------------------------------------------------------------------------%

:- interface.

	% The symbol table for constructors.
	% This table is used by the type-checker to look
	% up the type of functors/constants.

:- type cons_id		--->	cons(string, arity)	% name, arity
			;	int_const(int)
			;	string_const(string)
			;	float_const(float).

:- type cons_table	==	map(cons_id, list(hlds__cons_defn)).

%-----------------------------------------------------------------------------%

:- type arg_info	--->	arg_info(
					arg_loc,	% stored location
					arg_mode	% mode of top functor
				).

:- type arg_mode	
	--->	top_in
	;	top_out
	;	top_unused.

:- type arg_loc == int.

%-----------------------------------------------------------------------------%

	% Here's how goals are represented

:- type hlds__goal		== pair(hlds__goal_expr, hlds__goal_info).

:- type hlds__goal_expr    	--->	
				% A conjunction.
				% Note: conjunctions must be fully flattened.
				conj(hlds__goals)

				% A predicate call.
				% Initially only the sym_name and arguments
				% are filled in.  Type analysis fills in the
				% pred_id.  Mode analysis fills in the
				% proc_id and the is_builtin field.
				% `follow_vars.nl' fills in
				% the follow_vars field.
			;	call(pred_id, proc_id, list(term), is_builtin,
					sym_name, follow_vars)

				% Deterministic disjunctions are converted
				% into case statements by the switch
				% detection pass.
				% Variable, local determinism, cases
			;	switch(var, category, list(case))

				% A unification.
				% Initially only the terms and the context
				% are know.  Mode analysis fills in the
				% missing information.
			;	unify(term, term, unify_mode, unification,
								unify_context)
				% A disjunction.
				% Note: disjunctions must be fully flattened.
			;	disj(hlds__goals)

				% A negation
			;	not(hlds__goal)

				% An explicit quantification.
				% Quantification is stored in the goal_info,
				% so these get ignored (except to recompute
				% the goal_info quantification).
				% `all Vs' gets converted to `not some Vs not'.
			;	some(list(var), hlds__goal)

				% An if-then-else.
			;	if_then_else(list(var), hlds__goal,
					hlds__goal, hlds__goal).

	% Record whether a call is a builtin or not, and if so, which one.
:- type is_builtin	--->	not_builtin
			;	is_builtin.

:- type call_info	==	map(var, lval).

:- type case		--->	case(cons_id, hlds__goal).
			%	functor to match with,
			%	goal to execute if match succeeds.

	% Initially all unifications are represented as
	% unify(term, term, _, _), but mode analysis replaces
	% these with various special cases.

:- type follow_vars	==	map(var, lval).

:- type unification	--->	
				% Y = f(X) where the top node of Y is output,
				% written as Y := f(X).
				construct(var, cons_id, list(var),
								list(uni_mode))

				% Y = f(X) where the top node of Y is input,
				% written Y == f(X).
			;	deconstruct(var, cons_id, list(var),
						list(uni_mode), category)
					% Var, Functor, ArgVars, ArgModes, Det

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
			;	complicated_unify(uni_mode,
						category, follow_vars).

:- type unify_context	--->	unify_context(
					unify_main_context,
					unify_sub_contexts
				).

:- type unify_main_context --->	explicit
			;	head(int)
			;	call(pred_call_id, int).

:- type unify_sub_context ==	pair(cons_id, int).

:- type unify_sub_contexts ==	list(unify_sub_context).

:- type hlds__goals		==	list(hlds__goal).

:- type hlds__goal_info
	---> goal_info(
		delta_liveness,	% the changes in liveness after goal
		category,	% the `local' determinism of the goal
		category, 	% the overall determinism of the goal
		instmap_delta,
		term__context,
		set(var),	% the non-local vars in the goal
		delta_liveness,	% the changes in liveness before goal
		maybe(map(var, lval))
				% the new store_map, if any - this records
				% where to store variables at the end of
				% branched structures.
	).

:- type unify_mode	==	pair(mode, mode).

:- type uni_mode	--->	pair(inst) -> pair(inst).

%-----------------------------------------------------------------------------%

	% This is how type, modes and constructors are represented.
	% The parts that are not defined here (i.e. type_param, constructor,
	% type, inst, mode, condition) are represented in the same way as
	% in prog_io.nl, and are defined there.

:- type hlds__type_defn	--->	hlds__type_defn(
						% names of type vars (empty 
						% except for polymorphic types)
					tvarset,	
						% formal type parameters
					list(type_param),
						% the definition of the type
					hlds__type_body,
						% a class invariant for the
						% type (not used)
					condition,
					term__context
				).

	% du = discriminated union, uu = undiscriminated union,
	% eqv_type = equivalence type (a type defined to be eqv to some
	% other type)

:- type hlds__type_body
	--->	du_type(
			list(constructor), 	% the ctors for this type
			cons_tag_values,	% their tag values
			bool		% is this type an enumeration?
		)
	;	uu_type(list(type))	% not yet implemented!
	;	eqv_type(type)
	;	abstract_type.

:- type cons_tag_values
	== map(cons_id, cons_tag).

:- type cons_tag
	--->	string_constant(string)
	;	float_constant(float)
	;	int_constant(int)
			% This is used for enumerations and character
			% constants as well as for int constants.
	;	pred_constant(pred_id, proc_id)
			% Higher-order pred constants
			% (We don't handle closures yet.)
	;	simple_tag(tag_bits)	
			% This is for constants or functors which only
			% require a simple (two-bit) tag.
			% For constants we store a tagged zero, for functors 
			% we store a tagged pointer to the argument vector.
	;	complicated_tag(tag_bits, int)
			% This is for functors or constants which
			% require more than just a two-bit tag.  In this case,
			% we use both a primary and a secondary tag.
			% The secondary tag is stored as the first word of
			% the argument vector.  (If it's a constant, then
			% in this case there is an argument vector of size 1
			% which just holds the secondary tag.)
	;	complicated_constant_tag(tag_bits, int).
			% This is for constants which require more than a 
			% two-bit tag.  In this case, we use both a primary
			% and a secondary tag, but this time the secondary
			% tag is stored in the rest of the main word rather
			% than in the first word of the argument vector.

:- type tag_bits
	== int.		% actually only 2 (or maybe 3) bits

:- type hlds__inst_defn
	--->	hlds__inst_defn(
			varset,
			list(inst_param),
			hlds__inst_body,
			condition,
			term__context
		).

:- type hlds__inst_body	--->	eqv_inst(inst)
			;	abstract_inst.

:- type hlds__mode_defn --->	hlds__mode_defn(varset, list(inst_param),
					hlds__mode_body,
					condition, term__context).

:- type hlds__mode_body --->	eqv_mode(mode).

:- type hlds__cons_defn	--->	hlds__cons_defn(
					% maybe add varset?
					list(type),	% arg types
					type_id,	% result type
					term__context
				).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for manipulating the module_info data structure

:- interface.

:- pred module_info_init(string, module_info).
:- mode module_info_init(in, out) is det.

:- pred module_info_name(module_info, string).
:- mode module_info_name(in, out) is det.

:- pred module_info_get_predicate_table(module_info, predicate_table).
:- mode module_info_get_predicate_table(in, out) is det.

:- pred module_info_preds(module_info, pred_table).
:- mode module_info_preds(in, out) is det.

:- pred module_info_pred_info(module_info, pred_id, pred_info).
:- mode module_info_pred_info(in, in, out) is det.

:- pred module_info_pred_proc_info(module_info, pred_id, proc_id,
					pred_info, proc_info).
:- mode module_info_pred_proc_info(in, in, in, out, out) is det.

:- pred module_info_predids(module_info, list(pred_id)).
:- mode module_info_predids(in, out) is det.

:- pred module_info_reverse_predids(module_info, module_info).
:- mode module_info_reverse_predids(in, out) is det.

:- pred module_info_get_unify_requests(module_info, unify_requests).
:- mode module_info_get_unify_requests(in, out) is det.

:- pred module_info_get_unify_pred_map(module_info, unify_pred_map).
:- mode module_info_get_unify_pred_map(in, out) is det.

:- pred module_info_types(module_info, type_table).
:- mode module_info_types(in, out) is det.

:- pred module_info_typeids(module_info, list(type_id)).
:- mode module_info_typeids(in, out) is det.

:- pred module_info_insts(module_info, inst_table).
:- mode module_info_insts(in, out) is det.

:- pred module_info_instids(module_info, list(inst_id)).
:- mode module_info_instids(in, out) is det.

:- pred module_info_modes(module_info, mode_table).
:- mode module_info_modes(in, out) is det.

:- pred module_info_modeids(module_info, list(mode_id)).
:- mode module_info_modeids(in, out) is det.

:- pred module_info_ctors(module_info, cons_table).
:- mode module_info_ctors(in, out) is det.

:- pred module_info_num_errors(module_info, int).
:- mode module_info_num_errors(in, out) is det.

:- pred module_info_num_warnings(module_info, int).
:- mode module_info_num_warnings(in, out) is det.

:- pred module_info_consids(module_info, list(cons_id)).
:- mode module_info_consids(in, out) is det.

:- pred module_info_set_name(module_info, string, module_info).
:- mode module_info_set_name(in, in, out) is det.

:- pred module_info_set_predicate_table(module_info, predicate_table,
					module_info).
:- mode module_info_set_predicate_table(in, in, out) is det.

:- pred module_info_set_preds(module_info, pred_table, module_info).
:- mode module_info_set_preds(in, in, out) is det.

:- pred module_info_set_unify_requests(module_info, unify_requests,
					module_info).
:- mode module_info_set_unify_requests(in, in, out) is det.

:- pred module_info_set_unify_pred_map(module_info, unify_pred_map,
					module_info).
:- mode module_info_set_unify_pred_map(in, in, out) is det.

:- pred module_info_set_types(module_info, type_table, module_info).
:- mode module_info_set_types(in, in, out) is det.

:- pred module_info_set_insts(module_info, inst_table, module_info).
:- mode module_info_set_insts(in, in, out) is det.

:- pred module_info_set_modes(module_info, mode_table, module_info).
:- mode module_info_set_modes(in, in, out) is det.

:- pred module_info_set_ctors(module_info, cons_table, module_info).
:- mode module_info_set_ctors(in, in, out) is det.

:- pred module_info_set_num_errors(module_info, int, module_info).
:- mode module_info_set_num_errors(in, in, out) is det.

:- pred module_info_incr_errors(module_info, module_info).
:- mode module_info_incr_errors(in, out) is det.

:- pred module_info_incr_warnings(module_info, module_info).
:- mode module_info_incr_warnings(in, out) is det.

:- pred module_info_remove_predid(module_info, pred_id, module_info).
:- mode module_info_remove_predid(in, in, out) is det.

:- pred module_info_optimize(module_info, module_info).
:- mode module_info_optimize(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

	% A predicate which creates an empty module

module_info_init(Name, module(Name, PredicateTable, Requests, UnifyPredMap,
				Types, Insts, Modes, Ctors, 0, 0)) :-
	predicate_table_init(PredicateTable),
	unify_proc__init_requests(Requests),
	map__init(UnifyPredMap),
	map__init(Types),
	inst_table_init(Insts),
	map__init(Modes),
	map__init(Ctors).

	% Various access predicates which extract different pieces
	% of info from the module_info data structure.

module_info_name(ModuleInfo, Name) :-
	ModuleInfo = module(Name, _, _, _, _, _, _, _, _, _).

module_info_get_predicate_table(ModuleInfo, PredicateTable) :-
	ModuleInfo = module(_, PredicateTable, _, _, _, _, _, _, _, _).

module_info_preds(ModuleInfo, Preds) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds).

module_info_pred_info(ModuleInfo, PredId, PredInfo) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo).

module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo).

module_info_predids(ModuleInfo, PredIds) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_predids(PredicateTable, PredIds).

module_info_reverse_predids(ModuleInfo0, ModuleInfo) :-
	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_reverse_predids(PredicateTable0, PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo).

module_info_get_unify_requests(ModuleInfo, Requests) :-
	ModuleInfo = module(_, _, Requests, _, _, _, _, _, _, _).

module_info_get_unify_pred_map(ModuleInfo, UnifyPredMap) :-
	ModuleInfo = module(_, _, _, UnifyPredMap, _, _, _, _, _, _).

module_info_types(ModuleInfo, Types) :-
	ModuleInfo = module(_, _, _, _, Types, _, _, _, _, _).

module_info_typeids(ModuleInfo, TypeIDs) :-
	ModuleInfo = module(_, _, _, _, Types, _, _, _, _, _),
	map__keys(Types, TypeIDs).

module_info_insts(ModuleInfo, Insts) :-
	ModuleInfo = module(_, _, _, _, _, Insts, _, _, _, _).

module_info_instids(ModuleInfo, InstIDs) :-
	module_info_insts(ModuleInfo, InstTable),
	inst_table_get_user_insts(InstTable, UserInstTable),
	map__keys(UserInstTable, InstIDs).

module_info_modes(ModuleInfo, Modes) :-
	ModuleInfo = module(_, _, _, _, _, _, Modes, _, _, _).

module_info_modeids(ModuleInfo, ModeIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, Modes, _, _, _),
	map__keys(Modes, ModeIDs).

module_info_ctors(ModuleInfo, Ctors) :-
	ModuleInfo = module(_, _, _, _, _, _, _, Ctors, _, _).

module_info_consids(ModuleInfo, ConsIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, _, Ctors, _, _),
	map__keys(Ctors, ConsIDs).

module_info_num_errors(ModuleInfo, NumErrors) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, NumErrors, _).

module_info_num_warnings(ModuleInfo, NumWarnings) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, NumWarnings).

	% Various predicates which modify the module_info data structure.

module_info_set_name(ModuleInfo0, Name, ModuleInfo) :-
	ModuleInfo0 = module(_, B, C, D, E, F, G, H, I, J),
	ModuleInfo = module(Name, B, C, D, E, F, G, H, I, J).

module_info_set_predicate_table(ModuleInfo0, PredicateTable, ModuleInfo) :-
	ModuleInfo0 = module(A, _, C, D, E, F, G, H, I, J),
	ModuleInfo = module(A, PredicateTable, C, D, E, F, G, H, I, J).

module_info_set_preds(ModuleInfo0, Preds, ModuleInfo) :-
	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_set_preds(PredicateTable0, Preds, PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo).

module_info_set_unify_requests(ModuleInfo0, Requests, ModuleInfo) :-
	ModuleInfo0 = module(A, B, _, D, E, F, G, H, I, J),
	ModuleInfo = module(A, B, Requests, D, E, F, G, H, I, J).

module_info_set_unify_pred_map(ModuleInfo0, UnifyPredMap, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, _, E, F, G, H, I, J),
	ModuleInfo = module(A, B, C, UnifyPredMap, E, F, G, H, I, J).

module_info_set_types(ModuleInfo0, Types, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, _, F, G, H, I, J),
	ModuleInfo = module(A, B, C, D, Types, F, G, H, I, J).

module_info_set_insts(ModuleInfo0, Insts, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, _, G, H, I, J),
	ModuleInfo = module(A, B, C, D, E, Insts, G, H, I, J).

module_info_set_modes(ModuleInfo0, Modes, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, _, H, I, J),
	ModuleInfo = module(A, B, C, D, E, F, Modes, H, I, J).

module_info_set_ctors(ModuleInfo0, Ctors, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, _, I, J),
	ModuleInfo = module(A, B, C, D, E, F, G, Ctors, I, J).

module_info_set_num_errors(ModuleInfo0, Errs, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, _, J),
	ModuleInfo = module(A, B, C, D, E, F, G, H, Errs, J).

module_info_incr_errors(ModuleInfo0, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, E, F, G, H, Errs0, J),
	Errs is Errs0 + 1,
	ModuleInfo = module(A, B, C, D, E, F, G, H, Errs, J).

module_info_incr_warnings(ModuleInfo0, ModuleInfo) :-
	ModuleInfo0 = module(A, B, C, D, Types, F, G, H, I, Warns0),
	Warns is Warns0 + 1,
	ModuleInfo = module(A, B, C, D, Types, F, G, H, I, Warns).

module_info_remove_predid(ModuleInfo0, PredId, ModuleInfo) :-
	module_info_get_predicate_table(ModuleInfo0, PredicateTable0),
	predicate_table_remove_predid(PredicateTable0, PredId,
				PredicateTable),
	module_info_set_predicate_table(ModuleInfo0, PredicateTable,
				ModuleInfo).

	% After we have finished constructing the symbol tables,
	% we balance all the binary trees, to improve performance
	% in later stages of the compiler.

module_info_optimize(ModuleInfo0, ModuleInfo) :-

	module_info_get_predicate_table(ModuleInfo0, Preds0),
	predicate_table_optimize(Preds0, Preds),
	module_info_set_predicate_table(ModuleInfo0, Preds, ModuleInfo2),

	module_info_types(ModuleInfo2, Types0),
	map__optimize(Types0, Types),
	module_info_set_types(ModuleInfo2, Types, ModuleInfo3),

	module_info_insts(ModuleInfo3, InstTable0),
	inst_table_get_user_insts(InstTable0, Insts0),
	map__optimize(Insts0, Insts),
	inst_table_set_user_insts(InstTable0, Insts, InstTable),
	module_info_set_insts(ModuleInfo3, InstTable, ModuleInfo4),

	module_info_modes(ModuleInfo4, Modes0),
	map__optimize(Modes0, Modes),
	module_info_set_modes(ModuleInfo4, Modes, ModuleInfo5),

	module_info_ctors(ModuleInfo5, Ctors0),
	map__optimize(Ctors0, Ctors),
	module_info_set_ctors(ModuleInfo5, Ctors, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

	% Various predicates for accessing the predicate_table type

	% Initialize the predicate table

:- pred predicate_table_init(predicate_table).
:- mode predicate_table_init(out) is det.

	% Balance all the binary trees in the predicate table

:- pred predicate_table_optimize(predicate_table, predicate_table).
:- mode predicate_table_optimize(in, out) is det.

	% Get the pred_id->pred_info map.

:- pred predicate_table_get_preds(predicate_table, pred_table).
:- mode predicate_table_get_preds(in, out) is det.

	% Set the pred_id->pred_info map.
	% NB You shouldn't modify the keys in this table, only
	% the values.  If you want to add or delete pred_ids,
	% use predicate_table_insert and predicate_table_remove_predid.

:- pred predicate_table_set_preds(predicate_table, pred_table, predicate_table).
:- mode predicate_table_set_preds(in, in, out) is det.

	% Get a list of all the valid predids in the predicate_table.

:- pred predicate_table_get_predids(predicate_table, list(pred_id)).
:- mode predicate_table_get_predids(in, out) is det.

	% Remove a pred_id from the valid list.

:- pred predicate_table_remove_predid(predicate_table, pred_id,
					predicate_table).
:- mode predicate_table_remove_predid(in, in, out) is det.

	% Search the table for predicates matching this
	% (possibly module-qualified) sym_name & arity.

:- pred predicate_table_search_sym_arity(predicate_table, sym_name, arity,
					list(pred_id)).
:- mode predicate_table_search_sym_arity(in, in, in, out) is semidet.

	% Search the table for predicates matching this name.

:- pred predicate_table_search_name(predicate_table, string, list(pred_id)).
:- mode predicate_table_search_name(in, in, out) is semidet.

	% Search the table for predicates matching this name & arity.

:- pred predicate_table_search_name_arity(predicate_table, string, arity,
						list(pred_id)).
:- mode predicate_table_search_name_arity(in, in, in, out) is semidet.

	% Search the table for THE predicate matching this module,
	% name, and arity.  (We currently don't allow overloading
	% of predicates with the same name/arity in the same module.)
	% m_n_a is short for module, name, arity.
	
:- pred predicate_table_search_m_n_a(predicate_table, module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_m_n_a(in, in, in, in, out) is semidet.

	% Insert a new pred_info structure into the predicate_table
	% and assign it a new pred_id.  You should check beforehand
	% that the pred doesn't already occur in the table.

:- pred predicate_table_insert(predicate_table, pred_info, pred_id,
				predicate_table).
:- mode predicate_table_insert(in, in, out, out) is det.

	% Return an invalid pred_id.  Used to initialize the pred_id
	% in call(...) goals before we do typechecking or when type-checking
	% finds that there was no predicate which matched the call.

:- pred invalid_pred_id(pred_id).
:- mode invalid_pred_id(out) is det.
:- mode invalid_pred_id(in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module random.
	% we allocate pred_ids randomly, not sequentially, so
	% that the tree remains reasonably balanced.

:- type predicate_table --->
	predicate_table(
		pred_table,		% map from pred_id to pred_info
		random__supply,		% used to compute the next pred_id
		list(pred_id),		% the keys of the pred_table -
					% cached here for efficiency
		pred_name_index,	% map from pred name to pred_id
		pred_name_arity_index,	% map from pred name & arity to pred_id
		pred_module_name_arity_index
					% map from pred module, name & arity 
					% to pred_id
	).

:- type pred_id == int.

:- type pred_name_index	== map(string, list(pred_id)).
:- type pred_name_arity_index == map(name_arity, list(pred_id)).
:- type name_arity ---> string / arity.
:- type pred_module_name_arity_index == map(module_name_arity, list(pred_id)).
:- type module_name_arity ---> module_name_arity(module_name, string, arity).

predicate_table_init(PredicateTable) :-
	PredicateTable = predicate_table(Preds, NextPredId, PredIds,
				N_Index, NA_Index, MNA_Index),
	map__init(Preds),
	random__init(0, NextPredId),
	PredIds = [],
	map__init(N_Index),
	map__init(NA_Index),
	map__init(MNA_Index).

predicate_table_optimize(PredicateTable0, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, C,
				N_Index0, NA_Index0, MNA_Index0),
	map__optimize(N_Index0, N_Index),
	map__optimize(NA_Index0, NA_Index),
	map__optimize(MNA_Index0, MNA_Index),
	PredicateTable = predicate_table(A, B, C,
				N_Index, NA_Index, MNA_Index).

predicate_table_get_preds(PredicateTable, Preds) :-
	PredicateTable = predicate_table(Preds, _, _, _, _, _).

predicate_table_set_preds(PredicateTable0, Preds, PredicateTable) :-
	PredicateTable0 = predicate_table(_, B, C, D, E, F),
	PredicateTable = predicate_table(Preds, B, C, D, E, F).

predicate_table_get_predids(PredicateTable, PredIds) :-
	PredicateTable = predicate_table(_, _, PredIds, _, _, _).

predicate_table_remove_predid(PredicateTable0, PredId, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, PredIds0, D, E, F),
	list__delete_all(PredIds0, PredId, PredIds),
	PredicateTable = predicate_table(A, B, PredIds, D, E, F).

:- pred predicate_table_reverse_predids(predicate_table, predicate_table).
:- mode predicate_table_reverse_predids(in, out) is det.

predicate_table_reverse_predids(PredicateTable0, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, PredIds0, D, E, F),
	list__reverse(PredIds0, PredIds),
	PredicateTable = predicate_table(A, B, PredIds, D, E, F).

:- predicate_table_search_sym_arity(_, X, _, _) when X. % NU-Prolog indexing.

predicate_table_search_sym_arity(PredicateTable, qualified(Module, Name),
		Arity, PredIdList) :-
	predicate_table_search_m_n_a(PredicateTable, Module, Name, Arity,
		PredIdList).
predicate_table_search_sym_arity(PredicateTable, unqualified(Name),
		Arity, PredIdList) :-
	predicate_table_search_name_arity(PredicateTable, Name, Arity,
		PredIdList).

predicate_table_search_name(PredicateTable, PredName, PredId) :-
	PredicateTable = predicate_table(_, _, _, PredNameIndex, _, _),
	map__search(PredNameIndex, PredName, PredId).

predicate_table_search_name_arity(PredicateTable, PredName, Arity, PredId) :-
	PredicateTable = predicate_table(_, _, _, _, PredNameArityIndex, _),
	map__search(PredNameArityIndex, PredName / Arity, PredId).

predicate_table_search_m_n_a(PredicateTable, Module, PredName, Arity, PredIds)
		:-
	PredicateTable = predicate_table(_, _, _, _, _, MNA_Index),
	MNA = module_name_arity(Module, PredName, Arity),
	map__search(MNA_Index, MNA, PredIds).

predicate_table_insert(PredicateTable0, PredInfo, PredId, PredicateTable) :-
	PredicateTable0 = predicate_table(Preds0, NextPredId0, PredIds0,
				N_Index0, NA_Index0, MNA_Index0),
	pred_info_module(PredInfo, Module),
	pred_info_name(PredInfo, Name),
	pred_info_arity(PredInfo, Arity),

		% allocate a new pred_id
	predicate_table_next_pred_id(Preds0, NextPredId0, PredId, NextPredId),

		% insert the pred_id into the name index
	( map__search(N_Index0, Name, N_PredIdList0) ->
		N_PredIdList = [PredId | N_PredIdList0]
	;
		N_PredIdList = [PredId]
	),
	map__set(N_Index0, Name, N_PredIdList, N_Index),

		% insert it into the name/arity index
	NA = Name / Arity,
	( map__search(NA_Index0, NA, NA_PredIdList0) ->
		NA_PredIdList = [PredId | NA_PredIdList0]
	;
		NA_PredIdList = [PredId]
	),
	map__set(NA_Index0, NA, NA_PredIdList, NA_Index),

		% insert it into the module:name/arity index
	MNA = module_name_arity(Module, Name, Arity),
	( map__search(MNA_Index0, MNA, MNA_PredIdList0) ->
		MNA_PredIdList = [PredId | MNA_PredIdList0]
	;
		MNA_PredIdList = [PredId]
	),
	map__set(MNA_Index0, MNA, MNA_PredIdList, MNA_Index),

		% insert it into the pred_id list
	PredIds = [PredId | PredIds0],

		% save the pred_info for this pred_id
	map__set(Preds0, PredId, PredInfo, Preds),

	PredicateTable = predicate_table(Preds, NextPredId, PredIds,
				N_Index, NA_Index, MNA_Index).


:- pred predicate_table_next_pred_id(map(pred_id, pred_info), random__supply,
					pred_id, random__supply).
:- mode predicate_table_next_pred_id(in, in, out, out) is det.

	% get the next available pred id
predicate_table_next_pred_id(Preds, NextPredId0, PredId, NextPredId) :-
	random__random(PredId0, NextPredId0, NextPredId1),
	(
		\+ map__contains(Preds, PredId0),
		\+ invalid_pred_id(PredId0)
	->
		PredId = PredId0,
		NextPredId = NextPredId1
	;
		predicate_table_next_pred_id(Preds, NextPredId1,
						PredId, NextPredId)
	).

invalid_pred_id(-1).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the cons_id type.

:- interface.

:- pred make_functor_cons_id(const, arity, cons_id).
:- mode make_functor_cons_id(in, in, out) is det.

:- pred cons_id_get_name(cons_id, const).
:- mode cons_id_get_name(in, out) is det.

:- pred make_cons_id(sym_name, list(type), type_id, cons_id).
:- mode make_cons_id(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

make_functor_cons_id(term__atom(Name), Arity, cons(Name, Arity)).
make_functor_cons_id(term__integer(Int), _, int_const(Int)).
make_functor_cons_id(term__string(String), _, string_const(String)).
make_functor_cons_id(term__float(Float), _, float_const(Float)).

cons_id_get_name(cons(Name, _Arity), term__atom(Name)).
cons_id_get_name(int_const(Int), term__integer(Int)).
cons_id_get_name(string_const(String), term__string(String)).
cons_id_get_name(float_const(Float), term__float(Float)).

make_cons_id(qualified(_Module, Name), Args, _TypeId, cons(Name, Arity)) :-
	list__length(Args, Arity).
make_cons_id(unqualified(Name), Args, _TypeId, cons(Name, Arity)) :-
	list__length(Args, Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the information stored in the
	% pred_id and pred_info data structures.

:- interface.

:- type import_status
	--->	imported	% defined in the interface of some other module
				% or `external' (in some other language)
	;	exported	% defined in the interface of this module
	;	local.		% defined in the implementation of this module

:- pred predicate_module(module_info, pred_id, module_name).
:- mode predicate_module(in, in, out) is det.

:- pred predicate_name(module_info, pred_id, string).
:- mode predicate_name(in, in, out) is det.

:- pred predicate_arity(module_info, pred_id, arity).
:- mode predicate_arity(in, in, out) is det.

:- pred pred_info_init(module_name, sym_name, arity, tvarset, list(type),
			condition, term__context, clauses_info, import_status,
			pred_info).
:- mode pred_info_init(in, in, in, in, in, in, in, in, in, out) is det.

:- pred pred_info_module(pred_info, module_name).
:- mode pred_info_module(in, out) is det.

:- pred pred_info_name(pred_info, string).
:- mode pred_info_name(in, out) is det.

:- pred pred_info_arity(pred_info, arity).
:- mode pred_info_arity(in, out) is det.

:- pred pred_info_proc_ids(pred_info, list(proc_id)).
:- mode pred_info_proc_ids(in, out) is det.

:- pred pred_info_arg_types(pred_info, tvarset, list(type)).
:- mode pred_info_arg_types(in, out, out) is det.

:- pred pred_info_set_arg_types(pred_info, tvarset, list(type), pred_info).
:- mode pred_info_set_arg_types(in, in, in, out) is det.

:- pred pred_info_clauses_info(pred_info, clauses_info).
:- mode pred_info_clauses_info(in, out) is det.

:- pred pred_info_set_clauses_info(pred_info, clauses_info, pred_info).
:- mode pred_info_set_clauses_info(in, in, out) is det.

:- pred pred_info_procedures(pred_info, proc_table).
:- mode pred_info_procedures(in, out) is det.

:- pred pred_info_set_procedures(pred_info, proc_table, pred_info).
:- mode pred_info_set_procedures(in, in, out) is det.

:- pred pred_info_procids(pred_info, list(proc_id)).
:- mode pred_info_procids(in, out) is det.

:- pred pred_info_context(pred_info, term__context).
:- mode pred_info_context(in, out) is det.

:- pred pred_info_is_imported(pred_info::in) is semidet.

:- pred pred_info_is_exported(pred_info::in) is semidet.

:- pred pred_info_mark_as_external(pred_info::in, pred_info::out) is det.

:- pred pred_info_typevarset(pred_info, tvarset).
:- mode pred_info_typevarset(in, out) is det.

:- pred pred_info_set_typevarset(pred_info, tvarset, pred_info).
:- mode pred_info_set_typevarset(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

predicate_module(ModuleInfo, PredId, Module) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_module(PredInfo, Module).

predicate_name(ModuleInfo, PredId, PredName) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_name(PredInfo, PredName).

predicate_arity(ModuleInfo, PredId, Arity) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_arity(PredInfo, Arity).

	% The symbol table for predicates.

:- type pred_info	
	--->	predicate(
			tvarset,	% names of type vars
					% in the predicate's type decl
			list(type),	% argument types
			condition,	% formal specification
					% (not used)

			clauses_info,

			proc_table,

			term__context,	% the location (line #)
					% of the :- pred decl.

			module_name,	% module in which pred occurs
			string,		% predicate name
			arity,		% the arity of the pred
			import_status,
			tvarset		% names of type vars
					% in the predicate's type decl
					% or in the variable type assignments
		).

pred_info_init(ModuleName, SymName, Arity, TypeVarSet, Types, Cond, Context,
		ClausesInfo, Status, PredInfo) :-
	map__init(Procs),
	unqualify_name(SymName, PredName),
	sym_name_get_module_name(SymName, ModuleName, PredModuleName),
	PredInfo = predicate(TypeVarSet, Types, Cond, ClausesInfo, Procs,
		Context, PredModuleName, PredName, Arity, Status, TypeVarSet).

pred_info_proc_ids(PredInfo, ProcIds) :-
	PredInfo = predicate(_, _, _, _, Procs, _, _, _, _, _, _),
	map__keys(Procs, ProcIds).

pred_info_clauses_info(PredInfo, Clauses) :-
	PredInfo = predicate(_, _, _, Clauses, _, _, _, _, _, _, _).

pred_info_set_clauses_info(PredInfo0, Clauses, PredInfo) :-
	PredInfo0 = predicate(A, B, C,  _, E, F, G, H, I, J, K),
	PredInfo = predicate(A, B, C, Clauses, E, F, G, H, I, J, K).

pred_info_arg_types(PredInfo, TypeVars, ArgTypes) :-
	PredInfo = predicate(TypeVars, ArgTypes, _, _, _, _, _, _, _, _, _).

pred_info_set_arg_types(PredInfo0, TypeVarSet, ArgTypes, PredInfo) :-
	PredInfo0 = predicate(_, _, C,  D, E, F, G, H, I, J, K),
	PredInfo = predicate(TypeVarSet, ArgTypes, C, D, E, F, G, H, I, J, K).

pred_info_procedures(PredInfo, Procs) :-
	PredInfo = predicate(_, _, _, _, Procs, _, _, _, _, _, _).

pred_info_set_procedures(PredInfo0, Procedures, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, _, F, G, H, I, J, K),
	PredInfo = predicate(A, B, C, D, Procedures, F, G, H, I, J, K).

pred_info_procids(PredInfo, ProcIds) :-
	pred_info_procedures(PredInfo, Procedures),
	map__keys(Procedures, ProcIds).

pred_info_context(PredInfo, Context) :-
	PredInfo = predicate(_, _, _, _, _, Context, _, _, _, _, _).

pred_info_module(PredInfo, Module) :-
	PredInfo = predicate(_, _, _, _, _, _, Module, _, _, _, _).

pred_info_name(PredInfo, PredName) :-
	PredInfo = predicate(_, _, _, _, _, _, _, PredName, _, _, _).

pred_info_arity(PredInfo, Arity) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, Arity, _, _).

pred_info_is_imported(PredInfo) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, imported, _).

pred_info_is_exported(PredInfo) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, exported, _).

pred_info_mark_as_external(PredInfo0, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, _, K),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, imported, K).
	
pred_info_typevarset(PredInfo, TypeVarSet) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, TypeVarSet).

pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, _),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, TypeVarSet).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the proc_info data structure.

:- interface.

:- pred proc_info_init(list(mode), determinism, term__context, proc_info).
:- mode proc_info_init(in, in, in, out) is det.

:- pred determinism_to_category(determinism, category).
:- mode determinism_to_category(in, out) is det.

:- pred proc_info_declared_determinism(proc_info, determinism).
:- mode proc_info_declared_determinism(in, out) is det.

:- pred proc_info_interface_determinism(proc_info, category).
:- mode proc_info_interface_determinism(in, out) is det.

:- pred proc_info_inferred_determinism(proc_info, category).
:- mode proc_info_inferred_determinism(in, out) is det.

:- pred proc_info_variables(proc_info, varset).
:- mode proc_info_variables(in, out) is det.

:- pred proc_info_set_varset(proc_info, varset, proc_info).
:- mode proc_info_set_varset(in, in, out) is det.

:- pred proc_info_set_variables(proc_info, varset, proc_info).
:- mode proc_info_set_variables(in, in, out) is det.

:- pred proc_info_vartypes(proc_info, map(var, type)).
:- mode proc_info_vartypes(in, out) is det.

:- pred proc_info_set_vartypes(proc_info, map(var, type), proc_info).
:- mode proc_info_set_vartypes(in, in, out) is det.

:- pred proc_info_headvars(proc_info, list(var)).
:- mode proc_info_headvars(in, out) is det.

:- pred proc_info_set_headvars(proc_info, list(var), proc_info).
:- mode proc_info_set_headvars(in, in, out) is det.

:- pred proc_info_argmodes(proc_info, list(mode)).
:- mode proc_info_argmodes(in, out) is det.

:- pred proc_info_set_argmodes(proc_info, list(mode), proc_info).
:- mode proc_info_set_argmodes(in, in, out) is det.

:- pred proc_info_goal(proc_info, hlds__goal).
:- mode proc_info_goal(in, out) is det.

:- pred proc_info_context(proc_info, term__context).
:- mode proc_info_context(in, out) is det.

:- pred proc_info_call_info(proc_info, call_info).
:- mode proc_info_call_info(in, out) is det.

:- pred proc_info_liveness_info(proc_info, liveness_info).
:- mode proc_info_liveness_info(in, out) is det.

:- pred proc_info_follow_vars(proc_info, follow_vars).
:- mode proc_info_follow_vars(in, out) is det.

:- pred proc_info_set_body(proc_info, varset, map(var, type), list(var),
				hlds__goal, proc_info).
:- mode proc_info_set_body(in, in, in, in, in, out) is det.

:- pred proc_info_set_inferred_determinism(proc_info, category, proc_info).
:- mode proc_info_set_inferred_determinism(in, in, out) is det.

:- pred proc_info_set_goal(proc_info, hlds__goal, proc_info).
:- mode proc_info_set_goal(in, in, out) is det.

:- pred proc_info_arg_info(proc_info, list(arg_info)).
:- mode proc_info_arg_info(in, out) is det.

:- pred proc_info_set_arg_info(proc_info, list(arg_info), proc_info).
:- mode proc_info_set_arg_info(in, in, out) is det.

:- pred proc_info_get_initial_instmap(proc_info, module_info, instmap).
:- mode proc_info_get_initial_instmap(in, in, out) is det.

:- pred proc_info_set_liveness_info(proc_info, liveness_info, proc_info).
:- mode proc_info_set_liveness_info(in, in, out) is det.

:- pred proc_info_set_follow_vars(proc_info, follow_vars, proc_info).
:- mode proc_info_set_follow_vars(in, in, out) is det.

:- pred proc_info_set_call_info(proc_info, call_info, proc_info).
:- mode proc_info_set_call_info(in, in, out) is det.

:- implementation.

	% Some parts of the procedure aren't known yet.  We initialize
	% them to any old garbage which we will later throw away

proc_info_init(Modes, Det, MContext, NewProc) :-
	map__init(BodyTypes),
	goal_info_init(GoalInfo),
	varset__init(BodyVarSet),
	HeadVars = [],
	determinism_to_category(Det, Category),
	map__init(CallInfo),
	set__init(Liveness),
	ArgInfo = [],
	ClauseBody = conj([]) - GoalInfo,
	map__init(FollowVars),
	NewProc = procedure(
		Det, BodyVarSet, BodyTypes, HeadVars, Modes,
		ClauseBody, MContext, CallInfo, Category, ArgInfo,
		Liveness, FollowVars
	).

	% This predicate (and the types it operates on) are
	% misnamed.  It should be category_to_determinism.

determinism_to_category(det, deterministic).
determinism_to_category(semidet, semideterministic).
determinism_to_category(nondet, nondeterministic).
determinism_to_category(erroneous, deterministic).
determinism_to_category(failure, semideterministic).
	% If the source code doesn't specify any determinism annotation,
	% inferred determinism gets initialized to `deterministic'.
	% This is what `det_analysis.nl' wants.  If it turns out
	% that the procedure wasn't deterministic, then det_analysis.nl
	% provide the correct inferred determinism for it.
determinism_to_category(unspecified, deterministic).

proc_info_interface_determinism(ProcInfo, Category) :-
	proc_info_declared_determinism(ProcInfo, Determinism),
	( Determinism = unspecified ->
		proc_info_inferred_determinism(ProcInfo, Category)
	;
		determinism_to_category(Determinism, Category)
	).

proc_info_declared_determinism(ProcInfo, Determinism) :-
	ProcInfo = procedure(Determinism, _, _, _, _, _, _, _, _, _, _, _).
proc_info_variables(ProcInfo, VarSet) :-
	ProcInfo = procedure(_, VarSet, _, _, _, _, _, _, _, _, _, _).
proc_info_vartypes(ProcInfo, VarTypes) :-
	ProcInfo = procedure(_, _, VarTypes, _, _, _, _, _, _, _, _, _).
proc_info_headvars(ProcInfo, HeadVars) :-
	ProcInfo = procedure(_, _, _, HeadVars, _, _, _, _, _, _, _, _).
proc_info_argmodes(ProcInfo, ModeInfo) :-
	ProcInfo = procedure(_, _, _, _, ModeInfo, _, _, _, _, _, _, _).
proc_info_goal(ProcInfo, Goal) :-
	ProcInfo = procedure(_, _, _, _, _, Goal, _, _, _, _, _, _).
proc_info_context(ProcInfo, Context) :-
	ProcInfo = procedure(_, _, _, _, _, _, Context, _, _, _, _, _).
proc_info_call_info(ProcInfo, CallInfo) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, CallInfo, _, _, _, _).
proc_info_inferred_determinism(ProcInfo, Category) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, Category, _, _, _).
proc_info_arg_info(ProcInfo, ArgInfo) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, ArgInfo, _, _).
proc_info_liveness_info(ProcInfo, Liveness) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, Liveness, _).
proc_info_follow_vars(ProcInfo, Follow) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, Follow).

proc_info_set_body(ProcInfo0, VarSet, VarTypes, HeadVars, Goal, ProcInfo) :-
	ProcInfo0 = procedure(DeclaredDet, _, _, _, ArgModes, _, Context,
		CallInfo, InferredDet, ArgInfo, Liveness, Follow),
	ProcInfo = procedure(DeclaredDet, VarSet, VarTypes, HeadVars, ArgModes,
		Goal, Context, CallInfo, InferredDet, ArgInfo, Liveness,
		Follow).

proc_info_set_varset(ProcInfo0, VarSet, ProcInfo) :-
	ProcInfo0 = procedure(A, _, C, D, E, F, G, H, I, J, K, L),
	ProcInfo = procedure(A, VarSet, C, D, E, F, G, H, I, J, K, L).

proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, _, E, F, G, H, I, J, K, L),
	ProcInfo = procedure(A, B, C, HeadVars, E, F, G, H, I, J, K, L).

proc_info_set_argmodes(ProcInfo0, ArgModes, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, _, F, G, H, I, J, K, L),
	ProcInfo = procedure(A, B, C, D, ArgModes, F, G, H, I, J, K, L).

proc_info_set_inferred_determinism(ProcInfo0, Category, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, _, J, K, L),
	ProcInfo = procedure(A, B, C, D, E, F, G, H, Category, J, K, L).

proc_info_set_goal(ProcInfo0, Goal, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, _, G, H, I, J, K, L),
	ProcInfo = procedure(A, B, C, D, E, Goal, G, H, I, J, K, L).

proc_info_set_call_info(ProcInfo0, CallInfo, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, _, I, J, K, L),
	ProcInfo = procedure(A, B, C, D, E, F, G, CallInfo, I, J, K, L).

proc_info_set_arg_info(ProcInfo0, ArgInfo, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, _, K, L),
	ProcInfo = procedure(A, B, C, D, E, F, G, H, I, ArgInfo, K, L).

proc_info_set_liveness_info(ProcInfo0, K, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, _, L),
	ProcInfo = procedure(A, B, C, D, E, F, G, H, I, J, K, L).

proc_info_set_follow_vars(ProcInfo0, L, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, _),
	ProcInfo = procedure(A, B, C, D, E, F, G, H, I, J, K, L).

proc_info_get_initial_instmap(ProcInfo, ModuleInfo, reachable(InstMapping)) :-
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_argmodes(ProcInfo, ArgModes),
	mode_list_get_initial_insts(ArgModes, ModuleInfo, InitialInsts),
/***********
		% propagate type information into the modes
	proc_info_vartypes(ProcInfo, VarTypes),
	propagate_type_info_inst_list(VarTypes, ModuleInfo, InitialInsts0,
					InitialInsts),
***********/
	map__from_corresponding_lists(HeadVars, InitialInsts, InstMapping).

proc_info_set_variables(ProcInfo0, Vars, ProcInfo) :-
	ProcInfo0 = procedure(A, _, C, D, E, F, G, H, I, J, K, L),
	ProcInfo = procedure(A, Vars, C, D, E, F, G, H, I, J, K, L).

proc_info_set_vartypes(ProcInfo0, Vars, ProcInfo) :-
	ProcInfo0 = procedure(A, B, _, D, E, F, G, H, I, J, K, L),
	ProcInfo = procedure(A, B, Vars, D, E, F, G, H, I, J, K, L).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Access predicates for the hlds__goal_info data structure.

:- interface.

:- pred goal_info_init(hlds__goal_info).
:- mode goal_info_init(out) is det.

% Instead of recording the liveness of every variable at every
% part of the goal, we just keep track of the initial liveness
% and the changes in liveness.

:- type delta_liveness == pair(set(var)).
			% Births - Deaths.

:- pred goal_info_pre_delta_liveness(hlds__goal_info, delta_liveness).
:- mode goal_info_pre_delta_liveness(in, out) is det.

:- pred goal_info_set_pre_delta_liveness(hlds__goal_info, delta_liveness,
					hlds__goal_info).
:- mode goal_info_set_pre_delta_liveness(in, in, out) is det.

:- pred goal_info_post_delta_liveness(hlds__goal_info, delta_liveness).
:- mode goal_info_post_delta_liveness(in, out) is det.

:- pred goal_info_set_post_delta_liveness(hlds__goal_info, delta_liveness,
					hlds__goal_info).
:- mode goal_info_set_post_delta_liveness(in, in, out) is det.

:- pred goal_info_determinism(hlds__goal_info, category).
:- mode goal_info_determinism(in, out) is det.

:- pred goal_info_set_determinism(hlds__goal_info, category, hlds__goal_info).
:- mode goal_info_set_determinism(in, in, out) is det.

	% The `internal' determinism is the determinism _before_ we
	% prune nondet goals that don't have any output variables.

:- pred goal_info_get_internal_determinism(hlds__goal_info, category).
:- mode goal_info_get_internal_determinism(in, out) is det.

:- pred goal_info_set_internal_determinism(hlds__goal_info, category,
					hlds__goal_info).
:- mode goal_info_set_internal_determinism(in, in, out) is det.

:- pred goal_info_get_nonlocals(hlds__goal_info, set(var)).
:- mode goal_info_get_nonlocals(in, out) is det.

:- pred goal_info_set_nonlocals(hlds__goal_info, set(var), hlds__goal_info).
:- mode goal_info_set_nonlocals(in, in, out) is det.

	% The instmap delta stores the final instantiatedness
	% of the non-local variables whose instantiatedness
	% changed.

:- type instmap_delta == instmap.

:- type instmap
	--->	reachable(instmapping)
	;	unreachable.

:- type instmapping == map(var, inst).

:- pred goal_info_get_instmap_delta(hlds__goal_info, instmap_delta).
:- mode goal_info_get_instmap_delta(in, out) is det.

:- pred goal_info_set_instmap_delta(hlds__goal_info, instmap_delta,
				hlds__goal_info).
:- mode goal_info_set_instmap_delta(in, in, out) is det.

:- pred goal_info_context(hlds__goal_info, term__context).
:- mode goal_info_context(in, out) is det.

:- pred goal_info_set_context(hlds__goal_info, term__context, hlds__goal_info).
:- mode goal_info_set_context(in, in, out) is det.

:- pred goal_info_store_map(hlds__goal_info, maybe(map(var, lval))).
:- mode goal_info_store_map(in, out) is det.

:- pred goal_info_set_store_map(hlds__goal_info,
				maybe(map(var, lval)), hlds__goal_info).
:- mode goal_info_set_store_map(in, in, out) is det.

:- pred goal_to_conj_list(hlds__goal, list(hlds__goal)).
:- mode goal_to_conj_list(in, out) is det.

:- pred goal_to_disj_list(hlds__goal, list(hlds__goal)).
:- mode goal_to_disj_list(in, out) is det.

:- pred conj_list_to_goal(list(hlds__goal), hlds__goal_info, hlds__goal).
:- mode conj_list_to_goal(in, in, out) is det.

:- pred disj_list_to_goal(list(hlds__goal), hlds__goal_info, hlds__goal).
:- mode disj_list_to_goal(in, in, out) is det.

	% A goal is atomic iff it doesn't contain any sub-goals.

:- pred goal_is_atomic(hlds__goal_expr).
:- mode goal_is_atomic(in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

goal_info_init(GoalInfo) :-
	LocalDet = deterministic,
	Det = nondeterministic,
	set__init(Births),
	set__init(Deaths),
	DeltaLiveness = Births - Deaths,
	InstMapDelta = unreachable,
	set__init(NonLocals),
	term__context_init("", 0, Context),
	GoalInfo = goal_info(DeltaLiveness, LocalDet, Det,
			InstMapDelta, Context, NonLocals, DeltaLiveness, no).

goal_info_pre_delta_liveness(GoalInfo, DeltaLiveness) :-
	GoalInfo = goal_info(DeltaLiveness, _, _, _, _, _, _, _).

goal_info_set_pre_delta_liveness(GoalInfo0, DeltaLiveness, GoalInfo) :-
	GoalInfo0 = goal_info(_, B, C, D, E, F, G, H),
	GoalInfo = goal_info(DeltaLiveness, B, C, D, E, F, G, H).

goal_info_post_delta_liveness(GoalInfo, DeltaLiveness) :-
	GoalInfo = goal_info(_, _, _, _, _, _, DeltaLiveness, _).

goal_info_set_post_delta_liveness(GoalInfo0, DeltaLiveness, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, _, H),
	GoalInfo = goal_info(A, B, C, D, E, F, DeltaLiveness, H).

goal_info_get_internal_determinism(GoalInfo, LocalDeterminism) :-
	GoalInfo = goal_info(_, LocalDeterminism, _, _, _, _, _, _).

goal_info_set_internal_determinism(GoalInfo0, LocalDeterminism, GoalInfo) :-
	GoalInfo0 = goal_info(A, _, C, D, E, F, G, H),
	GoalInfo = goal_info(A, LocalDeterminism, C, D, E, F, G, H).

goal_info_determinism(GoalInfo, Determinism) :-
	GoalInfo = goal_info(_, _, Determinism, _, _, _, _, _).

goal_info_set_determinism(GoalInfo0, Determinism, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, _, D, E, F, G, H),
	GoalInfo = goal_info(A, B, Determinism, D, E, F, G, H).

goal_info_get_instmap_delta(GoalInfo, InstMapDelta) :-
	GoalInfo = goal_info(_, _, _, InstMapDelta, _, _, _, _).

goal_info_set_instmap_delta(GoalInfo0, InstMapDelta, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, _, E, F, G, H),
	GoalInfo = goal_info(A, B, C, InstMapDelta, E, F, G, H).

goal_info_context(GoalInfo, Context) :-
	GoalInfo = goal_info(_, _, _, _, Context, _, _, _).

goal_info_set_context(GoalInfo0, Context, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, _, F, G, H),
	GoalInfo = goal_info(A, B, C, D, Context, F, G, H).

goal_info_get_nonlocals(GoalInfo, NonLocals) :-
	GoalInfo = goal_info(_, _, _, _, _, NonLocals, _, _).

goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, _, G, H),
	GoalInfo  = goal_info(A, B, C, D, E, NonLocals, G, H).

goal_info_store_map(GoalInfo, H) :-
	GoalInfo = goal_info(_, _, _, _, _, _, _, H).

goal_info_set_store_map(GoalInfo0, H, GoalInfo) :-
	GoalInfo0 = goal_info(A, B, C, D, E, F, G, _),
	GoalInfo  = goal_info(A, B, C, D, E, F, G, H).

%-----------------------------------------------------------------------------%

	% Convert a goal to a list of conjuncts.
	% If the goal is a conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.

goal_to_conj_list(Goal, ConjList) :-
	( Goal = (conj(List) - _) ->
		ConjList = List
	;
		ConjList = [Goal]
	).

	% Convert a goal to a list of disjuncts.
	% If the goal is a conjunction, then return its conjuncts,
	% otherwise return the goal as a singleton list.

goal_to_disj_list(Goal, DisjList) :-
	( Goal = (disj(List) - _) ->
		DisjList = List
	;
		DisjList = [Goal]
	).

conj_list_to_goal(ConjList, GoalInfo, Goal) :-
	( ConjList = [Goal0] ->
		Goal = Goal0
	;
		Goal = conj(ConjList) - GoalInfo
	).

disj_list_to_goal(ConjList, GoalInfo, Goal) :-
	( ConjList = [Goal0] ->
		Goal = Goal0
	;
		Goal = disj(ConjList) - GoalInfo
	).

%-----------------------------------------------------------------------------%

goal_is_atomic(conj([])).
goal_is_atomic(disj([])).
goal_is_atomic(call(_,_,_,_,_,_)).
goal_is_atomic(unify(_,_,_,_,_)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
