%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: polymorphism.m
% main author: fjh

% This module is a pass over the HLDS.
% It does a syntactic transformation to implement polymorphism, including
% typeclasses, by passing extra `type_info' and `typeclass_info' arguments.
% These arguments are structures that contain, amoung other things,
% higher-order predicate terms for the polymorphic procedures or methods.

% See notes/type_class_transformation.html for a description of the
% transformation and data structures used to implement type classes.

% XXX The way the code in this module handles existential type classes
% and type class constraints is a bit ad-hoc, in general; there are
% definitely parts of this code (marked with XXXs below) that could
% do with a rewrite to make it more consistent and hence more maintainable.
%
%-----------------------------------------------------------------------------%
%
% Tranformation of polymorphic code:
%
% Every polymorphic predicate is transformed so that it takes one additional
% argument for every type variable in the predicate's type declaration.
% The argument gives information about the type, including higher-order
% predicate variables for each of the builtin polymorphic operations
% (currently unify/2, compare/3).
%
%-----------------------------------------------------------------------------%
%
% Representation of type information:
%
% IMPORTANT: ANY CHANGES TO THE DOCUMENTATION HERE MUST BE REFLECTED BY
% SIMILAR CHANGES TO THE #defines IN "runtime/mercury_type_info.h" AND
% TO THE TYPE SPECIALIZATION CODE IN "compiler/higher_order.m".
%
% Type information is represented using one or two cells. The cell which
% is always present is the type_ctor_info structure, whose structure is
% defined in runtime/mercury_type_info.h. The other cell is the type_info
% structure, laid out like this:
%
%	word 0		<pointer to the type_ctor_info structure>
%	word 1+		<the type_infos for the type params, at least one>
%
%	(but see note below for how variable arity types differ)
%
%-----------------------------------------------------------------------------%
%
% Optimization of common case (zero arity types):
%
% The type_info structure itself is redundant if the type has no type
% parameters (i.e. its arity is zero). Therefore if the arity is zero,
% we pass the address of the type_ctor_info structure directly, instead of
% wrapping it up in another cell. The runtime system will look at the first
% field of the cell it is passed. If this field is zero, the cell is a
% type_ctor_info structure for an arity zero type. If this field is not zero,
% the cell is a new type_info structure, with the first field being the
% pointer to the type_ctor_info structure.
%
%-----------------------------------------------------------------------------%
%
% Variable arity types:
%
% There is a slight variation on this for variable-arity type constructors, of
% there are exactly three: pred, func and tuple. Typeinfos of these types
% always have a pointer to the pred/0 type_ctor_info, regardless of their true
% arity, so we store the real arity in the type-info as well.
%
%	word 0		<pointer to the type_ctor_info structure (pred/0)>
%	word 1		<arity of predicate>
%	word 2+		<the type_infos for the type params, at least one>
%
%-----------------------------------------------------------------------------%
%
% Sharing type_ctor_info structures:
%
% For compilation models that can put code addresses in static ground terms,
% we can arrange to create one copy of the type_ctor_info structure statically,
% avoiding the need to create other copies at runtime. For compilation models
% that cannot put code addresses in static ground terms, there are a couple
% of things we could do:
%
% 	1. allocate all cells at runtime.
%	2. use a shared static type_ctor_info, but initialize its code
%	   addresses during startup (that is, during the module
%	   initialization code).
%
% We use option 2.
%
%-----------------------------------------------------------------------------%
%
% Example of transformation:
%
% Take the following code as an example, ignoring the requirement for
% super-homogeneous form for clarity:
%
%	:- pred p(T1).
%	:- pred q(T2).
%	:- pred r(T3).
%
%	p(X) :- q([X]), r(0).
%
% We add an extra argument for each type variable:
%
%	:- pred p(type_info(T1), T1).
%	:- pred q(type_info(T2), T2).
%	:- pred r(type_info(T3), T3).
%
% We transform the body of p to this:
%
%	p(TypeInfoT1, X) :-
%		TypeCtorInfoT2 = type_ctor_info(list/1),
%		TypeInfoT2 = type_info(
%			TypeCtorInfoT2,
%			TypeInfoT1),
%		q(TypeInfoT2, [X]),
%		TypeInfoT3 = type_ctor_info(int/0),
%		r(TypeInfoT3, 0).
%
% Note that type_ctor_infos are actually generated as references to a
% single shared type_ctor_info.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Transformation of code using existentially quantified types:
% 
% The transformation for existential types is similar to the
% transformation for universally quantified types, except
% that the type_infos and type_class_infos have mode `out'
% rather than mode `in'.
%
% The argument passing convention is that the new parameters
% introduced by this pass are placed in the following order:
%
%	First the UnivTypeInfos (for universally quantified type variables)
% 	then the ExistTypeInfos (for existentially quantified type variables)
%	then the UnivTypeClassInfos (for universally quantified constraints)
%	then the ExistTypeClassInfos (for existentially quantified constraints)
%	and finally the original arguments of the predicate.
%
% The convention for class method implementations is slightly different
% to match the order that the type_infos and typeclass_infos are passed
% in by do_call_class_method (in runtime/mercury_ho_call.c):
%
%	First the type_infos for the unconstrained type variables in
% 		the instance declaration
%	then the typeclass_infos for the class constraints on the
%		instance declaration
% 	then the remainder of the type_infos and typeclass_infos as above.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module check_hlds__polymorphism.
:- interface.

:- import_module hlds__hlds_goal, hlds__hlds_module, hlds__hlds_pred.
:- import_module parse_tree__prog_data, hlds__special_pred.

:- import_module io, list, term, map.

% Run the polymorphism pass over the whole HLDS.

:- pred polymorphism__process_module(module_info, module_info,
			io__state, io__state).
:- mode polymorphism__process_module(in, out, di, uo) is det.

% Run the polymorphism pass over a single pred.
% This is used to transform clauses introduced by unify_proc.m
% for complicated unification predicates for types
% for which unification predicates are generated lazily. 
%
% This predicate should be used with caution. polymorphism.m
% expects that the argument types of called predicates have not
% been transformed yet. This predicate will not work correctly
% after the original pass of polymorphism has been run if the
% predicate to be processed calls any polymorphic predicates
% which require type_infos or typeclass_infos to be added to
% the argument list.

:- pred polymorphism__process_generated_pred(pred_id,
		module_info, module_info).
:- mode polymorphism__process_generated_pred(in, in, out) is det.

% Add the type_info variables for a complicated unification to
% the appropriate fields in the unification and the goal_info.

:- pred polymorphism__unification_typeinfos(type, map(tvar, type_info_locn),
		unification, hlds_goal_info, unification, hlds_goal_info).
:- mode polymorphism__unification_typeinfos(in, in, in, in, out, out) is det.

% Given a list of types, create a list of variables to hold the type_info
% for those types, and create a list of goals to initialize those type_info
% variables to the appropriate type_info structures for the types.
% Update the varset and vartypes accordingly.

:- pred polymorphism__make_type_info_vars(list(type),
	term__context, list(prog_var), list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__make_type_info_vars(in, in, out, out, in, out) is det.

% Likewise, but for a single type.

:- pred polymorphism__make_type_info_var(type,
	term__context, prog_var, list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__make_type_info_var(in, in, out, out, in, out) is det.

	% polymorphism__gen_extract_type_info(TypeVar, TypeClassInfoVar, Index,
	%		ModuleInfo, Goals, TypeInfoVar, ...):
	%
	%	Generate code to extract a type_info variable from a
	%	given slot of a typeclass_info variable, by calling
	%	private_builtin:type_info_from_typeclass_info.
	%	TypeVar is the type variable to which this type_info
	%	variable corresponds.  TypeClassInfoVar is the variable
	%	holding the type_class_info.  Index specifies which
	%	slot it is.  The procedure returns TypeInfoVar, which
	%	is a fresh variable holding the type_info, and Goals,
	%	which is the code generated to initialize TypeInfoVar.
	%
:- pred polymorphism__gen_extract_type_info(tvar, prog_var, int, module_info,
		list(hlds_goal), prog_var, prog_varset, map(prog_var, type),
		prog_varset, map(prog_var, type)).
:- mode polymorphism__gen_extract_type_info(in, in, in, in, out, out,
		in, in, out, out) is det.

:- type poly_info.

	% Extract some fields from a pred_info and proc_info and use them to
	% create a poly_info, for use by the polymorphism transformation.
:- pred create_poly_info(module_info, pred_info, proc_info, poly_info).
:- mode create_poly_info(in, in, in, out) is det.

	% Update the fields in a pred_info and proc_info with
	% the values in a poly_info.
:- pred poly_info_extract(poly_info, pred_info, pred_info,
		proc_info, proc_info, module_info).
:- mode poly_info_extract(in, in, out, in, out, out) is det.

	% Build the type describing the typeclass_info for the
	% given class_constraint.
:- pred polymorphism__build_typeclass_info_type(class_constraint, (type)).
:- mode polymorphism__build_typeclass_info_type(in, out) is det.

	% From the type of a typeclass_info variable find the class_constraint
	% about which the variable carries information, failing if the
	% type is not a valid typeclass_info type.
:- pred polymorphism__typeclass_info_class_constraint((type),
		class_constraint).
:- mode polymorphism__typeclass_info_class_constraint(in, out) is semidet.

	% From the type of a type_info variable find the type about which
	% the type_info carries information, failing if the type is not a
	% valid type_info type.
:- pred polymorphism__type_info_type((type), (type)).
:- mode polymorphism__type_info_type(in, out) is semidet.

	% Construct the type of the type_info for the given type.
:- pred polymorphism__build_type_info_type((type), (type)).
:- mode polymorphism__build_type_info_type(in, out) is det.

	% Succeed if the predicate is one of the predicates defined in
	% library/private_builtin.m to extract type_infos or typeclass_infos
	% from typeclass_infos.
:- pred polymorphism__is_typeclass_info_manipulator(module_info,
		pred_id, typeclass_info_manipulator).
:- mode polymorphism__is_typeclass_info_manipulator(in, in, out) is semidet.

:- type typeclass_info_manipulator
	--->	type_info_from_typeclass_info
	;	superclass_from_typeclass_info
	;	instance_constraint_from_typeclass_info
	.

	% Look up the pred_id and proc_id for a type specific
	% unification/comparison/index predicate.
:- pred polymorphism__get_special_proc(type, special_pred_id,
		module_info, sym_name, pred_id, proc_id).
:- mode polymorphism__get_special_proc(in, in, in, out, out, out) is semidet.

	% convert a higher-order pred term to a lambda goal
:- pred convert_pred_to_lambda_goal(lambda_eval_method,
		prog_var, pred_id, proc_id, list(prog_var), list(type),
		unify_context, hlds_goal_info, context,
		module_info, prog_varset, map(prog_var, type),
		unify_rhs, prog_varset, map(prog_var, type)).
:- mode convert_pred_to_lambda_goal(in, in, in, in, in, in, in, 
		in, in, in, in, in, out, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_data, check_hlds__typecheck, ll_backend__llds.
:- import_module parse_tree__prog_io.
:- import_module check_hlds__type_util, check_hlds__mode_util.
:- import_module hlds__quantification, hlds__instmap, parse_tree__prog_out.
:- import_module ll_backend__code_util, check_hlds__unify_proc.
:- import_module parse_tree__prog_util.
:- import_module (parse_tree__inst), hlds__hlds_out.
:- import_module backend_libs__base_typeclass_info, hlds__goal_util.
:- import_module hlds__passes_aux.
:- import_module check_hlds__clause_to_proc.
:- import_module libs__globals, libs__options.

:- import_module bool, int, string, set, map.
:- import_module term, varset, std_util, require, assoc_list.

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.
	% We do two passes, the first to fix up the clauses_info and
	% proc_infos (and in fact everything except the pred_info argtypes),
	% the second to fix up the pred_info argtypes.
	% The reason we need two passes is that the first pass looks at
	% the argtypes of the called predicates, and so we need to make
	% sure we don't muck them up before we've finished the first pass.

polymorphism__process_module(ModuleInfo0, ModuleInfo, IO0, IO) :-
	module_info_preds(ModuleInfo0, Preds0),
	map__keys(Preds0, PredIds0),
	polymorphism__process_preds(PredIds0, ModuleInfo0, ModuleInfo1,
				IO0, IO),
	module_info_preds(ModuleInfo1, Preds1),
	map__keys(Preds1, PredIds1),

	polymorphism__fixup_preds(PredIds1, ModuleInfo1, ModuleInfo2),
	polymorphism__expand_class_method_bodies(ModuleInfo2, ModuleInfo).

:- pred polymorphism__process_preds(list(pred_id), module_info, module_info,
			io__state, io__state).
:- mode polymorphism__process_preds(in, in, out, di, uo) is det.

polymorphism__process_preds([], ModuleInfo, ModuleInfo) --> [].
polymorphism__process_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	polymorphism__maybe_process_pred(PredId, ModuleInfo0, ModuleInfo1),
	polymorphism__process_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred polymorphism__maybe_process_pred(pred_id, module_info, module_info,
			io__state, io__state).
:- mode polymorphism__maybe_process_pred(in, in, out, di, uo) is det.

polymorphism__maybe_process_pred(PredId, ModuleInfo0, ModuleInfo) -->
	{ module_info_pred_info(ModuleInfo0, PredId, PredInfo) },
	(
		{
			% Leave Aditi aggregates alone, since
			% calls to them must be monomorphic. This avoids
			% unnecessarily creating type_infos in Aditi code,
			% since they will just be stripped out later.
			% The input to an aggregate must be a closure holding
			% the address of an Aditi procedure. The
			% monomorphism of Aditi procedures is checked by
			% magic.m.
			% Other Aditi procedures should still be processed,
			% to handle complicated unifications.
			hlds_pred__pred_info_is_aditi_aggregate(PredInfo)
		;
			pred_info_module(PredInfo, PredModule),
			pred_info_name(PredInfo, PredName),
			pred_info_arity(PredInfo, PredArity),
			no_type_info_builtin(PredModule, PredName, PredArity) 
		}
	->
		% just copy the clauses to the proc_infos
		{ copy_module_clauses_to_procs([PredId],
			ModuleInfo0, ModuleInfo) }
	;
		polymorphism__process_pred(PredId, ModuleInfo0, ModuleInfo)
	).

%---------------------------------------------------------------------------%

:- pred polymorphism__fixup_preds(list(pred_id), module_info, module_info).
:- mode polymorphism__fixup_preds(in, in, out) is det.

polymorphism__fixup_preds(PredIds, ModuleInfo0, ModuleInfo) :-
	list__foldl(polymorphism__fixup_pred,
		PredIds, ModuleInfo0, ModuleInfo).

:- pred polymorphism__fixup_pred(pred_id, module_info, module_info).
:- mode polymorphism__fixup_pred(in, in, out) is det.

polymorphism__fixup_pred(PredId, ModuleInfo0, ModuleInfo) :-

	%
	% Recompute the arg types by finding the headvars and
	% the var->type mapping (from the clauses_info) and
	% applying the type mapping to the extra headvars to get the new
	% arg types.  Note that we are careful to only apply the mapping
	% to the extra head vars, not to the originals, because otherwise
	% we would stuff up the arg types for unification predicates for
	% equivalence types.
	%
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_clauses_info(PredInfo0, ClausesInfo0),
	clauses_info_vartypes(ClausesInfo0, VarTypes0),
	clauses_info_headvars(ClausesInfo0, HeadVars),

	pred_info_arg_types(PredInfo0, TypeVarSet, ExistQVars, ArgTypes0),
	list__length(ArgTypes0, NumOldArgs),
	list__length(HeadVars, NumNewArgs),
	NumExtraArgs is NumNewArgs - NumOldArgs,
	(
		list__split_list(NumExtraArgs, HeadVars, ExtraHeadVars0,
				OldHeadVars0)
	->
		ExtraHeadVars = ExtraHeadVars0,
		OldHeadVars = OldHeadVars0
	;
		error("polymorphism.m: list__split_list failed")
	),

	map__apply_to_list(ExtraHeadVars, VarTypes0, ExtraArgTypes),
	list__append(ExtraArgTypes, ArgTypes0, ArgTypes),
	pred_info_set_arg_types(PredInfo0, TypeVarSet, ExistQVars,
		ArgTypes, PredInfo1),

	%
	% If the clauses binds some existentially quantified
	% type variables, make sure the types of the type-infos
	% for those type variables in the variable types map
	% are as specific as possible. The predicate argument
	% types shouldn't be substituted, because the binding
	% should not be visible to calling predicates.
	%
	(
		ExistQVars \= [],
		% This can fail for unification procedures
		% of equivalence types.
		map__apply_to_list(OldHeadVars, VarTypes0, OldHeadVarTypes),
		type_list_subsumes(ArgTypes0, OldHeadVarTypes, Subn),
		\+ map__is_empty(Subn)
	->
		list__foldl(
			(pred(HeadVar::in, Types0::in, Types::out) is det :-
				map__lookup(Types0, HeadVar, HeadVarType0),
				term__apply_rec_substitution(HeadVarType0,
					Subn, HeadVarType),
				map__set(Types0, HeadVar, HeadVarType, Types)
			), ExtraHeadVars, VarTypes0, VarTypes),
		clauses_info_set_vartypes(ClausesInfo0, VarTypes, ClausesInfo),
		pred_info_set_clauses_info(PredInfo1, ClausesInfo, PredInfo2),

		% Fix up the var-types in the procedures as well.
		% It would be better if this were done before copying
		% clauses to procs, but that's difficult to arrange. 
		pred_info_procedures(PredInfo2, Procs0),
		map__map_values(
			(pred(_::in, ProcInfo0::in, ProcInfo::out) is det :- 
				proc_info_set_vartypes(ProcInfo0,
					VarTypes, ProcInfo)
			), Procs0, Procs),
		pred_info_set_procedures(PredInfo2, Procs, PredInfo)
	;				
		PredInfo = PredInfo1
	),

	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo).

%---------------------------------------------------------------------------%

:- pred polymorphism__process_pred(pred_id, module_info, module_info,
			io__state, io__state).
:- mode polymorphism__process_pred(in, in, out, di, uo) is det.

polymorphism__process_pred(PredId, ModuleInfo0, ModuleInfo) -->
	write_pred_progress_message("% Transforming polymorphism for ",
					PredId, ModuleInfo0),
	{ polymorphism__process_pred(PredId, ModuleInfo0, ModuleInfo) }.

polymorphism__process_generated_pred(PredId, ModuleInfo0, ModuleInfo) :-
	polymorphism__process_pred(PredId, ModuleInfo0, ModuleInfo1),
	polymorphism__fixup_pred(PredId, ModuleInfo1, ModuleInfo).

:- pred polymorphism__process_pred(pred_id, module_info, module_info).
:- mode polymorphism__process_pred(in, in, out) is det.

polymorphism__process_pred(PredId, ModuleInfo0, ModuleInfo) :-
	module_info_pred_info(ModuleInfo0, PredId, PredInfo0),
	%
	% run the polymorphism pass over the clauses_info,
	% updating the headvars, goals, varsets, types, etc.,
	% and computing some information in the poly_info.
	%
	pred_info_clauses_info(PredInfo0, ClausesInfo0),
	polymorphism__process_clause_info(
			ClausesInfo0, PredInfo0, ModuleInfo0,
			ClausesInfo, PolyInfo, ExtraArgModes),
	poly_info_get_module_info(PolyInfo, ModuleInfo1),
	poly_info_get_typevarset(PolyInfo, TypeVarSet),
	pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo1),
	pred_info_set_clauses_info(PredInfo1, ClausesInfo, PredInfo2),

	%
	% do a pass over the proc_infos, copying the relevant information
	% from the clauses_info and the poly_info, and updating all
	% the argmodes with modes for the extra arguments.
	%
	pred_info_procids(PredInfo2, ProcIds),
	pred_info_procedures(PredInfo2, Procs0),
	polymorphism__process_procs(ProcIds, Procs0, PredInfo2, ClausesInfo,
		ExtraArgModes, Procs),
	pred_info_set_procedures(PredInfo2, Procs, PredInfo),

	module_info_set_pred_info(ModuleInfo1, PredId, PredInfo,
		ModuleInfo).

:- pred polymorphism__process_clause_info(clauses_info, pred_info, module_info,
			clauses_info, poly_info, list(mode)).
:- mode polymorphism__process_clause_info(in, in, in, out, out, out) is det.

polymorphism__process_clause_info(ClausesInfo0, PredInfo0, ModuleInfo0,
				ClausesInfo, PolyInfo, ExtraArgModes) :-

	init_poly_info(ModuleInfo0, PredInfo0, ClausesInfo0, PolyInfo0),
	clauses_info_headvars(ClausesInfo0, HeadVars0),

	polymorphism__setup_headvars(PredInfo0, HeadVars0,
			HeadVars, ExtraArgModes, _HeadTypeVars,
			UnconstrainedTVars,
			ExtraTypeInfoHeadVars, ExistTypeClassInfoHeadVars,
			PolyInfo0, PolyInfo1),

	clauses_info_clauses(ClausesInfo0, Clauses0),
	list__map_foldl(polymorphism__process_clause(PredInfo0,
				HeadVars0, HeadVars, UnconstrainedTVars,
				ExtraTypeInfoHeadVars,
				ExistTypeClassInfoHeadVars),
			Clauses0, Clauses, PolyInfo1, PolyInfo),

	%
	% set the new values of the fields in clauses_info
	%
	poly_info_get_varset(PolyInfo, VarSet),
	poly_info_get_var_types(PolyInfo, VarTypes),
	poly_info_get_type_info_map(PolyInfo, TypeInfoMap),
	poly_info_get_typeclass_info_map(PolyInfo, TypeClassInfoMap),
	clauses_info_explicit_vartypes(ClausesInfo0, ExplicitVarTypes),
	map__init(TVarNameMap), % This is only used while adding the clauses.
	ClausesInfo = clauses_info(VarSet, ExplicitVarTypes, TVarNameMap,
				VarTypes, HeadVars, Clauses,
				TypeInfoMap, TypeClassInfoMap,
				ClausesInfo0 ^ have_foreign_clauses).

:- pred polymorphism__process_clause(pred_info, list(prog_var), list(prog_var),
		list(tvar), list(prog_var), list(prog_var),
		clause, clause,	poly_info, poly_info).
:- mode polymorphism__process_clause(in, in, in, in, in, in,
		in, out, in, out) is det.

polymorphism__process_clause(PredInfo0, OldHeadVars, NewHeadVars,
			UnconstrainedTVars,
			ExtraTypeInfoHeadVars, ExistTypeClassInfoHeadVars,
			Clause0, Clause) -->
	(
		{ pred_info_is_imported(PredInfo0) }
	->
		{ Clause = Clause0 }
	;
		{ Clause0 = clause(ProcIds, Goal0, Lang, Context) },
		%
		% process any polymorphic calls inside the goal
		%
		polymorphism__process_goal(Goal0, Goal1),

		%
		% generate code to construct the type-class-infos
		% and type-infos for existentially quantified type vars
		%
		polymorphism__produce_existq_tvars(
			PredInfo0, OldHeadVars,
			UnconstrainedTVars, ExtraTypeInfoHeadVars,
			ExistTypeClassInfoHeadVars,
			Goal1, Goal2),

		{ pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars) },
		polymorphism__fixup_quantification(NewHeadVars, ExistQVars,
			Goal2, Goal),
		{ Clause = clause(ProcIds, Goal, Lang, Context) }
	).

:- pred polymorphism__process_procs(list(proc_id), proc_table,
		pred_info, clauses_info, list(mode), proc_table).
:- mode polymorphism__process_procs(in, in, in, in, in, out) is det.

polymorphism__process_procs([], Procs, _, _, _, Procs).
polymorphism__process_procs([ProcId | ProcIds], Procs0, PredInfo, ClausesInfo,
		ExtraArgModes, Procs) :-
	map__lookup(Procs0, ProcId, ProcInfo0),
	polymorphism__process_proc(ProcId, ProcInfo0, PredInfo, ClausesInfo,
				ExtraArgModes, ProcInfo),
	map__det_update(Procs0, ProcId, ProcInfo, Procs1),
	polymorphism__process_procs(ProcIds, Procs1, PredInfo, ClausesInfo,
				ExtraArgModes, Procs).

:- pred polymorphism__process_proc(proc_id, proc_info, pred_info, clauses_info,
			list(mode), proc_info).
:- mode polymorphism__process_proc(in, in, in, in, in, out) is det.

polymorphism__process_proc(ProcId, ProcInfo0, PredInfo, ClausesInfo,
			ExtraArgModes, ProcInfo) :-
	%
	% copy all the information from the clauses_info into the proc_info
	%
	(
		( pred_info_is_imported(PredInfo)
		; pred_info_is_pseudo_imported(PredInfo),
		  hlds_pred__in_in_unification_proc_id(ProcId)
		)
	->
		% 
		% We need to set these fields in the proc_info here, because
		% some parts of the compiler (e.g. unused_args.m) depend on the
		% these fields being valid even for imported procedures.
		%
		clauses_info_headvars(ClausesInfo, HeadVars),
		clauses_info_typeclass_info_varmap(ClausesInfo,
			TypeClassInfoVarMap),
		clauses_info_type_info_varmap(ClausesInfo, TypeInfoVarMap),
		clauses_info_varset(ClausesInfo, VarSet),
		clauses_info_vartypes(ClausesInfo, VarTypes),
		proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo1),
		proc_info_set_typeclass_info_varmap(ProcInfo1, 
			TypeClassInfoVarMap, ProcInfo2),
		proc_info_set_typeinfo_varmap(ProcInfo2, 
			TypeInfoVarMap, ProcInfo3),
		proc_info_set_varset(ProcInfo3, VarSet, ProcInfo4),
		proc_info_set_vartypes(ProcInfo4, VarTypes, ProcInfo5)
	;
		copy_clauses_to_proc(ProcId, ClausesInfo, ProcInfo0, ProcInfo5)
	),

	%
	% add the ExtraArgModes to the proc_info argmodes
	%
	proc_info_argmodes(ProcInfo5, ArgModes1),
	list__append(ExtraArgModes, ArgModes1, ArgModes),
	proc_info_set_argmodes(ProcInfo5, ArgModes, ProcInfo).

% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

:- pred polymorphism__setup_headvars(pred_info, list(prog_var),
		list(prog_var), list(mode), list(tvar), list(tvar),
		list(prog_var), list(prog_var), poly_info, poly_info).
:- mode polymorphism__setup_headvars(in, in, out, out, out, out, out, out,
		in, out) is det.

polymorphism__setup_headvars(PredInfo, HeadVars0, HeadVars, ExtraArgModes,
		HeadTypeVars, UnconstrainedTVars, ExtraHeadTypeInfoVars,
		ExistHeadTypeClassInfoVars, PolyInfo0, PolyInfo) :-
	pred_info_get_maybe_instance_method_constraints(PredInfo,
		MaybeInstanceMethodConstraints),
	(
		MaybeInstanceMethodConstraints = no,
		pred_info_get_class_context(PredInfo, ClassContext),
		ExtraHeadVars0 = [],
		ExtraArgModes0 = [],
		InstanceUnconstrainedTVars = [],
		InstanceUnconstrainedTypeInfoVars = [],
		polymorphism__setup_headvars_2(PredInfo, ClassContext,
			ExtraHeadVars0, ExtraArgModes0,
			InstanceUnconstrainedTVars,
			InstanceUnconstrainedTypeInfoVars, HeadVars0, HeadVars,
			ExtraArgModes, HeadTypeVars, UnconstrainedTVars,
			ExtraHeadTypeInfoVars, ExistHeadTypeClassInfoVars,
			PolyInfo0, PolyInfo)
	;
		MaybeInstanceMethodConstraints =
			yes(InstanceMethodConstraints),	
		polymorphism__setup_headvars_instance_method(PredInfo,
			InstanceMethodConstraints, HeadVars0, HeadVars,
			ExtraArgModes, HeadTypeVars, UnconstrainedTVars,
			ExtraHeadTypeInfoVars, ExistHeadTypeClassInfoVars,
			PolyInfo0, PolyInfo)
	).

	%
	% For class method implementations, do_call_class_method
	% takes the type-infos and typeclass-infos from the
	% typeclass-info and pastes them onto the front of
	% the argument list. We need to match that order here.
	%
:- pred polymorphism__setup_headvars_instance_method(pred_info,
		instance_method_constraints, list(prog_var), list(prog_var),
		list(mode), list(tvar), list(tvar), list(prog_var),
		list(prog_var), poly_info, poly_info).
:- mode polymorphism__setup_headvars_instance_method(in, in, in, out, out, out,
		out, out, out, in, out) is det.

polymorphism__setup_headvars_instance_method(PredInfo,
		InstanceMethodConstraints, HeadVars0, HeadVars, ExtraArgModes,
		HeadTypeVars, UnconstrainedTVars, ExtraHeadTypeInfoVars,
		ExistHeadTypeClassInfoVars, PolyInfo0, PolyInfo) :-

	InstanceMethodConstraints = instance_method_constraints(_,
		InstanceTypes, InstanceConstraints, ClassContext),

	term__vars_list(InstanceTypes, InstanceTVars),
	get_unconstrained_tvars(InstanceTVars, InstanceConstraints,
		UnconstrainedInstanceTVars),
	pred_info_arg_types(PredInfo, ArgTypeVarSet, _, _),
	polymorphism__make_head_vars(UnconstrainedInstanceTVars,
		ArgTypeVarSet, UnconstrainedInstanceTypeInfoVars,
		PolyInfo0, PolyInfo1),
	polymorphism__make_typeclass_info_head_vars(InstanceConstraints,
		InstanceHeadTypeClassInfoVars, PolyInfo1, PolyInfo2),
	poly_info_get_typeclass_info_map(PolyInfo2, TCVarMap0),
	map__det_insert_from_corresponding_lists(TCVarMap0, 
		InstanceConstraints, InstanceHeadTypeClassInfoVars, TCVarMap),
	poly_info_set_typeclass_info_map(TCVarMap, PolyInfo2, PolyInfo3),
	list__append(UnconstrainedInstanceTypeInfoVars,
		InstanceHeadTypeClassInfoVars, ExtraHeadVars0),
	in_mode(InMode),
	list__duplicate(list__length(ExtraHeadVars0), InMode, ExtraArgModes0),
	polymorphism__setup_headvars_2(PredInfo, ClassContext,
		ExtraHeadVars0, ExtraArgModes0, UnconstrainedInstanceTVars,
		UnconstrainedInstanceTypeInfoVars, HeadVars0, HeadVars,
		ExtraArgModes, HeadTypeVars,
		UnconstrainedTVars, ExtraHeadTypeInfoVars,
		ExistHeadTypeClassInfoVars, PolyInfo3, PolyInfo).

:- pred polymorphism__setup_headvars_2(pred_info, class_constraints,
		list(prog_var), list(mode), list(tvar), list(prog_var),
		list(prog_var), list(prog_var), list(mode), list(tvar),
		list(tvar), list(prog_var), list(prog_var),
		poly_info, poly_info).
:- mode polymorphism__setup_headvars_2(in, in, in, in, in, in, in,
		out, out, out, out, out, out, in, out) is det.

polymorphism__setup_headvars_2(PredInfo, ClassContext, ExtraHeadVars0,
		ExtraArgModes0, UnconstrainedInstanceTVars,
		UnconstrainedInstanceTypeInfoVars, HeadVars0,
		HeadVars, ExtraArgModes, HeadTypeVars, AllUnconstrainedTVars,
		AllExtraHeadTypeInfoVars, ExistHeadTypeClassInfoVars,
		PolyInfo0, PolyInfo) :-
	%
	% grab the appropriate fields from the pred_info
	%
	pred_info_arg_types(PredInfo, ArgTypeVarSet, ExistQVars, ArgTypes),

	%
	% Insert extra head variables to hold the address of the
	% type_infos and typeclass_infos.
	% We insert one variable for each unconstrained type variable
	% (for the type_info) and one variable for each constraint (for
	% the typeclass_info).
	%

		% Make a fresh variable for each class constraint, returning
		% a list of variables that appear in the constraints, along
		% with the location of the type infos for them.
	ClassContext = constraints(UnivConstraints, ExistConstraints),
	polymorphism__make_typeclass_info_head_vars(ExistConstraints,
		ExistHeadTypeClassInfoVars, PolyInfo0, PolyInfo1),
	poly_info_get_type_info_map(PolyInfo1, TypeInfoMap1),
	map__keys(TypeInfoMap1, ExistConstrainedTVars),

	polymorphism__make_typeclass_info_head_vars(UnivConstraints,
		UnivHeadTypeClassInfoVars, PolyInfo1, PolyInfo2),
	poly_info_get_type_info_map(PolyInfo2, TypeInfoMap3),
	map__keys(TypeInfoMap3, UnivConstrainedTVars),

	list__append(UnivHeadTypeClassInfoVars, ExistHeadTypeClassInfoVars,
		ExtraHeadTypeClassInfoVars),

	term__vars_list(ArgTypes, HeadTypeVars),
	list__delete_elems(HeadTypeVars, UnivConstrainedTVars, 
		UnconstrainedTVars0),
	list__delete_elems(UnconstrainedTVars0, ExistConstrainedTVars, 
		UnconstrainedTVars1),

	% Type-infos for the unconstrained instance tvars have already
	% been introduced by polymorphism__setup_headvars_instance_method.
	list__delete_elems(UnconstrainedTVars1, UnconstrainedInstanceTVars,
		UnconstrainedTVars2),
	list__remove_dups(UnconstrainedTVars2, UnconstrainedTVars), 

	( ExistQVars = [] ->
		% optimize common case
		UnconstrainedUnivTVars = UnconstrainedTVars,
		UnconstrainedExistTVars = [],
		ExistHeadTypeInfoVars = [],
		PolyInfo3 = PolyInfo2
	;
		list__delete_elems(UnconstrainedTVars, ExistQVars,
			UnconstrainedUnivTVars),
		list__delete_elems(UnconstrainedTVars, UnconstrainedUnivTVars,
			UnconstrainedExistTVars),
		polymorphism__make_head_vars(UnconstrainedExistTVars,
			ArgTypeVarSet, ExistHeadTypeInfoVars,
			PolyInfo2, PolyInfo3)
	),

	polymorphism__make_head_vars(UnconstrainedUnivTVars,
		ArgTypeVarSet, UnivHeadTypeInfoVars, PolyInfo3, PolyInfo4),
	list__append(UnivHeadTypeInfoVars, ExistHeadTypeInfoVars,
		ExtraHeadTypeInfoVars),

	list__append(UnconstrainedInstanceTypeInfoVars, ExtraHeadTypeInfoVars,
		AllExtraHeadTypeInfoVars),
	list__condense([UnconstrainedInstanceTVars, UnconstrainedUnivTVars,
		UnconstrainedExistTVars], AllUnconstrainedTVars),

		% First the type_infos and typeclass_infos from
		% the typeclass_info if this is an instance method
		% implementation, then the type_infos, then the
		% typeclass_infos, but we have to do it in reverse
		% because we're appending...
	list__append(ExtraHeadTypeClassInfoVars, HeadVars0, HeadVars1),
	list__append(ExtraHeadTypeInfoVars, HeadVars1, HeadVars2),
	list__append(ExtraHeadVars0, HeadVars2, HeadVars),

	%
	% Figure out the modes of the introduced type_info and
	% typeclass_info arguments
	%
	in_mode(In),
	out_mode(Out),
	list__length(UnconstrainedUnivTVars, NumUnconstrainedUnivTVars),
	list__length(UnconstrainedExistTVars, NumUnconstrainedExistTVars),
	list__length(UnivHeadTypeClassInfoVars, NumUnivClassInfoVars),
	list__length(ExistHeadTypeClassInfoVars, NumExistClassInfoVars),
	list__duplicate(NumUnconstrainedUnivTVars, In, UnivTypeInfoModes),
	list__duplicate(NumUnconstrainedExistTVars, Out, ExistTypeInfoModes),
	list__duplicate(NumUnivClassInfoVars, In, UnivTypeClassInfoModes),
	list__duplicate(NumExistClassInfoVars, Out, ExistTypeClassInfoModes),
	list__condense([ExtraArgModes0, UnivTypeInfoModes, ExistTypeInfoModes,
		UnivTypeClassInfoModes, ExistTypeClassInfoModes],
		ExtraArgModes),
		
	%
	% Add the locations of the typeinfos
	% for unconstrained, universally quantified type variables.
	% to the initial tvar->type_info_var mapping
	%
	ToLocn = (pred(TheVar::in, TheLocn::out) is det :-
			TheLocn = type_info(TheVar)),

	list__map(ToLocn, UnivHeadTypeInfoVars, UnivTypeLocns),
	map__det_insert_from_corresponding_lists(TypeInfoMap3,
		UnconstrainedUnivTVars, UnivTypeLocns, TypeInfoMap4),

	list__map(ToLocn, ExistHeadTypeInfoVars, ExistTypeLocns),
	map__det_insert_from_corresponding_lists(TypeInfoMap4,
		UnconstrainedExistTVars, ExistTypeLocns, TypeInfoMap5),

	list__map(ToLocn, UnconstrainedInstanceTypeInfoVars,
		UnconstrainedInstanceTypeLocns),
	map__det_insert_from_corresponding_lists(TypeInfoMap5,
		UnconstrainedInstanceTVars, UnconstrainedInstanceTypeLocns,
		TypeInfoMap6),

	poly_info_set_type_info_map(TypeInfoMap6, PolyInfo4, PolyInfo5),

	% Make a map of the locations of the typeclass_infos
	poly_info_get_typeclass_info_map(PolyInfo5, TypeClassInfoMap0),
	map__set_from_corresponding_lists(TypeClassInfoMap0,
		UnivConstraints, UnivHeadTypeClassInfoVars, TypeClassInfoMap),
	poly_info_set_typeclass_info_map(TypeClassInfoMap,
		PolyInfo5, PolyInfo).


% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

%
% generate code to produce the values of type_infos and typeclass_infos
% for existentially quantified type variables in the head
%
:- pred polymorphism__produce_existq_tvars(pred_info, list(prog_var),
		list(tvar), list(prog_var), list(prog_var),
		hlds_goal, hlds_goal, poly_info, poly_info).
:- mode polymorphism__produce_existq_tvars(in, in, in, in, in, in, out,
			in, out) is det.

polymorphism__produce_existq_tvars(PredInfo, HeadVars0,
		UnconstrainedTVars, TypeInfoHeadVars,
		ExistTypeClassInfoHeadVars, Goal0, Goal, Info0, Info) :-
	poly_info_get_var_types(Info0, VarTypes0),
	pred_info_arg_types(PredInfo, ArgTypes),
	pred_info_get_class_context(PredInfo, ClassContext),

	%
	% Figure out the bindings for any existentially quantified
	% type variables in the head.
	%
	ClassContext = constraints(_UnivConstraints, ExistConstraints0),
	( map__is_empty(VarTypes0) ->
		% this can happen for compiler-generated procedures
		map__init(TypeSubst)
	;
		map__apply_to_list(HeadVars0, VarTypes0, ActualArgTypes),
		type_list_subsumes(ArgTypes, ActualArgTypes, ArgTypeSubst)
	->
		TypeSubst = ArgTypeSubst
	;
		% this can happen for unification procedures
		% of equivalence types
		% error("polymorphism.m: type_list_subsumes failed")
		map__init(TypeSubst)
	),

	%
	% generate code to produce values for any existentially quantified
	% typeclass-info variables in the head
	%
	ExistQVarsForCall = [],
	Goal0 = _ - GoalInfo,
	goal_info_get_context(GoalInfo, Context),
	apply_rec_subst_to_constraint_list(TypeSubst, ExistConstraints0,
		ExistConstraints),
	polymorphism__make_typeclass_info_vars(	
		ExistConstraints, ExistQVarsForCall, Context,
		ExistTypeClassVars, ExtraTypeClassGoals,
		Info0, Info1),
	polymorphism__update_typeclass_infos(ExistConstraints,
		ExistTypeClassVars, Info1, Info2),
	polymorphism__assign_var_list(
		ExistTypeClassInfoHeadVars, ExistTypeClassVars,
		ExtraTypeClassUnifyGoals),
	
	%
	% apply the type bindings to the unconstrained type variables
	% to give the actual types, and then generate code
	% to initialize the type_infos for those types
	%
	term__var_list_to_term_list(UnconstrainedTVars,
		UnconstrainedTVarTerms),
	term__apply_substitution_to_list(UnconstrainedTVarTerms,
		TypeSubst, ActualTypes),
	polymorphism__make_type_info_vars(ActualTypes, Context,
		TypeInfoVars, ExtraTypeInfoGoals, Info2, Info),
	polymorphism__assign_var_list(TypeInfoHeadVars, TypeInfoVars,
		ExtraTypeInfoUnifyGoals),
	list__condense([[Goal0],
			ExtraTypeClassGoals, ExtraTypeClassUnifyGoals,
			ExtraTypeInfoGoals, ExtraTypeInfoUnifyGoals],
			GoalList),
	conj_list_to_goal(GoalList, GoalInfo, Goal).

:- pred polymorphism__assign_var_list(list(prog_var), list(prog_var),
		list(hlds_goal)).
:- mode polymorphism__assign_var_list(in, in, out) is det.

polymorphism__assign_var_list([], [_|_], _) :-
	error("unify_proc__assign_var_list: length mismatch").
polymorphism__assign_var_list([_|_], [], _) :-
	error("unify_proc__assign_var_list: length mismatch").
polymorphism__assign_var_list([], [], []).
polymorphism__assign_var_list([Var1 | Vars1], [Var2 | Vars2], [Goal | Goals]) :-
	polymorphism__assign_var(Var1, Var2, Goal),
	polymorphism__assign_var_list(Vars1, Vars2, Goals).

:- pred polymorphism__assign_var(prog_var, prog_var, hlds_goal).
:- mode polymorphism__assign_var(in, in, out) is det.

polymorphism__assign_var(Var1, Var2, Goal) :-
	( Var1 = Var2 ->
		true_goal(Goal)
	;
		polymorphism__assign_var_2(Var1, Var2, Goal)
	).

:- pred polymorphism__assign_var_2(prog_var, prog_var, hlds_goal).
:- mode polymorphism__assign_var_2(in, in, out) is det.

polymorphism__assign_var_2(Var1, Var2, Goal) :-
	term__context_init(Context),
	create_atomic_unification(Var1, var(Var2), Context, explicit,
		[], Goal).

%-----------------------------------------------------------------------------%

:- pred polymorphism__process_goal(hlds_goal, hlds_goal,
					poly_info, poly_info).
:- mode polymorphism__process_goal(in, out, in, out) is det.

polymorphism__process_goal(Goal0 - GoalInfo0, Goal) -->
	polymorphism__process_goal_expr(Goal0, GoalInfo0, Goal).

:- pred polymorphism__process_goal_expr(hlds_goal_expr, hlds_goal_info,
					hlds_goal, poly_info, poly_info).
:- mode polymorphism__process_goal_expr(in, in, out, in, out) is det.

	% We don't need to add type-infos for higher-order calls,
	% since the type-infos are added when the closures are
	% constructed, not when they are called.
polymorphism__process_goal_expr(GoalExpr0, GoalInfo0, Goal) -->
	{ GoalExpr0 = generic_call(GenericCall, Args0, Modes0, Det) },

	%
	% For `aditi_insert' and `aditi_delete' calls, we need to add
	% type-infos for the tuple to insert.
	% 
	(
		{ GenericCall = aditi_builtin(
				aditi_tuple_insert_delete(_, _), _) }
	->
		% Aditi base relations must be monomorphic. 
		{ term__context_init(Context) },
		
		=(PolyInfo),
		{ poly_info_get_var_types(PolyInfo, VarTypes) },

		{ get_state_args_det(Args0, TupleArgs, _, _) },
		{ map__apply_to_list(TupleArgs, VarTypes, TupleTypes) },

		polymorphism__make_type_info_vars(TupleTypes,
			Context, TypeInfoVars, TypeInfoGoals),	

		{ list__append(TypeInfoVars, Args0, Args) },

		{ in_mode(InMode) },
		{ list__length(TypeInfoVars, NumTypeInfos) },
		{ list__duplicate(NumTypeInfos, InMode, TypeInfoModes) },
		{ list__append(TypeInfoModes, Modes0, Modes) },

		{ goal_info_get_nonlocals(GoalInfo0, NonLocals0) },
		{ set__insert_list(NonLocals0, TypeInfoVars, NonLocals) },
		{ goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo) },

		{ Call = generic_call(GenericCall, Args, Modes, Det)
			- GoalInfo },
		{ list__append(TypeInfoGoals, [Call], Goals) },
		{ conj_list_to_goal(Goals, GoalInfo0, Goal) }
	;
		{ Goal = GoalExpr0 - GoalInfo0 }
	).

polymorphism__process_goal_expr(Goal0, GoalInfo, Goal) -->
	{ Goal0 = call(PredId, ProcId, ArgVars0, Builtin,
			UnifyContext, Name) },
	polymorphism__process_call(PredId, ArgVars0, GoalInfo,
		ArgVars, _ExtraVars, CallGoalInfo, ExtraGoals),

	{ Call = call(PredId, ProcId, ArgVars, Builtin, UnifyContext, Name)
		- CallGoalInfo },
	{ list__append(ExtraGoals, [Call], GoalList) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) }.

polymorphism__process_goal_expr(Goal0, GoalInfo, Goal) -->
	{ Goal0 = foreign_proc(Attributes, PredId, ProcId,
		ArgVars0, ArgInfo0, OrigArgTypes0, PragmaCode0) },
	polymorphism__process_call(PredId, ArgVars0, GoalInfo,
		ArgVars, ExtraVars, CallGoalInfo, ExtraGoals),

	%
	% insert the type_info vars into the arg-name map,
	% so that the c_code can refer to the type_info variable
	% for type T as `TypeInfo_for_T'.
	%
	=(Info0),
	{ poly_info_get_module_info(Info0, ModuleInfo) },
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },

	{ pred_info_module(PredInfo, PredModule) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_arity(PredInfo, PredArity) },


	( { no_type_info_builtin(PredModule, PredName, PredArity) } ->
		{ Goal = Goal0 - GoalInfo }
	;
		{ list__length(ExtraVars, NumExtraVars) },
		{ polymorphism__process_c_code(PredInfo, NumExtraVars,
			PragmaCode0, OrigArgTypes0, OrigArgTypes, 
			ArgInfo0, ArgInfo) },

		%
		% Add the type info arguments to the list of variables
		% to call for a pragma import.
		%
		(
			{ PragmaCode0 = import(Name, HandleReturn,
					Variables0, MaybeContext) }
		->
			(
				{ list__remove_suffix(ArgInfo, ArgInfo0,
						TypeVarArgInfos) }
			->
				{ Variables = type_info_vars(ModuleInfo,
					TypeVarArgInfos, Variables0) }
			;
				{ error("polymorphism__process_goal_expr") }
			),
			{ PragmaCode = import(Name, HandleReturn,
					Variables, MaybeContext) }
		;
			{ PragmaCode = PragmaCode0 }
		),

		%
		% plug it all back together
		%
		{ Call = foreign_proc(Attributes, PredId, 
			ProcId, ArgVars, ArgInfo, OrigArgTypes, PragmaCode) - 
			CallGoalInfo },
		{ list__append(ExtraGoals, [Call], GoalList) },
		{ conj_list_to_goal(GoalList, GoalInfo, Goal) }
	).

polymorphism__process_goal_expr(unify(XVar, Y, Mode, Unification, UnifyContext),
				GoalInfo, Goal) -->
	polymorphism__process_unify(XVar, Y, Mode, Unification, UnifyContext,
				GoalInfo, Goal).

	% the rest of the clauses just process goals recursively

polymorphism__process_goal_expr(conj(Goals0), GoalInfo,
		conj(Goals) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_expr(par_conj(Goals0), GoalInfo,
		par_conj(Goals) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_expr(disj(Goals0), GoalInfo,
		disj(Goals) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_expr(not(Goal0), GoalInfo, not(Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_expr(switch(Var, CanFail, Cases0), GoalInfo,
				switch(Var, CanFail, Cases) - GoalInfo) -->
	polymorphism__process_case_list(Cases0, Cases).
polymorphism__process_goal_expr(some(Vars, CanRemove, Goal0), GoalInfo,
			some(Vars, CanRemove, Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_expr(if_then_else(Vars, A0, B0, C0), GoalInfo,
			if_then_else(Vars, A, B, C) - GoalInfo) -->
	polymorphism__process_goal(A0, A),
	polymorphism__process_goal(B0, B),
	polymorphism__process_goal(C0, C).
polymorphism__process_goal_expr(shorthand(_), _, _) -->
	% these should have been expanded out by now
	{ error("polymorphism__process_goal_expr: unexpected shorthand") }.


	% type_info_vars prepends a comma seperated list of variables
	% onto a string of variables.
	% It places an & at the start of the variable name if variable
	% is an output variable.
:- func type_info_vars(module_info, list(maybe(pair(string, mode))),
		string) = string.

type_info_vars(_ModuleInfo, [], InitString) = InitString.
type_info_vars(ModuleInfo, [ArgInfo | ArgInfos], InitString) = String :-
	String0 = type_info_vars(ModuleInfo, ArgInfos, InitString),
	( ArgInfo = yes(ArgName0 - Mode) ->
		( mode_is_output(ModuleInfo, Mode) ->
			string__append("&", ArgName0, ArgName)
		;
			ArgName = ArgName0
		),
		( String0 = "" ->
			String = ArgName
		;
			String = string__append_list([ArgName, ", ", String0])
		)
	;
		String = String0
	).

:- pred polymorphism__process_unify(prog_var, unify_rhs,
		unify_mode, unification, unify_context, hlds_goal_info,
		hlds_goal, poly_info, poly_info).
:- mode polymorphism__process_unify(in, in, in, in, in, in, out,
		in, out) is det.

polymorphism__process_unify(XVar, Y, Mode, Unification0, UnifyContext,
			GoalInfo0, Goal) -->
	% switch on Y
	(
		{ Y = var(_YVar) },
		%
		% var-var unifications (simple_test, assign,
		% or complicated_unify) are basically left unchanged.
		% Complicated unifications will eventually get converted into
		% calls, but that is done later on, by simplify.m, not now.
		% At this point we just need to figure out
		% which type_info/typeclass_info variables the unification
		% might need, and insert them in the non-locals.
		% We have to do that for all var-var unifications,
		% because at this point we haven't done mode analysis so
		% we don't know which ones will become complicated_unifies.
		% Note that we also store the type_info/typeclass_info
		% variables in a field in the unification, which
		% quantification.m uses when requantifying things.
		%
		=(Info0),
		{ poly_info_get_var_types(Info0, VarTypes) },
		{ map__lookup(VarTypes, XVar, Type) },
		polymorphism__unification_typeinfos(Type,
			Unification0, GoalInfo0, Unification, GoalInfo),
		{ Goal = unify(XVar, Y, Mode, Unification,
		 		UnifyContext) - GoalInfo }
	; 
		{ Y = functor(ConsId, _, Args) },
		polymorphism__process_unify_functor(XVar, ConsId, Args, Mode,
			Unification0, UnifyContext, GoalInfo0, Goal)
	;
		{ Y = lambda_goal(PredOrFunc, EvalMethod, FixModes,
			ArgVars0, LambdaVars, Modes, Det, LambdaGoal0) },
		%
		% for lambda expressions, we must recursively traverse the
		% lambda goal
		%
		polymorphism__process_goal(LambdaGoal0, LambdaGoal1),
		% Currently we don't allow lambda goals to be
		% existentially typed
		{ ExistQVars = [] },
		polymorphism__fixup_lambda_quantification(LambdaGoal1,
				ArgVars0, LambdaVars, ExistQVars,
				LambdaGoal, NonLocalTypeInfos),
		{ set__to_sorted_list(NonLocalTypeInfos,
				NonLocalTypeInfosList) },
		{ list__append(NonLocalTypeInfosList, ArgVars0, ArgVars) },
		{ Y1 = lambda_goal(PredOrFunc, EvalMethod, FixModes,
			ArgVars, LambdaVars, Modes, Det, LambdaGoal) },
                { goal_info_get_nonlocals(GoalInfo0, NonLocals0) },
		{ set__union(NonLocals0, NonLocalTypeInfos, NonLocals) },
		{ goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo) },

		%
		% Complicated (in-in) argument unifications are impossible
		% for lambda expressions, so we don't need to worry about
		% adding the type-infos that would be required for such
		% unifications.
		%
		{ Goal = unify(XVar, Y1, Mode, Unification0, UnifyContext)
				- GoalInfo }
	).

:- pred polymorphism__unification_typeinfos(type, unification,
	hlds_goal_info, unification, hlds_goal_info, poly_info, poly_info).
:- mode polymorphism__unification_typeinfos(in, in, in,
	out, out, in, out) is det.

polymorphism__unification_typeinfos(Type, Unification0, GoalInfo0,
		Unification, GoalInfo, Info0, Info) :-
	%
	% Compute the type_info/type_class_info variables that would be
	% used if this unification ends up being a complicated_unify.
	%
	type_util__vars(Type, TypeVars),
	list__map_foldl(get_type_info_locn, TypeVars, TypeInfoLocns,
		Info0, Info),

	polymorphism__add_unification_typeinfos(TypeInfoLocns,
		Unification0, GoalInfo0, Unification, GoalInfo).

	% This variant is for use by modecheck_unify.m.
	% During mode-checking all the type-infos should appear in
	% the type_info_varmap.
polymorphism__unification_typeinfos(Type, TypeInfoMap,
		Unification0, GoalInfo0, Unification, GoalInfo) :-
	%
	% Compute the type_info/type_class_info variables that would be
	% used if this unification ends up being a complicated_unify.
	%
	type_util__vars(Type, TypeVars),
	map__apply_to_list(TypeVars, TypeInfoMap, TypeInfoLocns),

	polymorphism__add_unification_typeinfos(TypeInfoLocns,
		Unification0, GoalInfo0, Unification, GoalInfo).

:- pred polymorphism__add_unification_typeinfos(list(type_info_locn)::in,
		unification::in, hlds_goal_info::in,
		unification::out, hlds_goal_info::out) is det.

polymorphism__add_unification_typeinfos(TypeInfoLocns,
		Unification0, GoalInfo0, Unification, GoalInfo) :-
	list__map(type_info_locn_var, TypeInfoLocns, TypeInfoVars0),
	list__remove_dups(TypeInfoVars0, TypeInfoVars),

	%
	% Insert the TypeInfoVars into the nonlocals field of the goal_info
	% for the unification goal.
	%
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	set__insert_list(NonLocals0, TypeInfoVars, NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),

	%
	% Also save those type_info vars into a field in the complicated_unify,
	% so that quantification.m can recompute variable scopes properly.
	% This field is also used by modecheck_unify.m -- for complicated
	% unifications, it checks that all these variables are ground.
	%
	( Unification0 = complicated_unify(Modes, CanFail, _) ->
		Unification = complicated_unify(Modes, CanFail, TypeInfoVars)
	;
		error("polymorphism__unification_typeinfos")
	).

:- pred polymorphism__process_unify_functor(prog_var, cons_id, list(prog_var),
		unify_mode, unification, unify_context, hlds_goal_info,
		hlds_goal, poly_info, poly_info).
:- mode polymorphism__process_unify_functor(in, in, in, in, in, in, in, out,
		in, out) is det.

polymorphism__process_unify_functor(X0, ConsId0, ArgVars0, Mode0,
		Unification0, UnifyContext, GoalInfo0, Goal,
		PolyInfo0, PolyInfo) :-
	poly_info_get_module_info(PolyInfo0, ModuleInfo0),
	poly_info_get_var_types(PolyInfo0, VarTypes0),
	map__lookup(VarTypes0, X0, TypeOfX),
	list__length(ArgVars0, Arity),
	(
	%
	% We replace any unifications with higher-order pred constants
	% by lambda expressions.  For example, we replace
	%
	%       X = list__append(Y)     % Y::in, X::out
	%
	% with
	%
	%       X = lambda [A1::in, A2::out] (list__append(Y, A1, A2))
	%
	% We do this because it makes two things easier.
	% Firstly, mode analysis needs to check that the lambda-goal doesn't
	% bind any non-local variables (e.g. `Y' in above example).
	% This would require a bit of moderately tricky special-case code
	% if we didn't expand them here.
	% Secondly, this pass (polymorphism.m) is a lot easier
	% if we don't have to handle higher-order pred consts.
	% If it turns out that the predicate was non-polymorphic,
	% lambda.m will turn the lambda expression back into a
	% higher-order pred constant again.
	%
	% Note that this transformation is also done by modecheck_unify.m,
	% in case we are rerunning mode analysis after lambda.m has already
	% been run; any changes to the code here will also need to be
	% duplicated there.
	%

		% check if variable has a higher-order type
		type_is_higher_order(TypeOfX, _,
			EvalMethod, PredArgTypes),
		ConsId0 = pred_const(PredId, ProcId, _)
	->
		%
		% convert the higher-order pred term to a lambda goal
		%
		poly_info_get_varset(PolyInfo0, VarSet0),
		goal_info_get_context(GoalInfo0, Context),
		convert_pred_to_lambda_goal(EvalMethod,
			X0, PredId, ProcId, ArgVars0, PredArgTypes,
			UnifyContext, GoalInfo0, Context,
			ModuleInfo0, VarSet0, VarTypes0,
			Functor0, VarSet, VarTypes),
		poly_info_set_varset_and_types(VarSet, VarTypes,
			PolyInfo0, PolyInfo1),
		%
		% process the unification in its new form
		%
		polymorphism__process_unify(X0, Functor0, Mode0,
				Unification0, UnifyContext, GoalInfo0, Goal,
				PolyInfo1, PolyInfo)
	;
		%
		% is this a construction or deconstruction of an
		% existentially typed data type?
		%

		%
		% Check whether the functor had a "new " prefix.
		% If so, assume it is a construction, and strip off the prefix.
		% Otherwise, assume it is a deconstruction.
		%
		ConsId0 = cons(Functor0, Arity),
		(
			remove_new_prefix(Functor0, OrigFunctor)
		->
			ConsId = cons(OrigFunctor, Arity),
			IsConstruction = yes
		;
			ConsId = ConsId0,
			IsConstruction = no
		),

		%
		% Check whether the functor (with the "new " prefix removed)
		% is an existentially typed functor.
		%
		type_util__get_existq_cons_defn(ModuleInfo0, TypeOfX, ConsId,
			ConsDefn)
	->
		%
		% add extra arguments to the unification for the
		% type_info and/or type_class_info variables
		%
		map__apply_to_list(ArgVars0, VarTypes0, ActualArgTypes),
		goal_info_get_context(GoalInfo0, Context),
		polymorphism__process_existq_unify_functor(ConsDefn,
			IsConstruction, ActualArgTypes, TypeOfX, Context,
			ExtraVars, ExtraGoals, PolyInfo0, PolyInfo1),
		list__append(ExtraVars, ArgVars0, ArgVars),
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		set__insert_list(NonLocals0, ExtraVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),

		%
		% Some of the argument unifications may be complicated
		% unifications, which may need type-infos.
		%
		polymorphism__unification_typeinfos(TypeOfX, Unification0,
			GoalInfo1, Unification, GoalInfo, PolyInfo1, PolyInfo),

		Unify = unify(X0, functor(ConsId, IsConstruction, ArgVars),
				Mode0, Unification, UnifyContext) - GoalInfo,
		list__append(ExtraGoals, [Unify], GoalList),
		conj_list_to_goal(GoalList, GoalInfo0, Goal)
	;
		%
		% We leave construction/deconstruction unifications alone.
		% Some of the argument unifications may be complicated
		% unifications, which may need type-infos.
		%
		polymorphism__unification_typeinfos(TypeOfX, Unification0,
			GoalInfo0, Unification, GoalInfo, PolyInfo0, PolyInfo),
		Goal = unify(X0, functor(ConsId0, no, ArgVars0), Mode0,
			Unification, UnifyContext) - GoalInfo
	).

convert_pred_to_lambda_goal(EvalMethod, X0, PredId, ProcId,
		ArgVars0, PredArgTypes, UnifyContext, GoalInfo0, Context,
		ModuleInfo0, VarSet0, VarTypes0,
		Functor, VarSet, VarTypes) :-
	%
	% Create the new lambda-quantified variables
	%
	make_fresh_vars(PredArgTypes, VarSet0, VarTypes0,
			LambdaVars, VarSet, VarTypes),
	list__append(ArgVars0, LambdaVars, Args),

	%
	% Build up the hlds_goal_expr for the call that will form
	% the lambda goal
	%
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
				PredInfo, ProcInfo),

	pred_info_module(PredInfo, PredModule),
	pred_info_name(PredInfo, PredName),
	QualifiedPName = qualified(PredModule, PredName),

	CallUnifyContext = call_unify_context(X0,
			functor(cons(QualifiedPName, list__length(ArgVars0)),
				no, ArgVars0),
			UnifyContext),
	LambdaGoalExpr = call(PredId, ProcId, Args, not_builtin,
			yes(CallUnifyContext), QualifiedPName),

	%
	% construct a goal_info for the lambda goal, making sure
	% to set up the nonlocals field in the goal_info correctly
	%
	goal_info_get_nonlocals(GoalInfo0, NonLocals),
	set__insert_list(NonLocals, LambdaVars, OutsideVars),
	set__list_to_set(Args, InsideVars),
	set__intersect(OutsideVars, InsideVars, LambdaNonLocals),
	goal_info_init(LambdaGoalInfo0),
	goal_info_set_context(LambdaGoalInfo0, Context,
			LambdaGoalInfo1),
	goal_info_set_nonlocals(LambdaGoalInfo1, LambdaNonLocals,
			LambdaGoalInfo),
	LambdaGoal = LambdaGoalExpr - LambdaGoalInfo,

	%
	% work out the modes of the introduced lambda variables
	% and the determinism of the lambda goal
	%
	proc_info_argmodes(ProcInfo, ArgModes),
	list__length(ArgModes, NumArgModes),
	list__length(LambdaVars, NumLambdaVars),
	( list__drop(NumArgModes - NumLambdaVars, ArgModes, LambdaModes0) ->
		LambdaModes = LambdaModes0
	;
		error("convert_pred_to_lambda_goal: list__drop failed")
	),
	proc_info_declared_determinism(ProcInfo, MaybeDet),
	( MaybeDet = yes(Det) ->
		LambdaDet = Det
	;
		error("Sorry, not implemented: determinism inference for higher-order predicate terms")
	),

	%
	% construct the lambda expression
	%
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	Functor = lambda_goal(PredOrFunc, EvalMethod, modes_are_ok,
		ArgVars0, LambdaVars, LambdaModes, LambdaDet, LambdaGoal).

:- pred make_fresh_vars(list(type), prog_varset, map(prog_var, type),
			list(prog_var), prog_varset, map(prog_var, type)).
:- mode make_fresh_vars(in, in, in, out, out, out) is det.

make_fresh_vars([], VarSet, VarTypes, [], VarSet, VarTypes).
make_fresh_vars([Type|Types], VarSet0, VarTypes0,
		[Var|Vars], VarSet, VarTypes) :-
	varset__new_var(VarSet0, Var, VarSet1),
	map__det_insert(VarTypes0, Var, Type, VarTypes1),
	make_fresh_vars(Types, VarSet1, VarTypes1, Vars, VarSet, VarTypes).

%-----------------------------------------------------------------------------%

%
% compute the extra arguments that we need to add to a unification with
% an existentially quantified data constructor.
%
:- pred polymorphism__process_existq_unify_functor(
		ctor_defn, bool, list(type), (type), prog_context,
		list(prog_var), list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__process_existq_unify_functor(in, in, in, in, in,
		out, out, in, out) is det.

polymorphism__process_existq_unify_functor(CtorDefn, IsConstruction,
		ActualArgTypes, ActualRetType, Context,
		ExtraVars, ExtraGoals, PolyInfo0, PolyInfo) :-

	CtorDefn = ctor_defn(CtorTypeVarSet, ExistQVars0,
		ExistentialConstraints0, CtorArgTypes0, CtorRetType0),

	%
	% rename apart the type variables in the constructor definition
	%
	poly_info_get_typevarset(PolyInfo0, TypeVarSet0),
	varset__merge_subst(TypeVarSet0, CtorTypeVarSet, TypeVarSet, Subst),
	term__var_list_to_term_list(ExistQVars0, ExistQVarTerms0),
	term__apply_substitution_to_list(ExistQVarTerms0, Subst,
			ExistQVarsTerms1),
	apply_subst_to_constraint_list(Subst, ExistentialConstraints0,
			ExistentialConstraints1),
	term__apply_substitution_to_list(CtorArgTypes0, Subst, CtorArgTypes1),
	term__apply_substitution(CtorRetType0, Subst, CtorRetType1),
	poly_info_set_typevarset(TypeVarSet, PolyInfo0, PolyInfo1),

	%
	% Compute the type bindings resulting from the functor's actual
	% argument and return types.
	% These are the ones that might bind the ExistQVars.
	%
	( type_list_subsumes([CtorRetType1 | CtorArgTypes1],
			[ActualRetType | ActualArgTypes], TypeSubst1) ->
		TypeSubst = TypeSubst1
	;
		error(
	"polymorphism__process_existq_unify_functor: type unification failed")
	),

	%
	% Apply those type bindings to the existential type class constraints
	%
	apply_rec_subst_to_constraint_list(TypeSubst, ExistentialConstraints1,
		ExistentialConstraints),

	%
	% create type_class_info variables for the
	% type class constraints
	%
	
	(
		IsConstruction = yes,
		% assume it's a construction
		polymorphism__make_typeclass_info_vars(	
				ExistentialConstraints, [], Context,
				ExtraTypeClassVars, ExtraTypeClassGoals,
				PolyInfo1, PolyInfo3)
	;
		IsConstruction = no,
		% assume it's a deconstruction
		polymorphism__make_existq_typeclass_info_vars(
			ExistentialConstraints, ExtraTypeClassVars,
			ExtraTypeClassGoals, PolyInfo1, PolyInfo3)
	),

	%
	% Compute the set of _unconstrained_ existentially quantified type
	% variables, and then apply the type bindings to those type variables
	% to figure out what types they are bound to.
	%
	constraint_list_get_tvars(ExistentialConstraints1,
			ExistConstrainedTVars),
	term__var_list_to_term_list(ExistConstrainedTVars,
			ExistConstrainedTVarTerms),
	list__delete_elems(ExistQVarsTerms1, ExistConstrainedTVarTerms,
			UnconstrainedExistQVarTerms),
	term__apply_rec_substitution_to_list(UnconstrainedExistQVarTerms,
			TypeSubst, ExistentialTypes),

	%
	% create type_info variables for the _unconstrained_
	% existentially quantified type variables
	%
	polymorphism__make_type_info_vars(ExistentialTypes,
			Context, ExtraTypeInfoVars, ExtraTypeInfoGoals,
			PolyInfo3, PolyInfo),

	%
	% the type_class_info variables go AFTER the type_info variables
	% (for consistency with the order for argument passing,
	% and because the RTTI support in the runtime system relies on it)
	%
	list__append(ExtraTypeInfoGoals, ExtraTypeClassGoals, ExtraGoals),
	list__append(ExtraTypeInfoVars, ExtraTypeClassVars, ExtraVars).

%-----------------------------------------------------------------------------%

:- pred polymorphism__process_c_code(pred_info, int, pragma_foreign_code_impl,
	list(type), list(type),
	list(maybe(pair(string, mode))), list(maybe(pair(string, mode)))).
:- mode polymorphism__process_c_code(in, in, in, in, out, in, out) is det.

polymorphism__process_c_code(PredInfo, NumExtraVars, Impl, OrigArgTypes0,
		OrigArgTypes, ArgInfo0, ArgInfo) :-
	pred_info_arg_types(PredInfo, PredTypeVarSet, ExistQVars,
			PredArgTypes),

		% Find out which variables are constrained (so that we don't
		% add type-infos for them.
	pred_info_get_class_context(PredInfo, constraints(UnivCs, ExistCs)),
	GetConstrainedVars = lambda([ClassConstraint::in, CVars::out] is det,
		(
			ClassConstraint = constraint(_, CTypes),
			term__vars_list(CTypes, CVars)
		)
	),
	list__map(GetConstrainedVars, UnivCs, UnivVars0),
	list__condense(UnivVars0, UnivConstrainedVars),
	list__map(GetConstrainedVars, ExistCs, ExistVars0),
	list__condense(ExistVars0, ExistConstrainedVars),

	term__vars_list(PredArgTypes, PredTypeVars0),
	list__remove_dups(PredTypeVars0, PredTypeVars1),
	list__delete_elems(PredTypeVars1, UnivConstrainedVars, 
		PredTypeVars2),
	list__delete_elems(PredTypeVars2, ExistConstrainedVars, 
		PredTypeVars),

		% sanity check
	list__length(UnivCs, NUCs),
	list__length(ExistCs, NECs),
	NCs is NUCs + NECs,
	list__length(PredTypeVars, NTs),
	NEVs is NCs + NTs,
	require(unify(NEVs, NumExtraVars), 
		"list length mismatch in polymorphism processing pragma_c"),

%	The argument order is as follows:
%	first the UnivTypeInfos (for universally quantified type variables)
% 	then the ExistTypeInfos (for existentially quantified type variables)
%	then the UnivTypeClassInfos (for universally quantified constraints)
%	then the ExistTypeClassInfos (for existentially quantified constraints)
%	and finally the original arguments of the predicate.
%
%	But since we're building ArgInfo by starting with the original
%	arguments and prepending things as we go, we need to do it in
%	reverse order.

	polymorphism__c_code_add_typeclass_infos(
			UnivCs, ExistCs, PredTypeVarSet, Impl,
			ArgInfo0, ArgInfo1),
	polymorphism__c_code_add_typeinfos(
			PredTypeVars, PredTypeVarSet, ExistQVars, Impl,
			ArgInfo1, ArgInfo),

	%
	% insert type_info/typeclass_info types for all the inserted 
	% type_info/typeclass_info vars into the arg-types list
	%
	term__var_list_to_term_list(PredTypeVars, PredTypeVarTypes),
	list__map(polymorphism__build_type_info_type, PredTypeVarTypes,
		TypeInfoTypes),
	list__map(polymorphism__build_typeclass_info_type, UnivCs, UnivTypes),
	list__map(polymorphism__build_typeclass_info_type, ExistCs, ExistTypes),
	list__append(TypeInfoTypes, OrigArgTypes0, OrigArgTypes1),
	list__append(ExistTypes, OrigArgTypes1, OrigArgTypes2),
	list__append(UnivTypes, OrigArgTypes2, OrigArgTypes).

:- pred polymorphism__c_code_add_typeclass_infos(
		list(class_constraint), list(class_constraint), 
		tvarset, pragma_foreign_code_impl,
		list(maybe(pair(string, mode))),
		list(maybe(pair(string, mode)))). 
:- mode polymorphism__c_code_add_typeclass_infos(in, in, in, in, 
		in, out) is det.

polymorphism__c_code_add_typeclass_infos(UnivCs, ExistCs, 
		PredTypeVarSet, Impl, ArgInfo0, ArgInfo) :-
	in_mode(In),
	out_mode(Out),
	polymorphism__c_code_add_typeclass_infos_2(ExistCs, Out, 
		PredTypeVarSet, Impl, ArgInfo0, ArgInfo1),
	polymorphism__c_code_add_typeclass_infos_2(UnivCs, In, 
		PredTypeVarSet, Impl, ArgInfo1, ArgInfo).

:- pred polymorphism__c_code_add_typeclass_infos_2(
		list(class_constraint), mode,
		tvarset, pragma_foreign_code_impl,
		list(maybe(pair(string, mode))),
		list(maybe(pair(string, mode)))). 
:- mode polymorphism__c_code_add_typeclass_infos_2(in, in, in, in, 
	in, out) is det.  
polymorphism__c_code_add_typeclass_infos_2([], _, _, _, ArgNames, ArgNames).
polymorphism__c_code_add_typeclass_infos_2([C|Cs], Mode, TypeVarSet, Impl,
		ArgNames0, ArgNames) :-
	polymorphism__c_code_add_typeclass_infos_2(Cs, Mode, TypeVarSet, 
		Impl, ArgNames0, ArgNames1),
	C = constraint(Name0, Types),
	prog_out__sym_name_to_string(Name0, "__", Name),
	term__vars_list(Types, TypeVars),
	GetName = lambda([TVar::in, TVarName::out] is det,
		(
			varset__lookup_name(TypeVarSet, TVar, TVarName0),
			string__append("_", TVarName0, TVarName)
		)
	),
	list__map(GetName, TypeVars, TypeVarNames),
	string__append_list(["TypeClassInfo_for_", Name|TypeVarNames],
		C_VarName),
	(
		% If the variable name corresponding to the
		% typeclass-info isn't mentioned in the C code
		% fragment, don't pass the variable to the
		% C code at all.

		foreign_code_does_not_use_variable(Impl, C_VarName)
	->
		ArgNames = [no | ArgNames1]
	;
		ArgNames = [yes(C_VarName - Mode) | ArgNames1]
	).

:- pred polymorphism__c_code_add_typeinfos(list(tvar),
		tvarset, existq_tvars, pragma_foreign_code_impl,
		list(maybe(pair(string, mode))),
		list(maybe(pair(string, mode)))). 
:- mode polymorphism__c_code_add_typeinfos(in, in, in, in, in, out) is det.

polymorphism__c_code_add_typeinfos(TVars, TypeVarSet,
		ExistQVars, Impl, ArgNames0, ArgNames) :-
	list__filter(lambda([X::in] is semidet, (list__member(X, ExistQVars))),
		TVars, ExistUnconstrainedVars, UnivUnconstrainedVars),
	in_mode(In),
	out_mode(Out),
	polymorphism__c_code_add_typeinfos_2(ExistUnconstrainedVars, TypeVarSet,
		Out, Impl, ArgNames0, ArgNames1),
	polymorphism__c_code_add_typeinfos_2(UnivUnconstrainedVars, TypeVarSet,
		In, Impl, ArgNames1, ArgNames).

:- pred polymorphism__c_code_add_typeinfos_2(list(tvar),
		tvarset, mode, pragma_foreign_code_impl,
		list(maybe(pair(string, mode))),
		list(maybe(pair(string, mode)))). 
:- mode polymorphism__c_code_add_typeinfos_2(in, in, in, in, in, out) is det.

polymorphism__c_code_add_typeinfos_2([], _, _, _, ArgNames, ArgNames).
polymorphism__c_code_add_typeinfos_2([TVar|TVars], TypeVarSet, Mode, Impl,
		ArgNames0, ArgNames) :-
	polymorphism__c_code_add_typeinfos_2(TVars, TypeVarSet,
		Mode, Impl, ArgNames0, ArgNames1),
	( varset__search_name(TypeVarSet, TVar, TypeVarName) ->
		string__append("TypeInfo_for_", TypeVarName, C_VarName),
		(
			% If the variable name corresponding to the
			% type-info isn't mentioned in the C code
			% fragment, don't pass the variable to the
			% C code at all.

			foreign_code_does_not_use_variable(Impl, C_VarName)
		->
			ArgNames = [no | ArgNames1]
		;
			ArgNames = [yes(C_VarName - Mode) | ArgNames1]
		)
	;
		ArgNames = [no | ArgNames1]
	).

:- pred foreign_code_does_not_use_variable(pragma_foreign_code_impl, string).
:- mode foreign_code_does_not_use_variable(in, in) is semidet.

foreign_code_does_not_use_variable(Impl, VarName) :-
		% XXX This test is temporarily turned off, as it causes
		% the compiler to abort when compiling
		% stage2/browser/declarative_execution.m
	semidet_fail,
	(
		Impl = ordinary(ForeignBody, _),
		\+ string__sub_string_search(ForeignBody,
			VarName, _)
	;
		Impl = nondet(FB1,_,FB2,_,FB3,_,_,FB4,_),
		\+ string__sub_string_search(FB1, VarName, _),
		\+ string__sub_string_search(FB2, VarName, _),
		\+ string__sub_string_search(FB3, VarName, _),
		\+ string__sub_string_search(FB4, VarName, _)
	).

:- pred polymorphism__process_goal_list(list(hlds_goal), list(hlds_goal),
					poly_info, poly_info).
:- mode polymorphism__process_goal_list(in, out, in, out) is det.

polymorphism__process_goal_list([], []) --> [].
polymorphism__process_goal_list([Goal0 | Goals0], [Goal | Goals]) -->
	polymorphism__process_goal(Goal0, Goal),
	polymorphism__process_goal_list(Goals0, Goals).

:- pred polymorphism__process_case_list(list(case), list(case),
					poly_info, poly_info).
:- mode polymorphism__process_case_list(in, out, in, out) is det.

polymorphism__process_case_list([], []) --> [].
polymorphism__process_case_list([Case0 | Cases0], [Case | Cases]) -->
	{ Case0 = case(ConsId, Goal0) },
	polymorphism__process_goal(Goal0, Goal),
	{ Case = case(ConsId, Goal) },
	polymorphism__process_case_list(Cases0, Cases).

%-----------------------------------------------------------------------------%

% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

:- pred polymorphism__process_call(pred_id, list(prog_var), hlds_goal_info,
		list(prog_var), list(prog_var), hlds_goal_info,
		list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__process_call(in, in, in,
		out, out, out, out, in, out) is det.

polymorphism__process_call(PredId, ArgVars0, GoalInfo0,
		ArgVars, ExtraVars, GoalInfo, ExtraGoals,
		Info0, Info) :-

	poly_info_get_var_types(Info0, VarTypes),
	poly_info_get_typevarset(Info0, TypeVarSet0),
	poly_info_get_module_info(Info0, ModuleInfo),

	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredExistQVars0,
		PredArgTypes0),
	pred_info_get_class_context(PredInfo, PredClassContext0),
		% rename apart
		% (this merge might be a performance bottleneck?)
	( varset__is_empty(PredTypeVarSet) ->
		% optimize common case
		PredArgTypes = PredArgTypes0,
		PredExistQVarTerms1 = [],
		PredTypeVars0 = [],
		TypeVarSet = TypeVarSet0,
		map__init(Subst)
	;
		varset__merge_subst(TypeVarSet0, PredTypeVarSet,
			TypeVarSet, Subst),
		term__apply_substitution_to_list(PredArgTypes0, Subst,
			PredArgTypes),
		term__var_list_to_term_list(PredExistQVars0,
			PredExistQVarTerms0),
		term__apply_substitution_to_list(PredExistQVarTerms0, Subst,
			PredExistQVarTerms1),
		term__vars_list(PredArgTypes, PredTypeVars0)
	),

	pred_info_module(PredInfo, PredModule),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, PredArity),
	( 
		(
			% optimize for common case of non-polymorphic call
			PredTypeVars0 = []
		;
			% some builtins don't need the type_info
			no_type_info_builtin(PredModule, PredName, PredArity)
		;
			% Leave Aditi relations alone, since they must
			% be monomorphic. This is checked by magic.m.
			hlds_pred__pred_info_is_aditi_relation(PredInfo)
		;
			hlds_pred__pred_info_is_aditi_aggregate(PredInfo)
		)
	->
		ArgVars = ArgVars0,
		GoalInfo = GoalInfo0,
		ExtraGoals = [],
		ExtraVars = [],
		Info = Info0
	;
		list__remove_dups(PredTypeVars0, PredTypeVars1),
		map__apply_to_list(ArgVars0, VarTypes, ActualArgTypes),
		( type_list_subsumes(PredArgTypes, ActualArgTypes,
				TypeSubst1) ->
			TypeSubst = TypeSubst1
		;
		error("polymorphism__process_goal_expr: type unification failed")
		),

		apply_subst_to_constraints(Subst, PredClassContext0,
			PredClassContext1),

		poly_info_set_typevarset(TypeVarSet, Info0, Info1),

			% Make the universally quantified typeclass_infos
			% for the call, and return a list of which type
			% variables were constrained by those constraints
		goal_info_get_context(GoalInfo0, Context),
		PredClassContext1 = constraints(UniversalConstraints1,
				ExistentialConstraints1),

			% compute which type variables are constrained
			% by the type class constraints
		constraint_list_get_tvars(ExistentialConstraints1,
			ExistConstrainedTVars),
		constraint_list_get_tvars(UniversalConstraints1,
			UnivConstrainedTVars),

		apply_rec_subst_to_constraint_list(TypeSubst,
			UniversalConstraints1, UniversalConstraints2),

		term__apply_rec_substitution_to_list(PredExistQVarTerms1,
			TypeSubst, PredExistQVarTerms),
		term__term_list_to_var_list(PredExistQVarTerms,
			PredExistQVars),

		polymorphism__make_typeclass_info_vars(	
			UniversalConstraints2,
			PredExistQVars, Context,
			UnivTypeClassVars, ExtraTypeClassGoals,
			Info1, Info2),

			% Make variables to hold any existentially
			% quantified typeclass_infos in the call,
			% insert them into the typeclass_info map
		apply_rec_subst_to_constraint_list(TypeSubst,
			ExistentialConstraints1, ExistentialConstraints),
		polymorphism__make_existq_typeclass_info_vars(
			ExistentialConstraints, ExistTypeClassVars,
			ExtraExistClassGoals, Info2, Info4),

		list__append(UnivTypeClassVars, ExistTypeClassVars,
			ExtraTypeClassVars),
		
			% No need to make typeinfos for the constrained vars
		list__delete_elems(PredTypeVars1, UnivConstrainedTVars,
			PredTypeVars2),
		list__delete_elems(PredTypeVars2, ExistConstrainedTVars,
			PredTypeVars),

		term__var_list_to_term_list(PredTypeVars, PredTypes0),
		term__apply_rec_substitution_to_list(PredTypes0, TypeSubst,
			PredTypes),

		polymorphism__make_type_info_vars(PredTypes,
			Context, ExtraTypeInfoVars, ExtraTypeInfoGoals,
			Info4, Info),
		list__append(ExtraTypeClassVars, ArgVars0, ArgVars1),
		list__append(ExtraTypeInfoVars, ArgVars1, ArgVars),
		ExtraGoals = ExtraTypeClassGoals ++ ExtraExistClassGoals
				++ ExtraTypeInfoGoals,
		ExtraVars = ExtraTypeClassVars ++ ExtraTypeInfoVars,

		%
		% update the non-locals
		%
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		set__insert_list(NonLocals0, ExtraVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo)
	).

:- pred polymorphism__update_typeclass_infos(list(class_constraint),
		list(prog_var), poly_info, poly_info).
:- mode polymorphism__update_typeclass_infos(in, in, in, out) is det.

polymorphism__update_typeclass_infos(Constraints, Vars, Info0, Info) :-
	poly_info_get_typeclass_info_map(Info0, TypeClassInfoMap0),
	insert_typeclass_info_locns( Constraints, Vars, TypeClassInfoMap0, 
		TypeClassInfoMap),
	poly_info_set_typeclass_info_map(TypeClassInfoMap, Info0, Info).

:- pred insert_typeclass_info_locns(list(class_constraint), list(prog_var), 
	map(class_constraint, prog_var), map(class_constraint, prog_var)).
:- mode insert_typeclass_info_locns(in, in, in, out) is det.

insert_typeclass_info_locns([], [], TypeClassInfoMap, TypeClassInfoMap).
insert_typeclass_info_locns([C|Cs], [V|Vs], TypeClassInfoMap0, 
		TypeClassInfoMap) :-
	map__set(TypeClassInfoMap0, C, V, TypeClassInfoMap1),
	insert_typeclass_info_locns(Cs, Vs, 
	TypeClassInfoMap1, TypeClassInfoMap).
insert_typeclass_info_locns([], [_|_], _, _) :-
	error("polymorphism:insert_typeclass_info_locns").
insert_typeclass_info_locns([_|_], [], _, _) :-
	error("polymorphism:insert_typeclass_info_locns").

%-----------------------------------------------------------------------------%

:- pred polymorphism__fixup_quantification(list(prog_var), existq_tvars,
			hlds_goal, hlds_goal, poly_info, poly_info).
:- mode polymorphism__fixup_quantification(in, in, in, out, in, out) is det.

%
% If the pred we are processing is a polymorphic predicate,
% or contains polymorphically-typed goals, we
% may need to fix up the quantification (non-local variables) of the goal
% so that it includes the extra type-info variables and type-class-info
% variables that we added to the headvars or the arguments of 
% existentially typed predicate calls, function calls and deconstruction
% unifications.
%
% Type(class)-infos for ground types added to predicate calls, function calls
% and existentially typed construction unifications do not require
% requantification because they are local to the conjunction containing
% the type(class)-info construction and the goal which uses the
% type(class)-info. The non-locals for those goals are adjusted by
% the code which creates/alters them.
%

polymorphism__fixup_quantification(HeadVars, ExistQVars, Goal0, Goal,
		Info0, Info) :-
	( 
		% optimize common case
		ExistQVars = [],
		poly_info_get_type_info_map(Info0, TypeVarMap),
		map__is_empty(TypeVarMap)
	->
		Info = Info0,
		Goal = Goal0
	;
		poly_info_get_varset(Info0, VarSet0),
		poly_info_get_var_types(Info0, VarTypes0),
		set__list_to_set(HeadVars, OutsideVars),
		implicitly_quantify_goal(Goal0, VarSet0, VarTypes0,
			OutsideVars, Goal, VarSet, VarTypes, _Warnings),
		poly_info_set_varset_and_types(VarSet, VarTypes, Info0, Info)
	).

:- pred polymorphism__fixup_lambda_quantification(hlds_goal,
		list(prog_var), list(prog_var), existq_tvars,
		hlds_goal, set(prog_var), poly_info, poly_info).
:- mode polymorphism__fixup_lambda_quantification(in, in, in, in, out, out,
		in, out) is det.

%
% If the lambda goal we are processing is polymorphically typed,
% may need to fix up the quantification (non-local variables)
% so that it includes the type-info variables and type-class-info
% variables for any polymorphically typed variables in the non-locals set
% or in the arguments (either the lambda vars or the implicit curried
% argument variables).  Including typeinfos for arguments which are
% not in the non-locals set of the goal, i.e. unused arguments, is
% necessary only if typeinfo_liveness is set, but we do it always,
% since we don't have the options available here, and the since
% cost is pretty minimal.
%

polymorphism__fixup_lambda_quantification(Goal0, ArgVars, LambdaVars,
		ExistQVars, Goal, NewOutsideVars, Info0, Info) :-
	poly_info_get_type_info_map(Info0, TypeVarMap),
	poly_info_get_typeclass_info_map(Info0, TypeClassVarMap),
	( map__is_empty(TypeVarMap) ->
		set__init(NewOutsideVars),
		Info = Info0,
		Goal = Goal0
	;
		poly_info_get_varset(Info0, VarSet0),
		poly_info_get_var_types(Info0, VarTypes0),
		Goal0 = _ - GoalInfo0,
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		set__insert_list(NonLocals, ArgVars, NonLocalsPlusArgs0),
		set__insert_list(NonLocalsPlusArgs0, LambdaVars,
			NonLocalsPlusArgs),
		goal_util__extra_nonlocal_typeinfos(TypeVarMap,
			TypeClassVarMap, VarTypes0, ExistQVars,
			NonLocalsPlusArgs, NewOutsideVars),
		set__union(NonLocals, NewOutsideVars, OutsideVars),
		implicitly_quantify_goal(Goal0, VarSet0, VarTypes0,
			OutsideVars, Goal, VarSet, VarTypes, _Warnings),
		poly_info_set_varset_and_types(VarSet, VarTypes, Info0, Info)
	).

%-----------------------------------------------------------------------------%

% Given the list of constraints for a called predicate, create a list of
% variables to hold the typeclass_info for those constraints,
% and create a list of goals to initialize those typeclass_info variables
% to the appropriate typeclass_info structures for the constraints.
% 
% Constraints which are already in the TypeClassInfoMap are assumed to
% have already had their typeclass_infos initialized; for them, we
% just return the variable in the TypeClassInfoMap.

:- pred polymorphism__make_typeclass_info_vars(list(class_constraint),
	existq_tvars, prog_context,
	list(prog_var), list(hlds_goal),
	poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_vars(in, in, in,
	out, out, in, out) is det.

polymorphism__make_typeclass_info_vars(PredClassContext,
		ExistQVars, Context,
		ExtraVars, ExtraGoals, Info0, Info) :-

		% initialise the accumulators
	ExtraVars0 = [],
	ExtraGoals0 = [],

	SeenInstances = [],

		% do the work
	polymorphism__make_typeclass_info_vars_2(PredClassContext, 
		SeenInstances, ExistQVars, Context,
		ExtraVars0, ExtraVars1, 
		ExtraGoals0, ExtraGoals1,
		Info0, Info),
	
		% We build up the vars and goals in reverse order
	list__reverse(ExtraVars1, ExtraVars),
	list__reverse(ExtraGoals1, ExtraGoals).

% Accumulator version of the above.
:- pred polymorphism__make_typeclass_info_vars_2(
	list(class_constraint),
	list(class_constraint),
	existq_tvars, prog_context,
	list(prog_var), list(prog_var), 
	list(hlds_goal), list(hlds_goal), 
	poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_vars_2(in, in, in, in,
	in, out, in, out, in, out) is det.

polymorphism__make_typeclass_info_vars_2([], _Seen, _ExistQVars,
		_Context, ExtraVars, ExtraVars, 
		ExtraGoals, ExtraGoals, 
		Info, Info).
polymorphism__make_typeclass_info_vars_2([C|Cs], Seen, ExistQVars,
		Context, ExtraVars0, ExtraVars,
		ExtraGoals0, ExtraGoals, 
		Info0, Info) :-
	polymorphism__make_typeclass_info_var(C, [C | Seen], ExistQVars,
			Context, ExtraGoals0, ExtraGoals1, 
			Info0, Info1, MaybeExtraVar),
	maybe_insert_var(MaybeExtraVar, ExtraVars0, ExtraVars1),
	polymorphism__make_typeclass_info_vars_2(Cs, Seen,
			ExistQVars, Context, 
			ExtraVars1, ExtraVars,
			ExtraGoals1, ExtraGoals, 
			Info1, Info).

:- pred polymorphism__make_typeclass_info_var(class_constraint,
	list(class_constraint), existq_tvars, prog_context, list(hlds_goal),
	list(hlds_goal), poly_info, poly_info, maybe(prog_var)). 
:- mode polymorphism__make_typeclass_info_var(in, in, in, in, in, out,
	in, out, out) is det.

polymorphism__make_typeclass_info_var(Constraint, Seen, ExistQVars,
		Context, ExtraGoals0, ExtraGoals, 
		Info0, Info, MaybeVar) :-
	(
		map__search(Info0^typeclass_info_map, Constraint, Location)
	->
			% We already have a typeclass_info for this constraint,
			% either from a parameter to the pred or from an
			% existentially quantified goal that we have already
			% processed.

		ExtraGoals = ExtraGoals0,
		Var = Location,
		MaybeVar = yes(Var),
		Info = Info0
	;
			% We don't have the typeclass_info, we must either have
			% a proof that tells us how to make it, or it will be
			% produced by an existentially typed goal that we
			% will process later on.

		map__search(Info0^proof_map, Constraint, Proof)
	->
		polymorphism__make_typeclass_info_from_proof(Constraint, Seen,
			Proof, ExistQVars, Context, MaybeVar, ExtraGoals0,
			ExtraGoals, Info0, Info)
	;
		polymorphism__make_typeclass_info_head_var(Constraint,
			NewVar, Info0, Info1),
		map__det_insert(Info1^typeclass_info_map, Constraint, NewVar,
			NewTypeClassInfoMap),
		Info = (Info1^typeclass_info_map := NewTypeClassInfoMap),
		MaybeVar = yes(NewVar),
		ExtraGoals = ExtraGoals0
	).

:- pred polymorphism__make_typeclass_info_from_proof(class_constraint,
	list(class_constraint), constraint_proof, existq_tvars, prog_context,
	maybe(prog_var), list(hlds_goal), list(hlds_goal), poly_info,
	poly_info).
:- mode polymorphism__make_typeclass_info_from_proof(in, in, in, in, in, out, 
	in, out, in, out) is det.

polymorphism__make_typeclass_info_from_proof(Constraint, Seen, Proof,
		ExistQVars, Context, MaybeVar, ExtraGoals0, ExtraGoals,
		Info0, Info) :-
	Info0 = poly_info(VarSet0, VarTypes0, TypeVarSet, TypeInfoMap0, 
		TypeClassInfoMap0, Proofs, PredName, ModuleInfo),
	Constraint = constraint(ClassName, ConstrainedTypes),
	list__length(ConstrainedTypes, ClassArity),
	ClassId = class_id(ClassName, ClassArity),
	(
			% We have to construct the typeclass_info
			% using an instance declaration
		Proof = apply_instance(InstanceNum),

		module_info_instances(ModuleInfo, InstanceTable),
		map__lookup(InstanceTable, ClassId, InstanceList),
		list__index1_det(InstanceList, InstanceNum,
			ProofInstanceDefn),

		ProofInstanceDefn = hlds_instance_defn(_, _, _,
			InstanceConstraints0, InstanceTypes0, _, _, 
			InstanceTVarset, SuperClassProofs0),

		term__vars_list(InstanceTypes0, InstanceTvars),
		get_unconstrained_tvars(InstanceTvars,
			InstanceConstraints0, UnconstrainedTvars0),

			% We can ignore the typevarset because all the
			% type variables that are created are bound
			% when we call type_list_subsumes then apply
			% the resulting bindings.
		varset__merge_subst(TypeVarSet, InstanceTVarset,
			_NewTVarset, RenameSubst),
		term__apply_substitution_to_list(InstanceTypes0,
			RenameSubst, InstanceTypes),
		(
			type_list_subsumes(InstanceTypes,
				ConstrainedTypes, InstanceSubst0)
		->
			InstanceSubst = InstanceSubst0
		;
			error("poly: wrong instance decl")
		),

		apply_subst_to_constraint_list(RenameSubst,
			InstanceConstraints0, InstanceConstraints1),
		apply_rec_subst_to_constraint_list(InstanceSubst,
			InstanceConstraints1, InstanceConstraints2),
		InstanceConstraints =
			InstanceConstraints2 `list__delete_elems` Seen,
		apply_subst_to_constraint_proofs(RenameSubst,
			SuperClassProofs0, SuperClassProofs1),
		apply_rec_subst_to_constraint_proofs(InstanceSubst,
			SuperClassProofs1, SuperClassProofs2),

		term__var_list_to_term_list(UnconstrainedTvars0,
			UnconstrainedTypes0),
		term__apply_substitution_to_list(UnconstrainedTypes0,
			RenameSubst, UnconstrainedTypes1),
		term__apply_rec_substitution_to_list(
			UnconstrainedTypes1, InstanceSubst, 
			UnconstrainedTypes),

		map__overlay(Proofs, SuperClassProofs2,
			SuperClassProofs),

			% Make the type_infos for the types
			% that are constrained by this. These
			% are packaged in the typeclass_info
		polymorphism__make_type_info_vars(
			ConstrainedTypes, Context, 
			InstanceExtraTypeInfoVars, TypeInfoGoals,
			Info0, Info1),

			% Make the typeclass_infos for the
			% constraints from the context of the
			% instance decl.
		polymorphism__make_typeclass_info_vars_2(
			InstanceConstraints, Seen,
			ExistQVars, Context,
			[], InstanceExtraTypeClassInfoVars0, 
			ExtraGoals0, ExtraGoals1, 
			Info1, Info2),

			% Make the type_infos for the unconstrained
			% type variables from the head of the
			% instance declaration
		polymorphism__make_type_info_vars(
			UnconstrainedTypes, Context, 
			InstanceExtraTypeInfoUnconstrainedVars, 
			UnconstrainedTypeInfoGoals,
			Info2, Info3),
		
			% The variables are built up in 
			% reverse order.
		list__reverse(InstanceExtraTypeClassInfoVars0,
			InstanceExtraTypeClassInfoVars),

		polymorphism__construct_typeclass_info(
			InstanceExtraTypeInfoUnconstrainedVars,
			InstanceExtraTypeInfoVars, 
			InstanceExtraTypeClassInfoVars, 
			ClassId, Constraint, InstanceNum,
			ConstrainedTypes,
			SuperClassProofs, ExistQVars, Var, NewGoals, 
			Info3, Info),

		MaybeVar = yes(Var),

			% Oh, yuck. The type_info goals have
			% already been reversed, so lets
			% reverse them back.
		list__reverse(TypeInfoGoals, RevTypeInfoGoals),
		list__reverse(UnconstrainedTypeInfoGoals, 
			RevUnconstrainedTypeInfoGoals),

		list__condense([RevUnconstrainedTypeInfoGoals, 
			NewGoals, ExtraGoals1, RevTypeInfoGoals],
			ExtraGoals)
	;
			% We have to extract the typeclass_info from
			% another one
		Proof = superclass(SubClassConstraint),

			% First create a variable to hold the new
			% typeclass_info 
		unqualify_name(ClassName, ClassNameString),
		polymorphism__new_typeclass_info_var(VarSet0,
			VarTypes0, Constraint, ClassNameString,
			Var, VarSet1, VarTypes1),

		MaybeVar = yes(Var),

			% Then work out where to extract it from
		SubClassConstraint = 
			constraint(SubClassName, SubClassTypes),
		list__length(SubClassTypes, SubClassArity),
		SubClassId = class_id(SubClassName, SubClassArity),

		Info1 = poly_info(VarSet1, VarTypes1, TypeVarSet, 
			TypeInfoMap0, TypeClassInfoMap0, Proofs, 
			PredName, ModuleInfo),

			% Make the typeclass_info for the subclass
		polymorphism__make_typeclass_info_var(
			SubClassConstraint, Seen,
			ExistQVars, Context,
			ExtraGoals0, ExtraGoals1, 
			Info1, Info2,
			MaybeSubClassVar), 
		( MaybeSubClassVar = yes(SubClassVar0) ->
			SubClassVar = SubClassVar0
		;
			error("MaybeSubClassVar = no")
		),

			% Look up the definition of the subclass
		module_info_classes(ModuleInfo, ClassTable),
		map__lookup(ClassTable, SubClassId, SubClassDefn), 
		SubClassDefn = hlds_class_defn(_, SuperClasses0,
			SubClassVars, _, _, _, _),

			% Work out which superclass typeclass_info to
			% take
		map__from_corresponding_lists(SubClassVars,
			SubClassTypes, SubTypeSubst),
		apply_subst_to_constraint_list(SubTypeSubst,
			SuperClasses0, SuperClasses),
		(
			list__nth_member_search(SuperClasses,
				Constraint, SuperClassIndex0)
		->
			SuperClassIndex0 = SuperClassIndex
		;
				% We shouldn't have got this far if
				% the constraints were not satisfied
			error("polymorphism.m: constraint not in constraint list")
		),

		poly_info_get_varset(Info2, VarSet2),
		poly_info_get_var_types(Info2, VarTypes2),
		polymorphism__make_count_var(SuperClassIndex, VarSet2,
			VarTypes2, IndexVar, IndexGoal, VarSet,
			VarTypes),
		poly_info_set_varset_and_types(VarSet, VarTypes,
			Info2, Info),

			% We extract the superclass typeclass_info by
			% inserting a call to
			% superclass_from_typeclass_info in
			% private_builtin.
			% Note that superclass_from_typeclass_info
			% does not need extra type_info arguments
			% even though its declaration is polymorphic.

			% Make the goal for the call
		varset__init(DummyTVarSet0),
		varset__new_var(DummyTVarSet0, TCVar,
			DummyTVarSet),
		mercury_private_builtin_module(PrivateBuiltin),
		ExtractSuperClass = qualified(PrivateBuiltin, 
				  "superclass_from_typeclass_info"),
		construct_type(qualified(PrivateBuiltin,
			"typeclass_info") - 1,
			[term__variable(TCVar)],
			TypeClassInfoType),
		construct_type(unqualified("int") - 0, [], IntType),
		get_pred_id_and_proc_id(ExtractSuperClass, predicate, 
			DummyTVarSet, 
			[TypeClassInfoType, IntType, TypeClassInfoType],
			ModuleInfo, PredId, ProcId),
		Call = call(PredId, ProcId, 
			[SubClassVar, IndexVar, Var],
			not_builtin, no, 
			ExtractSuperClass
			),

			% Make the goal info for the call
		set__list_to_set([SubClassVar, IndexVar, Var],
			NonLocals),
		goal_info_init(GoalInfo0),
		goal_info_set_nonlocals(GoalInfo0, NonLocals,
			GoalInfo),

			% Put them together
		SuperClassGoal = Call - GoalInfo,

			% Add it to the accumulator
		ExtraGoals = [SuperClassGoal,IndexGoal|ExtraGoals1]
	).

:- pred polymorphism__construct_typeclass_info(list(prog_var), list(prog_var), 
	list(prog_var), class_id, class_constraint, int, 
	list(type), map(class_constraint, constraint_proof),
	existq_tvars, prog_var, list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__construct_typeclass_info(in, in, in, in, in, 
	in, in, in, in, out, out, in, out) is det.

polymorphism__construct_typeclass_info(ArgUnconstrainedTypeInfoVars, 
		ArgTypeInfoVars, ArgTypeClassInfoVars,
		ClassId, Constraint, InstanceNum,
		InstanceTypes, SuperClassProofs, ExistQVars,
		NewVar, NewGoals, Info0, Info) :-

	poly_info_get_module_info(Info0, ModuleInfo),

	module_info_classes(ModuleInfo, ClassTable),
	map__lookup(ClassTable, ClassId, ClassDefn),

	polymorphism__get_arg_superclass_vars(ClassDefn, InstanceTypes,
		SuperClassProofs, ExistQVars, ArgSuperClassVars,
		SuperClassGoals, Info0, Info1),

	poly_info_get_varset(Info1, VarSet0),
	poly_info_get_var_types(Info1, VarTypes0),

		% lay out the argument variables as expected in the
		% typeclass_info
	list__append(ArgTypeClassInfoVars, ArgSuperClassVars, ArgVars0),
	list__append(ArgVars0, ArgTypeInfoVars, ArgVars1),
	list__append(ArgUnconstrainedTypeInfoVars, ArgVars1, ArgVars),

	ClassId = class_id(ClassName, _Arity),

	unqualify_name(ClassName, ClassNameString),
	polymorphism__new_typeclass_info_var(VarSet0, VarTypes0,
		Constraint, ClassNameString, BaseVar, VarSet1, VarTypes1),

	module_info_instances(ModuleInfo, InstanceTable),
	map__lookup(InstanceTable, ClassId, InstanceList),
	list__index1_det(InstanceList, InstanceNum, InstanceDefn),
	InstanceDefn = hlds_instance_defn(InstanceModuleName,
		_, _, _, _, _, _, _, _),
	base_typeclass_info__make_instance_string(InstanceTypes,
		InstanceString),
	ConsId = base_typeclass_info_const(InstanceModuleName, ClassId,
		InstanceNum, InstanceString),
	BaseTypeClassInfoTerm = functor(ConsId, no, []),

		% create the construction unification to initialize the variable
	RLExprnId = no,
	BaseUnification = construct(BaseVar, ConsId, [], [],
			construct_dynamically, cell_is_shared, RLExprnId),
	BaseUnifyMode = (free -> ground(shared, none)) -
			(ground(shared, none) -> ground(shared, none)),
	BaseUnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	BaseUnify = unify(BaseVar, BaseTypeClassInfoTerm, BaseUnifyMode,
			BaseUnification, BaseUnifyContext),

		% create a goal_info for the unification
	set__list_to_set([BaseVar], NonLocals),
	instmap_delta_from_assoc_list([BaseVar - ground(shared, none)],
		InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, det, BaseGoalInfo),

	BaseGoal = BaseUnify - BaseGoalInfo,

		% build a unification to add the argvars to the
		% base_typeclass_info
	mercury_private_builtin_module(PrivateBuiltin),
	NewConsId = cons(qualified(PrivateBuiltin, "typeclass_info"), 1),
	NewArgVars = [BaseVar|ArgVars],
	TypeClassInfoTerm = functor(NewConsId, no, NewArgVars),

		% introduce a new variable
	polymorphism__new_typeclass_info_var(VarSet1, VarTypes1,
		Constraint, ClassNameString, NewVar, VarSet, VarTypes),

		% create the construction unification to initialize the
		% variable
	UniMode = (free - ground(shared, none) ->
		   ground(shared, none) - ground(shared, none)),
	list__length(NewArgVars, NumArgVars),
	list__duplicate(NumArgVars, UniMode, UniModes),
	Unification = construct(NewVar, NewConsId, NewArgVars,
		UniModes, construct_dynamically, cell_is_unique, RLExprnId),
	UnifyMode = (free -> ground(shared, none)) -
			(ground(shared, none) -> ground(shared, none)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(NewVar, TypeClassInfoTerm, UnifyMode,
			Unification, UnifyContext),

	% create a goal_info for the unification
	goal_info_init(GoalInfo0),
	set__list_to_set([NewVar | NewArgVars], TheNonLocals),
	goal_info_set_nonlocals(GoalInfo0, TheNonLocals, GoalInfo1),
	list__duplicate(NumArgVars, ground(shared, none), ArgInsts),
		% note that we could perhaps be more accurate than
		% `ground(shared)', but it shouldn't make any
		% difference.
	InstConsId = cons(qualified(PrivateBuiltin, "typeclass_info"), 
		NumArgVars),
	instmap_delta_from_assoc_list(
		[NewVar - 
			bound(unique, [functor(InstConsId, ArgInsts)])],
		InstMapDelta),
	goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo2),
	goal_info_set_determinism(GoalInfo2, det, GoalInfo),

	TypeClassInfoGoal = Unify - GoalInfo,
	NewGoals0 = [TypeClassInfoGoal, BaseGoal],
	list__append(NewGoals0, SuperClassGoals, NewGoals),
	poly_info_set_varset_and_types(VarSet, VarTypes, Info1, Info).

%---------------------------------------------------------------------------%

:- pred polymorphism__get_arg_superclass_vars(hlds_class_defn, list(type),
	map(class_constraint, constraint_proof), existq_tvars,
	list(prog_var), list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__get_arg_superclass_vars(in, in, in, in, out, out, 
	in, out) is det.

polymorphism__get_arg_superclass_vars(ClassDefn, InstanceTypes, 
		SuperClassProofs, ExistQVars, NewVars, NewGoals,
		Info0, Info) :-

	poly_info_get_proofs(Info0, Proofs),

	poly_info_get_typevarset(Info0, TVarSet0),
	ClassDefn = hlds_class_defn(_, SuperClasses0, ClassVars0, 
		_, _, ClassTVarSet, _),
	varset__merge_subst(TVarSet0, ClassTVarSet, TVarSet1, Subst),
	poly_info_set_typevarset(TVarSet1, Info0, Info1),

	map__apply_to_list(ClassVars0, Subst, ClassVars1),
	term__vars_list(ClassVars1, ClassVars),
	map__from_corresponding_lists(ClassVars, InstanceTypes, TypeSubst),

	apply_subst_to_constraint_list(Subst, SuperClasses0, SuperClasses1),
	apply_rec_subst_to_constraint_list(TypeSubst, SuperClasses1,
		SuperClasses),

	poly_info_set_proofs(SuperClassProofs, Info1, Info2),
	polymorphism__make_superclasses_from_proofs(SuperClasses,
		ExistQVars, [], NewGoals, Info2, Info3,
		[], NewVars),

	poly_info_set_proofs(Proofs, Info3, Info).


:- pred polymorphism__make_superclasses_from_proofs(list(class_constraint), 
	existq_tvars, list(hlds_goal), list(hlds_goal), 
	poly_info, poly_info, list(prog_var), list(prog_var)).
:- mode polymorphism__make_superclasses_from_proofs(in, in, in, out, 
	in, out, in, out) is det.

polymorphism__make_superclasses_from_proofs([], _,
		Goals, Goals, Info, Info, Vars, Vars).
polymorphism__make_superclasses_from_proofs([C|Cs],
		ExistQVars, Goals0, Goals, Info0, Info, Vars0, Vars) :-
	polymorphism__make_superclasses_from_proofs(Cs,
		ExistQVars, Goals0, Goals1, Info0, Info1, Vars0, Vars1),
	term__context_init(Context),
	polymorphism__make_typeclass_info_var(C, [],
		ExistQVars, Context, Goals1, Goals, Info1, Info,
		MaybeVar),
	maybe_insert_var(MaybeVar, Vars1, Vars).

:- pred maybe_insert_var(maybe(prog_var), list(prog_var), list(prog_var)).
:- mode maybe_insert_var(in, in, out) is det.
maybe_insert_var(no, Vars, Vars).
maybe_insert_var(yes(Var), Vars, [Var | Vars]).

%-----------------------------------------------------------------------------%

	% Produce the typeclass_infos for the existential class
	% constraints for a call or deconstruction unification.
:- pred polymorphism__make_existq_typeclass_info_vars(
		list(class_constraint), list(prog_var), list(hlds_goal),
		poly_info, poly_info).
:- mode polymorphism__make_existq_typeclass_info_vars(in, out, out,
		in, out) is det.

polymorphism__make_existq_typeclass_info_vars(
		ExistentialConstraints, ExtraTypeClassVars,
		ExtraGoals, PolyInfo0, PolyInfo) :-
	poly_info_get_type_info_map(PolyInfo0, OldTypeInfoMap),
	polymorphism__make_typeclass_info_head_vars(ExistentialConstraints,
		ExtraTypeClassVars, PolyInfo0, PolyInfo1),
	polymorphism__update_typeclass_infos(ExistentialConstraints,
		ExtraTypeClassVars, PolyInfo1, PolyInfo2),

	constraint_list_get_tvars(ExistentialConstraints, TVars0),
	list__sort_and_remove_dups(TVars0, TVars),
	list__foldl2(polymorphism__maybe_extract_type_info(OldTypeInfoMap),
		TVars, [], ExtraGoals, PolyInfo2, PolyInfo).

	% For code which requires mode reordering, we may have already
	% seen uses of some of the type variables produced by this call.
	% At the point of the use of a type variable that we haven't seen
	% before, we assume that it is unconstrained. If it turns out that
	% the type variable is constrained, and the type_info is contained
	% in a typeclass_info, we need to generate code to extract it here.
:- pred polymorphism__maybe_extract_type_info(type_info_varmap,
		tvar, list(hlds_goal), list(hlds_goal),
		poly_info, poly_info).
:- mode polymorphism__maybe_extract_type_info(in, in, in, out, in, out) is det.

polymorphism__maybe_extract_type_info(OldTypeInfoMap, TVar,
		ExtraGoals0, ExtraGoals, Info0, Info) :-
	poly_info_get_type_info_map(Info0, TypeInfoMap),
	(
		map__search(OldTypeInfoMap, TVar, type_info(TypeInfoVar0)),
		map__search(TypeInfoMap, TVar,
			typeclass_info(TypeClassInfoVar, Index))
	->
		extract_type_info(TVar, TypeClassInfoVar,
			Index, ExtraGoals1, TypeInfoVar1, Info0, Info),
		polymorphism__assign_var(TypeInfoVar0,
			TypeInfoVar1, AssignGoal),
		ExtraGoals = ExtraGoals1 ++ [AssignGoal | ExtraGoals0]
	;
		ExtraGoals = ExtraGoals0,
		Info = Info0
	).

%---------------------------------------------------------------------------%

% Given a list of types, create a list of variables to hold the type_info
% for those types, and create a list of goals to initialize those type_info
% variables to the appropriate type_info structures for the types.
% Update the varset and vartypes accordingly.

polymorphism__make_type_info_vars([], _, [], [], Info, Info).
polymorphism__make_type_info_vars([Type | Types], Context,
		ExtraVars, ExtraGoals, Info0, Info) :-
	polymorphism__make_type_info_var(Type, Context,
		Var, ExtraGoals1, Info0, Info1),
	polymorphism__make_type_info_vars(Types, Context,
		ExtraVars2, ExtraGoals2, Info1, Info),
	ExtraVars = [Var | ExtraVars2],
	list__append(ExtraGoals1, ExtraGoals2, ExtraGoals).

polymorphism__make_type_info_var(Type, Context, Var, ExtraGoals,
		Info0, Info) :-
	%
	% First handle statically known types
	% (i.e. types which are not type variables)
	%
	(
		( type_is_higher_order(Type, PredOrFunc, _, TypeArgs0) ->
			TypeArgs = TypeArgs0,
			hlds_out__pred_or_func_to_str(PredOrFunc,
				PredOrFuncStr),
			TypeCtor = unqualified(PredOrFuncStr) - 0
		; type_is_tuple(Type, TypeArgs1) ->
			TypeArgs = TypeArgs1,
			TypeCtor = unqualified("tuple") - 0
		;
			fail
		)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a known higher-order or tuple value of the
		% type variable.
		% The transformation we perform is basically the same as
		% in the first-order case below, except that we map
		% pred types to pred/0, func types to func/0 and tuple
		% types to tuple/0 for the purposes of creating type_infos.  
		% To allow univ_to_type to check the type_infos
		% correctly, the actual arity of the pred is added to
		% the type_info of higher-order types.
		polymorphism__construct_type_info(Type, TypeCtor, TypeArgs,
			yes, Context, Var, ExtraGoals, Info0, Info)
	;
		type_to_ctor_and_args(Type, TypeCtor, TypeArgs)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a known value of the type variable.
		% The transformation we perform is shown in the comment
		% at the top of the module.

		polymorphism__construct_type_info(Type, TypeCtor, TypeArgs,
			no, Context, Var, ExtraGoals, Info0, Info)
	;
	%
	% Now handle the cases of types which are not known statically
	% (i.e. type variables)
	%
		Type = term__variable(TypeVar)
	->
		get_type_info_locn(TypeVar, TypeInfoLocn, Info0, Info1),
		get_type_info(TypeInfoLocn, TypeVar, ExtraGoals, Var,
				Info1, Info)
	;
		error("polymorphism__make_var: unknown type")
	).

:- pred get_type_info_locn(tvar, type_info_locn, poly_info, poly_info).
:- mode get_type_info_locn(in, out, in, out) is det.

get_type_info_locn(TypeVar, TypeInfoLocn, Info0, Info) :-
	%
	% If we have already allocated a location for this type_info,
	% then all we need to do is to extract the type_info variable
	% from its location.
	%
	poly_info_get_type_info_map(Info0, TypeInfoMap0),
	( map__search(TypeInfoMap0, TypeVar, TypeInfoLocn0) ->
		TypeInfoLocn = TypeInfoLocn0,
		Info = Info0
	;
		%
		% Otherwise, we need to create a new type_info
		% variable, and set the location for this type
		% variable to be that type_info variable.
		%
		% This is wrong if the type variable is one of
		% the existentially quantified variables of a called
		% predicate and the variable occurs in an existential
		% type-class constraint. In that case the type-info
		% will be stored in the typeclass_info variable produced
		% by the predicate, not in a type_info variable.
		% make_typeclass_info_headvar will fix this up when
		% the typeclass_info is created.
		%
		type_util__var(Type, TypeVar),
		polymorphism__new_type_info_var(Type, "type_info",
			typeinfo_prefix, Var, Info0, Info1),
		TypeInfoLocn = type_info(Var),
		map__det_insert(TypeInfoMap0, TypeVar, TypeInfoLocn,
			TypeInfoMap),
		poly_info_set_type_info_map(TypeInfoMap, Info1, Info)
	).

:- pred polymorphism__construct_type_info(type, type_ctor, list(type),
	bool, prog_context, prog_var, list(hlds_goal),
	poly_info, poly_info).
:- mode polymorphism__construct_type_info(in, in, in, in, in, out, out, 
	in, out) is det.

polymorphism__construct_type_info(Type, TypeCtor, TypeArgs,
		IsHigherOrderOrTuple, Context, Var, ExtraGoals, Info0, Info) :-

	% Create the typeinfo vars for the arguments
	polymorphism__make_type_info_vars(TypeArgs, Context,
		ArgTypeInfoVars, ArgTypeInfoGoals, Info0, Info1),

	poly_info_get_varset(Info1, VarSet1),
	poly_info_get_var_types(Info1, VarTypes1),
	poly_info_get_module_info(Info1, ModuleInfo),

	polymorphism__init_const_type_ctor_info_var(Type,
		TypeCtor, ModuleInfo, VarSet1, VarTypes1, 
		BaseVar, BaseGoal, VarSet2, VarTypes2),
	polymorphism__maybe_init_second_cell(ArgTypeInfoVars,
		ArgTypeInfoGoals, Type, IsHigherOrderOrTuple,
		BaseVar, VarSet2, VarTypes2, [BaseGoal],
		Var, VarSet, VarTypes, ExtraGoals),

	poly_info_set_varset_and_types(VarSet, VarTypes, Info1, Info).

		% Create a unification for the two-cell type_info
		% variable for this type if the type arity is not zero:
		%	TypeInfoVar = type_info(BaseVar,
		%				ArgTypeInfoVars...).
		% For closures, we add the actual arity before the
		% arguments, because all closures have a BaseVar
		% of "pred/0".
		% 	TypeInfoVar = type_info(BaseVar, Arity,
		% 				ArgTypeInfoVars...).

:- pred polymorphism__maybe_init_second_cell(list(prog_var), list(hlds_goal),
	type, bool, prog_var, prog_varset, map(prog_var, type), list(hlds_goal),
	prog_var, prog_varset, map(prog_var, type), list(hlds_goal)).
:- mode polymorphism__maybe_init_second_cell(in, in, in, in, in, in, in, in,
	out, out, out, out) is det.

polymorphism__maybe_init_second_cell(ArgTypeInfoVars, ArgTypeInfoGoals, Type,
		IsHigherOrderOrTuple, BaseVar, VarSet0, VarTypes0, ExtraGoals0,
		Var, VarSet, VarTypes, ExtraGoals) :-
	% Unfortunately, if we have higher order terms, we
	% can no longer just optimise them to be the actual
	% type_ctor_info
	( IsHigherOrderOrTuple = yes ->
		list__length(ArgTypeInfoVars, PredArity),
		polymorphism__make_count_var(PredArity,
			VarSet0, VarTypes0, ArityVar, ArityGoal,
			VarSet1, VarTypes1),
		polymorphism__init_type_info_var(Type,
			[BaseVar, ArityVar | ArgTypeInfoVars], "type_info",
			VarSet1, VarTypes1, Var, TypeInfoGoal,
			VarSet, VarTypes),
		list__append([ArityGoal |  ArgTypeInfoGoals], [TypeInfoGoal],
			ExtraGoals1),
		list__append(ExtraGoals0, ExtraGoals1, ExtraGoals)
	; ArgTypeInfoVars = [_ | _] ->
		polymorphism__init_type_info_var(Type,
			[BaseVar | ArgTypeInfoVars], "type_info",
			VarSet0, VarTypes0, Var, TypeInfoGoal,
			VarSet, VarTypes),
		list__append(ArgTypeInfoGoals, [TypeInfoGoal], ExtraGoals1),
		list__append(ExtraGoals0, ExtraGoals1, ExtraGoals)
	;
		Var = BaseVar,

		% Since this type_ctor_info is pretending to be
		% a type_info, we need to adjust its type.
		% Since type_ctor_info_const cons_ids are handled
		% specially, this should not cause problems.
		polymorphism__build_type_info_type(Type, NewBaseVarType),
		map__det_update(VarTypes0, BaseVar, NewBaseVarType, VarTypes),

		VarSet = VarSet0,
		ExtraGoals = ExtraGoals0
	).

	% Create a unification `CountVar = <NumTypeArgs>'

:- pred polymorphism__make_count_var(int, prog_varset, map(prog_var, type),
	prog_var, hlds_goal, prog_varset, map(prog_var, type)).
:- mode polymorphism__make_count_var(in, in, in, out, out, out, out) is det.

polymorphism__make_count_var(NumTypeArgs, VarSet0, VarTypes0,
		CountVar, CountGoal, VarSet, VarTypes) :-
	varset__new_var(VarSet0, CountVar, VarSet1),
	varset__name_var(VarSet1, CountVar, "TypeArity", VarSet),
	construct_type(unqualified("int") - 0, [], IntType),
	map__set(VarTypes0, CountVar, IntType, VarTypes),
	polymorphism__init_with_int_constant(CountVar, NumTypeArgs, CountGoal).

	% Create a construction unification `Var = <Num>'
	% where Var is a freshly introduced variable and Num is an
	% integer constant.

:- pred polymorphism__init_with_int_constant(prog_var, int, hlds_goal).
:- mode polymorphism__init_with_int_constant(in, in, out) is det.

polymorphism__init_with_int_constant(CountVar, Num, CountUnifyGoal) :-

	CountConsId = int_const(Num),
	RLExprnId = no,
	CountUnification = construct(CountVar, CountConsId, [], [],
		construct_dynamically, cell_is_shared, RLExprnId),

	CountTerm = functor(CountConsId, no, []),
	CountInst = bound(unique, [functor(int_const(Num), [])]),
	CountUnifyMode = (free -> CountInst) - (CountInst -> CountInst),
	CountUnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	CountUnify = unify(CountVar, CountTerm, CountUnifyMode,
		CountUnification, CountUnifyContext),

	% create a goal_info for the unification

	set__singleton_set(CountNonLocals, CountVar),
	instmap_delta_from_assoc_list([CountVar - CountInst], InstmapDelta),
	goal_info_init(CountNonLocals, InstmapDelta, det, CountGoalInfo),

	CountUnifyGoal = CountUnify - CountGoalInfo.

polymorphism__get_special_proc(Type, SpecialPredId, ModuleInfo,
			PredName, PredId, ProcId) :-
	classify_type(Type, ModuleInfo, TypeCategory),
	( ( TypeCategory = user_type ; TypeCategory = enum_type ) ->
		module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
		( type_to_ctor_and_args(Type, TypeCtor, _TypeArgs) ->
			map__search(SpecialPredMap, SpecialPredId - TypeCtor,
				PredId)
		;
			error(
		"polymorphism__get_special_proc: type_to_ctor_and_args failed")
		),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_module(PredInfo, Module),
		pred_info_name(PredInfo, Name),
		PredName = qualified(Module, Name)
	;
		polymorphism__get_category_name(TypeCategory, CategoryName),
		special_pred_name_arity(SpecialPredId, SpecialName, _, Arity),
		string__append_list(
			["builtin_", SpecialName, "_", CategoryName], Name),
		polymorphism__get_builtin_pred_id(Name, Arity, ModuleInfo,
			PredId),
		PredName = unqualified(Name)
	),
	special_pred_mode_num(SpecialPredId, ProcInt),
	proc_id_to_int(ProcId, ProcInt).

:- pred polymorphism__get_category_name(builtin_type, string).
:- mode polymorphism__get_category_name(in, out) is det.

polymorphism__get_category_name(int_type, "int").
polymorphism__get_category_name(char_type, "int").
polymorphism__get_category_name(enum_type, "int").
polymorphism__get_category_name(float_type, "float").
polymorphism__get_category_name(str_type, "string").
polymorphism__get_category_name(pred_type, "pred").
polymorphism__get_category_name(tuple_type, "tuple").
polymorphism__get_category_name(polymorphic_type, _) :-
	error("polymorphism__get_category_name: polymorphic type").
polymorphism__get_category_name(user_type, _) :-
	error("polymorphism__get_category_name: user_type").

	% find the builtin predicate with the specified name

:- pred polymorphism__get_builtin_pred_id(string, int, module_info, pred_id).
:- mode polymorphism__get_builtin_pred_id(in, in, in, out) is det.

polymorphism__get_builtin_pred_id(Name, Arity, ModuleInfo, PredId) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
		mercury_private_builtin_module(PrivateBuiltin),
		predicate_table_search_pred_m_n_a(PredicateTable,
			PrivateBuiltin, Name, Arity, [PredId1])
	->
		PredId = PredId1
	;
		error("polymorphism__get_builtin_pred_id: pred_id lookup failed")
	).

	% Create a unification for a type_info or type_ctor_info variable:
	%
	%	TypeInfoVar = type_info(CountVar,
	%				SpecialPredVars...,
	%				ArgTypeInfoVars...)
	%
	% or
	%
	%	TypeCtorInfoVar = type_ctor_info(CountVar,
	%				SpecialPredVars...)
	%
	% These unifications WILL lead to the creation of cells on the
	% heap at runtime.

:- pred polymorphism__init_type_info_var(type, list(prog_var), string,
	prog_varset, map(prog_var, type), prog_var, hlds_goal, prog_varset,
	map(prog_var, type)).
:- mode polymorphism__init_type_info_var(in, in, in, in, in, out, out, out, out)
	is det.

polymorphism__init_type_info_var(Type, ArgVars, Symbol, VarSet0, VarTypes0,
			TypeInfoVar, TypeInfoGoal, VarSet, VarTypes) :-

	mercury_private_builtin_module(PrivateBuiltin),
	ConsId = cons(qualified(PrivateBuiltin, Symbol), 1),
	TypeInfoTerm = functor(ConsId, no, ArgVars),

	% introduce a new variable
	polymorphism__new_type_info_var_raw(Type, Symbol, typeinfo_prefix,
		VarSet0, VarTypes0, TypeInfoVar, VarSet, VarTypes),

	% create the construction unification to initialize the variable
	UniMode = (free - ground(shared, none) ->
		   ground(shared, none) - ground(shared, none)),
	list__length(ArgVars, NumArgVars),
	list__duplicate(NumArgVars, UniMode, UniModes),
	RLExprnId = no,
	Unification = construct(TypeInfoVar, ConsId, ArgVars, UniModes,
			construct_dynamically, cell_is_unique, RLExprnId),
	UnifyMode = (free -> ground(shared, none)) -
			(ground(shared, none) -> ground(shared, none)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(TypeInfoVar, TypeInfoTerm, UnifyMode,
			Unification, UnifyContext),

	% create a goal_info for the unification
	set__list_to_set([TypeInfoVar | ArgVars], NonLocals),
	list__duplicate(NumArgVars, ground(shared, none), ArgInsts),
		% note that we could perhaps be more accurate than
		% `ground(shared)', but it shouldn't make any
		% difference.
	InstConsId = cons(qualified(PrivateBuiltin, Symbol), NumArgVars),
	instmap_delta_from_assoc_list(
		[TypeInfoVar - bound(unique, [functor(InstConsId, ArgInsts)])],
		InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, GoalInfo),

	TypeInfoGoal = Unify - GoalInfo.

	% Create a unification for a type_info or type_ctor_info variable:
	%
	%	TypeCtorInfoVar = type_ctor_info(CountVar,
	%				SpecialPredVars...)
	%
	% This unification will NOT lead to the creation of a cell on the
	% heap at runtime; it will cause TypeCtorInfoVar to refer to the
	% statically allocated type_ctor_info cell for the type, allocated
	% in the module that defines the type.

:- pred polymorphism__init_const_type_ctor_info_var(type, type_ctor,
	module_info, prog_varset, map(prog_var, type), prog_var, hlds_goal,
	prog_varset, map(prog_var, type)).
:- mode polymorphism__init_const_type_ctor_info_var(in, in, in, in, in,
	out, out, out, out) is det.

polymorphism__init_const_type_ctor_info_var(Type, TypeCtor,
		ModuleInfo, VarSet0, VarTypes0, TypeCtorInfoVar,
		TypeCtorInfoGoal, VarSet, VarTypes) :-

	type_util__type_ctor_module(ModuleInfo, TypeCtor, ModuleName),
	type_util__type_ctor_name(ModuleInfo, TypeCtor, TypeName),
	TypeCtor = _ - Arity,
	ConsId = type_ctor_info_const(ModuleName, TypeName, Arity),
	TypeInfoTerm = functor(ConsId, no, []),

	% introduce a new variable
	polymorphism__new_type_info_var_raw(Type, "type_ctor_info",
		typectorinfo_prefix, VarSet0, VarTypes0,
		TypeCtorInfoVar, VarSet, VarTypes),

	% create the construction unification to initialize the variable
	RLExprnId = no,
	Unification = construct(TypeCtorInfoVar, ConsId, [], [],
			construct_dynamically, cell_is_shared, RLExprnId),
	UnifyMode = (free -> ground(shared, none)) -
			(ground(shared, none) -> ground(shared, none)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(TypeCtorInfoVar, TypeInfoTerm, UnifyMode,
			Unification, UnifyContext),

	% create a goal_info for the unification
	set__list_to_set([TypeCtorInfoVar], NonLocals),
	instmap_delta_from_assoc_list([TypeCtorInfoVar - ground(shared, none)],
		InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, det, GoalInfo),

	TypeCtorInfoGoal = Unify - GoalInfo.

%---------------------------------------------------------------------------%

:- pred polymorphism__make_head_vars(list(tvar), tvarset, list(prog_var),
				poly_info, poly_info).
:- mode polymorphism__make_head_vars(in, in, out, in, out) is det.

polymorphism__make_head_vars([], _, []) --> [].
polymorphism__make_head_vars([TypeVar|TypeVars], TypeVarSet, TypeInfoVars) -->
	{ Type = term__variable(TypeVar) },
	polymorphism__new_type_info_var(Type, "type_info", typeinfo_prefix,
		Var),
	( { varset__search_name(TypeVarSet, TypeVar, TypeVarName) } ->
		=(Info0),
		{ poly_info_get_varset(Info0, VarSet0) },
		{ string__append("TypeInfo_for_", TypeVarName, VarName) },
		{ varset__name_var(VarSet0, Var, VarName, VarSet) },
		poly_info_set_varset(VarSet)
	;
		[]
	),
	{ TypeInfoVars = [Var | TypeInfoVars1] },
	polymorphism__make_head_vars(TypeVars, TypeVarSet, TypeInfoVars1).

:- pred polymorphism__new_type_info_var(type, string, string, prog_var,
	poly_info, poly_info).
:- mode polymorphism__new_type_info_var(in, in, in, out, in, out) is det.

polymorphism__new_type_info_var(Type, Symbol, Prefix, Var, Info0, Info) :-
	poly_info_get_varset(Info0, VarSet0),
	poly_info_get_var_types(Info0, VarTypes0),
	polymorphism__new_type_info_var_raw(Type, Symbol, Prefix,
		VarSet0, VarTypes0, Var, VarSet, VarTypes),
	poly_info_set_varset_and_types(VarSet, VarTypes, Info0, Info).

:- pred polymorphism__new_type_info_var_raw(type, string, string, prog_varset,
	map(prog_var, type), prog_var, prog_varset, map(prog_var, type)).
:- mode polymorphism__new_type_info_var_raw(in, in, in, in, in, out, out, out)
	is det.

polymorphism__new_type_info_var_raw(Type, Symbol, Prefix, VarSet0, VarTypes0,
		Var, VarSet, VarTypes) :-
	% introduce new variable
	varset__new_var(VarSet0, Var, VarSet1),
	term__var_to_int(Var, VarNum),
	string__int_to_string(VarNum, VarNumStr),
	string__append(Prefix, VarNumStr, Name),
	varset__name_var(VarSet1, Var, Name, VarSet),
	polymorphism__build_type_info_type(Symbol, Type, TypeInfoType),
	map__set(VarTypes0, Var, TypeInfoType, VarTypes).

:- func typeinfo_prefix = string.

typeinfo_prefix = "TypeInfo_".

:- func typectorinfo_prefix = string.

typectorinfo_prefix = "TypeCtorInfo_".

%---------------------------------------------------------------------------%

% Generate code to get the value of a type variable.

:- pred get_type_info(type_info_locn, tvar, list(hlds_goal),
		prog_var, poly_info, poly_info).
:- mode get_type_info(in, in, out, out, in, out) is det.

get_type_info(TypeInfoLocn, TypeVar, ExtraGoals, Var, Info0, Info) :-
	(
			% If the typeinfo is available in a variable,
			% just use it
		TypeInfoLocn = type_info(TypeInfoVar),
		Var = TypeInfoVar,
		ExtraGoals = [],
		Info = Info0
	;
			% If the typeinfo is in a typeclass_info, then
			% we need to extract it before using it
		TypeInfoLocn = typeclass_info(TypeClassInfoVar, Index),
		extract_type_info(TypeVar, TypeClassInfoVar,
			Index, ExtraGoals, Var, Info0, Info)
	).

:- pred extract_type_info(tvar, prog_var, int, list(hlds_goal),
		prog_var, poly_info, poly_info).
:- mode extract_type_info(in, in, in, out, out, in, out) is det.

extract_type_info(TypeVar, TypeClassInfoVar, Index, Goals,
		TypeInfoVar, PolyInfo0, PolyInfo) :-
	poly_info_get_varset(PolyInfo0, VarSet0),
	poly_info_get_var_types(PolyInfo0, VarTypes0),
	poly_info_get_module_info(PolyInfo0, ModuleInfo),
	polymorphism__gen_extract_type_info(TypeVar, TypeClassInfoVar, Index,
		ModuleInfo, Goals, TypeInfoVar,
		VarSet0, VarTypes0, VarSet, VarTypes),
	poly_info_set_varset_and_types(VarSet, VarTypes, PolyInfo0, PolyInfo).

polymorphism__gen_extract_type_info(TypeVar, TypeClassInfoVar, Index,
		ModuleInfo, Goals, TypeInfoVar,
		VarSet0, VarTypes0, VarSet, VarTypes) :-

		% We need a tvarset to pass to get_pred_id_and_proc_id
	varset__init(DummyTVarSet0),

	mercury_private_builtin_module(PrivateBuiltin),
	ExtractTypeInfo = qualified(PrivateBuiltin,
				"type_info_from_typeclass_info"),

		% We pretend that the `constraint' field of the
		% `typeclass_info' type is a type variable for the purposes of
		% locating `private_builtin:type_info_from_typeclass_info'.
	varset__new_var(DummyTVarSet0, DummyTypeClassTVar, DummyTVarSet1),
	construct_type(qualified(PrivateBuiltin, "typeclass_info") - 1,
		[term__variable(DummyTypeClassTVar)], TypeClassInfoType),

	construct_type(unqualified("int") - 0, [], IntType),

	varset__new_var(DummyTVarSet1, DummyTVar, DummyTVarSet),
	polymorphism__build_type_info_type(term__variable(DummyTVar),
		TypeInfoType),
	get_pred_id_and_proc_id(ExtractTypeInfo, predicate, DummyTVarSet, 
		[TypeClassInfoType, IntType, TypeInfoType],
		ModuleInfo, PredId, ProcId),
	
	polymorphism__make_count_var(Index, VarSet0, VarTypes0, IndexVar,
		IndexGoal, VarSet1, VarTypes1),

	polymorphism__new_type_info_var_raw(term__variable(TypeVar),
		"type_info", typeinfo_prefix, VarSet1, VarTypes1,
		TypeInfoVar, VarSet, VarTypes),

		% Make the goal info for the call.
		% `type_info_from_typeclass_info' does not require an extra
		% type_info argument even though its declaration is
		% polymorphic.
	set__list_to_set([TypeClassInfoVar, IndexVar, TypeInfoVar], NonLocals),
	instmap_delta_from_assoc_list([TypeInfoVar - ground(shared, none)],
		InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, det, GoalInfo),

	Call = call(PredId, ProcId, 
		[TypeClassInfoVar, IndexVar, TypeInfoVar],
		not_builtin, no, ExtractTypeInfo) - GoalInfo,

	Goals = [IndexGoal, Call].

%-----------------------------------------------------------------------------%

	% Create a head var for each class constraint, and make an entry in
	% the typeinfo locations map for each constrained type var.

:- pred polymorphism__make_typeclass_info_head_vars(list(class_constraint),
		list(prog_var), poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_head_vars(in, out, in, out)
		is det.

polymorphism__make_typeclass_info_head_vars(Constraints, ExtraHeadVars) -->
	list__map_foldl(polymorphism__make_typeclass_info_head_var,
		Constraints, ExtraHeadVars).

:- pred polymorphism__make_typeclass_info_head_var(class_constraint,
		prog_var, poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_head_var(in, out, in, out) is det.

polymorphism__make_typeclass_info_head_var(C, ExtraHeadVar, Info0, Info) :-

	poly_info_get_typeclass_info_map(Info0, TypeClassInfoMap),
	(
		map__search(TypeClassInfoMap, C, ExistingVar)
	->
		ExtraHeadVar = ExistingVar,
		Info = Info0
	;
		poly_info_get_varset(Info0, VarSet0),
		poly_info_get_var_types(Info0, VarTypes0),
		poly_info_get_type_info_map(Info0, TypeInfoMap0),
		poly_info_get_module_info(Info0, ModuleInfo),

		C = constraint(ClassName0, ClassTypes),

			% Work out how many superclass the class has
		list__length(ClassTypes, ClassArity),
		ClassId = class_id(ClassName0, ClassArity),
		module_info_classes(ModuleInfo, ClassTable),
		map__lookup(ClassTable, ClassId, ClassDefn),
		ClassDefn = hlds_class_defn(_, SuperClasses, _, _, _, _, _),
		list__length(SuperClasses, NumSuperClasses),

		unqualify_name(ClassName0, ClassName),

			% Make a new variable to contain the dictionary for 
			% this typeclass constraint
		polymorphism__new_typeclass_info_var(VarSet0, VarTypes0, C,
			ClassName, ExtraHeadVar, VarSet1, VarTypes1),

			% Find all the type variables in the constraint, and
			% remember what index they appear in in the typeclass
			% info.

			% The first type_info will be just after the superclass
			% infos
		First is NumSuperClasses + 1,
		term__vars_list(ClassTypes, ClassTypeVars0),
		MakeIndex = (pred(Elem0::in, Elem::out, 
					Index0::in, Index::out) is det :-
				Elem = Elem0 - Index0,
				Index is Index0 + 1,
				% the following call is a work-around for a
				% compiler bug with intermodule optimization: 
				% it is needed to resolve a type ambiguity
				is_pair(Elem)
			),
		list__map_foldl(MakeIndex, ClassTypeVars0, ClassTypeVars, 
			First, _),
			

			% Work out which type variables we haven't seen
			% before, or which we assumed earlier would be
			% produced in a type-info (this can happen for
			% code which needs mode reordering and which calls
			% existentially quantified predicates or
			% deconstructs existentially quantified terms).
		IsNew = (pred(TypeVar0::in) is semidet :-
				TypeVar0 = TypeVar - _Index,
				(
					map__search(TypeInfoMap0,
						TypeVar, TypeInfoLocn)
				->
					TypeInfoLocn = type_info(_)
				;
					true
				)	
			),
		list__filter(IsNew, ClassTypeVars, NewClassTypeVars),

			% Make an entry in the TypeInfo locations map for each
			% new type variable. The type variable can be found at
			% the previously calculated offset with the new
			% typeclass_info
		MakeEntry = (pred(IndexedTypeVar::in, 
					LocnMap0::in, LocnMap::out) is det :-
				IndexedTypeVar = TheTypeVar - Location,
				map__set(LocnMap0, TheTypeVar,
					typeclass_info(ExtraHeadVar, Location),
						LocnMap)
			),
		list__foldl(MakeEntry, NewClassTypeVars, TypeInfoMap0,
			TypeInfoMap1),

		poly_info_set_varset_and_types(VarSet1, VarTypes1, Info0,
			Info1),
		poly_info_set_type_info_map(TypeInfoMap1, Info1, Info)
	).

:- pred is_pair(pair(_, _)::in) is det.
is_pair(_).

:- pred polymorphism__new_typeclass_info_var(prog_varset, map(prog_var, type), 
		class_constraint, string, prog_var, 
		prog_varset, map(prog_var, type)).
:- mode polymorphism__new_typeclass_info_var(in, in,
		in, in, out, out, out) is det.

polymorphism__new_typeclass_info_var(VarSet0, VarTypes0, Constraint,
		ClassString, Var, VarSet, VarTypes) :-
	% introduce new variable
	varset__new_var(VarSet0, Var, VarSet1),
	string__append("TypeClassInfo_for_", ClassString, Name),
	varset__name_var(VarSet1, Var, Name, VarSet),

	polymorphism__build_typeclass_info_type(Constraint, DictionaryType),
	map__set(VarTypes0, Var, DictionaryType, VarTypes).

polymorphism__build_typeclass_info_type(Constraint, DictionaryType) :-
	Constraint = constraint(SymName, ArgTypes),

	% `constraint/n' is not really a type - it is a representation of a
	% class constraint about which a typeclass_info holds information.
	% `type_util:type_to_ctor_and_args' treats it as a type variable.
	construct_qualified_term(SymName, [], ClassNameTerm),
	mercury_private_builtin_module(PrivateBuiltin),
	construct_qualified_term(qualified(PrivateBuiltin, "constraint"),
		[ClassNameTerm | ArgTypes], ConstraintTerm),

	construct_type(qualified(PrivateBuiltin, "typeclass_info") - 1,
		[ConstraintTerm], DictionaryType).

%---------------------------------------------------------------------------%

polymorphism__typeclass_info_class_constraint(TypeClassInfoType, Constraint) :-
	mercury_private_builtin_module(PrivateBuiltin),
	type_to_ctor_and_args(TypeClassInfoType,
		qualified(PrivateBuiltin, "typeclass_info") - 1,
		[ConstraintTerm]),

	% type_to_ctor_and_args fails on `constraint/n', so we use
	% `sym_name_and_args' instead.
	mercury_private_builtin_module(PrivateBuiltin),
	sym_name_and_args(ConstraintTerm,
		qualified(PrivateBuiltin, "constraint"),
		[ClassNameTerm | ArgTypes]),
	sym_name_and_args(ClassNameTerm, ClassName, []),
	Constraint = constraint(ClassName, ArgTypes).

polymorphism__type_info_type(TypeInfoType, Type) :-
	mercury_private_builtin_module(PrivateBuiltin),
	type_to_ctor_and_args(TypeInfoType,
		qualified(PrivateBuiltin, "type_info") - 1,
		[Type]).

polymorphism__build_type_info_type(Type, TypeInfoType) :-
	polymorphism__build_type_info_type("type_info", Type, TypeInfoType). 

:- pred polymorphism__build_type_info_type(string, (type), (type)).
:- mode polymorphism__build_type_info_type(in, in, out) is det.

polymorphism__build_type_info_type(Symbol, Type, TypeInfoType) :-
	mercury_private_builtin_module(PrivateBuiltin),
	construct_type(qualified(PrivateBuiltin, Symbol) - 1,
		[Type], TypeInfoType).

%---------------------------------------------------------------------------%

polymorphism__is_typeclass_info_manipulator(ModuleInfo,
		PredId, TypeClassManipulator) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	mercury_private_builtin_module(PrivateBuiltin),
	pred_info_module(PredInfo, PrivateBuiltin),
	pred_info_name(PredInfo, PredName),
	(
		PredName = "type_info_from_typeclass_info",
		TypeClassManipulator = type_info_from_typeclass_info
	;
		PredName = "superclass_from_typeclass_info",
		TypeClassManipulator = superclass_from_typeclass_info
	;
		PredName = "instance_constraint_from_typeclass_info",
		TypeClassManipulator = instance_constraint_from_typeclass_info
	).

%---------------------------------------------------------------------------%

	% Expand the bodies of all class methods.
	% Class methods for imported classes are only expanded if
	% we are performing type specialization, so that method lookups
	% for imported classes can be optimized.
	%
	% The expansion involves inserting a class_method_call with the
	% appropriate arguments, which is responsible for extracting the
	% appropriate part of the dictionary.
:- pred polymorphism__expand_class_method_bodies(module_info, module_info).
:- mode polymorphism__expand_class_method_bodies(in, out) is det.

polymorphism__expand_class_method_bodies(ModuleInfo0, ModuleInfo) :-
	module_info_classes(ModuleInfo0, Classes),
	module_info_name(ModuleInfo0, ModuleName),
	map__keys(Classes, ClassIds0),

	module_info_globals(ModuleInfo0, Globals), 
	globals__lookup_bool_option(Globals, user_guided_type_specialization,
		TypeSpec),
	(
		TypeSpec = no,
		
			% Don't expand classes from other modules
		FromThisModule = lambda([ClassId::in] is semidet,
			(
				ClassId = class_id(qualified(ModuleName, _), _)
			)),
		list__filter(FromThisModule, ClassIds0, ClassIds)
	;
		TypeSpec = yes,
		ClassIds = ClassIds0
	),
	map__apply_to_list(ClassIds, Classes, ClassDefns),
	list__foldl(expand_bodies, ClassDefns, ModuleInfo0, ModuleInfo).

:- pred expand_bodies(hlds_class_defn, module_info, module_info).
:- mode expand_bodies(in, in, out) is det.

expand_bodies(hlds_class_defn(_, _, _, _, Interface, _, _), 
		ModuleInfo0, ModuleInfo) :-
	list__foldl2(expand_one_body, Interface, 1, _, ModuleInfo0, ModuleInfo).

:- pred expand_one_body(hlds_class_proc, int, int, module_info, module_info).
:- mode expand_one_body(in, in, out, in, out) is det.

expand_one_body(hlds_class_proc(PredId, ProcId), ProcNum0, ProcNum, 
		ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

		% Find which of the constraints on the pred is the one
		% introduced because it is a class method.
	pred_info_get_class_context(PredInfo0, ClassContext),
	(
		ClassContext = constraints([Head|_], _)
	->
		InstanceConstraint = Head
	;
		error("expand_one_body: class method is not constrained")
	),

	proc_info_typeclass_info_varmap(ProcInfo0, VarMap),
	map__lookup(VarMap, InstanceConstraint, TypeClassInfoVar),

	proc_info_headvars(ProcInfo0, HeadVars0),
	proc_info_argmodes(ProcInfo0, Modes0),
	proc_info_declared_determinism(ProcInfo0, Detism0),
	(
		Detism0 = yes(Detism1)
	->
		Detism = Detism1,
		ModuleInfo1 = ModuleInfo0
	;
		% Omitting the determinism for a method is not allowed.
		% But make_hlds.m will have already detected and reported
		% the error.  So here we can just pick some value at random;
		% hopefully something that won't cause flow-on errors.
		% We also mark the predicate as invalid, also to avoid
		% flow-on errors.
		Detism = nondet,
		module_info_remove_predid(ModuleInfo0, PredId, ModuleInfo1)
	),

		% Work out which argument corresponds to the constraint which
		% is introduced because this is a class method, then delete it
		% from the list of args to the class_method_call. That variable
		% becomes the "dictionary" variable for the class_method_call.
		% (cf. the closure for a higher order call).
	(
		list__nth_member_search(HeadVars0, TypeClassInfoVar, N),
		delete_nth(HeadVars0, N, HeadVars1),
		delete_nth(Modes0, N, Modes1)
	->
		HeadVars = HeadVars1,
		Modes = Modes1
	;
		error("expand_one_body: typeclass_info var not found")
	),

	InstanceConstraint = constraint(ClassName, InstanceArgs),
	list__length(InstanceArgs, InstanceArity),
	pred_info_get_call_id(PredInfo0, CallId),
	BodyGoalExpr = generic_call(
		class_method(TypeClassInfoVar, ProcNum0,
			class_id(ClassName, InstanceArity), CallId),
		HeadVars, Modes, Detism),

		% Make the goal info for the call. 
	set__list_to_set(HeadVars0, NonLocals),
	instmap_delta_from_mode_list(HeadVars0, Modes0, ModuleInfo0,
			InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, Detism, GoalInfo),
	BodyGoal = BodyGoalExpr - GoalInfo,

	proc_info_set_goal(ProcInfo0, BodyGoal, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo1),
	
	( pred_info_is_imported(PredInfo1) ->
		pred_info_set_import_status(PredInfo1, opt_imported, PredInfo)
	;
		PredInfo = PredInfo1
	),

	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo),

	ProcNum is ProcNum0 + 1.
	
:- pred delete_nth(list(T)::in, int::in, list(T)::out) is semidet.

delete_nth([X|Xs], N0, Result) :-
	(
		N0 > 1
	->
		N is N0 - 1,
		delete_nth(Xs, N, TheRest),
		Result = [X|TheRest]
	;
		Result = Xs
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type poly_info --->
		poly_info(
			% the first three fields are from the proc_info
			varset			:: prog_varset,
			vartypes		:: vartypes,
			typevarset		:: tvarset,

			type_info_varmap	:: type_info_varmap,		
						% specifies the location of
						% the type_info var
						% for each of the pred's type
						% parameters

			typeclass_info_map	::
				map(class_constraint, prog_var),		
						% specifies the location of
						% the typeclass_info var
						% for each of the pred's class
						% constraints
			proof_map		::
				map(class_constraint, constraint_proof),
						% specifies why each constraint
						% that was eliminated from the
						% pred was able to be eliminated
						% (this allows us to efficiently
						% construct the dictionary)

						% Note that the two maps above
						% are separate since the second
						% is the information calculated
						% by typecheck.m, while the
						% first is the information
						% calculated here in
						% polymorphism.m

			pred_info		:: pred_info,
			module_info		:: module_info
		).

%---------------------------------------------------------------------------%

	% init_poly_info initializes a poly_info from a pred_info
	% and clauses_info.
	% (See also create_poly_info.)
:- pred init_poly_info(module_info, pred_info, clauses_info, poly_info).
:- mode init_poly_info(in, in, in, out) is det.

init_poly_info(ModuleInfo, PredInfo, ClausesInfo, PolyInfo) :-
	clauses_info_varset(ClausesInfo, VarSet),
	clauses_info_vartypes(ClausesInfo, VarTypes),
	pred_info_typevarset(PredInfo, TypeVarSet),
	pred_info_get_constraint_proofs(PredInfo, Proofs),
	map__init(TypeInfoMap),
	map__init(TypeClassInfoMap),
	PolyInfo = poly_info(VarSet, VarTypes, TypeVarSet,
			TypeInfoMap, TypeClassInfoMap,
			Proofs, PredInfo, ModuleInfo).

	% create_poly_info creates a poly_info for an existing procedure.
	% (See also init_poly_info.)
create_poly_info(ModuleInfo, PredInfo, ProcInfo, PolyInfo) :-
	pred_info_typevarset(PredInfo, TypeVarSet),
	pred_info_get_constraint_proofs(PredInfo, Proofs),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_typeinfo_varmap(ProcInfo, TypeInfoMap),
	proc_info_typeclass_info_varmap(ProcInfo, TypeClassInfoMap),
	PolyInfo = poly_info(VarSet, VarTypes, TypeVarSet,
			TypeInfoMap, TypeClassInfoMap,
			Proofs, PredInfo, ModuleInfo).

poly_info_extract(Info, PredInfo0, PredInfo,
                ProcInfo0, ProcInfo, ModuleInfo) :-
	Info = poly_info(VarSet, VarTypes, TypeVarSet, TypeInfoMap,
		TypeclassInfoLocations, _Proofs, _OldPredInfo, ModuleInfo),

	% set the new values of the fields in proc_info and pred_info
	proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_typeinfo_varmap(ProcInfo2, TypeInfoMap, ProcInfo3),
	proc_info_set_typeclass_info_varmap(ProcInfo3, TypeclassInfoLocations,
		ProcInfo),
	pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo).

%---------------------------------------------------------------------------%

:- pred poly_info_get_varset(poly_info::in, prog_varset::out) is det.
:- pred poly_info_get_var_types(poly_info::in, vartypes::out) is det.
:- pred poly_info_get_typevarset(poly_info::in, tvarset::out) is det.
:- pred poly_info_get_type_info_map(poly_info::in, type_info_varmap::out)
	is det.
:- pred poly_info_get_typeclass_info_map(poly_info::in,
	map(class_constraint, prog_var)::out) is det.
:- pred poly_info_get_proofs(poly_info::in,
	map(class_constraint, constraint_proof)::out) is det.
:- pred poly_info_get_pred_info(poly_info::in, pred_info::out) is det.
:- pred poly_info_get_module_info(poly_info::in, module_info::out) is det.

poly_info_get_varset(PolyInfo, PolyInfo^varset).
poly_info_get_var_types(PolyInfo, PolyInfo^vartypes).
poly_info_get_typevarset(PolyInfo, PolyInfo^typevarset).
poly_info_get_type_info_map(PolyInfo, PolyInfo^type_info_varmap).
poly_info_get_typeclass_info_map(PolyInfo, PolyInfo^typeclass_info_map).
poly_info_get_proofs(PolyInfo, PolyInfo^proof_map).
poly_info_get_pred_info(PolyInfo, PolyInfo^pred_info).
poly_info_get_module_info(PolyInfo, PolyInfo^module_info).

:- pred poly_info_set_varset(prog_varset::in, poly_info::in,
	poly_info::out) is det.
:- pred poly_info_set_varset_and_types(prog_varset::in, vartypes::in,
	poly_info::in, poly_info::out) is det.
:- pred poly_info_set_typevarset(tvarset::in, poly_info::in,
	poly_info::out) is det.
:- pred poly_info_set_type_info_map(type_info_varmap::in,
	poly_info::in, poly_info::out) is det.
:- pred poly_info_set_typeclass_info_map(map(class_constraint, prog_var)::in,
	poly_info::in, poly_info::out) is det.
:- pred poly_info_set_proofs(map(class_constraint, constraint_proof)::in,
	poly_info::in, poly_info::out) is det.
:- pred poly_info_set_module_info(module_info::in, poly_info::in,
	poly_info::out) is det.

poly_info_set_varset(VarSet, PI, PI^varset := VarSet).
poly_info_set_varset_and_types(VarSet, VarTypes, PI,
	(PI ^varset := VarSet) ^vartypes := VarTypes).
poly_info_set_typevarset(TVarSet, PI, PI^typevarset := TVarSet).
poly_info_set_type_info_map(TVarMap, PI, PI^type_info_varmap := TVarMap).
poly_info_set_typeclass_info_map(TypeClassInfoMap, PI,
	PI^typeclass_info_map := TypeClassInfoMap).
poly_info_set_proofs(Proofs, PI, PI^proof_map := Proofs).
poly_info_set_module_info(ModuleInfo, PI, PI^module_info := ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
