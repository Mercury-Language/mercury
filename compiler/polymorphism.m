%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: polymorphism.m
% main author: fjh

% This module is a pass over the HLDS.
% It does a syntactic transformation to implement polymorphism, including
% typeclasses, by passing extra `type_info' and `typeclass_info' arguments.
% These arguments are structures that contain, amongst other things,
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
% always have a pointer to the pred/0, func/0 or tuple/0 type_ctor_info,
% regardless of their true arity, so we store the real arity in the type-info
% as well.
%
%	word 0		<pointer to the arity 0 type_ctor_info structure>
%	word 1		<arity of predicate>
%	word 2+		<the type_infos for the type params, if any>
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

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module io, list, term, map, std_util.

% Run the polymorphism pass over the whole HLDS.

:- pred polymorphism__process_module(module_info::in, module_info::out,
	io::di, io::uo) is det.

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

:- pred polymorphism__process_generated_pred(pred_id::in,
	module_info::in, module_info::out) is det.

% Add the type_info variables for a complicated unification to
% the appropriate fields in the unification and the goal_info.

:- pred polymorphism__unification_typeinfos((type)::in,
	map(tvar, type_info_locn)::in, unification::in, unification::out,
	hlds_goal_info::in, hlds_goal_info::out) is det.

% Add the type_info variables for a new call goal.  This predicate assumes
% that polymorphism__process_module has already been run so the called pred
% has already been processed.
%
% XXX This predicate does not yet handle calls whose arguments include
% existentially quantified types or type class constraints.

:- pred polymorphism__process_new_call(pred_id::in, proc_id::in,
	list(prog_var)::in, builtin_state::in, maybe(call_unify_context)::in,
	sym_name::in, hlds_goal_info::in, hlds_goal::out,
	poly_info::in, poly_info::out) is det.

% Given a list of types, create a list of variables to hold the type_info
% for those types, and create a list of goals to initialize those type_info
% variables to the appropriate type_info structures for the types.
% Update the varset and vartypes accordingly.

:- pred polymorphism__make_type_info_vars(list(type)::in,
	term__context::in, list(prog_var)::out, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

% Likewise, but for a single type.

:- pred polymorphism__make_type_info_var((type)::in,
	term__context::in, prog_var::out, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

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
:- pred polymorphism__gen_extract_type_info(tvar::in, prog_var::in, int::in,
	module_info::in, list(hlds_goal)::out, prog_var::out,
	prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out) is det.

:- type poly_info.

	% Extract some fields from a pred_info and proc_info and use them to
	% create a poly_info, for use by the polymorphism transformation.
:- pred create_poly_info(module_info::in, pred_info::in,
	proc_info::in, poly_info::out) is det.

	% Extract some fields from a pred_info and proc_info and use them to
	% create a poly_info, for use by the polymorphism transformation for
	% transforming a new call goal.
	%
:- pred create_poly_info_for_new_call(module_info::in, pred_info::in,
	proc_info::in, prog_varset::in, vartypes::in, poly_info::out) is det.

	% Update the fields in a pred_info and proc_info with
	% the values in a poly_info.
:- pred poly_info_extract(poly_info::in, pred_info::in, pred_info::out,
	proc_info::in, proc_info::out, module_info::out) is det.

	% Build the type describing the typeclass_info for the
	% given class_constraint.
:- pred polymorphism__build_typeclass_info_type(class_constraint::in,
	(type)::out) is det.

	% From the type of a typeclass_info variable find the class_constraint
	% about which the variable carries information, failing if the
	% type is not a valid typeclass_info type.
:- pred polymorphism__typeclass_info_class_constraint((type)::in,
	class_constraint::out) is semidet.

	% From the type of a type_info variable find the type about which
	% the type_info or type_ctor_info carries information, failing if the
	% type is not a valid type_info or type_ctor_info type.
:- pred polymorphism__type_info_or_ctor_type((type)::in, (type)::out)
	is semidet.

	% Construct the type of the type_info for the given type.
:- pred polymorphism__build_type_info_type((type)::in, (type)::out) is det.

	% Succeed if the predicate is one of the predicates defined in
	% library/private_builtin.m to extract type_infos or typeclass_infos
	% from typeclass_infos.
:- pred polymorphism__is_typeclass_info_manipulator(module_info::in,
	pred_id::in, typeclass_info_manipulator::out) is semidet.

:- type typeclass_info_manipulator
	--->	type_info_from_typeclass_info
	;	superclass_from_typeclass_info
	;	instance_constraint_from_typeclass_info.

	% Look up the pred_id and proc_id for a type specific
	% unification/comparison/index predicate.
:- pred polymorphism__get_special_proc((type)::in, special_pred_id::in,
	module_info::in, sym_name::out, pred_id::out, proc_id::out) is semidet.

	% convert a higher-order pred term to a lambda goal
:- pred convert_pred_to_lambda_goal(purity::in, lambda_eval_method::in,
	prog_var::in, pred_id::in, proc_id::in, list(prog_var)::in,
	list(type)::in, unify_context::in, hlds_goal_info::in, context::in,
	module_info::in, unify_rhs::out,
	prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out) is det.

	% init_type_info_var(Type, ArgVars, TypeInfoVar, TypeInfoGoal,
	%	!VarSet, !VarTypes) :-
	%
	% Create the unification the constructs the second cell of a type_info
	% for Type. ArgVars should contain the arguments of this unification.
	%
	% This unification WILL lead to the creation of cells on the heap
	% at runtime.
	%
	% The first variable in ArgVars should be bound to the type_ctor_info
	% for Type's principal type constructor. If that type constructor is
	% variable arity, the next variable in ArgVars should be bound to an
	% integer giving Type's actual arity. The remaining variables in
	% ArgVars should be bound to the type_infos or type_ctor_infos giving
	% Type's argument types.

:- pred polymorphism__init_type_info_var((type)::in, list(prog_var)::in,
	maybe(prog_var)::in, prog_var::out, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out) is det.

	% init_const_type_ctor_info_var(Type, TypeCtor,
	%	TypeCtorInfoVar, TypeCtorInfoGoal, ModuleInfo,
	%	!VarSet, !VarTypes):
	%
	% Create the unification (returned as TypeCtorInfoGoal) that binds a
	% new variable (returned as TypeCtorInfoVar) to the type_ctor_info
	% representing TypeCtor.
	%
	% This unification WILL NOT lead to the creation of a cell on the
	% heap at runtime; it will cause TypeCtorInfoVar to refer to the
	% statically allocated type_ctor_info cell for the type, allocated
	% in the module that defines the type.
	%
	% We take Type as input for historical reasons: we record Type as
	% the type whose type constructor TypeCtor is, in the type of
	% TypeCtorInfoVar.

:- pred polymorphism__init_const_type_ctor_info_var((type)::in, type_ctor::in,
	prog_var::out, hlds_goal::out, module_info::in,
	prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out) is det.

:- type type_info_kind
	--->	type_info
	;	type_ctor_info.

:- pred polymorphism__new_type_info_var_raw((type)::in, type_info_kind::in,
	prog_var::out, prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out) is det.

:- implementation.

:- import_module check_hlds__clause_to_proc.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__purity.
:- import_module check_hlds__type_util.
:- import_module check_hlds__typecheck.
:- import_module check_hlds__unify_proc.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_code_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_out.
:- import_module hlds__instmap.
:- import_module hlds__passes_aux.
:- import_module hlds__quantification.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module bool, int, string, set, map.
:- import_module term, varset, require, assoc_list.

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.
	% We do two passes, the first to fix up the clauses_info and
	% proc_infos (and in fact everything except the pred_info argtypes),
	% the second to fix up the pred_info argtypes.
	% The reason we need two passes is that the first pass looks at
	% the argtypes of the called predicates, and so we need to make
	% sure we don't muck them up before we've finished the first pass.

polymorphism__process_module(!ModuleInfo, !IO) :-
	module_info_preds(!.ModuleInfo, Preds0),
	map__keys(Preds0, PredIds0),
	list__foldl2(polymorphism__maybe_process_pred, PredIds0,
		!ModuleInfo, !IO),
	module_info_preds(!.ModuleInfo, Preds1),
	map__keys(Preds1, PredIds1),
	list__foldl(polymorphism__fixup_pred, PredIds1, !ModuleInfo),
	polymorphism__expand_class_method_bodies(!ModuleInfo).

:- pred polymorphism__maybe_process_pred(pred_id::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

polymorphism__maybe_process_pred(PredId, !ModuleInfo, !IO) :-
	module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
	(
		(
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
			PredModule = pred_info_module(PredInfo),
			PredName = pred_info_name(PredInfo),
			PredArity = pred_info_orig_arity(PredInfo),
			no_type_info_builtin(PredModule, PredName, PredArity)
		)
	->
		% just copy the clauses to the proc_infos
		copy_module_clauses_to_procs([PredId], !ModuleInfo)
	;
		polymorphism__process_pred(PredId, !ModuleInfo, !IO)
	).

%---------------------------------------------------------------------------%

:- pred polymorphism__fixup_pred(pred_id::in,
	module_info::in, module_info::out) is det.

polymorphism__fixup_pred(PredId, !ModuleInfo) :-
	%
	% Recompute the arg types by finding the headvars and
	% the var->type mapping (from the clauses_info) and
	% applying the type mapping to the extra headvars to get the new
	% arg types.  Note that we are careful to only apply the mapping
	% to the extra head vars, not to the originals, because otherwise
	% we would stuff up the arg types for unification predicates for
	% equivalence types.
	%
	module_info_preds(!.ModuleInfo, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_clauses_info(PredInfo0, ClausesInfo0),
	clauses_info_vartypes(ClausesInfo0, VarTypes0),
	clauses_info_headvars(ClausesInfo0, HeadVars),

	pred_info_arg_types(PredInfo0, TypeVarSet, ExistQVars, ArgTypes0),
	list__length(ArgTypes0, NumOldArgs),
	list__length(HeadVars, NumNewArgs),
	NumExtraArgs = NumNewArgs - NumOldArgs,
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
	pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes,
		PredInfo0, PredInfo1),

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
		clauses_info_set_vartypes(VarTypes, ClausesInfo0, ClausesInfo),
		pred_info_set_clauses_info(ClausesInfo, PredInfo1, PredInfo2),

		% Fix up the var-types in the procedures as well.
		% It would be better if this were done before copying
		% clauses to procs, but that's difficult to arrange.
		pred_info_procedures(PredInfo2, Procs0),
		map__map_values(
			(pred(_::in, ProcInfo0::in, ProcInfo::out) is det :-
				proc_info_set_vartypes(VarTypes,
					ProcInfo0, ProcInfo)
			), Procs0, Procs),
		pred_info_set_procedures(Procs, PredInfo2, PredInfo)
	;
		PredInfo = PredInfo1
	),

	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(PredTable, !ModuleInfo).

%---------------------------------------------------------------------------%

:- pred polymorphism__process_pred(pred_id::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

polymorphism__process_pred(PredId, !ModuleInfo, !IO) :-
	write_pred_progress_message("% Transforming polymorphism for ",
		PredId, !.ModuleInfo, !IO),
	polymorphism__process_pred(PredId, !ModuleInfo).

polymorphism__process_generated_pred(PredId, !ModuleInfo) :-
	polymorphism__process_pred(PredId, !ModuleInfo),
	polymorphism__fixup_pred(PredId, !ModuleInfo).

:- pred polymorphism__process_pred(pred_id::in,
	module_info::in, module_info::out) is det.

polymorphism__process_pred(PredId, !ModuleInfo) :-
	module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
	%
	% run the polymorphism pass over the clauses_info,
	% updating the headvars, goals, varsets, types, etc.,
	% and computing some information in the poly_info.
	%
	pred_info_clauses_info(PredInfo0, ClausesInfo0),
	polymorphism__process_clause_info(PredInfo0, !.ModuleInfo,
		ClausesInfo0, ClausesInfo, Info, ExtraArgModes),
	poly_info_get_module_info(Info, !:ModuleInfo),
	poly_info_get_typevarset(Info, TypeVarSet),
	pred_info_set_typevarset(TypeVarSet, PredInfo0, PredInfo1),
	pred_info_set_clauses_info(ClausesInfo, PredInfo1, PredInfo2),

	%
	% Do a pass over the proc_infos, copying the relevant information
	% from the clauses_info and the poly_info, and updating all
	% the argmodes with modes for the extra arguments.
	%
	ProcIds = pred_info_procids(PredInfo2),
	pred_info_procedures(PredInfo2, Procs0),
	list__foldl(polymorphism__process_proc_in_table(PredInfo2, ClausesInfo,
		ExtraArgModes), ProcIds, Procs0, Procs),
	pred_info_set_procedures(Procs, PredInfo2, PredInfo),

	module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred polymorphism__process_clause_info(pred_info::in, module_info::in,
	clauses_info::in, clauses_info::out, poly_info::out, list(mode)::out)
	is det.

polymorphism__process_clause_info(PredInfo0, ModuleInfo0,
	ClausesInfo0, ClausesInfo, Info, ExtraArgModes) :-

	init_poly_info(ModuleInfo0, PredInfo0, ClausesInfo0, Info0),
	clauses_info_headvars(ClausesInfo0, HeadVars0),

	polymorphism__setup_headvars(PredInfo0, HeadVars0, HeadVars,
		ExtraArgModes, _HeadTypeVars, UnconstrainedTVars,
		ExtraTypeInfoHeadVars, ExistTypeClassInfoHeadVars,
		Info0, Info1),

	clauses_info_clauses(ClausesInfo0, Clauses0),
	list__map_foldl(
		polymorphism__process_clause(PredInfo0,
			HeadVars0, HeadVars, UnconstrainedTVars,
			ExtraTypeInfoHeadVars,
			ExistTypeClassInfoHeadVars),
		Clauses0, Clauses, Info1, Info),

	%
	% Set the new values of the fields in clauses_info.
	%
	poly_info_get_varset(Info, VarSet),
	poly_info_get_var_types(Info, VarTypes),
	poly_info_get_type_info_map(Info, TypeInfoMap),
	poly_info_get_typeclass_info_map(Info, TypeClassInfoMap),
	clauses_info_explicit_vartypes(ClausesInfo0, ExplicitVarTypes),
	map__init(TVarNameMap), % This is only used while adding the clauses.
	ClausesInfo = clauses_info(VarSet, ExplicitVarTypes, TVarNameMap,
		VarTypes, HeadVars, Clauses, TypeInfoMap, TypeClassInfoMap,
		ClausesInfo0 ^ have_foreign_clauses).

:- pred polymorphism__process_clause(pred_info::in, list(prog_var)::in,
	list(prog_var)::in, list(tvar)::in,
	list(prog_var)::in, list(prog_var)::in,
	clause::in, clause::out, poly_info::in, poly_info::out) is det.

polymorphism__process_clause(PredInfo0, OldHeadVars, NewHeadVars,
		UnconstrainedTVars, ExtraTypeInfoHeadVars,
		ExistTypeClassInfoHeadVars, !Clause, !Info) :-
	( pred_info_is_imported(PredInfo0) ->
		true
	;
		Goal0 = !.Clause ^ clause_body,
		%
		% process any polymorphic calls inside the goal
		%
		polymorphism__process_goal(Goal0, Goal1, !Info),

		%
		% generate code to construct the type-class-infos
		% and type-infos for existentially quantified type vars
		%
		polymorphism__produce_existq_tvars(PredInfo0, OldHeadVars,
			UnconstrainedTVars, ExtraTypeInfoHeadVars,
			ExistTypeClassInfoHeadVars, Goal1, Goal2, !Info),

		pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
		polymorphism__fixup_quantification(NewHeadVars, ExistQVars,
			Goal2, Goal, !Info),
		!:Clause = !.Clause ^ clause_body := Goal
	).

:- pred polymorphism__process_proc_in_table(pred_info::in, clauses_info::in,
	list(mode)::in, proc_id::in, proc_table::in, proc_table::out) is det.

polymorphism__process_proc_in_table(PredInfo, ClausesInfo, ExtraArgModes,
		ProcId, !ProcTable) :-
	map__lookup(!.ProcTable, ProcId, ProcInfo0),
	polymorphism__process_proc(PredInfo, ClausesInfo, ExtraArgModes,
		ProcId, ProcInfo0, ProcInfo),
	map__det_update(!.ProcTable, ProcId, ProcInfo, !:ProcTable).

:- pred polymorphism__process_proc(pred_info::in, clauses_info::in,
	list(mode)::in, proc_id::in, proc_info::in, proc_info::out) is det.

polymorphism__process_proc(PredInfo, ClausesInfo, ExtraArgModes, ProcId,
		!ProcInfo) :-
	%
	% copy all the information from the clauses_info into the proc_info
	%
	(
		(
			pred_info_is_imported(PredInfo)
		;
			pred_info_is_pseudo_imported(PredInfo),
			hlds_pred__in_in_unification_proc_id(ProcId)
		)
	->
		%
		% We need to set these fields in the proc_info here, because
		% some parts of the compiler (e.g. unused_args.m) depend on
		% these fields being valid even for imported procedures.
		%
		clauses_info_headvars(ClausesInfo, HeadVars),
		clauses_info_typeclass_info_varmap(ClausesInfo,
			TypeClassInfoVarMap),
		clauses_info_type_info_varmap(ClausesInfo, TypeInfoVarMap),
		clauses_info_varset(ClausesInfo, VarSet),
		clauses_info_vartypes(ClausesInfo, VarTypes),
		proc_info_set_headvars(HeadVars, !ProcInfo),
		proc_info_set_typeclass_info_varmap(TypeClassInfoVarMap,
			!ProcInfo),
		proc_info_set_typeinfo_varmap(TypeInfoVarMap, !ProcInfo),
		proc_info_set_varset(VarSet, !ProcInfo),
		proc_info_set_vartypes(VarTypes, !ProcInfo)
	;
		copy_clauses_to_proc(ProcId, ClausesInfo, !ProcInfo)
	),

	%
	% add the ExtraArgModes to the proc_info argmodes
	%
	proc_info_argmodes(!.ProcInfo, ArgModes1),
	list__append(ExtraArgModes, ArgModes1, ArgModes),
	proc_info_set_argmodes(ArgModes, !ProcInfo).

% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

:- pred polymorphism__setup_headvars(pred_info::in, list(prog_var)::in,
	list(prog_var)::out, list(mode)::out, list(tvar)::out, list(tvar)::out,
	list(prog_var)::out, list(prog_var)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__setup_headvars(PredInfo, HeadVars0, HeadVars, ExtraArgModes,
		HeadTypeVars, UnconstrainedTVars, ExtraHeadTypeInfoVars,
		ExistHeadTypeClassInfoVars, !Info) :-
	pred_info_get_origin(PredInfo, Origin),
	( Origin = instance_method(InstanceMethodConstraints) ->
		polymorphism__setup_headvars_instance_method(PredInfo,
			InstanceMethodConstraints, HeadVars0, HeadVars,
			ExtraArgModes, HeadTypeVars, UnconstrainedTVars,
			ExtraHeadTypeInfoVars, ExistHeadTypeClassInfoVars,
			!Info)
	;
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
			!Info)
	).

	%
	% For class method implementations, do_call_class_method
	% takes the type-infos and typeclass-infos from the
	% typeclass-info and pastes them onto the front of
	% the argument list. We need to match that order here.
	%
:- pred polymorphism__setup_headvars_instance_method(pred_info::in,
	instance_method_constraints::in,
	list(prog_var)::in, list(prog_var)::out,
	list(mode)::out, list(tvar)::out, list(tvar)::out, list(prog_var)::out,
	list(prog_var)::out, poly_info::in, poly_info::out) is det.

polymorphism__setup_headvars_instance_method(PredInfo,
		InstanceMethodConstraints, HeadVars0, HeadVars, ExtraArgModes,
		HeadTypeVars, UnconstrainedTVars, ExtraHeadTypeInfoVars,
		ExistHeadTypeClassInfoVars, !Info) :-

	InstanceMethodConstraints = instance_method_constraints(_,
		InstanceTypes, InstanceConstraints, ClassContext),

	term__vars_list(InstanceTypes, InstanceTVars),
	get_unconstrained_tvars(InstanceTVars, InstanceConstraints,
		UnconstrainedInstanceTVars),
	pred_info_arg_types(PredInfo, ArgTypeVarSet, _, _),
	polymorphism__make_head_vars(UnconstrainedInstanceTVars,
		ArgTypeVarSet, UnconstrainedInstanceTypeInfoVars, !Info),
	polymorphism__make_typeclass_info_head_vars(InstanceConstraints,
		InstanceHeadTypeClassInfoVars, !Info),
	poly_info_get_typeclass_info_map(!.Info, TCVarMap0),
	map__det_insert_from_corresponding_lists(TCVarMap0,
		InstanceConstraints, InstanceHeadTypeClassInfoVars, TCVarMap),
	poly_info_set_typeclass_info_map(TCVarMap, !Info),
	list__append(UnconstrainedInstanceTypeInfoVars,
		InstanceHeadTypeClassInfoVars, ExtraHeadVars0),
	in_mode(InMode),
	list__duplicate(list__length(ExtraHeadVars0), InMode, ExtraArgModes0),
	polymorphism__setup_headvars_2(PredInfo, ClassContext,
		ExtraHeadVars0, ExtraArgModes0, UnconstrainedInstanceTVars,
		UnconstrainedInstanceTypeInfoVars, HeadVars0, HeadVars,
		ExtraArgModes, HeadTypeVars,
		UnconstrainedTVars, ExtraHeadTypeInfoVars,
		ExistHeadTypeClassInfoVars, !Info).

:- pred polymorphism__setup_headvars_2(pred_info::in, class_constraints::in,
		list(prog_var)::in, list(mode)::in, list(tvar)::in,
		list(prog_var)::in, list(prog_var)::in, list(prog_var)::out,
		list(mode)::out, list(tvar)::out, list(tvar)::out,
		list(prog_var)::out, list(prog_var)::out,
		poly_info::in, poly_info::out) is det.

polymorphism__setup_headvars_2(PredInfo, ClassContext, ExtraHeadVars0,
		ExtraArgModes0, UnconstrainedInstanceTVars,
		UnconstrainedInstanceTypeInfoVars, HeadVars0,
		HeadVars, ExtraArgModes, HeadTypeVars, AllUnconstrainedTVars,
		AllExtraHeadTypeInfoVars, ExistHeadTypeClassInfoVars, !Info) :-
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
		ExistHeadTypeClassInfoVars, !Info),
	poly_info_get_type_info_map(!.Info, TypeInfoMap1),
	map__keys(TypeInfoMap1, ExistConstrainedTVars),

	polymorphism__make_typeclass_info_head_vars(UnivConstraints,
		UnivHeadTypeClassInfoVars, !Info),
	poly_info_get_type_info_map(!.Info, TypeInfoMap3),
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
		ExistHeadTypeInfoVars = []
	;
		list__delete_elems(UnconstrainedTVars, ExistQVars,
			UnconstrainedUnivTVars),
		list__delete_elems(UnconstrainedTVars, UnconstrainedUnivTVars,
			UnconstrainedExistTVars),
		polymorphism__make_head_vars(UnconstrainedExistTVars,
			ArgTypeVarSet, ExistHeadTypeInfoVars, !Info)
	),

	polymorphism__make_head_vars(UnconstrainedUnivTVars,
		ArgTypeVarSet, UnivHeadTypeInfoVars, !Info),
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

	poly_info_set_type_info_map(TypeInfoMap6, !Info),

	% Make a map of the locations of the typeclass_infos
	poly_info_get_typeclass_info_map(!.Info, TypeClassInfoMap0),
	map__set_from_corresponding_lists(TypeClassInfoMap0,
		UnivConstraints, UnivHeadTypeClassInfoVars, TypeClassInfoMap),
	poly_info_set_typeclass_info_map(TypeClassInfoMap, !Info).

% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

%
% generate code to produce the values of type_infos and typeclass_infos
% for existentially quantified type variables in the head
%
:- pred polymorphism__produce_existq_tvars(pred_info::in, list(prog_var)::in,
	list(tvar)::in, list(prog_var)::in, list(prog_var)::in,
	hlds_goal::in, hlds_goal::out, poly_info::in, poly_info::out) is det.

polymorphism__produce_existq_tvars(PredInfo, HeadVars0, UnconstrainedTVars,
		TypeInfoHeadVars, ExistTypeClassInfoHeadVars, Goal0, Goal,
		!Info) :-
	poly_info_get_var_types(!.Info, VarTypes0),
	pred_info_arg_types(PredInfo, ArgTypes),
	pred_info_get_class_context(PredInfo, PredClassContext),

	%
	% Figure out the bindings for any existentially quantified
	% type variables in the head.
	%
	PredExistConstraints = PredClassContext ^ exist_constraints,
	( map__is_empty(VarTypes0) ->
		% this can happen for compiler-generated procedures
		map__init(PredToActualTypeSubst)
	;
		map__apply_to_list(HeadVars0, VarTypes0, ActualArgTypes),
		type_list_subsumes(ArgTypes, ActualArgTypes, ArgTypeSubst)
	->
		PredToActualTypeSubst = ArgTypeSubst
	;
		% this can happen for unification procedures
		% of equivalence types
		% error("polymorphism.m: type_list_subsumes failed")
		map__init(PredToActualTypeSubst)
	),

	%
	% generate code to produce values for any existentially quantified
	% typeclass-info variables in the head
	%
	ExistQVarsForCall = [],
	Goal0 = _ - GoalInfo,
	goal_info_get_context(GoalInfo, Context),
	apply_rec_subst_to_constraint_list(PredToActualTypeSubst,
		PredExistConstraints, ActualExistConstraints),
	polymorphism__make_typeclass_info_vars(ActualExistConstraints,
		ExistQVarsForCall, Context, ExistTypeClassVars,
		ExtraTypeClassGoals, !Info),
	polymorphism__update_typeclass_infos(ActualExistConstraints,
		ExistTypeClassVars, !Info),
	polymorphism__assign_var_list(ExistTypeClassInfoHeadVars,
		ExistTypeClassVars, ExtraTypeClassUnifyGoals),

	%
	% apply the type bindings to the unconstrained type variables
	% to give the actual types, and then generate code
	% to initialize the type_infos for those types
	%
	term__var_list_to_term_list(UnconstrainedTVars,
		UnconstrainedTVarTerms),
	term__apply_substitution_to_list(UnconstrainedTVarTerms,
		PredToActualTypeSubst, ActualTypes),
	polymorphism__make_type_info_vars(ActualTypes, Context,
		TypeInfoVars, ExtraTypeInfoGoals, !Info),
	polymorphism__assign_var_list(TypeInfoHeadVars, TypeInfoVars,
		ExtraTypeInfoUnifyGoals),
	list__condense([[Goal0], ExtraTypeClassGoals, ExtraTypeClassUnifyGoals,
		ExtraTypeInfoGoals, ExtraTypeInfoUnifyGoals], GoalList),
	conj_list_to_goal(GoalList, GoalInfo, Goal).

:- pred polymorphism__assign_var_list(list(prog_var)::in, list(prog_var)::in,
	list(hlds_goal)::out) is det.

polymorphism__assign_var_list([], [_ | _], _) :-
	error("unify_proc__assign_var_list: length mismatch").
polymorphism__assign_var_list([_ | _], [], _) :-
	error("unify_proc__assign_var_list: length mismatch").
polymorphism__assign_var_list([], [], []).
polymorphism__assign_var_list([Var1 | Vars1], [Var2 | Vars2], [Goal | Goals]) :-
	polymorphism__assign_var(Var1, Var2, Goal),
	polymorphism__assign_var_list(Vars1, Vars2, Goals).

:- pred polymorphism__assign_var(prog_var::in, prog_var::in, hlds_goal::out)
	is det.

polymorphism__assign_var(Var1, Var2, Goal) :-
	( Var1 = Var2 ->
		true_goal(Goal)
	;
		term__context_init(Context),
		create_atomic_unification(Var1, var(Var2), Context, explicit,
			[], Goal)
	).

%-----------------------------------------------------------------------------%

:- pred polymorphism__process_goal(hlds_goal::in, hlds_goal::out,
	poly_info::in, poly_info::out) is det.

polymorphism__process_goal(Goal0 - GoalInfo0, Goal, !Info) :-
	polymorphism__process_goal_expr(Goal0, GoalInfo0, Goal, !Info).

:- pred polymorphism__process_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
	hlds_goal::out, poly_info::in, poly_info::out) is det.

	% We don't need to add type-infos for higher-order calls,
	% since the type-infos are added when the closures are
	% constructed, not when they are called.
polymorphism__process_goal_expr(GoalExpr, GoalInfo, Goal, !Info) :-
	GoalExpr = generic_call(_, _, _, _),
	Goal = GoalExpr - GoalInfo.

polymorphism__process_goal_expr(Goal0, GoalInfo0, Goal, !Info) :-
	PredId = Goal0 ^ call_pred_id,
	ArgVars0 = Goal0 ^ call_args,
	polymorphism__process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
		ExtraVars, ExtraGoals, !Info),
	ArgVars = ExtraVars ++ ArgVars0,
	CallExpr = Goal0 ^ call_args := ArgVars,
	Call = CallExpr - GoalInfo,
	list__append(ExtraGoals, [Call], GoalList),
	conj_list_to_goal(GoalList, GoalInfo0, Goal).

polymorphism__process_goal_expr(Goal0, GoalInfo0, Goal, !Info) :-
	Goal0 = foreign_proc(_, PredId, _, _, _, _),
	poly_info_get_module_info(!.Info, ModuleInfo),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	PredModule = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
	PredArity = pred_info_orig_arity(PredInfo),

	( no_type_info_builtin(PredModule, PredName, PredArity) ->
		Goal = Goal0 - GoalInfo0
	;
		polymorphism__process_foreign_proc(ModuleInfo, PredInfo,
			Goal0, GoalInfo0, Goal, !Info)
	).

polymorphism__process_goal_expr(GoalExpr, GoalInfo, Goal, !Info) :-
	GoalExpr = unify(XVar, Y, Mode, Unification, UnifyContext),
	polymorphism__process_unify(XVar, Y, Mode, Unification, UnifyContext,
		GoalInfo, Goal, !Info).

	% the rest of the clauses just process goals recursively

polymorphism__process_goal_expr(GoalExpr, GoalInfo, Goal, !Info) :-
	GoalExpr = conj(Goals0),
	polymorphism__process_goal_list(Goals0, Goals, !Info),
	Goal = conj(Goals) - GoalInfo.
polymorphism__process_goal_expr(GoalExpr, GoalInfo, Goal, !Info) :-
	GoalExpr = par_conj(Goals0),
	polymorphism__process_goal_list(Goals0, Goals, !Info),
	Goal = par_conj(Goals) - GoalInfo.
polymorphism__process_goal_expr(GoalExpr, GoalInfo, Goal, !Info) :-
	GoalExpr = disj(Goals0),
	polymorphism__process_goal_list(Goals0, Goals, !Info),
	Goal = disj(Goals) - GoalInfo.
polymorphism__process_goal_expr(GoalExpr, GoalInfo, Goal, !Info) :-
	GoalExpr = not(SubGoal0),
	polymorphism__process_goal(SubGoal0, SubGoal, !Info),
	Goal = not(SubGoal) - GoalInfo.
polymorphism__process_goal_expr(GoalExpr, GoalInfo, Goal, !Info) :-
	GoalExpr = switch(Var, CanFail, Cases0),
	polymorphism__process_case_list(Cases0, Cases, !Info),
	Goal = switch(Var, CanFail, Cases) - GoalInfo.
polymorphism__process_goal_expr(GoalExpr, GoalInfo, Goal, !Info) :-
	GoalExpr = some(Vars, CanRemove, SubGoal0),
	polymorphism__process_goal(SubGoal0, SubGoal, !Info),
	Goal = some(Vars, CanRemove, SubGoal) - GoalInfo.
polymorphism__process_goal_expr(GoalExpr, GoalInfo, Goal, !Info) :-
	GoalExpr = if_then_else(Vars, Cond0, Then0, Else0),
	polymorphism__process_goal(Cond0, Cond, !Info),
	polymorphism__process_goal(Then0, Then, !Info),
	polymorphism__process_goal(Else0, Else, !Info),
	Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo.
polymorphism__process_goal_expr(GoalExpr, _GoalInfo, _Goal, !Info) :-
	% these should have been expanded out by now
	GoalExpr = shorthand(_),
	error("polymorphism__process_goal_expr: unexpected shorthand").

	% type_info_vars prepends a comma separated list of variables
	% onto a string of variables.
	% It places an & at the start of the variable name if the variable
	% is an output variable.
:- func type_info_vars(module_info, list(foreign_arg), string) = string.

type_info_vars(_ModuleInfo, [], InitString) = InitString.
type_info_vars(ModuleInfo, [Arg | Args], InitString) = String :-
	String0 = type_info_vars(ModuleInfo, Args, InitString),
	Arg = foreign_arg(_, MaybeNameMode, _),
	(
		MaybeNameMode = yes(ArgName0 - Mode),
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
		MaybeNameMode = no,
		String = String0
	).

:- pred polymorphism__process_unify(prog_var::in, unify_rhs::in,
	unify_mode::in, unification::in, unify_context::in, hlds_goal_info::in,
	hlds_goal::out, poly_info::in, poly_info::out) is det.

polymorphism__process_unify(XVar, Y, Mode, Unification0, UnifyContext,
		GoalInfo0, Goal, !Info) :-
	% switch on Y
	(
		Y = var(_YVar),
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
		poly_info_get_var_types(!.Info, VarTypes),
		map__lookup(VarTypes, XVar, Type),
		polymorphism__unification_typeinfos(Type,
			Unification0, Unification, GoalInfo0, GoalInfo, !Info),
		Goal = unify(XVar, Y, Mode, Unification, UnifyContext)
			- GoalInfo
	;
		Y = functor(ConsId, _, Args),
		polymorphism__process_unify_functor(XVar, ConsId, Args, Mode,
			Unification0, UnifyContext, GoalInfo0, Goal, !Info)
	;
		Y = lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
			ArgVars0, LambdaVars, Modes, Det, LambdaGoal0),
		%
		% for lambda expressions, we must recursively traverse the
		% lambda goal
		%
		polymorphism__process_goal(LambdaGoal0, LambdaGoal1, !Info),
		% Currently we don't allow lambda goals to be
		% existentially typed
		ExistQVars = [],
		polymorphism__fixup_lambda_quantification(ArgVars0, LambdaVars,
			ExistQVars, LambdaGoal1, LambdaGoal, NonLocalTypeInfos,
			!Info),
		set__to_sorted_list(NonLocalTypeInfos, NonLocalTypeInfosList),
		list__append(NonLocalTypeInfosList, ArgVars0, ArgVars),
		Y1 = lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
			ArgVars, LambdaVars, Modes, Det, LambdaGoal),
                goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		set__union(NonLocals0, NonLocalTypeInfos, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),

		%
		% Complicated (in-in) argument unifications are impossible
		% for lambda expressions, so we don't need to worry about
		% adding the type-infos that would be required for such
		% unifications.
		%
		Goal = unify(XVar, Y1, Mode, Unification0, UnifyContext)
			- GoalInfo
	).

:- pred polymorphism__unification_typeinfos((type)::in,
	unification::in, unification::out,
	hlds_goal_info::in, hlds_goal_info::out,
	poly_info::in, poly_info::out) is det.

polymorphism__unification_typeinfos(Type, !Unification, !GoalInfo, !Info) :-
	%
	% Compute the type_info/type_class_info variables that would be
	% used if this unification ends up being a complicated_unify.
	%
	prog_type__vars(Type, TypeVars),
	list__map_foldl(get_type_info_locn, TypeVars, TypeInfoLocns, !Info),
	polymorphism__add_unification_typeinfos(TypeInfoLocns,
		!Unification, !GoalInfo).

	% This variant is for use by modecheck_unify.m.
	% During mode-checking all the type-infos should appear in
	% the type_info_varmap.
polymorphism__unification_typeinfos(Type, TypeInfoMap, !Unification,
		!GoalInfo) :-
	%
	% Compute the type_info/type_class_info variables that would be
	% used if this unification ends up being a complicated_unify.
	%
	prog_type__vars(Type, TypeVars),
	map__apply_to_list(TypeVars, TypeInfoMap, TypeInfoLocns),
	polymorphism__add_unification_typeinfos(TypeInfoLocns,
		!Unification, !GoalInfo).

:- pred polymorphism__add_unification_typeinfos(list(type_info_locn)::in,
	unification::in, unification::out,
	hlds_goal_info::in, hlds_goal_info::out) is det.

polymorphism__add_unification_typeinfos(TypeInfoLocns, !Unification,
		!GoalInfo) :-
	list__map(type_info_locn_var, TypeInfoLocns, TypeInfoVars0),
	list__remove_dups(TypeInfoVars0, TypeInfoVars),

	%
	% Insert the TypeInfoVars into the nonlocals field of the goal_info
	% for the unification goal.
	%
	goal_info_get_nonlocals(!.GoalInfo, NonLocals0),
	set__insert_list(NonLocals0, TypeInfoVars, NonLocals),
	goal_info_set_nonlocals(!.GoalInfo, NonLocals, !:GoalInfo),

	%
	% Also save those type_info vars into a field in the complicated_unify,
	% so that quantification.m can recompute variable scopes properly.
	% This field is also used by modecheck_unify.m -- for complicated
	% unifications, it checks that all these variables are ground.
	%
	( !.Unification = complicated_unify(Modes, CanFail, _) ->
		!:Unification = complicated_unify(Modes, CanFail, TypeInfoVars)
	;
		% This can happen if an earlier stage of compilation
		% has already determined that this unification is particular
		% kind of unification.  In that case, the type_info vars
		% won't be needed.
		true
	).

:- pred polymorphism__process_unify_functor(prog_var::in, cons_id::in,
	list(prog_var)::in, unify_mode::in, unification::in, unify_context::in,
	hlds_goal_info::in, hlds_goal::out, poly_info::in, poly_info::out)
	is det.

polymorphism__process_unify_functor(X0, ConsId0, ArgVars0, Mode0,
		Unification0, UnifyContext, GoalInfo0, Goal, !Info) :-
	poly_info_get_module_info(!.Info, ModuleInfo0),
	poly_info_get_var_types(!.Info, VarTypes0),
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
	%       X = (pred(A1::in, A2::out) is ... :- list__append(Y, A1, A2))
	%
	% We do this because it makes two things easier.
	% First, mode analysis needs to check that the lambda-goal doesn't
	% bind any non-local variables (e.g. `Y' in above example).
	% This would require a bit of moderately tricky special-case code
	% if we didn't expand them here.
	% Second, this pass (polymorphism.m) is a lot easier
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
		type_is_higher_order(TypeOfX, Purity, _PredOrFunc,
			EvalMethod, CalleeArgTypes),
		ConsId0 = pred_const(ShroudedPredProcId, _)
	->
		%
		% convert the higher-order pred term to a lambda goal
		%
		poly_info_get_varset(!.Info, VarSet0),
		goal_info_get_context(GoalInfo0, Context),
		proc(PredId, ProcId) =
			unshroud_pred_proc_id(ShroudedPredProcId),
		convert_pred_to_lambda_goal(Purity, EvalMethod,
			X0, PredId, ProcId, ArgVars0, CalleeArgTypes,
			UnifyContext, GoalInfo0, Context, ModuleInfo0,
			Functor0, VarSet0, VarSet, VarTypes0, VarTypes),
		poly_info_set_varset_and_types(VarSet, VarTypes, !Info),
		%
		% process the unification in its new form
		%
		polymorphism__process_unify(X0, Functor0, Mode0,
			Unification0, UnifyContext, GoalInfo0, Goal, !Info)
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
		( remove_new_prefix(Functor0, OrigFunctor) ->
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
			ExtraVars, ExtraGoals, !Info),
		list__append(ExtraVars, ArgVars0, ArgVars),
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		set__insert_list(NonLocals0, ExtraVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),

		%
		% Some of the argument unifications may be complicated
		% unifications, which may need type-infos.
		%
		polymorphism__unification_typeinfos(TypeOfX,
			Unification0, Unification, GoalInfo1, GoalInfo, !Info),

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
		polymorphism__unification_typeinfos(TypeOfX,
			Unification0, Unification, GoalInfo0, GoalInfo, !Info),
		Goal = unify(X0, functor(ConsId0, no, ArgVars0), Mode0,
			Unification, UnifyContext) - GoalInfo
	).

convert_pred_to_lambda_goal(Purity, EvalMethod, X0, PredId, ProcId,
		ArgVars0, PredArgTypes, UnifyContext, GoalInfo0, Context,
		ModuleInfo0, Functor, !VarSet, !VarTypes) :-
	%
	% Create the new lambda-quantified variables
	%
	make_fresh_vars(PredArgTypes, LambdaVars, !VarSet, !VarTypes),
	list__append(ArgVars0, LambdaVars, Args),

	%
	% Build up the hlds_goal_expr for the call that will form
	% the lambda goal
	%
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
		PredInfo, ProcInfo),

	PredModule = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
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
		LambdaGoalInfo2),
	add_goal_info_purity_feature(LambdaGoalInfo2, Purity,
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
		error("Sorry, not implemented: determinism inference " ++
			"for higher-order predicate terms")
	),

	%
	% construct the lambda expression
	%
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	Functor = lambda_goal(Purity, PredOrFunc, EvalMethod, modes_are_ok,
		ArgVars0, LambdaVars, LambdaModes, LambdaDet, LambdaGoal).

:- pred make_fresh_vars(list(type)::in, list(prog_var)::out,
	prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out) is det.

make_fresh_vars([], [], !VarSet, !VarTypes).
make_fresh_vars([Type | Types], [Var | Vars], !VarSet, !VarTypes) :-
	varset__new_var(!.VarSet, Var, !:VarSet),
	map__det_insert(!.VarTypes, Var, Type, !:VarTypes),
	make_fresh_vars(Types, Vars, !VarSet, !VarTypes).

%-----------------------------------------------------------------------------%

%
% compute the extra arguments that we need to add to a unification with
% an existentially quantified data constructor.
%
:- pred polymorphism__process_existq_unify_functor(ctor_defn::in, bool::in,
	list(type)::in, (type)::in, prog_context::in, list(prog_var)::out,
	list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism__process_existq_unify_functor(CtorDefn, IsConstruction,
		ActualArgTypes, ActualRetType, Context,
		ExtraVars, ExtraGoals, !Info) :-

	CtorDefn = ctor_defn(CtorTypeVarSet, CtorExistQVars,
		CtorExistentialConstraints, CtorArgTypes, CtorRetType),

	%
	% rename apart the type variables in the constructor definition
	%
	poly_info_get_typevarset(!.Info, TypeVarSet0),
	varset__merge_subst(TypeVarSet0, CtorTypeVarSet, TypeVarSet,
		CtorToParentSubst),
	term__var_list_to_term_list(CtorExistQVars, CtorExistQVarTerms),
	term__apply_substitution_to_list(CtorExistQVarTerms, CtorToParentSubst,
		ParentExistQVarsTerms),
	apply_subst_to_constraint_list(CtorToParentSubst,
		CtorExistentialConstraints, ParentExistentialConstraints),
	term__apply_substitution_to_list(CtorArgTypes, CtorToParentSubst,
		ParentArgTypes),
	term__apply_substitution(CtorRetType, CtorToParentSubst,
		ParentRetType),
	poly_info_set_typevarset(TypeVarSet, !Info),

	%
	% Compute the type bindings resulting from the functor's actual
	% argument and return types.
	% These are the ones that might bind the ExistQVars.
	%
	type_list_subsumes_det([ParentRetType | ParentArgTypes],
		[ActualRetType | ActualArgTypes], ParentToActualTypeSubst),

	%
	% Apply those type bindings to the existential type class constraints
	%
	apply_rec_subst_to_constraint_list(ParentToActualTypeSubst,
		ParentExistentialConstraints,
		ActualExistentialConstraints),

	%
	% create type_class_info variables for the
	% type class constraints
	%

	(
		IsConstruction = yes,
		% assume it's a construction
		polymorphism__make_typeclass_info_vars(
			ActualExistentialConstraints, [], Context,
			ExtraTypeClassVars, ExtraTypeClassGoals, !Info)
	;
		IsConstruction = no,
		% assume it's a deconstruction
		polymorphism__make_existq_typeclass_info_vars(
			ActualExistentialConstraints, ExtraTypeClassVars,
			ExtraTypeClassGoals, !Info)
	),

	%
	% Compute the set of _unconstrained_ existentially quantified type
	% variables, and then apply the type bindings to those type variables
	% to figure out what types they are bound to.
	%
	constraint_list_get_tvars(ParentExistentialConstraints,
		ParentExistConstrainedTVars),
	term__var_list_to_term_list(ParentExistConstrainedTVars,
		ParentExistConstrainedTVarTerms),
	list__delete_elems(ParentExistQVarsTerms,
		ParentExistConstrainedTVarTerms,
		ParentUnconstrainedExistQVarTerms),
	term__apply_rec_substitution_to_list(ParentUnconstrainedExistQVarTerms,
		ParentToActualTypeSubst, ActualExistentialTypes),

	%
	% create type_info variables for the _unconstrained_
	% existentially quantified type variables
	%
	polymorphism__make_type_info_vars(ActualExistentialTypes, Context,
		ExtraTypeInfoVars, ExtraTypeInfoGoals, !Info),

	%
	% the type_class_info variables go AFTER the type_info variables
	% (for consistency with the order for argument passing,
	% and because the RTTI support in the runtime system relies on it)
	%
	list__append(ExtraTypeInfoGoals, ExtraTypeClassGoals, ExtraGoals),
	list__append(ExtraTypeInfoVars, ExtraTypeClassVars, ExtraVars).

%-----------------------------------------------------------------------------%

:- pred polymorphism__process_foreign_proc(module_info::in, pred_info::in,
	hlds_goal_expr::in(bound(foreign_proc(ground,ground,ground,ground,
	ground,ground))), hlds_goal_info::in, hlds_goal::out,
	poly_info::in, poly_info::out) is det.

polymorphism__process_foreign_proc(ModuleInfo, PredInfo, Goal0, GoalInfo0,
		Goal, !Info) :-
	%
	% insert the type_info vars into the arg-name map,
	% so that the foreign_proc can refer to the type_info variable
	% for type T as `TypeInfo_for_T'.
	%
	Goal0 = foreign_proc(Attributes, PredId, ProcId, Args0,
		ProcExtraArgs, PragmaCode0),
	ArgVars0 = list__map(foreign_arg_var, Args0),
	polymorphism__process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
		ExtraVars, ExtraGoals, !Info),
	polymorphism__process_foreign_proc_args(PredInfo, PragmaCode0,
		ExtraVars, ExtraArgs),
	Args = ExtraArgs ++ Args0,

	%
	% Add the type info arguments to the list of variables
	% to call for a pragma import.
	%
	( PragmaCode0 = import(Name, HandleReturn, Variables0, MaybeContext) ->
		Variables = type_info_vars(ModuleInfo, ExtraArgs, Variables0),
		PragmaCode = import(Name, HandleReturn,
			Variables, MaybeContext)
	;
		PragmaCode = PragmaCode0
	),

	%
	% plug it all back together
	%
	CallExpr = foreign_proc(Attributes, PredId, ProcId, Args,
		ProcExtraArgs, PragmaCode),
	Call = CallExpr - GoalInfo,
	list__append(ExtraGoals, [Call], GoalList),
	conj_list_to_goal(GoalList, GoalInfo0, Goal).

:- pred polymorphism__process_foreign_proc_args(pred_info::in,
	pragma_foreign_code_impl::in, list(prog_var)::in,
	list(foreign_arg)::out) is det.

polymorphism__process_foreign_proc_args(PredInfo, Impl, Vars, Args) :-
	pred_info_arg_types(PredInfo, PredTypeVarSet, ExistQVars,
		PredArgTypes),

		% Find out which variables are constrained (so that we don't
		% add type-infos for them.
	pred_info_get_class_context(PredInfo, constraints(UnivCs, ExistCs)),
	UnivVars0 = list__map(get_constrained_vars, UnivCs),
	list__condense(UnivVars0, UnivConstrainedVars),
	ExistVars0 = list__map(get_constrained_vars, ExistCs),
	list__condense(ExistVars0, ExistConstrainedVars),

	term__vars_list(PredArgTypes, PredTypeVars0),
	list__remove_dups(PredTypeVars0, PredTypeVars1),
	list__delete_elems(PredTypeVars1, UnivConstrainedVars, PredTypeVars2),
	list__delete_elems(PredTypeVars2, ExistConstrainedVars, PredTypeVars),

%	The argument order is as follows:
%	first the UnivTypeInfos (for universally quantified type variables)
% 	then the ExistTypeInfos (for existentially quantified type variables)
%	then the UnivTypeClassInfos (for universally quantified constraints)
%	then the ExistTypeClassInfos (for existentially quantified constraints)
%	and finally the original arguments of the predicate.

	in_mode(In),
	out_mode(Out),

	list__map(polymorphism__foreign_proc_add_typeclass_info(Out, Impl,
		PredTypeVarSet), ExistCs, ExistTypeClassArgInfos),
	list__map(polymorphism__foreign_proc_add_typeclass_info(In, Impl,
		PredTypeVarSet), UnivCs, UnivTypeClassArgInfos),
	TypeClassArgInfos = UnivTypeClassArgInfos ++ ExistTypeClassArgInfos,

	list__filter((pred(X::in) is semidet :- list__member(X, ExistQVars)),
		PredTypeVars, ExistUnconstrainedVars, UnivUnconstrainedVars),

	list__map(polymorphism__foreign_proc_add_typeinfo(Out, Impl,
		PredTypeVarSet), ExistUnconstrainedVars, ExistTypeArgInfos),
	list__map(polymorphism__foreign_proc_add_typeinfo(In, Impl,
		PredTypeVarSet), UnivUnconstrainedVars, UnivTypeArgInfos),
	TypeInfoArgInfos = UnivTypeArgInfos ++ ExistTypeArgInfos,

	ArgInfos = TypeInfoArgInfos ++ TypeClassArgInfos,

	%
	% insert type_info/typeclass_info types for all the inserted
	% type_info/typeclass_info vars into the arg-types list
	%
	term__var_list_to_term_list(PredTypeVars, PredTypeVarTypes),
	list__map(polymorphism__build_type_info_type, PredTypeVarTypes,
		TypeInfoTypes),
	list__map(polymorphism__build_typeclass_info_type, UnivCs, UnivTypes),
	list__map(polymorphism__build_typeclass_info_type, ExistCs, ExistTypes),
	OrigArgTypes = TypeInfoTypes ++ UnivTypes ++ ExistTypes,

	make_foreign_args(Vars, ArgInfos, OrigArgTypes, Args).

:- pred polymorphism__foreign_proc_add_typeclass_info((mode)::in,
	pragma_foreign_code_impl::in, tvarset::in, class_constraint::in,
	maybe(pair(string, mode))::out) is det.

polymorphism__foreign_proc_add_typeclass_info(Mode, Impl, TypeVarSet,
		Constraint, MaybeArgName) :-
	Constraint = constraint(Name0, Types),
	prog_out__sym_name_to_string(Name0, "__", Name),
	term__vars_list(Types, TypeVars),
	TypeVarNames =
		list__map(underscore_and_tvar_name(TypeVarSet), TypeVars),
	string__append_list(["TypeClassInfo_for_", Name | TypeVarNames],
		ConstraintVarName),
		% If the variable name corresponding to the
		% typeclass-info isn't mentioned in the C code
		% fragment, don't pass the variable to the
		% C code at all.
	( foreign_code_does_not_use_variable(Impl, ConstraintVarName) ->
		MaybeArgName = no
	;
		MaybeArgName = yes(ConstraintVarName - Mode)
	).

:- pred polymorphism__foreign_proc_add_typeinfo((mode)::in,
	pragma_foreign_code_impl::in, tvarset::in, tvar::in,
	maybe(pair(string, mode))::out) is det.

polymorphism__foreign_proc_add_typeinfo(Mode, Impl, TypeVarSet, TVar,
		MaybeArgName) :-
	( varset__search_name(TypeVarSet, TVar, TypeVarName) ->
		string__append("TypeInfo_for_", TypeVarName, C_VarName),
			% If the variable name corresponding to the
			% type-info isn't mentioned in the C code
			% fragment, don't pass the variable to the
			% C code at all.
		( foreign_code_does_not_use_variable(Impl, C_VarName) ->
			MaybeArgName = no
		;
			MaybeArgName = yes(C_VarName - Mode)
		)
	;
		MaybeArgName = no
	).

:- pred foreign_code_does_not_use_variable(pragma_foreign_code_impl::in,
	string::in) is semidet.

foreign_code_does_not_use_variable(Impl, VarName) :-
		% XXX This test is temporarily turned off, as it causes
		% the compiler to abort when compiling
		% stage2/browser/declarative_execution.m
	semidet_fail,
	(
		Impl = ordinary(ForeignBody, _),
		\+ string__sub_string_search(ForeignBody, VarName, _)
	;
		Impl = nondet(FB1,_,FB2,_,FB3,_,_,FB4,_),
		\+ string__sub_string_search(FB1, VarName, _),
		\+ string__sub_string_search(FB2, VarName, _),
		\+ string__sub_string_search(FB3, VarName, _),
		\+ string__sub_string_search(FB4, VarName, _)
	).

:- func underscore_and_tvar_name(tvarset, tvar) = string.

underscore_and_tvar_name(TypeVarSet, TVar) = TVarName :-
	varset__lookup_name(TypeVarSet, TVar, TVarName0),
	string__append("_", TVarName0, TVarName).

:- pred polymorphism__process_goal_list(list(hlds_goal)::in,
	list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism__process_goal_list([], [], !Info).
polymorphism__process_goal_list([Goal0 | Goals0], [Goal | Goals], !Info) :-
	polymorphism__process_goal(Goal0, Goal, !Info),
	polymorphism__process_goal_list(Goals0, Goals, !Info).

:- pred polymorphism__process_case_list(list(case)::in, list(case)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__process_case_list([], [], !Info).
polymorphism__process_case_list([Case0 | Cases0], [Case | Cases], !Info) :-
	Case0 = case(ConsId, Goal0),
	polymorphism__process_goal(Goal0, Goal, !Info),
	Case = case(ConsId, Goal),
	polymorphism__process_case_list(Cases0, Cases, !Info).

%-----------------------------------------------------------------------------%

% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

:- pred polymorphism__process_call(pred_id::in, list(prog_var)::in,
	hlds_goal_info::in, hlds_goal_info::out,
	list(prog_var)::out, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
		ExtraVars, ExtraGoals, !Info) :-
	poly_info_get_var_types(!.Info, VarTypes),
	poly_info_get_typevarset(!.Info, TypeVarSet0),
	poly_info_get_module_info(!.Info, ModuleInfo),

	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
		PredArgTypes),
	pred_info_get_class_context(PredInfo, PredClassContext),

		% VarTypes, TypeVarSet* etc come from the caller.
		% PredTypeVarSet, PredArgTypes, PredExistQVarTerms, etc come
		% directly from the callee.
		% ParentArgTypes, ParentExistQVarTerms etc come from a version
		% of the callee that has been renamed apart from the caller.
		%
		% The difference between e.g. PredArgTypes and ParentArgTypes
		% is the application of PredToParentTypeSubst, which maps the
		% type variables in the callee to new type variables in the
		% caller. Adding the new type variables to TypeVarSet0 yields
		% TypeVarSet.
	( varset__is_empty(PredTypeVarSet) ->
		% optimize a common case
		map__init(PredToParentTypeSubst),
		TypeVarSet = TypeVarSet0,
		ParentArgTypes = PredArgTypes,
		ParentTypeVars0 = [],
		ParentExistQVarTerms1 = []
	;
		% (this merge might be a performance bottleneck?)
		varset__merge_subst(TypeVarSet0, PredTypeVarSet, TypeVarSet,
			PredToParentTypeSubst),
		term__apply_substitution_to_list(PredArgTypes,
			PredToParentTypeSubst, ParentArgTypes),
		term__vars_list(ParentArgTypes, ParentTypeVars0),
		term__var_list_to_term_list(PredExistQVars,
			PredExistQVarTerms),
		term__apply_substitution_to_list(PredExistQVarTerms,
			PredToParentTypeSubst, ParentExistQVarTerms1)
	),

	PredModule = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
	PredArity = pred_info_orig_arity(PredInfo),
	(
		(
			% Optimize for the common case of non-polymorphic call
			% with no constraints.
			ParentTypeVars0 = [],
			PredClassContext = constraints([], [])
		;
			% Some builtins don't need or want the type_info.
			no_type_info_builtin(PredModule, PredName, PredArity)
		;
			% Leave Aditi relations alone, since they must
			% be monomorphic. This is checked by magic.m.
			hlds_pred__pred_info_is_aditi_relation(PredInfo)
		;
			hlds_pred__pred_info_is_aditi_aggregate(PredInfo)
		)
	->
		GoalInfo = GoalInfo0,
		ExtraGoals = [],
		ExtraVars = []
	;
		list__remove_dups(ParentTypeVars0, ParentTypeVars1),
		map__apply_to_list(ArgVars0, VarTypes, ActualArgTypes),
		type_list_subsumes_det(ParentArgTypes, ActualArgTypes,
			ParentToActualTypeSubst),
		apply_subst_to_constraints(PredToParentTypeSubst,
			PredClassContext, ParentClassContext),

		poly_info_set_typevarset(TypeVarSet, !Info),

			% Make the universally quantified typeclass_infos
			% for the call, and return a list of which type
			% variables were constrained by those constraints
		goal_info_get_context(GoalInfo0, Context),
		ParentClassContext = constraints(ParentUniversalConstraints,
			ParentExistentialConstraints),

			% Compute which type variables are constrained
			% by the type class constraints.
		constraint_list_get_tvars(ParentExistentialConstraints,
			ParentExistConstrainedTVars),
		constraint_list_get_tvars(ParentUniversalConstraints,
			ParentUnivConstrainedTVars),

		apply_rec_subst_to_constraint_list(ParentToActualTypeSubst,
			ParentUniversalConstraints,
			ActualUniversalConstraints),

		term__apply_rec_substitution_to_list(ParentExistQVarTerms1,
			ParentToActualTypeSubst, ParentExistQVarTerms),
		term__term_list_to_var_list(ParentExistQVarTerms,
			ParentExistQVars),

		polymorphism__make_typeclass_info_vars(
			ActualUniversalConstraints, ParentExistQVars, Context,
			UnivTypeClassVars, ExtraTypeClassGoals, !Info),

			% Make variables to hold any existentially
			% quantified typeclass_infos in the call,
			% insert them into the typeclass_info map
		apply_rec_subst_to_constraint_list(ParentToActualTypeSubst,
			ParentExistentialConstraints,
			ActualExistentialConstraints),
		polymorphism__make_existq_typeclass_info_vars(
			ActualExistentialConstraints, ExistTypeClassVars,
			ExtraExistClassGoals, !Info),

		list__append(UnivTypeClassVars, ExistTypeClassVars,
			ExtraTypeClassVars),

			% No need to make typeinfos for the constrained vars.
		list__delete_elems(ParentTypeVars1,
			ParentUnivConstrainedTVars, ParentTypeVars2),
		list__delete_elems(ParentTypeVars2,
			ParentExistConstrainedTVars, ParentTypeVars),

		term__var_list_to_term_list(ParentTypeVars, ParentTypes),
		term__apply_rec_substitution_to_list(ParentTypes,
			ParentToActualTypeSubst, ActualTypes),

		polymorphism__make_type_info_vars(ActualTypes, Context,
			ExtraTypeInfoVars, ExtraTypeInfoGoals, !Info),
		ExtraGoals = ExtraTypeClassGoals ++ ExtraExistClassGoals
			++ ExtraTypeInfoGoals,
		ExtraVars = ExtraTypeInfoVars ++ ExtraTypeClassVars,

		%
		% update the non-locals
		%
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		set__insert_list(NonLocals0, ExtraVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo)
	).

%-----------------------------------------------------------------------------%

% XXX This predicate does not yet handle calls whose arguments include
% existentially quantified types or type class constraints.

polymorphism__process_new_call(PredId, ProcId, CallArgs0, BuiltinState,
		MaybeCallUnifyContext, SymName, GoalInfo0, Goal, !Info) :-
	poly_info_get_var_types(!.Info, CallVarTypes),
	poly_info_get_typevarset(!.Info, CallTypeVarSet0),
	poly_info_get_pred_info(!.Info, PredInfo),
	pred_info_arg_types(PredInfo, PredArgTypes),

		% Work out the types of the provided call args.
		%
	CallArgTypes0 = map__apply_to_list(CallArgs0, CallVarTypes),

		% Work out how many type_info args we need to prepend.
		%
	NCallArgs0 = list__length(CallArgTypes0),
	NPredArgs  = list__length(PredArgTypes),
	NExtraArgs = NPredArgs - NCallArgs0,

		% Construct a fresh type var for each extra type_info
		% we need to prepend.
		%
		% That is, for every such type_info we construct a new
		% type variable ExtraTypeTypeVar which we will bind to a
		% term private_builtin.type_info(ExtraArgTypeParam),
		% where ExtraArgTypeParam is also a new type variable.
		%
	varset__new_vars(CallTypeVarSet0, NExtraArgs, ExtraArgTypeVars,
		CallTypeVarSet1),
	list__map2_foldl(bind_type_var_to_type_info_wrapper,
		ExtraArgTypeVars, ExtraArgTypes0, ExtraArgTypeParams0,
		CallTypeVarSet1, _CallTypeVarSet),

		% Prepend the list of types to the call arg types and unify
		% the resulting list with the pred arg types.  This should
		% result in the earlier fresh ExtraArgTypeParams being unified
		% with the types for which we need to construct type_infos.
		%
	CallArgTypes = ExtraArgTypes0 ++ CallArgTypes0,
	unify_corresponding_types(PredArgTypes, CallArgTypes,
		map__init, Substitution),
	ExtraArgTypeParams = term__apply_rec_substitution_to_list(
				ExtraArgTypeParams0, Substitution),

		% And finally construct the type_info goals and args we
		% need to prepend to complete the call.
		%
	Ctxt = term__context_init,
	make_type_info_vars(ExtraArgTypeParams, Ctxt, ExtraArgs, ExtraGoals,
		!Info),
	CallArgs = ExtraArgs ++ CallArgs0,
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	NonLocals1 = set__list_to_set(ExtraArgs),
	NonLocals = set__union(NonLocals0, NonLocals1),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),
	CallGoalExpr = call(PredId, ProcId, CallArgs, BuiltinState,
		MaybeCallUnifyContext, SymName),
	CallGoal = CallGoalExpr - GoalInfo,
	conj_list_to_goal(ExtraGoals ++ [CallGoal], GoalInfo, Goal).


	% bind_type_var_to_type_info_wrapper(X, Type, Param, VarSet0, VarSet)
	% constructs a new type var Param and binds X to the Type form of
	% `private_builtin.type_info(Param)'.
	%
:- pred bind_type_var_to_type_info_wrapper(tvar::in, (type)::out, (type)::out,
	tvarset::in, tvarset::out) is det.

bind_type_var_to_type_info_wrapper(X, Type, Param, TVarSet0, TVarSet) :-
	varset__new_var(TVarSet0, Y, TVarSet1),
	Param = variable(Y),
	Ctxt  = term__context_init,
	Type  = functor(atom("."),
			[ functor(atom("private_builtin"), [], Ctxt),
			  functor(atom("type_info"), [Param], Ctxt) ],
			Ctxt),
	varset__bind_var(TVarSet1, X, Type, TVarSet).


:- pred unify_corresponding_types(list(type)::in, list(type)::in, 
		tsubst::in, tsubst::out) is det.

unify_corresponding_types([], [], !Subst).
unify_corresponding_types([], [_ | _], !Subst) :-
	error("polymorphism__unify_corresponding_types: " ++
		"differing list lengths").
unify_corresponding_types([_ | _], [], !Subst) :-
	error("polymorphism__unify_corresponding_types: " ++
		"differing list lengths").
unify_corresponding_types([A | As], [B | Bs], !Subst) :-
	(
		term__unify(A, B, !Subst)
	->
		unify_corresponding_types(As, Bs, !Subst)
	;
		error("polymorphism__unify_corresponding_types: " ++
			"term__unify failed")
	).

%-----------------------------------------------------------------------------%

:- pred polymorphism__update_typeclass_infos(list(class_constraint)::in,
	list(prog_var)::in, poly_info::in, poly_info::out) is det.

polymorphism__update_typeclass_infos(Constraints, Vars, !Info) :-
	poly_info_get_typeclass_info_map(!.Info, TypeClassInfoMap0),
	insert_typeclass_info_locns(Constraints, Vars,
		TypeClassInfoMap0, TypeClassInfoMap),
	poly_info_set_typeclass_info_map(TypeClassInfoMap, !Info).

:- pred insert_typeclass_info_locns(list(class_constraint)::in,
	list(prog_var)::in,
	map(class_constraint, prog_var)::in,
	map(class_constraint, prog_var)::out) is det.

insert_typeclass_info_locns([], [], !TypeClassInfoMap).
insert_typeclass_info_locns([C | Cs], [V | Vs], !TypeClassInfoMap) :-
	map__set(!.TypeClassInfoMap, C, V, !:TypeClassInfoMap),
	insert_typeclass_info_locns(Cs, Vs, !TypeClassInfoMap).
insert_typeclass_info_locns([], [_ | _], _, _) :-
	error("polymorphism:insert_typeclass_info_locns").
insert_typeclass_info_locns([_ | _], [], _, _) :-
	error("polymorphism:insert_typeclass_info_locns").

%-----------------------------------------------------------------------------%

:- pred polymorphism__fixup_quantification(list(prog_var)::in,
	existq_tvars::in, hlds_goal::in, hlds_goal::out,
	poly_info::in, poly_info::out) is det.

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

polymorphism__fixup_quantification(HeadVars, ExistQVars, Goal0, Goal, !Info) :-
	(
		% optimize common case
		ExistQVars = [],
		poly_info_get_type_info_map(!.Info, TypeVarMap),
		map__is_empty(TypeVarMap)
	->
		Goal = Goal0
	;
		poly_info_get_varset(!.Info, VarSet0),
		poly_info_get_var_types(!.Info, VarTypes0),
		set__list_to_set(HeadVars, OutsideVars),
		implicitly_quantify_goal(OutsideVars, _Warnings,
			Goal0, Goal, VarSet0, VarSet, VarTypes0, VarTypes),
		poly_info_set_varset_and_types(VarSet, VarTypes, !Info)
	).

:- pred polymorphism__fixup_lambda_quantification(list(prog_var)::in,
	list(prog_var)::in, existq_tvars::in, hlds_goal::in, hlds_goal::out,
	set(prog_var)::out, poly_info::in, poly_info::out) is det.

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

polymorphism__fixup_lambda_quantification(ArgVars, LambdaVars, ExistQVars,
		!Goal, NewOutsideVars, !Info) :-
	poly_info_get_type_info_map(!.Info, TypeVarMap),
	poly_info_get_typeclass_info_map(!.Info, TypeClassVarMap),
	( map__is_empty(TypeVarMap) ->
		set__init(NewOutsideVars)
	;
		poly_info_get_varset(!.Info, VarSet0),
		poly_info_get_var_types(!.Info, VarTypes0),
		!.Goal = _ - GoalInfo0,
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		set__insert_list(NonLocals, ArgVars, NonLocalsPlusArgs0),
		set__insert_list(NonLocalsPlusArgs0, LambdaVars,
			NonLocalsPlusArgs),
		goal_util__extra_nonlocal_typeinfos(TypeVarMap,
			TypeClassVarMap, VarTypes0, ExistQVars,
			NonLocalsPlusArgs, NewOutsideVars),
		set__union(NonLocals, NewOutsideVars, OutsideVars),
		implicitly_quantify_goal(OutsideVars, _Warnings, !Goal,
			VarSet0, VarSet, VarTypes0, VarTypes),
		poly_info_set_varset_and_types(VarSet, VarTypes, !Info)
	).

%-----------------------------------------------------------------------------%

% Given the list of constraints for a called predicate, create a list of
% variables to hold the typeclass_info for those constraints,
% and create a list of goals to initialize those typeclass_info variables
% to the appropriate typeclass_info structures for the constraints.
%
% Constraints should be renamed-apart and actual-to-formal substituted constraints.
%
% Constraints which are already in the TypeClassInfoMap are assumed to
% have already had their typeclass_infos initialized; for them, we
% just return the variable in the TypeClassInfoMap.

:- pred polymorphism__make_typeclass_info_vars(list(class_constraint)::in,
	existq_tvars::in, prog_context::in,
	list(prog_var)::out, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__make_typeclass_info_vars(Constraints, ExistQVars, Context,
		ExtraVars, ExtraGoals, !Info) :-
		% initialise the accumulators
	RevExtraVars0 = [],
	RevExtraGoals0 = [],
	SeenInstances = [],
		% do the work
	polymorphism__make_typeclass_info_vars_2(Constraints, SeenInstances,
		ExistQVars, Context, RevExtraVars0, RevExtraVars,
		RevExtraGoals0, RevExtraGoals, !Info),
		% We build up the vars and goals in reverse order
	list__reverse(RevExtraVars, ExtraVars),
	list__reverse(RevExtraGoals, ExtraGoals).

% Accumulator version of the above.

:- pred polymorphism__make_typeclass_info_vars_2(
	list(class_constraint)::in, list(class_constraint)::in,
	existq_tvars::in, prog_context::in,
	list(prog_var)::in, list(prog_var)::out,
	list(hlds_goal)::in, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__make_typeclass_info_vars_2([], _Seen, _ExistQVars,
		_Context, !ExtraVars, !ExtraGoals, !Info).
polymorphism__make_typeclass_info_vars_2([Constraint | Constraints],
		Seen, ExistQVars, Context, !ExtraVars, !ExtraGoals, !Info) :-
	polymorphism__make_typeclass_info_var(Constraint, [Constraint | Seen],
		ExistQVars, Context, !ExtraGoals, !Info, MaybeExtraVar),
	maybe_insert_var(MaybeExtraVar, !ExtraVars),
	polymorphism__make_typeclass_info_vars_2(Constraints, Seen, ExistQVars,
		Context, !ExtraVars, !ExtraGoals, !Info).

:- pred polymorphism__make_typeclass_info_var(class_constraint::in,
	list(class_constraint)::in, existq_tvars::in, prog_context::in,
	list(hlds_goal)::in, list(hlds_goal)::out,
	poly_info::in, poly_info::out, maybe(prog_var)::out) is det.

polymorphism__make_typeclass_info_var(Constraint, Seen, ExistQVars,
		Context, !ExtraGoals, !Info, MaybeVar) :-
	(
		map__search(!.Info ^ typeclass_info_map, Constraint, Var)
	->
			% We already have a typeclass_info for this constraint,
			% either from a parameter to the pred or from an
			% existentially quantified goal that we have already
			% processed.

		MaybeVar = yes(Var)
	;
			% We don't have the typeclass_info, we must either have
			% a proof that tells us how to make it, or it will be
			% produced by an existentially typed goal that we
			% will process later on.

		map__search(!.Info ^ proof_map, Constraint, Proof)
	->
		polymorphism__make_typeclass_info_from_proof(Constraint, Seen,
			Proof, ExistQVars, Context, MaybeVar,
			!ExtraGoals, !Info)
	;
		polymorphism__make_typeclass_info_head_var(Constraint,
			NewVar, !Info),
		map__det_insert(!.Info ^ typeclass_info_map, Constraint,
			NewVar, NewTypeClassInfoMap),
		!:Info = (!.Info ^ typeclass_info_map := NewTypeClassInfoMap),
		MaybeVar = yes(NewVar)
	).

:- pred polymorphism__make_typeclass_info_from_proof(class_constraint::in,
	list(class_constraint)::in, constraint_proof::in, existq_tvars::in,
	prog_context::in, maybe(prog_var)::out,
	list(hlds_goal)::in, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__make_typeclass_info_from_proof(Constraint, Seen, Proof,
		ExistQVars, Context, MaybeVar, !ExtraGoals, !Info) :-
	Constraint = constraint(ClassName, ConstrainedTypes),
	list__length(ConstrainedTypes, ClassArity),
	ClassId = class_id(ClassName, ClassArity),
	(
			% We have to construct the typeclass_info
			% using an instance declaration
		Proof = apply_instance(InstanceNum),
		polymorphism__make_typeclass_info_from_instance(Constraint,
			Seen, ClassId, InstanceNum, ExistQVars, Context,
			MaybeVar, !ExtraGoals, !Info)
	;
		% XXX MR_Dictionary should have MR_Dictionaries for superclass
			% We have to extract the typeclass_info from
			% another one
		Proof = superclass(SubClassConstraint),
		polymorphism__make_typeclass_info_from_subclass(Constraint,
			Seen, ClassId, SubClassConstraint, ExistQVars, Context,
			MaybeVar, !ExtraGoals, !Info)
	).

:- pred polymorphism__make_typeclass_info_from_instance(class_constraint::in,
	list(class_constraint)::in, class_id::in, int::in, existq_tvars::in,
	prog_context::in, maybe(prog_var)::out,
	list(hlds_goal)::in, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__make_typeclass_info_from_instance(Constraint, Seen,
		ClassId, InstanceNum, ExistQVars, Context, MaybeVar,
		!ExtraGoals, !Info) :-
	Constraint = constraint(_ClassName, ConstrainedTypes),
	!.Info = poly_info(_VarSet0, _VarTypes0, TypeVarSet, _TypeInfoMap0,
		_TypeClassInfoMap0, Proofs, _PredName, ModuleInfo),

	module_info_instances(ModuleInfo, InstanceTable),
	map__lookup(InstanceTable, ClassId, InstanceList),
	list__index1_det(InstanceList, InstanceNum, ProofInstanceDefn),

	ProofInstanceDefn = hlds_instance_defn(_, _, _, InstanceConstraints0,
		InstanceTypes0, _, _, InstanceTVarset, SuperClassProofs0),

	term__vars_list(InstanceTypes0, InstanceTvars),
	get_unconstrained_tvars(InstanceTvars,
		InstanceConstraints0, UnconstrainedTvars0),

		% We can ignore the typevarset because all the
		% type variables that are created are bound
		% when we call type_list_subsumes then apply
		% the resulting bindings.
		% XXX expand comment
	varset__merge_subst(TypeVarSet, InstanceTVarset,
		_NewTVarset, RenameSubst),
	term__apply_substitution_to_list(InstanceTypes0,
		RenameSubst, InstanceTypes),
	type_list_subsumes_det(InstanceTypes, ConstrainedTypes, InstanceSubst),
	apply_subst_to_constraint_list(RenameSubst,
		InstanceConstraints0, InstanceConstraints1),
	apply_rec_subst_to_constraint_list(InstanceSubst,
		InstanceConstraints1, InstanceConstraints2),
	% XXX document diamond as guess
	InstanceConstraints = InstanceConstraints2 `list__delete_elems` Seen,
	apply_subst_to_constraint_proofs(RenameSubst,
		SuperClassProofs0, SuperClassProofs1),
	apply_rec_subst_to_constraint_proofs(InstanceSubst,
		SuperClassProofs1, SuperClassProofs2),

	term__var_list_to_term_list(UnconstrainedTvars0, UnconstrainedTypes0),
	term__apply_substitution_to_list(UnconstrainedTypes0, RenameSubst,
		UnconstrainedTypes1),
	term__apply_rec_substitution_to_list(UnconstrainedTypes1,
		InstanceSubst, UnconstrainedTypes),

		% XXX why name of output?
	map__overlay(Proofs, SuperClassProofs2, SuperClassProofs),

		% Make the type_infos for the types
		% that are constrained by this. These
		% are packaged in the typeclass_info
	polymorphism__make_type_info_vars(ConstrainedTypes, Context,
		InstanceExtraTypeInfoVars, TypeInfoGoals, !Info),

		% Make the typeclass_infos for the constraints from the
		% context of the instance decl.
	polymorphism__make_typeclass_info_vars_2(InstanceConstraints,
		Seen, ExistQVars, Context, [],
		InstanceExtraTypeClassInfoVars0, !ExtraGoals, !Info),

		% Make the type_infos for the unconstrained
		% type variables from the head of the
		% instance declaration
	polymorphism__make_type_info_vars(UnconstrainedTypes, Context,
		InstanceExtraTypeInfoUnconstrainedVars,
		UnconstrainedTypeInfoGoals, !Info),

		% The variables are built up in reverse order.
	list__reverse(InstanceExtraTypeClassInfoVars0,
		InstanceExtraTypeClassInfoVars),

	polymorphism__construct_typeclass_info(
		InstanceExtraTypeInfoUnconstrainedVars,
		InstanceExtraTypeInfoVars,
		InstanceExtraTypeClassInfoVars,
		ClassId, Constraint, InstanceNum, ConstrainedTypes,
		SuperClassProofs, ExistQVars, Var, NewGoals, !Info),

	MaybeVar = yes(Var),

		% Oh, yuck. The type_info goals have already been
		% reversed, so lets reverse them back.
	list__reverse(TypeInfoGoals, RevTypeInfoGoals),
	list__reverse(UnconstrainedTypeInfoGoals,
		RevUnconstrainedTypeInfoGoals),

	list__condense([RevUnconstrainedTypeInfoGoals, NewGoals,
		!.ExtraGoals, RevTypeInfoGoals], !:ExtraGoals).

:- pred polymorphism__make_typeclass_info_from_subclass(class_constraint::in,
	list(class_constraint)::in, class_id::in, class_constraint::in,
	existq_tvars::in, prog_context::in, maybe(prog_var)::out,
	list(hlds_goal)::in, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__make_typeclass_info_from_subclass(Constraint,
		Seen, ClassId, SubClassConstraint, ExistQVars, Context,
		MaybeVar, !ExtraGoals, !Info) :-
	!.Info = poly_info(VarSet0, VarTypes0, TypeVarSet, TypeInfoMap0,
		TypeClassInfoMap0, Proofs, PredName, ModuleInfo),
	ClassId = class_id(ClassName, _ClassArity),
	% First create a variable to hold the new typeclass_info.
	unqualify_name(ClassName, ClassNameString),
	polymorphism__new_typeclass_info_var(Constraint, ClassNameString,
		Var, VarSet0, VarSet1, VarTypes0, VarTypes1),
	MaybeVar = yes(Var),
	% Then work out where to extract it from
	SubClassConstraint = constraint(SubClassName, SubClassTypes),
	list__length(SubClassTypes, SubClassArity),
	SubClassId = class_id(SubClassName, SubClassArity),
	!:Info = poly_info(VarSet1, VarTypes1, TypeVarSet, TypeInfoMap0,
		TypeClassInfoMap0, Proofs, PredName, ModuleInfo),

	% Make the typeclass_info for the subclass
	polymorphism__make_typeclass_info_var(SubClassConstraint, Seen,
		ExistQVars, Context, !ExtraGoals, !Info, MaybeSubClassVar),
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

	% Work out which superclass typeclass_info to take.
	map__from_corresponding_lists(SubClassVars, SubClassTypes,
		SubTypeSubst),
	apply_subst_to_constraint_list(SubTypeSubst, SuperClasses0,
		SuperClasses),
	(
		list__nth_member_search(SuperClasses, Constraint,
			SuperClassIndex0)
	->
		SuperClassIndex0 = SuperClassIndex
	;
			% We shouldn't have got this far if
			% the constraints were not satisfied
		error("polymorphism.m: constraint not in constraint list")
	),

	poly_info_get_varset(!.Info, VarSet2),
	poly_info_get_var_types(!.Info, VarTypes2),
	make_int_const_construction(SuperClassIndex, yes("SuperClassIndex"),
		IndexGoal, IndexVar, VarTypes2, VarTypes, VarSet2, VarSet),
	poly_info_set_varset_and_types(VarSet, VarTypes, !Info),

	% We extract the superclass typeclass_info by inserting a call
	% to superclass_from_typeclass_info in private_builtin.
	% Note that superclass_from_typeclass_info does not need
	% extra type_info arguments even though its declaration
	% is polymorphic.
	goal_util__generate_simple_call(mercury_private_builtin_module,
		"superclass_from_typeclass_info", predicate, only_mode, det,
		[SubClassVar, IndexVar, Var], [], [], ModuleInfo,
		term__context_init, SuperClassGoal),
	!:ExtraGoals = [SuperClassGoal, IndexGoal | !.ExtraGoals].

:- pred polymorphism__construct_typeclass_info(list(prog_var)::in,
	list(prog_var)::in, list(prog_var)::in, class_id::in,
	class_constraint::in, int::in, list(type)::in,
	map(class_constraint, constraint_proof)::in, existq_tvars::in,
	prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out)
	is det.

polymorphism__construct_typeclass_info(ArgUnconstrainedTypeInfoVars,
		ArgTypeInfoVars, ArgTypeClassInfoVars, ClassId, Constraint,
		InstanceNum, InstanceTypes, SuperClassProofs, ExistQVars,
		NewVar, NewGoals, !Info) :-

	poly_info_get_module_info(!.Info, ModuleInfo),

	module_info_classes(ModuleInfo, ClassTable),
	map__lookup(ClassTable, ClassId, ClassDefn),

	polymorphism__get_arg_superclass_vars(ClassDefn, InstanceTypes,
		SuperClassProofs, ExistQVars, ArgSuperClassVars,
		SuperClassGoals, !Info),

	poly_info_get_varset(!.Info, VarSet0),
	poly_info_get_var_types(!.Info, VarTypes0),

		% lay out the argument variables as expected in the
		% typeclass_info
	list__append(ArgTypeClassInfoVars, ArgSuperClassVars, ArgVars0),
	list__append(ArgVars0, ArgTypeInfoVars, ArgVars1),
	list__append(ArgUnconstrainedTypeInfoVars, ArgVars1, ArgVars),

	ClassId = class_id(ClassName, _Arity),

	unqualify_name(ClassName, ClassNameString),
	polymorphism__new_typeclass_info_var(Constraint, ClassNameString,
		BaseVar, VarSet0, VarSet1, VarTypes0, VarTypes1),

	module_info_instances(ModuleInfo, InstanceTable),
	map__lookup(InstanceTable, ClassId, InstanceList),
	list__index1_det(InstanceList, InstanceNum, InstanceDefn),
	InstanceModuleName = InstanceDefn ^ instance_module,
	make_instance_string(InstanceTypes, InstanceString),
	ConsId = base_typeclass_info_const(InstanceModuleName, ClassId,
		InstanceNum, InstanceString),
	BaseTypeClassInfoTerm = functor(ConsId, no, []),

		% create the construction unification to initialize the variable
	BaseUnification = construct(BaseVar, ConsId, [], [],
		construct_dynamically, cell_is_shared, no),
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
	goal_info_init(NonLocals, InstmapDelta, det, pure, BaseGoalInfo),

	BaseGoal = BaseUnify - BaseGoalInfo,

		% build a unification to add the argvars to the
		% base_typeclass_info
	NewConsId = typeclass_info_cell_constructor,
	NewArgVars = [BaseVar | ArgVars],
	TypeClassInfoTerm = functor(NewConsId, no, NewArgVars),

		% introduce a new variable
	polymorphism__new_typeclass_info_var(Constraint, ClassNameString,
		NewVar, VarSet1, VarSet, VarTypes1, VarTypes),

		% create the construction unification to initialize the
		% variable
	UniMode = (free - ground(shared, none) ->
		ground(shared, none) - ground(shared, none)),
	list__length(NewArgVars, NumArgVars),
	list__duplicate(NumArgVars, UniMode, UniModes),
	Unification = construct(NewVar, NewConsId, NewArgVars, UniModes,
		construct_dynamically, cell_is_unique, no),
	UnifyMode = (free -> ground(shared, none)) -
		(ground(shared, none) -> ground(shared, none)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(NewVar, TypeClassInfoTerm, UnifyMode, Unification,
		UnifyContext),

	% create a goal_info for the unification
	goal_info_init(GoalInfo0),
	set__list_to_set([NewVar | NewArgVars], TheNonLocals),
	goal_info_set_nonlocals(GoalInfo0, TheNonLocals, GoalInfo1),
	list__duplicate(NumArgVars, ground(shared, none), ArgInsts),
		% note that we could perhaps be more accurate than
		% `ground(shared)', but it shouldn't make any
		% difference.
	InstConsId = cell_inst_cons_id(typeclass_info_cell, NumArgVars),
	instmap_delta_from_assoc_list(
		[NewVar - bound(unique, [functor(InstConsId, ArgInsts)])],
		InstMapDelta),
	goal_info_set_instmap_delta(GoalInfo1, InstMapDelta, GoalInfo2),
	goal_info_set_determinism(GoalInfo2, det, GoalInfo),

	TypeClassInfoGoal = Unify - GoalInfo,
	NewGoals0 = [TypeClassInfoGoal, BaseGoal],
	list__append(NewGoals0, SuperClassGoals, NewGoals),
	poly_info_set_varset_and_types(VarSet, VarTypes, !Info).

%---------------------------------------------------------------------------%

:- pred polymorphism__get_arg_superclass_vars(hlds_class_defn::in,
	list(type)::in, map(class_constraint, constraint_proof)::in,
	existq_tvars::in, list(prog_var)::out, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__get_arg_superclass_vars(ClassDefn, InstanceTypes,
		SuperClassProofs, ExistQVars, NewVars, NewGoals, !Info) :-

	poly_info_get_proofs(!.Info, Proofs),

	poly_info_get_typevarset(!.Info, TVarSet0),
	SuperClasses0 = ClassDefn ^ class_supers,
	ClassVars0 = ClassDefn ^ class_vars,
	ClassTVarSet = ClassDefn ^ class_tvarset,
	varset__merge_subst(TVarSet0, ClassTVarSet, TVarSet1, Subst),
	poly_info_set_typevarset(TVarSet1, !Info),

	map__apply_to_list(ClassVars0, Subst, ClassVars1),
	term__vars_list(ClassVars1, ClassVars),
	map__from_corresponding_lists(ClassVars, InstanceTypes, TypeSubst),

	apply_subst_to_constraint_list(Subst, SuperClasses0, SuperClasses1),
	apply_rec_subst_to_constraint_list(TypeSubst, SuperClasses1,
		SuperClasses),

	poly_info_set_proofs(SuperClassProofs, !Info),
	polymorphism__make_superclasses_from_proofs(SuperClasses,
		ExistQVars, [], NewGoals, !Info, [], NewVars),

	poly_info_set_proofs(Proofs, !Info).

:- pred polymorphism__make_superclasses_from_proofs(list(class_constraint)::in,
	existq_tvars::in, list(hlds_goal)::in, list(hlds_goal)::out,
	poly_info::in, poly_info::out, list(prog_var)::in, list(prog_var)::out)
	is det.

polymorphism__make_superclasses_from_proofs([], _, !Goals, !Info, !Vars).
polymorphism__make_superclasses_from_proofs([Constraint | Constraints],
		ExistQVars, !Goals, !Info, !Vars) :-
	polymorphism__make_superclasses_from_proofs(Constraints,
		ExistQVars, !Goals, !Info, !Vars),
	term__context_init(Context),
	polymorphism__make_typeclass_info_var(Constraint, [],
		ExistQVars, Context, !Goals, !Info, MaybeVar),
	maybe_insert_var(MaybeVar, !Vars).

:- pred maybe_insert_var(maybe(prog_var)::in, list(prog_var)::in,
	list(prog_var)::out) is det.

maybe_insert_var(no, Vars, Vars).
maybe_insert_var(yes(Var), Vars, [Var | Vars]).

%-----------------------------------------------------------------------------%

	% Produce the typeclass_infos for the existential class
	% constraints for a call or deconstruction unification.
:- pred polymorphism__make_existq_typeclass_info_vars(
	list(class_constraint)::in, list(prog_var)::out, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__make_existq_typeclass_info_vars(ExistentialConstraints,
		ExtraTypeClassVars, ExtraGoals, !Info) :-
	poly_info_get_type_info_map(!.Info, OldTypeInfoMap),
	polymorphism__make_typeclass_info_head_vars(ExistentialConstraints,
		ExtraTypeClassVars, !Info),
	polymorphism__update_typeclass_infos(ExistentialConstraints,
		ExtraTypeClassVars, !Info),

	constraint_list_get_tvars(ExistentialConstraints, TVars0),
	list__sort_and_remove_dups(TVars0, TVars),
	list__foldl2(polymorphism__maybe_extract_type_info(OldTypeInfoMap),
		TVars, [], ExtraGoals, !Info).

	% For code which requires mode reordering, we may have already
	% seen uses of some of the type variables produced by this call.
	% At the point of the use of a type variable that we haven't seen
	% before, we assume that it is unconstrained. If it turns out that
	% the type variable is constrained, and the type_info is contained
	% in a typeclass_info, we need to generate code to extract it here.
:- pred polymorphism__maybe_extract_type_info(type_info_varmap::in,
	tvar::in, list(hlds_goal)::in, list(hlds_goal)::out,
	poly_info::in, poly_info::out) is det.

polymorphism__maybe_extract_type_info(OldTypeInfoMap, TVar, !ExtraGoals,
		!Info) :-
	poly_info_get_type_info_map(!.Info, TypeInfoMap),
	(
		map__search(OldTypeInfoMap, TVar, type_info(TypeInfoVar0)),
		map__search(TypeInfoMap, TVar,
			typeclass_info(TypeClassInfoVar, Index))
	->
		extract_type_info(TVar, TypeClassInfoVar,
			Index, NewGoals, TypeInfoVar1, !Info),
		polymorphism__assign_var(TypeInfoVar0,
			TypeInfoVar1, AssignGoal),
		!:ExtraGoals = NewGoals ++ [AssignGoal | !.ExtraGoals]
	;
		true
	).

%---------------------------------------------------------------------------%

% Given a list of types, create a list of variables to hold the type_info
% for those types, and create a list of goals to initialize those type_info
% variables to the appropriate type_info structures for the types.
% Update the varset and vartypes accordingly.

polymorphism__make_type_info_vars([], _, [], [], !Info).
polymorphism__make_type_info_vars([Type | Types], Context,
		ExtraVars, ExtraGoals, !Info) :-
	polymorphism__make_type_info_var(Type, Context,
		Var, ExtraGoals1, !Info),
	polymorphism__make_type_info_vars(Types, Context,
		ExtraVars2, ExtraGoals2, !Info),
	ExtraVars = [Var | ExtraVars2],
	list__append(ExtraGoals1, ExtraGoals2, ExtraGoals).

polymorphism__make_type_info_var(Type, Context, Var, ExtraGoals, !Info) :-
	%
	% First handle statically known types
	% (i.e. types which are not type variables)
	%
	( type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs) ->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a type whose type constructor is of variable
		% arity.
		% The transformation we perform is basically the same as
		% in the usual case below, except that we map
		% pred types to pred/0, func types to func/0 and tuple
		% types to tuple/0 for the purposes of creating type_infos.
		% To allow univ_to_type to check the type_infos
		% correctly, the actual arity is added to the type_info
		% we create.
		%
		% XXX FIXME (RTTI for higher-order impure code)
		% we should not ignore the purity of higher order procs;
		% it should get included in the RTTI.
		polymorphism__construct_type_info(Type, TypeCtor, TypeArgs,
			yes, Context, Var, ExtraGoals, !Info)
	; type_to_ctor_and_args(Type, TypeCtor, TypeArgs) ->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a known value of the type variable.
		% The transformation we perform is shown in the comment
		% at the top of the module.

		polymorphism__construct_type_info(Type, TypeCtor, TypeArgs,
			no, Context, Var, ExtraGoals, !Info)
	;
	%
	% Now handle the cases of types which are not known statically
	% (i.e. type variables)
	%
		Type = term__variable(TypeVar)
	->
		get_type_info_locn(TypeVar, TypeInfoLocn, !Info),
		get_type_info(TypeInfoLocn, TypeVar, ExtraGoals, Var, !Info)
	;
		error("polymorphism__make_var: unknown type")
	).

:- pred get_type_info_locn(tvar::in, type_info_locn::out,
	poly_info::in, poly_info::out) is det.

get_type_info_locn(TypeVar, TypeInfoLocn, !Info) :-
	%
	% If we have already allocated a location for this type_info,
	% then all we need to do is to extract the type_info variable
	% from its location.
	%
	poly_info_get_type_info_map(!.Info, TypeInfoMap0),
	( map__search(TypeInfoMap0, TypeVar, TypeInfoLocn0) ->
		TypeInfoLocn = TypeInfoLocn0
	;
		%
		% Otherwise, we need to create a new type_info variable, and
		% set the location for this type variable to be that
		% type_info variable.
		%
		% This is wrong if the type variable is one of the
		% existentially quantified variables of a called predicate
		% and the variable occurs in an existential type-class
		% constraint. In that case the type-info will be stored
		% in the typeclass_info variable produced by the predicate,
		% not in a type_info variable. make_typeclass_info_headvar
		% will fix this up when the typeclass_info is created.
		%
		prog_type__var(Type, TypeVar),
		polymorphism__new_type_info_var(Type, type_info, Var, !Info),
		TypeInfoLocn = type_info(Var),
		map__det_insert(TypeInfoMap0, TypeVar, TypeInfoLocn,
			TypeInfoMap),
		poly_info_set_type_info_map(TypeInfoMap, !Info)
	).

:- pred polymorphism__construct_type_info((type)::in, type_ctor::in,
	list(type)::in, bool::in, prog_context::in, prog_var::out,
	list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism__construct_type_info(Type, TypeCtor, TypeArgs,
		TypeCtorIsVarArity, Context, Var, ExtraGoals, !Info) :-
	% Create the typeinfo vars for the arguments
	polymorphism__make_type_info_vars(TypeArgs, Context,
		ArgTypeInfoVars, ArgTypeInfoGoals, !Info),

	poly_info_get_varset(!.Info, VarSet1),
	poly_info_get_var_types(!.Info, VarTypes1),
	poly_info_get_module_info(!.Info, ModuleInfo),

	polymorphism__init_const_type_ctor_info_var(Type, TypeCtor,
		TypeCtorVar, TypeCtorGoal, ModuleInfo,
		VarSet1, VarSet2, VarTypes1, VarTypes2),
	polymorphism__maybe_init_second_cell(Type, TypeCtorVar,
		TypeCtorIsVarArity, ArgTypeInfoVars, Context, Var,
		VarSet2, VarSet, VarTypes2, VarTypes,
		ArgTypeInfoGoals, [TypeCtorGoal], ExtraGoals),

	poly_info_set_varset_and_types(VarSet, VarTypes, !Info).

	% maybe_init_second_cell(Type, TypeCtorVar, TypeCtorIsVarArity,
	%	ArgTypeInfoVars, Context, Var, VarSet0, VarSet,
	%	VarTypes0, VarTypes, ArgTypeInfoGoals, ExtraGoals0, ExtraGoals):
	%
	% Create a unification the constructs the second cell of a type_info
	% for Type if necessary. This cell will usually be of the form:
	%
	%	TypeInfoVar = type_info(TypeCtorVar, ArgTypeInfoVars...)
	%
	% However, if TypeCtorIsVarArity is true, then it will be of the form
	%
	% 	TypeInfoVar = type_info(TypeCtorVar, Arity, ArgTypeInfoVars...)
	%
	% TypeCtorVar should be the variable holding the type_ctor_info for the
	% principal type constructor of Type, and TypeCtorIsVarArity should be
	% true iff the type constructor it represents has a variable arity.
	%
	% ArgTypeInfoVars should be variables holding the type_infos (or
	% type_ctor_infos for zero-arity types) of the argument types of Type.
	%
	% The returned Var will be bound to the type_info cell of Type if such
	% a cell had to be allocated, and to the type_ctor_info of Type's only
	% type constructor if it didn't. The returned ExtraGoals is a
	% concatenation of ArgTypeInfoGoals, ExtraGoals0, and any goals needed
	% to construct Var.

:- pred polymorphism__maybe_init_second_cell((type)::in, prog_var::in,
	bool::in, list(prog_var)::in, prog_context::in, prog_var::out,
	prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out,
	list(hlds_goal)::in, list(hlds_goal)::in, list(hlds_goal)::out) is det.

polymorphism__maybe_init_second_cell(Type, TypeCtorVar, TypeCtorIsVarArity,
		ArgTypeInfoVars, _Context, Var, !VarSet, !VarTypes,
		ArgTypeInfoGoals, ExtraGoals0, ExtraGoals) :-
	(
		TypeCtorIsVarArity = yes,
		% Unfortunately, if the type's type constructor has variable
		% arity, we cannot use a one-cell representation for that type.
		list__length(ArgTypeInfoVars, ActualArity),
		make_int_const_construction(ActualArity, yes("ActualArity"),
			ArityGoal, ArityVar, !VarTypes, !VarSet),
		polymorphism__init_type_info_var(Type,
			[TypeCtorVar, ArityVar | ArgTypeInfoVars],
			no, Var, TypeInfoGoal, !VarSet, !VarTypes),
		list__append([ArityGoal |  ArgTypeInfoGoals], [TypeInfoGoal],
			ExtraGoals1),
		list__append(ExtraGoals0, ExtraGoals1, ExtraGoals)
	;
		TypeCtorIsVarArity = no,
		(
			ArgTypeInfoVars = [_ | _],
			polymorphism__init_type_info_var(Type,
				[TypeCtorVar | ArgTypeInfoVars], no, Var,
				TypeInfoGoal, !VarSet, !VarTypes),
			list__append(ArgTypeInfoGoals, [TypeInfoGoal],
				ExtraGoals1),
			list__append(ExtraGoals0, ExtraGoals1, ExtraGoals)
		;
			ArgTypeInfoVars = [],
			% Since this type_ctor_info is pretending to be
			% a type_info, we need to adjust its type.
			% Since type_ctor_info_const cons_ids are handled
			% specially, this should not cause problems.
			polymorphism__build_type_info_type(type_info, Type,
				TypeInfoType),
			map__det_update(!.VarTypes, TypeCtorVar, TypeInfoType,
				!:VarTypes),
			Var = TypeCtorVar,
			list__append(ArgTypeInfoGoals, ExtraGoals0, ExtraGoals)

			% The type_info to represent Type is just a
			% type_ctor_info. We used to simply change the type
			% of TypeCtorVar from type_ctor_info(Type) to
			% type_info(Type), but that would confuse size_prof.m.
			% We cannot leave its type as it is without extending
			% type_util.type_unify to consider type_ctor_info and
			% type_info interchangeable. We therefore create a
			% new variable of type type_info(Type), and cast
			% TypeCtorVar to it.
			%
			% polymorphism__new_type_info_var_raw(Type, type_info,
			% 	Var, !VarSet, !VarTypes),
			% generate_unsafe_cast(TypeCtorVar, Var, Context,
			%	CastGoal),
			% list__append(ArgTypeInfoGoals, [CastGoal],
			%	ExtraGoals1),
			% list__append(ExtraGoals0, ExtraGoals1, ExtraGoals)
		)
	).

polymorphism__get_special_proc(Type, SpecialPredId, ModuleInfo,
		PredName, PredId, ProcId) :-
	TypeCategory = classify_type(ModuleInfo, Type),
	polymorphism__get_category_name(TypeCategory) = MaybeCategoryName,
	(
		MaybeCategoryName = no,
		module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
		( type_to_ctor_and_args(Type, TypeCtor, _TypeArgs) ->
			map__search(SpecialPredMap, SpecialPredId - TypeCtor,
				PredId)
		;
			error("polymorphism__get_special_proc: " ++
				"type_to_ctor_and_args failed")
		),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		Module = pred_info_module(PredInfo),
		Name = pred_info_name(PredInfo),
		PredName = qualified(Module, Name),
		special_pred_mode_num(SpecialPredId, ProcInt),
		proc_id_to_int(ProcId, ProcInt)
	;
		MaybeCategoryName = yes(CategoryName),
		special_pred_name_arity(SpecialPredId, SpecialName, Arity),
		string__append_list(
			["builtin_", SpecialName, "_", CategoryName], Name),
		lookup_builtin_pred_proc_id(ModuleInfo,
			mercury_private_builtin_module, Name, predicate,
			Arity, only_mode, PredId, ProcId),
		PredName = qualified(mercury_private_builtin_module, Name)
	).

:- func polymorphism__get_category_name(type_category) = maybe(string).

polymorphism__get_category_name(int_type) = yes("int").
polymorphism__get_category_name(char_type) = yes("int").
polymorphism__get_category_name(enum_type) = no.
polymorphism__get_category_name(float_type) = yes("float").
polymorphism__get_category_name(str_type) = yes("string").
polymorphism__get_category_name(higher_order_type) = yes("pred").
polymorphism__get_category_name(tuple_type) = yes("tuple").
polymorphism__get_category_name(variable_type) = _ :-
	error("polymorphism__get_category_name: variable type").
polymorphism__get_category_name(void_type) = _ :-
	error("polymorphism__get_category_name: void_type").
polymorphism__get_category_name(user_ctor_type) = no.
polymorphism__get_category_name(type_info_type) = no.
polymorphism__get_category_name(type_ctor_info_type) = no.
polymorphism__get_category_name(typeclass_info_type) = no.
polymorphism__get_category_name(base_typeclass_info_type) = no.

polymorphism__init_type_info_var(Type, ArgVars, MaybePreferredVar, TypeInfoVar,
		TypeInfoGoal, !VarSet, !VarTypes) :-
	( type_to_ctor_and_args(Type, Ctor, _) ->
		Cell = type_info_cell(Ctor)
	;
		error(
	"polymorphism__init_type_info_var: type_to_ctor_and_args failed")
	),
	ConsId = cell_cons_id(Cell),
	TypeInfoTerm = functor(ConsId, no, ArgVars),

	% introduce a new variable
	(
		MaybePreferredVar = yes(TypeInfoVar)
	;
		MaybePreferredVar = no,
		polymorphism__new_type_info_var_raw(Type, type_info,
			TypeInfoVar, !VarSet, !VarTypes)
	),

	% create the construction unification to initialize the variable
	UniMode = (free - ground(shared, none) ->
		ground(shared, none) - ground(shared, none)),
	list__length(ArgVars, NumArgVars),
	list__duplicate(NumArgVars, UniMode, UniModes),
	Unification = construct(TypeInfoVar, ConsId, ArgVars, UniModes,
		construct_dynamically, cell_is_unique, no),
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
	InstConsId = cell_inst_cons_id(Cell, NumArgVars),
	instmap_delta_from_assoc_list(
		[TypeInfoVar - bound(unique, [functor(InstConsId, ArgInsts)])],
		InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, pure, GoalInfo),

	TypeInfoGoal = Unify - GoalInfo.

polymorphism__init_const_type_ctor_info_var(Type, TypeCtor, TypeCtorInfoVar,
		TypeCtorInfoGoal, ModuleInfo, !VarSet, !VarTypes) :-
	type_util__type_ctor_module(ModuleInfo, TypeCtor, ModuleName),
	type_util__type_ctor_name(ModuleInfo, TypeCtor, TypeName),
	TypeCtor = _ - Arity,
	ConsId = type_ctor_info_const(ModuleName, TypeName, Arity),
	TypeInfoTerm = functor(ConsId, no, []),

	% introduce a new variable
	polymorphism__new_type_info_var_raw(Type, type_ctor_info,
		TypeCtorInfoVar, !VarSet, !VarTypes),

	% create the construction unification to initialize the variable
	Unification = construct(TypeCtorInfoVar, ConsId, [], [],
		construct_dynamically, cell_is_shared, no),
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
	goal_info_init(NonLocals, InstmapDelta, det, pure, GoalInfo),

	TypeCtorInfoGoal = Unify - GoalInfo.

%---------------------------------------------------------------------------%

:- pred polymorphism__make_head_vars(list(tvar)::in, tvarset::in,
	list(prog_var)::out, poly_info::in, poly_info::out) is det.

polymorphism__make_head_vars([], _, [], !Info).
polymorphism__make_head_vars([TypeVar | TypeVars], TypeVarSet, TypeInfoVars,
		!Info) :-
	Type = term__variable(TypeVar),
	polymorphism__new_type_info_var(Type, type_info, Var, !Info),
	( varset__search_name(TypeVarSet, TypeVar, TypeVarName) ->
		poly_info_get_varset(!.Info, VarSet0),
		string__append("TypeInfo_for_", TypeVarName, VarName),
		varset__name_var(VarSet0, Var, VarName, VarSet),
		poly_info_set_varset(VarSet, !Info)
	;
		true
	),
	polymorphism__make_head_vars(TypeVars, TypeVarSet, TypeInfoVars1,
		!Info),
	TypeInfoVars = [Var | TypeInfoVars1].

:- pred polymorphism__new_type_info_var((type)::in, type_info_kind::in,
	prog_var::out, poly_info::in, poly_info::out) is det.

polymorphism__new_type_info_var(Type, Kind, Var, !Info) :-
	poly_info_get_varset(!.Info, VarSet0),
	poly_info_get_var_types(!.Info, VarTypes0),
	polymorphism__new_type_info_var_raw(Type, Kind, Var,
		VarSet0, VarSet, VarTypes0, VarTypes),
	poly_info_set_varset_and_types(VarSet, VarTypes, !Info).

polymorphism__new_type_info_var_raw(Type, Kind, Var, !VarSet, !VarTypes) :-
	% introduce new variable
	varset__new_var(!.VarSet, Var, !:VarSet),
	term__var_to_int(Var, VarNum),
	string__int_to_string(VarNum, VarNumStr),
	(
		Kind = type_info,
		Prefix = typeinfo_prefix
	;
		Kind = type_ctor_info,
		Prefix = typectorinfo_prefix
	),
	string__append(Prefix, VarNumStr, Name),
	varset__name_var(!.VarSet, Var, Name, !:VarSet),
	polymorphism__build_type_info_type(Kind, Type, TypeInfoType),
	map__set(!.VarTypes, Var, TypeInfoType, !:VarTypes).

:- func typeinfo_prefix = string.

typeinfo_prefix = "TypeInfo_".

:- func typectorinfo_prefix = string.

typectorinfo_prefix = "TypeCtorInfo_".

%---------------------------------------------------------------------------%

% Generate code to get the value of a type variable.

:- pred get_type_info(type_info_locn::in, tvar::in, list(hlds_goal)::out,
	prog_var::out, poly_info::in, poly_info::out) is det.

get_type_info(TypeInfoLocn, TypeVar, ExtraGoals, Var, !Info) :-
	(
			% If the typeinfo is available in a variable,
			% just use it
		TypeInfoLocn = type_info(TypeInfoVar),
		Var = TypeInfoVar,
		ExtraGoals = []
	;
			% If the typeinfo is in a typeclass_info, then
			% we need to extract it before using it
		TypeInfoLocn = typeclass_info(TypeClassInfoVar, Index),
		extract_type_info(TypeVar, TypeClassInfoVar,
			Index, ExtraGoals, Var, !Info)
	).

:- pred extract_type_info(tvar::in, prog_var::in, int::in,
	list(hlds_goal)::out, prog_var::out, poly_info::in, poly_info::out)
	is det.

extract_type_info(TypeVar, TypeClassInfoVar, Index, Goals, TypeInfoVar,
		!Info) :-
	poly_info_get_varset(!.Info, VarSet0),
	poly_info_get_var_types(!.Info, VarTypes0),
	poly_info_get_module_info(!.Info, ModuleInfo),
	polymorphism__gen_extract_type_info(TypeVar, TypeClassInfoVar, Index,
		ModuleInfo, Goals, TypeInfoVar,
		VarSet0, VarSet, VarTypes0, VarTypes),
	poly_info_set_varset_and_types(VarSet, VarTypes, !Info).

polymorphism__gen_extract_type_info(TypeVar, TypeClassInfoVar, Index,
		ModuleInfo, Goals, TypeInfoVar, !VarSet, !VarTypes) :-
	make_int_const_construction(Index, yes("TypeInfoIndex"),
		IndexGoal, IndexVar, !VarTypes, !VarSet),
	polymorphism__new_type_info_var_raw(term__variable(TypeVar), type_info,
		TypeInfoVar, !VarSet, !VarTypes),
	goal_util__generate_simple_call(mercury_private_builtin_module,
		"type_info_from_typeclass_info", predicate, only_mode, det,
		[TypeClassInfoVar, IndexVar, TypeInfoVar], [],
		[TypeInfoVar - ground(shared, none)], ModuleInfo,
		term__context_init, CallGoal),
	Goals = [IndexGoal, CallGoal].

%-----------------------------------------------------------------------------%

	% Create a head var for each class constraint, and make an entry in
	% the typeinfo locations map for each constrained type var.

:- pred polymorphism__make_typeclass_info_head_vars(list(class_constraint)::in,
	list(prog_var)::out, poly_info::in, poly_info::out) is det.

polymorphism__make_typeclass_info_head_vars(Constraints, ExtraHeadVars,
		!Info) :-
	list__map_foldl(polymorphism__make_typeclass_info_head_var,
		Constraints, ExtraHeadVars, !Info).

:- pred polymorphism__make_typeclass_info_head_var(class_constraint::in,
	prog_var::out, poly_info::in, poly_info::out) is det.

polymorphism__make_typeclass_info_head_var(Constraint, ExtraHeadVar, !Info) :-
	poly_info_get_typeclass_info_map(!.Info, TypeClassInfoMap),
	( map__search(TypeClassInfoMap, Constraint, ExistingVar) ->
		ExtraHeadVar = ExistingVar
	;
		poly_info_get_varset(!.Info, VarSet0),
		poly_info_get_var_types(!.Info, VarTypes0),
		poly_info_get_type_info_map(!.Info, TypeInfoMap0),
		poly_info_get_module_info(!.Info, ModuleInfo),

		Constraint = constraint(ClassName0, ClassTypes),

			% Work out how many superclasses the class has.
		list__length(ClassTypes, ClassArity),
		ClassId = class_id(ClassName0, ClassArity),
		module_info_classes(ModuleInfo, ClassTable),
		map__lookup(ClassTable, ClassId, ClassDefn),
		SuperClasses = ClassDefn ^ class_supers,
		list__length(SuperClasses, NumSuperClasses),

		unqualify_name(ClassName0, ClassName),

			% Make a new variable to contain the dictionary for
			% this typeclass constraint.
		polymorphism__new_typeclass_info_var(Constraint, ClassName,
			ExtraHeadVar, VarSet0, VarSet1, VarTypes0, VarTypes1),

			% Find all the type variables in the constraint, and
			% remember what index they appear in in the typeclass
			% info.

			% The first type_info will be just after the superclass
			% infos.
		First = NumSuperClasses + 1,
		term__vars_list(ClassTypes, ClassTypeVars0),
		MakeIndex = (pred(Elem0::in, Elem::out,
					Index0::in, Index::out) is det :-
				Elem = Elem0 - Index0,
				Index = Index0 + 1,
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

		poly_info_set_varset_and_types(VarSet1, VarTypes1, !Info),
		poly_info_set_type_info_map(TypeInfoMap1, !Info)
	).

:- pred is_pair(pair(_, _)::in) is det.

is_pair(_).

:- pred polymorphism__new_typeclass_info_var(class_constraint::in, string::in,
	prog_var::out, prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out) is det.

polymorphism__new_typeclass_info_var(Constraint, ClassString, Var,
		!VarSet, !VarTypes) :-
	% introduce new variable
	varset__new_var(!.VarSet, Var, !:VarSet),
	string__append("TypeClassInfo_for_", ClassString, Name),
	varset__name_var(!.VarSet, Var, Name, !:VarSet),
	polymorphism__build_typeclass_info_type(Constraint, DictionaryType),
	map__set(!.VarTypes, Var, DictionaryType, !:VarTypes).

polymorphism__build_typeclass_info_type(Constraint, DictionaryType) :-
	Constraint = constraint(SymName, ArgTypes),

	% `constraint/n' is not really a type - it is a representation of a
	% class constraint about which a typeclass_info holds information.
	% `type_util:type_to_ctor_and_args' treats it as a type variable.
	construct_qualified_term(SymName, [], ClassNameTerm),
	PrivateBuiltin = mercury_private_builtin_module,
	construct_qualified_term(qualified(PrivateBuiltin, "constraint"),
		[ClassNameTerm | ArgTypes], ConstraintTerm),
	construct_type(qualified(PrivateBuiltin, "typeclass_info") - 1,
		[ConstraintTerm], DictionaryType).

%---------------------------------------------------------------------------%

polymorphism__typeclass_info_class_constraint(TypeClassInfoType, Constraint) :-
	PrivateBuiltin = mercury_private_builtin_module,
	type_to_ctor_and_args(TypeClassInfoType,
		qualified(PrivateBuiltin, "typeclass_info") - 1,
		[ConstraintTerm]),

	% type_to_ctor_and_args fails on `constraint/n', so we use
	% `sym_name_and_args' instead.
	sym_name_and_args(ConstraintTerm,
		qualified(PrivateBuiltin, "constraint"),
		[ClassNameTerm | ArgTypes]),
	sym_name_and_args(ClassNameTerm, ClassName, []),
	Constraint = constraint(ClassName, ArgTypes).

polymorphism__type_info_or_ctor_type(TypeInfoType, Type) :-
	type_to_ctor_and_args(TypeInfoType,
		qualified(mercury_private_builtin_module, TypeName) - 1,
		[Type]),
	( TypeName = "type_info" ; TypeName = "type_ctor_info" ).

polymorphism__build_type_info_type(Type, TypeInfoType) :-
	( type_has_variable_arity_ctor(Type, _, _) ->
		% We cannot use a plain type_ctor_info because we need to
		% record the arity.
		Kind = type_info
	; type_to_ctor_and_args(Type, _Ctor, Args) ->
		(
			Args = [],
			Kind = type_ctor_info
		;
			Args = [_ | _],
			Kind = type_info
		)
	;
		% The type is variable, which means we have a type_info for it.
		% That type_info may actually be a type_ctor_info, but the code
		% of the current predicate won't treat it as such.
		Kind = type_info
	),
	polymorphism__build_type_info_type(Kind, Type, TypeInfoType).

:- pred polymorphism__build_type_info_type(type_info_kind::in,
	(type)::in, (type)::out) is det.

polymorphism__build_type_info_type(Kind, Type, TypeInfoType) :-
	(
		Kind = type_info,
		TypeInfoType = type_info_type(Type)
	;
		Kind = type_ctor_info,
		TypeInfoType = type_ctor_info_type(Type)
	).

%---------------------------------------------------------------------------%

polymorphism__is_typeclass_info_manipulator(ModuleInfo, PredId,
		TypeClassManipulator) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	mercury_private_builtin_module = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
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
:- pred polymorphism__expand_class_method_bodies(module_info::in,
	module_info::out) is det.

polymorphism__expand_class_method_bodies(!ModuleInfo) :-
	module_info_classes(!.ModuleInfo, Classes),
	module_info_name(!.ModuleInfo, ModuleName),
	map__keys(Classes, ClassIds0),

	module_info_globals(!.ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, user_guided_type_specialization,
		TypeSpec),
	(
		TypeSpec = no,
			% Don't expand classes from other modules
		list__filter(class_id_is_from_given_module(ModuleName),
			ClassIds0, ClassIds)
	;
		TypeSpec = yes,
		ClassIds = ClassIds0
	),
	map__apply_to_list(ClassIds, Classes, ClassDefns),
	list__foldl(expand_bodies, ClassDefns, !ModuleInfo).

:- pred class_id_is_from_given_module(module_name::in, class_id::in)
	is semidet.

class_id_is_from_given_module(ModuleName, ClassId) :-
	ClassId = class_id(qualified(ModuleName, _), _).

:- pred expand_bodies(hlds_class_defn::in, module_info::in, module_info::out)
	is det.

expand_bodies(HLDSClassDefn, !ModuleInfo) :-
	Interface = HLDSClassDefn ^ class_hlds_interface,
	list__foldl2(expand_one_body, Interface, 1, _, !ModuleInfo).

:- pred expand_one_body(hlds_class_proc::in, int::in, int::out,
	module_info::in, module_info::out) is det.

expand_one_body(hlds_class_proc(PredId, ProcId), !ProcNum, !ModuleInfo) :-
	module_info_preds(!.ModuleInfo, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

		% Find which of the constraints on the pred is the one
		% introduced because it is a class method.
	pred_info_get_class_context(PredInfo0, ClassContext),
	(
		ClassContext = constraints([Head | _], _)
	->
		InstanceConstraint = Head
	;
		error("expand_one_body: class method is not constrained")
	),

	proc_info_typeclass_info_varmap(ProcInfo0, VarMap),
	map__lookup(VarMap, InstanceConstraint, TypeClassInfoVar),

	proc_info_headvars(ProcInfo0, HeadVars0),
	proc_info_argmodes(ProcInfo0, Modes0),
	proc_info_declared_determinism(ProcInfo0, MaybeDetism0),
	( MaybeDetism0 = yes(Detism0) ->
		Detism = Detism0
	;
		% Omitting the determinism for a method is not allowed.
		% But make_hlds.m will have already detected and reported
		% the error.  So here we can just pick some value at random;
		% hopefully something that won't cause flow-on errors.
		% We also mark the predicate as invalid, also to avoid
		% flow-on errors.
		Detism = nondet,
		module_info_remove_predid(PredId, !ModuleInfo)
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
		class_method(TypeClassInfoVar, !.ProcNum,
			class_id(ClassName, InstanceArity), CallId),
		HeadVars, Modes, Detism),

		% Make the goal info for the call.
	set__list_to_set(HeadVars0, NonLocals),
	instmap_delta_from_mode_list(HeadVars0, Modes0, !.ModuleInfo,
		InstmapDelta),
	pred_info_get_purity(PredInfo0, Purity),
	goal_info_init(NonLocals, InstmapDelta, Detism, Purity, GoalInfo),
	BodyGoal = BodyGoalExpr - GoalInfo,

	proc_info_set_goal(BodyGoal, ProcInfo0, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(ProcTable, PredInfo0, PredInfo1),
	( pred_info_is_imported(PredInfo1) ->
		pred_info_set_import_status(opt_imported, PredInfo1, PredInfo)
	;
		PredInfo = PredInfo1
	),

	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(PredTable, !ModuleInfo),

	!:ProcNum = !.ProcNum + 1.

:- pred delete_nth(list(T)::in, int::in, list(T)::out) is semidet.

delete_nth([X | Xs], N0, Result) :-
	( N0 > 1 ->
		N = N0 - 1,
		delete_nth(Xs, N, TheRest),
		Result = [X | TheRest]
	;
		Result = Xs
	).

%---------------------------------------------------------------------------%

:- func get_constrained_vars(class_constraint) = list(tvar).

get_constrained_vars(ClassConstraint) = CVars :-
	ClassConstraint = constraint(_, CTypes),
	term__vars_list(CTypes, CVars).

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

		typeclass_info_map	:: map(class_constraint, prog_var),
					% specifies the location of
					% the typeclass_info var
					% for each of the pred's class
					% constraints
		proof_map		:: map(class_constraint,
						constraint_proof),
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
:- pred init_poly_info(module_info::in, pred_info::in, clauses_info::in,
	poly_info::out) is det.

init_poly_info(ModuleInfo, PredInfo, ClausesInfo, PolyInfo) :-
	clauses_info_varset(ClausesInfo, VarSet),
	clauses_info_vartypes(ClausesInfo, VarTypes),
	pred_info_typevarset(PredInfo, TypeVarSet),
	pred_info_get_constraint_proofs(PredInfo, Proofs),
	map__init(TypeInfoMap),
	map__init(TypeClassInfoMap),
	PolyInfo = poly_info(VarSet, VarTypes, TypeVarSet, TypeInfoMap,
		TypeClassInfoMap, Proofs, PredInfo, ModuleInfo).

	% create_poly_info creates a poly_info for an existing procedure.
	% (See also init_poly_info.)
create_poly_info(ModuleInfo, PredInfo, ProcInfo, PolyInfo) :-
	pred_info_typevarset(PredInfo, TypeVarSet),
	pred_info_get_constraint_proofs(PredInfo, Proofs),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_typeinfo_varmap(ProcInfo, TypeInfoMap),
	proc_info_typeclass_info_varmap(ProcInfo, TypeClassInfoMap),
	PolyInfo = poly_info(VarSet, VarTypes, TypeVarSet, TypeInfoMap,
		TypeClassInfoMap, Proofs, PredInfo, ModuleInfo).

	% create_poly_info creates a poly_info for a call.
	% (See also init_poly_info.)
create_poly_info_for_new_call(ModuleInfo, PredInfo, ProcInfo, VarSet, VarTypes,
		PolyInfo) :-
	pred_info_typevarset(PredInfo, TypeVarSet),
	pred_info_get_constraint_proofs(PredInfo, Proofs),
	proc_info_typeinfo_varmap(ProcInfo, TypeInfoMap),
	proc_info_typeclass_info_varmap(ProcInfo, TypeClassInfoMap),
	PolyInfo = poly_info(VarSet, VarTypes, TypeVarSet, TypeInfoMap,
		TypeClassInfoMap, Proofs, PredInfo, ModuleInfo).

poly_info_extract(Info, !PredInfo, !ProcInfo, ModuleInfo) :-
	Info = poly_info(VarSet, VarTypes, TypeVarSet, TypeInfoMap,
		TypeclassInfoLocations, _Proofs, _OldPredInfo, ModuleInfo),

	% set the new values of the fields in proc_info and pred_info
	proc_info_set_varset(VarSet, !ProcInfo),
	proc_info_set_vartypes(VarTypes, !ProcInfo),
	proc_info_set_typeinfo_varmap(TypeInfoMap, !ProcInfo),
	proc_info_set_typeclass_info_varmap(TypeclassInfoLocations, !ProcInfo),
	pred_info_set_typevarset(TypeVarSet, !PredInfo).

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

poly_info_get_varset(PolyInfo, PolyInfo ^ varset).
poly_info_get_var_types(PolyInfo, PolyInfo ^ vartypes).
poly_info_get_typevarset(PolyInfo, PolyInfo ^ typevarset).
poly_info_get_type_info_map(PolyInfo, PolyInfo ^ type_info_varmap).
poly_info_get_typeclass_info_map(PolyInfo, PolyInfo ^ typeclass_info_map).
poly_info_get_proofs(PolyInfo, PolyInfo ^ proof_map).
poly_info_get_pred_info(PolyInfo, PolyInfo ^ pred_info).
poly_info_get_module_info(PolyInfo, PolyInfo ^ module_info).

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

poly_info_set_varset(VarSet, PI, PI ^ varset := VarSet).
poly_info_set_varset_and_types(VarSet, VarTypes, PI,
	(PI ^ varset := VarSet) ^ vartypes := VarTypes).
poly_info_set_typevarset(TVarSet, PI, PI ^ typevarset := TVarSet).
poly_info_set_type_info_map(TVarMap, PI, PI ^ type_info_varmap := TVarMap).
poly_info_set_typeclass_info_map(TypeClassInfoMap, PI,
	PI ^ typeclass_info_map := TypeClassInfoMap).
poly_info_set_proofs(Proofs, PI, PI ^ proof_map := Proofs).
poly_info_set_module_info(ModuleInfo, PI, PI ^ module_info := ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
