%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: polymorphism.m
% main author: fjh

% This module is a pass over the HLDS.
% It does a syntactic transformation to implement polymorphism, including
% typeclasses, using higher-order predicates, and also invokes
% `lambda__transform_lambda' to handle lambda expressions by creating new
% predicates for them.

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
% (currently unify/2, compare/3, index/2).
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
% is always present is the type_ctor_info structure, laid out like this:
%
%	word 0		<arity of type constructor>
%			e.g. 0 for `int', 1 for `list(T)', 2 for `map(K, V)'.
%	word 1		<=/2 predicate for type>
%	word 2		<index/2 predicate for type>
%	word 3		<compare/3 predicate for type>
%	word 4		<MR_TypeCtorRepresentation for type constructor>
%	word 5		<type_ctor_functors for type>
%	word 6		<type_ctor_layout for type>
%	word 7		<string name of type constructor>
%			e.g. "int" for `int', "list" for `list(T)',
%			"map" for `map(K,V)'
%	word 8		<string name of module>
%
% The other cell is the type_info structure, laid out like this:
%
%	word 0		<pointer to the type_ctor_info structure>
%	word 1+		<the type_infos for the type params, at least one>
%
%	(but see note below for how higher order types differ)
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
% Higher order types:
%
% There is a slight variation on this for higher-order types. Higher
% order type_infos always have a pointer to the pred/0 type_ctor_info,
% regardless of their true arity, so we store the real arity in the
% type-info as well.
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
% Currently we use option 2.
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
%		TypeCtorInfoT2 = type_ctor_info(
%			1,
%			'__Unify__'<list/1>,
%			'__Index__'<list/1>,
%			'__Compare__'<list/1>,
%			<type_ctor_layout for list/1>,
%			<type_ctor_functors for list/1>,
%			"list",
%			"list"),
%		TypeInfoT2 = type_info(
%			TypeCtorInfoT2,
%			TypeInfoT1),
%		q(TypeInfoT2, [X]),
%		TypeInfoT3 = type_ctor_info(
%			0,
%			builtin_unify_int,
%			builtin_index_int,
%			builtin_compare_int,
%			<type_ctor_layout for int/0>,
%			<type_ctor_functors for int/0>,
%			"int",
%			"builtin"),
%		r(TypeInfoT3, 0).
%
% Note that type_ctor_infos are actually generated as references to a
% single shared type_ctor_info.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Tranformation of code using typeclasses:
%
% Every predicate which has a typeclass constraint is given an extra
% argument for every constraint in the predicate's type declaration.
% The argument is the "dictionary", or "typeclass_info" for the typeclass.
% The dictionary contains pointers to each of the class methods.
%
%-----------------------------------------------------------------------------%
%
% Representation of a typeclass_info:
%	The typeclass_info is represented in two parts (the typeclass_info
%	itself, and a base_typeclass_info), in a similar fashion to the
%	type_info being represented in two parts (the type_info and the
%	type_ctor_info).
%
%		The base_typeclass_info contains:
%		  * the number of constraints on the instance decl.
%		  * pointer to method #1
%		    ...
%		  * pointer to method #n
%
%		The typeclass_info contains:
%		  * a pointer to the base typeclass info
%		  * typeclass info #1 for constraint on instance decl
%		  * ...
%		  * typeclass info #n for constraint on instance decl
%		  * typeclass info for superclass #1
%		    ...
%		  * typeclass info for superclass #n
%		  * type info #1 
%		  * ...
%		  * type info #n
%
% The base_typeclass_info is produced statically, and there is one for
% each instance declaration. For each constraint on the instance
% declaration, the corresponding typeclass_info is stored in the second
% part.
%
% eg. for the following program:
%
%	:- typeclass foo(T) where [...].
%	:- instance  foo(int) where [...].
%	:- instance  foo(list(T)) <= foo(T) where [...].
%
%	The typeclass_info for foo(int) is:
%		The base_typeclass_info:
%		  * 0 (arity of the instance declaration) 
%		  * pointer to method #1
%		    ...
%		  * pointer to method #n
%
%		The typeclass_info:
%		  * a pointer to the base typeclass info
%		  * type_info for int
%
%	The typeclass_info for foo(list(T)) is:
%		The base_typeclass_info:
%		  * 1 (arity of the instance declaration)
%		  * pointer to method #1
%		    ...
%		  * pointer to method #n
%
%		The typeclass_info contains:
%		  * a pointer to the base typeclass info
%		  * typeclass info for foo(T)
%		  * type_info for list(T)
%
% If the "T" for the list is known, the whole typeclass_info will be static
% data. When we do not know until runtime, the typeclass_info is constructed
% dynamically.
%
%-----------------------------------------------------------------------------%
%
% Example of transformation:
%
% Take the following code as an example (assuming the declarations above),
% ignoring the requirement for super-homogeneous form for clarity:
%
%	:- pred p(T1) <= foo(T1).
%	:- pred q(T2, T3) <= foo(T2), bar(T3).
%	:- pred r(T4, T5) <= foo(T4).
%
%	p(X) :- q([X], 0), r(X, 0).
%
% We add an extra argument for each type class constraint, and one
% argument for each unconstrained type variable.
%
%	:- pred p(typeclass_info(foo(T1)), T1).
%	:- pred q(typeclass_info(foo(T2)), typeclass_info(bar(T3)), T2, T3).
%	:- pred r(typeclass_info(foo(T4)), type_info(T5), T4, T5).
%
% We transform the body of p to this:
%
%	p(TypeClassInfoT1, X) :-
%		BaseTypeClassInfoT2 = base_typeclass_info(
%			1,
%			...
%			... (The methods for the foo class from the list
%			...  instance)
%			...
%			),
%		TypeClassInfoT2 = typeclass_info(
%			BaseTypeClassInfoT2,
%			TypeClassInfoT1,
%			<type_info for list(T1)>),
%		BaseTypeClassInfoT3 = base_typeclass_info(
%			0,
%			...
%			... (The methods for the bar class from the int
%			...  instance)
%			...
%			),
%		TypeClassInfoT3 = typeclass_info(
%			BaseTypeClassInfoT3,
%			<type_info for int>),
%		q(TypeClassInfoT2, TypeClassInfoT3, [X], 0),
%		BaseTypeClassInfoT4 = baseclass_type_info(
%			0,
%			...
%			... (The methods for the foo class from the int
%			...  instance)
%			...
%			),
%		TypeClassInfoT4 = typeclass_info(
%			BaseTypeClassInfoT4,
%			<type_info for int>),
%		r(TypeClassInfoT1, <type_info for int>, X, 0).
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
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module polymorphism.
:- interface.

:- import_module hlds_goal, hlds_module, hlds_pred, prog_data, special_pred.
:- import_module io, list, term.

:- pred polymorphism__process_module(module_info, module_info,
			io__state, io__state).
:- mode polymorphism__process_module(in, out, di, uo) is det.

% Given a list of types, create a list of variables to hold the type_info
% for those types, and create a list of goals to initialize those type_info
% variables to the appropriate type_info structures for the types.
% Update the varset and vartypes accordingly.

:- pred polymorphism__make_type_info_vars(list(type), existq_tvars,
	term__context, list(prog_var), list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__make_type_info_vars(in, in, in, out, out, in, out) is det.

:- type poly_info.

	% Extract some fields from a pred_info and proc_info for use
	% by the polymorphism transformation.
:- pred init_poly_info(module_info, pred_info, proc_info, poly_info).
:- mode init_poly_info(in, in, in, out) is det.

	% Update the fields in a pred_info and proc_info with
	% the values in a poly_info.
:- pred poly_info_extract(poly_info, pred_info, pred_info,
		proc_info, proc_info, module_info).
:- mode poly_info_extract(in, in, out, in, out, out) is det.

	% unsafe_type_cast and unsafe_promise_unique are polymorphic
	% builtins which do not need their type_infos. unsafe_type_cast
	% can be introduced by common.m after polymorphism is run, so it
	% is much simpler to avoid introducing type_info arguments for it.
	% Since both of these are really just assignment unifications, it
	% is desirable to generate them inline.
	% There are also some predicates in private_builtin.m to
	% manipulate typeclass_infos which don't need their type_infos.
:- pred polymorphism__no_type_info_builtin(module_name, string, int).
:- mode polymorphism__no_type_info_builtin(in, in, out) is semidet.

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
:- mode polymorphism__get_special_proc(in, in, in, out, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, llds, (lambda), prog_io.
:- import_module type_util, mode_util, quantification, instmap, prog_out.
:- import_module code_util, unify_proc, prog_util, make_hlds, inst_util.
:- import_module (inst), hlds_out, base_typeclass_info, goal_util, passes_aux.

:- import_module bool, int, string, set, map.
:- import_module term, varset, std_util, require, assoc_list.

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.
	% We do two passes, the first to fix up the procedure bodies,
	% (and in fact everything except the pred_info argtypes),
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
	polymorphism__expand_class_method_bodies(ModuleInfo2, ModuleInfo3),

	% Need update the dependency graph to include the lambda predicates. 
	module_info_clobber_dependency_info(ModuleInfo3, ModuleInfo).

:- pred polymorphism__process_preds(list(pred_id), module_info, module_info,
			io__state, io__state).
:- mode polymorphism__process_preds(in, in, out, di, uo) is det.

polymorphism__process_preds([], ModuleInfo, ModuleInfo) --> [].
polymorphism__process_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	polymorphism__process_pred(PredId, ModuleInfo0, ModuleInfo1),
	polymorphism__process_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred polymorphism__process_pred(pred_id, module_info, module_info,
			io__state, io__state).
:- mode polymorphism__process_pred(in, in, out, di, uo) is det.

polymorphism__process_pred(PredId, ModuleInfo0, ModuleInfo, IO0, IO) :-
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
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
			% Other Aditi procedures should still be processed
			% to remove complicated unifications and
			% lambda expressions.
			hlds_pred__pred_info_is_aditi_aggregate(PredInfo)
		;
			pred_info_module(PredInfo, PredModule),
			pred_info_name(PredInfo, PredName),
			pred_info_arity(PredInfo, PredArity),
			polymorphism__no_type_info_builtin(PredModule,
				PredName, PredArity) 
		)
	->
		ModuleInfo = ModuleInfo0,
		IO = IO0
	;
		pred_info_procids(PredInfo, ProcIds),
		polymorphism__process_procs(PredId, ProcIds,
			ModuleInfo0, ModuleInfo, IO0, IO)
	).

:- pred polymorphism__process_procs(pred_id, list(proc_id),
					module_info, module_info,
					io__state, io__state).
:- mode polymorphism__process_procs(in, in, in, out, di, uo) is det.

polymorphism__process_procs(_PredId, [], ModuleInfo, ModuleInfo, IO, IO).
polymorphism__process_procs(PredId, [ProcId | ProcIds], ModuleInfo0,
		ModuleInfo, IO0, IO) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

%	It is misleading to output this message for predicates which are
%	not defined in this module, and we get far too many of them anyway.
	% write_proc_progress_message("% Transforming polymorphism for ",
	% 			PredId, ProcId, ModuleInfo0, IO0, IO1),
	IO1 = IO0,

	polymorphism__process_proc(ProcId, ProcInfo0, PredInfo0, 
		ModuleInfo0, ProcInfo, PredInfo1, ModuleInfo1),

	pred_info_procedures(PredInfo1, ProcTable1),
	map__det_update(ProcTable1, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo1, ProcTable, PredInfo),
	module_info_preds(ModuleInfo1, PredTable1),
	map__det_update(PredTable1, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo2),

	polymorphism__process_procs(PredId, ProcIds, ModuleInfo2, ModuleInfo,
			IO1, IO).

polymorphism__no_type_info_builtin(MercuryBuiltin, "unsafe_type_cast", 2) :-
	mercury_private_builtin_module(MercuryBuiltin).
polymorphism__no_type_info_builtin(MercuryBuiltin,
		"unsafe_promise_unique", 2) :-
	mercury_public_builtin_module(MercuryBuiltin).
polymorphism__no_type_info_builtin(MercuryBuiltin,
		"superclass_from_typeclass_info", 3) :-
	mercury_private_builtin_module(MercuryBuiltin).
polymorphism__no_type_info_builtin(MercuryBuiltin,
		"instance_constraint_from_typeclass_info", 3) :-
	mercury_private_builtin_module(MercuryBuiltin).
polymorphism__no_type_info_builtin(MercuryBuiltin,
		"type_info_from_typeclass_info", 3) :-
	mercury_private_builtin_module(MercuryBuiltin).

%---------------------------------------------------------------------------%

:- pred polymorphism__fixup_preds(list(pred_id), module_info, module_info).
:- mode polymorphism__fixup_preds(in, in, out) is det.

polymorphism__fixup_preds([], ModuleInfo, ModuleInfo).
polymorphism__fixup_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	%
	% Recompute the arg types by finding the headvars and the var->type
	% mapping (from the first procedure for the predicate) and
	% applying the type mapping to the extra headvars to get the new
	% arg types.  Note that we are careful to only apply the mapping
	% to the extra head vars, not to the originals, because otherwise
	% we would stuff up the arg types for unification predicates for
	% equivalence types.
	%
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	pred_info_procids(PredInfo0, ProcIds),
	( ProcIds = [ProcId | _] ->
		map__lookup(ProcTable0, ProcId, ProcInfo),
		proc_info_vartypes(ProcInfo, VarTypes),
		proc_info_headvars(ProcInfo, HeadVars),
		pred_info_arg_types(PredInfo0, TypeVarSet, ExistQVars,
			ArgTypes0),
		list__length(ArgTypes0, NumOldArgs),
		list__length(HeadVars, NumNewArgs),
		NumExtraArgs is NumNewArgs - NumOldArgs,
		(
			list__split_list(NumExtraArgs, HeadVars, ExtraHeadVars,
					_OldHeadVars)
		->
			map__apply_to_list(ExtraHeadVars, VarTypes,
				ExtraArgTypes),
			list__append(ExtraArgTypes, ArgTypes0, ArgTypes)
		;
			error("polymorphism.m: list__split_list failed")
		),

		%
		% If there were any existentially quantified type variables,
		% either in this predicate or in any predicate that it calls,
		% then we may need to recompute the instmap deltas too.
		% (The instmap deltas only need to be recomputed if we
		% change which variables are bound by the subgoals, i.e.
		% if any of the new variables that we introduced have mode
		% `out' rather than mode `in'.  This can happen only if some
		% of the type variables are existentially quantified rather
		% than universally quantified.)
		%
		(
			ExistQVars = [],
			pred_info_get_head_type_params(PredInfo0,
				HeadTypeParams),
			term__vars_list(ArgTypes, HeadTypeParams)
		->
			PredInfo1 = PredInfo0,
			ModuleInfo1 = ModuleInfo0
		;
			polymorphism__fixup_procs(ProcIds, PredId, ProcTable0,
				ProcTable, ModuleInfo0, ModuleInfo1),
			pred_info_set_procedures(PredInfo0, ProcTable,
				PredInfo1)
		),
		pred_info_set_arg_types(PredInfo1, TypeVarSet, ExistQVars,
			ArgTypes, PredInfo),
		map__det_update(PredTable0, PredId, PredInfo, PredTable),
		module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo2)
	;
		ModuleInfo2 = ModuleInfo0
	),
	polymorphism__fixup_preds(PredIds, ModuleInfo2, ModuleInfo).

:- pred polymorphism__fixup_procs(list(proc_id), pred_id,
			proc_table, proc_table, module_info, module_info).
:- mode polymorphism__fixup_procs(in, in, in, out, in, out) is det.

polymorphism__fixup_procs([], _, ProcTable, ProcTable, ModuleInfo, ModuleInfo).
polymorphism__fixup_procs([ProcId | ProcIds], PredId, ProcTable0, ProcTable,
		ModuleInfo0, ModuleInfo) :-
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_arglives(ProcInfo0, ModuleInfo0, ArgLives),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap),
	proc_info_inst_table(ProcInfo0, InstTable0),
	proc_info_vartypes(ProcInfo0, VarTypes),
	(
		list__same_length(HeadVars, ArgLives)
	->
		proc_info_goal(ProcInfo0, Goal0)
	;
		error("polymorphism__fixup_procs")
	),
	recompute_instmap_delta(HeadVars, ArgLives, VarTypes, Goal0, Goal,
		InstMap, InstTable0, InstTable, _, ModuleInfo0, ModuleInfo1),
	proc_info_set_inst_table(ProcInfo0, InstTable, ProcInfo1),
	proc_info_set_goal(ProcInfo1, Goal, ProcInfo),
	% proc_info_set_maybe_arglives(ProcInfo2, yes(ArgLives), ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable1),
	polymorphism__fixup_procs(ProcIds, PredId, ProcTable1, ProcTable,
		ModuleInfo1, ModuleInfo).

%---------------------------------------------------------------------------%


:- pred polymorphism__process_proc(proc_id, proc_info, pred_info,
			module_info, proc_info, pred_info, module_info).
:- mode polymorphism__process_proc(in, in, in, in, out, out, out) is det.

polymorphism__process_proc(ProcId, ProcInfo0, PredInfo0, ModuleInfo0,
				ProcInfo, PredInfo, ModuleInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	init_poly_info(ModuleInfo0, PredInfo0, ProcInfo0, Info0),
	polymorphism__setup_headvars(PredInfo0, ProcInfo0,
			HeadVars, ArgModes, UnconstrainedTVars,
			ExtraTypeInfoHeadVars, ExistTypeClassInfoHeadVars,
			Info0, Info1),

	(
		( pred_info_is_imported(PredInfo0)
		; pred_info_is_pseudo_imported(PredInfo0),
		  hlds_pred__in_in_unification_proc_id(ProcId)
		)
	->
		Goal = Goal0,
		Info = Info1
	;
		%
		% process any polymorphic calls inside the goal
		%
		polymorphism__process_goal(Goal0, Goal1, Info1, Info2),

		%
		% generate code to construct the type-class-infos
		% and type-infos for existentially quantified type vars
		%
		polymorphism__produce_existq_tvars(
			PredInfo0, ProcInfo0,
			UnconstrainedTVars, ExtraTypeInfoHeadVars,
			ExistTypeClassInfoHeadVars,
			Goal1, Goal2, Info2, Info3),

		pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
		polymorphism__fixup_quantification(HeadVars, ExistQVars,
			Goal2, Goal, Info3, Info)
	),

	%
	% set the new values of the fields in proc_info and pred_info
	%
	proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo1),
	proc_info_set_goal(ProcInfo1, Goal, ProcInfo2),

		% Clear the arglives, because the arity of the
		% proc may have changed.  These will be put back
		% in polymorphism__fixup_procs.
	proc_info_set_maybe_arglives(ProcInfo2, no, ProcInfo3),
	proc_info_set_argmodes(ProcInfo3, ArgModes, ProcInfo4),
	poly_info_extract(Info, PredInfo0, PredInfo,
		ProcInfo4, ProcInfo, ModuleInfo).

% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

:- pred polymorphism__setup_headvars(pred_info, proc_info,
		list(prog_var), argument_modes, list(tvar), list(prog_var),
		list(prog_var), poly_info, poly_info).
:- mode polymorphism__setup_headvars(in, in, out, out, out, out, out,
		in, out) is det.

polymorphism__setup_headvars(PredInfo, ProcInfo, HeadVars, ArgModes,
		UnconstrainedTVars, ExtraHeadTypeInfoVars,
		ExistHeadTypeClassInfoVars, PolyInfo0, PolyInfo) :-
	%
	% grab the appropriate fields from the pred_info and proc_info
	%
	pred_info_arg_types(PredInfo, ArgTypeVarSet, ExistQVars, ArgTypes),
	pred_info_get_class_context(PredInfo, ClassContext),
	proc_info_headvars(ProcInfo, HeadVars0),
	proc_info_argmodes(ProcInfo, ArgModes0),


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
	list__remove_dups(UnconstrainedTVars1, UnconstrainedTVars), 

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
	polymorphism__make_head_vars(UnconstrainedUnivTVars, ArgTypeVarSet,
		UnivHeadTypeInfoVars, PolyInfo3, PolyInfo4),
	list__append(UnivHeadTypeInfoVars, ExistHeadTypeInfoVars,
		ExtraHeadTypeInfoVars),

		% First the type_infos, then the typeclass_infos, 
		% but we have to do it in reverse because we're appending...
	list__append(ExtraHeadTypeClassInfoVars, HeadVars0, HeadVars1),
	list__append(ExtraHeadTypeInfoVars, HeadVars1, HeadVars),

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
	ArgModes0 = argument_modes(ArgIT, Modes0),
	list__condense([UnivTypeClassInfoModes, ExistTypeClassInfoModes,
		UnivTypeInfoModes, ExistTypeInfoModes, Modes0], Modes),
	ArgModes = argument_modes(ArgIT, Modes),
		
	%
	% Add the locations of the typeinfos
	% for unconstrained, universally quantified type variables.
	% to the initial tvar->type_info_var mapping
	%
	ToLocn = lambda([TheVar::in, TheLocn::out] is det,
			TheLocn = type_info(TheVar)),

	list__map(ToLocn, UnivHeadTypeInfoVars, UnivTypeLocns),
	map__det_insert_from_corresponding_lists(TypeInfoMap3,
		UnconstrainedUnivTVars, UnivTypeLocns, TypeInfoMap4),

	list__map(ToLocn, ExistHeadTypeInfoVars, ExistTypeLocns),
	map__det_insert_from_corresponding_lists(TypeInfoMap4,
		UnconstrainedExistTVars, ExistTypeLocns, TypeInfoMap5),

	poly_info_set_type_info_map(TypeInfoMap5, PolyInfo4, PolyInfo5),

	% Make a map of the locations of the typeclass_infos
	map__from_corresponding_lists(UnivConstraints,
			UnivHeadTypeClassInfoVars, TypeClassInfoMap),
	poly_info_set_typeclass_info_map(TypeClassInfoMap, PolyInfo5, PolyInfo).


% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

%
% generate code to produce the values of type_infos and typeclass_infos
% for existentially quantified type variables in the head
%
:- pred polymorphism__produce_existq_tvars(
		pred_info, proc_info, list(tvar), list(prog_var), list(prog_var),
		hlds_goal, hlds_goal, poly_info, poly_info).
:- mode polymorphism__produce_existq_tvars(in, in, in, in, in, in, out,
			in, out) is det.

polymorphism__produce_existq_tvars(PredInfo, ProcInfo,
		UnconstrainedTVars, TypeInfoHeadVars,
		ExistTypeClassInfoHeadVars, Goal0, Goal, Info0, Info) :-
	poly_info_get_var_types(Info0, VarTypes0),
	pred_info_arg_types(PredInfo, _ArgTypeVarSet, ExistQVars, ArgTypes),
	pred_info_get_class_context(PredInfo, ClassContext),
	proc_info_headvars(ProcInfo, HeadVars0),

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
	% figure out the list of universally quantified type variables
	%
	term__vars_list(ArgTypes, HeadTypeVars0),
	list__remove_dups(HeadTypeVars0, HeadTypeVars),
	list__delete_elems(HeadTypeVars, ExistQVars, UnivQTVars),

	%
	% apply the type bindings to the unconstrained type variables
	% to give the actual types, and then generate code
	% to initialize the type_infos for those types
	%
	term__var_list_to_term_list(UnconstrainedTVars,
		UnconstrainedTVarTerms),
	term__apply_substitution_to_list(UnconstrainedTVarTerms,
		TypeSubst, ActualTypes),
	polymorphism__make_type_info_vars(ActualTypes, UnivQTVars, Context,
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

	% Doing just this wouldn't work, because we also need to fill in
	% the mode and determinism info:
	%	term__context_init(Context),
	%	create_atomic_unification(Var1, var(Var2), Context, explicit,
	% 		[], Goal),

	Ground = ground(shared, no),
	Mode = ((free(unique) - Ground) - (Ground - Ground)),
	UnifyInfo = assign(Var1, Var2),
	UnifyC = unify_context(explicit, []),
	set__list_to_set([Var1, Var2], NonLocals),
	instmap_delta_from_assoc_list([Var1 - Ground], InstMapDelta),
	Determinism = det,
	goal_info_init(NonLocals, InstMapDelta, Determinism, GoalInfo),
	Goal = unify(Var1, var(Var2), Mode, UnifyInfo, UnifyC) - GoalInfo.

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
polymorphism__process_goal_expr(higher_order_call(A, B, C, D, E, F),
		GoalInfo, higher_order_call(A, B, C, D, E, F) - GoalInfo)
		--> [].

	% The same goes for class method calls
polymorphism__process_goal_expr(class_method_call(A, B, C, D, E, F),
		GoalInfo, class_method_call(A, B, C, D, E, F) - GoalInfo)
		--> [].

polymorphism__process_goal_expr(call(PredId0, ProcId0, ArgVars0,
		Builtin, UnifyContext, Name0), GoalInfo, Goal) -->
	% Check for a call to a special predicate like compare/3
	% for which the type is known at compile-time.
	% Replace such calls with calls to the particular version
	% for that type.
	(
		{ Name0 = unqualified(PredName0) },
		{ list__length(ArgVars0, Arity) },
		{ special_pred_name_arity(SpecialPredId, PredName0,
						MangledPredName, Arity) },
		=(Info0),
		{ poly_info_get_var_types(Info0, VarTypes) },
		{ special_pred_get_type(MangledPredName, ArgVars0, MainVar) },
		{ map__lookup(VarTypes, MainVar, Type) },
		{ Type \= term__variable(_) },

		% don't try this for any special preds if they're not
		% implemented

		{ special_pred_list(SpecialPredIds) },
		{ list__member(SpecialPredId, SpecialPredIds) }
	->
		{ poly_info_get_module_info(Info0, ModuleInfo) },
		{ polymorphism__get_special_proc(Type, SpecialPredId,
			ModuleInfo, Name, PredId, ProcId) }
	;
		{ PredId = PredId0 },
		{ ProcId = ProcId0 },
		{ Name = Name0 }
	),

	polymorphism__process_call(PredId, ArgVars0, GoalInfo,
		ArgVars, _ExtraVars, CallGoalInfo, ExtraGoals),

	{ Call = call(PredId, ProcId, ArgVars, Builtin, UnifyContext, Name)
		- CallGoalInfo },
	{ list__append(ExtraGoals, [Call], GoalList) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) }.

polymorphism__process_goal_expr(unify(XVar, Y, Mode, Unification, Context),
				GoalInfo, Goal) -->
	(
		{ Unification = complicated_unify(UniMode, CanFail) },
		{ Y = var(YVar) }
	->
		=(Info0),
		{ poly_info_get_var_types(Info0, VarTypes) },
		{ poly_info_get_type_info_map(Info0, TypeInfoMap) },
		{ poly_info_get_module_info(Info0, ModuleInfo) },
		{ poly_info_get_inst_table(Info0, InstTable) },
		{ map__lookup(VarTypes, XVar, Type) },
		( { Type = term__variable(TypeVar) } ->
			% Convert polymorphic unifications into calls to
			% `unify/2', the general unification predicate, passing
			% the appropriate Type_info
			% 	=(TypeInfoVar, X, Y)
			% where TypeInfoVar is the type_info variable
			% associated with the type of the variables that
			% are being unified.

			{ module_info_get_predicate_table(ModuleInfo,
				PredicateTable) },
			{ mercury_public_builtin_module(MercuryBuiltin) },
			{ predicate_table_search_pred_m_n_a(PredicateTable,
				MercuryBuiltin, "unify", 2, [CallPredId])
			->
				PredId = CallPredId
			;
				error("polymorphism.m: can't find `builtin:unify/2'")
			},
%		YYY we don't have an instmap here.  
%		Polymorphic unification should be caught earlier 
%		(e.g. in mode analysis).
%		`tests/invalid/polymorphic_unification' will fail until this
%		error is caught properly.
%			{ Mode = XMode - YMode },
%			{
%				mode_is_fully_input(ModuleInfo, XMode),
%				mode_is_fully_input(ModuleInfo, YMode)
%			->
%				true
%			;
%				goal_info_get_context(GoalInfo, GoalContext),
%				context_to_string(GoalContext, ContextMsg),
%				string__append(ContextMsg,
%"Sorry, not implemented: polymorphic unification in mode other than (in, in)",
%						ErrorMsg),
%				error(ErrorMsg)
%			},
			{ hlds_pred__in_in_unification_proc_id(ProcId) },
			{ map__lookup(TypeInfoMap, TypeVar, TypeInfoLocn) },
			{ SymName = unqualified("unify") },
			{ code_util__builtin_state(ModuleInfo, PredId, ProcId,
				BuiltinState) },
			{ CallContext = call_unify_context(XVar, Y, Context) },
			(
					% If the typeinfo is available in a
					% variable, just use it
				{ TypeInfoLocn = type_info(TypeInfoVar) },
				{ ArgVars = [TypeInfoVar, XVar, YVar] },
				{ Goal = call(PredId, ProcId, ArgVars,
					BuiltinState, yes(CallContext), SymName)
					- GoalInfo }
			;
					% If the typeinfo is in a
					% typeclass_info, first extract it, 
					% then use it
				{ TypeInfoLocn =
					typeclass_info(TypeClassInfoVar,
					Index) },
				extract_type_info(Type, TypeVar,
					TypeClassInfoVar, Index,
					Goals, TypeInfoVar),

				{ ArgVars = [TypeInfoVar, XVar, YVar] },
				{ Call = call(PredId, ProcId, ArgVars,
					BuiltinState, yes(CallContext), SymName)
					- GoalInfo },

				{ list__append(Goals, [Call], TheGoals) },
				{ Goal = conj(TheGoals) - GoalInfo }
			)

		; { type_is_higher_order(Type, _, _) } ->
			{ SymName = unqualified("builtin_unify_pred") },
			{ ArgVars = [XVar, YVar] },
			{ module_info_get_predicate_table(ModuleInfo,
				PredicateTable) },
			{
				mercury_private_builtin_module(PrivateBuiltin),
				predicate_table_search_pred_m_n_a(
				    PredicateTable,
				    PrivateBuiltin, "builtin_unify_pred", 2,
				    [PredId0])
			->
				PredId = PredId0
			;
				error("can't locate private_builtin:builtin_unify_pred/2")
			},
			{ hlds_pred__in_in_unification_proc_id(ProcId) },
			{ CallContext = call_unify_context(XVar, Y, Context) },
			{ Call = call(PredId, ProcId, ArgVars, not_builtin,
				yes(CallContext), SymName) },
			polymorphism__process_goal_expr(Call, GoalInfo, Goal)
			
		; { type_to_type_id(Type, TypeId, _) } ->

			% Convert other complicated unifications into
			% calls to specific unification predicates, and then
			% recursively call polymorphism__process_goal_expr
			% to insert extra arguments if necessary.

			{ module_info_get_special_pred_map(ModuleInfo,
				SpecialPredMap) },
			{ map__lookup(SpecialPredMap, unify - TypeId, PredId) },
			{ determinism_components(Det, CanFail, at_most_one) },
			{ unify_proc__lookup_mode_num(InstTable, ModuleInfo,
				TypeId, UniMode, Det, ProcId) },
			{ SymName = unqualified("__Unify__") },
			{ ArgVars = [XVar, YVar] },
			{ CallContext = call_unify_context(XVar, Y, Context) },
			{ Call = call(PredId, ProcId, ArgVars, not_builtin,
				yes(CallContext), SymName) },
			polymorphism__process_goal_expr(Call, GoalInfo, Goal)
		;
			{ error("polymorphism: type_to_type_id failed") }
		)
	; 
		{ Y = lambda_goal(PredOrFunc, ArgVars, LambdaVars,
			Modes, Det, _IMDelta, LambdaGoal0) }
	->
		% for lambda expressions, we must recursively traverse the
		% lambda goal and then convert the lambda expression
		% into a new predicate
		polymorphism__process_goal(LambdaGoal0, LambdaGoal1),
		% XXX currently we don't allow lambda goals to be
		% existentially typed
		{ ExistQVars = [] },
		polymorphism__fixup_lambda_quantification(LambdaGoal1,
				ArgVars, LambdaVars, ExistQVars,
				LambdaGoal, NonLocalTypeInfos),
		polymorphism__process_lambda(PredOrFunc, LambdaVars, Modes,
				Det, ArgVars, NonLocalTypeInfos, LambdaGoal,
				Unification, Y1, Unification1),
		{ Goal = unify(XVar, Y1, Mode, Unification1, Context)
				- GoalInfo }
	;
		% ordinary unifications are left unchanged,
		{ Goal = unify(XVar, Y, Mode, Unification, Context) - GoalInfo }
	).

	% the rest of the clauses just process goals recursively

polymorphism__process_goal_expr(conj(Goals0), GoalInfo,
		conj(Goals) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_expr(par_conj(Goals0, SM), GoalInfo,
		par_conj(Goals, SM) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_expr(disj(Goals0, SM), GoalInfo,
		disj(Goals, SM) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_expr(not(Goal0), GoalInfo, not(Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_expr(switch(Var, CanFail, Cases0, SM), GoalInfo,
				switch(Var, CanFail, Cases, SM) - GoalInfo) -->
	polymorphism__process_case_list(Cases0, Cases).
polymorphism__process_goal_expr(some(Vars, Goal0), GoalInfo,
			some(Vars, Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_expr(if_then_else(Vars, A0, B0, C0, SM), GoalInfo,
			if_then_else(Vars, A, B, C, SM) - GoalInfo) -->
	polymorphism__process_goal(A0, A),
	polymorphism__process_goal(B0, B),
	polymorphism__process_goal(C0, C).

polymorphism__process_goal_expr(Goal0, GoalInfo, Goal) -->
	{ Goal0 = pragma_c_code(IsRecursive, PredId, ProcId,
		ArgVars0, ArgInfo0, OrigArgTypes0, PragmaCode) },
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


	(
		{ polymorphism__no_type_info_builtin(PredModule,
			PredName, PredArity)  }
	->
		{ Goal = Goal0 - GoalInfo }
	;
		{ list__length(ExtraVars, NumExtraVars) },
		{ polymorphism__process_c_code(PredInfo, NumExtraVars,
			OrigArgTypes0, OrigArgTypes, ArgInfo0, ArgInfo) },

		%
		% plug it all back together
		%
		{ Call = pragma_c_code(IsRecursive, PredId, ProcId, ArgVars,
			ArgInfo, OrigArgTypes, PragmaCode) - CallGoalInfo },
		{ list__append(ExtraGoals, [Call], GoalList) },
		{ conj_list_to_goal(GoalList, GoalInfo, Goal) }
	).


:- pred polymorphism__process_c_code(pred_info, int, list(type), list(type),
	pragma_c_code_arg_info, pragma_c_code_arg_info).
:- mode polymorphism__process_c_code(in, in, in, out, in, out) is det.

polymorphism__process_c_code(PredInfo, NumExtraVars, OrigArgTypes0,
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

	ArgInfo0 = pragma_c_code_arg_info(ArgInstTable, ArgModes0),
	polymorphism__c_code_add_typeinfos(
			PredTypeVars, PredTypeVarSet, ExistQVars, 
			ArgModes0, ArgModes1),
	polymorphism__c_code_add_typeclass_infos(
			UnivCs, ExistCs, PredTypeVarSet, ArgModes1, ArgModes),
	ArgInfo = pragma_c_code_arg_info(ArgInstTable, ArgModes),

	%
	% insert type_info/typeclass_info types for all the inserted 
	% type_info/typeclass_info vars into the arg-types list
	%
	mercury_private_builtin_module(PrivateBuiltin),
	MakeType = lambda([TypeVar::in, TypeInfoType::out] is det,
		construct_type(qualified(PrivateBuiltin, "type_info") - 1,
			[term__variable(TypeVar)], TypeInfoType)),
	list__map(MakeType, PredTypeVars, TypeInfoTypes),
	MakeTypeClass = lambda([_::in, TypeClassInfoType::out] is det,
		construct_type(qualified(PrivateBuiltin, "typeclass_info") - 0,
			[], TypeClassInfoType)),
	list__map(MakeTypeClass, UnivCs, UnivTypes),
	list__map(MakeTypeClass, ExistCs, ExistTypes),
	list__append(TypeInfoTypes, OrigArgTypes0, OrigArgTypes1),
	list__append(ExistTypes, OrigArgTypes1, OrigArgTypes2),
	list__append(UnivTypes, OrigArgTypes2, OrigArgTypes).

:- pred polymorphism__c_code_add_typeclass_infos(
		list(class_constraint), list(class_constraint), 
		tvarset, list(maybe(pair(string, mode))),
		list(maybe(pair(string, mode)))). 
:- mode polymorphism__c_code_add_typeclass_infos(in, in, in, in, out) is det.

polymorphism__c_code_add_typeclass_infos(UnivCs, ExistCs, 
		PredTypeVarSet, ArgInfo0, ArgInfo) :-
	in_mode(In),
	out_mode(Out),
	polymorphism__c_code_add_typeclass_infos_2(ExistCs, Out, 
		PredTypeVarSet, ArgInfo0, ArgInfo1),
	polymorphism__c_code_add_typeclass_infos_2(UnivCs, In, 
		PredTypeVarSet, ArgInfo1, ArgInfo).

:- pred polymorphism__c_code_add_typeclass_infos_2(
		list(class_constraint), mode,
		tvarset, list(maybe(pair(string, mode))),
		list(maybe(pair(string, mode)))). 
:- mode polymorphism__c_code_add_typeclass_infos_2(in, in, in, in, out) is det.  
polymorphism__c_code_add_typeclass_infos_2([], _, _, ArgNames, ArgNames).
polymorphism__c_code_add_typeclass_infos_2([C|Cs], Mode, TypeVarSet, 
		ArgNames0, ArgNames) :-
	polymorphism__c_code_add_typeclass_infos_2(Cs, Mode, TypeVarSet, 
		ArgNames0, ArgNames1),
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
	ArgNames = [yes(C_VarName - Mode) | ArgNames1].

:- pred polymorphism__c_code_add_typeinfos(list(tvar),
		tvarset, existq_tvars, list(maybe(pair(string, mode))),
		list(maybe(pair(string, mode)))). 
:- mode polymorphism__c_code_add_typeinfos(in, in, in, in, out) is det.

polymorphism__c_code_add_typeinfos(TVars, TypeVarSet,
		ExistQVars, ArgNames0, ArgNames) :-
	list__filter(lambda([X::in] is semidet, (list__member(X, ExistQVars))),
		TVars, ExistUnconstrainedVars, UnivUnconstrainedVars),
	in_mode(In),
	out_mode(Out),
	polymorphism__c_code_add_typeinfos_2(ExistUnconstrainedVars, TypeVarSet,
		Out, ArgNames0, ArgNames1),
	polymorphism__c_code_add_typeinfos_2(UnivUnconstrainedVars, TypeVarSet,
		In, ArgNames1, ArgNames).

:- pred polymorphism__c_code_add_typeinfos_2(list(tvar),
		tvarset, mode, list(maybe(pair(string, mode))),
		list(maybe(pair(string, mode)))). 
:- mode polymorphism__c_code_add_typeinfos_2(in, in, in, in, out) is det.

polymorphism__c_code_add_typeinfos_2([], _, _, ArgNames, ArgNames).
polymorphism__c_code_add_typeinfos_2([TVar|TVars], TypeVarSet, Mode,
		ArgNames0, ArgNames) :-
	polymorphism__c_code_add_typeinfos_2(TVars, TypeVarSet,
		Mode, ArgNames0, ArgNames1),
	( varset__search_name(TypeVarSet, TVar, TypeVarName) ->
		string__append("TypeInfo_for_", TypeVarName, C_VarName),
		ArgNames = [yes(C_VarName - Mode) | ArgNames1]
	;
		ArgNames = [no | ArgNames1]
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
	{ Case0 = case(ConsId, IMDelta, Goal0) },
	polymorphism__process_goal(Goal0, Goal),
	{ Case = case(ConsId, IMDelta, Goal) },
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
			polymorphism__no_type_info_builtin(PredModule,
				PredName, PredArity)
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
		polymorphism__make_typeclass_info_head_vars(
			ExistentialConstraints, ExistTypeClassVars,
			Info2, Info3),
		polymorphism__update_typeclass_infos(
			ExistentialConstraints, ExistTypeClassVars,
			Info3, Info4),

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
		term__apply_rec_substitution_to_list(PredExistQVarTerms1,
			TypeSubst, PredExistQVarTerms),
		term__term_list_to_var_list(PredExistQVarTerms,
			PredExistQVars),

		polymorphism__make_type_info_vars(PredTypes, PredExistQVars,
			Context, ExtraTypeInfoVars, ExtraTypeInfoGoals,
			Info4, Info),
		list__append(ExtraTypeClassVars, ArgVars0, ArgVars1),
		list__append(ExtraTypeInfoVars, ArgVars1, ArgVars),
		list__append(ExtraTypeClassGoals, ExtraTypeInfoGoals,
			ExtraGoals),
		list__append(ExtraTypeClassVars, ExtraTypeInfoVars,
			ExtraVars),

		%
		% update the non-locals
		%
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		set__insert_list(NonLocals0, ExtraVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),

		%
		% update the instmap delta for typeinfo vars and
		% typeclassinfo vars for any existentially quantified
		% type vars in the callee's type: such typeinfo variables
		% are produced by this call
		% (universally quantified typeinfo and typeclassinfo vars
		% are input to the goal, and their inst is not changed by
		% the goal, so they don't need to be mentioned in the
		% instmap delta)
		%
% YYY Andrew, why is this commented out? - dmo
%		poly_info_get_type_info_map(Info, TypeVarMap),
%		poly_info_get_typeclass_info_map(Info, TypeClassVarMap),
%		goal_info_get_instmap_delta(GoalInfo1, InstmapDelta0),
%		AddInstDelta = lambda([TVar::in, IMD0::in, IMD::out] is det, (
%			map__lookup(TypeVarMap, TVar, TypeInfoLocn),
%			(
%				TypeInfoLocn = type_info(TypeInfoVar),
%				instmap_delta_set(IMD0, TypeInfoVar,
%					ground(shared, no), IMD)
%			;
%				TypeInfoLocn = typeclass_info(_, _),
%				% the instmap delta for the type class info
%				% variable will be added by AddTCInstDelta
%				% (below)
%				IMD = IMD0
%			))),
%		AddTCInstDelta = lambda([Constraint::in, IMD0::in, IMD::out]
%					is det, (
%			map__lookup(TypeClassVarMap, Constraint,
%				TypeClassInfoVar),
%			instmap_delta_set(IMD0, TypeClassInfoVar,
%				ground(shared, no), IMD)
%			)),
%		list__foldl(AddInstDelta, PredExistQVars,
%			InstmapDelta0, InstmapDelta1),
%		list__foldl(AddTCInstDelta, ExistentialConstraints,
%			InstmapDelta1, InstmapDelta),
%		goal_info_set_instmap_delta(GoalInfo1, InstmapDelta, GoalInfo)
		GoalInfo = GoalInfo1
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

% constraint_list_get_tvars(Constraints, TVars):
%	return the list of type variables contained in a list of constraints

:- pred constraint_list_get_tvars(list(class_constraint), list(tvar)).
:- mode constraint_list_get_tvars(in, out) is det.
constraint_list_get_tvars(Constraints, TVars) :-
	list__map(constraint_get_tvars, Constraints, TVarsList),
	list__condense(TVarsList, TVars).

:- pred constraint_get_tvars(class_constraint, list(tvar)).
:- mode constraint_get_tvars(in, out) is det.
constraint_get_tvars(constraint(_Name, Args), TVars) :-
	term__vars_list(Args, TVars).

%-----------------------------------------------------------------------------%

:- pred polymorphism__fixup_quantification(list(prog_var), existq_tvars,
			hlds_goal, hlds_goal, poly_info, poly_info).
:- mode polymorphism__fixup_quantification(in, in, in, out, in, out) is det.

%
% If the lambda predicate we are processing is a polymorphic predicate,
% or contains polymorphically-typed goals, we
% may need to fix up the quantification (non-local variables)
% so that it includes the extra type-info variables and type-class-info
% variables that we added to the headvars in the non-locals set.
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

:- pred polymorphism__process_lambda(pred_or_func, list(prog_var),
		argument_modes, determinism, list(prog_var), set(prog_var),
		hlds_goal, unification, unify_rhs, unification,
		poly_info, poly_info).
:- mode polymorphism__process_lambda(in, in, in, in, in, in, in, in, out, out,
		in, out) is det.

polymorphism__process_lambda(PredOrFunc, Vars, Modes, Det, OrigNonLocals,
		NonLocalTypeInfos, LambdaGoal, Unification0, Functor,
		Unification, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(VarSet, VarTypes, TVarSet, TVarMap, 
			TCVarMap, _Proofs, PredName, ModuleInfo0, InstTable0,
			Markers, Owner),

		% Calculate the constraints which apply to this lambda
		% expression. 
		% XXX Note currently we only allow lambda expressions
		% to have universally quantified constraints.
	map__keys(TCVarMap, AllConstraints),
	map__apply_to_list(Vars, VarTypes, LambdaVarTypes),
	list__map(type_util__vars, LambdaVarTypes, LambdaTypeVarsList),
	list__condense(LambdaTypeVarsList, LambdaTypeVars),
	list__filter(polymorphism__constraint_contains_vars(LambdaTypeVars), 
		AllConstraints, UnivConstraints),

	Modes = argument_modes(ArgInstTable, ArgModes0),
	inst_table_create_sub(InstTable0, ArgInstTable, Sub, InstTable),
	list__map(apply_inst_key_sub_mode(Sub), ArgModes0, ArgModes),

	Constraints = constraints(UnivConstraints, []),
	lambda__transform_lambda(PredOrFunc, PredName, Vars, ArgModes, Det,
		OrigNonLocals, NonLocalTypeInfos, LambdaGoal, Unification0,
		VarSet, VarTypes, Constraints, TVarSet, TVarMap, TCVarMap,
		Markers, Owner, InstTable, ModuleInfo0, Functor,
		Unification, ModuleInfo),
	poly_info_set_module_info(ModuleInfo, PolyInfo0, PolyInfo).

:- pred polymorphism__constraint_contains_vars(list(tvar), class_constraint).
:- mode polymorphism__constraint_contains_vars(in, in) is semidet.

polymorphism__constraint_contains_vars(LambdaVars, ClassConstraint) :-
	ClassConstraint = constraint(_, ConstraintTypes),
	list__map(type_util__vars, ConstraintTypes, ConstraintVarsList),
	list__condense(ConstraintVarsList, ConstraintVars),
		% Probably not the most efficient way of doing it, but I
		% wouldn't think that it matters.
	set__list_to_set(LambdaVars, LambdaVarsSet),
	set__list_to_set(ConstraintVars, ConstraintVarsSet),
	set__subset(ConstraintVarsSet, LambdaVarsSet).

%---------------------------------------------------------------------------%

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

		% do the work
	polymorphism__make_typeclass_info_vars_2(PredClassContext, 
		ExistQVars, Context,
		ExtraVars0, ExtraVars1, 
		ExtraGoals0, ExtraGoals1,
		Info0, Info),
	
		% We build up the vars and goals in reverse order
	list__reverse(ExtraVars1, ExtraVars),
	list__reverse(ExtraGoals1, ExtraGoals).

% Accumulator version of the above.
:- pred polymorphism__make_typeclass_info_vars_2(
	list(class_constraint),
	existq_tvars, prog_context,
	list(prog_var), list(prog_var), 
	list(hlds_goal), list(hlds_goal), 
	poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_vars_2(in, in, in,
	in, out, in, out, in, out) is det.

polymorphism__make_typeclass_info_vars_2([], _ExistQVars,
		_Context, ExtraVars, ExtraVars, 
		ExtraGoals, ExtraGoals, 
		Info, Info).
polymorphism__make_typeclass_info_vars_2([C|Cs], ExistQVars,
		Context, ExtraVars0, ExtraVars,
		ExtraGoals0, ExtraGoals, 
		Info0, Info) :-
	polymorphism__make_typeclass_info_var(C, ExistQVars,
			Context, ExtraGoals0, ExtraGoals1, 
			Info0, Info1, MaybeExtraVar),
	maybe_insert_var(MaybeExtraVar, ExtraVars0, ExtraVars1),
	polymorphism__make_typeclass_info_vars_2(Cs,
			ExistQVars, Context, 
			ExtraVars1, ExtraVars,
			ExtraGoals1, ExtraGoals, 
			Info1, Info).

:- pred polymorphism__make_typeclass_info_var(class_constraint,
	existq_tvars, prog_context,
	list(hlds_goal), list(hlds_goal),
	poly_info, poly_info, maybe(prog_var)). 
:- mode polymorphism__make_typeclass_info_var(in, in, in, in, out,
	in, out, out) is det.

polymorphism__make_typeclass_info_var(Constraint, ExistQVars,
		Context, ExtraGoals0, ExtraGoals, 
		Info0, Info, MaybeVar) :-
	Constraint = constraint(ClassName, ConstrainedTypes),
	list__length(ConstrainedTypes, ClassArity),
	ClassId = class_id(ClassName, ClassArity),

	Info0 = poly_info(VarSet0, VarTypes0, TypeVarSet, TypeInfoMap0, 
		TypeClassInfoMap0, Proofs, PredName, ModuleInfo, InstTable,
		Markers, Owner),

	(
		map__search(TypeClassInfoMap0, Constraint, Location)
	->
			% We already have a typeclass_info for this constraint
		ExtraGoals = ExtraGoals0,
		Var = Location,
		MaybeVar = yes(Var),
		Info = Info0
	;
			% We don't have the typeclass_info as a parameter to
			% the pred, so we must be able to create it from
			% somewhere else

			% Work out how to make it
		map__lookup(Proofs, Constraint, Proof),
		(
				% We have to construct the typeclass_info
				% using an instance declaration
			Proof = apply_instance(InstanceNum),

			module_info_instances(ModuleInfo, InstanceTable),
			map__lookup(InstanceTable, ClassId, InstanceList),
			list__index1_det(InstanceList, InstanceNum,
				ProofInstanceDefn),

			ProofInstanceDefn = hlds_instance_defn(_, _,
				InstanceConstraints0, InstanceTypes0, _, _, 
				InstanceTVarset, SuperClassProofs0),

				% We can ignore the typevarset because all the
				% type variables that are created are bound.
				% When we call type_list_subsumes then apply
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
				InstanceConstraints1, InstanceConstraints),
			apply_subst_to_constraint_proofs(RenameSubst,
				SuperClassProofs0, SuperClassProofs1),
			apply_rec_subst_to_constraint_proofs(InstanceSubst,
				SuperClassProofs1, SuperClassProofs),

				% Make the type_infos for the types
				% that are constrained by this. These
				% are packaged in the typeclass_info
			polymorphism__make_type_info_vars(
				ConstrainedTypes, ExistQVars, Context, 
				InstanceExtraTypeInfoVars, TypeInfoGoals,
				Info0, Info1),

				% Make the typeclass_infos for the
				% constraints from the context of the
				% instance decl.
			polymorphism__make_typeclass_info_vars_2(
				InstanceConstraints,
				ExistQVars, Context,
				[], InstanceExtraTypeClassInfoVars0, 
				ExtraGoals0, ExtraGoals1, 
				Info1, Info2),
			
				% The variables are built up in 
				% reverse order.
			list__reverse(InstanceExtraTypeClassInfoVars0,
				InstanceExtraTypeClassInfoVars),

			polymorphism__construct_typeclass_info(
				InstanceExtraTypeInfoVars, 
				InstanceExtraTypeClassInfoVars, 
				ClassId, Constraint, InstanceNum,
				ConstrainedTypes,
				SuperClassProofs, ExistQVars, Var, NewGoals, 
				Info2, Info),

			MaybeVar = yes(Var),

				% Oh, yuck. The type_info goals have
				% already been reversed, so lets
				% reverse them back.
			list__reverse(TypeInfoGoals, RevTypeInfoGoals),

			list__append(ExtraGoals1, RevTypeInfoGoals,
				ExtraGoals2),
			list__append(NewGoals, ExtraGoals2, ExtraGoals)
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
				PredName, ModuleInfo, InstTable,
				Markers, Owner),

				% Make the typeclass_info for the subclass
			polymorphism__make_typeclass_info_var(
				SubClassConstraint,
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
			SubClassDefn = hlds_class_defn(SuperClasses0,
				SubClassVars, _, _, _),

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
			instmap_delta_from_assoc_list(
				[Var - ground(shared, no)],
				InstmapDelta),
			goal_info_init(NonLocals, InstmapDelta, det, GoalInfo),

				% Put them together
			SuperClassGoal = Call - GoalInfo,

				% Add it to the accumulator
			ExtraGoals = [SuperClassGoal,IndexGoal|ExtraGoals1]
		)
	).

:- pred polymorphism__construct_typeclass_info(list(prog_var), list(prog_var),
	class_id, class_constraint, int, 
	list(type), map(class_constraint, constraint_proof),
	existq_tvars, prog_var, list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__construct_typeclass_info(in, in, in, in, in, in, in, in,
	out, out, in, out) is det.

polymorphism__construct_typeclass_info(ArgTypeInfoVars, ArgTypeClassInfoVars,
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
	list__append(ArgVars0, ArgTypeInfoVars, ArgVars),

	ClassId = class_id(ClassName, _Arity),

	unqualify_name(ClassName, ClassNameString),
	polymorphism__new_typeclass_info_var(VarSet0, VarTypes0,
		Constraint, ClassNameString, BaseVar, VarSet1, VarTypes1),

		% XXX I don't think we actually need to carry the module name
		% around.
	ModuleName = unqualified("some bogus module name"),
	base_typeclass_info__make_instance_string(InstanceTypes,
		InstanceString),
	ConsId = base_typeclass_info_const(ModuleName, ClassId,
		InstanceNum, InstanceString),
	BaseTypeClassInfoTerm = functor(ConsId, []),

		% create the construction unification to initialize the variable
	BaseUnification = construct(BaseVar, ConsId, [], []),
	BaseUnifyMode = (free(unique) - ground(shared, no)) -
			(ground(shared, no) - ground(shared, no)),
	BaseUnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	BaseUnify = unify(BaseVar, BaseTypeClassInfoTerm, BaseUnifyMode,
			BaseUnification, BaseUnifyContext),

		% create a goal_info for the unification
	set__list_to_set([BaseVar], NonLocals),
	instmap_delta_from_assoc_list([BaseVar - ground(shared, no)],
		InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, det, BaseGoalInfo),

	BaseGoal = BaseUnify - BaseGoalInfo,

		% build a unification to add the argvars to the
		% base_typeclass_info
	mercury_private_builtin_module(PrivateBuiltin),
	NewConsId = cons(qualified(PrivateBuiltin, "typeclass_info"), 1),
	NewArgVars = [BaseVar|ArgVars],
	TypeClassInfoTerm = functor(NewConsId, NewArgVars),

		% introduce a new variable
	polymorphism__new_typeclass_info_var(VarSet1, VarTypes1,
		Constraint, ClassNameString, NewVar, VarSet, VarTypes),

		% create the construction unification to initialize the
		% variable
	UniMode = (free(unique) - ground(shared, no) ->
		   ground(shared, no) - ground(shared, no)),
	list__length(NewArgVars, NumArgVars),
	list__duplicate(NumArgVars, UniMode, UniModes),
	Unification = construct(NewVar, NewConsId, NewArgVars,
		UniModes),
	UnifyMode = (free(unique) - ground(shared, no)) -
			(ground(shared, no) - ground(shared, no)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(NewVar, TypeClassInfoTerm, UnifyMode,
			Unification, UnifyContext),

	% create a goal_info for the unification
	goal_info_init(GoalInfo0),
	set__list_to_set([NewVar | NewArgVars], TheNonLocals),
	goal_info_set_nonlocals(GoalInfo0, TheNonLocals, GoalInfo1),
	list__duplicate(NumArgVars, ground(shared, no), ArgInsts),
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
	ClassDefn = hlds_class_defn(SuperClasses0, ClassVars0, 
		_, ClassTVarSet, _),
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
	polymorphism__make_typeclass_info_var(C,
		ExistQVars, Context, Goals1, Goals, Info1, Info,
		MaybeVar),
	maybe_insert_var(MaybeVar, Vars1, Vars).

:- pred maybe_insert_var(maybe(prog_var), list(prog_var), list(prog_var)).
:- mode maybe_insert_var(in, in, out) is det.
maybe_insert_var(no, Vars, Vars).
maybe_insert_var(yes(Var), Vars, [Var | Vars]).

%---------------------------------------------------------------------------%

% Given a list of types, create a list of variables to hold the type_info
% for those types, and create a list of goals to initialize those type_info
% variables to the appropriate type_info structures for the types.
% Update the varset and vartypes accordingly.

polymorphism__make_type_info_vars([], _, _, [], [], Info, Info).
polymorphism__make_type_info_vars([Type | Types], ExistQVars, Context,
		ExtraVars, ExtraGoals, Info0, Info) :-
	polymorphism__make_type_info_var(Type, ExistQVars, Context,
		Var, ExtraGoals1, Info0, Info1),
	polymorphism__make_type_info_vars(Types, ExistQVars, Context,
		ExtraVars2, ExtraGoals2, Info1, Info),
	ExtraVars = [Var | ExtraVars2],
	list__append(ExtraGoals1, ExtraGoals2, ExtraGoals).

:- pred polymorphism__make_type_info_var(type, existq_tvars, prog_context,
		prog_var, list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__make_type_info_var(in, in, in, out, out, in, out) is det.

polymorphism__make_type_info_var(Type, ExistQVars, Context, Var, ExtraGoals,
		Info0, Info) :-
	(
		%
		% Check for type variables which are existentially quantified
		% in the callee's type declaration.
		% For these type variables, we assume that the callee will
		% return the type_info.  So all we need to do is to make
		% a variable to hold the returned type_info, and insert
		% that in the TypeInfoMap.
		%
		% [XXX This would need to change if we allow
		% `in' modes for arguments with existential types,
		% because in that case the mode for the type_info
		% must also be `in', so we would need to construct it.
		% The condition of the if-then-else below would
		% need to be changed to fail for those cases]
		%
		Type = term__variable(TVar),
		list__member(TVar, ExistQVars)
	->
		poly_info_get_type_info_map(Info0, TypeInfoMap0),
		% existentially quantified tvars in the head will already
		% have a type_info var allocated for them
		( map__search(TypeInfoMap0, TVar, type_info(HeadVar)) ->
			Var = HeadVar,
			Info = Info0
		;
			polymorphism__new_type_info_var(Type, "type_info",
				Var, Info0, Info1),
			map__det_insert(TypeInfoMap0, TVar, type_info(Var),
				TypeInfoMap),
			poly_info_set_type_info_map(TypeInfoMap, Info1, Info)
		),
		ExtraGoals = []
	;
		type_is_higher_order(Type, PredOrFunc, TypeArgs)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a known higher-order value of the type
		% variable.
		% The transformation we perform is basically the same as
		% in the first-order case below, except that we map
		% pred/func types to builtin pred/0 or func/0 for the
		% purposes of creating type_infos.  
		% To allow univ_to_type to check the type_infos
		% correctly, the actual arity of the pred is added to
		% the type_info of higher-order types.
		hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
		TypeId = unqualified(PredOrFuncStr) - 0,
		polymorphism__construct_type_info(Type, TypeId, TypeArgs,
			yes, ExistQVars, Context,
			Var, ExtraGoals, Info0, Info)
	;
		type_to_type_id(Type, TypeId, TypeArgs)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a known value of the type variable.
		% The transformation we perform is shown in the comment
		% at the top of the module.

		polymorphism__construct_type_info(Type, TypeId, TypeArgs,
			no, ExistQVars, Context, Var, ExtraGoals, Info0, Info)
	;
		Type = term__variable(TypeVar),
		poly_info_get_type_info_map(Info0, TypeInfoMap0),
		map__search(TypeInfoMap0, TypeVar, TypeInfoLocn)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a bound but unknown value of the type variable.
		% For example, in
		%
		%	:- pred p(T1).
		%	:- pred q(T2).
		%
		%	p(X) :- q(X).
		%
		% we know that `T2' is bound to `T1', and we translate it into
		%
		%	:- pred p(TypeInfo(T1), T1).
		%	:- pred q(TypeInfo(T2), T2).
		%
		%	p(TypeInfo, X) :- q(TypeInfo, X).

		(
				% If the typeinfo is available in a variable,
				% just use it
			TypeInfoLocn = type_info(TypeInfoVar),
			Var = TypeInfoVar,
			ExtraGoals = [],
			Info = Info0
		;
				% If the typeinfo is in a typeclass_info, first
				% extract it, then use it
			TypeInfoLocn = typeclass_info(TypeClassInfoVar, Index),
			extract_type_info(Type, TypeVar, TypeClassInfoVar,
				Index, ExtraGoals, Var, Info0, Info)
		)
	;
		Type = term__variable(TypeVar)
	->
		%
		% This occurs for code where a predicate calls a polymorphic
		% predicate with an unbound type variable.
		% Cases where there is no producer at all for the type
		% variable should get caught by post_typecheck.m.
		% XXX Cases where there is a producer but it occurs
		% somewhere further on in the goal should be avoided by
		% mode reordering, but currently mode analysis doesn't
		% do that.
		%
		poly_info_get_typevarset(Info0, TypeVarSet),
		varset__lookup_name(TypeVarSet, TypeVar, TypeVarName),
		term__context_file(Context, FileName),
		term__context_line(Context, LineNumber),
		( FileName = "" ->
			ContextMessage = ""
		;
			string__format("%s:%03d: ",
				[s(FileName), i(LineNumber)], ContextMessage)
		),
		poly_info_get_pred_name(Info0, PredName),
		string__append_list([
			"polymorphism__make_var:\n",
			ContextMessage, "In predicate `", PredName, "':\n",
			ContextMessage, "  unbound type variable `",
				TypeVarName, "'."
			], Message),
		error(Message)
	;
		error("polymorphism__make_var: unknown type")
	).

:- pred polymorphism__construct_type_info(type, type_id, list(type),
	bool, existq_tvars, prog_context, prog_var, list(hlds_goal),
	poly_info, poly_info).
:- mode polymorphism__construct_type_info(in, in, in, in, in, in, out, out, 
	in, out) is det.

polymorphism__construct_type_info(Type, TypeId, TypeArgs, IsHigherOrder, 
		ExistQVars, Context, Var, ExtraGoals, Info0, Info) :-

	% Create the typeinfo vars for the arguments
	polymorphism__make_type_info_vars(TypeArgs, ExistQVars, Context,
		ArgTypeInfoVars, ArgTypeInfoGoals, Info0, Info1),

	poly_info_get_varset(Info1, VarSet1),
	poly_info_get_var_types(Info1, VarTypes1),
	poly_info_get_module_info(Info1, ModuleInfo),

	polymorphism__init_const_type_ctor_info_var(Type,
		TypeId, ModuleInfo, VarSet1, VarTypes1, 
		BaseVar, BaseGoal, VarSet2, VarTypes2),
	polymorphism__maybe_init_second_cell(ArgTypeInfoVars,
		ArgTypeInfoGoals, Type, IsHigherOrder,
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
		IsHigherOrder, BaseVar, VarSet0, VarTypes0, ExtraGoals0,
		Var, VarSet, VarTypes, ExtraGoals) :-
	( 
		ArgTypeInfoVars = [],
		IsHigherOrder = no
	->
		Var = BaseVar,

		% Since this type_ctor_info is pretending to be
		% a type_info, we need to adjust its type.
		% Since type_ctor_info_const cons_ids are handled
		% specially, this should not cause problems.
		mercury_private_builtin_module(MercuryBuiltin),
		construct_type(qualified(MercuryBuiltin, "type_info") - 1,
			[Type], NewBaseVarType),
		map__det_update(VarTypes0, BaseVar, NewBaseVarType, VarTypes),

		VarSet = VarSet0,
		ExtraGoals = ExtraGoals0
	;
		% Unfortunately, if we have higher order terms, we
		% can no longer just optimise them to be the actual
		% type_ctor_info
		(
			IsHigherOrder = yes
		->
			list__length(ArgTypeInfoVars, PredArity),
			polymorphism__make_count_var(PredArity, VarSet0,
				VarTypes0, ArityVar, ArityGoal, VarSet1,
				VarTypes1),
			TypeInfoArgVars = [BaseVar, ArityVar | ArgTypeInfoVars],
			TypeInfoArgGoals = [ArityGoal |  ArgTypeInfoGoals]
		;
			TypeInfoArgVars = [BaseVar | ArgTypeInfoVars],
			TypeInfoArgGoals = ArgTypeInfoGoals,
			VarTypes1 = VarTypes0,
			VarSet1 = VarSet0
		),
		polymorphism__init_type_info_var(Type,
			TypeInfoArgVars, "type_info",
			VarSet1, VarTypes1, Var, TypeInfoGoal,
			VarSet, VarTypes),
		list__append(TypeInfoArgGoals, [TypeInfoGoal], ExtraGoals1),
		list__append(ExtraGoals0, ExtraGoals1, ExtraGoals)
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
	CountUnification = construct(CountVar, CountConsId, [], []),

	CountTerm = functor(CountConsId, []),
	CountInst = bound(unique, [functor(int_const(Num), [])]),
	CountUnifyMode = (free(unique) - CountInst) -
			(CountInst - CountInst),
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
	( TypeCategory = user_type ->
		module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
		( type_to_type_id(Type, TypeId, _TypeArgs) ->
			map__lookup(SpecialPredMap, SpecialPredId - TypeId,
				PredId)
		;
			error(
		"polymorphism__get_special_proc: type_to_type_id failed")
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
	TypeInfoTerm = functor(ConsId, ArgVars),

	% introduce a new variable
	polymorphism__new_type_info_var(Type, Symbol, VarSet0, VarTypes0,
		TypeInfoVar, VarSet, VarTypes),

	% create the construction unification to initialize the variable
	UniMode = (free(unique) - ground(shared, no) ->
		   ground(shared, no) - ground(shared, no)),
	list__length(ArgVars, NumArgVars),
	list__duplicate(NumArgVars, UniMode, UniModes),
	Unification = construct(TypeInfoVar, ConsId, ArgVars, UniModes),
	UnifyMode = (free(unique) - ground(shared, no)) -
			(ground(shared, no) - ground(shared, no)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(TypeInfoVar, TypeInfoTerm, UnifyMode,
			Unification, UnifyContext),

	% create a goal_info for the unification
	set__list_to_set([TypeInfoVar | ArgVars], NonLocals),
	list__duplicate(NumArgVars, ground(shared, no), ArgInsts),
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

:- pred polymorphism__init_const_type_ctor_info_var(type, type_id,
	module_info, prog_varset, map(prog_var, type), prog_var, hlds_goal,
	prog_varset, map(prog_var, type)).
:- mode polymorphism__init_const_type_ctor_info_var(in, in, in, in, in,
	out, out, out, out) is det.

polymorphism__init_const_type_ctor_info_var(Type, TypeId,
		ModuleInfo, VarSet0, VarTypes0, TypeCtorInfoVar,
		TypeCtorInfoGoal, VarSet, VarTypes) :-

	type_util__type_id_module(ModuleInfo, TypeId, ModuleName),
	type_util__type_id_name(ModuleInfo, TypeId, TypeName),
	TypeId = _ - Arity,
	ConsId = type_ctor_info_const(ModuleName, TypeName, Arity),
	TypeInfoTerm = functor(ConsId, []),

	% introduce a new variable
	polymorphism__new_type_info_var(Type, "type_ctor_info",
		VarSet0, VarTypes0, TypeCtorInfoVar, VarSet, VarTypes),

	% create the construction unification to initialize the variable
	Unification = construct(TypeCtorInfoVar, ConsId, [], []),
	UnifyMode = (free(unique) - ground(shared, no)) -
			(ground(shared, no) - ground(shared, no)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(TypeCtorInfoVar, TypeInfoTerm, UnifyMode,
			Unification, UnifyContext),

	% create a goal_info for the unification
	set__list_to_set([TypeCtorInfoVar], NonLocals),
	instmap_delta_from_assoc_list([TypeCtorInfoVar - ground(shared, no)],
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
	polymorphism__new_type_info_var(Type, "type_info", Var),
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


:- pred polymorphism__new_type_info_var(type, string, prog_var,
					poly_info, poly_info).
:- mode polymorphism__new_type_info_var(in, in, out, in, out) is det.

polymorphism__new_type_info_var(Type, Symbol, Var, Info0, Info) :-
	poly_info_get_varset(Info0, VarSet0),
	poly_info_get_var_types(Info0, VarTypes0),
	polymorphism__new_type_info_var(Type, Symbol, VarSet0, VarTypes0,
					Var, VarSet, VarTypes),
	poly_info_set_varset_and_types(VarSet, VarTypes, Info0, Info).


:- pred polymorphism__new_type_info_var(type, string, prog_varset,
		map(prog_var, type), prog_var, prog_varset,
		map(prog_var, type)).
:- mode polymorphism__new_type_info_var(in, in, in, in, out, out, out) is det.

polymorphism__new_type_info_var(Type, Symbol, VarSet0, VarTypes0,
				Var, VarSet, VarTypes) :-
	% introduce new variable
	varset__new_var(VarSet0, Var, VarSet1),
	term__var_to_int(Var, VarNum),
	string__int_to_string(VarNum, VarNumStr),
	string__append("TypeInfo_", VarNumStr, Name),
	varset__name_var(VarSet1, Var, Name, VarSet),
	mercury_private_builtin_module(PrivateBuiltin),
	construct_type(qualified(PrivateBuiltin, Symbol) - 1, [Type],
		UnifyPredType),
	map__set(VarTypes0, Var, UnifyPredType, VarTypes).

%---------------------------------------------------------------------------%

:- pred extract_type_info(type, tvar, prog_var, int, list(hlds_goal),
		prog_var, poly_info, poly_info).
:- mode extract_type_info(in, in, in, in, out, out, in, out) is det.

extract_type_info(Type, TypeVar, TypeClassInfoVar, Index, Goals,
		TypeInfoVar, PolyInfo0, PolyInfo) :-
	poly_info_get_varset(PolyInfo0, VarSet0),
	poly_info_get_var_types(PolyInfo0, VarTypes0),
	poly_info_get_type_info_map(PolyInfo0, TypeInfoLocns0),
	poly_info_get_module_info(PolyInfo0, ModuleInfo),
	extract_type_info_2(Type, TypeVar, TypeClassInfoVar, Index, ModuleInfo,
		Goals, TypeInfoVar, VarSet0, VarTypes0, TypeInfoLocns0,
		VarSet, VarTypes, TypeInfoLocns),
	poly_info_set_varset_and_types(VarSet, VarTypes, PolyInfo0, PolyInfo1),
	poly_info_set_type_info_map(TypeInfoLocns, PolyInfo1, PolyInfo).

:- pred extract_type_info_2(type, tvar, prog_var, int, module_info,
		list(hlds_goal), prog_var, prog_varset, map(prog_var, type),
		map(tvar, type_info_locn), prog_varset, map(prog_var, type),
		map(tvar, type_info_locn)).
:- mode extract_type_info_2(in, in, in, in, in, out, out, in, in, in, out, out,
	out) is det.

extract_type_info_2(Type, _TypeVar, TypeClassInfoVar, Index, ModuleInfo, Goals,
		TypeInfoVar, VarSet0, VarTypes0, TypeInfoLocns0,
		VarSet, VarTypes, TypeInfoLocns0) :-

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
	construct_type(qualified(PrivateBuiltin, "type_info") - 1,
		[term__variable(DummyTVar)], TypeInfoType),
	get_pred_id_and_proc_id(ExtractTypeInfo, predicate, DummyTVarSet, 
		[TypeClassInfoType, IntType, TypeInfoType],
		ModuleInfo, PredId, ProcId),
	
	polymorphism__make_count_var(Index, VarSet0, VarTypes0, IndexVar,
		IndexGoal, VarSet1, VarTypes1),

	polymorphism__new_type_info_var(Type, "type_info", VarSet1, VarTypes1,
		TypeInfoVar, VarSet, VarTypes),

		% Make the goal info for the call.
		% `type_info_from_typeclass_info' does not require an extra
		% type_info argument even though its declaration is
		% polymorphic.
	set__list_to_set([TypeClassInfoVar, IndexVar, TypeInfoVar], NonLocals),
	instmap_delta_from_assoc_list([TypeInfoVar - ground(shared, no)],
		InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, det, GoalInfo),

	Call = call(PredId, ProcId, 
		[TypeClassInfoVar, IndexVar, TypeInfoVar],
		not_builtin, no, ExtractTypeInfo) - GoalInfo,

	Goals = [IndexGoal, Call].

%---------------------------------------------------------------------------%

	% Create a head var for each class constraint, and make an entry in
	% the typeinfo locations map for each constrained type var.

:- pred polymorphism__make_typeclass_info_head_vars(list(class_constraint),
		list(prog_var), poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_head_vars(in, out, in, out)
		is det.

polymorphism__make_typeclass_info_head_vars(Constraints, ExtraHeadVars) -->
	{ ExtraHeadVars0 = [] },
	polymorphism__make_typeclass_info_head_vars_2(Constraints,
		ExtraHeadVars0, ExtraHeadVars1),
	{ list__reverse(ExtraHeadVars1, ExtraHeadVars) }.

:- pred polymorphism__make_typeclass_info_head_vars_2(list(class_constraint),
		list(prog_var), list(prog_var), poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_head_vars_2(in, in, out, in, out)
		is det.

polymorphism__make_typeclass_info_head_vars_2([],
		ExtraHeadVars, ExtraHeadVars) --> [].
polymorphism__make_typeclass_info_head_vars_2([C|Cs], 
		ExtraHeadVars0, ExtraHeadVars, Info0, Info) :-

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
	ClassDefn = hlds_class_defn(SuperClasses, _, _, _, _),
	list__length(SuperClasses, NumSuperClasses),

	unqualify_name(ClassName0, ClassName),

		% Make a new variable to contain the dictionary for this
		% typeclass constraint
	polymorphism__new_typeclass_info_var(VarSet0, VarTypes0, C,
		ClassName, Var, VarSet1, VarTypes1),
	ExtraHeadVars1 = [Var | ExtraHeadVars0],

		% Find all the type variables in the constraint, and remember
		% what index they appear in in the typeclass info.

		% The first type_info will be just after the superclass infos
	First is NumSuperClasses + 1,
	term__vars_list(ClassTypes, ClassTypeVars0),
	MakeIndex = lambda([Elem0::in, Elem::out, 
				Index0::in, Index::out] is det,
		(
			Elem = Elem0 - Index0,
			Index is Index0 + 1,
			% the following call is a work-around for a compiler
			% bug with intermodule optimization: it is needed to
			% resolve a type ambiguity
			is_pair(Elem)
		)),
	list__map_foldl(MakeIndex, ClassTypeVars0, ClassTypeVars, First, _),
		

		% Work out which ones haven't been seen before
	IsNew = lambda([TypeVar0::in] is semidet,
		(
			TypeVar0 = TypeVar - _Index,
			\+ map__search(TypeInfoMap0, TypeVar, _)
		)),
	list__filter(IsNew, ClassTypeVars, NewClassTypeVars),

		% Make an entry in the TypeInfo locations map for each new
		% type variable. The type variable can be found at the
		% previously calculated offset with the new typeclass_info
	MakeEntry = lambda([IndexedTypeVar::in, 
				LocnMap0::in, LocnMap::out] is det,
		(
			IndexedTypeVar = TheTypeVar - Location,
			map__det_insert(LocnMap0, TheTypeVar,
				typeclass_info(Var, Location), LocnMap)
		)),
	list__foldl(MakeEntry, NewClassTypeVars, TypeInfoMap0, TypeInfoMap1),

	poly_info_set_varset_and_types(VarSet1, VarTypes1, Info0, Info1),
	poly_info_set_type_info_map(TypeInfoMap1, Info1, Info2),

		% Handle the rest of the constraints
	polymorphism__make_typeclass_info_head_vars_2(Cs, 
		ExtraHeadVars1, ExtraHeadVars, Info2, Info).

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
	% `type_util:type_to_type_id' treats it as a type variable.
	construct_qualified_term(SymName, [], ClassNameTerm),
	mercury_private_builtin_module(PrivateBuiltin),
	construct_qualified_term(qualified(PrivateBuiltin, "constraint"),
		[ClassNameTerm | ArgTypes], ConstraintTerm),

	construct_type(qualified(PrivateBuiltin, "typeclass_info") - 1,
		[ConstraintTerm], DictionaryType).

%---------------------------------------------------------------------------%

polymorphism__typeclass_info_class_constraint(TypeClassInfoType, Constraint) :-
	mercury_private_builtin_module(PrivateBuiltin),
	type_to_type_id(TypeClassInfoType,
		qualified(PrivateBuiltin, "typeclass_info") - 1,
		[ConstraintTerm]),

	% type_to_type_id fails on `constraint/n', so we use
	% `sym_name_and_args' instead.
	mercury_private_builtin_module(PrivateBuiltin),
	sym_name_and_args(ConstraintTerm,
		qualified(PrivateBuiltin, "constraint"),
		[ClassNameTerm | ArgTypes]),
	sym_name_and_args(ClassNameTerm, ClassName, []),
	Constraint = constraint(ClassName, ArgTypes).

polymorphism__type_info_type(TypeInfoType, Type) :-
	mercury_private_builtin_module(PrivateBuiltin),
	type_to_type_id(TypeInfoType,
		qualified(PrivateBuiltin, "type_info") - 1,
		[Type]).

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

	% Expand the bodies of all class methods for typeclasses which
	% were defined in this module. The expansion involves inserting a
	% class_method_call with the appropriate arguments, which is 
	% responsible for extracting the appropriate part of the dictionary.
:- pred polymorphism__expand_class_method_bodies(module_info, module_info).
:- mode polymorphism__expand_class_method_bodies(in, out) is det.

polymorphism__expand_class_method_bodies(ModuleInfo0, ModuleInfo) :-
	module_info_classes(ModuleInfo0, Classes),
	module_info_name(ModuleInfo0, ModuleName),
	map__keys(Classes, ClassIds0),

		% Don't expand classes from other modules
	FromThisModule = lambda([ClassId::in] is semidet,
		(
			ClassId = class_id(qualified(ModuleName, _), _)
		)),
	list__filter(FromThisModule, ClassIds0, ClassIds),

	map__apply_to_list(ClassIds, Classes, ClassDefns),
	list__foldl(expand_bodies, ClassDefns, ModuleInfo0, ModuleInfo).

:- pred expand_bodies(hlds_class_defn, module_info, module_info).
:- mode expand_bodies(in, in, out) is det.

expand_bodies(hlds_class_defn(_, _, Interface, _, _), 
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
	proc_info_vartypes(ProcInfo0, Types0),
	proc_info_argmodes(ProcInfo0, argument_modes(ArgInstTable, Modes0)),
	proc_info_declared_determinism(ProcInfo0, Detism0),
	(
		Detism0 = yes(Detism1)
	->
		Detism = Detism1
	;
		error("missing determinism decl. How did we get this far?")
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
		map__apply_to_list(HeadVars1, Types0, Types),
		Modes = Modes1
	;
		error("expand_one_body: typeclass_info var not found")
	),

	BodyGoalExpr = class_method_call(TypeClassInfoVar, ProcNum0,
		HeadVars, Types, argument_modes(ArgInstTable, Modes), Detism),

		% Make the goal info for the call. 
	set__list_to_set(HeadVars0, NonLocals),

	% YYY Is this valid if ArgIKT is non-empty?
	% instmap_delta_from_mode_list(HeadVars0, ArgModes0, ModuleInfo0,
	%		InstmapDelta),

	% YYY No, but I think this is...
	instmap_delta_init_reachable(InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, Detism, GoalInfo),
	BodyGoal0 = BodyGoalExpr - GoalInfo,

	proc_info_arglives(ProcInfo0, ModuleInfo0, HeadLives),
	proc_info_vartypes(ProcInfo0, VarTypes),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	proc_info_inst_table(ProcInfo0, InstTable0),

	recompute_instmap_delta(HeadVars0, HeadLives, VarTypes, BodyGoal0,
		BodyGoal, InstMap0, InstTable0, InstTable, _, ModuleInfo0,
		ModuleInfo1),

	proc_info_set_goal(ProcInfo0, BodyGoal, ProcInfo1),
	proc_info_set_inst_table(ProcInfo1, InstTable, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
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

:- type poly_info --->
		poly_info(
			prog_varset,		% from the proc_info
			map(prog_var, type),	% from the proc_info
			tvarset,		% from the proc_info
			map(tvar, type_info_locn),		
						% specifies the location of
						% the type_info var
						% for each of the pred's type
						% parameters

			map(class_constraint, prog_var),		
						% specifies the location of
						% the typeclass_info var
						% for each of the pred's class
						% constraints
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

			string,			% pred name
			module_info,
			inst_table,
			pred_markers,		% from the pred_info
			aditi_owner
		).

init_poly_info(ModuleInfo, PredInfo, ProcInfo, PolyInfo) :-
	pred_info_name(PredInfo, PredName),
	pred_info_typevarset(PredInfo, TypeVarSet),
	pred_info_get_constraint_proofs(PredInfo, Proofs),
	pred_info_get_markers(PredInfo, Markers),
	pred_info_get_aditi_owner(PredInfo, Owner),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_inst_table(ProcInfo, InstTable),
	map__init(TypeInfoMap),
	map__init(TypeClassInfoMap),
	PolyInfo = poly_info(VarSet, VarTypes, TypeVarSet,
		TypeInfoMap, TypeClassInfoMap, Proofs, PredName,
		ModuleInfo, InstTable, Markers, Owner).

poly_info_extract(Info, PredInfo0, PredInfo,
                ProcInfo0, ProcInfo, ModuleInfo) :-
	Info = poly_info(VarSet, VarTypes, TypeVarSet, TypeInfoMap,
		TypeclassInfoLocations, _Proofs, _Name, ModuleInfo,
		InstTable, _, _),

	% set the new values of the fields in proc_info and pred_info
	proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_typeinfo_varmap(ProcInfo2, TypeInfoMap, ProcInfo3),
	proc_info_set_typeclass_info_varmap(ProcInfo3, TypeclassInfoLocations,
		ProcInfo4),
	proc_info_set_inst_table(ProcInfo4, InstTable, ProcInfo),
	pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo).

:- pred poly_info_get_varset(poly_info, prog_varset).
:- mode poly_info_get_varset(in, out) is det.

poly_info_get_varset(PolyInfo, VarSet) :-
	PolyInfo = poly_info(VarSet, _, _, _, _, _, _, _, _, _, _).

:- pred poly_info_get_var_types(poly_info, map(prog_var, type)).
:- mode poly_info_get_var_types(in, out) is det.

poly_info_get_var_types(PolyInfo, VarTypes) :-
	PolyInfo = poly_info(_, VarTypes, _, _, _, _, _, _, _, _, _).

:- pred poly_info_get_typevarset(poly_info, tvarset).
:- mode poly_info_get_typevarset(in, out) is det.

poly_info_get_typevarset(PolyInfo, TypeVarSet) :-
	PolyInfo = poly_info(_, _, TypeVarSet, _, _, _, _, _, _, _, _).

:- pred poly_info_get_type_info_map(poly_info, map(tvar, type_info_locn)).
:- mode poly_info_get_type_info_map(in, out) is det.

poly_info_get_type_info_map(PolyInfo, TypeInfoMap) :-
	PolyInfo = poly_info(_, _, _, TypeInfoMap, _, _, _, _, _, _, _).

:- pred poly_info_get_typeclass_info_map(poly_info,
					map(class_constraint, prog_var)).
:- mode poly_info_get_typeclass_info_map(in, out) is det.

poly_info_get_typeclass_info_map(PolyInfo, TypeClassInfoMap) :-
	PolyInfo = poly_info(_, _, _, _, TypeClassInfoMap, _, _, _, _, _, _).

:- pred poly_info_get_proofs(poly_info,
				map(class_constraint, constraint_proof)).
:- mode poly_info_get_proofs(in, out) is det.

poly_info_get_proofs(PolyInfo, Proofs) :-
	PolyInfo = poly_info(_, _, _, _, _, Proofs, _, _, _, _, _).

:- pred poly_info_get_pred_name(poly_info, string).
:- mode poly_info_get_pred_name(in, out) is det.

poly_info_get_pred_name(PolyInfo, PredName) :-
	PolyInfo = poly_info(_, _, _, _, _, _, PredName, _, _, _, _).

:- pred poly_info_get_module_info(poly_info, module_info).
:- mode poly_info_get_module_info(in, out) is det.

poly_info_get_module_info(PolyInfo, ModuleInfo) :-
	PolyInfo = poly_info(_, _, _, _, _, _, _, ModuleInfo, _, _, _).

:- pred poly_info_get_inst_table(poly_info, inst_table).
:- mode poly_info_get_inst_table(in, out) is det.

poly_info_get_inst_table(PolyInfo, InstTable) :-
	PolyInfo = poly_info(_, _, _, _, _, _, _, _, InstTable, _, _).

:- pred poly_info_get_markers(poly_info, pred_markers).
:- mode poly_info_get_markers(in, out) is det.

poly_info_get_markers(PolyInfo, Markers) :-
	PolyInfo = poly_info(_, _, _, _, _, _, _, _, _, Markers, _).

:- pred poly_info_get_aditi_owner(poly_info, aditi_owner).
:- mode poly_info_get_aditi_owner(in, out) is det.

poly_info_get_aditi_owner(PolyInfo, Owner) :-
	PolyInfo = poly_info(_, _, _, _, _, _, _, _, _, _, Owner).

:- pred poly_info_set_varset(prog_varset, poly_info, poly_info).
:- mode poly_info_set_varset(in, in, out) is det.

poly_info_set_varset(VarSet, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(_, B, C, D, E, F, G, H, I, J, K),
	PolyInfo = poly_info(VarSet, B, C, D, E, F, G, H, I, J, K).

:- pred poly_info_set_varset_and_types(prog_varset, map(prog_var, type),
					poly_info, poly_info).
:- mode poly_info_set_varset_and_types(in, in, in, out) is det.

poly_info_set_varset_and_types(VarSet, VarTypes, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(_, _, C, D, E, F, G, H, I, J, K),
	PolyInfo = poly_info(VarSet, VarTypes, C, D, E, F, G, H, I, J, K).

:- pred poly_info_set_typevarset(tvarset, poly_info, poly_info).
:- mode poly_info_set_typevarset(in, in, out) is det.

poly_info_set_typevarset(TypeVarSet, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, _, D, E, F, G, H, I, J, K),
	PolyInfo = poly_info(A, B, TypeVarSet, D, E, F, G, H, I, J, K).

:- pred poly_info_set_type_info_map(map(tvar, type_info_locn),
					poly_info, poly_info).
:- mode poly_info_set_type_info_map(in, in, out) is det.

poly_info_set_type_info_map(TypeInfoMap, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, _, E, F, G, H, I, J, K),
	PolyInfo = poly_info(A, B, C, TypeInfoMap, E, F, G, H, I, J, K).

:- pred poly_info_set_typeclass_info_map(map(class_constraint, prog_var),
					poly_info, poly_info).
:- mode poly_info_set_typeclass_info_map(in, in, out) is det.

poly_info_set_typeclass_info_map(TypeClassInfoMap, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, D, _, F, G, H, I, J, K),
	PolyInfo = poly_info(A, B, C, D, TypeClassInfoMap, F, G, H, I, J, K).

:- pred poly_info_set_proofs(map(class_constraint, constraint_proof),
				poly_info, poly_info).
:- mode poly_info_set_proofs(in, in, out) is det.

poly_info_set_proofs(Proofs, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, D, E, _, G, H, I, J, K),
	PolyInfo = poly_info(A, B, C, D, E, Proofs, G, H, I, J, K).

:- pred poly_info_set_module_info(module_info, poly_info, poly_info).
:- mode poly_info_set_module_info(in, in, out) is det.

poly_info_set_module_info(ModuleInfo, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, D, E, F, G, _, I, J, K),
	PolyInfo = poly_info(A, B, C, D, E, F, G, ModuleInfo, I, J, K).

:- pred poly_info_set_inst_table(inst_table, poly_info, poly_info).
:- mode poly_info_set_inst_table(in, in, out) is det.

poly_info_set_inst_table(InstTable, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, D, E, F, G, H, _, J, K),
	PolyInfo = poly_info(A, B, C, D, E, F, G, H, InstTable, J, K).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
