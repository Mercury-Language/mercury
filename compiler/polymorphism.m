%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
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
%
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
% SIMILAR CHANGES TO THE #defines IN "runtime/type_info.h"
% AND VICE VERSA.
%
% Type information is represented using one or two cells. The cell which
% is always present is the base_type_info structure, laid out like this:
%
%	word 0		<arity of type constructor>
%			e.g. 0 for `int', 1 for `list(T)', 2 for `map(K, V)'.
%	word 1		<=/2 predicate for type>
%	word 2		<index/2 predicate for type>
%	word 3		<compare/3 predicate for type>
%	word 4		<base_type_layout for type>
%	word 5		<base_type_functors for type>
%	word 6		<string name of type constructor>
%			e.g. "int" for `int', "list" for `list(T)',
%			"map" for `map(K,V)'
%	word 7		<string name of module>
%
% The other cell is the type_info structure, laid out like this:
%
%	word 0		<pointer to the base_type_info structure>
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
% we pass the address of the base_type_info structure directly, instead of
% wrapping it up in another cell. The runtime system will look at the first
% field of the cell it is passed. If this field is zero, the cell is a
% base_type_info structure for an arity zero type. If this field is not zero,
% the cell is a new type_info structure, with the first field being the
% pointer to the base_type_info structure.
%
%-----------------------------------------------------------------------------%
%
% Higher order types:
%
% There is a slight variation on this for higher-order types. Higher
% order type_infos always have a pointer to the pred/0 base_type_info,
% regardless of their true arity, so we store the real arity in the
% type-info as well.
%
%	word 0		<pointer to the base_type_info structure (pred/0)>
%	word 1		<arity of predicate>
%	word 2+		<the type_infos for the type params, at least one>
%
%-----------------------------------------------------------------------------%
%
% Sharing base_type_info structures:
%
% For compilation models that can put code addresses in static ground terms,
% we can arrange to create one copy of the base_type_info structure statically,
% avoiding the need to create other copies at runtime. For compilation models
% that cannot put code addresses in static ground terms, there are a couple
% of things we could do:
%
% 	1. allocate all cells at runtime.
%	2. use a shared static base_type_info, but initialize its code
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
%		BaseTypeInfoT2 = base_type_info(
%			1,
%			'__Unify__'<list/1>,
%			'__Index__'<list/1>,
%			'__Compare__'<list/1>,
%			<base_type_layout for list/1>,
%			<base_type_functors for list/1>,
%			"list",
%			"list"),
%		TypeInfoT2 = type_info(
%			BaseTypeInfoT2,
%			TypeInfoT1),
%		q(TypeInfoT2, [X]),
%		TypeInfoT3 = base_type_info(
%			0,
%			builtin_unify_int,
%			builtin_index_int,
%			builtin_compare_int,
%			<base_type_layout for int/0>,
%			<base_type_functors for int/0>,
%			"int",
%			"builtin"),
%		r(TypeInfoT3, 0).
%
% Note that base_type_infos are actually generated as references to a
% single shared base_type_info.
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
%	base_type_info).
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
% The base_type_info is produced statically, and there is one for each instance
% declaration. For each constraint on the instance declaration, the
% corresponding typeclass info is stored in the second part.
%
% eg. for the following program:
%
%	:- typeclass foo(T) where [...].
%	:- instance  foo(int) where [...].
%	:- instance  foo(list(T)) <= foo(T) where [...].
%
%	The typeclass_info for foo(int) is:
%		The base_type_info:
%		  * 0 (arity of the instance declaration) 
%		  * pointer to method #1
%		    ...
%		  * pointer to method #n
%
%		The type_info:
%		  * a pointer to the base typeclass info
%		  * type info for int
%
%	The typeclass_info for foo(list(T)) is:
%		The base_type_info:
%		  * 1 (arity of the instance declaration)
%		  * pointer to method #1
%		    ...
%		  * pointer to method #n
%
%		The type_info contains:
%		  * a pointer to the base typeclass info
%		  * typeclass info for foo(T)
%		  * type info for list(T)
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
% We add an extra argument for each typeclass constraint, and one argument for
% each unconstrained type variable.
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
%			BaseClassTypeInfoT2,
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
%			BaseClassTypeInfoT3,
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
%	First the UnivTypeClassInfos (for universally quantified constraints)
%	then the ExistTypeClassInfos (for existentially quantified constraints)
%	then the UnivTypeInfos (for universally quantified type variables)
% 	then the ExistTypeInfos (for existentially quantified type variables)
%	and finally the original arguments of the predicate.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module polymorphism.
:- interface.

:- import_module hlds_module, prog_data.
:- import_module io.

:- pred polymorphism__process_module(module_info, module_info,
			io__state, io__state).
:- mode polymorphism__process_module(in, out, di, uo) is det.

	% unsafe_type_cast and unsafe_promise_unique are polymorphic
	% builtins which do not need their type_infos. unsafe_type_cast
	% can be introduced by common.m after polymorphism is run, so it
	% is much simpler to avoid introducing type_info arguments for it.
	% Since both of these are really just assignment unifications, it
	% is desirable to generate them inline.
:- pred polymorphism__no_type_info_builtin(module_name, string, int).
:- mode polymorphism__no_type_info_builtin(in, in, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, hlds_goal, hlds_data, llds, (lambda).
:- import_module type_util, mode_util, quantification, instmap.
:- import_module code_util, unify_proc, special_pred, prog_util, make_hlds.
:- import_module (inst), hlds_out, base_typeclass_info, goal_util, passes_aux.

:- import_module bool, int, string, list, set, map.
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
	polymorphism__expand_class_method_bodies(ModuleInfo2, ModuleInfo).

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
	pred_info_module(PredInfo, PredModule),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, PredArity),
	(
		polymorphism__no_type_info_builtin(PredModule,
			PredName, PredArity) 
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
%	write_proc_progress_message("% Transforming polymorphism for ",
%				PredId, ProcId, ModuleInfo0, IO0, IO1),
	IO1 = IO0,

	polymorphism__process_proc(ProcId, ProcInfo0, PredId, PredInfo0, 
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

		pred_info_set_arg_types(PredInfo0, TypeVarSet, ExistQVars,
			ArgTypes, PredInfo),
		map__det_update(PredTable0, PredId, PredInfo, PredTable),
		module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1)
	;
		ModuleInfo1 = ModuleInfo0
	),
	polymorphism__fixup_preds(PredIds, ModuleInfo1, ModuleInfo).

%---------------------------------------------------------------------------%


:- pred polymorphism__process_proc(proc_id, proc_info, pred_id, pred_info,
			module_info, proc_info, pred_info, module_info).
:- mode polymorphism__process_proc(in, in, in, in, in, out, out, out) is det.

polymorphism__process_proc(ProcId, ProcInfo0, PredId, PredInfo0, ModuleInfo0,
				ProcInfo, PredInfo, ModuleInfo) :-
	proc_info_goal(ProcInfo0, Goal0),
	init_poly_info(ModuleInfo0, PredInfo0, ProcInfo0, Info0),
	polymorphism__setup_headvars(PredInfo0, ProcInfo0,
			HeadVars, ArgModes, HeadTypeVars, UnconstrainedTVars,
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
			PredId, PredInfo0, ProcId, ProcInfo0,
			UnconstrainedTVars, ExtraTypeInfoHeadVars,
			ExistTypeClassInfoHeadVars,
			Goal1, Goal2, Info2, Info3),

		pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
		polymorphism__fixup_quantification(HeadVars, ExistQVars,
			Goal2, Goal3, Info3, Info4),

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
			HeadTypeVars = HeadTypeParams
		->
			Goal = Goal3,
			Info = Info4
		;
			poly_info_get_module_info(Info4, ModuleInfo4),
			mode_list_get_initial_insts(ArgModes, ModuleInfo4,
				InitialInsts),
			assoc_list__from_corresponding_lists(HeadVars,
				InitialInsts, InstAL),
			instmap__from_assoc_list(InstAL, InstMap),
			recompute_instmap_delta(no, Goal3, Goal, InstMap,
				ModuleInfo4, ModuleInfo5),
			poly_info_set_module_info(ModuleInfo5, Info4, Info)
		)
	),

	%
	% set the new values of the fields in proc_info and pred_info
	%
	Info = poly_info(VarSet, VarTypes, TypeVarSet,
				TypeInfoMap, TypeClassInfoMap,
				_Proofs, _PredName, ModuleInfo),
	proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo1),
	proc_info_set_goal(ProcInfo1, Goal, ProcInfo2),
	proc_info_set_varset(ProcInfo2, VarSet, ProcInfo3),
	proc_info_set_vartypes(ProcInfo3, VarTypes, ProcInfo4),
	proc_info_set_argmodes(ProcInfo4, ArgModes, ProcInfo5),
	proc_info_set_typeinfo_varmap(ProcInfo5, TypeInfoMap, ProcInfo6),
	proc_info_set_typeclass_info_varmap(ProcInfo6, TypeClassInfoMap,
		ProcInfo),
	pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo).

% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

:- pred polymorphism__setup_headvars(pred_info, proc_info,
		list(var), list(mode), list(tvar), list(tvar), list(var),
		list(var), poly_info, poly_info).
:- mode polymorphism__setup_headvars(in, in, out, out, out, out, out, out,
		in, out) is det.

polymorphism__setup_headvars(PredInfo, ProcInfo, HeadVars, ArgModes,
		HeadTypeVars, UnconstrainedTVars, ExtraHeadTypeInfoVars,
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

		% the above call inserts entries into the typeinfo map
		% which we don't want, so we must reset it
	map__init(TypeInfoMap2),
	poly_info_set_type_info_map(TypeInfoMap2, PolyInfo1, PolyInfo2),
	polymorphism__make_typeclass_info_head_vars(UnivConstraints,
		UnivHeadTypeClassInfoVars, PolyInfo2, PolyInfo3),
	poly_info_get_type_info_map(PolyInfo3, TypeInfoMap3),
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
		PolyInfo4 = PolyInfo3
	;
		list__delete_elems(UnconstrainedTVars, ExistQVars,
			UnconstrainedUnivTVars),
		list__delete_elems(UnconstrainedTVars, UnconstrainedUnivTVars,
			UnconstrainedExistTVars),
		polymorphism__make_head_vars(UnconstrainedExistTVars,
			ArgTypeVarSet, ExistHeadTypeInfoVars,
			PolyInfo3, PolyInfo4)
	),
	polymorphism__make_head_vars(UnconstrainedUnivTVars, ArgTypeVarSet,
		UnivHeadTypeInfoVars, PolyInfo4, PolyInfo5),
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
	list__condense([UnivTypeClassInfoModes, ExistTypeClassInfoModes,
		UnivTypeInfoModes, ExistTypeInfoModes, ArgModes0], ArgModes),
		
	%
	% Add the locations of the typeinfos
	% for unconstrained, universally quantified type variables.
	% to the initial tvar->type_info_var mapping
	%
	ToLocn = lambda([TheVar::in, TheLocn::out] is det,
			TheLocn = type_info(TheVar)),
	list__map(ToLocn, UnivHeadTypeInfoVars, UnivTypeLocns),
	map__det_insert_from_corresponding_lists(TypeInfoMap3,
		UnconstrainedUnivTVars, UnivTypeLocns, TypeInfoMap6),
	poly_info_set_type_info_map(TypeInfoMap6, PolyInfo5, PolyInfo6),

	% Make a map of the locations of the typeclass_infos
	map__from_corresponding_lists(UnivConstraints,
			UnivHeadTypeClassInfoVars, TypeClassInfoMap),
	poly_info_set_typeclass_info_map(TypeClassInfoMap, PolyInfo6, PolyInfo).


% XXX the following code ought to be rewritten to handle
% existential/universal type_infos and type_class_infos
% in a more consistent manner.

%
% generate code to produce the values of type_infos and typeclass_infos
% for existentially quantified type variables in the head
%
:- pred polymorphism__produce_existq_tvars(
			pred_id, pred_info, proc_id, proc_info,
			list(tvar), list(var), list(var), hlds_goal, hlds_goal,
			poly_info, poly_info).
:- mode polymorphism__produce_existq_tvars(in, in, in, in, in, in, in, in, out,
			in, out) is det.

polymorphism__produce_existq_tvars(PredId, PredInfo, ProcId, ProcInfo,
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
	ClassContext = constraints(_UnivConstraints, ExistConstraints),
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
	map__init(Subst),
	ExistQVarsForCall = [],
	Goal0 = _ - GoalInfo,
	goal_info_get_context(GoalInfo, Context),
	polymorphism__make_typeclass_info_vars(	
		ExistConstraints, Subst, TypeSubst, ExistQVarsForCall, Context,
		hlds_class_proc(PredId, ProcId),
		hlds_class_proc(NewPredId, NewProcId),
		ExistTypeClassVars, ExtraTypeClassGoals,
		_ExistConstrainedTVars, Info0, Info1),
	% sanity check
	( PredId = NewPredId, ProcId = NewProcId ->
		true
	;
		error("polymorphism.m: impossible specialization")
	),
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
		TypeInfoVars, ExtraTypeInfoGoals, Info1, Info),
	polymorphism__assign_var_list(TypeInfoHeadVars, TypeInfoVars,
		ExtraTypeInfoUnifyGoals),
	list__condense([[Goal0],
			ExtraTypeClassGoals, ExtraTypeClassUnifyGoals,
			ExtraTypeInfoGoals, ExtraTypeInfoUnifyGoals],
			GoalList),
	conj_list_to_goal(GoalList, GoalInfo, Goal).

:- pred polymorphism__assign_var_list(list(var), list(var), list(hlds_goal)).
:- mode polymorphism__assign_var_list(in, in, out) is det.

polymorphism__assign_var_list([], [_|_], _) :-
	error("unify_proc__assign_var_list: length mismatch").
polymorphism__assign_var_list([_|_], [], _) :-
	error("unify_proc__assign_var_list: length mismatch").
polymorphism__assign_var_list([], [], []).
polymorphism__assign_var_list([Var1 | Vars1], [Var2 | Vars2], [Goal | Goals]) :-
	polymorphism__assign_var(Var1, Var2, Goal),
	polymorphism__assign_var_list(Vars1, Vars2, Goals).

:- pred polymorphism__assign_var(var, var, hlds_goal).
:- mode polymorphism__assign_var(in, in, out) is det.

polymorphism__assign_var(Var1, Var2, Goal) :-
	( Var1 = Var2 ->
		true_goal(Goal)
	;
		polymorphism__assign_var_2(Var1, Var2, Goal)
	).

:- pred polymorphism__assign_var_2(var, var, hlds_goal).
:- mode polymorphism__assign_var_2(in, in, out) is det.

polymorphism__assign_var_2(Var1, Var2, Goal) :-

	% Doing just this wouldn't work, because we also need to fill in
	% the mode and determinism info:
	%	term__context_init(Context),
	%	create_atomic_unification(Var1, var(Var2), Context, explicit,
	% 		[], Goal),

	Ground = ground(shared, no),
	Mode = ((free -> Ground) - (Ground -> Ground)),
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
	% constructed, not when they are called.  (Or at least I
	% think we don't... -fjh.)
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
		{ classify_type(Type, ModuleInfo, TypeCategory) },
		{ polymorphism__get_special_proc(TypeCategory, Type,
			SpecialPredId, ModuleInfo, Name, PredId1, ProcId1) }
	;
		{ PredId1 = PredId0 },
		{ ProcId1 = ProcId0 },
		{ Name = Name0 }
	),

	polymorphism__process_call(PredId1, ProcId1, ArgVars0, GoalInfo,
		PredId, ProcId, ArgVars, _ExtraVars, CallGoalInfo, ExtraGoals),

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
			% XXX Bug! - we should check that the mode is (in, in),
			%     and report an error (e.g. "unification of
			%     polymorphically typed variables in partially
			%     instantiated mode") if it isn't
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
			{ unify_proc__lookup_mode_num(ModuleInfo, TypeId,
				UniMode, Det, ProcId) },
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
		{ Y = lambda_goal(PredOrFunc, ArgVars, Vars,
			Modes, Det, LambdaGoal0) }
	->
		% for lambda expressions, we must recursively traverse the
		% lambda goal and then convert the lambda expression
		% into a new predicate
		polymorphism__process_goal(LambdaGoal0, LambdaGoal1),
		% XXX currently we don't allow lambda goals to be
		% existentially typed
		{ ExistQVars = [] },
		polymorphism__fixup_lambda_quantification(LambdaGoal1,
				ExistQVars, LambdaGoal, NonLocalTypeInfos),
		polymorphism__process_lambda(PredOrFunc, Vars, Modes,
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

polymorphism__process_goal_expr(pragma_c_code(IsRecursive, PredId0, ProcId0,
		ArgVars0, ArgInfo0, OrigArgTypes0, PragmaCode),
		GoalInfo, Goal) -->
	polymorphism__process_call(PredId0, ProcId0, ArgVars0, GoalInfo,
		PredId, ProcId, ArgVars, ExtraVars, CallGoalInfo, ExtraGoals),

	%
	% insert the type_info vars into the arg-name map,
	% so that the c_code can refer to the type_info variable
	% for type T as `TypeInfo_for_T'.
	%
	=(Info0),
	{ poly_info_get_module_info(Info0, ModuleInfo) },
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_arg_types(PredInfo, PredTypeVarSet, ExistQVars,
			PredArgTypes) },
	{ term__vars_list(PredArgTypes, PredTypeVars0) },
	{ list__remove_dups(PredTypeVars0, PredTypeVars) },
	{ polymorphism__c_code_add_typeinfos(ExtraVars, PredTypeVars,
			PredTypeVarSet, ExistQVars, ArgInfo0, ArgInfo) },

	%
	% insert type_info types for all the inserted type_info vars
	% into the arg-types list
	%
	{ mercury_private_builtin_module(PrivateBuiltin) },
	{ MakeType = lambda([TypeVar::in, TypeInfoType::out] is det,
		construct_type(qualified(PrivateBuiltin, "type_info") - 1,
			[term__variable(TypeVar)], TypeInfoType)) },
	{ list__map(MakeType, PredTypeVars, TypeInfoTypes) },
	{ list__append(TypeInfoTypes, OrigArgTypes0, OrigArgTypes) },

	%
	% plug it all back together
	%
	{ Call = pragma_c_code(IsRecursive, PredId, ProcId, ArgVars,
			ArgInfo, OrigArgTypes, PragmaCode) - CallGoalInfo },
	{ list__append(ExtraGoals, [Call], GoalList) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) }.

:- pred polymorphism__c_code_add_typeinfos(list(var), list(tvar),
		tvarset, existq_tvars, list(maybe(pair(string, mode))),
		list(maybe(pair(string, mode)))). 
:- mode polymorphism__c_code_add_typeinfos(in, in, in, in, in, out) is det.

polymorphism__c_code_add_typeinfos([], [], _, _, ArgNames, ArgNames).
polymorphism__c_code_add_typeinfos([_Var|Vars], [TVar|TVars], TypeVarSet,
		ExistQVars, ArgNames0, ArgNames) :-
	polymorphism__c_code_add_typeinfos(Vars, TVars, TypeVarSet,
		ExistQVars, ArgNames0, ArgNames1),
	( varset__search_name(TypeVarSet, TVar, TypeVarName) ->
		string__append("TypeInfo_for_", TypeVarName, C_VarName),
		polymorphism__typeinfo_mode(ExistQVars, TVar, Mode),
		ArgNames = [yes(C_VarName - Mode) | ArgNames1]
	;
		ArgNames = [no | ArgNames1]
	).
polymorphism__c_code_add_typeinfos([], [_|_], _, _, _, _) :-
	error("polymorphism__c_code_add_typeinfos: length mismatch").
polymorphism__c_code_add_typeinfos([_|_], [], _, _, _, _) :-
	error("polymorphism__c_code_add_typeinfos: length mismatch").

:- pred polymorphism__typeinfo_mode(existq_tvars, tvar, mode).
:- mode polymorphism__typeinfo_mode(in, in, out) is det.

polymorphism__typeinfo_mode(ExistQVars, TVar, Mode) :-
	%
	% type_infos have mode `in', unless the type
	% variable is existentially quantified, in which
	% case the mode is `out'.
	%
	% [XXX this would need to change if we allow
	% `in' modes for arguments with existential types,
	% because in that case the mode for the type_info
	% must also be `in']
	%
	( list__member(TVar, ExistQVars) ->
		out_mode(Mode)
	;
		in_mode(Mode)
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

:- pred polymorphism__process_call(pred_id, proc_id, list(var), hlds_goal_info,
		pred_id, proc_id, list(var), list(var), hlds_goal_info,
		list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__process_call(in, in, in, in,
		out, out, out, out, out, out, in, out) is det.

polymorphism__process_call(PredId0, ProcId0, ArgVars0, GoalInfo0,
		PredId, ProcId, ArgVars, ExtraVars, GoalInfo, ExtraGoals,
		Info0, Info) :-

	poly_info_get_var_types(Info0, VarTypes),
	poly_info_get_typevarset(Info0, TypeVarSet0),
	poly_info_get_module_info(Info0, ModuleInfo),

	module_info_pred_info(ModuleInfo, PredId0, PredInfo),
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
		varset__merge_subst(TypeVarSet0, PredTypeVarSet, TypeVarSet,
			Subst),
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
		)
	->
		PredId = PredId0,
		ProcId = ProcId0,
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
		error("polymorphism__process_call: type unification failed")
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
		polymorphism__make_typeclass_info_vars(	
			UniversalConstraints1, Subst, TypeSubst,
			PredExistQVars, Context,
			hlds_class_proc(PredId0, ProcId0),
			hlds_class_proc(PredId, ProcId),
			UnivTypeClassVars, ExtraTypeClassGoals,
			UnivConstrainedTVars, Info1, Info2),

			% compute which type variables were constrained
			% by the existential type class constraints
		constraint_list_get_tvars(ExistentialConstraints1,
			ExistConstrainedTVars),

		list__append(UnivTypeClassVars, ExistTypeClassVars,
			ExtraTypeClassVars),

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
		poly_info_get_type_info_map(Info, TypeVarMap),
		poly_info_get_typeclass_info_map(Info, TypeClassVarMap),
		goal_info_get_instmap_delta(GoalInfo1, InstmapDelta0),
		AddInstDelta = lambda([TVar::in, IMD0::in, IMD::out] is det, (
			map__lookup(TypeVarMap, TVar, TypeInfoLocn),
			type_info_locn_var(TypeInfoLocn, TypeInfoVar),
			instmap_delta_set(IMD0, TypeInfoVar,
				ground(shared, no), IMD)
			)),
		AddTCInstDelta = lambda([Constraint::in, IMD0::in, IMD::out]
					is det, (
			map__lookup(TypeClassVarMap, Constraint,
				TypeClassInfoVar),
			instmap_delta_set(IMD0, TypeClassInfoVar,
				ground(shared, no), IMD)
			)),
		list__foldl(AddInstDelta, PredExistQVars,
			InstmapDelta0, InstmapDelta1),
		list__foldl(AddTCInstDelta, ExistentialConstraints,
			InstmapDelta1, InstmapDelta),
		goal_info_set_instmap_delta(GoalInfo1, InstmapDelta, GoalInfo)
	).

:- pred polymorphism__update_typeclass_infos(list(class_constraint), list(var),
			poly_info, poly_info).
:- mode polymorphism__update_typeclass_infos(in, in, in, out) is det.

polymorphism__update_typeclass_infos(Constraints, Vars, Info0, Info) :-
	poly_info_get_typeclass_info_map(Info0, TypeClassInfoMap0),
	map__det_insert_from_corresponding_lists(TypeClassInfoMap0,
		Constraints, Vars, TypeClassInfoMap),
	poly_info_set_typeclass_info_map(TypeClassInfoMap, Info0, Info).

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

:- pred polymorphism__fixup_quantification(list(var), existq_tvars,
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

:- pred polymorphism__fixup_lambda_quantification(hlds_goal, existq_tvars,
		hlds_goal, set(var), poly_info, poly_info).
:- mode polymorphism__fixup_lambda_quantification(in, in, out, out, in, out)
		is det.

%
% If the lambda goal we are processing is polymorphically typed,
% may need to fix up the quantification (non-local variables)
% so that it includes the type-info variables and type-class-info
% variables for any polymorphically typed variables in the non-locals set.
%

polymorphism__fixup_lambda_quantification(Goal0, ExistQVars, Goal,
		NewOutsideVars, Info0, Info) :-
	poly_info_get_type_info_map(Info0, TypeVarMap),
	poly_info_get_typeclass_info_map(Info0, TypeClassVarMap),
	( map__is_empty(TypeVarMap) ->
		set__init(NewOutsideVars),
		Info = Info0,
		Goal = Goal0
	;
		poly_info_get_varset(Info0, VarSet0),
		poly_info_get_var_types(Info0, VarTypes0),
		goal_util__extra_nonlocal_typeinfos(TypeVarMap,
			TypeClassVarMap, VarTypes0, ExistQVars, Goal0,
			NewOutsideVars),
		Goal0 = _ - GoalInfo0,
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		set__union(NewOutsideVars, NonLocals, OutsideVars),
		implicitly_quantify_goal(Goal0, VarSet0, VarTypes0,
			OutsideVars, Goal, VarSet, VarTypes, _Warnings),
		poly_info_set_varset_and_types(VarSet, VarTypes, Info0, Info)
	).

%-----------------------------------------------------------------------------%

:- pred polymorphism__process_lambda(pred_or_func, list(var),
		list(mode), determinism, list(var), set(var),
		hlds_goal, unification, unify_rhs, unification,
		poly_info, poly_info).
:- mode polymorphism__process_lambda(in, in, in, in, in, in, in, in, out, out,
		in, out) is det.

polymorphism__process_lambda(PredOrFunc, Vars, Modes, Det, OrigNonLocals,
		NonLocalTypeInfos, LambdaGoal, Unification0, Functor,
		Unification, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(VarSet, VarTypes, TVarSet, TVarMap, 
			TCVarMap, _Proofs, PredName, ModuleInfo0),

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
	Constraints = constraints(UnivConstraints, []),
	lambda__transform_lambda(PredOrFunc, PredName, Vars, Modes, Det,
		OrigNonLocals, NonLocalTypeInfos, LambdaGoal, Unification0,
		VarSet, VarTypes, Constraints, TVarSet, TVarMap, TCVarMap,
		ModuleInfo0, Functor, Unification, ModuleInfo),
	poly_info_set_module_info(ModuleInfo, PolyInfo0, PolyInfo).

:- pred polymorphism__constraint_contains_vars(list(var), class_constraint).
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
%
% If the called predicate is a class method, and we know which instance
% it is, then instead of creating a type_info variable for the type class
% instance, we just return the pred_proc_id for that instance.
% Otherwise we return the original pred_proc_id unchanged.

:- pred polymorphism__make_typeclass_info_vars(list(class_constraint),
	substitution, tsubst, existq_tvars, term__context,
	hlds_class_proc, hlds_class_proc,
	list(var), list(hlds_goal), list(tvar),
	poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_vars(in, in, in, in, in, in, out,
	out, out, out, in, out) is det.

polymorphism__make_typeclass_info_vars(PredClassContext, Subst, TypeSubst, 
		ExistQVars, Context, PredProcId0, PredProcId,
		ExtraVars, ExtraGoals, ConstrainedTVars, Info0, Info) :-

		% initialise the accumulators
	ExtraVars0 = [],
	ExtraGoals0 = [],
	ConstrainedTVars0 = [],

		% The PredProcId is set to `yes(_)' for the first call only,
		% because we can only specialize method calls if we know
		% which instance of the method's type class it is; knowing
		% the instances for any of the other type class constraints
		% on a method doesn't help us specialize the call.
	MaybePredProcId0 = yes(PredProcId0),

		% do the work
	polymorphism__make_typeclass_info_vars_2(PredClassContext, 
		Subst, TypeSubst, ExistQVars, Context,
		MaybePredProcId0, MaybePredProcId,
		ExtraVars0, ExtraVars1, 
		ExtraGoals0, ExtraGoals1,
		ConstrainedTVars0, ConstrainedTVars, 
		Info0, Info),
	
		% We build up the vars and goals in reverse order
	list__reverse(ExtraVars1, ExtraVars),
	list__reverse(ExtraGoals1, ExtraGoals),

		% If we succeeded in specializing this call, then use
		% the specialization, otherwise use the original call.
	( MaybePredProcId = yes(PredProcId1) ->
		PredProcId = PredProcId1
	;
		PredProcId = PredProcId0
	).

% Accumulator version of the above.
:- pred polymorphism__make_typeclass_info_vars_2(
	list(class_constraint), substitution, tsubst,
	existq_tvars, term__context,
	maybe(hlds_class_proc), maybe(hlds_class_proc),
	list(var), list(var), 
	list(hlds_goal), list(hlds_goal), 
	list(var), list(var),
	poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_vars_2(in, in, in, in, in,
	in, out, in, out, in, out, in, out, in, out) is det.

polymorphism__make_typeclass_info_vars_2([], _Subst, _TypeSubst, _ExistQVars,
		_Context, MaybePredProcId, MaybePredProcId,
		ExtraVars, ExtraVars, 
		ExtraGoals, ExtraGoals, 
		ConstrainedTVars, ConstrainedTVars,
		Info, Info).
polymorphism__make_typeclass_info_vars_2([C|Cs], Subst, TypeSubst, ExistQVars,
		Context, MaybePredProcId0, MaybePredProcId,
		ExtraVars0, ExtraVars,
		ExtraGoals0, ExtraGoals, 
		ConstrainedTVars0, ConstrainedTVars,
		Info0, Info) :-
	polymorphism__make_typeclass_info_var(C, Subst, TypeSubst, ExistQVars,
			Context, MaybePredProcId0, MaybePredProcId,
			ExtraGoals0, ExtraGoals1, 
			ConstrainedTVars0, ConstrainedTVars1,
			Info0, Info1, MaybeExtraVar),
	maybe_insert_var(MaybeExtraVar, ExtraVars0, ExtraVars1),
	polymorphism__make_typeclass_info_vars_2(Cs, Subst, TypeSubst,
			ExistQVars, Context, no, _,
			ExtraVars1, ExtraVars,
			ExtraGoals1, ExtraGoals, 
			ConstrainedTVars1, ConstrainedTVars,
			Info1, Info).

:- pred polymorphism__make_typeclass_info_var(class_constraint,
	substitution, tsubst, existq_tvars, term__context,
	maybe(hlds_class_proc), maybe(hlds_class_proc),
	list(hlds_goal), list(hlds_goal), list(var), list(var),
	poly_info, poly_info, maybe(var)). 
:- mode polymorphism__make_typeclass_info_var(in, in, in, in, in, in, out,
	in, out, in, out, in, out, out) is det.

polymorphism__make_typeclass_info_var(Constraint, Subst, TypeSubst, ExistQVars,
		Context, MaybePredProcId0, MaybePredProcId,
		ExtraGoals0, ExtraGoals, ConstrainedTVars0, ConstrainedTVars, 
		Info0, Info, MaybeVar) :-
	Constraint = constraint(ClassName, NewConstrainedTypes),
	list__length(NewConstrainedTypes, ClassArity),
	ClassId = class_id(ClassName, ClassArity),
	term__vars_list(NewConstrainedTypes, NewConstrainedTVars),
	list__append(NewConstrainedTVars, ConstrainedTVars0, ConstrainedTVars),
	term__apply_substitution_to_list(NewConstrainedTypes, TypeSubst, 
		ConstrainedTypes0),
	% we need to maintain the invariant that types in class constraints
	% do not contain any information in their term__context fields
	strip_term_contexts(ConstrainedTypes0, ConstrainedTypes),
	NewC = constraint(ClassName, ConstrainedTypes),

	Info0 = poly_info(VarSet0, VarTypes0, TypeVarSet0, TypeInfoMap0, 
		TypeClassInfoMap0, Proofs, PredName, ModuleInfo),

	(
		map__search(TypeClassInfoMap0, NewC, Location)
	->
			% We already have a typeclass_info for this constraint
		ExtraGoals = ExtraGoals0,
		Var = Location,
		MaybeVar = yes(Var),
		MaybePredProcId = no,
		Info = Info0
	;
			% We don't have the typeclass_info as a parameter to
			% the pred, so we must be able to create it from
			% somewhere else

			% Work out how to make it
		map__lookup(Proofs, NewC, Proof),
		(
				% We have to construct the typeclass_info
				% using an instance declaration
			Proof = apply_instance(ProofInstanceDefn, InstanceNum),

				% The subst has already been applied to these
				% constraints in typecheck.m
			ProofInstanceDefn = hlds_instance_defn(_,
				InstanceConstraints, _, _, _, _, _),

			%
			% Check whether the callee is a class method,
			% and that this contraint is the first constraint
			% in that callee's constraint list (the one for
			% its own type class).
			% If so, specialize the call by replacing the
			% generic class method call with a direct call
			% to the class method for this instance.
			%
			(
				% check that this constraint is the
				% first constraint in the callee's
				% constraint list
				MaybePredProcId0 = yes(PredProcId0),

				% check that the called pred is a class method
				PredProcId0 = hlds_class_proc(PredId0, _),
				module_info_pred_info(ModuleInfo, PredId0,
						PredInfo),
				pred_info_get_markers(PredInfo, Markers),
				check_marker(Markers, class_method),

				% enabling this optimisation causes a bug
				% where implied instances are concerned. 
				% When the class method call is inlined, the
				% extra typeclass_infos from the instance
				% declaration are not included. Until that
				% bug is fixed, we will disable the
				% optimisation.
				semidet_fail
			->
				% Get the class methods, and figure out
				% the method number of this class method.
				module_info_classes(ModuleInfo, ClassTable),
				map__lookup(ClassTable, ClassId, ClassDefn),
				ClassDefn = hlds_class_defn(_, _, ClassMethods,
						_, _),
				( list__nth_member_search(ClassMethods,
						PredProcId0, MethodNum0) ->
					MethodNum = MethodNum0
				;
					error("poly: nth_member_search failed")
				),

				% Get the instance methods, and lookup
				% the pred for the corresponding method number.
				% (NB. We can't use ProofInstanceDefn,
				% because its MaybeInstanceMethods field
				% has not been updated (is still `no').)
				module_info_instances(ModuleInfo,
					InstanceTable),
				map__lookup(InstanceTable, ClassId,
					InstanceDefns),
				list__index1_det(InstanceDefns, InstanceNum,
					InstanceDefn),
				InstanceDefn = hlds_instance_defn(_, _, _, _,
					MaybeInstanceMethods, _, _),
				( MaybeInstanceMethods = yes(InstanceMethods0)
				->
					InstanceMethods = InstanceMethods0
				;
					error("poly: no instance methods")
				),
				list__index1_det(InstanceMethods, MethodNum,
					InstanceMethod),
				MaybePredProcId = yes(InstanceMethod),
				MaybeVar = no,
				ExtraGoals = ExtraGoals0,
				Info = Info0
			;

					% Make the type_infos for the types
					% that are constrained by this. These
					% are packaged in the typeclass_info
				term__apply_substitution_to_list(
					ConstrainedTypes, VarTypes0,
					RenamedConstrainedTypes),
				polymorphism__make_type_info_vars(
					RenamedConstrainedTypes, ExistQVars,
					Context, InstanceExtraTypeInfoVars,
					TypeInfoGoals,
					Info0, Info1),

					% Make the typeclass_infos for the
					% constraints from the context of the
					% instance decl.
				polymorphism__make_typeclass_info_vars_2(
					InstanceConstraints, Subst, TypeSubst, 
					ExistQVars, Context, no, _,
					[], InstanceExtraTypeClassInfoVars, 
					ExtraGoals0, ExtraGoals1, 
					[], _,
					Info1, Info2),

				polymorphism__construct_typeclass_info(
					InstanceExtraTypeInfoVars, 
					InstanceExtraTypeClassInfoVars, 
					ClassId, InstanceNum,
					ExistQVars,
					Var, NewGoals, 
					Info2, Info),

				MaybeVar = yes(Var),
				MaybePredProcId = no,

					% Oh, yuck. The type_info goals have
					% already been reversed, so lets
					% reverse them back.
				list__reverse(TypeInfoGoals, RevTypeInfoGoals),

				list__append(ExtraGoals1, RevTypeInfoGoals,
					ExtraGoals2),
				list__append(NewGoals, ExtraGoals2, ExtraGoals)
			)
		;
				% We have to extract the typeclass_info from
				% another one
			Proof = superclass(SubClassConstraint0),

				% First create a variable to hold the new
				% typeclass_info 
			unqualify_name(ClassName, ClassNameString),
			polymorphism__new_typeclass_info_var(VarSet0, VarTypes0,
				ClassNameString, Var, VarSet1, VarTypes1),

			MaybeVar = yes(Var),
			MaybePredProcId = no,

				% Then work out where to extract it from
			SubClassConstraint0 = 
				constraint(SubClassName, SubClassTypes0),
			term__apply_substitution_to_list(SubClassTypes0, Subst,
				SubClassTypes1),
			% we need to maintain the invariant that types in
			% class constraints do not contain any information
			% in their term__context fields
			strip_term_contexts(SubClassTypes1, SubClassTypes),
			SubClassConstraint = 
				constraint(SubClassName, SubClassTypes),
			list__length(SubClassTypes, SubClassArity),
			SubClassId = class_id(SubClassName, SubClassArity),

			Info1 = poly_info(VarSet1, VarTypes1, TypeVarSet0, 
				TypeInfoMap0, TypeClassInfoMap0, Proofs, 
				PredName, ModuleInfo),

				% Make the typeclass_info for the subclass
			polymorphism__make_typeclass_info_var(
				SubClassConstraint, Subst, TypeSubst, 
				ExistQVars, Context, no, _,
				ExtraGoals0, ExtraGoals1, 
				[], _,
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
			ToTerm = lambda([TheVar::in, TheTerm::out] is det,
				(
					TheTerm = term__variable(TheVar)
				)),
			list__map(ToTerm, SubClassVars, SubClassVarTerms),
			(
				type_list_subsumes(SubClassVarTerms,
					SubClassTypes, SubTypeSubst0)
			->
				SubTypeSubst0 = SubTypeSubst
			;
				error("polymorphism__make_typeclass_info_var")
			),
			apply_rec_subst_to_constraint_list(SubTypeSubst,
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

				% Make the goal for the call
			varset__init(Empty),
			mercury_private_builtin_module(PrivateBuiltin),
			ExtractSuperClass = qualified(PrivateBuiltin, 
					  "superclass_from_typeclass_info"),
			construct_type(qualified(PrivateBuiltin,
					"typeclass_info") - 0,
					[], TypeClassInfoType),
			construct_type(unqualified("int") - 0, [], IntType),
			get_pred_id_and_proc_id(ExtractSuperClass, predicate, 
				Empty, 
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

:- pred polymorphism__construct_typeclass_info(list(var), list(var), class_id, 
	int, existq_tvars, var, list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__construct_typeclass_info(in, in, in, in, in, out, out, 
	in, out) is det.

polymorphism__construct_typeclass_info(ArgTypeInfoVars, ArgTypeClassInfoVars,
		ClassId, InstanceNum, ExistQVars,
		NewVar, NewGoals, Info0, Info) :-

	poly_info_get_module_info(Info0, ModuleInfo),

	module_info_instances(ModuleInfo, InstanceTable),
	map__lookup(InstanceTable, ClassId, InstanceList),
	list__index1_det(InstanceList, InstanceNum, InstanceDefn),
	InstanceDefn = hlds_instance_defn(_, _, InstanceTypes, _, _, _, 
		SuperClassProofs),

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
		ClassNameString, BaseVar, VarSet1, VarTypes1),

	base_typeclass_info__make_instance_string(InstanceTypes,
		InstanceString),

		% XXX I don't think we actually need to carry the module name
		% around.
	ModuleName = unqualified("some bogus module name"),
	ConsId = base_typeclass_info_const(ModuleName, ClassId, InstanceString),
	BaseTypeClassInfoTerm = functor(ConsId, []),

		% create the construction unification to initialize the variable
	BaseUnification = construct(BaseVar, ConsId, [], []),
	BaseUnifyMode = (free -> ground(shared, no)) -
			(ground(shared, no) -> ground(shared, no)),
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
		ClassNameString, NewVar, VarSet, VarTypes),

		% create the construction unification to initialize the
		% variable
	UniMode = (free - ground(shared, no) ->
		   ground(shared, no) - ground(shared, no)),
	list__length(NewArgVars, NumArgVars),
	list__duplicate(NumArgVars, UniMode, UniModes),
	Unification = construct(NewVar, NewConsId, NewArgVars,
		UniModes),
	UnifyMode = (free -> ground(shared, no)) -
			(ground(shared, no) -> ground(shared, no)),
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
	InstConsId = cons( qualified(PrivateBuiltin, "typeclass_info"), 
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
	list(var), list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__get_arg_superclass_vars(in, in, in, in, out, out, 
	in, out) is det.

polymorphism__get_arg_superclass_vars(ClassDefn, InstanceTypes, 
		SuperClassProofs, ExistQVars, NewVars, NewGoals,
		Info0, Info) :-

	poly_info_get_proofs(Info0, Proofs),

	poly_info_get_varset(Info0, VarSet0),
	ClassDefn = hlds_class_defn(SuperClasses, ClassVars, _, ClassVarSet, _),
	map__from_corresponding_lists(ClassVars, InstanceTypes, TypeSubst),
	varset__merge_subst(VarSet0, ClassVarSet, VarSet1, Subst),
	poly_info_set_varset(VarSet1, Info0, Info1),

	poly_info_set_proofs(SuperClassProofs, Info1, Info2),
	polymorphism__make_superclasses_from_proofs(SuperClasses, Subst,
		TypeSubst, ExistQVars, [], NewGoals, Info2, Info3,
		[], NewVars),

	poly_info_set_proofs(Proofs, Info3, Info).


:- pred polymorphism__make_superclasses_from_proofs(list(class_constraint), 
	substitution, tsubst, existq_tvars, list(hlds_goal), list(hlds_goal), 
	poly_info, poly_info, list(var), list(var)).
:- mode polymorphism__make_superclasses_from_proofs(in, in, in, in, in, out, 
	in, out, in, out) is det.

polymorphism__make_superclasses_from_proofs([], _, _, _,
		Goals, Goals, Info, Info, Vars, Vars).
polymorphism__make_superclasses_from_proofs([C|Cs], Subst, TypeSubst,
		ExistQVars, Goals0, Goals, Info0, Info, Vars0, Vars) :-
	polymorphism__make_superclasses_from_proofs(Cs, Subst, TypeSubst,
		ExistQVars, Goals0, Goals1, Info0, Info1, Vars0, Vars1),
	term__context_init(Context),
	polymorphism__make_typeclass_info_var(C, Subst, TypeSubst,
		ExistQVars, Context, no, _, Goals1, Goals, [], _, Info1, Info,
		MaybeVar),
	maybe_insert_var(MaybeVar, Vars1, Vars).

:- pred maybe_insert_var(maybe(var), list(var), list(var)).
:- mode maybe_insert_var(in, in, out) is det.
maybe_insert_var(no, Vars, Vars).
maybe_insert_var(yes(Var), Vars, [Var | Vars]).

%---------------------------------------------------------------------------%

% Given a list of types, create a list of variables to hold the type_info
% for those types, and create a list of goals to initialize those type_info
% variables to the appropriate type_info structures for the types.
% Update the varset and vartypes accordingly.

:- pred polymorphism__make_type_info_vars(list(type), existq_tvars,
	term__context, list(var), list(hlds_goal), poly_info, poly_info).
:- mode polymorphism__make_type_info_vars(in, in, in, out, out, in, out) is det.

polymorphism__make_type_info_vars([], _, _, [], [], Info, Info).
polymorphism__make_type_info_vars([Type | Types], ExistQVars, Context,
		ExtraVars, ExtraGoals, Info0, Info) :-
	polymorphism__make_type_info_var(Type, ExistQVars, Context,
		Var, ExtraGoals1, Info0, Info1),
	polymorphism__make_type_info_vars(Types, ExistQVars, Context,
		ExtraVars2, ExtraGoals2, Info1, Info),
	ExtraVars = [Var | ExtraVars2],
	list__append(ExtraGoals1, ExtraGoals2, ExtraGoals).

:- pred polymorphism__make_type_info_var(type, existq_tvars, term__context,
		var, list(hlds_goal), poly_info, poly_info).
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
	bool, existq_tvars, term__context, var, list(hlds_goal),
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

	polymorphism__init_const_base_type_info_var(Type,
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

:- pred polymorphism__maybe_init_second_cell(list(var), list(hlds_goal), type,
	bool, var, varset, map(var, type), list(hlds_goal),
	var, varset, map(var, type), list(hlds_goal)).
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

		% Since this base_type_info is pretending to be
		% a type_info, we need to adjust its type.
		% Since base_type_info_const cons_ids are handled
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
		% base_type_info
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

:- pred polymorphism__make_count_var(int, varset, map(var, type),
	var, hlds_goal, varset, map(var, type)).
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

:- pred polymorphism__init_with_int_constant(var, int, hlds_goal).
:- mode polymorphism__init_with_int_constant(in, in, out) is det.

polymorphism__init_with_int_constant(CountVar, Num, CountUnifyGoal) :-

	CountConsId = int_const(Num),
	CountUnification = construct(CountVar, CountConsId, [], []),

	CountTerm = functor(CountConsId, []),
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

:- pred polymorphism__get_special_proc(builtin_type, type, special_pred_id,
				module_info, sym_name, pred_id, proc_id).
:- mode polymorphism__get_special_proc(in, in, in, in, out, out, out) is det.

polymorphism__get_special_proc(TypeCategory, Type, SpecialPredId, ModuleInfo,
			PredName, PredId, ProcId) :-
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
		error("polymorphism__get_pred_id: pred_id lookup failed")
	).

	% Create a unification for a type_info or base_type_info variable:
	%
	%	TypeInfoVar = type_info(CountVar,
	%				SpecialPredVars...,
	%				ArgTypeInfoVars...)
	%
	% or
	%
	%	BaseTypeInfoVar = base_type_type_info(CountVar,
	%				SpecialPredVars...)
	%
	% These unifications WILL lead to the creation of cells on the
	% heap at runtime.

:- pred polymorphism__init_type_info_var(type, list(var), string,
	varset, map(var, type), var, hlds_goal, varset, map(var, type)).
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
	UniMode = (free - ground(shared, no) ->
		   ground(shared, no) - ground(shared, no)),
	list__length(ArgVars, NumArgVars),
	list__duplicate(NumArgVars, UniMode, UniModes),
	Unification = construct(TypeInfoVar, ConsId, ArgVars, UniModes),
	UnifyMode = (free -> ground(shared, no)) -
			(ground(shared, no) -> ground(shared, no)),
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

	% Create a unification for a type_info or base_type_info variable:
	%
	%	BaseTypeInfoVar = base_type_type_info(CountVar,
	%				SpecialPredVars...)
	%
	% This unification will NOT lead to the creation of a cell on the
	% heap at runtime; it will cause BaseTypeInfoVar to refer to the
	% statically allocated base_type_info cell for the type, allocated
	% in the module that defines the type.

:- pred polymorphism__init_const_base_type_info_var(type, type_id,
	module_info, varset, map(var, type), var, hlds_goal,
	varset, map(var, type)).
:- mode polymorphism__init_const_base_type_info_var(in, in, in, in, in,
	out, out, out, out) is det.

polymorphism__init_const_base_type_info_var(Type, TypeId,
		ModuleInfo, VarSet0, VarTypes0, BaseTypeInfoVar,
		BaseTypeInfoGoal, VarSet, VarTypes) :-

	type_util__type_id_module(ModuleInfo, TypeId, ModuleName),
	type_util__type_id_name(ModuleInfo, TypeId, TypeName),
	TypeId = _ - Arity,
	ConsId = base_type_info_const(ModuleName, TypeName, Arity),
	TypeInfoTerm = functor(ConsId, []),

	% introduce a new variable
	polymorphism__new_type_info_var(Type, "base_type_info",
		VarSet0, VarTypes0, BaseTypeInfoVar, VarSet, VarTypes),

	% create the construction unification to initialize the variable
	Unification = construct(BaseTypeInfoVar, ConsId, [], []),
	UnifyMode = (free -> ground(shared, no)) -
			(ground(shared, no) -> ground(shared, no)),
	UnifyContext = unify_context(explicit, []),
		% XXX the UnifyContext is wrong
	Unify = unify(BaseTypeInfoVar, TypeInfoTerm, UnifyMode,
			Unification, UnifyContext),

	% create a goal_info for the unification
	set__list_to_set([BaseTypeInfoVar], NonLocals),
	instmap_delta_from_assoc_list([BaseTypeInfoVar - ground(shared, no)],
		InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, det, GoalInfo),

	BaseTypeInfoGoal = Unify - GoalInfo.

%---------------------------------------------------------------------------%

:- pred polymorphism__make_head_vars(list(tvar), tvarset, list(var),
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


:- pred polymorphism__new_type_info_var(type, string, var,
					poly_info, poly_info).
:- mode polymorphism__new_type_info_var(in, in, out, in, out) is det.

polymorphism__new_type_info_var(Type, Symbol, Var, Info0, Info) :-
	poly_info_get_varset(Info0, VarSet0),
	poly_info_get_var_types(Info0, VarTypes0),
	polymorphism__new_type_info_var(Type, Symbol, VarSet0, VarTypes0,
					Var, VarSet, VarTypes),
	poly_info_set_varset_and_types(VarSet, VarTypes, Info0, Info).


:- pred polymorphism__new_type_info_var(type, string, varset, map(var, type),
					var, varset, map(var, type)).
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

:- pred extract_type_info(type, tvar, var, int, list(hlds_goal),
	var, poly_info, poly_info).
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

:- pred extract_type_info_2(type, tvar, var, int, module_info, list(hlds_goal),
	var, varset, map(var, type), map(tvar, type_info_locn),
	varset, map(var, type), map(tvar, type_info_locn)).
:- mode extract_type_info_2(in, in, in, in, in, out, out, in, in, in, out, out,
	out) is det.

extract_type_info_2(Type, _TypeVar, TypeClassInfoVar, Index, ModuleInfo, Goals,
		TypeInfoVar, VarSet0, VarTypes0, TypeInfoLocns0,
		VarSet, VarTypes, TypeInfoLocns) :-

		% We need a tvarset to pass to get_pred_id_and_proc_id
	varset__init(TVarSet0),
	varset__new_var(TVarSet0, TVar, TVarSet),

	mercury_private_builtin_module(PrivateBuiltin),
	ExtractTypeInfo = qualified(PrivateBuiltin,
				"type_info_from_typeclass_info"),
	construct_type(qualified(PrivateBuiltin, "typeclass_info") - 0, [],
		TypeClassInfoType),
	construct_type(unqualified("int") - 0, [], IntType),
	construct_type(qualified(PrivateBuiltin, "type_info") - 1,
		[term__variable(TVar)], TypeInfoType),
	get_pred_id_and_proc_id(ExtractTypeInfo, predicate, TVarSet, 
		[TypeClassInfoType, IntType, TypeInfoType],
		ModuleInfo, PredId, ProcId),
	polymorphism__make_count_var(Index, VarSet0, VarTypes0, IndexVar,
		IndexGoal, VarSet1, VarTypes1),

	polymorphism__new_type_info_var(Type, "type_info", VarSet1, VarTypes1,
		TypeInfoVar, VarSet2, VarTypes2),

		% We have to put an extra type_info at the front of the call to
		% type_info_from_typeclass_info, and pass it a bogus value
		% because the pred has a type parameter... even though we are
		% actually _extracting_ the type_info.  Existential typing of
		% type_info_from_typeclass_info would fix this.
	polymorphism__new_type_info_var(Type, "type_info", VarSet2, VarTypes2,
		DummyTypeInfoVar, VarSet, VarTypes),

		% Now we put a dummy value in the dummy type-info variable.
	polymorphism__init_with_int_constant(DummyTypeInfoVar, 0,
		DummyTypeInfoGoal),

		% Make the goal info for the call
	set__list_to_set([DummyTypeInfoVar, TypeClassInfoVar, IndexVar,
		TypeInfoVar], NonLocals),
	instmap_delta_from_assoc_list([TypeInfoVar - ground(shared, no)],
		InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, det, GoalInfo),

	Call = call(PredId, ProcId, 
		[DummyTypeInfoVar, TypeClassInfoVar, IndexVar, TypeInfoVar],
		not_builtin, no, ExtractTypeInfo) - GoalInfo,

	Goals = [IndexGoal, DummyTypeInfoGoal, Call],

		% Update the location of the type_info so that we don't go to
		% the bother of re-extracting it.
	map__det_update(TypeInfoLocns0, TVar, type_info(TypeInfoVar),
		TypeInfoLocns).

%-----------------------------------------------------------------------------%

	% Create a head var for each class constraint, and make an entry in
	% the typeinfo locations map for each constrained type var.

:- pred polymorphism__make_typeclass_info_head_vars(list(class_constraint),
		list(var), poly_info, poly_info).
:- mode polymorphism__make_typeclass_info_head_vars(in, out, in, out)
		is det.

polymorphism__make_typeclass_info_head_vars(Constraints, ExtraHeadVars) -->
	{ ExtraHeadVars0 = [] },
	polymorphism__make_typeclass_info_head_vars_2(Constraints,
		ExtraHeadVars0, ExtraHeadVars1),
	{ list__reverse(ExtraHeadVars1, ExtraHeadVars) }.

:- pred polymorphism__make_typeclass_info_head_vars_2(list(class_constraint),
		list(var), list(var), poly_info, poly_info).
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
	polymorphism__new_typeclass_info_var(VarSet0, VarTypes0, ClassName,
		Var, VarSet1, VarTypes1),
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

:- pred polymorphism__new_typeclass_info_var(varset, map(var, type), 
		string, var, 
		varset, map(var, type)).
:- mode polymorphism__new_typeclass_info_var(in, in, in, out, out, out) is det.

polymorphism__new_typeclass_info_var(VarSet0, VarTypes0, ClassName, 
		Var, VarSet, VarTypes) :-
	% introduce new variable
	varset__new_var(VarSet0, Var, VarSet1),
	string__append("TypeClassInfo_for_", ClassName, Name),
	varset__name_var(VarSet1, Var, Name, VarSet),

	mercury_private_builtin_module(PrivateBuiltin),
	construct_type(qualified(PrivateBuiltin, "typeclass_info") - 0,
					[], DictionaryType),
	map__set(VarTypes0, Var, DictionaryType, VarTypes).

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
	proc_info_argmodes(ProcInfo0, Modes0),
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
		HeadVars, Types, Modes, Detism),

		% Make the goal info for the call. 
	set__list_to_set(HeadVars0, NonLocals),
	instmap_delta_from_mode_list(HeadVars0, Modes0, ModuleInfo0,
			InstmapDelta),
	goal_info_init(NonLocals, InstmapDelta, Detism, GoalInfo),
	BodyGoal = BodyGoalExpr - GoalInfo,

	proc_info_set_goal(ProcInfo0, BodyGoal, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo),

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
			varset,			% from the proc_info
			map(var, type),		% from the proc_info
			tvarset,		% from the proc_info
			map(tvar, type_info_locn),		
						% specifies the location of
						% the type_info var
						% for each of the pred's type
						% parameters

			map(class_constraint, var),		
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
			module_info
		).

:- pred init_poly_info(module_info, pred_info, proc_info, poly_info).
:- mode init_poly_info(in, in, in, out) is det.

init_poly_info(ModuleInfo, PredInfo, ProcInfo, PolyInfo) :-
	pred_info_name(PredInfo, PredName),
	pred_info_typevarset(PredInfo, TypeVarSet),
	pred_info_get_constraint_proofs(PredInfo, Proofs),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	map__init(TypeInfoMap),
	map__init(TypeClassInfoMap),
	PolyInfo = poly_info(VarSet, VarTypes, TypeVarSet,
				TypeInfoMap, TypeClassInfoMap,
				Proofs, PredName, ModuleInfo).

:- pred poly_info_get_varset(poly_info, varset).
:- mode poly_info_get_varset(in, out) is det.

poly_info_get_varset(PolyInfo, VarSet) :-
	PolyInfo = poly_info(VarSet, _, _, _, _, _, _, _).

:- pred poly_info_get_var_types(poly_info, map(var, type)).
:- mode poly_info_get_var_types(in, out) is det.

poly_info_get_var_types(PolyInfo, VarTypes) :-
	PolyInfo = poly_info(_, VarTypes, _, _, _, _, _, _).

:- pred poly_info_get_typevarset(poly_info, tvarset).
:- mode poly_info_get_typevarset(in, out) is det.

poly_info_get_typevarset(PolyInfo, TypeVarSet) :-
	PolyInfo = poly_info(_, _, TypeVarSet, _, _, _, _, _).

:- pred poly_info_get_type_info_map(poly_info, map(tvar, type_info_locn)).
:- mode poly_info_get_type_info_map(in, out) is det.

poly_info_get_type_info_map(PolyInfo, TypeInfoMap) :-
	PolyInfo = poly_info(_, _, _, TypeInfoMap, _, _, _, _).

:- pred poly_info_get_typeclass_info_map(poly_info,
					map(class_constraint, var)).
:- mode poly_info_get_typeclass_info_map(in, out) is det.

poly_info_get_typeclass_info_map(PolyInfo, TypeClassInfoMap) :-
	PolyInfo = poly_info(_, _, _, _, TypeClassInfoMap, _, _, _).

:- pred poly_info_get_proofs(poly_info,
				map(class_constraint, constraint_proof)).
:- mode poly_info_get_proofs(in, out) is det.

poly_info_get_proofs(PolyInfo, Proofs) :-
	PolyInfo = poly_info(_, _, _, _, _, Proofs, _, _).

:- pred poly_info_get_pred_name(poly_info, string).
:- mode poly_info_get_pred_name(in, out) is det.

poly_info_get_pred_name(PolyInfo, PredName) :-
	PolyInfo = poly_info(_, _, _, _, _, _, PredName, _).

:- pred poly_info_get_module_info(poly_info, module_info).
:- mode poly_info_get_module_info(in, out) is det.

poly_info_get_module_info(PolyInfo, ModuleInfo) :-
	PolyInfo = poly_info(_, _, _, _, _, _, _, ModuleInfo).


:- pred poly_info_set_varset(varset, poly_info, poly_info).
:- mode poly_info_set_varset(in, in, out) is det.

poly_info_set_varset(VarSet, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(_, B, C, D, E, F, G, H),
	PolyInfo = poly_info(VarSet, B, C, D, E, F, G, H).

:- pred poly_info_set_varset_and_types(varset, map(var, type),
					poly_info, poly_info).
:- mode poly_info_set_varset_and_types(in, in, in, out) is det.

poly_info_set_varset_and_types(VarSet, VarTypes, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(_, _, C, D, E, F, G, H),
	PolyInfo = poly_info(VarSet, VarTypes, C, D, E, F, G, H).

:- pred poly_info_set_typevarset(tvarset, poly_info, poly_info).
:- mode poly_info_set_typevarset(in, in, out) is det.

poly_info_set_typevarset(TypeVarSet, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, _, D, E, F, G, H),
	PolyInfo = poly_info(A, B, TypeVarSet, D, E, F, G, H).

:- pred poly_info_set_type_info_map(map(tvar, type_info_locn),
					poly_info, poly_info).
:- mode poly_info_set_type_info_map(in, in, out) is det.

poly_info_set_type_info_map(TypeInfoMap, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, _, E, F, G, H),
	PolyInfo = poly_info(A, B, C, TypeInfoMap, E, F, G, H).

:- pred poly_info_set_typeclass_info_map(map(class_constraint, var),
					poly_info, poly_info).
:- mode poly_info_set_typeclass_info_map(in, in, out) is det.

poly_info_set_typeclass_info_map(TypeClassInfoMap, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, D, _, F, G, H),
	PolyInfo = poly_info(A, B, C, D, TypeClassInfoMap, F, G, H).


:- pred poly_info_set_proofs(map(class_constraint, constraint_proof),
				poly_info, poly_info).
:- mode poly_info_set_proofs(in, in, out) is det.

poly_info_set_proofs(Proofs, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, D, E, _, G, H),
	PolyInfo = poly_info(A, B, C, D, E, Proofs, G, H).

:- pred poly_info_set_module_info(module_info, poly_info, poly_info).
:- mode poly_info_set_module_info(in, in, out) is det.

poly_info_set_module_info(ModuleInfo, PolyInfo0, PolyInfo) :-
	PolyInfo0 = poly_info(A, B, C, D, E, F, G, _),
	PolyInfo = poly_info(A, B, C, D, E, F, G, ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
