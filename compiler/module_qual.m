%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
:- module module_qual.
%	Main author: stayl
%
%	Based on undef_types and undef_modes by fjh.
%	Module qualifies types, insts and modes within declaration items.
%	The head of all declarations should be module qualified in prog_io.m.
%	This module qualifies the bodies of the declarations.
%	Checks for undefined types, insts and modes.
%	Uses two passes over the item list, one to collect all type, mode
%	and inst ids and a second to do the qualification and report errors.
%	If the --warn-interface-imports option is set, warns about modules
%	imported in the interface that do not need to be in the interface.
%	The modes of lambda expressions are qualified in modes.m.
%
:- interface.

:- import_module prog_data.
:- import_module bool, io.

	% module_qualify_items(Items0, Items, ReportUndefErrors,
	%				NumErrors, UndefTypes, UndefModes).
	%
	% Items is Items0 with all items module qualified as much
	% as possible. If ReportUndefErrors is yes, then
	% report undefined types, insts and modes. 
	% ReportUndefErrors should be no when module qualifying the
	% short interface.
:- pred module_qual__module_qualify_items(item_list, item_list,
		string, bool, mq_info, int, bool, bool, io__state, io__state).
:- mode module_qual__module_qualify_items(in, out, in, in,
		out, out, out, out, di, uo) is det.

	% This is called from make_hlds.m to qualify the mode of a lambda
	% expression.
:- pred module_qual__qualify_lambda_mode_list(list(mode), list(mode),
		term__context, mq_info, mq_info,
		io__state, io__state) is det.
:- mode module_qual__qualify_lambda_mode_list(in, out, 
		in, in, out, di, uo) is det.

	% This is called from make_hlds.m to qualify an 
	% explicit type qualification.
:- pred module_qual__qualify_type_qualification(type, type, term__context,
		mq_info, mq_info, io__state, io__state).
:- mode module_qual__qualify_type_qualification(in, out, in, in,
		out, di, uo) is det.

:- type mq_info.

:- pred mq_info_get_num_errors(mq_info::in, int::out) is det.
:- pred mq_info_get_type_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_mode_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_set_need_qual_flag(mq_info::in, 
		need_qualifier::in, mq_info::out) is det.
:- pred mq_info_get_need_qual_flag(mq_info::in, need_qualifier::out) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module hlds_data, hlds_module, hlds_pred, type_util, prog_out.
:- import_module prog_util, mercury_to_mercury, globals, options.
:- import_module (inst), instmap.
:- import_module int, list, map, require, set, std_util, string, term, varset.

module_qual__module_qualify_items(Items0, Items, ModuleName, ReportErrors,
			Info, NumErrors, UndefTypes, UndefModes) -->
	{ init_mq_info(ReportErrors, Info0) },
	{ collect_mq_info(Items0, Info0, Info1) },
	do_module_qualify_items(Items0, Items, Info1, Info),
	{ mq_info_get_type_error_flag(Info, UndefTypes) },
	{ mq_info_get_mode_error_flag(Info, UndefModes) },
	( { ReportErrors = yes } ->
		{ mq_info_get_interface_modules(Info, UnusedImports0) },
		{ set__to_sorted_list(UnusedImports0, UnusedImports) },
		maybe_warn_unused_interface_imports(ModuleName, UnusedImports)
	;
		[]
	),
	{ mq_info_get_num_errors(Info, NumErrors) }.

module_qual__qualify_lambda_mode_list(Modes0, Modes, Context, Info0, Info) -->
	{ mq_info_set_error_context(Info0, lambda_expr - Context, Info1) },
	qualify_mode_list(Modes0, Modes, Info1, Info).

module_qual__qualify_type_qualification(Type0, Type, Context, Info0, Info) -->
	{ mq_info_set_error_context(Info0, type_qual - Context, Info1) },
	qualify_type(Type0, Type, Info1, Info).

:- type mq_info
	--->	mq_info(
			type_id_set,	% Sets of all types, modes and
			inst_id_set,	% insts visible in this module.
			mode_id_set,
			set(module_name), % modules imported in the
				% interface that are not definitely
				% needed in the interface.
			import_status, % import status of the current item.
			int,	% number of errors found.
			bool,	% are there any undefined types.
			bool,	% are there any undefined insts or modes.
			bool, 	% do we want to report errors.
			error_context,	% context of the current item.
			need_qualifier	% must uses of the current item be 
				% explicitly module qualified.
	).
		
	% Pass over the item list collecting all defined type, mode and
	% inst ids and the names of all modules imported in the interface.
:- pred collect_mq_info(item_list::in, mq_info::in, mq_info::out) is det.

collect_mq_info([], Info, Info).
collect_mq_info([Item - _ | Items], Info0, Info) :-
	collect_mq_info_2(Item, Info0, Info1),
	collect_mq_info(Items, Info1, Info).

:- pred collect_mq_info_2(item::in, mq_info::in, mq_info::out) is det.

collect_mq_info_2(pred_clause(_,_,_,_), Info, Info).
collect_mq_info_2(func_clause(_,_,_,_,_), Info, Info).
collect_mq_info_2(type_defn(_, TypeDefn, _), Info0, Info) :-
	add_type_defn(TypeDefn, Info0, Info).
collect_mq_info_2(inst_defn(_, InstDefn, _), Info0, Info) :-
	add_inst_defn(InstDefn, Info0, Info).
collect_mq_info_2(mode_defn(_, ModeDefn, _), Info0, Info) :-
	add_mode_defn(ModeDefn, Info0, Info).
collect_mq_info_2(module_defn(_, ModuleDefn), Info0, Info) :-
	process_module_defn(ModuleDefn, Info0, Info).
collect_mq_info_2(pred(_,_,_,_,_), Info, Info).
collect_mq_info_2(func(_,_,_,_,_,_), Info, Info).
collect_mq_info_2(pred_mode(_,_,_,_,_), Info, Info).
collect_mq_info_2(func_mode(_,_,_,_,_,_), Info, Info).
collect_mq_info_2(pragma(_), Info, Info).
collect_mq_info_2(nothing, Info, Info).

% Predicates to add the type, inst and mode ids visible
% in this module to the mq_info.

:- pred add_type_defn(type_defn::in, mq_info::in, mq_info::out) is det.

add_type_defn(TypeDefn, Info0, Info) :-
	(	TypeDefn = du_type(SymName, Params, _, _EqualityPred)
	;	TypeDefn = uu_type(SymName, Params, _)
	;	TypeDefn = eqv_type(SymName, Params, _)
	;	TypeDefn = abstract_type(SymName, Params)
	),
	list__length(Params, Arity),
	mq_info_get_types(Info0, Types0),
	mq_info_get_need_qual_flag(Info0, NeedQualifier),
	id_set_insert(NeedQualifier, SymName - Arity, Types0, Types),
	mq_info_set_types(Info0, Types, Info).

:- pred add_inst_defn(inst_defn::in, mq_info::in, mq_info::out) is det.

add_inst_defn(InstDefn, Info0, Info) :-
	(	InstDefn = eqv_inst(SymName, Params, _)
	;	InstDefn = abstract_inst(SymName, Params)
	),
	list__length(Params, Arity),
	mq_info_get_insts(Info0, Insts0),
	mq_info_get_need_qual_flag(Info0, NeedQualifier),
	id_set_insert(NeedQualifier, SymName - Arity, Insts0, Insts),
	mq_info_set_insts(Info0, Insts, Info).

:- pred add_mode_defn(mode_defn::in, mq_info::in, mq_info::out) is det.

add_mode_defn(eqv_mode(SymName, Params, _), Info0, Info) :-
	list__length(Params, Arity),
	mq_info_get_modes(Info0, Modes0),
	mq_info_get_need_qual_flag(Info0, NeedQualifier),
	id_set_insert(NeedQualifier, SymName - Arity, Modes0, Modes),
	mq_info_set_modes(Info0, Modes, Info).

	% Update import status.
	% Add imported modules if in the interface.
:- pred process_module_defn(module_defn::in, mq_info::in, mq_info::out) is det.

process_module_defn(module(_), Info, Info).
process_module_defn(interface, Info0, Info) :-
	mq_info_set_import_status(Info0, exported, Info).
process_module_defn(implementation, Info0, Info) :-
	mq_info_set_import_status(Info0, local, Info).
process_module_defn(imported, Info0, Info) :-
	mq_info_set_import_status(Info0, imported, Info1),
	mq_info_set_need_qual_flag(Info1, may_be_unqualified, Info).
process_module_defn(used, Info0, Info) :-
	mq_info_set_import_status(Info0, imported, Info1),
	mq_info_set_need_qual_flag(Info1, must_be_qualified, Info).
process_module_defn(opt_imported, Info0, Info) :-
	mq_info_set_import_status(Info0, opt_imported, Info1),
	mq_info_set_need_qual_flag(Info1, must_be_qualified, Info).
process_module_defn(external(_), Info, Info).
process_module_defn(end_module(_), Info, Info).
process_module_defn(export(_), Info, Info).
process_module_defn(import(Imports), Info0, Info) :-
	add_interface_imports(Imports, Info0, Info).
process_module_defn(use(Imports), Info0, Info) :-
	add_interface_imports(Imports, Info0, Info).

:- pred add_interface_imports(sym_list::in,
		mq_info::in, mq_info::out) is det.

add_interface_imports(Imports, Info0, Info) :-
	( Imports = module(ImportedModules) ->
		mq_info_add_interface_modules(Info0, ImportedModules, Info)
	;
		Info = Info0
	).

%------------------------------------------------------------------------------

	% Iterate over the item list module qualifying all declarations.
	% Stop when the :- imported or :- opt_imported pseudo-declaration
	% is reached, since imported declarations should already be
	% module qualified.
:- pred do_module_qualify_items(item_list::in, item_list::out,
		mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

do_module_qualify_items([], [], Info, Info) --> [].
do_module_qualify_items([Item0 | Items0], [Item | Items], Info0, Info) -->
	module_qualify_item(Item0, Item, Info0, Info1, Continue),
	( { Continue = yes } ->
		do_module_qualify_items(Items0, Items, Info1, Info)
	;
		{ Items = Items0 },
		{ Info = Info1 }
	).

	% Call predicates to qualify a single item.
:- pred module_qualify_item(item_and_context::in, item_and_context::out,
		mq_info::in, mq_info::out, bool::out,
		io__state::di, io__state::uo) is det.

module_qualify_item(pred_clause(A,B,C,D) - Con, pred_clause(A,B,C,D) - Con,
			Info, Info, yes) --> [].

module_qualify_item(func_clause(A,B,C,D,E) - Con, func_clause(A,B,C,D,E) - Con,
			Info, Info, yes) --> [].

module_qualify_item(type_defn(A, TypeDefn0, C) - Context,
		type_defn(A, TypeDefn, C) - Context, Info0, Info, yes) --> 
	qualify_type_defn(TypeDefn0, TypeDefn, Info0, Info, Context).  

module_qualify_item(inst_defn(A, InstDefn0, C) - Context,
		inst_defn(A, InstDefn, C) - Context, Info0, Info, yes) --> 
	qualify_inst_defn(InstDefn0, InstDefn, Info0, Info, Context).

module_qualify_item(mode_defn(A, ModeDefn0, C) - Context,
		mode_defn(A, ModeDefn, C) - Context, Info0, Info, yes) -->
	qualify_mode_defn(ModeDefn0, ModeDefn, Info0, Info, Context).

module_qualify_item(module_defn(A, ModuleDefn) - Context,
		module_defn(A, ModuleDefn) - Context, Info0, Info, Continue) -->
	{ update_import_status(ModuleDefn, Info0, Info, Continue) }.

module_qualify_item(pred(A, SymName, TypesAndModes0, D, E) - Context,
		pred(A, SymName, TypesAndModes, D, E) - Context,
		Info0, Info, yes) -->
	{ list__length(TypesAndModes0, Arity) },
	{ mq_info_set_error_context(Info0, pred(SymName - Arity) - Context,
								Info1) },
	qualify_types_and_modes(TypesAndModes0, TypesAndModes, Info1, Info).

module_qualify_item(func(A,SymName, TypesAndModes0, TypeAndMode0,D,E) - Context,
		func(A, SymName, TypesAndModes, TypeAndMode, D, E) - Context,
		Info0, Info, yes) -->
	{ list__length(TypesAndModes0, Arity) },
	{ mq_info_set_error_context(Info0, func(SymName - Arity) - Context,
								Info1) },
	qualify_types_and_modes(TypesAndModes0, TypesAndModes, Info1, Info2),
	qualify_type_and_mode(TypeAndMode0, TypeAndMode, Info2, Info).

module_qualify_item(pred_mode(A, SymName, Modes0, C, D) - Context,
		 	pred_mode(A, SymName, Modes, C, D) - Context,
			Info0, Info, yes) -->
	{ list__length(Modes0, Arity) },
	{ mq_info_set_error_context(Info0, pred_mode(SymName - Arity) - Context,
								 Info1) },
	qualify_mode_list(Modes0, Modes, Info1, Info).

module_qualify_item(func_mode(A, SymName, Modes0, Mode0, C, D) - Context, 
		func_mode(A, SymName, Modes, Mode, C, D) - Context,
		Info0, Info, yes) -->
	{ list__length(Modes0, Arity) },
	{ mq_info_set_error_context(Info0, func_mode(SymName - Arity) - Context,
								 Info1) },
	qualify_mode_list(Modes0, Modes, Info1, Info2),
	qualify_mode(Mode0, Mode, Info2, Info).

module_qualify_item(pragma(Pragma0) - Context, pragma(Pragma) - Context,
						Info0, Info, yes) -->
	{ mq_info_set_error_context(Info0, (pragma) - Context, Info1) },
	qualify_pragma(Pragma0, Pragma, Info1, Info).
module_qualify_item(nothing - Context, nothing - Context,
						Info, Info, yes) --> [].

:- pred update_import_status(module_defn::in, mq_info::in, mq_info::out,
							bool::out) is det.

update_import_status(opt_imported, Info, Info, no).
update_import_status(module(_), Info, Info, yes).
update_import_status(interface, Info0, Info, yes) :-
	mq_info_set_import_status(Info0, exported, Info).
update_import_status(implementation, Info0, Info, yes) :-
	mq_info_set_import_status(Info0, local, Info).
update_import_status(imported, Info, Info, no).
update_import_status(used, Info, Info, no).
update_import_status(external(_), Info, Info, yes).
update_import_status(end_module(_), Info, Info, yes).
update_import_status(export(_), Info, Info, yes).
update_import_status(import(_), Info, Info, yes).
update_import_status(use(_), Info, Info, yes).

	% Qualify the constructors or other types in a type definition.	
:- pred qualify_type_defn(type_defn::in, type_defn::out, mq_info::in,
	mq_info::out, term__context::in, io__state::di, io__state::uo) is det.

qualify_type_defn(du_type(SymName, Params, Ctors0, MaybeEqualityPred0),
		du_type(SymName, Params, Ctors, MaybeEqualityPred),
		Info0, Info, Context) -->
	{ list__length(Params, Arity) },
	{ mq_info_set_error_context(Info0, type(SymName - Arity) - Context,
								Info1) },
	qualify_constructors(Ctors0, Ctors, Info1, Info),

	% User-defined equality pred names will be converted into
	% predicate calls and then module-qualified after type analysis
	% (during mode analysis).  That way they get full type overloading
	% resolution, etc.  Thus we don't module-qualify them here.
	{ MaybeEqualityPred = MaybeEqualityPred0 }.

qualify_type_defn(uu_type(SymName, Params, Types0),
		uu_type(SymName, Params, Types), Info0, Info, Context) -->
	{ list__length(Params, Arity) },
	{ mq_info_set_error_context(Info0, type(SymName - Arity) - Context,
								Info1) },
	qualify_type_list(Types0, Types, Info1, Info).	

qualify_type_defn(eqv_type(SymName, Params, Type0),
		eqv_type(SymName, Params, Type),
		Info0, Info, Context) -->
	{ list__length(Params, Arity) },
	{ mq_info_set_error_context(Info0, type(SymName - Arity) - Context,
								Info1) },
	qualify_type(Type0, Type, Info1, Info).	

qualify_type_defn(abstract_type(SymName, Params),
		abstract_type(SymName, Params), Info, Info, _) --> [].

:- pred qualify_constructors(list(constructor)::in, list(constructor)::out,
		mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.
				
qualify_constructors([], [], Info, Info) --> [].
qualify_constructors([SymName - Args0 | Ctors0], [SymName - Args | Ctors],
					Info0, Info) -->
	qualify_constructor_arg_list(Args0, Args, Info0, Info1),
	qualify_constructors(Ctors0, Ctors, Info1, Info).

	% Qualify the inst parameters of an inst definition.
:- pred qualify_inst_defn(inst_defn::in, inst_defn::out, mq_info::in,
	mq_info::out, term__context::in, io__state::di, io__state::uo) is det.

qualify_inst_defn(eqv_inst(SymName, Params, Inst0),
		eqv_inst(SymName, Params, Inst), Info0, Info, Context) -->
	{ list__length(Params, Arity) },
	{ mq_info_set_error_context(Info0, inst(SymName - Arity) - Context,
								Info1) },
	qualify_inst(Inst0, Inst, Info1, Info).	

qualify_inst_defn(abstract_inst(SymName, Params),
	 abstract_inst(SymName, Params), Info, Info, _) --> [].

	% Qualify the mode parameter of an equivalence mode definition.
:- pred qualify_mode_defn(mode_defn::in, mode_defn::out, mq_info::in,
	mq_info::out, term__context::in, io__state::di, io__state::uo) is det.

qualify_mode_defn(eqv_mode(SymName, Params, Mode0),
		eqv_mode(SymName, Params, Mode), Info0, Info, Context) -->
	{ list__length(Params, Arity) },
	{ mq_info_set_error_context(Info0, mode(SymName - Arity) - Context,
								Info1) },
	qualify_mode(Mode0, Mode, Info1, Info).	

	% Qualify a list of items of the form Type::Mode, as in a
	% predicate declaration.
:- pred qualify_types_and_modes(list(type_and_mode)::in,
		list(type_and_mode)::out, mq_info::in, mq_info::out,
		io__state::di, io__state::uo) is det.

qualify_types_and_modes([], [], Info, Info) --> [].
qualify_types_and_modes([TypeAndMode0 | TypesAndModes0],
		[TypeAndMode | TypesAndModes], Info0, Info) -->
	qualify_type_and_mode(TypeAndMode0, TypeAndMode, Info0, Info1),
	qualify_types_and_modes(TypesAndModes0, TypesAndModes, Info1, Info).

:- pred qualify_type_and_mode(type_and_mode::in, type_and_mode::out,
	mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

qualify_type_and_mode(type_only(Type0), type_only(Type), Info0, Info) -->
	qualify_type(Type0, Type, Info0, Info).

qualify_type_and_mode(type_and_mode(Type0, Mode0), type_and_mode(Type, Mode),
			Info0, Info) -->
	qualify_type(Type0, Type, Info0, Info1),
	qualify_mode(Mode0, Mode, Info1, Info).

:- pred qualify_mode_list(list(mode)::in, list(mode)::out, mq_info::in,
		mq_info::out, io__state::di, io__state::uo) is det.

qualify_mode_list([], [], Info, Info) --> [].
qualify_mode_list([Mode0 | Modes0], [Mode | Modes], Info0, Info) -->
	qualify_mode(Mode0, Mode, Info0, Info1),
	qualify_mode_list(Modes0, Modes, Info1, Info).

:- pred qualify_mode((mode)::in, (mode)::out, mq_info::in, mq_info::out,
			io__state::di, io__state::uo) is det.

qualify_mode((Inst0a -> Inst1a), (Inst0 -> Inst1), Info0, Info) -->
	qualify_inst(Inst0a, Inst0, Info0, Info1),
	qualify_inst(Inst1a, Inst1, Info1, Info).	

qualify_mode(user_defined_mode(SymName0, Insts0),
		user_defined_mode(SymName, Insts), Info0, Info) -->
	qualify_inst_list(Insts0, Insts, Info0, Info1),
	{ list__length(Insts, Arity) },
	{ mq_info_get_modes(Info1, Modes) },
	find_unique_match(SymName0 - Arity, SymName - _, Modes,
						mode_id, Info1, Info).
	
:- pred qualify_inst_list(list(inst)::in, list(inst)::out, mq_info::in,
		mq_info::out, io__state::di, io__state::uo) is det.

qualify_inst_list([], [], Info, Info) --> [].
qualify_inst_list([Inst0 | Insts0], [Inst | Insts], Info0, Info) -->
	qualify_inst(Inst0, Inst, Info0, Info1),
	qualify_inst_list(Insts0, Insts, Info1, Info).

	% Qualify a single inst.
:- pred qualify_inst((inst)::in, (inst)::out, mq_info::in, mq_info::out,
			io__state::di, io__state::uo) is det.

qualify_inst(any(A), any(A), Info, Info) --> [].
qualify_inst(alias(V), alias(V), Info, Info) -->
	{ error("qualify_inst: alias") }.
qualify_inst(free, free, Info, Info) --> [].
qualify_inst(not_reached, not_reached, Info, Info) --> [].
qualify_inst(free(_), _, _, _) -->
	{ error("compiler generated inst not expected") }.
qualify_inst(bound(Uniq, BoundInsts0), bound(Uniq, BoundInsts),
				Info0, Info) -->
	qualify_bound_inst_list(BoundInsts0, BoundInsts, Info0, Info).
qualify_inst(ground(Uniq, MaybePredInstInfo0), ground(Uniq, MaybePredInstInfo),
				Info0, Info) -->
	(
		{ MaybePredInstInfo0 = yes(pred_inst_info(A, Modes0, Det)) },
		% XXX This code is not correct if the pred inst has
		%     aliasing in the argument_modes.
		{ Modes0 = argument_modes(ArgIKT, ArgModes0) },
		qualify_mode_list(ArgModes0, ArgModes, Info0, Info),
		{ Modes = argument_modes(ArgIKT, ArgModes) },
		{ MaybePredInstInfo = yes(pred_inst_info(A, Modes, Det)) }
	;
		{ MaybePredInstInfo0 = no },
		{ MaybePredInstInfo = no },
		{ Info = Info0 }
	).
qualify_inst(inst_var(Var), inst_var(Var), Info, Info) --> [].
qualify_inst(defined_inst(InstName0), defined_inst(InstName), Info0, Info) -->
	qualify_inst_name(InstName0, InstName, Info0, Info).
qualify_inst(abstract_inst(Name, Args0), abstract_inst(Name, Args),
				Info0, Info) -->
	qualify_inst_list(Args0, Args, Info0, Info).

	% Find the unique inst_id that matches this inst, and qualify
	% the argument insts.
:- pred qualify_inst_name(inst_name::in, inst_name::out, mq_info::in,
		mq_info::out, io__state::di, io__state::uo) is det.

qualify_inst_name(user_inst(SymName0, Insts0), user_inst(SymName, Insts),
				Info0, Info) -->
	qualify_inst_list(Insts0, Insts, Info0, Info1),
	{ mq_info_get_insts(Info1, InstIds) },
	{ list__length(Insts0, Arity) },
	find_unique_match(SymName0 - Arity, SymName - _,
				InstIds, inst_id, Info1, Info).
qualify_inst_name(merge_inst(_, _), _, _, _) -->
	{ error("compiler generated inst unexpected") }.
qualify_inst_name(unify_inst(_, _, _, _), _, _, _) -->
	{ error("compiler generated inst unexpected") }.
qualify_inst_name(ground_inst(_, _, _, _), _, _, _) -->
	{ error("compiler generated inst unexpected") }.
qualify_inst_name(any_inst(_, _, _, _), _, _, _) -->
	{ error("compiler generated inst unexpected") }.
qualify_inst_name(shared_inst(_), _, _, _) -->
	{ error("compiler generated inst unexpected") }.
qualify_inst_name(mostly_uniq_inst(_), _, _, _) -->
	{ error("compiler generated inst unexpected") }.
qualify_inst_name(typed_ground(_, _), _, _, _) -->
	{ error("compiler generated inst unexpected") }.
qualify_inst_name(typed_inst(_, _), _, _, _) -->
	{ error("compiler generated inst unexpected") }.

	% Qualify an inst of the form bound(functor(...)).
:- pred qualify_bound_inst_list(list(bound_inst)::in, list(bound_inst)::out,
	mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

qualify_bound_inst_list([], [], Info, Info) --> [].
qualify_bound_inst_list([functor(ConsId, Insts0) | BoundInsts0],
		 [functor(ConsId, Insts) | BoundInsts], Info0, Info) -->
	qualify_inst_list(Insts0, Insts, Info0, Info1),
	qualify_bound_inst_list(BoundInsts0, BoundInsts, Info1, Info).

:- pred qualify_constructor_arg_list(list(constructor_arg)::in,
	list(constructor_arg)::out, mq_info::in, mq_info::out,
	io__state::di, io__state::uo) is det.

qualify_constructor_arg_list([], [], Info, Info) --> [].
qualify_constructor_arg_list([Name - Type0 | Args0], [Name - Type | Args],
		Info0, Info) -->
	qualify_type(Type0, Type, Info0, Info1),
	qualify_constructor_arg_list(Args0, Args, Info1, Info).

:- pred qualify_type_list(list(type)::in, list(type)::out, mq_info::in,
			mq_info::out, io__state::di, io__state::uo) is det.

qualify_type_list([], [], Info, Info) --> [].
qualify_type_list([Type0 | Types0], [Type | Types], Info0, Info) -->
	qualify_type(Type0, Type, Info0, Info1),
	qualify_type_list(Types0, Types, Info1, Info).

	% Qualify a type and its argument types.
:- pred qualify_type((type)::in, (type)::out, mq_info::in, mq_info::out,
				io__state::di, io__state::uo) is det.

qualify_type(term__variable(Var), term__variable(Var), Info, Info) --> [].
qualify_type(Type0, Type, Info0, Info) -->
	{ Type0 = term__functor(F, As, _) },
	( { is_builtin_func_type(F, As, ArgTypes0, RetType0) } ->
		qualify_type_list(ArgTypes0, ArgTypes, Info0, Info1),
		qualify_type(RetType0, RetType, Info1, Info2),
		{ term__context_init(Context) },
		{ Type = term__functor(term__atom("="),
				[term__functor(term__atom("func"),
				ArgTypes, Context), RetType], Context) }
	; { type_to_type_id(Type0, TypeId0, Args0) } ->
		( { is_builtin_atomic_type(TypeId0) } ->
			{ TypeId = TypeId0 },
			{ Info1 = Info0 }
		; { is_builtin_pred_type(TypeId0) } ->
			{ TypeId = TypeId0 },
			{ Info1 = Info0 }
		;
			{ mq_info_get_types(Info0, Types) },
			find_unique_match(TypeId0, TypeId, Types,
						type_id, Info0, Info1)
		),
		qualify_type_list(Args0, Args, Info1, Info2),
		{ TypeId = SymName - _ },
		{ construct_qualified_term(SymName, Args, Type) }	
	;
		{ mq_info_get_error_context(Info0, ErrorContext) },
		report_invalid_type(Type0, ErrorContext),
		{ Type = Type0 },
		{ Info2 = Info0 }
	),
	%
	% The types `int', `float', and `string' are builtin types,
	% defined by the compiler, but arguably they ought to be
	% defined in int.m, float.m, and string.m, and so if someone
	% uses the type `int' in the interface, then we don't want
	% to warn about `import_module int' in the interface.
	%
	{
		Type = term__functor(term__atom(Typename), [], _),
		( Typename = "int"
		; Typename = "string"
		; Typename = "float"
		)
	->
		mq_info_set_module_used(Info2, Typename, Info)
	;
		Info = Info2
	}.

	% Qualify the modes in a pragma(c_code, ...) decl.
:- pred qualify_pragma((pragma_type)::in, (pragma_type)::out,
		mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

qualify_pragma(source_file(File), source_file(File), Info, Info) --> [].
qualify_pragma(c_header_code(Code), c_header_code(Code), Info, Info) --> [].
qualify_pragma(c_code(Code), c_code(Code), Info, Info) --> [].
qualify_pragma(c_code(Rec, SymName, PredOrFunc, PragmaVars0, Varset, CCode),
	c_code(Rec, SymName, PredOrFunc, PragmaVars, Varset, CCode), 
		Info0, Info) -->
	qualify_pragma_vars(PragmaVars0, PragmaVars, Info0, Info).
qualify_pragma(c_code(Rec, SymName, PredOrFunc, PragmaVars0,
		SavedVars, LabelCount, Varset, CCode),
	c_code(Rec, SymName, PredOrFunc, PragmaVars,
		SavedVars, LabelCount, Varset, CCode), Info0, Info) -->
	qualify_pragma_vars(PragmaVars0, PragmaVars, Info0, Info).
qualify_pragma(memo(A, B), memo(A, B), Info, Info) --> [].
qualify_pragma(inline(A, B), inline(A, B), Info, Info) --> [].
qualify_pragma(no_inline(A, B), no_inline(A, B), Info, Info) --> [].
qualify_pragma(obsolete(A, B), obsolete(A, B), Info, Info) --> [].
qualify_pragma(export(Name, PredOrFunc, Modes0, CFunc),
		export(Name, PredOrFunc, Modes, CFunc), Info0, Info) -->
	qualify_mode_list(Modes0, Modes, Info0, Info).
qualify_pragma(unused_args(A, B, C, D, E), unused_args(A, B, C, D, E),
				Info, Info) --> [].
qualify_pragma(fact_table(SymName, Arity, FileName),
	fact_table(SymName, Arity, FileName), Info, Info) --> [].
qualify_pragma(termination_info(PredOrFunc, SymName, ModeList0, Termination), 
		termination_info(PredOrFunc, SymName, ModeList, Termination), 
		Info0, Info) --> 
	qualify_mode_list(ModeList0, ModeList, Info0, Info).
qualify_pragma(terminates(A, B), terminates(A, B), Info, Info) --> [].
qualify_pragma(does_not_terminate(A, B), does_not_terminate(A, B), 
		Info, Info) --> [].
qualify_pragma(check_termination(A, B), check_termination(A, B), Info, 
		Info) --> [].

:- pred qualify_pragma_vars(list(pragma_var)::in, list(pragma_var)::out,
		mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

qualify_pragma_vars([], [], Info, Info) --> [].
qualify_pragma_vars([pragma_var(Var, Name, Mode0) | PragmaVars0],
		[pragma_var(Var, Name, Mode) | PragmaVars], Info0, Info) -->
	qualify_mode(Mode0, Mode, Info0, Info1),
	qualify_pragma_vars(PragmaVars0, PragmaVars, Info1, Info).

	% Find the unique match in the current name space for a given id
	% from a list of ids. If none exists, either because no match was
	% found or mulitiple matches were found, report an error.
	% This predicate assumes that type_ids, inst_ids and mode_ids
	% have the same representation.
:- pred find_unique_match(id::in, id::out, id_set::in, id_type::in,
		mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

find_unique_match(Id0, Id, Ids, TypeOfId, Info0, Info) -->
	(
		{ Id0 = qualified(Module, Name) - Arity },
		{ Id = Id0 },
		( { id_set_search_m_n_a(Ids, Module, Name, Arity) } ->
			{ mq_info_set_module_used(Info0, Module, Info) }
		;
			( { mq_info_get_report_error_flag(Info0, yes) } ->
				{ mq_info_get_error_context(Info0,
							ErrorContext) },
				report_undefined(ErrorContext, Id, TypeOfId),
				{ mq_info_set_error_flag(Info0,
							TypeOfId, Info1) },
				{ mq_info_incr_errors(Info1, Info) }
			;
				{ Info = Info0 }
			)
		)
	;
		{ Id0 = unqualified(IdName) - Arity },

		% Find all IDs which match the current ID's name and
		% arity and which come from modules imported by the
		% module where the current ID is used.

		{ id_set_search_name_arity(Ids, IdName, Arity, Modules) },
		( { Modules = [] } ->
			% No matches for this id.
			{ Id = Id0 },
			( { mq_info_get_report_error_flag(Info0, yes) } ->
				{ mq_info_get_error_context(Info0,
							ErrorContext) },
				report_undefined(ErrorContext, Id0, TypeOfId),
				{ mq_info_set_error_flag(Info0,
							TypeOfId, Info1) },
				{ mq_info_incr_errors(Info1, Info) }
			;
				{ Info = Info0 }
			)
		; { Modules = [Module] } ->
			% A unique match for this ID.
			{ Id = qualified(Module, IdName) - Arity },
			{ mq_info_set_module_used(Info0, Module, Info) }
		;
			% There are multiple matches.
			{ Id = Id0 },
			( { mq_info_get_report_error_flag(Info0, yes) } ->
				{ mq_info_get_error_context(Info0,
							ErrorContext) },
				report_multiply_defined(ErrorContext, Id0,
							TypeOfId, Modules),
				{ mq_info_set_error_flag(Info0,
							TypeOfId, Info1) },
				{ mq_info_incr_errors(Info1, Info) }
			;
				{ Info = Info0 }
			)
		)
	).
				
%------------------------------------------------------------------------------

:- type id_type --->
		type_id
	;	mode_id
	;	inst_id.

:- type error_context == pair(error_context2, term__context).

:- type id == pair(sym_name, int).

:- type error_context2 --->
		type(id) 
	;	inst(id)
	;	mode(id)
	;	pred(id)
	;	func(id)
	; 	pred_mode(id)
	;	func_mode(id)
	;	(pragma)
	;	lambda_expr
	;	type_qual.

	% Report an undefined type, inst or mode.
:- pred report_undefined(error_context, pair(sym_name, int),
				id_type, io__state, io__state).
:- mode report_undefined(in, in, in, di, uo) is det.

report_undefined(ErrorContext - Context, Id, IdType) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("In "),
	write_error_context2(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: undefined "),
	{ id_type_to_string(IdType, IdStr) },
	io__write_string(IdStr),
	io__write_string(" "),
	write_id(Id),
	io__write_string(".\n").

	% Report an error where a type, inst or mode had multiple possible
	% matches.
:- pred report_multiply_defined(error_context::in, id::in, id_type::in,
		list(module_name)::in, io__state::di, io__state::uo) is det.

report_multiply_defined(ErrorContext - Context, Id, IdType, Modules) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("In "),
	write_error_context2(ErrorContext),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  ambiguity error: multiple possible matches for "),
	{ id_type_to_string(IdType, IdStr) },
	io__write_string(IdStr),
	io__write_string(" "),
	write_id(Id),
	io__write_string(".\n"),
	prog_out__write_context(Context),
	io__write_string("  The possible matches are in modules\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	prog_out__write_module_list(Modules),
	io__write_string(".\n"),
	globals__io_lookup_bool_option(verbose_errors, Verbose),
	( { Verbose = yes } ->
		prog_out__write_context(Context),
		io__write_string("  An explicit module qualifier may be necessary.\n")
	;
		[]
	).

	% Give a context for the current error message.
:- pred write_error_context2(error_context2::in, io__state::di,
						io__state::uo) is det.

write_error_context2(type(Id)) -->
	io__write_string("definition of type "),
	write_id(Id).
write_error_context2(mode(Id)) -->
	io__write_string("definition of mode "),
	write_id(Id).
write_error_context2(inst(Id)) -->
	io__write_string("definition of inst "),
	write_id(Id).
write_error_context2(pred(Id)) -->
	io__write_string("definition of predicate "),
	write_id(Id).
write_error_context2(pred_mode(Id)) -->
	io__write_string("mode declaration for predicate "),
	write_id(Id).
write_error_context2(func(Id)) -->
	io__write_string("definition of function "),
	write_id(Id).
write_error_context2(func_mode(Id)) -->
	io__write_string("mode declaration for function "),
	write_id(Id).
write_error_context2(lambda_expr) -->
	io__write_string("mode declaration for lambda expression").
write_error_context2(pragma) -->
	io__write_string("pragma").
write_error_context2(type_qual) -->
	io__write_string("explicit type qualification").

:- pred id_type_to_string(id_type::in, string::out) is det.

id_type_to_string(type_id, "type").
id_type_to_string(mode_id, "mode").
id_type_to_string(inst_id, "inst").

	% Write sym_name/arity.
:- pred write_id(id::in, io__state::di, io__state::uo) is det.

write_id(SymName - Arity) -->
	io__write_string("`"),
	prog_out__write_sym_name(SymName),
	io__write_string("'/"),
	io__write_int(Arity).

	% Warn about modules imported in the interface when they do not
	% need to be.
:- pred maybe_warn_unused_interface_imports(module_name::in,
		list(module_name)::in, io__state::di, io__state::uo) is det.

maybe_warn_unused_interface_imports(ModuleName, UnusedImports) -->
	globals__io_lookup_bool_option(warn_interface_imports, Warn),
	(
		{ UnusedImports = []
		; Warn = no
		}
	->
		[]
	;
		{ string__append(ModuleName, ".m", FileName) },
		{ term__context_init(FileName, 1, Context) },
		prog_out__write_context(Context),
		io__write_string("Warning: "),
		( { UnusedImports = [_] } ->
			io__write_string("module ")
		;
			io__write_string("modules ")
		),
		prog_out__write_module_list(UnusedImports),
		io__write_string("\n"),
		prog_out__write_context(Context),
		{ is_or_are(UnusedImports, IsOrAre) },
		io__write_strings([
			"  ", IsOrAre,
			" imported in the interface, but ",
			IsOrAre, " not\n"
		]),
		prog_out__write_context(Context),
		io__write_string("  used in the interface.\n")
	).

:- pred is_or_are(list(T)::in, string::out) is det.

is_or_are([], "") :- error("module_qual:is_or_are").
is_or_are([_], "is").
is_or_are([_, _ | _], "are").

	% Output an error message about an ill-formed type.
:- pred report_invalid_type(term, error_context, io__state, io__state).
:- mode report_invalid_type(in, in, di, uo) is det.

report_invalid_type(Type, ErrorContext - Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("In definition of "),
	write_error_context2(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: ill-formed type `"),
	{ varset__init(VarSet) },
	mercury_output_term(Type, VarSet, no),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

	% is_builtin_atomic_type(TypeId)
	%	is true iff 'TypeId' is the type_id of a builtin atomic type

:- pred is_builtin_atomic_type(type_id).
:- mode is_builtin_atomic_type(in) is semidet.

is_builtin_atomic_type(unqualified("int") - 0).
is_builtin_atomic_type(unqualified("float") - 0).
is_builtin_atomic_type(unqualified("string") - 0).
is_builtin_atomic_type(unqualified("character") - 0).

	% is_builtin_pred_type(TypeId)
	%	is true iff 'TypeId' is the type_id of a builtin higher-order
	%	predicate type.

:- pred is_builtin_pred_type(type_id).
:- mode is_builtin_pred_type(in) is semidet.

is_builtin_pred_type(unqualified("pred") - _Arity).

	% is_builtin_func_type(Functor, Args)
	%	is true iff `term__functor(Functor, Args, _)' is a builtin
	%	higher-order function type.

:- pred is_builtin_func_type(const, list(type), list(type), type).
:- mode is_builtin_func_type(in, in, out, out) is semidet.

is_builtin_func_type(term__atom("="),
		[term__functor(term__atom("func"), ArgTypes, _), RetType],
		ArgTypes, RetType).

%-----------------------------------------------------------------------------%
% Access and initialisation predicates.

:- pred init_mq_info(bool::in, mq_info::out) is det.

init_mq_info(ReportErrors, Info0) :-
	term__context_init(Context),
	ErrorContext = type(unqualified("") - 0) - Context,
	set__init(InterfaceModules0),
	id_set_init(Empty),
	Info0 = mq_info(Empty, Empty, Empty, InterfaceModules0, local, 0,
		no, no, ReportErrors, ErrorContext, may_be_unqualified).

:- pred mq_info_get_types(mq_info::in, type_id_set::out) is det.
:- pred mq_info_get_insts(mq_info::in, inst_id_set::out) is det.
:- pred mq_info_get_modes(mq_info::in, mode_id_set::out) is det.
:- pred mq_info_get_interface_modules(mq_info::in,
					set(module_name)::out) is det.
:- pred mq_info_get_import_status(mq_info::in, import_status::out) is det.
% :- pred mq_info_get_num_errors(mq_info::in, int::out) is det.
% :- pred mq_info_get_type_error_flag(mq_info::in, bool::out) is det.
% :- pred mq_info_get_mode_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_report_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_error_context(mq_info::in, error_context::out) is det.

mq_info_get_types(mq_info(Types, _,_,_,_,_,_,_,_,_,_), Types).
mq_info_get_insts(mq_info(_, Insts, _,_,_,_,_,_,_,_,_), Insts).
mq_info_get_modes(mq_info(_,_, Modes, _,_,_,_,_,_,_,_), Modes).
mq_info_get_interface_modules(mq_info(_,_,_, Modules, _,_,_,_,_,_,_), Modules).
mq_info_get_import_status(mq_info(_,_,_,_, Status, _,_,_,_,_,_), Status).
mq_info_get_num_errors(mq_info(_,_,_,_,_, NumErrors, _,_,_,_,_), NumErrors).
mq_info_get_type_error_flag(mq_info(_,_,_,_,_,_, TypeErrs, _,_,_,_), TypeErrs).
mq_info_get_mode_error_flag(mq_info(_,_,_,_,_,_,_, ModeError, _,_,_),
						ModeError).
mq_info_get_report_error_flag(mq_info(_,_,_,_,_,_,_,_, Report,_,_), Report).
mq_info_get_error_context(mq_info(_,_,_,_,_,_,_,_,_, Context,_), Context).
mq_info_get_need_qual_flag(mq_info(_,_,_,_,_,_,_,_,_,_,UseModule), UseModule).

:- pred mq_info_set_types(mq_info::in, type_id_set::in, mq_info::out) is det.
:- pred mq_info_set_insts(mq_info::in, inst_id_set::in, mq_info::out) is det.
:- pred mq_info_set_modes(mq_info::in, mode_id_set::in, mq_info::out) is det.
:- pred mq_info_set_interface_modules(mq_info::in, set(module_name)::in,
						mq_info::out) is det.
:- pred mq_info_set_import_status(mq_info::in, import_status::in,
						mq_info::out) is det.
:- pred mq_info_set_type_error_flag(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_mode_error_flag(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_error_context(mq_info::in, error_context::in,
						mq_info::out) is det.

mq_info_set_types(mq_info(_, B,C,D,E,F,G,H,I,J,K), Types,
		mq_info(Types, B,C,D,E,F,G,H,I,J,K)).
mq_info_set_insts(mq_info(A,_,C,D,E,F,G,H,I,J,K), Insts,
		mq_info(A, Insts, C,D,E,F,G,H,I,J,K)).
mq_info_set_modes(mq_info(A,B,_,D,E,F,G,H,I,J,K), Modes,
		mq_info(A,B, Modes, D,E,F,G,H,I,J,K)).
mq_info_set_interface_modules(mq_info(A,B,C,_,E,F,G,H,I,J,K), Modules,
		mq_info(A,B,C, Modules, E,F,G,H,I,J,K)).
mq_info_set_import_status(mq_info(A,B,C,D,_,F,G,H,I,J,K), Status,
		mq_info(A,B,C,D, Status, F,G,H,I,J,K)).
mq_info_set_type_error_flag(mq_info(A,B,C,D,E,F, _, H,I,J,K),
		mq_info(A,B,C,D,E,F, yes, H,I,J,K)).
mq_info_set_mode_error_flag(mq_info(A,B,C,D,E,F,G,_,I,J,K),
		mq_info(A,B,C,D,E,F,G, yes, I,J,K)).
mq_info_set_error_context(mq_info(A,B,C,D,E,F,G,H,I,_,K), Context,
		mq_info(A,B,C,D,E,F,G,H,I, Context,K)).
mq_info_set_need_qual_flag(mq_info(A,B,C,D,E,F,G,H,I,J,_), Flag,
		mq_info(A,B,C,D,E,F,G,H,I,J, Flag)).

:- pred mq_info_incr_errors(mq_info::in, mq_info::out) is det.

mq_info_incr_errors(mq_info(A,B,C,D,E, NumErrors0, G,H,I,J,K), 
		mq_info(A,B,C,D,E, NumErrors, G,H,I,J,K)) :-
	NumErrors is NumErrors0 + 1.

:- pred mq_info_set_error_flag(mq_info::in, id_type::in, mq_info::out) is det.

mq_info_set_error_flag(Info0, type_id, Info) :-
	mq_info_set_type_error_flag(Info0, Info).
mq_info_set_error_flag(Info0, mode_id, Info) :-
	mq_info_set_mode_error_flag(Info0, Info).
mq_info_set_error_flag(Info0, inst_id, Info) :-
	mq_info_set_mode_error_flag(Info0, Info).

	% If the current item is in the interface, remove its module 
	% name from the list of modules not used in the interface.
:- pred mq_info_set_module_used(mq_info::in, module_name::in,
						mq_info::out) is det.

mq_info_set_module_used(Info0, Module, Info) :-
	( mq_info_get_import_status(Info0, exported) ->
		mq_info_get_interface_modules(Info0, Modules0),
		set__delete(Modules0, Module, Modules),
		mq_info_set_interface_modules(Info0, Modules, Info)
	;
		Info = Info0
	).

	% Add to the list of modules imported in the interface.
:- pred mq_info_add_interface_modules(mq_info::in, list(module_name)::in,
						mq_info::out) is det.

mq_info_add_interface_modules(Info0, NewModules, Info) :-
	( mq_info_get_import_status(Info0, exported) ->
		mq_info_get_interface_modules(Info0, Modules0),
		set__insert_list(Modules0, NewModules, Modules),
		mq_info_set_interface_modules(Info0, Modules, Info)
	;
		Info = Info0
	).

%----------------------------------------------------------------------------%
% Define a type for representing sets of ids during module qualification
% to allow efficient retrieval of all the modules which define an id
% with a certain name and arity.

% The first set of module_names can be used without module qualifiers,
% items from the second set can only be used with module qualifiers.
% Items from modules imported with a :- use_module declaration and from `.opt'
% files should go into the second set.
:- type id_set == map(pair(string, arity), pair(set(module_name))).

:- type type_id_set == id_set.
:- type mode_id_set == id_set.
:- type inst_id_set == id_set.

:- pred id_set_init(id_set::out) is det.

id_set_init(IdSet) :-
	map__init(IdSet).

	% Insert an id into an id_set, aborting with an error if the
	% id is not module qualified.
:- pred id_set_insert(need_qualifier::in, id::in, 
		id_set::in, id_set::out) is det.

id_set_insert(_, unqualified(_) - _, _, _) :-
	error("module_qual:id_set_insert - unqualified id").
id_set_insert(NeedQualifier, qualified(Module, Name) - Arity, IdSet0, IdSet) :-
	( map__search(IdSet0, Name - Arity, ImportModules0 - UseModules0) ->
		ImportModules1 = ImportModules0,
		UseModules1 = UseModules0
	;
		set__init(ImportModules1),
		set__init(UseModules1)
	),
	(
		NeedQualifier = must_be_qualified,
		set__insert(UseModules1, Module, UseModules),
		ImportModules = ImportModules1
	;
		NeedQualifier = may_be_unqualified,
		set__insert(ImportModules1, Module, ImportModules),
		UseModules = UseModules1
	),
	map__set(IdSet0, Name - Arity, ImportModules - UseModules, IdSet).

:- pred id_set_search_name_arity(id_set::in, string::in, int::in,
				list(module_name)::out) is det.

id_set_search_name_arity(IdSet0, Name, Arity, Modules) :-
	( map__search(IdSet0, Name - Arity, ImportModules - _) ->
		set__to_sorted_list(ImportModules, Modules)
	;
		Modules = []
	).

:- pred id_set_search_m_n_a(id_set::in, module_name::in,
			 	string::in, int::in) is semidet.

id_set_search_m_n_a(IdSet0, Module, Name, Arity) :-
	map__search(IdSet0, Name - Arity, ImportModules - UseModules),
	( 
		set__member(Module, ImportModules)
	;
		set__member(Module, UseModules)
	).

%----------------------------------------------------------------------------%
