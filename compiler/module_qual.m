%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
:- module parse_tree__module_qual.
%	Main authors: stayl, fjh.
%
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

:- import_module parse_tree__prog_data, recompilation.
:- import_module bool, list, std_util, io.

	% module_qualify_items(Items0, Items, ModuleName, ReportUndefErrors,
	%			MQ_Info, NumErrors, UndefTypes, UndefModes):
	%
	% Items is Items0 with all items module qualified as much
	% as possible. If ReportUndefErrors is yes, then
	% report undefined types, insts and modes. 
	% ReportUndefErrors should be no when module qualifying the
	% short interface.
:- pred module_qual__module_qualify_items(item_list, item_list,
		module_name, bool, mq_info, int, bool, bool,
		io__state, io__state).
:- mode module_qual__module_qualify_items(in, out, in, in,
		out, out, out, out, di, uo) is det.

	% This is called from make_hlds.m to qualify the mode of a lambda
	% expression.
:- pred module_qual__qualify_lambda_mode_list(list(mode), list(mode),
		prog_context, mq_info, mq_info,
		io__state, io__state) is det.
:- mode module_qual__qualify_lambda_mode_list(in, out, 
		in, in, out, di, uo) is det.

	% This is called from make_hlds.m to qualify the modes in a
	% clause mode annotation.
:- pred module_qual__qualify_clause_mode_list(list(mode), list(mode),
		prog_context, mq_info, mq_info,
		io__state, io__state) is det.
:- mode module_qual__qualify_clause_mode_list(in, out, 
		in, in, out, di, uo) is det.

	% This is called from make_hlds.m to qualify an 
	% explicit type qualification.
:- pred module_qual__qualify_type_qualification(type, type, prog_context,
		mq_info, mq_info, io__state, io__state).
:- mode module_qual__qualify_type_qualification(in, out, in, in,
		out, di, uo) is det.

	% The type mq_info holds information needed for doing module
	% qualification.
:- type mq_info.

:- pred mq_info_get_num_errors(mq_info::in, int::out) is det.
:- pred mq_info_get_type_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_mode_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_set_need_qual_flag(mq_info::in, 
		need_qualifier::in, mq_info::out) is det.
:- pred mq_info_get_need_qual_flag(mq_info::in, need_qualifier::out) is det.
:- pred mq_info_get_partial_qualifier_info(mq_info::in,
		partial_qualifier_info::out) is det.
:- pred mq_info_get_recompilation_info(mq_info::in,
		maybe(recompilation_info)::out) is det.
:- pred mq_info_set_recompilation_info(mq_info::in,
		maybe(recompilation_info)::in, mq_info::out) is det.

	% The type partial_qualifier_info holds info need for computing which
	% partial quantifiers are visible -- see get_partial_qualifiers/3.
:- type partial_qualifier_info.

% Suppose we are processing a definition which defines the symbol
% foo:bar:baz:quux/1.  Then we insert the following symbols
% into the symbol table:
%	- if the current value of the NeedQual flag at this point
%		is `may_be_unqualified',
%		i.e. module `foo:bar:baz' was imported
%		then we insert the fully unqualified symbol quux/1;
%	- if module `foo:bar:baz' occurs in the "imported" section,
%		i.e. if module `foo:bar' was imported,
%		then we insert the partially qualified symbol baz:quux/1;
%	- if module `foo:bar' occurs in the "imported" section,
%		i.e. if module `foo' was imported,
%		then we insert the partially qualified symbol bar:baz:quux/1;
%	- we always insert the fully qualified symbol foo:bar:baz:quux/1.
%
% The predicate `get_partial_qualifiers' returns all of the
% partial qualifiers for which we need to insert definitions,
% i.e. all the ones which are visible.  For example,
% given as input `foo:bar:baz', it returns a list containing
%	(1) `baz', iff `foo:bar' is imported
% and 	(2) `bar:baz', iff `foo' is imported.
% Note that the caller will still need to handle the fully-qualified
% and fully-unqualified versions separately.

:- pred get_partial_qualifiers(module_name, partial_qualifier_info,
		list(module_name)).
:- mode get_partial_qualifiers(in, in, out) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module check_hlds__type_util, parse_tree__prog_io.
:- import_module parse_tree__prog_out, hlds__hlds_out.
:- import_module parse_tree__prog_util, parse_tree__mercury_to_mercury.
:- import_module parse_tree__modules, libs__globals, libs__options.
:- import_module (parse_tree__inst), hlds__instmap.
:- import_module hlds__hlds_data.	% for cons_id.

:- import_module int, map, require, set, string, term, varset.
:- import_module assoc_list.

module_qual__module_qualify_items(Items0, Items, ModuleName, ReportErrors,
			Info, NumErrors, UndefTypes, UndefModes) -->
	globals__io_get_globals(Globals),
	{ init_mq_info(Items0, Globals, ReportErrors, ModuleName, Info0) },
	{ collect_mq_info(Items0, Info0, Info1) },
	do_module_qualify_items(Items0, Items, Info1, Info),
	{ mq_info_get_type_error_flag(Info, UndefTypes) },
	{ mq_info_get_mode_error_flag(Info, UndefModes) },
	( { ReportErrors = yes } ->
		{ mq_info_get_unused_interface_modules(Info, UnusedImports0) },
		{ set__to_sorted_list(UnusedImports0, UnusedImports) },
		maybe_warn_unused_interface_imports(ModuleName, UnusedImports)
	;
		[]
	),
	{ mq_info_get_num_errors(Info, NumErrors) }.

module_qual__qualify_lambda_mode_list(Modes0, Modes, Context, Info0, Info) -->
	{ mq_info_set_error_context(Info0, lambda_expr - Context, Info1) },
	qualify_mode_list(Modes0, Modes, Info1, Info).

module_qual__qualify_clause_mode_list(Modes0, Modes, Context, Info0, Info) -->
	{ mq_info_set_error_context(Info0, clause_mode_annotation - Context,
		Info1) },
	qualify_mode_list(Modes0, Modes, Info1, Info).

module_qual__qualify_type_qualification(Type0, Type, Context, Info0, Info) -->
	{ mq_info_set_error_context(Info0, type_qual - Context, Info1) },
	qualify_type(Type0, Type, Info1, Info).

:- type mq_info
	--->	mq_info(
				% Modules which have been imported or used,
				% i.e. ones for which there was a
				% `:- import_module' or `:- use_module'
				% declaration in this module.
			imported_modules::set(module_name),

				% Sets of all modules, types, insts, modes,
				% and typeclasses visible in this module.
			modules::module_id_set,
			types::type_id_set,
			insts::inst_id_set,
			modes::mode_id_set,
			classes::class_id_set,

			unused_interface_modules::set(module_name), 
				% modules imported in the
				% interface that are not definitely
				% needed in the interface.

				% import status of the current item.
			import_status::import_status, 

			num_errors::int,% number of errors found.

				% are there any undefined types or typeclasses.
			type_error_flag::bool,	
				% are there any undefined insts or modes.
			mode_error_flag::bool,	
				% do we want to report errors.
			report_error_flag::bool, 	
				% context of the current item.
			error_context::error_context,	
				% name of the current module
			this_module::module_name,  
				% must uses of the current item be 
				% explicitly module qualified.
			need_qual_flag::need_qualifier,
			maybe_recompilation_info::maybe(recompilation_info)
	).

:- type partial_qualifier_info --->
	partial_qualifier_info(module_id_set).

mq_info_get_partial_qualifier_info(MQInfo, QualifierInfo) :-
	mq_info_get_modules(MQInfo, ModuleIdSet),
	QualifierInfo = partial_qualifier_info(ModuleIdSet).

	% We only need to keep track of what is exported and what isn't,
	% so we use a simpler data type here than hlds_pred__import_status.
:- type import_status
	--->	exported
	;	local
	;	imported.
		
	% Pass over the item list collecting all defined module, type, mode and
	% inst ids, all module synonym definitions, and the names of all
	% modules imported in the interface.
:- pred collect_mq_info(item_list::in, mq_info::in, mq_info::out) is det.

collect_mq_info([], Info, Info).
collect_mq_info([Item - _ | Items], Info0, Info) :-
	( Item = module_defn(_, transitively_imported) ->
		% Don't process the transitively imported items (from `.int2'
		% files). They can't be used in the current module.
		Info = Info0
	;
		collect_mq_info_2(Item, Info0, Info1),
		collect_mq_info(Items, Info1, Info)
	).

:- pred collect_mq_info_2(item::in, mq_info::in, mq_info::out) is det.

collect_mq_info_2(clause(_,_,_,_,_), Info, Info).
collect_mq_info_2(type_defn(_, SymName, Params, _, _), Info0, Info) :-
	list__length(Params, Arity),
	mq_info_get_types(Info0, Types0),
	mq_info_get_need_qual_flag(Info0, NeedQualifier),
	id_set_insert(NeedQualifier, SymName - Arity, Types0, Types),
	mq_info_set_types(Info0, Types, Info).
collect_mq_info_2(inst_defn(_, SymName, Params, _, _), Info0, Info) :-
	list__length(Params, Arity),
	mq_info_get_insts(Info0, Insts0),
	mq_info_get_need_qual_flag(Info0, NeedQualifier),
	id_set_insert(NeedQualifier, SymName - Arity, Insts0, Insts),
	mq_info_set_insts(Info0, Insts, Info).
collect_mq_info_2(mode_defn(_, SymName, Params, _, _), Info0, Info) :-
	list__length(Params, Arity),
	mq_info_get_modes(Info0, Modes0),
	mq_info_get_need_qual_flag(Info0, NeedQualifier),
	id_set_insert(NeedQualifier, SymName - Arity, Modes0, Modes),
	mq_info_set_modes(Info0, Modes, Info).
collect_mq_info_2(module_defn(_, ModuleDefn), Info0, Info) :-
	process_module_defn(ModuleDefn, Info0, Info).
collect_mq_info_2(pred_or_func(_,_,_,_,__,_,_,_,_,_,_,_), Info, Info).
collect_mq_info_2(pred_or_func_mode(_,_,_,_,_,_,_), Info, Info).
collect_mq_info_2(pragma(_), Info, Info).
collect_mq_info_2(promise(_PromiseType, Goal, _ProgVarSet, _UnivVars), Info0, 
		Info) :-
	process_assert(Goal, SymNames, Success),
	(
		Success = yes,
		list__foldl((pred(SymName::in, I0::in, I::out) is det :- 
				(
					SymName = qualified(ModuleName, _)
				->
					mq_info_set_module_used(I0,
							ModuleName, I)
				;
					error("collect_mq_info_2: SymName not qualified.")
				)
			), 
			SymNames, Info0, Info)
	;
			% Any unqualified symbol in the promise might
			% come from *any* of the imported modules.
			% There's no way for us to tell which ones.  So
			% we conservatively assume that it uses all of
			% them. 
		Success = no,
		set__init(UnusedInterfaceModules),
		mq_info_set_unused_interface_modules(Info0,
				UnusedInterfaceModules, Info)
	).
collect_mq_info_2(nothing(_), Info, Info).
collect_mq_info_2(typeclass(_, SymName, Params, _, _), Info0, Info) :-
	list__length(Params, Arity),
	mq_info_get_classes(Info0, Classes0),
	mq_info_get_need_qual_flag(Info0, NeedQualifier),
	id_set_insert(NeedQualifier, SymName - Arity, Classes0, Classes),
	mq_info_set_classes(Info0, Classes, Info).
collect_mq_info_2(instance(_,_,_,_,_,_), Info, Info).

	% process_module_defn:
	%
	% - Update the import status.
	%
	% - For sub-module definitions (whether nested or separate,
	%   i.e. either `:- module foo.' or `:- include_module foo.'),
	%   add the module id to the module_id_set.
	%
	% - For import declarations (`:- import_module' or `:- use_module'),
	%   if we're currently in the interface section, then add the
	%   imported modules to the unused_interface_modules list.

:- pred process_module_defn(module_defn::in, mq_info::in, mq_info::out) is det.

process_module_defn(module(ModuleName), Info0, Info) :-
	add_module_defn(ModuleName, Info0, Info).
process_module_defn(include_module(ModuleNameList), Info0, Info) :-
	list__foldl(add_module_defn, ModuleNameList, Info0, Info).
process_module_defn(interface, Info0, Info) :-
	mq_info_set_import_status(Info0, exported, Info).
process_module_defn(private_interface, Info0, Info) :-
	mq_info_set_import_status(Info0, local, Info).
process_module_defn(implementation, Info0, Info) :-
	mq_info_set_import_status(Info0, local, Info).
process_module_defn(imported(_), Info0, Info) :-
	mq_info_set_import_status(Info0, imported, Info1),
	mq_info_set_need_qual_flag(Info1, may_be_unqualified, Info).
process_module_defn(used(_), Info0, Info) :-
	mq_info_set_import_status(Info0, imported, Info1),
	mq_info_set_need_qual_flag(Info1, must_be_qualified, Info).
process_module_defn(opt_imported, Info0, Info) :-
	mq_info_set_import_status(Info0, imported, Info1),
	mq_info_set_need_qual_flag(Info1, must_be_qualified, Info).
process_module_defn(transitively_imported, _, _) :-
	error("process_module_defn: transitively_imported item").
process_module_defn(external(_), Info, Info).
process_module_defn(end_module(_), Info, Info).
process_module_defn(export(_), Info, Info).
process_module_defn(import(Imports), Info0, Info) :-
	add_imports(Imports, Info0, Info).
process_module_defn(use(Imports), Info0, Info) :-
	add_imports(Imports, Info0, Info).
process_module_defn(version_numbers(_, _), Info, Info).

:- pred add_module_defn(module_name, mq_info, mq_info).
:- mode add_module_defn(in, in, out) is det.

add_module_defn(ModuleName, Info0, Info) :-
	mq_info_get_modules(Info0, Modules0),
	mq_info_get_need_qual_flag(Info0, NeedQualifier),
	Arity = 0,
	id_set_insert(NeedQualifier, ModuleName - Arity, Modules0, Modules),
	mq_info_set_modules(Info0, Modules, Info).

:- pred add_imports(sym_list::in, mq_info::in, mq_info::out) is det.

add_imports(Imports, Info0, Info) :-
	mq_info_get_import_status(Info0, Status),
	( Imports = module(ImportedModules), Status \= imported ->
		mq_info_add_imported_modules(Info0, ImportedModules, Info1),
		( Status = exported ->
			mq_info_add_unused_interface_modules(Info1,
				ImportedModules, Info)
		;
			Info = Info1
		)
	;
		Info = Info0
	).

%------------------------------------------------------------------------------

	% process_assert(G, SNs, B)
	%
	% Scan the goal, G, building the list of qualified symbols, SNs.
	% If there exists a single unqualifed symbol in G, the bool, B,
	% will be set to no.
:- pred process_assert(goal::in, list(sym_name)::out, bool::out) is det.

	% AAA Some more stuff to do accumulator introduction on, it
	% would be better to rewrite using maybes and then to declare
	% the maybe_and predicate to be associative.
	% NB. accumulator introduction doesn't work on this case yet.
process_assert((GA , GB) - _, Symbols, Success) :-
	process_assert(GA, SymbolsA, SuccessA),
	process_assert(GB, SymbolsB, SuccessB),
	list__append(SymbolsA, SymbolsB, Symbols),
	bool__and(SuccessA, SuccessB, Success).
process_assert(true - _, [], yes).
process_assert((GA & GB) - _, Symbols, Success) :-
	process_assert(GA, SymbolsA, SuccessA),
	process_assert(GB, SymbolsB, SuccessB),
	list__append(SymbolsA, SymbolsB, Symbols),
	bool__and(SuccessA, SuccessB, Success).
process_assert((GA ; GB) - _, Symbols, Success) :-
	process_assert(GA, SymbolsA, SuccessA),
	process_assert(GB, SymbolsB, SuccessB),
	list__append(SymbolsA, SymbolsB, Symbols),
	bool__and(SuccessA, SuccessB, Success).
process_assert(fail - _, [], yes).
process_assert(some(_, G) - _, Symbols, Success) :-
	process_assert(G, Symbols, Success).
process_assert(some_state_vars(_, G) - _, Symbols, Success) :-
	process_assert(G, Symbols, Success).
process_assert(all(_, G) - _, Symbols, Success) :-
	process_assert(G, Symbols, Success).
process_assert(all_state_vars(_, G) - _, Symbols, Success) :-
	process_assert(G, Symbols, Success).
process_assert(implies(GA, GB) - _, Symbols, Success) :-
	process_assert(GA, SymbolsA, SuccessA),
	process_assert(GB, SymbolsB, SuccessB),
	list__append(SymbolsA, SymbolsB, Symbols),
	bool__and(SuccessA, SuccessB, Success).
process_assert(equivalent(GA, GB) - _, Symbols, Success) :-
	process_assert(GA, SymbolsA, SuccessA),
	process_assert(GB, SymbolsB, SuccessB),
	list__append(SymbolsA, SymbolsB, Symbols),
	bool__and(SuccessA, SuccessB, Success).
process_assert(not(G) - _, Symbols, Success) :-
	process_assert(G, Symbols, Success).
process_assert(if_then(_, _, GA, GB) - _, Symbols, Success) :-
	process_assert(GA, SymbolsA, SuccessA),
	process_assert(GB, SymbolsB, SuccessB),
	list__append(SymbolsA, SymbolsB, Symbols),
	bool__and(SuccessA, SuccessB, Success).
process_assert(if_then_else(_, _, GA, GB, GC) - _, Symbols, Success) :-
	process_assert(GA, SymbolsA, SuccessA),
	process_assert(GB, SymbolsB, SuccessB),
	process_assert(GC, SymbolsC, SuccessC),
	list__append(SymbolsA, SymbolsB, Symbols0),
	list__append(Symbols0, SymbolsC, Symbols),
	bool__and(SuccessA, SuccessB, Success0),
	bool__and(Success0, SuccessC, Success).
process_assert(call(SymName, Args0, _Purity) - _, Symbols, Success) :-
	(
		SymName = qualified(_, _)
	->
		list__map(term__coerce, Args0, Args),
		(
			term_qualified_symbols_list(Args, Symbols0)
		->
			Symbols = [SymName | Symbols0],
			Success = yes
		;
			Symbols = [],
			Success = no
		)
	;
		Symbols = [],
		Success = no
	).
process_assert(unify(LHS0, RHS0, _Purity) - _, Symbols, Success) :-
	term__coerce(LHS0, LHS),
	term__coerce(RHS0, RHS),
	(
		term_qualified_symbols(LHS, SymbolsL),
		term_qualified_symbols(RHS, SymbolsR)
	->
		list__append(SymbolsL, SymbolsR, Symbols),
		Success = yes
	;
		Symbols = [],
		Success = no
	).

	% term_qualified_symbols(T, S)
	%
	% Given a term, T, return the list of all the sym_names, S, in
	% the term.  The predicate fails if any sub-term of T is
	% unqualified.
:- pred term_qualified_symbols(term::in, list(sym_name)::out) is semidet.

term_qualified_symbols(Term, Symbols) :-
	(
		sym_name_and_args(Term, SymName, Args)
	->
		SymName = qualified(_, _),
		term_qualified_symbols_list(Args, Symbols0),
		Symbols = [SymName | Symbols0]
	;
		Symbols = []
	).

:- pred term_qualified_symbols_list(list(term)::in,
		list(sym_name)::out) is semidet.

	% Yeah one more place where accumulators will be introduced!
term_qualified_symbols_list([], []).
term_qualified_symbols_list([Term | Terms], Symbols) :-
	term_qualified_symbols(Term, TermSymbols),
	term_qualified_symbols_list(Terms, Symbols0),
	list__append(Symbols0, TermSymbols, Symbols).

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

module_qualify_item(clause(A,B,C,D,E) - Con, clause(A,B,C,D,E) - Con,
			Info, Info, yes) --> [].

module_qualify_item(type_defn(A, SymName, Params, TypeDefn0, C) - Context,
		type_defn(A, SymName, Params, TypeDefn, C) - Context,
		Info0, Info, yes) --> 
	{ list__length(Params, Arity) },
	{ mq_info_set_error_context(Info0,
		type(SymName - Arity) - Context, Info1) },
	qualify_type_defn(TypeDefn0, TypeDefn, Info1, Info).  

module_qualify_item(inst_defn(A, SymName, Params, InstDefn0, C) - Context,
		inst_defn(A, SymName, Params, InstDefn, C) - Context,
		Info0, Info, yes) --> 
	{ list__length(Params, Arity) },
	{ mq_info_set_error_context(Info0,
		inst(SymName - Arity) - Context, Info1) },
	qualify_inst_defn(InstDefn0, InstDefn, Info1, Info).

module_qualify_item(mode_defn(A, SymName, Params, ModeDefn0, C) - Context,
		mode_defn(A, SymName, Params, ModeDefn, C) - Context,
		Info0, Info, yes) -->
	{ list__length(Params, Arity) },
	{ mq_info_set_error_context(Info0,
		mode(SymName - Arity) - Context, Info1) },
	qualify_mode_defn(ModeDefn0, ModeDefn, Info1, Info).

module_qualify_item(module_defn(A, ModuleDefn) - Context,
		module_defn(A, ModuleDefn) - Context, Info0, Info, Continue) -->
	{ update_import_status(ModuleDefn, Info0, Info, Continue) }.

module_qualify_item(
		pred_or_func(A, IVs, B, PredOrFunc, SymName, TypesAndModes0,
			WithType0, WithInst0, C, D, E, Constraints0) - Context,
		pred_or_func(A, IVs, B, PredOrFunc, SymName, TypesAndModes,
			WithType, WithInst, C, D, E, Constraints) - Context,
		Info0, Info, yes) -->
	{ list__length(TypesAndModes0, Arity) },
	{ mq_info_set_error_context(Info0,
		pred_or_func(PredOrFunc, SymName - Arity) - Context,
		Info1) },
	qualify_types_and_modes(TypesAndModes0, TypesAndModes, Info1, Info2),
	qualify_class_constraints(Constraints0, Constraints, Info2, Info3),
	map_fold2_maybe(qualify_type, WithType0, WithType, Info3, Info4),
	map_fold2_maybe(qualify_inst, WithInst0, WithInst, Info4, Info).

module_qualify_item(
		pred_or_func_mode(A, PredOrFunc, SymName, Modes0,
			WithInst0, C, D) - Context,
	 	pred_or_func_mode(A, PredOrFunc, SymName, Modes,
			WithInst, C, D) - Context,
		Info0, Info, yes) -->
	{ list__length(Modes0, Arity) },
	{ mq_info_set_error_context(Info0,
		pred_or_func_mode(PredOrFunc, SymName- Arity) - Context,
		Info1) },
	qualify_mode_list(Modes0, Modes, Info1, Info2),
	map_fold2_maybe(qualify_inst, WithInst0, WithInst, Info2, Info).

module_qualify_item(pragma(Pragma0) - Context, pragma(Pragma) - Context,
						Info0, Info, yes) -->
	{ mq_info_set_error_context(Info0, (pragma) - Context, Info1) },
	qualify_pragma(Pragma0, Pragma, Info1, Info).
module_qualify_item(promise(T, G, V, U) - Context, 
		promise(T, G, V, U) - Context, Info, Info, yes) --> [].
module_qualify_item(nothing(A) - Context, nothing(A) - Context,
						Info, Info, yes) --> [].
module_qualify_item(typeclass(Constraints0, Name, Vars, Interface0, VarSet) -
			Context, 
		typeclass(Constraints, Name, Vars, Interface, VarSet) -
			Context, 
		Info0, Info, yes) -->
	{ list__length(Vars, Arity) },
	{ Id = Name - Arity },
	{ mq_info_set_error_context(Info0, class(Id) - Context, Info1) },
	qualify_class_constraint_list(Constraints0, Constraints, Info1, Info2),
	(
		{ Interface0 = abstract },
		{ Interface = abstract },
		{ Info = Info2 }
	;
		{ Interface0 = concrete(Methods0) },
		qualify_class_interface(Methods0, Methods, Info2, Info),
		{ Interface = concrete(Methods) }
	).

module_qualify_item(
		instance(Constraints0, Name0, Types0, Body0, VarSet,
			ModName) - Context, 
		instance(Constraints, Name, Types, Body, VarSet,
			ModName) - Context, 
		Info0, Info, yes) -->
	{ list__length(Types0, Arity) },
	{ Id = Name0 - Arity },
	{ mq_info_set_error_context(Info0, instance(Id) - Context, Info1) },
		% We don't qualify the implementation yet, since that requires
		% us to resolve overloading.
	qualify_class_constraint_list(Constraints0, Constraints, Info1, Info2),
	qualify_class_name(Id, Name - _, Info2, Info3),
	qualify_type_list(Types0, Types, Info3, Info),
	{ qualify_instance_body(Name, Body0, Body) }.

:- pred update_import_status(module_defn::in, mq_info::in, mq_info::out,
							bool::out) is det.

update_import_status(opt_imported, Info, Info, no).
update_import_status(transitively_imported, Info, Info, no).
update_import_status(module(_), Info, Info, yes).
update_import_status(interface, Info0, Info, yes) :-
	mq_info_set_import_status(Info0, exported, Info).
update_import_status(implementation, Info0, Info, yes) :-
	mq_info_set_import_status(Info0, local, Info).
update_import_status(private_interface, Info0, Info, yes) :-
	mq_info_set_import_status(Info0, local, Info).
update_import_status(imported(_), Info, Info, no).
update_import_status(used(_), Info, Info, no).
update_import_status(external(_), Info, Info, yes).
update_import_status(end_module(_), Info, Info, yes).
update_import_status(export(_), Info, Info, yes).
update_import_status(import(_), Info, Info, yes).
update_import_status(use(_), Info, Info, yes).
update_import_status(version_numbers(_, _), Info, Info, yes).
update_import_status(include_module(_), Info0, Info, yes) :-
	% The sub-module might make use of *any* of the imported modules.
	% There's no way for us to tell which ones.
	% So we conservatively assume that it uses all of them.
	set__init(UnusedInterfaceModules),
	mq_info_set_unused_interface_modules(Info0, UnusedInterfaceModules,
		Info).

	% Qualify the constructors or other types in a type definition.	
:- pred qualify_type_defn(type_defn::in, type_defn::out, mq_info::in,
	mq_info::out, io__state::di, io__state::uo) is det.

qualify_type_defn(du_type(Ctors0, MaybeEqualityPred0),
		du_type(Ctors, MaybeEqualityPred), Info0, Info) -->
	qualify_constructors(Ctors0, Ctors, Info0, Info),

	% User-defined equality pred names will be converted into
	% predicate calls and then module-qualified after type analysis
	% (during mode analysis).  That way they get full type overloading
	% resolution, etc.  Thus we don't module-qualify them here.
	{ MaybeEqualityPred = MaybeEqualityPred0 }.
qualify_type_defn(eqv_type(Type0), eqv_type(Type), Info0, Info) -->
	qualify_type(Type0, Type, Info0, Info).	
qualify_type_defn(abstract_type, abstract_type, Info, Info) --> [].

:- pred qualify_constructors(list(constructor)::in, list(constructor)::out,
		mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.
				
qualify_constructors([], [], Info, Info) --> [].
qualify_constructors([Ctor0 | Ctors0], [Ctor | Ctors], Info0, Info) -->
	{ Ctor0 = ctor(ExistQVars, Constraints0, SymName, Args0) },
	{ Ctor = ctor(ExistQVars, Constraints, SymName, Args) },
	qualify_constructor_arg_list(Args0, Args, Info0, Info1),
	qualify_constructors(Ctors0, Ctors, Info1, Info2),
	qualify_class_constraint_list(Constraints0, Constraints, Info2, Info).

	% Qualify the inst parameters of an inst definition.
:- pred qualify_inst_defn(inst_defn::in, inst_defn::out, mq_info::in,
	mq_info::out, io__state::di, io__state::uo) is det.

qualify_inst_defn(eqv_inst(Inst0), eqv_inst(Inst), Info0, Info) -->
	qualify_inst(Inst0, Inst, Info0, Info).	
qualify_inst_defn(abstract_inst, abstract_inst, Info, Info) --> [].

	% Qualify the mode parameter of an equivalence mode definition.
:- pred qualify_mode_defn(mode_defn::in, mode_defn::out, mq_info::in,
	mq_info::out, io__state::di, io__state::uo) is det.

qualify_mode_defn(eqv_mode(Mode0), eqv_mode(Mode), Info0, Info) -->
	qualify_mode(Mode0, Mode, Info0, Info).	

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
qualify_inst(free, free, Info, Info) --> [].
qualify_inst(not_reached, not_reached, Info, Info) --> [].
qualify_inst(free(_), _, _, _) -->
	{ error("compiler generated inst not expected") }.
qualify_inst(bound(Uniq, BoundInsts0), bound(Uniq, BoundInsts),
				Info0, Info) -->
	qualify_bound_inst_list(BoundInsts0, BoundInsts, Info0, Info).
qualify_inst(ground(Uniq, GroundInstInfo0), ground(Uniq, GroundInstInfo),
				Info0, Info) -->
	(
		{ GroundInstInfo0 = higher_order(pred_inst_info(A, Modes0,
				Det)) },
		qualify_mode_list(Modes0, Modes, Info0, Info),
		{ GroundInstInfo = higher_order(pred_inst_info(A, Modes, Det)) }
	;
		{ GroundInstInfo0 = none },
		{ GroundInstInfo = none },
		{ Info = Info0 }
	).
qualify_inst(inst_var(Var), inst_var(Var), Info, Info) --> [].
qualify_inst(constrained_inst_vars(Vars, Inst0),
		constrained_inst_vars(Vars, Inst), Info0, Info) -->
	qualify_inst(Inst0, Inst, Info0, Info).
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
	{ ConsId = cons(Name, Arity) ->
		Id = Name - Arity,
		update_recompilation_info(
			recompilation__record_used_item(functor, Id, Id),
			Info0, Info1)
	;
		Info1 = Info0
	},
	qualify_inst_list(Insts0, Insts, Info1, Info2),
	qualify_bound_inst_list(BoundInsts0, BoundInsts, Info2, Info).

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
	{ Type0 = term__functor(_, _, _) },
	( { type_to_ctor_and_args(Type0, TypeCtor0, Args0) } ->
		( { is_builtin_atomic_type(TypeCtor0) } ->
			{ TypeCtor = TypeCtor0 },
			{ Info1 = Info0 }
		; { type_ctor_is_higher_order(TypeCtor0, _, _) } ->
			{ TypeCtor = TypeCtor0 },
			{ Info1 = Info0 }
		; { type_ctor_is_tuple(TypeCtor0) } ->
			{ TypeCtor = TypeCtor0 },
			{ Info1 = Info0 }
		;
			{ mq_info_get_types(Info0, Types) },
			find_unique_match(TypeCtor0, TypeCtor, Types,
						type_id, Info0, Info1)
		),
		qualify_type_list(Args0, Args, Info1, Info2),
		{ construct_type(TypeCtor, Args, Type) }	
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
		% -- not yet:
		% StdLibraryModule = qualified(unqualified("std"), Typename),
		StdLibraryModule = unqualified(Typename),
		mq_info_set_module_used(Info2, StdLibraryModule, Info)
	;
		Info = Info2
	}.

	% Qualify the modes in a pragma c_code(...) decl.
:- pred qualify_pragma((pragma_type)::in, (pragma_type)::out,
		mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

qualify_pragma(X@source_file(_), X, Info, Info) --> [].
qualify_pragma(X@foreign_decl(_, _), X, Info, Info) --> [].
qualify_pragma(X@foreign_code(_, _), X, Info, Info) --> [].
qualify_pragma(X@foreign_type(_, _, _, _), X, Info, Info) --> [].
qualify_pragma(X@foreign_import_module(_, _), X, Info, Info) --> [].
qualify_pragma(
	    foreign_proc(Rec, SymName, PredOrFunc, PragmaVars0, Varset, Code),
	    foreign_proc(Rec, SymName, PredOrFunc, PragmaVars, Varset, Code), 
		Info0, Info) -->
	qualify_pragma_vars(PragmaVars0, PragmaVars, Info0, Info).
qualify_pragma(tabled(A, B, C, D, MModes0), tabled(A, B, C, D, MModes), 
	Info0, Info) --> 
	(
		{ MModes0 = yes(Modes0) }
	->
		qualify_mode_list(Modes0, Modes, Info0, Info),
		{ MModes = yes(Modes) }
	;
		{ Info = Info0 },
		{ MModes = no }
	).
qualify_pragma(X@inline(_, _), X, Info, Info) --> [].
qualify_pragma(X@no_inline(_, _), X, Info, Info) --> [].
qualify_pragma(X@obsolete(_, _), X, Info, Info) --> [].
qualify_pragma(import(Name, PredOrFunc, Modes0, Attributes, CFunc),
		import(Name, PredOrFunc, Modes, Attributes, CFunc),
		Info0, Info) -->
	qualify_mode_list(Modes0, Modes, Info0, Info).
qualify_pragma(export(Name, PredOrFunc, Modes0, CFunc),
		export(Name, PredOrFunc, Modes, CFunc), Info0, Info) -->
	qualify_mode_list(Modes0, Modes, Info0, Info).
qualify_pragma(X@unused_args(_, _, _, _, _), X, Info, Info) --> [].
qualify_pragma(type_spec(A, B, C, D, MaybeModes0, Subst0, G, H),
		type_spec(A, B, C, D, MaybeModes, Subst, G, H),
		Info0, Info) -->
	(
		{ MaybeModes0 = yes(Modes0) }
	->
		qualify_mode_list(Modes0, Modes, Info0, Info1),
		{ MaybeModes = yes(Modes) }
	;
		{ Info1 = Info0 },
		{ MaybeModes = no }
	),
	qualify_type_spec_subst(Subst0, Subst, Info1, Info).
qualify_pragma(X@fact_table(_, _, _), X, Info, Info) --> [].
qualify_pragma(X@aditi(_, _), X, Info, Info) --> [].
qualify_pragma(X@base_relation(_, _), X, Info, Info) --> [].
qualify_pragma(X@aditi_index(_, _, _), X, Info, Info) --> [].
qualify_pragma(X@supp_magic(_, _), X, Info, Info) --> [].
qualify_pragma(X@context(_, _), X, Info, Info) --> [].
qualify_pragma(X@aditi_memo(_, _), X, Info, Info) --> [].
qualify_pragma(X@aditi_no_memo(_, _), X, Info, Info) --> [].
qualify_pragma(X@naive(_, _), X, Info, Info) --> [].
qualify_pragma(X@psn(_, _), X, Info, Info) --> [].
qualify_pragma(X@owner(_, _, _), X, Info, Info) --> [].
qualify_pragma(X@promise_pure(_, _), X, Info, Info) --> [].
qualify_pragma(X@promise_semipure(_, _), X, Info, Info) --> [].
qualify_pragma(termination_info(PredOrFunc, SymName, ModeList0, Args, Term), 
		termination_info(PredOrFunc, SymName, ModeList, Args, Term), 
		Info0, Info) --> 
	qualify_mode_list(ModeList0, ModeList, Info0, Info).
qualify_pragma(X@terminates(_, _), X, Info, Info) --> [].
qualify_pragma(X@does_not_terminate(_, _), X, Info, Info) --> [].
qualify_pragma(X@check_termination(_, _), X, Info, Info) --> [].

:- pred qualify_pragma_vars(list(pragma_var)::in, list(pragma_var)::out,
		mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

qualify_pragma_vars([], [], Info, Info) --> [].
qualify_pragma_vars([pragma_var(Var, Name, Mode0) | PragmaVars0],
		[pragma_var(Var, Name, Mode) | PragmaVars], Info0, Info) -->
	qualify_mode(Mode0, Mode, Info0, Info1),
	qualify_pragma_vars(PragmaVars0, PragmaVars, Info1, Info).

:- pred qualify_type_spec_subst(assoc_list(tvar, type)::in,
		assoc_list(tvar, type)::out, mq_info::in, mq_info::out,
		io__state::di, io__state::uo) is det.

qualify_type_spec_subst([], [], Info, Info) --> [].
qualify_type_spec_subst([Var - Type0 |  Subst0], [Var - Type | Subst],
		Info0, Info) -->
	qualify_type(Type0, Type, Info0, Info1),
	qualify_type_spec_subst(Subst0, Subst, Info1, Info).

:- pred qualify_class_constraints(class_constraints::in,
	class_constraints::out, mq_info::in, mq_info::out, io__state::di,
	io__state::uo) is det. 

qualify_class_constraints(constraints(UnivCs0, ExistCs0),
			constraints(UnivCs, ExistCs), MQInfo0, MQInfo) -->
	qualify_class_constraint_list(UnivCs0, UnivCs, MQInfo0, MQInfo1),
	qualify_class_constraint_list(ExistCs0, ExistCs, MQInfo1, MQInfo).

:- pred qualify_class_constraint_list(list(class_constraint)::in,
	list(class_constraint)::out, mq_info::in, mq_info::out, io__state::di,
	io__state::uo) is det. 

qualify_class_constraint_list([], [], MQInfo, MQInfo) --> [].
qualify_class_constraint_list([C0|C0s], [C|Cs], MQInfo0, MQInfo) -->
	qualify_class_constraint(C0, C, MQInfo0, MQInfo1),
	qualify_class_constraint_list(C0s, Cs, MQInfo1, MQInfo).

:- pred qualify_class_constraint(class_constraint::in, class_constraint::out,
	mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

qualify_class_constraint(constraint(ClassName0, Types0), 
	constraint(ClassName, Types), MQInfo0, MQInfo) -->
	{ list__length(Types0, Arity) },
	qualify_class_name(ClassName0 - Arity, ClassName - _, MQInfo0, MQInfo1),
	qualify_type_list(Types0, Types, MQInfo1, MQInfo).

:- pred qualify_class_name(pair(class_name, arity)::in, 
	pair(class_name, arity)::out, mq_info::in, mq_info::out, 
	io__state::di, io__state::uo) is det.

qualify_class_name(Class0, Class, MQInfo0, MQInfo) -->
	{ mq_info_get_classes(MQInfo0, ClassIdSet) },
	find_unique_match(Class0, Class, ClassIdSet, class_id,
		MQInfo0, MQInfo).

:- pred qualify_class_interface(list(class_method)::in,
	list(class_method)::out, mq_info::in, mq_info::out,
	io__state::di, io__state::uo) is det. 

qualify_class_interface([], [], MQInfo, MQInfo) --> [].
qualify_class_interface([M0|M0s], [M|Ms], MQInfo0, MQInfo) -->
	qualify_class_method(M0, M, MQInfo0, MQInfo1),
	qualify_class_interface(M0s, Ms, MQInfo1, MQInfo).

:- pred qualify_class_method(class_method::in, class_method::out,
	mq_info::in, mq_info::out, io__state::di, io__state::uo) is det. 

	% There is no need to qualify the method name, since that is
	% done when the item is parsed.
qualify_class_method(
		pred_or_func(TypeVarset, InstVarset, ExistQVars, PredOrFunc,
			Name, TypesAndModes0, WithType0, WithInst0, MaybeDet,
			Cond, Purity, ClassContext0, Context), 
		pred_or_func(TypeVarset, InstVarset, ExistQVars, PredOrFunc,
			Name, TypesAndModes, WithType, WithInst, MaybeDet,
			Cond, Purity, ClassContext, Context), 
		MQInfo0, MQInfo
		) -->
	qualify_types_and_modes(TypesAndModes0, TypesAndModes, 
		MQInfo0, MQInfo1),
	qualify_class_constraints(ClassContext0, ClassContext, 
		MQInfo1, MQInfo2),
	map_fold2_maybe(qualify_type, WithType0, WithType, MQInfo2, MQInfo3),
	map_fold2_maybe(qualify_inst, WithInst0, WithInst, MQInfo3, MQInfo).
qualify_class_method(
		pred_or_func_mode(Varset, PredOrFunc, Name, Modes0,
			WithInst0, MaybeDet, Cond, Context), 
		pred_or_func_mode(Varset, PredOrFunc, Name, Modes,
			WithInst, MaybeDet, Cond, Context), 
		MQInfo0, MQInfo
		) -->
	qualify_mode_list(Modes0, Modes, MQInfo0, MQInfo1),
	map_fold2_maybe(qualify_inst, WithInst0, WithInst, MQInfo1, MQInfo).

:- pred qualify_instance_body(sym_name::in, instance_body::in, 
	instance_body::out) is det. 

qualify_instance_body(_ClassName, abstract, abstract).
qualify_instance_body(ClassName, concrete(M0s), concrete(Ms)) :-
	( ClassName = unqualified(_) ->
		Ms = M0s
	;
		sym_name_get_module_name(ClassName, unqualified(""), Module),
		Qualify = lambda([M0::in, M::out] is det, (
			M0 = instance_method(A, Method0, C, D, E),
			add_module_qualifier(Module, Method0, Method),
			M = instance_method(A, Method, C, D, E)
		)),
		list__map(Qualify, M0s, Ms)
	).

:- pred add_module_qualifier(sym_name::in, sym_name::in, sym_name::out) is det.

add_module_qualifier(Module, unqualified(SymName), qualified(Module, SymName)).
add_module_qualifier(DefaultModule, qualified(SymModule, SymName),
			qualified(Module, SymName)) :-
	( match_sym_name(SymModule, DefaultModule) ->
		Module = DefaultModule
	;
		% This case is an error.  The user must have written something
		% like
		%	:- instance foo:bar(some_type) where [
		%		pred(baz:p/1) is q
		%	].
		% where the module qualifier on the pred or func in the
		% instance (`baz:') does not match the qualifier for the 
		% class name (`foo:').
		%
		% We don't report the error here, we just leave the original
		% module qualifier intact so that the error can be reported
		% later on.

		Module = SymModule
	).

	% Find the unique match in the current name space for a given id
	% from a list of ids. If none exists, either because no match was
	% found or multiple matches were found, report an error.
	% This predicate assumes that type_ids, inst_ids, mode_ids and
	% class_ids have the same representation.
:- pred find_unique_match(id::in, id::out, id_set::in, id_type::in,
		mq_info::in, mq_info::out, io__state::di, io__state::uo) is det.

find_unique_match(Id0, Id, Ids, TypeOfId, Info0, Info) -->

	% Find all IDs which match the current id.
	{ Id0 = SymName0 - Arity },
	{ mq_info_get_modules(Info0, Modules) },
	{ id_set_search_sym_arity(Ids, SymName0, Arity, Modules,
		MatchingModules) },

	( { MatchingModules = [] } ->
		% No matches for this id.
		{ Id = Id0 },
		( { mq_info_get_report_error_flag(Info0, yes) } ->
			report_undefined(Info0, Id0, TypeOfId),
			{ mq_info_set_error_flag(Info0, TypeOfId, Info1) },
			{ mq_info_incr_errors(Info1, Info) }
		;
			{ Info = Info0 }
		)
	; { MatchingModules = [Module] } ->
		% A unique match for this ID.
		{ unqualify_name(SymName0, IdName) },
		{ Id = qualified(Module, IdName) - Arity },
		{ mq_info_set_module_used(Info0, Module, Info1) },
		{ ItemType = convert_simple_item_type(TypeOfId) },
		{ update_recompilation_info(
			recompilation__record_used_item(ItemType, Id0, Id),
			Info1, Info) }
	;
		% There are multiple matches.
		{ Id = Id0 },
		( { mq_info_get_report_error_flag(Info0, yes) } ->
			{ mq_info_get_error_context(Info0, ErrorContext) },
			report_ambiguous_match(ErrorContext, Id0, TypeOfId,
						MatchingModules),
			{ mq_info_set_error_flag(Info0, TypeOfId, Info1) },
			{ mq_info_incr_errors(Info1, Info) }
		;
			{ Info = Info0 }
		)
	).

:- pred update_recompilation_info(pred(recompilation_info, recompilation_info),
		mq_info, mq_info).
:- mode update_recompilation_info(pred(in, out) is det, in, out) is det.

update_recompilation_info(Pred, Info0, Info) :-
	mq_info_get_recompilation_info(Info0, MaybeRecompInfo0),
	(
		MaybeRecompInfo0 = yes(RecompInfo0),
		Pred(RecompInfo0, RecompInfo),
		mq_info_set_recompilation_info(Info0, yes(RecompInfo), Info)
	;
		MaybeRecompInfo0 = no,
		Info = Info0
	).

:- func convert_simple_item_type(id_type) = item_type.
:- mode convert_simple_item_type(in) = out is det.

convert_simple_item_type(type_id) = (type).
convert_simple_item_type(mode_id) = (mode).
convert_simple_item_type(inst_id) = (inst).
convert_simple_item_type(class_id) = (typeclass).
				
%------------------------------------------------------------------------------

:- type id_type --->
		type_id
	;	mode_id
	;	inst_id
	;	class_id.

:- type error_context == pair(error_context2, prog_context).

:- type id == pair(sym_name, int).

:- type error_context2 --->
		type(id) 
	;	inst(id)
	;	mode(id)
	;	pred_or_func(pred_or_func, id)
	; 	pred_or_func_mode(maybe(pred_or_func), id)
	;	(pragma)
	;	lambda_expr
	;	clause_mode_annotation
	;	type_qual
	;	class(id)
	;	instance(id).

	% Report an undefined type, inst or mode.
:- pred report_undefined(mq_info, pair(sym_name, int),
				id_type, io__state, io__state).
:- mode report_undefined(in, in, in, di, uo) is det.

report_undefined(Info, Id, IdType) -->
	{ mq_info_get_error_context(Info, ErrorContext - Context) },
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
	(
		%
		% if it is a qualified symbol, then check whether the module
		% specified has been imported
		%
		{ Id = qualified(ModuleName, _) - _Arity },
		{ mq_info_get_imported_modules(Info, ImportedModules) },
		{ \+ set__member(ModuleName, ImportedModules) },
		{ \+ ModuleName = Info^this_module }
	->
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("  (the module `"),
		mercury_output_bracketed_sym_name(ModuleName),
		io__write_string("' has not been imported).\n")
	;
		io__write_string(".\n")
	).

	% Report an error where a type, inst or mode had multiple possible
	% matches.
:- pred report_ambiguous_match(error_context::in, id::in, id_type::in,
		list(module_name)::in, io__state::di, io__state::uo) is det.

report_ambiguous_match(ErrorContext - Context, Id, IdType, Modules) -->
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
write_error_context2(pred_or_func(PredOrFunc, SymName - Arity)) -->
	io__write_string("definition of "),
	io__write(PredOrFunc),
	io__write_string(" "),
	{ adjust_func_arity(PredOrFunc, OrigArity, Arity) },
	write_id(SymName - OrigArity).
write_error_context2(pred_or_func_mode(MaybePredOrFunc, SymName - Arity)) -->
	io__write_string("mode declaration for "),
	( 
		{ MaybePredOrFunc = yes(PredOrFunc) },
		io__write(PredOrFunc),
		io__write_string(" "),
		{ adjust_func_arity(PredOrFunc, OrigArity, Arity) }
	;
		{ MaybePredOrFunc = no },
		{ OrigArity = Arity }
	),
	write_id(SymName - OrigArity).
write_error_context2(lambda_expr) -->
	io__write_string("mode declaration for lambda expression").
write_error_context2(clause_mode_annotation) -->
	io__write_string("clause mode annotation").
write_error_context2(pragma) -->
	io__write_string("pragma").
write_error_context2(type_qual) -->
	io__write_string("explicit type qualification").
write_error_context2(class(Id)) -->
	io__write_string("declaration of typeclass "),
	write_id(Id).
write_error_context2(instance(Id)) -->
	io__write_string("declaration of instance of typeclass "),
	write_id(Id).

:- pred id_type_to_string(id_type::in, string::out) is det.

id_type_to_string(type_id, "type").
id_type_to_string(mode_id, "mode").
id_type_to_string(inst_id, "inst").
id_type_to_string(class_id, "typeclass").

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
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
	(
		{ UnusedImports = []
		; Warn = no
		}
	->
		[]
	;
		module_name_to_file_name(ModuleName, ".m", no, FileName),
		{ term__context_init(FileName, 1, Context) },
		prog_out__write_context(Context),
		io__write_string("In module `"),
		prog_out__write_sym_name(ModuleName),
		io__write_string("':\n"),
		prog_out__write_context(Context),
		io__write_string("  warning: "),
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
		io__write_string("  used in the interface.\n"),
		(
			{ HaltAtWarn = yes }
		->
			io__set_exit_status(1)
		;
			[]
		)
	).

:- pred is_or_are(list(T)::in, string::out) is det.

is_or_are([], "") :- error("module_qual:is_or_are").
is_or_are([_], "is").
is_or_are([_, _ | _], "are").

	% Output an error message about an ill-formed type.
:- pred report_invalid_type(type, error_context, io__state, io__state).
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

	% is_builtin_atomic_type(TypeCtor)
	%	is true iff 'TypeCtor' is the type_ctor of a builtin atomic type

:- pred is_builtin_atomic_type(type_ctor).
:- mode is_builtin_atomic_type(in) is semidet.

is_builtin_atomic_type(unqualified("int") - 0).
is_builtin_atomic_type(unqualified("float") - 0).
is_builtin_atomic_type(unqualified("string") - 0).
is_builtin_atomic_type(unqualified("character") - 0).

%-----------------------------------------------------------------------------%
% Access and initialisation predicates.

:- pred init_mq_info(item_list::in, globals::in, bool::in, module_name::in,
	mq_info::out) is det.

init_mq_info(Items, Globals, ReportErrors, ModuleName, Info0) :-
	term__context_init(Context),
	ErrorContext = type(unqualified("") - 0) - Context,
	set__init(InterfaceModules0),
	get_implicit_dependencies(Items, Globals, ImportDeps, UseDeps),
	set__list_to_set(ImportDeps `list__append` UseDeps, ImportedModules),
	id_set_init(Empty),
	globals__lookup_bool_option(Globals, smart_recompilation,
		SmartRecompilation),
	(
		SmartRecompilation = no,
		MaybeRecompInfo = no
	;
		SmartRecompilation = yes,
		MaybeRecompInfo = yes(init_recompilation_info(ModuleName))
	),
	Info0 = mq_info(ImportedModules, Empty, Empty, Empty, Empty,
			Empty, InterfaceModules0, local, 0, no, no,
			ReportErrors, ErrorContext, ModuleName,
			may_be_unqualified, MaybeRecompInfo).

:- pred mq_info_get_imported_modules(mq_info::in, set(module_name)::out) is det.
:- pred mq_info_get_modules(mq_info::in, module_id_set::out) is det.
:- pred mq_info_get_types(mq_info::in, type_id_set::out) is det.
:- pred mq_info_get_insts(mq_info::in, inst_id_set::out) is det.
:- pred mq_info_get_modes(mq_info::in, mode_id_set::out) is det.
:- pred mq_info_get_classes(mq_info::in, class_id_set::out) is det.
:- pred mq_info_get_unused_interface_modules(mq_info::in,
					set(module_name)::out) is det.
:- pred mq_info_get_import_status(mq_info::in, import_status::out) is det.
% :- pred mq_info_get_num_errors(mq_info::in, int::out) is det.
% :- pred mq_info_get_type_error_flag(mq_info::in, bool::out) is det.
% :- pred mq_info_get_mode_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_report_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_error_context(mq_info::in, error_context::out) is det.

mq_info_get_imported_modules(MQInfo, MQInfo^imported_modules).
mq_info_get_modules(MQInfo, MQInfo^modules).
mq_info_get_types(MQInfo, MQInfo^types).
mq_info_get_insts(MQInfo, MQInfo^insts).
mq_info_get_modes(MQInfo, MQInfo^modes).
mq_info_get_classes(MQInfo, MQInfo^classes).
mq_info_get_unused_interface_modules(MQInfo, MQInfo^unused_interface_modules).
mq_info_get_import_status(MQInfo, MQInfo^import_status).
mq_info_get_num_errors(MQInfo, MQInfo^num_errors).
mq_info_get_type_error_flag(MQInfo, MQInfo^type_error_flag).
mq_info_get_mode_error_flag(MQInfo, MQInfo^mode_error_flag).
mq_info_get_report_error_flag(MQInfo, MQInfo^report_error_flag).
mq_info_get_error_context(MQInfo, MQInfo^error_context).
mq_info_get_need_qual_flag(MQInfo, MQInfo^need_qual_flag).
mq_info_get_recompilation_info(Info, Info ^ maybe_recompilation_info).

:- pred mq_info_set_imported_modules(mq_info::in, set(module_name)::in,
		mq_info::out) is det.
:- pred mq_info_set_modules(mq_info::in, module_id_set::in, mq_info::out)
		is det.
:- pred mq_info_set_types(mq_info::in, type_id_set::in, mq_info::out) is det.
:- pred mq_info_set_insts(mq_info::in, inst_id_set::in, mq_info::out) is det.
:- pred mq_info_set_modes(mq_info::in, mode_id_set::in, mq_info::out) is det.
:- pred mq_info_set_classes(mq_info::in, class_id_set::in, mq_info::out) is det.
:- pred mq_info_set_unused_interface_modules(mq_info::in, set(module_name)::in,
						mq_info::out) is det.
:- pred mq_info_set_import_status(mq_info::in, import_status::in,
						mq_info::out) is det.
:- pred mq_info_set_type_error_flag(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_mode_error_flag(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_error_context(mq_info::in, error_context::in,
						mq_info::out) is det.

mq_info_set_imported_modules(MQInfo,
		ImportedModules, MQInfo^imported_modules := ImportedModules).
mq_info_set_modules(MQInfo, Modules, MQInfo^modules := Modules).
mq_info_set_types(MQInfo, Types, MQInfo^types := Types).
mq_info_set_insts(MQInfo, Insts, MQInfo^insts := Insts).
mq_info_set_modes(MQInfo, Modes, MQInfo^modes := Modes).
mq_info_set_classes(MQInfo, Classes, MQInfo^classes := Classes).
mq_info_set_unused_interface_modules(MQInfo, 
		Modules, MQInfo^unused_interface_modules := Modules).
mq_info_set_import_status(MQInfo, Status, MQInfo^import_status := Status).
mq_info_set_type_error_flag(MQInfo, MQInfo^type_error_flag := yes).
mq_info_set_mode_error_flag(MQInfo, MQInfo^mode_error_flag := yes).
mq_info_set_error_context(MQInfo, Context, MQInfo^error_context := Context).
mq_info_set_need_qual_flag(MQInfo, Flag, MQInfo^need_qual_flag := Flag).
mq_info_set_recompilation_info(Info, RecompInfo,
		Info ^ maybe_recompilation_info := RecompInfo).

:- pred mq_info_incr_errors(mq_info::in, mq_info::out) is det.

mq_info_incr_errors(MQInfo, MQInfo^num_errors := (MQInfo^num_errors +1)).

:- pred mq_info_set_error_flag(mq_info::in, id_type::in, mq_info::out) is det.

mq_info_set_error_flag(Info0, IdType, Info) :-
	mq_info_set_error_flag_2(Info0, IdType, Info).

:- pred mq_info_set_error_flag_2(mq_info::in,
		id_type::in, mq_info::out) is det.

mq_info_set_error_flag_2(Info0, type_id, Info) :-
	mq_info_set_type_error_flag(Info0, Info).
mq_info_set_error_flag_2(Info0, mode_id, Info) :-
	mq_info_set_mode_error_flag(Info0, Info).
mq_info_set_error_flag_2(Info0, inst_id, Info) :-
	mq_info_set_mode_error_flag(Info0, Info).
mq_info_set_error_flag_2(Info0, class_id, Info) :-
	mq_info_set_type_error_flag(Info0, Info).

	% If the current item is in the interface, remove its module 
	% name from the list of modules not used in the interface
	% (and if the module name is itself module-qualified,
	% recursively mark its parent module as used).
:- pred mq_info_set_module_used(mq_info::in, module_name::in,
						mq_info::out) is det.

mq_info_set_module_used(Info0, Module, Info) :-
	( mq_info_get_import_status(Info0, exported) ->
		mq_info_get_unused_interface_modules(Info0, Modules0),
		set__delete(Modules0, Module, Modules),
		mq_info_set_unused_interface_modules(Info0, Modules, Info1),
		(
			Module = qualified(ParentModule, _),
			mq_info_set_module_used(Info1, ParentModule, Info)
		;
			Module = unqualified(_),
			Info = Info1
		)
	;
		Info = Info0
	).

	% Add to the list of modules imported in the interface and not used.
:- pred mq_info_add_unused_interface_modules(mq_info::in, list(module_name)::in,
						mq_info::out) is det.

mq_info_add_unused_interface_modules(Info0, NewModules, Info) :-
	mq_info_get_unused_interface_modules(Info0, Modules0),
	set__insert_list(Modules0, NewModules, Modules),
	mq_info_set_unused_interface_modules(Info0, Modules, Info).

	% Add to the list of imported modules.
:- pred mq_info_add_imported_modules(mq_info::in, list(module_name)::in,
						mq_info::out) is det.

mq_info_add_imported_modules(Info0, NewModules, Info) :-
	mq_info_get_imported_modules(Info0, Modules0),
	set__insert_list(Modules0, NewModules, Modules),
	mq_info_set_imported_modules(Info0, Modules, Info).

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
:- type class_id_set == id_set.
	% Modules don't have an arity, but for simplicity we use the same
	% data structure here, assigning arity zero to all module names.
:- type module_id_set == id_set.

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

:- pred id_set_search_sym_arity(id_set::in, sym_name::in, int::in,
		module_id_set::in, list(module_name)::out) is det.

id_set_search_sym_arity(IdSet, Sym, Arity, Modules, MatchingModules) :-
	unqualify_name(Sym, UnqualName),
	(
		map__search(IdSet, UnqualName - Arity,
			ImportModules - UseModules)
	->
		(
			Sym = unqualified(_),
			set__to_sorted_list(ImportModules, MatchingModules)
		;
			Sym = qualified(Module, _),

			%
			% first, compute the set of modules that this
			% module specifier could possibly refer to
			%

			% do a recursive search to find nested modules
			% which match the specified module name
			ModuleArity = 0,
			id_set_search_sym_arity(Modules, Module, ModuleArity,
				Modules, MatchingParentModules),
			unqualify_name(Module, UnqualModule),
			AppendModuleName = (pred(X::in, Y::out) is det :-
					Y = qualified(X, UnqualModule)),
			list__map(AppendModuleName,
				MatchingParentModules,
				MatchingNestedModules),

			% add the specified module name itself, in case
			% it refers to a top-level (unnested) module name,
			% since top-level modules don't get inserted into
			% the module_id_set.
			AllMatchingModules = [Module | MatchingNestedModules],

			%
			% second, compute the set of modules that define
			% this symbol 
			%
			set__union(ImportModules, UseModules, DefiningModules),

			%
			% third, take the intersection of the sets computed
			% in the first two steps
			%
			FindMatch =
				lambda([MatchModule::out] is nondet, (
				    list__member(MatchModule,
				    	AllMatchingModules),
				    set__member(MatchModule, DefiningModules)
				)),
			solutions(FindMatch, MatchingModules)
		)
	;
		MatchingModules = []
	).

%-----------------------------------------------------------------------------%

get_partial_qualifiers(ModuleName, PartialQualInfo, PartialQualifiers) :-
	PartialQualInfo = partial_qualifier_info(ModuleIdSet),
	(
		ModuleName = unqualified(_),
		PartialQualifiers = []
	;
		ModuleName = qualified(Parent, Child),
		get_partial_qualifiers_2(Parent, unqualified(Child),
			ModuleIdSet, [], PartialQualifiers)
	).

:- pred get_partial_qualifiers_2(module_name, module_name, module_id_set,
		list(module_name), list(module_name)).
:- mode get_partial_qualifiers_2(in, in, in, in, out) is det.

get_partial_qualifiers_2(ImplicitPart, ExplicitPart, ModuleIdSet,
		Qualifiers0, Qualifiers) :-
	%
	% if the ImplicitPart module was imported, rather than just being
	% used, then insert the ExplicitPart module into the list of
	% valid partial qualifiers.
	%
	( parent_module_is_imported(ImplicitPart, ExplicitPart, ModuleIdSet) ->
		Qualifiers1 = [ExplicitPart | Qualifiers0]
	;
		Qualifiers1 = Qualifiers0
	),
	%
	% recursively try to add the other possible partial qualifiers
	%
	( ImplicitPart = qualified(Parent, Child) ->
		NextImplicitPart = Parent,
		insert_module_qualifier(Child, ExplicitPart, NextExplicitPart),
		get_partial_qualifiers_2(NextImplicitPart, NextExplicitPart,
			ModuleIdSet, Qualifiers1, Qualifiers)
	;
		Qualifiers = Qualifiers1
	).

	% Check whether the parent module was imported, given the name of a
	% child (or grandchild, etc.) module occurring in that parent module.
	%
:- pred parent_module_is_imported(module_name, module_name, module_id_set).
:- mode parent_module_is_imported(in, in, in) is semidet.

parent_module_is_imported(ParentModule, ChildModule, ModuleIdSet) :-
	% Find the module name at the start of the ChildModule;
	% this sub-module will be a direct sub-module of ParentModule
	get_first_module_name(ChildModule, DirectSubModuleName),

	% Check that the ParentModule was imported.
	% We do this by looking up the definitions for the direct sub-module
	% and checking that the one in ParentModule came from an
	% imported module.
	Arity = 0,
	map__search(ModuleIdSet, DirectSubModuleName - Arity,
			ImportModules - _UseModules),
	set__member(ParentModule, ImportModules).

	% Given a module name, possibly module-qualified,
	% return the name of the first module in the qualifier list.
	% e.g. given `foo:bar:baz', this returns `foo',
	% and given just `baz', it returns `baz'.
	%
:- pred get_first_module_name(module_name, string).
:- mode get_first_module_name(in, out) is det.

get_first_module_name(unqualified(ModuleName), ModuleName).
get_first_module_name(qualified(Parent, _), ModuleName) :-
	get_first_module_name(Parent, ModuleName).

%----------------------------------------------------------------------------%
