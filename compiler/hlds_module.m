%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines the part of the High Level Data Structure or HLDS
% that deals with issues that are wider than a single predicate.

% The main data structures defined here are the types
%
%	module_info
%	dependency_info
%	predicate_table
%
% There is a separate interface section for each of these.

% Main authors: fjh, conway.

:- module hlds__hlds_module.

:- interface.

:- import_module analysis.
:- import_module backend_libs__foreign.
:- import_module check_hlds__unify_proc.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_pred.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module parse_tree__module_qual.
:- import_module parse_tree__prog_data.
:- import_module recompilation.

:- import_module relation, map, std_util, list, set, multi_map.

:- implementation.

:- import_module check_hlds__typecheck.
:- import_module hlds__hlds_out.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module transform_hlds__mmc_analysis.

:- import_module bool, require, int, string.

%-----------------------------------------------------------------------------%

:- interface.

:- type module_info.

:- type foreign_code_info
	--->	foreign_code_info(
			foreign_decl_info,
			foreign_body_info
		).

:- type pragma_exported_proc	
	--->	pragma_exported_proc(
			pred_id,
			proc_id,
			string,	% the name of the C function
			prog_context
		).

	% This structure contains the information we need to generate
	% a type_ctor_info structure for a type defined in this module.

:- type type_ctor_gen_info
	--->	type_ctor_gen_info(
			type_ctor,
			module_name,	% module name
			string,		% type name
			int,		% type arity
			import_status,	% of the type
			hlds_type_defn,	% defn of type
			pred_proc_id,	% unify procedure
			pred_proc_id	% compare procedure
			% maybe(pred_proc_id)	% prettyprinter, if relevant
		).

	% map from proc to a list of unused argument numbers.
:- type unused_arg_info == map(pred_proc_id, list(int)).

	% List of procedures for which there are user-requested type
	% specializations, and a list of predicates which should be
	% processed by higher_order.m to ensure the production of those
	% versions.
:- type type_spec_info
	---> type_spec_info(
		set(pred_proc_id),	% Procedures for which there are
					% user-requested type specializations.
		set(pred_id),		% Set of procedures which need to be
					% processed by higher_order.m to
					% produce those specialized versions.
		multi_map(pred_id, pred_id),
					% Map from predicates for which the
					% user requested a type specialization
					% to the list of predicates which must
					% be processed by higher_order.m to
					% force the production of those
					% versions. This is used by
					% dead_proc_elim.m to avoid creating
					% versions unnecessarily for versions
					% in imported modules.
		multi_map(pred_id, pragma_type)
					% Type spec pragmas to be placed in
					% the `.opt' file if a predicate
					% becomes exported.
	).

	% This field should be set to `do_aditi_compilation' if there
	% are local Aditi predicates.
:- type do_aditi_compilation
	--->    do_aditi_compilation
	;       no_aditi_compilation.

%-----------------------------------------------------------------------------%

	% Various predicates for manipulating the module_info data structure

	% Create an empty module_info for a given module name (and the
	% global options).  The item_list is passed so that we can
	% call get_implicit_dependencies to figure out whether to
	% import `table_builtin', but the items are not inserted into
	% the module_info.
	%
:- pred module_info_init(module_name, item_list, globals,
		partial_qualifier_info, maybe(recompilation_info), module_info).
:- mode module_info_init(in, in, in, in, in, out) is det.

:- pred module_info_get_predicate_table(module_info, predicate_table).
:- mode module_info_get_predicate_table(in, out) is det.

:- pred module_info_set_predicate_table(module_info, predicate_table,
	module_info).
:- mode module_info_set_predicate_table(in, in, out) is det.

	% For an explanation of the proc_requests structure,
	% see unify_proc.m.
:- pred module_info_get_proc_requests(module_info, proc_requests).
:- mode module_info_get_proc_requests(in, out) is det.

:- pred module_info_get_special_pred_map(module_info, special_pred_map).
:- mode module_info_get_special_pred_map(in, out) is det.

:- pred module_info_set_special_pred_map(module_info, special_pred_map,
	module_info).
:- mode module_info_set_special_pred_map(in, in, out) is det.

:- pred module_info_get_partial_qualifier_info(module_info,
	partial_qualifier_info).
:- mode module_info_get_partial_qualifier_info(in, out) is det.

:- pred module_info_set_partial_qualifier_info(module_info,
	partial_qualifier_info, module_info).
:- mode module_info_set_partial_qualifier_info(in, in, out) is det.

:- pred module_info_types(module_info, type_table).
:- mode module_info_types(in, out) is det.

:- pred module_info_set_types(module_info, type_table, module_info).
:- mode module_info_set_types(in, in, out) is det.

:- pred module_info_insts(module_info, inst_table).
:- mode module_info_insts(in, out) is det.

:- pred module_info_set_insts(module_info, inst_table, module_info).
:- mode module_info_set_insts(in, in, out) is det.

:- pred module_info_modes(module_info, mode_table).
:- mode module_info_modes(in, out) is det.

:- pred module_info_set_modes(module_info, mode_table, module_info).
:- mode module_info_set_modes(in, in, out) is det.

:- pred module_info_ctors(module_info, cons_table).
:- mode module_info_ctors(in, out) is det.

:- pred module_info_set_ctors(module_info, cons_table, module_info).
:- mode module_info_set_ctors(in, in, out) is det.

:- pred module_info_classes(module_info, class_table).
:- mode module_info_classes(in, out) is det.

:- pred module_info_set_classes(module_info, class_table, module_info).
:- mode module_info_set_classes(in, in, out) is det.

:- pred module_info_instances(module_info, instance_table).
:- mode module_info_instances(in, out) is det.

:- pred module_info_set_instances(module_info, instance_table, module_info).
:- mode module_info_set_instances(in, in, out) is det.

:- pred module_info_superclasses(module_info, superclass_table).
:- mode module_info_superclasses(in, out) is det.

:- pred module_info_set_superclasses(module_info, superclass_table,
	module_info).
:- mode module_info_set_superclasses(in, in, out) is det.

:- pred module_info_assertion_table(module_info, assertion_table).
:- mode module_info_assertion_table(in, out) is det.

:- pred module_info_set_assertion_table(module_info, assertion_table,
	module_info).
:- mode module_info_set_assertion_table(in, in, out) is det.

:- pred module_info_exclusive_table(module_info, exclusive_table).
:- mode module_info_exclusive_table(in, out) is det.

:- pred module_info_set_exclusive_table(module_info, exclusive_table, 
	module_info).
:- mode module_info_set_exclusive_table(in, in, out) is det.

:- pred module_info_ctor_field_table(module_info, ctor_field_table).
:- mode module_info_ctor_field_table(in, out) is det.

:- pred module_info_set_ctor_field_table(module_info,
	ctor_field_table, module_info).
:- mode module_info_set_ctor_field_table(in, in, out) is det.

:- pred module_info_get_maybe_recompilation_info(module_info,
		maybe(recompilation_info)).
:- mode module_info_get_maybe_recompilation_info(in, out) is det.

:- pred module_info_set_maybe_recompilation_info(module_info,
		maybe(recompilation_info), module_info).
:- mode module_info_set_maybe_recompilation_info(in, in, out) is det.

:- pred module_add_imported_module_specifiers(list(module_specifier),
		module_info, module_info).
:- mode module_add_imported_module_specifiers(in, in, out) is det.

:- pred module_info_get_imported_module_specifiers(module_info,
		set(module_specifier)).
:- mode module_info_get_imported_module_specifiers(in, out) is det.

:- pred module_add_indirectly_imported_module_specifiers(
		list(module_specifier), module_info, module_info).
:- mode module_add_indirectly_imported_module_specifiers(in, in, out) is det.

:- pred module_info_get_indirectly_imported_module_specifiers(module_info,
		set(module_specifier)).
:- mode module_info_get_indirectly_imported_module_specifiers(in, out) is det.

	% The visible modules are the current module, any
	% imported modules, any ancestor modules and any
	% modules imported by ancestor modules.
	% It excludes transitively imported modules (those
	% for which we read `.int2' files).
:- pred visible_module(module_name, module_info).
:- mode visible_module(out, in) is multi.

	% This returns all the modules that this module's code depends on,
	% i.e. all modules that have been used or imported by this module,
	% directly or indirectly, including parent modules.
:- pred module_info_get_all_deps(module_info, set(module_name)).
:- mode module_info_get_all_deps(in, out) is det.

%-----------------------------------------------------------------------------%

:- pred module_info_name(module_info, module_name).
:- mode module_info_name(in, out) is det.

:- pred module_info_globals(module_info, globals).
:- mode module_info_globals(in, out) is det.

:- pred module_info_set_globals(module_info, globals, module_info).
:- mode module_info_set_globals(in, in, out) is det.

:- pred module_info_contains_foreign_type(module_info).
:- mode module_info_contains_foreign_type(in) is semidet.

:- pred module_info_contains_foreign_type(module_info, module_info).
:- mode module_info_contains_foreign_type(in, out) is det.

:- pred module_info_get_foreign_decl(module_info, foreign_decl_info).
:- mode module_info_get_foreign_decl(in, out) is det.

:- pred module_info_set_foreign_decl(module_info, 
		foreign_decl_info, module_info).
:- mode module_info_set_foreign_decl(in, in, out) is det.

:- pred module_info_get_foreign_body_code(module_info, foreign_body_info).
:- mode module_info_get_foreign_body_code(in, out) is det.

:- pred module_info_set_foreign_body_code(module_info,
		foreign_body_info, module_info).
:- mode module_info_set_foreign_body_code(in, in, out) is det.

:- pred module_info_get_foreign_import_module(module_info,
		foreign_import_module_info).
:- mode module_info_get_foreign_import_module(in, out) is det.

:- pred module_info_set_foreign_import_module(module_info, 
		foreign_import_module_info, module_info).
:- mode module_info_set_foreign_import_module(in, in, out) is det.

:- pred module_add_foreign_decl(foreign_language, string, prog_context,
		module_info, module_info).
:- mode module_add_foreign_decl(in, in, in, in, out) is det.

:- pred module_add_foreign_import_module(foreign_language,
		module_name, prog_context, module_info, module_info).
:- mode module_add_foreign_import_module(in, in, in, in, out) is det.

:- pred module_add_foreign_body_code(foreign_language, string, prog_context,
		module_info, module_info).
:- mode module_add_foreign_body_code(in, in, in, in, out) is det.

	% Please see module_info_ensure_dependency_info for the
	% constraints on this dependency_info.
:- pred module_info_get_maybe_dependency_info(module_info,
	maybe(dependency_info)).
:- mode module_info_get_maybe_dependency_info(in, out) is det.

:- pred module_info_num_errors(module_info, int).
:- mode module_info_num_errors(in, out) is det.

:- pred module_info_unused_arg_info(module_info, unused_arg_info).
:- mode module_info_unused_arg_info(in, out) is det.

:- pred module_info_set_proc_requests(module_info, proc_requests,
	module_info).
:- mode module_info_set_proc_requests(in, in, out) is det.

:- pred module_info_set_unused_arg_info(module_info,
		unused_arg_info, module_info).
:- mode module_info_set_unused_arg_info(in, in, out) is det.

:- pred module_info_set_num_errors(module_info, int, module_info).
:- mode module_info_set_num_errors(in, in, out) is det.

:- pred module_info_get_pragma_exported_procs(module_info,
	list(pragma_exported_proc)).
:- mode module_info_get_pragma_exported_procs(in, out) is det.

:- pred module_info_set_pragma_exported_procs(module_info,
	list(pragma_exported_proc), module_info).
:- mode module_info_set_pragma_exported_procs(in, in, out) is det.

:- pred module_info_type_ctor_gen_infos(module_info, list(type_ctor_gen_info)).
:- mode module_info_type_ctor_gen_infos(in, out) is det.

:- pred module_info_set_type_ctor_gen_infos(module_info,
	list(type_ctor_gen_info), module_info).
:- mode module_info_set_type_ctor_gen_infos(in, in, out) is det.

:- pred module_info_stratified_preds(module_info, set(pred_id)).
:- mode module_info_stratified_preds(in, out) is det.

:- pred module_info_set_stratified_preds(module_info, set(pred_id),
	module_info).
:- mode module_info_set_stratified_preds(in, in, out) is det.

:- pred module_info_get_do_aditi_compilation(module_info,
		do_aditi_compilation).
:- mode module_info_get_do_aditi_compilation(in, out) is det.

:- pred module_info_set_do_aditi_compilation(module_info, module_info).
:- mode module_info_set_do_aditi_compilation(in, out) is det.

:- pred module_info_type_spec_info(module_info, type_spec_info).
:- mode module_info_type_spec_info(in, out) is det.

:- pred module_info_set_type_spec_info(module_info,
		type_spec_info, module_info).
:- mode module_info_set_type_spec_info(in, in, out) is det.

:- pred module_info_no_tag_types(module_info, no_tag_type_table).
:- mode module_info_no_tag_types(in, out) is det.

:- pred module_info_set_no_tag_types(module_info,
		no_tag_type_table, module_info).
:- mode module_info_set_no_tag_types(in, in, out) is det.

:- pred module_info_analysis_info(module_info, analysis_info).
:- mode module_info_analysis_info(in, out) is det.

:- pred module_info_set_analysis_info(module_info,
		analysis_info, module_info).
:- mode module_info_set_analysis_info(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred module_info_preds(module_info, pred_table).
:- mode module_info_preds(in, out) is det.

:- pred module_info_pred_info(module_info, pred_id, pred_info).
:- mode module_info_pred_info(in, in, out) is det.

	% Given a pred_id and a proc_id, get the
	% pred_info that predicate and the proc_info for that
	% mode of that predicate.
:- pred module_info_pred_proc_info(module_info, pred_id, proc_id,
	pred_info, proc_info).
:- mode module_info_pred_proc_info(in, in, in, out, out) is det.

:- pred module_info_pred_proc_info(module_info, pred_proc_id,
	pred_info, proc_info).
:- mode module_info_pred_proc_info(in, in, out, out) is det.

	% Return a list of the pred_ids of all the "valid" predicates.
	% (Predicates whose definition contains a type error, etc.
	% get removed from this list, so that later passes can rely
	% on the predicates in this list being type-correct, etc.)
:- pred module_info_predids(module_info, list(pred_id)).
:- mode module_info_predids(in, out) is det.

	% Reverse the list of pred_ids.
	% (The list is built up by inserting values at the front,
	% for efficiency; once we've done so, we reverse the list
	% so that progress messages and error messages come out
	% in the expected order.)
:- pred module_info_reverse_predids(module_info, module_info).
:- mode module_info_reverse_predids(in, out) is det.

	% Remove a predicate from the list of pred_ids, to prevent
	% further processing of this predicate after an error is encountered.
:- pred module_info_remove_predid(module_info, pred_id, module_info).
:- mode module_info_remove_predid(in, in, out) is det.

	% Completely remove a predicate from a module.
:- pred module_info_remove_predicate(pred_id, module_info, module_info).
:- mode module_info_remove_predicate(in, in, out) is det.

:- pred module_info_set_preds(module_info, pred_table, module_info).
:- mode module_info_set_preds(in, in, out) is det.

:- pred module_info_set_pred_info(module_info, pred_id, pred_info, module_info).
:- mode module_info_set_pred_info(in, in, in, out) is det.

:- pred module_info_set_pred_proc_info(module_info,
	pred_id, proc_id, pred_info, proc_info, module_info).
:- mode module_info_set_pred_proc_info(in, in, in, in, in, out) is det.

:- pred module_info_set_pred_proc_info(module_info,
	pred_proc_id, pred_info, proc_info, module_info).
:- mode module_info_set_pred_proc_info(in, in, in, in, out) is det.

:- pred module_info_typeids(module_info, list(type_ctor)).
:- mode module_info_typeids(in, out) is det.

:- pred module_info_instids(module_info, list(inst_id)).
:- mode module_info_instids(in, out) is det.

:- pred module_info_modeids(module_info, list(mode_id)).
:- mode module_info_modeids(in, out) is det.

:- pred module_info_consids(module_info, list(cons_id)).
:- mode module_info_consids(in, out) is det.

	% Please see module_info_ensure_dependency_info for the
	% constraints on this dependency_info.
:- pred module_info_dependency_info(module_info, dependency_info).
:- mode module_info_dependency_info(in, out) is det.

:- pred module_info_aditi_dependency_ordering(module_info, 
		aditi_dependency_ordering).
:- mode module_info_aditi_dependency_ordering(in, out) is det.

	% Please see module_info_ensure_dependency_info for the
	% constraints on this dependency_info.
:- pred module_info_set_dependency_info(module_info, dependency_info,
	module_info).
:- mode module_info_set_dependency_info(in, in, out) is det.

:- pred module_info_clobber_dependency_info(module_info, module_info).
:- mode module_info_clobber_dependency_info(in, out) is det.

:- pred module_info_incr_errors(module_info, module_info).
:- mode module_info_incr_errors(in, out) is det.

	% The module_info stores a counter which is used to number
	% introduced lambda predicates as __LambdaGoal__1, __LambdaGoal__2,
	% etc.; this predicate returns the next number and increments
	% the counter.
:- pred module_info_next_lambda_count(module_info, int, module_info).
:- mode module_info_next_lambda_count(in, out, out) is det.

:- pred module_info_next_model_non_pragma_count(module_info, int, module_info).
:- mode module_info_next_model_non_pragma_count(in, out, out) is det.

	% Once the module_info has been built, we call module_info_optimize
	% to attempt to optimize the data structures for lots of accesses
	% and relatively few insertion/deletions. (This was useful when
	% we were using unbalanced binary trees, but now that we are using
	% 234-trees, it is a no-op, except for the mode and inst tables,
	% where the cached lists of mode_ids and inst_ids are sorted for
	% efficient conversion to sets in module_qual.m.)
:- pred module_info_optimize(module_info, module_info).
:- mode module_info_optimize(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module counter.

:- pred module_info_get_lambda_counter(module_info, counter).
:- mode module_info_get_lambda_counter(in, out) is det.

:- pred module_info_set_lambda_counter(module_info, counter, module_info).
:- mode module_info_set_lambda_counter(in, in, out) is det.

:- pred module_info_get_model_non_pragma_counter(module_info, counter).
:- mode module_info_get_model_non_pragma_counter(in, out) is det.

:- pred module_info_set_model_non_pragma_counter(module_info, counter,
	module_info).
:- mode module_info_set_model_non_pragma_counter(in, in, out) is det.

:- pred module_info_set_maybe_dependency_info(module_info,
	maybe(dependency_info), module_info).
:- mode module_info_set_maybe_dependency_info(in, in, out) is det.

:- type module_info --->
	module(
		sub_info ::			module_sub_info,
		predicate_table ::		predicate_table,
		proc_requests :: 		proc_requests,
		special_pred_map ::		special_pred_map,
		partial_qualifier_info ::	partial_qualifier_info,
		type_table ::			type_table,
		inst_table ::			inst_table,
		mode_table ::			mode_table,
		cons_table ::			cons_table,
		class_table ::			class_table,
		instance_table ::		instance_table,
		superclass_table ::		superclass_table,
		assertion_table ::		assertion_table,
		exclusive_table ::		exclusive_table,
		ctor_field_table ::		ctor_field_table,
		maybe_recompilation_info ::	maybe(recompilation_info)
	).

:- type module_sub_info --->
	module_sub(
		module_name ::			module_name,
		globals ::			globals,
		contains_foreign_type ::	bool,
		foreign_decl_info ::		foreign_decl_info,
		foreign_body_info ::		foreign_body_info,
		foreign_import_module_info ::	foreign_import_module_info,
			
			% This dependency info is constrained to be only
			% for between procedures which have clauses
			% defined for them in this compilation unit
			% (that includes opt_imported procedures).
		maybe_dependency_info ::	maybe(dependency_info),
		num_errors ::			int,
		pragma_exported_procs ::	list(pragma_exported_proc),
					% list of the procs for which
					% there is a pragma export(...)
					% declaration
		type_ctor_gen_infos ::		list(type_ctor_gen_info),
					% info about the types defined here
		must_be_stratified_preds ::	set(pred_id),
		unused_arg_info ::		unused_arg_info,
					% unused argument info about
					% predicates in the current
					% module which has been exported
					% in .opt files.
		lambda_number_counter ::	counter,
		model_non_pragma_counter ::	counter,
					% number of the structure types defined
					% so far for model_non pragma C codes
		imported_module_specifiers ::	set(module_specifier),
					% All the directly imported module
					% specifiers (used during type
					% checking, and by the MLDS back-end)
		indirectly_imported_module_specifiers :: set(module_specifier),
					% All the indirectly imported modules
					% (used by the MLDS back-end)
		do_aditi_compilation ::		do_aditi_compilation,
					% are there any local Aditi predicates
					% for which Aditi-RL must be produced.
		type_spec_info ::		type_spec_info,
					% data used for user-guided type
					% specialization.
		no_tag_type_table ::		no_tag_type_table,
					% Information about no tag
					% types. This information is
					% also in the type_table,
					% but lookups in this table
					% will be much faster.

		analysis_info ::		analysis_info
					% Information for the inter-module
					% analysis framework.
	).

	% A predicate which creates an empty module

module_info_init(Name, Items, Globals, QualifierInfo, RecompInfo,
		ModuleInfo) :-
	predicate_table_init(PredicateTable),
	unify_proc__init_requests(Requests),
	map__init(UnifyPredMap),
	map__init(Types),
	inst_table_init(Insts),
	mode_table_init(Modes),
	map__init(Ctors),
	set__init(StratPreds),
	map__init(UnusedArgInfo),

	set__init(TypeSpecPreds),
	set__init(TypeSpecForcePreds),
	map__init(SpecMap),
	map__init(PragmaMap),
	TypeSpecInfo = type_spec_info(TypeSpecPreds,
		TypeSpecForcePreds, SpecMap, PragmaMap),

	map__init(ClassTable),
	map__init(InstanceTable),
	map__init(SuperClassTable),

	% the builtin modules are automatically imported
	get_implicit_dependencies(Items, Globals, ImportDeps, UseDeps),
	set__list_to_set(ImportDeps `list__append` UseDeps, ImportedModules),
	set__init(IndirectlyImportedModules),

	assertion_table_init(AssertionTable),
	exclusive_table_init(ExclusiveTable),
	map__init(FieldNameTable),

	map__init(NoTagTypes),
	ModuleSubInfo = module_sub(Name, Globals, no, [], [], [], no, 0, [], 
		[], StratPreds, UnusedArgInfo, counter__init(1),
		counter__init(1), ImportedModules, IndirectlyImportedModules,
		no_aditi_compilation, TypeSpecInfo,
		NoTagTypes, init_analysis_info(mmc)),
	ModuleInfo = module(ModuleSubInfo, PredicateTable, Requests,
		UnifyPredMap, QualifierInfo, Types, Insts, Modes, Ctors,
		ClassTable, SuperClassTable, InstanceTable, AssertionTable,
		ExclusiveTable, FieldNameTable, RecompInfo).

%-----------------------------------------------------------------------------%

	% Various predicates which access the module_info data structure.

module_info_get_predicate_table(MI, MI ^ predicate_table).
module_info_get_proc_requests(MI, MI ^ proc_requests).
module_info_get_special_pred_map(MI, MI ^ special_pred_map).
module_info_get_partial_qualifier_info(MI, MI ^ partial_qualifier_info).
module_info_types(MI, MI ^ type_table).
module_info_insts(MI, MI ^ inst_table).
module_info_modes(MI, MI ^ mode_table).
module_info_ctors(MI, MI ^ cons_table).
module_info_classes(MI, MI ^ class_table).
module_info_instances(MI, MI ^ instance_table).
module_info_superclasses(MI, MI ^ superclass_table).
module_info_assertion_table(MI, MI ^ assertion_table).
module_info_exclusive_table(MI, MI ^ exclusive_table).
module_info_ctor_field_table(MI, MI ^ ctor_field_table).
module_info_get_maybe_recompilation_info(MI, MI ^ maybe_recompilation_info).

%-----------------------------------------------------------------------------%

	% Various predicates which modify the module_info data structure.

module_info_set_predicate_table(MI, PT, MI ^ predicate_table := PT).
module_info_set_proc_requests(MI, PR, MI ^ proc_requests := PR).
module_info_set_special_pred_map(MI, SPM, MI ^ special_pred_map := SPM).
module_info_set_partial_qualifier_info(MI, PQ,
	MI ^ partial_qualifier_info := PQ).
module_info_set_types(MI, T, MI ^ type_table := T).
module_info_set_insts(MI, I, MI ^ inst_table := I).
module_info_set_modes(MI, M, MI ^ mode_table := M).
module_info_set_ctors(MI, C, MI ^ cons_table := C).
module_info_set_classes(MI, C, MI ^ class_table := C).
module_info_set_instances(MI, I, MI ^ instance_table := I).
module_info_set_superclasses(MI, S, MI ^ superclass_table := S).
module_info_set_assertion_table(MI, A, MI ^ assertion_table := A).
module_info_set_exclusive_table(MI, PXT, MI ^ exclusive_table := PXT).
module_info_set_ctor_field_table(MI, CF, MI ^ ctor_field_table := CF).
module_info_set_maybe_recompilation_info(MI, I,
	MI ^ maybe_recompilation_info := I).

%-----------------------------------------------------------------------------%

	% Various predicates which access the module_sub_info data structure
	% via the module_info structure.

module_info_name(MI, MI ^ sub_info ^ module_name).
module_info_globals(MI, MI ^ sub_info ^ globals).
module_info_contains_foreign_type(MI) :-
	MI ^ sub_info ^ contains_foreign_type = yes.
module_info_get_foreign_decl(MI, MI ^ sub_info ^ foreign_decl_info).
module_info_get_foreign_body_code(MI, MI ^ sub_info ^ foreign_body_info).
module_info_get_foreign_import_module(MI,
	MI ^ sub_info ^ foreign_import_module_info).
module_info_get_maybe_dependency_info(MI,
	MI ^ sub_info ^ maybe_dependency_info).
module_info_num_errors(MI, MI ^ sub_info ^ num_errors).
module_info_get_pragma_exported_procs(MI,
	MI ^ sub_info ^ pragma_exported_procs).
module_info_type_ctor_gen_infos(MI, MI ^ sub_info ^ type_ctor_gen_infos).
module_info_stratified_preds(MI, MI ^ sub_info ^ must_be_stratified_preds).
module_info_unused_arg_info(MI, MI ^ sub_info ^ unused_arg_info).
module_info_get_lambda_counter(MI, MI ^ sub_info ^ lambda_number_counter).
module_info_get_model_non_pragma_counter(MI,
	MI ^ sub_info ^ model_non_pragma_counter).
module_info_get_imported_module_specifiers(MI,
	MI ^ sub_info ^ imported_module_specifiers).
module_info_get_indirectly_imported_module_specifiers(MI,
	MI ^ sub_info ^ indirectly_imported_module_specifiers).
module_info_get_do_aditi_compilation(MI,
	MI ^ sub_info ^ do_aditi_compilation).
module_info_type_spec_info(MI, MI ^ sub_info ^ type_spec_info).
module_info_no_tag_types(MI, MI ^ sub_info ^ no_tag_type_table).
module_info_analysis_info(MI, MI ^ sub_info ^ analysis_info).

%-----------------------------------------------------------------------------%

	% Various predicates which modify the module_sub_info data structure
	% via the module_info structure.

module_info_set_globals(MI, NewVal,
	MI ^ sub_info ^ globals := NewVal).
module_info_contains_foreign_type(MI,
	MI ^ sub_info ^ contains_foreign_type := yes).
module_info_set_foreign_decl(MI, NewVal,
	MI ^ sub_info ^ foreign_decl_info := NewVal).
module_info_set_foreign_body_code(MI, NewVal,
	MI ^ sub_info ^ foreign_body_info := NewVal).
module_info_set_foreign_import_module(MI, NewVal,
	MI ^ sub_info ^ foreign_import_module_info := NewVal).
module_info_set_maybe_dependency_info(MI, NewVal,
	MI ^ sub_info ^ maybe_dependency_info := NewVal).
module_info_set_num_errors(MI, NewVal,
	MI ^ sub_info ^ num_errors := NewVal).
module_info_set_pragma_exported_procs(MI, NewVal,
	MI ^ sub_info ^ pragma_exported_procs := NewVal).
module_info_set_type_ctor_gen_infos(MI, NewVal,
	MI ^ sub_info ^ type_ctor_gen_infos := NewVal).
module_info_set_stratified_preds(MI, NewVal,
	MI ^ sub_info ^ must_be_stratified_preds := NewVal).
module_info_set_unused_arg_info(MI, NewVal,
	MI ^ sub_info ^ unused_arg_info := NewVal).
module_info_set_lambda_counter(MI, NewVal,
	MI ^ sub_info ^ lambda_number_counter := NewVal).
module_info_set_model_non_pragma_counter(MI, NewVal,
	MI ^ sub_info ^ model_non_pragma_counter := NewVal).
module_add_imported_module_specifiers(ModuleSpecifiers, MI,
	MI ^ sub_info ^ imported_module_specifiers :=
		set__insert_list(
			MI ^ sub_info ^ imported_module_specifiers,
			ModuleSpecifiers)).
module_add_indirectly_imported_module_specifiers(Modules, MI,
	MI ^ sub_info ^ indirectly_imported_module_specifiers :=
		set__insert_list(
			MI ^ sub_info ^ indirectly_imported_module_specifiers,
			Modules)).
module_info_set_do_aditi_compilation(MI,
	MI ^ sub_info ^ do_aditi_compilation := do_aditi_compilation).
module_info_set_type_spec_info(MI, NewVal,
	MI ^ sub_info ^ type_spec_info := NewVal).
module_info_set_no_tag_types(MI, NewVal,
	MI ^ sub_info ^ no_tag_type_table := NewVal).
module_info_set_analysis_info(MI, NewVal,
	MI ^ sub_info ^ analysis_info := NewVal).

%-----------------------------------------------------------------------------%

	% Various predicates which do simple things that are nevertheless
	% beyond the capability of an access predicate.

module_info_preds(MI, Preds) :-
	module_info_get_predicate_table(MI, PredTable),
	predicate_table_get_preds(PredTable, Preds).

module_info_pred_info(MI, PredId, PredInfo) :-
	module_info_preds(MI, Preds),
	( map__search(Preds, PredId, PredInfoPrime) ->
		PredInfo = PredInfoPrime
	;
		pred_id_to_int(PredId, PredInt),
		string__int_to_string(PredInt, PredStr),
		string__append("cannot find predicate number ", PredStr, Msg),
		error(Msg)
	).

module_info_pred_proc_info(MI, PredId, ProcId, PredInfo, ProcInfo) :-
	module_info_pred_info(MI, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo).

module_info_pred_proc_info(MI, proc(PredId, ProcId), PredInfo, ProcInfo) :-
	module_info_pred_proc_info(MI, PredId, ProcId, PredInfo, ProcInfo).

module_info_predids(MI, PredIds) :-
	module_info_get_predicate_table(MI, PredTable),
	predicate_table_get_predids(PredTable, PredIds).

module_info_reverse_predids(MI0, MI) :-
	module_info_get_predicate_table(MI0, PredTable0),
	predicate_table_reverse_predids(PredTable0, PredTable),
	module_info_set_predicate_table(MI0, PredTable, MI).

module_info_remove_predid(MI0, PredId, MI) :-
	module_info_get_predicate_table(MI0, PredTable0),
	predicate_table_remove_predid(PredTable0, PredId, PredTable),
	module_info_set_predicate_table(MI0, PredTable, MI).

module_info_remove_predicate(PredId, MI0, MI) :-
	module_info_get_predicate_table(MI0, PredTable0),
	predicate_table_remove_predicate(PredTable0, PredId, PredTable),
	module_info_set_predicate_table(MI0, PredTable, MI).

module_info_set_preds(MI0, Preds, MI) :-
	module_info_get_predicate_table(MI0, PredTable0),
	predicate_table_set_preds(PredTable0, Preds, PredTable),
	module_info_set_predicate_table(MI0, PredTable, MI).

module_info_set_pred_info(MI0, PredId, PredInfo, MI) :-
	module_info_preds(MI0, Preds0),
	map__set(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(MI0, Preds, MI).

module_info_set_pred_proc_info(MI0, proc(PredId, ProcId),
		PredInfo, ProcInfo, MI) :-
	module_info_set_pred_proc_info(MI0, PredId, ProcId,
		PredInfo, ProcInfo, MI).

module_info_set_pred_proc_info(MI0, PredId, ProcId, PredInfo0, ProcInfo, MI) :-
	pred_info_procedures(PredInfo0, Procs0),
	map__set(Procs0, ProcId, ProcInfo, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo),
	module_info_set_pred_info(MI0, PredId, PredInfo, MI).

module_info_typeids(MI, TypeCtors) :-
	module_info_types(MI, Types),
	map__keys(Types, TypeCtors).

module_info_instids(MI, InstIds) :-
	module_info_insts(MI, InstTable),
	inst_table_get_user_insts(InstTable, UserInstTable),
	user_inst_table_get_inst_ids(UserInstTable, InstIds).

module_info_modeids(MI, ModeIds) :-
	module_info_modes(MI, Modes),
	mode_table_get_mode_ids(Modes, ModeIds).

module_info_consids(MI, ConsIds) :-
	module_info_ctors(MI, Ctors),
	map__keys(Ctors, ConsIds).

module_info_dependency_info(MI, DepInfo) :-
	module_info_get_maybe_dependency_info(MI, MaybeDepInfo),
	( MaybeDepInfo = yes(DepInfoPrime) ->
		DepInfo = DepInfoPrime
	;
		error("Attempted to access invalid dependency_info")
	).

module_info_aditi_dependency_ordering(MI, AditiOrdering) :-
	module_info_dependency_info(MI, DepInfo),
	hlds_dependency_info_get_maybe_aditi_dependency_ordering(DepInfo,
		MaybeOrdering),
	( MaybeOrdering = yes(OrderingPrime) ->
		AditiOrdering = OrderingPrime
	;
		error("Attempted to access invalid aditi_dependency_ordering")
	).

module_info_set_dependency_info(MI0, DependencyInfo, MI) :-
	module_info_set_maybe_dependency_info(MI0, yes(DependencyInfo), MI).

module_info_clobber_dependency_info(MI0, MI) :-
	module_info_set_maybe_dependency_info(MI0, no, MI).

module_info_incr_errors(MI0, MI) :-
	module_info_num_errors(MI0, Errs0),
	Errs = Errs0 + 1,
	module_info_set_num_errors(MI0, Errs, MI).

module_info_next_lambda_count(MI0, Count, MI) :-
	module_info_get_lambda_counter(MI0, Counter0),
	counter__allocate(Count, Counter0, Counter),
	module_info_set_lambda_counter(MI0, Counter, MI).

module_info_next_model_non_pragma_count(MI0, Count, MI) :-
	module_info_get_model_non_pragma_counter(MI0, Counter0),
	counter__allocate(Count, Counter0, Counter),
	module_info_set_model_non_pragma_counter(MI0, Counter, MI).

	% After we have finished constructing the symbol tables,
	% we balance all the binary trees, to improve performance
	% in later stages of the compiler.

module_info_optimize(ModuleInfo0, ModuleInfo) :-

	module_info_get_predicate_table(ModuleInfo0, Preds0),
	predicate_table_optimize(Preds0, Preds),
	module_info_set_predicate_table(ModuleInfo0, Preds, ModuleInfo3),

	module_info_types(ModuleInfo3, Types0),
	map__optimize(Types0, Types),
	module_info_set_types(ModuleInfo3, Types, ModuleInfo4),

	module_info_insts(ModuleInfo4, InstTable0),
	inst_table_get_user_insts(InstTable0, Insts0),
	user_inst_table_optimize(Insts0, Insts),
	inst_table_set_user_insts(InstTable0, Insts, InstTable),
	module_info_set_insts(ModuleInfo4, InstTable, ModuleInfo5),

	module_info_modes(ModuleInfo5, Modes0),
	mode_table_optimize(Modes0, Modes),
	module_info_set_modes(ModuleInfo4, Modes, ModuleInfo6),

	module_info_ctors(ModuleInfo6, Ctors0),
	map__optimize(Ctors0, Ctors),
	module_info_set_ctors(ModuleInfo6, Ctors, ModuleInfo).

visible_module(VisibleModule, ModuleInfo) :-
	module_info_name(ModuleInfo, ThisModule),
	module_info_get_imported_module_specifiers(ModuleInfo, ImportedModules),
	(
		VisibleModule = ThisModule
	;
		set__member(VisibleModule, ImportedModules)
	;
		ParentModules = get_ancestors(ThisModule),
		list__member(VisibleModule, ParentModules)
	).

module_info_get_all_deps(ModuleInfo, AllImports) :-
	module_info_name(ModuleInfo, ModuleName),
	Parents = get_ancestors(ModuleName),
	module_info_get_imported_module_specifiers(ModuleInfo, DirectImports),
	module_info_get_indirectly_imported_module_specifiers(ModuleInfo,
		IndirectImports),
	AllImports = (IndirectImports `set__union` DirectImports)
			`set__union` set__list_to_set(Parents).

module_add_foreign_decl(Lang, ForeignDecl, Context, Module0, Module) :-
	module_info_get_foreign_decl(Module0, ForeignDeclIndex0),
		% store the decls in reverse order and reverse them later
		% for efficiency
	ForeignDeclIndex1 = [foreign_decl_code(Lang, ForeignDecl, Context) |
		ForeignDeclIndex0],
	module_info_set_foreign_decl(Module0, ForeignDeclIndex1, Module).

module_add_foreign_import_module(Lang, ModuleName, Context, Module0, Module) :-
	module_info_get_foreign_import_module(Module0, ForeignImportIndex0),
		% store the decls in reverse order and reverse them later
		% for efficiency
	ForeignImportIndex1 =
		[foreign_import_module(Lang, ModuleName, Context) |
		ForeignImportIndex0],
	module_info_set_foreign_import_module(Module0,
		ForeignImportIndex1, Module).

module_add_foreign_body_code(Lang, Foreign_Body_Code, Context,
		Module0, Module) :-
	module_info_get_foreign_body_code(Module0, Foreign_Body_List0),
		% store the decls in reverse order and reverse them later
		% for efficiency
	Foreign_Body_List = 
		[foreign_body_code(Lang, Foreign_Body_Code, Context) |
			Foreign_Body_List0],
	module_info_set_foreign_body_code(Module0, Foreign_Body_List, Module).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- type dependency_ordering(T)		== list(list(T)).
:- type dependency_ordering		== dependency_ordering(pred_proc_id).

:- type aditi_dependency_ordering	== list(aditi_scc).

	% Each Aditi SCC contains one or more SCCs from the original 
	% dependency ordering and the entry points of the SCC.
	% SCCs which are only called from one other SCC and are not
	% called through negation or aggregation are merged into the
	% parent SCC. This makes the low-level RL optimizations more
	% effective while maintaining stratification. 
:- type aditi_scc
	--->	aditi_scc(dependency_ordering, list(pred_proc_id)).

:- type dependency_graph(T)		== relation(T).
:- type dependency_graph		== dependency_graph(pred_proc_id).
:- type dependency_info(T).
:- type dependency_info			== dependency_info(pred_proc_id).

:- pred hlds_dependency_info_init(dependency_info(T)).
:- mode hlds_dependency_info_init(out) is det.

:- pred hlds_dependency_info_get_dependency_graph(dependency_info(T), 
	dependency_graph(T)).
:- mode hlds_dependency_info_get_dependency_graph(in, out) is det.

:- pred hlds_dependency_info_get_dependency_ordering(dependency_info(T), 
	dependency_ordering(T)).
:- mode hlds_dependency_info_get_dependency_ordering(in, out) is det.

:- pred hlds_dependency_info_get_maybe_aditi_dependency_ordering(
		dependency_info, maybe(aditi_dependency_ordering)).
:- mode hlds_dependency_info_get_maybe_aditi_dependency_ordering(in, 
		out) is det.

:- pred hlds_dependency_info_set_dependency_graph(dependency_info(T),
	dependency_graph(T), dependency_info(T)).
:- mode hlds_dependency_info_set_dependency_graph(in, in, out) is det.

:- pred hlds_dependency_info_set_dependency_ordering(dependency_info(T),
	dependency_ordering(T), dependency_info(T)).
:- mode hlds_dependency_info_set_dependency_ordering(in, in, out) is det.

:- pred hlds_dependency_info_set_aditi_dependency_ordering(dependency_info,
	aditi_dependency_ordering, dependency_info).
:- mode hlds_dependency_info_set_aditi_dependency_ordering(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type dependency_info(T) --->
		dependency_info(
			dependency_graph(T),	% Dependency graph
			dependency_ordering(T),	% Dependency ordering
			maybe(aditi_dependency_ordering)
					% Dependency ordering of Aditi SCCs 
		).

hlds_dependency_info_init(DepInfo) :-
	relation__init(DepRel),
	DepOrd = [],
	DepInfo = dependency_info(DepRel, DepOrd, no).

hlds_dependency_info_get_dependency_graph(DepInfo, A) :-
	DepInfo = dependency_info(A, _, _).

hlds_dependency_info_get_dependency_ordering(DepInfo, B) :-
	DepInfo = dependency_info(_, B, _).

hlds_dependency_info_get_maybe_aditi_dependency_ordering(DepInfo, C) :-
	DepInfo = dependency_info(_, _, C).

hlds_dependency_info_set_dependency_graph(DepInfo0, DepRel, DepInfo) :-
	DepInfo0 = dependency_info(_, B, C),
	DepInfo = dependency_info(DepRel, B, C).

hlds_dependency_info_set_dependency_ordering(DepInfo0, DepRel, DepInfo) :-
	DepInfo0 = dependency_info(A, _, C),
	DepInfo = dependency_info(A, DepRel, C).

hlds_dependency_info_set_aditi_dependency_ordering(DepInfo0, 
		DepOrd, DepInfo) :-
	DepInfo0 = dependency_info(A, B, _),
	DepInfo = dependency_info(A, B, yes(DepOrd)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- type predicate_table.

:- type pred_table	==	map(pred_id, pred_info).

	% Various predicates for accessing the predicate_table type.
	% The predicate_table holds information about the predicates
	% and functions defined in this module or imported from other modules.
	% The primary key for this table is the `pred_id', but there
	% are also secondary indexes on each of name, name+arity, and
	% module+name+arity, for both functions and predicates.

	% Initialize the predicate table

:- pred predicate_table_init(predicate_table).
:- mode predicate_table_init(out) is det.

	% Balance all the binary trees in the predicate table

:- pred predicate_table_optimize(predicate_table, predicate_table).
:- mode predicate_table_optimize(in, out) is det.

	% Get the pred_id->pred_info map.

:- pred predicate_table_get_preds(predicate_table, pred_table).
:- mode predicate_table_get_preds(in, out) is det.

	% Restrict the predicate table to the list of predicates.
	% This predicate should only be used when the set of predicates
	% to restrict the table to is significantly smaller then the
	% predicate_table size, as rather than removing entries from
	% the table it builds a new table from scratch.

:- pred predicate_table_restrict(partial_qualifier_info::in,
		predicate_table::in, list(pred_id)::in,
		predicate_table::out) is det.

	% Set the pred_id->pred_info map.
	% NB You shouldn't modify the keys in this table, only
	% use predicate_table_insert, predicate_table_remove_predid and
	% predicate_table_remove_predicate.

:- pred predicate_table_set_preds(predicate_table, pred_table, predicate_table).
:- mode predicate_table_set_preds(in, in, out) is det.

	% Get a list of all the valid predids in the predicate_table.

:- pred predicate_table_get_predids(predicate_table, list(pred_id)).
:- mode predicate_table_get_predids(in, out) is det.

	% Remove a pred_id from the valid list.

:- pred predicate_table_remove_predid(predicate_table, pred_id,
					predicate_table).
:- mode predicate_table_remove_predid(in, in, out) is det.

:- pred predicate_table_remove_predicate(predicate_table, pred_id,
					predicate_table).
:- mode predicate_table_remove_predicate(in, in, out) is det.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only
	% matching this (possibly module-qualified) sym_name.

:- pred predicate_table_search_sym(predicate_table, is_fully_qualified,
		sym_name, list(pred_id)).
:- mode predicate_table_search_sym(in, in, in, out) is semidet.

:- pred predicate_table_search_pred_sym(predicate_table,
		is_fully_qualified, sym_name, list(pred_id)).
:- mode predicate_table_search_pred_sym(in, in, in, out) is semidet.

:- pred predicate_table_search_func_sym(predicate_table,
		is_fully_qualified, sym_name, list(pred_id)).
:- mode predicate_table_search_func_sym(in, in, in, out) is semidet.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only matching this
	% (possibly module-qualified) sym_name & arity.

:- pred predicate_table_search_sym_arity(predicate_table, is_fully_qualified,
		sym_name, arity, list(pred_id)).
:- mode predicate_table_search_sym_arity(in, in, in, in, out) is semidet.

:- pred predicate_table_search_pred_sym_arity(predicate_table,
		is_fully_qualified, sym_name, arity, list(pred_id)).
:- mode predicate_table_search_pred_sym_arity(in, in, in, in, out) is semidet.

:- pred predicate_table_search_func_sym_arity(predicate_table,
		is_fully_qualified, sym_name, arity, list(pred_id)).
:- mode predicate_table_search_func_sym_arity(in, in, in, in, out) is semidet.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only
	% matching this name.

:- pred predicate_table_search_name(predicate_table, string, list(pred_id)).
:- mode predicate_table_search_name(in, in, out) is semidet.

:- pred predicate_table_search_pred_name(predicate_table, string,
					list(pred_id)).
:- mode predicate_table_search_pred_name(in, in, out) is semidet.

:- pred predicate_table_search_func_name(predicate_table, string,
					list(pred_id)).
:- mode predicate_table_search_func_name(in, in, out) is semidet.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only
	% matching this name & arity.
	% When searching for functions, the arity used
	% is the arity of the function, not the arity N+1 predicate
	% that it gets converted to.

:- pred predicate_table_search_name_arity(predicate_table, string, arity,
						list(pred_id)).
:- mode predicate_table_search_name_arity(in, in, in, out) is semidet.

:- pred predicate_table_search_pred_name_arity(predicate_table, string, arity,
						list(pred_id)).
:- mode predicate_table_search_pred_name_arity(in, in, in, out) is semidet.

:- pred predicate_table_search_func_name_arity(predicate_table, string, arity,
						list(pred_id)).
:- mode predicate_table_search_func_name_arity(in, in, in, out) is semidet.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only
	% matching this module, name & arity.
	% When searching for functions, the arity used
	% is the arity of the function, not the arity N+1 predicate
	% that it gets converted to.
	%
	% Note that in cases (b) and (c) it was previously the case
	% that there could only be one matching pred_id, since
	% each predicate or function could be uniquely identified
	% by its module, name, arity, and category (function/predicate).
	% However this is no longer true, due to nested modules.
	% (For example, `pred foo:bar/2' might match both
	% `pred mod1:foo:bar/2' and `pred mod2:foo:bar/2').
	% I hope it doesn't break anything too badly...
	%
	% (`m_n_a' here is short for "module, name, arity".)
	
	% Is the item known to be fully qualified?
	% If so, a search for `pred foo.bar/2' will not match
	% `pred baz.foo.bar/2'.
:- type is_fully_qualified
	--->	is_fully_qualified
	;	may_be_partially_qualified
	.

:- pred predicate_table_search_m_n_a(predicate_table, is_fully_qualified,
		module_name, string, arity, list(pred_id)).
:- mode predicate_table_search_m_n_a(in, in, in, in, in, out) is semidet.

:- pred predicate_table_search_pred_m_n_a(predicate_table, is_fully_qualified,
		module_name, string, arity, list(pred_id)).
:- mode predicate_table_search_pred_m_n_a(in, in, in, in, in, out) is semidet.

:- pred predicate_table_search_func_m_n_a(predicate_table, is_fully_qualified,
		module_name, string, arity, list(pred_id)).
:- mode predicate_table_search_func_m_n_a(in, in, in, in, in, out) is semidet.

	% Search the table for predicates or functions matching
	% this pred_or_func category, module, name, and arity.
	% When searching for functions, the arity used
	% is the arity of the predicate that the function gets converted
	% to, i.e. the arity of the function plus one.
	% NB.  This is opposite to what happens with the search
	% predicates declared above!!

:- pred predicate_table_search_pf_m_n_a(predicate_table, is_fully_qualified,
		pred_or_func, module_name, string, arity, list(pred_id)).
:- mode predicate_table_search_pf_m_n_a(in, in, in, in, in, in, out) is semidet.

	% Search the table for predicates or functions matching
	% this pred_or_func category, name, and arity.
	% When searching for functions, the arity used
	% is the arity of the predicate that the function gets converted
	% to, i.e. the arity of the function plus one.
	% NB.  This is opposite to what happens with the search
	% predicates declared above!!

:- pred predicate_table_search_pf_name_arity(predicate_table, pred_or_func,
					string, arity, list(pred_id)).
:- mode predicate_table_search_pf_name_arity(in, in, in, in, out) is semidet.

	% Search the table for predicates or functions matching
	% this pred_or_func category, sym_name, and arity.
	% When searching for functions, the arity used
	% is the arity of the predicate that the function gets converted
	% to, i.e. the arity of the function plus one.
	% NB.  This is opposite to what happens with the search
	% predicates declared above!!

:- pred predicate_table_search_pf_sym_arity(predicate_table,
		is_fully_qualified, pred_or_func,
		sym_name, arity, list(pred_id)) is semidet.
:- mode predicate_table_search_pf_sym_arity(in,
		in, in, in, in, out) is semidet.

	% Search the table for predicates or functions matching
	% this pred_or_func category and sym_name.

:- pred predicate_table_search_pf_sym(predicate_table, is_fully_qualified,
		pred_or_func, sym_name, list(pred_id)) is semidet.
:- mode predicate_table_search_pf_sym(in, in, in, in, out) is semidet.

	% predicate_table_insert(PredTable0, PredInfo,
	%		NeedQual, PartialQualInfo, PredId, PredTable).
	% 
	% Insert PredInfo into PredTable0 and assign it a new pred_id.
	% You should check beforehand that the pred doesn't already 
	% occur in the table. 
:- pred predicate_table_insert(predicate_table, pred_info, need_qualifier,
		partial_qualifier_info, pred_id, predicate_table).
:- mode predicate_table_insert(in, in, in, in, out, out) is det.

	% Equivalent to predicate_table_insert/6, except that only the
	% fully-qualified version of the predicate will be inserted into
	% the predicate symbol table.  This is useful for creating
	% compiler-generated predicates which will only ever be accessed
	% via fully-qualified names.
:- pred predicate_table_insert(predicate_table, pred_info, pred_id,
				predicate_table).
:- mode predicate_table_insert(in, in, out, out) is det.

:- pred predicate_id(module_info, pred_id, module_name, string, arity).
:- mode predicate_id(in, in, out, out, out) is det.

:- pred predicate_module(module_info, pred_id, module_name).
:- mode predicate_module(in, in, out) is det.

:- pred predicate_name(module_info, pred_id, string).
:- mode predicate_name(in, in, out) is det.

:- pred predicate_arity(module_info, pred_id, arity).
:- mode predicate_arity(in, in, out) is det.

	% Get the pred_id and proc_id matching a higher-order term with
	% the given argument types, aborting with an error if none is
	% found.
:- pred get_pred_id_and_proc_id(is_fully_qualified, sym_name, pred_or_func,
		tvarset, list(type), module_info, pred_id, proc_id).
:- mode get_pred_id_and_proc_id(in, in, in, in, in, in, out, out) is det.

	% Get the pred_id matching a higher-order term with
	% the given argument types, failing if none is found.
:- pred get_pred_id(is_fully_qualified, sym_name, pred_or_func,
		tvarset, list(type), module_info, pred_id).
:- mode get_pred_id(in, in, in, in, in, in, out) is semidet.

	% Given a pred_id, return the single proc_id, aborting
	% if there are no modes or more than one mode.
:- pred get_proc_id(module_info, pred_id, proc_id).
:- mode get_proc_id(in, in, out) is det.

:- type mode_no
	--->    only_mode		% The pred must have exactly one mode.
	;       mode_no(int).		% The Nth mode, counting from 0.

:- pred lookup_builtin_pred_proc_id(module_info, module_name,
		string, pred_or_func, arity, mode_no, pred_id, proc_id).
:- mode lookup_builtin_pred_proc_id(in, in, in, in, in, in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type predicate_table --->
	predicate_table(
		pred_table,		% map from pred_id to pred_info
		pred_id,		% next available pred_id
		list(pred_id),		% the keys of the pred_table -
					% cached here for efficiency
		% indexes on predicates
		name_index,		% map from pred name to pred_id
		name_arity_index,	% map from pred name & arity to pred_id
		module_name_arity_index,
					% map from pred module, name & arity
					% to pred_id
		% indexes on functions
		name_index,		% map from func name to pred_id
		name_arity_index,	% map from func name & arity to pred_id
		module_name_arity_index
					% map from func module, name & arity
					% to pred_id
	).

:- type name_index	== map(string, list(pred_id)).

:- type name_arity_index == map(name_arity, list(pred_id)).
:- type name_arity ---> string / arity.

	% First search on module and name, then search on arity. The two 
	% levels are needed because typecheck.m needs to be able to search
	% on module and name only for higher-order terms.
:- type module_name_arity_index == map(pair(module_name, string), 
					map(arity, list(pred_id))).

predicate_table_init(PredicateTable) :-
	PredicateTable = predicate_table(Preds, NextPredId, PredIds,
				Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
				Func_N_Index, Func_NA_Index, Func_MNA_Index),
	map__init(Preds),
	hlds_pred__initial_pred_id(NextPredId),
	PredIds = [],
	map__init(Pred_N_Index),
	map__init(Pred_NA_Index),
	map__init(Pred_MNA_Index),
	map__init(Func_N_Index),
	map__init(Func_NA_Index),
	map__init(Func_MNA_Index).

predicate_table_optimize(PredicateTable0, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, C,
				Pred_N_Index0, Pred_NA_Index0, Pred_MNA_Index0,
				Func_N_Index0, Func_NA_Index0, Func_MNA_Index0),
	map__optimize(Pred_N_Index0, Pred_N_Index),
	map__optimize(Pred_NA_Index0, Pred_NA_Index),
	map__optimize(Pred_MNA_Index0, Pred_MNA_Index),
	map__optimize(Func_N_Index0, Func_N_Index),
	map__optimize(Func_NA_Index0, Func_NA_Index),
	map__optimize(Func_MNA_Index0, Func_MNA_Index),
	PredicateTable = predicate_table(A, B, C,
				Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
				Func_N_Index, Func_NA_Index, Func_MNA_Index).

predicate_table_get_preds(PredicateTable, Preds) :-
	PredicateTable = predicate_table(Preds, _, _, _, _, _, _, _, _).

predicate_table_set_preds(PredicateTable0, Preds, PredicateTable) :-
	PredicateTable0 = predicate_table(_, B, C, D, E, F, G, H, I),
	PredicateTable = predicate_table(Preds, B, C, D, E, F, G, H, I).

predicate_table_get_predids(PredicateTable, PredIds) :-
	PredicateTable = predicate_table(_, _, PredIds, _, _, _, _, _, _).

predicate_table_remove_predid(PredicateTable0, PredId, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, PredIds0, D, E, F, G, H, I),
	list__delete_all(PredIds0, PredId, PredIds),
	PredicateTable = predicate_table(A, B, PredIds, D, E, F, G, H, I).

predicate_table_remove_predicate(PredicateTable0, PredId, PredicateTable) :-
	PredicateTable0 = predicate_table(Preds0, NextPredId, PredIds0, 
		PredN0, PredNA0, PredMNA0, FuncN0, FuncNA0, FuncMNA0),
	list__delete_all(PredIds0, PredId, PredIds),
	map__det_remove(Preds0, PredId, PredInfo, Preds),
	pred_info_module(PredInfo, Module),
	pred_info_name(PredInfo, Name),
	pred_info_arity(PredInfo, Arity),
	pred_info_get_is_pred_or_func(PredInfo, IsPredOrFunc),
	(
		IsPredOrFunc = predicate,
		predicate_table_remove_from_index(Module, Name, Arity, PredId,
			PredN0, PredN, PredNA0, PredNA, PredMNA0, PredMNA),
		PredicateTable = predicate_table(Preds, NextPredId, PredIds, 
			PredN, PredNA, PredMNA, FuncN0, FuncNA0, FuncMNA0)
	;
		IsPredOrFunc = function,
		FuncArity = Arity - 1,
		predicate_table_remove_from_index(Module, Name, FuncArity, 
			PredId, FuncN0, FuncN, FuncNA0, FuncNA, 
			FuncMNA0, FuncMNA),
		PredicateTable = predicate_table(Preds, NextPredId, PredIds, 
			PredN0, PredNA0, PredMNA0, FuncN, FuncNA, FuncMNA)
	).

:- pred predicate_table_remove_from_index(module_name, string, int, pred_id, 
		name_index, name_index, name_arity_index, name_arity_index, 
		module_name_arity_index, module_name_arity_index).
:- mode predicate_table_remove_from_index(in, in, in, in, in, out, 
		in, out, in, out) is det.

predicate_table_remove_from_index(Module, Name, Arity, PredId,
		N0, N, NA0, NA, MNA0, MNA) :-
	do_remove_from_index(N0, Name, PredId, N),
	do_remove_from_index(NA0, Name / Arity, PredId, NA),
	do_remove_from_m_n_a_index(MNA0, Module, Name, Arity, PredId, MNA).

:- pred do_remove_from_index(map(T, list(pred_id)), T, pred_id, 
			map(T, list(pred_id))).
:- mode do_remove_from_index(in, in, in, out) is det.

do_remove_from_index(Index0, T, PredId, Index) :-
	( map__search(Index0, T, NamePredIds0) ->
		list__delete_all(NamePredIds0, PredId, NamePredIds),
		( NamePredIds = [] ->
			map__delete(Index0, T, Index)	
		;
			map__det_update(Index0, T, NamePredIds, Index)
		)
	;
		Index = Index0
	).

:- pred	do_remove_from_m_n_a_index(module_name_arity_index, 
		module_name, string, int, pred_id, module_name_arity_index).
:- mode do_remove_from_m_n_a_index(in, in, in, in, in, out) is det.

do_remove_from_m_n_a_index(MNA0, Module, Name, Arity, PredId, MNA) :-
	map__lookup(MNA0, Module - Name, Arities0),
	map__lookup(Arities0, Arity, PredIds0),
	list__delete_all(PredIds0, PredId, PredIds),
	( PredIds = [] ->
		map__delete(Arities0, Arity, Arities),
		( map__is_empty(Arities) ->
			map__delete(MNA0, Module - Name, MNA)	
		;
			map__det_update(MNA0, Module - Name, Arities, MNA)
		)
	;
		map__det_update(Arities0, Arity, PredIds, Arities),
		map__det_update(MNA0, Module - Name, Arities, MNA)
	).	

%-----------------------------------------------------------------------------%

:- pred predicate_table_reverse_predids(predicate_table, predicate_table).
:- mode predicate_table_reverse_predids(in, out) is det.

predicate_table_reverse_predids(PredicateTable0, PredicateTable) :-
	PredicateTable0 = predicate_table(A, B, PredIds0, D, E, F, G, H, I),
	list__reverse(PredIds0, PredIds),
	PredicateTable = predicate_table(A, B, PredIds, D, E, F, G, H, I).

%-----------------------------------------------------------------------------%

predicate_table_search_sym(PredicateTable, may_be_partially_qualified,
		unqualified(Name), PredIdList) :-
	predicate_table_search_name(PredicateTable, Name, PredIdList).
predicate_table_search_sym(PredicateTable, IsFullyQualified,
		qualified(Module, Name), PredIdList) :-
	predicate_table_search_module_name(PredicateTable, IsFullyQualified,
		Module, Name, PredIdList),
	PredIdList \= [].

predicate_table_search_pred_sym(PredicateTable, may_be_partially_qualified,
		unqualified(Name), PredIdList) :-
	predicate_table_search_pred_name(PredicateTable, Name, PredIdList).
predicate_table_search_pred_sym(PredicateTable, IsFullyQualified,
		qualified(Module, Name), PredIdList) :-
	predicate_table_search_pred_module_name(PredicateTable,
		IsFullyQualified, Module, Name, PredIdList),
	PredIdList \= [].

predicate_table_search_func_sym(PredicateTable, may_be_partially_qualified,
		unqualified(Name), PredIdList) :-
	predicate_table_search_func_name(PredicateTable, Name, PredIdList).
predicate_table_search_func_sym(PredicateTable, IsFullyQualified,
		qualified(Module, Name), PredIdList) :-
	predicate_table_search_func_module_name(PredicateTable,
		IsFullyQualified, Module, Name, PredIdList),
	PredIdList \= [].

	% Given a list of predicates, and a module name, find all the
	% predicates which came from that module.

%-----------------------------------------------------------------------------%

predicate_table_search_sym_arity(PredicateTable, IsFullyQualified,
		qualified(Module, Name), Arity, PredIdList) :-
	predicate_table_search_m_n_a(PredicateTable,
		IsFullyQualified, Module, Name, Arity, PredIdList).
predicate_table_search_sym_arity(PredicateTable, may_be_partially_qualified,
		unqualified(Name), Arity, PredIdList) :-
	predicate_table_search_name_arity(PredicateTable, Name, Arity,
		PredIdList).

predicate_table_search_pred_sym_arity(PredicateTable, IsFullyQualified,
		qualified(Module, Name), Arity, PredIdList) :-
	predicate_table_search_pred_m_n_a(PredicateTable,
		IsFullyQualified, Module, Name, Arity, PredIdList).
predicate_table_search_pred_sym_arity(PredicateTable,
		may_be_partially_qualified, unqualified(Name),
		Arity, PredIdList) :-
	predicate_table_search_pred_name_arity(PredicateTable, Name, Arity,
		PredIdList).

predicate_table_search_func_sym_arity(PredicateTable, IsFullyQualified,
		qualified(Module, Name), Arity, PredIdList) :-
	predicate_table_search_func_m_n_a(PredicateTable,
		IsFullyQualified, Module, Name, Arity, PredIdList).
predicate_table_search_func_sym_arity(PredicateTable,
		may_be_partially_qualified, unqualified(Name),
		Arity, PredIdList) :-
	predicate_table_search_func_name_arity(PredicateTable, Name, Arity,
		PredIdList).

%-----------------------------------------------------------------------------%

predicate_table_search_name(PredicateTable, Name, PredIds) :-
	(
		predicate_table_search_pred_name(PredicateTable, Name,
			PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_name(PredicateTable, Name,
			FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

predicate_table_search_pred_name(PredicateTable, PredName, PredIds) :-
	PredicateTable = predicate_table(_, _, _, PredNameIndex, _, _, _, _, _),
	map__search(PredNameIndex, PredName, PredIds).

predicate_table_search_func_name(PredicateTable, FuncName, PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, _, FuncNameIndex, _, _),
	map__search(FuncNameIndex, FuncName, PredIds).

%-----------------------------------------------------------------------------%

:- pred predicate_table_search_module_name(predicate_table, is_fully_qualified,
		module_name, string, list(pred_id)).
:- mode predicate_table_search_module_name(in, in, in, in, out) is semidet.

predicate_table_search_module_name(PredicateTable, IsFullyQualified,
		Module, Name, PredIds) :-
	(
		predicate_table_search_pred_module_name(PredicateTable,
			IsFullyQualified, Module, Name, PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_module_name(PredicateTable, 
			IsFullyQualified, Module, Name, FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

:- pred predicate_table_search_pred_module_name(predicate_table,
		is_fully_qualified, module_name, string, list(pred_id)).
:- mode predicate_table_search_pred_module_name(in, in, in, in, out) is semidet.

predicate_table_search_pred_module_name(PredicateTable, IsFullyQualified,
		Module, PredName, PredIds) :-
	PredicateTable = predicate_table(_,_,_,_,_, Pred_MNA_Index, _,_,_),
	map__search(Pred_MNA_Index, Module - PredName, Arities),
	map__values(Arities, PredIdLists),
	list__condense(PredIdLists, PredIds0),
	maybe_filter_pred_ids_matching_module(IsFullyQualified,
		Module, PredicateTable, PredIds0, PredIds).

:- pred predicate_table_search_func_module_name(predicate_table,
		is_fully_qualified, module_name, string, list(pred_id)).
:- mode predicate_table_search_func_module_name(in,
		in, in, in, out) is semidet.

predicate_table_search_func_module_name(PredicateTable, IsFullyQualified,
		Module, FuncName, PredIds) :-
	PredicateTable = predicate_table(_,_,_,_,_,_,_,_, Func_MNA_Index),
	map__search(Func_MNA_Index, Module - FuncName, Arities),
	map__values(Arities, PredIdLists),
	list__condense(PredIdLists, PredIds0),
	maybe_filter_pred_ids_matching_module(IsFullyQualified,
		Module, PredicateTable, PredIds0, PredIds).

%-----------------------------------------------------------------------------%

predicate_table_search_name_arity(PredicateTable, Name, Arity, PredIds) :-
	(
		predicate_table_search_pred_name_arity(PredicateTable,
			Name, Arity, PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_name_arity(PredicateTable,
			Name, Arity, FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

predicate_table_search_pred_name_arity(PredicateTable, PredName, Arity,
		PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, PredNameArityIndex, _,
					_, _, _),
	map__search(PredNameArityIndex, PredName / Arity, PredIds).

predicate_table_search_func_name_arity(PredicateTable, FuncName, Arity,
		PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, _,
					_, FuncNameArityIndex, _),
	map__search(FuncNameArityIndex, FuncName / Arity, PredIds).

%-----------------------------------------------------------------------------%

predicate_table_search_m_n_a(PredicateTable, IsFullyQualified,
		Module, Name, Arity, PredIds) :-
	(
		predicate_table_search_pred_m_n_a(PredicateTable,
			IsFullyQualified, Module, Name, Arity, PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_m_n_a(PredicateTable,
			IsFullyQualified, Module, Name, Arity, FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

predicate_table_search_pred_m_n_a(PredicateTable, IsFullyQualified,
		Module, PredName, Arity, PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, P_MNA_Index, _, _, _),
	map__search(P_MNA_Index, Module - PredName, ArityIndex),
	map__search(ArityIndex, Arity, PredIds0),
	maybe_filter_pred_ids_matching_module(IsFullyQualified,
		Module, PredicateTable, PredIds0, PredIds).

predicate_table_search_func_m_n_a(PredicateTable, IsFullyQualified,
		Module, FuncName, Arity, PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, _, _, _, F_MNA_Index),
	map__search(F_MNA_Index, Module - FuncName, ArityIndex),
	map__search(ArityIndex, Arity, PredIds0),
	maybe_filter_pred_ids_matching_module(IsFullyQualified,
		Module, PredicateTable, PredIds0, PredIds).

:- pred maybe_filter_pred_ids_matching_module(is_fully_qualified,
		module_name, predicate_table, list(pred_id), list(pred_id)).
:- mode maybe_filter_pred_ids_matching_module(in, in, in, in, out) is det.

maybe_filter_pred_ids_matching_module(may_be_partially_qualified, _, _,
		PredIds, PredIds).
maybe_filter_pred_ids_matching_module(is_fully_qualified, ModuleName,
		PredicateTable, PredIds0, PredIds) :-
	predicate_table_get_preds(PredicateTable, Preds),
	PredIds = list__filter(
			(pred(PredId::in) is semidet :-
				map__lookup(Preds, PredId, PredInfo),
				pred_info_module(PredInfo, ModuleName)
			), PredIds0).

%-----------------------------------------------------------------------------%

predicate_table_search_pf_m_n_a(PredicateTable, IsFullyQualified,
		predicate, Module, Name, Arity, PredIds) :-
	predicate_table_search_pred_m_n_a(PredicateTable, IsFullyQualified,
		Module, Name, Arity, PredIds).
predicate_table_search_pf_m_n_a(PredicateTable, IsFullyQualified,
		function, Module, Name, Arity, PredIds) :-
	FuncArity = Arity - 1,
	predicate_table_search_func_m_n_a(PredicateTable, IsFullyQualified,
		Module, Name, FuncArity, PredIds).

predicate_table_search_pf_name_arity(PredicateTable, predicate, Name, Arity,
		PredIds) :-
	predicate_table_search_pred_name_arity(PredicateTable, Name, Arity,
		PredIds).
predicate_table_search_pf_name_arity(PredicateTable, function, Name, Arity,
		PredIds) :-
	FuncArity = Arity - 1,
	predicate_table_search_func_name_arity(PredicateTable, Name, FuncArity,
		PredIds).

predicate_table_search_pf_sym_arity(PredicateTable, IsFullyQualified,
		PredOrFunc, qualified(Module, Name), Arity, PredIdList) :-
	predicate_table_search_pf_m_n_a(PredicateTable,
		IsFullyQualified, PredOrFunc,
		Module, Name, Arity, PredIdList).
predicate_table_search_pf_sym_arity(PredicateTable, may_be_partially_qualified,
		PredOrFunc, unqualified(Name), Arity, PredIdList) :-
	predicate_table_search_pf_name_arity(PredicateTable, PredOrFunc,
		Name, Arity, PredIdList).

predicate_table_search_pf_sym(PredicateTable, IsFullyQualified, predicate,
		SymName, PredIdList) :-
	predicate_table_search_pred_sym(PredicateTable, IsFullyQualified,
		SymName, PredIdList).
predicate_table_search_pf_sym(PredicateTable, IsFullyQualified,
		function, SymName, PredIdList) :-
	predicate_table_search_func_sym(PredicateTable, IsFullyQualified,
		SymName, PredIdList).

%-----------------------------------------------------------------------------%

predicate_table_restrict(PartialQualInfo,
		OrigPredicateTable, PredIds, PredicateTable) :-
	predicate_table_reset(OrigPredicateTable, PredicateTable0),
	predicate_table_get_preds(OrigPredicateTable, Preds),
	% Note that we use foldr here rather than foldl,
	% so that the PredIds list in the predicate table
	% is the same as the PredIds list argument here
	% (if we used foldl, it would get reversed, since each
	% new predicate inserted into the table gets its pred_id
	% added at the start of the list).
	PredicateTable = list__foldr(
		(func(PredId, Table0) = Table :-
			PredInfo = map__lookup(Preds, PredId),
			pred_info_get_markers(PredInfo, Markers),
			(
			    check_marker(Markers,
				not_accessible_by_unqualifed_name)
			->
			    NeedQual = must_be_qualified
			;
			    NeedQual = may_be_unqualified
			),
			(
			    check_marker(Markers,
			    	not_accessible_by_partially_qualified_names)
			->
			    MaybeQualInfo = no
			;
			    MaybeQualInfo = yes(PartialQualInfo)
			),

			predicate_table_insert_2(Table0,
					yes(PredId), PredInfo,
					NeedQual, MaybeQualInfo,
					_, Table)
			
		), PredIds, PredicateTable0).

:- pred predicate_table_reset(predicate_table::in, predicate_table::out) is det.

predicate_table_reset(PredicateTable0, PredicateTable) :-
	PredicateTable0 = predicate_table(_, NextPredId, _, _, _, _, _, _, _),
	PredicateTable = predicate_table(map__init, NextPredId, [], map__init,
			map__init, map__init, map__init, map__init, map__init).

%-----------------------------------------------------------------------------%

predicate_table_insert(PredicateTable0, PredInfo, PredId, PredicateTable) :-
	predicate_table_insert_2(PredicateTable0, no, PredInfo,
			must_be_qualified, no, PredId, PredicateTable).

predicate_table_insert(PredicateTable0, PredInfo, NeedQual, QualInfo,
		PredId, PredicateTable) :-
	predicate_table_insert_2(PredicateTable0, no, PredInfo,
			NeedQual, yes(QualInfo),
			PredId, PredicateTable).

:- pred predicate_table_insert_2(predicate_table, maybe(pred_id),
		pred_info, need_qualifier,
		maybe(partial_qualifier_info), pred_id, predicate_table).
:- mode predicate_table_insert_2(in, in, in, in, in, out, out) is det.

predicate_table_insert_2(PredicateTable0, MaybePredId, PredInfo0,
		NeedQual, MaybeQualInfo, PredId, PredicateTable) :-

	PredicateTable0 = predicate_table(Preds0, NextPredId0, PredIds0,
				Pred_N_Index0, Pred_NA_Index0, Pred_MNA_Index0,
				Func_N_Index0, Func_NA_Index0, Func_MNA_Index0),
	pred_info_module(PredInfo0, Module),
	pred_info_name(PredInfo0, Name),
	pred_info_arity(PredInfo0, Arity),

	( MaybePredId = yes(PredId),
		NextPredId = NextPredId0

		% allocate a new pred_id
	; MaybePredId = no,
		PredId = NextPredId0,
		hlds_pred__next_pred_id(PredId, NextPredId)
	),

		% insert the pred_id into either the function or predicate
		% indices, as appropriate
	pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc),
	( 
		PredOrFunc = predicate,
		predicate_table_do_insert(Module, Name, Arity,
			NeedQual, MaybeQualInfo, PredId,
			Pred_N_Index0, Pred_N_Index, 
			Pred_NA_Index0, Pred_NA_Index,
			Pred_MNA_Index0, Pred_MNA_Index,
			PredInfo0, PredInfo),

		Func_N_Index = Func_N_Index0,
		Func_NA_Index = Func_NA_Index0,
		Func_MNA_Index = Func_MNA_Index0
	;
		PredOrFunc = function,
		FuncArity = Arity - 1,
		predicate_table_do_insert(Module, Name, FuncArity,
			NeedQual, MaybeQualInfo, PredId,
			Func_N_Index0, Func_N_Index, 
			Func_NA_Index0, Func_NA_Index,
			Func_MNA_Index0, Func_MNA_Index,
			PredInfo0, PredInfo),

		Pred_N_Index = Pred_N_Index0,
		Pred_NA_Index = Pred_NA_Index0,
		Pred_MNA_Index = Pred_MNA_Index0
	),

		% insert the pred_id into the pred_id list
	PredIds = [PredId | PredIds0],

		% save the pred_info for this pred_id
	map__det_insert(Preds0, PredId, PredInfo, Preds),

	PredicateTable = predicate_table(Preds, NextPredId, PredIds,
				Pred_N_Index, Pred_NA_Index, Pred_MNA_Index,
				Func_N_Index, Func_NA_Index, Func_MNA_Index).

:- pred predicate_table_do_insert(module_name, string, arity,
	need_qualifier, maybe(partial_qualifier_info),
	pred_id, name_index, name_index, name_arity_index,
	name_arity_index, module_name_arity_index, module_name_arity_index,
	pred_info, pred_info).
:- mode predicate_table_do_insert(in, in, in, in, in, in,
	in, out, in, out, in, out, in, out) is det.

predicate_table_do_insert(Module, Name, Arity, NeedQual, MaybeQualInfo,
		PredId, N_Index0, N_Index, NA_Index0, NA_Index, 
		MNA_Index0, MNA_Index, PredInfo0, PredInfo) :-
	( NeedQual = may_be_unqualified ->
			% insert the unqualified name into the name index
		multi_map__set(N_Index0, Name, PredId, N_Index),

			% insert the unqualified name/arity into the
			% name/arity index
		NA = Name / Arity,
		multi_map__set(NA_Index0, NA, PredId, NA_Index),

		PredInfo1 = PredInfo0
	;
		N_Index = N_Index0,
		NA_Index = NA_Index0,

		pred_info_get_markers(PredInfo0, MarkersA0),
		add_marker(MarkersA0, not_accessible_by_unqualifed_name,
				MarkersA),
		pred_info_set_markers(PredInfo0, MarkersA, PredInfo1)
	),

	( MaybeQualInfo = yes(QualInfo) ->

			% insert partially module-qualified versions
			% of the name into the module:name/arity index
		get_partial_qualifiers(Module, QualInfo, PartialQuals),
		list__map_foldl(lambda([AncModule::in, AncModule::out,
				MNAs0::in, MNAs::out] is det,
			insert_into_mna_index(AncModule, Name, Arity, PredId,
					MNAs0, MNAs)),
			PartialQuals, _, MNA_Index0, MNA_Index1),

		PredInfo = PredInfo1
	;
		MNA_Index1 = MNA_Index0,

		pred_info_get_markers(PredInfo1, MarkersB0),
		add_marker(MarkersB0,
				not_accessible_by_partially_qualified_names,
				MarkersB),
		pred_info_set_markers(PredInfo1, MarkersB, PredInfo)
	),

		% insert the fully-qualified name into the
		% module:name/arity index
	insert_into_mna_index(Module, Name, Arity, PredId,
			MNA_Index1, MNA_Index).

:- pred insert_into_mna_index(module_name, string, arity, pred_id,
			module_name_arity_index, module_name_arity_index).
:- mode insert_into_mna_index(in, in, in, in, in, out) is det.
insert_into_mna_index(Module, Name, Arity, PredId, MNA_Index0, MNA_Index) :-
	( map__search(MNA_Index0, Module - Name, MN_Arities0) ->
		multi_map__set(MN_Arities0, Arity, PredId, MN_Arities),
		map__det_update(MNA_Index0, Module - Name, MN_Arities,
			MNA_Index)
	;
		map__init(MN_Arities0),
		map__det_insert(MN_Arities0, Arity, [PredId], MN_Arities),
		map__det_insert(MNA_Index0, Module - Name, MN_Arities,
			MNA_Index)
	).

%-----------------------------------------------------------------------------%

get_pred_id(IsFullyQualified, SymName, PredOrFunc, TVarSet,
		ArgTypes, ModuleInfo, PredId) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	list__length(ArgTypes, Arity),
	(
		predicate_table_search_pf_sym_arity(PredicateTable,
			IsFullyQualified, PredOrFunc, SymName,
			Arity, PredIds),
		% Resolve overloading using the argument types. 
		typecheck__find_matching_pred_id(PredIds, ModuleInfo,
			TVarSet, ArgTypes, PredId0, _PredName)
	->
		PredId = PredId0
	;
		% Undefined/invalid pred or func.
		fail
	).

get_pred_id_and_proc_id(IsFullyQualified, SymName, PredOrFunc, TVarSet,
		ArgTypes, ModuleInfo, PredId, ProcId) :-
	( 
		get_pred_id(IsFullyQualified, SymName, PredOrFunc, TVarSet,
			ArgTypes, ModuleInfo, PredId0)
	->
		PredId = PredId0
	;
                % Undefined/invalid pred or func.
		% the type-checker should ensure that this never happens
		list__length(ArgTypes, Arity),
		hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
		prog_out__sym_name_to_string(SymName, Name2),
		string__int_to_string(Arity, ArityString),
		string__append_list(["get_pred_id_and_proc_id: ", 
			"undefined/invalid ", PredOrFuncStr,
			"\n`", Name2, "/", ArityString, "'"], Msg),
		error(Msg)

	),
	get_proc_id(ModuleInfo, PredId, ProcId).

get_proc_id(ModuleInfo, PredId, ProcId) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	( ProcIds = [ProcId0] ->
		ProcId = ProcId0
	;
		pred_info_name(PredInfo, Name),
		pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
		pred_info_arity(PredInfo, Arity),
		hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
		string__int_to_string(Arity, ArityString),
		( ProcIds = [] ->
			string__append_list([
				"cannot take address of ", PredOrFuncStr,
				"\n`", Name, "/", ArityString,
				"' with no modes.\n",
				"(Sorry, confused by earlier errors -- ",
				"bailing out.)"],
				Message)
		;
			string__append_list([
				"sorry, not implemented: ",
				"taking address of ", PredOrFuncStr,
				"\n`", Name, "/", ArityString,
				"' with multiple modes.\n",
				"(use an explicit lambda expression instead)"],
				Message)
		),
		error(Message)
	).

lookup_builtin_pred_proc_id(Module, ModuleName, ProcName, PredOrFunc,
		Arity, ModeNo, PredId, ProcId) :-
	module_info_get_predicate_table(Module, PredTable),
	(
		(
			PredOrFunc = predicate,
			predicate_table_search_pred_m_n_a(PredTable,
				is_fully_qualified, ModuleName, ProcName, Arity,
				[PredId0])
		;
			PredOrFunc = function,
			predicate_table_search_func_m_n_a(PredTable,
				is_fully_qualified, ModuleName, ProcName, Arity,
				[PredId0])
		)
	->
		PredId = PredId0
	;
		% Some of the table builtins are polymorphic,
		% and for them we need to subtract one from the arity
		% to take into account the type_info argument.
		% XXX The caller should supply us with the exact arity.
		% Guessing how many of the arguments are typeinfos and/or
		% typeclass_infos, as this code here does, is error-prone
		% as well as inefficient.
		(
			PredOrFunc = predicate,
			predicate_table_search_pred_m_n_a(PredTable,
				is_fully_qualified, ModuleName, ProcName,
				Arity - 1, [PredId0])
		;
			PredOrFunc = function,
			predicate_table_search_func_m_n_a(PredTable,
				is_fully_qualified, ModuleName, ProcName,
				Arity - 1, [PredId0])
		)
	->
		PredId = PredId0
	;
		string__int_to_string(Arity, ArityS),
		string__append_list(["can't locate ", ProcName,
			"/", ArityS], ErrorMessage),
		error(ErrorMessage)
	),
	module_info_pred_info(Module, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	(
		ModeNo = only_mode,
		(
			ProcIds = [ProcId0]
		->
			ProcId = ProcId0
		;
			error(string__format( 
				"expected single mode for %s/%d",
				[s(ProcName), i(Arity)]))
		)
	;
		ModeNo = mode_no(N),
		(	       
			list__index0(ProcIds, N, ProcId0)
		->
			ProcId = ProcId0
		;
			error(string__format(
				"there is no mode %d for %s/%d",
				[i(N), s(ProcName), i(Arity)]))
		)
	).

%-----------------------------------------------------------------------------%

predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_module(PredInfo, ModuleName),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, Arity).

predicate_module(ModuleInfo, PredId, ModuleName) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_module(PredInfo, ModuleName).

predicate_name(ModuleInfo, PredId, PredName) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_name(PredInfo, PredName).

predicate_arity(ModuleInfo, PredId, Arity) :-
	module_info_preds(ModuleInfo, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_arity(PredInfo, Arity).

%-----------------------------------------------------------------------------%
