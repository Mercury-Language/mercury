%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1999 The University of Melbourne.
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

:- module hlds_module.

:- interface.

:- import_module prog_data, module_qual.
:- import_module hlds_pred, hlds_data, unify_proc, special_pred.
:- import_module globals, llds.
:- import_module relation, map, std_util, list, set, multi_map.

:- implementation.

:- import_module hlds_out, prog_out, prog_util.
:- import_module typecheck, modules.
:- import_module bool, require, int, string.

%-----------------------------------------------------------------------------%

:- interface.

:- type module_info.

:- type c_code_info
	--->	c_code_info(
			c_header_info,
			c_body_info
		).

:- type pragma_exported_proc	
	--->	pragma_exported_proc(
			pred_id,
			proc_id,
			string	% the name of the C function
		).

	% This structure contains the information we need to generate
	% a type_ctor_info structure for a type defined in this module.

:- type base_gen_info
	--->	base_gen_info(
			type_id,
			module_name,	% module name
			string,		% type name
			int,		% type arity
			import_status,	% of the type
			maybe(int),	% eliminated procs?
					% and how many if so
			list(pred_proc_id),
					% the ids of the procs
					% referred to from the
					% type_ctor_info
			hlds_type_defn	% defn of type
		).

	% This structure contains the information we need to generate
	% a type_ctor_layout structure for a type defined in this module.
	
:- type base_gen_layout
	--->	base_gen_layout(
			type_id,
			module_name,	% module name
			string,		% type name
			int,		% type arity
			import_status,	% of the type
			hlds_type_defn	% defn of type
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
	% global options).

:- pred module_info_init(module_name, globals, partial_qualifier_info,
		module_info).
:- mode module_info_init(in, in, in, out) is det.

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

	% The cell count is used as a unique cell number for
	% constants in the generated C code.
:- pred module_info_get_cell_count(module_info, int).
:- mode module_info_get_cell_count(in, out) is det.

:- pred module_info_set_cell_count(module_info, int, module_info).
:- mode module_info_set_cell_count(in, in, out) is det.

:- pred module_add_imported_module_specifiers(list(module_specifier),
		module_info, module_info).
:- mode module_add_imported_module_specifiers(in, in, out) is det.

:- pred module_info_get_imported_module_specifiers(module_info,
		set(module_specifier)).
:- mode module_info_get_imported_module_specifiers(in, out) is det.

%-----------------------------------------------------------------------------%

:- pred module_info_name(module_info, module_name).
:- mode module_info_name(in, out) is det.

:- pred module_info_globals(module_info, globals).
:- mode module_info_globals(in, out) is det.

:- pred module_info_set_globals(module_info, globals, module_info).
:- mode module_info_set_globals(in, in, out) is det.

:- pred module_info_get_c_header(module_info, c_header_info).
:- mode module_info_get_c_header(in, out) is det.

:- pred module_info_set_c_header(module_info, c_header_info, module_info).
:- mode module_info_set_c_header(in, in, out) is det.

:- pred module_info_get_c_body_code(module_info, c_body_info).
:- mode module_info_get_c_body_code(in, out) is det.

:- pred module_info_set_c_body_code(module_info, c_body_info, module_info).
:- mode module_info_set_c_body_code(in, in, out) is det.

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

:- pred module_info_base_gen_infos(module_info, list(base_gen_info)).
:- mode module_info_base_gen_infos(in, out) is det.

:- pred module_info_set_base_gen_infos(module_info, list(base_gen_info),
	module_info).
:- mode module_info_set_base_gen_infos(in, in, out) is det.

:- pred module_info_base_gen_layouts(module_info, list(base_gen_layout)).
:- mode module_info_base_gen_layouts(in, out) is det.

:- pred module_info_set_base_gen_layouts(module_info, list(base_gen_layout),
	module_info).
:- mode module_info_set_base_gen_layouts(in, in, out) is det.

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

:- pred module_info_typeids(module_info, list(type_id)).
:- mode module_info_typeids(in, out) is det.

:- pred module_info_instids(module_info, list(inst_id)).
:- mode module_info_instids(in, out) is det.

:- pred module_info_modeids(module_info, list(mode_id)).
:- mode module_info_modeids(in, out) is det.

:- pred module_info_consids(module_info, list(cons_id)).
:- mode module_info_consids(in, out) is det.

:- pred module_info_dependency_info(module_info, dependency_info).
:- mode module_info_dependency_info(in, out) is det.

:- pred module_info_aditi_dependency_ordering(module_info, 
		aditi_dependency_ordering).
:- mode module_info_aditi_dependency_ordering(in, out) is det.

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

:- pred module_info_get_sub_info(module_info, module_sub_info).
:- mode module_info_get_sub_info(in, out) is det.

:- pred module_info_set_sub_info(module_info, module_sub_info, module_info).
:- mode module_info_set_sub_info(in, in, out) is det.

:- pred module_info_get_lambda_count(module_info, int).
:- mode module_info_get_lambda_count(in, out) is det.

:- pred module_info_set_lambda_count(module_info, int, module_info).
:- mode module_info_set_lambda_count(in, in, out) is det.

:- pred module_info_get_model_non_pragma_count(module_info, int).
:- mode module_info_get_model_non_pragma_count(in, out) is det.

:- pred module_info_set_model_non_pragma_count(module_info, int, module_info).
:- mode module_info_set_model_non_pragma_count(in, in, out) is det.

:- pred module_info_set_maybe_dependency_info(module_info,
	maybe(dependency_info), module_info).
:- mode module_info_set_maybe_dependency_info(in, in, out) is det.

:- pred module_sub_get_name(module_sub_info, module_name).
:- mode module_sub_get_name(in, out) is det.

:- pred module_sub_get_globals(module_sub_info, globals).
:- mode module_sub_get_globals(in, out) is det.

:- pred module_sub_set_globals(module_sub_info, globals, module_sub_info).
:- mode module_sub_set_globals(in, in, out) is det.

:- pred module_sub_get_c_header_info(module_sub_info, c_header_info).
:- mode module_sub_get_c_header_info(in, out) is det.

:- pred module_sub_get_c_body_info(module_sub_info, c_body_info).
:- mode module_sub_get_c_body_info(in, out) is det.

:- pred module_sub_get_maybe_dependency_info(module_sub_info,
	maybe(dependency_info)).
:- mode module_sub_get_maybe_dependency_info(in, out) is det.

:- pred module_sub_get_num_errors(module_sub_info, int).
:- mode module_sub_get_num_errors(in, out) is det.

:- pred module_sub_get_lambda_count(module_sub_info, int).
:- mode module_sub_get_lambda_count(in, out) is det.

:- pred module_sub_get_pragma_exported_procs(module_sub_info,
	list(pragma_exported_proc)).
:- mode module_sub_get_pragma_exported_procs(in, out) is det.

:- pred module_sub_get_base_gen_infos(module_sub_info, list(base_gen_info)).
:- mode module_sub_get_base_gen_infos(in, out) is det.

:- pred module_sub_get_base_gen_layouts(module_sub_info, list(base_gen_layout)).
:- mode module_sub_get_base_gen_layouts(in, out) is det.

:- pred module_sub_get_stratified_preds(module_sub_info, set(pred_id)).
:- mode module_sub_get_stratified_preds(in, out) is det.

:- pred module_sub_get_unused_arg_info(module_sub_info, unused_arg_info).
:- mode module_sub_get_unused_arg_info(in, out) is det.

:- pred module_sub_get_model_non_pragma_count(module_sub_info, int).
:- mode module_sub_get_model_non_pragma_count(in, out) is det.

:- pred module_sub_get_imported_module_specifiers(module_sub_info,
		set(module_specifier)).
:- mode module_sub_get_imported_module_specifiers(in, out) is det.

:- pred module_sub_get_do_aditi_compilation(module_sub_info,
		do_aditi_compilation).
:- mode module_sub_get_do_aditi_compilation(in, out) is det.

:- pred module_sub_get_type_spec_info(module_sub_info, type_spec_info).
:- mode module_sub_get_type_spec_info(in, out) is det.

:- pred module_sub_set_c_header_info(module_sub_info, c_header_info,
	module_sub_info).
:- mode module_sub_set_c_header_info(in, in, out) is det.

:- pred module_sub_set_c_body_info(module_sub_info, c_body_info,
	module_sub_info).
:- mode module_sub_set_c_body_info(in, in, out) is det.

:- pred module_sub_set_maybe_dependency_info(module_sub_info,
	maybe(dependency_info), module_sub_info).
:- mode module_sub_set_maybe_dependency_info(in, in, out) is det.

:- pred module_sub_set_num_errors(module_sub_info, int, module_sub_info).
:- mode module_sub_set_num_errors(in, in, out) is det.

:- pred module_sub_set_lambda_count(module_sub_info, int, module_sub_info).
:- mode module_sub_set_lambda_count(in, in, out) is det.

:- pred module_sub_set_pragma_exported_procs(module_sub_info,
	list(pragma_exported_proc), module_sub_info).
:- mode module_sub_set_pragma_exported_procs(in, in, out) is det.

:- pred module_sub_set_base_gen_infos(module_sub_info, list(base_gen_info),
	module_sub_info).
:- mode module_sub_set_base_gen_infos(in, in, out) is det.

:- pred module_sub_set_base_gen_layouts(module_sub_info, list(base_gen_layout),
	module_sub_info).
:- mode module_sub_set_base_gen_layouts(in, in, out) is det.

:- pred module_sub_set_stratified_preds(module_sub_info, set(pred_id),
	module_sub_info).
:- mode module_sub_set_stratified_preds(in, in, out) is det.

:- pred module_sub_set_unused_arg_info(module_sub_info, unused_arg_info,
	module_sub_info).
:- mode module_sub_set_unused_arg_info(in, in, out) is det.

:- pred module_sub_set_model_non_pragma_count(module_sub_info, int,
	module_sub_info).
:- mode module_sub_set_model_non_pragma_count(in, in, out) is det.

:- pred module_sub_set_imported_module_specifiers(module_sub_info,
		set(module_specifier), module_sub_info).
:- mode module_sub_set_imported_module_specifiers(in, in, out) is det.

:- pred module_sub_set_do_aditi_compilation(module_sub_info, module_sub_info).
:- mode module_sub_set_do_aditi_compilation(in, out) is det.

:- pred module_sub_set_type_spec_info(module_sub_info,
		type_spec_info, module_sub_info).
:- mode module_sub_set_type_spec_info(in, in, out) is det.

:- type module_info
	--->	module(
			module_sub_info,
			predicate_table,
			proc_requests,
			special_pred_map,
			partial_qualifier_info,
			type_table,
			inst_table,
			mode_table,
			cons_table,
			class_table,
			instance_table,
			superclass_table,
			assertion_table,
			int		% cell count, passed into code_info
					% and used to generate unique label
					% numbers for constant terms in the
					% generated C code
		).

:- type module_sub_info
	--->	module_sub(
			module_name,	% module name
			globals, 	% global options
			c_header_info,
			c_body_info,
			maybe(dependency_info),
			int,		% number of errors
			int,		% lambda predicate counter
			list(pragma_exported_proc),
					% list of the procs for which
					% there is a pragma export(...)
					% declaration
			list(base_gen_info),
			list(base_gen_layout),
					% info about the types defined here
			set(pred_id),
					% preds which must be stratified
			unused_arg_info,
					% unused argument info about
					% predicates in the current
					% module which has been exported
					% in .opt files.
			int,		% number of the structure types defined
					% so far for model_non pragma C codes
			set(module_specifier),
					% All the imported module specifiers
					% (used during type checking).
			do_aditi_compilation,
					% are there any local Aditi predicates
					% for which Aditi-RL must be produced.
			type_spec_info
					% data used for user-guided type
					% specialization.
		).

	% A predicate which creates an empty module

module_info_init(Name, Globals, QualifierInfo, ModuleInfo) :-
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
	mercury_public_builtin_module(PublicBuiltin),
	mercury_private_builtin_module(PrivateBuiltin),
	set__list_to_set([PublicBuiltin, PrivateBuiltin], ImportedModules),

	assertion_table_init(AssertionTable),

	ModuleSubInfo = module_sub(Name, Globals, [], [], no, 0, 0, [], 
		[], [], StratPreds, UnusedArgInfo, 0, ImportedModules,
		no_aditi_compilation, TypeSpecInfo),
	ModuleInfo = module(ModuleSubInfo, PredicateTable, Requests,
		UnifyPredMap, QualifierInfo, Types, Insts, Modes, Ctors,
		ClassTable, SuperClassTable, InstanceTable, AssertionTable, 0).

%-----------------------------------------------------------------------------%

% :- type module_sub_info
%	--->	module_sub(
% A			module_name,	% module name
% B			globals, 	% global options
% C			c_header_info,
% D			c_body_info,
% E			maybe(dependency_info),
% F			int,		% number of errors
% G			int,		% lambda predicate counter
% H			list(pragma_exported_proc),
%					% list of the procs for which
%					% there is a pragma export(...)
%					% declaration
% I			list(base_gen_info),
% J			list(base_gen_layout)
%					% info about the types defined here
% K			set(pred_id),
%					% preds which must be stratified
% L			unused_arg_info,
%					% unused argument info about
%					% predicates in the current
%					% module which has been exported
%					% in .opt files.
% M			int,		% number of the structure types defined
%					% so far for model_non pragma C codes
% N			set(module_name),
%					% All the imported module names
%					% (used during type checking).
% O			do_aditi_compilation
%					% are there any local Aditi predicates
%					% for which Aditi-RL must be produced.
% P			type_spec_info
%		).

%-----------------------------------------------------------------------------%

	% Various predicates which access the module_sub_info data structure.

module_sub_get_name(MI0, A) :-
	MI0 = module_sub(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

module_sub_get_globals(MI0, B) :-
	MI0 = module_sub(_, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _).

module_sub_get_c_header_info(MI0, C) :-
	MI0 = module_sub(_, _, C, _, _, _, _, _, _, _, _, _, _, _, _, _).

module_sub_get_c_body_info(MI0, D) :-
	MI0 = module_sub(_, _, _, D, _, _, _, _, _, _, _, _, _, _, _, _).

module_sub_get_maybe_dependency_info(MI0, E) :-
	MI0 = module_sub(_, _, _, _, E, _, _, _, _, _, _, _, _, _, _, _).

module_sub_get_num_errors(MI0, F) :-
	MI0 = module_sub(_, _, _, _, _, F, _, _, _, _, _, _, _, _, _, _).

module_sub_get_lambda_count(MI0, G) :-
	MI0 = module_sub(_, _, _, _, _, _, G, _, _, _, _, _, _, _, _, _).

module_sub_get_pragma_exported_procs(MI0, H) :-
	MI0 = module_sub(_, _, _, _, _, _, _, H, _, _, _, _, _, _, _, _).

module_sub_get_base_gen_infos(MI0, I) :-
	MI0 = module_sub(_, _, _, _, _, _, _, _, I, _, _, _, _, _, _, _).

module_sub_get_base_gen_layouts(MI0, J) :-
	MI0 = module_sub(_, _, _, _, _, _, _, _, _, J, _, _, _, _, _, _).

module_sub_get_stratified_preds(MI0, K) :-
	MI0 = module_sub(_, _, _, _, _, _, _, _, _, _, K, _, _, _, _, _).

module_sub_get_unused_arg_info(MI0, L) :-
	MI0 = module_sub(_, _, _, _, _, _, _, _, _, _, _, L, _, _, _, _).

module_sub_get_model_non_pragma_count(MI0, M) :-
	MI0 = module_sub(_, _, _, _, _, _, _, _, _, _, _, _, M, _, _, _).

module_sub_get_imported_module_specifiers(MI0, N) :-
	MI0 = module_sub(_, _, _, _, _, _, _, _, _, _, _, _, _, N, _, _).

module_sub_get_do_aditi_compilation(MI0, O) :-
	MI0 = module_sub(_, _, _, _, _, _, _, _, _, _, _, _, _, _, O, _).

module_sub_get_type_spec_info(MI0, P) :-
	MI0 = module_sub(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, P).

%-----------------------------------------------------------------------------%

	% Various predicates which modify the module_sub_info data structure.

module_sub_set_globals(MI0, B, MI) :-
	MI0 = module_sub(A, _, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_c_header_info(MI0, C, MI) :-
	MI0 = module_sub(A, B, _, D, E, F, G, H, I, J, K, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_c_body_info(MI0, D, MI) :-
	MI0 = module_sub(A, B, C, _, E, F, G, H, I, J, K, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_maybe_dependency_info(MI0, E, MI) :-
	MI0 = module_sub(A, B, C, D, _, F, G, H, I, J, K, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_num_errors(MI0, F, MI) :-
	MI0 = module_sub(A, B, C, D, E, _, G, H, I, J, K, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_lambda_count(MI0, G, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, _, H, I, J, K, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_pragma_exported_procs(MI0, H, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, G, _, I, J, K, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_base_gen_infos(MI0, I, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, G, H, _, J, K, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_base_gen_layouts(MI0, J, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_stratified_preds(MI0, K, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_unused_arg_info(MI0, L, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, G, H, I, J, K, _, M, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_model_non_pragma_count(MI0, M, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_imported_module_specifiers(MI0, N, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, _, O, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

module_sub_set_do_aditi_compilation(MI0, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, _, P),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N,
		do_aditi_compilation, P).

module_sub_set_type_spec_info(MI0, P, MI) :-
	MI0 = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, _),
	MI  = module_sub(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P).

%-----------------------------------------------------------------------------%

% :- type module_info
%	--->	module(
% A			module_sub_info,
% B			predicate_table,
% C			proc_requests,
% D			special_pred_map,
% E			partial_qualifier_info,
% F			type_table,
% G			inst_table,
% H			mode_table,
% I			cons_table,
% J			class_table,
% K			instance_table,
% L			superclass_table,
% M			assertion_table
% N			int		% cell count, passed into code_info
%					% and used to generate unique label
%					% numbers for constant terms in the
%					% generated C code
%		).

%-----------------------------------------------------------------------------%

	% Various predicates which access the module_info data structure.

module_info_get_sub_info(MI0, A) :-
	MI0 = module(A, _, _, _, _, _, _, _, _, _, _, _, _, _).

module_info_get_predicate_table(MI0, B) :-
	MI0 = module(_, B, _, _, _, _, _, _, _, _, _, _, _, _).

module_info_get_proc_requests(MI0, C) :-
	MI0 = module(_, _, C, _, _, _, _, _, _, _, _, _, _, _).

module_info_get_special_pred_map(MI0, D) :-
	MI0 = module(_, _, _, D, _, _, _, _, _, _, _, _, _, _).

module_info_get_partial_qualifier_info(MI0, E) :-
	MI0 = module(_, _, _, _, E, _, _, _, _, _, _, _, _, _).

module_info_types(MI0, F) :-
	MI0 = module(_, _, _, _, _, F, _, _, _, _, _, _, _, _).

module_info_insts(MI0, G) :-
	MI0 = module(_, _, _, _, _, _, G, _, _, _, _, _, _, _).

module_info_modes(MI0, H) :-
	MI0 = module(_, _, _, _, _, _, _, H, _, _, _, _, _, _).

module_info_ctors(MI0, I) :-
	MI0 = module(_, _, _, _, _, _, _, _, I, _, _, _, _, _).

module_info_classes(MI0, J) :-
	MI0 = module(_, _, _, _, _, _, _, _, _, J, _, _, _, _).

module_info_instances(MI0, K) :-
	MI0 = module(_, _, _, _, _, _, _, _, _, _, K, _, _, _).

module_info_superclasses(MI0, L) :-
	MI0 = module(_, _, _, _, _, _, _, _, _, _, _, L, _, _).

module_info_assertion_table(MI0, M) :-
	MI0 = module(_, _, _, _, _, _, _, _, _, _, _, _, M, _).

module_info_get_cell_count(MI0, N) :-
	MI0 = module(_, _, _, _, _, _, _, _, _, _, _, _, _, N).

%-----------------------------------------------------------------------------%

	% Various predicates which modify the module_info data structure.

module_info_set_sub_info(MI0, A, MI) :-
	MI0 = module(_, B, C, D, E, F, G, H, I, J, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_predicate_table(MI0, B, MI) :-
	MI0 = module(A, _, C, D, E, F, G, H, I, J, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_proc_requests(MI0, C, MI) :-
	MI0 = module(A, B, _, D, E, F, G, H, I, J, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_special_pred_map(MI0, D, MI) :-
	MI0 = module(A, B, C, _, E, F, G, H, I, J, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_partial_qualifier_info(MI0, E, MI) :-
	MI0 = module(A, B, C, D, _, F, G, H, I, J, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_types(MI0, F, MI) :-
	MI0 = module(A, B, C, D, E, _, G, H, I, J, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_insts(MI0, G, MI) :-
	MI0 = module(A, B, C, D, E, F, _, H, I, J, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_modes(MI0, H, MI) :-
	MI0 = module(A, B, C, D, E, F, G, _, I, J, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_ctors(MI0, I, MI) :-
	MI0 = module(A, B, C, D, E, F, G, H, _, J, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_classes(MI0, J, MI) :-
	MI0 = module(A, B, C, D, E, F, G, H, I, _, K, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_instances(MI0, K, MI) :-
	MI0 = module(A, B, C, D, E, F, G, H, I, J, _, L, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_superclasses(MI0, L, MI) :-
	MI0 = module(A, B, C, D, E, F, G, H, I, J, K, _, M, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_assertion_table(MI0, M, MI) :-
	MI0 = module(A, B, C, D, E, F, G, H, I, J, K, L, _, N),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

module_info_set_cell_count(MI0, N, MI) :-
	MI0 = module(A, B, C, D, E, F, G, H, I, J, K, L, M, _),
	MI  = module(A, B, C, D, E, F, G, H, I, J, K, L, M, N).

%-----------------------------------------------------------------------------%

	% Various predicates which access the module_sub_info data structure
	% via the module_info structure.

module_info_name(MI0, A) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_name(MS0, A).

module_info_globals(MI0, B) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_globals(MS0, B).

module_info_get_c_header(MI0, C) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_c_header_info(MS0, C).

module_info_get_c_body_code(MI0, D) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_c_body_info(MS0, D).

module_info_get_maybe_dependency_info(MI0, E) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_maybe_dependency_info(MS0, E).

module_info_num_errors(MI0, F) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_num_errors(MS0, F).

module_info_get_lambda_count(MI0, G) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_lambda_count(MS0, G).

module_info_get_pragma_exported_procs(MI0, H) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_pragma_exported_procs(MS0, H).

module_info_base_gen_infos(MI0, I) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_base_gen_infos(MS0, I).

module_info_base_gen_layouts(MI0, J) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_base_gen_layouts(MS0, J).

module_info_stratified_preds(MI0, K) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_stratified_preds(MS0, K).

module_info_unused_arg_info(MI0, L) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_unused_arg_info(MS0, L).

module_info_get_model_non_pragma_count(MI0, M) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_model_non_pragma_count(MS0, M).

module_info_get_imported_module_specifiers(MI0, N) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_imported_module_specifiers(MS0, N).

module_info_type_spec_info(MI0, P) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_type_spec_info(MS0, P).

module_info_get_do_aditi_compilation(MI0, O) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_do_aditi_compilation(MS0, O).

%-----------------------------------------------------------------------------%

	% Various predicates which modify the module_sub_info data structure
	% via the module_info structure.

module_info_set_globals(MI0, B, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_globals(MS0, B, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_c_header(MI0, C, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_c_header_info(MS0, C, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_c_body_code(MI0, D, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_c_body_info(MS0, D, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_maybe_dependency_info(MI0, E, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_maybe_dependency_info(MS0, E, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_num_errors(MI0, F, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_num_errors(MS0, F, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_lambda_count(MI0, G, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_lambda_count(MS0, G, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_pragma_exported_procs(MI0, H, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_pragma_exported_procs(MS0, H, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_base_gen_infos(MI0, I, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_base_gen_infos(MS0, I, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_base_gen_layouts(MI0, J, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_base_gen_layouts(MS0, J, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_stratified_preds(MI0, K, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_stratified_preds(MS0, K, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_unused_arg_info(MI0, L, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_unused_arg_info(MS0, L, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_model_non_pragma_count(MI0, M, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_model_non_pragma_count(MS0, M, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_add_imported_module_specifiers(Ss, MI0, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_get_imported_module_specifiers(MS0, SpecSet0),
	set__insert_list(SpecSet0, Ss, SpecSet),
	module_sub_set_imported_module_specifiers(MS0, SpecSet, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_do_aditi_compilation(MI0, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_do_aditi_compilation(MS0, MS),
	module_info_set_sub_info(MI0, MS, MI).

module_info_set_type_spec_info(MI0, P, MI) :-
	module_info_get_sub_info(MI0, MS0),
	module_sub_set_type_spec_info(MS0, P, MS),
	module_info_set_sub_info(MI0, MS, MI).

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

module_info_typeids(MI, TypeIds) :-
	module_info_types(MI, Types),
	map__keys(Types, TypeIds).

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
	Errs is Errs0 + 1,
	module_info_set_num_errors(MI0, Errs, MI).

module_info_next_lambda_count(MI0, Count, MI) :-
	module_info_get_lambda_count(MI0, Count0),
	Count is Count0 + 1,
	module_info_set_lambda_count(MI0, Count, MI).

module_info_next_model_non_pragma_count(MI0, Count, MI) :-
	module_info_get_model_non_pragma_count(MI0, Count0),
	Count is Count0 + 1,
	module_info_set_model_non_pragma_count(MI0, Count, MI).

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- type dependency_ordering		== list(list(pred_proc_id)).

:- type aditi_dependency_ordering	== list(aditi_scc).

	% Each Aditi SCC contains one or more SCCs from the original 
	% dependency ordering and the entry points of the SCC.
	% SCCs which are only called from one other SCC and are not
	% called through negation or aggregation are merged into the
	% parent SCC. This makes the low-level RL optimizations more
	% effective while maintaining stratification. 
:- type aditi_scc
	--->	aditi_scc(dependency_ordering, list(pred_proc_id)).

:- type dependency_graph		== relation(pred_proc_id).
:- type dependency_info.

:- pred hlds_dependency_info_init(dependency_info).
:- mode hlds_dependency_info_init(out) is det.

:- pred hlds_dependency_info_get_dependency_graph(dependency_info, 
	dependency_graph).
:- mode hlds_dependency_info_get_dependency_graph(in, out) is det.

:- pred hlds_dependency_info_get_dependency_ordering(dependency_info, 
	dependency_ordering).
:- mode hlds_dependency_info_get_dependency_ordering(in, out) is det.

:- pred hlds_dependency_info_get_maybe_aditi_dependency_ordering(
		dependency_info, maybe(aditi_dependency_ordering)).
:- mode hlds_dependency_info_get_maybe_aditi_dependency_ordering(in, 
		out) is det.

:- pred hlds_dependency_info_set_dependency_graph(dependency_info,
	dependency_graph, dependency_info).
:- mode hlds_dependency_info_set_dependency_graph(in, in, out) is det.

:- pred hlds_dependency_info_set_dependency_ordering(dependency_info,
	dependency_ordering, dependency_info).
:- mode hlds_dependency_info_set_dependency_ordering(in, in, out) is det.

:- pred hlds_dependency_info_set_aditi_dependency_ordering(dependency_info,
	aditi_dependency_ordering, dependency_info).
:- mode hlds_dependency_info_set_aditi_dependency_ordering(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type dependency_info --->
		dependency_info(
			dependency_graph,	% Dependency graph
			dependency_ordering,	% Dependency ordering
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

:- pred predicate_table_search_sym(predicate_table, sym_name, list(pred_id)).
:- mode predicate_table_search_sym(in, in, out) is semidet.

:- pred predicate_table_search_pred_sym(predicate_table, sym_name,
					list(pred_id)).
:- mode predicate_table_search_pred_sym(in, in, out) is semidet.

:- pred predicate_table_search_func_sym(predicate_table, sym_name,
					list(pred_id)).
:- mode predicate_table_search_func_sym(in, in, out) is semidet.

	% Search the table for (a) predicates or functions
	% (b) predicates only or (c) functions only matching this
	% (possibly module-qualified) sym_name & arity.

:- pred predicate_table_search_sym_arity(predicate_table, sym_name, arity,
					list(pred_id)).
:- mode predicate_table_search_sym_arity(in, in, in, out) is semidet.

:- pred predicate_table_search_pred_sym_arity(predicate_table, sym_name, arity,
					list(pred_id)).
:- mode predicate_table_search_pred_sym_arity(in, in, in, out) is semidet.

:- pred predicate_table_search_func_sym_arity(predicate_table, sym_name, arity,
					list(pred_id)).
:- mode predicate_table_search_func_sym_arity(in, in, in, out) is semidet.

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

:- pred predicate_table_search_m_n_a(predicate_table, module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_m_n_a(in, in, in, in, out) is semidet.

:- pred predicate_table_search_pred_m_n_a(predicate_table, module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_pred_m_n_a(in, in, in, in, out) is semidet.

:- pred predicate_table_search_func_m_n_a(predicate_table, module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_func_m_n_a(in, in, in, in, out) is semidet.

	% Search the table for predicates or functions matching
	% this pred_or_func category, module, name, and arity.
	% When searching for functions, the arity used
	% is the arity of the predicate that the function gets converted
	% to, i.e. the arity of the function plus one.
	% NB.  This is opposite to what happens with the search
	% predicates declared above!!

:- pred predicate_table_search_pf_m_n_a(predicate_table, pred_or_func,
						module_name, string,
						arity, list(pred_id)).
:- mode predicate_table_search_pf_m_n_a(in, in, in, in, in, out) is semidet.

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

:- pred predicate_table_search_pf_sym_arity(predicate_table, pred_or_func,
				sym_name, arity, list(pred_id)) is semidet.
:- mode predicate_table_search_pf_sym_arity(in, in, in, in, out) is semidet.

	% Search the table for predicates or functions matching
	% this pred_or_func category and sym_name.

:- pred predicate_table_search_pf_sym(predicate_table, pred_or_func,
				sym_name, list(pred_id)) is semidet.
:- mode predicate_table_search_pf_sym(in, in, in, out) is semidet.

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
:- pred get_pred_id_and_proc_id(sym_name, pred_or_func, tvarset, list(type),
				module_info, pred_id, proc_id).
:- mode get_pred_id_and_proc_id(in, in, in, in, in, out, out) is det.

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
		FuncArity is Arity - 1,
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

predicate_table_search_sym(PredicateTable, unqualified(Name), PredIdList) :-
	predicate_table_search_name(PredicateTable, Name, PredIdList).
predicate_table_search_sym(PredicateTable, qualified(Module, Name),
		PredIdList) :-
	predicate_table_search_module_name(PredicateTable, 
		Module, Name, PredIdList),
	PredIdList \= [].

predicate_table_search_pred_sym(PredicateTable, unqualified(Name), PredIdList)
		:-
	predicate_table_search_pred_name(PredicateTable, Name, PredIdList).
predicate_table_search_pred_sym(PredicateTable, qualified(Module, Name),
		PredIdList) :-
	predicate_table_search_pred_module_name(PredicateTable, 
		Module, Name, PredIdList),
	PredIdList \= [].

predicate_table_search_func_sym(PredicateTable, unqualified(Name), PredIdList)
		:-
	predicate_table_search_func_name(PredicateTable, Name, PredIdList).
predicate_table_search_func_sym(PredicateTable, qualified(Module, Name),
		PredIdList) :-
	predicate_table_search_func_module_name(PredicateTable, Module,
		Name, PredIdList),
	PredIdList \= [].

	% Given a list of predicates, and a module name, find all the
	% predicates which came from that module.

%-----------------------------------------------------------------------------%

predicate_table_search_sym_arity(PredicateTable, qualified(Module, Name),
		Arity, PredIdList) :-
	predicate_table_search_m_n_a(PredicateTable, Module, Name, Arity,
		PredIdList).
predicate_table_search_sym_arity(PredicateTable, unqualified(Name),
		Arity, PredIdList) :-
	predicate_table_search_name_arity(PredicateTable, Name, Arity,
		PredIdList).

predicate_table_search_pred_sym_arity(PredicateTable, qualified(Module, Name),
		Arity, PredIdList) :-
	predicate_table_search_pred_m_n_a(PredicateTable, Module, Name, Arity,
		PredIdList).
predicate_table_search_pred_sym_arity(PredicateTable, unqualified(Name),
		Arity, PredIdList) :-
	predicate_table_search_pred_name_arity(PredicateTable, Name, Arity,
		PredIdList).

predicate_table_search_func_sym_arity(PredicateTable, qualified(Module, Name),
		Arity, PredIdList) :-
	predicate_table_search_func_m_n_a(PredicateTable, Module, Name, Arity,
		PredIdList).
predicate_table_search_func_sym_arity(PredicateTable, unqualified(Name),
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

:- pred predicate_table_search_module_name(predicate_table, module_name, 
		string, list(pred_id)).
:- mode predicate_table_search_module_name(in, in, in, out) is semidet.

predicate_table_search_module_name(PredicateTable, Module, Name, PredIds) :-
	(
		predicate_table_search_pred_module_name(PredicateTable, 
			Module, Name, PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_module_name(PredicateTable, 
			Module, Name, FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

:- pred predicate_table_search_pred_module_name(predicate_table, module_name,
		string, list(pred_id)).
:- mode predicate_table_search_pred_module_name(in, in, in, out) is semidet.

predicate_table_search_pred_module_name(PredicateTable, 
		Module, PredName, PredIds) :-
	PredicateTable = predicate_table(_,_,_,_,_, Pred_MNA_Index, _,_,_),
	map__search(Pred_MNA_Index, Module - PredName, Arities),
	map__values(Arities, PredIdLists),
	list__condense(PredIdLists, PredIds).

:- pred predicate_table_search_func_module_name(predicate_table, module_name,
		string, list(pred_id)).
:- mode predicate_table_search_func_module_name(in, in, in, out) is semidet.

predicate_table_search_func_module_name(PredicateTable, 
		Module, FuncName, PredIds) :-
	PredicateTable = predicate_table(_,_,_,_,_,_,_,_, Func_MNA_Index),
	map__search(Func_MNA_Index, Module - FuncName, Arities),
	map__values(Arities, PredIdLists),
	list__condense(PredIdLists, PredIds).

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

predicate_table_search_m_n_a(PredicateTable, Module, Name, Arity,
		PredIds) :-
	(
		predicate_table_search_pred_m_n_a(PredicateTable, Module,
			Name, Arity, PredPredIds0)
	->
		PredPredIds = PredPredIds0
	;
		PredPredIds = []
	),
	(
		predicate_table_search_func_m_n_a(PredicateTable, Module,
			Name, Arity, FuncPredIds0)
	->
		FuncPredIds = FuncPredIds0
	;
		FuncPredIds = []
	),
	list__append(FuncPredIds, PredPredIds, PredIds),
	PredIds \= [].

predicate_table_search_pred_m_n_a(PredicateTable, Module, PredName, Arity,
		PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, P_MNA_Index, _, _, _),
	map__search(P_MNA_Index, Module - PredName, ArityIndex),
	map__search(ArityIndex, Arity, PredIds).

predicate_table_search_func_m_n_a(PredicateTable, Module, FuncName, Arity,
		PredIds) :-
	PredicateTable = predicate_table(_, _, _, _, _, _, _, _, F_MNA_Index),
	map__search(F_MNA_Index, Module - FuncName, ArityIndex),
	map__search(ArityIndex, Arity, PredIds).

%-----------------------------------------------------------------------------%

predicate_table_search_pf_m_n_a(PredicateTable, predicate, Module, Name, Arity,
		PredIds) :-
	predicate_table_search_pred_m_n_a(PredicateTable, Module, Name, Arity,
			PredIds).
predicate_table_search_pf_m_n_a(PredicateTable, function, Module, Name, Arity,
		PredIds) :-
	FuncArity is Arity - 1,
	predicate_table_search_func_m_n_a(PredicateTable, Module, Name,
			FuncArity, PredIds).

predicate_table_search_pf_name_arity(PredicateTable, predicate, Name, Arity,
		PredIds) :-
	predicate_table_search_pred_name_arity(PredicateTable, Name, Arity,
			PredIds).
predicate_table_search_pf_name_arity(PredicateTable, function, Name, Arity,
		PredIds) :-
	FuncArity is Arity - 1,
	predicate_table_search_func_name_arity(PredicateTable, Name, FuncArity,
			PredIds).

predicate_table_search_pf_sym_arity(PredicateTable, PredOrFunc,
		qualified(Module, Name), Arity, PredIdList) :-
	predicate_table_search_pf_m_n_a(PredicateTable, PredOrFunc,
		Module, Name, Arity, PredIdList).
predicate_table_search_pf_sym_arity(PredicateTable, PredOrFunc,
		unqualified(Name), Arity, PredIdList) :-
	predicate_table_search_pf_name_arity(PredicateTable, PredOrFunc,
		Name, Arity, PredIdList).

predicate_table_search_pf_sym(PredicateTable, predicate,
		SymName, PredIdList) :-
	predicate_table_search_pred_sym(PredicateTable, SymName, PredIdList).
predicate_table_search_pf_sym(PredicateTable, function, SymName, PredIdList) :-
	predicate_table_search_func_sym(PredicateTable, SymName, PredIdList).

%-----------------------------------------------------------------------------%

predicate_table_insert(PredicateTable0, PredInfo, PredId, PredicateTable) :-
	predicate_table_insert_2(PredicateTable0, PredInfo,
			must_be_qualified, no, PredId, PredicateTable).

predicate_table_insert(PredicateTable0, PredInfo, NeedQual, QualInfo,
		PredId, PredicateTable) :-
	predicate_table_insert_2(PredicateTable0, PredInfo,
			NeedQual, yes(QualInfo),
			PredId, PredicateTable).

:- pred predicate_table_insert_2(predicate_table, pred_info, need_qualifier,
		maybe(partial_qualifier_info), pred_id, predicate_table).
:- mode predicate_table_insert_2(in, in, in, in, out, out) is det.

predicate_table_insert_2(PredicateTable0, PredInfo, NeedQual, MaybeQualInfo,
		PredId, PredicateTable) :-

	PredicateTable0 = predicate_table(Preds0, NextPredId0, PredIds0,
				Pred_N_Index0, Pred_NA_Index0, Pred_MNA_Index0,
				Func_N_Index0, Func_NA_Index0, Func_MNA_Index0),
	pred_info_module(PredInfo, Module),
	pred_info_name(PredInfo, Name),
	pred_info_arity(PredInfo, Arity),

		% allocate a new pred_id
	PredId = NextPredId0,
	hlds_pred__next_pred_id(PredId, NextPredId),

		% insert the pred_id into either the function or predicate
		% indices, as appropriate
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	( 
		PredOrFunc = predicate,
		predicate_table_do_insert(Module, Name, Arity,
			NeedQual, MaybeQualInfo, PredId,
			Pred_N_Index0, Pred_N_Index, 
			Pred_NA_Index0, Pred_NA_Index,
			Pred_MNA_Index0, Pred_MNA_Index),

		Func_N_Index = Func_N_Index0,
		Func_NA_Index = Func_NA_Index0,
		Func_MNA_Index = Func_MNA_Index0
	;
		PredOrFunc = function,

		FuncArity is Arity - 1,

		predicate_table_do_insert(Module, Name, FuncArity,
			NeedQual, MaybeQualInfo, PredId,
			Func_N_Index0, Func_N_Index, 
			Func_NA_Index0, Func_NA_Index,
			Func_MNA_Index0, Func_MNA_Index),

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
	name_arity_index, module_name_arity_index, module_name_arity_index).
:- mode predicate_table_do_insert(in, in, in, in, in, in,
	in, out, in, out, in, out) is det.

predicate_table_do_insert(Module, Name, Arity, NeedQual, MaybeQualInfo,
		PredId, N_Index0, N_Index, NA_Index0, NA_Index, 
		MNA_Index0, MNA_Index) :-
	( NeedQual = may_be_unqualified ->
			% insert the unqualified name into the name index
		multi_map__set(N_Index0, Name, PredId, N_Index),

			% insert the unqualified name/arity into the
			% name/arity index
		NA = Name / Arity,
		multi_map__set(NA_Index0, NA, PredId, NA_Index)
	;
		N_Index = N_Index0,
		NA_Index = NA_Index0
	),

	( MaybeQualInfo = yes(QualInfo) ->
			% insert partially module-qualified versions
			% of the name into the module:name/arity index
		get_partial_qualifiers(Module, QualInfo, PartialQuals),
		list__map_foldl(lambda([AncModule::in, AncModule::out,
				MNAs0::in, MNAs::out] is det,
			insert_into_mna_index(AncModule, Name, Arity, PredId,
					MNAs0, MNAs)),
			PartialQuals, _, MNA_Index0, MNA_Index1)
	;
		MNA_Index1 = MNA_Index0
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

get_pred_id_and_proc_id(SymName, PredOrFunc, TVarSet, ArgTypes, ModuleInfo,
			PredId, ProcId) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	list__length(ArgTypes, Arity),
	(
		predicate_table_search_pf_sym_arity(PredicateTable,
			PredOrFunc, SymName, Arity, PredIds),
		% Resolve overloading using the argument types. 
		typecheck__find_matching_pred_id(PredIds, ModuleInfo,
			TVarSet, ArgTypes, PredId0, _PredName)
	->
		PredId = PredId0,
		get_proc_id(PredicateTable, PredId, ProcId)
	;
		% Undefined/invalid pred or func.
		% the type-checker should ensure that this never happens
		hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncStr),
		prog_out__sym_name_to_string(SymName, Name2),
		string__int_to_string(Arity, ArityString),
		string__append_list(
			["get_pred_id_and_proc_id: ",
			"undefined/invalid ", PredOrFuncStr,
			"\n`", Name2, "/", ArityString, "'"],
			Msg),
		error(Msg)
	).

:- pred get_proc_id(predicate_table, pred_id, proc_id).
:- mode get_proc_id(in, in, out) is det.

get_proc_id(PredicateTable, PredId, ProcId) :-
	predicate_table_get_preds(PredicateTable, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, Procs),
	map__keys(Procs, ProcIds),
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
