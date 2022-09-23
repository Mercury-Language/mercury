%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_module.m.
% Main authors: fjh, conway.
%
% This module defines the main part of the High Level Data Structure or HLDS
% that deals with issues that concern the module as a whole.
%
% The main data structures defined here are the types
%
%   module_info
%
% There is a separate interface section for each of these.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_module.
:- interface.

:- import_module analysis.
:- import_module check_hlds.
:- import_module check_hlds.proc_requests.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_promise.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_data_used_modules.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_item.
:- import_module recompilation.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module one_or_more.
:- import_module pair.
:- import_module set.
:- import_module set_tree234.

%---------------------------------------------------------------------------%
%
% The module_info contains many fields of many types. These are the types
% that are not defined elsewhere.
%

:- type module_info.

:- type pragma_exported_proc
    --->    pragma_exported_proc(
                foreign_language,       % The language we are exporting to.
                pred_id,
                proc_id,
                string,                 % The exported name of the procedure.
                                        % i.e. the function name in C, method
                                        % name in Java etc.
                prog_context
            ).

    % This structure contains the information we need to generate
    % a type_ctor_info structure for a type defined in this module.

:- type type_ctor_gen_info
    --->    type_ctor_gen_info(
                type_ctor,
                module_name,            % module name
                string,                 % type name
                int,                    % type arity
                type_status,            % status of the type
                hlds_type_defn,         % defn of type
                pred_proc_id,           % unify procedure
                pred_proc_id            % compare procedure
                % maybe(pred_proc_id)   % prettyprinter, if relevant
            ).

    % Map from proc to a list of unused argument numbers.
    %
:- type unused_arg_info == map(pred_proc_id, list(int)).

    % For every procedure that requires its own tabling structure,
    % this field records the information needed to define that structure.
:- type table_struct_map == map(pred_proc_id, table_struct_info).

:- type table_struct_info
    --->    table_struct_info(
                table_struct_proc                   :: proc_table_struct_info,
                table_struct_attrs                  :: table_attributes
            ).

    % List of procedures for which there are user-requested type
    % specializations, and a list of predicates which should be processed
    % by higher_order.m to ensure the production of those versions.
:- type type_spec_info
    --->    type_spec_info(
                % Procedures for which there are user-requested type
                % specializations.
                user_req_procs      :: set(pred_proc_id),

                % Set of predicates which need to be processed by
                % higher_order.m to produce those specialized versions.
                must_process_preds  ::  set(pred_id),

                % Map from predicates for which the user requested a type
                % specialization to the list of predicates which must be
                % processed by higher_order.m to force the production of those
                % versions. This is used by dead_proc_elim.m to avoid creating
                % versions unnecessarily for versions in imported modules.
                user_to_process_map :: multi_map(pred_id, pred_id),

                % Type spec pragmas to be placed in the `.opt' file if a
                % predicate becomes exported.
                pragma_map          :: multi_map(pred_id,
                                        pragma_info_type_spec)
            ).

    % Once filled in by simplify_proc.m (for all non-lambda procedures)
    % and by lambda.m (for procedures created to implement lambda expressions),
    % this map should have an entry for every procedure that is of interest to
    % direct_arg_in_out.m. A procedure may be of interest to that module
    % because it has one or more arguments that it needs to clone,
    % or because it has one or more arguments whose modes do not specify
    % whether they need to be cloned, and for which therefore it should
    % generate a "sorry, not implemented" message. In both cases, the
    % one_or_more(int) specify the argument positions involved; in the latter
    % case, we also record the list of arguments for which we *could* tell
    % they need to be cloned.
    %
:- type direct_arg_proc_map == map(pred_proc_id, direct_arg_proc).
:- type direct_arg_proc
    --->    direct_arg_clone_proc(
                clone_daio_args     :: one_or_more(int)
            )
    ;       direct_arg_problem_proc(
                problem_args        :: one_or_more(int),
                no_problem_args     :: list(int)
            ).

    % Maps the full names of procedures (in the sense of complexity_proc_name
    % in complexity.m) to the number of their slot in MR_complexity_proc_table.
:- type complexity_proc_map == map(string, int).

:- type complexity_proc_info
    --->    complexity_proc_info(
                % The index of the procedure in the runtime system's
                % MR_complexity_procs array.
                complexity_proc_num     :: int,

                % The full name of the procedure, in the form
                % fqn/arity-modenum, where fqn is the predicate or function's
                % fully qualified name.
                complexity_proc_name    :: string,

                complexity_proc_args    :: list(complexity_arg_info)
            ).

:- type complexity_arg_info
    --->    complexity_arg_info(
                complexity_arg_name :: maybe(string),
                complexity_arg_kind :: complexity_arg_kind
            ).

:- type complexity_arg_kind
    --->    complexity_input_variable_size
    ;       complexity_input_fixed_size
    ;       complexity_output.

:- type exported_enum_info
    --->    exported_enum_info(
                % The type whose constants we are exporting to a foreign
                % language.
                eei_type_ctor               :: type_ctor,

                % The constants we are exporting.
                eei_constants               :: list(constructor_repn),

                % The language we are exporting to.
                eei_language                :: foreign_language,

                % This maps the names of the constructors in the Mercury type
                % to their names in the foreign language.
                eei_name_map                :: map(string, string),

                % The context of the foreign_export_enum pragma
                % that asked for all this.
                eei_context                 :: prog_context
            ).

:- type proc_analysis_kind
    --->    pak_exception
    ;       pak_trailing
    ;       pak_mm_tabling
    ;       pak_termination
    ;       pak_termination2
    ;       pak_structure_sharing
    ;       pak_structure_reuse.

    % Types for order-independent state update (oisu).
    %
:- type oisu_map == map(type_ctor, oisu_preds).
:- type oisu_preds
    --->    oisu_preds(
                op_creators     :: list(pred_id),
                op_mutators     :: list(pred_id),
                op_destructors  :: list(pred_id)
            ).

%---------------------------------------------------------------------------%
%
% The initialization predicate for the module_info data structure,
% and its raw getter and setter predicates.
%

    % Create an empty module_info for the module named in the given
    % compilation unit.
    %
:- pred module_info_init(globals::in, module_name::in, prog_context::in,
    string::in, include_module_map::in, used_modules::in, set(module_name)::in,
    partial_qualifier_info::in, maybe(recompilation_info)::in,
    type_repn_decision_data::in, module_info::out) is det.

    % Once the module_info has been built, we call module_info_optimize
    % to attempt to optimize the data structures for lots of accesses
    % and relatively few insertion/deletions. This was useful when
    % we stored maps using not-necessarily-balanced binary trees;
    % this was when we balanced them. Now that we store maps in 234-trees,
    % it is a no-op. However, we keep this predicate around, since we may
    % yet switch to newer data structures that could also benefit from
    % knowing when their access patterns change from mostly-write to
    % mostly read. For example, we could switch some tables from
    % 234-trees to arrays.
    %
:- pred module_info_optimize(module_info::in, module_info::out) is det.

:- type avail_module_map == map(module_name, avail_module_entry).
:- type avail_module_entry
    --->    avail_module_entry(
                % ms_interface iff any avail_module has ms_interface.
                module_section,

                % import_decl iff any avail_module has import_decl.
                % XXX CLEANUP This is wrong. A module may have
                % a ":- use_module" declaration in the interface and
                % an ":- import_module" declaration in the implementation,
                % and this design cannot express that.
                import_or_use,

                % The locations of the *explicit* import_module and
                % use_import declarations, in the *source* of the module.
                % The implicit ones aren't in the list, and neither
                % are the imports and uses read in from interface
                % and optimization files, so it is possible for the list
                % to be empty.
                list(avail_module)
            ).

:- type avail_module
    --->    avail_module(
                module_section,
                import_or_use,
                prog_context
            ).

    % The getter predicates. Please keep the order of declarations here
    % and the order of the clauses below in sync with the order of the
    % fields in the module_info and module_sub_info types.

:- pred module_info_get_globals(module_info::in, globals::out) is det.
:- pred module_info_get_predicate_table(module_info::in,
    predicate_table::out) is det.
:- pred module_info_get_type_table(module_info::in, type_table::out) is det.
:- pred module_info_get_no_tag_types(module_info::in,
    no_tag_type_table::out) is det.
:- pred module_info_get_inst_table(module_info::in, inst_table::out) is det.
:- pred module_info_get_mode_table(module_info::in, mode_table::out) is det.
:- pred module_info_get_cons_table(module_info::in, cons_table::out) is det.
:- pred module_info_get_ctor_field_table(module_info::in,
    ctor_field_table::out) is det.

:- pred module_info_get_special_pred_maps(module_info::in,
    special_pred_maps::out) is det.
:- pred module_info_get_class_table(module_info::in, class_table::out) is det.
:- pred module_info_get_instance_table(module_info::in,
    instance_table::out) is det.
:- pred module_info_get_type_spec_info(module_info::in,
    type_spec_info::out) is det.
:- pred module_info_get_const_struct_db(module_info::in,
    const_struct_db::out) is det.
:- pred module_info_get_c_j_cs_fims(module_info::in,
    c_j_cs_fims::out) is det.
:- pred module_info_get_pragma_exported_procs(module_info::in,
    cord(pragma_exported_proc)::out) is det.

:- pred module_info_get_name(module_info::in, module_name::out) is det.
:- pred module_info_get_name_context(module_info::in,
    prog_context::out) is det.
:- pred module_info_get_dump_hlds_base_file_name(module_info::in,
    string::out) is det.
:- pred module_info_get_include_module_map(module_info::in,
    include_module_map::out) is det.
:- pred module_info_get_partial_qualifier_info(module_info::in,
    partial_qualifier_info::out) is det.
:- pred module_info_get_maybe_recompilation_info(module_info::in,
    maybe(recompilation_info)::out) is det.
:- pred module_info_get_proc_requests(module_info::in,
    proc_requests::out) is det.
:- pred module_info_get_assertion_table(module_info::in,
    assertion_table::out) is det.
:- pred module_info_get_exclusive_table(module_info::in,
    exclusive_table::out) is det.
:- pred module_info_get_has_parallel_conj(module_info::in,
    has_parallel_conj::out) is det.
:- pred module_info_get_has_user_event(module_info::in,
    has_user_event::out) is det.
:- pred module_info_get_direct_arg_proc_map(module_info::in,
    direct_arg_proc_map::out) is det.
:- pred module_info_get_foreign_decl_codes_user(module_info::in,
    cord(foreign_decl_code)::out) is det.
:- pred module_info_get_foreign_decl_codes_aux(module_info::in,
    cord(foreign_decl_code)::out) is det.
:- pred module_info_get_foreign_body_codes(module_info::in,
    cord(foreign_body_code)::out) is det.
:- pred module_info_get_fact_table_file_names(module_info::in,
    list(string)::out) is det.
:- pred module_info_get_int_bad_clauses(module_info::in,
    set(pred_pf_name_arity)::out) is det.
:- pred module_info_get_maybe_dependency_info(module_info::in,
    maybe(hlds_dependency_info)::out) is det.
:- pred module_info_get_type_ctor_gen_infos(module_info::in,
    list(type_ctor_gen_info)::out) is det.
:- pred module_info_get_must_be_stratified_preds(module_info::in,
    set(pred_id)::out) is det.
:- pred module_info_get_unused_arg_info(module_info::in,
    unused_arg_info::out) is det.
:- pred module_info_get_table_struct_map(module_info::in,
    table_struct_map::out) is det.
:- pred module_info_get_avail_module_map(module_info::in,
    avail_module_map::out) is det.
:- pred module_info_get_used_modules(module_info::in,
    used_modules::out) is det.
:- pred module_info_get_ancestor_avail_modules(module_info::in,
    set(module_name)::out) is det.
:- pred module_info_get_maybe_complexity_proc_map(module_info::in,
    maybe(pair(int, complexity_proc_map))::out) is det.
:- pred module_info_get_complexity_proc_infos(module_info::in,
    list(complexity_proc_info)::out) is det.
:- pred module_info_get_proc_analysis_kinds(module_info::in,
    set(proc_analysis_kind)::out) is det.
:- pred module_info_get_analysis_info(module_info::in,
    analysis_info::out) is det.
:- pred module_info_get_user_init_pred_target_names(module_info::in,
    pred_target_names::out) is det.
:- pred module_info_get_user_final_pred_target_names(module_info::in,
    pred_target_names::out) is det.
:- pred module_info_get_structure_reuse_preds(module_info::in,
    set(pred_id)::out) is det.
:- pred module_info_get_format_call_pragma_preds(module_info::in,
    set(pred_id)::out) is det.
:- pred module_info_get_exported_enums(module_info::in,
    list(exported_enum_info)::out) is det.
:- pred module_info_get_event_set(module_info::in, event_set::out) is det.
:- pred module_info_get_oisu_map(module_info::in, oisu_map::out) is det.
:- pred module_info_get_oisu_procs(module_info::in,
    set(pred_proc_id)::out) is det.
:- pred module_info_get_ts_rev_string_table(module_info::in, int::out,
    list(string)::out) is det.
:- pred module_info_get_type_repn_dec(module_info::in,
    type_repn_decision_data::out) is det.

    % The setter predicates. Please keep the order of declarations here
    % and the order of the clauses below in sync with the order of the
    % fields in the module_info and module_sub_info types.

:- pred module_info_set_globals(globals::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_predicate_table(predicate_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_type_table(type_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_no_tag_types(no_tag_type_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_inst_table(inst_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_mode_table(mode_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_cons_table(cons_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_ctor_field_table(ctor_field_table::in,
    module_info::in, module_info::out) is det.

:- pred module_info_set_special_pred_maps(special_pred_maps::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_class_table(class_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_instance_table(instance_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_type_spec_info(type_spec_info::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_const_struct_db(const_struct_db::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_c_j_cs_fims(c_j_cs_fims::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_pragma_exported_procs(cord(pragma_exported_proc)::in,
    module_info::in, module_info::out) is det.

:- pred module_info_set_maybe_recompilation_info(maybe(recompilation_info)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_proc_requests(proc_requests::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_assertion_table(assertion_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_exclusive_table(exclusive_table::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_has_parallel_conj(
    module_info::in, module_info::out) is det.
:- pred module_info_set_has_user_event(
    module_info::in, module_info::out) is det.
:- pred module_info_set_direct_arg_proc_map(direct_arg_proc_map::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_foreign_decl_codes_user(cord(foreign_decl_code)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_foreign_decl_codes_aux(cord(foreign_decl_code)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_foreign_body_codes(cord(foreign_body_code)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_int_bad_clauses(set(pred_pf_name_arity)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_type_ctor_gen_infos(list(type_ctor_gen_info)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_must_be_stratified_preds(set(pred_id)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_unused_arg_info(unused_arg_info::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_table_struct_map(table_struct_map::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_used_modules(used_modules::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_ancestor_avail_modules(set(module_name)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_maybe_complexity_proc_map(
    maybe(pair(int, complexity_proc_map))::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_complexity_proc_infos(list(complexity_proc_info)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_proc_analysis_kinds(set(proc_analysis_kind)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_analysis_info(analysis_info::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_user_init_pred_target_names(pred_target_names::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_user_final_pred_target_names(pred_target_names::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_structure_reuse_preds(set(pred_id)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_format_call_pragma_preds(set(pred_id)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_exported_enums(list(exported_enum_info)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_event_set(event_set::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_oisu_map(oisu_map::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_oisu_procs(set(pred_proc_id)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_ts_rev_string_table(int::in, list(string)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_type_repn_dec(type_repn_decision_data::in,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utility predicates that are a bit more complicated than
% a simple getter or setter predicates.
%

:- pred module_info_get_pred_id_table(module_info::in,
    pred_id_table::out) is det.
:- pred module_info_set_pred_id_table(pred_id_table::in,
    module_info::in, module_info::out) is det.

    % Given a pred_id, return the pred_info of the specified pred.
    %
:- pred module_info_pred_info(module_info::in, pred_id::in, pred_info::out)
    is det.

    % Given a pred_proc_id, return the proc_info of the specified procedure.
    %
:- pred module_info_proc_info(module_info::in, pred_proc_id::in,
    proc_info::out) is det.
:- pred module_info_proc_info(module_info::in, pred_id::in, proc_id::in,
    proc_info::out) is det.

    % Given a pred_id and a proc_id, get the pred_info of that predicate
    % and the proc_info for that mode of that predicate.
    %
:- pred module_info_pred_proc_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::out, proc_info::out) is det.
:- pred module_info_pred_proc_info(module_info::in, pred_proc_id::in,
    pred_info::out, proc_info::out) is det.

    % Return a set of the pred_ids of all the valid predicates.
    % Predicates whose definition contains a type error, etc.
    % get removed from this set, so that later passes can rely
    % on the predicates in this set being type-correct, etc.
    %
:- pred module_info_get_valid_pred_id_set(module_info::in,
    set_tree234(pred_id)::out) is det.

    % Return the same pred_ids as module_info_get_valid_pred_id_set,
    % but in the form of a sorted list.
    %
:- pred module_info_get_valid_pred_ids(module_info::in,
    list(pred_id)::out) is det.

    % Remove one or more predicates from the set of valid pred_ids,
    % to prevent further processing of those predicates after errors
    % have been encountered in them.
    %
:- pred module_info_make_pred_id_invalid(pred_id::in,
    module_info::in, module_info::out) is det.
:- pred module_info_make_pred_ids_invalid(list(pred_id)::in,
    module_info::in, module_info::out) is det.

    % Completely remove a predicate from a module.
    %
:- pred module_info_remove_predicate(pred_id::in,
    module_info::in, module_info::out) is det.

:- pred module_info_set_pred_info(pred_id::in, pred_info::in,
    module_info::in, module_info::out) is det.

:- pred module_info_set_pred_proc_info(pred_proc_id::in,
    pred_info::in, proc_info::in, module_info::in, module_info::out) is det.
:- pred module_info_set_pred_proc_info(pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, module_info::in, module_info::out) is det.

%---------------------%

:- pred predicate_id(module_info::in, pred_id::in, module_name::out,
    string::out, arity::out) is det.

:- func predicate_module(module_info, pred_id) = module_name.
:- func predicate_name(module_info, pred_id) = string.
:- func predicate_arity(module_info, pred_id) = arity.

%---------------------%

:- pred module_add_foreign_decl_code_user(foreign_decl_code::in,
    module_info::in, module_info::out) is det.
:- pred module_add_foreign_decl_code_aux(foreign_decl_code::in,
    module_info::in, module_info::out) is det.

:- pred module_add_foreign_body_code(foreign_body_code::in,
    module_info::in, module_info::out) is det.

:- pred module_add_item_fim(item_fim::in,
    module_info::in, module_info::out) is det.

:- pred module_add_fact_table_file(string::in,
    module_info::in, module_info::out) is det.

%---------------------%

    % Please see module_info_ensure_dependency_info for the constraints
    % on the returned dependency_info.
    %
:- pred module_info_dependency_info(module_info::in,
    hlds_dependency_info::out) is det.

:- pred module_info_set_dependency_info(hlds_dependency_info::in,
    module_info::in, module_info::out) is det.

:- pred module_info_clobber_dependency_info(
    module_info::in, module_info::out) is det.

%---------------------%

    % The module_info stores a counter which is used to distinguish
    % lambda predicates which appear on the same line in the same file.
    % This predicate returns the next number for the given context
    % and increments the counter for that context.
    %
:- pred module_info_next_lambda_count(prog_context::in, int::out,
    module_info::in, module_info::out) is det.

:- pred module_info_next_atomic_count(prog_context::in, int::out,
    module_info::in, module_info::out) is det.

:- pred module_info_next_loop_inv_count(prog_context::in, int::out,
    module_info::in, module_info::out) is det.

%---------------------%

:- pred module_add_avail_module_name(module_name::in,
    module_section::in, import_or_use::in, maybe(prog_context)::in,
    module_info::in, module_info::out) is det.

:- pred module_add_indirectly_imported_module_name(module_name::in,
    module_info::in, module_info::out) is det.

    % Return the set of the visible modules. These are
    %
    % (1) the current module,
    % (2) any imported modules,
    % (3) any ancestor modules, and
    % (4) any modules imported by ancestor modules.
    %
    % It excludes transitively imported modules (those for which we read
    % `.int2' files).
    %
:- pred module_info_get_visible_modules(module_info::in,
    set(module_name)::out) is det.

    % This returns all the modules that this module's code depends on,
    % i.e. all modules that have been used or imported by this module,
    % directly or indirectly, including parent modules.
    %
:- pred module_info_get_all_deps(module_info::in,
    set(module_name)::out) is det.

:- pred module_info_add_module_to_public_used_modules(module_name::in,
    module_info::in, module_info::out) is det.

%---------------------%

:- pred module_info_user_init_pred_target_names(module_info::in,
    list(string)::out) is det.
:- pred module_info_user_final_pred_target_names(module_info::in,
    list(string)::out) is det.

:- pred module_info_user_init_pred_procs(module_info::in,
    list(pred_proc_id)::out) is det.
:- pred module_info_user_final_pred_procs(module_info::in,
    list(pred_proc_id)::out) is det.

%---------------------------------------------------------------------------%

    % The information needed by the pass that decides type representations.
    % The first three fields contain information that it uses make its
    % decisions; the fourth contains information that says what it should *do*
    % with the results of its decisions.
    %
:- type type_repn_decision_data
    --->    type_repn_decision_data(
                % The contents of type_repn items read in from the interface
                % files of other modules, containing information about the
                % representations of the types defined in those modules.
                trdd_type_repns         :: type_ctor_repn_map,

                % The contents of direct_arg clauses in type definitions
                % read in from the interface files of other modules,
                % containing information about the direct_arg part of the
                % representations of those types.
                %
                % XXX TYPE_REPN In the future, this information should *also*
                % be stored in type_repn items.
                trdd_direct_arg_map     :: direct_arg_map,

                % The contents of foreign_enum pragmas read in either
                % from interface files of other modules, or the source file
                % of the module being compiled, giving the foreign language
                % definition of a type.
                trdd_foreign_enums      :: list({item_mercury_status,
                                                item_foreign_enum_info}),

                % The contents of foreign_export_enum pragmas read in
                % from the source file of the module being compiled,
                % asking the compiler to generate definitions
                % of the named types in the named foreign languages.
                trdd_foreign_exports    :: list(item_foreign_export_enum_info)
            ).

:- type direct_arg_map == map(type_ctor, list(sym_name_arity)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.op_mode.
:- import_module mdbcomp.builtin_modules.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module require.
:- import_module string.

    % The module_info, module_sub_info and module_rare_info types
    % constitute a single logical data structure split into three parts
    % for efficiency purposes.
    %
    % The module_info type contains the most frequently accessed and updated
    % pieces of information about the module.
    %
    % We keep the other pieces of information we need about the module
    % in the module_sub_info or the module_rare_info types. Those that are
    % reasonably frequently updated are in the module_sub_info; those that
    % are rarely or never updated are in the module_rare_info.
    %
    % This arrangement minimizes the amount of memory that needs to be
    % allocated, and filled in, when a field is updated.
    %
    % Note that a field may be rarely read or written for two main reasons.
    %
    % - One reason is that the compiler simply does not need to read or write
    %   the field very often. This may be e.g. because it is used only
    %   when processing a language construct that is rare, or because
    %   its uses are concentrated in a few pieces of code that read it
    %   only when they start and write it only when they finish.
    %
    % - Another reason is that it is used only when a compiler option
    %   is given, and it is rarely given.

    % Please keep the order of the fields in module_info, module_sub_info
    % and module_rare_info in sync with the order of the both the
    % declarations and definitions of both the getter and setter predicates.

:- type module_info
    --->    module_info(
                % The Boehm collector allocates blocks whose sizes are
                % multiples of 2. Please keep the number of fields here
                % to a multiple of 2 as well.

                % Note that the no_tag_type_table contains information
                % about notag types that is also available in the type_table,
                % but in a format that allows faster access.

/* 01 */        mi_sub_info                     :: module_sub_info,
/* 02 */        mi_rare_info                    :: module_rare_info,
/* 03 */        mi_globals                      :: globals,
/* 04 */        mi_predicate_table              :: predicate_table,
/* 05 */        mi_type_table                   :: type_table,
/* 06 */        mi_no_tag_type_table            :: no_tag_type_table,
/* 07 */        mi_inst_table                   :: inst_table,
/* 08 */        mi_mode_table                   :: mode_table,
/* 09 */        mi_cons_table                   :: cons_table,
/* 10 */        mi_ctor_field_table             :: ctor_field_table
            ).

:- type module_sub_info
    --->    module_sub_info(
                msi_special_pred_maps           :: special_pred_maps,
                msi_class_table                 :: class_table,
                msi_instance_table              :: instance_table,

                % Data used for user-guided type specialization.
                msi_type_spec_info              :: type_spec_info,

                % The database of constant structures the code generator
                % will generate independently, outside all the procedures
                % of the program.
                msi_const_struct_db             :: const_struct_db,

                msi_c_j_cs_fims                 :: c_j_cs_fims,

                % List of the procs for which there is a
                % pragma foreign_export(...) declaration.
                msi_pragma_exported_procs       :: cord(pragma_exported_proc)
            ).

:- type module_rare_info
    --->    module_rare_info(
                mri_module_name                 :: module_name,
                mri_module_name_context         :: prog_context,
                mri_dump_base_file_name         :: string,

                mri_include_module_map          :: include_module_map,

                mri_partial_qualifier_info      :: partial_qualifier_info,
                mri_maybe_recompilation_info    :: maybe(recompilation_info),

                mri_proc_requests               :: proc_requests,
                mri_assertion_table             :: assertion_table,
                mri_exclusive_table             :: exclusive_table,

                mri_direct_arg_proc_map         :: direct_arg_proc_map,

                mri_has_parallel_conj           :: has_parallel_conj,
                mri_has_user_event              :: has_user_event,

                % We classify foreign code fragments bodily included in the
                % generated target language file into three categories,
                % based on two criteria.
                %
                % The first criterion is declarations (decl_codes) vs
                % non-declarations (body codes). We separate these because
                % we have to put declarations before code that may use those
                % declarations.
                %
                % We would prefer the second criterion to be declarations
                % that define types vs declarations that define other entities
                % that may refer to those types, such as global variables
                % or function, again so that we can emit the former before
                % the latter, Unfortunately, foreign_decl pragmas do not
                % specify what they define. Instead, our second criterion is
                % user-provided declarations vs aux declarations added
                % by the Mercury compiler itself to implement either
                % (a) mutables, or (b) fact tables. Neither of the latter
                % define types, so putting these after user-provided
                % declarations will work, as long as in the user-provided
                % declarations, definitions of types precede definitions
                % of other entities that refer to those types. Ensuring that
                % is the programmer's responsibility.
                mri_foreign_decl_codes_user     :: cord(foreign_decl_code),
                mri_foreign_decl_codes_aux      :: cord(foreign_decl_code),
                mri_foreign_body_codes          :: cord(foreign_body_code),

                % The names of the files containing fact tables implementing
                % predicates defined in this module.
                mri_fact_table_file_names       :: list(string),

                % The set of predicates and functions for which there was
                % an attempt to define them in the interface (by clause,
                % foreign_proc, or external_proc pragma), which means that
                % if we find no definition for them in the implementation
                % section either, we should NOT generate an error message
                % complaining about the definition being missing. Such a
                % message would be misleading, since the definition is not
                % missing, it was just misplaced, and we have already
                % generated an error message about that misplaced attempt
                % at definition.
                mri_int_bad_clauses             :: set(pred_pf_name_arity),

                % Please see module_info_ensure_dependency_info for the
                % meaning of this dependency_info, and the constraints on it.
                mri_maybe_dependency_info       :: maybe(hlds_dependency_info),

                mri_type_ctor_gen_infos         :: list(type_ctor_gen_info),
                mri_must_be_stratified_preds    :: set(pred_id),

                % Unused argument info about predicates in the current module
                % which has been exported in .opt files.
                mri_unused_arg_info             :: unused_arg_info,

                % For every procedure that requires its own tabling structure,
                % this field records the information needed to define that
                % structure.
                mri_table_struct_map            :: table_struct_map,

                % How many lambda expressions there are at different contexts
                % in the module. This is used to uniquely identify lambda
                % expressions that appear on the same line of the same file.
                mri_lambdas_per_context         :: map(prog_context, counter),

                % How many STM atomic expressions there are at different
                % contexts in the module. This is used to uniquely identify
                % STM atomic expressions that appear on the same line of
                % the same file.
                mri_atomics_per_context         :: map(prog_context, counter),

                % How many loop invariant optimizations we have done at
                % different contexts in the module. This is used to provide
                % a uniquely identify the predicates that we create
                % using the loop invariant optimization, in the rare case
                % that one line contains the definition of more than one
                % predicate.
                mri_loop_invs_per_context       :: map(prog_context, counter),

                % The add_item_avails predicate in make_hlds_passes.m fills
                % this field with information about all the import- and
                % use_module declarations both in the module being compiled,
                % and in the .int0 interface files of its ancestors.
                %
                % Each entry in the avail_module_map will specify
                %
                % - whether the module is imported or used in the interface
                %   of either the module or its ancestors, and
                %
                % - whether the module is imported (as opposed to used)
                %   in either the module or its ancestors.
                %
                % Each entry will also contain a list of avail_modules
                % *for import/use_module declarations in this module only*;
                % there won't be any entries for imports/uses in ancestors.
                %
                % This field is used by:
                %
                % - intermod.m to (over-)estimate the set of use_module decls
                %   needed by the code put into a .opt file;
                %
                % - try_expand.m to see whether exception is imported
                %   and hence whether it has anything to do at all
                %   (since we import exception.m implicitly if some code
                %   contains a try goal);
                %
                % - by unused_imports.m to decide what import_module and/or
                %   use_module declarations to warn about;
                %
                % - by xml_documentation to prettyprint a module as XML;
                %
                % and possibly more.
                mri_avail_module_map            :: avail_module_map,

                % The names of all the indirectly imported modules
                % (used by the MLDS back-end).
                %
                % XXX CLEANUP The above is misleading.
                % When added to the info in the previous field, the value
                % in this field is used to for two purposes, both of which
                % need the full set of modules imported and/or used both
                % directly and indirectly, and both explicitly or implicitly.
                % The purposes are:
                %
                % - #including those the .mh and .mih files of those modules;
                % - reading and writing the .analysis files of those modules.
                mri_indirectly_imported_module_names
                                                :: set(module_name),

                % The modules which have already been calculated as being used.
                % This slot is initialized to the set of modules that have
                % been seen to be used during the expansion of equivalence
                % types and insts.
                mri_used_modules                :: used_modules,

                % The set of modules imported by ancestor modules.
                %
                % We used to add these to mri_used_modules, but that prevented
                % the compiler from generating useful warnings about unused
                % local imports/uses of those modules. We now keep this info
                % on a "may be useful later" basis; it is current unused.
                mri_ancestor_avail_modules      :: set(module_name),

                % Information about the procedures we are performing
                % complexity experiments on.
                mri_maybe_complexity_proc_map   :: maybe(pair(int,
                                                complexity_proc_map)),
                mri_complexity_proc_infos       :: list(complexity_proc_info),

                % Records the set of analyses whose results are now available
                % in the proc_infos. As each analysis puts its results
                % in the proc_infos, it should add its id to this set.
                mri_proc_analysis_kinds         :: set(proc_analysis_kind),

                % Information for the inter-module analysis framework.
                mri_analysis_info               :: analysis_info,

                % Exported target language names for preds appearing in
                % `:- initialise' directives, and implicitly in
                % `:- mutable' directives, in this module.
                mri_user_init_pred_target_names :: pred_target_names,

                % Exported target names for preds appearing in
                % `:- finalise' directives in this module.
                mri_user_final_pred_target_names :: pred_target_names,

                % Predicates which were created as reuse versions of other
                % procedures. Its only use is to avoid writing out pragmas
                % for structure reuse predicates to `.trans_opt' files.
                mri_structure_reuse_preds       :: set(pred_id),

                % The set of predicates in the HLDS that have
                % a format_call pragma.
                %
                % This set is filled in by add_pragma.m when the HLDS
                % is first constructed. Usually, this set will be empty,
                % but if it is not, the check_pragma_format_call_preds pass
                % will check all these pragmas for errors.
                %
                % This pass must be after type and mode analysis (since it
                % needs to know the types and modes of procedure arguments),
                % and must be before the simplification pass at the end of
                % semantic analysis (since that is when format_call.m's code
                % uses this info, relying on it having being checked).
                mri_format_call_pragma_preds    :: set(pred_id),

                % Enumeration types that have been exported to a foreign
                % language.
                mri_exported_enums              :: list(exported_enum_info),

                mri_event_set                   :: event_set,

                % The set of visible declarations about order-independent
                % state update.
                mri_oisu_map                    :: oisu_map,

                % The set of procedures defined in this module that
                % have OISU arguments.
                mri_oisu_procs                  :: set(pred_proc_id),

                % A table of strings used by some threadscope events.
                % Currently threadscope events are introduced for each future
                % in dep_par_conj.m which is why we need to record the table
                % within the HLDS. The LLDS also uses threadscope string
                % tables, see global_data.m, the LLDS introduces strings during
                % the HLDS->LLDS transformation of parallel conjunctions.
                mri_ts_string_table_size        :: int,
                mri_ts_rev_string_table         :: list(string),

                % Information needed to decide type representations.
                mri_type_repn_dec               :: type_repn_decision_data
            ).

% Access stats for the module_info structure on 30 december 2014.
%
%  i      read      same      diff   same%
%  0 188233540        17  38369323   0.00%  predicate_table
%  1      7933         0       480   0.00%  proc_requests
%  2    261171         0    103230   0.00%  special_pred_maps
%  3   4576898         0         0          partial_qualifier_info
%  4  21758620      2908   1589788   0.18%  type_table
%  5  22754063         0   2360725   0.00%  inst_table
%  6 145877431     10501    149637   6.56%  mode_table
%  7  16110150      3767    803152   0.47%  cons_table
%  8   7125765       353     65777   0.53%  class_table
%  9   2543131       353    206012   0.17%  instance_table
% 10      7935         0      3798   0.00%  assertion_table
% 11         0         0         0          exclusive_table
% 12   4552293    620283    180042  77.50%  ctor_field_table
% 13   2256146   2219707       235  99.99%  maybe_recompilation_info
% 14  14893776         0         0          name
% 15         0         0         0          dump_hlds_base_file_name
% 16  39144524         0     16950   0.00%  globals
% 17      8481        17        64  20.99%  has_parallel_conj
% 18       253         4         5  44.44%  has_user_event
% 19         0    223574      3371  98.51%  contains_foreign_type
% 20      9180         0       710   0.00%  foreign_decl_codes
% 21      3683         0       856   0.00%  foreign_body_codes
% 22    966008         0    963108   0.00%  foreign_import_modules
% 23    331870    281405     35898  88.69%  pragma_exported_procs
% 24         1         0         0          fact_table_file_names
% 25     10630     11345      8490  57.20%  maybe_dependency_info
% 26     11342      4885       128  97.45%  num_errors
% 27     35292      2442      3209  43.21%  type_ctor_gen_infos
% 28      3506         0         2   0.00%  must_be_stratified_preds
% 29         1         0         1   0.00%  unused_arg_info
% 30   3972001         0   3445467   0.00%  exception_info
% 31       296         0         0          trailing_info
% 32      4071         0        75   0.00%  table_struct_map
% 33       296         0         0          mm_tabling_info
% 34      4019         0      4019   0.00%  lambdas_per_context
% 35         0         0         0          atomics_per_context
% 36      7053         0         0          imported_module_names
% 37      3135         0         0          indirectly_imported_mod_specs
% 38         1         0         0          interface_module_names
% 39      6568         0      3767   0.00%  used_modules
% 40   1656135         0    126058   0.00%  type_spec_info
% 41  22588003         0     87106   0.00%  no_tag_types
% 42    171993         0         0          complexity_proc_map
% 43      2821         0         0          complexity_proc_infos
% 44       763       336         2  99.41%  analysis_info
% 45       316         0        20   0.00%  structure_reuse_preds
% 46      5703         0        55   0.00%  exported_enums
% 47        48         0      3767   0.00%  event_set
% 48      3510         0         6   0.00%  oisu_map
% 49         0         0         2   0.00%  oisu_procs
% 50   2684695   2592456     10222  99.61%  const_struct_db
% 51      2821         0         0          threadscope_string_table

%---------------------------------------------------------------------------%

module_info_init(Globals, ModuleName, ModuleNameContext, DumpBaseFileName,
        InclMap, UsedModules, ImplicitlyUsedModules,
        QualifierInfo, MaybeRecompInfo, TypeRepnDec, ModuleInfo) :-
    SpecialPredMaps = special_pred_maps(map.init, map.init, map.init),
    map.init(ClassTable),
    map.init(InstanceTable),

    set.init(TypeSpecPreds),
    set.init(TypeSpecForcePreds),
    map.init(SpecMap),
    map.init(PragmaMap),
    TypeSpecInfo = type_spec_info(TypeSpecPreds, TypeSpecForcePreds,
        SpecMap, PragmaMap),

    const_struct_db_init(Globals, ConstStructDb),
    ForeignImportModules = init_foreign_import_modules,
    PragmaExportedProcs = cord.init,

    ModuleSubInfo = module_sub_info(
        SpecialPredMaps,
        ClassTable,
        InstanceTable,
        TypeSpecInfo,
        ConstStructDb,
        ForeignImportModules,
        PragmaExportedProcs),

    init_requests(ProcRequests),
    assertion_table_init(AssertionTable),
    exclusive_table_init(ExclusiveTable),
    map.init(DirectArgInOutMap),
    HasParallelConj = has_no_parallel_conj,
    HasUserEvent = has_no_user_event,
    ForeignDeclsUser = cord.init,
    ForeignDeclsAux = cord.init,
    ForeignBodies = cord.init,
    FactTableFiles = [],
    set.init(IntBadPreds),
    MaybeDependencyInfo = maybe.no,
    MustBeStratifiedPreds = [],
    set.init(StratPreds),
    map.init(UnusedArgInfo),
    map.init(TablingStructMap),
    map.init(LambdasPerContext),
    map.init(AtomicsPerContext),
    map.init(LoopInvsPerContext),

    % XXX ITEM_LIST Given that we start with an aug_compilation_unit,
    % shouldn't the work of finding implicit dependencies have already
    % been done?
    % XXX ITEM_LIST Should a tabled predicate declared in a .int* or .*opt
    % file generate an implicit dependency?

    % XXX ITEM_LIST We should record ImportDeps and UseDeps separately.
    % XXX ITEM_LIST Should we record implicitly and explicitly imported
    % separately, or at least record for each import (and use) whether
    % it was explicit or implicit, and one (or more) context where either
    % the explicit imported was requested, or the implicit import was required.

    map.init(AvailModuleMap0),
    add_implicit_avail_module(import_decl, mercury_public_builtin_module,
        AvailModuleMap0, AvailModuleMap1),
    set.fold(add_implicit_avail_module(use_decl), ImplicitlyUsedModules,
        AvailModuleMap1, AvailModuleMap),

    set.init(IndirectlyImportedModules),

    set.init(AncestorAvailModules),
    MaybeComplexityMap = no,
    ComplexityProcInfos = [],
    set.init(ProcAnalysisKinds),

    globals.get_op_mode(Globals, OpMode),
    ( if OpMode = opm_top_args(opma_augment(opmau_make_analysis_registry)) then
        MakeAnalysisReg = yes
    else
        MakeAnalysisReg = no
    ),
    AnalysisInfo = init_analysis_info(mmc, ModuleName, MakeAnalysisReg),

    UserInitPredTargetNames = pred_target_names(map.init),
    UserFinalPredTargetNames = pred_target_names(map.init),
    set.init(StructureReusePredIds),
    set.init(FormatCallPragmaPredIds),
    ExportedEnums = [],
    EventSet = event_set("", map.init),
    map.init(OISUMap),
    set.init(OISUProcs),
    TSStringTableSize = 0,
    TSRevStringTable = [],

    ModuleRareInfo = module_rare_info(
        ModuleName,
        ModuleNameContext,
        DumpBaseFileName,
        InclMap,
        QualifierInfo,
        MaybeRecompInfo,
        ProcRequests,
        AssertionTable,
        ExclusiveTable,
        DirectArgInOutMap,
        HasParallelConj,
        HasUserEvent,
        ForeignDeclsUser,
        ForeignDeclsAux,
        ForeignBodies,
        FactTableFiles,
        IntBadPreds,
        MaybeDependencyInfo,
        MustBeStratifiedPreds,
        StratPreds,
        UnusedArgInfo,
        TablingStructMap,
        LambdasPerContext,
        AtomicsPerContext,
        LoopInvsPerContext,
        AvailModuleMap,
        IndirectlyImportedModules,
        UsedModules,
        AncestorAvailModules,
        MaybeComplexityMap,
        ComplexityProcInfos,
        ProcAnalysisKinds,
        AnalysisInfo,
        UserInitPredTargetNames,
        UserFinalPredTargetNames,
        StructureReusePredIds,
        FormatCallPragmaPredIds,
        ExportedEnums,
        EventSet,
        OISUMap,
        OISUProcs,
        TSStringTableSize,
        TSRevStringTable,
        TypeRepnDec),

    predicate_table_init(PredicateTable),
    TypeTable = init_type_table,
    map.init(NoTagTypes),
    inst_table_init(InstTable),
    mode_table_init(ModeTable),
    CtorTable = init_cons_table,
    map.init(FieldNameTable),

    ModuleInfo = module_info(
        ModuleSubInfo,
        ModuleRareInfo,
        Globals,
        PredicateTable,
        TypeTable,
        NoTagTypes,
        InstTable,
        ModeTable,
        CtorTable,
        FieldNameTable).

:- pred add_implicit_avail_module(import_or_use::in, module_name::in,
    avail_module_map::in, avail_module_map::out) is det.

add_implicit_avail_module(ImportOrUse, ModuleName, !AvailModuleMap) :-
    Entry = avail_module_entry(ms_implementation, ImportOrUse, []),
    map.det_insert(ModuleName, Entry, !AvailModuleMap).

module_info_optimize(!ModuleInfo) :-
    % Currently, all the calls to *_table_optimize are no-ops.
    % We keep them, and this predicate, in case that changes in the future.

    module_info_get_predicate_table(!.ModuleInfo, Preds0),
    predicate_table_optimize(Preds0, Preds),
    module_info_set_predicate_table(Preds, !ModuleInfo),

    module_info_get_inst_table(!.ModuleInfo, InstTable0),
    inst_table_get_user_insts(InstTable0, UserInstTable0),
    map.optimize(UserInstTable0, UserInstTable),
    inst_table_set_user_insts(UserInstTable, InstTable0, InstTable),
    module_info_set_inst_table(InstTable, !ModuleInfo),

    module_info_get_mode_table(!.ModuleInfo, Modes0),
    mode_table_optimize(Modes0, Modes),
    module_info_set_mode_table(Modes, !ModuleInfo),

    module_info_get_cons_table(!.ModuleInfo, Ctors0),
    cons_table_optimize(Ctors0, Ctors),
    module_info_set_cons_table(Ctors, !ModuleInfo).

%---------------------------------------------------------------------------%
%
% Getter and setter predicates on the module_info that are local to this
% module.
%

:- pred module_info_get_lambdas_per_context(module_info::in,
    map(prog_context, counter)::out) is det.
:- pred module_info_get_atomics_per_context(module_info::in,
    map(prog_context, counter)::out) is det.
:- pred module_info_get_loop_invs_per_context(module_info::in,
    map(prog_context, counter)::out) is det.
:- pred module_info_get_indirectly_imported_module_names(module_info::in,
    set(module_name)::out) is det.

:- pred module_info_set_maybe_dependency_info(maybe(hlds_dependency_info)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_lambdas_per_context(map(prog_context, counter)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_atomics_per_context(map(prog_context, counter)::in,
    module_info::in, module_info::out) is det.
:- pred module_info_set_loop_invs_per_context(map(prog_context, counter)::in,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%
% Getter predicates for the module_info.
%

module_info_get_globals(MI, X) :-
    X = MI ^ mi_globals.
module_info_get_predicate_table(MI, X) :-
    X = MI ^ mi_predicate_table.
module_info_get_type_table(MI, X) :-
    X = MI ^ mi_type_table.
module_info_get_no_tag_types(MI, X) :-
    X = MI ^ mi_no_tag_type_table.
module_info_get_inst_table(MI, X) :-
    X = MI ^ mi_inst_table.
module_info_get_mode_table(MI, X) :-
    X = MI ^ mi_mode_table.
module_info_get_cons_table(MI, X) :-
    X = MI ^ mi_cons_table.
module_info_get_ctor_field_table(MI, X) :-
    X = MI ^ mi_ctor_field_table.

module_info_get_special_pred_maps(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_special_pred_maps.
module_info_get_class_table(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_class_table.
module_info_get_instance_table(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_instance_table.
module_info_get_type_spec_info(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_type_spec_info.
module_info_get_const_struct_db(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_const_struct_db.
module_info_get_c_j_cs_fims(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_c_j_cs_fims.
module_info_get_pragma_exported_procs(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_pragma_exported_procs.

module_info_get_name(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_module_name.
module_info_get_name_context(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_module_name_context.
module_info_get_dump_hlds_base_file_name(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_dump_base_file_name.
module_info_get_include_module_map(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_include_module_map.
module_info_get_partial_qualifier_info(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_partial_qualifier_info.
module_info_get_maybe_recompilation_info(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_maybe_recompilation_info.
module_info_get_proc_requests(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_proc_requests.
module_info_get_assertion_table(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_assertion_table.
module_info_get_exclusive_table(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_exclusive_table.
module_info_get_has_parallel_conj(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_has_parallel_conj.
module_info_get_has_user_event(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_has_user_event.
module_info_get_direct_arg_proc_map(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_direct_arg_proc_map.
module_info_get_foreign_decl_codes_user(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_foreign_decl_codes_user.
module_info_get_foreign_decl_codes_aux(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_foreign_decl_codes_aux.
module_info_get_foreign_body_codes(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_foreign_body_codes.
module_info_get_fact_table_file_names(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_fact_table_file_names.
module_info_get_int_bad_clauses(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_int_bad_clauses.
module_info_get_maybe_dependency_info(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_maybe_dependency_info.
module_info_get_type_ctor_gen_infos(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_type_ctor_gen_infos.
module_info_get_must_be_stratified_preds(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_must_be_stratified_preds.
module_info_get_unused_arg_info(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_unused_arg_info.
module_info_get_table_struct_map(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_table_struct_map.
module_info_get_lambdas_per_context(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_lambdas_per_context.
module_info_get_atomics_per_context(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_atomics_per_context.
module_info_get_loop_invs_per_context(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_loop_invs_per_context.
module_info_get_avail_module_map(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_avail_module_map.
module_info_get_indirectly_imported_module_names(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_indirectly_imported_module_names.
module_info_get_used_modules(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_used_modules.
module_info_get_ancestor_avail_modules(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_ancestor_avail_modules.
module_info_get_maybe_complexity_proc_map(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_maybe_complexity_proc_map.
module_info_get_complexity_proc_infos(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_complexity_proc_infos.
module_info_get_proc_analysis_kinds(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_proc_analysis_kinds.
module_info_get_analysis_info(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_analysis_info.
module_info_get_user_init_pred_target_names(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_user_init_pred_target_names.
module_info_get_user_final_pred_target_names(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_user_final_pred_target_names.
module_info_get_structure_reuse_preds(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_structure_reuse_preds.
module_info_get_format_call_pragma_preds(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_format_call_pragma_preds.
module_info_get_exported_enums(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_exported_enums.
module_info_get_event_set(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_event_set.
module_info_get_oisu_map(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_oisu_map.
module_info_get_oisu_procs(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_oisu_procs.
module_info_get_ts_rev_string_table(MI, X, Y) :-
    X = MI ^ mi_rare_info ^ mri_ts_string_table_size,
    Y = MI ^ mi_rare_info ^ mri_ts_rev_string_table.
module_info_get_type_repn_dec(MI, X) :-
    X = MI ^ mi_rare_info ^ mri_type_repn_dec.

%---------------------------------------------------------------------------%
%
% Setter predicates for the module_info.
%

module_info_set_globals(X, !MI) :-
    !MI ^ mi_globals := X.
module_info_set_predicate_table(X, !MI) :-
    !MI ^ mi_predicate_table := X.
module_info_set_type_table(X, !MI) :-
    !MI ^ mi_type_table := X.
module_info_set_no_tag_types(X, !MI) :-
    !MI ^ mi_no_tag_type_table := X.
module_info_set_inst_table(X, !MI) :-
    !MI ^ mi_inst_table := X.
module_info_set_mode_table(X, !MI) :-
    !MI ^ mi_mode_table := X.
module_info_set_cons_table(X, !MI) :-
    !MI ^ mi_cons_table := X.
module_info_set_ctor_field_table(X, !MI) :-
    !MI ^ mi_ctor_field_table := X.

module_info_set_special_pred_maps(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_special_pred_maps := X.
module_info_set_class_table(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_class_table := X.
module_info_set_instance_table(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_instance_table := X.
module_info_set_type_spec_info(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_type_spec_info := X.
module_info_set_const_struct_db(X, !MI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.MI ^ mi_sub_info ^ msi_const_struct_db)
    then
        true
    else
        !MI ^ mi_sub_info ^ msi_const_struct_db := X
    ).
module_info_set_c_j_cs_fims(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_c_j_cs_fims := X.
module_info_set_pragma_exported_procs(X, !MI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.MI ^ mi_sub_info ^ msi_pragma_exported_procs)
    then
        true
    else
        !MI ^ mi_sub_info ^ msi_pragma_exported_procs := X
    ).

module_info_set_maybe_recompilation_info(X, !MI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.MI ^ mi_rare_info ^ mri_maybe_recompilation_info)
    then
        true
    else
        !MI ^ mi_rare_info ^ mri_maybe_recompilation_info := X
    ).
module_info_set_proc_requests(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_proc_requests := X.
module_info_set_assertion_table(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_assertion_table := X.
module_info_set_exclusive_table(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_exclusive_table := X.
module_info_set_has_parallel_conj(!MI) :-
    X = has_parallel_conj,
    !MI ^ mi_rare_info ^ mri_has_parallel_conj := X.
module_info_set_has_user_event(!MI) :-
    X = has_user_event,
    !MI ^ mi_rare_info ^ mri_has_user_event := X.
module_info_set_direct_arg_proc_map(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_direct_arg_proc_map := X.
module_info_set_foreign_decl_codes_user(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_foreign_decl_codes_user := X.
module_info_set_foreign_decl_codes_aux(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_foreign_decl_codes_aux := X.
module_info_set_foreign_body_codes(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_foreign_body_codes := X.
module_info_set_int_bad_clauses(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_int_bad_clauses := X.
module_info_set_maybe_dependency_info(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_maybe_dependency_info := X.
module_info_set_type_ctor_gen_infos(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_type_ctor_gen_infos := X.
module_info_set_must_be_stratified_preds(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_must_be_stratified_preds := X.
module_info_set_unused_arg_info(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_unused_arg_info := X.
module_info_set_table_struct_map(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_table_struct_map := X.
module_info_set_lambdas_per_context(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_lambdas_per_context := X.
module_info_set_atomics_per_context(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_atomics_per_context := X.
module_info_set_loop_invs_per_context(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_loop_invs_per_context := X.
module_info_set_used_modules(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_used_modules := X.
module_info_set_ancestor_avail_modules(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_ancestor_avail_modules := X.
module_info_set_maybe_complexity_proc_map(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_maybe_complexity_proc_map := X.
module_info_set_complexity_proc_infos(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_complexity_proc_infos := X.
module_info_set_proc_analysis_kinds(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_proc_analysis_kinds := X.
module_info_set_analysis_info(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_analysis_info := X.
module_info_set_user_init_pred_target_names(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_user_init_pred_target_names := X.
module_info_set_user_final_pred_target_names(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_user_final_pred_target_names := X.
module_info_set_structure_reuse_preds(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_structure_reuse_preds := X.
module_info_set_format_call_pragma_preds(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_format_call_pragma_preds := X.
module_info_set_exported_enums(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_exported_enums := X.
module_info_set_event_set(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_event_set := X.
module_info_set_oisu_map(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_oisu_map := X.
module_info_set_oisu_procs(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_oisu_procs := X.
module_info_set_ts_rev_string_table(X, Y, !MI) :-
    !MI ^ mi_rare_info ^ mri_ts_string_table_size := X,
    !MI ^ mi_rare_info ^ mri_ts_rev_string_table := Y.
module_info_set_type_repn_dec(X, !MI) :-
    !MI ^ mi_rare_info ^ mri_type_repn_dec := X.

%---------------------------------------------------------------------------%
%
% Utility predicates that are a bit more complicated than
% a simple getter or setter predicates.
%

module_info_get_pred_id_table(MI, PredIdTable) :-
    module_info_get_predicate_table(MI, PredTable),
    predicate_table_get_pred_id_table(PredTable, PredIdTable).

module_info_set_pred_id_table(PredIdTable, !MI) :-
    module_info_get_predicate_table(!.MI, PredTable0),
    predicate_table_set_pred_id_table(PredIdTable, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !MI).

module_info_pred_info(MI, PredId, PredInfo) :-
    module_info_get_pred_id_table(MI, PredIdTable),
    ( if map.search(PredIdTable, PredId, PredInfoPrime) then
        PredInfo = PredInfoPrime
    else
        pred_id_to_int(PredId, PredInt),
        string.int_to_string(PredInt, PredStr),
        unexpected($pred, "cannot find predicate number " ++ PredStr)
    ).

module_info_proc_info(MI, PPId, ProcInfo) :-
    module_info_pred_proc_info(MI, PPId, _, ProcInfo).

module_info_proc_info(MI, PredId, ProcId, ProcInfo) :-
    module_info_pred_proc_info(MI, PredId, ProcId, _, ProcInfo).

module_info_pred_proc_info(MI, PredId, ProcId, PredInfo, ProcInfo) :-
    module_info_pred_info(MI, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo).

module_info_pred_proc_info(MI, proc(PredId, ProcId), PredInfo, ProcInfo) :-
    module_info_pred_proc_info(MI, PredId, ProcId, PredInfo, ProcInfo).

module_info_get_valid_pred_id_set(MI, PredIdSet) :-
    module_info_get_predicate_table(MI, PredTable),
    predicate_table_get_valid_pred_id_set(PredTable, PredIdSet).

module_info_get_valid_pred_ids(MI, PredIds) :-
    module_info_get_predicate_table(MI, PredTable),
    predicate_table_get_valid_pred_id_set(PredTable, PredIdSet),
    set_tree234.to_sorted_list(PredIdSet, PredIds).

module_info_make_pred_id_invalid(PredId, !MI) :-
    module_info_get_predicate_table(!.MI, PredTable0),
    predicate_table_make_pred_id_invalid(PredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !MI).

module_info_make_pred_ids_invalid(PredIds, !MI) :-
    module_info_get_predicate_table(!.MI, PredTable0),
    predicate_table_make_pred_ids_invalid(PredIds, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !MI).

module_info_remove_predicate(PredId, !MI) :-
    module_info_get_predicate_table(!.MI, PredTable0),
    predicate_table_remove_predicate(PredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !MI).

module_info_set_pred_info(PredId, PredInfo, !MI) :-
    module_info_get_pred_id_table(!.MI, PredIdTable0),
    % XXX Should be map.det_update.
    map.set(PredId, PredInfo, PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !MI).

module_info_set_pred_proc_info(proc(PredId, ProcId), PredInfo, ProcInfo,
        !MI) :-
    module_info_set_pred_proc_info(PredId, ProcId,
        PredInfo, ProcInfo, !MI).

module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo, !MI) :-
    % XXX Should be pred_info_set_proc_info, which calls map.det_update.
    pred_info_get_proc_table(PredInfo0, Procs0),
    map.set(ProcId, ProcInfo, Procs0, Procs),
    pred_info_set_proc_table(Procs, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !MI).

%---------------------%

predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo).

predicate_module(ModuleInfo, PredId) = ModuleName :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo).

predicate_name(ModuleInfo, PredId) = PredName :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredName = pred_info_name(PredInfo).

predicate_arity(ModuleInfo, PredId) = Arity :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    Arity = pred_info_orig_arity(PredInfo).

%---------------------%

module_add_foreign_decl_code_user(ForeignDeclCode, !Module) :-
    module_info_get_foreign_decl_codes_user(!.Module, ForeignDeclCodes0),
    ForeignDeclCodes = cord.snoc(ForeignDeclCodes0, ForeignDeclCode),
    module_info_set_foreign_decl_codes_user(ForeignDeclCodes, !Module).

module_add_foreign_decl_code_aux(ForeignDeclCode, !Module) :-
    module_info_get_foreign_decl_codes_aux(!.Module, ForeignDeclCodes0),
    ForeignDeclCodes = cord.snoc(ForeignDeclCodes0, ForeignDeclCode),
    module_info_set_foreign_decl_codes_aux(ForeignDeclCodes, !Module).

module_add_foreign_body_code(ForeignBodyCode, !Module) :-
    module_info_get_foreign_body_codes(!.Module, ForeignBodyCodes0),
    ForeignBodyCodes = cord.snoc(ForeignBodyCodes0, ForeignBodyCode),
    module_info_set_foreign_body_codes(ForeignBodyCodes, !Module).

module_add_item_fim(ItemFIM, !Module) :-
    module_info_get_c_j_cs_fims(!.Module, FIMs0),
    ItemFIM = item_fim(Lang, ModuleName, _Context, _SeqNum),
    add_fim(Lang, ModuleName, FIMs0, FIMs),
    module_info_set_c_j_cs_fims(FIMs, !Module).

module_add_fact_table_file(FileName, !Module) :-
    FileNames0 = !.Module ^ mi_rare_info ^ mri_fact_table_file_names,
    FileNames = [FileName | FileNames0],
    !Module ^ mi_rare_info ^ mri_fact_table_file_names := FileNames.

%---------------------%

module_info_dependency_info(MI, DepInfo) :-
    module_info_get_maybe_dependency_info(MI, MaybeDepInfo),
    (
        MaybeDepInfo = yes(DepInfoPrime),
        DepInfo = DepInfoPrime
    ;
        MaybeDepInfo = no,
        unexpected($pred, "attempted to access invalid dependency_info")
    ).

module_info_set_dependency_info(DependencyInfo, !MI) :-
    module_info_set_maybe_dependency_info(yes(DependencyInfo), !MI).

module_info_clobber_dependency_info(!MI) :-
    module_info_set_maybe_dependency_info(no, !MI).

%---------------------%

module_info_next_lambda_count(Context, Count, !MI) :-
    module_info_get_lambdas_per_context(!.MI, ContextCounter0),
    ( if
        map.insert(Context, counter.init(2),
            ContextCounter0, FoundContextCounter)
    then
        Count = 1,
        ContextCounter = FoundContextCounter
    else
        map.lookup(ContextCounter0, Context, Counter0),
        counter.allocate(Count, Counter0, Counter),
        map.det_update(Context, Counter, ContextCounter0, ContextCounter)
    ),
    module_info_set_lambdas_per_context(ContextCounter, !MI).

module_info_next_atomic_count(Context, Count, !MI) :-
    module_info_get_atomics_per_context(!.MI, ContextCounter0),
    ( if
        map.insert(Context, counter.init(2),
            ContextCounter0, FoundContextCounter)
    then
        Count = 1,
        ContextCounter = FoundContextCounter
    else
        map.lookup(ContextCounter0, Context, Counter0),
        counter.allocate(Count, Counter0, Counter),
        map.det_update(Context, Counter, ContextCounter0, ContextCounter)
    ),
    module_info_set_atomics_per_context(ContextCounter, !MI).

module_info_next_loop_inv_count(Context, Count, !MI) :-
    module_info_get_loop_invs_per_context(!.MI, ContextCounter0),
    ( if
        map.insert(Context, counter.init(2),
            ContextCounter0, FoundContextCounter)
    then
        Count = 1,
        ContextCounter = FoundContextCounter
    else
        map.lookup(ContextCounter0, Context, Counter0),
        counter.allocate(Count, Counter0, Counter),
        map.det_update(Context, Counter, ContextCounter0, ContextCounter)
    ),
    module_info_set_loop_invs_per_context(ContextCounter, !MI).

%---------------------%

module_add_avail_module_name(ModuleName, NewSection, NewImportOrUse,
        MaybeContext, !MI) :-
    (
        MaybeContext = no,
        NewAvails = []
    ;
        MaybeContext = yes(Context),
        NewAvails = [avail_module(NewSection, NewImportOrUse, Context)]
    ),
    AvailMap0 = !.MI ^ mi_rare_info ^ mri_avail_module_map,
    ( if map.search(AvailMap0, ModuleName, OldEntry) then
        OldEntry = avail_module_entry(OldSection, OldImportOrUse, OldAvails),
        % XXX: If one of the entries (new or old) is a use_module in the
        % interface section, while the other is an import_module in the
        % implementation section, the result *ought* to be something like
        % the int_use_imp_import alternative of the section_import_or_use type,
        % BUT the design of avail_module_entry has no way to express that.
        % The code of combine_old_new_avail_attrs returns "import_module in the
        % interface section" in such cases, which is almost certainly a bug.
        combine_old_new_avail_attrs(OldSection, NewSection,
            OldImportOrUse, NewImportOrUse, Section, ImportOrUse),
        Avails = NewAvails ++ OldAvails,
        NewEntry = avail_module_entry(Section, ImportOrUse, Avails),
        map.det_update(ModuleName, NewEntry, AvailMap0, AvailMap)
    else
        NewEntry = avail_module_entry(NewSection, NewImportOrUse, NewAvails),
        map.det_insert(ModuleName, NewEntry, AvailMap0, AvailMap)
    ),
    !MI ^ mi_rare_info ^ mri_avail_module_map := AvailMap.

:- pred combine_old_new_avail_attrs(module_section::in, module_section::in,
    import_or_use::in, import_or_use::in,
    module_section::out, import_or_use::out) is det.

combine_old_new_avail_attrs(OldSection, NewSection,
        OldImportOrUse, NewImportOrUse, Section, ImportOrUse) :-
    ( if
        ( OldSection = ms_interface
        ; NewSection = ms_interface
        )
    then
        Section = ms_interface
    else
        Section = ms_implementation
    ),
    ( if
        ( OldImportOrUse = import_decl
        ; NewImportOrUse = import_decl
        )
    then
        ImportOrUse = import_decl
    else
        ImportOrUse = use_decl
    ).

module_add_indirectly_imported_module_name(AddedModuleSpecifier, !MI) :-
    Modules0 = !.MI ^ mi_rare_info ^ mri_indirectly_imported_module_names,
    set.insert(AddedModuleSpecifier, Modules0, Modules),
    !MI ^ mi_rare_info ^ mri_indirectly_imported_module_names := Modules.

module_info_get_visible_modules(ModuleInfo, !:VisibleModules) :-
    module_info_get_name(ModuleInfo, ThisModule),
    module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),

    map.keys(AvailModuleMap, AvailModules),
    set.list_to_set(AvailModules, !:VisibleModules),
    set.insert(ThisModule, !VisibleModules),
    set.insert_list(get_ancestors(ThisModule), !VisibleModules).

module_info_get_all_deps(ModuleInfo, AllImports) :-
    module_info_get_name(ModuleInfo, ModuleName),
    Parents = get_ancestors(ModuleName),
    module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),
    map.keys(AvailModuleMap, DirectImports),
    module_info_get_indirectly_imported_module_names(ModuleInfo,
        IndirectImports),
    AllImports = set.union_list([IndirectImports,
        set.list_to_set(DirectImports), set.list_to_set(Parents)]).

module_info_add_module_to_public_used_modules(ModuleName, !MI) :-
    module_info_get_used_modules(!.MI, UsedModules0),
    record_module_and_ancestors_as_used(visibility_public, ModuleName,
        UsedModules0, UsedModules),
    module_info_set_used_modules(UsedModules, !MI).

%---------------------%

module_info_user_init_pred_target_names(MI, CNames) :-
    module_info_get_user_init_pred_target_names(MI, InitPredTargetNames),
    InitPredTargetNames = pred_target_names(SeqNumToPredTargetNamesMap),
    map.values(SeqNumToPredTargetNamesMap, PredTargetNamesLists),
    list.condense(PredTargetNamesLists, PredTargetNames),
    CNames = assoc_list.values(PredTargetNames).

module_info_user_final_pred_target_names(MI, CNames) :-
    module_info_get_user_final_pred_target_names(MI, FinalPredTargetNames),
    FinalPredTargetNames = pred_target_names(SeqNumToPredTargetNamesMap),
    map.values(SeqNumToPredTargetNamesMap, PredTargetNamesLists),
    list.condense(PredTargetNamesLists, PredTargetNames),
    CNames = assoc_list.values(PredTargetNames).

module_info_user_init_pred_procs(MI, PredProcIds) :-
    module_info_get_user_init_pred_target_names(MI, InitPredTargetNames),
    InitPredTargetNames = pred_target_names(SeqNumToPredTargetNamesMap),
    map.values(SeqNumToPredTargetNamesMap, PredTargetNamesLists),
    list.condense(PredTargetNamesLists, PredTargetNames),
    SymNamesArities = assoc_list.keys(PredTargetNames),
    list.map(get_unique_pred_proc_id_for_pred_sym_name_arity(MI),
        SymNamesArities, PredProcIds).

module_info_user_final_pred_procs(MI, PredProcIds) :-
    module_info_get_user_final_pred_target_names(MI, FinalPredTargetNames),
    FinalPredTargetNames = pred_target_names(SeqNumToPredTargetNamesMap),
    map.values(SeqNumToPredTargetNamesMap, PredTargetNamesLists),
    list.condense(PredTargetNamesLists, PredTargetNames),
    SymNamesArities = assoc_list.keys(PredTargetNames),
    list.map(get_unique_pred_proc_id_for_pred_sym_name_arity(MI),
        SymNamesArities, PredProcIds).

:- pred get_unique_pred_proc_id_for_pred_sym_name_arity(module_info::in,
    sym_name_arity::in, pred_proc_id::out) is det.

get_unique_pred_proc_id_for_pred_sym_name_arity(MI, SNA, PredProcId) :-
    module_info_get_predicate_table(MI, PredTable),
    SNA = sym_name_arity(SymName, Arity),
    UserArity = user_arity(Arity),
    predicate_table_lookup_pred_sym_arity(PredTable,
        may_be_partially_qualified, SymName, UserArity, PredIds),
    ( if PredIds = [PredId] then
        pred_table.get_single_proc_id(MI, PredId, ProcId),
        PredProcId = proc(PredId, ProcId)
    else
        unexpected($pred, "lookup failed")
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_module.
%---------------------------------------------------------------------------%
