%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_pred.m.
% Main authors: fjh, conway.
%
% This module defines the part of the HLDS that deals with predicates
% and procedures.
%
%---------------------------------------------------------------------------%

:- module hlds.proc_info_types.
:- interface.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_data_rare.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type is_address_taken
    --->    address_is_taken
    ;       address_is_not_taken.

:- type deep_recursion_info
    --->    deep_recursion_info(
                dri_role                :: deep_profile_role,

                % If the procedure is not tail recursive, this list is empty.
                % Otherwise, it contains outer-inner pairs of procedures
                % in the visible SCC, including this procedure and its copy.
                dri_visible_scc         :: list(visible_scc_data)
            ).

:- type deep_profile_role
    --->    deep_prof_inner_proc(
                dpip_outer_proc         :: pred_proc_id
            )
    ;       deep_prof_outer_proc(
                dpop_inner_proc         :: pred_proc_id
            ).

:- type visible_scc_data
    --->    visible_scc_data(
                vis_outer_proc          :: pred_proc_id,
                vis_inner_proc          :: pred_proc_id,

                % A list of all the call site numbers that correspond to
                % tail calls. (Call sites are numbered depth-first,
                % left-to-right, starting from zero.)
                rec_call_sites          :: list(int)
            ).

:- type call_site_static_data           % defines MR_CallSiteStatic
    --->    normal_call(
                normal_callee           :: rtti_proc_label,
                normal_type_subst       :: string,
                normal_file_name        :: string,
                normal_line_number      :: int,
                normal_goal_path        :: forward_goal_path
            )
    ;       special_call(
                special_file_name       :: string,
                special_line_number     :: int,
                special_goal_path       :: forward_goal_path
            )
    ;       higher_order_call(
                higher_order_file_name  :: string,
                ho_line_number          :: int,
                ho_goal_path            :: forward_goal_path
            )
    ;       method_call(
                method_file_name        :: string,
                method_line_number      :: int,
                method_goal_path        :: forward_goal_path
            )
    ;       callback(
                callback_file_name      :: string,
                callback_line_number    :: int,
                callback_goal_path      :: forward_goal_path
            ).

:- type hlds_proc_static
    --->    hlds_proc_static(           % defines part of MR_ProcStatic
                proc_static_file_name   :: string,
                proc_static_line_number :: int,
                proc_is_in_interface    :: bool,
                call_site_statics       :: list(call_site_static_data),
                coverage_points         :: list(coverage_point_info)
            ).

    % The hlds_deep_excp_vars gives the variables that hold the values returned
    % by the call port code, which are needed to let exception.throw perform
    % the work we need to do at the excp port.
:- type hlds_deep_excp_vars
    --->    hlds_deep_excp_vars(
                top_csd                 :: prog_var,
                middle_csd              :: prog_var,
                old_outermost           :: maybe(prog_var)
                                        % Needed only with the save/restore
                                        % approach, not the activation counting
                                        % approach.
            ).

:- type hlds_deep_layout
    --->    hlds_deep_layout(
                deep_layout_static      :: hlds_proc_static,
                deep_layout_excp        :: hlds_deep_excp_vars
            ).

:- type deep_profile_proc_info
    --->    deep_profile_proc_info(
                % This field is set during the first part of the deep profiling
                % transformation; tail recursion, if that is enabled.
                deep_rec                :: maybe(deep_recursion_info),

                % This field is set during the second part; it will be bound
                % to `no' before and during the first part, and to `yes'
                % after the second. The contents of this field govern
                % what will go into MR_ProcStatic structures.
                deep_layout             :: maybe(hlds_deep_layout),

                % This field stores the original body of a procedure,
                % before either part of the deep profiling transformation
                % was executed. For inner procedures created by the tail
                % recursion part of the deep profiling transformation,
                % it holds the original body of the outer procedure.
                deep_orig_body          :: deep_original_body
            ).

:- type deep_original_body
    --->    deep_original_body(
                dob_body                :: hlds_goal,
                dob_head_vars           :: list(prog_var),
                dob_instmap             :: instmap,
                dob_var_table           :: var_table,
                dob_detism              :: determinism
            ).

:- type table_arg_infos
    --->    table_arg_infos(
                list(table_arg_info),
                map(tvar, table_locn)
            ).

:- type table_arg_info
    --->    table_arg_info(
                orig_var_num            :: int,
                orig_var_name           :: string,
                slot_num                :: int,
                arg_type                :: mer_type
            ).

    % This type is analogous to layout_locn in llds.m, but it refers
    % not to lvals, but to slots in the extended answer blocks used by
    % I/O action tabling for declarative debugging.
:- type table_locn
    --->    table_locn_direct(int)
    ;       table_locn_indirect(int, int).

    % This type differs from the type table_step_kind in table_statistics.m
    % in the library in that
    % (a) in gives more information about the type of the corresponding
    % argument (if this info is needed and available),
    % (b) it doesn't have to be an enum, and
    % (c) it doesn't have to handle dummy steps.
    %
:- type table_trie_step
    --->    table_trie_step_dummy
    ;       table_trie_step_int(int_type)
    ;       table_trie_step_char
    ;       table_trie_step_string
    ;       table_trie_step_float
    ;       table_trie_step_enum(
                % The int gives the maximum enum value in the enum type + 1,
                % and thus the size of the corresponding trie node.
                % If the enum type is not a subtype, then the value is equal to
                % the number of alternatives in the type.
                int
            )
    ;       table_trie_step_foreign_enum
    ;       table_trie_step_general(
                mer_type,
                table_is_poly,
                table_value_or_addr
            )
    ;       table_trie_step_typeinfo
    ;       table_trie_step_typeclassinfo
    ;       table_trie_step_promise_implied.

:- type table_is_poly
    --->    table_is_mono       % The table type is monomorphic.
    ;       table_is_poly.      % The table type is polymorphic.

:- type table_value_or_addr
    --->    table_value         % We are tabling the value itself.
    ;       table_addr.         % We are tabling only the address.

    % Return a description of what kind of statistics we collect for a trie
    % step of a given kind. The description is the name of a value in the C
    % enum type MR_TableStepStatsKind. (We will need to generalize this
    % when we implement tabling for non-C backends.)
    %
:- func table_step_stats_kind(table_trie_step) = string.

:- type proc_table_io_info
    --->    proc_table_io_info(
                % The information we need to display an I/O action to the user.
                %
                % The table_arg_infos correspond one to one to the elements
                % of the block saved for an I/O action. The first element
                % will be the pointer to the proc_layout of the action's
                % procedure.
                %
                % The right tvarset for interpreting the types in the
                % table_arg_infos is the one in the proc_info in which
                % the proc_table_io_info is stored.

                maybe(table_arg_infos)
            ).

:- type table_step_desc
    --->    table_step_desc(
                tsd_var_name                :: string,
                tsd_step                    :: table_trie_step
            ).

:- type proc_table_struct_info
    --->    proc_table_struct_info(
                % The information we need to create the data structures
                % created by tabling for a procedure, and to interpret them
                % for the debugger (except the information -such as
                % determinism- that is already available from proc_layout
                % structures.
                %
                % The table_arg_infos list first all the input arguments,
                % then all the output arguments.
                %
                % The right tvarset for interpreting the types in the
                % table_arg_infos is the one stored below. It is taken from
                % the proc_info of the procedure whose table this structure
                % describes. Since we care only about the shapes of the types,
                % we don't care about neither the actual numerical values
                % nor the names of the type variables, so we don't care if
                % the tvarset in that proc_info changes after table_gen.m
                % takes the snapshot stored here.
                %
                % We record the rtti_proc_label of the procedure whose table
                % this is. We can't record its identity in the form of a
                % pred_proc_id, since that won't work if the procedure is
                % deleted before the code generation phase.

                ptsi_proc_label             :: rtti_proc_label,
                ptsi_tvarset                :: tvarset,
                ptsi_context                :: prog_context,
                ptsi_num_inputs             :: int,
                ptsi_num_outputs            :: int,
                ptsi_input_steps            :: list(table_step_desc),
                ptsi_maybe_output_steps     :: maybe(list(table_step_desc)),
                ptsi_gen_arg_infos          :: table_arg_infos,
                ptsi_eval_method            :: tabled_eval_method
            ).

:- type special_proc_return
    --->    generator_return(
                % The generator is stored in this location. We can't use an
                % rval to represent the location, since we don't want this
                % module to depend on the ll_backend package.
                generator_rval          :: string,

                % What should we pass as the value of the debug parameter
                % in the call to MR_tbl_mmos_return_answer?
                return_debug            :: string
            ).

:- type structure_sharing_domain_and_status
    --->    structure_sharing_domain_and_status(
                structure_sharing_domain,
                analysis_status
            ).

:- type structure_reuse_domain_and_status
    --->    structure_reuse_domain_and_status(
                structure_reuse_domain,
                analysis_status
            ).

:- type untuple_proc_info
    --->    untuple_proc_info(
                map(prog_var, list(prog_var))
            ).

:- type detism_decl
    --->    detism_decl_explicit
    ;       detism_decl_implicit
    ;       detism_decl_none.
            % The determinism of the procedure is not declared.

:- type can_process
    --->    cannot_process_yet
    ;       can_process_now.

:- type needs_maxfr_slot
    --->    needs_maxfr_slot
    ;       does_not_need_maxfr_slot.

:- type has_parallel_conj
    --->    has_parallel_conj
    ;       has_no_parallel_conj.

:- type has_user_event
    --->    has_user_event
    ;       has_no_user_event.

:- type has_self_tail_rec_call
    --->    has_self_tail_rec_call
    ;       has_no_self_tail_rec_call.

:- type has_mutual_tail_rec_call
    --->    has_mutual_tail_rec_call
    ;       has_no_mutual_tail_rec_call.

:- type has_tail_rec_call
    --->    has_tail_rec_call(
                has_self_tail_rec_call,
                has_mutual_tail_rec_call
            ).

:- type oisu_pred_kind_for
    --->    oisu_creator_for(type_ctor)
    ;       oisu_mutator_for(type_ctor)
    ;       oisu_destructor_for(type_ctor).

    % Is a procedure the subject of any foreign_export pragmas?
    %
:- type proc_foreign_exports
    --->    no_foreign_exports
    ;       has_foreign_exports.

    % Gives an indication of whether or not the procedure
    % might throw an exception.
    %
:- type proc_exception_info
    --->    proc_exception_info(
                proc_exception_status               :: exception_status,
                proc_maybe_excep_analysis_status    :: maybe(analysis_status)
            ).

    % Gives an indication of whether or not the procedure modifies the trail.
    %
:- type proc_trailing_info
    --->    proc_trailing_info(
                proc_trailing_status                :: trailing_status,
                proc_maybe_trail_analysis_status    :: maybe(analysis_status)
            ).

    % Gives an indication of whether or not the procedure, or one of its
    % subgoals, calls a procedure that is tabled using minimal model tabling.
:- type proc_mm_tabling_info
    --->    proc_mm_tabling_info(
                % The tabling status for this procedures as determined
                % by tabling analysis.
                proc_mm_status                      :: mm_tabling_status,

                % The status of the tabling analysis results for this
                % procedure. This is used by the intermodule analysis
                % framework to determine if there is any benefit in
                % re-analysing this procedure.
                proc_mm_analysis_status             :: maybe(analysis_status)
            ).

:- type structure_sharing_info
    --->    structure_sharing_info(
                maybe_sharing   :: maybe(structure_sharing_domain_and_status),
                maybe_imported_sharing      :: maybe(imported_sharing)
                % Records the sharing information from any `.opt' or
                % `.trans_opt' file. This information needs to be processed at
                % the beginning of structure sharing analysis. After that,
                % this field is of no use.
            ).

    % Sharing information is expressed in terms of head variables and the
    % type variables occurring in their types. In order to correctly process
    % (mainly renaming) this information, we need both the list of head
    % variables as well as their types. As this list of head variables may
    % contain any compiler-added head variables, the processing of imported
    % structure sharing information needs to be postponed until the actual
    % structure sharing analysis, which explains the need for the type
    % imported_sharing to temporarily store the imported sharing information.
    %
:- type imported_sharing
    --->    imported_sharing(
                % The list of head variables in which terms the imported
                % sharing is expressed.
                s_headvars        :: list(prog_var),

                % The types of the head variables.
                s_types           :: list(mer_type),

                s_sharing         :: structure_sharing_domain
            ).

:- type structure_reuse_info
    --->    structure_reuse_info(
                maybe_reuse     :: maybe(structure_reuse_domain_and_status),

                maybe_imported_reuse  :: maybe(imported_reuse)
                % Records the reuse information from any `.opt' or
                % `.trans_opt' file. This information needs to be processed
                % at the beginning of structure reuse analysis. After that
                % this field is of no use.
            ).

    % Same rationale as for imported_sharing.
    %
:- type imported_reuse
    --->    imported_reuse(
                % The list of headvars in which terms the imported reuse
                % information is expressed.
                r_headvars        :: list(prog_var),

                % The types of the headvars.
                r_types           :: list(mer_type),

                r_reuse           :: structure_reuse_domain
            ).

:- func structure_sharing_info_init = structure_sharing_info.

:- func structure_reuse_info_init = structure_reuse_info.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

table_step_stats_kind(Step) = KindStr :-
    (
        ( Step = table_trie_step_int(_)
        ; Step = table_trie_step_char
        ; Step = table_trie_step_string
        ; Step = table_trie_step_float
        ; Step = table_trie_step_typeinfo
        ; Step = table_trie_step_typeclassinfo
        ; Step = table_trie_step_foreign_enum
        ),
        KindStr = "MR_TABLE_STATS_DETAIL_HASH"
    ;
        Step = table_trie_step_enum(_),
        KindStr = "MR_TABLE_STATS_DETAIL_ENUM"
    ;
        Step = table_trie_step_general(_Type, IsPoly, ValueOrAddr),
        (
            ValueOrAddr = table_addr,
            KindStr = "MR_TABLE_STATS_DETAIL_HASH"
        ;
            ValueOrAddr = table_value,
            (
                IsPoly = table_is_mono,
                KindStr = "MR_TABLE_STATS_DETAIL_DU"
            ;
                IsPoly = table_is_poly,
                KindStr = "MR_TABLE_STATS_DETAIL_POLY"
            )
        )
    ;
        ( Step = table_trie_step_promise_implied
        ; Step = table_trie_step_dummy
        ),
        KindStr = "MR_TABLE_STATS_DETAIL_NONE"
    ).

structure_sharing_info_init = structure_sharing_info(no, no).

structure_reuse_info_init = structure_reuse_info(no, no).

%---------------------------------------------------------------------------%
:- end_module hlds.proc_info_types.
%---------------------------------------------------------------------------%
