%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: report.m.
% Author: pbone.
%
% This module defines deep profiling reports. Each report is the result of
% querying a Deep.data file. Different queries result in different types
% of reports. Reports can be converted into HTML by display_report.m and
% html_format.m, but some tools may use the data structures constructed here
% directly.
%
%---------------------------------------------------------------------------%

:- module report.
:- interface.

:- import_module coverage.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module measurement_units.
:- import_module profile.
% TODO: The data structures in query should be moved into a different module,
% then this module may depend on those data structures but not the rest of
% query.
:- import_module query.
:- import_module var_use_analysis.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module unit.

%---------------------------------------------------------------------------%

:- type deep_report
    --->    report_message(
                message_report
            )
    ;       report_menu(
                maybe_error(menu_report)
            )
    ;       report_clique(
                maybe_error(clique_report)
            )
    ;       report_clique_recursion_costs(
                maybe_error(clique_recursion_report)
            )
    ;       report_recursion_types_frequency(
                maybe_error(recursion_types_frequency_report)
            )
    ;       report_program_modules(
                maybe_error(program_modules_report)
            )
    ;       report_module(
                maybe_error(module_report)
            )
    ;       report_module_getter_setters(
                maybe_error(module_getter_setters_report)
            )
    ;       report_module_rep(
                maybe_error(module_rep_report)
            )
    ;       report_top_procs(
                maybe_error(top_procs_report)
            )
    ;       report_proc(
                maybe_error(proc_report)
            )
    ;       report_procrep_coverage(
                maybe_error(procrep_coverage_info)
            )
    ;       report_proc_callers(
                maybe_error(proc_callers_report)
            )
    ;       report_proc_static_dump(
                maybe_error(proc_static_dump_info)
            )
    ;       report_proc_dynamic_dump(
                maybe_error(proc_dynamic_dump_info)
            )
    ;       report_call_site_static_dump(
                maybe_error(call_site_static_dump_info)
            )
    ;       report_call_site_dynamic_dump(
                maybe_error(call_site_dynamic_dump_info)
            )
    ;       report_clique_dump(
                maybe_error(clique_dump_info)
            )
    ;       report_call_site_dynamic_var_use(
                maybe_error(call_site_dynamic_var_use_info)
            ).

:- type message_report
    --->    message_report(
                % A simple message, this may be used in response to the
                % 'shutdown' and similar queries.

                string
            ).

:- type menu_report
    --->    menu_report(
                % Statistics about the deep profiling data. Gives simple
                % information about the size of the program and its runtime.
                % These statistics are displayed on the menu of the mdprof_cgi
                % program.

                menu_program_name           :: string,
                menu_quanta_per_sec         :: int,
                menu_user_quanta            :: int,
                menu_inst_quanta            :: int,
                menu_num_callseqs           :: int,
                menu_num_csd                :: int,
                menu_num_css                :: int,
                menu_num_pd                 :: int,
                menu_num_ps                 :: int,
                menu_num_clique             :: int
            ).

:- type clique_report
    --->    clique_report(
                % The clique the report is for.
                cr_clique_ptr               :: clique_ptr,

                % The ancestor call sites of the clique,
                % - from the innermost ancestor call site (in the parent)
                % - to the outermost ancestor call site (in main/2).
                cr_ancestor_call_sites      :: list(
                                                perf_row_data(ancestor_desc)),

                % Reports for every procedure in the clique. The first
                % procedure in the list is the entry procedure (if the parent
                % clique had a valid CSD.) and that CSD was found in the
                % clique. TODO: we might want to express this within this
                % type.
                cr_clique_procs             :: list(clique_proc_report)
            ).

:- type clique_proc_report
    --->    clique_proc_report(
                % Summary information for the cost of this procedure in this
                % clique.
                cpr_proc_summary            :: perf_row_data(proc_desc),

                % A clique may contain more than one proc_dynamic for the same
                % procedure in one rare circumstance. Consider a clique of
                % two procedures, let's call them p and q. The entry procedure
                % is p. When p calls q and q then calls p, the runtime
                % discovers they are in the same clique. However, after all
                % those mutually recursive calls unwind and we get back to the
                % original call to p, p can call q again. If it does so through
                % a call site through which it has not called q before, then
                % the call port code in q will allocate a new proc_dynamic,
                % since it will not find an existing one it can reuse;
                % q's proc_static won't point to an active proc_dynamic, and
                % the call site will not supply one either.
                %
                % We could modify the code we invoke at call ports to keep a
                % list of the proc_dynamics of the procedures known to be in
                % the current clique, and to reuse the proc_dynamic of the
                % callee in these cases, but that would significantly increase
                % the call port overhead. Instead, we let the callee allocate a
                % second (and maybe third etc) proc_dynamic for a procedure,
                % and let the deep profiler sort it out.
                %
                % Note that the entry procedure of a clique will always have
                % exactly one proc_dynamic.
                cpr_first_proc_dynamic      :: clique_proc_dynamic_report,
                cpr_other_proc_dynamics     :: list(clique_proc_dynamic_report)
            ).

:- type clique_proc_dynamic_report
    --->    clique_proc_dynamic_report(
                % Summary information for the cost of this proc _dynamic
                % in this clique.
                cpdr_proc_summary           :: perf_row_data(proc_desc),

                % Information about the costs of the call sites in this
                % procedure in this clique.
                cpdr_call_sites             :: list(clique_call_site_report)
            ).

:- type clique_call_site_report
    --->    clique_call_site_report(
                % The id of the call site, and the summary performance for
                % that call site.
                ccsr_call_site_summary      :: perf_row_data(call_site_desc),

                ccsr_kind_and_callee        :: call_site_kind_and_callee(
                                                proc_desc),

                % All the callees and their associated performance information.
                % If ccsr_kind_and_callee indicates a normal call, the list
                % will have either zero entries (no calls through this site)
                % or one entry (some calls through this site). If this is
                % some other kind of call site, the list may have any number
                % of entries.
                ccsr_callee_perfs           :: list(perf_row_data(clique_desc))
            ).

:- type clique_recursion_report
    --->    clique_recursion_report(
                % The clique the report is for.
                crr_clique_ptr              :: clique_ptr,

                % The type of recursion in this clique.
                crr_recursion_type          :: recursion_type,

                % The number of procedures in the clique.
                crr_num_procs               :: int
            ).

:- type recursion_type
    --->    rt_not_recursive
    ;       rt_single(
                rts_base                    :: recursion_level_report,
                rts_recursive               :: recursion_level_report,
                rts_avg_max_depth           :: float,

                % These costs are per-call.
                rts_avg_rec_cost            :: float,

                % The cost at any level is Cost = func(Level).
                rts_any_rec_cost            :: (func(int) = float)
            )
    ;       rt_divide_and_conquer(
                rtdsc_base                  :: recursion_level_report,
                rtdsc_recursive             :: recursion_level_report
            )
    ;       rt_mutual_recursion(
                rtml_num_procs              :: int
            )
    ;       rt_other(
                rto_all_levels              :: list(recursion_level_report)
            )
    ;       rt_errors(
                rte_errors                  :: list(string)
            ).

    % Recursion types for which call costs at any level can be calculated.
    %
:- inst recursion_type_known_costs
    --->    rt_not_recursive
    ;       rt_single(ground, ground, ground, ground, ground).

    % Recursion types for which call costs at any level can not be calculated.
    %
:- inst recursion_type_unknown_costs
    --->    rt_divide_and_conquer(ground, ground)
    ;       rt_mutual_recursion(ground)
    ;       rt_other(ground)
    ;       rt_errors(ground).

:- type recursion_level_report
    --->    recursion_level_report(
                rlr_level                       :: int,
                rlr_calls                       :: int,
                rlr_prob                        :: probability,
                rlr_non_rec_calls_cost          :: float,
                rlr_rec_calls_ex_chld_cost      :: float
            ).

:- type recursion_types_frequency_report
    --->    recursion_types_frequency_report(
                rtfr_histogram                  :: recursion_type_histogram
            ).

:- type recursion_type_histogram ==
    map(recursion_type_simple, recursion_type_freq_data).

:- type recursion_type_simple
    --->    rts_not_recursive
    ;       rts_single
    ;       rts_divide_and_conquer
    ;       rts_mutual_recursion(
                rtsmr_num_procs                 :: int
            )
    ;       rts_other(
                rtso_levels                     :: set(int)
            )
    ;       rts_error(
                rtse_error                      :: string
            )
    ;       rts_total_error_instances.

:- type recursion_type_freq_data
    --->    recursion_type_freq_data(
                rtfd_freq                       :: int,
                rtfd_percent                    :: percent,
                rtfd_maybe_summary              :: maybe(perf_row_data(unit)),
                rtfd_entry_procs                :: recursion_type_proc_map
            ).

:- type recursion_type_proc_map ==
    map(proc_static_ptr, recursion_type_proc_freq_data).

:- type recursion_type_proc_freq_data
    --->    recursion_type_proc_freq_data(
                rtpfd_freq                      :: int,
                rtpfd_percent                   :: percent,
                rtpfd_summary                   :: perf_row_data(proc_desc)
            ).

:- type program_modules_report
    --->    program_modules_report(
                % Summary information about all the modules of the program.
                program_modules             :: list(
                                                perf_row_data(module_active))
            ).

:- type module_report
    --->    module_report(
                % Summary information about all the procedures in one module
                % of the program.
                mr_module_name              :: string,
                mr_have_module_rep          :: maybe_have_module_rep,
                mr_procs                    :: list(perf_row_data(proc_active))
            ).

:- type maybe_have_module_rep
    --->    do_not_have_module_rep
    ;       have_module_rep.

:- type data_struct_name
    --->    data_struct_name(string).

:- type field_name
    --->    field_name(string).

:- type getter_or_setter
    --->    getter
    ;       setter.

:- type gs_field_info(I, S)
    --->    gs_field_both(
                gsf_both_getter             :: I,
                gsf_both_setter             :: I,
                gsf_both_summary            :: S
            )
    ;       gs_field_getter(
                gsf_getter                  :: I
            )
    ;       gs_field_setter(
                gsf_setter                  :: I
            ).

:- type gs_field_map(T) == map(field_name, T).
:- type gs_ds_map(T)    == map(data_struct_name, gs_field_map(T)).

:- type gs_field_info   == gs_field_info(
                                perf_row_data(proc_desc),
                                perf_row_data(unit)
                           ).
:- type gs_field_map    == gs_field_map(gs_field_info).
:- type gs_ds_map       == gs_ds_map(gs_field_info).

:- type module_getter_setters_report
    --->    module_getter_setters_report(
                % Summary information about all the getter/setter procedures
                % in one module of the program.
                mgsr_module_name            :: string,
                mgsr_procs                  :: gs_ds_map
            ).

:- type module_rep_report
    --->    module_rep_report(
                % A representation of one module of the program.
                mrr_module_name             :: string,
                mrr_report                  :: string
            ).

:- type top_procs_report
    --->    top_procs_report(
                % Information about the most expensive procedures. The ordering
                % field defines what measurements are used to select and order
                % the procedures in this report.

                tp_ordering                 :: report_ordering,
                tp_top_procs                :: list(perf_row_data(proc_desc))
            ).

:- type proc_report
    --->    proc_report(
                % The integer is a count of the number of cliques this
                % procedure appears in.
                proc_callers_summary        :: perf_row_data(callers_counts),

                % The proc description is inside the proc_summary field.
                proc_summary                :: perf_row_data(proc_desc),
                proc_call_site_summaries    :: list(call_site_perf)
            ).

    % A count of the number of callers.
    %
:- type callers_counts
    --->    callers_counts(
                cc_static                   :: int,
                cc_dynamic                  :: int
            ).

:- type call_site_perf
    --->    call_site_perf(
                % The csf_summary_perf field contains the description of this
                % call site and the summary of its performance.
                %
                % If the call site can call only one procedure, the csf_kind
                % field tell you the id and the name of that procedure, and
                % the type substitution made in the call to it, if there is one
                % and it is known.
                %
                % Otherwise, if the call site can call more than one procedure,
                % the csf_sub_callees field will have one performance row
                % giving the contribution of each of the called procedures.
                %
                % If csf_kind = normal_call_and_info(_), then csf_sub_callees
                % should be the empty list.
                %
                % The call site description is inside the csf_summary field.

                csf_kind                    :: call_site_kind_and_info(
                                                normal_callee_id),
                csf_summary_perf            :: perf_row_data(call_site_desc),
                csf_sub_callees             :: list(perf_row_data(proc_desc))
            ).

:- type normal_callee_id
    --->    normal_callee_id(
                nci_callee_desc             :: proc_desc,
                nci_type_subst              :: string
            ).

:- type procrep_coverage_info
    --->    procrep_coverage_info(
                prci_proc                   :: proc_static_ptr,
                prci_proc_rep               :: proc_rep(goal_id),
                prci_coverage_array         :: goal_attr_array(coverage_info)
            ).

:- type proc_callers_report
    --->    proc_callers_report(
                % The id of the procedure.
                pc_proc_desc                :: proc_desc,

                % The call sites that call this procedure.
                pc_callers                  :: proc_callers,

                % Which batch of rows to show; transmitted without processing
                % from the request.
                pc_batch_number             :: int,

                % How many callers per batch, also transmitted without
                % processing from the request.
                pc_callers_per_batch        :: int,

                % Whether contour exclusion was applied in computing the
                % pc_callers field.
                pc_contour_exclusion        :: contour_exclusion,

                % If contour exclusion was asked for, this field may contain
                % a warning message about nonfatal problems with the exclusion
                % specification file.
                pc_contour_warn_message     :: maybe(string)
            ).

:- type proc_callers
    --->    proc_caller_call_sites(
                pc_caller_call_sites        :: list(
                                                perf_row_data(call_site_desc))
            )
    ;       proc_caller_procedures(
                pc_caller_procedures        :: list(
                                                perf_row_data(proc_desc))
            )
    ;       proc_caller_modules(
                pc_caller_modules           :: list(
                                                perf_row_data(string))
            )
    ;       proc_caller_cliques(
                pc_caller_cliques           :: list(
                                                perf_row_data(clique_desc))
            ).

:- type proc_static_dump_info
    --->    proc_static_dump_info(
                psdi_psptr                  :: proc_static_ptr,
                psdi_raw_name               :: string,
                psdi_uq_refined_name        :: string,
                psdi_q_refined_name         :: string,
                psdi_filename               :: string,
                psdi_linenumber             :: int,
                % Should we list the call_site_statics and coverage points
                % themselves?
                psdi_num_call_sites         :: int,
                psdi_num_coverage_points    :: int
            ).

:- type proc_dynamic_dump_info
    --->    proc_dynamic_dump_info(
                pddi_pdptr                  :: proc_dynamic_ptr,
                pddi_psptr                  :: proc_static_ptr,
                pddi_ps_raw_name            :: string,
                pddi_ps_module_name         :: string,
                pddi_ps_uq_refined_name     :: string,
                pddi_ps_q_refined_name      :: string,
                pddi_call_sites             :: list(call_site_array_slot),
                pddi_coverage_points        :: maybe(list(coverage_point))
            ).

:- type call_site_static_dump_info
    --->    call_site_static_dump_info(
                cssdi_cssptr                :: call_site_static_ptr,
                cssdi_containing_psptr      :: proc_static_ptr,
                cssdi_slot_number           :: int,
                cssdi_line_number           :: int,
                cssdi_goal_path             :: reverse_goal_path,
                cssdi_callee                :: call_site_kind_and_callee
            ).

:- type call_site_dynamic_dump_info
    --->    call_site_dynamic_dump_info(
                csddi_csdptr                :: call_site_dynamic_ptr,
                csddi_caller_pdptr          :: proc_dynamic_ptr,
                csddi_callee_pdptr          :: proc_dynamic_ptr,
                csddi_own_perf              :: perf_row_data(call_site_desc)
            ).

:- type clique_dump_info
    --->    clique_dump_info(
                cdi_clique_desc             :: clique_desc,
                cdi_caller_csd_ptr          :: call_site_dynamic_ptr,
                cdi_member_pdptrs           :: list(proc_dynamic_ptr)
            ).

:- type call_site_dynamic_var_use_info
    --->    call_site_dynamic_var_use_info(
                csdvui_total_cost           :: float,
                csdvui_var_uses             :: list(var_use_and_name)
            ).

:- type var_use_and_name
    --->    var_use_and_name(
                vun_var_name                :: string,
                vun_use                     :: var_use_info
            ).

    % This type represents information about the performance of the subject.
    % It is intended to be displayed on a browser page or used by a tool as is.
    % It is NOT intended to be subject to further processing, such as adding
    % two perf_row_datas together. Any such arithmetic is much better done
    % on the data structures from which perf_row_datas are derived
    % (own_prof_info and inherit_prof_info in the measurements module).
    %
:- type perf_row_data(T)
    --->    perf_row_data(
                % The item represented by this data row.
                perf_row_subject                :: T,

                % Port counts.
                perf_row_calls                  :: int,
                perf_row_exits                  :: int,
                perf_row_fails                  :: int,
                perf_row_redos                  :: int,
                perf_row_excps                  :: int,

                perf_row_bytes_per_word         :: int,
                perf_row_self                   :: inheritable_perf,
                perf_row_maybe_total            :: maybe(inheritable_perf)
            ).

:- type inheritable_perf
    --->    inheritable_perf(
                % Clock ticks and times. We always use simple integers to
                % represent clock ticks, whereas for time, we use more
                % user-friendly units. When the total time for the program
                % is close to zero, i.e. the number of ticks or quanta is zero
                % for the whole program, then the percentage may be zero
                % for everything (it used to be a NaN for 0/0, but we now
                % check explicitly for division by zero).
                perf_row_ticks             :: int,
                perf_row_time              :: time,
                perf_row_time_percent      :: percent,
                perf_row_time_percall      :: time,

                % Call sequence counts.
                perf_row_callseqs          :: int,
                perf_row_callseqs_percent  :: percent,
                perf_row_callseqs_percall  :: float,

                % Memory allocations.
                perf_row_allocs            :: int,
                perf_row_allocs_percent    :: percent,
                perf_row_allocs_percall    :: float,

                % Memory used. We try to use the most appropriate units
                % for representing each given amount of memory.
                % XXX Memory per call might not be an integer, so we should
                % make sure that the memory type can represent fractions.
                perf_row_mem               :: memory,
                perf_row_mem_percent       :: percent,
                perf_row_mem_percall       :: memory
            ).

    % This type is used to define 'most expensive procedures'. It contains all
    % the parameters that the query is given to select and order the procedures
    % shown in the report. See also the function symbol deep_cmd_top_procs of
    % the cmd type.
    %
:- type report_ordering
    --->    report_ordering(
                display_limit               :: display_limit,
                cost_kind                   :: cost_kind,
                incl_desc                   :: include_descendants,
                scope                       :: measurement_scope
            ).

    % The representation of a module in a report structure.
    %
:- type module_active
    --->    module_active(
                ma_module_name              :: string,
                ma_is_active                :: module_is_active
            ).

:- type module_is_active
    --->    module_is_active
    ;       module_is_not_active.

:- type proc_active
    --->    proc_active(
                pa_proc_desc                :: proc_desc,
                pa_is_active                :: proc_is_active
            ).

:- type proc_is_active
    --->    proc_is_active
    ;       proc_is_not_active.

    % The representation of a procedure in a report structure, including
    % information about its location in Mercury source code.
    %
:- type proc_desc
    --->    proc_desc(
                pdesc_ps_ptr                    :: proc_static_ptr,
                pdesc_file_name                 :: string,
                pdesc_line_number               :: int,
                pdesc_module_name               :: string,
                pdesc_uq_refined_name           :: string,
                pdesc_q_refined_name            :: string
            ).

    % The representation of a call site in a report structure, including
    % information about its location in Mercury source code.
    %
:- type call_site_desc
    --->    call_site_desc(
                csdesc_css_ptr                  :: call_site_static_ptr,
                csdesc_container                :: proc_static_ptr,
                csdesc_file_name                :: string,
                csdesc_line_number              :: int,
                csdesc_caller_module_name       :: string,
                csdesc_caller_uq_refined_name   :: string,
                csdesc_caller_q_refined_name    :: string,
                csdesc_slot_number              :: int,
                csdesc_goal_path                :: reverse_goal_path,
                csdesc_maybe_callee             :: maybe(proc_desc)
            ).

:- type ancestor_desc
    --->    ancestor_desc(
                % The caller and callee cliques.
                ad_caller_clique_ptr            :: clique_ptr,
                ad_callee_clique_ptr            :: clique_ptr,

                % The callee procedure.
                ad_callee_pdesc                 :: proc_desc,

                % The description of the call site. This includes a description
                % of the caller procedure.
                ad_call_site_desc               :: call_site_desc
            ).

    % The description of a clique in a report structure.
    %
:- type clique_desc
    --->    clique_desc(
                cdesc_clique_ptr                :: clique_ptr,
                cdesc_entry_member              :: proc_desc,
                cdesc_other_members             :: list(proc_desc)
            ).

%---------------------------------------------------------------------------%
%
% Predicates to make common actions on these types easier.
%

:- pred proc_label_from_proc_desc(deep::in, proc_desc::in,
    string_proc_label::out) is det.

    % foldl(add_call_site_report_to_map(CallSiteReports, map.init,
    %   CallSitesMap).
    %
    % Build a map from goal paths to call site reports.
    %
:- pred add_call_site_report_to_map(clique_call_site_report::in,
    map(reverse_goal_path, clique_call_site_report)::in,
    map(reverse_goal_path, clique_call_site_report)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

proc_label_from_proc_desc(Deep, ProcDesc, ProcLabel) :-
    PSPtr = ProcDesc ^ pdesc_ps_ptr,
    deep_lookup_proc_statics(Deep, PSPtr, ProcStatic),
    ProcLabel = ProcStatic ^ ps_id.

add_call_site_report_to_map(CallSite, !Map) :-
    GoalPath = CallSite ^ ccsr_call_site_summary ^ perf_row_subject
        ^ csdesc_goal_path,
    map.det_insert(GoalPath, CallSite, !Map).

%---------------------------------------------------------------------------%
:- end_module report.
%---------------------------------------------------------------------------%
