%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
%-----------------------------------------------------------------------------%

:- module report.
:- interface.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module measurement_units.
:- import_module profile.
% TODO: The data structures in query should be moved into a different module,
% then this module may depend on those data structures but not the rest of
% query.
:- import_module query.

%-----------------------------------------------------------------------------%

:- type deep_report
    --->    report_message(message_info)
    ;       report_menu(maybe_error(menu_info))
    ;       report_top_procs(maybe_error(top_procs_info))
    ;       report_proc_static_dump(maybe_error(proc_static_dump_info))
    ;       report_proc_dynamic_dump(maybe_error(proc_dynamic_dump_info))
    ;       report_call_site_static_dump(
                maybe_error(call_site_static_dump_info))
    ;       report_call_site_dynamic_dump(
                maybe_error(call_site_dynamic_dump_info)).

:- type message_info
    --->    message_info(
                % A simple message, this may be used in response to the
                % 'shutdown' and similar queries.

                string
            ).

:- type menu_info
    --->    menu_info(
                % Statistics about the deep profiling data. Gives simple
                % information about the size of the program and its runtime.
                % These statistics are displayed on the menu of the mdprof_cgi
                % program.

                quanta_per_sec              :: int,
                user_quanta                 :: int,
                inst_quanta                 :: int,
                num_callsequs               :: int,
                num_csd                     :: int,
                num_css                     :: int,
                num_pd                      :: int,
                num_ps                      :: int,
                num_clique                  :: int
            ).

:- type top_procs_info
    --->    top_procs_info(
                % Information about the most expensive procedures. The ordering
                % field defines what measurements are used to select and order
                % the procedures in this report.

                ordering                    :: report_ordering,
                top_procs                   :: list(perf_row_data(proc_desc))
            ).

:- type proc_static_dump_info
    --->    proc_static_dump_info(
                psdi_psptr                  :: proc_static_ptr,
                psdi_raw_name               :: string,
                psdi_refined_name           :: string,
                psdi_filename               :: string,
                psdi_linenumber             :: int,
                % Should we list the call_site_statics themselves?
                psdi_num_call_sites         :: int
            ).

:- type proc_dynamic_dump_info
    --->    proc_dynamic_dump_info(
                pddi_pdptr                  :: proc_dynamic_ptr,
                pddi_psptr                  :: proc_static_ptr,
                pddi_ps_raw_name            :: string,
                pddi_ps_refined_name        :: string,
                pddi_call_sites             :: list(call_site_array_slot)
            ).

:- type call_site_static_dump_info
    --->    call_site_static_dump_info(
                cssdi_cssptr                :: call_site_static_ptr,
                cssdi_containing_psptr      :: proc_static_ptr,
                cssdi_slot_number           :: int,
                cssdi_line_number           :: int,
                cssdi_goal_path             :: string,
                cssdi_callee                :: call_site_kind_and_callee
            ).

:- type call_site_dynamic_dump_info
    --->    call_site_dynamic_dump_info(
                csddi_csdptr                :: call_site_dynamic_ptr,
                csddi_caller_pdptr          :: proc_dynamic_ptr,
                csddi_callee_pdptr          :: proc_dynamic_ptr,
                csddi_own_perf              :: perf_row_data(call_site_desc)
            ).

:- type perf_row_data(T)
    --->    perf_row_data(
                % The item represented by this data row.
                subject                     :: T,

                % Port counts.
                calls                       :: int,
                exits                       :: int,
                fails                       :: int,
                redos                       :: int,
                excps                       :: int,

                % Clock ticks and times. We always use simple integers to
                % represent clock ticks, whereas for time, we use more
                % user-friendly units. When the total time for the program
                % is close to zero, the percentage may be NaN representing
                % 'not_applicable' or 'do not know'.
                self_ticks                  :: int,
                self_time                   :: time,
                self_time_percent           :: percent,
                self_time_percall           :: time,

                ticks                       :: int,
                time                        :: time,
                time_percent                :: percent,
                time_percall                :: time,

                % Call sequence counts.
                self_callseqs               :: int,
                self_callseqs_percent       :: percent,
                self_callseqs_percall       :: float,

                callseqs                    :: int,
                callseqs_percent            :: percent,
                callseqs_percall            :: float,

                % Memory allocations.
                self_allocs                 :: int,
                self_allocs_percent         :: percent,
                self_allocs_percall         :: float,

                allocs                      :: int,
                allocs_percent              :: percent,
                allocs_percall              :: float,

                % Memory used. We try to use the most appropriate units
                % for representing each given amount of memory.
                % XXX Memory per call might not be an integer, so we should
                % make sure that the memory type can represent fractions.
                bytes_per_word              :: int,
                self_mem                    :: memory,
                self_mem_percent            :: percent,
                self_mem_percall            :: memory,

                mem                         :: memory,
                mem_percent                 :: percent,
                mem_percall                 :: memory
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

    % The representation of a procedure in a report structure, including
    % information about its location in Mercury source code.
    %
:- type proc_desc
    --->    proc_desc(
                proc_static_ptr             :: proc_static_ptr,
                proc_file_name              :: string,
                proc_line_number            :: int,
                proc_refined_name           :: string
            ).

    % The representation of a call site in a report structure, including
    % information about its location in Mercury source code.
    %
:- type call_site_desc
    --->    call_site_desc(
                call_site_static_ptr        :: call_site_static_ptr,
                call_site_container         :: proc_static_ptr,
                call_site_file_name         :: string,
                call_site_line_number       :: int,
                call_site_refined_name      :: string,
                call_site_slot_number       :: int,
                call_site_goal_path         :: string
            ).

%-----------------------------------------------------------------------------%
:- end_module report.
%-----------------------------------------------------------------------------%
