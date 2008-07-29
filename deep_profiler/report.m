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
% This module defined deep profiling reports.  Deep profiling reports are the
% result of querying a Deep profile, a different query results in a different
% type of report.  They are processed further by display_report.m and
% html_format.m in order to create HTML output.  Other tools may make use of
% the information in deep profiling reports.
%
%-----------------------------------------------------------------------------%

:- module report.
:- interface.

:- import_module list.
:- import_module string.

:- import_module measurement_units.
:- import_module profile.
% TODO: The data structures in query should be moved into a different module,
% then this module may depend on those datastructures but not the rest of
% query.
:- import_module query.

%-----------------------------------------------------------------------------%

:- type deep_report
    --->    report_message(string)
                % A simple message, this may be used in response to the
                % 'shutdown' and similar queries.
    
    ;       report_menu(
                % Statistics about the deep profiling data.  Gives simple
                % information about the size of the program and it's runtime.
                % These statistics are displayed on the menu of the mdprof_cgi
                % program.

                quanta_per_sec  :: int,
                user_quanta     :: int,
                inst_quanta     :: int,
                num_callsequs   :: int,
                num_csd         :: int,
                num_css         :: int,
                num_pd          :: int,
                num_ps          :: int,
                num_clique      :: int
            )
    ;       report_top_procs(
                % Information about the most expensive procedures.  ordering
                % defines what measurements are used to select and order the
                % procedures in this report.

                ordering        :: report_ordering,
                top_procs       :: list(row_data(report_proc))
            ).


:- type row_data(T)
    --->    row_data(
                % The item represented by this data row.
                subject             :: T,

                % Port counts
                calls               :: int,
                exits               :: int,
                fails               :: int,
                redos               :: int,
                excps               :: int,

                % Clock ticks and times, Ticks are 'countable' so int is
                % used.  Time is discrete and has units so a time type is
                % used.  When percent is NaN because the total time for the
                % program is close to zero the percent type will represent
                % 'not_applicable' or 'do not know'.
                self_ticks          :: int,
                self_time           :: time,
                self_time_percent   :: percent,
                self_time_percall   :: time,
                
                ticks               :: int,
                time                :: time,
                time_percent        :: percent,
                time_percall        :: time,
                
                % Call sequence counts,
                self_callseqs           :: int,
                self_callseqs_percent   :: percent,
                self_callseqs_percall   :: float,

                callseqs                :: int,
                callseqs_percent        :: percent,
                callseqs_percall        :: float,

                % Memory allocations,
                self_allocs         :: int,
                self_allocs_percent :: percent,
                self_allocs_percall :: float,

                allocs              :: int,
                allocs_percent      :: percent,
                allocs_percall      :: float,

                % Memory used.  Since memory has a scale it's stored in a
                % memory type.  Memory per call might not be an integer,
                % perhaps a new type should be used here.
                bytes_per_word      :: int,
                self_mem            :: memory,
                self_mem_percent    :: percent,
                self_mem_percall    :: memory,

                mem                 :: memory,
                mem_percent         :: percent,
                mem_percall         :: memory
            ).

    % This type is used to define 'most expensive procedures'.  It contains all
    % the parameters that the query is given to select and order the procedures
    % shown in the report.  See also the function symbol deep_cmd_top_procs of
    % the cmd type.
    %
:- type report_ordering
    --->    report_ordering(
                display_limit   :: display_limit,
                cost_kind       :: cost_kind,
                incl_desc       :: include_descendants,
                scope           :: measurement_scope
            ).


    % The representation of a procedure in the report structure, including
    % information about its location in mercury source code.
    %
:- type report_proc
    --->    report_proc(
                proc_static_ptr     :: proc_static_ptr,
                proc_filename       :: string,
                proc_linenumber     :: int,
                proc_name           :: string
            ).

%-----------------------------------------------------------------------------%
:- end_module report.
%-----------------------------------------------------------------------------%
