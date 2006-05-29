%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.analysis.m
% Main authors: nancy
%
% Implementation of the structure reuse analysis (compile-time garbage
% collection system): each procedure is analysed to see whether some
% of the terms it manipulates become garbage thus making it possible
% to reuse that garbage straight away for creating new terms.
%
% Structure reuse is broken up into three phases: 
%   * the direct reuse analysis (structure_reuse.direct.m) 
%   * the indirect analysis (structure_reuse.indirect.m)
%   * and the generation of the optimised procedures.
% 
% The following example shows instances of direct and indirect reuse: 
%
% list__append(H1, H2, H3) :-
%   (
%       H1 => [],
%       H3 := H2
%   ;
%           % Cell H1 dies provided some condition about the
%           % structure sharing of H1 is true.  A deconstruction
%           % generating a dead cell, followed by a
%           % construction reusing that cell, is called a direct
%           % reuse. 
%       H1 => [X | Xs],
%
%           % If the condition about the structure sharing of H1
%           % is true then we can call the version of list__append 
%           % which does reuse. Calling the optimised version here leads
%           % to a new condition to be met by the headvars of any
%           % call to the resulting optimised version of append.
%           % This is an indirect reuse.
%       list__append(Xs, H2, Zs),
%
%           % Reuse the dead cell H1.  This is a direct reuse.
%       H3 <= [X | Zs]
%   ).
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.analysis.

:- interface.

:- import_module hlds.hlds_module.

:- import_module io. 

:- pred structure_reuse_analysis(module_info::in, module_info::out, 
    io::di, io::uo) is det.

:- implementation.

:- import_module check_hlds.goal_path.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_out.
:- import_module transform_hlds.ctgc.structure_reuse.direct.
:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.ctgc.structure_reuse.indirect.
:- import_module transform_hlds.ctgc.structure_reuse.lbu.
:- import_module transform_hlds.ctgc.structure_reuse.lfu.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module string.


structure_reuse_analysis(!ModuleInfo, !IO):- 
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),

    % Load all available structure sharing information into a sharing table.
    SharingTable = load_structure_sharing_table(!.ModuleInfo),

    % Load all the available reuse information into a reuse table.
    % XXX TO DO!
    % ReuseTable0 = load_structure_reuse_table(!.ModuleInfo), 
   
    % Pre-annotate each of the goals with "Local Forward Use" and
    % "Local Backward Use" information, and fill in all the goal_path slots
    % as well. 

    maybe_write_string(VeryVerbose, "% Annotating in use information...", !IO), 
    process_all_nonimported_procs(update_proc_io(annotate_in_use_information),
         !ModuleInfo, !IO),
    maybe_write_string(VeryVerbose, "done.\n", !IO),

    % Determine information about possible direct reuses.
    maybe_write_string(VeryVerbose, "% Direct reuse...\n", !IO), 
    DummyReuseTable = reuse_as_table_init, 
    direct_reuse_pass(SharingTable, !ModuleInfo, 
        DummyReuseTable, ReuseTable1, !IO),
    maybe_write_string(VeryVerbose, "% Direct reuse: done.\n", !IO),
    reuse_as_table_maybe_dump(VeryVerbose, ReuseTable1, !IO),

    % Determine information about possible indirect reuses.
    maybe_write_string(VeryVerbose, "% Indirect reuse...\n", !IO), 
    indirect_reuse_pass(SharingTable, !ModuleInfo, ReuseTable1, ReuseTable2, 
       !IO), 
    maybe_write_string(VeryVerbose, "% Indirect reuse: done.\n", !IO),
    reuse_as_table_maybe_dump(VeryVerbose, ReuseTable2, !IO).

    % For every procedure that has some potential (conditional) reuse (either 
    % direct or indirect), create a new procedure that actually implements
    % that reuse. 
    % XXX TO DO!
    % split_reuse_procedures(!ModuleInfo, ReuseTable2, ReuseTable3, !IO), 
    % reuse_as_table_maybe_dump(VeryVerbose, ReuseTable3, !IO).

    % Record the results of the reuse table into the HLDS.
    % XXX TO DO!
    % map.foldl(save_reuse_in_module_info, ReuseTable3, !ModuleInfo).
    %
    % Output some profiling information.
    % XXX TO DO!
    % profiling(!.ModuleInfo, ReuseTable3).

:- pred annotate_in_use_information(pred_id::in, proc_id::in,
    module_info::in, proc_info::in, proc_info::out, io::di, io::uo) is det.

annotate_in_use_information(_PredId, _ProcId, ModuleInfo, !ProcInfo, !IO) :- 
    forward_use_information(!ProcInfo), 
    backward_use_information(ModuleInfo, !ProcInfo),
    goal_path.fill_goal_path_slots(ModuleInfo, !ProcInfo).


%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "structure_reuse.analysis.m".

:- end_module transform_hlds.ctgc.structure_reuse.analysis.
