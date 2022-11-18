%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1997, 2003-2009, 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mode_debug.m.
% Main author: fjh.
%
% This module contains code for tracing the actions of the mode checker.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.mode_debug.
:- interface.

:- import_module check_hlds.mode_info.

    % Print a debugging message which includes the port, message string,
    % and the current instmap (but only if `--debug-modes' was enabled).
    %
:- pred mode_checkpoint(port::in, string::in, mode_info::in, mode_info::out)
    is det.

:- type port
    --->    enter
    ;       exit
    ;       wakeup.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

mode_checkpoint(Port, Msg, !ModeInfo) :-
    mode_info_get_debug_modes(!.ModeInfo, DebugModes),
    (
        DebugModes = no
    ;
        DebugModes = yes(DebugFlags),
        (
            Port = enter,
            PortStr = "Enter ",
            Detail = yes
        ;
            Port = wakeup,
            PortStr = "Wake ",
            Detail = no
        ;
            Port = exit,
            mode_info_get_errors(!.ModeInfo, Errors),
            (
                Errors = [],
                PortStr = "Exit ",
                Detail = yes
            ;
                Errors = [_ | _],
                PortStr = "Delay ",
                Detail = no
            )
        ),
        DebugFlags = debug_flags(UniquePrefix, Verbose, Minimal, Statistics),
        (
            Detail = yes,
            mode_info_get_instmap(!.ModeInfo, InstMap),
            trace [io(!IO)] (
                mode_info_get_module_info(!.ModeInfo, ModuleInfo),
                module_info_get_globals(ModuleInfo, Globals),
                module_info_get_name(ModuleInfo, ModuleName),
                get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
                io.format(DebugStream, "%s%s%s:\n",
                    [s(PortStr), s(UniquePrefix), s(Msg)], !IO),
                maybe_report_stats(DebugStream, Statistics, !IO),
                maybe_flush_output(DebugStream, Statistics, !IO),
                ( if instmap_is_reachable(InstMap) then
                    instmap_to_assoc_list(InstMap, NewInsts),
                    mode_info_get_last_checkpoint_insts(!.ModeInfo,
                        OldInstMap),
                    mode_info_get_var_table(!.ModeInfo, VarTable),
                    mode_info_get_instvarset(!.ModeInfo, InstVarSet),
                    write_var_insts(DebugStream, VarTable, InstVarSet,
                        OldInstMap, Verbose, Minimal, NewInsts, !IO)
                else
                    io.write_string(DebugStream, "\tUnreachable\n", !IO)
                ),
                io.write_string(DebugStream, "\n", !IO),
                io.flush_output(DebugStream, !IO)
            ),
            mode_info_set_last_checkpoint_insts(InstMap, !ModeInfo)
        ;
            Detail = no,
            trace [io(!IO)] (
                mode_info_get_module_info(!.ModeInfo, ModuleInfo),
                module_info_get_globals(ModuleInfo, Globals),
                module_info_get_name(ModuleInfo, ModuleName),
                get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
                io.format(DebugStream, "%s%s%s:\n",
                    [s(PortStr), s(UniquePrefix), s(Msg)], !IO),
                io.flush_output(DebugStream, !IO)
            )
        )
    ).

:- pred write_var_insts(io.text_output_stream::in,
    var_table::in, inst_varset::in, instmap::in, bool::in, bool::in,
    assoc_list(prog_var, mer_inst)::in,
    io::di, io::uo) is det.

write_var_insts(_, _, _, _, _, _, [], !IO).
write_var_insts(Stream, VarTable, InstVarSet, OldInstMap, Verbose, Minimal,
        [Var - Inst | VarInsts], !IO) :-
    instmap_lookup_var(OldInstMap, Var, OldInst),
    ( if
        (
            identical_insts(Inst, OldInst)
        ;
            Inst = OldInst
        )
    then
        (
            Verbose = yes,
            io.write_string(Stream, "\t", !IO),
            mercury_output_var(VarTable, print_name_only, Var, Stream, !IO),
            io.write_string(Stream, " :: unchanged", !IO)
        ;
            Verbose = no
        )
    else
        io.write_string(Stream, "\t", !IO),
        mercury_output_var(VarTable, print_name_only, Var, Stream, !IO),
        (
            Minimal = yes,
            io.write_string(Stream, " :: changed\n", !IO)
        ;
            Minimal = no,
            io.write_string(Stream, " ::\n", !IO),
            mercury_output_structured_inst(Stream, output_debug, InstVarSet,
                do_not_incl_addr, 2, Inst, !IO)
        )
    ),
    write_var_insts(Stream, VarTable, InstVarSet, OldInstMap, Verbose, Minimal,
        VarInsts, !IO).

    % In the usual case of a C backend, this predicate allows us to conclude
    % that two insts are identical without traversing them. Since the terms
    % can be very large, this is a big gain; it can turn the complexity
    % of printing a checkpoint from quadratic in the number of variables
    % live at the checkpoint (when the variables are e.g. all part of a
    % single long list) to linear. The minor increase in the constant factor
    % in cases where identical_insts fails is much easier to live with.
    %
:- pred identical_insts(mer_inst::in, mer_inst::in) is semidet.

identical_insts(_, _) :-
    semidet_fail.

:- pragma foreign_proc("C",
    identical_insts(InstA::in, InstB::in),
    [will_not_call_mercury, promise_pure],
"
    if (InstA == InstB) {
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

%-----------------------------------------------------------------------------%
:- end_module check_hlds.mode_debug.
%-----------------------------------------------------------------------------%
