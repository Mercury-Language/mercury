%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
%-----------------------------------------------------------------------------%

:- module mode_debug.

:- interface.

:- import_module hlds_module, hlds_pred, mode_info.

        % Print a debugging message which includes the port, message string,
        % and the current instmap (but only if `--debug-modes' was enabled).
        %
:- pred mode_checkpoint(port, string, mode_info, mode_info).
:- mode mode_checkpoint(in, in, mode_info_di, mode_info_uo) is det.

:- type port
        --->    enter
        ;       exit
        ;       wakeup.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module globals, std_util, assoc_list, io, bool, map.
:- import_module modes, options, mercury_to_mercury, passes_aux.
:- import_module hlds_goal, instmap.

%-----------------------------------------------------------------------------%

        % This code is used to trace the actions of the mode checker.

mode_checkpoint(Port, Msg, ModeInfo0, ModeInfo) :-
        mode_info_get_io_state(ModeInfo0, IOState0),
        globals__io_lookup_bool_option(debug_modes, DoCheckPoint,
                IOState0, IOState1),
        ( DoCheckPoint = yes ->
                mode_checkpoint_2(Port, Msg, ModeInfo0, IOState1, IOState)
        ;
                IOState = IOState1
        ),
        mode_info_set_io_state(ModeInfo0, IOState, ModeInfo).

:- pred mode_checkpoint_2(port, string, mode_info, io__state, io__state).
:- mode mode_checkpoint_2(in, in, mode_info_ui, di, uo) is det.

mode_checkpoint_2(Port, Msg, ModeInfo) -->
        { mode_info_get_errors(ModeInfo, Errors) },
        ( { Port = enter } ->
                io__write_string("Enter "),
                { Detail = yes }
        ; { Port = wakeup } ->
                io__write_string("Wake  "),
                { Detail = no }
        ; { Errors = [] } ->
                io__write_string("Exit "),
                { Detail = yes }
        ;
                io__write_string("Delay  "),
                { Detail = no }
        ),
        io__write_string(Msg),
        ( { Detail = yes } ->
                io__write_string(":\n"),
                globals__io_lookup_bool_option(statistics, Statistics),
                maybe_report_stats(Statistics),
                { mode_info_get_instmap(ModeInfo, InstMap) },
                ( { instmap__is_reachable(InstMap) } ->
                        { instmap__to_assoc_list(InstMap, AssocList) },
                        { mode_info_get_varset(ModeInfo, VarSet) },
                        { mode_info_get_instvarset(ModeInfo, InstVarSet) },
                        write_var_insts(AssocList, VarSet, InstVarSet)
                ;
                        io__write_string("\tUnreachable\n")
                )
        ;
                []
        ),
        io__write_string("\n").

:- pred write_var_insts(assoc_list(var, inst), varset, varset,
                        io__state, io__state).
:- mode write_var_insts(in, in, in, di, uo) is det.

write_var_insts([], _, _) --> [].
write_var_insts([Var - Inst | VarInsts], VarSet, InstVarSet) -->
        io__write_string("\t"),
        mercury_output_var(Var, VarSet, no),
        io__write_string(" :: "),
        mercury_output_inst(Inst, InstVarSet),
        ( { VarInsts = [] } ->
                []
        ;
                io__write_string("\n"),
                write_var_insts(VarInsts, VarSet, InstVarSet)
        ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
