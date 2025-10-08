%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015, 2021, 2024-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_test.m.
%
% This module contains tests on modes.
%
%---------------------------------------------------------------------------%

:- module check_hlds.mode_test.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%
%
% Tests on modes.
%

    % Succeed iff the mode is input.
    % Throw an exception if the mode is undefined.
    %
    % A mode is considered input if the initial inst is bound.
    %
:- pred mode_is_input(module_info::in, mer_mode::in) is semidet.
:- pred init_inst_is_input(module_info::in, mer_inst::in) is semidet.

    % Succeed iff the mode is fully input.
    % Throw an exception if the mode is undefined.
    %
    % A mode is considered fully input if the initial inst is ground.
    %
:- pred mode_is_fully_input(module_info::in, mer_type::in, mer_mode::in)
    is semidet.
:- pred init_inst_is_fully_input(module_info::in, mer_type::in, mer_inst::in)
    is semidet.

    % Succeed iff each mode is fully input, when paired with
    % the corresponding type. Throw an exception if a mode is undefined,
    % or if the two lists have different lengths.
    %
:- pred all_modes_are_fully_input(module_info::in,
    list(mer_type)::in, list(mer_mode)::in) is semidet.

%---------------------%

    % Succeed iff the mode is output.
    % Throw an exception if the mode is undefined.
    %
    % A mode is considered output if the initial inst is free and
    % the final inst is bound.
    %
:- pred mode_is_output(module_info::in, mer_mode::in) is semidet.
:- pred init_final_insts_is_output(module_info::in,
    mer_inst::in, mer_inst::in) is semidet.

    % Succeed iff the mode is fully output.
    % Throw an exception if the mode is undefined.
    %
    % A mode is considered fully output if the initial inst is free
    % and the final inst is ground.
    %
:- pred mode_is_fully_output(module_info::in, mer_type::in,
    mer_mode::in) is semidet.
:- pred init_final_insts_is_fully_output(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::in) is semidet.

    % Succeed iff each mode is fully output, when paired with
    % the corresponding type. Throw an exception if a mode is undefined,
    % or if the two lists have different lengths.
    %
:- pred all_modes_are_fully_output(module_info::in,
    list(mer_type)::in, list(mer_mode)::in) is semidet.

%---------------------%

    % Succeed iff the mode is unused.
    % Throws an exception if the mode is undefined.
    %
    % A mode is considered unused if both the initial and final insts are free.
    %
:- pred mode_is_unused(module_info::in, mer_mode::in) is semidet.
:- pred init_final_insts_is_unused(module_info::in,
    mer_inst::in, mer_inst::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.inst_test.
:- import_module hlds.mode_util.

:- import_module require.

%---------------------------------------------------------------------------%

mode_is_input(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
    inst_is_bound(ModuleInfo, InitialInst).

init_inst_is_input(ModuleInfo, InitialInst) :-
    inst_is_bound(ModuleInfo, InitialInst).

%---------------------%

mode_is_fully_input(ModuleInfo, Type, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
    inst_is_ground(ModuleInfo, Type, InitialInst).

init_inst_is_fully_input(ModuleInfo, Type, InitialInst) :-
    inst_is_ground(ModuleInfo, Type, InitialInst).

all_modes_are_fully_input(_, [], []).
all_modes_are_fully_input(_, [], [_ | _]) :-
    unexpected($pred, "list length mismatch").
all_modes_are_fully_input(_, [_ | _], []) :-
    unexpected($pred, "list length mismatch").
all_modes_are_fully_input(ModuleInfo, [Type | Types], [Mode | Modes]) :-
    mode_is_fully_input(ModuleInfo, Type, Mode),
    all_modes_are_fully_input(ModuleInfo, Types, Modes).

%---------------------%

mode_is_output(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_bound(ModuleInfo, FinalInst).

init_final_insts_is_output(ModuleInfo, InitialInst, FinalInst) :-
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_bound(ModuleInfo, FinalInst).

%---------------------%

mode_is_fully_output(ModuleInfo, Type, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_ground(ModuleInfo, Type, FinalInst).

init_final_insts_is_fully_output(ModuleInfo, Type, InitialInst, FinalInst) :-
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_ground(ModuleInfo, Type, FinalInst).

all_modes_are_fully_output(_, [], []).
all_modes_are_fully_output(_, [], [_ | _]) :-
    unexpected($pred, "list length mismatch").
all_modes_are_fully_output(_, [_ | _], []) :-
    unexpected($pred, "list length mismatch").
all_modes_are_fully_output(ModuleInfo, [Type | Types], [Mode | Modes]) :-
    mode_is_fully_output(ModuleInfo, Type, Mode),
    all_modes_are_fully_output(ModuleInfo, Types, Modes).

%---------------------%

mode_is_unused(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_free(ModuleInfo, FinalInst).

init_final_insts_is_unused(ModuleInfo, InitialInst, FinalInst) :-
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_free(ModuleInfo, FinalInst).

%---------------------------------------------------------------------------%
:- end_module check_hlds.mode_test.
%---------------------------------------------------------------------------%
