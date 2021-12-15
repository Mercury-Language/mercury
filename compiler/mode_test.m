%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
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
:- pred mode_is_fully_input(module_info::in, mer_mode::in) is semidet.
:- pred init_inst_is_fully_input(module_info::in, mer_inst::in) is semidet.

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
:- pred mode_is_fully_output(module_info::in, mer_mode::in) is semidet.
:- pred init_final_insts_is_fully_output(module_info::in,
    mer_inst::in, mer_inst::in) is semidet.

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

:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.

%---------------------------------------------------------------------------%

mode_is_input(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
    inst_is_bound(ModuleInfo, InitialInst).

init_inst_is_input(ModuleInfo, InitialInst) :-
    inst_is_bound(ModuleInfo, InitialInst).

%---------------------%

mode_is_fully_input(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
    inst_is_ground(ModuleInfo, InitialInst).

init_inst_is_fully_input(ModuleInfo, InitialInst) :-
    inst_is_ground(ModuleInfo, InitialInst).

%---------------------%

mode_is_output(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_bound(ModuleInfo, FinalInst).

init_final_insts_is_output(ModuleInfo, InitialInst, FinalInst) :-
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_bound(ModuleInfo, FinalInst).

%---------------------%

mode_is_fully_output(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_ground(ModuleInfo, FinalInst).

init_final_insts_is_fully_output(ModuleInfo, InitialInst, FinalInst) :-
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_ground(ModuleInfo, FinalInst).

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
