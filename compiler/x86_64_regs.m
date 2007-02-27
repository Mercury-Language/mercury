%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: x86_64_regs.m.
% Main author: fhandoko.
%
% This module defines a mapping of llds l-values in MVM registers to x86_64 
% registers. 
%
% NOTE:
%   The first field of reg_map type is a list of available scratch registers. 
%   Although the reg_map is updated everytime a scratch register is used in an 
%   instruction (updated here means that the new reg_map is equivalent to the 
%   old reg_map with the first element (the used scratch register) being 
%   removed), it does not seem to work well. It seems to always get the first
%   scratch register. So, there are instructions in which the next instruction
%   overrides the value in a scratch register being used by a previous 
%   instruction. XXX
%-----------------------------------------------------------------------------%

:- module ll_backend.x86_64_regs.
:- interface.

:- import_module ll_backend.llds.
:- import_module ll_backend.x86_64_instrs.

:- import_module assoc_list.

%----------------------------------------------------------------------------%

    % This type stores information about the mapping from MVM registers
    % to x86_64 registers.  MVM registers will correspond to either
    % (1) a physical x86_64 register
    % or (2) a slot in the fake_reg array (see runtime/mercury_engine.h).
    % 
:- type reg_map.

    % This type represents the location of an MVM register on x86_64
    % hardware.  `actual/1' is a physical x86_64 register.  `virtual/1'
    % is the slot in the fake reg array given by the argument.
    %
:- type reg_locn
	--->	actual(x86_64_reg)
	;		virtual(int).		% Index into fake reg array.

    % Create an association list of lvals and reg_lcons. This is identical to 
    % the one defined in runtime/machdeps/x86_64_regs.h.
    %
:- pred default_x86_64_reg_mapping(assoc_list(llds.lval, reg_locn)::out) is det. 

    % Create a reg_map given an association list of lvals and reg_locns.
    % Throws an exception if an l-value in the association list does not
    % correspond to a MVM register.
    %
:- func reg_map_init(assoc_list(llds.lval, reg_locn)) = reg_map.

    % Given an LLDS lval that corresponds to a virtual machine register
    % look up it's actual location according to the current register mapping.
    % Throws an exception for l-values that do not correspond to virtual
    % machine registers.
    % 
:- func reg_map_lookup_reg_locn(reg_map, llds.lval) = reg_locn.

    % Reset the contents of scratch registers. As a result, all scratch 
    % registers will be available. 
    %
:- pred reg_map_reset_scratch_reg_info(reg_map::in, reg_map::out) is det.

    % Given a reg_map, get the first available scratch register.
    %
:- func reg_map_get_scratch_reg(reg_map) = x86_64_reg.

    % Get an x86_64_register. 
    %
:- func get_scratch_reg = x86_64_reg.

    % Remove the first index of scratch register list (which is the first
    % field of reg_map).
    %
:- pred reg_map_remove_scratch_reg(reg_map::in, reg_map::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.

:- import_module bool.
:- import_module list. 
:- import_module map.
:- import_module pair. 
:- import_module string. 

:- import_module io.

%----------------------------------------------------------------------------%
% 
% LLDS -> x86_64 register mapping. 
%

:- type reg_map
	--->	reg_map(
				scratch_reg_info        :: list(x86_64_reg), 
                % A list of unused scratch registers.
				map(llds.lval, reg_locn)
			    % Mapping lval to an actual or virtual register. 
            ).

%----------------------------------------------------------------------------%

default_x86_64_reg_mapping([
        sp              - actual(r12),       % The top of det stack frame. 
        succip          - actual(r13),
        reg(reg_r, 1)   - actual(r14),
        reg(reg_r, 2)   - actual(r15),
        reg(reg_r, 3)   - virtual(4),   
        hp              - virtual(5),
        reg(reg_r, 4)   - virtual(6),
        reg(reg_r, 5)   - virtual(7),
        curfr           - virtual(8),
        maxfr           - virtual(9),
        reg(reg_r, 6)   - virtual(10),
        reg(reg_r, 7)   - virtual(11),
        reg(reg_r, 8)   - virtual(12),
        reg(reg_r, 9)   - virtual(13),
        reg(reg_r, 10)  - virtual(14),
        reg(reg_r, 11)  - virtual(15),
        reg(reg_r, 12)  - virtual(16),
        reg(reg_r, 13)  - virtual(17),
        reg(reg_r, 14)  - virtual(18),
        reg(reg_r, 15)  - virtual(19),
        reg(reg_r, 16)  - virtual(20),
        reg(reg_r, 17)  - virtual(21),
        reg(reg_r, 18)  - virtual(22),
        reg(reg_r, 19)  - virtual(23),
        reg(reg_r, 20)  - virtual(24),
        reg(reg_r, 21)  - virtual(25),
        reg(reg_r, 22)  - virtual(26),
        reg(reg_r, 23)  - virtual(27),
        reg(reg_r, 24)  - virtual(28),
        reg(reg_r, 25)  - virtual(29),
        reg(reg_r, 26)  - virtual(30),
        reg(reg_r, 27)  - virtual(31),
        reg(reg_r, 28)  - virtual(32),
        reg(reg_r, 29)  - virtual(33),
        reg(reg_r, 30)  - virtual(34),
        reg(reg_r, 31)  - virtual(35),
        reg(reg_r, 32)  - virtual(36)
    ]).

reg_map_init(AssocRegMap) = RegMap :-
    map.init(Map0),
    assoc_list.keys(AssocRegMap, ListOfKeys),
    check_if_all_mvm_registers(ListOfKeys, Result),
    (
        Result = yes,
        map.det_insert_from_assoc_list(Map0, AssocRegMap, Map1),
        RegMap = reg_map(init_scratch_regs, Map1)
    ;
        Result = no,
        unexpected(this_file, "reg_map_init: unexpected: non-MVM register"
            ++ " found in the association list")
    ).

reg_map_lookup_reg_locn(Map, Lval) = RegLocn :-
    Map = reg_map(_, RegMap),
    (
        ( Lval = parent_stackvar(_)
        ; Lval = succip_slot(_)
        ; Lval = redoip_slot(_)
        ; Lval = redofr_slot(_)
        ; Lval = succfr_slot(_)
        ; Lval = prevfr_slot(_)
        ; Lval = mem_ref(_)
        ; Lval = global_var_ref(_)
        )
    ->
        unexpected(this_file, "reg_map_lookup_reg_locn: unexpected: " 
            ++ "lval is not a virtual machine register")
    ;
        Lval = lvar(_)
    ->
        unexpected(this_file, "reg_map_lookup_reg_locn: unexpected: "
            ++ "lvar/1 during x86_64 code generation")
    ;
        map.lookup(RegMap, Lval, RegLocn)
    ).

reg_map_reset_scratch_reg_info(RegMap0, RegMap) :-
    ScratchRegs = init_scratch_regs,
    RegMap = RegMap0 ^ scratch_reg_info := ScratchRegs.

reg_map_get_scratch_reg(RegMap) = ScratchReg :-
    ScratchRegs = RegMap ^ scratch_reg_info,
    ( list.index0(ScratchRegs, first_list_idx, ScratchReg0) ->
        ScratchReg = ScratchReg0
    ;
        unexpected(this_file, "reg_map_get_scratch_reg: unexpected:" 
           ++ " scratch registers exhausted")
    ).

reg_map_remove_scratch_reg(RegMap0, RegMap) :-
    ScratchRegs0 = RegMap0 ^ scratch_reg_info,
    ( list.drop(first_list_idx, ScratchRegs0, ScratchRegs1) ->
        RegMap = RegMap0 ^ scratch_reg_info := ScratchRegs1
    ;
        unexpected(this_file, "reg_map_remove_scratch_reg: unexpected:" 
           ++ " scratch registers exhausted")
    ).

    % Check if all l-values in the association list correspond to MVM 
    % registers.
    %
:- pred check_if_all_mvm_registers(list(llds.lval)::in, bool::out) is det. 

check_if_all_mvm_registers([], yes).
check_if_all_mvm_registers([Lval | Lvals], Result) :-
    ( 
        ( Lval = reg(reg_r, _)
        ; Lval = succip
        ; Lval = maxfr
        ; Lval = curfr
        ; Lval = hp
        ; Lval = sp
        )
    ->
        check_if_all_mvm_registers(Lvals, Result)
    ;
        Result = no
    ).

    % Initialize scratch registers to be used in an instruction. 
    %
:- func init_scratch_regs = list(x86_64_reg).

init_scratch_regs = [r9, r10, r11].

get_scratch_reg = r9.

    % Returns the index of the first element in the list. 
    %
:- func first_list_idx = int.

first_list_idx = 0.

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "x86_64_regs.m".

%----------------------------------------------------------------------------%
:- end_module ll_backend.x86_64_regs.
%----------------------------------------------------------------------------%
