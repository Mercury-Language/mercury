%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: llds_to_x86_64_out.m.
% Main author: fhandoko.
%
% This module defines the routines for printing out instructions produced by
% llds->x86_64 asm generator. 
%
%-----------------------------------------------------------------------------%

:- module ll_backend.llds_to_x86_64_out.
:- interface.

:- import_module ll_backend.x86_64_instrs.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred output_x86_64_asm(list(x86_64_procedure)::in, io::di, io::uo) is det. 

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation. 

:- import_module ll_backend.llds_to_x86_64.
:- import_module ll_backend.x86_64_out.

output_x86_64_asm(AsmProcs, !IO) :-
	output_asm_proc_list(AsmProcs, !IO).


:- pred output_asm_proc_list(list(x86_64_procedure)::in, io::di, io::uo) is det.

output_asm_proc_list([], !IO). 
output_asm_proc_list([AsmProc | AsmProcs], !IO) :- 
	output_asm_instr_list(AsmProc ^ x86_64_code, !IO),
	output_asm_proc_list(AsmProcs, !IO).

:- pred output_asm_instr_list(list(x86_64_instruction)::in, io::di, io::uo) 
	is det.

output_asm_instr_list([], !IO).
output_asm_instr_list([AsmInstr | AsmInstrs], !IO) :-
	output_x86_64_instruction(AsmInstr, !IO), 	
	output_asm_instr_list(AsmInstrs, !IO).

%----------------------------------------------------------------------------%
:- end_module llds_to_x86_64_out.
%----------------------------------------------------------------------------%
