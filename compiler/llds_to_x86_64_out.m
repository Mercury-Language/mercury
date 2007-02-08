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

:- pred output_x86_64_asm(io.output_stream::in, list(x86_64_procedure)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation. 

:- import_module ll_backend.llds_to_x86_64.
:- import_module ll_backend.x86_64_out.

%-----------------------------------------------------------------------------%

output_x86_64_asm(Stream, AsmProcs, !IO) :-
    output_asm_proc_list(Stream, AsmProcs, !IO).

:- pred output_asm_proc_list(io.output_stream::in,
    list(x86_64_procedure)::in, io::di, io::uo) is det.

output_asm_proc_list(_, [], !IO). 
output_asm_proc_list(Stream, [AsmProc | AsmProcs], !IO) :- 
    output_asm_instr_list(Stream, AsmProc ^ x86_64_code, !IO),
    output_asm_proc_list(Stream, AsmProcs, !IO).

:- pred output_asm_instr_list(io.output_stream::in,
    list(x86_64_instruction)::in, io::di, io::uo) is det.

output_asm_instr_list(_, [], !IO).
output_asm_instr_list(Stream, [AsmInstr | AsmInstrs], !IO) :-
    output_x86_64_instruction(Stream, AsmInstr, !IO),   
    output_asm_instr_list(Stream, AsmInstrs, !IO).

%----------------------------------------------------------------------------%
:- end_module llds_to_x86_64_out.
%----------------------------------------------------------------------------%
