%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the bytecode used by the debugger.
%
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module bytecode.

:- interface.

:- import_module hlds_pred, hlds_data, tree.
:- import_module list, io.

:- type byte_tree	==	tree(list(byte_code)).

:- type byte_code	--->	enter_pred(byte_pred_id, string)
			;	endof_pred
			;	enter_proc(byte_proc_id, determinism)
			;	endof_proc
			;	label(byte_label_id)
			;	enter_disjunction(byte_label_id)
			;	endof_disjunction
			;	enter_disjunct(byte_label_id)
			;	endof_disjunct
			;	enter_commit
			;	endof_commit
			;	assign(var, var)
			;	test(var, var)
			;	construct(var, cons_id, tag, list(var))
			;	deconstruct(var, cons_id, tag, list(var))
			;	place_arg(int, var)
			;	call(byte_module_id, byte_pred_id, byte_mode_id)
			;	pickup_arg(int, var)
			;	builtin_binop(binop, var, var, var)
			;	builtin_unop(unop, var, var)
			.

:- type byte_file	--->	byte_file(
					string,		% filename
					list(byte_code)
				).

:- pred debug_byte_file(byte_file::in, io__state::di, io__state::uo) is det.

:- pred output_byte_file(byte_file::in, io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

debug_byte_file(byte_file(BaseName, ByteCodes)) -->
	{ string__append(BaseName, ".debug", FileName) },
	io__tell(FileName, Result),
	(
		{ Result = ok }
	->
		{ library__version(Version) },
		io__write_string(Version),
		io__write_string("\n"),
		debug_byte_list(ByteCodes),
		io__told
	;
		io__progname_base("byte.m", ProgName),
		io__write_string("\n"),
		io__write_string(ProgName),
		io__write_string(": can't open `"),
		io__write_string(FileName),
		io__write_string("' for output\n"),
		io__set_exit_status(1)
	).

:- pred debug_byte_list(list(byte_code), io__state, io__state).
:- mode debug_byte_list(in, di, uo) is det.

output_byte_file(byte_file(BaseName, ByteCodes)) -->
	{ string__append(BaseName, ".byte", FileName) },
	io__tell_binary(FileName, Result),
	(
		{ Result = ok }
	->
		{ library__version(Version) },
		io__write_string(Version),
		io__write_string("\n"),
		output_byte_list(ByteCodes),
		io__told
	;
		io__progname_base("byte.m", ProgName),
		io__write_string("\n"),
		io__write_string(ProgName),
		io__write_string(": can't open `"),
		io__write_string(FileName),
		io__write_string("' for output\n"),
		io__set_exit_status(1)
	).

:- pred output_byte_list(list(byte_code), io__state, io__state).
:- mode output_byte_list(in, di, uo) is det.

output_byte_list([]) --> [].
output_byte_list([ByteCode | ByteCodes]) -->
	byte_code(ByteCode, Byte),
	io__write_byte(Byte),
	output_args(ByteCode),
	io__write_char('\n'),
	output_byte_list(ByteCodes).

:- pred byte_code(byte_code, int).
:- mode byte_code(in, out) is det.

byte_code(enter_pred(_, _),			 0).
byte_code(endof_pred,				 1).
byte_code(enter_proc(_, _),			 2).
byte_code(endof_proc,				 3).
byte_code(label(_),				 4).
byte_code(enter_disjunction(_),			 5).
byte_code(endof_disjunction,			 6).
byte_code(enter_disjunct(_),			 7).
byte_code(endof_disjunct,			 8).
byte_code(enter_commit,				 9).
byte_code(endof_commit,				10).
byte_code(assign(_, _),				11).
byte_code(test(_, _),				12).
byte_code(construct(_, _, _, _),		13).
byte_code(deconstruct(_, _, _, _),		14).
byte_code(place_arg(_, _),			15).
byte_code(call(_, _, _),			16).
byte_code(pickup_arg(_, _),			17).
byte_code(builtin_binop(_, _, _, _),		18).
byte_code(builtin_unop(_, _, _),		19).

:- pred output_args(byte_code, io__state, io__state).
:- mode output_args(in, di, uo) is det.

output_args(enter_pred(PredId, Name)) -->
	output_pred_id(PredId),
	output_string(Name).
output_args(endof_pred) --> [].
output_args(enter_proc(ProcId, Detism)) -->
	output_proc_id(ProcId),
	output_determinism(Detism).
output_args(endof_proc) --> [].
output_args(label(LabelId)) -->
	output_label_id(LabelId).
output_args(enter_disjunction(LabelId)) -->
	output_label_id(LabelId).
output_args(endof_disjunction) --> [].
output_args(enter_disjunct(LabelId)) -->
	output_label_id(LabelId).
output_args(endof_disjunct) --> [].
output_args(enter_commit) --> [].
output_args(endof_commit) --> [].
output_args(assign(Var1, Var2)) -->
	output_var(Var1),
	output_var(Var2).
output_args(test(Var1, Var2)) -->
	output_var(Var1),
	output_var(Var2).
output_args(construct(Var, ConsId, Tag, Vars)) -->
	output_var(Var),
	output_consid(ConsId),
	output_tag(Tag),
	list__length(Vars, Length),
	output_length(Length),
	output_vars(Vars).
output_args(deconstruct(Var, ConsId, Tag, Vars)) -->
	output_var(Var),
	output_consid(ConsId),
	output_tag(Tag),
	list__length(Vars, Length),
	output_length(Length),
	output_vars(Vars).
output_args(place_arg(Arg, Var)) -->
	output_arg_num(Arg),
	output_var(Var).
output_args(call(ModuleId, PredId, ProcId)) -->
	output_module_id(ModuleId),
	output_pred_id(PredId),
	output_proc_id(ProcId).
output_args(pickup_arg(Arg, Var)) -->
	output_arg_num(Arg),
	output_var(Var).
output_args(builtin_binop(Binop, Var1, Var2, Var3)) -->
	output_binop(Binop),
	output_var(Var1),
	output_var(Var2),
	output_var(Var3).
output_args(builtin_unop(Unop, Var1, Var2)) -->
	output_unop(Unop),
	output_var(Var1),
	output_var(Var2).

:- pred output_var(var, io__state, io__state).
:- mode output_var(in, di, uo) is det.

:- pred output_vars(list(var), io__state, io__state).
:- mode output_vars(in, di, uo) is det.

output_vars([]) --> [].
output_vars([Var | Vars]) -->
	output_var(Var),
	output_vars(Vars).

:- pred output_module_id(byte_module_id, io__state, io__state).
:- mode output_module_id(in, di, uo) is det.

:- pred output_pred_id(byte_pred_id, io__state, io__state).
:- mode output_pred_id(in, di, uo) is det.

:- pred output_proc_id(byte_proc_id, io__state, io__state).
:- mode output_proc_id(in, di, uo) is det.

:- pred output_binop(binop, io__state, io__state).
:- mode output_binop(in, di, uo) is det.

:- pred output_unop(unop, io__state, io__state).
:- mode output_unop(in, di, uo) is det.
