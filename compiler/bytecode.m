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

:- import_module hlds_data, prog_data, llds, tree.
:- import_module list, std_util, io.

:- type byte_tree	==	tree(list(byte_code)).

:- type byte_code	--->	enter_pred(byte_pred_id, int, 
					byte_pred_or_func, int)
			;	endof_pred
			;	enter_proc(byte_proc_id, determinism,
					int, int, list(byte_var_info))
			;	endof_proc
			;	label(byte_label_id)
			;	enter_disjunction(byte_label_id)
			;	endof_disjunction
			;	enter_disjunct(byte_label_id)
			;	endof_disjunct(byte_label_id)
			;	enter_switch(byte_var, byte_label_id)
			;	endof_switch
			;	enter_switch_arm(byte_cons_id, byte_label_id)
			;	endof_switch_arm(byte_label_id)
			;	enter_if(byte_label_id, byte_label_id,
					byte_temp)
			;	enter_then(byte_temp)
			;	endof_then(byte_label_id)
			;	endof_if
			;	enter_negation(byte_label_id)
			;	endof_negation
			;	enter_commit(byte_temp)
			;	endof_commit(byte_temp)
			;	assign(byte_var, byte_var)
			;	test(byte_var, byte_var)
			;	construct(byte_var, byte_cons_id,
					list(byte_var))
			;	deconstruct(byte_var, byte_cons_id,
					list(byte_var))
			;	complex_construct(byte_var, byte_cons_id,
					list(pair(byte_var, byte_dir)))
			;	complex_deconstruct(byte_var, byte_cons_id,
					list(pair(byte_var, byte_dir)))
			;	place_arg(reg_type, int, byte_var)
			;	pickup_arg(reg_type, int, byte_var)
			;	call(byte_module_id, byte_pred_id,
					arity, byte_proc_id)
			;	higher_order_call(byte_var, arity, arity,
					determinism)
			;	builtin_binop(binary_op, byte_arg, byte_arg,
					byte_var)
			;	builtin_unop(unary_op, byte_arg, byte_var)
			;	builtin_bintest(binary_op, byte_arg, byte_arg)
			;	builtin_untest(unary_op, byte_arg)
			;	semidet_succeed
			;	semidet_success_check
			;	fail
			;	context(int)
			;	not_supported
			.

:- type byte_cons_id	--->	cons(byte_module_id, string,
					arity, byte_cons_tag)
			;	int_const(int)
			;	string_const(string)
			;	float_const(float)
			;	pred_const(byte_module_id, byte_pred_id,
					arity, byte_proc_id)
			;	code_addr_const(byte_module_id, byte_pred_id,
					arity, byte_proc_id)
			;	base_type_info_const(byte_module_id, string,
					int)
			;	char_const(char)
			.

:- type byte_var_info	--->	var_info(string, type).

:- type byte_cons_tag	--->	no_tag
			;	simple_tag(tag_bits)
			;	complicated_tag(tag_bits, int)
			;	complicated_constant_tag(tag_bits, int)
			;	enum_tag(int)
			.

:- type byte_arg	--->	var(byte_var)
			;	int_const(int)
			;	float_const(float)
			.

:- type byte_dir	--->	to_arg
			;	to_var
			;	to_none
			.

:- type byte_module_id	==	string.
:- type byte_pred_id	==	string.
:- type byte_proc_id	==	int.
:- type byte_label_id	==	int.
:- type byte_var	==	int.
:- type byte_temp	==	int.
:- type byte_pred_or_func ==	int.

:- pred output_bytecode_file(string::in, list(byte_code)::in,
	io__state::di, io__state::uo) is det.

:- pred debug_bytecode_file(string::in, list(byte_code)::in,
	io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, llds_out.
:- import_module char, library, int, string, require.

:- pred bytecode__version(int::out) is det.

bytecode__version(9).

output_bytecode_file(FileName, ByteCodes) -->
	io__tell_binary(FileName, Result),
	(
		{ Result = ok }
	->
		{ bytecode__version(Version) },
		output_short(Version),
		output_bytecode_list(ByteCodes),
		io__told_binary
	;
		io__progname_base("byte.m", ProgName),
		io__write_string("\n"),
		io__write_string(ProgName),
		io__write_string(": can't open `"),
		io__write_string(FileName),
		io__write_string("' for output\n"),
		io__set_exit_status(1)
	).

debug_bytecode_file(FileName, ByteCodes) -->
	io__tell(FileName, Result),
	(
		{ Result = ok }
	->
		{ bytecode__version(Version) },
		io__write_string("bytecode_version "),
		io__write_int(Version),
		io__write_string("\n"),
		debug_bytecode_list(ByteCodes),
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

:- pred output_bytecode_list(list(byte_code), io__state, io__state).
:- mode output_bytecode_list(in, di, uo) is det.

output_bytecode_list([]) --> [].
output_bytecode_list([ByteCode | ByteCodes]) -->
	{ byte_code(ByteCode, Byte) },
	io__write_byte(Byte),
	output_args(ByteCode),
	output_bytecode_list(ByteCodes).

:- pred debug_bytecode_list(list(byte_code), io__state, io__state).
:- mode debug_bytecode_list(in, di, uo) is det.

debug_bytecode_list([]) --> [].
debug_bytecode_list([ByteCode | ByteCodes]) -->
	{ byte_debug(ByteCode, Debug) },
	debug_string(Debug),
	debug_args(ByteCode),
	io__write_char('\n'),
	debug_bytecode_list(ByteCodes).

:- pred output_args(byte_code, io__state, io__state).
:- mode output_args(in, di, uo) is det.

output_args(enter_pred(PredId, PredArity, IsFunc, ProcCount)) -->
	output_pred_id(PredId),
	output_length(PredArity),
	output_byte(IsFunc),
	output_length(ProcCount).
output_args(endof_pred) --> [].
output_args(enter_proc(ProcId, Detism, LabelCount, TempCount, Vars)) -->
	output_proc_id(ProcId),
	output_determinism(Detism),
	output_length(LabelCount),
	output_length(TempCount),
	{ list__length(Vars, VarCount) },
	output_length(VarCount),
	output_var_infos(Vars).
output_args(endof_proc) --> [].
output_args(label(LabelId)) -->
	output_label_id(LabelId).
output_args(enter_disjunction(LabelId)) -->
	output_label_id(LabelId).
output_args(endof_disjunction) --> [].
output_args(enter_disjunct(LabelId)) -->
	output_label_id(LabelId).
output_args(endof_disjunct(LabelId)) -->
	output_label_id(LabelId).
output_args(enter_switch(Var, LabelId)) -->
	output_var(Var),
	output_label_id(LabelId).
output_args(endof_switch) --> [].
output_args(enter_switch_arm(ConsId, LabelId)) -->
	output_cons_id(ConsId),
	output_label_id(LabelId).
output_args(endof_switch_arm(LabelId)) -->
	output_label_id(LabelId).
output_args(enter_if(ElseLabelId, FollowLabelId, FramePtrTemp)) -->
	output_label_id(ElseLabelId),
	output_label_id(FollowLabelId),
	output_temp(FramePtrTemp).
output_args(enter_then(FramePtrTemp)) -->
	output_temp(FramePtrTemp).
output_args(endof_then(FollowLabelId)) -->
	output_label_id(FollowLabelId).
output_args(endof_if) --> [].
output_args(enter_negation(LabelId)) -->
	output_label_id(LabelId).
output_args(endof_negation) --> [].
output_args(enter_commit(Temp)) -->
	output_temp(Temp).
output_args(endof_commit(Temp)) -->
	output_temp(Temp).
output_args(assign(Var1, Var2)) -->
	output_var(Var1),
	output_var(Var2).
output_args(test(Var1, Var2)) -->
	output_var(Var1),
	output_var(Var2).
output_args(construct(Var, ConsId, Vars)) -->
	output_var(Var),
	output_cons_id(ConsId),
	{ list__length(Vars, Length) },
	output_length(Length),
	output_vars(Vars).
output_args(deconstruct(Var, ConsId, Vars)) -->
	output_var(Var),
	output_cons_id(ConsId),
	{ list__length(Vars, Length) },
	output_length(Length),
	output_vars(Vars).
output_args(complex_construct(Var, ConsId, VarDirs)) -->
	output_var(Var),
	output_cons_id(ConsId),
	{ list__length(VarDirs, Length) },
	output_length(Length),
	output_var_dirs(VarDirs).
output_args(complex_deconstruct(Var, ConsId, VarDirs)) -->
	output_var(Var),
	output_cons_id(ConsId),
	{ list__length(VarDirs, Length) },
	output_length(Length),
	output_var_dirs(VarDirs).
output_args(place_arg(RegType, RegNum, Var)) -->
	output_reg(RegType, RegNum),
	output_var(Var).
output_args(pickup_arg(RegType, RegNum, Var)) -->
	output_reg(RegType, RegNum),
	output_var(Var).
output_args(call(ModuleId, PredId, Arity, ProcId)) -->
	output_module_id(ModuleId),
	output_pred_id(PredId),
	output_length(Arity),
	output_proc_id(ProcId).
output_args(higher_order_call(PredVar, InVarCount, OutVarCount, Detism)) -->
	output_var(PredVar),
	output_length(InVarCount),
	output_length(OutVarCount),
	output_determinism(Detism).
output_args(builtin_binop(Binop, Var1, Var2, Var3)) -->
	output_binop(Binop),
	output_arg(Var1),
	output_arg(Var2),
	output_var(Var3).
output_args(builtin_unop(Unop, Var1, Var2)) -->
	output_unop(Unop),
	output_arg(Var1),
	output_var(Var2).
output_args(builtin_bintest(Binop, Var1, Var2)) -->
	output_binop(Binop),
	output_arg(Var1),
	output_arg(Var2).
output_args(builtin_untest(Unop, Var1)) -->
	output_unop(Unop),
	output_arg(Var1).
output_args(semidet_succeed) --> [].
output_args(semidet_success_check) --> [].
output_args(fail) --> [].
output_args(context(Line)) -->
	output_short(Line).
output_args(not_supported) --> [].

:- pred debug_args(byte_code, io__state, io__state).
:- mode debug_args(in, di, uo) is det.

debug_args(enter_pred(PredId, PredArity, IsFunc, ProcsCount)) -->
	debug_pred_id(PredId),
	debug_length(PredArity),
	(
		{ IsFunc = 0 } ->
			debug_string("pred")
		;
			debug_string("func")
	),
	debug_length(ProcsCount).
debug_args(endof_pred) --> [].
debug_args(enter_proc(ProcId, Detism, LabelCount, TempCount, Vars)) -->
	debug_proc_id(ProcId),
	debug_determinism(Detism),
	debug_length(LabelCount),
	debug_length(TempCount),
	{ list__length(Vars, VarCount) },
	debug_length(VarCount),
	debug_var_infos(Vars).
debug_args(endof_proc) --> [].
debug_args(label(LabelId)) -->
	debug_label_id(LabelId).
debug_args(enter_disjunction(LabelId)) -->
	debug_label_id(LabelId).
debug_args(endof_disjunction) --> [].
debug_args(enter_disjunct(LabelId)) -->
	debug_label_id(LabelId).
debug_args(endof_disjunct(LabelId)) -->
	debug_label_id(LabelId).
debug_args(enter_switch(Var, LabelId)) -->
	debug_var(Var),
	debug_label_id(LabelId).
debug_args(endof_switch) --> [].
debug_args(enter_switch_arm(ConsId, LabelId)) -->
	debug_cons_id(ConsId),
	debug_label_id(LabelId).
debug_args(endof_switch_arm(LabelId)) -->
	debug_label_id(LabelId).
debug_args(enter_if(ElseLabelId, FollowLabelId, FramePtrTemp)) -->
	debug_label_id(ElseLabelId),
	debug_label_id(FollowLabelId),
	debug_temp(FramePtrTemp).
debug_args(enter_then(FramePtrTemp)) -->
	debug_temp(FramePtrTemp).
debug_args(endof_then(FollowLabelId)) -->
	debug_label_id(FollowLabelId).
debug_args(endof_if) --> [].
debug_args(enter_negation(LabelId)) -->
	debug_label_id(LabelId).
debug_args(endof_negation) --> [].
debug_args(enter_commit(Temp)) -->
	debug_temp(Temp).
debug_args(endof_commit(Temp)) -->
	debug_temp(Temp).
debug_args(assign(Var1, Var2)) -->
	debug_var(Var1),
	debug_var(Var2).
debug_args(test(Var1, Var2)) -->
	debug_var(Var1),
	debug_var(Var2).
debug_args(construct(Var, ConsId, Vars)) -->
	debug_var(Var),
	debug_cons_id(ConsId),
	{ list__length(Vars, Length) },
	debug_length(Length),
	debug_vars(Vars).
debug_args(deconstruct(Var, ConsId, Vars)) -->
	debug_var(Var),
	debug_cons_id(ConsId),
	{ list__length(Vars, Length) },
	debug_length(Length),
	debug_vars(Vars).
debug_args(complex_construct(Var, ConsId, VarDirs)) -->
	debug_var(Var),
	debug_cons_id(ConsId),
	{ list__length(VarDirs, Length) },
	debug_length(Length),
	debug_var_dirs(VarDirs).
debug_args(complex_deconstruct(Var, ConsId, VarDirs)) -->
	debug_var(Var),
	debug_cons_id(ConsId),
	{ list__length(VarDirs, Length) },
	debug_length(Length),
	debug_var_dirs(VarDirs).
debug_args(place_arg(RegType, RegNum, Var)) -->
	debug_reg(RegType, RegNum),
	debug_var(Var).
debug_args(pickup_arg(RegType, RegNum, Var)) -->
	debug_reg(RegType, RegNum),
	debug_var(Var).
debug_args(call(ModuleId, PredId, Arity, ProcId)) -->
	debug_module_id(ModuleId),
	debug_pred_id(PredId),
	debug_length(Arity),
	debug_proc_id(ProcId).
debug_args(higher_order_call(PredVar, InVarCount, OutVarCount, Detism)) -->
	debug_var(PredVar),
	debug_length(InVarCount),
	debug_length(OutVarCount),
	debug_determinism(Detism).
debug_args(builtin_binop(Binop, Var1, Var2, Var3)) -->
	debug_binop(Binop),
	debug_arg(Var1),
	debug_arg(Var2),
	debug_var(Var3).
debug_args(builtin_unop(Unop, Var1, Var2)) -->
	debug_unop(Unop),
	debug_arg(Var1),
	debug_var(Var2).
debug_args(builtin_bintest(Binop, Var1, Var2)) -->
	debug_binop(Binop),
	debug_arg(Var1),
	debug_arg(Var2).
debug_args(builtin_untest(Unop, Var1)) -->
	debug_unop(Unop),
	debug_arg(Var1).
debug_args(semidet_succeed) --> [].
debug_args(semidet_success_check) --> [].
debug_args(fail) --> [].
debug_args(context(Line)) -->
	debug_int(Line).
debug_args(not_supported) --> [].

%---------------------------------------------------------------------------%

:- pred output_var_infos(list(byte_var_info), io__state, io__state).
:- mode output_var_infos(in, di, uo) is det.

output_var_infos([]) --> [].
output_var_infos([Var | Vars]) -->
	output_var_info(Var),
	output_var_infos(Vars).

:- pred output_var_info(byte_var_info, io__state, io__state).
:- mode output_var_info(in, di, uo) is det.

output_var_info(var_info(Name, _)) -->
	output_string(Name).

:- pred debug_var_infos(list(byte_var_info), io__state, io__state).
:- mode debug_var_infos(in, di, uo) is det.

debug_var_infos([]) --> [].
debug_var_infos([Var | Vars]) -->
	debug_var_info(Var),
	debug_var_infos(Vars).

:- pred debug_var_info(byte_var_info, io__state, io__state).
:- mode debug_var_info(in, di, uo) is det.

debug_var_info(var_info(Name, _)) -->
	debug_string(Name).

%---------------------------------------------------------------------------%

:- pred output_determinism(determinism, io__state, io__state).
:- mode output_determinism(in, di, uo) is det.

output_determinism(Detism) -->
	{ determinism_code(Detism, Code) },
	output_byte(Code).

:- pred debug_determinism(determinism, io__state, io__state).
:- mode debug_determinism(in, di, uo) is det.

debug_determinism(Detism) -->
	{ determinism_debug(Detism, Debug) },
	debug_string(Debug).

%---------------------------------------------------------------------------%

:- pred output_reg(reg_type, int, io__state, io__state).
:- mode output_reg(in, in, di, uo) is det.

output_reg(r, N) -->
	output_byte(N).
output_reg(f, _) -->
	{ error("we do not handle floating point registers yet") }.

:- pred debug_reg(reg_type, int, io__state, io__state).
:- mode debug_reg(in, in, di, uo) is det.

debug_reg(r, N) -->
	debug_int(N).
debug_reg(f, _) -->
	{ error("we do not handle floating point registers yet") }.

%---------------------------------------------------------------------------%

:- pred output_length(int, io__state, io__state).
:- mode output_length(in, di, uo) is det.

output_length(Length) -->
	output_short(Length).

:- pred debug_length(int, io__state, io__state).
:- mode debug_length(in, di, uo) is det.

debug_length(Length) -->
	debug_int(Length).

%---------------------------------------------------------------------------%

:- pred output_arg(byte_arg, io__state, io__state).
:- mode output_arg(in, di, uo) is det.

output_arg(var(Var)) -->
	output_byte(0),
	output_var(Var).
output_arg(int_const(IntVal)) -->
	output_byte(1),
	output_int(IntVal).
output_arg(float_const(FloatVal)) -->
	output_byte(2),
	output_float(FloatVal).

:- pred debug_arg(byte_arg, io__state, io__state).
:- mode debug_arg(in, di, uo) is det.

debug_arg(var(Var)) -->
	debug_string("var"),
	debug_var(Var).
debug_arg(int_const(IntVal)) -->
	debug_string("int"),
	debug_int(IntVal).
debug_arg(float_const(FloatVal)) -->
	debug_string("float"),
	debug_float(FloatVal).

%---------------------------------------------------------------------------%

:- pred output_var(byte_var, io__state, io__state).
:- mode output_var(in, di, uo) is det.

output_var(Var) -->
	output_short(Var).

:- pred output_vars(list(byte_var), io__state, io__state).
:- mode output_vars(in, di, uo) is det.

output_vars([]) --> [].
output_vars([Var | Vars]) -->
	output_var(Var),
	output_vars(Vars).

:- pred debug_var(byte_var, io__state, io__state).
:- mode debug_var(in, di, uo) is det.

debug_var(Var) -->
	debug_int(Var).

:- pred debug_vars(list(byte_var), io__state, io__state).
:- mode debug_vars(in, di, uo) is det.

debug_vars([]) --> [].
debug_vars([Var | Vars]) -->
	debug_var(Var),
	debug_vars(Vars).

%---------------------------------------------------------------------------%

:- pred output_temp(byte_temp, io__state, io__state).
:- mode output_temp(in, di, uo) is det.

output_temp(Var) -->
	output_short(Var).

:- pred debug_temp(byte_temp, io__state, io__state).
:- mode debug_temp(in, di, uo) is det.

debug_temp(Var) -->
	debug_int(Var).

%---------------------------------------------------------------------------%

:- pred output_dir(byte_dir, io__state, io__state).
:- mode output_dir(in, di, uo) is det.

output_dir(to_arg) -->
	output_byte(0).
output_dir(to_var) -->
	output_byte(1).
output_dir(to_none) -->
	output_byte(2).

:- pred output_var_dirs(list(pair(byte_var, byte_dir)), io__state, io__state).
:- mode output_var_dirs(in, di, uo) is det.

output_var_dirs([]) --> [].
output_var_dirs([Var - Dir | VarDirs]) -->
	output_var(Var),
	output_dir(Dir),
	output_var_dirs(VarDirs).

:- pred debug_dir(byte_dir, io__state, io__state).
:- mode debug_dir(in, di, uo) is det.

debug_dir(to_arg) -->
	debug_string("to_arg").
debug_dir(to_var) -->
	debug_string("to_var").
debug_dir(to_none) -->
	debug_string("to_none").

:- pred debug_var_dirs(list(pair(byte_var, byte_dir)), io__state, io__state).
:- mode debug_var_dirs(in, di, uo) is det.

debug_var_dirs([]) --> [].
debug_var_dirs([Var - Dir | VarDirs]) -->
	debug_var(Var),
	debug_dir(Dir),
	debug_var_dirs(VarDirs).

%---------------------------------------------------------------------------%

:- pred output_module_id(byte_module_id, io__state, io__state).
:- mode output_module_id(in, di, uo) is det.

output_module_id(ModuleId) -->
	output_string(ModuleId).

:- pred debug_module_id(byte_module_id, io__state, io__state).
:- mode debug_module_id(in, di, uo) is det.

debug_module_id(ModuleId) -->
	debug_string(ModuleId).

%---------------------------------------------------------------------------%

:- pred output_pred_id(byte_pred_id, io__state, io__state).
:- mode output_pred_id(in, di, uo) is det.

output_pred_id(PredId) -->
	output_string(PredId).

:- pred debug_pred_id(byte_pred_id, io__state, io__state).
:- mode debug_pred_id(in, di, uo) is det.

debug_pred_id(PredId) -->
	debug_string(PredId).

%---------------------------------------------------------------------------%

:- pred output_proc_id(byte_proc_id, io__state, io__state).
:- mode output_proc_id(in, di, uo) is det.

output_proc_id(ProcId) -->
	{ ModeId is ProcId mod 10000 },
	output_byte(ModeId).

:- pred debug_proc_id(byte_proc_id, io__state, io__state).
:- mode debug_proc_id(in, di, uo) is det.

debug_proc_id(ProcId) -->
	{ ModeId is ProcId mod 10000 },
	debug_int(ModeId).

%---------------------------------------------------------------------------%

:- pred output_label_id(int, io__state, io__state).
:- mode output_label_id(in, di, uo) is det.

output_label_id(LabelId) -->
	output_short(LabelId).

:- pred debug_label_id(int, io__state, io__state).
:- mode debug_label_id(in, di, uo) is det.

debug_label_id(LabelId) -->
	debug_int(LabelId).

%---------------------------------------------------------------------------%

:- pred output_cons_id(byte_cons_id, io__state, io__state).
:- mode output_cons_id(in, di, uo) is det.

output_cons_id(cons(ModuleId, Functor, Arity, Tag)) -->
	output_byte(0),
	output_string(ModuleId),
	output_string(Functor),
	output_short(Arity),
	output_tag(Tag).
output_cons_id(int_const(IntVal)) -->
	output_byte(1),
	output_int(IntVal).
output_cons_id(string_const(StringVal)) -->
	output_byte(2),
	output_string(StringVal).
output_cons_id(float_const(FloatVal)) -->
	output_byte(3),
	output_float(FloatVal).
output_cons_id(pred_const(ModuleId, PredId, Arity, ProcId)) -->
	output_byte(4),
	output_module_id(ModuleId),
	output_pred_id(PredId),
	output_length(Arity),
	output_proc_id(ProcId).
output_cons_id(code_addr_const(ModuleId, PredId, Arity, ProcId)) -->
	output_byte(5),
	output_module_id(ModuleId),
	output_pred_id(PredId),
	output_length(Arity),
	output_proc_id(ProcId).
output_cons_id(base_type_info_const(ModuleId, TypeName, TypeArity)) -->
	output_byte(6),
	output_module_id(ModuleId),
	output_string(TypeName),
	output_byte(TypeArity).
output_cons_id(char_const(Char)) -->
	output_byte(7),
	{ char__to_int(Char, Byte) },
	output_byte(Byte).

:- pred debug_cons_id(byte_cons_id, io__state, io__state).
:- mode debug_cons_id(in, di, uo) is det.

debug_cons_id(cons(ModuleId, Functor, Arity, Tag)) -->
	debug_string("functor"),
	debug_string(ModuleId),
	debug_string(Functor),
	debug_int(Arity),
	debug_tag(Tag).
debug_cons_id(int_const(IntVal)) -->
	debug_string("int_const"),
	debug_int(IntVal).
debug_cons_id(string_const(StringVal)) -->
	debug_string("string_const"),
	debug_cstring(StringVal).
debug_cons_id(float_const(FloatVal)) -->
	debug_string("float_const"),
	debug_float(FloatVal).
debug_cons_id(pred_const(ModuleId, PredId, Arity, ProcId)) -->
	debug_string("pred_const"),
	debug_module_id(ModuleId),
	debug_pred_id(PredId),
	debug_length(Arity),
	debug_proc_id(ProcId).
debug_cons_id(code_addr_const(ModuleId, PredId, Arity, ProcId)) -->
	debug_string("code_addr_const"),
	debug_module_id(ModuleId),
	debug_pred_id(PredId),
	debug_length(Arity),
	debug_proc_id(ProcId).
debug_cons_id(base_type_info_const(ModuleId, TypeName, TypeArity)) -->
	debug_string("base_type_info_const"),
	debug_module_id(ModuleId),
	debug_string(TypeName),
	debug_int(TypeArity).
debug_cons_id(char_const(Char)) -->
	debug_string("char_const"),
	{ string__from_char_list([Char], String) },
	debug_string(String).

%---------------------------------------------------------------------------%

:- pred output_tag(byte_cons_tag, io__state, io__state).
:- mode output_tag(in, di, uo) is det.

output_tag(simple_tag(Primary)) -->
	output_byte(0),
	output_byte(Primary).
output_tag(complicated_tag(Primary, Secondary)) -->
	output_byte(1),
	output_byte(Primary),
	output_int(Secondary).
output_tag(complicated_constant_tag(Primary, Secondary)) -->
	output_byte(2),
	output_byte(Primary),
	output_int(Secondary).
output_tag(enum_tag(Enum)) -->
	output_byte(3),
	output_byte(Enum).
output_tag(no_tag) -->
	output_byte(4).

:- pred debug_tag(byte_cons_tag, io__state, io__state).
:- mode debug_tag(in, di, uo) is det.

debug_tag(simple_tag(Primary)) -->
	debug_string("simple_tag"),
	debug_int(Primary).
debug_tag(complicated_tag(Primary, Secondary)) -->
	debug_string("complicated_tag"),
	debug_int(Primary),
	debug_int(Secondary).
debug_tag(complicated_constant_tag(Primary, Secondary)) -->
	debug_string("complicated_constant_tag"),
	debug_int(Primary),
	debug_int(Secondary).
debug_tag(enum_tag(Enum)) -->
	debug_string("enum_tag"),
	debug_int(Enum).
debug_tag(no_tag) -->
	debug_string("no_tag").

%---------------------------------------------------------------------------%

:- pred output_binop(binary_op, io__state, io__state).
:- mode output_binop(in, di, uo) is det.

output_binop(Binop) -->
	{ binop_code(Binop, Code) },
	output_byte(Code).

:- pred debug_binop(binary_op, io__state, io__state).
:- mode debug_binop(in, di, uo) is det.

debug_binop(Binop) -->
	{ binop_debug(Binop, Debug) },
	debug_string(Debug).

%---------------------------------------------------------------------------%

:- pred output_unop(unary_op, io__state, io__state).
:- mode output_unop(in, di, uo) is det.

output_unop(Unop) -->
	{ unop_code(Unop, Code) },
	output_byte(Code).

:- pred debug_unop(unary_op, io__state, io__state).
:- mode debug_unop(in, di, uo) is det.

debug_unop(Unop) -->
	{ unop_debug(Unop, Debug) },
	debug_string(Debug).

%---------------------------------------------------------------------------%

:- pred byte_code(byte_code, int).
:- mode byte_code(in, out) is det.

byte_code(enter_pred(_, _, _, _),		 0).
byte_code(endof_pred,				 1).
byte_code(enter_proc(_, _, _, _, _),		 2).
byte_code(endof_proc,				 3).
byte_code(label(_),				 4).
byte_code(enter_disjunction(_),			 5).
byte_code(endof_disjunction,			 6).
byte_code(enter_disjunct(_),			 7).
byte_code(endof_disjunct(_),			 8).
byte_code(enter_switch(_, _),			 9).
byte_code(endof_switch,				10).
byte_code(enter_switch_arm(_, _),		11).
byte_code(endof_switch_arm(_),			12).
byte_code(enter_if(_, _, _),			13).
byte_code(enter_then(_),			14).
byte_code(endof_then(_),			15).
byte_code(endof_if,				16).
byte_code(enter_negation(_),			17).
byte_code(endof_negation,			18).
byte_code(enter_commit(_),			19).
byte_code(endof_commit(_),			20).
byte_code(assign(_, _),				21).
byte_code(test(_, _),				22).
byte_code(construct(_, _, _),			23).
byte_code(deconstruct(_, _, _),			24).
byte_code(complex_construct(_, _, _),		25).
byte_code(complex_deconstruct(_, _, _),		26).
byte_code(place_arg(_, _, _),			27).
byte_code(pickup_arg(_, _, _),			28).
byte_code(call(_, _, _, _),			29).
byte_code(higher_order_call(_, _, _, _),	30).
byte_code(builtin_binop(_, _, _, _),		31).
byte_code(builtin_unop(_, _, _),		32).
byte_code(builtin_bintest(_, _, _),		33).
byte_code(builtin_untest(_, _),			34).
byte_code(semidet_succeed,			35).
byte_code(semidet_success_check,		36).
byte_code(fail,					37).
byte_code(context(_),				38).
byte_code(not_supported,			39).

:- pred byte_debug(byte_code, string).
:- mode byte_debug(in, out) is det.

byte_debug(enter_pred(_, _, _, _),		"enter_pred").
byte_debug(endof_pred,				"endof_pred").
byte_debug(enter_proc(_, _, _, _, _),		"enter_proc").
byte_debug(endof_proc,				"endof_proc").
byte_debug(label(_),				"label").
byte_debug(enter_disjunction(_),		"enter_disjunction").
byte_debug(endof_disjunction,			"endof_disjunction").
byte_debug(enter_disjunct(_),			"enter_disjunct").
byte_debug(endof_disjunct(_),			"endof_disjunct").
byte_debug(enter_switch(_, _),			"enter_switch").
byte_debug(endof_switch,			"endof_switch").
byte_debug(enter_switch_arm(_, _),		"enter_switch_arm").
byte_debug(endof_switch_arm(_),			"endof_switch_arm").
byte_debug(enter_if(_, _, _),			"enter_if").
byte_debug(enter_then(_),			"enter_then").
byte_debug(endof_then(_),			"endof_then").
byte_debug(endof_if,				"endof_if").
byte_debug(enter_negation(_),			"enter_negation").
byte_debug(endof_negation,			"endof_negation").
byte_debug(enter_commit(_),			"enter_commit").
byte_debug(endof_commit(_),			"endof_commit").
byte_debug(assign(_, _),			"assign").
byte_debug(test(_, _),				"test").
byte_debug(construct(_, _, _),			"construct").
byte_debug(deconstruct(_, _, _),		"deconstruct").
byte_debug(complex_construct(_, _, _),		"complex_construct").
byte_debug(complex_deconstruct(_, _, _),	"complex_deconstruct").
byte_debug(place_arg(_, _, _),			"place_arg").
byte_debug(pickup_arg(_, _, _),			"pickup_arg").
byte_debug(call(_, _, _, _),			"call").
byte_debug(higher_order_call(_, _, _, _),	"higher_order_call").
byte_debug(builtin_binop(_, _, _, _),		"builtin_binop").
byte_debug(builtin_unop(_, _, _),		"builtin_unop").
byte_debug(builtin_bintest(_, _, _),		"builtin_bintest").
byte_debug(builtin_untest(_, _),		"builtin_untest").
byte_debug(semidet_succeed,			"semidet_succeed").
byte_debug(semidet_success_check,		"semidet_success_check").
byte_debug(fail,				"fail").
byte_debug(context(_),				"context").
byte_debug(not_supported,			"not_supported").

:- pred determinism_code(determinism, int).
:- mode determinism_code(in, out) is det.

determinism_code(det,		0).
determinism_code(semidet,	1).
determinism_code(multidet,	2).
determinism_code(nondet,	3).
determinism_code(cc_multidet,	4).
determinism_code(cc_nondet,	5).
determinism_code(erroneous,	6).
determinism_code(failure,	7).

:- pred determinism_debug(determinism, string).
:- mode determinism_debug(in, out) is det.

determinism_debug(det,		"det").
determinism_debug(semidet,	"semidet").
determinism_debug(multidet,	"multidet").
determinism_debug(nondet,	"nondet").
determinism_debug(cc_multidet,	"cc_multidet").
determinism_debug(cc_nondet,	"cc_nondet").
determinism_debug(erroneous,	"erroneous").
determinism_debug(failure,	"failure").

:- pred binop_code(binary_op, int).
:- mode binop_code(in, out) is det.
:- mode binop_code(out, in) is semidet.	% enforce non-duplication of bytecodes

binop_code((+),			 0).
binop_code((-),			 1).
binop_code((*),			 2).
binop_code((/),			 3).
binop_code((mod),		 4).
binop_code((<<),		 5).
binop_code((>>),		 6).
binop_code((&),			 7).
binop_code(('|'),		 8).
binop_code((^),			 9).
binop_code((and),		10).
binop_code((or),		11).
binop_code(eq,			12).
binop_code(ne,			13).
binop_code(array_index,		14).
binop_code(str_eq,		15).
binop_code(str_ne,		16).
binop_code(str_lt,		17).
binop_code(str_gt,		18).
binop_code(str_le,		19).
binop_code(str_ge,		20).
binop_code((<),			21).
binop_code((>),			22).
binop_code((<=),		23).
binop_code((>=),		24).
binop_code(float_plus,		25).
binop_code(float_minus,		26).
binop_code(float_times,		27).
binop_code(float_divide,	28).
binop_code(float_eq,		29).
binop_code(float_ne,		30).
binop_code(float_lt,		31).
binop_code(float_gt,		32).
binop_code(float_le,		33).
binop_code(float_ge,		34).

:- pred binop_debug(binary_op, string).
:- mode binop_debug(in, out) is det.

binop_debug((+),		"+").
binop_debug((-),		"-").
binop_debug((*),		"*").
binop_debug((/),		"/").
binop_debug((mod),		"mod").
binop_debug((<<),		"<<").
binop_debug((>>),		">>").
binop_debug((&),		"&").
binop_debug(('|'),		"|").
binop_debug((^),		"^").
binop_debug((and),		"and").
binop_debug((or),		"or").
binop_debug(eq,			"eq").
binop_debug(ne,			"ne").
binop_debug(array_index,	"array_index").
binop_debug(str_eq,		"str_eq").
binop_debug(str_ne,		"str_ne").
binop_debug(str_lt,		"str_lt").
binop_debug(str_gt,		"str_gt").
binop_debug(str_le,		"str_le").
binop_debug(str_ge,		"str_ge").
binop_debug((<),		"<").
binop_debug((>),		">").
binop_debug((<=),		"<=").
binop_debug((>=),		">=").
binop_debug(float_plus,		"float_plus").
binop_debug(float_minus,	"float_minus").
binop_debug(float_times,	"float_times").
binop_debug(float_divide,	"float_divide").
binop_debug(float_eq,		"float_eq").
binop_debug(float_ne,		"float_ne").
binop_debug(float_lt,		"float_lt").
binop_debug(float_gt,		"float_gt").
binop_debug(float_le,		"float_le").
binop_debug(float_ge,		"float_ge").

:- pred unop_code(unary_op, int).
:- mode unop_code(in, out) is det.

unop_code(mktag,		 0).
unop_code(tag,			 1).
unop_code(unmktag,		 2).
unop_code(mkbody,		 3).
unop_code(body,			 4).
unop_code(unmkbody,		 5).
unop_code(cast_to_unsigned,	 6).
unop_code(hash_string,		 7).
unop_code(bitwise_complement,	 8).
unop_code((not),		 9).

:- pred unop_debug(unary_op, string).
:- mode unop_debug(in, out) is det.

unop_debug(mktag,		"mktag").
unop_debug(tag,			"tag").
unop_debug(unmktag,		"unmktag").
unop_debug(mkbody,		"mkbody").
unop_debug(body,		"body").
unop_debug(unmkbody,		"unmkbody").
unop_debug(cast_to_unsigned,	"cast_to_unsigned").
unop_debug(hash_string,		"has_string").
unop_debug(bitwise_complement,	"bitwise_complement").
unop_debug((not),		"not").

%---------------------------------------------------------------------------%

:- pred output_string(string, io__state, io__state).
:- mode output_string(in, di, uo) is det.

output_string(Val) -->
	io__write_bytes(Val),
	io__write_byte(0).


/*
**	debug_cstring prints a string quoted in the manner of C.
*/

:- pred debug_cstring(string, io__state, io__state).
:- mode debug_cstring(in, di, uo) is det.

debug_cstring(Str) -->
	io__write_char('"'),
	output_c_quoted_string(Str),
	% XXX: We need the trailing space in case something follows
	% the string as a bytecode argument. This is not very elegant.
	io__write_char('"'),
	io__write_char(' ').
	

:- pred output_byte(int, io__state, io__state).
:- mode output_byte(in, di, uo) is det.

output_byte(Val) -->
	( { Val < 256 } ->
		io__write_byte(Val)
	;
		{ error("byte does not fit in eight bits") }
	).

/*
** Spit out a `short' in a portable format.
** This format is: big-endian, 16-bit, 2's-complement.
**
** NOTE: We -assume- the machine architecture uses 2's-complement.
*/
:- pred output_short(int, io__state, io__state).
:- mode output_short(in, di, uo) is det.

output_short(Val) -->
	{ Val1 is Val >> 8 },
	{ Val2 is Val mod 256 },
	( { Val1 < 256 } ->
		io__write_byte(Val1),
		io__write_byte(Val2)
	;
		{ error("small integer does not fit in sixteen bits") }
	).

/*
** Spit out an `int' in a portable `highest common denominator' format.
** This format is: big-endian, 64-bit, 2's-complement int.
**
** NOTE: We -assume- the machine architecture uses 2's-complement.
*/
:- pred output_int(int, io__state, io__state).
:- mode output_int(in, di, uo) is det.

output_int(IntVal) -->
	{ int__bits_per_int(IntBits) },
	( { IntBits > bytecode_int_bits } ->
		{ error("size of int is larger than size of bytecode integer.")}
	;
		{ ZeroPadBytes is (bytecode_int_bits - IntBits) // 
			bits_per_byte },
		output_padding_zeros(ZeroPadBytes),
		{ FirstByteToDump is bytecode_int_bytes - ZeroPadBytes - 1 },
		output_int_bytes(FirstByteToDump, IntVal)
	).

:- func bytecode_int_bits = int.
:- mode bytecode_int_bits = out is det.

bytecode_int_bits = bits_per_byte * bytecode_int_bytes.

:- func bytecode_int_bytes = int.
:- mode bytecode_int_bytes = out is det.

bytecode_int_bytes = 8.

:- func bits_per_byte = int.
:- mode bits_per_byte = out is det.

bits_per_byte = 8.

:- pred output_padding_zeros(int, io__state, io__state).
:- mode output_padding_zeros(in, di, uo) is det.

output_padding_zeros(NumBytes) -->
	( { NumBytes > 0 } ->
		io__write_byte(0),
		{ NumBytes1 is NumBytes - 1 },
		output_padding_zeros(NumBytes1)
	;
		{ true }
	).

:- pred output_int_bytes(int, int, io__state, io__state).
:- mode output_int_bytes(in, in, di, uo) is det.

output_int_bytes(ByteNum, IntVal) -->
	( { ByteNum >= 0 } ->
		{ BitShifts is ByteNum * bits_per_byte },
		{ Byte is (IntVal >> BitShifts) mod (1 << bits_per_byte) },
		{ ByteNum1 is ByteNum - 1 },
		io__write_byte(Byte),
		output_int_bytes(ByteNum1, IntVal)
	;
		{ true }
	).

/*
** Spit out a `float' in a portable `highest common denominator format.
** This format is: big-endian, 64-bit, IEEE-754 floating point value.
**
** NOTE: We -assume- the machine architecture uses IEEE-754.
*/
:- pred output_float(float, io__state, io__state).
:- mode output_float(in, di, uo) is det.

output_float(Val) -->
	{ float_to_float64_bytes(Val, B0, B1, B2, B3, B4, B5, B6, B7) },
	output_byte(B0),
	output_byte(B1),
	output_byte(B2),
	output_byte(B3),
	output_byte(B4),
	output_byte(B5),
	output_byte(B6),
	output_byte(B7).

/*
** Convert a `float' to the representation used in the bytecode.
** That is, a sequence of eight bytes.
*/
:- pred float_to_float64_bytes(float::in, 
		int::out, int::out, int::out, int::out, 
		int::out, int::out, int::out, int::out) is det.
:- pragma(c_code, 
	float_to_float64_bytes(FloatVal::in, B0::out, B1::out, B2::out, B3::out,
		B4::out, B5::out, B6::out, B7::out),
	"

	{
		Float64		float64;
		unsigned char	*raw_mem_p;

		float64 = (Float64) FloatVal;
		raw_mem_p = (unsigned char*) &float64;

		#if defined(MR_BIG_ENDIAN)
			B0 = raw_mem_p[0];
			B1 = raw_mem_p[1];
			B2 = raw_mem_p[2];
			B3 = raw_mem_p[3];
			B4 = raw_mem_p[4];
			B5 = raw_mem_p[5];
			B6 = raw_mem_p[6];
			B7 = raw_mem_p[7];
		#elif defined(MR_LITTLE_ENDIAN)
			B7 = raw_mem_p[0];
			B6 = raw_mem_p[1];
			B5 = raw_mem_p[2];
			B4 = raw_mem_p[3];
			B3 = raw_mem_p[4];
			B2 = raw_mem_p[5];
			B1 = raw_mem_p[6];
			B0 = raw_mem_p[7];
		#else
			#error	Weird-endian architecture
		#endif
	}
	
	"
).

%---------------------------------------------------------------------------%

:- pred debug_string(string, io__state, io__state).
:- mode debug_string(in, di, uo) is det.

debug_string(Val) -->
	io__write_string(Val),
	io__write_char(' ').

:- pred debug_int(int, io__state, io__state).
:- mode debug_int(in, di, uo) is det.

debug_int(Val) -->
	io__write_int(Val),
	io__write_char(' ').

:- pred debug_float(float, io__state, io__state).
:- mode debug_float(in, di, uo) is det.

debug_float(Val) -->
	io__write_float(Val),
	io__write_char(' ').

%---------------------------------------------------------------------------%
