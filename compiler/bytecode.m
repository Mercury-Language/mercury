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

:- import_module hlds_pred, hlds_data, llds, tree.
:- import_module list, io.

:- type byte_tree	==	tree(list(byte_code)).

:- type byte_code	--->	enter_pred(byte_pred_id, int)
			;	endof_pred
			;	enter_proc(byte_proc_id, determinism)
			;	endof_proc
			;	label(byte_label_id)
			;	enter_disjunction(byte_label_id)
			;	endof_disjunction
			;	enter_disjunct(byte_label_id)
			;	endof_disjunct
			;	enter_switch(byte_var, byte_label_id)
			;	endof_switch
			;	enter_switch_arm(cons_id, tag, byte_label_id)
			;	endof_switch_arm
			;	enter_negation(byte_label_id)
			;	endof_negation
			;	enter_commit
			;	endof_commit
			;	assign(byte_var, byte_var)
			;	test(byte_var, byte_var)
			;	construct(byte_var, cons_id, tag,
					list(byte_var))
			;	deconstruct(byte_var, cons_id, tag,
					list(byte_var))
			;	place_arg(reg, byte_var)
			;	call(byte_module_id, byte_pred_id, arity,
					byte_proc_id)
			;	pickup_arg(reg, byte_var)
			;	builtin_binop(binary_op, byte_var, byte_var,
					byte_var)
			;	builtin_unop(unary_op, byte_var, byte_var)
			;	pragma_c_code
			.

:- type byte_module_id	==	string.
:- type byte_pred_id	==	string.
:- type byte_proc_id	==	int.
:- type byte_label_id	==	int.
:- type byte_var	==	int.

:- pred output_bytecode_file(string::in, list(byte_code)::in,
	io__state::di, io__state::uo) is det.

:- pred debug_bytecode_file(string::in, list(byte_code)::in,
	io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module library, int, require.

output_bytecode_file(FileName, ByteCodes) -->
	io__tell_binary(FileName, Result),
	(
		{ Result = ok }
	->
		{ library__version(Version) },
		io__write_string(Version),
		io__write_string("\n"),
		output_bytecode_list(ByteCodes),
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

debug_bytecode_file(FileName, ByteCodes) -->
	io__tell(FileName, Result),
	(
		{ Result = ok }
	->
		{ library__version(Version) },
		io__write_string(Version),
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
	io__write_char('\n'),
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

output_args(enter_pred(PredId, ProcCount)) -->
	output_pred_id(PredId),
	output_length(ProcCount).
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
output_args(enter_switch(Var, LabelId)) -->
	output_var(Var),
	output_label_id(LabelId).
output_args(endof_switch) --> [].
output_args(enter_switch_arm(ConsId, Tag, LabelId)) -->
	output_cons_id(ConsId),
	output_tag(Tag),
	output_label_id(LabelId).
output_args(endof_switch_arm) --> [].
output_args(enter_negation(LabelId)) -->
	output_label_id(LabelId).
output_args(endof_negation) --> [].
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
	output_cons_id(ConsId),
	output_tag(Tag),
	{ list__length(Vars, Length) },
	output_length(Length),
	output_vars(Vars).
output_args(deconstruct(Var, ConsId, Tag, Vars)) -->
	output_var(Var),
	output_cons_id(ConsId),
	output_tag(Tag),
	{ list__length(Vars, Length) },
	output_length(Length),
	output_vars(Vars).
output_args(place_arg(Reg, Var)) -->
	output_reg(Reg),
	output_var(Var).
output_args(call(ModuleId, PredId, Arity, ProcId)) -->
	output_module_id(ModuleId),
	output_pred_id(PredId),
	output_length(Arity),
	output_proc_id(ProcId).
output_args(pickup_arg(Reg, Var)) -->
	output_reg(Reg),
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
output_args(pragma_c_code) --> [].

:- pred debug_args(byte_code, io__state, io__state).
:- mode debug_args(in, di, uo) is det.

debug_args(enter_pred(PredId, ProcsCount)) -->
	debug_pred_id(PredId),
	debug_length(ProcsCount).
debug_args(endof_pred) --> [].
debug_args(enter_proc(ProcId, Detism)) -->
	debug_proc_id(ProcId),
	debug_determinism(Detism).
debug_args(endof_proc) --> [].
debug_args(label(LabelId)) -->
	debug_label_id(LabelId).
debug_args(enter_disjunction(LabelId)) -->
	debug_label_id(LabelId).
debug_args(endof_disjunction) --> [].
debug_args(enter_disjunct(LabelId)) -->
	debug_label_id(LabelId).
debug_args(endof_disjunct) --> [].
debug_args(enter_switch(Var, LabelId)) -->
	debug_var(Var),
	debug_label_id(LabelId).
debug_args(endof_switch) --> [].
debug_args(enter_switch_arm(ConsId, Tag, LabelId)) -->
	debug_cons_id(ConsId),
	debug_tag(Tag),
	debug_label_id(LabelId).
debug_args(endof_switch_arm) --> [].
debug_args(enter_negation(LabelId)) -->
	debug_label_id(LabelId).
debug_args(endof_negation) --> [].
debug_args(enter_commit) --> [].
debug_args(endof_commit) --> [].
debug_args(assign(Var1, Var2)) -->
	debug_var(Var1),
	debug_var(Var2).
debug_args(test(Var1, Var2)) -->
	debug_var(Var1),
	debug_var(Var2).
debug_args(construct(Var, ConsId, Tag, Vars)) -->
	debug_var(Var),
	debug_cons_id(ConsId),
	debug_tag(Tag),
	{ list__length(Vars, Length) },
	debug_length(Length),
	debug_vars(Vars).
debug_args(deconstruct(Var, ConsId, Tag, Vars)) -->
	debug_var(Var),
	debug_cons_id(ConsId),
	debug_tag(Tag),
	{ list__length(Vars, Length) },
	debug_length(Length),
	debug_vars(Vars).
debug_args(place_arg(Reg, Var)) -->
	debug_reg(Reg),
	debug_var(Var).
debug_args(call(ModuleId, PredId, Arity, ProcId)) -->
	debug_module_id(ModuleId),
	debug_pred_id(PredId),
	debug_length(Arity),
	debug_proc_id(ProcId).
debug_args(pickup_arg(Reg, Var)) -->
	debug_reg(Reg),
	debug_var(Var).
debug_args(builtin_binop(Binop, Var1, Var2, Var3)) -->
	debug_binop(Binop),
	debug_var(Var1),
	debug_var(Var2),
	debug_var(Var3).
debug_args(builtin_unop(Unop, Var1, Var2)) -->
	debug_unop(Unop),
	debug_var(Var1),
	debug_var(Var2).
debug_args(pragma_c_code) --> [].

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

:- pred output_tag(int, io__state, io__state).
:- mode output_tag(in, di, uo) is det.

output_tag(Tag) -->
	output_two_byte(Tag).

:- pred debug_tag(int, io__state, io__state).
:- mode debug_tag(in, di, uo) is det.

debug_tag(Tag) -->
	debug_int(Tag).

%---------------------------------------------------------------------------%

:- pred output_reg(reg, io__state, io__state).
:- mode output_reg(in, di, uo) is det.

output_reg(r(N)) -->
	output_byte(N).
output_reg(f(_)) -->
	{ error("we do not handle floating point registers yet") }.

:- pred debug_reg(reg, io__state, io__state).
:- mode debug_reg(in, di, uo) is det.

debug_reg(r(N)) -->
	debug_int(N).
debug_reg(f(_)) -->
	{ error("we do not handle floating point registers yet") }.

%---------------------------------------------------------------------------%

:- pred output_length(int, io__state, io__state).
:- mode output_length(in, di, uo) is det.

output_length(Length) -->
	output_two_byte(Length).

:- pred debug_length(int, io__state, io__state).
:- mode debug_length(in, di, uo) is det.

debug_length(Length) -->
	debug_int(Length).

%---------------------------------------------------------------------------%

:- pred output_var(byte_var, io__state, io__state).
:- mode output_var(in, di, uo) is det.

output_var(Var) -->
	output_two_byte(Var).

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
	output_two_byte(ProcId).

:- pred debug_proc_id(byte_proc_id, io__state, io__state).
:- mode debug_proc_id(in, di, uo) is det.

debug_proc_id(ProcId) -->
	debug_int(ProcId).

%---------------------------------------------------------------------------%

:- pred output_label_id(int, io__state, io__state).
:- mode output_label_id(in, di, uo) is det.

output_label_id(LabelId) -->
	output_two_byte(LabelId).

:- pred debug_label_id(int, io__state, io__state).
:- mode debug_label_id(in, di, uo) is det.

debug_label_id(LabelId) -->
	debug_int(LabelId).

%---------------------------------------------------------------------------%

% XXX incomplete

:- pred output_cons_id(cons_id, io__state, io__state).
:- mode output_cons_id(in, di, uo) is det.

output_cons_id(_) --> [].

:- pred debug_cons_id(cons_id, io__state, io__state).
:- mode debug_cons_id(in, di, uo) is det.

debug_cons_id(_) --> [].

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
	io__write_string(Debug),
	io__write_char(' ').

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
	io__write_string(Debug),
	io__write_char(' ').

%---------------------------------------------------------------------------%

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
byte_code(enter_switch(_, _),			 9).
byte_code(endof_switch,				10).
byte_code(enter_switch_arm(_, _, _),		11).
byte_code(endof_switch_arm,			12).
byte_code(enter_negation(_),			13).
byte_code(endof_negation,			14).
byte_code(enter_commit,				15).
byte_code(endof_commit,				16).
byte_code(assign(_, _),				17).
byte_code(test(_, _),				18).
byte_code(construct(_, _, _, _),		19).
byte_code(deconstruct(_, _, _, _),		20).
byte_code(place_arg(_, _),			21).
byte_code(call(_, _, _, _),			22).
byte_code(pickup_arg(_, _),			23).
byte_code(builtin_binop(_, _, _, _),		24).
byte_code(builtin_unop(_, _, _),		25).
byte_code(pragma_c_code,			26).

:- pred byte_debug(byte_code, string).
:- mode byte_debug(in, out) is det.

byte_debug(enter_pred(_, _),			"enter_pred").
byte_debug(endof_pred,				"endof_pred").
byte_debug(enter_proc(_, _),			"enter_proc").
byte_debug(endof_proc,				"endof_proc").
byte_debug(label(_),				"label").
byte_debug(enter_disjunction(_),		"enter_disjunction").
byte_debug(endof_disjunction,			"endof_disjunction").
byte_debug(enter_disjunct(_),			"enter_disjunct").
byte_debug(endof_disjunct,			"endof_disjunct").
byte_debug(enter_switch(_, _),			"enter_switch").
byte_debug(endof_switch,			"endof_switch").
byte_debug(enter_switch_arm(_, _, _),		"enter_switch_arm").
byte_debug(endof_switch_arm,			"endof_switch_arm").
byte_debug(enter_negation(_),			"enter_negation").
byte_debug(endof_negation,			"enter_negation").
byte_debug(enter_commit,			"enter_commit").
byte_debug(endof_commit,			"enter_commit").
byte_debug(assign(_, _),			"assign").
byte_debug(test(_, _),				"test").
byte_debug(construct(_, _, _, _),		"construct").
byte_debug(deconstruct(_, _, _, _),		"deconstruct").
byte_debug(place_arg(_, _),			"place_arg").
byte_debug(call(_, _, _, _),			"call").
byte_debug(pickup_arg(_, _),			"pickup_arg").
byte_debug(builtin_binop(_, _, _, _),		"builtin_binop").
byte_debug(builtin_unop(_, _, _),		"builtin_unop").
byte_debug(pragma_c_code,			"pragma_c_code").

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
binop_code(array_index,		13).
binop_code(str_eq,		14).
binop_code(str_ne,		15).
binop_code(str_lt,		16).
binop_code(str_gt,		17).
binop_code(str_le,		18).
binop_code(str_ge,		19).
binop_code((<),			20).
binop_code((>),			21).
binop_code((<=),		22).
binop_code((>=),		23).
binop_code(float_plus,		24).
binop_code(float_minus,		25).
binop_code(float_times,		26).
binop_code(float_divide,	27).
binop_code(float_eq,		28).
binop_code(float_ne,		29).
binop_code(float_lt,		30).
binop_code(float_gt,		31).
binop_code(float_le,		32).
binop_code(float_ge,		33).

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
	io__write_string(Val),
	io__write_byte(0).

:- pred output_byte(int, io__state, io__state).
:- mode output_byte(in, di, uo) is det.

output_byte(Val) -->
	( { Val < 256 } ->
		io__write_byte(Val)
	;
		{ error("byte does not fit in eight bits") }
	).

:- pred output_two_byte(int, io__state, io__state).
:- mode output_two_byte(in, di, uo) is det.

output_two_byte(Val) -->
	( { Val < 256*256 } ->
		{ Val1 is Val >> 8 },
		{ Val2 is Val mod 256 },
		io__write_byte(Val1),
		io__write_byte(Val2)
	;
		{ error("byte does not fit in eight bits") }
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

%---------------------------------------------------------------------------%
