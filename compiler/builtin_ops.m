%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% builtin_ops.m -- defines the builtin operator types.
% Main author: fjh.
%
% This module defines various types which enumerate the different builtin
% operators.  Several of the different back-ends -- the bytecode back-end,
% the LLDS, and the MLDS -- all use the same set of builtin operators.
% These operators are defined here.

%-----------------------------------------------------------------------------%

:- module builtin_ops.
:- interface.
:- import_module prog_data, hlds_pred.
:- import_module list.


:- type unary_op
	--->	mktag
	;	tag
	;	unmktag
	;	mkbody
	;	unmkbody
	;	cast_to_unsigned
	;	hash_string
	;	bitwise_complement
	;	(not).

:- type binary_op
	--->	(+)	% integer arithmetic
	;	(-)
	;	(*)
	;	(/)	% integer division
			% assumed to truncate toward zero
	;	(mod)	% remainder (w.r.t. truncating integer division)
			% XXX `mod' should be renamed `rem'
	;	(<<)	% unchecked left shift
	;	(>>)	% unchecked right shift
	;	(&)	% bitwise and
	;	('|')	% bitwise or
	;	(^)	% bitwise xor
	;	(and)	% logical and
	;	(or)	% logical or
	;	eq	% ==
	;	ne	% !=
	;	body
	;	array_index
	;	str_eq	% string comparisons
	;	str_ne
	;	str_lt
	;	str_gt
	;	str_le
	;	str_ge
	;	(<)	% integer comparions
	;	(>)
	;	(<=)
	;	(>=)
	;	float_plus
	;	float_minus
	;	float_times
	;	float_divide
	;	float_eq
	;	float_ne
	;	float_lt
	;	float_gt
	;	float_le
	;	float_ge.

	% translate_builtin:
	%
	% Given a module name, a predicate name, a proc_id and a list of
	% the arguments, find out if that procedure of that predicate
	% is an inline builtin. If so, return code which can be used
	% to evaluate that call: either an assignment (if the builtin is det)
	% or a test (if the builtin is semidet).
	%
	% There are some further guarantees on the form of the expressions
	% in the code returned -- see below for details.
	% (bytecode_gen.m depends on these guarantees.)
	%
:- pred translate_builtin(module_name, string, proc_id, list(T),
		simple_code(T)).
:- mode translate_builtin(in, in, in, in, out(simple_code)) is semidet.

:- type simple_code(T)
	--->	assign(T, simple_expr(T))
	;	test(simple_expr(T)).

:- type simple_expr(T)
	--->	leaf(T)
	;	int_const(int)
	;	float_const(float)
	;	unary(unary_op, simple_expr(T))
	;	binary(binary_op, simple_expr(T), simple_expr(T)).

	% Each test expression returned is guaranteed to be either a unary
	% or binary operator, applied to arguments that are either variables
	% (from the argument list) or constants.
	%
	% Each to be assigned expression is guaranteed to be either in a form
	% acceptable for a test rval, or in the form of a variable.

:- inst simple_code
	--->	assign(ground, simple_assign_expr)
	;	test(simple_test_expr).

:- inst simple_arg_expr
	--->	leaf(ground)
	;	int_const(ground)
	;	float_const(ground).

:- inst simple_test_expr
	--->	unary(ground, simple_arg_expr)
	;	binary(ground, simple_arg_expr, simple_arg_expr).

:- inst simple_assign_expr
	--->	unary(ground, simple_arg_expr)
	;	binary(ground, simple_arg_expr, simple_arg_expr)
	;	leaf(ground).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

translate_builtin(FullyQualifiedModule, PredName, ProcId, Args, Code) :-
	proc_id_to_int(ProcId, ProcInt),
	% -- not yet:
	% FullyQualifiedModule = qualified(unqualified("std"), ModuleName),
	FullyQualifiedModule = unqualified(ModuleName),
	builtin_translation(ModuleName, PredName, ProcInt, Args, Code).

:- pred builtin_translation(string, string, int, list(T), simple_code(T)).
:- mode builtin_translation(in, in, in, in, out) is semidet.

builtin_translation("private_builtin", "unsafe_type_cast", 0,
		[X, Y], assign(Y, leaf(X))).
builtin_translation("builtin", "unsafe_promise_unique", 0,
		[X, Y], assign(Y, leaf(X))).

builtin_translation("private_builtin", "builtin_int_gt", 0, [X, Y],
	test(binary((>), leaf(X), leaf(Y)))).
builtin_translation("private_builtin", "builtin_int_lt", 0, [X, Y],
	test(binary((<), leaf(X), leaf(Y)))).

builtin_translation("int", "builtin_plus", 0, [X, Y, Z],
	assign(Z, binary((+), leaf(X), leaf(Y)))).
builtin_translation("int", "builtin_plus", 1, [X, Y, Z],
	assign(X, binary((-), leaf(Z), leaf(Y)))).
builtin_translation("int", "builtin_plus", 2, [X, Y, Z],
	assign(Y, binary((-), leaf(Z), leaf(X)))).
builtin_translation("int", "+", 0, [X, Y, Z],
	assign(Z, binary((+), leaf(X), leaf(Y)))).
builtin_translation("int", "+", 1, [X, Y, Z],
	assign(X, binary((-), leaf(Z), leaf(Y)))).
builtin_translation("int", "+", 2, [X, Y, Z],
	assign(Y, binary((-), leaf(Z), leaf(X)))).
builtin_translation("int", "builtin_minus", 0, [X, Y, Z],
	assign(Z, binary((-), leaf(X), leaf(Y)))).
builtin_translation("int", "builtin_minus", 1, [X, Y, Z],
	assign(X, binary((+), leaf(Y), leaf(Z)))).
builtin_translation("int", "builtin_minus", 2, [X, Y, Z],
	assign(Y, binary((-), leaf(X), leaf(Z)))).
builtin_translation("int", "-", 0, [X, Y, Z],
	assign(Z, binary((-), leaf(X), leaf(Y)))).
builtin_translation("int", "-", 1, [X, Y, Z],
	assign(X, binary((+), leaf(Y), leaf(Z)))).
builtin_translation("int", "-", 2, [X, Y, Z],
	assign(Y, binary((-), leaf(X), leaf(Z)))).
builtin_translation("int", "builtin_times", 0, [X, Y, Z],
	assign(Z, binary((*), leaf(X), leaf(Y)))).
builtin_translation("int", "builtin_times", 1, [X, Y, Z],
	assign(X, binary((/), leaf(Z), leaf(Y)))).
builtin_translation("int", "builtin_times", 2, [X, Y, Z],
	assign(Y, binary((/), leaf(Z), leaf(X)))).
builtin_translation("int", "*", 0, [X, Y, Z],
	assign(Z, binary((*), leaf(X), leaf(Y)))).
builtin_translation("int", "*", 1, [X, Y, Z],
	assign(X, binary((/), leaf(Z), leaf(Y)))).
builtin_translation("int", "*", 2, [X, Y, Z],
	assign(Y, binary((/), leaf(Z), leaf(X)))).
builtin_translation("int", "builtin_div", 0, [X, Y, Z],
	assign(Z, binary((/), leaf(X), leaf(Y)))).
builtin_translation("int", "builtin_div", 1, [X, Y, Z],
	assign(X, binary((*), leaf(Y), leaf(Z)))).
builtin_translation("int", "builtin_div", 2, [X, Y, Z],
	assign(Y, binary((/), leaf(X), leaf(Z)))).
builtin_translation("int", "//", 0, [X, Y, Z],
	assign(Z, binary((/), leaf(X), leaf(Y)))).
builtin_translation("int", "//", 1, [X, Y, Z],
	assign(X, binary((*), leaf(Y), leaf(Z)))).
builtin_translation("int", "//", 2, [X, Y, Z],
	assign(Y, binary((/), leaf(X), leaf(Z)))).
builtin_translation("int", "builtin_mod", 0, [X, Y, Z],
	assign(Z, binary((mod), leaf(X), leaf(Y)))).
builtin_translation("int", "rem", 0, [X, Y, Z],
	assign(Z, binary((mod), leaf(X), leaf(Y)))).
builtin_translation("int", "builtin_left_shift", 0, [X, Y, Z],
	assign(Z, binary((<<), leaf(X), leaf(Y)))).
builtin_translation("int", "unchecked_left_shift", 0, [X, Y, Z],
	assign(Z, binary((<<), leaf(X), leaf(Y)))).
builtin_translation("int", "builtin_right_shift", 0, [X, Y, Z],
	assign(Z, binary((>>), leaf(X), leaf(Y)))).
builtin_translation("int", "unchecked_right_shift", 0, [X, Y, Z],
	assign(Z, binary((>>), leaf(X), leaf(Y)))).
builtin_translation("int", "builtin_bit_and", 0, [X, Y, Z],
	assign(Z, binary((&), leaf(X), leaf(Y)))).
builtin_translation("int", "/\\", 0, [X, Y, Z],
	assign(Z, binary((&), leaf(X), leaf(Y)))).
builtin_translation("int", "builtin_bit_or", 0, [X, Y, Z],
	assign(Z, binary(('|'), leaf(X), leaf(Y)))).
builtin_translation("int", "\\/", 0, [X, Y, Z],
	assign(Z, binary(('|'), leaf(X), leaf(Y)))).
builtin_translation("int", "builtin_bit_xor", 0, [X, Y, Z],
	assign(Z, binary((^), leaf(X), leaf(Y)))).
builtin_translation("int", "^", 0, [X, Y, Z],
	assign(Z, binary((^), leaf(X), leaf(Y)))).
builtin_translation("int", "xor", 0, [X, Y, Z],
	assign(Z, binary((^), leaf(X), leaf(Y)))).
builtin_translation("int", "xor", 1, [X, Y, Z],
	assign(Y, binary((^), leaf(X), leaf(Z)))).
builtin_translation("int", "xor", 2, [X, Y, Z],
	assign(X, binary((^), leaf(Y), leaf(Z)))).
builtin_translation("int", "builtin_unary_plus", 0, [X, Y],
	assign(Y, leaf(X))).
builtin_translation("int", "+", 0, [X, Y],
	assign(Y, leaf(X))).
builtin_translation("int", "builtin_unary_minus", 0, [X, Y],
	assign(Y, binary((-), int_const(0), leaf(X)))).
builtin_translation("int", "-", 0, [X, Y],
	assign(Y, binary((-), int_const(0), leaf(X)))).
builtin_translation("int", "builtin_bit_neg", 0, [X, Y],
	assign(Y, unary(bitwise_complement, leaf(X)))).
builtin_translation("int", "\\", 0, [X, Y],
	assign(Y, unary(bitwise_complement, leaf(X)))).
builtin_translation("int", ">", 0, [X, Y],
	test(binary((>), leaf(X), leaf(Y)))).
builtin_translation("int", "<", 0, [X, Y],
	test(binary((<), leaf(X), leaf(Y)))).
builtin_translation("int", ">=", 0, [X, Y],
	test(binary((>=), leaf(X), leaf(Y)))).
builtin_translation("int", "=<", 0, [X, Y],
	test(binary((<=), leaf(X), leaf(Y)))).

builtin_translation("float", "builtin_float_plus", 0, [X, Y, Z],
	assign(Z, binary(float_plus, leaf(X), leaf(Y)))).
builtin_translation("float", "builtin_float_plus", 1, [X, Y, Z],
	assign(X, binary(float_minus, leaf(Z), leaf(Y)))).
builtin_translation("float", "builtin_float_plus", 2, [X, Y, Z],
	assign(Y, binary(float_minus, leaf(Z), leaf(X)))).
builtin_translation("float", "+", 0, [X, Y, Z],
	assign(Z, binary(float_plus, leaf(X), leaf(Y)))).
builtin_translation("float", "+", 1, [X, Y, Z],
	assign(X, binary(float_minus, leaf(Z), leaf(Y)))).
builtin_translation("float", "+", 2, [X, Y, Z],
	assign(Y, binary(float_minus, leaf(Z), leaf(X)))).
builtin_translation("float", "builtin_float_minus", 0, [X, Y, Z],
	assign(Z, binary(float_minus, leaf(X), leaf(Y)))).
builtin_translation("float", "builtin_float_minus", 1, [X, Y, Z],
	assign(X, binary(float_plus, leaf(Y), leaf(Z)))).
builtin_translation("float", "builtin_float_minus", 2, [X, Y, Z],
	assign(Y, binary(float_minus, leaf(X), leaf(Z)))).
builtin_translation("float", "-", 0, [X, Y, Z],
	assign(Z, binary(float_minus, leaf(X), leaf(Y)))).
builtin_translation("float", "-", 1, [X, Y, Z],
	assign(X, binary(float_plus, leaf(Y), leaf(Z)))).
builtin_translation("float", "-", 2, [X, Y, Z],
	assign(Y, binary(float_minus, leaf(X), leaf(Z)))).
builtin_translation("float", "builtin_float_times", 0, [X, Y, Z],
	assign(Z, binary(float_times, leaf(X), leaf(Y)))).
builtin_translation("float", "builtin_float_times", 1, [X, Y, Z],
	assign(X, binary(float_divide, leaf(Z), leaf(Y)))).
builtin_translation("float", "builtin_float_times", 2, [X, Y, Z],
	assign(Y, binary(float_divide, leaf(Z), leaf(X)))).
builtin_translation("float", "*", 0, [X, Y, Z],
	assign(Z, binary(float_times, leaf(X), leaf(Y)))).
builtin_translation("float", "*", 1, [X, Y, Z],
	assign(X, binary(float_divide, leaf(Z), leaf(Y)))).
builtin_translation("float", "*", 2, [X, Y, Z],
	assign(Y, binary(float_divide, leaf(Z), leaf(X)))).
builtin_translation("float", "builtin_float_divide", 0, [X, Y, Z],
	assign(Z, binary(float_divide, leaf(X), leaf(Y)))).
builtin_translation("float", "builtin_float_divide", 1, [X, Y, Z],
	assign(X, binary(float_times, leaf(Y), leaf(Z)))).
builtin_translation("float", "builtin_float_divide", 2, [X, Y, Z],
	assign(Y, binary(float_divide, leaf(X), leaf(Z)))).
builtin_translation("float", "/", 0, [X, Y, Z],
	assign(Z, binary(float_divide, leaf(X), leaf(Y)))).
builtin_translation("float", "/", 1, [X, Y, Z],
	assign(X, binary(float_times, leaf(Y), leaf(Z)))).
builtin_translation("float", "/", 2, [X, Y, Z],
	assign(Y, binary(float_divide, leaf(X), leaf(Z)))).
builtin_translation("float", "+", 0, [X, Y],
	assign(Y, leaf(X))).
builtin_translation("float", "-", 0, [X, Y],
	assign(Y, binary(float_minus, float_const(0.0), leaf(X)))).
builtin_translation("float", "builtin_float_gt", 0, [X, Y],
	test(binary(float_gt, leaf(X), leaf(Y)))).
builtin_translation("float", ">", 0, [X, Y],
	test(binary(float_gt, leaf(X), leaf(Y)))).
builtin_translation("float", "builtin_float_lt", 0, [X, Y],
	test(binary(float_lt, leaf(X), leaf(Y)))).
builtin_translation("float", "<", 0, [X, Y],
	test(binary(float_lt, leaf(X), leaf(Y)))).
builtin_translation("float", "builtin_float_ge", 0, [X, Y],
	test(binary(float_ge, leaf(X), leaf(Y)))).
builtin_translation("float", ">=", 0, [X, Y],
	test(binary(float_ge, leaf(X), leaf(Y)))).
builtin_translation("float", "builtin_float_le", 0, [X, Y],
	test(binary(float_le, leaf(X), leaf(Y)))).
builtin_translation("float", "=<", 0, [X, Y],
	test(binary(float_le, leaf(X), leaf(Y)))).

