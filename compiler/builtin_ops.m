%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001 The University of Melbourne.
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
	;	strip_tag
	;	mkbody
	;	unmkbody
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
	;	array_index(array_elem_type)
	;	str_eq	% string comparisons
	;	str_ne
	;	str_lt
	;	str_gt
	;	str_le
	;	str_ge
	;	(<)	% signed integer comparions
	;	(>)
	;	(<=)
	;	(>=)
	;	unsigned_le	% unsigned integer comparison
		% Note that the arguments to `unsigned_le' are just ordinary
		% (signed) Mercury ints, but it does the comparison as
		% if they were first cast to an unsigned type, so e.g.
		% binary(unsigned_le, int_const(1), int_const(-1) returns true,
		% since (MR_Unsigned) 1 <= (MR_Unsigned) -1).

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

	% For the MLDS back-end, we need to know the element type for each
	% array_index operation.
	%
	% Currently array index operations are only generated in limited
	% circumstances.  Using a simple representation for them here,
	% rather than just putting the MLDS type here, avoids the need
	% for this module to depend on back-end specific stuff like MLDS types.
:- type array_elem_type
	--->	elem_type_string	% ml_string_type
	;	elem_type_int		% mlds__native_int_type
	;	elem_type_generic	% mlds__generic_type
	.


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

	% Note that the code we generate for unsafe_type_cast is not
	% type-correct.  Back-ends that require type-correct intermediate
	% code (e.g. the MLDS back-end) must handle unsafe_type_cast
	% separately, rather than by calling builtin_translation.
builtin_translation("private_builtin", "unsafe_type_cast", 0,
		[X, Y], assign(Y, leaf(X))).
builtin_translation("builtin", "unsafe_promise_unique", 0,
		[X, Y], assign(Y, leaf(X))).

builtin_translation("private_builtin", "builtin_int_gt", 0, [X, Y],
	test(binary((>), leaf(X), leaf(Y)))).
builtin_translation("private_builtin", "builtin_int_lt", 0, [X, Y],
	test(binary((<), leaf(X), leaf(Y)))).

builtin_translation("int", "+", 0, [X, Y, Z],
	assign(Z, binary((+), leaf(X), leaf(Y)))).
builtin_translation("int", "+", 1, [X, Y, Z],
	assign(X, binary((-), leaf(Z), leaf(Y)))).
builtin_translation("int", "+", 2, [X, Y, Z],
	assign(Y, binary((-), leaf(Z), leaf(X)))).
builtin_translation("int", "-", 0, [X, Y, Z],
	assign(Z, binary((-), leaf(X), leaf(Y)))).
builtin_translation("int", "-", 1, [X, Y, Z],
	assign(X, binary((+), leaf(Y), leaf(Z)))).
builtin_translation("int", "-", 2, [X, Y, Z],
	assign(Y, binary((-), leaf(X), leaf(Z)))).
builtin_translation("int", "*", 0, [X, Y, Z],
	assign(Z, binary((*), leaf(X), leaf(Y)))).
builtin_translation("int", "unchecked_quotient", 0, [X, Y, Z],
	assign(Z, binary((/), leaf(X), leaf(Y)))).
builtin_translation("int", "rem", 0, [X, Y, Z],
	assign(Z, binary((mod), leaf(X), leaf(Y)))).
builtin_translation("int", "unchecked_rem", 0, [X, Y, Z],
	assign(Z, binary((mod), leaf(X), leaf(Y)))).
builtin_translation("int", "unchecked_left_shift", 0, [X, Y, Z],
	assign(Z, binary((<<), leaf(X), leaf(Y)))).
builtin_translation("int", "unchecked_right_shift", 0, [X, Y, Z],
	assign(Z, binary((>>), leaf(X), leaf(Y)))).
builtin_translation("int", "/\\", 0, [X, Y, Z],
	assign(Z, binary((&), leaf(X), leaf(Y)))).
builtin_translation("int", "\\/", 0, [X, Y, Z],
	assign(Z, binary(('|'), leaf(X), leaf(Y)))).
builtin_translation("int", "xor", 0, [X, Y, Z],
	assign(Z, binary((^), leaf(X), leaf(Y)))).
builtin_translation("int", "xor", 1, [X, Y, Z],
	assign(Y, binary((^), leaf(X), leaf(Z)))).
builtin_translation("int", "xor", 2, [X, Y, Z],
	assign(X, binary((^), leaf(Y), leaf(Z)))).
builtin_translation("int", "+", 0, [X, Y],
	assign(Y, leaf(X))).
builtin_translation("int", "-", 0, [X, Y],
	assign(Y, binary((-), int_const(0), leaf(X)))).
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

builtin_translation("float", "+", 0, [X, Y, Z],
	assign(Z, binary(float_plus, leaf(X), leaf(Y)))).
builtin_translation("float", "-", 0, [X, Y, Z],
	assign(Z, binary(float_minus, leaf(X), leaf(Y)))).
builtin_translation("float", "*", 0, [X, Y, Z],
	assign(Z, binary(float_times, leaf(X), leaf(Y)))).
builtin_translation("float", "unchecked_quotient", 0, [X, Y, Z],
	assign(Z, binary(float_divide, leaf(X), leaf(Y)))).
builtin_translation("float", "+", 0, [X, Y],
	assign(Y, leaf(X))).
builtin_translation("float", "-", 0, [X, Y],
	assign(Y, binary(float_minus, float_const(0.0), leaf(X)))).
builtin_translation("float", ">", 0, [X, Y],
	test(binary(float_gt, leaf(X), leaf(Y)))).
builtin_translation("float", "<", 0, [X, Y],
	test(binary(float_lt, leaf(X), leaf(Y)))).
builtin_translation("float", ">=", 0, [X, Y],
	test(binary(float_ge, leaf(X), leaf(Y)))).
builtin_translation("float", "=<", 0, [X, Y],
	test(binary(float_le, leaf(X), leaf(Y)))).

