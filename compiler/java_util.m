%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: java_util.m
% Main authors: juliensf, mjwybrow.

% This module defines utility routines that are used by the
% Java backend.  Much of the code below is similar to that in c_util.m;
% changes made to this module may require changes c_util.m. 

%-----------------------------------------------------------------------------%

:- module java_util.
:- interface.
:- import_module string.
:- import_module builtin_ops.

%-----------------------------------------------------------------------------%

	% Succeeds iff the given string matches a reserved
	% word in Java.
	%
:- pred java_util__is_keyword(string).
:- mode java_util__is_keyword(in) is semidet.

%-----------------------------------------------------------------------------%
%
% The following predicates all take as input an operator,
% check if it is an operator of the specified kind,
% and if so, return the name of the corresponding Java operator
% that can be used to implement it.
%

	% The operator returned will be either a prefix operator
	% or function name.  The operand needs
	% to be placed in parentheses after the operator name.
	%
:- pred java_util__unary_prefix_op(builtin_ops__unary_op, string).
:- mode java_util__unary_prefix_op(in, out) is det.

	% The operator returned will be <, >, etc.;
	% it can be used in the form: 
	% `<string_object>.CompareTo(<Arg1>, <Arg2>) <Op> 0'.
	% 
:- pred java_util__string_compare_op(binary_op, string).
:- mode java_util__string_compare_op(in, out) is semidet.

	% The operator returned will be +, *, etc.;
	% the arguments should be floats and the result will be a float.
	%
:- pred java_util__float_op(binary_op, string).
:- mode java_util__float_op(in, out) is semidet.

	% The operator returned will be <, >, etc.;
	% the arguments should be floats and the result will be a boolean.
	%
:- pred java_util__float_compare_op(binary_op, string).
:- mode java_util__float_compare_op(in, out) is semidet.

	% The operator returned will be an infix operator.
	% The arguments should be integer or booleans
	% and the result will be an integer or a boolean.
	%
:- pred java_util__binary_infix_op(binary_op, string).
:- mode java_util__binary_infix_op(in, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list.
:- import_module error_util.

%-----------------------------------------------------------------------------%


	% Tags are not used in the Java back-end, as such, all of the tagging
	% operators except for `tag' return no-ops. The `tag' case is handled
	% seperately in mlds_to_java__output_std_unop.
	% 
java_util__unary_prefix_op(mktag, 		"/* mktag */ ").
java_util__unary_prefix_op(unmktag, 		"/* unmktag */ ").
java_util__unary_prefix_op(strip_tag,           "/* strip_tag */ ").
java_util__unary_prefix_op(mkbody, 		"/* mkbody */ ").
java_util__unary_prefix_op(unmkbody,		"/* unmkbody */ ").
java_util__unary_prefix_op(hash_string, 	"mercury.String.hash_1_f_0").
java_util__unary_prefix_op(bitwise_complement,	"~").
java_util__unary_prefix_op((not),		"!").
java_util__unary_prefix_op(tag,	"").	% This case is never used.

java_util__string_compare_op(str_eq, "==").
java_util__string_compare_op(str_ne, "!=").
java_util__string_compare_op(str_le, "<=").
java_util__string_compare_op(str_ge, ">=").
java_util__string_compare_op(str_lt, "<").
java_util__string_compare_op(str_gt, ">").

java_util__float_compare_op(float_eq, "==").
java_util__float_compare_op(float_ne, "!=").
java_util__float_compare_op(float_le, "<=").
java_util__float_compare_op(float_ge, ">=").
java_util__float_compare_op(float_lt, "<").
java_util__float_compare_op(float_gt, ">").

java_util__float_op(float_plus, "+").
java_util__float_op(float_minus, "-").
java_util__float_op(float_times, "*").
java_util__float_op(float_divide, "/").

java_util__binary_infix_op(+, "+").
java_util__binary_infix_op(-, "-").
java_util__binary_infix_op(*, "*").
java_util__binary_infix_op(/, "/").
java_util__binary_infix_op(<<, "<<").
java_util__binary_infix_op(>>, ">>").
java_util__binary_infix_op(&, "&").
java_util__binary_infix_op('|', "|").
java_util__binary_infix_op(^, "^").
java_util__binary_infix_op(mod, "%").
java_util__binary_infix_op(eq, "==").
java_util__binary_infix_op(ne, "!=").
java_util__binary_infix_op(and, "&&").
java_util__binary_infix_op(or, "||").
java_util__binary_infix_op(<, "<").
java_util__binary_infix_op(>, ">").
java_util__binary_infix_op(<=, "<=").

%------------------------------------------------------------------------------%
java_util__is_keyword("abstract").
java_util__is_keyword("boolean").
java_util__is_keyword("break").
java_util__is_keyword("byte").
java_util__is_keyword("case").
java_util__is_keyword("catch").
java_util__is_keyword("char").
java_util__is_keyword("class").
java_util__is_keyword("const").
java_util__is_keyword("continue").
java_util__is_keyword("default").
java_util__is_keyword("do").
java_util__is_keyword("double").
java_util__is_keyword("else").
java_util__is_keyword("extends").
java_util__is_keyword("false").
java_util__is_keyword("final").
java_util__is_keyword("finally").
java_util__is_keyword("float").
java_util__is_keyword("for").
java_util__is_keyword("goto").
java_util__is_keyword("if").
java_util__is_keyword("implements").
java_util__is_keyword("import").
java_util__is_keyword("instanceof").
java_util__is_keyword("int").
java_util__is_keyword("interface").
java_util__is_keyword("long").
java_util__is_keyword("native").
java_util__is_keyword("new").
java_util__is_keyword("null").
java_util__is_keyword("package").
java_util__is_keyword("private").
java_util__is_keyword("protected").
java_util__is_keyword("public").
java_util__is_keyword("return").
java_util__is_keyword("short").
java_util__is_keyword("static").
java_util__is_keyword("strictfp").
java_util__is_keyword("super").
java_util__is_keyword("switch").
java_util__is_keyword("synchronized").
java_util__is_keyword("this").
java_util__is_keyword("throw").
java_util__is_keyword("throws").
java_util__is_keyword("transient").
java_util__is_keyword("true").
java_util__is_keyword("try").
java_util__is_keyword("void").
java_util__is_keyword("volatile").
java_util__is_keyword("while").

:- func this_file = string.
this_file = "java_util.m".

:- end_module java_util.
%-----------------------------------------------------------------------------%
