%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: java_util.m.
% Main authors: juliensf, mjwybrow.
%
% This module defines utility routines that are used by the Java backend.
% Much of the code below is similar to that in c_util.m; changes made to this
% module may require changes to c_util.m.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.java_util.
:- interface.

:- import_module backend_libs.builtin_ops.

:- import_module string.

%-----------------------------------------------------------------------------%

    % Succeeds iff the given string matches a reserved word in Java.
    %
:- pred java_is_keyword(string::in) is semidet.

%-----------------------------------------------------------------------------%
%
% The following predicates all take as input an operator, check if it is an
% operator of the specified kind, and if so, return the name of the
% corresponding Java operator that can be used to implement it.
%

    % The operator returned will be either a prefix operator or function name.
    % The operand needs to be placed in parentheses after the operator name.
    %
:- pred java_unary_prefix_op(unary_op::in, string::out) is det.

    % The operator returned will be <, >, etc.; it can be used in the form:
    % `<string_object>.CompareTo(<Arg1>, <Arg2>) <Op> 0'.
    %
:- pred java_string_compare_op(binary_op::in, string::out) is semidet.

    % The operator returned will be +, *, etc.;
    % the arguments should be floats and the result will be a float.
    %
:- pred java_float_op(binary_op::in, string::out) is semidet.

    % The operator returned will be <, >, etc.;
    % the arguments should be floats and the result will be a boolean.
    %
:- pred java_float_compare_op(binary_op::in, string::out) is semidet.

    % The operator returned will be an infix operator.
    % The arguments should be integer or booleans
    % and the result will be an integer or a boolean.
    %
:- pred java_binary_infix_op(binary_op::in, string::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_util.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Tags are not used in the Java back-end, as such, all of the tagging
    % operators except for `tag' return no-ops. The `tag' case is handled
    % separately in mlds_to_java__output_std_unop.
    %
java_unary_prefix_op(mktag,           "/* mktag */ ").
java_unary_prefix_op(unmktag,         "/* unmktag */ ").
java_unary_prefix_op(strip_tag,       "/* strip_tag */ ").
java_unary_prefix_op(mkbody,          "/* mkbody */ ").
java_unary_prefix_op(unmkbody,        "/* unmkbody */ ").
java_unary_prefix_op(hash_string,     "mercury.String.hash_1_f_0").
java_unary_prefix_op(bitwise_complement,  "~").
java_unary_prefix_op(logical_not,      "!").
java_unary_prefix_op(tag, "").    % This case is never used.

java_string_compare_op(str_eq, "==").
java_string_compare_op(str_ne, "!=").
java_string_compare_op(str_le, "<=").
java_string_compare_op(str_ge, ">=").
java_string_compare_op(str_lt, "<").
java_string_compare_op(str_gt, ">").

java_float_compare_op(float_eq, "==").
java_float_compare_op(float_ne, "!=").
java_float_compare_op(float_le, "<=").
java_float_compare_op(float_ge, ">=").
java_float_compare_op(float_lt, "<").
java_float_compare_op(float_gt, ">").

java_float_op(float_plus, "+").
java_float_op(float_minus, "-").
java_float_op(float_times, "*").
java_float_op(float_divide, "/").

java_binary_infix_op(int_add, "+").
java_binary_infix_op(int_sub, "-").
java_binary_infix_op(int_mul, "*").
java_binary_infix_op(int_div, "/").
java_binary_infix_op(int_mod, "%").
java_binary_infix_op(unchecked_left_shift, "<<").
java_binary_infix_op(unchecked_right_shift, ">>").
java_binary_infix_op(bitwise_and, "&").
java_binary_infix_op(bitwise_or, "|").
java_binary_infix_op(bitwise_xor, "^").
java_binary_infix_op(logical_and, "&&").
java_binary_infix_op(logical_or, "||").
java_binary_infix_op(eq, "==").
java_binary_infix_op(ne, "!=").
java_binary_infix_op(int_lt, "<").
java_binary_infix_op(int_gt, ">").
java_binary_infix_op(int_le, "<=").
java_binary_infix_op(int_ge, ">=").

%-----------------------------------------------------------------------------%

java_is_keyword("abstract").
java_is_keyword("boolean").
java_is_keyword("break").
java_is_keyword("byte").
java_is_keyword("case").
java_is_keyword("catch").
java_is_keyword("char").
java_is_keyword("class").
java_is_keyword("const").
java_is_keyword("continue").
java_is_keyword("default").
java_is_keyword("do").
java_is_keyword("double").
java_is_keyword("else").
java_is_keyword("enum").
java_is_keyword("extends").
java_is_keyword("false").
java_is_keyword("final").
java_is_keyword("finally").
java_is_keyword("float").
java_is_keyword("for").
java_is_keyword("goto").
java_is_keyword("if").
java_is_keyword("implements").
java_is_keyword("import").
java_is_keyword("instanceof").
java_is_keyword("int").
java_is_keyword("interface").
java_is_keyword("long").
java_is_keyword("native").
java_is_keyword("new").
java_is_keyword("null").
java_is_keyword("package").
java_is_keyword("private").
java_is_keyword("protected").
java_is_keyword("public").
java_is_keyword("return").
java_is_keyword("short").
java_is_keyword("static").
java_is_keyword("strictfp").
java_is_keyword("super").
java_is_keyword("switch").
java_is_keyword("synchronized").
java_is_keyword("this").
java_is_keyword("throw").
java_is_keyword("throws").
java_is_keyword("transient").
java_is_keyword("true").
java_is_keyword("try").
java_is_keyword("void").
java_is_keyword("volatile").
java_is_keyword("while").

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "java_util.m".

%-----------------------------------------------------------------------------%
:- end_module java_util.
%-----------------------------------------------------------------------------%
