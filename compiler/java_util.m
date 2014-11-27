%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006, 2010-2011 The University of Melbourne.
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

%-----------------------------------------------------------------------------%
%
% The following predicates all take as input an operator, check if it is an
% operator of the specified kind, and if so, return the name of the
% corresponding Java operator that can be used to implement it.
%
% XXX This scheme seems fundamentally incapable of providing a guarantee
% that a piece of code that switches on binary_ops is a *complete* switch,
% i.e. that it handles *all* binary operators.

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
java_unary_prefix_op(bitwise_complement,  "~").
java_unary_prefix_op(logical_not,      "!").
java_unary_prefix_op(tag, "").    % This case is never used.
java_unary_prefix_op(hash_string,     "mercury.String.hash_1_f_0").
java_unary_prefix_op(hash_string2,    "mercury.String.hash2_1_f_0").
java_unary_prefix_op(hash_string3,    "mercury.String.hash3_1_f_0").

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
:- end_module ml_backend.java_util.
%-----------------------------------------------------------------------------%
