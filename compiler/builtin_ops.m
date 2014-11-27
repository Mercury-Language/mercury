%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001, 2003-2006, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: builtin_ops.m -- defines the builtin operator types.
% Main author: fjh.
%
% This module defines various types which enumerate the different builtin
% operators. Several of the different back-ends -- the bytecode back-end,
% the LLDS, and the MLDS -- all use the same set of builtin operators.
% These operators are defined here.
%
%-----------------------------------------------------------------------------%

:- module backend_libs.builtin_ops.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type unary_op
    --->    mktag
    ;       tag
    ;       unmktag
    ;       strip_tag
    ;       mkbody
    ;       unmkbody
    ;       bitwise_complement
    ;       logical_not
    ;       hash_string
    ;       hash_string2
    ;       hash_string3.

:- type binary_op
    --->    int_add
    ;       int_sub
    ;       int_mul
    ;       int_div % assumed to truncate toward zero
    ;       int_mod % remainder (w.r.t. truncating integer division)
                    % XXX `mod' should be renamed `rem'
    ;       unchecked_left_shift
    ;       unchecked_right_shift
    ;       bitwise_and
    ;       bitwise_or
    ;       bitwise_xor
    ;       logical_and
    ;       logical_or
    ;       eq      % ==
    ;       ne      % !=
    ;       body
    ;       array_index(array_elem_type)
    ;       str_eq  % string comparisons
    ;       str_ne
    ;       str_lt
    ;       str_gt
    ;       str_le
    ;       str_ge
    ;       str_cmp % returns -ve, 0, or +ve
    ;       int_lt  % signed integer comparions
    ;       int_gt
    ;       int_le
    ;       int_ge
    ;       unsigned_le % unsigned integer comparison
            % Note that the arguments to `unsigned_le' are just ordinary
            % (signed) Mercury ints, but it does the comparison as
            % if they were first cast to an unsigned type, so e.g.
            % binary(unsigned_le, int_const(1), int_const(-1)) returns true,
            % since (MR_Unsigned) 1 <= (MR_Unsigned) -1.

    ;       float_plus
    ;       float_minus
    ;       float_times
    ;       float_divide
    ;       float_eq
    ;       float_ne
    ;       float_lt
    ;       float_gt
    ;       float_le
    ;       float_ge
    ;       float_word_bits
    ;       float_from_dword

    ;       pointer_equal_conservative

    ;       compound_eq
    ;       compound_lt.
            % Comparisons on values of non-atomic types. This is likely to be
            % supported only on very high-level back-ends.

    % For the MLDS back-end, we need to know the element type for each
    % array_index operation.
    %
    % Currently array index operations are only generated in limited
    % circumstances. Using a simple representation for them here,
    % rather than just putting the MLDS type here, avoids the need
    % for this module to depend on back-end specific stuff like MLDS types.
:- type array_elem_type
    --->    array_elem_scalar(scalar_array_elem_type)
    ;       array_elem_struct(list(scalar_array_elem_type)).

:- type scalar_array_elem_type
    --->    scalar_elem_string    % ml_string_type
    ;       scalar_elem_int       % mlds_native_int_type
    ;       scalar_elem_generic.  % mlds_generic_type

    % test_if_builtin(ModuleName, PredName, ProcId, Args):
    %
    % Given a module name, a predicate name, a proc_id and a list of the
    % arguments, find out if that procedure of that predicate is an inline
    % builtin.
    %
:- pred test_if_builtin(module_name::in, string::in, proc_id::in,
    list(T)::in) is semidet.

    % translate_builtin(ModuleName, PredName, ProcId, Args, Code):
    %
    % This predicate should be invoked only in cases where
    % test_if_builtin(ModuleName, PredName, ProcId, Args) has succeeded.
    %
    % In such cases, it returns an abstract representation of the code
    % that can be used to evaluate that call, which will be either
    % an assignment (if the builtin is det) or a test (if the builtin
    % is semidet).
    %
    % There are some further guarantees on the form of the expressions
    % in the code returned, expressed in the form of the insts below.
    % (bytecode_gen.m depends on these guarantees.)
    %
:- pred translate_builtin(module_name::in, string::in, proc_id::in,
    list(T)::in, simple_code(T)::out(simple_code)) is det.

:- type simple_code(T)
    --->    assign(T, simple_expr(T))
    ;       ref_assign(T, T)
    ;       test(simple_expr(T))
    ;       noop(list(T)).

:- type simple_expr(T)
    --->    leaf(T)
    ;       int_const(int)
    ;       float_const(float)
    ;       unary(unary_op, simple_expr(T))
    ;       binary(binary_op, simple_expr(T), simple_expr(T)).

    % Each test expression returned is guaranteed to be either a unary
    % or binary operator, applied to arguments that are either variables
    % (from the argument list) or constants.
    %
    % Each to be assigned expression is guaranteed to be either in a form
    % acceptable for a test rval, or in the form of a variable.

:- inst simple_code
    --->    assign(ground, simple_assign_expr)
    ;       ref_assign(ground, ground)
    ;       test(simple_test_expr)
    ;       noop(ground).

:- inst simple_arg_expr
    --->    leaf(ground)
    ;       int_const(ground)
    ;       float_const(ground).

:- inst simple_test_expr
    --->    unary(ground, simple_arg_expr)
    ;       binary(ground, simple_arg_expr, simple_arg_expr).

:- inst simple_assign_expr
    --->    unary(ground, simple_arg_expr)
    ;       binary(ground, simple_arg_expr, simple_arg_expr)
    ;       leaf(ground).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.builtin_modules.

:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

test_if_builtin(FullyQualifiedModule, PredName, ProcId, Args) :-
    is_std_lib_module_name(FullyQualifiedModule, ModuleName),
    proc_id_to_int(ProcId, ProcNum),
    builtin_translation(ModuleName, PredName, ProcNum, Args, _Code).

translate_builtin(FullyQualifiedModule, PredName, ProcId, Args, Code) :-
    (
        is_std_lib_module_name(FullyQualifiedModule, ModuleName),
        proc_id_to_int(ProcId, ProcNum),
        builtin_translation(ModuleName, PredName, ProcNum, Args, CodePrime)
    ->
        Code = CodePrime
    ;
        list.length(Args, Arity),
        string.format("unknown builtin %s/%d", [s(PredName), i(Arity)], Msg),
        unexpected($module, $pred, Msg)
    ).

:- pred builtin_translation(string::in, string::in, int::in, list(T)::in,
    simple_code(T)::out(simple_code)) is semidet.
:- pragma inline(builtin_translation/5).

builtin_translation(ModuleName, PredName, ProcNum, Args, Code) :-
    (
        ModuleName = "builtin",
        PredName = "unsafe_promise_unique", ProcNum = 0, Args = [X, Y],
        Code = assign(Y, leaf(X))
    ;
        ModuleName = "private_builtin",
        (
            PredName = "trace_get_io_state", ProcNum = 0, Args = [X],
            Code = noop([X])
        ;
            PredName = "trace_set_io_state", ProcNum = 0, Args = [_X],
            Code = noop([])
        ;
            ( PredName = "store_at_ref"
            ; PredName = "store_at_ref_impure"
            ),
            ProcNum = 0, Args = [X, Y],
            Code = ref_assign(X, Y)
        ;
            PredName = "unsafe_type_cast", ProcNum = 0, Args = [X, Y],
            % Note that the code we generate for unsafe_type_cast
            % is not type-correct. Back-ends that require type-correct
            % intermediate code (e.g. the MLDS back-end) must handle
            % unsafe_type_cast separately, rather than by calling
            % builtin_translation.
            Code = assign(Y, leaf(X))
        ;
            ( PredName = "builtin_int_gt", CompareOp = int_gt
            ; PredName = "builtin_int_lt", CompareOp = int_lt
            ),
            ProcNum = 0, Args = [X, Y],
            Code = test(binary(CompareOp, leaf(X), leaf(Y)))
        ;
            ( PredName = "builtin_compound_eq", CompareOp = compound_eq
            ; PredName = "builtin_compound_lt", CompareOp = compound_lt
            ),
            ProcNum = 0, Args = [X, Y],
            Code = test(binary(CompareOp, leaf(X), leaf(Y)))

        ;
            PredName = "pointer_equal", ProcNum = 0,
            % The arity of this predicate is two during parsing,
            % and three after the polymorphism pass.
            ( Args = [X, Y]
            ; Args = [_TypeInfo, X, Y]
            ),
            Code = test(binary(pointer_equal_conservative, leaf(X), leaf(Y)))
        )
    ;
        ModuleName = "term_size_prof_builtin",
        PredName = "term_size_plus", ProcNum = 0, Args = [X, Y, Z],
        Code = assign(Z, binary(int_add, leaf(X), leaf(Y)))
    ;
        ModuleName = "int",
        (
            PredName = "+",
            (
                Args = [X, Y, Z],
                (
                    ProcNum = 0,
                    Code = assign(Z, binary(int_add, leaf(X), leaf(Y)))
                ;
                    ProcNum = 1,
                    Code = assign(X, binary(int_sub, leaf(Z), leaf(Y)))
                ;
                    ProcNum = 2,
                    Code = assign(Y, binary(int_sub, leaf(Z), leaf(X)))
                )
            ;
                Args = [X, Y],
                ProcNum = 0,
                Code = assign(Y, leaf(X))
            )
        ;
            PredName = "-",
            (
                Args = [X, Y, Z],
                (
                    ProcNum = 0,
                    Code = assign(Z, binary(int_sub, leaf(X), leaf(Y)))
                ;
                    ProcNum = 1,
                    Code = assign(X, binary(int_add, leaf(Y), leaf(Z)))
                ;
                    ProcNum = 2,
                    Code = assign(Y, binary(int_sub, leaf(X), leaf(Z)))
                )
            ;
                Args = [X, Y],
                ProcNum = 0,
                Code = assign(Y, binary(int_sub, int_const(0), leaf(X)))
            )
        ;
            PredName = "xor", Args = [X, Y, Z],
            (
                ProcNum = 0,
                Code = assign(Z, binary(bitwise_xor, leaf(X), leaf(Y)))
            ;
                ProcNum = 1,
                Code = assign(Y, binary(bitwise_xor, leaf(X), leaf(Z)))
            ;
                ProcNum = 2,
                Code = assign(X, binary(bitwise_xor, leaf(Y), leaf(Z)))
            )
        ;
            ( PredName = "plus", ArithOp = int_add
            ; PredName = "minus", ArithOp = int_sub
            ; PredName = "*", ArithOp = int_mul
            ; PredName = "times", ArithOp = int_mul
            ; PredName = "unchecked_quotient", ArithOp = int_div
            ; PredName = "unchecked_rem", ArithOp = int_mod
            ; PredName = "unchecked_left_shift",
                ArithOp = unchecked_left_shift
            ; PredName = "unchecked_right_shift",
                ArithOp = unchecked_right_shift
            ; PredName = "/\\", ArithOp = bitwise_and
            ; PredName = "\\/", ArithOp = bitwise_or
            ),
            ProcNum = 0, Args = [X, Y, Z],
            Code = assign(Z, binary(ArithOp, leaf(X), leaf(Y)))
        ;
            PredName = "\\", ProcNum = 0, Args = [X, Y],
            Code = assign(Y, unary(bitwise_complement, leaf(X)))
        ;
            ( PredName = ">", CompareOp = int_gt
            ; PredName = "<", CompareOp = int_lt
            ; PredName = ">=", CompareOp = int_ge
            ; PredName = "=<", CompareOp = int_le
            ),
            ProcNum = 0, Args = [X, Y],
            Code = test(binary(CompareOp, leaf(X), leaf(Y)))
        )
    ;
        ModuleName = "float",
        (
            PredName = "+",
            (
                Args = [X, Y],
                ProcNum = 0,
                Code = assign(Y, leaf(X))
            ;
                Args = [X, Y, Z],
                ProcNum = 0,
                Code = assign(Z, binary(float_plus, leaf(X), leaf(Y)))
            )
        ;
            PredName = "-",
            (
                Args = [X, Y],
                ProcNum = 0,
                Code = assign(Y,
                    binary(float_minus, float_const(0.0), leaf(X)))
            ;
                Args = [X, Y, Z],
                ProcNum = 0,
                Code = assign(Z, binary(float_minus, leaf(X), leaf(Y)))
            )
        ;
            ( PredName = "*", ArithOp = float_times
            ; PredName = "unchecked_quotient", ArithOp = float_divide
            ),
            ProcNum = 0, Args = [X, Y, Z],
            Code = assign(Z, binary(ArithOp, leaf(X), leaf(Y)))
        ;
            ( PredName = ">", CompareOp = float_gt
            ; PredName = "<", CompareOp = float_lt
            ; PredName = ">=", CompareOp = float_ge
            ; PredName = "=<", CompareOp = float_le
            ),
            ProcNum = 0, Args = [X, Y],
            Code = test(binary(CompareOp, leaf(X), leaf(Y)))
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module backend_libs.builtin_ops.
%-----------------------------------------------------------------------------%
