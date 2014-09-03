%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: const_prop.m.
% Main author: conway.
%
% This module provides the facility to evaluate calls to standard library
% routines at compile time, transforming them to simpler goals such as
% construction unifications.
%
% XXX We should check for overflow.  This is particularly important when
% cross-compiling, since the overflow behaviour of the host machine might not
% be the same as that of the target machine, e.g. if they have different word
% sizes.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.const_prop.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % evaluate_call(ModuleName, PredName, Args, VarTypes, Instmap, ModuleInfo,
    %   GoalExpr, !GoalInfo):
    %
    % This attempts to evaluate a call to the specified procedure with the
    % specified arguments. If the call can be statically evaluated, or
    % simplified, evaluate_builtin will succeed, returning the new goal
    % in GoalExpr (and updating GoalInfo). Otherwise it fails.
    %
:- pred evaluate_call(string::in, string::in, int::in, list(prog_var)::in,
    vartypes::in, instmap::in, module_info::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module hlds.make_goal.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

    % This type groups the information from the HLDS about a procedure call
    % argument.
    %
:- type arg_hlds_info
    --->    arg_hlds_info(
                arg_var     :: prog_var,
                arg_type    :: mer_type,
                arg_inst    :: mer_inst
            ).

evaluate_call(ModuleName, PredName, ProcIdInt, Args, VarTypes, InstMap,
        ModuleInfo, GoalExpr, !GoalInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, cross_compiling, CrossCompiling),
    LookupArgs = (func(Var) = arg_hlds_info(Var, Type, Inst) :-
        instmap_lookup_var(InstMap, Var, Inst),
        lookup_var_type(VarTypes, Var, Type)
    ),
    ArgHldsInfos = list.map(LookupArgs, Args),
    evaluate_call_2(ModuleName, PredName, ProcIdInt, ArgHldsInfos,
       CrossCompiling, GoalExpr, !GoalInfo).

:- pred evaluate_call_2(string::in, string::in, int::in,
    list(arg_hlds_info)::in, bool::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out) is semidet.

evaluate_call_2(ModuleName, Pred, ModeNum, Args, CrossCompiling, GoalExpr,
        !GoalInfo) :-
    (
        evaluate_det_call(ModuleName, Pred, ModeNum, CrossCompiling,
            Args, OutputArg, Cons)
    ->
        make_construction_goal(OutputArg, Cons, GoalExpr, !GoalInfo)
    ;
        evaluate_test(ModuleName, Pred, ModeNum, Args, Succeeded)
    ->
        make_true_or_fail(Succeeded, GoalExpr)
    ;
        evaluate_semidet_call(ModuleName, Pred, ModeNum, Args, Result)
    ->
        (
            Result = yes(OutputArg - const(Cons)),
            make_construction_goal(OutputArg, Cons, GoalExpr, !GoalInfo)
        ;
            Result = yes(OutputArg - var(InputArg)),
            make_assignment_goal(OutputArg, InputArg, GoalExpr, !GoalInfo)
        ;
            Result = no,
            make_true_or_fail(no, GoalExpr)
        )
    ;
        fail
    ).

%---------------------------------------------------------------------------%

    % evaluate_det_call(ModuleName, ProcName, ModeNum, CrossCompiling,
    %   Args, OutputArg, OutputArgVal):
    %
    % This attempts to evaluate a call to
    %   ModuleName.ProcName(Args)
    % whose mode is specified by ModeNum.
    %
    % If the call is a det call with one output that can be statically
    % evaluated, evaluate_det_call succeeds with OutputArg being whichever of
    % Args is output, and with OutputArgVal being the computed value of
    % OutputArg. Otherwise it fails.
    %
:- pred evaluate_det_call(string::in, string::in, int::in, bool::in,
    list(arg_hlds_info)::in, arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call(ModuleName, ProcName, ModeNum, CrossCompiling, Args,
        OutputArg, OutputArgVal) :-
    (
        Args = [X],
        % Constant functions.
        (
            ModuleName = "int",
            evaluate_det_call_int_1(ProcName, ModeNum, CrossCompiling,
                X, OutputArg, OutputArgVal)
        )
    ;
        Args = [X, Y],
        % Unary functions.
        (
            ModuleName = "int",
            evaluate_det_call_int_2(ProcName, ModeNum, CrossCompiling,
                X, Y, OutputArg, OutputArgVal)
        ;
            ModuleName = "float",
            evaluate_det_call_float_2(ProcName, ModeNum, CrossCompiling,
                X, Y, OutputArg, OutputArgVal)
        ;
            ModuleName = "string",
            evaluate_det_call_string_2(ProcName, ModeNum, CrossCompiling,
                X, Y, OutputArg, OutputArgVal)
        )
    ;
        Args = [X, Y, Z],
        % Binary functions.
        (
            ModuleName = "int",
            evaluate_det_call_int_3(ProcName, ModeNum, CrossCompiling,
                X, Y, Z, OutputArg, OutputArgVal)
        ;
            ModuleName = "float",
            evaluate_det_call_float_3(ProcName, ModeNum, CrossCompiling,
                X, Y, Z, OutputArg, OutputArgVal)
        ;
            ModuleName = "string",
            evaluate_det_call_string_3(ProcName, ModeNum, CrossCompiling,
                X, Y, Z, OutputArg, OutputArgVal)
        )
    ).

:- pred evaluate_det_call_int_1(string::in, int::in, bool::in,
    arg_hlds_info::in, arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_int_1(ProcName, ModeNum, CrossCompiling, X,
        OutputArg, int_const(OutputArgVal)) :-
    (
        ProcName = "bits_per_int",
        ModeNum = 0,
        CrossCompiling = no,
        OutputArg = X,
        OutputArgVal = int.bits_per_int
    ).

:- pred evaluate_det_call_int_2(string::in, int::in, bool::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::out, cons_id::out)
    is semidet.

evaluate_det_call_int_2(ProcName, ModeNum, CrossCompiling, X, Y,
        OutputArg, int_const(OutputArgVal)) :-
    (
        ProcName = "+",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = XVal
    ;
        ProcName = "-",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = -XVal
    ;
        ProcName = "\\",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = \ XVal
    ;
        ProcName = "floor_to_multiple_of_bits_per_int",
        ModeNum = 0,
        CrossCompiling = no,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = int.floor_to_multiple_of_bits_per_int(XVal)
    ;
        ProcName = "quot_bits_per_int",
        ModeNum = 0,
        CrossCompiling = no,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = int.quot_bits_per_int(XVal)
    ;
        ProcName = "times_bits_per_int",
        ModeNum = 0,
        CrossCompiling = no,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = int.times_bits_per_int(XVal)
    ;
        ProcName = "rem_bits_per_int",
        ModeNum = 0,
        CrossCompiling = no,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = int.rem_bits_per_int(XVal)
    ).

:- pred evaluate_det_call_float_2(string::in, int::in, bool::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::out, cons_id::out)
    is semidet.

evaluate_det_call_float_2(ProcName, ModeNum, _CrossCompiling, X, Y,
        OutputArg, float_const(OutputArgVal)) :-
    (
        ProcName = "+",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = XVal
    ;
        ProcName = "-",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = -XVal
    ).

:- pred evaluate_det_call_string_2(string::in, int::in, bool::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::out,
    cons_id::out) is semidet.

evaluate_det_call_string_2(ProcName, ModeNum, _CrossCompiling, X, Y,
        OutputArg, OutputArgVal) :-
    ProcName = "count_codepoints",
    ModeNum = 0,
    X ^ arg_inst = bound(_, _, [bound_functor(string_const(XVal), [])]),
    OutputArg = Y,
    CodePointCountX = string.count_codepoints(XVal),
    OutputArgVal = int_const(CodePointCountX).

:- pred evaluate_det_call_int_3(string::in, int::in, bool::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_int_3(ProcName, ModeNum, _CrossCompiling, X, Y, Z,
        OutputArg, int_const(OutputArgVal)) :-
    (
        ProcName = "plus",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal + YVal
    ;
        ProcName = "+",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal + YVal
    ;
        ProcName = "+",
        ModeNum = 1,
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = X,
        OutputArgVal = ZVal - YVal
    ;
        ProcName = "+",
        ModeNum = 2,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = Y,
        OutputArgVal = ZVal - XVal
    ;
        ProcName = "minus",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal - YVal
    ;
        ProcName = "-",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal - YVal
    ;
        ProcName = "-",
        ModeNum = 1,
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = X,
        OutputArgVal = YVal + ZVal
    ;
        ProcName = "-",
        ModeNum = 2,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = Y,
        OutputArgVal = XVal - ZVal
    ;
        ProcName = "times",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal * YVal
    ;
        ProcName = "*",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal * YVal
    ;
        ProcName = "unchecked_quotient",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        OutputArgVal = unchecked_quotient(XVal, YVal)
    ;
        ProcName = "//",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        OutputArgVal = XVal // YVal
    ;
        ProcName = "mod",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        OutputArgVal = XVal mod YVal
    ;
        ProcName = "rem",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        OutputArgVal = XVal rem YVal
    ;
        ProcName = "unchecked_rem",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        OutputArgVal = unchecked_rem(XVal, YVal)
    ;
        ProcName = "unchecked_left_shift",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = unchecked_left_shift(XVal, YVal)
    ;
        ProcName = "<<",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal << YVal
    ;
        ProcName = "unchecked_right_shift",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = unchecked_right_shift(XVal, YVal)
    ;
        ProcName = ">>",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal >> YVal
    ;
        ProcName = "/\\",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal /\ YVal
    ;
        ProcName = "\\/",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal \/ YVal
    ;
        ProcName = "xor",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = xor(XVal, YVal)
    ).

:- pred evaluate_det_call_float_3(string::in, int::in, bool::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_float_3(ProcName, ModeNum, _CrossCompiling, X, Y, Z,
        OutputArg, float_const(OutputArgVal)) :-
    (
        ProcName = "+",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal + YVal
    ;
        ProcName = "-",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal - YVal
    ;
        ProcName = "*",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal * YVal
    ;
        ProcName = "/",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
        OutputArg = Z,
        YVal \= 0.0,
        OutputArgVal = XVal / YVal
    ;
        ProcName = "unchecked_quotient",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
        OutputArg = Z,
        YVal \= 0.0,
        OutputArgVal = unchecked_quotient(XVal, YVal)
    ).

:- pred evaluate_det_call_string_3(string::in, int::in, bool::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_string_3(ProcName, ModeNum, _CrossCompiling, X, Y, Z,
        OutputArg, string_const(OutputArgVal)) :-
    (
        ( ProcName = "++"
        ; ProcName = "append"
        ),
        % We can only do the append if Z is free (this allows us to ignore
        % the mode number and pick up both the predicate and function versions
        % of append).
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(string_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(string_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal ++ YVal
    ).

%---------------------------------------------------------------------------%

    % evaluate_test(ModuleName, ProcName, ModeNum, Args, Result):
    %
    % This attempts to evaluate a call to
    %   ModuleName.ProcName(Args)
    % whose mode is specified by ModeNum.
    %
    % If the call is a semidet call with no outputs that can be statically
    % evaluated, evaluate_test succeeds with Result being "yes" if the call
    % will succeed and "no" if the call will fail. Otherwise (i.e. if the call
    % is not semidet, has any outputs, or cannot be statically evaluated),
    % evaluate_test fails.
    %
:- pred evaluate_test(string::in, string::in, int::in, list(arg_hlds_info)::in,
    bool::out) is semidet.

    % Integer comparisons

evaluate_test("int", "<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
    ( XVal < YVal ->
        Result = yes
    ;
        Result = no
    ).
evaluate_test("int", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
    ( XVal =< YVal ->
        Result = yes
    ;
        Result = no
    ).
evaluate_test("int", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
    ( XVal > YVal ->
        Result = yes
    ;
        Result = no
    ).
evaluate_test("int", ">=", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
    ( XVal >= YVal ->
        Result = yes
    ;
        Result = no
    ).

    % Float comparisons

evaluate_test("float", "<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
    ( XVal < YVal ->
        Result = yes
    ;
        Result = no
    ).
evaluate_test("float", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
    ( XVal =< YVal ->
        Result = yes
    ;
        Result = no
    ).
evaluate_test("float", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
    ( XVal > YVal ->
        Result = yes
    ;
        Result = no
    ).
evaluate_test("float", ">=", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
    ( XVal >= YVal ->
        Result = yes
    ;
        Result = no
    ).
evaluate_test("private_builtin", "typed_unify", Mode, Args, Result) :-
    % mode 0 is the (in, in) mode
    % mode 1 is the (in, out) mode
    % both modes are semidet
    Mode = 0,
    Args = [TypeOfX, TypeOfY, X, Y],
    eval_unify(TypeOfX, TypeOfY, Result0),
    (
        Result0 = no,
        Result = no
    ;
        Result0 = yes,
        eval_unify(X, Y, Result)
    ).

    % evaluate_semidet_call(ModuleName, ProcName, ModeNum, Args, Result):
    %
    % This attempts to evaluate a call to
    %   ModuleName.ProcName(Args)
    % whose mode is specified by ModeNum.
    %
    % If the call is a semidet call with one output that can be statically
    % evaluated, evaluate_semidet_call succeeds with Result being "no"
    % if the call will fail, or yes(OutputArg - OutputArgValue) if it will
    % succeed, with OutputArg being whichever of the arguments is output,
    % and with OutputArgVal being the computed value of OutputArg.
    %
    % Otherwise (i.e. if the call is not semidet, or has no outputs
    % or more than one output, or cannot be statically evaluated),
    % evaluate_semidet_call fails.

:- type arg_val
    --->    const(cons_id)
    ;       var(arg_hlds_info).

:- pred evaluate_semidet_call(string::in, string::in, int::in,
    list(arg_hlds_info)::in, maybe(pair(arg_hlds_info, arg_val))::out)
    is semidet.

evaluate_semidet_call("builtin", "dynamic_cast", 0, Args, Result) :-
    evaluate_semidet_call("private_builtin", "typed_unify", 1, Args, Result).

evaluate_semidet_call("private_builtin", "typed_unify", Mode, Args, Result) :-
    % mode 0 is the (in, in) mode
    % mode 1 is the (in, out) mode
    % both modes are semidet
    Mode = 1,
    Args = [TypeOfX, TypeOfY, X, Y],
    eval_unify(TypeOfX, TypeOfY, Result0),
    (
        Result0 = no,
        Result = no
    ;
        Result0 = yes,
        Result = yes(Y - var(X))
    ).

    % evaluate_unify(FirstArg, SecondArg, Result):
    %
    % This attempts to evaluate a call to
    %   builtin.unify(FirstArg, SecondArg)
    % with mode (in, in).
    % If the unification can be statically evaluated, evaluate_builtin_test
    % succeeds with Result being "yes" if the unification will succeed
    % and "no" if the unification will fail. Otherwise (i.e. if the unification
    % cannot be statically evaluated), evaluate_unify fails.
    %
:- pred eval_unify(arg_hlds_info::in, arg_hlds_info::in, bool::out) is semidet.

eval_unify(X, Y, Result) :-
    (
        X ^ arg_var = Y ^ arg_var
    ->
        Result = yes
    ;
        X ^ arg_inst = bound(_, _, [bound_functor(XCtor, XArgVars)]),
        Y ^ arg_inst = bound(_, _, [bound_functor(YCtor, YArgVars)])
    ->
        ( XCtor = YCtor, XArgVars = YArgVars ->
            Result = yes
        ;
            ( XCtor \= YCtor
            ; length(XArgVars) \= length(YArgVars) `with_type` int
            )
        ->
            Result = no
        ;
            fail
        )
    ;
        fail
    ).

%---------------------------------------------------------------------------%

:- pred make_assignment_goal(arg_hlds_info::in, arg_hlds_info::in,
    hlds_goal_expr::out, hlds_goal_info::in, hlds_goal_info::out) is det.

make_assignment_goal(OutputArg, InputArg, Goal, !GoalInfo) :-
    make_assignment(OutputArg, InputArg, Goal),
    Delta0 = goal_info_get_instmap_delta(!.GoalInfo),
    instmap_delta_set_var(OutputArg ^ arg_var, InputArg ^ arg_inst,
        Delta0, Delta),
    goal_info_set_instmap_delta(Delta, !GoalInfo),
    goal_info_set_determinism(detism_det, !GoalInfo).

:- pred make_construction_goal(arg_hlds_info::in, cons_id::in,
    hlds_goal_expr::out, hlds_goal_info::in, hlds_goal_info::out) is det.

make_construction_goal(OutputArg, Cons, Goal, !GoalInfo) :-
    make_construction(OutputArg, Cons, Goal),
    Delta0 = goal_info_get_instmap_delta(!.GoalInfo),
    Inst = bound(unique, inst_test_results_fgtc, [bound_functor(Cons, [])]),
    instmap_delta_set_var(OutputArg ^ arg_var, Inst, Delta0, Delta),
    goal_info_set_instmap_delta(Delta, !GoalInfo),
    goal_info_set_determinism(detism_det, !GoalInfo).

:- pred make_assignment(arg_hlds_info::in, arg_hlds_info::in,
    hlds_goal_expr::out) is det.

make_assignment(OutputArg, InputArg, Goal) :-
    OutVar = OutputArg ^ arg_var,
    InVar = InputArg ^ arg_var,
    Inst = InputArg ^ arg_inst,
    OutputArgMode = (free -> Inst),
    InputArgMode = (Inst -> Inst),
    UniMode = OutputArgMode - InputArgMode,
    Context = unify_context(umc_explicit, []),
    Goal = unify(OutVar, rhs_var(InVar), UniMode, assign(OutVar, InVar),
        Context).

    % recompute_instmap_delta is run by simplify.m if anything changes,
    % so the insts are not important here.
    %
:- pred make_construction(arg_hlds_info::in, cons_id::in, hlds_goal_expr::out)
    is det.

make_construction(Arg, ConsId, GoalExpr) :-
    make_const_construction(Arg ^ arg_var, ConsId, hlds_goal(GoalExpr, _)).

%---------------------------------------------------------------------------%

:- pred make_true_or_fail(bool::in, hlds_goal_expr::out) is det.

make_true_or_fail(yes, true_goal_expr).
make_true_or_fail(no, fail_goal_expr).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.const_prop.
%---------------------------------------------------------------------------%
