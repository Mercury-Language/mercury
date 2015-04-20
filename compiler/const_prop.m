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
%---------------------------------------------------------------------------%

:- module transform_hlds.const_prop.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_data.
:- import_module libs.
:- import_module libs.globals.

:- import_module list.

%---------------------------------------------------------------------------%

    % evaluate_call(Globals, VarTypes, Instmap,
    %   ModuleName, ProcName, ModeNum, Args, GoalExpr, !GoalInfo):
    %
    % Try to statically evaluate a call to ModuleName.ProcName(Args)
    % (which may be a call to a predicate or a function) in the mode ModeNum.
    % If the attempt succeeds, return in GoalExpr and the updated GoalInfo
    % a goal that binds the output variables of the call to their statically
    % known values. If the attempt fails, fail.
    %
:- pred evaluate_call(globals::in, vartypes::in, instmap::in,
    string::in, string::in, int::in, list(prog_var)::in,
    hlds_goal_expr::out, hlds_goal_info::in, hlds_goal_info::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module hlds.make_goal.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module libs.int_emu.
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

    % This type groups together all the information we need from the HLDS
    % about a procedure call argument.
    %
:- type arg_hlds_info
    --->    arg_hlds_info(
                arg_var     :: prog_var,
                arg_type    :: mer_type,
                arg_inst    :: mer_inst
            ).

evaluate_call(Globals, VarTypes, InstMap,
        ModuleName, ProcName, ModeNum, Args, GoalExpr, !GoalInfo) :-
    LookupArgs = (func(Var) = arg_hlds_info(Var, Type, Inst) :-
        instmap_lookup_var(InstMap, Var, Inst),
        lookup_var_type(VarTypes, Var, Type)
    ),
    ArgHldsInfos = list.map(LookupArgs, Args),
    evaluate_call_2(Globals, ModuleName, ProcName, ModeNum, ArgHldsInfos,
       GoalExpr, !GoalInfo).

:- pred evaluate_call_2(globals::in, string::in, string::in, int::in,
    list(arg_hlds_info)::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out) is semidet.

evaluate_call_2(Globals, ModuleName, Pred, ModeNum, Args, GoalExpr,
        !GoalInfo) :-
    (
        evaluate_det_call(Globals, ModuleName, Pred, ModeNum,
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
            GoalExpr = fail_goal_expr
        )
    ;
        fail
    ).

%---------------------------------------------------------------------------%

    % evaluate_det_call(Globals, ModuleName, ProcName, ModeNum, Args,
    %   OutputArg, OutputArgVal):
    %
    % This attempts to evaluate a call to
    %   ModuleName.ProcName(Args)
    % in mode ModeNum.
    %
    % If the call is a det call with one output that can be statically
    % evaluated, evaluate_det_call succeeds with OutputArg being whichever of
    % Args is output, and with OutputArgVal being the computed value of
    % OutputArg. Otherwise it fails.
    %
:- pred evaluate_det_call(globals::in, string::in, string::in, int::in,
    list(arg_hlds_info)::in, arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call(Globals, ModuleName, ProcName, ModeNum, Args,
        OutputArg, OutputArgVal) :-
    % Note that many of these functions have predicate versions as well.
    % In every one of those cases, the code we use to evaluate the function
    % version will also evaluate the predicate version, because all the
    % library predicates we evaluate here the same argument sequence for
    % the two versions once the function return values have been put
    % at the end of the argument list. (If the argument orders were different
    % between the two versions for some predicates, we could still evaluate
    % both; we would just need our caller to pass us a pred_or_func
    % indication.)
    (
        Args = [X],
        % Constant functions.
        (
            ModuleName = "int",
            evaluate_det_call_int_1(Globals, ProcName, ModeNum, X,
                OutputArg, OutputArgVal)
        )
    ;
        Args = [X, Y],
        % Unary functions.
        (
            ModuleName = "int",
            evaluate_det_call_int_2(Globals, ProcName, ModeNum, X, Y,
                OutputArg, OutputArgVal)
        ;
            ModuleName = "float",
            evaluate_det_call_float_2(Globals, ProcName, ModeNum, X, Y,
                OutputArg, OutputArgVal)
        ;
            ModuleName = "string",
            evaluate_det_call_string_2(Globals, ProcName, ModeNum, X, Y,
                OutputArg, OutputArgVal)
        )
    ;
        Args = [X, Y, Z],
        % Binary functions.
        (
            ModuleName = "int",
            evaluate_det_call_int_3(Globals, ProcName, ModeNum, X, Y, Z,
                OutputArg, OutputArgVal)
        ;
            ModuleName = "float",
            evaluate_det_call_float_3(Globals, ProcName, ModeNum, X, Y, Z,
                OutputArg, OutputArgVal)
        ;
            ModuleName = "string",
            evaluate_det_call_string_3(Globals, ProcName, ModeNum, X, Y, Z,
                OutputArg, OutputArgVal)
        )
    ).

:- pred evaluate_det_call_int_1(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_int_1(Globals, ProcName, ModeNum, X,
        OutputArg, int_const(OutputArgVal)) :-
    (
        ProcName = "bits_per_int",
        ModeNum = 0,
        OutputArg = X,
        target_bits_per_int(Globals, bits_per_int(OutputArgVal))
    ).

:- pred evaluate_det_call_int_2(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::out, cons_id::out)
    is semidet.

evaluate_det_call_int_2(Globals, ProcName, ModeNum, X, Y,
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
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, 0, XVal, OutputArgVal)
    ;
        ProcName = "\\",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        OutputArgVal = \ XVal
    ;
        ProcName = "floor_to_multiple_of_bits_per_int",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.floor_to_multiple_of_bits_per_int(XVal, BitsPerInt,
            OutputArgVal)
    ;
        ProcName = "quot_bits_per_int",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.quot_bits_per_int(XVal, BitsPerInt, OutputArgVal)
    ;
        ProcName = "times_bits_per_int",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.times_bits_per_int(XVal, BitsPerInt, OutputArgVal)
    ;
        ProcName = "rem_bits_per_int",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.rem_bits_per_int(XVal, BitsPerInt, OutputArgVal)
    ).

:- pred evaluate_det_call_float_2(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::out, cons_id::out)
    is semidet.

evaluate_det_call_float_2(_Globals, ProcName, ModeNum, X, Y,
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

:- pred evaluate_det_call_string_2(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::out,
    cons_id::out) is semidet.

evaluate_det_call_string_2(_Globals, ProcName, ModeNum, X, Y,
        OutputArg, OutputArgVal) :-
    ProcName = "count_codepoints",
    ModeNum = 0,
    X ^ arg_inst = bound(_, _, [bound_functor(string_const(XVal), [])]),
    OutputArg = Y,
    CodePointCountX = string.count_codepoints(XVal),
    OutputArgVal = int_const(CodePointCountX).

:- pred evaluate_det_call_int_3(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_int_3(Globals, ProcName, ModeNum, X, Y, Z,
        OutputArg, int_const(OutputArgVal)) :-
    (
        ProcName = "plus",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.plus(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "+",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.plus(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "+",
        ModeNum = 1,
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = X,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, ZVal, YVal, OutputArgVal)
    ;
        ProcName = "+",
        ModeNum = 2,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = Y,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, ZVal, XVal, OutputArgVal)
    ;
        ProcName = "minus",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "-",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "-",
        ModeNum = 1,
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = X,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.plus(BitsPerInt, YVal, ZVal, OutputArgVal)
    ;
        ProcName = "-",
        ModeNum = 2,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = Y,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, XVal, ZVal, OutputArgVal)
    ;
        ProcName = "times",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.times(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "*",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.times(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_quotient",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.unchecked_quotient(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "//",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.quotient(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "mod",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.mod(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "rem",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.rem(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_rem",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.unchecked_rem(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_left_shift",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.unchecked_left_shift(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "<<",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.left_shift(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_right_shift",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.unchecked_right_shift(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = ">>",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.right_shift(BitsPerInt, XVal, YVal, OutputArgVal)
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

:- pred evaluate_det_call_float_3(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_float_3(_Globals, ProcName, ModeNum, X, Y, Z,
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

:- pred evaluate_det_call_string_3(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_string_3(_Globals, ProcName, ModeNum, X, Y, Z,
        OutputArg, string_const(OutputArgVal)) :-
    (
        ( ProcName = "++"
        ; ProcName = "append"
        ),
        % We can only do the append if Z is free.
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
    % in mode ModeNum.
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

make_construction_goal(OutputArg, Cons, GoalExpr, !GoalInfo) :-
    make_construction_goal_expr(OutputArg, Cons, GoalExpr),
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
:- pred make_construction_goal_expr(arg_hlds_info::in, cons_id::in,
    hlds_goal_expr::out) is det.

make_construction_goal_expr(Arg, ConsId, GoalExpr) :-
    make_const_construction(Arg ^ arg_var, ConsId, Goal),
    % We ignore the generic goal info returned by make_const_construction;
    % our caller will construct a goal info that is specialized to the
    % call being replaced.
    Goal = hlds_goal(GoalExpr, _).

%---------------------------------------------------------------------------%

:- pred make_true_or_fail(bool::in, hlds_goal_expr::out) is det.

make_true_or_fail(yes, true_goal_expr).
make_true_or_fail(no, fail_goal_expr).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.const_prop.
%---------------------------------------------------------------------------%
