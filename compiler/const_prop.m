%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2008, 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
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

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

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

:- import_module hlds.make_goal.
:- import_module libs.int_emu.
:- import_module libs.options.
:- import_module libs.uint_emu.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

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
    LookupArgs =
        ( func(Var) = arg_hlds_info(Var, Type, Inst) :-
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
    ( if
        evaluate_det_call(Globals, ModuleName, Pred, ModeNum,
            Args, OutputArg, Cons)
    then
        make_construction_goal(OutputArg, Cons, GoalExpr, !GoalInfo)
    else if
        evaluate_test(ModuleName, Pred, ModeNum, Args, Succeeded)
    then
        make_true_or_fail(Succeeded, GoalExpr)
    else if
        evaluate_semidet_call(ModuleName, Pred, ModeNum, Args, Result)
    then
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
    else
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
        ;
            ModuleName = "uint",
            evaluate_det_call_uint_1(Globals, ProcName, ModeNum, X,
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
            ModuleName = "uint",
            evaluate_det_call_uint_2(Globals, ProcName, ModeNum, X, Y,
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
            ModuleName = "uint",
            evaluate_det_call_uint_3(Globals, ProcName, ModeNum, X, Y ,Z,
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
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        target_bits_per_int(Globals, bits_per_int(OutputArgVal))
    ).

:- pred evaluate_det_call_uint_1(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_uint_1(Globals, ProcName, ModeNum, X, OutputArg, ConsId) :-
    (
        ProcName = "bits_per_uint",
        ModeNum = 0,
        OutputArg = X,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        target_bits_per_uint(Globals, bits_per_uint(OutputArgVal)),
        % NOTE: this returns an int not a uint.
        ConsId = int_const(OutputArgVal)
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
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, 0, XVal, OutputArgVal)
    ;
        ProcName = "\\",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        target_bits_per_int(Globals, TargetBitsPerInt),
        TargetBitsPerInt = bits_per_int(int.bits_per_int),
        OutputArg = Y,
        OutputArgVal = \ XVal
    ;
        ProcName = "floor_to_multiple_of_bits_per_int",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.floor_to_multiple_of_bits_per_int(XVal, BitsPerInt,
            OutputArgVal)
    ;
        ProcName = "quot_bits_per_int",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.quot_bits_per_int(XVal, BitsPerInt, OutputArgVal)
    ;
        ProcName = "times_bits_per_int",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.times_bits_per_int(XVal, BitsPerInt, OutputArgVal)
    ;
        ProcName = "rem_bits_per_int",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        OutputArg = Y,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.rem_bits_per_int(XVal, BitsPerInt, OutputArgVal)
    ).

:- pred evaluate_det_call_uint_2(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::out, cons_id::out)
    is semidet.

evaluate_det_call_uint_2(Globals, ProcName, ModeNum, X, Y,
        OutputArg, uint_const(OutputArgVal)) :-
    (
        ProcName = "\\",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        target_bits_per_uint(Globals, TargetBitsPerUInt),
        TargetBitsPerUInt = bits_per_uint(uint.bits_per_uint),
        OutputArg = Y,
        OutputArgVal = \ XVal
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
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.plus(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "+",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.plus(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "+",
        ModeNum = 1,
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = X,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, ZVal, YVal, OutputArgVal)
    ;
        ProcName = "+",
        ModeNum = 2,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = Y,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, ZVal, XVal, OutputArgVal)
    ;
        ProcName = "minus",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "-",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "-",
        ModeNum = 1,
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = X,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.plus(BitsPerInt, YVal, ZVal, OutputArgVal)
    ;
        ProcName = "-",
        ModeNum = 2,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = Y,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.minus(BitsPerInt, XVal, ZVal, OutputArgVal)
    ;
        ProcName = "times",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.times(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "*",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.times(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_quotient",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.unchecked_quotient(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "//",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.quotient(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "mod",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.mod(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "rem",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.rem(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_rem",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        YVal \= 0,
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.unchecked_rem(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_left_shift",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.unchecked_left_shift(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "<<",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.left_shift(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_right_shift",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        int_emu.target_bits_per_int(Globals, BitsPerInt),
        int_emu.unchecked_right_shift(BitsPerInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = ">>",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
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
    ;
        ProcName = "xor",
        ModeNum = 1,
        X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = Y,
        OutputArgVal = xor(XVal, ZVal)
    ;
        ProcName = "xor",
        ModeNum = 2,
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(int_const(ZVal), [])]),
        OutputArg = X,
        OutputArgVal = xor(YVal, ZVal)
    ).

:- pred evaluate_det_call_uint_3(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_uint_3(Globals, ProcName, ModeNum, X, Y, Z,
        OutputArg, uint_const(OutputArgVal)) :-
    (
        ProcName = "+",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.plus(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "+",
        ModeNum = 1,
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(uint_const(ZVal), [])]),
        OutputArg = X,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.minus(BitsPerUInt, ZVal, YVal, OutputArgVal)
    ;
        ProcName = "+",
        ModeNum = 2,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(uint_const(ZVal), [])]),
        OutputArg = Y,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.minus(BitsPerUInt, ZVal, XVal, OutputArgVal)
    ;
        ProcName = "-",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.minus(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "-",
        ModeNum = 1,
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(uint_const(ZVal), [])]),
        OutputArg = X,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.plus(BitsPerUInt, YVal, ZVal, OutputArgVal)
    ;
        ProcName = "-",
        ModeNum = 2,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(uint_const(ZVal), [])]),
        OutputArg = Y,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.minus(BitsPerUInt, XVal, ZVal, OutputArgVal)
    ;
        ProcName = "*",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.times(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "//",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        YVal \= 0u,
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.quotient(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "mod",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        YVal \= 0u,
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.mod(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "rem",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        YVal \= 0u,
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.rem(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_rem",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        YVal \= 0u,
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.unchecked_rem(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_left_shift",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.unchecked_left_shift(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "<<",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.left_shift(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "unchecked_right_shift",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.unchecked_right_shift(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = ">>",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
        OutputArg = Z,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        uint_emu.target_bits_per_uint(Globals, BitsPerUInt),
        uint_emu.right_shift(BitsPerUInt, XVal, YVal, OutputArgVal)
    ;
        ProcName = "/\\",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal /\ YVal
    ;
        ProcName = "\\/",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = XVal \/ YVal
    ;
        ProcName = "xor",
        ModeNum = 0,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        OutputArg = Z,
        OutputArgVal = xor(XVal, YVal)
    ;
        ProcName = "xor",
        ModeNum = 1,
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(uint_const(ZVal), [])]),
        OutputArg = Y,
        OutputArgVal = xor(XVal, ZVal)
    ;
        ProcName = "xor",
        ModeNum = 2,
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(uint_const(ZVal), [])]),
        OutputArg = X,
        OutputArgVal = xor(YVal, ZVal)
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
    ( if XVal < YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
    ( if XVal =< YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
    ( if XVal > YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int", ">=", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int_const(YVal), [])]),
    ( if XVal >= YVal then
        Result = yes
    else
        Result = no
    ).

    % 8-bit signed integer comparison.

evaluate_test("int8", "<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int8_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int8_const(YVal), [])]),
    ( if XVal < YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int8", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int8_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int8_const(YVal), [])]),
    ( if XVal =< YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int8", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int8_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int8_const(YVal), [])]),
    ( if XVal > YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int8", ">=", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int8_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int8_const(YVal), [])]),
    ( if XVal >= YVal then
        Result = yes
    else
        Result = no
    ).

    % 16-bit signed integer comparison.

evaluate_test("int16", "<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int16_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int16_const(YVal), [])]),
    ( if XVal < YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int16", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int16_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int16_const(YVal), [])]),
    ( if XVal =< YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int16", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int16_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int16_const(YVal), [])]),
    ( if XVal > YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int16", ">=", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int16_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int16_const(YVal), [])]),
    ( if XVal >= YVal then
        Result = yes
    else
        Result = no
    ).

    % 32-bit signed integer comparison.

evaluate_test("int32", "<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int32_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int32_const(YVal), [])]),
    ( if XVal < YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int32", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int32_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int32_const(YVal), [])]),
    ( if XVal =< YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int32", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int32_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int32_const(YVal), [])]),
    ( if XVal > YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int32", ">=", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int32_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int32_const(YVal), [])]),
    ( if XVal >= YVal then
        Result = yes
    else
        Result = no
    ).

    % 64-bit signed integer comparison.

evaluate_test("int64", "<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int64_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int64_const(YVal), [])]),
    ( if XVal < YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int64", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int64_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int64_const(YVal), [])]),
    ( if XVal =< YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int64", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int64_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int64_const(YVal), [])]),
    ( if XVal > YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("int64", ">=", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(int64_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(int64_const(YVal), [])]),
    ( if XVal >= YVal then
        Result = yes
    else
        Result = no
    ).

    % Unsigned integer comparisons.

evaluate_test("uint", "<", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
    ( if YVal = 0u then
        % Special case: there are no uints that are < 0u.
        Result = no
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        ( if XVal < YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
    ( if XVal = 0u then
        % Special case: 0u =< all uints.
        Result = yes
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        ( if XVal =< YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
    ( if XVal = 0u then
        % Special case: 0u > than no uints.
        Result = no
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
        ( if XVal > YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint", ">=", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint_const(YVal), [])]),
    ( if YVal = 0u then
        % Special case: all uints are >= 0u.
        Result = yes
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint_const(XVal), [])]),
        ( if XVal >= YVal then
            Result = yes
        else
            Result = no
        )
    ).

    % 8-bit unsigned integer comparisons.

evaluate_test("uint8", "<", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint8_const(YVal), [])]),
    ( if YVal = 0u8 then
        % Special case: there are no uint8s that are < 0u8.
        Result = no
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint8_const(XVal), [])]),
        ( if XVal < YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint8", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint8_const(XVal), [])]),
    ( if XVal = 0u8 then
        % Special case: 0u8 =< all uint8s.
        Result = yes
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint8_const(YVal), [])]),
        ( if XVal =< YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint8", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint8_const(XVal), [])]),
    ( if XVal = 0u8 then
        % Special case: 0u8 > than no uint8s.
        Result = no
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint8_const(YVal), [])]),
        ( if XVal > YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint8", ">=", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint8_const(YVal), [])]),
    ( if YVal = 0u8 then
        % Special case: all uint8s are >= 0u8.
        Result = yes
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint8_const(XVal), [])]),
        ( if XVal >= YVal then
            Result = yes
        else
            Result = no
        )
    ).

    % 16-bit unsigned integer comparisons.

evaluate_test("uint16", "<", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint16_const(YVal), [])]),
    ( if YVal = 0u16 then
        % Special case: there are no uint16s that are < 0u16.
        Result = no
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint16_const(XVal), [])]),
        ( if XVal < YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint16", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint16_const(XVal), [])]),
    ( if XVal = 0u16 then
        % Special case: 0u16 =< all uint16s.
        Result = yes
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint16_const(YVal), [])]),
        ( if XVal =< YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint16", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint16_const(XVal), [])]),
    ( if XVal = 0u16 then
        % Special case: 0u16 > than no uint16s.
        Result = no
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint16_const(YVal), [])]),
        ( if XVal > YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint16", ">=", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint16_const(YVal), [])]),
    ( if YVal = 0u16 then
        % Special case: all uint16s are >= 0u16.
        Result = yes
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint16_const(XVal), [])]),
        ( if XVal >= YVal then
            Result = yes
        else
            Result = no
        )
    ).

    % 32-bit unsigned integer comparisons.

evaluate_test("uint32", "<", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint32_const(YVal), [])]),
    ( if YVal = 0u32 then
        % Special case: there are no uint32s that are < 0u32.
        Result = no
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint32_const(XVal), [])]),
        ( if XVal < YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint32", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint32_const(XVal), [])]),
    ( if XVal = 0u32 then
        % Special case: 0u32 =< all uint32s.
        Result = yes
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint32_const(YVal), [])]),
        ( if XVal =< YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint32", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint32_const(XVal), [])]),
    ( if XVal = 0u32 then
        % Special case: 0u32 > than no uint32s.
        Result = no
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint32_const(YVal), [])]),
        ( if XVal > YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint32", ">=", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint32_const(YVal), [])]),
    ( if YVal = 0u32 then
        % Special case: all uint32s are >= 0u32.
        Result = yes
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint32_const(XVal), [])]),
        ( if XVal >= YVal then
            Result = yes
        else
            Result = no
        )
    ).

    % 64-bit unsigned integer comparisons.

evaluate_test("uint64", "<", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint64_const(YVal), [])]),
    ( if YVal = 0u64 then
        % Special case: there are no uint64s that are < 0u64.
        Result = no
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint64_const(XVal), [])]),
        ( if XVal < YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint64", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint64_const(XVal), [])]),
    ( if XVal = 0u64 then
        % Special case: 0u64 =< all uint64s.
        Result = yes
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint64_const(YVal), [])]),
        ( if XVal =< YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint64", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(uint64_const(XVal), [])]),
    ( if XVal = 0u64 then
        % Special case: 0u64 > than no uint64s.
        Result = no
    else
        Y ^ arg_inst = bound(_, _, [bound_functor(uint64_const(YVal), [])]),
        ( if XVal > YVal then
            Result = yes
        else
            Result = no
        )
    ).
evaluate_test("uint64", ">=", 0, Args, Result) :-
    Args = [X, Y],
    Y ^ arg_inst = bound(_, _, [bound_functor(uint64_const(YVal), [])]),
    ( if YVal = 0u64 then
        % Special case: all uint64s are >= 0u64.
        Result = yes
    else
        X ^ arg_inst = bound(_, _, [bound_functor(uint64_const(XVal), [])]),
        ( if XVal >= YVal then
            Result = yes
        else
            Result = no
        )
    ).

    % Float comparisons

evaluate_test("float", "<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
    ( if XVal < YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("float", "=<", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
    ( if XVal =< YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("float", ">", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
    ( if XVal > YVal then
        Result = yes
    else
        Result = no
    ).
evaluate_test("float", ">=", 0, Args, Result) :-
    Args = [X, Y],
    X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
    ( if XVal >= YVal then
        Result = yes
    else
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

%---------------------------------------------------------------------------%

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
    ( if
        X ^ arg_var = Y ^ arg_var
    then
        Result = yes
    else if
        X ^ arg_inst = bound(_, _, [bound_functor(XCtor, XArgVars)]),
        Y ^ arg_inst = bound(_, _, [bound_functor(YCtor, YArgVars)])
    then
        ( if
            XCtor = YCtor,
            XArgVars = YArgVars
        then
            Result = yes
        else if
            ( XCtor \= YCtor
            ; length(XArgVars) \= length(YArgVars) `with_type` int
            )
        then
            Result = no
        else
            fail
        )
    else
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
    UnifyMode = unify_modes_li_lf_ri_rf(free, Inst, Inst, Inst),
    Context = unify_context(umc_explicit, []),
    Goal = unify(OutVar, rhs_var(InVar), UnifyMode, assign(OutVar, InVar),
        Context).

    % recompute_instmap_delta is run by simplify.m if anything changes,
    % so the insts are not important here.
    %
:- pred make_construction_goal_expr(arg_hlds_info::in, cons_id::in,
    hlds_goal_expr::out) is det.

make_construction_goal_expr(Arg, ConsId, GoalExpr) :-
    % We ignore the generic goal info returned by make_const_construction;
    % our caller will construct a goal_info that is specialized to the
    % call being replaced.
    make_const_construction(term.context_init, Arg ^ arg_var, ConsId, Goal),
    Goal = hlds_goal(GoalExpr, _).

%---------------------------------------------------------------------------%

:- pred make_true_or_fail(bool::in, hlds_goal_expr::out) is det.

make_true_or_fail(yes, true_goal_expr).
make_true_or_fail(no, fail_goal_expr).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.const_prop.
%---------------------------------------------------------------------------%
