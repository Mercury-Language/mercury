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
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module list.

%---------------------------------------------------------------------------%

    % evaluate_call(Globals, VarTable, Instmap,
    %   ModuleName, ProcName, ModeNum, Args, GoalExpr, !GoalInfo):
    %
    % Try to statically evaluate a call to ModuleName.ProcName(Args)
    % (which may be a call to a predicate or a function) in the mode ModeNum.
    % If the attempt succeeds, return in GoalExpr and the updated GoalInfo
    % a goal that binds the output variables of the call to their statically
    % known values. If the attempt fails, fail.
    %
:- pred evaluate_call(globals::in, var_table::in, instmap::in,
    string::in, string::in, int::in, list(prog_var)::in,
    hlds_goal_expr::out, hlds_goal_info::in, hlds_goal_info::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.make_goal.
:- import_module libs.options.
:- import_module parse_tree.int_emu.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module int8.
:- import_module integer.
:- import_module string.
:- import_module term_context.
:- import_module uint.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.
:- import_module uint8.

%---------------------------------------------------------------------------%

evaluate_call(Globals, VarTable, InstMap,
        ModuleName, ProcName, ModeNum, ArgVars, GoalExpr, !GoalInfo) :-
    lookup_arg_vars(VarTable, InstMap, ArgVars, ArgInfos),
    ( if
        evaluate_det_call(Globals, ModuleName, ProcName, ModeNum,
            ArgInfos, OutputArg, Cons)
    then
        make_construction_goal(OutputArg, Cons, GoalExpr, !GoalInfo)
    else if
        evaluate_test(ModuleName, ProcName, ModeNum, ArgInfos, Succeeded)
    then
        make_true_or_fail(Succeeded, GoalExpr)
    else if
        evaluate_semidet_call(ModuleName, ProcName, ModeNum, ArgInfos, Result)
    then
        (
            Result = semi_call_failure,
            GoalExpr = fail_goal_expr
        ;
%           Result = semi_call_binds_to_const(OutputArg, Cons),
%           make_construction_goal(OutputArg, Cons, GoalExpr, !GoalInfo)
%       ;
            Result = semi_call_binds_to_var(OutputArg, InputArg),
            make_assignment_goal(OutputArg, InputArg, GoalExpr, !GoalInfo)
        )
    else
        fail
    ).

    % This type groups together all the information we need from the HLDS
    % about a procedure call argument.
    %
:- type arg_hlds_info
    --->    arg_hlds_info(
                arg_var     :: prog_var,
                arg_type    :: mer_type,
                arg_inst    :: mer_inst
            ).

:- pred lookup_arg_vars(var_table::in, instmap::in,
    list(prog_var)::in, list(arg_hlds_info)::out) is det.

lookup_arg_vars(_VarTable, _InstMap, [], []).
lookup_arg_vars(VarTable, InstMap, [Var | Vars], [Info | Infos]) :-
    lookup_var_type(VarTable, Var, Type),
    instmap_lookup_var(InstMap, Var, Inst),
    Info = arg_hlds_info(Var, Type, Inst),
    lookup_arg_vars(VarTable, InstMap, Vars, Infos).

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
    % library predicates we evaluate here have the same argument sequence
    % for the two versions once the function return values have been put
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
            ( ModuleName = "int",    OpType = op_int(target_op_type(Globals))
            ; ModuleName = "int8",   OpType = op_int(bits_8)
            ; ModuleName = "int16",  OpType = op_int(bits_16)
            ; ModuleName = "int32",  OpType = op_int(bits_32)
            ; ModuleName = "int64",  OpType = op_int(bits_64)
            ; ModuleName = "uint",   OpType = op_uint(target_op_type(Globals))
            ; ModuleName = "uint8",  OpType = op_uint(bits_8)
            ; ModuleName = "uint16", OpType = op_uint(bits_16)
            ; ModuleName = "uint32", OpType = op_uint(bits_32)
            ; ModuleName = "uint64", OpType = op_uint(bits_64)
            ),
            evaluate_det_call_int_uint_2(Globals, OpType,
                ProcName, ModeNum, X, Y, OutputArg, OutputArgVal)
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
            ( ModuleName = "int",    OpType = op_int(target_op_type(Globals))
            ; ModuleName = "int8",   OpType = op_int(bits_8)
            ; ModuleName = "int16",  OpType = op_int(bits_16)
            ; ModuleName = "int32",  OpType = op_int(bits_32)
            ; ModuleName = "int64",  OpType = op_int(bits_64)
            ; ModuleName = "uint",   OpType = op_uint(target_op_type(Globals))
            ; ModuleName = "uint8",  OpType = op_uint(bits_8)
            ; ModuleName = "uint16", OpType = op_uint(bits_16)
            ; ModuleName = "uint32", OpType = op_uint(bits_32)
            ; ModuleName = "uint64", OpType = op_uint(bits_64)
            ),
            (
                ModeNum = 0,
                evaluate_det_call_int_uint_3_mode_0(Globals, OpType,
                    ProcName, X, Y, Z, OutputArg, OutputArgVal)
            ;
                ModeNum = 1,
                evaluate_det_call_int_uint_3_mode_1(Globals, OpType,
                    ProcName, X, Y, Z, OutputArg, OutputArgVal)
            ;
                ModeNum = 2,
                evaluate_det_call_int_uint_3_mode_2(Globals, OpType,
                    ProcName, X, Y, Z, OutputArg, OutputArgVal)
            )
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

%---------------------%

:- pred evaluate_det_call_int_1(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_int_1(Globals, ProcName, ModeNum, X, OutputArg, ConsId) :-
    (
        ( ProcName = "bits_per_int"
        ; ProcName = "ubits_per_int"
        ),
        ModeNum = 0,
        OutputArg = X,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        target_word_bits(Globals) = WordBits,
        (
            ProcName = "bits_per_int",
            ConsId = some_int_const(int_const(WordBits))
        ;
            ProcName = "ubits_per_int",
            ConsId = some_int_const(uint_const(uint.det_from_int(WordBits)))
        )
    ).

:- pred evaluate_det_call_uint_1(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_uint_1(Globals, ProcName, ModeNum, X, OutputArg, ConsId) :-
    (
        ( ProcName = "bits_per_uint"
        ; ProcName = "ubits_per_uint"
        ),
        ModeNum = 0,
        OutputArg = X,
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        target_word_bits(Globals) = WordBits,
        (
            ProcName = "bits_per_uint",
            ConsId = some_int_const(int_const(WordBits))
        ;
            ProcName = "ubits_per_uint",
            ConsId = some_int_const(uint_const(uint.det_from_int(WordBits)))
        )
    ).

%---------------------%

:- pred evaluate_det_call_int_uint_2(globals::in, op_type::in,
    string::in, int::in, arg_hlds_info::in,
    arg_hlds_info::in, arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_int_uint_2(Globals, OpType, ProcName, ModeNum, X, Y,
        OutputArg, some_int_const(OutputArgVal)) :-
    ModeNum = 0,
    X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
    FunctorX = some_int_const(ConstX),
    OutputArg = Y,
    % All of the int{,8,16,32,64} modules define unary plus, unary minus,
    % and all of the {int,uint}{,8,16,32,64} modules define bitwise negation.
    % The other procedures below are defined only in int.m; this is checked
    % by the predicates that emulate them.
    (
        ProcName = "+",
        OpType = op_int(_),
        OutputArgVal = ConstX
    ;
        ProcName = "-",
        OpType = op_int(_),
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        is_integer_for_op_type(OpType, FunctorX, IntegerX),
        emu_minus(OpType, zero, IntegerX, OutputArgVal)
    ;
        ProcName = "\\",
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        emu_not(OpType, FunctorX, OutputArgVal)
    ;
        ProcName = "floor_to_multiple_of_bits_per_int",
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        emu_int_floor_to_multiple_of_bits_per_int(OpType, ConstX, OutputArgVal)
    ;
        ProcName = "quot_bits_per_int",
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        emu_int_quot_bits_per_int(OpType, ConstX, OutputArgVal)
    ;
        ProcName = "times_bits_per_int",
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        emu_int_times_bits_per_int(OpType, ConstX, OutputArgVal)
    ;
        ProcName = "rem_bits_per_int",
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        emu_int_rem_bits_per_int(OpType, ConstX, OutputArgVal)
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
    ( ProcName = "count_codepoints"
    ; ProcName = "count_code_points"
    ),
    ModeNum = 0,
    X ^ arg_inst = bound(_, _, [bound_functor(string_const(XVal), [])]),
    OutputArg = Y,
    CodePointCountX = string.count_code_points(XVal),
    OutputArgVal = some_int_const(int_const(CodePointCountX)).

%---------------------%

:- pred evaluate_det_call_int_uint_3_mode_0(globals::in, op_type::in,
    string::in, arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_int_uint_3_mode_0(Globals, OpType, ProcName, X, Y, Z,
        OutputArg, some_int_const(OutputArgVal)) :-
    X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
    % Note that we optimize calls to the checked and unchecked versions
    % of operations such as // and << under the same conditions: when
    % the checked version's checks would succeed.
    %
    % For the checked operations, we do this because we don't want
    % this optimization to replace a check failure leading to a runtime abort
    % with silently returning a nonsense result *without* an abort.
    %
    % For the unchecked operations, we do this because there is no point
    % in trying to optimize operations that *will* return a nonsense result,
    % thus creating a landmine that will go off sometime later in the
    % program's execution (unless if user is unlucky, and he/she just
    % silently gets nonsense output).
    (
        ( ProcName = "+"    ; ProcName = "plus"
        ; ProcName = "-"    ; ProcName = "minus"
        ; ProcName = "*"    ; ProcName = "times"
        ; ProcName = "//"   ; ProcName = "/"
        ; ProcName = "unchecked_quotient"
        ; ProcName = "mod"
        ; ProcName = "rem"  ; ProcName = "unchecked_rem"
        ),
        is_integer_for_op_type(OpType, FunctorX, IntegerX),
        is_integer_for_op_type(OpType, FunctorY, IntegerY),
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        require_complete_switch [ProcName]
        (
            ( ProcName = "+" ; ProcName = "plus" ),
            emu_plus(OpType, IntegerX, IntegerY, OutputArgVal)
        ;
            ( ProcName = "-" ; ProcName = "minus" ),
            emu_minus(OpType, IntegerX, IntegerY, OutputArgVal)
        ;
            ( ProcName = "*" ; ProcName = "times" ),
            emu_times(OpType, IntegerX, IntegerY, OutputArgVal)
        ;
            ( ProcName = "//"
            ; ProcName = "/"
            ; ProcName = "unchecked_quotient"
            ),
            emu_quotient(OpType, IntegerX, IntegerY, OutputArgVal)
        ;
            ProcName = "mod",
            emu_mod(OpType, IntegerX, IntegerY, OutputArgVal)
        ;
            ( ProcName = "rem" ; ProcName = "unchecked_rem" ),
            emu_rem(OpType, IntegerX, IntegerY, OutputArgVal)
        )
    ;
        ( ProcName = "<<"
        ; ProcName = "<<u"
        ; ProcName = ">>"
        ; ProcName = ">>u"
        ; ProcName = "unchecked_left_shift"
        ; ProcName = "unchecked_left_ushift"
        ; ProcName = "unchecked_right_shift"
        ; ProcName = "unchecked_right_ushift"
        ),
        is_integer_for_op_type(OpType, FunctorX, IntegerX),
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        % Regardless of size of the X operand (the value to be shifted),
        % the Y operand (the shift amount) is always word sized, though
        % there are operation versions where it is an int, and versions
        % where it is a uint.
        require_complete_switch [ProcName]
        (
            ( ProcName = "<<"
            ; ProcName = ">>"
            ; ProcName = "unchecked_left_shift"
            ; ProcName = "unchecked_right_shift"
            ),
            FunctorY = some_int_const(int_const(IntY))
        ;
            ( ProcName = "<<u"
            ; ProcName = ">>u"
            ; ProcName = "unchecked_left_ushift"
            ; ProcName = "unchecked_right_ushift"
            ),
            FunctorY = some_int_const(uint_const(UIntY)),
            IntY = uint.cast_to_int(UIntY)
        ),
        require_complete_switch [ProcName]
        (
            ( ProcName = "<<"
            ; ProcName = "<<u"
            ; ProcName = "unchecked_left_shift"
            ; ProcName = "unchecked_left_ushift"
            ),
            emu_left_shift(OpType, IntegerX, IntY, OutputArgVal)
        ;
            ( ProcName = ">>"
            ; ProcName = ">>u"
            ; ProcName = "unchecked_right_shift"
            ; ProcName = "unchecked_right_ushift"
            ),
            emu_right_shift(OpType, IntegerX, IntY, OutputArgVal)
        )
    ;
        ProcName = "/\\",
        emu_and(OpType, FunctorX, FunctorY, OutputArgVal)
    ;
        ProcName = "\\/",
        emu_or(OpType, FunctorX, FunctorY, OutputArgVal)
    ;
        ProcName = "xor",
        emu_xor(OpType, FunctorX, FunctorY, OutputArgVal)
    ),
    OutputArg = Z.

:- pred evaluate_det_call_int_uint_3_mode_1(globals::in, op_type::in,
    string::in, arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_int_uint_3_mode_1(Globals, OpType, ProcName, X, Y, Z,
        OutputArg, some_int_const(OutputArgVal)) :-
    (
        ProcName = "+",
        Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(FunctorZ, [])]),
        is_integer_for_op_type(OpType, FunctorY, IntegerY),
        is_integer_for_op_type(OpType, FunctorZ, IntegerZ),
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        OutputArg = X,
        emu_minus(OpType, IntegerZ, IntegerY, OutputArgVal)
    ;
        ProcName = "-",
        Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(FunctorZ, [])]),
        is_integer_for_op_type(OpType, FunctorY, IntegerY),
        is_integer_for_op_type(OpType, FunctorZ, IntegerZ),
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        OutputArg = X,
        emu_plus(OpType, IntegerY, IntegerZ, OutputArgVal)
    ;
        ProcName = "xor",
        X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(FunctorZ, [])]),
        OutputArg = Y,
        emu_xor(OpType, FunctorX, FunctorZ, OutputArgVal)
    ).

:- pred evaluate_det_call_int_uint_3_mode_2(globals::in, op_type::in,
    string::in, arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_int_uint_3_mode_2(Globals, OpType, ProcName, X, Y, Z,
        OutputArg, some_int_const(OutputArgVal)) :-
    (
        ProcName = "+",
        X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(FunctorZ, [])]),
        is_integer_for_op_type(OpType, FunctorX, IntegerX),
        is_integer_for_op_type(OpType, FunctorZ, IntegerZ),
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        OutputArg = Y,
        emu_minus(OpType, IntegerZ, IntegerX, OutputArgVal)
    ;
        ProcName = "-",
        X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(FunctorZ, [])]),
        is_integer_for_op_type(OpType, FunctorX, IntegerX),
        is_integer_for_op_type(OpType, FunctorZ, IntegerZ),
        globals.lookup_bool_option(Globals, pregenerated_dist, no),
        OutputArg = Y,
        emu_minus(OpType, IntegerX, IntegerZ, OutputArgVal)
    ;
        ProcName = "xor",
        Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
        Z ^ arg_inst = bound(_, _, [bound_functor(FunctorZ, [])]),
        OutputArg = X,
        emu_xor(OpType, FunctorY, FunctorZ, OutputArgVal)
    ).

%---------------------%

:- pred evaluate_det_call_float_3(globals::in, string::in, int::in,
    arg_hlds_info::in, arg_hlds_info::in, arg_hlds_info::in,
    arg_hlds_info::out, cons_id::out) is semidet.

evaluate_det_call_float_3(_Globals, ProcName, ModeNum, X, Y, Z,
        OutputArg, float_const(OutputArgVal)) :-
    ModeNum = 0,
    X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
    Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
    OutputArg = Z,
    (
        ProcName = "+",
        OutputArgVal = XVal + YVal
    ;
        ProcName = "-",
        OutputArgVal = XVal - YVal
    ;
        ProcName = "*",
        OutputArgVal = XVal * YVal
    ;
        ProcName = "/",
        YVal \= 0.0,
        OutputArgVal = XVal / YVal
    ;
        ProcName = "unchecked_quotient",
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

evaluate_test(ModuleName, PredName, ModeNum, Args, Result) :-
    (
        ModuleName = "int",
        % Signed integer comparisons.
        Args = [X, Y],
        X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
        FunctorX = some_int_const(int_const(XVal)),
        FunctorY = some_int_const(int_const(YVal)),
        (
            PredName = "<", ModeNum = 0,
            ( if XVal < YVal then Result = yes else Result = no )
        ;
            PredName = "=<", ModeNum = 0,
            ( if XVal =< YVal then Result = yes else Result = no )
        ;
            PredName = ">", ModeNum = 0,
            ( if XVal > YVal then Result = yes else Result = no )
        ;
            PredName = ">=", ModeNum = 0,
            ( if XVal >= YVal then Result = yes else Result = no )
        )
    ;
        ModuleName = "int8",
        % 8-bit signed integer comparisons.
        Args = [X, Y],
        X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
        FunctorX = some_int_const(int8_const(XVal)),
        FunctorY = some_int_const(int8_const(YVal)),
        (
            PredName = "<", ModeNum = 0,
            ( if XVal < YVal then Result = yes else Result = no )
        ;
            PredName = "=<", ModeNum = 0,
            ( if XVal =< YVal then Result = yes else Result = no )
        ;
            PredName = ">", ModeNum = 0,
            ( if XVal > YVal then Result = yes else Result = no )
        ;
            PredName = ">=", ModeNum = 0,
            ( if XVal >= YVal then Result = yes else Result = no )
        )
    ;
        ModuleName = "int16",
        % 16-bit signed integer comparisons.
        Args = [X, Y],
        X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
        FunctorX = some_int_const(int16_const(XVal)),
        FunctorY = some_int_const(int16_const(YVal)),
        (
            PredName = "<", ModeNum = 0,
            ( if XVal < YVal then Result = yes else Result = no )
        ;
            PredName = "=<", ModeNum = 0,
            ( if XVal =< YVal then Result = yes else Result = no )
        ;
            PredName = ">", ModeNum = 0,
            ( if XVal > YVal then Result = yes else Result = no )
        ;
            PredName = ">=", ModeNum = 0,
            ( if XVal >= YVal then Result = yes else Result = no )
        )
    ;
        ModuleName = "int32",
        % 32-bit signed integer comparisons.
        Args = [X, Y],
        X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
        FunctorX = some_int_const(int32_const(XVal)),
        FunctorY = some_int_const(int32_const(YVal)),
        (
            PredName = "<", ModeNum = 0,
            ( if XVal < YVal then Result = yes else Result = no )
        ;
            PredName = "=<", ModeNum = 0,
            ( if XVal =< YVal then Result = yes else Result = no )
        ;
            PredName = ">", ModeNum = 0,
            ( if XVal > YVal then Result = yes else Result = no )
        ;
            PredName = ">=", ModeNum = 0,
            ( if XVal >= YVal then Result = yes else Result = no )
        )
    ;
        ModuleName = "int64",
        % 64-bit signed integer comparisons.
        Args = [X, Y],
        X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
        FunctorX = some_int_const(int64_const(XVal)),
        FunctorY = some_int_const(int64_const(YVal)),
        (
            PredName = "<", ModeNum = 0,
            ( if XVal < YVal then Result = yes else Result = no )
        ;
            PredName = "=<", ModeNum = 0,
            ( if XVal =< YVal then Result = yes else Result = no )
        ;
            PredName = ">", ModeNum = 0,
            ( if XVal > YVal then Result = yes else Result = no )
        ;
            PredName = ">=", ModeNum = 0,
            ( if XVal >= YVal then Result = yes else Result = no )
        )
    ;
        ModuleName = "uint",
        % Unsigned integer comparisons.
        Args = [X, Y],
        (
            PredName = "<", ModeNum = 0,
            Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint_const(YVal)),
            ( if YVal = 0u then
                % Special case: no uints are < 0u.
                Result = no
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint_const(XVal)),
                ( if XVal < YVal then Result = yes else Result = no )
            )
        ;
            PredName = "=<", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint_const(XVal)),
            ( if XVal = 0u then
                % Special case: 0u =< all uints.
                Result = yes
            else
                Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint_const(YVal)),
                ( if XVal =< YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint_const(XVal)),
            ( if XVal = 0u then
                % Special case: 0u > than no uints.
                Result = no
            else
                Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint_const(YVal)),
                ( if XVal > YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">=", ModeNum = 0,
            Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint_const(YVal)),
            ( if YVal = 0u then
                % Special case: all uints are >= 0u.
                Result = yes
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint_const(XVal)),
                ( if XVal >= YVal then Result = yes else Result = no )
            )
        )
    ;
        ModuleName = "uint8",
        % 8-bit unsigned integer comparisons.
        Args = [X, Y],
        (
            PredName = "<", ModeNum = 0,
            Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint8_const(YVal)),
            ( if YVal = 0u8 then
                % Special case: no uints are < 0u.
                Result = no
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint8_const(XVal)),
                ( if XVal < YVal then Result = yes else Result = no )
            )
        ;
            PredName = "=<", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint8_const(XVal)),
            ( if XVal = 0u8 then
                % Special case: 0u =< all uints.
                Result = yes
            else
                Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint8_const(YVal)),
                ( if XVal =< YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint8_const(XVal)),
            ( if XVal = 0u8 then
                % Special case: 0u > than no uints.
                Result = no
            else
                Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint8_const(YVal)),
                ( if XVal > YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">=", ModeNum = 0,
            Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint8_const(YVal)),
            ( if YVal = 0u8 then
                % Special case: all uints are >= 0u.
                Result = yes
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint8_const(XVal)),
                ( if XVal >= YVal then Result = yes else Result = no )
            )
        )
    ;
        ModuleName = "uint16",
        % 16-bit unsigned integer comparisons.
        Args = [X, Y],
        (
            PredName = "<", ModeNum = 0,
            Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint16_const(YVal)),
            ( if YVal = 0u16 then
                % Special case: no uints are < 0u16.
                Result = no
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint16_const(XVal)),
                ( if XVal < YVal then Result = yes else Result = no )
            )
        ;
            PredName = "=<", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint16_const(XVal)),
            ( if XVal = 0u16 then
                % Special case: 0u =< all uints.
                Result = yes
            else
                Y ^ arg_inst =
                    bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint16_const(YVal)),
                ( if XVal =< YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint16_const(XVal)),
            ( if XVal = 0u16 then
                % Special case: 0u > than no uints.
                Result = no
            else
                Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint16_const(YVal)),
                ( if XVal > YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">=", ModeNum = 0,
            Y ^ arg_inst =
                bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint16_const(YVal)),
            ( if YVal = 0u16 then
                % Special case: all uints are >= 0u.
                Result = yes
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint16_const(XVal)),
                ( if XVal >= YVal then Result = yes else Result = no )
            )
        )
    ;
        ModuleName = "uint32",
        % 32-bit unsigned integer comparisons.
        Args = [X, Y],
        (
            PredName = "<", ModeNum = 0,
            Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint32_const(YVal)),
            ( if YVal = 0u32 then
                % Special case: no uints are < 0u.
                Result = no
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint32_const(XVal)),
                ( if XVal < YVal then Result = yes else Result = no )
            )
        ;
            PredName = "=<", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint32_const(XVal)),
            ( if XVal = 0u32 then
                % Special case: 0u =< all uints.
                Result = yes
            else
                Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint32_const(YVal)),
                ( if XVal =< YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint32_const(XVal)),
            ( if XVal = 0u32 then
                % Special case: 0u > than no uints.
                Result = no
            else
                Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint32_const(YVal)),
                ( if XVal > YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">=", ModeNum = 0,
            Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint32_const(YVal)),
            ( if YVal = 0u32 then
                % Special case: all uints are >= 0u.
                Result = yes
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint32_const(XVal)),
                ( if XVal >= YVal then Result = yes else Result = no )
            )
        )
    ;
        ModuleName = "uint64",
        % 64-bit unsigned integer comparisons.
        Args = [X, Y],
        (
            PredName = "<", ModeNum = 0,
            Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint64_const(YVal)),
            ( if YVal = 0u64 then
                % Special case: no uints are < 0u.
                Result = no
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint64_const(XVal)),
                ( if XVal < YVal then Result = yes else Result = no )
            )
        ;
            PredName = "=<", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint64_const(XVal)),
            ( if XVal = 0u64 then
                % Special case: 0u =< all uints.
                Result = yes
            else
                Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint64_const(YVal)),
                ( if XVal =< YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">", ModeNum = 0,
            X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
            FunctorX = some_int_const(uint64_const(XVal)),
            ( if XVal = 0u64 then
                % Special case: 0u > than no uints.
                Result = no
            else
                Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
                FunctorY = some_int_const(uint64_const(YVal)),
                ( if XVal > YVal then Result = yes else Result = no )
            )
        ;
            PredName = ">=", ModeNum = 0,
            Y ^ arg_inst = bound(_, _, [bound_functor(FunctorY, [])]),
            FunctorY = some_int_const(uint64_const(YVal)),
            ( if YVal = 0u64 then
                % Special case: all uints are >= 0u.
                Result = yes
            else
                X ^ arg_inst = bound(_, _, [bound_functor(FunctorX, [])]),
                FunctorX = some_int_const(uint64_const(XVal)),
                ( if XVal >= YVal then Result = yes else Result = no )
            )
        )
    ;
        ModuleName = "float",
        % Float comparisons.
        Args = [X, Y],
        X ^ arg_inst = bound(_, _, [bound_functor(float_const(XVal), [])]),
        Y ^ arg_inst = bound(_, _, [bound_functor(float_const(YVal), [])]),
        (
            PredName = "<", ModeNum = 0,
            ( if XVal < YVal then Result = yes else Result = no )
        ;
            PredName = "=<", ModeNum = 0,
            ( if XVal =< YVal then Result = yes else Result = no )
        ;
            PredName = ">", ModeNum = 0,
            ( if XVal > YVal then Result = yes else Result = no )
        ;
            PredName = ">=", ModeNum = 0,
            ( if XVal >= YVal then Result = yes else Result = no )
        )
    ;
        ModuleName = "private_builtin",
        PredName = "typed_unify", ModeNum = 0,
        % mode 0 is the (in, in) mode
        % mode 1 is the (in, out) mode
        % both modes are semidet
        Args = [TypeOfX, TypeOfY, X, Y],
        eval_unify(TypeOfX, TypeOfY, Result0),
        (
            Result0 = no,
            Result = no
        ;
            Result0 = yes,
            eval_unify(X, Y, Result)
        )
    ).

%---------------------------------------------------------------------------%

:- pred is_integer_for_op_type(op_type::in, cons_id::in, integer::out)
    is semidet.

is_integer_for_op_type(OpType, Functor, Integer) :-
    Functor = some_int_const(Const),
    (
        OpType = op_int(OpNumBits),
        (
            OpNumBits = word_bits(_),
            Const = int_const(I),
            Integer = integer(I)
        ;
            OpNumBits = bits_8,
            Const = int8_const(I8),
            Integer = from_int8(I8)
        ;
            OpNumBits = bits_16,
            Const = int16_const(I16),
            Integer = from_int16(I16)
        ;
            OpNumBits = bits_32,
            Const = int32_const(I32),
            Integer = from_int32(I32)
        ;
            OpNumBits = bits_64,
            Const = int64_const(I64),
            Integer = from_int64(I64)
        )
    ;
        OpType = op_uint(OpNumBits),
        (
            OpNumBits = word_bits(_),
            Const = uint_const(U),
            Integer = from_uint(U)
        ;
            OpNumBits = bits_8,
            Const = uint8_const(U8),
            Integer = from_uint8(U8)
        ;
            OpNumBits = bits_16,
            Const = uint16_const(U16),
            Integer = from_uint16(U16)
        ;
            OpNumBits = bits_32,
            Const = uint32_const(U32),
            Integer = from_uint32(U32)
        ;
            OpNumBits = bits_64,
            Const = uint64_const(U64),
            Integer = from_uint64(U64)
        )
    ).

%---------------------------------------------------------------------------%

    % If we can statically determine the result of a semidet call
    % to a standard library predicate or function, we return a value
    % of this type to specify that result. The result can be either ...
:- type semi_call_result
    --->    semi_call_failure
            % ... that the call will fail;
    ;       semi_call_binds_to_var(arg_hlds_info, arg_hlds_info).
            % ... that the call will succeed, assigning the value
            % of the variable described by the second arg_hlds_info
            % to the variable described by the first; or
    % ;     semi_call_binds_to_const(arg_hlds_info, cons_id).
            % ... that the call will succeed, assigning the value
            % of the constant  described by the cons_id to the variable
            % described by the first arg_hlds_info. This last alternative
            % is commented out, because it is not currently used.

    % evaluate_semidet_call(ModuleName, ProcName, ModeNum, Args, Result):
    %
    % This attempts to evaluate a call to
    %   ModuleName.ProcName(Args)
    % in mode ModeNum.
    %
    % If the call is a semidet call with one output that can be statically
    % evaluated, evaluate_semidet_call will succeed, returning in Result
    % the description of the result.
    %
:- pred evaluate_semidet_call(string::in, string::in, int::in,
    list(arg_hlds_info)::in, semi_call_result::out) is semidet.

evaluate_semidet_call(ModuleName, ProcName, ModeNum, Args, Result) :-
    (
        ModuleName = "builtin",
        ProcName = "dynamic_cast",
        ModeNum = 0
    ;
        ModuleName = "private_builtin",
        ProcName = "typed_unify",
        % mode 0 is the (in, in) mode
        % mode 1 is the (in, out) mode
        % both modes are semidet
        ModeNum = 1
    ),
    Args = [TypeOfX, TypeOfY, X, Y],
    eval_unify(TypeOfX, TypeOfY, Result0),
    (
        Result0 = no,
        Result = semi_call_failure
    ;
        Result0 = yes,
        Result = semi_call_binds_to_var(Y, X)
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
    make_const_construction(dummy_context, Arg ^ arg_var, ConsId, Goal),
    Goal = hlds_goal(GoalExpr, _).

%---------------------------------------------------------------------------%

:- pred make_true_or_fail(bool::in, hlds_goal_expr::out) is det.

make_true_or_fail(yes, true_goal_expr).
make_true_or_fail(no, fail_goal_expr).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.const_prop.
%---------------------------------------------------------------------------%
