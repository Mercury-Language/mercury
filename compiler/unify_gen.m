%---------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------e
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unify_gen.m.
%
% This module handles code generation for "simple" unifications,
% i.e. those unifications which are simple enough for us to generate
% inline code.
%
% For "complicated" unifications, we generate a call to an out-of-line
% unification predicate (the call is handled in call_gen.m) - and then
% eventually generate the out-of-line code (unify_proc.m).
%
%---------------------------------------------------------------------------%

:- module ll_backend.unify_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.

%---------------------------------------------------------------------------%

    % Generate LLDS code for a unification.
    %
:- pred generate_unification(code_model::in, unification::in,
    hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_module.
:- import_module ll_backend.unify_gen_construct.
:- import_module ll_backend.unify_gen_deconstruct.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

generate_unification(CodeModel, Unification, GoalInfo, Code, !CI, !CLD) :-
    (
        Unification = assign(LHSVar, RHSVar),
        expect(unify(CodeModel, model_det), $pred, "assign not det"),
        generate_assignment_unification(LHSVar, RHSVar, Code, !CLD)
    ;
        Unification = simple_test(VarA, VarB),
        expect(unify(CodeModel, model_semi), $pred, "simple_test not semidet"),
        generate_simple_test_unification(VarA, VarB, Code, !CI, !CLD)
    ;
        Unification = construct(LHSVar, ConsId, RHSVars, ArgModes,
            HowToConstruct, _CellIsUnique, SubInfo),
        expect(unify(CodeModel, model_det), $pred, "construct not det"),
        (
            SubInfo = no_construct_sub_info,
            MaybeTakeAddr = no,
            MaybeSize = no
        ;
            SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize)
        ),
        ( if
            ( variable_is_forward_live(!.CLD, LHSVar)
            ; MaybeTakeAddr = yes(_)
            )
        then
            (
                MaybeTakeAddr = yes(TakeAddr)
            ;
                MaybeTakeAddr = no,
                TakeAddr = []
            ),
            generate_construction_unification(LHSVar, ConsId,
                RHSVars, ArgModes, HowToConstruct, TakeAddr, MaybeSize,
                GoalInfo, Code, !CI, !CLD)
        else
            Code = empty
        )
    ;
        Unification = deconstruct(LHSVar, ConsId, RHSVars, ArgModes,
            CanFail, CanCGC),
        generate_deconstruction_unification(LHSVar, ConsId, RHSVars, ArgModes,
            CanFail, CanCGC, Code, !CI, !CLD)
    ;
        Unification = complicated_unify(_, _, _),
        % These should have been converted into calls to unification predicates
        % by the simplification pass.
        unexpected($pred, "complicated unify")
    ).

%---------------------------------------------------------------------------%

    % Generate code for the assignment unification LHSVar := RHSVar.
    %
:- pred generate_assignment_unification(prog_var::in, prog_var::in,
    llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_assignment_unification(LHSVar, RHSVar, Code, !CLD) :-
    ( if variable_is_forward_live(!.CLD, LHSVar) then
        assign_var_to_var(LHSVar, RHSVar, !CLD),
        % The assignment is cached; we do not generate any code for it *here*
        % (though we *will* generate code for it later, when the value
        % of LHSVar is materialized).
        Code = empty
    else
        % Mode analysis reports free-free unifications as assignments
        % to a dead variable. For such unifications, we of course
        % do not generate any code.
        Code = empty
    ).

%---------------------------------------------------------------------------%

    % generate_simple_test_unification(VarA, VarB, Code, !CI, !CLD):
    %
    % We generate code for a simple test unification by flushing both variables
    % from the cache, and producing code that branches to the location that is
    % appropriate for a failure in the current environment if the two values
    % are not the same. Simple tests are in-in unifications on enumerations,
    % integers, strings and floats.
    %
:- pred generate_simple_test_unification(prog_var::in, prog_var::in,
    llds_code::out, code_info::in, code_info::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_simple_test_unification(VarA, VarB, Code, !CI, !CLD) :-
    Type = variable_type(!.CI, VarA),
    get_module_info(!.CI, ModuleInfo),
    IsDummyType = is_type_a_dummy(ModuleInfo, Type),
    (
        IsDummyType = is_dummy_type,
        Code = empty
    ;
        IsDummyType = is_not_dummy_type,
        ( if Type = builtin_type(BuiltinType) then
            (
                BuiltinType = builtin_type_string,
                EqOp = str_eq
            ;
                BuiltinType = builtin_type_float,
                EqOp = float_eq
            ;
                BuiltinType = builtin_type_char,
                EqOp = eq(int_type_int)
            ;
                BuiltinType = builtin_type_int(IntType),
                EqOp = eq(IntType)
            )
        else
            % The else branch handles enumerations.
            EqOp = eq(int_type_int)
        ),
        produce_variable(VarA, VarCodeA, VarRvalA, !CLD),
        produce_variable(VarB, VarCodeB, VarRvalB, !CLD),
        fail_if_rval_is_false(binop(EqOp, VarRvalA, VarRvalB), FailCode,
            !CI, !CLD),
        Code = VarCodeA ++ VarCodeB ++ FailCode
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.unify_gen.
%---------------------------------------------------------------------------%
