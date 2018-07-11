%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012, 2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates MLDS code for unifications.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_unify_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Generate MLDS code for a unification.
    %
:- pred ml_generate_unification(code_model::in, unification::in,
    prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_unify_gen_construct.
:- import_module ml_backend.ml_unify_gen_deconstruct.

:- import_module maybe.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

ml_generate_unification(CodeModel, Unification, Context,
        Defns, Stmts, !Info) :-
    (
        Unification = assign(TargetVar, SourceVar),
        expect(unify(CodeModel, model_det), $pred, "assign not det"),
        ml_generate_assignment_unification(TargetVar, SourceVar, Context,
            Stmts, !Info),
        Defns = []
    ;
        Unification = simple_test(VarA, VarB),
        expect(unify(CodeModel, model_semi), $pred, "simple_test not semidet"),
        ml_generate_simple_test_unification(VarA, VarB, Context, Stmt, !Info),
        Defns = [],
        Stmts = [Stmt]
    ;
        Unification = construct(LHSVar, ConsId, RHSVars, ArgModes,
            HowToConstruct, _CellIsUnique, SubInfo),
        expect(unify(CodeModel, model_det), $pred, "construct not det"),
        (
            SubInfo = no_construct_sub_info,
            TakeAddr = []
        ;
            SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSizeProfInfo),
            (
                MaybeTakeAddr = no,
                TakeAddr = []
            ;
                MaybeTakeAddr = yes(TakeAddr)
            ),
            expect(unify(MaybeSizeProfInfo, no), $pred,
                "term size profiling not yet supported")
        ),
        ml_generate_construction_unification(LHSVar, ConsId, RHSVars, ArgModes,
            TakeAddr, HowToConstruct, Context, Defns, Stmts, !Info)
    ;
        Unification = deconstruct(LHSVar, ConsId, RHSVars, ArgModes,
            CanFail, CanCGC),
        ml_generate_deconstruction_unification(LHSVar, ConsId,
            RHSVars, ArgModes, CanFail, CanCGC, CodeModel, Context,
            Defns, Stmts, !Info)
    ;
        Unification = complicated_unify(_, _, _),
        % These should have been converted into calls to unification predicates
        % by the simplification pass.
        unexpected($pred, "complicated unify")
    ).

%---------------------------------------------------------------------------%

:- pred ml_generate_assignment_unification(prog_var::in, prog_var::in,
    prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_assignment_unification(TargetVar, SourceVar, Context,
        Stmts, !Info) :-
    ml_variable_type(!.Info, TargetVar, Type),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    IsDummyType = is_type_a_dummy(ModuleInfo, Type),
    (
        IsDummyType = is_dummy_type,
        % We want to avoid generating references to variables of dummy types,
        % since they will not have been declared.
        Stmts = []
    ;
        IsDummyType = is_not_dummy_type,
        ml_gen_var(!.Info, TargetVar, TargetLval),
        ml_gen_var(!.Info, SourceVar, SourceLval),
        Stmt = ml_gen_assign(TargetLval, ml_lval(SourceLval), Context),
        Stmts = [Stmt]
    ),
    ( if ml_gen_info_search_const_var(!.Info, SourceVar, GroundTerm) then
        % If the source variable is a constant, so is the target after
        % this assignment.
        %
        % The mark_static_terms assumes that if SourceVar is a constant
        % term, then after this assignment unification TargetVar is a
        % constant term also. Therefore later constant terms may contain
        % TargetVar among their arguments. If we didn't copy the constant
        % info here, the construction of the later constant could cause
        % a code generator abort.
        ml_gen_info_set_const_var(TargetVar, GroundTerm, !Info)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred ml_generate_simple_test_unification(prog_var::in, prog_var::in,
    prog_context::in, mlds_stmt::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_simple_test_unification(VarA, VarB, Context, Stmt, !Info) :-
    ml_variable_type(!.Info, VarA, Type),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    IsDummyType = is_type_a_dummy(ModuleInfo, Type),
    (
        IsDummyType = is_dummy_type,
        % We want to avoid generating references to variables of dummy types,
        % since they will not have been declared.
        %
        % Since a dummy type has only one value, two values of the same dummy
        % type are always equal.
        SidesAreEqualRval = ml_const(mlconst_true)
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
        ml_gen_var(!.Info, VarA, VarLvalA),
        ml_gen_var(!.Info, VarB, VarLvalB),
        SidesAreEqualRval =
            ml_binop(EqOp, ml_lval(VarLvalA), ml_lval(VarLvalB))
    ),
    ml_gen_set_success(SidesAreEqualRval, Context, Stmt, !Info).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_unify_gen.
%---------------------------------------------------------------------------%
