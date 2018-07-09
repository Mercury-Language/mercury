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

:- pred generate_unification(code_model::in, unification::in,
    hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.hlds_module.
:- import_module ll_backend.unify_gen_construct.
:- import_module ll_backend.unify_gen_deconstruct.
:- import_module ll_backend.unify_gen_test.
:- import_module ll_backend.unify_gen_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

generate_unification(CodeModel, Uni, GoalInfo, Code, !CI, !CLD) :-
    (
        Uni = assign(LHSVar, RHSVar),
        (
            CodeModel = model_det
        ;
            ( CodeModel = model_semi
            ; CodeModel = model_non
            ),
            unexpected($pred, "assign is not model_det")
        ),
        ( if variable_is_forward_live(!.CLD, LHSVar) then
            generate_assignment(LHSVar, RHSVar, Code, !CLD)
        else
            Code = empty
        )
    ;
        Uni = construct(LHSVar, ConsId, RHSVars, ArgModes, HowToConstruct, _,
            SubInfo),
        (
            CodeModel = model_det
        ;
            ( CodeModel = model_semi
            ; CodeModel = model_non
            ),
            unexpected($pred, "construct is not model_det")
        ),
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
            get_module_info(!.CI, ModuleInfo),
            get_cons_arg_widths(ModuleInfo, ConsId, RHSVars, RHSVarsWidths),
            generate_construction(LHSVar, ConsId, RHSVarsWidths, ArgModes,
                HowToConstruct, TakeAddr, MaybeSize, GoalInfo, Code, !CI, !CLD)
        else
            Code = empty
        )
    ;
        Uni = deconstruct(LHSVar, ConsId, RHSVars, ArgModes, _CanFail, CanCGC),
        get_module_info(!.CI, ModuleInfo),
        get_cons_arg_widths(ModuleInfo, ConsId, RHSVars, RHSVarsWidths),
        (
            CodeModel = model_det,
            generate_det_deconstruction(LHSVar, ConsId,
                RHSVarsWidths, ArgModes, DeconstructCode, !.CI, !CLD)
        ;
            CodeModel = model_semi,
            generate_semi_deconstruction(LHSVar, ConsId,
                RHSVarsWidths, ArgModes, DeconstructCode, !CI, !CLD)
        ;
            CodeModel = model_non,
            unexpected($pred, "deconstruct is model_non")
        ),
        (
            CanCGC = can_cgc,
            LHSVarName = variable_name(!.CI, LHSVar),
            produce_variable(LHSVar, ProduceVarCode, VarRval, !.CI, !CLD),
            ( if VarRval = lval(VarLval) then
                save_reused_cell_fields(LHSVar, VarLval, SaveArgsCode, Regs,
                    !.CI, !CLD),
                % This seems to be fine.
                list.foldl(release_reg, Regs, !CLD),
                % XXX avoid strip_tag when we know what ptag it will have
                FreeVarCode = singleton(
                    llds_instr(free_heap(unop(strip_tag, VarRval)),
                        "Free " ++ LHSVarName)
                ),
                Code = DeconstructCode ++
                    ProduceVarCode ++ SaveArgsCode ++ FreeVarCode
            else
                Code = DeconstructCode
            )
        ;
            CanCGC = cannot_cgc,
            Code = DeconstructCode
        )
    ;
        Uni = simple_test(VarA, VarB),
        (
            CodeModel = model_semi,
            generate_simple_test_unification(VarA, VarB, Code, !CI, !CLD)
        ;
            ( CodeModel = model_det
            ; CodeModel = model_non
            ),
            unexpected($pred, "simple_test is not model_semi")
        )
    ;
        % These should have been transformed into calls to unification
        % procedures by polymorphism.m.
        Uni = complicated_unify(_Mode, _CanFail, _TypeInfoVars),
        unexpected($pred, "complicated unify")
    ).

%---------------------------------------------------------------------------%

    % Assignment unifications are generated by simply caching the bound
    % variable as the expression that generates the free variable.
    % No immediate code is generated.
    %
:- pred generate_assignment(prog_var::in, prog_var::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_assignment(VarA, VarB, empty, !CLD) :-
    ( if variable_is_forward_live(!.CLD, VarA) then
        assign_var_to_var(VarA, VarB, !CLD)
    else
        % Mode analysis reports free-free unifications as assignments
        % to a dead variable. For such unifications, we of course
        % do not generate any code.
        true
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.unify_gen.
%---------------------------------------------------------------------------%
