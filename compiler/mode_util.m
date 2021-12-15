%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_util.m.
% Main author: fjh.
%
% This module contains utility predicates for dealing with modes.
%
%---------------------------------------------------------------------------%

:- module check_hlds.mode_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%
%
% Breaking down modes into their initial and final insts.
%

:- func from_to_insts_to_init_inst(from_to_insts) = mer_inst.
:- func from_to_insts_to_final_inst(from_to_insts) = mer_inst.

%---------------------%

    % Return the initial and final instantiatedness for the given mode.
    % Fail if the mode is undefined.
    %
:- pred mode_get_insts_semidet(module_info::in, mer_mode::in,
    mer_inst::out, mer_inst::out) is semidet.

    % Return the initial and final instantiatedness for the given mode.
    % Throw an exception if the mode is undefined.
    %
:- pred mode_get_insts(module_info::in, mer_mode::in,
    mer_inst::out, mer_inst::out) is det.
:- pred mode_get_from_to_insts(module_info::in, mer_mode::in,
    from_to_insts::out) is det.

%---------------------%

    % Return the initial or final instantiatedness for the given mode.
    % Throw an exception if the mode is undefined.
    %
:- func mode_get_initial_inst(module_info, mer_mode) = mer_inst.
:- func mode_get_final_inst(module_info, mer_mode) = mer_inst.

    % Return the initial or final instantiatedness for each of
    % the given modes.
    % Throw an exception if any mode is undefined.
    %
:- pred mode_list_get_initial_insts(module_info::in,
    list(mer_mode)::in, list(mer_inst)::out) is det.
:- pred mode_list_get_final_insts(module_info::in,
    list(mer_mode)::in, list(mer_inst)::out) is det.

%---------------------------------------------------------------------------%
%
% Converting between from_to_insts and modes.
%

:- func from_to_insts_to_mode(from_to_insts) = mer_mode.
:- func mode_to_from_to_insts(module_info, mer_mode) = from_to_insts.

%---------------------------------------------------------------------------%
%
% Converting between unify_modes and from_to_insts/modes.
%

:- pred unify_mode_to_lhs_rhs_from_to_insts(unify_mode::in,
    from_to_insts::out, from_to_insts::out) is det.

    % Return the modes of the operands on the given side of the unifications.
    %
:- func unify_mode_to_lhs_mode(unify_mode) = mer_mode.
:- func unify_mode_to_rhs_mode(unify_mode) = mer_mode.
:- func unify_mode_to_lhs_from_to_insts(unify_mode) = from_to_insts.
:- func unify_mode_to_rhs_from_to_insts(unify_mode) = from_to_insts.

    % Given the modes of the two sides of a unification, return the unify_mode.
    %
:- pred modes_to_unify_mode(module_info::in,
    mer_mode::in, mer_mode::in, unify_mode::out) is det.
:- pred from_to_insts_to_unify_mode(from_to_insts::in, from_to_insts::in,
    unify_mode::out) is det.

    % Given two lists of modes (of equal length), with each pair
    % giving the modes of the two sides of a unification,
    % return a unify_mode for each corresponding pair of modes.
    %
:- pred modes_to_unify_modes(module_info::in,
    list(mer_mode)::in, list(mer_mode)::in, list(unify_mode)::out) is det.
:- pred from_to_insts_to_unify_modes(
    list(from_to_insts)::in, list(from_to_insts)::in, list(unify_mode)::out)
    is det.

%---------------------------------------------------------------------------%
%
% Figure out which arguments are live in callers.
%

    % Given the mode of a predicate, work out which arguments are live
    % (might be used again by the caller of that predicate) and which are dead.
    %
:- pred get_arg_lives(module_info::in, list(mer_mode)::in, list(is_live)::out)
    is det.

%---------------------------------------------------------------------------%
%
% Constructing bound_insts.
%

    % Convert a list of constructors to a list of bound_insts where the
    % arguments are `ground'.
    %
    % NOTE: the list(bound_inst) is not sorted and may contain duplicates.
    %
:- pred constructors_to_bound_insts(module_info::in, uniqueness::in,
    type_ctor::in, list(constructor)::in, list(bound_inst)::out) is det.

    % Convert a list of constructors to a list of bound_insts where the
    % arguments are `any'.
    %
    % NOTE: the list(bound_inst) is not sorted and may contain duplicates.
    %
:- pred constructors_to_bound_any_insts(module_info::in, uniqueness::in,
    type_ctor::in, list(constructor)::in, list(bound_inst)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_test.
:- import_module hlds.hlds_inst_mode.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.

:- import_module map.
:- import_module require.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

from_to_insts_to_init_inst(FromToInsts) = Init :-
    FromToInsts = from_to_insts(Init, _Final).

from_to_insts_to_final_inst(FromToInsts) = Final :-
    FromToInsts = from_to_insts(_Init, Final).

%---------------------%

mode_get_insts_semidet(ModuleInfo, Mode0, InitialInst, FinalInst) :-
    (
        Mode0 = from_to_mode(InitialInst, FinalInst)
    ;
        Mode0 = user_defined_mode(Name, Args),
        list.length(Args, Arity),
        module_info_get_mode_table(ModuleInfo, Modes),
        mode_table_get_mode_defns(Modes, ModeDefns),
        % Try looking up Name as-is. If that fails and Name is unqualified,
        % try looking it up with the builtin qualifier.
        % XXX This is a makeshift fix for a problem that requires more
        % investigation (without this fix the compiler occasionally
        % throws an exception in mode_get_insts/4).
        ( if map.search(ModeDefns, mode_ctor(Name, Arity), HLDS_Mode0) then
            HLDS_Mode = HLDS_Mode0
        else
            Name = unqualified(String),
            BuiltinName = qualified(mercury_public_builtin_module, String),
            map.search(ModeDefns, mode_ctor(BuiltinName, Arity), HLDS_Mode)
        ),
        HLDS_Mode = hlds_mode_defn(_VarSet, Params, ModeDefn,
            _Context, _Status),
        ModeDefn = hlds_mode_body(Mode1),
        mode_substitute_arg_list(Mode1, Params, Args, Mode),
        mode_get_insts_semidet(ModuleInfo, Mode, InitialInst, FinalInst)
    ).

mode_get_insts(ModuleInfo, Mode, InitInst, FinalInst) :-
    ( if
        mode_get_insts_semidet(ModuleInfo, Mode, InitInstPrime, FinalInstPrime)
    then
        InitInst = InitInstPrime,
        FinalInst = FinalInstPrime
    else
        unexpected($pred, "mode_get_insts_semidet failed")
    ).

mode_get_from_to_insts(ModuleInfo, Mode, FromToInsts) :-
    mode_get_insts(ModuleInfo, Mode, InitInst, FinalInst),
    FromToInsts = from_to_insts(InitInst, FinalInst).

%---------------------%

mode_get_initial_inst(ModuleInfo, Mode) = Inst :-
    mode_get_insts(ModuleInfo, Mode, Inst, _).

mode_get_final_inst(ModuleInfo, Mode) = Inst :-
    mode_get_insts(ModuleInfo, Mode, _, Inst).

mode_list_get_initial_insts(_ModuleInfo, [], []).
mode_list_get_initial_insts(ModuleInfo, [Mode | Modes], [Inst | Insts]) :-
    mode_get_insts(ModuleInfo, Mode, Inst, _),
    mode_list_get_initial_insts(ModuleInfo, Modes, Insts).

mode_list_get_final_insts(_ModuleInfo, [], []).
mode_list_get_final_insts(ModuleInfo, [Mode | Modes], [Inst | Insts]) :-
    mode_get_insts(ModuleInfo, Mode, _, Inst),
    mode_list_get_final_insts(ModuleInfo, Modes, Insts).

%---------------------------------------------------------------------------%

from_to_insts_to_mode(FromToInsts) = Mode :-
    FromToInsts = from_to_insts(Init, Final),
    Mode = from_to_mode(Init, Final).

mode_to_from_to_insts(ModuleInfo, Mode) = FromToInsts :-
    mode_get_insts(ModuleInfo, Mode, Init, Final),
    FromToInsts = from_to_insts(Init, Final).

%---------------------------------------------------------------------------%

unify_mode_to_lhs_rhs_from_to_insts(UnifyMode, LHSInsts, RHSInsts) :-
    LHSInsts = from_to_insts(LHSInitInst, LHSFinalInst),
    RHSInsts = from_to_insts(RHSInitInst, RHSFinalInst),
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInitInst, LHSFinalInst,
        RHSInitInst, RHSFinalInst).

unify_mode_to_lhs_mode(UnifyMode) = LHSMode :-
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInitInst, LHSFinalInst, _, _),
    LHSMode = from_to_mode(LHSInitInst, LHSFinalInst).

unify_mode_to_rhs_mode(UnifyMode) = RHSMode :-
    UnifyMode = unify_modes_li_lf_ri_rf(_, _, RHSInitInst, RHSFinalInst),
    RHSMode = from_to_mode(RHSInitInst, RHSFinalInst).

unify_mode_to_lhs_from_to_insts(UnifyMode) = LHSFromToInsts :-
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInitInst, LHSFinalInst, _, _),
    LHSFromToInsts = from_to_insts(LHSInitInst, LHSFinalInst).

unify_mode_to_rhs_from_to_insts(UnifyMode) = RHSFromToInsts :-
    UnifyMode = unify_modes_li_lf_ri_rf(_, _, RHSInitInst, RHSFinalInst),
    RHSFromToInsts = from_to_insts(RHSInitInst, RHSFinalInst).

%---------------------%

modes_to_unify_mode(ModuleInfo, ModeX, ModeY, UnifyMode) :-
    mode_get_insts(ModuleInfo, ModeX, InitialX, FinalX),
    mode_get_insts(ModuleInfo, ModeY, InitialY, FinalY),
    UnifyMode = unify_modes_li_lf_ri_rf(InitialX, FinalX, InitialY, FinalY).

from_to_insts_to_unify_mode(FromToInstsX, FromToInstsY, UnifyMode) :-
    FromToInstsX = from_to_insts(InitInstX, FinalInstX),
    FromToInstsY = from_to_insts(InitInstY, FinalInstY),
    UnifyMode = unify_modes_li_lf_ri_rf(InitInstX, FinalInstX,
        InitInstY, FinalInstY).

modes_to_unify_modes(_ModuleInfo, [], [], []).
modes_to_unify_modes(_ModuleInfo, [], [_ | _], _) :-
    unexpected($pred, "length mismatch").
modes_to_unify_modes(_ModuleInfo, [_ | _], [], _) :-
    unexpected($pred, "length mismatch").
modes_to_unify_modes(ModuleInfo,
        [ModeX | ModeXs], [ModeY | ModeYs],
        [UnifyMode | UnifyModes]) :-
    modes_to_unify_mode(ModuleInfo, ModeX, ModeY, UnifyMode),
    modes_to_unify_modes(ModuleInfo, ModeXs, ModeYs, UnifyModes).

from_to_insts_to_unify_modes([], [], []).
from_to_insts_to_unify_modes([], [_ | _], _) :-
    unexpected($pred, "length mismatch").
from_to_insts_to_unify_modes([_ | _], [], _) :-
    unexpected($pred, "length mismatch").
from_to_insts_to_unify_modes(
        [FromToInstsX | FromToInstsXs], [FromToInstsY | FromToInstsYs],
        [UnifyMode | UnifyModes]) :-
    from_to_insts_to_unify_mode(FromToInstsX, FromToInstsY, UnifyMode),
    from_to_insts_to_unify_modes(FromToInstsXs, FromToInstsYs, UnifyModes).

%---------------------------------------------------------------------------%

get_arg_lives(_, [], []).
get_arg_lives(ModuleInfo, [Mode | Modes], [IsLive | IsLives]) :-
    % Arguments with final inst `clobbered' are dead, any others
    % are assumed to be live.
    mode_get_insts(ModuleInfo, Mode, _InitialInst, FinalInst),
    ( if inst_is_clobbered(ModuleInfo, FinalInst) then
        IsLive = is_dead
    else
        IsLive = is_live
    ),
    get_arg_lives(ModuleInfo, Modes, IsLives).

%---------------------------------------------------------------------------%

constructors_to_bound_insts(ModuleInfo, Uniq, TypeCtor, Constructors,
        BoundInsts) :-
    constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Uniq, TypeCtor,
        Constructors, ground(Uniq, none_or_default_func), BoundInsts).

constructors_to_bound_any_insts(ModuleInfo, Uniq, TypeCtor, Constructors,
        BoundInsts) :-
    constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Uniq, TypeCtor,
        Constructors, any(Uniq, none_or_default_func), BoundInsts).

:- pred constructors_to_bound_insts_loop_over_ctors(module_info::in,
    uniqueness::in, type_ctor::in, list(constructor)::in, mer_inst::in,
    list(bound_inst)::out) is det.

constructors_to_bound_insts_loop_over_ctors(_, _, _, [], _, []).
constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Uniq, TypeCtor,
        [Ctor | Ctors], ArgInst, [BoundInst | BoundInsts]) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, Name, Args, _Arity, _Ctxt),
    ctor_arg_list_to_inst_list(Args, ArgInst, Insts),
    list.length(Insts, Arity),
    BoundInst = bound_functor(cons(Name, Arity, TypeCtor), Insts),
    constructors_to_bound_insts_loop_over_ctors(ModuleInfo, Uniq, TypeCtor,
        Ctors, ArgInst, BoundInsts).

:- pred ctor_arg_list_to_inst_list(list(constructor_arg)::in, mer_inst::in,
    list(mer_inst)::out) is det.

ctor_arg_list_to_inst_list([], _, []).
ctor_arg_list_to_inst_list([_ | Args], Inst, [Inst | Insts]) :-
    ctor_arg_list_to_inst_list(Args, Inst, Insts).

%---------------------------------------------------------------------------%
:- end_module check_hlds.mode_util.
%---------------------------------------------------------------------------%
