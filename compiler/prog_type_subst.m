%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Operations for performing various kinds of type substitutions on data
% structures that are part of the parse tree.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_type_subst.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%
%
% Type substitutions.
%

:- pred apply_variable_renaming_to_tvar_kind_map(tvar_renaming::in,
    tvar_kind_map::in, tvar_kind_map::out) is det.

%---------------------%

:- pred apply_variable_renaming_to_tvar(tvar_renaming::in,
    tvar::in, tvar::out) is det.

:- pred apply_subst_to_tvar(tvar_kind_map::in, tsubst::in,
    tvar::in, mer_type::out) is det.

:- pred apply_rec_subst_to_tvar(tvar_kind_map::in, tsubst::in,
    tvar::in, mer_type::out) is det.

%---------------------%

:- pred apply_variable_renaming_to_tvar_list(tvar_renaming::in,
    list(tvar)::in, list(tvar)::out) is det.

:- pred apply_subst_to_tvar_list(tvar_kind_map::in, tsubst::in,
    list(tvar)::in, list(mer_type)::out) is det.

:- pred apply_rec_subst_to_tvar_list(tvar_kind_map::in, tsubst::in,
    list(tvar)::in, list(mer_type)::out) is det.

%---------------------%

:- pred apply_variable_renaming_to_type(tvar_renaming::in,
    mer_type::in, mer_type::out) is det.

:- pred apply_subst_to_type(tsubst::in, mer_type::in, mer_type::out) is det.

:- pred apply_rec_subst_to_type(tsubst::in, mer_type::in, mer_type::out)
    is det.

%---------------------%

:- pred apply_variable_renaming_to_type_list(tvar_renaming::in,
    list(mer_type)::in, list(mer_type)::out) is det.

:- pred apply_subst_to_type_list(tsubst::in,
    list(mer_type)::in, list(mer_type)::out) is det.

:- pred apply_rec_subst_to_type_list(tsubst::in,
    list(mer_type)::in, list(mer_type)::out) is det.

%---------------------------------------------------------------------------%
%
% Utility predicates dealing with typeclass constraints.
%

:- pred apply_variable_renaming_to_prog_constraint(tvar_renaming::in,
    prog_constraint::in, prog_constraint::out) is det.

:- pred apply_subst_to_prog_constraint(tsubst::in, prog_constraint::in,
    prog_constraint::out) is det.

:- pred apply_rec_subst_to_prog_constraint(tsubst::in, prog_constraint::in,
    prog_constraint::out) is det.

%---------------------%

:- pred apply_variable_renaming_to_prog_constraint_list(tvar_renaming::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_subst_to_prog_constraint_list(tsubst::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_rec_subst_to_prog_constraint_list(tsubst::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

%---------------------%

:- pred apply_variable_renaming_to_prog_constraints(tvar_renaming::in,
    prog_constraints::in, prog_constraints::out) is det.

:- pred apply_subst_to_prog_constraints(tsubst::in, prog_constraints::in,
    prog_constraints::out) is det.

:- pred apply_rec_subst_to_prog_constraints(tsubst::in, prog_constraints::in,
    prog_constraints::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.

%---------------------------------------------------------------------------%

apply_variable_renaming_to_tvar_kind_map(Renaming, KindMap0, KindMap) :-
    map.foldl(apply_variable_renaming_to_tvar_kind_map_2(Renaming),
        KindMap0, map.init, KindMap).

:- pred apply_variable_renaming_to_tvar_kind_map_2(tvar_renaming::in, tvar::in,
    kind::in, tvar_kind_map::in, tvar_kind_map::out) is det.

apply_variable_renaming_to_tvar_kind_map_2(Renaming, TVar0, Kind, !KindMap) :-
    apply_variable_renaming_to_tvar(Renaming, TVar0, TVar),
    map.det_insert(TVar, Kind, !KindMap).

%---------------------------------------------------------------------------%

apply_variable_renaming_to_tvar(Renaming, TVar0, TVar) :-
    ( if map.search(Renaming, TVar0, TVar1) then
        TVar = TVar1
    else
        TVar = TVar0
    ).

apply_subst_to_tvar(KindMap, Subst, TVar, Type) :-
    ( if map.search(Subst, TVar, Type0) then
        apply_subst_to_type(Subst, Type0, Type)
    else
        get_tvar_kind(KindMap, TVar, Kind),
        Type = type_variable(TVar, Kind)
    ).

apply_rec_subst_to_tvar(KindMap, Subst, TVar, Type) :-
    ( if map.search(Subst, TVar, Type0) then
        apply_rec_subst_to_type(Subst, Type0, Type)
    else
        get_tvar_kind(KindMap, TVar, Kind),
        Type = type_variable(TVar, Kind)
    ).

%---------------------------------------------------------------------------%

apply_variable_renaming_to_tvar_list(Renaming, TVars0, TVars) :-
    list.map(apply_variable_renaming_to_tvar(Renaming), TVars0, TVars).

apply_subst_to_tvar_list(KindMap, Subst, TVars, Types) :-
    list.map(apply_subst_to_tvar(KindMap, Subst), TVars, Types).

apply_rec_subst_to_tvar_list(KindMap, Subst, TVars, Types) :-
    list.map(apply_rec_subst_to_tvar(KindMap, Subst), TVars, Types).

%---------------------------------------------------------------------------%

apply_variable_renaming_to_type(Renaming, Type0, Type) :-
    (
        Type0 = type_variable(TVar0, Kind),
        apply_variable_renaming_to_tvar(Renaming, TVar0, TVar),
        Type = type_variable(TVar, Kind)
    ;
        Type0 = defined_type(Name, Args0, Kind),
        apply_variable_renaming_to_type_list(Renaming, Args0, Args),
        Type = defined_type(Name, Args, Kind)
    ;
        Type0 = builtin_type(_),
        Type = Type0
    ;
        Type0 = higher_order_type(PorF, Args0, HOInstInfo, Purity, EvalMethod),
        apply_variable_renaming_to_type_list(Renaming, Args0, Args),
        Type = higher_order_type(PorF, Args, HOInstInfo, Purity, EvalMethod)
    ;
        Type0 = tuple_type(Args0, Kind),
        apply_variable_renaming_to_type_list(Renaming, Args0, Args),
        Type = tuple_type(Args, Kind)
    ;
        Type0 = apply_n_type(TVar0, Args0, Kind),
        apply_variable_renaming_to_type_list(Renaming, Args0, Args),
        apply_variable_renaming_to_tvar(Renaming, TVar0, TVar),
        Type = apply_n_type(TVar, Args, Kind)
    ;
        Type0 = kinded_type(BaseType0, Kind),
        apply_variable_renaming_to_type(Renaming, BaseType0, BaseType),
        Type = kinded_type(BaseType, Kind)
    ).

apply_subst_to_type(Subst, Type0, Type) :-
    (
        Type0 = type_variable(TVar, Kind),
        ( if map.search(Subst, TVar, Type1) then
            ensure_type_has_kind(Kind, Type1, Type)
        else
            Type = Type0
        )
    ;
        Type0 = defined_type(Name, Args0, Kind),
        apply_subst_to_type_list(Subst, Args0, Args),
        Type = defined_type(Name, Args, Kind)
    ;
        Type0 = builtin_type(_),
        Type = Type0
    ;
        Type0 = higher_order_type(PorF, Args0, HOInstInfo, Purity, EvalMethod),
        apply_subst_to_type_list(Subst, Args0, Args),
        Type = higher_order_type(PorF, Args, HOInstInfo, Purity, EvalMethod)
    ;
        Type0 = tuple_type(Args0, Kind),
        apply_subst_to_type_list(Subst, Args0, Args),
        Type = tuple_type(Args, Kind)
    ;
        Type0 = apply_n_type(TVar, Args0, Kind),
        apply_subst_to_type_list(Subst, Args0, Args),
        ( if map.search(Subst, TVar, AppliedType) then
            apply_type_args(AppliedType, Args, Type)
        else
            Type = apply_n_type(TVar, Args, Kind)
        )
    ;
        Type0 = kinded_type(BaseType0, Kind),
        apply_subst_to_type(Subst, BaseType0, BaseType),
        Type = kinded_type(BaseType, Kind)
    ).

apply_rec_subst_to_type(Subst, Type0, Type) :-
    (
        Type0 = type_variable(TVar, Kind),
        ( if map.search(Subst, TVar, Type1) then
            ensure_type_has_kind(Kind, Type1, Type2),
            apply_rec_subst_to_type(Subst, Type2, Type)
        else
            Type = Type0
        )
    ;
        Type0 = defined_type(Name, Args0, Kind),
        apply_rec_subst_to_type_list(Subst, Args0, Args),
        Type = defined_type(Name, Args, Kind)
    ;
        Type0 = builtin_type(_),
        Type = Type0
    ;
        Type0 = higher_order_type(PorF, Args0, HOInstInfo, Purity, EvalMethod),
        apply_rec_subst_to_type_list(Subst, Args0, Args),
        Type = higher_order_type(PorF, Args, HOInstInfo, Purity, EvalMethod)
    ;
        Type0 = tuple_type(Args0, Kind),
        apply_rec_subst_to_type_list(Subst, Args0, Args),
        Type = tuple_type(Args, Kind)
    ;
        Type0 = apply_n_type(TVar, Args0, Kind),
        apply_rec_subst_to_type_list(Subst, Args0, Args),
        ( if map.search(Subst, TVar, AppliedType0) then
            apply_rec_subst_to_type(Subst, AppliedType0, AppliedType),
            apply_type_args(AppliedType, Args, Type)
        else
            Type = apply_n_type(TVar, Args, Kind)
        )
    ;
        Type0 = kinded_type(BaseType0, Kind),
        apply_rec_subst_to_type(Subst, BaseType0, BaseType),
        Type = kinded_type(BaseType, Kind)
    ).

%---------------------------------------------------------------------------%

apply_variable_renaming_to_type_list(Renaming, Types0, Types) :-
    list.map(apply_variable_renaming_to_type(Renaming), Types0, Types).

apply_subst_to_type_list(Subst, Types0, Types) :-
    list.map(apply_subst_to_type(Subst), Types0, Types).

apply_rec_subst_to_type_list(Subst, Types0, Types) :-
    list.map(apply_rec_subst_to_type(Subst), Types0, Types).

%---------------------------------------------------------------------------%

:- pred apply_type_args(mer_type::in, list(mer_type)::in, mer_type::out)
    is det.

apply_type_args(Type0, Args, Type) :-
    (
        Type0 = type_variable(TVar, Kind0),
        apply_type_args_to_kind(Kind0, Args, Kind),
        Type = apply_n_type(TVar, Args, Kind)
    ;
        Type0 = defined_type(Name, Args0, Kind0),
        apply_type_args_to_kind(Kind0, Args, Kind),
        Type = defined_type(Name, Args0 ++ Args, Kind)
    ;
        ( Type0 = builtin_type(_)
        ; Type0 = higher_order_type(_, _, _, _, _)
        ),
        (
            Args = []
        ;
            Args = [_ | _],
            unexpected($pred, "applied type args to builtin")
        ),
        Type = Type0
    ;
        Type0 = tuple_type(Args0, Kind0),
        apply_type_args_to_kind(Kind0, Args, Kind),
        Type = tuple_type(Args0 ++ Args, Kind)
    ;
        Type0 = apply_n_type(TVar, Args0, Kind0),
        apply_type_args_to_kind(Kind0, Args, Kind),
        Type = apply_n_type(TVar, Args0 ++ Args, Kind)
    ;
        Type0 = kinded_type(BaseType0, _),
        % We drop the explicit kind annotation, since:
        %   - it will already have been used by kind inference, and
        %   - it no longer corresponds to any explicit annotation given.
        apply_type_args(BaseType0, Args, Type)
    ).

:- pred apply_type_args_to_kind(kind::in, list(mer_type)::in, kind::out)
    is det.

apply_type_args_to_kind(Kind0, ArgTypes, Kind) :-
    (
        ArgTypes = [],
        Kind = Kind0
    ;
        ArgTypes = [HeadArgType | TailArgTypes],
        (
            Kind0 = kind_star,
            unexpected($pred, "too many args in apply_n")
        ;
            Kind0 = kind_arrow(KindA, KindB),
            ( if get_type_kind(HeadArgType) = KindA then
                apply_type_args_to_kind(KindB, TailArgTypes, Kind)
            else
                unexpected($pred, "kind error in apply_n")
            )
        ;
            Kind0 = kind_variable(_),
            unexpected($pred, "unbound kind variable")
        )
    ).

:- pred ensure_type_has_kind(kind::in, mer_type::in, mer_type::out) is det.

ensure_type_has_kind(Kind, Type0, Type) :-
    ( if get_type_kind(Type0) = Kind then
        Type = Type0
    else
        unexpected($pred, "substitution not kind preserving")
    ).

%---------------------------------------------------------------------------%

apply_variable_renaming_to_prog_constraint(Renaming, !Constraint) :-
    !.Constraint = constraint(ClassName, ClassArgTypes0),
    apply_variable_renaming_to_type_list(Renaming,
        ClassArgTypes0, ClassArgTypes),
    !:Constraint = constraint(ClassName, ClassArgTypes).

apply_subst_to_prog_constraint(Subst, !Constraint) :-
    !.Constraint = constraint(ClassName, Types0),
    apply_subst_to_type_list(Subst, Types0, Types),
    !:Constraint = constraint(ClassName, Types).

apply_rec_subst_to_prog_constraint(Subst, !Constraint) :-
    !.Constraint = constraint(ClassName, Types0),
    apply_rec_subst_to_type_list(Subst, Types0, Types),
    !:Constraint = constraint(ClassName, Types).

%---------------------------------------------------------------------------%

apply_variable_renaming_to_prog_constraint_list(Renaming, !Constraints) :-
    list.map(apply_variable_renaming_to_prog_constraint(Renaming),
        !Constraints).

apply_subst_to_prog_constraint_list(Subst, !Constraints) :-
    list.map(apply_subst_to_prog_constraint(Subst), !Constraints).

apply_rec_subst_to_prog_constraint_list(Subst, !Constraints) :-
    list.map(apply_rec_subst_to_prog_constraint(Subst), !Constraints).

%---------------------------------------------------------------------------%

apply_variable_renaming_to_prog_constraints(Renaming, !Constraints) :-
    !.Constraints = constraints(UnivConstraints0, ExistConstraints0),
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        UnivConstraints0, UnivConstraints),
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        ExistConstraints0, ExistConstraints),
    !:Constraints = constraints(UnivConstraints, ExistConstraints).

apply_subst_to_prog_constraints(Subst,
        constraints(UniversalCs0, ExistentialCs0),
        constraints(UniversalCs, ExistentialCs)) :-
    apply_subst_to_prog_constraint_list(Subst, UniversalCs0, UniversalCs),
    apply_subst_to_prog_constraint_list(Subst, ExistentialCs0,
        ExistentialCs).

apply_rec_subst_to_prog_constraints(Subst, !Constraints) :-
    !.Constraints = constraints(UnivCs0, ExistCs0),
    apply_rec_subst_to_prog_constraint_list(Subst, UnivCs0, UnivCs),
    apply_rec_subst_to_prog_constraint_list(Subst, ExistCs0, ExistCs),
    !:Constraints = constraints(UnivCs, ExistCs).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_type_subst.
%---------------------------------------------------------------------------%
