%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Operations for performing various kinds of type substitutions on data
% structures that are part of the parse tree.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_type_subst.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%
%
% Type substitutions.
%

:- pred apply_variable_renaming_to_tvar_kind_map(tvar_renaming::in,
    tvar_kind_map::in, tvar_kind_map::out) is det.

%---------%

:- pred apply_variable_renaming_to_tvar(tvar_renaming::in,
    tvar::in, tvar::out) is det.

:- pred apply_subst_to_tvar(tvar_kind_map::in, tsubst::in,
    tvar::in, mer_type::out) is det.

:- pred apply_rec_subst_to_tvar(tvar_kind_map::in, tsubst::in,
    tvar::in, mer_type::out) is det.

%---------%

:- pred apply_variable_renaming_to_tvar_list(tvar_renaming::in,
    list(tvar)::in, list(tvar)::out) is det.

:- pred apply_subst_to_tvar_list(tvar_kind_map::in, tsubst::in,
    list(tvar)::in, list(mer_type)::out) is det.

:- pred apply_rec_subst_to_tvar_list(tvar_kind_map::in, tsubst::in,
    list(tvar)::in, list(mer_type)::out) is det.

%---------%

:- pred apply_variable_renaming_to_type(tvar_renaming::in,
    mer_type::in, mer_type::out) is det.

:- pred apply_subst_to_type(tsubst::in, mer_type::in, mer_type::out) is det.

:- pred apply_rec_subst_to_type(tsubst::in, mer_type::in, mer_type::out)
    is det.

%---------%

:- pred apply_variable_renaming_to_type_list(tvar_renaming::in,
    list(mer_type)::in, list(mer_type)::out) is det.

:- pred apply_subst_to_type_list(tsubst::in,
    list(mer_type)::in, list(mer_type)::out) is det.

:- pred apply_rec_subst_to_type_list(tsubst::in,
    list(mer_type)::in, list(mer_type)::out) is det.

%---------%

:- pred apply_variable_renaming_to_vartypes(tvar_renaming::in,
    vartypes::in, vartypes::out) is det.

:- pred apply_subst_to_vartypes(tsubst::in, vartypes::in, vartypes::out)
    is det.

:- pred apply_rec_subst_to_vartypes(tsubst::in, vartypes::in, vartypes::out)
    is det.

%-----------------------------------------------------------------------------%
%
% Utility predicates dealing with typeclass constraints.
%

:- pred apply_variable_renaming_to_prog_constraint(tvar_renaming::in,
    prog_constraint::in, prog_constraint::out) is det.

:- pred apply_subst_to_prog_constraint(tsubst::in, prog_constraint::in,
    prog_constraint::out) is det.

:- pred apply_rec_subst_to_prog_constraint(tsubst::in, prog_constraint::in,
    prog_constraint::out) is det.

%---------%

:- pred apply_variable_renaming_to_prog_constraint_list(tvar_renaming::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_subst_to_prog_constraint_list(tsubst::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_rec_subst_to_prog_constraint_list(tsubst::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

%---------%

:- pred apply_variable_renaming_to_prog_constraints(tvar_renaming::in,
    prog_constraints::in, prog_constraints::out) is det.

:- pred apply_subst_to_prog_constraints(tsubst::in, prog_constraints::in,
    prog_constraints::out) is det.

:- pred apply_rec_subst_to_prog_constraints(tsubst::in, prog_constraints::in,
    prog_constraints::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.

:- import_module map.
:- import_module maybe.
:- import_module svmap.

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_tvar_kind_map(Renaming, KindMap0, KindMap) :-
    map.foldl(apply_variable_renaming_to_tvar_kind_map_2(Renaming),
        KindMap0, map.init, KindMap).

:- pred apply_variable_renaming_to_tvar_kind_map_2(tvar_renaming::in, tvar::in,
    kind::in, tvar_kind_map::in, tvar_kind_map::out) is det.

apply_variable_renaming_to_tvar_kind_map_2(Renaming, TVar0, Kind, !KindMap) :-
    apply_variable_renaming_to_tvar(Renaming, TVar0, TVar),
    svmap.det_insert(TVar, Kind, !KindMap).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_tvar(Renaming, TVar0, TVar) :-
    ( map.search(Renaming, TVar0, TVar1) ->
        TVar = TVar1
    ;
        TVar = TVar0
    ).

apply_subst_to_tvar(KindMap, Subst, TVar, Type) :-
    ( map.search(Subst, TVar, Type0) ->
        apply_subst_to_type(Subst, Type0, Type)
    ;
        get_tvar_kind(KindMap, TVar, Kind),
        Type = type_variable(TVar, Kind)
    ).

apply_rec_subst_to_tvar(KindMap, Subst, TVar, Type) :-
    ( map.search(Subst, TVar, Type0) ->
        apply_rec_subst_to_type(Subst, Type0, Type)
    ;
        get_tvar_kind(KindMap, TVar, Kind),
        Type = type_variable(TVar, Kind)
    ).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_tvar_list(Renaming, TVars0, TVars) :-
    list.map(apply_variable_renaming_to_tvar(Renaming), TVars0, TVars).

apply_subst_to_tvar_list(KindMap, Subst, TVars, Types) :-
    list.map(apply_subst_to_tvar(KindMap, Subst), TVars, Types).

apply_rec_subst_to_tvar_list(KindMap, Subst, TVars, Types) :-
    list.map(apply_rec_subst_to_tvar(KindMap, Subst), TVars, Types).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_type(Renaming, type_variable(TVar0, Kind),
        type_variable(TVar, Kind)) :-
    apply_variable_renaming_to_tvar(Renaming, TVar0, TVar).
apply_variable_renaming_to_type(Renaming, defined_type(Name, Args0, Kind),
        defined_type(Name, Args, Kind)) :-
    apply_variable_renaming_to_type_list(Renaming, Args0, Args).
apply_variable_renaming_to_type(_Renaming, Type @ builtin_type(_), Type).
apply_variable_renaming_to_type(Renaming,
        higher_order_type(Args0, MaybeReturn0, Purity, EvalMethod),
        higher_order_type(Args, MaybeReturn, Purity, EvalMethod)) :-
    apply_variable_renaming_to_type_list(Renaming, Args0, Args),
    (
        MaybeReturn0 = yes(Return0),
        apply_variable_renaming_to_type(Renaming, Return0, Return),
        MaybeReturn = yes(Return)
    ;
        MaybeReturn0 = no,
        MaybeReturn = no
    ).
apply_variable_renaming_to_type(Renaming, tuple_type(Args0, Kind),
        tuple_type(Args, Kind)) :-
    apply_variable_renaming_to_type_list(Renaming, Args0, Args).
apply_variable_renaming_to_type(Renaming, apply_n_type(TVar0, Args0, Kind),
        apply_n_type(TVar, Args, Kind)) :-
    apply_variable_renaming_to_type_list(Renaming, Args0, Args),
    apply_variable_renaming_to_tvar(Renaming, TVar0, TVar).
apply_variable_renaming_to_type(Renaming, kinded_type(Type0, Kind),
        kinded_type(Type, Kind)) :-
    apply_variable_renaming_to_type(Renaming, Type0, Type).

apply_subst_to_type(Subst, Type0 @ type_variable(TVar, Kind), Type) :-
    ( map.search(Subst, TVar, Type1) ->
        ensure_type_has_kind(Kind, Type1, Type)
    ;
        Type = Type0
    ).
apply_subst_to_type(Subst, defined_type(Name, Args0, Kind),
        defined_type(Name, Args, Kind)) :-
    apply_subst_to_type_list(Subst, Args0, Args).
apply_subst_to_type(_Subst, Type @ builtin_type(_), Type).
apply_subst_to_type(Subst,
        higher_order_type(Args0, MaybeReturn0, Purity, EvalMethod),
        higher_order_type(Args, MaybeReturn, Purity, EvalMethod)) :-
    apply_subst_to_type_list(Subst, Args0, Args),
    (
        MaybeReturn0 = yes(Return0),
        apply_subst_to_type(Subst, Return0, Return),
        MaybeReturn = yes(Return)
    ;
        MaybeReturn0 = no,
        MaybeReturn = no
    ).
apply_subst_to_type(Subst, tuple_type(Args0, Kind), tuple_type(Args, Kind)) :-
    apply_subst_to_type_list(Subst, Args0, Args).
apply_subst_to_type(Subst, apply_n_type(TVar, Args0, Kind), Type) :-
    apply_subst_to_type_list(Subst, Args0, Args),
    ( map.search(Subst, TVar, AppliedType) ->
        apply_type_args(AppliedType, Args, Type)
    ;
        Type = apply_n_type(TVar, Args, Kind)
    ).
apply_subst_to_type(Subst, kinded_type(Type0, Kind),
        kinded_type(Type, Kind)) :-
    apply_subst_to_type(Subst, Type0, Type).

apply_rec_subst_to_type(Subst, Type0 @ type_variable(TVar, Kind), Type) :-
    ( map.search(Subst, TVar, Type1) ->
        ensure_type_has_kind(Kind, Type1, Type2),
        apply_rec_subst_to_type(Subst, Type2, Type)
    ;
        Type = Type0
    ).
apply_rec_subst_to_type(Subst, defined_type(Name, Args0, Kind),
        defined_type(Name, Args, Kind)) :-
    apply_rec_subst_to_type_list(Subst, Args0, Args).
apply_rec_subst_to_type(_Subst, Type @ builtin_type(_), Type).
apply_rec_subst_to_type(Subst,
        higher_order_type(Args0, MaybeReturn0, Purity, EvalMethod),
        higher_order_type(Args, MaybeReturn, Purity, EvalMethod)) :-
    apply_rec_subst_to_type_list(Subst, Args0, Args),
    (
        MaybeReturn0 = yes(Return0),
        apply_rec_subst_to_type(Subst, Return0, Return),
        MaybeReturn = yes(Return)
    ;
        MaybeReturn0 = no,
        MaybeReturn = no
    ).
apply_rec_subst_to_type(Subst, tuple_type(Args0, Kind),
        tuple_type(Args, Kind)) :-
    apply_rec_subst_to_type_list(Subst, Args0, Args).
apply_rec_subst_to_type(Subst, apply_n_type(TVar, Args0, Kind), Type) :-
    apply_rec_subst_to_type_list(Subst, Args0, Args),
    ( map.search(Subst, TVar, AppliedType0) ->
        apply_rec_subst_to_type(Subst, AppliedType0, AppliedType),
        apply_type_args(AppliedType, Args, Type)
    ;
        Type = apply_n_type(TVar, Args, Kind)
    ).
apply_rec_subst_to_type(Subst, kinded_type(Type0, Kind),
        kinded_type(Type, Kind)) :-
    apply_rec_subst_to_type(Subst, Type0, Type).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_type_list(Renaming, Types0, Types) :-
    list.map(apply_variable_renaming_to_type(Renaming), Types0, Types).

apply_subst_to_type_list(Subst, Types0, Types) :-
    list.map(apply_subst_to_type(Subst), Types0, Types).

apply_rec_subst_to_type_list(Subst, Types0, Types) :-
    list.map(apply_rec_subst_to_type(Subst), Types0, Types).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_vartypes(Renaming, !Map) :-
    map.map_values(apply_variable_renaming_to_vartypes_2(Renaming), !Map).

:- pred apply_variable_renaming_to_vartypes_2(tvar_renaming::in, prog_var::in,
    mer_type::in, mer_type::out) is det.

apply_variable_renaming_to_vartypes_2(Renaming, _, !Type) :-
    apply_variable_renaming_to_type(Renaming, !Type).

apply_subst_to_vartypes(Subst, !VarTypes) :-
    map.map_values(apply_subst_to_vartypes_2(Subst), !VarTypes).

:- pred apply_subst_to_vartypes_2(tsubst::in, prog_var::in,
    mer_type::in, mer_type::out) is det.

apply_subst_to_vartypes_2(Subst, _, !Type) :-
    apply_subst_to_type(Subst, !Type).

apply_rec_subst_to_vartypes(Subst, !VarTypes) :-
    map.map_values(apply_rec_subst_to_vartypes_2(Subst), !VarTypes).

:- pred apply_rec_subst_to_vartypes_2(tsubst::in, prog_var::in,
    mer_type::in, mer_type::out) is det.

apply_rec_subst_to_vartypes_2(Subst, _, !Type) :-
    apply_rec_subst_to_type(Subst, !Type).

%-----------------------------------------------------------------------------%

:- pred apply_type_args(mer_type::in, list(mer_type)::in, mer_type::out)
    is det.

apply_type_args(type_variable(TVar, Kind0), Args,
        apply_n_type(TVar, Args, Kind)) :-
    apply_type_args_to_kind(Kind0, Args, Kind).
apply_type_args(defined_type(Name, Args0, Kind0), Args,
        defined_type(Name, Args0 ++ Args, Kind)) :-
    apply_type_args_to_kind(Kind0, Args, Kind).
apply_type_args(Type @ builtin_type(_), [], Type).
apply_type_args(builtin_type(_), [_ | _], _) :-
    unexpected(this_file, "applied type args to builtin").
apply_type_args(Type @ higher_order_type(_, _, _, _), [], Type).
apply_type_args(higher_order_type(_, _, _, _), [_ | _], _) :-
    unexpected(this_file, "applied type args to higher_order").
apply_type_args(tuple_type(Args0, Kind0), Args,
        tuple_type(Args0 ++ Args, Kind)) :-
    apply_type_args_to_kind(Kind0, Args, Kind).
apply_type_args(apply_n_type(TVar, Args0, Kind0), Args,
        apply_n_type(TVar, Args0 ++ Args, Kind)) :-
    apply_type_args_to_kind(Kind0, Args, Kind).
apply_type_args(kinded_type(Type0, _), Args, Type) :-
    % We drop the explicit kind annotation, since:
    %   - it will already have been used by kind inference, and
    %   - it no longer corresponds to any explicit annotation given.
    apply_type_args(Type0, Args, Type).

:- pred apply_type_args_to_kind(kind::in, list(mer_type)::in, kind::out)
    is det.

apply_type_args_to_kind(Kind, [], Kind).
apply_type_args_to_kind(kind_star, [_ | _], _) :-
    unexpected(this_file, "too many args in apply_n").
apply_type_args_to_kind(kind_arrow(Kind0, Kind1), [ArgType | ArgTypes], Kind) :-
    ( get_type_kind(ArgType) = Kind0 ->
        apply_type_args_to_kind(Kind1, ArgTypes, Kind)
    ;
        unexpected(this_file, "kind error in apply_n")
    ).
apply_type_args_to_kind(kind_variable(_), [_ | _], _) :-
    unexpected(this_file, "unbound kind variable").

:- pred ensure_type_has_kind(kind::in, mer_type::in, mer_type::out) is det.

ensure_type_has_kind(Kind, Type0, Type) :-
    ( get_type_kind(Type0) = Kind ->
        Type = Type0
    ;
        unexpected(this_file, "substitution not kind preserving")
    ).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_prog_constraint_list(Renaming, !Constraints) :-
    list.map(apply_variable_renaming_to_prog_constraint(Renaming),
        !Constraints).

apply_subst_to_prog_constraint_list(Subst, !Constraints) :-
    list.map(apply_subst_to_prog_constraint(Subst), !Constraints).

apply_rec_subst_to_prog_constraint_list(Subst, !Constraints) :-
    list.map(apply_rec_subst_to_prog_constraint(Subst), !Constraints).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_type_subst.m".

%-----------------------------------------------------------------------------%
