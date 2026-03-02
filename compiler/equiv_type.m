%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Expand out equivalence types (and in a few cases, equivalence insts)
% in parts of Mercury program's representation that is shared between
% the parse tree and the HLDS.
%
% Pretty much all of the predicates this module exports are used by
% equiv_type_parse_tree.m. Some are also used by equiv_type_hlds.m.
% And a very few are used by other modules of the compiler as well
% (decide_type_repn.m and qual_info.m, as of 2026 mar 1).
%
%---------------------------------------------------------------------------%

:- module parse_tree.equiv_type.
:- interface.

:- import_module libs.
:- import_module libs.maybe_util.
:- import_module parse_tree.build_eqv_maps.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_used_modules.
:- import_module recompilation.
:- import_module recompilation.record_uses.

:- import_module list.
:- import_module one_or_more.
:- import_module set.

%---------------------------------------------------------------------------%

:- type circ_types == set(type_ctor).

:- type maybe_record_sym_name_use
    --->    do_not_record_sym_name_use
    ;       record_sym_name_use(item_visibility).

%---------------------------------------------------------------------------%

:- pred replace_in_ctors(type_eqv_map::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out) is det.

:- pred replace_in_ctors_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    one_or_more(constructor)::in, one_or_more(constructor)::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

%------------------%

:- pred replace_in_univ_exist_constraints(type_eqv_map::in,
    univ_exist_constraints::in, univ_exist_constraints::out,
    tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out) is det.

:- pred replace_in_univ_exist_constraints_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    univ_exist_constraints::in, univ_exist_constraints::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

%------------------%

:- pred replace_in_prog_constraints(type_eqv_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out)
    is det.

:- pred replace_in_prog_constraints_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

%------------------%

:- pred replace_in_type_list(type_eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, maybe_changed::out,
    tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out) is det.

:- pred replace_in_type_list_location(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

:- pred replace_in_type_list_location_circ(type_eqv_map::in,
    maybe_record_sym_name_use::in, list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

%------------------%

    % Replace all equivalence types in a given type, reporting
    % any circularities, and whether the type has changed.
    %
:- pred replace_in_type_report_circular_eqvs(type_eqv_map::in, tvarset::in,
    prog_context::in, mer_type::in, mer_type::out, maybe_changed::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Replace equivalence types in a given type.
    % The bool output is `yes' if anything changed.
    %
:- pred replace_in_type(type_eqv_map::in, mer_type::in, mer_type::out,
    maybe_changed::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out) is det.

%------------------%

:- pred replace_in_type_maybe_record_use_ignore_circ(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    mer_type::in, mer_type::out, maybe_changed::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

    % Replace all equivalence types in a given type, detecting
    % any circularities.
    %
:- pred replace_in_type_maybe_record_use(type_eqv_map::in,
    maybe_record_sym_name_use::in, list(type_ctor)::in,
    mer_type::in, mer_type::out,
    maybe_changed::out, circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

%---------------------------------------------------------------------------%

:- pred replace_in_inst(inst_eqv_map::in, maybe_record_sym_name_use::in,
    mer_inst::in, mer_inst::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type_subst.
:- import_module recompilation.item_types.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

replace_in_ctors(TypeEqvMap, !Ctors, !TVarSet, !ItemRecompDeps) :-
    replace_in_ctors_location(TypeEqvMap, do_not_record_sym_name_use,
        !Ctors, !TVarSet, !ItemRecompDeps, used_modules_init, _).

replace_in_ctors_location(TypeEqvMap, MaybeRecord, Ctors0, Ctors, !TVarSet,
        !ItemRecompDeps, !UsedModules) :-
    Ctors0 = one_or_more(HeadCtor0, TailCtors0),
    replace_in_ctor(TypeEqvMap, MaybeRecord, HeadCtor0, HeadCtor,
        !TVarSet, !ItemRecompDeps, !UsedModules),
    list.map_foldl3(replace_in_ctor(TypeEqvMap, MaybeRecord),
        TailCtors0, TailCtors,
        !TVarSet, !ItemRecompDeps, !UsedModules),
    Ctors = one_or_more(HeadCtor, TailCtors).

:- pred replace_in_ctor(type_eqv_map::in, maybe_record_sym_name_use::in,
    constructor::in, constructor::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor(TypeEqvMap, MaybeRecord, Ctor0, Ctor,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    Ctor0 = ctor(Ordinal, MaybeExistConstraints0, CtorName, CtorArgs0, Arity,
        Ctxt),
    replace_in_ctor_arg_list(TypeEqvMap, MaybeRecord,
        CtorArgs0, CtorArgs, _, !TVarSet, !ItemRecompDeps, !UsedModules),
    (
        MaybeExistConstraints0 = no_exist_constraints,
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints0 = exist_constraints(ExistConstraints0),
        ExistConstraints0 = cons_exist_constraints(ExistQVars, Constraints0,
            UnconstrainedExistQTVars, ConstrainedExistQTVars),
        replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
            Constraints0, Constraints,
            !TVarSet, !ItemRecompDeps, !UsedModules),
        ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
            UnconstrainedExistQTVars, ConstrainedExistQTVars),
        MaybeExistConstraints = exist_constraints(ExistConstraints)
    ),
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorName, CtorArgs, Arity,
        Ctxt).

%---------------------%

:- pred replace_in_ctor_arg_list(type_eqv_map::in,
    maybe_record_sym_name_use::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list(TypeEqvMap, MaybeRecord, !CtorArgs,
        ContainsCirc, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    replace_in_ctor_arg_list_loop(TypeEqvMap, MaybeRecord, [], !CtorArgs,
        set.init, ContainsCirc, !TVarSet, !ItemRecompDeps, !UsedModules).

:- pred replace_in_ctor_arg_list_loop(type_eqv_map::in,
    maybe_record_sym_name_use::in, list(type_ctor)::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    circ_types::in, circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list_loop(_TypeEqvMap, _MaybeRecord, _Seen, [], [],
        !Circ, !TVarSet, !ItemRecompDeps, !UsedModules).
replace_in_ctor_arg_list_loop(TypeEqvMap, MaybeRecord, Seen,
        [CtorArg0 | CtorArgs0], [CtorArg | CtorArgs],
        !Circ, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    CtorArg0 = ctor_arg(Name, Type0, Context),
    replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, Seen,
        Type0, Type, _, TypeCirc, !TVarSet, !ItemRecompDeps, !UsedModules),
    CtorArg = ctor_arg(Name, Type, Context),
    set.union(TypeCirc, !Circ),
    replace_in_ctor_arg_list_loop(TypeEqvMap, MaybeRecord, Seen,
        CtorArgs0, CtorArgs, !Circ, !TVarSet, !ItemRecompDeps, !UsedModules).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

replace_in_univ_exist_constraints(TypeEqvMap, Cs0, Cs,
        !TVarSet, !ItemRecompDeps) :-
    replace_in_univ_exist_constraints_location(TypeEqvMap,
        do_not_record_sym_name_use, Cs0, Cs,
        !TVarSet, !ItemRecompDeps, used_modules_init, _).

replace_in_univ_exist_constraints_location(TypeEqvMap, MaybeRecord, Cs0, Cs,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    Cs0 = univ_exist_constraints(UnivCs0, ExistCs0),
    replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        UnivCs0, UnivCs, !TVarSet, !ItemRecompDeps, !UsedModules),
    replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        ExistCs0, ExistCs, !TVarSet, !ItemRecompDeps, !UsedModules),
    Cs = univ_exist_constraints(UnivCs, ExistCs).

replace_in_prog_constraints(TypeEqvMap,
        !Constraints, !TVarSet, !ItemRecompDeps) :-
    replace_in_prog_constraints_location(TypeEqvMap,
        do_not_record_sym_name_use, !Constraints,
        !TVarSet, !ItemRecompDeps, used_modules_init, _).

replace_in_prog_constraints_location(TypeEqvMap, MaybeRecord,
        !Constraints, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    list.map_foldl3(
        replace_in_prog_constraint_location(TypeEqvMap, MaybeRecord),
        !Constraints, !TVarSet, !ItemRecompDeps, !UsedModules).

:- pred replace_in_prog_constraint_location(type_eqv_map::in,
    maybe_record_sym_name_use::in, prog_constraint::in, prog_constraint::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_location(TypeEqvMap, MaybeRecord,
        Constraint0, Constraint, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    Constraint0 = constraint(ClassName, ArgTypes0),
    replace_in_type_list_location_circ(TypeEqvMap, MaybeRecord,
        ArgTypes0, ArgTypes, _, _, !TVarSet, !ItemRecompDeps, !UsedModules),
    Constraint = constraint(ClassName, ArgTypes).

%---------------------------------------------------------------------------%

replace_in_type_list(TypeEqvMap, !Types, Changed, !TVarSet, !ItemRecompDeps) :-
    replace_in_type_list_location(TypeEqvMap, do_not_record_sym_name_use,
        !Types, Changed, !TVarSet, !ItemRecompDeps, used_modules_init, _).

replace_in_type_list_location(TypeEqvMap, MaybeRecord, !Types,
        Changed, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    replace_in_type_list_location_circ(TypeEqvMap, MaybeRecord, !Types,
        Changed, _, !TVarSet, !ItemRecompDeps, !UsedModules).

replace_in_type_list_location_circ(TypeEqvMap, MaybeRecord, !Types,
        Changed, ContainsCirc, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord, [], !Types,
        Changed, set.init, ContainsCirc, !TVarSet,
        !ItemRecompDeps, !UsedModules).

:- pred replace_in_type_list_location_acc_circ(type_eqv_map::in,
    maybe_record_sym_name_use::in, list(type_ctor)::in,
    list(mer_type)::in, list(mer_type)::out,
    maybe_changed::out, circ_types::in, circ_types::out,
    tvarset::in, tvarset::out, item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_acc_circ(_TypeEqvMap, _MaybeRecord, _Seen,
        [], [], unchanged, !ContainsCirc, !TVarSet,
        !ItemRecompDeps, !UsedModules).
replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord, Seen,
        Types0 @ [HeadType0 | TailTypes0], Types, Changed, !Circ, !TVarSet,
        !ItemRecompDeps, !UsedModules) :-
    replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, Seen,
        HeadType0, HeadType, HeadChanged, HeadCirc, !TVarSet,
        !ItemRecompDeps, !UsedModules),
    set.union(HeadCirc, !Circ),
    replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord, Seen,
        TailTypes0, TailTypes, TailChanged, !Circ, !TVarSet,
        !ItemRecompDeps, !UsedModules),
    ( if
        ( HeadChanged = changed
        ; TailChanged = changed
        )
    then
        Changed = changed,
        Types = [HeadType | TailTypes]
    else
        Changed = unchanged,
        Types = Types0
    ).

%---------------------------------------------------------------------------%

replace_in_type_report_circular_eqvs(TypeEqvMap, TVarSet0, Context,
        Type0, Type, Changed, !Specs) :-
    replace_in_type_maybe_record_use(TypeEqvMap, do_not_record_sym_name_use,
        [], Type0, Type, Changed, Circ,
        TVarSet0, _TVarSet, no_item_recomp_deps, _, used_modules_init, _),
    set.to_sorted_list(Circ, CircTypes),
    (
        CircTypes = [HeadCircTypeCtor | TailCircTypeCtors],
        Spec = report_contains_circular_eqv_type(TVarSet0, Type0, Context,
            HeadCircTypeCtor, TailCircTypeCtors),
        !:Specs = [Spec | !.Specs]
    ;
        CircTypes = []
    ).

replace_in_type(TypeEqvMap, Type0, Type, Changed, !TVarSet, !ItemRecompDeps) :-
    replace_in_type_maybe_record_use(TypeEqvMap, do_not_record_sym_name_use,
        [], Type0, Type, Changed, _Circ, !TVarSet,
        !ItemRecompDeps, used_modules_init, _).

replace_in_type_maybe_record_use_ignore_circ(TypeEqvMap, MaybeRecord,
        Type0, Type, Changed, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord, [],
        Type0, Type, Changed, _, !TVarSet, !ItemRecompDeps, !UsedModules).

replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord,
        TypeCtorsAlreadyExpanded, Type0, Type, Changed, Circ,
        !TVarSet, !ItemRecompDeps, !UsedModules) :-
    (
        Type0 = type_variable(Var, Kind),
        Type = type_variable(Var, Kind),
        Changed = unchanged,
        Circ = set.init
    ;
        Type0 = defined_type(SymName, ArgTypes0, Kind),
        replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, ArgTypes0, ArgTypes, ArgTypesChanged,
            set.init, Circ0, !TVarSet, !ItemRecompDeps, !UsedModules),
        Arity = list.length(ArgTypes),
        TypeCtor = type_ctor(SymName, Arity),
        replace_type_ctor(TypeEqvMap, MaybeRecord, TypeCtorsAlreadyExpanded,
            Type0, TypeCtor, ArgTypes, Kind, Type, ArgTypesChanged, Changed,
            Circ0, Circ, !TVarSet, !ItemRecompDeps, !UsedModules)
    ;
        Type0 = builtin_type(_),
        Type = Type0,
        Changed = unchanged,
        Circ = set.init
    ;
        Type0 = higher_order_type(PorF, HOArgTypes0, HOInstInfo, Purity),
        replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, HOArgTypes0, HOArgTypes, Changed,
            set.init, Circ, !TVarSet, !ItemRecompDeps, !UsedModules),
        (
            Changed = changed,
            Type = higher_order_type(PorF, HOArgTypes, HOInstInfo, Purity)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ;
        Type0 = tuple_type(TupleArgTypes0, Kind),
        replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, TupleArgTypes0, TupleArgTypes, Changed,
            set.init, Circ, !TVarSet, !ItemRecompDeps, !UsedModules),
        (
            Changed = changed,
            Type = tuple_type(TupleArgTypes, Kind)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ;
        Type0 = apply_n_type(Var, ApplyArgTypes0, Kind),
        replace_in_type_list_location_acc_circ(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, ApplyArgTypes0, ApplyArgTypes, Changed,
            set.init, Circ, !TVarSet, !ItemRecompDeps, !UsedModules),
        (
            Changed = changed,
            Type = apply_n_type(Var, ApplyArgTypes, Kind)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ;
        Type0 = kinded_type(RawType0, Kind),
        replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord,
            TypeCtorsAlreadyExpanded, RawType0, RawType, Changed, Circ,
            !TVarSet, !ItemRecompDeps, !UsedModules),
        (
            Changed = changed,
            Type = kinded_type(RawType, Kind)
        ;
            Changed = unchanged,
            Type = Type0
        )
    ).

%---------------------------------------------------------------------------%

:- pred replace_type_ctor(type_eqv_map::in, maybe_record_sym_name_use::in,
    list(type_ctor)::in, mer_type::in, type_ctor::in, list(mer_type)::in,
    kind::in, mer_type::out, maybe_changed::in, maybe_changed::out,
    circ_types::in, circ_types::out, tvarset::in, tvarset::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_type_ctor(TypeEqvMap, MaybeRecord, TypeCtorsAlreadyExpanded, Type0,
        TypeCtor, ArgTypes, Kind, Type, ArgTypesChanged, Changed,
        !Circ, !TVarSet, !ItemRecompDeps, !UsedModules) :-
    ( if list.member(TypeCtor, TypeCtorsAlreadyExpanded) then
        AlreadyExpanded = yes,
        NewCirc = set.make_singleton_set(TypeCtor)
    else
        AlreadyExpanded = no,
        NewCirc = set.init
    ),
    ( if
        map.search(TypeEqvMap, TypeCtor, EqvTypeBody),
        EqvTypeBody = eqv_type_body(EqvTVarSet, EqvTypeParams0, Body0),
        % Don't merge in the variable names from the type declaration,
        % in order to to avoid creating multiple variables with the same name.
        % This is so that make_hlds can later use `varset.create_name_var_map'
        % on the resulting tvarset to match up type variables in
        % `:- pragma type_spec' declarations, and explicit type qualifications
        % with the type variables in the predicate's declaration.
        % XXX That paragraphs should be simplified by someone who understands
        % exactly what it is talking about :-(.
        tvarset_merge_renaming_without_names(!.TVarSet, EqvTVarSet, !:TVarSet,
            Renaming),
        set.is_empty(!.Circ),
        AlreadyExpanded = no
    then
        maybe_record_type_ctor_sym_name_use(MaybeRecord, TypeCtor,
            !UsedModules),
        Changed = changed,
        map.apply_to_list(EqvTypeParams0, Renaming, EqvTypeParams),
        apply_renaming_to_type(Renaming, Body0, Body1),
        TypeCtorItem = type_ctor_to_recomp_item_name(TypeCtor),
        gather_item_recomp_dep(recomp_item_id(recomp_type_name, TypeCtorItem),
            !ItemRecompDeps),
        map.from_corresponding_lists(EqvTypeParams, ArgTypes, Subst),
        apply_subst_to_type(Subst, Body1, Body),
        replace_in_type_maybe_record_use(TypeEqvMap, MaybeRecord,
            [TypeCtor | TypeCtorsAlreadyExpanded], Body,
            Type, _, !:Circ, !TVarSet, !ItemRecompDeps, !UsedModules)
    else
        (
            ArgTypesChanged = changed,
            TypeCtor = type_ctor(SymName, _Arity),
            Type = defined_type(SymName, ArgTypes, Kind)
        ;
            ArgTypesChanged = unchanged,
            Type = Type0
        ),
        Changed = ArgTypesChanged,
        set.union(NewCirc, !Circ)
    ).

%---------------------------------------------------------------------------%

replace_in_inst(InstEqvMap, MaybeRecord, Inst0, Inst,
        !ItemRecompDeps, !UsedModules) :-
    replace_in_inst_location(InstEqvMap, MaybeRecord, set.init, Inst0, Inst,
        !ItemRecompDeps, !UsedModules).

:- pred replace_in_inst_location(inst_eqv_map::in,
    maybe_record_sym_name_use::in, set(inst_ctor)::in,
    mer_inst::in, mer_inst::out,
    item_recomp_deps::in, item_recomp_deps::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst_location(InstEqvMap, MaybeRecord,
        ExpandedInstCtors0, Inst0, Inst, !ItemRecompDeps, !UsedModules) :-
    % XXX Need to record the used modules
    ( if Inst0 = defined_inst(user_inst(SymName, ArgInsts)) then
        InstCtor = inst_ctor(SymName, length(ArgInsts)),
        ( if
            set.member(InstCtor, ExpandedInstCtors0)
        then
            Inst = Inst0
        else if
            map.search(InstEqvMap, InstCtor, EqvInstBody),
            EqvInstBody = eqv_inst_body(EqvInstParams, EqvInst)
        then
            inst_substitute_arg_list(EqvInstParams, ArgInsts, EqvInst, Inst1),
            InstCtorItem = inst_ctor_to_recomp_item_name(InstCtor),
            gather_item_recomp_dep(recomp_item_id(recomp_inst, InstCtorItem),
                !ItemRecompDeps),
            set.insert(InstCtor, ExpandedInstCtors0, ExpandedInstCtors),
            replace_in_inst_location(InstEqvMap, MaybeRecord,
                ExpandedInstCtors, Inst1, Inst, !ItemRecompDeps, !UsedModules)
        else
            Inst = Inst0
        )
    else
        Inst = Inst0
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred maybe_record_type_ctor_sym_name_use(maybe_record_sym_name_use::in,
    type_ctor::in, used_modules::in, used_modules::out) is det.

maybe_record_type_ctor_sym_name_use(MaybeRecord, TypeCtor, !UsedModules) :-
    (
        MaybeRecord = do_not_record_sym_name_use
    ;
        MaybeRecord = record_sym_name_use(Visibility),
        TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
        record_sym_name_module_as_used(Visibility, TypeCtorSymName,
            !UsedModules)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- func report_contains_circular_eqv_type(tvarset, mer_type, prog_context,
    type_ctor, list(type_ctor)) = error_spec.

report_contains_circular_eqv_type(TVarSet, Type, Context,
        HeadTypeCtor, TailTypeCtors) = Spec :-
    TypeStr = mercury_type_to_string(TVarSet, print_name_only, Type),
    MainPieces = [words("Error: the type")] ++
        color_as_subject([quote(TypeStr)]) ++
        [words("cannot have its equivalences fully expanded,"),
        words("because its expansion contains the")],
    (
        TailTypeCtors = [],
        CircSpecs =
            color_as_incorrect([words("circular equivalence type")]) ++
            color_as_subject([qual_type_ctor(HeadTypeCtor), suffix(".")]) ++
            [nl]
    ;
        TailTypeCtors = [_ | _],
        TypeCtorPieces = list.map((func(TC) = qual_type_ctor(TC)),
            [HeadTypeCtor | TailTypeCtors]),
        CircSpecs =
            color_as_incorrect([words("circular equivalence types")]) ++
            piece_list_to_color_pieces(color_subject, "and", [suffix(".")],
                TypeCtorPieces) ++
            [nl]
    ),
    Pieces = MainPieces ++ CircSpecs,
    Spec = spec($pred, severity_error, phase_expand_types, Context, Pieces).

%---------------------------------------------------------------------------%
:- end_module parse_tree.equiv_type.
%---------------------------------------------------------------------------%
