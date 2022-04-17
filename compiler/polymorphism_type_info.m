%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012, 2014 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: polymorphism_info.m.
% Main authors: fjh and zs (when this code was in polymorphism.m).
%
% This module defines the poly_info structure used by polymorphism.m.
%
%---------------------------------------------------------------------------%

:- module check_hlds.polymorphism_type_info.
:- interface.

:- import_module check_hlds.polymorphism_info.
:- import_module hlds.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module list.
:- import_module maybe.
:- import_module term.

%---------------------------------------------------------------------------%

    % Given a type, create a variable to hold the type_info for that type,
    % and create a list of goals to initialize that type_info variable
    % to the appropriate type_info structure for the type.
    % Update the varset and vartypes accordingly.
    %
:- pred polymorphism_make_type_info_var(mer_type::in, term.context::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

    % Given a list of types, create a list of variables to hold the type_infos
    % for those types, and create a list of goals to initialize those type_info
    % variables to the appropriate type_info structures for the types.
    % Update the varset and vartypes accordingly.
    %
:- pred polymorphism_make_type_info_vars(list(mer_type)::in, term.context::in,
    list(prog_var)::out, list(hlds_goal)::out, poly_info::in, poly_info::out)
    is det.

    % A version of polymorphism_make_type_info_vars that also returns
    % information for each var that can be useful when putting type_infos
    % into static data.
    %
:- pred polymorphism_do_make_type_info_vars(list(mer_type)::in,
    term.context::in,
    assoc_list(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

%---------------------%

    % Versions of the above predicates that get their info from, and update,
    % module_infos, pred_infos and proc_infos, rather than a poly_info.
    % These do not store the updated pred_infos and proc_infos back into
    % the module_info.
    %
:- pred polymorphism_make_type_info_var_raw(mer_type::in,
    term.context::in, prog_var::out, list(hlds_goal)::out,
    module_info::in, module_info::out,
    pred_info::in, pred_info::out, proc_info::in, proc_info::out) is det.
:- pred polymorphism_make_type_info_vars_raw(list(mer_type)::in,
    term.context::in, list(prog_var)::out, list(hlds_goal)::out,
    module_info::in, module_info::out,
    pred_info::in, pred_info::out, proc_info::in, proc_info::out) is det.

%---------------------------------------------------------------------------%

    % init_type_info_var(Type, ArgVars, TypeInfoVar, TypeInfoGoal,
    %   !VarSet, !VarTypes) :-
    %
    % Create the unification the constructs the second cell of a type_info
    % for Type. ArgVars should contain the arguments of this unification.
    %
    % This unification WILL lead to the creation of cells on the heap
    % at runtime.
    %
    % The first variable in ArgVars should be bound to the type_ctor_info
    % for Type's principal type constructor. If that type constructor is
    % variable arity, the next variable in ArgVars should be bound to an
    % integer giving Type's actual arity. The remaining variables in
    % ArgVars should be bound to the type_infos or type_ctor_infos giving
    % Type's argument types.
    %
:- pred init_type_info_var(mer_type::in, list(prog_var)::in,
    maybe(prog_var)::in, prog_var::out, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % init_const_type_ctor_info_var(Type, TypeCtor,
    %   TypeCtorInfoVar, TypeCtorConsId, TypeCtorInfoGoal,
    %   !VarSet, !VarTypes, !RttiVarMaps):
    %
    % Create the unification (returned as TypeCtorInfoGoal) that binds a
    % new variable (returned as TypeCtorInfoVar) to the type_ctor_info
    % representing TypeCtor. This will be the constant represented by
    % TypeCtorConsId.
    %
    % This unification WILL NOT lead to the creation of a cell on the
    % heap at runtime; it will cause TypeCtorInfoVar to refer to the
    % statically allocated type_ctor_info cell for the type, allocated
    % in the module that defines the type.
    %
    % We take Type as input for historical reasons: we record Type as
    % the type whose type constructor TypeCtor is, in the type of
    % TypeCtorInfoVar.
    %
:- pred init_const_type_ctor_info_var(mer_type::in, type_ctor::in,
    prog_var::out, cons_id::out, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

%---------------------%

:- type type_info_kind
    --->    type_info
    ;       type_ctor_info.

:- pred new_type_info_var(mer_type::in, type_info_kind::in,
    prog_var::out, poly_info::in, poly_info::out) is det.

:- pred new_type_info_var_raw(mer_type::in, type_info_kind::in,
    prog_var::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

%---------------------%

:- pred get_type_info_locn(tvar::in, type_info_locn::out,
    poly_info::in, poly_info::out) is det.

%---------------------%

:- type int_or_var
    --->    iov_int(int)
    ;       iov_var(prog_var).

    % gen_extract_type_info(ModuleInfo, TypeVar, Kind,
    %   TypeClassInfoVar, IndexIntOrVar, Context, Goals, TypeInfoVar,
    %   !VarSet, !VarTypes, !RttiVarMaps):
    %
    % Generate code to extract a type_info variable from a given slot of a
    % typeclass_info variable, by calling type_info_from_typeclass_info from
    % private_builtin. TypeVar is the type variable to which this type_info
    % variable corresponds. Kind is the kind of the type variable.
    % TypeClassInfoVar is the variable holding the type_class_info.
    % IndexIntOrVar specifies which slot it is. The procedure returns
    % TypeInfoVar, which is a fresh variable holding the type_info,
    % and Goals, which is the code generated to initialize TypeInfoVar.
    % The context of these goals will be Context.
    %
:- pred gen_extract_type_info(module_info::in, tvar::in, kind::in,
    prog_var::in, int_or_var::in, prog_context::in,
    list(hlds_goal)::out, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % A version of gen_extract_type_info that gets its data from the poly_info,
    % and puts the updated date there as well.
    %
:- pred polymorphism_extract_type_info(tvar::in, prog_var::in, int::in,
    prog_context::in, list(hlds_goal)::out, prog_var::out,
    poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

polymorphism_make_type_info_var(Type, Context, Var, ExtraGoals, !Info) :-
    polymorphism_do_make_type_info_var(Type, Context,
        VarMCA, ExtraGoals, !Info),
    VarMCA = Var - _.

polymorphism_make_type_info_vars(Types, Context, Vars, ExtraGoals, !Info) :-
    polymorphism_do_make_type_info_vars(Types, Context,
        VarsMCAs, ExtraGoals, !Info),
    assoc_list.keys(VarsMCAs, Vars).

:- pred polymorphism_do_make_type_info_var(mer_type::in, term.context::in,
    pair(prog_var, maybe(const_struct_arg))::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

polymorphism_do_make_type_info_var(Type, Context, VarMCA, ExtraGoals, !Info) :-
    % First handle statically known types (i.e. types which are not
    % type variables).
    ( if type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs) then
        % This occurs for code where a predicate calls a polymorphic predicate
        % with a type whose type constructor is of variable arity. The
        % transformation we perform is basically the same as in the usual case
        % below, except that we map pred types to pred/0, func types to func/0
        % and tuple types to tuple/0 for the purposes of creating type_infos.
        % To allow univ_to_type to check the type_infos correctly, the actual
        % arity is added to the type_info we create.
        %
        % XXX FIXME (RTTI for higher order impure code)
        % we should not ignore the purity of higher order procs;
        % it should get included in the RTTI.
        polymorphism_make_type_info(Type, TypeCtor, TypeArgs, yes,
            Context, VarMCA, ExtraGoals, !Info)
    else
        (
            ( Type = defined_type(_, _, _)
            ; Type = builtin_type(_)
            ; Type = tuple_type(_, _)
            ; Type = higher_order_type(_,_, _, _, _)
            ; Type = apply_n_type(_, _, _)
            ; Type = kinded_type(_, _)
            ),
            type_to_ctor_and_args_det(Type, TypeCtor, TypeArgs),
            % This occurs for code where a predicate calls a polymorphic
            % predicate with a known value of the type variable. The
            % transformation we perform is shown in the comment at the top
            % of the module.
            polymorphism_make_type_info(Type, TypeCtor, TypeArgs, no,
                Context, VarMCA, ExtraGoals, !Info)
        ;
            % Now handle the cases of types which are not known statically,
            % i.e. type variables.
            Type = type_variable(TypeVar, _),
            get_type_info_locn(TypeVar, TypeInfoLocn, !Info),
            get_type_info_from_locn(TypeVar, TypeInfoLocn, Context,
                Var, ExtraGoals, !Info),
            VarMCA = Var - no
        )
    ).

polymorphism_do_make_type_info_vars([], _, [], [], !Info).
polymorphism_do_make_type_info_vars([Type | Types], Context,
        VarsMCAs, ExtraGoals, !Info) :-
    polymorphism_do_make_type_info_var(Type, Context, HeadVarMCA,
        HeadGoals, !Info),
    polymorphism_do_make_type_info_vars(Types, Context, TailVarsMCAs,
        TailGoals, !Info),
    VarsMCAs = [HeadVarMCA | TailVarsMCAs],
    ExtraGoals = HeadGoals ++ TailGoals.

%---------------------%

polymorphism_make_type_info_var_raw(Type, Context, Var, ExtraGoals,
        !ModuleInfo, !PredInfo, !ProcInfo) :-
    create_poly_info(!.ModuleInfo, !.PredInfo, !.ProcInfo, PolyInfo0),
    polymorphism_make_type_info_var(Type, Context, Var, ExtraGoals,
        PolyInfo0, PolyInfo),
    poly_info_extract(PolyInfo, PolySpecs, !PredInfo, !ProcInfo, !:ModuleInfo),
    expect(unify(PolySpecs, []), $pred, "errors while making type_info var").

polymorphism_make_type_info_vars_raw(Types, Context, Vars, ExtraGoals,
        !ModuleInfo, !PredInfo, !ProcInfo) :-
    create_poly_info(!.ModuleInfo, !.PredInfo, !.ProcInfo, PolyInfo0),
    polymorphism_make_type_info_vars(Types, Context, Vars, ExtraGoals,
        PolyInfo0, PolyInfo),
    poly_info_extract(PolyInfo, PolySpecs, !PredInfo, !ProcInfo, !:ModuleInfo),
    expect(unify(PolySpecs, []), $pred, "errors while making type_info vars").

%---------------------%

:- pred polymorphism_make_type_info(mer_type::in, type_ctor::in,
    list(mer_type)::in, bool::in, prog_context::in,
    pair(prog_var, maybe(const_struct_arg))::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism_make_type_info(Type, TypeCtor, TypeArgs, TypeCtorIsVarArity,
        Context, TypeInfoVarMCA, ExtraGoals, !Info) :-
    poly_info_get_type_info_var_map(!.Info, TypeInfoVarMap0),
    ( if
        map.search(TypeInfoVarMap0, TypeCtor, TypeCtorVarMap0),
        map.search(TypeCtorVarMap0, TypeArgs, OldTypeInfoVarMCA)
    then
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        TypeInfoVarMCA = OldTypeInfoVarMCA,
        ExtraGoals = []
    else
        polymorphism_construct_type_info(Type, TypeCtor, TypeArgs,
            TypeCtorIsVarArity, Context, TypeInfoVar, TypeInfoConstArg,
            ExtraGoals, !Info),
        TypeInfoVarMCA = TypeInfoVar - TypeInfoConstArg,
        % We have to get the type_info_var_map again since the call just above
        % could have added relevant new entries to it.
        poly_info_get_type_info_var_map(!.Info, TypeInfoVarMap1),
        ( if map.search(TypeInfoVarMap1, TypeCtor, TypeCtorVarMap1) then
            map.det_insert(TypeArgs, TypeInfoVarMCA,
                TypeCtorVarMap1, TypeCtorVarMap),
            map.det_update(TypeCtor, TypeCtorVarMap,
                TypeInfoVarMap1, TypeInfoVarMap)
        else
            TypeCtorVarMap = map.singleton(TypeArgs, TypeInfoVarMCA),
            map.det_insert(TypeCtor, TypeCtorVarMap,
                TypeInfoVarMap1, TypeInfoVarMap)
        ),
        poly_info_set_type_info_var_map(TypeInfoVarMap, !Info)
    ).

:- pred polymorphism_construct_type_info(mer_type::in, type_ctor::in,
    list(mer_type)::in, bool::in, prog_context::in,
    prog_var::out, maybe(const_struct_arg)::out,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism_construct_type_info(Type, TypeCtor, TypeArgs, TypeCtorIsVarArity,
        Context, Var, MCA, ExtraGoals, !Info) :-
    get_var_maps_snapshot("polymorphism_construct_type_info",
        InitialVarMapsSnapshot, !Info),

    % Create the typeinfo vars for the arguments.
    polymorphism_do_make_type_info_vars(TypeArgs, Context,
        ArgTypeInfoVarsMCAs, ArgTypeInfoGoals, !Info),

    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),

    TypeCtorConsId = type_ctor_info_cons_id(TypeCtor),
    TypeCtorConsIdConstArg = csa_constant(TypeCtorConsId, type_info_type),
    poly_info_get_const_struct_var_map(!.Info, ConstStructVarMap0),
    ( if
        map.search(ConstStructVarMap0, TypeCtorConsIdConstArg, OldTypeCtorVar)
    then
        poly_info_get_num_reuses(!.Info, NumReuses),
        poly_info_set_num_reuses(NumReuses + 1, !Info),
        TypeCtorVar = OldTypeCtorVar,
        TypeCtorGoals = [],
        VarSet1 = VarSet0,
        VarTypes1 = VarTypes0,
        RttiVarMaps1 = RttiVarMaps0
    else
        init_const_type_ctor_info_var_from_cons_id(Type, TypeCtorConsId,
            TypeCtorVar, TypeCtorGoal, VarSet0, VarSet1, VarTypes0, VarTypes1,
            RttiVarMaps0, RttiVarMaps1),
        TypeCtorGoals = [TypeCtorGoal],
        map.det_insert(TypeCtorConsIdConstArg, TypeCtorVar,
            ConstStructVarMap0, ConstStructVarMap1),
        poly_info_set_const_struct_var_map(ConstStructVarMap1, !Info)
    ),

    poly_info_set_varset_types_rtti(VarSet1, VarTypes1, RttiVarMaps1, !Info),

    % The rest of this predicate create code that constructs the second cell
    % of a type_info for Type if we need a second cell for Type. This cell
    % will usually be of the form:
    %
    %   TypeInfoVar = type_info(TypeCtorVar, ArgTypeInfoVars...)
    %
    % However, if TypeCtorIsVarArity is true and we are not compiling for
    % the Java backend, then it will be of the form
    %
    %   TypeInfoVar = type_info(TypeCtorVar, Arity, ArgTypeInfoVars...)
    %
    % TypeCtorVar should be the variable holding the type_ctor_info for the
    % principal type constructor of Type, and TypeCtorIsVarArity should be
    % true iff the type constructor it represents has a variable arity.
    %
    % ArgTypeInfoVars should be variables holding the type_infos (or
    % type_ctor_infos for zero-arity types) of the argument types of Type.
    %
    % The returned Var will be bound to the type_info cell of Type if such
    % a cell had to be allocated, and to the type_ctor_info of Type's only
    % type constructor if it didn't.
    %
    % NOTE: the special handling for the java backend must be kept
    % consistent with:
    %   rtti_to_mlds.gen_type_info_defn/6
    %   java/runtime/TypeInfo_Struct.java

    % Determine if we need to explicitly pass the arity.
    (
        TypeCtorIsVarArity = no,
        PassArity = no
    ;
        TypeCtorIsVarArity = yes,
        poly_info_get_module_info(!.Info, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        get_target(Globals, Target),
        (
            Target = target_java,
            PassArity = no
        ;
            ( Target = target_c
            ; Target = target_csharp
            ),
            PassArity = yes
        )
    ),

    ( if
        % Unfortunately, if the type's type constructor has variable arity,
        % we cannot use a one-cell representation for that type.
        PassArity = no,
        ArgTypeInfoVarsMCAs = []
    then
        % We do not need a second cell for a separate typeinfo; we will use
        % the type_ctor_info as the type_info.

        % Since this type_ctor_info is pretending to be a type_info,
        % we need to adjust its type. We handle type_ctor_info_const cons_ids
        % specially to make sure that this causes no problems.
        TypeInfoType = type_info_type,
        Var = TypeCtorVar,
        TypeCtorConstArg = csa_constant(TypeCtorConsId, type_info_type),
        MCA = yes(TypeCtorConstArg),
        ExtraGoals = ArgTypeInfoGoals ++ TypeCtorGoals,
        update_var_type(TypeCtorVar, TypeInfoType, VarTypes1, VarTypes),
        poly_info_set_varset_types(VarSet1, VarTypes, !Info)
    else
        % We do need a second cell for a separate typeinfo.
        Cell = type_info_cell(TypeCtor),
        ConsId = cell_cons_id(Cell),

        poly_info_get_const_struct_db(!.Info, ConstStructDb0),
        const_struct_db_get_poly_enabled(ConstStructDb0, Enabled),
        ( if
            Enabled = enable_const_struct_poly,
            all_are_const_struct_args(ArgTypeInfoVarsMCAs,
                ArgTypeInfoConstArgs)
        then
            TypeCtorConstArg = csa_constant(TypeCtorConsId, type_info_type),
            TypeCtorInst = bound(shared, inst_test_results_fgtc,
                [bound_functor(TypeCtorConsId, [])]),
            list.map(get_inst_of_const_struct_arg(ConstStructDb0),
                ArgTypeInfoConstArgs, ArgTypeInfoInsts),
            (
                PassArity = yes,
                list.length(ArgTypeInfoVarsMCAs, ActualArity),
                ArityConsId = some_int_const(int_const(ActualArity)),
                ArityConstArg = csa_constant(ArityConsId, int_type),
                ArityInst = bound(shared, inst_test_results_fgtc,
                    [bound_functor(ArityConsId, [])]),
                StructConstArgs =
                    [TypeCtorConstArg, ArityConstArg | ArgTypeInfoConstArgs],
                StructArgInsts = [TypeCtorInst, ArityInst | ArgTypeInfoInsts]
            ;
                PassArity = no,
                StructConstArgs = [TypeCtorConstArg | ArgTypeInfoConstArgs],
                StructArgInsts = [TypeCtorInst | ArgTypeInfoInsts]
            ),
            StructType = type_info_type,
            list.length(StructConstArgs, NumArgs),
            InstConsId = cell_inst_cons_id(Cell, NumArgs),
            StructInst = bound(shared, inst_test_results_fgtc,
                [bound_functor(InstConsId, StructArgInsts)]),

            poly_info_get_defined_where(!.Info, DefinedWhere),
            ConstStruct = const_struct(ConsId, StructConstArgs,
                StructType, StructInst, DefinedWhere),
            lookup_insert_const_struct(ConstStruct, ConstNum,
                ConstStructDb0, ConstStructDb),
            MCA = yes(csa_const_struct(ConstNum)),
            poly_info_set_const_struct_db(ConstStructDb, !Info),

            set_var_maps_snapshot("maybe_init_second_cell",
                InitialVarMapsSnapshot, !Info),

            new_type_info_var(Type, type_info, Var, !Info),
            Unification = construct(Var, type_info_const(ConstNum),
                [], [], construct_statically(born_static), cell_is_shared,
                no_construct_sub_info),
            Ground = ground(shared, none_or_default_func),
            UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
            % XXX The UnifyContext is wrong.
            UnifyContext = unify_context(umc_explicit, []),
            TypeInfoRHS = rhs_functor(type_info_const(ConstNum),
                is_not_exist_constr, []),
            Unify = unify(Var, TypeInfoRHS, UnifyMode, Unification,
                UnifyContext),

            % Create a goal_info for the unification.
            NonLocals = set_of_var.make_singleton(Var),
            % Note that we could be more accurate than `ground(shared)',
            % but it shouldn't make any difference.
            VarInst = bound(shared, inst_test_results_fgtc,
                [bound_functor(InstConsId, [])]),
            InstMapDelta = instmap_delta_from_assoc_list([Var - VarInst]),
            goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
                GoalInfo),
            TypeInfoGoal = hlds_goal(Unify, GoalInfo),
            ExtraGoals = [TypeInfoGoal]
        else
            assoc_list.keys(ArgTypeInfoVarsMCAs, ArgTypeInfoVars),
            (
                PassArity = yes,
                list.length(ArgTypeInfoVars, ActualArity),
                get_poly_const(ActualArity, ArityVar, ArityGoals, !Info),
                % The call get_poly_const may (and probably will) allocate
                % a variable, so VarSet1, VarTypes1 and RttiVarMaps1 are
                % all out of date.
                poly_info_get_varset(!.Info, VarSet2),
                poly_info_get_var_types(!.Info, VarTypes2),
                poly_info_get_rtti_varmaps(!.Info, RttiVarMaps2),
                init_type_info_var(Type,
                    [TypeCtorVar, ArityVar | ArgTypeInfoVars],
                    no, Var, TypeInfoGoal,
                    VarSet2, VarSet, VarTypes2, VarTypes,
                    RttiVarMaps2, RttiVarMaps),
                ExtraGoals = TypeCtorGoals ++ ArityGoals ++ ArgTypeInfoGoals
                    ++ [TypeInfoGoal]
            ;
                PassArity = no,
                init_type_info_var(Type, [TypeCtorVar | ArgTypeInfoVars],
                    no, Var, TypeInfoGoal,
                    VarSet1, VarSet, VarTypes1, VarTypes,
                    RttiVarMaps1, RttiVarMaps),
                ExtraGoals = TypeCtorGoals ++ ArgTypeInfoGoals ++
                    [TypeInfoGoal]
            ),
            poly_info_set_varset_types_rtti(VarSet, VarTypes, RttiVarMaps,
                !Info),
            MCA = no
        )
    ).

%---------------------%

init_type_info_var(Type, ArgVars, MaybePreferredVar, TypeInfoVar, TypeInfoGoal,
        !VarSet, !VarTypes, !RttiVarMaps) :-
    type_to_ctor_det(Type, TypeCtor),
    Cell = type_info_cell(TypeCtor),
    ConsId = cell_cons_id(Cell),
    do_init_type_info_var(Type, Cell, ConsId, ArgVars, MaybePreferredVar,
        TypeInfoVar, TypeInfoGoal, !VarSet, !VarTypes, !RttiVarMaps).

:- pred do_init_type_info_var(mer_type::in, polymorphism_cell::in, cons_id::in,
    list(prog_var)::in, maybe(prog_var)::in, prog_var::out, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

do_init_type_info_var(Type, Cell, ConsId, ArgVars, MaybePreferredVar,
        TypeInfoVar, TypeInfoGoal, !VarSet, !VarTypes, !RttiVarMaps) :-
    TypeInfoRHS = rhs_functor(ConsId, is_not_exist_constr, ArgVars),
    % Introduce a new variable.
    (
        MaybePreferredVar = yes(TypeInfoVar)
    ;
        MaybePreferredVar = no,
        new_type_info_var_raw(Type, type_info, TypeInfoVar,
            !VarSet, !VarTypes, !RttiVarMaps)
    ),

    % Create the construction unification to initialize the variable.
    Ground = ground(shared, none_or_default_func),
    ArgMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
    list.length(ArgVars, NumArgVars),
    list.duplicate(NumArgVars, ArgMode, ArgModes),
    Unification = construct(TypeInfoVar, ConsId, ArgVars, ArgModes,
        construct_dynamically, cell_is_unique, no_construct_sub_info),
    UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
    % XXX The UnifyContext is wrong.
    UnifyContext = unify_context(umc_explicit, []),
    Unify = unify(TypeInfoVar, TypeInfoRHS, UnifyMode, Unification,
        UnifyContext),

    % Create a goal_info for the unification.
    set_of_var.list_to_set([TypeInfoVar | ArgVars], NonLocals),
    list.duplicate(NumArgVars, Ground, ArgInsts),
    % Note that we could perhaps be more accurate than `ground(shared)',
    % but it shouldn't make any difference.
    InstConsId = cell_inst_cons_id(Cell, NumArgVars),
    InstResults = inst_test_results(inst_result_is_ground,
        inst_result_does_not_contain_any,
        inst_result_contains_inst_names_known(set.init),
        inst_result_contains_inst_vars_unknown,
        inst_result_contains_types_unknown,
        inst_result_no_type_ctor_propagated),
    TypeInfoVarInst = bound(unique, InstResults,
        [bound_functor(InstConsId, ArgInsts)]),
    InstMapDelta = instmap_delta_from_assoc_list(
        [TypeInfoVar - TypeInfoVarInst]),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, GoalInfo),
    TypeInfoGoal = hlds_goal(Unify, GoalInfo).

init_const_type_ctor_info_var(Type, TypeCtor, TypeCtorInfoVar,
        ConsId, TypeCtorInfoGoal, !VarSet, !VarTypes, !RttiVarMaps) :-
    ConsId = type_ctor_info_cons_id(TypeCtor),
    init_const_type_ctor_info_var_from_cons_id(Type, ConsId,
        TypeCtorInfoVar, TypeCtorInfoGoal, !VarSet, !VarTypes, !RttiVarMaps).

:- pred init_const_type_ctor_info_var_from_cons_id(mer_type::in, cons_id::in,
    prog_var::out, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

init_const_type_ctor_info_var_from_cons_id(Type, ConsId,
        TypeCtorInfoVar, TypeCtorInfoGoal, !VarSet, !VarTypes, !RttiVarMaps) :-
    % Introduce a new variable.
    new_type_info_var_raw(Type, type_ctor_info, TypeCtorInfoVar,
        !VarSet, !VarTypes, !RttiVarMaps),

    % Create the construction unification to initialize the variable.
    TypeInfoRHS = rhs_functor(ConsId, is_not_exist_constr, []),
    Unification = construct(TypeCtorInfoVar, ConsId, [], [],
        construct_dynamically, cell_is_shared, no_construct_sub_info),
    Ground = ground(shared, none_or_default_func),
    UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
    % XXX The UnifyContext is wrong.
    UnifyContext = unify_context(umc_explicit, []),
    Unify = unify(TypeCtorInfoVar, TypeInfoRHS, UnifyMode,
        Unification, UnifyContext),

    % Create a goal_info for the unification.
    NonLocals = set_of_var.make_singleton(TypeCtorInfoVar),
    InstmapDelta = instmap_delta_bind_var(TypeCtorInfoVar),
    goal_info_init(NonLocals, InstmapDelta, detism_det, purity_pure, GoalInfo),
    TypeCtorInfoGoal = hlds_goal(Unify, GoalInfo).

%---------------------%

new_type_info_var(Type, Kind, Var, !Info) :-
    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    new_type_info_var_raw(Type, Kind, Var, VarSet0, VarSet,
        VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
    poly_info_set_varset_types_rtti(VarSet, VarTypes, RttiVarMaps, !Info).

new_type_info_var_raw(Type, Kind, Var, !VarSet, !VarTypes, !RttiVarMaps) :-
    % Introduce new variable.
    varset.new_var(Var, !VarSet),
    term.var_to_int(Var, VarNum),
    string.int_to_string(VarNum, VarNumStr),
    (
        Kind = type_info,
        Prefix = "TypeInfo_",
        rtti_det_insert_type_info_type(Var, Type, !RttiVarMaps)
    ;
        Kind = type_ctor_info,
        Prefix = "TypeCtorInfo_"

        % XXX Perhaps we should record the variables holding
        % type_ctor_infos in the rtti_varmaps somewhere.
    ),
    Name = Prefix ++ VarNumStr,
    varset.name_var(Var, Name, !VarSet),
    add_var_type(Var, type_info_type, !VarTypes).

%---------------------%

get_type_info_locn(TypeVar, TypeInfoLocn, !Info) :-
    % If we have already allocated a location for this type_info, then all
    % we need to do is to extract the type_info variable from its location.
    ( if
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_search_type_info_locn(RttiVarMaps0, TypeVar, TypeInfoLocnPrime)
    then
        TypeInfoLocn = TypeInfoLocnPrime
    else
        % Otherwise, we need to create a new type_info variable, and set the
        % location for this type variable to be that type_info variable.
        %
        % This is wrong if the type variable is one of the existentially
        % quantified variables of a called predicate and the variable occurs
        % in an existential typeclass constraint. In that case the type_info
        % will be stored in the typeclass_info variable produced by the
        % predicate, not in a type_info variable. maybe_extract_type_info
        % will fix this up when the typeclass_info is created.

        poly_info_get_tvar_kind_map(!.Info, TVarKindMap),
        get_tvar_kind(TVarKindMap, TypeVar, Kind),
        Type = type_variable(TypeVar, Kind),
        new_type_info_var(Type, type_info, Var, !Info),
        TypeInfoLocn = type_info(Var),
        % Since the call to new_type_info_var above may update the rtti
        % varmaps, we have to get them again here; we can't use RttiVarMaps0.
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps1),
        rtti_det_insert_type_info_locn(TypeVar, TypeInfoLocn,
            RttiVarMaps1, RttiVarMaps),
        poly_info_set_rtti_varmaps(RttiVarMaps, !Info)
    ).

    % Generate code to get the value of a type variable.
    %
:- pred get_type_info_from_locn(tvar::in, type_info_locn::in, prog_context::in,
    prog_var::out, list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

get_type_info_from_locn(TypeVar, TypeInfoLocn, Context, Var, ExtraGoals,
        !Info) :-
    (
        % If the typeinfo is available in a variable, just use it.
        TypeInfoLocn = type_info(TypeInfoVar),
        Var = TypeInfoVar,
        ExtraGoals = []
    ;
        % If the typeinfo is in a typeclass_info, then we need to extract it
        % before using it.
        TypeInfoLocn = typeclass_info(TypeClassInfoVar, Index),
        polymorphism_extract_type_info(TypeVar, TypeClassInfoVar, Index,
            Context, ExtraGoals, Var, !Info)
    ).

gen_extract_type_info(ModuleInfo, TypeVar, Kind, TypeClassInfoVar,
        IndexIntOrVar, Context, Goals, TypeInfoVar,
        !VarSet, !VarTypes, !RttiVarMaps) :-
    (
        IndexIntOrVar = iov_int(Index),
        % We cannot call get_poly_const since we don't have a poly_info.
        make_int_const_construction_alloc(Index, yes("TypeInfoIndex"),
            IndexGoal, IndexVar, !VarSet, !VarTypes),
        IndexGoals = [IndexGoal]
    ;
        IndexIntOrVar = iov_var(IndexVar),
        IndexGoals = []
    ),
    Type = type_variable(TypeVar, Kind),
    new_type_info_var_raw(Type, type_info, TypeInfoVar,
        !VarSet, !VarTypes, !RttiVarMaps),
    generate_plain_call(ModuleInfo, pf_predicate,
        mercury_private_builtin_module, "type_info_from_typeclass_info",
        [], [TypeClassInfoVar, IndexVar, TypeInfoVar],
        instmap_delta_bind_var(TypeInfoVar), only_mode,
        detism_det, purity_pure, [], Context, CallGoal),
    Goals = IndexGoals ++ [CallGoal].

polymorphism_extract_type_info(TypeVar, TypeClassInfoVar, Index, Context,
        Goals, TypeInfoVar, !Info) :-
    get_poly_const(Index, IndexVar, IndexGoals, !Info),
    poly_info_get_varset(!.Info, VarSet0),
    poly_info_get_var_types(!.Info, VarTypes0),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    poly_info_get_module_info(!.Info, ModuleInfo),
    poly_info_get_tvar_kind_map(!.Info, TVarKindMap),
    get_tvar_kind(TVarKindMap, TypeVar, Kind),
    IndexIntOrVar = iov_var(IndexVar),
    gen_extract_type_info(ModuleInfo, TypeVar, Kind, TypeClassInfoVar,
        IndexIntOrVar, Context, ExtractGoals, TypeInfoVar,
        VarSet0, VarSet, VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
    Goals = IndexGoals ++ ExtractGoals,
    poly_info_set_varset_types_rtti(VarSet, VarTypes, RttiVarMaps, !Info).

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism_type_info.
%---------------------------------------------------------------------------%
