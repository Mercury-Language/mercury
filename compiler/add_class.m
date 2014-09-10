%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_class.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.qual_info.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred module_add_class_defn(item_typeclass_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_add_instance_defn(module_name::in, list(prog_constraint)::in,
    sym_name::in, list(mer_type)::in, list(mer_type)::in, instance_body::in,
    tvarset::in, import_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Given the definition for a predicate or function from a
    % type class instance declaration, produce the clauses_info
    % for that definition.
    %
:- pred do_produce_instance_method_clauses(instance_proc_def::in,
    pred_or_func::in, arity::in, list(mer_type)::in,
    pred_markers::in, term.context::in, import_status::in, clauses_info::out,
    tvarset::in, tvarset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, list(error_spec)::in, list(error_spec)::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.add_type.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.pred_table.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module varset.

module_add_class_defn(ItemTypeClassInfo, Status, !ModuleInfo, !Specs) :-
    ItemTypeClassInfo = item_typeclass_info(Constraints, FunDeps, Name, Vars,
        Interface, VarSet, Context, _SeqNum),
    module_info_get_class_table(!.ModuleInfo, Classes0),
    list.length(Vars, ClassArity),
    ClassId = class_id(Name, ClassArity),
    Status = item_status(ImportStatus0, _),
    (
        Interface = class_interface_abstract,
        make_status_abstract(ImportStatus0, ImportStatus1)
    ;
        Interface = class_interface_concrete(_),
        ImportStatus1 = ImportStatus0
    ),
    HLDSFunDeps = list.map(make_hlds_fundep(Vars), FunDeps),
    (
        % The typeclass is exported if *any* occurrence is exported,
        % even a previous abstract occurrence.
        map.search(Classes0, ClassId, OldDefn)
    ->
        OldDefn = hlds_class_defn(OldStatus, OldConstraints, OldFunDeps,
            _OldAncestors, OldVars, _OldKinds, OldInterface, OldMethods,
            OldVarSet, OldContext),
        combine_status(ImportStatus1, OldStatus, ImportStatus),
        (
            OldInterface = class_interface_concrete(_),
            ClassMethods0 = OldMethods,
            ClassInterface = OldInterface
        ;
            OldInterface = class_interface_abstract,
            ClassMethods0 = [],
            ClassInterface = Interface
        ),
        (
            % Check that the superclass constraints are identical
            \+ constraints_are_identical(OldVars, OldVarSet,
                OldConstraints, Vars, VarSet, Constraints)
        ->
            % Always report the error, even in `.opt' files.
            DummyStatus = status_local,
            Extras = [words("The superclass constraints do not match."), nl],
            multiple_def_error(DummyStatus, Name, ClassArity, "typeclass",
                Context, OldContext, Extras, !Specs),
            ErrorOrPrevDef = yes
        ;
            \+ class_fundeps_are_identical(OldFunDeps, HLDSFunDeps)
        ->
            % Always report the error, even in `.opt' files.
            DummyStatus = status_local,
            Extras = [words("The functional dependencies do not match."), nl],
            multiple_def_error(DummyStatus, Name, ClassArity, "typeclass",
                Context, OldContext, Extras, !Specs),
            ErrorOrPrevDef = yes
        ;
            Interface = class_interface_concrete(_),
            OldInterface = class_interface_concrete(_)
        ->
            multiple_def_error(ImportStatus, Name, ClassArity,
                "typeclass", Context, OldContext, [], !Specs),
            ErrorOrPrevDef = yes
        ;
            ErrorOrPrevDef = no
        ),

        IsNewDefn = no
    ;
        IsNewDefn = yes `with_type` bool,
        ErrorOrPrevDef = no `with_type` bool,
        ClassMethods0 = [],
        ClassInterface = Interface,
        ImportStatus = ImportStatus1
    ),
    (
        ErrorOrPrevDef = no,
        (
            Interface = class_interface_concrete(Methods),
            module_add_class_interface(Name, Vars, Methods,
                Status, PredProcIds0, !ModuleInfo, !Specs),
            % Get rid of the `no's from the list of maybes
            IsYes = (pred(Maybe::in, PredProcId::out) is semidet :-
                Maybe = yes(Pred - Proc),
                PredProcId = hlds_class_proc(Pred, Proc)
            ),
            list.filter_map(IsYes, PredProcIds0, PredProcIds1),

            % The list must be sorted on pred_id and then proc_id --
            % check_typeclass.m assumes this when it is generating the
            % corresponding list of pred_proc_ids for instance definitions.
            list.sort(PredProcIds1, ClassMethods)
        ;
            Interface = class_interface_abstract,
            ClassMethods = ClassMethods0
        ),

        % Ancestors is not set until check_typeclass.
        Ancestors = [],
        % XXX kind inference:
        % We set all the kinds to `star' at the moment.  This should be
        % done differently when we have a proper kind system.
        Kinds = map.init,
        Defn = hlds_class_defn(ImportStatus, Constraints, HLDSFunDeps,
            Ancestors, Vars, Kinds, ClassInterface, ClassMethods, VarSet,
            Context),
        map.set(ClassId, Defn, Classes0, Classes),
        module_info_set_class_table(Classes, !ModuleInfo),

        (
            IsNewDefn = yes,
            % When we find the class declaration, make an entry
            % for the instances.
            module_info_get_instance_table(!.ModuleInfo, Instances0),
            map.det_insert(ClassId, [], Instances0, Instances),
            module_info_set_instance_table(Instances, !ModuleInfo)
        ;
            IsNewDefn = no
        )
    ;
        ErrorOrPrevDef = yes
    ).

:- func make_hlds_fundep(list(tvar), prog_fundep) = hlds_class_fundep.

make_hlds_fundep(TVars, fundep(Domain0, Range0)) = fundep(Domain, Range) :-
    Domain = make_hlds_fundep_2(TVars, Domain0),
    Range = make_hlds_fundep_2(TVars, Range0).

:- func make_hlds_fundep_2(list(tvar), list(tvar)) = set(hlds_class_argpos).

make_hlds_fundep_2(TVars, List) = list.foldl(Func, List, set.init) :-
    Func = (func(TVar, Set0) = set.insert(Set0, N) :-
        N = get_list_index(TVars, 1, TVar)
    ).

:- func get_list_index(list(T), hlds_class_argpos, T) = hlds_class_argpos.

get_list_index([], _, _) = _ :-
    unexpected($module, $pred, "element not found").
get_list_index([E | Es], N, X) =
    ( X = E ->
        N
    ;
        get_list_index(Es, N + 1, X)
    ).

:- pred constraints_are_identical(list(tvar)::in, tvarset::in,
    list(prog_constraint)::in, list(tvar)::in, tvarset::in,
    list(prog_constraint)::in) is semidet.

constraints_are_identical(OldVars0, OldVarSet, OldConstraints0,
        Vars, VarSet, Constraints) :-
    tvarset_merge_renaming(VarSet, OldVarSet, _, Renaming),
    apply_variable_renaming_to_prog_constraint_list(Renaming, OldConstraints0,
        OldConstraints1),
    apply_variable_renaming_to_tvar_list(Renaming, OldVars0,  OldVars),

    map.from_corresponding_lists(OldVars, Vars, VarRenaming),
    apply_variable_renaming_to_prog_constraint_list(VarRenaming,
        OldConstraints1, OldConstraints),
    OldConstraints = Constraints.

:- pred class_fundeps_are_identical(hlds_class_fundeps::in,
    hlds_class_fundeps::in) is semidet.

class_fundeps_are_identical(OldFunDeps0, FunDeps0) :-
    % Allow for the functional dependencies to be in a different order.
    % we rely on the fact that sets (ordered lists) have a canonical
    % representation.
    sort_and_remove_dups(OldFunDeps0, OldFunDeps),
    sort_and_remove_dups(FunDeps0, FunDeps),
    OldFunDeps = FunDeps.

:- pred module_add_class_interface(sym_name::in, list(tvar)::in,
    class_methods::in, item_status::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_class_interface(Name, Vars, Methods, Status, PredProcIds,
        !ModuleInfo, !Specs) :-
    list.filter(is_class_method_mode_item, Methods, ModeMethods,
        PredOrFuncMethods),
    some [!PPIds] (
        add_class_pred_or_func_methods(Name, Vars, PredOrFuncMethods, Status,
            !:PPIds, !ModuleInfo, !Specs),

        % Add the pred_or_func_mode decls.  Since we have already added the
        % predicate/function method decls there should already be an entry in
        % the predicate table corresponding to the mode item we are about to
        % add.  If not, report an error.
        list.foldl3(add_class_pred_or_func_mode_method(Name, Vars, Status),
            ModeMethods, !PPIds, !ModuleInfo, !Specs),
        check_method_modes(Methods, !.PPIds, PredProcIds, !ModuleInfo, !Specs)
    ).

:- pred is_class_method_mode_item(class_method::in) is semidet.

is_class_method_mode_item(Method) :-
    Method = method_pred_or_func_mode(_, _, _, _, _, _, _, _).

:- pred add_class_pred_or_func_mode_method(sym_name::in,
    list(tvar)::in, item_status::in, class_method::in,
    list(maybe(pair(pred_id, proc_id)))::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_pred_or_func_mode_method(Name, Vars, Status, Method,
        !PredProcIds, !ModuleInfo, !Specs) :-
    (
        Method = method_pred_or_func(_, _, _, _, _, _, _, _, _, _, _, _, _),
        unexpected($module, $pred, "pred_or_func method item")
    ;
        Method = method_pred_or_func_mode(_VarSet, MaybePredOrFunc, PredName,
            Modes, _WithInst, _MaybeDet, _Cond, Context)
    ),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    PredArity = list.length(Modes) : int,
    (
        MaybePredOrFunc = no,
        % The only way this could have happened now is if a `with_inst`
        % annotation was not expanded.
        unexpected($module, $pred, "unexpanded `with_inst` annotation")
    ;
        MaybePredOrFunc = yes(PredOrFunc)
    ),
    predicate_table_lookup_pf_sym_arity(PredTable, is_fully_qualified,
        PredOrFunc, PredName, PredArity, PredIds),
    (
        PredIds = [],
        missing_pred_or_func_method_error(PredName, PredArity, PredOrFunc,
            Context, !Specs)
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            PredId = HeadPredId,
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_markers(PredInfo, PredMarkers),
            ( check_marker(PredMarkers, marker_class_method) ->
                module_add_class_method(Method, Name, Vars, Status,
                    PredProcId, !ModuleInfo, !Specs),
                list.cons(PredProcId, !PredProcIds)
            ;
                % XXX It may also be worth reporting that although there
                % wasn't a matching class method, there was a matching
                % predicate/function.
                missing_pred_or_func_method_error(PredName, PredArity,
                    PredOrFunc, Context, !Specs)
            )
        ;
            TailPredIds = [_ | _],
            % This shouldn't happen.
            unexpected($module, $pred, "multiple preds matching method mode")
        )
    ).

:- pred add_class_pred_or_func_methods(sym_name::in, list(tvar)::in,
    class_methods::in, item_status::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_class_pred_or_func_methods(_, _, [], _, [], !ModuleInfo, !Specs).
add_class_pred_or_func_methods(Name, Vars, [M | Ms], Status, [P | Ps],
        !ModuleInfo, !Specs) :-
    module_add_class_method(M, Name, Vars, Status, P, !ModuleInfo, !Specs),
    add_class_pred_or_func_methods(Name, Vars, Ms, Status, Ps, !ModuleInfo,
        !Specs).

:- pred module_add_class_method(class_method::in, sym_name::in, list(tvar)::in,
    item_status::in, maybe(pair(pred_id, proc_id))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_class_method(Method, Name, Vars, Status, MaybePredIdProcId,
        !ModuleInfo, !Specs) :-
    (
        Method = method_pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
            PredOrFunc, PredName, TypesAndModes, _WithType, _WithInst,
            MaybeDet, _Cond, Purity, ClassContext, Context),
        % XXX kind inference:
        % We set the kinds to `star' at the moment.  This will be different
        % when we have a kind system.
        prog_type.var_list_to_type_list(map.init, Vars, Args),
        ClassContext = constraints(UnivCnstrs, ExistCnstrs),
        NewUnivCnstrs = [constraint(Name, Args) | UnivCnstrs],
        NewClassContext = constraints(NewUnivCnstrs, ExistCnstrs),
        init_markers(Markers0),
        add_marker(marker_class_method, Markers0, Markers),
        module_add_pred_or_func(TypeVarSet, InstVarSet, ExistQVars, PredOrFunc,
            PredName, TypesAndModes, MaybeDet, Purity, NewClassContext,
            Markers, Context, Status, MaybePredIdProcId, !ModuleInfo, !Specs)
    ;
        Method = method_pred_or_func_mode(VarSet, MaybePredOrFunc, PredName,
            Modes, _WithInst, MaybeDet, _Cond, Context),
        (
            MaybePredOrFunc = yes(PredOrFunc),
            Status = item_status(ImportStatus, _),
            IsClassMethod = yes,
            module_add_mode(VarSet, PredName, Modes, MaybeDet, ImportStatus,
                Context, PredOrFunc, IsClassMethod, PredIdProcId, !ModuleInfo,
                !Specs),
            MaybePredIdProcId = yes(PredIdProcId)
        ;
            MaybePredOrFunc = no,
            % equiv_type.m should have either set the
            % pred_or_func or removed the item from the list.
            unexpected($module, $pred, "no pred_or_func on mode declaration")
        )
    ).

    % Go through the list of class methods, looking for
    % - functions without mode declarations: add a default mode
    % - predicates without mode declarations: report an error
    % - mode declarations with no determinism: report an error
    %
:- pred check_method_modes(class_methods::in,
    list(maybe(pair(pred_id, proc_id)))::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_method_modes([], !PredProcIds, !ModuleInfo, !Specs).
check_method_modes([Method | Methods], !PredProcIds, !ModuleInfo, !Specs) :-
    (
        Method = method_pred_or_func(_, _, _, PorF, QualName, TypesAndModes,
            _WithType, _WithInst, _, _, _, _, _),
        (
            QualName = qualified(ModuleName0, Name0),
            ModuleName = ModuleName0,
            Name = Name0
        ;
            QualName = unqualified(_),
            % The class interface should be fully module qualified
            % by prog_io.m at the time it is read in.
            unexpected($module, $pred, "unqualified func")
        ),
        list.length(TypesAndModes, PredArity),
        module_info_get_predicate_table(!.ModuleInfo, PredTable),
        predicate_table_lookup_pf_m_n_a(PredTable, is_fully_qualified,
            PorF, ModuleName, Name, PredArity, PredIds),
        ( PredIds = [PredId] ->
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
            (
                PorF = pf_function,
                maybe_add_default_func_mode(PredInfo0, PredInfo, MaybeProc),
                (
                    MaybeProc = no
                ;
                    MaybeProc = yes(ProcId),
                    NewPredProc = yes(PredId - ProcId),
                    !:PredProcIds = [NewPredProc | !.PredProcIds],
                    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
                )
            ;
                PorF = pf_predicate,
                pred_info_get_procedures(PredInfo0, Procs),
                ( map.is_empty(Procs) ->
                    pred_method_with_no_modes_error(PredInfo0, !Specs)
                ;
                    true
                )
            )
        ;
            unexpected($module, $pred, "number of preds != 1")
        )
    ;
        Method = method_pred_or_func_mode(_, _, _, _, _, _, _, _)
    ),
    check_method_modes(Methods, !PredProcIds, !ModuleInfo, !Specs).

module_add_instance_defn(InstanceModuleName, Constraints, ClassName,
        Types, OriginalTypes, Body0, VarSet, Status, Context,
        !ModuleInfo, !Specs) :-
    module_info_get_class_table(!.ModuleInfo, Classes),
    module_info_get_instance_table(!.ModuleInfo, Instances0),
    list.length(Types, ClassArity),
    ClassId = class_id(ClassName, ClassArity),
    expand_bang_states_instance_body(Body0, Body),
    ( map.search(Classes, ClassId, _) ->
        map.init(Empty),
        NewInstanceDefn = hlds_instance_defn(InstanceModuleName, Status,
            Context, Constraints, Types, OriginalTypes, Body, no,
            VarSet, Empty),
        map.lookup(Instances0, ClassId, InstanceDefns),

        check_for_overlapping_instances(NewInstanceDefn, InstanceDefns,
            ClassId, !Specs),
        check_instance_compatibility(NewInstanceDefn, InstanceDefns,
            ClassId, !Specs),

        map.det_update(ClassId, [NewInstanceDefn | InstanceDefns],
            Instances0, Instances),
        module_info_set_instance_table(Instances, !ModuleInfo)
    ;
        undefined_type_class_error(ClassName, ClassArity, Context,
            "instance declaration", !Specs)
    ).

:- pred check_for_overlapping_instances(hlds_instance_defn::in,
    list(hlds_instance_defn)::in, class_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_overlapping_instances(NewInstanceDefn, InstanceDefns, ClassId,
        !Specs) :-
    IsOverlapping = (pred((Context - OtherContext)::out) is nondet :-
        NewInstanceDefn = hlds_instance_defn(_, _Status, Context,
            _, Types, _, Body, _, VarSet, _),
        Body = instance_body_concrete(_), % XXX
        list.member(OtherInstanceDefn, InstanceDefns),
        OtherInstanceDefn = hlds_instance_defn(_, _OtherStatus,
            OtherContext, _, OtherTypes, _, OtherBody, _, OtherVarSet, _),
        OtherBody = instance_body_concrete(_), % XXX
        tvarset_merge_renaming(VarSet, OtherVarSet, _NewVarSet, Renaming),
        apply_variable_renaming_to_type_list(Renaming, OtherTypes,
            NewOtherTypes),
        type_list_subsumes(Types, NewOtherTypes, _)
    ),
    solutions.aggregate(IsOverlapping,
        report_overlapping_instance_declaration(ClassId), !Specs).

:- pred report_overlapping_instance_declaration(class_id::in,
    pair(prog_context)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_overlapping_instance_declaration(class_id(ClassName, ClassArity),
        Context - OtherContext, !Specs) :-
    Pieces1 = [words("Error: multiply defined (or overlapping)"),
        words("instance declarations for class"),
        sym_name_and_arity(ClassName / ClassArity),
        suffix("."), nl],
    Pieces2 = [words("Previous instance declaration was here.")],
    Msg1 = simple_msg(Context, [always(Pieces1)]),
    Msg2 = error_msg(yes(OtherContext), treat_as_first, 0, [always(Pieces2)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg1, Msg2]),
    !:Specs = [Spec | !.Specs].


    %
    % If two instance declarations are about the same type, then
    % the declarations must be compatible.  This consists of checking
    % that the constraints are identical.
    % In other words, the abstract declaration must match the
    % concrete definition.
    %
:- pred check_instance_compatibility(hlds_instance_defn::in,
    list(hlds_instance_defn)::in, class_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_compatibility(InstanceDefn, InstanceDefns, ClassId, !Specs) :-
    list.filter(same_type_hlds_instance_defn(InstanceDefn),
        InstanceDefns, EquivInstanceDefns),
    list.foldl(check_instance_constraints(InstanceDefn, ClassId),
        EquivInstanceDefns, !Specs).

:- pred check_instance_constraints(hlds_instance_defn::in,
    class_id::in, hlds_instance_defn::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_constraints(InstanceDefnA, ClassId, InstanceDefnB, !Specs) :-
    type_vars_list(InstanceDefnA ^ instance_types, TVarsA),
    TVarsetA = InstanceDefnA ^ instance_tvarset,
    ConstraintsA = InstanceDefnA ^ instance_constraints,

    type_vars_list(InstanceDefnB ^ instance_types, TVarsB),
    TVarsetB = InstanceDefnB ^ instance_tvarset,
    ConstraintsB = InstanceDefnB ^ instance_constraints,

    (
        constraints_are_identical(TVarsA, TVarsetA, ConstraintsA,
            TVarsB, TVarsetB, ConstraintsB)
    ->
        true
    ;
        ClassId = class_id(Name, ClassArity),
        ContextA = InstanceDefnA ^ instance_context,

        TxtA = [words("In instance declaration for class "),
            sym_name_and_arity(Name / ClassArity), nl,
            words("instance constraints are incompatible with")],
        MsgA = simple_msg(ContextA, [always(TxtA)]),

        ContextB = InstanceDefnB ^ instance_context,
        TxtB = [words("instance constraints here.")],
        MsgB = simple_msg(ContextB, [always(TxtB)]),

        Spec = error_spec(severity_error,
            phase_parse_tree_to_hlds, [MsgA, MsgB]),
        !:Specs = [Spec | !.Specs]
    ).

    % Do two hlds_instance_defn refer to the same type?
    % eg "instance tc(f(T))" compares equal to "instance tc(f(U))"
    %
    % Note we don't check that the constraints of the declarations are the
    % same.
    %
:- pred same_type_hlds_instance_defn(hlds_instance_defn::in,
    hlds_instance_defn::in) is semidet.

same_type_hlds_instance_defn(InstanceDefnA, InstanceDefnB) :-
    TypesA = InstanceDefnA ^ instance_types,
    TypesB0 = InstanceDefnB ^ instance_types,

    VarSetA = InstanceDefnA ^ instance_tvarset,
    VarSetB = InstanceDefnB ^ instance_tvarset,

    % Rename the two lists of types apart.
    tvarset_merge_renaming(VarSetA, VarSetB, _NewVarSet, RenameApart),
    apply_variable_renaming_to_type_list(RenameApart, TypesB0, TypesB1),

    type_vars_list(TypesA, TVarsA),
    type_vars_list(TypesB1, TVarsB),

    % If the lengths are different they can't be the same type.
    list.length(TVarsA, L),
    list.length(TVarsB, L),

    map.from_corresponding_lists(TVarsB, TVarsA, Renaming),
    apply_variable_renaming_to_type_list(Renaming, TypesB1, TypesB),

    TypesA = TypesB.

do_produce_instance_method_clauses(InstanceProcDefn, PredOrFunc, PredArity,
        ArgTypes, Markers, Context, Status, ClausesInfo, !TVarSet, !ModuleInfo,
        !QualInfo, !Specs) :-
    (
        % Handle the `pred(<MethodName>/<Arity>) is <ImplName>' syntax.
        InstanceProcDefn = instance_proc_def_name(InstancePredName),
        % Add the body of the introduced pred.
        % First the goal info, ...
        goal_info_init(GoalInfo0),
        goal_info_set_context(Context, GoalInfo0, GoalInfo1),
        set_of_var.list_to_set(HeadVars, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo1, GoalInfo2),
        ( check_marker(Markers, marker_is_impure) ->
            goal_info_set_purity(purity_impure, GoalInfo2, GoalInfo)
        ; check_marker(Markers, marker_is_semipure) ->
            goal_info_set_purity(purity_semipure, GoalInfo2, GoalInfo)
        ;
            GoalInfo = GoalInfo2
        ),
        % ... and then the goal itself.
        varset.init(VarSet0),
        make_n_fresh_vars("HeadVar__", PredArity, HeadVars, VarSet0, VarSet),
        construct_pred_or_func_call(invalid_pred_id, PredOrFunc,
            InstancePredName, HeadVars, GoalInfo, IntroducedGoal, !QualInfo),
        IntroducedClause = clause(all_modes, IntroducedGoal, impl_lang_mercury,
            Context, []),

        map.init(TVarNameMap),
        vartypes_from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
        HeadVarVec = proc_arg_vector_init(PredOrFunc, HeadVars),
        set_clause_list([IntroducedClause], ClausesRep),
        rtti_varmaps_init(RttiVarMaps),
        HasForeignClauses = no,
        ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
            HeadVarVec, ClausesRep, init_clause_item_numbers_comp_gen,
            RttiVarMaps, HasForeignClauses)
    ;
        % Handle the arbitrary clauses syntax.
        InstanceProcDefn = instance_proc_def_clauses(InstanceClauses),
        clauses_info_init(PredOrFunc, PredArity,
            init_clause_item_numbers_comp_gen, ClausesInfo0),
        list.foldl5(
            produce_instance_method_clause(PredOrFunc, Context, Status),
            InstanceClauses, !TVarSet, !ModuleInfo, !QualInfo,
            ClausesInfo0, ClausesInfo, !Specs)
    ).

:- pred produce_instance_method_clause(pred_or_func::in,
    prog_context::in, import_status::in, item_clause_info::in,
    tvarset::in, tvarset::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, clauses_info::in, clauses_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

produce_instance_method_clause(PredOrFunc, Context, Status, InstanceClause,
        TVarSet0, TVarSet, !ModuleInfo, !QualInfo, !ClausesInfo, !Specs) :-
    InstanceClause = item_clause_info(_Origin, CVarSet, ClausePredOrFunc,
        PredName, HeadTerms0, Body, _ClauseContext, _SeqNum),
    % XXX Can this ever fail? If yes, we should generate an error message
    % instead of aborting.
    expect(unify(PredOrFunc, ClausePredOrFunc), $module, $pred,
        "PredOrFunc mismatch"),
    ( illegal_state_var_func_result(PredOrFunc, HeadTerms0, StateVar) ->
        TVarSet = TVarSet0,
        report_illegal_func_svar_result(Context, CVarSet, StateVar, !Specs)
    ;
        expand_bang_states(HeadTerms0, HeadTerms),
        PredArity = list.length(HeadTerms),
        adjust_func_arity(PredOrFunc, Arity, PredArity),

        % AllProcIds is only used when the predicate has foreign procs,
        % which the instance method pred should not have, so this dummy value
        % should be ok.
        AllProcIds = [],

        GoalType = goal_type_none,    % goal is not a promise
        clauses_info_add_clause(all_modes, AllProcIds, CVarSet, TVarSet0,
            HeadTerms, Body, Context, no, Status, PredOrFunc, Arity,
            GoalType, Goal, VarSet, TVarSet, !ClausesInfo, Warnings,
            !ModuleInfo, !QualInfo, !Specs),

        SimpleCallId = simple_call_id(PredOrFunc, PredName, Arity),

        % Warn about singleton variables.
        warn_singletons(!.ModuleInfo, SimpleCallId, VarSet, Goal, !Specs),

        % Warn about variables with overlapping scopes.
        add_quant_warnings(SimpleCallId, VarSet, Warnings, !Specs)
    ).

:- pred pred_method_with_no_modes_error(pred_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

pred_method_with_no_modes_error(PredInfo, !Specs) :-
    pred_info_get_context(PredInfo, Context),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),

    Pieces = [words("Error: no mode declaration"),
        words("for type class method predicate"),
        sym_name_and_arity(qualified(Module, Name) / Arity), suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred undefined_type_class_error(sym_name::in, arity::in, prog_context::in,
    string::in, list(error_spec)::in, list(error_spec)::out) is det.

undefined_type_class_error(ClassName, Arity, Context, Description, !Specs) :-
    Pieces = [words("Error:"), words(Description), words("for"),
        sym_name_and_arity(ClassName / Arity),
        words("without corresponding"), decl("typeclass"), words("declaration."),
        nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred missing_pred_or_func_method_error(sym_name::in, arity::in,
    pred_or_func::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

missing_pred_or_func_method_error(Name, Arity, PredOrFunc, Context, !Specs) :-
    Pieces = [words("Error: mode declaration for type class method"),
        sym_name_and_arity(Name / Arity), words("without corresponding"),
        p_or_f(PredOrFunc), words("method declaration."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_class.
%-----------------------------------------------------------------------------%
