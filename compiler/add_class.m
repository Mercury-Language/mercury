%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds__make_hlds__add_class.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.qual_info.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred module_add_class_defn(list(prog_constraint)::in,
    list(prog_fundep)::in, sym_name::in, list(tvar)::in, class_interface::in,
    tvarset::in, prog_context::in, item_status::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred module_add_instance_defn(module_name::in, list(prog_constraint)::in,
    sym_name::in, list(mer_type)::in, instance_body::in, tvarset::in,
    import_status::in, prog_context::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

    % Given the definition for a predicate or function from a
    % type class instance declaration, produce the clauses_info
    % for that definition.
    %
:- pred do_produce_instance_method_clauses(instance_proc_def::in,
    pred_or_func::in, arity::in, list(mer_type)::in, pred_markers::in,
    term__context::in, import_status::in, clauses_info::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.add_type.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.state_var.
:- import_module libs.compiler_util.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module varset.

module_add_class_defn(Constraints, FunDeps, Name, Vars, Interface, VarSet,
        Context, Status, !ModuleInfo, !IO) :-
    module_info_get_class_table(!.ModuleInfo, Classes0),
    module_info_get_superclass_table(!.ModuleInfo, SuperClasses0),
    list__length(Vars, ClassArity),
    ClassId = class_id(Name, ClassArity),
    Status = item_status(ImportStatus0, _),
    ( Interface = abstract ->
        make_status_abstract(ImportStatus0, ImportStatus1)
    ;
        ImportStatus1 = ImportStatus0
    ),
    HLDSFunDeps = list__map(make_hlds_fundep(Vars), FunDeps),
    (
        % The typeclass is exported if *any* occurrence is exported,
        % even a previous abstract occurrence.
        map__search(Classes0, ClassId, OldDefn)
    ->
        OldDefn = hlds_class_defn(OldStatus, OldConstraints, OldFunDeps,
            _OldAncestors, OldVars, _OldKinds, OldInterface, OldMethods,
            OldVarSet, OldContext),
        combine_status(ImportStatus1, OldStatus, ImportStatus),
        (
            OldInterface = concrete(_),
            ClassMethods0 = OldMethods,
            ClassInterface = OldInterface
        ;
            OldInterface = abstract,
            ClassMethods0 = [],
            ClassInterface = Interface
        ),
        (
            \+ superclass_constraints_are_identical(OldVars, OldVarSet,
                OldConstraints, Vars, VarSet, Constraints)
        ->
            % Always report the error, even in `.opt' files.
            DummyStatus = local,
            multiple_def_error(DummyStatus, Name, ClassArity, "typeclass",
                Context, OldContext, _, !IO),
            prog_out__write_context(Context, !IO),
            io__write_string("  The superclass constraints do not match.\n",
                !IO),
            io__set_exit_status(1, !IO),
            ErrorOrPrevDef = yes
        ;
            \+ class_fundeps_are_identical(OldFunDeps, HLDSFunDeps)
        ->
            % Always report the error, even in `.opt' files.
            DummyStatus = local,
            multiple_def_error(DummyStatus, Name, ClassArity, "typeclass",
                Context, OldContext, _, !IO),
            prog_out__write_context(Context, !IO),
            io__write_string("  The functional dependencies do not match.\n",
                !IO),
            io__set_exit_status(1, !IO),
            ErrorOrPrevDef = yes
        ;
            Interface = concrete(_),
            OldInterface = concrete(_)
        ->
            multiple_def_error(ImportStatus, Name, ClassArity,
                "typeclass", Context, OldContext, _, !IO),
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
            Interface = concrete(Methods),
            module_add_class_interface(Name, Vars, Methods,
                Status, PredProcIds0, !ModuleInfo, !IO),
                % Get rid of the `no's from the list of maybes
            IsYes = (pred(Maybe::in, PredProcId::out) is semidet :-
                Maybe = yes(Pred - Proc),
                PredProcId = hlds_class_proc(Pred, Proc)
            ),
            list__filter_map(IsYes, PredProcIds0, PredProcIds1),

                %
                % The list must be sorted on pred_id and then
                % proc_id -- check_typeclass.m assumes this
                % when it is generating the corresponding list
                % of pred_proc_ids for instance definitions.
                %
            list__sort(PredProcIds1, ClassMethods)
        ;
            Interface = abstract,
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
        map__set(Classes0, ClassId, Defn, Classes),
        module_info_set_class_table(Classes, !ModuleInfo),

        (
            IsNewDefn = yes,
            update_superclass_table(ClassId, Vars, VarSet, Constraints,
                SuperClasses0, SuperClasses),

            module_info_set_superclass_table(SuperClasses, !ModuleInfo),

                % When we find the class declaration, make an
                % entry for the instances.
            module_info_get_instance_table(!.ModuleInfo, Instances0),
            map__det_insert(Instances0, ClassId, [], Instances),
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
	unexpected(this_file, "get_list_index: element not found").
get_list_index([E | Es], N, X) =
	( X = E ->
		N
	;
		get_list_index(Es, N + 1, X)
	).

:- pred superclass_constraints_are_identical(list(tvar)::in, tvarset::in,
    list(prog_constraint)::in, list(tvar)::in, tvarset::in,
    list(prog_constraint)::in) is semidet.

superclass_constraints_are_identical(OldVars0, OldVarSet, OldConstraints0,
        Vars, VarSet, Constraints) :-
    tvarset_merge_renaming(VarSet, OldVarSet, _, Renaming),
    apply_variable_renaming_to_prog_constraint_list(Renaming, OldConstraints0,
        OldConstraints1),
    apply_variable_renaming_to_tvar_list(Renaming, OldVars0,  OldVars),

    map__from_corresponding_lists(OldVars, Vars, VarRenaming),
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
    list(class_method)::in, item_status::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

module_add_class_interface(Name, Vars, Methods, Status, PredProcIds,
        !ModuleInfo, !IO) :-
    module_add_class_interface_2(Name, Vars, Methods, Status, PredProcIds0,
        !ModuleInfo, !IO),
    check_method_modes(Methods, PredProcIds0, PredProcIds,
        !ModuleInfo, !IO).

:- pred module_add_class_interface_2(sym_name::in, list(tvar)::in,
    list(class_method)::in, item_status::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

module_add_class_interface_2(_, _, [], _, [], !ModuleInfo, !IO).
module_add_class_interface_2(Name, Vars, [M | Ms], Status, [P | Ps],
        !ModuleInfo, !IO) :-
    module_add_class_method(M, Name, Vars, Status, P, !ModuleInfo, !IO),
    module_add_class_interface_2(Name, Vars, Ms, Status, Ps, !ModuleInfo, !IO).

:- pred module_add_class_method(class_method::in, sym_name::in, list(tvar)::in,
    item_status::in, maybe(pair(pred_id, proc_id))::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

module_add_class_method(Method, Name, Vars, Status, MaybePredIdProcId,
        !ModuleInfo, !IO) :-
    (
        Method = pred_or_func(TypeVarSet, InstVarSet, ExistQVars, PredOrFunc,
            PredName, TypesAndModes, _WithType, _WithInst, MaybeDet, _Cond,
            Purity, ClassContext, Context),
        % XXX kind inference:
        % We set the kinds to `star' at the moment.  This will be different
        % when we have a kind system.
        prog_type.var_list_to_type_list(map.init, Vars, Args),
        ClassContext = constraints(UnivCnstrs, ExistCnstrs),
        NewUnivCnstrs = [constraint(Name, Args) | UnivCnstrs],
        NewClassContext = constraints(NewUnivCnstrs, ExistCnstrs),
        init_markers(Markers0),
        add_marker(class_method, Markers0, Markers),
        module_add_pred_or_func(TypeVarSet, InstVarSet, ExistQVars, PredOrFunc,
            PredName, TypesAndModes, MaybeDet, Purity, NewClassContext,
            Markers, Context, Status, MaybePredIdProcId, !ModuleInfo, !IO)
    ;
        Method = pred_or_func_mode(VarSet, MaybePredOrFunc, PredName,
            Modes, _WithInst, MaybeDet, _Cond, Context),
        (
            MaybePredOrFunc = yes(PredOrFunc),
            Status = item_status(ImportStatus, _),
            IsClassMethod = yes,
            module_add_mode(VarSet, PredName, Modes, MaybeDet, ImportStatus,
                Context, PredOrFunc, IsClassMethod, PredIdProcId, !ModuleInfo,
                !IO),
            MaybePredIdProcId = yes(PredIdProcId)
        ;
            MaybePredOrFunc = no,
            % equiv_type.m should have either set the
            % pred_or_func or removed the item from the list.
            unexpected(this_file, "module_add_class_method: " ++
                "no pred_or_func on mode declaration")
        )
    ).

    % Insert an entry into the super class table for each super class of
    % this class.
    %
:- pred update_superclass_table(class_id::in, list(tvar)::in, tvarset::in,
    list(prog_constraint)::in, superclass_table::in, superclass_table::out)
    is det.

update_superclass_table(ClassId, Vars, VarSet, Constraints, !Supers) :-
    list.foldl(update_superclass_table_2(ClassId, Vars, VarSet), Constraints,
        !Supers).

:- pred update_superclass_table_2(class_id::in, list(tvar)::in, tvarset::in,
    prog_constraint::in, superclass_table::in, superclass_table::out) is det.

update_superclass_table_2(ClassId, Vars, VarSet, Constraint, !Supers) :-
    Constraint = constraint(SuperName, SuperTypes),
    list__length(SuperTypes, SuperClassArity),
    SuperClassId = class_id(SuperName, SuperClassArity),
    SubClassDetails = subclass_details(SuperTypes, ClassId, Vars, VarSet),
    multi_map__set(!.Supers, SuperClassId, SubClassDetails, !:Supers).

    % Go through the list of class methods, looking for
    % - functions without mode declarations: add a default mode
    % - predicates without mode declarations: report an error
    % - mode declarations with no determinism: report an error
    %
:- pred check_method_modes(list(class_method)::in,
    list(maybe(pair(pred_id, proc_id)))::in,
    list(maybe(pair(pred_id, proc_id)))::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

check_method_modes([], !PredProcIds, !ModuleInfo, !IO).
check_method_modes([Method | Methods], !PredProcIds, !ModuleInfo, !IO) :-
    (
        Method = pred_or_func(_, _, _, PorF, QualName, TypesAndModes,
            _WithType, _WithInst, _, _, _, _, _)
    ->
        (
            QualName = qualified(ModuleName0, Name0),
            ModuleName = ModuleName0,
            Name = Name0
        ;
            QualName = unqualified(_),
            % The class interface should be fully module qualified
            % by prog_io.m at the time it is read in.
            unexpected(this_file,
                "add_default_class_method_func_modes: unqualified func")
        ),
        list__length(TypesAndModes, PredArity),
        module_info_get_predicate_table(!.ModuleInfo, PredTable),
        (
            predicate_table_search_pf_m_n_a(PredTable, is_fully_qualified,
                PorF, ModuleName, Name, PredArity, [PredId])
        ->
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
            (
                PorF = function,
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
                PorF = predicate,
                pred_info_procedures(PredInfo0, Procs),
                ( map__is_empty(Procs) ->
                    pred_method_with_no_modes_error(PredInfo0, !IO)
                ;
                    true
                )
            )
        ;
            unexpected(this_file, "handle_methods_with_no_modes")
        )
    ;
        true
    ),
    check_method_modes(Methods, !PredProcIds, !ModuleInfo, !IO).

module_add_instance_defn(InstanceModuleName, Constraints, ClassName,
        Types, Body0, VarSet, Status, Context, !ModuleInfo, !IO) :-
    module_info_get_class_table(!.ModuleInfo, Classes),
    module_info_get_instance_table(!.ModuleInfo, Instances0),
    list__length(Types, ClassArity),
    ClassId = class_id(ClassName, ClassArity),
    Body = expand_bang_state_var_args_in_instance_method_heads(Body0),
    (
        map__search(Classes, ClassId, _)
    ->
        map__init(Empty),
        NewInstanceDefn = hlds_instance_defn(InstanceModuleName, Status,
            Context, Constraints, Types, Body, no, VarSet, Empty),
        map__lookup(Instances0, ClassId, InstanceDefns),
        check_for_overlapping_instances(NewInstanceDefn, InstanceDefns,
            ClassId, !IO),
        map__det_update(Instances0, ClassId,
            [NewInstanceDefn | InstanceDefns], Instances),
        module_info_set_instance_table(Instances, !ModuleInfo)
    ;
        undefined_type_class_error(ClassName, ClassArity, Context,
            "instance declaration", !IO)
    ).

:- pred check_for_overlapping_instances(hlds_instance_defn::in,
    list(hlds_instance_defn)::in, class_id::in, io::di, io::uo) is det.

check_for_overlapping_instances(NewInstanceDefn, InstanceDefns, ClassId,
        !IO) :-
    IsOverlapping = (pred((Context - OtherContext)::out) is nondet :-
        NewInstanceDefn = hlds_instance_defn(_, _Status, Context,
            _, Types, Body, _, VarSet, _),
        Body \= abstract, % XXX
        list__member(OtherInstanceDefn, InstanceDefns),
        OtherInstanceDefn = hlds_instance_defn(_, _OtherStatus,
            OtherContext, _, OtherTypes, OtherBody, _, OtherVarSet, _),
        OtherBody \= abstract, % XXX
        tvarset_merge_renaming(VarSet, OtherVarSet, _NewVarSet, Renaming),
        apply_variable_renaming_to_type_list(Renaming, OtherTypes,
            NewOtherTypes),
        type_list_subsumes(Types, NewOtherTypes, _)
    ),
    aggregate(IsOverlapping, report_overlapping_instance_declaration(ClassId),
        !IO).

:- pred report_overlapping_instance_declaration(class_id::in,
    pair(prog_context)::in, io::di, io::uo) is det.

report_overlapping_instance_declaration(class_id(ClassName, ClassArity),
        Context - OtherContext, !IO) :-
    io__set_exit_status(1, !IO),
    Pieces1 = [words("Error: multiply defined (or overlapping)"),
        words("instance declarations for class"),
        sym_name_and_arity(ClassName / ClassArity),
        suffix("."), nl],
    Pieces2 = [words("Previous instance declaration was here.")],
    write_error_pieces(Context, 0, Pieces1, !IO),
    write_error_pieces(OtherContext, 0, Pieces2, !IO).

do_produce_instance_method_clauses(InstanceProcDefn, PredOrFunc, PredArity,
        ArgTypes, Markers, Context, Status, ClausesInfo, !ModuleInfo,
        !QualInfo, !IO) :-
    (
        % Handle the `pred(<MethodName>/<Arity>) is <ImplName>' syntax.
        InstanceProcDefn = name(InstancePredName), 
        % Add the body of the introduced pred.
        % First the goal info, ...
        goal_info_init(GoalInfo0),
        goal_info_set_context(Context, GoalInfo0, GoalInfo1),
        set__list_to_set(HeadVars, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo1, GoalInfo2),
        ( check_marker(Markers, is_impure) ->
            goal_info_add_feature(impure_goal, GoalInfo2, GoalInfo)
        ; check_marker(Markers, is_semipure) ->
            goal_info_add_feature(semipure_goal, GoalInfo2, GoalInfo)
        ;
            GoalInfo = GoalInfo2
        ),
        % ... and then the goal itself.
        varset__init(VarSet0),
        make_n_fresh_vars("HeadVar__", PredArity, HeadVars, VarSet0, VarSet),
        construct_pred_or_func_call(invalid_pred_id, PredOrFunc,
            InstancePredName, HeadVars, GoalInfo, IntroducedGoal, !QualInfo),
        IntroducedClause = clause([], IntroducedGoal, mercury, Context),

        map__from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
        map__init(TVarNameMap),
        rtti_varmaps_init(RttiVarMaps),
        HasForeignClauses = no,
        set_clause_list([IntroducedClause], ClausesRep),
        ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
            HeadVars, ClausesRep, RttiVarMaps, HasForeignClauses)
    ;
        % Handle the arbitrary clauses syntax.
        InstanceProcDefn = clauses(InstanceClauses),
        clauses_info_init(PredArity, ClausesInfo0),
        list__foldl4(
            produce_instance_method_clause(PredOrFunc, Context, Status),
            InstanceClauses, !ModuleInfo, !QualInfo,
            ClausesInfo0, ClausesInfo, !IO)
    ).

:- pred produce_instance_method_clause(pred_or_func::in,
    prog_context::in, import_status::in, item::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    clauses_info::in, clauses_info::out, io::di, io::uo) is det.

produce_instance_method_clause(PredOrFunc, Context, Status, InstanceClause,
        !ModuleInfo, !QualInfo, !ClausesInfo, !IO) :-
    (
        InstanceClause = clause(_Origin, CVarSet, PredOrFunc, PredName,
            HeadTerms0, Body)
    ->
        ( illegal_state_var_func_result(PredOrFunc, HeadTerms0, StateVar) ->
            report_illegal_func_svar_result(Context, CVarSet, StateVar, !IO)
        ;
            HeadTerms = expand_bang_state_var_args(HeadTerms0),
            PredArity = list__length(HeadTerms),
            adjust_func_arity(PredOrFunc, Arity, PredArity),
            % The tvarset argument is only used for explicit type
            % qualifications, of which there are none in this
            % clause, so it is set to a dummy value.
            varset__init(TVarSet0),

            ProcIds = [],
            % means this clause applies to _every_ mode of the procedure
            GoalType = none,    % goal is not a promise
            clauses_info_add_clause(ProcIds, CVarSet, TVarSet0, HeadTerms,
                Body, Context, Status, PredOrFunc, Arity, GoalType, Goal,
                VarSet, _TVarSet, !ClausesInfo, Warnings, !ModuleInfo,
                !QualInfo, !IO),

            % Warn about singleton variables.
            maybe_warn_singletons(VarSet, PredOrFunc - PredName/Arity,
                !.ModuleInfo, Goal, !IO),

            % Warn about variables with overlapping scopes.
            maybe_warn_overlap(Warnings, VarSet, PredOrFunc - PredName/Arity,
                !IO)
        )
    ;
        unexpected(this_file, "produce_clause: invalid instance item")
    ).

:- pred pred_method_with_no_modes_error(pred_info::in, io::di, io::uo) is det.

pred_method_with_no_modes_error(PredInfo, !IO) :-
    pred_info_context(PredInfo, Context),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    
    Pieces = [words("Error: no mode declaration for type class method"),
        words("predicate"),
        sym_name_and_arity(qualified(Module, Name) / Arity), suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io__set_exit_status(1, !IO).

:- pred undefined_type_class_error(sym_name::in, int::in, prog_context::in,
    string::in, io::di, io::uo) is det.

undefined_type_class_error(ClassName, Arity, Context, Description, !IO) :-
    Pieces = [words("Error:"), words(Description), words("for"),
        sym_name_and_arity(ClassName / Arity),
        words("without preceding typeclass declaration.")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io__set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "add_class.m".

%-----------------------------------------------------------------------------%
:- end_module add_class.
%-----------------------------------------------------------------------------%
