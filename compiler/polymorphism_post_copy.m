%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012, 2014 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module check_hlds.polymorphism_post_copy.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module list.

%---------------------------------------------------------------------------%

    % Do the parts of the polymorphism transformation that need to be done
    % *after* goals have been copied from clauses to procedures.
    %
:- pred post_copy_polymorphism(list(pred_id)::in,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.introduce_exists_casts.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

post_copy_polymorphism(ExistsCastPredIds, !ModuleInfo) :-
    list.foldl(introduce_exists_casts_poly, ExistsCastPredIds, !ModuleInfo),

    % Expand the bodies of all class methods. Class methods for imported
    % classes are only expanded if we are performing type specialization,
    % so that method lookups for imported classes can be optimized.
    %
    % The expansion involves inserting a class_method_call with the appropriate
    % arguments, which is responsible for extracting the appropriate part
    % of the dictionary.

    module_info_get_class_table(!.ModuleInfo, ClassMap),
    module_info_get_name(!.ModuleInfo, ModuleName),
    map.keys(ClassMap, ClassIds0),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    UserTypeSpec = OptTuple ^ ot_spec_types_user_guided,
    (
        UserTypeSpec = do_not_spec_types_user_guided,
        % Don't expand classes from other modules.
        list.filter(class_id_is_from_given_module(ModuleName),
            ClassIds0, ClassIds)
    ;
        UserTypeSpec = spec_types_user_guided,
        ClassIds = ClassIds0
    ),
    map.apply_to_list(ClassIds, ClassMap, ClassDefns),
    list.foldl(expand_class_method_bodies_in_defn, ClassDefns, !ModuleInfo).

:- pred class_id_is_from_given_module(module_name::in, class_id::in)
    is semidet.

class_id_is_from_given_module(ModuleName, ClassId) :-
    ClassId = class_id(qualified(ModuleName, _), _).

:- pred expand_class_method_bodies_in_defn(hlds_class_defn::in,
    module_info::in, module_info::out) is det.

expand_class_method_bodies_in_defn(ClassDefn, !ModuleInfo) :-
    MethodInfos = ClassDefn ^ classdefn_method_infos,
    list.foldl2(expand_class_method_body, MethodInfos,
        1, _, !ModuleInfo).

:- pred expand_class_method_body(method_info::in, int::in, int::out,
    module_info::in, module_info::out) is det.

expand_class_method_body(ClassMethodInfo, !ProcNum, !ModuleInfo) :-
    ClassMethodInfo = method_info(_MethodNum, _MethodName,
        _ClassOrigProc, ClassProc),
    ClassProc = proc(PredId, ProcId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),

    % XXX Looking up the proc_info for ProcId can fail here because
    % post_typecheck.m deletes proc_ids corresponding to indistinguishable
    % modes from the proc_table but does *not* delete any references to those
    % proc_ids from the class table.
    ( if map.search(ProcTable0, ProcId, ProcInfo0) then
        % Find which of the constraints on the pred is the one introduced
        % because it is a class method.
        pred_info_get_class_context(PredInfo0, ClassContext),
        ( if ClassContext = constraints([Head | _], _) then
            InstanceConstraint = Head
        else
            unexpected($pred, "class method is not constrained")
        ),

        proc_info_get_rtti_varmaps(ProcInfo0, RttiVarMaps),
        rtti_lookup_typeclass_info_var(RttiVarMaps, InstanceConstraint,
            TypeClassInfoVar),

        proc_info_get_headvars(ProcInfo0, HeadVars0),
        proc_info_get_argmodes(ProcInfo0, Modes0),
        proc_info_get_declared_determinism(ProcInfo0, MaybeDetism0),
        (
            MaybeDetism0 = yes(Detism)
        ;
            MaybeDetism0 = no,
            % Omitting the determinism for a method is not allowed.
            % But make_hlds will have already detected and reported the error.
            % So here we can just pick some value at random; hopefully
            % something that won't cause flow-on errors. We also mark
            % the predicate as invalid, also to avoid flow-on errors.
            Detism = detism_non,
            module_info_make_pred_id_invalid(PredId, !ModuleInfo)
        ),

        % Work out which argument corresponds to the constraint which is
        % introduced because this is a class method, then delete it
        % from the list of args to the class_method_call. That variable becomes
        % the "dictionary" variable for the class_method_call.
        % (cf. the closure for a higher order call).
        ( if
            list.index1_of_first_occurrence(HeadVars0, TypeClassInfoVar, N),
            delete_nth(HeadVars0, N, HeadVarsPrime),
            delete_nth(Modes0, N, ModesPrime)
        then
            HeadVars = HeadVarsPrime,
            Modes = ModesPrime
        else
            unexpected($pred, "typeclass_info var not found")
        ),

        InstanceConstraint = constraint(ClassName, InstanceArgs),
        list.length(InstanceArgs, InstanceArity),
        pred_info_get_pf_sym_name_arity(PredInfo0, PFSymNameArity),
        BodyGoalExpr = generic_call(
            class_method(TypeClassInfoVar, method_proc_num(!.ProcNum),
                class_id(ClassName, InstanceArity), PFSymNameArity),
            HeadVars, Modes, arg_reg_types_unset, Detism),

        % Make the goal info for the call.
        set_of_var.list_to_set(HeadVars0, NonLocals),
        instmap_delta_from_mode_list(!.ModuleInfo, HeadVars0, Modes0,
            InstmapDelta),
        pred_info_get_purity(PredInfo0, Purity),
        pred_info_get_context(PredInfo0, Context),
        goal_info_init(NonLocals, InstmapDelta, Detism, Purity, Context,
            GoalInfo),
        BodyGoal = hlds_goal(BodyGoalExpr, GoalInfo),

        proc_info_set_goal(BodyGoal, ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo1),
        % XXX STATUS
        ( if pred_info_is_imported(PredInfo1) then
            pred_info_set_status(pred_status(status_opt_imported),
                PredInfo1, PredInfo)
        else
            PredInfo = PredInfo1
        ),

        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    else
        true
    ),
    !:ProcNum = !.ProcNum + 1.

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism_post_copy.
%---------------------------------------------------------------------------%
