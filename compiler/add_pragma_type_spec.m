%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015,2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma.add_pragma_type_spec.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_item.

:- import_module list.

:- pred add_pragma_type_spec_constr(io.text_output_stream::in,
    decl_pragma_type_spec_constr_info::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_type_spec(decl_pragma_type_spec_info::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.
:- import_module recompilation.

:- import_module assoc_list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

add_pragma_type_spec_constr(ProgressStream, TypeSpecConstr,
        !ModuleInfo, !QualInfo, !Specs) :-
    % The general approach we use to implement type_spec_constrained_preds
    % pragmas is to compute the set of ordinary type_spec pragmas they
    % correspond to, and add *those* to !ModuleInfo.
    TypeSpecConstr = decl_pragma_type_spec_constr_info(ModuleName,
        OoMConstraints, ApplyToSupers, OoMTypeSubsts, PragmaTVarSet, _,
        Context, _),
    module_info_get_class_table(!.ModuleInfo, ClassTable),
    % Start by finding out which typeclass constraints we should look for
    % in the predicates in the predicate table. This includes checking
    % whether all the type classes named in OoMConstraints actually exist.
    Constraints = one_or_more_to_list(OoMConstraints),
    list.foldl2(
        build_class_constraint_map(ClassTable, ApplyToSupers, PragmaTVarSet),
        Constraints, map.init, ClassConstraintMap, [], ClassSpecs),
    (
        ClassSpecs = [],
        % All the typeclass constraints in OoMConstraints exist.
        trace [
            compile_time(flag("type_spec_constr_preds")),
            run_time(env("TYPE_SPEC_CONSTR_PREDS")),
            io(!IO)]
        (
            io.output_stream(Stream, !IO),
            ClassConstraintMapAL =
                one_or_more_map.to_flat_assoc_list(ClassConstraintMap),
            io.nl(Stream, !IO),
            list.foldl(write_class_constraint_map_entry(Stream, PragmaTVarSet),
                ClassConstraintMapAL, !IO)
        ),
        % Check all predicates defined in either ModuleName or its submodules
        % to see whether they include one or more of the typeclass constraints
        % we are looking out for, and when we find one, generate type_spec
        % pragmas for that predicate for all the substitutions in
        % OoMTypeSubsts.
        module_info_get_predicate_table(!.ModuleInfo, PredTable),
        predicate_table_get_pred_id_table(PredTable, PredIdTable),
        map.foldl_values(
            maybe_generate_pragma_type_specs_for_pred(ModuleName,
                ClassConstraintMap, PragmaTVarSet, OoMTypeSubsts),
            PredIdTable, [], Pragmas),
        % For one reason for why Pragmas may contain duplicates,
        % see the comment about this in build_class_constraint_map.
        % That one is about different but equivalent instances of
        % the same type class. Another reason is that instances of
        % different type classes may result in the same specialization
        % request.
        list.sort_and_remove_dups(Pragmas, SortedPragmas),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals,
            inform_generated_type_spec_pragmas, Inform),
        (
            Inform = no
        ;
            Inform = yes,
            trace [io(!IO)] (
                Context = context(FileName, LineNumber),
                io.format(ProgressStream,
                    "%% For the type_spec_constrained_preds pragma" ++
                        " at %s:%d,\n",
                    [s(FileName), i(LineNumber)], !IO),
                io.write_string(ProgressStream,
                    "% the compiler generated ", !IO),
                (
                    SortedPragmas = [],
                    io.write_string(ProgressStream,
                        "no type_spec pragmas.\n", !IO)
                ;
                    SortedPragmas = [_ | _],
                    io.write_string(ProgressStream,
                        "these type_spec pragmas:\n", !IO),
                    list.foldl(report_generated_pragma(ProgressStream),
                        SortedPragmas, !IO)
                )
            )
        ),
        % Actually add the generated type_spec pragmas to !ModuleInfo.
        %
        % XXX Since Pragmas were generated by the compiler, if adding them
        % to !ModuleInfo results in any errors, they are the compiler's fault,
        % not the user's. But for now, we want to report them, because if
        % we ignored them, we would never be alerted to their existence.
        list.foldl3(add_pragma_type_spec, SortedPragmas,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        ClassSpecs = [_ | _],
        !:Specs = ClassSpecs ++ !.Specs
    ).

%---------------------%

    % Values of this type represent the set of typeclass constraints
    % we want to specialize. Each class_id in here will correspond
    % either to a constraint in a type_spec_constrained_preds pragma,
    % one its superclasses, or one of *their* superclasses, and so on.
    %
    % We may be on the lookout for more than one instance of a given class,
    % since the constraints in the pragma may refer to multiple instances
    % of that class, either directly, or indirectly through superclasses.
    %
    % All type variables in values of these types come from the pragma.
:- type type_spec_constraint_map == one_or_more_map(class_id, arg_vector).
:- type arg_vector
    --->    arg_vector(list(var_or_ground_type)).

    % Build the set of typeclass instances we need to look for to handle
    % a given type_spec_constrained_preds pragma. This will include the
    % constraints in the first argument of the type_spec_constrained_preds
    % pragma, but may include their projections to their superclasses as well.
    %
:- pred build_class_constraint_map(class_table::in, maybe_apply_to_supers::in,
    tvarset::in, var_or_ground_constraint::in,
    type_spec_constraint_map::in, type_spec_constraint_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_class_constraint_map(ClassTable, ApplyToSupers, PragmaTVarSet,
        Constraint, !ClassConstraintMap, !Specs) :-
    Constraint =
        var_or_ground_constraint(ClassSymName, VarOrGroundTypes, Context),
    list.length(VarOrGroundTypes, NumTypes),
    ClassId = class_id(ClassSymName, NumTypes),
    ( if map.search(ClassTable, ClassId, ClassDefn) then
        ArgVector = arg_vector(VarOrGroundTypes),
        ( if map.search(!.ClassConstraintMap, ClassId, ArgVectors0) then
            ( if one_or_more.member(ArgVector, ArgVectors0) then
                OldOrNew = "main  old"
            else
                OldOrNew = "main  nw+",
                % It is possible for ArgVector to differ from an entry already
                % in ArgVectors0 *only* in the number and/or the name of a type
                % variable. If this is the case, then adding ArgVector
                % to the map will cause our caller to generate duplicate
                % type_spec pragmas. We handle this by having our caller
                % remove duplicates while sorting those pragmas. This requires
                % less code than checking for such differences here, while
                % causing duplicate work to be done only in a situation
                % that is extremely unlikely to arise in practice.
                one_or_more_map.add(ClassId, ArgVector, !ClassConstraintMap)
            )
        else
            OldOrNew = "main  new",
            one_or_more_map.add(ClassId, ArgVector, !ClassConstraintMap)
        ),
        trace [
            compile_time(flag("type_spec_constr_preds")),
            run_time(env("TYPE_SPEC_CONSTR_PREDS")),
            io(!IO)]
        (
            EntryStr = class_constraint_map_entry_to_string(PragmaTVarSet,
                OldOrNew, ClassId, ArgVector),
            io.output_stream(Stream, !IO),
            io.nl(Stream, !IO),
            io.write_string(Stream, EntryStr, !IO)
        ),
        (
            ApplyToSupers = do_not_apply_to_supers
        ;
            ApplyToSupers = apply_to_supers,
            ClassTVars = ClassDefn ^ classdefn_vars,
            map.from_corresponding_lists(ClassTVars, VarOrGroundTypes, Subst0),
            Supers = ClassDefn ^ classdefn_supers,
            list.foldl(
                build_superclass_constraint_map(ClassTable, PragmaTVarSet,
                    Context, Subst0),
                Supers, !ClassConstraintMap)
        )
    else
        Pieces = [words("In the first argument of a"),
            pragma_decl("type_spec_constrained_preds"),
            words("declaration:"), nl,
            words("error: the constraint list references"),
            words("a type class named"), qual_class_id(ClassId), suffix(","),
            words("but there is no visible type class"),
            words("with this name and arity."), nl],
        % XXX TSCP Warn about other arities, and "did you mean" close enough
        % class names
        % XXX Make any code for doing that general enough to handle
        % all other error messages about references to unknown classes.
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

    % This predicate does the same job as build_class_constraint_map above,
    % but specialized to the situation where Constraint comes *not* from
    % the original pragma, but from being the Nth level superclass of
    % one of those constraints. This requires different handling, because
    % any errors we find here are caused by typeclass declarations elsewhere,
    % *not* by the pragma we are processing.
    %
:- pred build_superclass_constraint_map(class_table::in, tvarset::in,
    prog_context::in, map(tvar, var_or_ground_type)::in, prog_constraint::in,
    type_spec_constraint_map::in, type_spec_constraint_map::out) is det.

build_superclass_constraint_map(ClassTable, PragmaTVarSet, Context,
        Subst0, Constraint, !ClassConstraintMap) :-
    constraint_get_class_id_and_types(Constraint, ClassId, Types),
    ( if map.search(ClassTable, ClassId, ClassDefn) then
        compute_superclass_arg_types(Subst0, Types, VarOrGroundTypes),
        ArgVector = arg_vector(VarOrGroundTypes),
        ( if map.search(!.ClassConstraintMap, ClassId, ArgVectors0) then
            ( if one_or_more.member(ArgVector, ArgVectors0) then
                OldOrNew = "super old"
            else
                OldOrNew = "super nw+",
                one_or_more_map.add(ClassId, ArgVector, !ClassConstraintMap)
            )
        else
            OldOrNew = "super new",
            one_or_more_map.add(ClassId, ArgVector, !ClassConstraintMap)
        ),
        trace [
            compile_time(flag("type_spec_constr_preds")),
            run_time(env("TYPE_SPEC_CONSTR_PREDS")),
            io(!IO)]
        (
            EntryStr = class_constraint_map_entry_to_string(PragmaTVarSet,
                OldOrNew, ClassId, ArgVector),
            io.output_stream(Stream, !IO),
            io.write_string(Stream, EntryStr, !IO)
        ),

        ClassTVars = ClassDefn ^ classdefn_vars,
        map.from_corresponding_lists(ClassTVars, VarOrGroundTypes, Subst),
        Supers = ClassDefn ^ classdefn_supers,
        list.foldl(
            build_superclass_constraint_map(ClassTable, PragmaTVarSet,
                Context, Subst),
            Supers, !ClassConstraintMap)
    else
        % The non-existence of the superclass is an error, but it is an error
        % in the declaration of the subclass, not an error in the pragma
        % we are processing. The error will reported when we process the
        % declaration of the subclass; reporting it here also would not
        % help the user.
        true
    ).

%---------------------%

:- pred compute_superclass_arg_types(map(tvar, var_or_ground_type)::in,
    list(mer_type)::in, list(var_or_ground_type)::out) is det.

compute_superclass_arg_types(_, [], []).
compute_superclass_arg_types(Subst, [Type | Types],
        [VarOrGroundType | VarOrGroundTypes]) :-
    ( if Type = type_variable(Var, _) then
        map.lookup(Subst, Var, VarOrGroundType)
    else if type_is_ground(Type, GroundType) then
        VarOrGroundType = ground_type(GroundType)
    else
        unexpected($pred, "type is not var or ground")
    ),
    compute_superclass_arg_types(Subst, Types, VarOrGroundTypes).

%---------------------------------------------------------------------------%

:- pred maybe_generate_pragma_type_specs_for_pred(module_name::in,
    type_spec_constraint_map::in, tvarset::in, one_or_more(type_subst)::in,
    pred_info::in,
    list(decl_pragma_type_spec_info)::in,
    list(decl_pragma_type_spec_info)::out) is det.

maybe_generate_pragma_type_specs_for_pred(PragmaModuleName, ClassConstraintMap,
        PragmaTVarSet, OoMTypeSubsts, PredInfo, !Pragmas) :-
    pred_info_get_module_name(PredInfo, PredModuleName),
    ( if
        is_same_module_or_submodule(PredModuleName, PragmaModuleName),
        pred_info_get_class_context(PredInfo, ClassContext),
        ClassContext =
            univ_exist_constraints(UnivConstraints, _ExistConstraints),
        UnivConstraints = [_ | _],
        % We don't want to type-specialize predicates create by
        % other type_spec pragmas, either user-provided or compiler-generated,
        % for two reasons.
        %
        % First, it does not work; it seems that the way we set up
        % the predicates created by type specialization differs from
        % how we set up ordinary class-constrained predicates, in way
        % that causes compiler errors on a *second* application of type
        % specialization. (I -zs- don't know what the difference is exactly,
        % but you could delete this test and find out. The test data for it
        % was juliensf's csv parser, as it was on 2024 feb 1.)
        %
        % Second, even if type-specializing a type-specialized predicate
        % worked, the result would depend on the *order* in which we processed
        % type_spec pragmas, which is not a good idea.
        pred_info_get_origin(PredInfo, Origin),
        is_pred_origin_type_spec(Origin) = origin_is_not_type_spec
    then
        trace [
            compile_time(flag("type_spec_constr_preds")),
            run_time(env("TYPE_SPEC_CONSTR_PREDS")),
            io(!IO)]
        (
            pred_info_get_name(PredInfo, PredName),
            pred_info_get_context(PredInfo, PredContext),
            PredContext = context(File, Line),
            io.output_stream(Stream, !IO),
            io.format(Stream,
                "\nProcessing %s at %s:%d\n",
                [s(PredName), s(File), i(Line)], !IO)
        ),
        generate_type_spec_solns_for_pred(ClassConstraintMap, PragmaTVarSet,
            PredInfo, UnivConstraints, Solns),
        list.foldl(
            generate_pragma_type_specs_for_pred_soln(PragmaModuleName,
                PragmaTVarSet, PredInfo, OoMTypeSubsts),
            Solns, !Pragmas)
    else
        true
    ).

:- type is_origin_type_spec
    --->    origin_is_not_type_spec
    ;       origin_is_type_spec.

:- func is_pred_origin_type_spec(pred_origin) = is_origin_type_spec.

is_pred_origin_type_spec(Origin) = IsTypeSpec :-
    (
        ( Origin = origin_user(_)
        ; Origin = origin_compiler(_)
        ),
        IsTypeSpec = origin_is_not_type_spec
    ;
        Origin = origin_proc_transform(_, BeforeTransformOrigin, _, _),
        IsTypeSpec = is_pred_origin_type_spec(BeforeTransformOrigin)
    ;
        Origin = origin_pred_transform(PredTransform,
            BeforeTransformOrigin, _),
        (
            PredTransform = pred_transform_pragma_type_spec(_),
            IsTypeSpec = origin_is_type_spec
        ;
            ( PredTransform = pred_transform_distance_granularity(_)
            ; PredTransform = pred_transform_table_generator
            ; PredTransform = pred_transform_ssdebug(_)
            ; PredTransform = pred_transform_structure_reuse
            ),
            IsTypeSpec = is_pred_origin_type_spec(BeforeTransformOrigin)
        )
    ).

%---------------------%

    % Figure out which type vars in the given predicate's signature
    % should be specialized to which types specified in the the
    % type_spec_constrained_preds pragma we are processing.
    %
:- pred generate_type_spec_solns_for_pred(type_spec_constraint_map::in,
    tvarset::in, pred_info::in, list(prog_constraint)::in,
    list(subst_soln)::out) is det.

generate_type_spec_solns_for_pred(ClassConstraintMap, PragmaTVarSet, PredInfo,
        UnivConstraints, Solns) :-
    pred_info_get_typevarset(PredInfo, PredTVarSet),
    % Find out the substitutions implied by each constraint that occurs
    % in both the predicate's class context and in the first argument
    % of the type_spec_constrained_preds pragma. This code calls these
    % substitutions "solutions".
    %
    % If the predicate's class context contains two or more constraints
    % for the same typeclass, record all of the resulting solutions
    % as alternatives for that class.
    list.foldl(
        acc_class_ground_substs(PragmaTVarSet, PredTVarSet,
            ClassConstraintMap),
        UnivConstraints, map.init, ClassSolnsMap),
    map.to_sorted_assoc_list(ClassSolnsMap, ClassSolnsMapAL),
    (
        ClassSolnsMapAL = [],
        Solns = []
    ;
        ClassSolnsMapAL = [HeadClassSoln | TailClassSolns],
        % There is at least one typeclass that occurs in both the predicate's
        % class context and in the first argument of the pragma.
        % If there are two or more, each of which may have more than one
        % solution (though almost all will have just one), then consider
        % all possible combinations that take one solution from each typeclass,
        % and see whether they are compatible. Return, as SolnSetSet,
        % the resulting combined solutions.
        find_all_ground_subst_combinations(PragmaTVarSet, PredTVarSet,
            HeadClassSoln, TailClassSolns, SolnSet),
        set.to_sorted_list(SolnSet, Solns)
    ),
    trace [
        compile_time(flag("type_spec_constr_preds")),
        run_time(env("TYPE_SPEC_CONSTR_PREDS")),
        io(!IO)]
    (
        SolnsStr = dump_subst_soln_list(PragmaTVarSet, PredTVarSet, "\n",
            1, Solns),
        io.output_stream(Stream, !IO),
        io.write_string(Stream, "Solns:\n", !IO),
        io.write_string(Stream, SolnsStr, !IO),
        io.write_string(Stream, "end Solns\n", !IO)
    ).

%---------------------%

    % This maps each class_id that has a constraint in the first argument
    % of the type_spec_constrained_preds pragma we are processing, either
    % directly or (if allowed) indirectly as a superclass, to the set of
    % substitutions specified by those constraints.
    %
    % It is this type that requires subst_soln to be a type that has
    % a canonical representation.
    %
:- type class_solns_map == map(class_id, set(subst_soln)).

    % Values of this type represent either
    %
    % - the substitutions in a class_solns_map (in which case they are
    %   implicitly for the class whose class_id is the corresponding key), or
    %
    % - the result of unifying one ore more of those class-specific solutions.
    %
    % The latter are both the intermediate data structures, and the final
    % result, of find_all_ground_subst_combinations.
    %
:- type subst_soln
    --->    subst_soln(
                % This is a map from the predicate's type vars to the types
                % in the pragma's first argument, but in a form which has
                % a canonical representation, which allows us to construct
                % a set of ground_substs without worrying that the set
                % contains two (or more) elements that are syntactically
                % different but semantically identical.
                set(ground_or_tvar_subst),

                % For the subset of the entries in the previous field
                % where the type in the pragma's first arg is a variable,
                % a map from the pragma's tvar back to the predicate's tvar,
                % again in a form with a canonical representation.
                set(pragma_to_pred_tvar)
            ).

:- type ground_or_tvar_subst
    --->    ground_or_tvar_subst(tvar, var_or_ground_type).
            % Map from the predicate's type vars to the types
            % in the pragma's first argument.

:- type pragma_to_pred_tvar
    --->    pragma_to_pred_tvar(tvar, tvar).
            % The first tvar is from the pragma's tvarset, the second
            % is from the predicate's tvarset.

%---------------------%

    % Accumulate in !SolnsMap the set of solutions for the given typeclass.
    %
:- pred acc_class_ground_substs(tvarset::in, tvarset::in,
    type_spec_constraint_map::in, prog_constraint::in,
    class_solns_map::in, class_solns_map::out) is det.

acc_class_ground_substs(PragmaTVarSet, PredTVarSet, ClassConstraintMap,
        Constraint, !SolnsMap) :-
    constraint_get_class_id_and_types(Constraint, ClassId, Types),
    ( if map.search(ClassConstraintMap, ClassId, OoMClassArgVectors) then
        ClassArgVectors = one_or_more_to_list(OoMClassArgVectors),
        acc_matching_arg_vectors(PragmaTVarSet, PredTVarSet, ClassId, Types,
            ClassArgVectors, !SolnsMap)
    else
        true
    ).

:- pred acc_matching_arg_vectors(tvarset::in, tvarset::in,
    class_id::in, list(mer_type)::in, list(arg_vector)::in,
    class_solns_map::in, class_solns_map::out) is det.

acc_matching_arg_vectors(_, _, _, _Types, [], !SolnsMap).
acc_matching_arg_vectors(PragmaTVarSet, PredTVarSet, ClassId, Types,
        [ArgVector | ArgVectors], !SolnsMap) :-
    % Types come from PredTVarSet, ArgVectors come from PragmaTVarSet.
    ArgVector = arg_vector(VarOrGroundTypes),
    ( if
        is_matching_arg_vector(Types, VarOrGroundTypes,
            map.init, Subst, map.init, RevTVarMap)
    then
        map.to_sorted_assoc_list(Subst, SubstAL),
        PairToGroundOrTVarSubst =
            (func(TV - VoG) = ground_or_tvar_subst(TV, VoG)),
        GroundOrTVarSubsts = list.map(PairToGroundOrTVarSubst, SubstAL),
        GroundOrTVarSubstSet = set.sorted_list_to_set(GroundOrTVarSubsts),

        map.to_sorted_assoc_list(RevTVarMap, RevTVarMapAL),
        PairToPragmaToPred =
            (func(Prag - Pred) = pragma_to_pred_tvar(Prag, Pred)),
        PragmaToPreds = list.map(PairToPragmaToPred, RevTVarMapAL),
        PragmaToPredsSet = set.sorted_list_to_set(PragmaToPreds),

        SubstSoln = subst_soln(GroundOrTVarSubstSet, PragmaToPredsSet),
        ( if map.search(!.SolnsMap, ClassId, SubstSolns0) then
            set.insert(SubstSoln, SubstSolns0, SubstSolns),
            map.det_update(ClassId, SubstSolns, !SolnsMap)
        else
            SubstSolns = set.make_singleton_set(SubstSoln),
            map.det_insert(ClassId, SubstSolns, !SolnsMap)
        ),

        trace [
            compile_time(flag("type_spec_constr_preds")),
            run_time(env("TYPE_SPEC_CONSTR_PREDS")),
            io(!IO)]
        (
            TypesStr = mercury_types_to_string(PredTVarSet, print_name_and_num,
                Types),
            ArgVectorStr = arg_vector_to_string(PragmaTVarSet, ArgVector),
            SolnStr =
                dump_subst_soln(PragmaTVarSet, PredTVarSet, "", SubstSoln),
            io.output_stream(Stream, !IO),
            io.format(Stream, "\nacc_matching_arg_vector for %s\n",
                [s(class_id_to_string(ClassId))], !IO),
            io.format(Stream, "types: %s\n", [s(TypesStr)], !IO),
            io.format(Stream, "arg_vector: %s\n", [s(ArgVectorStr)], !IO),
            io.format(Stream, "subst_soln: %s\n", [s(SolnStr)], !IO)
        )
    else
        true
    ),
    acc_matching_arg_vectors(PragmaTVarSet, PredTVarSet, ClassId, Types,
        ArgVectors, !SolnsMap).

%---------------------%

    % The first argument is the list of the argument types of a typeclass
    % from the class context of the predicate we are processing.
    % The second argument is either the list of argument types of a constraint
    % in the first argument of the type_spec_constrained_preds pragma
    % we are also processing, or is the list of corresponding argument types
    % of its superclass, or *its* superclass, and so on.
    % 
    % The first arg may contain arbitrary type variables anywhere.
    % The types in the second arg will be either variables or ground terms,
    % with nothing in between. Any type variables in it should also be
    % distinct (XXX is this guaranteed to be true?) but we can't express
    % that invariant in the type system, and we don't (yet) check that
    % it actually holds.
    %
    % This predicate tests whether the constraint from the predicate
    % has the constraint from the pragma as an instance. If it does,
    % we succeed, and return
    %
    % - the substitution from predicate tvars to pragma types specifying
    %   that instance, as !:Subst, and
    % - the tvar-to-tvar part of that substitution in reverse form, i.e.
    %   as a renaming from pragma tvars to predicate tvars, as !:RevTVarMap.
    %
:- pred is_matching_arg_vector(list(mer_type)::in,
    list(var_or_ground_type)::in,
    map(tvar, var_or_ground_type)::in, map(tvar, var_or_ground_type)::out,
    map(tvar, tvar)::in, map(tvar, tvar)::out) is semidet.

is_matching_arg_vector([], [], !Subst, !RevTVarMap).
is_matching_arg_vector([Type | Types], [VarOrGroundType | VarOrGroundTypes],
        !Subst, !RevTVarMap) :-
    is_matching_arg_type(Type, VarOrGroundType, !Subst, !RevTVarMap),
    is_matching_arg_vector(Types, VarOrGroundTypes, !Subst, !RevTVarMap).

:- pred is_matching_arg_type(mer_type::in, var_or_ground_type::in,
    map(tvar, var_or_ground_type)::in, map(tvar, var_or_ground_type)::out,
    map(tvar, tvar)::in, map(tvar, tvar)::out) is semidet.

is_matching_arg_type(Type, VarOrGroundType, !Subst, !RevTVarMap) :-
    % There should not be any tvar-to-tvar-to-tvar bindings, because
    % any tvars in Type can be mapped only to things in VarOrGroundType,
    % which then cannot be mapped any further.
    ( if Type = type_variable(TVar, kind_star) then
        ( if map.search(!.Subst, TVar, OldBinding) then
            (
                VarOrGroundType = ground_type(_GroundType),
                % Since VarOrGroundType is ground_type, we *require* this
                % argument type to be ground. Therefore we cannot allow
                % a type variable to match a ground type.
                fail
            ;
                VarOrGroundType = type_var_name(_VoGTypeVar, _VoGTypeName),
                % tVar cannot be allowed to simultaneously match
                % both VoGTypeVar and either
                % - a different VoGTypeVar, or
                % - any ground type.
                ( if OldBinding = VarOrGroundType then
                    true
                else
                    fail
                )
            )
        else
            map.det_insert(TVar, VarOrGroundType, !Subst),
            (
                VarOrGroundType = ground_type(_)
            ;
                VarOrGroundType = type_var_name(VoGTypeVar, _),
                ( if map.insert(VoGTypeVar, TVar, !RevTVarMap) then
                    true
                else
                    fail
                )
            )
        )
    else
        (
            VarOrGroundType = ground_type(GroundType),
            % Since VarOrGroundType is ground_type, we *require* this
            % argument type to be ground. Therefore we cannot allow
            % a non-ground Type such as map(K, string) to match
            % a GroundType such as map(int, string).
            ( if Type = coerce(GroundType) then
                true
            else
                fail
            )
        ;
            VarOrGroundType = type_var_name(_VoGTypeVar, _VoGTypeName),
            % VarOrGroundType makes no demands on Type.
            true
        )
    ).

%---------------------%

    % find_all_ground_subst_combinations(PragmaTVarSet, PredTVarSet,
    %     HeadClassId - HeadSubstSolnSet, TailClassIdsSubstSolns,
    %     FinalSubstSet):
    %
    % Unify all the class-specific solutions in HeadSubstSolnSet with
    % all the class-specific solutions in TailClassIdsSubstSolns,
    % considering all possible combinations that take one solution
    % from each class-specific solution set.
    %
    % While a combinatorial blowup is *theoretically* possible,
    % it will almost certainly be vanishingly rare in practice,
    % both because the class contexts of most predicates contain
    % only very small number of constraints, and because it is very rare
    % for two or more of those constraints to involved the same typeclass.
    %
:- pred find_all_ground_subst_combinations(tvarset::in, tvarset::in,
    pair(class_id, set(subst_soln))::in,
    assoc_list(class_id, set(subst_soln))::in, set(subst_soln)::out) is det.

find_all_ground_subst_combinations(PragmaTVarSet, PredTVarSet,
        HeadClassId - HeadSubstSolnSet, TailClassIdsSubstSolns,
        FinalSubstSet) :-
    trace [
        compile_time(flag("type_spec_constr_preds")),
        run_time(env("TYPE_SPEC_CONSTR_PREDS")),
        io(!IO)]
    (
        io.output_stream(Stream, !IO),
        io.write_string(Stream, "\nfind_all_ground_subst_combinations\n", !IO),
        % Printing HeadClassId here can be slightly misleading, as
        % HeadSubstSolnSet will corresponding to HeadClassId only for the
        % top-level invocation of find_all_ground_subst_combinations.
        % For all later invocations, it will correspond to the
        % result of unifying all the solution sets for the class_ids
        % we have already processed. Since this output is only for debugging,
        % there is no point in creating a more exact description.
        HeadStr = dump_class_id_subst_soln(PragmaTVarSet, PredTVarSet,
            "head ", "\n", HeadClassId - HeadSubstSolnSet),
        io.write_string(Stream, HeadStr, !IO)
    ),
    (
        TailClassIdsSubstSolns = [],
        FinalSubstSet = HeadSubstSolnSet,
        trace [
            compile_time(flag("type_spec_constr_preds")),
            run_time(env("TYPE_SPEC_CONSTR_PREDS")),
            io(!IO)]
        (
            io.output_stream(Stream, !IO),
            io.write_string(Stream, "DONE\n\n", !IO)
        )
    ;
        TailClassIdsSubstSolns =
            [HeadTailClassIdSubstSoln | TailTailClassIdsSubstSolns],
        trace [
            compile_time(flag("type_spec_constr_preds")),
            run_time(env("TYPE_SPEC_CONSTR_PREDS")),
            io(!IO)]
        (
            HeadTailStr =
                dump_class_id_subst_soln(PragmaTVarSet, PredTVarSet,
                    "head_tail ", "\n", HeadTailClassIdSubstSoln),
            io.output_stream(Stream, !IO),
            io.write_string(Stream, HeadTailStr, !IO)
        ),
        HeadTailClassIdSubstSoln = HeadTailClassId - HeadTailSubstSolnSet,
        set.to_sorted_list(HeadSubstSolnSet, HeadSubstSolns),
        set.to_sorted_list(HeadTailSubstSolnSet, HeadTailSubstSolns),
        unify_two_soln_lists_outer_loop(HeadSubstSolns,
            HeadTailSubstSolns, set.init, NextSubstSolnSet),
        find_all_ground_subst_combinations(PragmaTVarSet, PredTVarSet,
            HeadTailClassId - NextSubstSolnSet,
            TailTailClassIdsSubstSolns, FinalSubstSet)
    ).

%---------------------%

:- pred unify_two_soln_lists_outer_loop(
    list(subst_soln)::in, list(subst_soln)::in,
    set(subst_soln)::in, set(subst_soln)::out) is det.

unify_two_soln_lists_outer_loop([], _SolnsB, !UnifiedSolns).
unify_two_soln_lists_outer_loop([SubstA | SolnsA], SolnsB,
        !UnifiedSolns) :-
    unify_two_soln_lists_inner_loop(SubstA, SolnsB,
        !UnifiedSolns),
    unify_two_soln_lists_outer_loop(SolnsA, SolnsB,
        !UnifiedSolns).

:- pred unify_two_soln_lists_inner_loop(
    subst_soln::in, list(subst_soln)::in,
    set(subst_soln)::in, set(subst_soln)::out) is det.

unify_two_soln_lists_inner_loop(_SolnA, [], !UnifiedSolns).
unify_two_soln_lists_inner_loop(SolnA, [SolnB | SolnsB],
        !UnifiedSolns) :-
    ( if unify_two_solns(SolnA, SolnB, UnifiedSoln) then
        set.insert(UnifiedSoln, !UnifiedSolns)
    else
        true
    ),
    unify_two_soln_lists_inner_loop(SolnA, SolnsB, !UnifiedSolns).

:- pred unify_two_solns(subst_soln::in, subst_soln::in,
    subst_soln::out) is semidet.

unify_two_solns(SolnA, SolnB, UnifiedSoln) :-
    SolnA = subst_soln(TVarSubstSetA, PragmaToPredSetA),
    SolnB = subst_soln(TVarSubstSetB, PragmaToPredSetB),
    set.to_sorted_list(TVarSubstSetA, TVarSubstsA),
    set.to_sorted_list(TVarSubstSetB, TVarSubstsB),
    unify_two_subst_lists_loop(TVarSubstsA, TVarSubstsB, UnifiedSubsts),
    set.sorted_list_to_set(UnifiedSubsts, UnifiedSubstsSet),

    unify_pragma_to_pred_sets(PragmaToPredSetA, PragmaToPredSetB,
        PragmaToPredSet),
    UnifiedSoln = subst_soln(UnifiedSubstsSet, PragmaToPredSet).

:- pred unify_two_subst_lists_loop(list(ground_or_tvar_subst)::in,
    list(ground_or_tvar_subst)::in, list(ground_or_tvar_subst)::out)
    is semidet.

unify_two_subst_lists_loop(TVarSubstsA, TVarSubstsB, UnifiedSubst) :-
    (
        TVarSubstsA = [],
        TVarSubstsB = [],
        UnifiedSubst = []
    ;
        TVarSubstsA = [],
        TVarSubstsB = [_ | _],
        UnifiedSubst = TVarSubstsB
    ;
        TVarSubstsA = [_ | _],
        TVarSubstsB = [],
        UnifiedSubst = TVarSubstsA
    ;
        TVarSubstsA = [HeadTVarSubstA | TailTVarSubstsA],
        TVarSubstsB = [HeadTVarSubstB | TailTVarSubstsB],
        HeadTVarSubstA = ground_or_tvar_subst(TVarA, VoGTypeA),
        HeadTVarSubstB = ground_or_tvar_subst(TVarB, VoGTypeB),
        compare(Cmp, TVarA, TVarB),
        (
            Cmp = (=),
            VoGTypeA = VoGTypeB,
            unify_two_subst_lists_loop(TailTVarSubstsA, TailTVarSubstsB,
                TailUnifiedSubst),
            % HeadTVarSubstA and HeadTVarSubstB are identical.
            UnifiedSubst = [HeadTVarSubstA | TailUnifiedSubst]
        ;
            Cmp = (<),
            % TVarA < TVarB
            unify_two_subst_lists_loop(TailTVarSubstsA, TVarSubstsB,
                TailUnifiedSubst),
            UnifiedSubst = [HeadTVarSubstA | TailUnifiedSubst]
        ;
            Cmp = (>),
            % TVarA > TVarB
            unify_two_subst_lists_loop(TVarSubstsA, TailTVarSubstsB,
                TailUnifiedSubst),
            UnifiedSubst = [HeadTVarSubstB | TailUnifiedSubst]
        )
    ).

    % unify_pragma_to_pred_sets(PragmaToPredSetA, PragmaToPredSetB,
    %   PragmaToPredSet):
    %
    % PragmaToPredSetA and PragmaToPredSetB each should describe a map
    % from pragma tvars to pred tvars. Return the union of the two maps
    % (in set form), provided that the two are compatible, in the sense that
    % for any pragma tvars that occur in both, they both map it to the same
    % pred tvar.
    %
:- pred unify_pragma_to_pred_sets(set(pragma_to_pred_tvar)::in,
    set(pragma_to_pred_tvar)::in, set(pragma_to_pred_tvar)::out) is semidet.

unify_pragma_to_pred_sets(PragmaToPredSetA, PragmaToPredSetB,
        PragmaToPredSet) :-
    set.union(PragmaToPredSetA, PragmaToPredSetB, PragmaToPredSetAB),
    set.to_sorted_list(PragmaToPredSetAB, PragmaToPredListAB),
    (
        PragmaToPredListAB = []
    ;
        PragmaToPredListAB = [HeadPragmaToPredAB | TailPragmaToPredAB],
        no_pragma_tvar_is_double_mapped(HeadPragmaToPredAB, TailPragmaToPredAB)
    ),
    PragmaToPredSet = PragmaToPredSetAB.

:- pred no_pragma_tvar_is_double_mapped(pragma_to_pred_tvar::in,
    list(pragma_to_pred_tvar)::in) is semidet.

no_pragma_tvar_is_double_mapped(Head, Tail) :-
    (
        Tail = []
    ;
        Tail = [HeadTail | TailTail],
        Head = pragma_to_pred_tvar(HeadPragmaTVar, _),
        HeadTail = pragma_to_pred_tvar(HeadTailPragmaTVar, _),
        % If HeadPragmaTVar = HeadTailPragmaTVar, then this pragma tvar
        % is mapped to two different pred tvars by the two input args
        % of unify_pragma_to_pred_sets.
        HeadPragmaTVar \= HeadTailPragmaTVar,
        no_pragma_tvar_is_double_mapped(HeadTail, TailTail)
    ).

%---------------------------------------------------------------------------%

    % Given some solutions we have computed for a type_spec_constrained_preds
    % pragma, generate an actual type_spec pragma for each.
    %
:- pred generate_pragma_type_specs_for_pred_soln(module_name::in, tvarset::in,
    pred_info::in, one_or_more(type_subst)::in, subst_soln::in,
    list(decl_pragma_type_spec_info)::in,
    list(decl_pragma_type_spec_info)::out) is det.

generate_pragma_type_specs_for_pred_soln(PragmaModuleName, PragmaTVarSet,
        PredInfo, OoMTypeSubsts, Soln, !Pragmas) :-
    OoMTypeSubsts = one_or_more(HeadTypeSubst, TailTypeSubts),
    generate_pragma_type_spec(PragmaModuleName, PragmaTVarSet, PredInfo,
        Soln, HeadTypeSubst, !Pragmas),
    list.foldl(
        generate_pragma_type_spec(PragmaModuleName, PragmaTVarSet, PredInfo,
            Soln),
        TailTypeSubts, !Pragmas).

:- pred generate_pragma_type_spec(module_name::in, tvarset::in, pred_info::in,
    subst_soln::in, type_subst::in,
    list(decl_pragma_type_spec_info)::in,
    list(decl_pragma_type_spec_info)::out) is det.

generate_pragma_type_spec(PragmaModuleName, PragmaTVarSet, PredInfo,
        Soln, TypeSubst, !Pragmas) :-
    UserArity = pred_info_user_arity(PredInfo),
    MoA = moa_arity(UserArity),
    pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
    (
        PredOrFunc = pf_predicate,
        PFUMM = pfumm_predicate(MoA)
    ;
        PredOrFunc = pf_function,
        PFUMM = pfumm_function(MoA)
    ),
    pred_info_get_sym_name(PredInfo, PredSymName),
    set.init(RecompItems),

    Soln = subst_soln(_Subst, PragmaToPredSet),
    PragmaToPreds = set.to_sorted_list(PragmaToPredSet),
    list.foldl(build_pragma_to_pred_tvar_map, PragmaToPreds,
        map.init, PragmaToPredMap),

    TypeSubst = one_or_more(HeadTVarSubst, TailTVarSubsts),
    TVarSubsts = [HeadTVarSubst | TailTVarSubsts],
    find_type_vars_in_tvar_substs(PragmaToPredMap, TVarSubsts, EffTVarSubsts,
        set.init, EffTypeSubstTVars),
    one_or_more.det_list_to_one_or_more(EffTVarSubsts, EffTypeSubst),
    pred_info_get_typevarset(PredInfo, PredTVarSet),
    construct_pragma_tvarset_components(PredTVarSet, PragmaToPredMap,
        set.to_sorted_list(EffTypeSubstTVars),
        1, NewPragmaNumTVars, map.init, NewPragmaTVarNames,
        map.init, Renaming),
    list.map(construct_pragma_tvar_subst(Renaming),
        EffTVarSubsts, NewPragmaTVarSubsts),
    one_or_more.det_list_to_one_or_more(NewPragmaTVarSubsts,
        NewPragmaTypeSubst),
    varset.construct_varset(NewPragmaNumTVars, NewPragmaTVarNames,
        NewPragmaTVarSet),

    Pragma = decl_pragma_type_spec_info(PFUMM, PredSymName, PragmaModuleName,
        NewPragmaTypeSubst, NewPragmaTVarSet, RecompItems,
        dummy_context, item_no_seq_num),
    !:Pragmas = [Pragma | !.Pragmas],

    trace [
        compile_time(flag("type_spec_constr_preds")),
        run_time(env("TYPE_SPEC_CONSTR_PREDS")),
        io(!IO)]
    (
        PragmaTVarSetStr = dump_tvarset(PragmaTVarSet),
        PredTVarSetStr = dump_tvarset(PredTVarSet),
        SolnStr = dump_subst_soln(PragmaTVarSet, PredTVarSet, "\n", Soln),
        EffTypeSubstStr = dump_type_subst(PragmaTVarSet, "\n", EffTypeSubst),
        NewPragmaTypeSubstStr = dump_type_subst(PragmaTVarSet, "\n",
            NewPragmaTypeSubst),
        io.output_stream(Stream, !IO),
        io.write_string(Stream, "\ngenerate_pragma_type_spec:\n", !IO),
        io.write_string(Stream, "PragmaTVarSet:\n", !IO),
        io.write_string(Stream, PragmaTVarSetStr, !IO),
        io.write_string(Stream, "PredTVarSet:\n", !IO),
        io.write_string(Stream, PredTVarSetStr, !IO),
        io.write_string(Stream, "Soln:\n", !IO),
        io.write_string(Stream, SolnStr, !IO),
        io.write_string(Stream, "EffTypeSubst:\n", !IO),
        io.write_string(Stream, EffTypeSubstStr, !IO),
        io.write_string(Stream, "NewPragmaTypeSubst:\n", !IO),
        io.write_string(Stream, NewPragmaTypeSubstStr, !IO),
        io.write_string(Stream, "Pragma:\n", !IO),
        report_generated_pragma(Stream, Pragma, !IO)
    ).

:- pred build_pragma_to_pred_tvar_map(pragma_to_pred_tvar::in,
    map(tvar, tvar)::in, map(tvar, tvar)::out) is det.

build_pragma_to_pred_tvar_map(PragmaToPred, !PragmaToPredMap) :-
    PragmaToPred = pragma_to_pred_tvar(PragmaTVar, PredTVar),
    map.det_insert(PragmaTVar, PredTVar, !PragmaToPredMap).

:- pred find_type_vars_in_tvar_substs(map(tvar, tvar)::in,
    list(tvar_subst)::in, list(tvar_subst)::out,
    set(tvar)::in, set(tvar)::out) is det.

find_type_vars_in_tvar_substs(_, [], [], !TVars).
find_type_vars_in_tvar_substs(PragmaToPredMap,
        [HeadTVarSubst | TailTVarSubsts], EffTVarSubsts, !TVars) :-
    HeadTVarSubst = tvar_subst(HeadTVar, HeadType),
    (  if map.search(PragmaToPredMap, HeadTVar, _) then
        set.insert(HeadTVar, !TVars),
        set_of_type_vars_in_type(HeadType, HeadTypeTVars),
        set.union(HeadTypeTVars, !TVars),
        find_type_vars_in_tvar_substs(PragmaToPredMap,
            TailTVarSubsts, TailEffTVarSubsts, !TVars),
        EffTVarSubsts = [HeadTVarSubst | TailEffTVarSubsts]
    else
        find_type_vars_in_tvar_substs(PragmaToPredMap,
            TailTVarSubsts, EffTVarSubsts, !TVars)
    ).

:- pred construct_pragma_tvarset_components(tvarset::in, map(tvar, tvar)::in,
    list(tvar)::in, int::in, int::out,
    map(tvar, string)::in, map(tvar, string)::out,
    map(tvar, tvar)::in, map(tvar, tvar)::out) is det.

construct_pragma_tvarset_components(_, _, [],
        !PragmaNumTVars, !PragmaTVarNames, !Renaming).
construct_pragma_tvarset_components(PredTVarSet, RevTVarMap,
        [VoGTVar | VoGTVars], !PragmaNumTVars, !PragmaTVarNames, !Renaming) :-
    Var = force_construct_var(!.PragmaNumTVars),
    map.det_insert(VoGTVar, Var, !Renaming),
    ( if
        map.search(RevTVarMap, VoGTVar, PredTVar),
        varset.lookup_name(PredTVarSet, PredTVar, PredTVarName)
    then
        map.det_insert(Var, PredTVarName, !PragmaTVarNames)
    else
        true
    ),
    !:PragmaNumTVars = !.PragmaNumTVars + 1,
    construct_pragma_tvarset_components(PredTVarSet, RevTVarMap,
        VoGTVars, !PragmaNumTVars, !PragmaTVarNames, !Renaming).

:- pred construct_pragma_tvar_subst(map(tvar, tvar)::in,
    tvar_subst::in, tvar_subst::out) is det.

construct_pragma_tvar_subst(Renaming, VoGTVarSubst, PragmaTVarSubst) :-
    VoGTVarSubst = tvar_subst(VoGTVar, VoGType),
    apply_variable_renaming_to_tvar(Renaming, VoGTVar, PragmaTVar),
    apply_variable_renaming_to_type(Renaming, VoGType, PragmaType),
    PragmaTVarSubst = tvar_subst(PragmaTVar, PragmaType).

%---------------------------------------------------------------------------%
%
% Auxiliary routines for add_pragma_type_spec_constr. Most of them
% are intended to be used in trace goals to help debug the code.
%

:- pred constraint_get_class_id_and_types(prog_constraint::in, class_id::out,
    list(mer_type)::out) is det.

constraint_get_class_id_and_types(Constraint, ClassId, Types) :-
    Constraint = constraint(ClassSymName, Types),
    list.length(Types, NumTypes),
    ClassId = class_id(ClassSymName, NumTypes).

%---------------------%

:- pred report_generated_pragma(io.text_output_stream::in,
    decl_pragma_type_spec_info::in, io::di, io::uo) is det.

report_generated_pragma(Stream, Pragma, !IO) :-
    io.write_string(Stream, "% ", !IO),
    mercury_format_pragma_type_spec(Stream, output_mercury, Pragma, !IO).

%---------------------%

:- pred write_class_constraint_map_entry(io.text_output_stream::in,
    tvarset::in, pair(class_id, arg_vector)::in, io::di, io::uo) is det.

write_class_constraint_map_entry(Stream, TVarSet, ClassId - ArgVector, !IO) :-
    EntryStr = class_constraint_map_entry_to_string(TVarSet, "table",
        ClassId, ArgVector),
    io.write_string(Stream, EntryStr, !IO).

:- func class_constraint_map_entry_to_string(tvarset, string,
    class_id, arg_vector) = string.

class_constraint_map_entry_to_string(TVarSet, Prefix, ClassId, ArgVector)
        = Str :-
    ClassId = class_id(ClassSymName, _ClassArity),
    ClassNameStr = mercury_sym_name_to_string(ClassSymName),
    ArgVectorStr = arg_vector_to_string(TVarSet, ArgVector),
    string.format("%s %s(%s)\n",
        [s(Prefix), s(ClassNameStr), s(ArgVectorStr)], Str).

:- func arg_vector_to_string(tvarset, arg_vector) = string.

arg_vector_to_string(TVarSet, ArgVector) = Str :-
    ArgVector = arg_vector(VarOrGroundTypes),
    (
        VarOrGroundTypes = [],
        Str = ""
    ;
        VarOrGroundTypes = [HeadVoGType | TailVoGTypes],
        Str = var_or_ground_types_to_string(TVarSet, HeadVoGType, TailVoGTypes)
    ).

:- func var_or_ground_types_to_string(tvarset, var_or_ground_type,
    list(var_or_ground_type)) = string.

var_or_ground_types_to_string(TVarSet, HeadVoGType, TailVoGTypes) = Str :-
    HeadStr = var_or_ground_type_to_string(TVarSet, HeadVoGType),
    (
        TailVoGTypes = [],
        Str = HeadStr
    ;
        TailVoGTypes = [HeadTailVoGType | TailTailVoGTypes],
        Str = HeadStr ++ ", " ++ var_or_ground_types_to_string(TVarSet,
            HeadTailVoGType, TailTailVoGTypes)
    ).

:- func var_or_ground_type_to_string(tvarset, var_or_ground_type) = string.

var_or_ground_type_to_string(TVarSet, VoGType) = Str :-
    (
        VoGType = type_var_name(TVar, TVarName),
        Type = type_variable(TVar, kind_star),
        TypeStr = mercury_type_to_string(TVarSet, print_name_and_num, Type),
        string.format("tvar %s %s", [s(TVarName), s(TypeStr)], Str)
    ;
        VoGType = ground_type(GroundType),
        Type = coerce(GroundType),
        Str = mercury_type_to_string(varset.init, print_name_and_num, Type)
    ).

%---------------------%

:- func dump_tvarset(tvarset) = string.

dump_tvarset(TVarSet) = Str :-
    NumAllocated = varset.num_allocated(TVarSet),
    varset.var_name_list(TVarSet, VarNames),
    VarNameStrs = list.map(dump_tvarset_entry, VarNames),
    VarNamesStr = string.join_list(", ", VarNameStrs),
    string.format("tvarset(%d, [%s])\n",
        [i(NumAllocated), s(VarNamesStr)], Str).

:- func dump_tvarset_entry(pair(tvar, string)) = string.

dump_tvarset_entry(TVar - Name) = Str :-
    string.format("%d -> %s", [i(var_to_int(TVar)), s(Name)], Str).

:- func dump_class_id_subst_soln(tvarset, tvarset, string, string,
    pair(class_id, set(subst_soln))) = string.

dump_class_id_subst_soln(PragmaTVarSet, PredTVarSet, Prefix, Suffix,
        ClassId - SubstSolnSet) = Str :-
    ClassId = class_id(ClassSymName, ClassArity),
    string.format("%sclass_id %s/%d\n",
        [s(Prefix), s(sym_name_to_string(ClassSymName)), i(ClassArity)],
        ClassStr),
    SolnStrs = dump_subst_soln_list(PragmaTVarSet, PredTVarSet, Suffix,
        1, set.to_sorted_list(SubstSolnSet)),
    Str = ClassStr ++ SolnStrs.

:- func dump_subst_soln_list(tvarset, tvarset, string, int, list(subst_soln))
    = string.

dump_subst_soln_list(_, _, _, _, []) = "".
dump_subst_soln_list(PragmaTVarSet, PredTVarSet, Suffix, CurSolnNum,
        [HeadSoln | TailSolns]) = Str :-
    HeadStr0 = dump_subst_soln(PragmaTVarSet, PredTVarSet, Suffix, HeadSoln),
    string.format("%d: %s", [i(CurSolnNum), s(HeadStr0)], HeadStr),
    TailStr = dump_subst_soln_list(PragmaTVarSet, PredTVarSet, Suffix,
        CurSolnNum + 1, TailSolns),
    Str = HeadStr ++ TailStr.

:- func dump_subst_soln(tvarset, tvarset, string, subst_soln) = string.

dump_subst_soln(PragmaTVarSet, PredTVarSet, Suffix, SubstSoln) = Str :-
    SubstSoln = subst_soln(GroundOrTVarSubstSet, PragmaToPredTVarMap),
    set.to_sorted_list(GroundOrTVarSubstSet, GroundOrTVarSubsts),
    GroundOrTVarSubstStrs = list.map(
        dump_ground_or_tvar_subst(PragmaTVarSet, PredTVarSet, "  "),
        GroundOrTVarSubsts),
    GroundOrTVarSubstsStr = string.append_list(GroundOrTVarSubstStrs),
    set.to_sorted_list(PragmaToPredTVarMap, PragmaToPredTVarMapAL),
    RenameStrs = list.map(dump_tvar_rename(PragmaTVarSet, PredTVarSet, "  "),
        PragmaToPredTVarMapAL),
    RenamesStr = string.append_list(RenameStrs),
    string.format("subst_soln(\n%s\n%s)%s",
        [s(GroundOrTVarSubstsStr), s(RenamesStr), s(Suffix)], Str).

:- func dump_ground_or_tvar_subst(tvarset, tvarset, string,
    ground_or_tvar_subst) = string.

dump_ground_or_tvar_subst(PragmaTVarSet, PredTVarSet, Prefix, GroundTVarSubst)
        = Str :-
    GroundTVarSubst = ground_or_tvar_subst(PredTVar, VoG),
    PredTVarStr =
        mercury_var_to_string_vs(PredTVarSet, print_name_and_num, PredTVar),
    (
        VoG = type_var_name(PragmaTVar, VoGStr0),
        VoGKindStr = "tvar",
        PragmaTVarStr = mercury_var_to_string_vs(PragmaTVarSet,
            print_name_and_num, PragmaTVar),
        string.format("%s %s", [s(VoGStr0), s(PragmaTVarStr)], VoGStr)
    ;
        VoG = ground_type(GroundType),
        VoGKindStr = "grnd",
        Type = coerce(GroundType),
        % There should be no type variables in Type.
        VoGStr = mercury_type_to_string(varset.init, print_num_only, Type)
    ),
    string.format("%sground_or_tvar_subst(pred %-10s -> pragma %s %s)\n",
        [s(Prefix), s(PredTVarStr), s(VoGKindStr), s(VoGStr)], Str).

:- func dump_type_subst(tvarset, string, type_subst) = string.

dump_type_subst(TVarSet, Suffix, Subst) = Str :-
    Subst = one_or_more(HeadTVarSubst, TailTVarSubsts),
    TVarSubstStrs = list.map(dump_tvar_subst(TVarSet, "  "),
        [HeadTVarSubst | TailTVarSubsts]),
    string.format("type_subst(\n%s)%s",
        [s(string.append_list(TVarSubstStrs)), s(Suffix)], Str).

:- func dump_tvar_subst(tvarset, string, tvar_subst) = string.

dump_tvar_subst(TVarSet, Prefix, TVarSubst) = Str :-
    TVarSubst = tvar_subst(TVar, Type),
    TVarStr = mercury_var_to_string_vs(TVarSet, print_name_and_num, TVar),
    TypeStr = mercury_type_to_string(TVarSet, print_name_and_num, Type),
    string.format("%stvar_subst(%-10s -> %s)\n",
        [s(Prefix), s(TVarStr), s(TypeStr)], Str).

:- func dump_tvar_rename(tvarset, tvarset, string, pragma_to_pred_tvar)
    = string.

dump_tvar_rename(PragmaTVarSet, PredTVarSet, Prefix, PragmaToPredTVar) = Str :-
    PragmaToPredTVar = pragma_to_pred_tvar(PragmaTVar, PredTVar),
    Print = print_name_and_num,
    PragmaTVarStr = mercury_var_to_string_vs(PragmaTVarSet, Print, PragmaTVar),
    PredTVarStr = mercury_var_to_string_vs(PredTVarSet, Print, PredTVar),
    string.format("%spragma_to_pred_tvar_rename(%-10s -> %s)\n",
        [s(Prefix), s(PragmaTVarStr), s(PredTVarStr)], Str).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

add_pragma_type_spec(TypeSpec, !ModuleInfo, !QualInfo, !Specs) :-
    TypeSpec = decl_pragma_type_spec_info(PFUMM, SymName, _, _, _, _,
        Context, _),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    (
        (
            PFUMM = pfumm_predicate(ModesOrArity),
            PredOrFunc = pf_predicate
        ;
            PFUMM = pfumm_function(ModesOrArity),
            PredOrFunc = pf_function
        ),
        MaybePredOrFunc = yes(PredOrFunc),
        (
            ModesOrArity = moa_modes(Modes),
            PredFormArity = arg_list_arity(Modes),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity)
        ;
            ModesOrArity = moa_arity(UserArity),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity)
        ),
        predicate_table_lookup_pf_sym_arity(PredTable, is_fully_qualified,
            PredOrFunc, SymName, PredFormArity, PredIds),
        predicate_table_lookup_pf_sym(PredTable, is_fully_qualified,
            PredOrFunc, SymName, AllArityPredIds)
    ;
        PFUMM = pfumm_unknown(UserArity),
        maybe_warn_about_pfumm_unknown(!.ModuleInfo, "type_spec",
            PFUMM, SymName, Context, !Specs),
        MaybePredOrFunc = no,
        predicate_table_lookup_sym_arity(PredTable, is_fully_qualified,
            SymName, UserArity, PredIds),
        predicate_table_lookup_sym(PredTable, is_fully_qualified,
            SymName, AllArityPredIds)
    ),
    (
        PredIds = [],
        module_info_get_pred_id_table(!.ModuleInfo, PredIdTable),
        find_user_arities_other_than(PredIdTable, AllArityPredIds, UserArity,
            OtherUserArities),
        report_undefined_pred_or_func_error(MaybePredOrFunc, SymName,
            UserArity, OtherUserArities, Context,
            [pragma_decl("type_spec"), words("declaration")], !Specs)
    ;
        PredIds = [_ | _],
        list.foldl3(add_pragma_type_spec_for_pred(TypeSpec),
            PredIds, !ModuleInfo, !QualInfo, !Specs)
    ).

:- pred add_pragma_type_spec_for_pred(decl_pragma_type_spec_info::in,
    pred_id::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pragma_type_spec_for_pred(TypeSpec, PredId,
        !ModuleInfo, !QualInfo, !Specs) :-
    TypeSpec = decl_pragma_type_spec_info(PFUMM0, SymName, _SpecModuleName,
        Subst, TVarSet0, _ExpandedItems, Context, _SeqNum),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    handle_pragma_type_spec_subst(PredInfo0, TVarSet0, Subst, Context,
        MaybeSubstResult),
    (
        MaybeSubstResult = ok5(TVarSet, Types, ExistQVars, ClassContext,
            RenamedSubst),
        pred_info_get_proc_table(PredInfo0, ProcTable0),
        handle_pragma_type_spec_modes(!.ModuleInfo, PredId, PredInfo0,
            ProcTable0, TVarSet0, Context, PFUMM0, MaybeSpecProcs),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.get_opt_tuple(Globals, OptTuple),
        DoTypeSpec = OptTuple ^ ot_spec_types_user_guided,
        globals.lookup_bool_option(Globals, smart_recompilation, Smart),
        % XXX Should check whether smart recompilation has been disabled?
        ( if
            MaybeSpecProcs = ok6(SpecProcTable0, ApplicableModes, SpecProcIds,
                UserArity, PredFormArity, PFUMM),
            % Even if we aren't doing type specialization, we need to create
            % the interface procedures for local predicates to check the
            % type-class correctness of the requested specializations.
            %
            % If we are doing smart recompilation, we need to record the
            % pragmas even if we aren't doing type specialization, to avoid
            % problems with differing output for the recompilation tests
            % in debugging grades.

            ( DoTypeSpec = spec_types_user_guided
            ; not pred_info_is_imported(PredInfo0)
            ; Smart = yes
            )
        then
            add_type_spec_version_of_pred(PredId, PredInfo0, PredFormArity,
                TypeSpec, TVarSet, Types, ExistQVars, ClassContext,
                SpecProcTable0, ApplicableModes,
                SpecPredId, SpecPredStatus, !ModuleInfo),
            record_type_specialization(TypeSpec, PredId, SpecPredId,
                SpecPredStatus, SpecProcIds, RenamedSubst, TVarSet, PFUMM,
                !ModuleInfo),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
            maybe_record_type_spec_in_qual_info(PredOrFunc, SymName, UserArity,
                SpecPredStatus, TypeSpec, !QualInfo)
        else
            !:Specs = get_any_errors6(MaybeSpecProcs) ++ !.Specs
        )
    ;
        MaybeSubstResult = error5(SubstSpecs),
        !:Specs = SubstSpecs ++ !.Specs
    ).

:- func tvar_subst_desc(tvar_subst) = pair(int, mer_type).

tvar_subst_desc(tvar_subst(TVar, Type)) = var_to_int(TVar) - Type.

:- pred add_type_spec_version_of_pred(pred_id::in, pred_info::in,
    pred_form_arity::in, decl_pragma_type_spec_info::in,
    tvarset::in, list(mer_type)::in, existq_tvars::in,
    univ_exist_constraints::in, proc_table::in, clause_applicable_modes::in,
    pred_id::out, pred_status::out, module_info::in, module_info::out) is det.

add_type_spec_version_of_pred(PredId, PredInfo0, PredFormArity, TSInfo0,
        TVarSet, Types, ExistQVars, Constraints, SpecProcTable0,
        ApplicableModes, SpecPredId, SpecPredStatus, !ModuleInfo) :-
    TSInfo0 = decl_pragma_type_spec_info(PFUMM0, SymName, SpecModuleName,
        Subst, TVarSet0, _ExpandedItems, _PragmaContext, _SeqNum),

    % Remove any imported structure sharing and reuse information
    % for the original procedure as they won't be (directly)
    % applicable to the specialized versions.
    map.map_values_only(reset_imported_structure_sharing_reuse,
        SpecProcTable0, SpecProcTable),

    % Build a clause to call the old predicate with the specified types
    % to force the specialization. For imported predicates this forces
    % the creation of the proper interface.
    varset.init(ArgVarSet0),
    PredFormArity = pred_form_arity(PredFormArityInt),
    make_n_fresh_vars("HeadVar__", PredFormArityInt, ArgVars,
        ArgVarSet0, ArgVarSet),

    pred_info_get_context(PredInfo0, PredContext),
    goal_info_init(GoalInfo0),
    set_of_var.list_to_set(ArgVars, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
    goal_info_set_context(PredContext, GoalInfo1, GoalInfo),

    % We don't record the called predicate as used -- it is only used
    % if there is some other call. This call is only used to make
    % higher_order.m generate the interface for the type specialized
    % procedure, and will be removed by higher_order.m after that
    % is done.
    PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
    construct_pred_or_func_call(PredId, PredOrFunc, SymName, ArgVars,
        GoalInfo, Goal),
    Clause = clause(ApplicableModes, Goal, impl_lang_mercury, PredContext, []),
    % XXX We could use explicit type qualifications here for the
    % argument types, but explicit type qualification doesn't work
    % correctly with type inference due to a bug somewhere in
    % typecheck.m -- the explicitly declared types are not kept in
    % sync with the predicate's tvarset after the first pass of
    % type checking.
    % map.from_corresponding_lists(ArgVars, Types, ExplicitVarTypes0)
    init_vartypes(ExplicitVarTypes),
    init_var_table(VarTable),
    rtti_varmaps_init(RttiVarMaps),
    map.init(TVarNameMap),
    ArgsVec = proc_arg_vector_init(PredOrFunc, ArgVars),
    set_clause_list([Clause], ClausesRep),
    ItemNumbers = init_clause_item_numbers_comp_gen,
    Clauses = clauses_info(ArgVarSet, ExplicitVarTypes,
        VarTable, RttiVarMaps, TVarNameMap, ArgsVec, ClausesRep,
        ItemNumbers, no_foreign_lang_clauses, no_clause_syntax_errors),
    pred_info_get_markers(PredInfo0, Markers0),
    add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
    map.init(Proofs),
    map.init(ConstraintMap),

    ( if pred_info_is_imported(PredInfo0) then
        SpecPredStatus = pred_status(status_opt_imported)
    else
        pred_info_get_status(PredInfo0, SpecPredStatus)
    ),

    pfumm_to_maybe_pf_arity_maybe_modes(PFUMM0, MaybePredOrFunc0,
        _Arity, _MaybeModes),
    UnqualName = unqualify_name(SymName),
    Transform = tn_pragma_type_spec(MaybePredOrFunc0, TVarSet0, Subst),
    make_transformed_pred_name(UnqualName, Transform, SpecName),
    pred_info_get_origin(PredInfo0, OrigOrigin),
    SubstDesc = one_or_more.map(tvar_subst_desc, Subst),
    PredTransform = pred_transform_pragma_type_spec(SubstDesc),
    Origin = origin_pred_transform(PredTransform, OrigOrigin, PredId),
    MaybeCurUserDecl = maybe.no,
    GoalType = goal_not_for_promise(np_goal_type_none),
    pred_info_get_var_name_remap(PredInfo0, VarNameRemap),
    pred_info_init(PredOrFunc, SpecModuleName, SpecName, PredFormArity,
        PredContext, Origin, SpecPredStatus, MaybeCurUserDecl, GoalType,
        Markers, Types, TVarSet, ExistQVars, Constraints, Proofs,
        ConstraintMap, Clauses, VarNameRemap, SpecPredInfo0),
    pred_info_set_proc_table(SpecProcTable, SpecPredInfo0, SpecPredInfo),
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(SpecPredInfo, SpecPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo).

:- pred record_type_specialization(decl_pragma_type_spec_info::in,
    pred_id::in, pred_id::in, pred_status::in, list(proc_id)::in,
    type_subst::in, tvarset::in, pred_func_or_unknown_maybe_modes::in,
    module_info::in, module_info::out) is det.

record_type_specialization(TSInfo0, PredId, SpecPredId, SpecPredStatus,
        SpecProcIds, RenamedSubst, TVarSet, PFUMM, !ModuleInfo) :-
    % Record the type specialisation in the module_info.
    module_info_get_type_spec_info(!.ModuleInfo, TypeSpecInfo0),
    TypeSpecInfo0 = type_spec_info(ProcsToSpec0, ForceVersions0, SpecMap0,
        PragmaMap0),
    list.map(
        ( pred(ProcId::in, PredProcId::out) is det :-
            PredProcId = proc(PredId, ProcId)
        ), SpecProcIds, SpecPredProcIds),
    set.insert_list(SpecPredProcIds, ProcsToSpec0, ProcsToSpec),
    set.insert(SpecPredId, ForceVersions0, ForceVersions),

    ( if SpecPredStatus = pred_status(status_opt_imported) then
        % For imported predicates dead_proc_elim.m needs to know that
        % if the original predicate is used, the predicate to force
        % the production of the specialised interface is also used.
        multi_map.set(PredId, SpecPredId, SpecMap0, SpecMap)
    else
        SpecMap = SpecMap0
    ),
    TSInfo = (((TSInfo0
        ^ tspec_pfumm := PFUMM)
        ^ tspec_tsubst := RenamedSubst)
        ^ tspec_tvarset := TVarSet),
    one_or_more_map.add(PredId, TSInfo, PragmaMap0, PragmaMap),
    TypeSpecInfo = type_spec_info(ProcsToSpec, ForceVersions, SpecMap,
        PragmaMap),
    module_info_set_type_spec_info(TypeSpecInfo, !ModuleInfo).

:- pred maybe_record_type_spec_in_qual_info(pred_or_func::in, sym_name::in,
    user_arity::in, pred_status::in, decl_pragma_type_spec_info::in,
    qual_info::in, qual_info::out) is det.

maybe_record_type_spec_in_qual_info(PredOrFunc, SymName, UserArity, PredStatus,
        TSInfo, !QualInfo) :-
    IsImported = pred_status_is_imported(PredStatus),
    (
        IsImported = yes,
        ItemType = pred_or_func_to_recomp_item_type(PredOrFunc),
        UserArity = user_arity(UserArityInt),
        ItemName = recomp_item_name(SymName, UserArityInt),
        ItemId = recomp_item_id(ItemType, ItemName),
        ExpandedItems = TSInfo ^ tspec_items,
        apply_to_recompilation_info(
            recompilation.record_expanded_items(ItemId, ExpandedItems),
            !QualInfo)
    ;
        IsImported = no
    ).

    % Check that the type substitution for a `:- pragma type_spec'
    % declaration is valid.
    % A type substitution is invalid if:
    %   - it substitutes unknown type variables
    %   - it substitutes existentially quantified type variables
    % Type substitutions are also invalid if the replacement types are
    % not ground, however this is a (hopefully temporary) limitation
    % of the current implementation, so it only results in a warning.
    %
:- pred handle_pragma_type_spec_subst(pred_info::in, tvarset::in,
    type_subst::in, prog_context::in,
    maybe5(tvarset, list(mer_type), existq_tvars, univ_exist_constraints,
        type_subst)::out) is det.

handle_pragma_type_spec_subst(PredInfo0, TVarSet0, Subst, Context,
        MaybeSubstResult) :-
    SubstList = one_or_more_to_list(Subst),
    GetTVarType =
        ( pred(tvar_subst(TVar, Type)::in, TVar::out, Type::out) is det ),
    list.map2(GetTVarType, SubstList, VarsToSub, SubstTypes0),
    find_duplicate_list_elements(VarsToSub, MultiSubstVars0),
    (
        MultiSubstVars0 = [_ | _],
        list.sort_and_remove_dups(MultiSubstVars0, MultiSubstVars),
        report_multiple_subst_vars(PredInfo0, Context, TVarSet0,
            MultiSubstVars, Spec),
        MaybeSubstResult = error5([Spec])
    ;
        MultiSubstVars0 = [],
        pred_info_get_typevarset(PredInfo0, CalledTVarSet),
        varset.create_name_var_map(CalledTVarSet, NameVarIndex0),
        list.filter(
            ( pred(Var::in) is semidet :-
                varset.lookup_name(TVarSet0, Var, VarName),
                not map.contains(NameVarIndex0, VarName)
            ), VarsToSub, UnknownVarsToSub),
        (
            UnknownVarsToSub = [],
            % Check that the substitution is not recursive.
            set.list_to_set(VarsToSub, VarsToSubSet),

            type_vars_in_types(SubstTypes0, TVarsInSubstTypes0),
            set.list_to_set(TVarsInSubstTypes0, TVarsInSubstTypes),

            set.intersect(TVarsInSubstTypes, VarsToSubSet, RecSubstTVars0),
            set.to_sorted_list(RecSubstTVars0, RecSubstTVars),

            (
                RecSubstTVars = [],
                map.init(TVarRenaming0),
                list.append(VarsToSub, TVarsInSubstTypes0, VarsToReplace),

                get_new_tvars(VarsToReplace, TVarSet0, CalledTVarSet,
                    TVarSet, NameVarIndex0, _, TVarRenaming0, TVarRenaming),

                % Check that none of the existentially quantified variables
                % were substituted.
                map.apply_to_list(VarsToSub, TVarRenaming, RenamedVarsToSub),
                pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
                list.filter(
                    ( pred(RenamedVar::in) is semidet :-
                        list.member(RenamedVar, ExistQVars)
                    ), RenamedVarsToSub, SubExistQVars),
                (
                    SubExistQVars = [],
                    apply_variable_renaming_to_type_list(TVarRenaming,
                        SubstTypes0, SubstTypes),
                    assoc_list.from_corresponding_lists(RenamedVarsToSub,
                        SubstTypes, SubAL),
                    map.from_assoc_list(SubAL, TypeSubst),

                    % Apply the substitution.
                    pred_info_get_arg_types(PredInfo0, Types0),
                    pred_info_get_class_context(PredInfo0, ClassContext0),
                    apply_rec_subst_to_type_list(TypeSubst, Types0, Types),
                    apply_rec_subst_to_univ_exist_constraints(TypeSubst,
                        ClassContext0, ClassContext),
                    PairToTVarSubst =
                        ( func(TVar - Type) = tvar_subst(TVar, Type) ),
                    RenamedSubsts = list.map(PairToTVarSubst, SubAL),
                    det_list_to_one_or_more(RenamedSubsts, OoMRenamedSubsts),
                    MaybeSubstResult = ok5(TVarSet, Types, ExistQVars,
                        ClassContext, OoMRenamedSubsts)
                ;
                    SubExistQVars = [_ | _],
                    report_subst_existq_tvars(PredInfo0, Context,
                        SubExistQVars, Spec),
                    MaybeSubstResult = error5([Spec])
                )
            ;
                RecSubstTVars = [_ | _],
                report_recursive_subst(PredInfo0, Context, TVarSet0,
                    RecSubstTVars, Spec),
                MaybeSubstResult = error5([Spec])
            )
        ;
            UnknownVarsToSub = [_ | _],
            report_unknown_vars_to_subst(PredInfo0, Context, TVarSet0,
                UnknownVarsToSub, Spec),
            MaybeSubstResult = error5([Spec])
        )
    ).

%---------------------%

:- pred find_duplicate_list_elements(list(T)::in, list(T)::out) is det.

find_duplicate_list_elements([], []).
find_duplicate_list_elements([H | T], DupVars) :-
    find_duplicate_list_elements(T, DupVars0),
    ( if list.member(H, T) then
        DupVars = [H | DupVars0]
    else
        DupVars = DupVars0
    ).

%---------------------------------------------------------------------------%

:- pred report_subst_existq_tvars(pred_info::in, prog_context::in,
    list(tvar)::in, error_spec::out) is det.

report_subst_existq_tvars(PredInfo, Context, SubExistQVars, Spec) :-
    pred_info_get_typevarset(PredInfo, TVarSet),
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error: the substitution includes"),
        words("the existentially quantified type")] ++
        report_variables(SubExistQVars, TVarSet) ++ [suffix(".")],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- pred report_recursive_subst(pred_info::in, prog_context::in, tvarset::in,
    list(tvar)::in, error_spec::out) is det.

report_recursive_subst(PredInfo, Context, TVarSet, RecursiveVars, Spec) :-
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error:")] ++ report_variables(RecursiveVars, TVarSet) ++
        [words(choose_number(RecursiveVars, "occurs", "occur")),
        words("on both sides of the substitution.")],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- pred report_multiple_subst_vars(pred_info::in, prog_context::in,
    tvarset::in, list(tvar)::in, error_spec::out) is det.

report_multiple_subst_vars(PredInfo, Context, TVarSet, MultiSubstVars, Spec) :-
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error:")] ++ report_variables(MultiSubstVars, TVarSet) ++
        [words(choose_number(MultiSubstVars, "has", "have")),
        words("multiple replacement types.")],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- pred report_unknown_vars_to_subst(pred_info::in, prog_context::in,
    tvarset::in, list(tvar)::in, error_spec::out) is det.

report_unknown_vars_to_subst(PredInfo, Context, TVarSet, UnknownVars, Spec) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    (
        PredOrFunc = pf_predicate,
        Decl = "pred"
    ;
        PredOrFunc = pf_function,
        Decl = "func"
    ),
    Pieces = pragma_type_spec_to_pieces(PredInfo) ++
        [words("error:")] ++ report_variables(UnknownVars, TVarSet) ++
        [words(choose_number(UnknownVars, "does not", "do not")),
        words("occur in the"), decl(Decl), words("declaration.")],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- func pragma_type_spec_to_pieces(pred_info) = list(format_piece).

pragma_type_spec_to_pieces(PredInfo) = Pieces :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PFSymNameArity =
        pf_sym_name_arity(PredOrFunc, qualified(Module, Name), PredFormArity),
    Pieces = [words("In"), pragma_decl("type_spec"),
        words("declaration for"),
        qual_pf_sym_name_pred_form_arity(PFSymNameArity), suffix(":"), nl].

:- func report_variables(list(tvar), tvarset) = list(format_piece).

report_variables(SubExistQVars, VarSet) =
    [words(choose_number(SubExistQVars, "variable", "variables")),
    quote(mercury_vars_to_name_only_vs(VarSet, SubExistQVars))].

%---------------------------------------------------------------------------%

    % Check that the mode list for a `:- pragma type_spec' declaration
    % specifies a known procedure.
    %
:- pred handle_pragma_type_spec_modes(module_info::in,
    pred_id::in, pred_info::in, proc_table::in, tvarset::in, prog_context::in,
    pred_func_or_unknown_maybe_modes::in,
    maybe6(proc_table, clause_applicable_modes, list(proc_id), user_arity,
        pred_form_arity, pred_func_or_unknown_maybe_modes)::out)
    is det.

handle_pragma_type_spec_modes(ModuleInfo, PredId, PredInfo, ProcTable, TVarSet,
        Context, PFUMM0, MaybeSpecProcs) :-
    (
        (
            PFUMM0 = pfumm_predicate(ModesOrArity),
            PredOrFunc = pf_predicate
        ;
            PFUMM0 = pfumm_function(ModesOrArity),
            PredOrFunc = pf_function
        ),
        PFUMM = PFUMM0,
        (
            ModesOrArity = moa_modes(ArgModes),
            PredFormArity = arg_list_arity(ArgModes),
            ( if
                get_procedure_matching_argmodes(ModuleInfo, ProcTable,
                    ArgModes, ProcId, ProcInfo)
            then
                % Only this procedure in the original predicate
                % is to be type specialized.
                SpecProcTable = map.singleton(ProcId, ProcInfo),
                user_arity_pred_form_arity(PredOrFunc,
                    UserArity, PredFormArity),
                MaybeSpecProcs = ok6(SpecProcTable, selected_modes([ProcId]),
                    [ProcId], UserArity, PredFormArity, PFUMM)
            else
                varset.coerce(TVarSet, VarSet),
                DescPieces = [pragma_decl("type_spec"), words("declaration")],
                report_undeclared_mode_error(ModuleInfo, PredId, PredInfo,
                    VarSet, ArgModes, DescPieces, Context, [], Specs),
                MaybeSpecProcs = error6(Specs)
            )
        ;
            ModesOrArity = moa_arity(UserArity),
            % Every procedure in the original predicate
            % is to be type specialized.
            map.keys(ProcTable, ProcIds),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            MaybeSpecProcs = ok6(ProcTable, all_modes, ProcIds,
                UserArity, PredFormArity, PFUMM)
        )
    ;
        PFUMM0 = pfumm_unknown(UserArity),
        % Every procedure in the original predicate is to be type specialized.
        map.keys(ProcTable, ProcIds),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        (
            PredOrFunc = pf_predicate,
            PFUMM = pfumm_predicate(moa_arity(UserArity))
        ;
            PredOrFunc = pf_function,
            PFUMM = pfumm_function(moa_arity(UserArity))
        ),
        user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
        MaybeSpecProcs = ok6(ProcTable, all_modes, ProcIds,
            UserArity, PredFormArity, PFUMM)
    ).

:- pred reset_imported_structure_sharing_reuse(
    proc_info::in, proc_info::out) is det.

reset_imported_structure_sharing_reuse(!ProcInfo) :-
    proc_info_reset_imported_structure_sharing(!ProcInfo),
    proc_info_reset_imported_structure_reuse(!ProcInfo).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma.add_pragma_type_spec.
%---------------------------------------------------------------------------%
