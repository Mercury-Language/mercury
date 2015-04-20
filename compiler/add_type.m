%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: add_type.m.
%
% This submodule of make_hlds handles the declarations of new types.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_type.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % We allow more than one "definition" for a given type, as long as
    % only one of them is actually a definition for the current back end.
    % The others may be declarations, which just declare the type name,
    % such as the abstract type "definition" `:- type t.', or they may be
    % definitions for other backends.
    %
:- pred module_add_type_defn(tvarset::in, sym_name::in, list(type_param)::in,
    type_defn::in, prog_context::in, item_status::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Add the constructors and special preds for a type to the HLDS.
    %
:- pred process_type_defn(type_ctor::in, hlds_type_defn::in,
    found_invalid_type::in, found_invalid_type::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred make_status_abstract(import_status::in, import_status::out) is det.

:- pred combine_status(import_status::in, import_status::in,
    import_status::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module hlds.make_hlds.add_special_pred.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_tags.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

module_add_type_defn(TVarSet, Name, Args, TypeDefn, Context,
        item_status(Status0, NeedQual), !ModuleInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    list.length(Args, Arity),
    TypeCtor = type_ctor(Name, Arity),
    convert_type_defn(TypeDefn, TypeCtor, Globals, Body0),
    (
        (
            Body0 = hlds_abstract_type(_)
        ;
            Body0 = hlds_du_type(_, _, _, _, _, _, _, _, _),
            string.suffix(term.context_file(Context), ".int2")
            % If the type definition comes from a .int2 file then we must
            % treat it as abstract. The constructors may only be used
            % by the mode system for comparing `bound' insts to `ground'.
            % XXX This is NOT a robust record of the source; the context
            % could be lost for any number of reasons.
        )
    ->
        make_status_abstract(Status0, Status1)
    ;
        Status1 = Status0
    ),
    (
        % Discriminated unions whose definition consists of a single
        % zero-arity constructor are dummy types. Dummy types are not allowed
        % to have user-defined equality or comparison.

        TypeDefn = parse_tree_du_type(Ctors, MaybeUserUC, _MaybeDirectArg),
        Ctors = [Constructor],
        list.length(Constructor ^ cons_args, 0),
        MaybeUserUC = yes(_),
        % Only report errors for types defined in this module.
        status_defined_in_this_module(Status0) = yes
    ->
        DummyMainPieces = [words("Error: the type"),
            sym_name_and_arity(Name / Arity),
            words("is not allowed to have user-defined equality"),
            words("or comparison.")],
        DummyVerbosePieces = [words("Discriminated unions whose body"),
            words("consists of a single zero-arity constructor"),
            words("cannot have user-defined equality or comparison.")],
        DummyMsg = simple_msg(Context,
            [always(DummyMainPieces),
            verbose_only(verbose_once, DummyVerbosePieces)]),
        DummySpec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [DummyMsg]),
        !:Specs = [DummySpec | !.Specs]
    ;
        true
    ),
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    (
        % The type is exported if *any* occurrence is exported,
        % even a previous abstract occurrence.
        search_type_ctor_defn(TypeTable0, TypeCtor, OldDefn0)
    ->
        hlds_data.get_type_defn_status(OldDefn0, OldStatus),
        combine_status(Status1, OldStatus, Status),
        hlds_data.get_type_defn_body(OldDefn0, OldBody0),
        combine_is_solver_type(OldBody0, OldBody, Body0, Body),
        ( is_solver_type_is_inconsistent(OldBody, Body) ->
            % The existing definition has an is_solver_type annotation
            % which is different to the current definition.
            SolverPieces = [words("In definition of type"),
                sym_name_and_arity(Name / Arity), suffix(":"), nl,
                words("error: all definitions of a type must have"),
                words("consistent"), quote("solver"), words("annotations")],
            SolverMsg = simple_msg(Context, [always(SolverPieces)]),
            SolverSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [SolverMsg]),
            !:Specs = [SolverSpec | !.Specs],
            MaybeOldDefn = no
        ;
            hlds_data.set_type_defn_body(OldBody, OldDefn0, OldDefn),
            MaybeOldDefn = yes(OldDefn)
        )
    ;
        MaybeOldDefn = no,
        Status = Status1,
        Body = Body0
    ),
    % XXX kind inference:
    % We set the kinds to `star'. This will be different when we have a
    % kind system.
    map.init(KindMap),
    hlds_data.set_type_defn(TVarSet, Args, KindMap, Body, Status,
        no, NeedQual, type_defn_no_prev_errors, Context, TypeDefn1),
    (
        MaybeOldDefn = no,
        Body = hlds_foreign_type(_)
    ->
        ForeignDeclPieces = [words("Error: type "),
            sym_name_and_arity(Name / Arity),
            words("defined as foreign_type without being declared.")],
        ForeignDeclMsg = simple_msg(Context, [always(ForeignDeclPieces)]),
        ForeignDeclSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [ForeignDeclMsg]),
        !:Specs = [ForeignDeclSpec | !.Specs]
    ;
        MaybeOldDefn = yes(OldDefn1),
        Body = hlds_foreign_type(_),
        hlds_data.get_type_defn_status(OldDefn1, OldStatus1),
        hlds_data.get_type_defn_body(OldDefn1, OldBody1),
        OldBody1 = hlds_abstract_type(_),
        status_is_exported_to_non_submodules(OldStatus1) = no,
        status_is_exported_to_non_submodules(Status0) = yes
    ->
        ForeignVisPieces = [words("Error: pragma foreign_type "),
            sym_name_and_arity(Name / Arity),
            words("must have the same visibility as the type declaration.")],
        ForeignVisMsg = simple_msg(Context, [always(ForeignVisPieces)]),
        ForeignVisSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [ForeignVisMsg]),
        !:Specs = [ForeignVisSpec | !.Specs],
        % We don't want check_for_missing_type_defns to later report
        % that this type has no non-abstract definition.
        set_type_defn_prev_errors(type_defn_prev_errors,
            TypeDefn1, ErrTypeDefn),
        replace_type_ctor_defn(TypeCtor, ErrTypeDefn,
            TypeTable0, TypeTable),
        module_info_set_type_table(TypeTable, !ModuleInfo)
    ;
        % If there was an existing non-abstract definition for the type, ...
        MaybeOldDefn = yes(OldDefn2),
        hlds_data.get_type_defn_tvarset(OldDefn2, TVarSet2),
        hlds_data.get_type_defn_tparams(OldDefn2, Params2),
        hlds_data.get_type_defn_kind_map(OldDefn2, KindMap2),
        hlds_data.get_type_defn_body(OldDefn2, Body2),
        hlds_data.get_type_defn_context(OldDefn2, OrigContext),
        hlds_data.get_type_defn_status(OldDefn2, OrigStatus),
        hlds_data.get_type_defn_in_exported_eqv(OldDefn2, OrigInExportedEqv),
        hlds_data.get_type_defn_need_qualifier(OldDefn2, OrigNeedQual),
        Body2 \= hlds_abstract_type(_)
    ->
        globals.get_target(Globals, Target),
        globals.lookup_bool_option(Globals, make_optimization_interface,
            MakeOptInt),
        % We used to record that we have seen a foreign type, using the call
        %   module_info_set_contains_foreign_type(!ModuleInfo)
        % but we deleted the code that used this information some time ago.
        (
            % ... then if this definition was abstract, ignore it
            % (but update the status of the old defn if necessary).
            Body = hlds_abstract_type(_)
        ->
            ( Status = OrigStatus ->
                true
            ;
                hlds_data.set_type_defn(TVarSet2, Params2, KindMap2,
                    Body2, Status, OrigInExportedEqv, OrigNeedQual,
                    type_defn_no_prev_errors, OrigContext, TypeDefn3),
                replace_type_ctor_defn(TypeCtor, TypeDefn3,
                    TypeTable0, TypeTable),
                module_info_set_type_table(TypeTable, !ModuleInfo)
            )
        ;
            merge_foreign_type_bodies(Target, MakeOptInt, Body, Body2,
                NewBody)
        ->
            ( check_foreign_type_visibility(OrigStatus, Status1) ->
                hlds_data.set_type_defn(TVarSet2, Params2, KindMap2, NewBody,
                    Status, OrigInExportedEqv, NeedQual,
                    type_defn_no_prev_errors, Context, TypeDefn3),
                replace_type_ctor_defn(TypeCtor, TypeDefn3,
                    TypeTable0, TypeTable),
                module_info_set_type_table(TypeTable, !ModuleInfo)
            ;
                module_info_incr_errors(!ModuleInfo),
                DiffVisPieces = [words("In definition of type"),
                    sym_name_and_arity(Name / Arity), suffix(":"), nl,
                    words("error: all definitions of a type"),
                    words("must have the same visibility."), nl],
                DiffVisMsg = simple_msg(Context, [always(DiffVisPieces)]),
                DiffVisSpec = error_spec(severity_error,
                    phase_parse_tree_to_hlds, [DiffVisMsg]),
                !:Specs = [DiffVisSpec | !.Specs],
                set_type_defn_prev_errors(type_defn_prev_errors,
                    TypeDefn1, ErrTypeDefn),
                replace_type_ctor_defn(TypeCtor, ErrTypeDefn,
                    TypeTable0, TypeTable),
                module_info_set_type_table(TypeTable, !ModuleInfo)
            )
        ;
            % ..., otherwise issue an error message if the second
            % definition wasn't read while reading .opt files.
            Status = status_opt_imported
        ->
            true
        ;
            module_info_incr_errors(!ModuleInfo),
            multiple_def_error(Status, Name, Arity, "type", Context,
                OrigContext, [], !Specs)
        )
    ;
        add_or_replace_type_ctor_defn(TypeCtor, TypeDefn1,
            TypeTable0, TypeTable),
        module_info_set_type_table(TypeTable, !ModuleInfo),
        (
            % XXX We can't handle abstract exported polymorphic equivalence
            % types with monomorphic bodies, because the compiler stuffs up
            % the type_info handling -- the caller passes type_infos,
            % but the callee expects no type_infos.
            Body = hlds_eqv_type(EqvType),
            Status = status_abstract_exported,
            list.member(Var, Args),
            \+ type_contains_var(EqvType, Var)
        ->
            PolyEqvPieces = [words("Sorry, not implemented:"),
                words("polymorphic equivalence type,"),
                words("with monomorphic definition,"),
                words("exported as abstract type.")],
            PolyEqvMsg = simple_msg(Context,
                [always(PolyEqvPieces),
                verbose_only(verbose_once, abstract_monotype_workaround)]),
            PolyEqvSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [PolyEqvMsg]),
            !:Specs = [PolyEqvSpec | !.Specs]
        ;
            true
        )
    ).

:- func abstract_monotype_workaround = list(format_component).

abstract_monotype_workaround = [
    words("A quick work-around is to just export the type as a concrete,"),
    words("type by putting the type definition in the interface section."),
    words("A better work-around is to use a ""wrapper"" type, with just one"),
    words("functor that has just one arg, instead of an equivalence type."),
    words("(There's no performance penalty for this -- the compiler will"),
    words("optimize the wrapper away.)")
    ].

%-----------------------------------------------------------------------------%

    % We do not have syntax for adding `solver' annotations to
    % `:- pragma foreign_type' declarations, so foreign_type bodies
    % default to having an is_solver_type field of `non_solver_type'.
    % If another declaration for the type has a `solver' annotation then
    % we must update the foreign_type body to reflect this.
    %
    % rafe: XXX think it should be an error for foreign types to
    % be solver types.
    %
:- pred combine_is_solver_type(hlds_type_body::in, hlds_type_body::out,
    hlds_type_body::in, hlds_type_body::out) is det.

combine_is_solver_type(OldBody, OldBody, Body, Body).

    % Succeed iff the two type bodies have inconsistent is_solver_type
    % annotations.
:- pred is_solver_type_is_inconsistent(hlds_type_body::in, hlds_type_body::in)
    is semidet.

is_solver_type_is_inconsistent(OldBody, Body) :-
    maybe_get_body_is_solver_type(OldBody, OldIsSolverType),
    maybe_get_body_is_solver_type(Body, IsSolverType),
    OldIsSolverType \= IsSolverType.

:- pred maybe_get_body_is_solver_type(hlds_type_body::in, is_solver_type::out)
    is semidet.

maybe_get_body_is_solver_type(hlds_solver_type(_, _), solver_type).
maybe_get_body_is_solver_type(hlds_abstract_type(Details), IsSolverType) :-
    (
        Details = abstract_type_general,
        IsSolverType = non_solver_type
    ;
        Details = abstract_enum_type(_),
        IsSolverType = non_solver_type
    ;
        Details = abstract_solver_type,
        IsSolverType = solver_type
    ).

    % check_foreign_type_visibility(OldStatus, NewDefnStatus).
    %
    % Check that the visibility of the new definition for a foreign type
    % matches that of previous definitions.
    %
:- pred check_foreign_type_visibility(import_status::in, import_status::in)
    is semidet.

check_foreign_type_visibility(OldStatus, NewDefnStatus) :-
    ( OldStatus = status_abstract_exported  ->
        % If OldStatus is abstract_exported, the previous definitions
        % were local.
        status_is_exported_to_non_submodules(NewDefnStatus) = no
    ; OldStatus = status_exported ->
        NewDefnStatus = status_exported
    ;
        status_is_exported_to_non_submodules(OldStatus) = no,
        status_is_exported_to_non_submodules(NewDefnStatus) = no
    ).

process_type_defn(TypeCtor, TypeDefn, !FoundInvalidType, !ModuleInfo,
        !Specs) :-
    get_type_defn_context(TypeDefn, Context),
    get_type_defn_tvarset(TypeDefn, TVarSet),
    get_type_defn_tparams(TypeDefn, Args),
    get_type_defn_kind_map(TypeDefn, KindMap),
    get_type_defn_body(TypeDefn, Body),
    get_type_defn_status(TypeDefn, Status),
    get_type_defn_need_qualifier(TypeDefn, NeedQual),
    module_info_get_globals(!.ModuleInfo, Globals),
    (
        Body = hlds_du_type(ConsList, _, _, _, UserEqCmp, _DirectArgCtors,
            ReservedTag, _, _),
        module_info_get_cons_table(!.ModuleInfo, Ctors0),
        module_info_get_partial_qualifier_info(!.ModuleInfo, PQInfo),
        module_info_get_ctor_field_table(!.ModuleInfo, CtorFields0),
        TypeCtor = type_ctor(TypeCtorSymName, _),
        (
            TypeCtorSymName = unqualified(_),
            unexpected($module, $pred, "unqualified TypeCtorSymName")
        ;
            TypeCtorSymName = qualified(TypeCtorModuleName, _)
        ),
        add_type_defn_ctors(ConsList, TypeCtor, TypeCtorModuleName, TVarSet,
            Args, KindMap, NeedQual, PQInfo, Status,
            CtorFields0, CtorFields, Ctors0, Ctors, [], CtorAddSpecs),
        module_info_set_cons_table(Ctors, !ModuleInfo),
        module_info_set_ctor_field_table(CtorFields, !ModuleInfo),

        (
            CtorAddSpecs = []
        ;
            CtorAddSpecs = [_ | _],
            !:FoundInvalidType = found_invalid_type,
            !:Specs = CtorAddSpecs ++ !.Specs
        ),

        % Note that process_type_defn is invoked only *after* all the types
        % have been added into the HLDS.
        (
            type_ctor_should_be_notag(Globals, TypeCtor,
                ReservedTag, ConsList, UserEqCmp, CtorName, CtorArgType, _)
        ->
            NoTagType = no_tag_type(Args, CtorName, CtorArgType),
            module_info_get_no_tag_types(!.ModuleInfo, NoTagTypes0),
            map.set(TypeCtor, NoTagType, NoTagTypes0, NoTagTypes),
            module_info_set_no_tag_types(NoTagTypes, !ModuleInfo)
        ;
            true
        )
    ;
        Body = hlds_foreign_type(ForeignTypeBody),
        get_type_defn_prev_errors(TypeDefn, PrevErrors),
        check_foreign_type(TypeCtor, ForeignTypeBody, PrevErrors, Context,
            FoundInvalidTypeInForeignBody, !ModuleInfo, !Specs),
        (
            FoundInvalidTypeInForeignBody = found_invalid_type,
            !:FoundInvalidType = found_invalid_type
        ;
            FoundInvalidTypeInForeignBody = did_not_find_invalid_type
        )
    ;
        ( Body = hlds_abstract_type(_)
        ; Body = hlds_solver_type(_, _)
        ; Body = hlds_eqv_type(_)
        )
    ),
    (
        !.FoundInvalidType = found_invalid_type
    ;
        !.FoundInvalidType = did_not_find_invalid_type,
        % XXX kind inference:
        % We set the kinds to `star'. This will be different when we have
        % a kind system.
        prog_type.var_list_to_type_list(map.init, Args, ArgTypes),
        construct_type(TypeCtor, ArgTypes, Type),
        add_special_preds(TVarSet, Type, TypeCtor, Body, Context, Status,
            !ModuleInfo)
    ).

    % Check_foreign_type ensures that if we are generating code for a specific
    % backend that the foreign type has a representation on that backend.
    %
:- pred check_foreign_type(type_ctor::in, foreign_type_body::in,
    type_defn_prev_errors::in, prog_context::in, found_invalid_type::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_type(TypeCtor, ForeignTypeBody, PrevErrors, Context,
        FoundInvalidType, !ModuleInfo, !Specs) :-
    TypeCtor = type_ctor(Name, Arity),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    ( have_foreign_type_for_backend(Target, ForeignTypeBody, yes) ->
        FoundInvalidType = did_not_find_invalid_type
    ; PrevErrors = type_defn_prev_errors ->
        % The error message being generated below may be misleading,
        % since the relevant foreign language definition of this type
        % may have been present, but in error.
        FoundInvalidType = found_invalid_type
    ;
        ( Target = target_c, LangStr = "C"
        ; Target = target_il, LangStr = "IL"
        ; Target = target_csharp, LangStr = "C#"
        ; Target = target_java, LangStr = "Java"
        ; Target = target_erlang, LangStr = "Erlang"
        ),
        MainPieces = [words("Error: no"), fixed(LangStr),
            pragma_decl("foreign_type"), words("declaration for"),
            sym_name_and_arity(Name/Arity), suffix("."), nl],
        VerbosePieces = [words("There are representations for this type"),
            words("on other back-ends, but none for this back-end."), nl],
        Msg = simple_msg(Context,
            [always(MainPieces), verbose_only(verbose_always, VerbosePieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [Msg]),
        !:Specs = [Spec | !.Specs],
        FoundInvalidType = found_invalid_type
    ).

    % Ignore Mercury definitions if we've got a foreign type
    % declaration suitable for this back-end and we aren't making the
    % optimization interface.  We need to keep the Mercury definition
    % if we are making the optimization interface so that it gets
    % output in the .opt file.
    %
:- pred merge_foreign_type_bodies(compilation_target::in, bool::in,
    hlds_type_body::in, hlds_type_body::in, hlds_type_body::out)
    is semidet.

merge_foreign_type_bodies(Target, MakeOptInterface,
        hlds_foreign_type(ForeignTypeBody0), Body1, Body) :-
    MaybeForeignTypeBody1 = Body1 ^ du_type_is_foreign_type,
    (
        MaybeForeignTypeBody1 = yes(ForeignTypeBody1)
    ;
        MaybeForeignTypeBody1 = no,
        ForeignTypeBody1 = foreign_type_body(no, no, no, no, no)
    ),
    merge_foreign_type_bodies_2(ForeignTypeBody0, ForeignTypeBody1,
        ForeignTypeBody),
    (
        have_foreign_type_for_backend(Target, ForeignTypeBody, yes),
        MakeOptInterface = no
    ->
        Body = hlds_foreign_type(ForeignTypeBody)
    ;
        Body = Body1 ^ du_type_is_foreign_type := yes(ForeignTypeBody)
    ).
merge_foreign_type_bodies(Target, MakeOptInterface,
        Body0 @ hlds_du_type(_, _, _, _, _, _, _, _, _),
        Body1 @ hlds_foreign_type(_), Body) :-
    merge_foreign_type_bodies(Target, MakeOptInterface, Body1, Body0, Body).
merge_foreign_type_bodies(_, _, hlds_foreign_type(Body0),
        hlds_foreign_type(Body1), hlds_foreign_type(Body)) :-
    merge_foreign_type_bodies_2(Body0, Body1, Body).

:- pred merge_foreign_type_bodies_2(foreign_type_body::in,
    foreign_type_body::in, foreign_type_body::out) is semidet.

merge_foreign_type_bodies_2(TypeBodyA, TypeBodyB, TypeBody) :-
    TypeBodyA = foreign_type_body(MaybeILA, MaybeCA, MaybeJavaA, MaybeCSharpA,
        MaybeErlangA),
    TypeBodyB = foreign_type_body(MaybeILB, MaybeCB, MaybeJavaB, MaybeCSharpB,
        MaybeErlangB),
    merge_maybe(MaybeILA, MaybeILB, MaybeIL),
    merge_maybe(MaybeCA, MaybeCB, MaybeC),
    merge_maybe(MaybeJavaA, MaybeJavaB, MaybeJava),
    merge_maybe(MaybeCSharpA, MaybeCSharpB, MaybeCSharp),
    merge_maybe(MaybeErlangA, MaybeErlangB, MaybeErlang),
    TypeBody = foreign_type_body(MaybeIL, MaybeC, MaybeJava, MaybeCSharp,
        MaybeErlang).

:- pred merge_maybe(maybe(T)::in, maybe(T)::in, maybe(T)::out) is semidet.

merge_maybe(no, no, no).
merge_maybe(yes(T), no, yes(T)).
merge_maybe(no, yes(T), yes(T)).

make_status_abstract(Status, AbstractStatus) :-
    ( Status = status_exported ->
        AbstractStatus = status_abstract_exported
    ; Status = status_imported(_) ->
        AbstractStatus = status_abstract_imported
    ;
        AbstractStatus = Status
    ).

combine_status(StatusA, StatusB, Status) :-
    ( combine_status_2(StatusA, StatusB, CombinedStatus) ->
        Status = CombinedStatus
    ;
        unexpected($module, $pred, "unexpected status for type definition")
    ).

:- pred combine_status_2(import_status::in, import_status::in,
    import_status::out) is semidet.

combine_status_2(status_imported(ImportLocn), Status2, Status) :-
    require_complete_switch [ImportLocn]
    (
        ( ImportLocn = import_locn_implementation
        ; ImportLocn = import_locn_interface
        ; ImportLocn = import_locn_ancestor
        ),
        combine_status_imported_non_private(Status2, Status)
    ;
        ImportLocn = import_locn_ancestor_private_interface_proper,
        % If it's private, it's private.
        Status = status_imported(import_locn_ancestor_private_interface_proper)
    ).
combine_status_2(status_local, Status2, Status) :-
    combine_status_local(Status2, Status).
combine_status_2(status_exported, _Status2, status_exported).
combine_status_2(status_exported_to_submodules, Status2, Status) :-
    combine_status_local(Status2, Status3),
    ( Status3 = status_local ->
        Status = status_exported_to_submodules
    ;
        Status = Status3
    ).
combine_status_2(status_opt_imported, _Status2, status_opt_imported).
combine_status_2(status_abstract_imported, Status2, Status) :-
    combine_status_abstract_imported(Status2, Status).
combine_status_2(status_abstract_exported, Status2, Status) :-
    combine_status_abstract_exported(Status2, Status).

:- pred combine_status_imported_non_private(import_status::in,
    import_status::out) is semidet.

combine_status_imported_non_private(Status2, Status) :-
    (
        Status2 = status_imported(Section),
        Status = status_imported(Section)
    ;
        Status2 = status_local,
        Status = status_imported(import_locn_implementation)
    ;
        Status2 = status_exported,
        Status = status_exported
    ;
        Status2 = status_opt_imported,
        Status = status_opt_imported
    ;
        Status2 = status_abstract_imported,
        Status = status_imported(import_locn_interface)
    ;
        Status2 = status_abstract_exported,
        Status = status_abstract_exported
    ).

:- pred combine_status_local(import_status::in, import_status::out) is semidet.

combine_status_local(status_exported_to_submodules,
    status_exported_to_submodules).
combine_status_local(status_imported(_),            status_local).
combine_status_local(status_local,                  status_local).
combine_status_local(status_exported,               status_exported).
combine_status_local(status_opt_imported,           status_local).
combine_status_local(status_abstract_imported,      status_local).
combine_status_local(status_abstract_exported,      status_abstract_exported).

:- pred combine_status_abstract_exported(import_status::in, import_status::out)
    is det.

combine_status_abstract_exported(Status2, Status) :-
    ( Status2 = status_exported ->
        Status = status_exported
    ;
        Status = status_abstract_exported
    ).

:- pred combine_status_abstract_imported(import_status::in, import_status::out)
    is det.

combine_status_abstract_imported(Status2, Status) :-
    ( Status2 = status_imported(Section) ->
        Status = status_imported(Section)
    ;
        Status = status_abstract_imported
    ).

:- pred convert_type_defn(type_defn::in, type_ctor::in, globals::in,
    hlds_type_body::out) is det.

convert_type_defn(parse_tree_du_type(Body, MaybeUserEqComp,
        MaybeDirectArgCtors), TypeCtor, Globals, HLDSBody) :-
    % Initially, when we first see the `:- type' definition,
    % we assign the constructor tags assuming that there is no
    % `:- pragma reserve_tag' declaration for this type.
    % (If it turns out that there was one, then we will recompute the
    % constructor tags by calling assign_constructor_tags again,
    % with ReservedTagPragma = uses_reserved_tag, when processing the pragma.)
    ReservedTagPragma = does_not_use_reserved_tag,
    assign_constructor_tags(Body, MaybeUserEqComp, TypeCtor, ReservedTagPragma,
        Globals, CtorTagMap, ReservedAddr, IsEnum),
    IsForeign = no,
    ( ReservedAddr = does_not_use_reserved_address ->
        compute_cheaper_tag_test(CtorTagMap, CheaperTagTest)
    ;
        CheaperTagTest = no_cheaper_tag_test
    ),
    HLDSBody = hlds_du_type(Body, CtorTagMap, CheaperTagTest, IsEnum,
        MaybeUserEqComp, MaybeDirectArgCtors, ReservedTagPragma, ReservedAddr,
        IsForeign).
convert_type_defn(parse_tree_eqv_type(Body), _, _, hlds_eqv_type(Body)).
convert_type_defn(parse_tree_solver_type(SolverTypeDetails, MaybeUserEqComp),
        _, _, hlds_solver_type(SolverTypeDetails, MaybeUserEqComp)).
convert_type_defn(parse_tree_abstract_type(Details), _, _,
        hlds_abstract_type(Details)).
convert_type_defn(parse_tree_foreign_type(ForeignType, MaybeUserEqComp,
        Assertions), _, _, hlds_foreign_type(Body)) :-
    (
        ForeignType = il(ILForeignType),
        Data = foreign_type_lang_data(ILForeignType, MaybeUserEqComp,
            Assertions),
        Body = foreign_type_body(yes(Data), no, no, no, no)
    ;
        ForeignType = c(CForeignType),
        Data = foreign_type_lang_data(CForeignType, MaybeUserEqComp,
            Assertions),
        Body = foreign_type_body(no, yes(Data), no, no, no)
    ;
        ForeignType = java(JavaForeignType),
        Data = foreign_type_lang_data(JavaForeignType, MaybeUserEqComp,
            Assertions),
        Body = foreign_type_body(no, no, yes(Data), no, no)
    ;
        ForeignType = csharp(CSharpForeignType),
        Data = foreign_type_lang_data(CSharpForeignType, MaybeUserEqComp,
            Assertions),
        Body = foreign_type_body(no, no, no, yes(Data), no)
    ;
        ForeignType = erlang(ErlangForeignType),
        Data = foreign_type_lang_data(ErlangForeignType, MaybeUserEqComp,
            Assertions),
        Body = foreign_type_body(no, no, no, no, yes(Data))
    ).

:- pred add_type_defn_ctors(list(constructor)::in, type_ctor::in,
    module_name::in, tvarset::in, list(type_param)::in, tvar_kind_map::in,
    need_qualifier::in, partial_qualifier_info::in, import_status::in,
    ctor_field_table::in, ctor_field_table::out,
    cons_table::in, cons_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_type_defn_ctors([], _, _, _, _, _, _, _, _,
        !FieldNameTable, !ConsTable, !Specs).
add_type_defn_ctors([Ctor | Ctors], TypeCtor, TypeCtorModuleName, TVarSet,
        TypeParams, KindMap, NeedQual, PQInfo, ImportStatus,
        !FieldNameTable, !ConsTable, !Specs) :-
    add_type_defn_ctor(Ctor, TypeCtor, TypeCtorModuleName, TVarSet,
        TypeParams, KindMap, NeedQual, PQInfo, ImportStatus,
        !FieldNameTable, !ConsTable, !Specs),
    add_type_defn_ctors(Ctors, TypeCtor, TypeCtorModuleName, TVarSet,
        TypeParams, KindMap, NeedQual, PQInfo, ImportStatus,
        !FieldNameTable, !ConsTable, !Specs).

:- pred add_type_defn_ctor(constructor::in, type_ctor::in,
    module_name::in, tvarset::in, list(type_param)::in, tvar_kind_map::in,
    need_qualifier::in, partial_qualifier_info::in, import_status::in,
    ctor_field_table::in, ctor_field_table::out,
    cons_table::in, cons_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_type_defn_ctor(Ctor, TypeCtor, TypeCtorModuleName, TVarSet,
        TypeParams, KindMap, NeedQual, PQInfo, ImportStatus,
        !FieldNameTable, !ConsTable, !Specs) :-
    Ctor = ctor(ExistQVars, Constraints, Name, Args, Arity, Context),
    BaseName = unqualify_name(Name),
    QualifiedName = qualified(TypeCtorModuleName, BaseName),
    UnqualifiedName = unqualified(BaseName),
    QualifiedConsIdA = cons(QualifiedName, Arity, TypeCtor),
    QualifiedConsIdB = cons(QualifiedName, Arity, cons_id_dummy_type_ctor),
    UnqualifiedConsIdA = cons(UnqualifiedName, Arity, TypeCtor),
    UnqualifiedConsIdB = cons(UnqualifiedName, Arity, cons_id_dummy_type_ctor),

    ConsDefn = hlds_cons_defn(TypeCtor, TVarSet, TypeParams, KindMap,
        ExistQVars, Constraints, Args, Context),
    get_partial_qualifiers(TypeCtorModuleName, PQInfo, PartialQuals),

    % Check that there is at most one definition of a given cons_id
    % in each type.
    (
        search_cons_table(!.ConsTable, QualifiedConsIdA, QualifiedConsDefnsA),
        some [OtherConsDefn] (
            list.member(OtherConsDefn, QualifiedConsDefnsA),
            OtherConsDefn ^ cons_type_ctor = TypeCtor
        )
    ->
        QualifiedConsIdStr = cons_id_and_arity_to_string(QualifiedConsIdA),
        TypeCtorStr = type_ctor_to_string(TypeCtor),
        Pieces = [words("Error: constructor"), quote(QualifiedConsIdStr),
            words("for type"), quote(TypeCtorStr),
            words("multiply defined."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        some [!OtherConsIds] (
            % Schedule the addition of the fully-qualified cons_id
            % into the cons_table.
            MainConsId = QualifiedConsIdA,
            !:OtherConsIds = [QualifiedConsIdB],

            % Schedule the addition of the unqualified version of the cons_id
            % to the cons_table, if appropriate.
            (
                NeedQual = may_be_unqualified,
                !:OtherConsIds =
                    [UnqualifiedConsIdA, UnqualifiedConsIdB | !.OtherConsIds]
            ;
                NeedQual = must_be_qualified
            ),

            % Schedule the partially qualified versions of the cons_id.
            list.foldl(add_ctor_to_list(TypeCtor, BaseName, Arity),
                PartialQuals, !OtherConsIds),

            % Do the scheduled additions.
            insert_into_cons_table(MainConsId, !.OtherConsIds, ConsDefn,
                !ConsTable)
        )
    ),

    FieldNames = list.map(func(C) = C ^ arg_field_name, Args),
    FirstField = 1,
    add_ctor_field_names(FieldNames, NeedQual, PartialQuals, TypeCtor,
        QualifiedConsIdA, ImportStatus, FirstField, !FieldNameTable, !Specs).

:- pred add_ctor_to_list(type_ctor::in, string::in, int::in, module_name::in,
    list(cons_id)::in, list(cons_id)::out) is det.

add_ctor_to_list(TypeCtor, ConsName, Arity, ModuleQual, !ConsIds) :-
    ConsIdA = cons(qualified(ModuleQual, ConsName), Arity, TypeCtor),
    ConsIdB = cons(qualified(ModuleQual, ConsName), Arity,
        cons_id_dummy_type_ctor),
    !:ConsIds = [ConsIdA, ConsIdB | !.ConsIds].

:- pred add_ctor_field_names(list(maybe(ctor_field_name))::in,
    need_qualifier::in, list(module_name)::in, type_ctor::in, cons_id::in,
    import_status::in, int::in,
    ctor_field_table::in, ctor_field_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_ctor_field_names([], _, _, _, _, _, _, !FieldNameTable, !Specs).
add_ctor_field_names([MaybeCtorFieldName | MaybeCtorFieldNames], NeedQual,
        PartialQuals, TypeCtor, ConsId, ImportStatus,
        FieldNumber, !FieldNameTable, !Specs) :-
    (
        MaybeCtorFieldName = yes(ctor_field_name(FieldName, FieldNameContext)),
        FieldDefn = hlds_ctor_field_defn(FieldNameContext, ImportStatus,
            TypeCtor, ConsId, FieldNumber),
        add_ctor_field_name(FieldName, FieldDefn, NeedQual, PartialQuals,
            !FieldNameTable, !Specs)
    ;
        MaybeCtorFieldName = no
    ),
    add_ctor_field_names(MaybeCtorFieldNames, NeedQual, PartialQuals, TypeCtor,
        ConsId, ImportStatus, FieldNumber + 1, !FieldNameTable, !Specs).

:- pred add_ctor_field_name(sym_name::in, hlds_ctor_field_defn::in,
    need_qualifier::in, list(module_name)::in,
    ctor_field_table::in, ctor_field_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_ctor_field_name(FieldName, FieldDefn, NeedQual, PartialQuals,
        !FieldNameTable, !Specs) :-
    (
        FieldName = qualified(FieldModule0, _),
        FieldModule = FieldModule0
    ;
        FieldName = unqualified(_),
        unexpected($module, $pred, "unqualified field name")
    ),
    % Field names must be unique within a module, not just within a type,
    % because the function names for user-defined override functions
    % for the builtin field access functions must be unique within a
    % module.
    ( map.search(!.FieldNameTable, FieldName, ConflictingDefns) ->
        ( ConflictingDefns = [ConflictingDefn] ->
            ConflictingDefn = hlds_ctor_field_defn(OrigContext, _, _, _, _)
        ;
            unexpected($module, $pred, "multiple conflicting fields")
        ),

        % XXX We should record each error.
        % using module_info_incr_errors
        FieldDefn = hlds_ctor_field_defn(Context, _, _, _, _),
        FieldString = sym_name_to_string(FieldName),
        Pieces = [words("Error: field"), quote(FieldString),
            words("multiply defined."), nl],
        HereMsg = simple_msg(Context, [always(Pieces)]),
        PrevPieces = [words("Here is the previous definition of field"),
            quote(FieldString), suffix("."), nl],
        PrevMsg = simple_msg(OrigContext, [always(PrevPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [HereMsg, PrevMsg]),
        !:Specs = [Spec | !.Specs]
    ;
        UnqualFieldName = unqualify_name(FieldName),

        % Add an unqualified version of the field name to the table,
        % if appropriate.
        (
            NeedQual = may_be_unqualified,
            multi_map.set(unqualified(UnqualFieldName), FieldDefn,
                !FieldNameTable)
        ;
            NeedQual = must_be_qualified
        ),

        % Add partially qualified versions of the cons_id
        list.foldl(do_add_ctor_field(UnqualFieldName, FieldDefn),
            [FieldModule | PartialQuals], !FieldNameTable)
    ).

:- pred do_add_ctor_field(string::in, hlds_ctor_field_defn::in,
    module_name::in, ctor_field_table::in, ctor_field_table::out) is det.

do_add_ctor_field(FieldName, FieldNameDefn, ModuleName, !FieldNameTable) :-
    multi_map.set(qualified(ModuleName, FieldName), FieldNameDefn,
        !FieldNameTable).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_type.
%----------------------------------------------------------------------------%
