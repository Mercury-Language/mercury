%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This submodule of make_hlds handles the declarations of new types.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_type.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % We allow more than one "definition" for a given type so
    % long all of them except one are actually just declarations,
    % e.g. `:- type t.', which is parsed as an type definition for
    % t which defines t as an abstract_type.
    %
:- pred module_add_type_defn(tvarset::in, sym_name::in, list(type_param)::in,
    type_defn::in, condition::in, prog_context::in, item_status::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

    % Add the constructors and special preds for a type to the HLDS.
    %
:- pred process_type_defn(type_ctor::in, hlds_type_defn::in,
    bool::in, bool::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

:- pred make_status_abstract(import_status::in, import_status::out) is det.

:- pred combine_status(import_status::in, import_status::in,
    import_status::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.type_util.
:- import_module hlds.make_hlds.add_special_pred.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_tags.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_out.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module pair.
:- import_module string.
:- import_module svmap.
:- import_module term.

%-----------------------------------------------------------------------------%

module_add_type_defn(TVarSet, Name, Args, TypeDefn, _Cond, Context,
        item_status(Status0, NeedQual), !ModuleInfo, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
    list.length(Args, Arity),
    TypeCtor = type_ctor(Name, Arity),
    convert_type_defn(TypeDefn, TypeCtor, Globals, Body0),
    module_info_get_type_table(!.ModuleInfo, Types0),
    (
        (
            Body0 = abstract_type(_)
        ;
            Body0 = du_type(_, _, _, _, _, _),
            string.suffix(term.context_file(Context), ".int2")
            % If the type definition comes from a .int2 file then
            % we need to treat it as abstract.  The constructors
            % may only be used by the mode system for comparing
            % `bound' insts to `ground'.
        )
    ->
        make_status_abstract(Status0, Status1)
    ;
        Status1 = Status0
    ),
    (
        % Discriminated unions whose definition consists of a single
        % zero-arity constructor are not allowed to have user-defined
        % equality or comparison.
        %
        TypeDefn = du_type(Ctors, MaybeUserUC),
        Ctors = [ Constructor ],
        list.length(Constructor ^ cons_args, 0),
        MaybeUserUC \= no,
        % Only report errors for types defined in this module.
        status_defined_in_this_module(Status0, yes)
    ->
        DummyTypeError = [
            words("Error: the type"),
            sym_name_and_arity(Name / Arity),
            words("is not allowed to have user-defined equality"),
            words("or comparison.")
        ],
        (
            VerboseErrors = yes,
            VerboseDummyTypeError = [
                words("Discriminated unions whose body consists of a single"),
                words("zero-arity constructor cannot have user-defined"),
                words("equality or comparison.")
            ],
            CompleteDummyTypeError = DummyTypeError ++ VerboseDummyTypeError
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO),
            CompleteDummyTypeError = DummyTypeError
        ),
        write_error_pieces(Context, 0, CompleteDummyTypeError, !IO),
        io.set_exit_status(1, !IO)
    ;
        true
    ),
    (
        % The type is exported if *any* occurrence is exported,
        % even a previous abstract occurrence.
        map.search(Types0, TypeCtor, OldDefn0)
    ->
        hlds_data.get_type_defn_status(OldDefn0, OldStatus),
        combine_status(Status1, OldStatus, Status),
        hlds_data.get_type_defn_body(OldDefn0, OldBody0),
        combine_is_solver_type(OldBody0, OldBody, Body0, Body),
        ( is_solver_type_is_inconsistent(OldBody, Body) ->
            % The existing definition has an is_solver_type annotation
            % which is different to the current definition.
            module_info_incr_errors(!ModuleInfo),
            Pieces0 = [words("In definition of type"),
                fixed(describe_sym_name_and_arity(Name / Arity) ++ ":"), nl,
                words("error: all definitions of a type must"),
                words("have consistent `solver'"),
                words("annotations")],
            write_error_pieces(Context, 0, Pieces0, !IO),
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
    % We set the kinds to `star'.  This will be different when we have a
    % kind system.
    map.init(KindMap),
    hlds_data.set_type_defn(TVarSet, Args, KindMap, Body, Status, no,
        NeedQual, Context, T),
    (
        MaybeOldDefn = no,
        Body = foreign_type(_)
    ->
        TypeStr = describe_sym_name_and_arity(Name / Arity),
        ErrorPieces = [
            words("Error: type "),
            fixed(TypeStr),
            words("defined as foreign_type without being declared.")
        ],
        write_error_pieces(Context, 0, ErrorPieces, !IO),
        module_info_incr_errors(!ModuleInfo)
    ;
        MaybeOldDefn = yes(OldDefn1),
        Body = foreign_type(_),
        hlds_data.get_type_defn_status(OldDefn1, OldStatus1),
        hlds_data.get_type_defn_body(OldDefn1, OldBody1),
        OldBody1 = abstract_type(_),
        status_is_exported_to_non_submodules(OldStatus1, no),
        status_is_exported_to_non_submodules(Status0, yes)
    ->
        TypeStr = describe_sym_name_and_arity(Name / Arity),
        ErrorPieces = [
            words("Error: pragma foreign_type "),
            fixed(TypeStr),
            words("must have the same visibility as the type declaration.")
        ],
        write_error_pieces(Context, 0, ErrorPieces, !IO),
        module_info_incr_errors(!ModuleInfo)
    ;
        % If there was an existing non-abstract definition for the type, ...
        MaybeOldDefn = yes(T2),
        hlds_data.get_type_defn_tvarset(T2, TVarSet_2),
        hlds_data.get_type_defn_tparams(T2, Params_2),
        hlds_data.get_type_defn_kind_map(T2, KindMap_2),
        hlds_data.get_type_defn_body(T2, Body_2),
        hlds_data.get_type_defn_context(T2, OrigContext),
        hlds_data.get_type_defn_status(T2, OrigStatus),
        hlds_data.get_type_defn_in_exported_eqv(T2, OrigInExportedEqv),
        hlds_data.get_type_defn_need_qualifier(T2, OrigNeedQual),
        Body_2 \= abstract_type(_)
    ->
        globals.io_get_target(Target, !IO),
        globals.io_lookup_bool_option(make_optimization_interface,
            MakeOptInt, !IO),
        ( Body = foreign_type(_) ->
            module_info_contains_foreign_type(!ModuleInfo)
        ;
            true
        ),
        (
            % ... then if this definition was abstract, ignore it
            % (but update the status of the old defn if necessary).
            Body = abstract_type(_)
        ->
            ( Status = OrigStatus ->
                true
            ;
                hlds_data.set_type_defn(TVarSet_2, Params_2, KindMap_2,
                    Body_2, Status, OrigInExportedEqv, OrigNeedQual,
                    OrigContext, T3),
                map.det_update(Types0, TypeCtor, T3, Types),
                module_info_set_type_table(Types, !ModuleInfo)
            )
        ;
            merge_foreign_type_bodies(Target, MakeOptInt, Body, Body_2,
                NewBody)
        ->
            ( check_foreign_type_visibility(OrigStatus, Status1) ->
                hlds_data.set_type_defn(TVarSet_2, Params_2, KindMap_2,
                    NewBody, Status, OrigInExportedEqv, NeedQual, Context, T3),
                map.det_update(Types0, TypeCtor, T3, Types),
                module_info_set_type_table(Types, !ModuleInfo)
            ;
                module_info_incr_errors(!ModuleInfo),
                Pieces = [words("In definition of type"),
                    fixed(describe_sym_name_and_arity(Name / Arity) ++ ":"),
                    nl,
                    words("error: all definitions of a"),
                    words("type must have the same"),
                    words("visibility")],
                write_error_pieces(Context, 0,
                    Pieces, !IO)
            )
        ;
            % ..., otherwise issue an error message if the second
            % definition wasn't read while reading .opt files.
            Status = opt_imported
        ->
            true
        ;
            module_info_incr_errors(!ModuleInfo),
            multiple_def_error(Status, Name, Arity, "type", Context,
                OrigContext, _, !IO)
        )
    ;
        map.set(Types0, TypeCtor, T, Types),
        module_info_set_type_table(Types, !ModuleInfo),
        (
            % XXX We can't handle abstract exported polymorphic equivalence
            % types with monomorphic bodies, because the compiler stuffs up
            % the type_info handling -- the caller passes type_infos,
            % but the callee expects no type_infos.
            Body = eqv_type(EqvType),
            Status = abstract_exported,
            list.member(Var, Args),
            \+ type_contains_var(EqvType, Var)
        ->
            Pieces = [words("Sorry, not implemented:"),
                words("polymorphic equivalence type,"),
                words("with monomorphic definition,"),
                words("exported as abstract type.")],
            write_error_pieces(Context, 0, Pieces, !IO),
            (
                VerboseErrors = yes,
                write_error_pieces(Context, 0, abstract_monotype_workaround,
                    !IO)
            ;
                VerboseErrors = no,
                globals.io_set_extra_error_info(yes, !IO)
            ),
            io.set_exit_status(1, !IO)
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

maybe_get_body_is_solver_type(abstract_type(IsSolverType), IsSolverType).
maybe_get_body_is_solver_type(solver_type(_, _), solver_type).

    % check_foreign_type_visibility(OldStatus, NewDefnStatus).
    %
    % Check that the visibility of the new definition for
    % a foreign type matches that of previous definitions.
    %
:- pred check_foreign_type_visibility(import_status::in,
    import_status::in) is semidet.

check_foreign_type_visibility(OldStatus, NewDefnStatus) :-
    ( OldStatus = abstract_exported  ->
        % If OldStatus is abstract_exported, the previous
        % definitions were local.
        status_is_exported_to_non_submodules(NewDefnStatus, no)
    ; OldStatus = exported ->
        NewDefnStatus = exported
    ;
        status_is_exported_to_non_submodules(OldStatus, no),
        status_is_exported_to_non_submodules(NewDefnStatus, no)
    ).

process_type_defn(TypeCtor, TypeDefn, !FoundError, !ModuleInfo, !IO) :-
    hlds_data.get_type_defn_context(TypeDefn, Context),
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, Args),
    hlds_data.get_type_defn_body(TypeDefn, Body),
    hlds_data.get_type_defn_status(TypeDefn, Status),
    hlds_data.get_type_defn_need_qualifier(TypeDefn, NeedQual),
    (
        ConsList = Body ^ du_type_ctors,
        UserEqCmp = Body ^ du_type_usereq,
        ReservedTag = Body ^ du_type_reserved_tag,
        module_info_get_cons_table(!.ModuleInfo, Ctors0),
        module_info_get_partial_qualifier_info(!.ModuleInfo, PQInfo),
        check_for_errors(
            (pred(M0::in, M::out, IO0::di, IO::uo) is det :-
                module_info_get_ctor_field_table(M0, CtorFields0),
                ctors_add(ConsList, TypeCtor, TVarSet, NeedQual, PQInfo,
                    Context, Status, CtorFields0, CtorFields, Ctors0, Ctors,
                    IO0, IO),
                module_info_set_cons_table(Ctors, M0, M1),
                module_info_set_ctor_field_table(CtorFields, M1, M)
        ), NewFoundError, !ModuleInfo, !IO),

        globals.io_get_globals(Globals, !IO),
        (
            type_with_constructors_should_be_no_tag(Globals, TypeCtor,
                ReservedTag, ConsList, UserEqCmp, Name, CtorArgType, _)
        ->
            NoTagType = no_tag_type(Args, Name, CtorArgType),
            module_info_get_no_tag_types(!.ModuleInfo, NoTagTypes0),
            map.set(NoTagTypes0, TypeCtor, NoTagType, NoTagTypes),
            module_info_set_no_tag_types(NoTagTypes, !ModuleInfo)
        ;
            true
        )
    ;
        Body = abstract_type(_),
        NewFoundError = no
    ;
        Body = solver_type(_, _),
        NewFoundError = no
    ;
        Body = eqv_type(_),
        NewFoundError = no
    ;
        Body = foreign_type(ForeignTypeBody),
        check_foreign_type(TypeCtor, ForeignTypeBody, Context,
            NewFoundError, !ModuleInfo, !IO)
    ),
    !:FoundError = !.FoundError `and` NewFoundError,
    (
        !.FoundError = yes
    ->
        true
    ;
        % Equivalence types are fully expanded on the IL and Java
        % backends, so the special predicates aren't required.
        are_equivalence_types_expanded(!.ModuleInfo),
        Body = eqv_type(_)
    ->
        true
    ;
        % XXX kind inference:
        % We set the kinds to `star'.  This will be different when we have
        % a kind system.
        prog_type.var_list_to_type_list(map.init, Args, ArgTypes),
        construct_type(TypeCtor, ArgTypes, Type),
        add_special_preds(TVarSet, Type, TypeCtor, Body, Context, Status,
            !ModuleInfo)
    ).

    % Check_foreign_type ensures that if we are generating code for
    % a specific backend that the foreign type has a representation
    % on that backend.
    %
:- pred check_foreign_type(type_ctor::in, foreign_type_body::in,
    prog_context::in, bool::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

check_foreign_type(TypeCtor, ForeignTypeBody, Context, FoundError, !ModuleInfo,
        !IO) :-
    TypeCtor = type_ctor(Name, Arity),
    module_info_get_globals(!.ModuleInfo, Globals),
    generating_code(GeneratingCode, !IO),
    globals.get_target(Globals, Target),
    ( have_foreign_type_for_backend(Target, ForeignTypeBody, yes) ->
        FoundError = no
    ; GeneratingCode = yes ->
        %
        % If we're not generating code the error may only have
        % occurred because the grade options weren't passed.
        %
        io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
        (
            VeryVerbose = yes,
            VerboseErrorPieces = [
                nl,
                words("There are representations for"),
                words("this type on other back-ends,"),
                words("but none for this back-end.")
            ]
        ;
            VeryVerbose = no,
            VerboseErrorPieces = []
        ),
        ( Target = target_c, LangStr = "C"
        ; Target = target_il, LangStr = "IL"
        ; Target = target_java, LangStr = "Java"
        ; Target = target_asm, LangStr = "C"
        ),
        TypeStr = describe_sym_name_and_arity(Name/Arity),
        ErrorPieces = [
            words("Error: no"), words(LangStr),
            words("`pragma foreign_type' declaration for"),
            fixed(TypeStr) | VerboseErrorPieces
        ],
        write_error_pieces(Context, 0, ErrorPieces, !IO),
        FoundError = yes,
        module_info_incr_errors(!ModuleInfo)
    ;
        FoundError = yes
    ).

    % Do the options imply that we will generate code for a specific
    % back-end?
    %
:- pred generating_code(bool::out, io::di, io::uo) is det.

generating_code(bool.not(NotGeneratingCode), !IO) :-
    io_lookup_bool_option(make_short_interface, MakeShortInterface, !IO),
    io_lookup_bool_option(make_interface, MakeInterface, !IO),
    io_lookup_bool_option(make_private_interface, MakePrivateInterface, !IO),
    io_lookup_bool_option(make_transitive_opt_interface,
        MakeTransOptInterface, !IO),
    io_lookup_bool_option(generate_source_file_mapping, GenSrcFileMapping,
        !IO),
    io_lookup_bool_option(generate_dependencies, GenDepends, !IO),
    io_lookup_bool_option(generate_dependency_file, GenDependFile, !IO),
    io_lookup_bool_option(convert_to_mercury, ConvertToMercury, !IO),
    io_lookup_bool_option(typecheck_only, TypeCheckOnly, !IO),
    io_lookup_bool_option(errorcheck_only, ErrorCheckOnly, !IO),
    io_lookup_bool_option(output_grade_string, OutputGradeString, !IO),
    bool.or_list([MakeShortInterface, MakeInterface,
        MakePrivateInterface, MakeTransOptInterface,
        GenSrcFileMapping, GenDepends, GenDependFile, ConvertToMercury,
        TypeCheckOnly, ErrorCheckOnly, OutputGradeString],
        NotGeneratingCode).

:- pred merge_foreign_type_bodies(compilation_target::in, bool::in,
    hlds_type_body::in, hlds_type_body::in, hlds_type_body::out)
    is semidet.

    % Ignore Mercury definitions if we've got a foreign type
    % declaration suitable for this back-end and we aren't making the
    % optimization interface.  We need to keep the Mercury definition
    % if we are making the optimization interface so that it gets
    % output in the .opt file.
    %
merge_foreign_type_bodies(Target, MakeOptInterface,
        foreign_type(ForeignTypeBody0), Body1, Body) :-
    MaybeForeignTypeBody1 = Body1 ^ du_type_is_foreign_type,
    (
        MaybeForeignTypeBody1 = yes(ForeignTypeBody1)
    ;
        MaybeForeignTypeBody1 = no,
        ForeignTypeBody1 = foreign_type_body(no, no, no)
    ),
    merge_foreign_type_bodies_2(ForeignTypeBody0, ForeignTypeBody1,
        ForeignTypeBody),
    (
        have_foreign_type_for_backend(Target, ForeignTypeBody, yes),
        MakeOptInterface = no
    ->
        Body = foreign_type(ForeignTypeBody)
    ;
        Body = Body1 ^ du_type_is_foreign_type := yes(ForeignTypeBody)
    ).
merge_foreign_type_bodies(Target, MakeOptInterface,
        Body0 @ du_type(_, _, _, _, _, _),
        Body1 @ foreign_type(_), Body) :-
    merge_foreign_type_bodies(Target, MakeOptInterface, Body1, Body0, Body).
merge_foreign_type_bodies(_, _, foreign_type(Body0),
        foreign_type(Body1),
        foreign_type(Body)) :-
    merge_foreign_type_bodies_2(Body0, Body1, Body).

:- pred merge_foreign_type_bodies_2(foreign_type_body::in,
    foreign_type_body::in, foreign_type_body::out) is semidet.

merge_foreign_type_bodies_2(foreign_type_body(MaybeILA, MaybeCA, MaybeJavaA),
        foreign_type_body(MaybeILB, MaybeCB, MaybeJavaB),
        foreign_type_body(MaybeIL, MaybeC, MaybeJava)) :-
    merge_maybe(MaybeILA, MaybeILB, MaybeIL),
    merge_maybe(MaybeCA, MaybeCB, MaybeC),
    merge_maybe(MaybeJavaA, MaybeJavaB, MaybeJava).

:- pred merge_maybe(maybe(T)::in, maybe(T)::in, maybe(T)::out) is semidet.

merge_maybe(no, no, no).
merge_maybe(yes(T), no, yes(T)).
merge_maybe(no, yes(T), yes(T)).

make_status_abstract(Status, AbstractStatus) :-
    ( Status = exported ->
        AbstractStatus = abstract_exported
    ; Status = imported(_) ->
        AbstractStatus = abstract_imported
    ;
        AbstractStatus = Status
    ).

combine_status(StatusA, StatusB, Status) :-
    ( combine_status_2(StatusA, StatusB, CombinedStatus) ->
        Status = CombinedStatus
    ;
        unexpected(this_file, "unexpected status for type definition")
    ).

:- pred combine_status_2(import_status::in, import_status::in,
    import_status::out) is semidet.

combine_status_2(imported(_), Status2, Status) :-
    combine_status_imported(Status2, Status).
combine_status_2(local, Status2, Status) :-
    combine_status_local(Status2, Status).
combine_status_2(exported, _Status2, exported).
combine_status_2(exported_to_submodules, Status2, Status) :-
    combine_status_local(Status2, Status3),
    ( Status3 = local ->
        Status = exported_to_submodules
    ;
        Status = Status3
    ).
combine_status_2(opt_imported, _Status2, opt_imported).
combine_status_2(abstract_imported, Status2, Status) :-
    combine_status_abstract_imported(Status2, Status).
combine_status_2(abstract_exported, Status2, Status) :-
    combine_status_abstract_exported(Status2, Status).

:- pred combine_status_imported(import_status::in, import_status::out)
    is semidet.

combine_status_imported(imported(Section),  imported(Section)).
combine_status_imported(local,              imported(implementation)).
combine_status_imported(exported,           exported).
combine_status_imported(opt_imported,       opt_imported).
combine_status_imported(abstract_imported,  imported(interface)).
combine_status_imported(abstract_exported,  abstract_exported).

:- pred combine_status_local(import_status::in, import_status::out) is semidet.

combine_status_local(exported_to_submodules, exported_to_submodules).
combine_status_local(imported(_),            local).
combine_status_local(local,                  local).
combine_status_local(exported,               exported).
combine_status_local(opt_imported,           local).
combine_status_local(abstract_imported,      local).
combine_status_local(abstract_exported,      abstract_exported).

:- pred combine_status_abstract_exported(import_status::in, import_status::out)
    is det.

combine_status_abstract_exported(Status2, Status) :-
    ( Status2 = exported ->
        Status = exported
    ;
        Status = abstract_exported
    ).

:- pred combine_status_abstract_imported(import_status::in, import_status::out)
    is det.

combine_status_abstract_imported(Status2, Status) :-
    ( Status2 = imported(Section) ->
        Status = imported(Section)
    ;
        Status = abstract_imported
    ).

:- pred convert_type_defn(type_defn::in, type_ctor::in, globals::in,
    hlds_type_body::out) is det.

convert_type_defn(du_type(Body, MaybeUserEqComp), TypeCtor, Globals,
        HLDSBody) :-
    % Initially, when we first see the `:- type' definition,
    % we assign the constructor tags assuming that there is no
    % `:- pragma reserve_tag' declaration for this type.
    % (If it turns out that there was one, then we will recompute the
    % constructor tags by calling assign_constructor_tags again,
    % with ReservedTagPragma = yes, when processing the pragma.)
    ReservedTagPragma = no,
    assign_constructor_tags(Body, MaybeUserEqComp, TypeCtor, ReservedTagPragma,
        Globals, CtorTags, IsEnum),
    IsForeign = no,
    HLDSBody = du_type(Body, CtorTags, IsEnum, MaybeUserEqComp,
        ReservedTagPragma, IsForeign).
convert_type_defn(eqv_type(Body), _, _, eqv_type(Body)).
convert_type_defn(solver_type(SolverTypeDetails, MaybeUserEqComp), _, _,
        solver_type(SolverTypeDetails, MaybeUserEqComp)).
convert_type_defn(abstract_type(IsSolverType), _, _,
        abstract_type(IsSolverType)).
convert_type_defn(foreign_type(ForeignType, MaybeUserEqComp, Assertions),
        _, _, foreign_type(Body)) :-
    (
        ForeignType = il(ILForeignType),
        Data = foreign_type_lang_data(ILForeignType, MaybeUserEqComp,
            Assertions),
        Body = foreign_type_body(yes(Data), no, no)
    ;
        ForeignType = c(CForeignType),
        Data = foreign_type_lang_data(CForeignType, MaybeUserEqComp,
            Assertions),
        Body = foreign_type_body(no, yes(Data), no)
    ;
        ForeignType = java(JavaForeignType),
        Data = foreign_type_lang_data(JavaForeignType, MaybeUserEqComp,
            Assertions),
        Body = foreign_type_body(no, no, yes(Data))
    ).

:- pred ctors_add(list(constructor)::in, type_ctor::in, tvarset::in,
    need_qualifier::in, partial_qualifier_info::in, prog_context::in,
    import_status::in, ctor_field_table::in, ctor_field_table::out,
    cons_table::in, cons_table::out, io::di, io::uo) is det.

ctors_add([], _, _, _, _, _, _, !FieldNameTable, !Ctors, !IO).
ctors_add([Ctor | Rest], TypeCtor, TVarSet, NeedQual, PQInfo, Context,
        ImportStatus, !FieldNameTable, !Ctors, !IO) :-
    Ctor = ctor(ExistQVars, Constraints, Name, Args),
    QualifiedConsId = make_cons_id(Name, Args, TypeCtor),
    ConsDefn = hlds_cons_defn(ExistQVars, Constraints, Args, TypeCtor,
        Context),
    %
    % Insert the fully-qualified version of this cons_id into the
    % cons_table.
    % Also check that there is at most one definition of a given
    % cons_id in each type.
    %
    ( map.search(!.Ctors, QualifiedConsId, QualifiedConsDefns0) ->
        QualifiedConsDefns1 = QualifiedConsDefns0
    ;
        QualifiedConsDefns1 = []
    ),
    (
        list.member(OtherConsDefn, QualifiedConsDefns1),
        OtherConsDefn = hlds_cons_defn(_, _, _, TypeCtor, _)
    ->
        % XXX we should record each error using module_info_incr_errors
        QualifiedConsIdStr = cons_id_to_string(QualifiedConsId),
        TypeCtorStr = type_ctor_to_string(TypeCtor),
        ErrMsg = [
            words("Error: constructor"),
            quote(QualifiedConsIdStr),
            words("for type"),
            quote(TypeCtorStr),
            words("multiply defined.")
        ],
        write_error_pieces(Context, 0, ErrMsg, !IO),
        io.set_exit_status(1, !IO),
        QualifiedConsDefns = QualifiedConsDefns1
    ;
        QualifiedConsDefns = [ConsDefn | QualifiedConsDefns1]
    ),
    svmap.set(QualifiedConsId, QualifiedConsDefns, !Ctors),

    ( QualifiedConsId = cons(qualified(Module, ConsName), Arity) ->
        % Add unqualified version of the cons_id to the
        % cons_table, if appropriate.
        ( NeedQual = may_be_unqualified ->
            UnqualifiedConsId = cons(unqualified(ConsName), Arity),
            multi_map.set(!.Ctors, UnqualifiedConsId, ConsDefn, !:Ctors)
        ;
            true
        ),

        % Add partially qualified versions of the cons_id
        get_partial_qualifiers(Module, PQInfo, PartialQuals),
        list.map_foldl(add_ctor(ConsName, Arity, ConsDefn),
            PartialQuals, _PartiallyQualifiedConsIds, !Ctors),

        assoc_list.keys(Args, FieldNames),
        FirstField = 1,

        add_ctor_field_names(FieldNames, NeedQual, PartialQuals, TypeCtor,
            QualifiedConsId, Context, ImportStatus, FirstField,
            !FieldNameTable, !IO)
    ;
        unexpected(this_file, "ctors_add: cons_id not qualified")
    ),
    ctors_add(Rest, TypeCtor, TVarSet, NeedQual, PQInfo, Context,
        ImportStatus, !FieldNameTable, !Ctors, !IO).

:- pred add_ctor(string::in, int::in, hlds_cons_defn::in, module_name::in,
    cons_id::out, cons_table::in, cons_table::out) is det.

add_ctor(ConsName, Arity, ConsDefn, ModuleQual, ConsId, CtorsIn, CtorsOut) :-
    ConsId = cons(qualified(ModuleQual, ConsName), Arity),
    multi_map.set(CtorsIn, ConsId, ConsDefn, CtorsOut).

:- pred add_ctor_field_names(list(maybe(ctor_field_name))::in,
    need_qualifier::in, list(module_name)::in, type_ctor::in, cons_id::in,
    prog_context::in, import_status::in, int::in,
    ctor_field_table::in, ctor_field_table::out, io::di, io::uo) is det.

add_ctor_field_names([], _, _, _, _, _, _, _, !FieldNameTable, !IO).
add_ctor_field_names([MaybeFieldName | FieldNames], NeedQual,
        PartialQuals, TypeCtor, ConsId, Context, ImportStatus,
        FieldNumber, !FieldNameTable, !IO) :-
    (
        MaybeFieldName = yes(FieldName),
        FieldDefn = hlds_ctor_field_defn(Context, ImportStatus, TypeCtor,
            ConsId, FieldNumber),
        add_ctor_field_name(FieldName, FieldDefn, NeedQual, PartialQuals,
            !FieldNameTable, !IO)
    ;
        MaybeFieldName = no
    ),
    add_ctor_field_names(FieldNames, NeedQual, PartialQuals, TypeCtor,
        ConsId, Context, ImportStatus, FieldNumber + 1,
        !FieldNameTable, !IO).

:- pred add_ctor_field_name(ctor_field_name::in, hlds_ctor_field_defn::in,
    need_qualifier::in, list(module_name)::in,
    ctor_field_table::in, ctor_field_table::out, io::di, io::uo) is det.

add_ctor_field_name(FieldName, FieldDefn, NeedQual, PartialQuals,
        !FieldNameTable, !IO) :-
    ( FieldName = qualified(FieldModule0, _) ->
        FieldModule = FieldModule0
    ;
        unexpected(this_file, "add_ctor_field_name: unqualified field name")
    ),
    (
        %
        % Field names must be unique within a module, not
        % just within a type because the function names for
        % user-defined override functions for the builtin field
        % access functions must be unique within a module.
        %
        map.search(!.FieldNameTable, FieldName, ConflictingDefns)
    ->
        ( ConflictingDefns = [ConflictingDefn] ->
            ConflictingDefn = hlds_ctor_field_defn(OrigContext, _, _, _, _)
        ;
            unexpected(this_file,
                "add_ctor_field_name: multiple conflicting fields")
        ),

        % XXX we should record each error
        % using module_info_incr_errors
        FieldDefn = hlds_ctor_field_defn(Context, _, _, _, _),
        sym_name_to_string(FieldName, FieldString),
        ErrorPieces = [words("Error: field"), quote(FieldString),
            words("multiply defined.")],
        write_error_pieces(Context, 0, ErrorPieces, !IO),

        PrevPieces = [words("Here is the previous definition of field"),
            quote(FieldString), suffix(".")],
        write_error_pieces_not_first_line(OrigContext, 0, PrevPieces, !IO),
        io.set_exit_status(1, !IO)
    ;
        unqualify_name(FieldName, UnqualFieldName),

        % Add an unqualified version of the field name to the
        % table, if appropriate.
        ( NeedQual = may_be_unqualified ->
            multi_map.set(!.FieldNameTable, unqualified(UnqualFieldName),
                FieldDefn, !:FieldNameTable)
        ;
            true
        ),

        % Add partially qualified versions of the cons_id
        list.foldl(do_add_ctor_field(UnqualFieldName, FieldDefn),
            [FieldModule | PartialQuals], !FieldNameTable)
    ).

:- pred do_add_ctor_field(string::in, hlds_ctor_field_defn::in,
    module_name::in, ctor_field_table::in, ctor_field_table::out) is det.

do_add_ctor_field(FieldName, FieldNameDefn, ModuleName, !FieldNameTable) :-
    multi_map.set(!.FieldNameTable, qualified(ModuleName, FieldName),
        FieldNameDefn, !:FieldNameTable).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "add_type.m".

%----------------------------------------------------------------------------%
