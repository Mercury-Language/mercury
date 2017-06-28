%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: add_type.m.
%
% This submodule of make_hlds handles the declarations of new types.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_type.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Add a declaration or definition of a type constructor.
    %
:- pred module_add_type_defn(type_status::in, need_qualifier::in,
    item_type_defn_info::in, module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Add the constructors and special preds for a type to the HLDS.
    %
:- pred process_type_defn(type_ctor::in, hlds_type_defn::in,
    found_invalid_type::in, found_invalid_type::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module hlds.make_hlds.add_special_pred.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_tags.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%
%
% The top level: adding the three kinds of item_type_defns (abstract,
% Mercury, foreign) to the HLDS.
%

module_add_type_defn(TypeStatus0, NeedQual, ItemTypeDefnInfo,
        !ModuleInfo, !FoundInvalidType, !Specs) :-
    ItemTypeDefnInfo = item_type_defn_info(SymName, TypeParams,
        ParseTreeTypeDefn, TVarSet, Context, _SeqNum),
    module_info_get_globals(!.ModuleInfo, Globals),
    list.length(TypeParams, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    convert_type_defn_to_hlds(ParseTreeTypeDefn, TypeCtor, Globals, Body),
    ( if
        (
            Body = hlds_abstract_type(_)
        ;
            Body = hlds_du_type(_, _, _, _, _, _, _, _, _),
            string.suffix(term.context_file(Context), ".int2")
            % If the type definition comes from a .int2 file then we must
            % treat it as abstract. The constructors may only be used
            % by the mode system for comparing `bound' insts to `ground'.
            % XXX This is NOT a robust record of the source; the context
            % could be lost for any number of reasons.
            % XXX STATUS The status should tell us.
        )
    then
        type_make_status_abstract(TypeStatus0, TypeStatus1)
    else
        TypeStatus1 = TypeStatus0
    ),
    % XXX kind inference:
    % We set the kinds to `star'. This will be different when we have a
    % kind system.
    map.init(KindMap),
    create_hlds_type_defn(TVarSet, TypeParams, KindMap, Body, no, TypeStatus1,
        NeedQual, type_defn_no_prev_errors, Context, HLDSTypeDefn0),

    % Our caller in make_hlds_passes.m ensures that we get called
    %
    % - first, for all abstract type declarations (the first switch arm),
    % - second, for all Mercury type definitions (the second switch arm),
    % - and last for all foreign type definitions (the third switch arm).

    (
        ParseTreeTypeDefn = parse_tree_abstract_type(_),
        module_add_type_defn_abstract(TypeStatus1, TypeCtor, Body,
            HLDSTypeDefn0, Context, !ModuleInfo, !FoundInvalidType, [], Specs)
    ;
        ( ParseTreeTypeDefn = parse_tree_du_type(_)
        ; ParseTreeTypeDefn = parse_tree_eqv_type(_)
        ; ParseTreeTypeDefn = parse_tree_solver_type(_)
        ),
        module_add_type_defn_mercury(TypeStatus1, TypeCtor, TypeParams,
            ParseTreeTypeDefn, Body, HLDSTypeDefn0, Context,
            !ModuleInfo, !FoundInvalidType, [], Specs)
    ;
        ParseTreeTypeDefn = parse_tree_foreign_type(_),
        module_add_type_defn_foreign(TypeStatus0, TypeStatus1, TypeCtor, Body,
            HLDSTypeDefn0, Context, !ModuleInfo, !FoundInvalidType, [], Specs)
    ),
    ( if contains_errors(Globals, Specs) = yes then
        module_info_incr_errors(!ModuleInfo)
    else
        true
    ),
    !:Specs = Specs ++ !.Specs.

%---------------------%

:- pred module_add_type_defn_abstract(type_status::in,
    type_ctor::in, hlds_type_body::in, hlds_type_defn::in,
    prog_context::in, module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_type_defn_abstract(TypeStatus1, TypeCtor, Body, TypeDefn0, Context,
        !ModuleInfo, !FoundInvalidType, !Specs) :-
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    ( if search_type_ctor_defn(TypeTable0, TypeCtor, OldDefn) then
        % Since make_hlds_passes.m adds all abstract definitions first,
        % the previous definition can only be another abstract definition.

        % Even if the source code includes only one declaration of a type,
        % augmenting a raw compilation unit can yield duplicates of that
        % declaration, included e.g. in both x.int2 and then x.int,
        % or in both x.int and x.opt.
        get_type_defn_context(OldDefn, OldContext),
        ( if
            string.suffix(term.context_file(Context), ".m"),
            string.suffix(term.context_file(OldContext), ".m")
        then
            TypeCtor = type_ctor(SymName, Arity),
            DupPieces = [words("Warning: duplicate declaration for type "),
                unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
                suffix("."), nl],
            DupMsg = simple_msg(Context, [always(DupPieces)]),
            OldPieces = [words("The previous declaration was here."), nl],
            OldMsg = simple_msg(OldContext, [always(OldPieces)]),
            DupSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [DupMsg, OldMsg]),
            !:Specs = [DupSpec | !.Specs]
            % This is only a warning, not an error.
            % XXX I (zs) think it should be an error, at least in situations
            % in which the two declarations have different statuses.
            % !:FoundInvalidType = found_invalid_type
        else
            true
        ),

        combine_old_and_new_type_status(OldDefn, TypeStatus1, _TypeStatus,
            TypeDefn0, TypeDefn),
        check_for_inconsistent_solver_nosolver_type(TypeCtor,
            OldDefn, Body, Context, !FoundInvalidType, !Specs),
        replace_type_ctor_defn(TypeCtor, TypeDefn, TypeTable0, TypeTable)
    else
        add_type_ctor_defn(TypeCtor, TypeDefn0, TypeTable0, TypeTable)
    ),
    module_info_set_type_table(TypeTable, !ModuleInfo).

%---------------------%

:- inst type_defn_mercury
    --->    parse_tree_du_type(ground)
    ;       parse_tree_eqv_type(ground)
    ;       parse_tree_solver_type(ground).

:- pred module_add_type_defn_mercury(type_status::in,
    type_ctor::in, list(type_param)::in, type_defn::in(type_defn_mercury),
    hlds_type_body::in, hlds_type_defn::in, prog_context::in,
    module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_type_defn_mercury(TypeStatus1, TypeCtor, TypeParams,
        ParseTreeTypeDefn, Body, TypeDefn0, Context,
        !ModuleInfo, !FoundInvalidType, !Specs) :-
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    ( if search_type_ctor_defn(TypeTable0, TypeCtor, OldDefn) then
        % Since make_hlds_passes.m adds all abstract definitions first
        % and then Mercury definitions, the previous definition can be
        % either an abstract definition or another Mercury definition.
        % The latter is an error.
        combine_old_and_new_type_status(OldDefn, TypeStatus1, TypeStatus,
            TypeDefn0, TypeDefn),
        check_for_inconsistent_solver_nosolver_type(TypeCtor, OldDefn,
            Body, Context, !FoundInvalidType, !Specs),
        ( if
            hlds_data.get_type_defn_body(OldDefn, OldDefnBody),
            OldDefnBody \= hlds_abstract_type(_)
        then
            maybe_report_multiple_def_error(TypeStatus, TypeCtor, Context,
                OldDefn, !ModuleInfo, !FoundInvalidType, !Specs)
        else
            replace_type_ctor_defn(TypeCtor, TypeDefn, TypeTable0, TypeTable),
            module_info_set_type_table(TypeTable, !ModuleInfo)
        )
    else
        TypeStatus = TypeStatus1,
        add_type_ctor_defn(TypeCtor, TypeDefn0, TypeTable0, TypeTable),
        module_info_set_type_table(TypeTable, !ModuleInfo)
    ),
    (
        ParseTreeTypeDefn = parse_tree_du_type(DetailsDu),
        check_for_dummy_type_with_unify_compare(TypeStatus, TypeCtor,
            DetailsDu, Context, !FoundInvalidType, !Specs)
    ;
        ParseTreeTypeDefn = parse_tree_eqv_type(DetailsEqv),
        check_for_polymorphic_eqv_type_with_monomorphic_body(TypeStatus,
            TypeCtor, TypeParams, DetailsEqv, Context,
            !FoundInvalidType, !Specs)
    ;
        ParseTreeTypeDefn = parse_tree_solver_type(_)
    ).

%---------------------%

:- pred module_add_type_defn_foreign(type_status::in, type_status::in,
    type_ctor::in, hlds_type_body::in, hlds_type_defn::in, prog_context::in,
    module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_type_defn_foreign(TypeStatus0, TypeStatus1, TypeCtor,
        Body, TypeDefn0, Context, !ModuleInfo, !FoundInvalidType, !Specs) :-
    TypeCtor = type_ctor(SymName, Arity),
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    ( if search_type_ctor_defn(TypeTable0, TypeCtor, OldDefn) then
        % Since make_hlds_passes.m adds all abstract definitions first,
        % then Mercury definitions, and only the foreign definitions,
        % the previous definition can be an abstract definition,
        % a Mercury definition (which may have had an abstract definition
        % before it), or either of those followed by a previously-added
        % foreign definition.

        combine_old_and_new_type_status(OldDefn, TypeStatus1, TypeStatus,
            TypeDefn0, TypeDefn1),
        check_for_inconsistent_solver_nosolver_type(TypeCtor,
            OldDefn, Body, Context, !FoundInvalidType, !Specs),

        hlds_data.get_type_defn_status(OldDefn, OldTypeStatus),
        hlds_data.get_type_defn_body(OldDefn, OldBody),
        ( if OldBody = hlds_abstract_type(_) then
            % This is the first actual definition (not an abstract declaration)
            % for this type.
            check_for_inconsistent_foreign_type_visibility(TypeCtor,
                old_defn_is_abstract, OldTypeStatus, TypeStatus0, Context,
                TypeDefn1, TypeDefn, !FoundInvalidType, !Specs),
            replace_type_ctor_defn(TypeCtor, TypeDefn,
                TypeTable0, TypeTable),
            module_info_set_type_table(TypeTable, !ModuleInfo)
        else
            % This is not the first non-abstract definition for this type.
            % The previous definition(s) was/were ....
            module_info_get_globals(!.ModuleInfo, Globals),
            ( if
                merge_maybe_foreign_type_bodies(Globals, OldBody, Body,
                    MergedBody)
            then
                % ... either compatible with this definition, ...
                set_type_defn_body(MergedBody, TypeDefn1, TypeDefn2),
                check_for_inconsistent_foreign_type_visibility(TypeCtor,
                    old_defn_is_not_abstract, OldTypeStatus, TypeStatus1,
                    Context, TypeDefn2, TypeDefn, !FoundInvalidType, !Specs),
                replace_type_ctor_defn(TypeCtor, TypeDefn,
                    TypeTable0, TypeTable),
                module_info_set_type_table(TypeTable, !ModuleInfo)
            else
                % ... or not.
                maybe_report_multiple_def_error(TypeStatus, TypeCtor, Context,
                    OldDefn, !ModuleInfo, !FoundInvalidType, !Specs)
            )
        )
    else
        ForeignDeclPieces = [words("Error: type "),
            unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
            words("defined as foreign_type without being declared."), nl],
        ForeignDeclMsg = simple_msg(Context, [always(ForeignDeclPieces)]),
        ForeignDeclSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [ForeignDeclMsg]),
        !:Specs = [ForeignDeclSpec | !.Specs],
        !:FoundInvalidType = found_invalid_type
    ).

%---------------------------------------------------------------------------%
%
% Predicates that help the top level predicates do their jobs.
%

:- pred convert_type_defn_to_hlds(type_defn::in, type_ctor::in, globals::in,
    hlds_type_body::out) is det.

convert_type_defn_to_hlds(TypeDefn, TypeCtor, Globals, HLDSBody) :-
    (
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu =
            type_details_du(Body, MaybeUserEqComp, MaybeDirectArgCtors),
        % Initially, when we first see the `:- type' definition,
        % we assign the constructor tags assuming that there is no
        % `:- pragma reserve_tag' declaration for this type.
        % (If it turns out that there was one, then we will recompute the
        % constructor tags by calling assign_constructor_tags again,
        % with ReservedTagPragma = uses_reserved_tag, when processing
        % the pragma.)
        ReservedTagPragma = does_not_use_reserved_tag,
        assign_constructor_tags(Body, MaybeUserEqComp, TypeCtor,
            ReservedTagPragma, Globals, CtorTagMap, ReservedAddr, IsEnum),
        IsForeign = no,
        ( if ReservedAddr = does_not_use_reserved_address then
            compute_cheaper_tag_test(CtorTagMap, CheaperTagTest)
        else
            CheaperTagTest = no_cheaper_tag_test
        ),
        HLDSBody = hlds_du_type(Body, CtorTagMap, CheaperTagTest, IsEnum,
            MaybeUserEqComp, MaybeDirectArgCtors,
            ReservedTagPragma, ReservedAddr, IsForeign)
    ;
        TypeDefn = parse_tree_eqv_type(type_details_eqv(EqvType)),
        HLDSBody = hlds_eqv_type(EqvType)
    ;
        TypeDefn = parse_tree_solver_type(DetailsSolver),
        HLDSBody = hlds_solver_type(DetailsSolver)
    ;
        TypeDefn = parse_tree_abstract_type(DetailsAbstract),
        HLDSBody = hlds_abstract_type(DetailsAbstract)
    ;
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(ForeignType, MaybeUserEqComp,
            Assertions),
        (
            ForeignType = c(CForeignType),
            Data = foreign_type_lang_data(CForeignType, MaybeUserEqComp,
                Assertions),
            Body = foreign_type_body(yes(Data), no, no, no)
        ;
            ForeignType = java(JavaForeignType),
            Data = foreign_type_lang_data(JavaForeignType, MaybeUserEqComp,
                Assertions),
            Body = foreign_type_body(no, yes(Data), no, no)
        ;
            ForeignType = csharp(CSharpForeignType),
            Data = foreign_type_lang_data(CSharpForeignType, MaybeUserEqComp,
                Assertions),
            Body = foreign_type_body(no, no, yes(Data), no)
        ;
            ForeignType = erlang(ErlangForeignType),
            Data = foreign_type_lang_data(ErlangForeignType, MaybeUserEqComp,
                Assertions),
            Body = foreign_type_body(no, no, no, yes(Data))
        ),
        HLDSBody = hlds_foreign_type(Body)
    ).

%---------------------%

    % Given the old HLDS definition of a type and the status of a new
    % item_type_defn we are adding to it, compute the status of the resulting
    % modified type definition, and put it into the updated version
    % of the given type definition.
    %
:- pred combine_old_and_new_type_status(hlds_type_defn::in, type_status::in,
    type_status::out, hlds_type_defn::in, hlds_type_defn::out) is det.

combine_old_and_new_type_status(OldDefn, NewTypeStatus, CombinedTypeStatus,
        !TypeDefn) :-
    % The type is exported if *any* occurrence is exported,
    % even a previous abstract occurrence.
    get_type_defn_status(OldDefn, OldTypeStatus),
    type_combine_status(NewTypeStatus, OldTypeStatus, CombinedTypeStatus),
    set_type_defn_status(CombinedTypeStatus, !TypeDefn).
    % XXX The use of type_combine_status here is making it difficult to do
    % sanity checks on the statuses of the various item_type_defns that
    % together contribute to a HLDS type definition. We should record
    % (a) the status of the abstract declaration, if any, and (b) the
    % statuses of the definition or definitions (there can be more than one,
    % for foreign types).
    %
    % It would then be simple to test whether
    %
    % - the statuses of all the actual definitions are the same; and
    % - the statuses of all the foreign definitions (if any) are the same
    %   as the status of the declaration.
    %
    % As it is, we are forced to use cumbersome code; see the code of
    % do_foreign_type_visibilities_match.

%---------------------%

    % Ignore Mercury definitions if we have a foreign type declaration
    % suitable for this back-end, and we aren't making the optimization
    % interface. We need to keep the Mercury definition if we are making
    % the optimization interface so that it gets output in the .opt file.
    %
:- pred merge_maybe_foreign_type_bodies(globals::in,
    hlds_type_body::in, hlds_type_body::in, hlds_type_body::out) is semidet.

merge_maybe_foreign_type_bodies(Globals, BodyA, BodyB, Body) :-
    (
        BodyA = hlds_foreign_type(ForeignTypeBodyA),
        BodyB = hlds_du_type(_, _, _, _, _, _, _, _, _),
        merge_foreign_and_du_type_bodies(Globals, ForeignTypeBodyA, BodyB,
            Body)
    ;
        BodyA = hlds_du_type(_, _, _, _, _, _, _, _, _),
        BodyB = hlds_foreign_type(ForeignTypeBodyB),
        merge_foreign_and_du_type_bodies(Globals, ForeignTypeBodyB, BodyA,
            Body)
    ;
        BodyA = hlds_foreign_type(ForeignTypeBodyA),
        BodyB = hlds_foreign_type(ForeignTypeBodyB),
        merge_foreign_type_bodies(ForeignTypeBodyA, ForeignTypeBodyB,
            ForeignTypeBody),
        Body = hlds_foreign_type(ForeignTypeBody)
    ).

:- inst hlds_type_body_du
    --->    hlds_du_type(ground, ground, ground, ground, ground,
                ground, ground, ground, ground).

:- pred merge_foreign_and_du_type_bodies(globals::in,
    foreign_type_body::in, hlds_type_body::in(hlds_type_body_du),
    hlds_type_body::out) is semidet.

merge_foreign_and_du_type_bodies(Globals, ForeignTypeBodyA, DuTypeBodyB,
        Body) :-
    DuTypeBodyB = hlds_du_type(_Ctors, _TagValues, _CheaperTagTest,
        _DuTypeKind, _UserEq, _DirectArgCtors, _ReservedTag, _ReservedAddr,
        MaybeForeignTypeBodyB),
    (
        MaybeForeignTypeBodyB = yes(ForeignTypeBodyB)
    ;
        MaybeForeignTypeBodyB = no,
        ForeignTypeBodyB = foreign_type_body(no, no, no, no)
    ),
    merge_foreign_type_bodies(ForeignTypeBodyA, ForeignTypeBodyB,
        ForeignTypeBody),
    globals.get_target(Globals, Target),
    globals.get_op_mode(Globals, OpMode),
    ( if
        have_foreign_type_for_backend(Target, ForeignTypeBody, yes),
        OpMode \= opm_top_args(opma_augment(opmau_make_opt_int))
    then
        Body = hlds_foreign_type(ForeignTypeBody)
    else
        Body = DuTypeBodyB ^ du_type_is_foreign_type := yes(ForeignTypeBody)
    ).

:- pred merge_foreign_type_bodies(foreign_type_body::in,
    foreign_type_body::in, foreign_type_body::out) is semidet.

merge_foreign_type_bodies(TypeBodyA, TypeBodyB, TypeBody) :-
    TypeBodyA = foreign_type_body(MaybeCA, MaybeJavaA, MaybeCSharpA,
        MaybeErlangA),
    TypeBodyB = foreign_type_body(MaybeCB, MaybeJavaB, MaybeCSharpB,
        MaybeErlangB),
    merge_maybe(MaybeCA, MaybeCB, MaybeC),
    merge_maybe(MaybeJavaA, MaybeJavaB, MaybeJava),
    merge_maybe(MaybeCSharpA, MaybeCSharpB, MaybeCSharp),
    merge_maybe(MaybeErlangA, MaybeErlangB, MaybeErlang),
    TypeBody = foreign_type_body(MaybeC, MaybeJava, MaybeCSharp,
        MaybeErlang).

:- pred merge_maybe(maybe(T)::in, maybe(T)::in, maybe(T)::out) is semidet.

merge_maybe(no, no, no).
merge_maybe(yes(T), no, yes(T)).
merge_maybe(no, yes(T), yes(T)).

%---------------------------------------------------------------------------%
%
% Predicates that check for errors and/or report them.
%

:- pred maybe_report_multiple_def_error(type_status::in, type_ctor::in,
    prog_context::in, hlds_type_defn::in, module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_report_multiple_def_error(TypeStatus, TypeCtor, Context, OldDefn,
        !ModuleInfo, !FoundInvalidType, !Specs) :-
    % Issue an error message if the second definition wasn't read
    % while reading .opt files.
    % XXX STATUS
    ( if TypeStatus = type_status(status_opt_imported) then
        true
    else
        TypeCtor = type_ctor(SymName, Arity),
        hlds_data.get_type_defn_context(OldDefn, OldContext),
        report_multiple_def_error(SymName, Arity, "type", Context, OldContext,
            [], !Specs),
        !:FoundInvalidType = found_invalid_type
    ).

%---------------------%

:- pred check_for_dummy_type_with_unify_compare(type_status::in,
    type_ctor::in, type_details_du::in, prog_context::in,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_dummy_type_with_unify_compare(TypeStatus, TypeCtor, DetailsDu,
        Context, !FoundInvalidType, !Specs) :-
    ( if
        % Discriminated unions whose definition consists of a single
        % zero-arity constructor are dummy types. Dummy types are not allowed
        % to have user-defined equality or comparison.

        DetailsDu = type_details_du(Ctors, MaybeUserUC, _MaybeDirectArg),
        Ctors = [Constructor],
        Constructor ^ cons_args = [],
        MaybeUserUC = yes(_),
        % Only report errors for types defined in this module.
        type_status_defined_in_this_module(TypeStatus) = yes
    then
        TypeCtor = type_ctor(SymName, Arity),
        DummyMainPieces = [words("Error: the type"),
            unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
            words("contains no information,"),
            words("and as such it is not allowed to have"),
            words("user-defined equality or comparison."), nl],
        DummyVerbosePieces = [words("Discriminated unions whose body"),
            words("consists of a single zero-arity constructor"),
            words("cannot have user-defined equality or comparison."), nl],
        DummyMsg = simple_msg(Context,
            [always(DummyMainPieces),
            verbose_only(verbose_once, DummyVerbosePieces)]),
        DummySpec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [DummyMsg]),
        !:Specs = [DummySpec | !.Specs],
        !:FoundInvalidType = found_invalid_type
    else
        true
    ).

%---------------------%

:- pred check_for_polymorphic_eqv_type_with_monomorphic_body(type_status::in,
    type_ctor::in, list(type_param)::in, type_details_eqv::in,
    prog_context::in, found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_polymorphic_eqv_type_with_monomorphic_body(TypeStatus, TypeCtor,
        TypeParams, DetailsEqv, Context, !FoundInvalidType, !Specs) :-
    DetailsEqv = type_details_eqv(EqvType),
    ( if
        % XXX We can't handle abstract exported polymorphic equivalence
        % types with monomorphic bodies, because the compiler stuffs up
        % the type_info handling -- the caller passes type_infos,
        % but the callee expects no type_infos.
        TypeStatus = type_status(status_abstract_exported),
        some [Var] (
            list.member(Var, TypeParams),
            not type_contains_var(EqvType, Var)
        )
    then
        TypeCtor = type_ctor(SymName, Arity),
        PolyEqvPieces = [words("Error: the type"),
            unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
            words("is a polymorphic equivalence type"),
            words("with a monomorphic definition."),
            words("The export of such types as abstract types"),
            words("is not yet implemented."), nl],
        PolyEqvMsg = simple_msg(Context,
            [always(PolyEqvPieces),
            verbose_only(verbose_once, abstract_monotype_workaround)]),
        PolyEqvSpec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [PolyEqvMsg]),
        !:Specs = [PolyEqvSpec | !.Specs],
        !:FoundInvalidType = found_invalid_type
    else
        true
    ).

:- func abstract_monotype_workaround = list(format_component).

abstract_monotype_workaround = [
    words("A quick workaround is to just export the type as a concrete type"),
    words("by putting the type definition in the interface section."),
    words("A better workaround is to use a ""wrapper"" type, with just one"),
    words("functor that has just one arg, instead of an equivalence type."),
    words("(There is no performance penalty for this -- the compiler will"),
    words("optimize the wrapper away.)")
    ].

%---------------------%

:- pred check_for_inconsistent_solver_nosolver_type(type_ctor::in,
    hlds_type_defn::in, hlds_type_body::in, prog_context::in,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_inconsistent_solver_nosolver_type(TypeCtor, OldDefn, Body, Context,
        !FoundInvalidType, !Specs) :-
    get_type_defn_body(OldDefn, OldBody),
    get_body_is_solver_type(OldBody, OldIsSolverType),
    get_body_is_solver_type(Body, IsSolverType),
    ( if OldIsSolverType = IsSolverType then
        true
    else
        TypeCtor = type_ctor(SymName, Arity),
        SNA = unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
        (
            IsSolverType = solver_type,
            ThisIsOrIsnt = "is a solver type",
            OldIsOrIsnt = "is not"
        ;
            IsSolverType = non_solver_type,
            ThisIsOrIsnt = "is not a solver type",
            OldIsOrIsnt = "is"
        ),
        ( if OldBody = hlds_abstract_type(_) then
            OldDeclOrDefn = "declaration"
        else
            OldDeclOrDefn = "definition"
        ),
        ( if Body = hlds_abstract_type(_) then
            ThisDeclOrDefn = "declaration"
        else
            ThisDeclOrDefn = "definition"
        ),
        MainPieces = [words("Error: this"), words(ThisDeclOrDefn),
            words("of type"), SNA, words(ThisIsOrIsnt), suffix(","),
            words("but its previous"), words(OldDeclOrDefn),
            words(OldIsOrIsnt), suffix("."), nl],
        OldPieces = [words("The previous"), words(OldDeclOrDefn),
            words("is here."), nl],
        MainMsg = simple_msg(Context, [always(MainPieces)]),
        get_type_defn_context(OldDefn, OldContext),
        OldMsg = simple_msg(OldContext, [always(OldPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [MainMsg, OldMsg]),
        !:Specs = [Spec | !.Specs],
        !:FoundInvalidType = found_invalid_type
    ).

:- pred get_body_is_solver_type(hlds_type_body::in, is_solver_type::out)
    is det.

get_body_is_solver_type(Body, IsSolverType) :-
    (
        Body = hlds_solver_type(_),
        IsSolverType = solver_type
    ;
        Body = hlds_abstract_type(Details),
        (
            Details = abstract_type_general,
            IsSolverType = non_solver_type
        ;
            Details = abstract_enum_type(_),
            IsSolverType = non_solver_type
        ;
            Details = abstract_solver_type,
            IsSolverType = solver_type
        )
    ;
        ( Body = hlds_du_type(_, _, _, _, _, _, _, _, _)
        ; Body = hlds_eqv_type(_)
        ; Body = hlds_foreign_type(_)
        ),
        IsSolverType = non_solver_type
    ).

%---------------------%

:- type old_defn_maybe_abstract
    --->    old_defn_is_abstract
    ;       old_defn_is_not_abstract.

:- pred check_for_inconsistent_foreign_type_visibility(type_ctor::in,
    old_defn_maybe_abstract::in, type_status::in, type_status::in,
    prog_context::in, hlds_type_defn::in, hlds_type_defn::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_inconsistent_foreign_type_visibility(TypeCtor, OldIsAbstract,
        OldStatus, NewStatus, Context, !TypeDefn, !FoundInvalidType, !Specs) :-
    ( if
        (
            OldIsAbstract = old_defn_is_abstract,
            type_status_is_exported_to_non_submodules(OldStatus) = no,
            type_status_is_exported_to_non_submodules(NewStatus) = yes
        ;
            OldIsAbstract = old_defn_is_not_abstract,
            not do_foreign_type_visibilities_match(OldStatus, NewStatus)
        )
    then
        TypeCtor = type_ctor(SymName, Arity),
        SNA = unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
        (
            OldIsAbstract = old_defn_is_abstract,
            Pieces = [words("Error: the definition of the foreign type"),
                SNA, words("must have the same visibility"),
                words("as its declaration."), nl]
        ;
            OldIsAbstract = old_defn_is_not_abstract,
            Pieces = [words("Error: all definitions of the foreign type"),
                SNA, words("must have the same visibility."), nl]
        ),
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs],
        !:FoundInvalidType = found_invalid_type,
        set_type_defn_prev_errors(type_defn_prev_errors, !TypeDefn)
    else
        true
    ).

    % do_foreign_type_visibilities_match(OldStatus, NewStatus):
    %
    % Check that the visibility of the new definition for a foreign type
    % matches that of previous definitions.
    %
:- pred do_foreign_type_visibilities_match(type_status::in, type_status::in)
    is semidet.

do_foreign_type_visibilities_match(OldStatus, NewStatus) :-
    ( if OldStatus = type_status(status_abstract_exported)  then
        % If OldStatus is abstract_exported, the previous definitions
        % were local.
        type_status_is_exported_to_non_submodules(NewStatus) = no
    else if OldStatus = type_status(status_exported) then
        NewStatus = type_status(status_exported)
    else
        type_status_is_exported_to_non_submodules(OldStatus) = no,
        type_status_is_exported_to_non_submodules(NewStatus) = no
    ).

%---------------------%

    % Check_foreign_type ensures that if we are generating code for a specific
    % backend that the foreign type has a representation on that backend.
    %
:- pred check_foreign_type_for_current_target(type_ctor::in,
    foreign_type_body::in, type_defn_prev_errors::in, prog_context::in,
    found_invalid_type::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_type_for_current_target(TypeCtor, ForeignTypeBody, PrevErrors,
        Context, FoundInvalidType, !ModuleInfo, !Specs) :-
    TypeCtor = type_ctor(Name, Arity),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    ( if have_foreign_type_for_backend(Target, ForeignTypeBody, yes) then
        FoundInvalidType = did_not_find_invalid_type
    else if PrevErrors = type_defn_prev_errors then
        % The error message being generated below may be misleading,
        % since the relevant foreign language definition of this type
        % may have been present, but in error.
        FoundInvalidType = found_invalid_type
    else
        ( Target = target_c, LangStr = "C"
        ; Target = target_csharp, LangStr = "C#"
        ; Target = target_java, LangStr = "Java"
        ; Target = target_erlang, LangStr = "Erlang"
        ),
        MainPieces = [words("Error: no"), fixed(LangStr),
            pragma_decl("foreign_type"), words("declaration for"),
            unqual_sym_name_and_arity(sym_name_arity(Name, Arity)),
            suffix("."), nl],
        VerbosePieces = [words("There are representations for this type"),
            words("on other back-ends, but none for this back-end."), nl],
        Msg = simple_msg(Context,
            [always(MainPieces), verbose_only(verbose_always, VerbosePieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [Msg]),
        !:Specs = [Spec | !.Specs],
        FoundInvalidType = found_invalid_type
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

process_type_defn(TypeCtor, TypeDefn, !FoundInvalidType, !ModuleInfo,
        !Specs) :-
    get_type_defn_context(TypeDefn, Context),
    get_type_defn_tvarset(TypeDefn, TVarSet),
    get_type_defn_tparams(TypeDefn, Args),
    get_type_defn_kind_map(TypeDefn, KindMap),
    get_type_defn_body(TypeDefn, Body),
    get_type_defn_status(TypeDefn, Status),
    get_type_defn_ctors_need_qualifier(TypeDefn, NeedQual),
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
        ( if
            type_ctor_should_be_notag(Globals, TypeCtor,
                ReservedTag, ConsList, UserEqCmp, CtorName, CtorArgType, _)
        then
            NoTagType = no_tag_type(Args, CtorName, CtorArgType),
            module_info_get_no_tag_types(!.ModuleInfo, NoTagTypes0),
            map.set(TypeCtor, NoTagType, NoTagTypes0, NoTagTypes),
            module_info_set_no_tag_types(NoTagTypes, !ModuleInfo)
        else
            true
        )
    ;
        Body = hlds_foreign_type(ForeignTypeBody),
        get_type_defn_prev_errors(TypeDefn, PrevErrors),
        check_foreign_type_for_current_target(TypeCtor, ForeignTypeBody,
            PrevErrors, Context, FoundInvalidTypeInForeignBody,
            !ModuleInfo, !Specs),
        (
            FoundInvalidTypeInForeignBody = found_invalid_type,
            !:FoundInvalidType = found_invalid_type
        ;
            FoundInvalidTypeInForeignBody = did_not_find_invalid_type
        )
    ;
        ( Body = hlds_abstract_type(_)
        ; Body = hlds_solver_type(_)
        ; Body = hlds_eqv_type(_)
        )
    ),
    (
        !.FoundInvalidType = found_invalid_type
        % If there is an error in this type definition, we may not
        % be able to add the special preds for it correctly.
        % Even if the errors occurred only in other types, adding the special
        % predicates for this error-free type wouldn't help us, because
        % the presence of an invalid type anywhere will prevent the compiler
        % from going on to process those special predicates. It won't even
        % look at them to find errors, such as user-defined unify and compare
        % predicates being not found or having the wrong signature.
        % XXX If this ever changes, we *should* add the special preds
        % for error-free type definitions regardless of the presence
        % of any previous type errors.
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

:- pred add_type_defn_ctors(list(constructor)::in, type_ctor::in,
    module_name::in, tvarset::in, list(type_param)::in, tvar_kind_map::in,
    need_qualifier::in, partial_qualifier_info::in, type_status::in,
    ctor_field_table::in, ctor_field_table::out,
    cons_table::in, cons_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_type_defn_ctors([], _, _, _, _, _, _, _, _,
        !FieldNameTable, !ConsTable, !Specs).
add_type_defn_ctors([Ctor | Ctors], TypeCtor, TypeCtorModuleName, TVarSet,
        TypeParams, KindMap, NeedQual, PQInfo, TypeStatus,
        !FieldNameTable, !ConsTable, !Specs) :-
    add_type_defn_ctor(Ctor, TypeCtor, TypeCtorModuleName, TVarSet,
        TypeParams, KindMap, NeedQual, PQInfo, TypeStatus,
        !FieldNameTable, !ConsTable, !Specs),
    add_type_defn_ctors(Ctors, TypeCtor, TypeCtorModuleName, TVarSet,
        TypeParams, KindMap, NeedQual, PQInfo, TypeStatus,
        !FieldNameTable, !ConsTable, !Specs).

:- pred add_type_defn_ctor(constructor::in, type_ctor::in,
    module_name::in, tvarset::in, list(type_param)::in, tvar_kind_map::in,
    need_qualifier::in, partial_qualifier_info::in, type_status::in,
    ctor_field_table::in, ctor_field_table::out,
    cons_table::in, cons_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_type_defn_ctor(Ctor, TypeCtor, TypeCtorModuleName, TVarSet,
        TypeParams, KindMap, NeedQual, PQInfo, TypeStatus,
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
    get_partial_qualifiers(mq_not_used_in_interface, TypeCtorModuleName,
        PQInfo, PartialQuals),

    % Check that there is at most one definition of a given cons_id
    % in each type.
    ( if
        search_cons_table(!.ConsTable, QualifiedConsIdA, QualifiedConsDefnsA),
        some [OtherConsDefn] (
            list.member(OtherConsDefn, QualifiedConsDefnsA),
            OtherConsDefn ^ cons_type_ctor = TypeCtor
        )
    then
        QualifiedConsIdStr = cons_id_and_arity_to_string(QualifiedConsIdA),
        TypeCtorStr = type_ctor_to_string(TypeCtor),
        Pieces = [words("Error: constructor"), quote(QualifiedConsIdStr),
            words("for type"), quote(TypeCtorStr),
            words("multiply defined."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
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
        QualifiedConsIdA, TypeStatus, FirstField, !FieldNameTable, !Specs).

:- pred add_ctor_to_list(type_ctor::in, string::in, int::in, module_name::in,
    list(cons_id)::in, list(cons_id)::out) is det.

add_ctor_to_list(TypeCtor, ConsName, Arity, ModuleQual, !ConsIds) :-
    ConsIdA = cons(qualified(ModuleQual, ConsName), Arity, TypeCtor),
    ConsIdB = cons(qualified(ModuleQual, ConsName), Arity,
        cons_id_dummy_type_ctor),
    !:ConsIds = [ConsIdA, ConsIdB | !.ConsIds].

:- pred add_ctor_field_names(list(maybe(ctor_field_name))::in,
    need_qualifier::in, list(module_name)::in, type_ctor::in, cons_id::in,
    type_status::in, int::in,
    ctor_field_table::in, ctor_field_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_ctor_field_names([], _, _, _, _, _, _, !FieldNameTable, !Specs).
add_ctor_field_names([MaybeCtorFieldName | MaybeCtorFieldNames], NeedQual,
        PartialQuals, TypeCtor, ConsId, TypeStatus,
        FieldNumber, !FieldNameTable, !Specs) :-
    (
        MaybeCtorFieldName = yes(ctor_field_name(FieldName, FieldNameContext)),
        FieldDefn = hlds_ctor_field_defn(FieldNameContext, TypeStatus,
            TypeCtor, ConsId, FieldNumber),
        add_ctor_field_name(FieldName, FieldDefn, NeedQual, PartialQuals,
            !FieldNameTable, !Specs)
    ;
        MaybeCtorFieldName = no
    ),
    add_ctor_field_names(MaybeCtorFieldNames, NeedQual, PartialQuals, TypeCtor,
        ConsId, TypeStatus, FieldNumber + 1, !FieldNameTable, !Specs).

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
    ( if map.search(!.FieldNameTable, FieldName, ConflictingDefns) then
        ( if ConflictingDefns = [ConflictingDefn] then
            ConflictingDefn = hlds_ctor_field_defn(OrigContext, _, _, _, _)
        else
            unexpected($module, $pred, "multiple conflicting fields")
        ),

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
    else
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

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_type.
%---------------------------------------------------------------------------%
