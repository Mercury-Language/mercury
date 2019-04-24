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
% XXX TYPE_REPN Consider whether any code in this module should be elsewhere.
% XXX TYPE_REPN Put the remaining predicates in a top down order.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_type.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
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

    % Add the constructors of du types to constructor table of the HLDS,
    % and check that Mercury types defined solely by foreign types
    % have a definition that works for the target backend.
    %
:- pred add_du_ctors_check_foreign_type_for_cur_backend(type_ctor::in,
    hlds_type_defn::in, found_invalid_type::in, found_invalid_type::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module hlds.hlds_cons.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds_error.
:- import_module libs.
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
:- import_module maybe.
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
    % XXX We should consider setting !:FoundInvalidType *only* for type
    % errors that can cause later compiler passes to either crash or to
    % report nonexistent problems. If the only effect of a type error
    % is to prevent the diagnosis of other errors, then we should leave
    % !.FoundInvalidType as it is, to let later compiler passes run
    % and try to find more errors.
    %
    % Errors that fall into this category include errors involving
    % inconsistent statuses.

    ItemTypeDefnInfo = item_type_defn_info(SymName, TypeParams,
        ParseTreeTypeDefn, TVarSet, Context, _SeqNum),
    module_info_get_globals(!.ModuleInfo, Globals),
    list.length(TypeParams, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    convert_type_defn_to_hlds(ParseTreeTypeDefn, TypeCtor, Body, !ModuleInfo),
    ( if
        (
            Body = hlds_abstract_type(_)
        ;
            Body = hlds_du_type(_, _, _, _),
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

        check_for_duplicate_type_declaration(TypeCtor, OldDefn, TypeStatus1,
            Context, !FoundInvalidType, !Specs),
        combine_old_and_new_type_status(OldDefn, TypeStatus1, _TypeStatus,
            TypeDefn0, TypeDefn),
        check_for_inconsistent_solver_nosolver_type(TypeCtor,
            OldDefn, Body, Context, !FoundInvalidType, !Specs),
        replace_type_ctor_defn(TypeCtor, TypeDefn, TypeTable0, TypeTable)
    else
        add_type_ctor_defn(TypeCtor, TypeDefn0, TypeTable0, TypeTable)
    ),
    module_info_set_type_table(TypeTable, !ModuleInfo).

:- pred check_for_duplicate_type_declaration(type_ctor::in, hlds_type_defn::in,
    type_status::in, prog_context::in,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_duplicate_type_declaration(TypeCtor, OldDefn, NewStatus, NewContext,
        !FoundInvalidType, !Specs) :-
    % Even if the source code includes only one declaration of a type,
    % augmenting a raw compilation unit can yield duplicates of that
    % declaration, included e.g. in both x.int2 and then x.int,
    % or in both x.int and x.opt.
    get_type_defn_context(OldDefn, OldContext),
    get_type_defn_status(OldDefn, OldStatus),
    ( if
        string.suffix(term.context_file(OldContext), ".m"),
        string.suffix(term.context_file(NewContext), ".m")
    then
        % The flattening of source item blocks by modules.m puts
        % all items in a given section together. Since the original
        % source code may have had the contents of the different sections
        % intermingled, this may change the relative order of items.
        % Put them back in the original order for this error message.
        compare(CmpRes, OldContext, NewContext),
        (
            ( CmpRes = (<)
            ; CmpRes = (=)
            ),
            FirstContext = OldContext,
            FirstStatus = OldStatus,
            SecondContext = NewContext,
            SecondStatus = NewStatus
        ;
            CmpRes = (>),
            FirstContext = NewContext,
            FirstStatus = NewStatus,
            SecondContext = OldContext,
            SecondStatus = OldStatus
        ),
        FirstIsExported =
            type_status_is_exported_to_non_submodules(FirstStatus),
        SecondIsExported =
            type_status_is_exported_to_non_submodules(SecondStatus),
        TypeCtor = type_ctor(SymName, Arity),
        SNA = unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
        ( if FirstIsExported = SecondIsExported then
            Severity = severity_warning,
            DupPieces = [words("Warning: duplicate declaration for type "),
                SNA, suffix("."), nl]
        else
            Severity = severity_error,
            !:FoundInvalidType = found_invalid_type,
            % XXX If there were not one but *two or more* previous
            % declarations for the type, then FirstStatus may not have come
            % from the previous declaration at FirstContext; it could have
            % come from a *different* previous declaration.
            % We can't avoid this possibility without keeping a *separate*
            % record of the context and type_status of every item_type_defn
            % for every type_ctor.
            (
                SecondIsExported = yes,
                DupPieces = [words("Error: This declaration for type "),
                    SNA, words("says it is exported, while"),
                    words("the previous declaration says it is private."), nl]
            ;
                SecondIsExported = no,
                DupPieces = [words("Error: This declaration for type "),
                    SNA, words("says it is private, while"),
                    words("the previous declaration says it is exported."), nl]
            )
        ),
        DupMsg = simple_msg(SecondContext, [always(DupPieces)]),
        FirstPieces = [words("The previous declaration was here."), nl],
        FirstMsg = simple_msg(FirstContext, [always(FirstPieces)]),
        DupSpec = error_spec(Severity, phase_parse_tree_to_hlds,
            [DupMsg, FirstMsg]),
        !:Specs = [DupSpec | !.Specs]
    else
        true
    ).

%---------------------%

:- inst type_defn_mercury for type_defn/0
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
        hlds_data.get_type_defn_context(OldDefn, OldContext),
        ( if OldBody = hlds_abstract_type(_) then
            % This is the first actual definition (not an abstract declaration)
            % for this type.
            check_for_inconsistent_foreign_type_visibility(TypeCtor,
                old_defn_is_abstract, OldTypeStatus, OldContext,
                TypeStatus0, Context, TypeDefn1, TypeDefn,
                !FoundInvalidType, !Specs),
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
                    old_defn_is_not_abstract, OldTypeStatus, OldContext,
                    TypeStatus1, Context, TypeDefn2, TypeDefn,
                    !FoundInvalidType, !Specs),
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

:- pred convert_type_defn_to_hlds(type_defn::in, type_ctor::in,
    hlds_type_body::out, module_info::in, module_info::out) is det.

convert_type_defn_to_hlds(TypeDefn, TypeCtor, HLDSBody, !ModuleInfo) :-
    (
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu =
            type_details_du(Ctors, MaybeUserEqComp, MaybeDirectArgCtors),
        MaybeRepn = no,
        MaybeForeign = no,
        HLDSBody = hlds_du_type(Ctors, MaybeUserEqComp, MaybeRepn,
            MaybeForeign),
        (
            MaybeDirectArgCtors = no
        ;
            MaybeDirectArgCtors = yes(DirectArgCtors),
            % In one test case (submodules/direct_arg_cycle1.m), we insert
            % the same value of DirectArgCtors into DirectArgMap0 *twice*.
            %
            % I (zs) don't know whether this is something that we should allow,
            % since one of those is from writing a "where direct_arg is"
            % clause in the *source* code of the program, even though
            % that syntax was intended to be used only in automatically
            % generated interface files.
            %
            % For now, I left the old behavior.
            % XXX TYPE_REPN Peter and I agree; we should disallow
            % "where direct_arg" clauses in type definitions.
            module_info_get_type_repn_dec(!.ModuleInfo, TypeRepnDec0),
            DirectArgMap0 = TypeRepnDec0 ^ trdd_direct_arg_map,
            ( if map.search(DirectArgMap0, TypeCtor, OldDirectArgCtors) then
                ( if DirectArgCtors = OldDirectArgCtors then
                    true
                else
                    unexpected($pred,
                        "different DirectArgCtors for same TypeCtor")
                )
            else
                map.det_insert(TypeCtor, DirectArgCtors,
                    DirectArgMap0, DirectArgMap),
                TypeRepnDec = TypeRepnDec0 ^ trdd_direct_arg_map
                    := DirectArgMap,
                module_info_set_type_repn_dec(TypeRepnDec, !ModuleInfo)
            )
        )
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
        BodyB = hlds_du_type(_, _, _, _),
        merge_foreign_and_du_type_bodies(Globals, ForeignTypeBodyA, BodyB,
            Body)
    ;
        BodyA = hlds_du_type(_, _, _, _),
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

:- inst hlds_type_body_du for hlds_type_body/0
    --->    hlds_du_type(ground, ground, ground, ground).

:- pred merge_foreign_and_du_type_bodies(globals::in,
    foreign_type_body::in, hlds_type_body::in(hlds_type_body_du),
    hlds_type_body::out) is semidet.

merge_foreign_and_du_type_bodies(Globals, ForeignTypeBodyA, DuTypeBodyB,
        Body) :-
    DuTypeBodyB = hlds_du_type(_Ctors, _MaybeUserEq, _MaybeRepn,
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
    TypeBody = foreign_type_body(MaybeC, MaybeJava, MaybeCSharp, MaybeErlang).

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

        DetailsDu = type_details_du(Ctors, MaybeCanonical, _MaybeDirectArg),
        Ctors = [Ctor],
        Ctor ^ cons_args = [],
        MaybeCanonical = noncanon(_),
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

check_for_inconsistent_solver_nosolver_type(TypeCtor, OldDefn, NewBody,
        NewContext, !FoundInvalidType, !Specs) :-
    get_type_defn_body(OldDefn, OldBody),
    get_body_is_solver_type(OldBody, OldIsSolverType),
    get_body_is_solver_type(NewBody, NewIsSolverType),
    ( if OldIsSolverType = NewIsSolverType then
        true
    else
        get_type_defn_context(OldDefn, OldContext),
        (
            NewIsSolverType = solver_type,
            ThisIsOrIsnt = "is a solver type",
            OldIsOrIsnt = "is not"
        ;
            NewIsSolverType = non_solver_type,
            ThisIsOrIsnt = "is not a solver type",
            OldIsOrIsnt = "is"
        ),
        ( if NewBody = hlds_abstract_type(_) then
            ThisDeclOrDefn = "this declaration",
            ( if OldBody = hlds_abstract_type(_) then
                % We add all declarations in their order in the source.
                OldDeclOrDefn = "previous declaration"
            else
                % We add all declarations before we add any definitions.
                unexpected($pred, "definition before declaration")
            )
        else
            ThisDeclOrDefn = "this definition",
            ( if OldBody = hlds_abstract_type(_) then
                OldDeclOrDefn = "declaration"
            else
                % We add some declarations OUT of their order in the source.
                OldContext = term.context(OldFileName, OldLineNumber),
                NewContext = term.context(NewFileName, NewLineNumber),
                ( if
                    % Did we do so in this case?
                    OldFileName = NewFileName,
                    OldLineNumber < NewLineNumber
                then
                    % No.
                    OldDeclOrDefn = "previous definition"
                else
                    % Yes, or we don't know.
                    OldDeclOrDefn = "other definition"
                )
            )
        ),
        TypeCtor = type_ctor(SymName, Arity),
        SNA = unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
        MainPieces = [words("Error:"), words(ThisDeclOrDefn),
            words("of type"), SNA, words(ThisIsOrIsnt), suffix(","),
            words("but its"), words(OldDeclOrDefn),
            words(OldIsOrIsnt), suffix("."), nl],
        OldPieces = [words("The"), words(OldDeclOrDefn), words("is here."),
            nl],
        MainMsg = simple_msg(NewContext, [always(MainPieces)]),
        OldMsg = simple_msg(OldContext, [always(OldPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [MainMsg, OldMsg]),
        !:Specs = [Spec | !.Specs],
        !:FoundInvalidType = found_invalid_type
    ).

:- pred get_body_is_solver_type(hlds_type_body::in, is_solver_type::out)
    is det.

get_body_is_solver_type(Body, IsSolverType) :-
    % Please keep in sync with type_body_is_solver_type in type_util.m.
    (
        Body = hlds_solver_type(_),
        IsSolverType = solver_type
    ;
        Body = hlds_abstract_type(Details),
        (
            Details = abstract_solver_type,
            IsSolverType = solver_type
        ;
            ( Details = abstract_type_general
            ; Details = abstract_dummy_type
            ; Details = abstract_notag_type
            ; Details = abstract_type_fits_in_n_bits(_)
            ),
            IsSolverType = non_solver_type
        )
    ;
        ( Body = hlds_du_type(_, _, _, _)
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
    old_defn_maybe_abstract::in, type_status::in, prog_context::in,
    type_status::in, prog_context::in,
    hlds_type_defn::in, hlds_type_defn::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_inconsistent_foreign_type_visibility(TypeCtor,
        OldIsAbstract, OldStatus, OldContext, NewStatus, NewContext,
        !TypeDefn, !FoundInvalidType, !Specs) :-
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
        % The flattening of source item blocks by modules.m puts
        % all items in a given section together. Since the original
        % source code may have had the contents of the different sections
        % intermingled, this may change the relative order of items.
        % Make sure we generate the error message for the context of
        % the item that came *second* in the original order.
        compare(CmpRes, OldContext, NewContext),
        (
            ( CmpRes = (<)
            ; CmpRes = (=)
            ),
            Context = NewContext
        ;
            CmpRes = (>),
            Context = OldContext
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
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs],
        FoundInvalidType = found_invalid_type
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

add_du_ctors_check_foreign_type_for_cur_backend(TypeCtor, TypeDefn,
        !FoundInvalidType, !ModuleInfo, !Specs) :-
    get_type_defn_context(TypeDefn, Context),
    get_type_defn_tvarset(TypeDefn, TVarSet),
    get_type_defn_tparams(TypeDefn, TypeParams),
    get_type_defn_kind_map(TypeDefn, KindMap),
    get_type_defn_body(TypeDefn, Body),
    get_type_defn_status(TypeDefn, Status),
    get_type_defn_ctors_need_qualifier(TypeDefn, NeedQual),
    (
        Body = hlds_du_type(ConsList, _MaybeUserEqCmp, _MaybeRepn,
            _MaybeForeign),
        module_info_get_cons_table(!.ModuleInfo, CtorMap0),
        module_info_get_partial_qualifier_info(!.ModuleInfo, PQInfo),
        module_info_get_ctor_field_table(!.ModuleInfo, CtorFieldMap0),
        TypeCtor = type_ctor(TypeCtorSymName, _),
        (
            TypeCtorSymName = unqualified(_),
            unexpected($pred, "unqualified TypeCtorSymName")
        ;
            TypeCtorSymName = qualified(TypeCtorModuleName, _)
        ),
        add_type_defn_ctors(ConsList, TypeCtor, TypeCtorModuleName, TVarSet,
            TypeParams, KindMap, NeedQual, PQInfo, Status,
            CtorFieldMap0, CtorFieldMap, CtorMap0, CtorMap, [], CtorAddSpecs),
        module_info_set_cons_table(CtorMap, !ModuleInfo),
        module_info_set_ctor_field_table(CtorFieldMap, !ModuleInfo),

        (
            CtorAddSpecs = []
        ;
            CtorAddSpecs = [_ | _],
            !:FoundInvalidType = found_invalid_type,
            !:Specs = CtorAddSpecs ++ !.Specs
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
    Ctor = ctor(_Ordinal, MaybeExistConstraints, Name, Args, Arity, Context),
    BaseName = unqualify_name(Name),
    QualifiedName = qualified(TypeCtorModuleName, BaseName),
    UnqualifiedName = unqualified(BaseName),
    QualifiedConsIdA = cons(QualifiedName, Arity, TypeCtor),
    QualifiedConsIdB = cons(QualifiedName, Arity, cons_id_dummy_type_ctor),
    UnqualifiedConsIdA = cons(UnqualifiedName, Arity, TypeCtor),
    UnqualifiedConsIdB = cons(UnqualifiedName, Arity, cons_id_dummy_type_ctor),

    ConsDefn = hlds_cons_defn(TypeCtor, TVarSet, TypeParams, KindMap,
        MaybeExistConstraints, Args, Context),
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
        unexpected($pred, "unqualified field name")
    ),
    % Field names must be unique within a module, not just within a type,
    % because the function names for user-defined override functions
    % for the builtin field access functions must be unique within a
    % module.
    ( if map.search(!.FieldNameTable, FieldName, ConflictingDefns) then
        ( if ConflictingDefns = [ConflictingDefn] then
            ConflictingDefn = hlds_ctor_field_defn(OrigContext, _, _, _, _)
        else
            unexpected($pred, "multiple conflicting fields")
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
