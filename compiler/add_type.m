%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2011 The University of Melbourne.
% Copyright (C) 2013-2021 The Mercury team.
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
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module hlds.status.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%

    % Add a declaration or definition of a type constructor.
    %
:- pred module_add_type_defn(type_status::in, need_qualifier::in,
    item_type_defn_info::in, module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Add the constructors of du types to the constructor table of the HLDS,
    % check subtype definitions, and check that Mercury types defined solely
    % by foreign types have a definition that works for the target backend.
    %
:- pred add_du_ctors_check_subtype_check_foreign_type(type_table::in,
    type_ctor::in, hlds_type_defn::in,
    found_invalid_type::in, found_invalid_type::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module hlds.hlds_cons.
:- import_module hlds.make_hlds_error.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module bool.
:- import_module edit_seq.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module one_or_more.
:- import_module require.
:- import_module string.
:- import_module term_context.

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
    list.length(TypeParams, Arity),
    TypeCtor = type_ctor(SymName, Arity),
    convert_type_defn_to_hlds(ParseTreeTypeDefn, TypeCtor, Body, !ModuleInfo),
    ( if
        (
            Body = hlds_abstract_type(_)
        ;
            Body = hlds_du_type(_),
            string.suffix(term_context.context_file(Context), ".int2")
            % If the type definition comes from a .int2 file then we must
            % treat it as abstract. The constructors may only be used
            % by the mode system for comparing `bound' insts to `ground'.
            % XXX This is NOT a robust record of the source; the context
            % could be lost for any number of reasons.
            % XXX STATUS The status should tell us.
        )
    then
        type_make_status_abstract(TypeStatus0, TypeStatus)
    else
        TypeStatus = TypeStatus0
    ),
    % XXX kind inference:
    % We set the kinds to `star'. This will be different when we have a
    % kind system.
    map.init(KindMap),
    create_hlds_type_defn(TVarSet, TypeParams, KindMap, Body, no, TypeStatus,
        NeedQual, type_defn_no_prev_errors, Context, HLDSTypeDefn0),

    % Our caller in make_hlds_passes.m ensures that we get called
    %
    % - first, for all abstract type declarations (the first switch arm),
    % - second, for all Mercury type definitions (the second switch arm),
    % - and last for all foreign type definitions (the third switch arm).

    (
        ParseTreeTypeDefn = parse_tree_abstract_type(_),
        module_add_type_defn_abstract(TypeStatus, TypeCtor, Body,
            HLDSTypeDefn0, Context, !ModuleInfo, !FoundInvalidType, [], Specs)
    ;
        ( ParseTreeTypeDefn = parse_tree_du_type(_)
        ; ParseTreeTypeDefn = parse_tree_sub_type(_)
        ; ParseTreeTypeDefn = parse_tree_eqv_type(_)
        ),
        module_add_type_defn_mercury(TypeStatus, TypeCtor, TypeParams,
            ParseTreeTypeDefn, Body, HLDSTypeDefn0, Context,
            !ModuleInfo, !FoundInvalidType, [], Specs)
    ;
        ParseTreeTypeDefn = parse_tree_solver_type(_),
        ( if
            type_status_defined_in_this_module(TypeStatus) = yes,
            type_status_defined_in_impl_section(TypeStatus) = no
        then
            SolverPieces = [words("Error: the definition"),
                words("(as opposed to the name) of a solver type such as"),
                unqual_type_ctor(TypeCtor),
                words("must not be exported from its defining module."), nl],
            SolverSpec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, SolverPieces),
            Specs0 = [SolverSpec]
        else
            Specs0 = []
        ),
        module_add_type_defn_mercury(TypeStatus, TypeCtor, TypeParams,
            ParseTreeTypeDefn, Body, HLDSTypeDefn0, Context,
            !ModuleInfo, !FoundInvalidType, Specs0, Specs)
    ;
        ParseTreeTypeDefn = parse_tree_foreign_type(_),
        module_add_type_defn_foreign(TypeStatus0, TypeStatus, TypeCtor, Body,
            HLDSTypeDefn0, Context, !ModuleInfo, !FoundInvalidType, [], Specs)
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
        string.suffix(term_context.context_file(OldContext), ".m"),
        string.suffix(term_context.context_file(NewContext), ".m")
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
        UTC = unqual_type_ctor(TypeCtor),
        ( if FirstIsExported = SecondIsExported then
            Severity = severity_warning,
            DupPieces = [words("Warning: duplicate declaration for type"),
                UTC, suffix("."), nl]
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
                DupPieces = [words("Error: This declaration for type"),
                    UTC, words("says it is exported, while"),
                    words("the previous declaration says it is private."), nl]
            ;
                SecondIsExported = no,
                DupPieces = [words("Error: This declaration for type"),
                    UTC, words("says it is private, while"),
                    words("the previous declaration says it is exported."), nl]
            )
        ),
        DupMsg = simplest_msg(SecondContext, DupPieces),
        FirstPieces = [words("The previous declaration was here."), nl],
        FirstMsg = simplest_msg(FirstContext, FirstPieces),
        DupSpec = error_spec($pred, Severity, phase_parse_tree_to_hlds,
            [DupMsg, FirstMsg]),
        !:Specs = [DupSpec | !.Specs]
    else
        true
    ).

%---------------------%

:- inst type_defn_mercury for type_defn/0
    --->    parse_tree_du_type(ground)
    ;       parse_tree_sub_type(ground)
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
            maybe_report_multiply_defined_type(TypeStatus, TypeCtor, Context,
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
        check_for_invalid_user_defined_unify_compare(TypeStatus,
            TypeCtor, DetailsDu, Context, !FoundInvalidType, !Specs)
    ;
        ParseTreeTypeDefn = parse_tree_eqv_type(DetailsEqv),
        check_for_polymorphic_eqv_type_with_monomorphic_body(TypeStatus,
            TypeCtor, TypeParams, DetailsEqv, Context,
            !FoundInvalidType, !Specs)
    ;
        ( ParseTreeTypeDefn = parse_tree_sub_type(_)
        ; ParseTreeTypeDefn = parse_tree_solver_type(_)
        )
    ).

%---------------------%

:- pred module_add_type_defn_foreign(type_status::in, type_status::in,
    type_ctor::in, hlds_type_body::in, hlds_type_defn::in, prog_context::in,
    module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_add_type_defn_foreign(TypeStatus0, TypeStatus1, TypeCtor,
        Body, TypeDefn0, Context, !ModuleInfo, !FoundInvalidType, !Specs) :-
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
                maybe_report_multiply_defined_type(TypeStatus, TypeCtor,
                    Context, OldDefn, !ModuleInfo, !FoundInvalidType, !Specs)
            )
        )
    else
        ForeignDeclPieces = [words("Error: type"), unqual_type_ctor(TypeCtor),
            words("defined as foreign_type without being declared."), nl],
        ForeignDeclSpec = simplest_spec($pred, severity_error,
            phase_parse_tree_to_hlds, Context, ForeignDeclPieces),
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
        DetailsDu = type_details_du(Ctors, MaybeCanon, MaybeDirectArgCtors),
        MaybeSubtype = not_a_subtype,
        MaybeRepn = maybe.no,
        MaybeForeign = maybe.no,
        TypeBodyDu = type_body_du(Ctors, MaybeSubtype, MaybeCanon, MaybeRepn,
            MaybeForeign),
        HLDSBody = hlds_du_type(TypeBodyDu),
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
        TypeDefn = parse_tree_sub_type(DetailsSub),
        DetailsSub = type_details_sub(SuperType, Ctors),
        MaybeSubtype = subtype_of(SuperType),
        MaybeCanon = canon,
        MaybeRepn = maybe.no,
        MaybeForeign = maybe.no,
        TypeBodyDu = type_body_du(Ctors, MaybeSubtype, MaybeCanon,
            MaybeRepn, MaybeForeign),
        HLDSBody = hlds_du_type(TypeBodyDu)
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
            Data = type_details_foreign(CForeignType, MaybeUserEqComp,
                Assertions),
            Body = foreign_type_body(yes(Data), no, no)
        ;
            ForeignType = java(JavaForeignType),
            Data = type_details_foreign(JavaForeignType, MaybeUserEqComp,
                Assertions),
            Body = foreign_type_body(no, yes(Data), no)
        ;
            ForeignType = csharp(CSharpForeignType),
            Data = type_details_foreign(CSharpForeignType, MaybeUserEqComp,
                Assertions),
            Body = foreign_type_body(no, no, yes(Data))
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
        BodyB = hlds_du_type(BodyDuB),
        merge_foreign_and_du_type_bodies(Globals, ForeignTypeBodyA, BodyDuB,
            Body)
    ;
        BodyA = hlds_du_type(BodyDuA),
        BodyB = hlds_foreign_type(ForeignTypeBodyB),
        merge_foreign_and_du_type_bodies(Globals, ForeignTypeBodyB, BodyDuA,
            Body)
    ;
        BodyA = hlds_foreign_type(ForeignTypeBodyA),
        BodyB = hlds_foreign_type(ForeignTypeBodyB),
        merge_foreign_type_bodies(ForeignTypeBodyA, ForeignTypeBodyB,
            ForeignTypeBody),
        Body = hlds_foreign_type(ForeignTypeBody)
    ).

:- pred merge_foreign_and_du_type_bodies(globals::in,
    foreign_type_body::in, type_body_du::in, hlds_type_body::out) is semidet.

merge_foreign_and_du_type_bodies(Globals, ForeignTypeBodyA, TypeBodyDuB,
        Body) :-
    TypeBodyDuB = type_body_du(_Ctors, MaybeSuperTypeB, _MaybeUserEq,
        _MaybeRepn, MaybeForeignTypeBodyB),
    MaybeSuperTypeB = not_a_subtype,
    (
        MaybeForeignTypeBodyB = yes(ForeignTypeBodyB)
    ;
        MaybeForeignTypeBodyB = no,
        ForeignTypeBodyB = foreign_type_body(no, no, no)
    ),
    merge_foreign_type_bodies(ForeignTypeBodyA, ForeignTypeBodyB,
        ForeignTypeBody),
    globals.get_target(Globals, Target),
    globals.get_op_mode(Globals, OpMode),
    ( if
        have_foreign_type_for_backend(Target, ForeignTypeBody, yes),
        OpMode \= opm_top_args(opma_augment(opmau_make_plain_opt))
    then
        Body = hlds_foreign_type(ForeignTypeBody)
    else
        TypeBodyDu = TypeBodyDuB ^ du_type_is_foreign_type
            := yes(ForeignTypeBody),
        Body = hlds_du_type(TypeBodyDu)
    ).

:- pred merge_foreign_type_bodies(foreign_type_body::in,
    foreign_type_body::in, foreign_type_body::out) is semidet.

merge_foreign_type_bodies(TypeBodyA, TypeBodyB, TypeBody) :-
    TypeBodyA = foreign_type_body(MaybeCA, MaybeJavaA, MaybeCSharpA),
    TypeBodyB = foreign_type_body(MaybeCB, MaybeJavaB, MaybeCSharpB),
    merge_maybe(MaybeCA, MaybeCB, MaybeC),
    merge_maybe(MaybeJavaA, MaybeJavaB, MaybeJava),
    merge_maybe(MaybeCSharpA, MaybeCSharpB, MaybeCSharp),
    TypeBody = foreign_type_body(MaybeC, MaybeJava, MaybeCSharp).

:- pred merge_maybe(maybe(T)::in, maybe(T)::in, maybe(T)::out) is semidet.

merge_maybe(no, no, no).
merge_maybe(yes(T), no, yes(T)).
merge_maybe(no, yes(T), yes(T)).

%---------------------------------------------------------------------------%
%
% Predicates that check for errors and/or report them.
%

:- pred maybe_report_multiply_defined_type(type_status::in, type_ctor::in,
    prog_context::in, hlds_type_defn::in, module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_report_multiply_defined_type(TypeStatus, TypeCtor, Context, OldDefn,
        !ModuleInfo, !FoundInvalidType, !Specs) :-
    % Issue an error message if the second definition wasn't read
    % while reading .opt files.
    % XXX STATUS
    ( if TypeStatus = type_status(status_opt_imported) then
        true
    else
        TypeCtor = type_ctor(SymName, Arity),
        hlds_data.get_type_defn_context(OldDefn, OldContext),
        report_multiply_defined("type", SymName, user_arity(Arity),
            Context, OldContext, [], !Specs),
        !:FoundInvalidType = found_invalid_type
    ).

%---------------------%

:- pred check_for_invalid_user_defined_unify_compare(type_status::in,
    type_ctor::in, type_details_du::in, prog_context::in,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_invalid_user_defined_unify_compare(TypeStatus, TypeCtor, DetailsDu,
        Context, !FoundInvalidType, !Specs) :-
    DetailsDu = type_details_du(Ctors, MaybeCanon, _MaybeDirectArg),
    ( if
        MaybeCanon = noncanon(_),
        % Only report errors for types defined in this module.
        type_status_defined_in_this_module(TypeStatus) = yes
    then
        ( if
            % Discriminated unions whose definition consists of a single
            % zero-arity constructor are dummy types. Dummy types are not
            % allowed to have user-defined equality or comparison.
            Ctors = one_or_more(Ctor, []),
            Ctor ^ cons_args = []
        then
            MainPieces = [words("Error: the type"),
                unqual_type_ctor(TypeCtor), words("contains no information,"),
                words("and as such it is not allowed to have"),
                words("user-defined equality or comparison."), nl],
            VerbosePieces = [words("Discriminated union types"),
                words("whose body consists of a single zero-arity constructor"),
                words("cannot have user-defined equality or comparison."), nl],
            DummyMsg = simple_msg(Context, [always(MainPieces),
                verbose_only(verbose_once, VerbosePieces)]),
            DummySpec = error_spec($pred, severity_error,
                phase_parse_tree_to_hlds, [DummyMsg]),
            !:Specs = [DummySpec | !.Specs],
            !:FoundInvalidType = found_invalid_type
        else
            true
        )
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
        PolyEqvPieces = [words("Error: the type"), unqual_type_ctor(TypeCtor),
            words("is a polymorphic equivalence type"),
            words("with a monomorphic definition."),
            words("The export of such types as abstract types"),
            words("is not yet implemented."), nl],
        PolyEqvMsg = simple_msg(Context,
            [always(PolyEqvPieces),
            verbose_only(verbose_once, abstract_monotype_workaround)]),
        PolyEqvSpec = error_spec($pred, severity_error,
            phase_parse_tree_to_hlds, [PolyEqvMsg]),
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
                OldContext = term_context.context(OldFileName, OldLineNumber),
                NewContext = term_context.context(NewFileName, NewLineNumber),
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
        MainPieces = [words("Error:"), words(ThisDeclOrDefn),
            words("of type"), unqual_type_ctor(TypeCtor),
            words(ThisIsOrIsnt), suffix(","),
            words("but its"), words(OldDeclOrDefn),
            words(OldIsOrIsnt), suffix("."), nl],
        OldPieces = [words("The"), words(OldDeclOrDefn), words("is here."),
            nl],
        MainMsg = simplest_msg(NewContext, MainPieces),
        OldMsg = simplest_msg(OldContext, OldPieces),
        Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
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
            ; Details = abstract_subtype(_)
            ),
            IsSolverType = non_solver_type
        )
    ;
        ( Body = hlds_du_type(_)
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
        UTC = unqual_type_ctor(TypeCtor),
        (
            OldIsAbstract = old_defn_is_abstract,
            Pieces = [words("Error: the definition of the foreign type"),
                UTC, words("must have the same visibility"),
                words("as its declaration."), nl]
        ;
            OldIsAbstract = old_defn_is_not_abstract,
            Pieces = [words("Error: all definitions of the foreign type"),
                UTC, words("must have the same visibility."), nl]
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
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

add_du_ctors_check_subtype_check_foreign_type(TypeTable, TypeCtor, TypeDefn,
        !FoundInvalidType, !ModuleInfo, !Specs) :-
    get_type_defn_context(TypeDefn, Context),
    get_type_defn_tvarset(TypeDefn, TVarSet),
    get_type_defn_tparams(TypeDefn, TypeParams),
    get_type_defn_kind_map(TypeDefn, KindMap),
    get_type_defn_body(TypeDefn, Body),
    get_type_defn_status(TypeDefn, Status),
    get_type_defn_ctors_need_qualifier(TypeDefn, NeedQual),
    (
        Body = hlds_du_type(BodyDu),
        BodyDu = type_body_du(OoMCtors, MaybeSuperType, _MaybeUserEqCmp,
            _MaybeRepn, _MaybeForeign),

        % Check subtype conditions if this is a subtype definitions.
        % There is no particular reason to do this here except to
        % save a pass over the type table.
        (
            MaybeSuperType = subtype_of(SuperType),
            check_subtype_defn(TypeTable, TVarSet, TypeCtor, TypeDefn, BodyDu,
                SuperType, MaybeSetSubtypeNoncanon, !FoundInvalidType, !Specs),
            (
                MaybeSetSubtypeNoncanon = do_not_set_subtype_noncanon
            ;
                MaybeSetSubtypeNoncanon = set_subtype_noncanon,
                % Set noncanonical flag on subtype definition if the base type
                % is noncanonical.
                NoncanonBodyDu = BodyDu ^ du_type_canonical :=
                    noncanon(noncanon_subtype),
                NoncanonBody = hlds_du_type(NoncanonBodyDu),
                set_type_defn_body(NoncanonBody, TypeDefn, NoncanonTypeDefn),
                module_info_get_type_table(!.ModuleInfo, TypeTable0),
                replace_type_ctor_defn(TypeCtor, NoncanonTypeDefn,
                    TypeTable0, TypeTable1),
                module_info_set_type_table(TypeTable1, !ModuleInfo)
            )
        ;
            MaybeSuperType = not_a_subtype
        ),

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
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        add_type_defn_ctor(HeadCtor, TypeCtor, TypeCtorModuleName,
            TVarSet, TypeParams, KindMap, NeedQual, PQInfo, Status,
            CtorFieldMap0, CtorFieldMap1, CtorMap0, CtorMap1,
            [], CtorAddSpecs1),
        add_type_defn_ctors(TailCtors, TypeCtor, TypeCtorModuleName,
            TVarSet, TypeParams, KindMap, NeedQual, PQInfo, Status,
            CtorFieldMap1, CtorFieldMap, CtorMap1, CtorMap,
            CtorAddSpecs1, CtorAddSpecs),
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
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        some [!OtherConsIds] (
            % Schedule the addition of the fully qualified cons_id
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
    % Field names must be unique within a type.
    ( if
        map.search(!.FieldNameTable, FieldName, ExistingDefns),
        list.find_first_match(is_conflicting_field_defn(FieldDefn),
            ExistingDefns, _ConflictingDefn)
    then
        % check_type_inst_mode_defns has already generated an error message
        % for this.
        %
        % ConflictingDefn = hlds_ctor_field_defn(OrigContext, _, _, _, _),
        % FieldDefn = hlds_ctor_field_defn(Context, _, _, _, _),
        % FieldString = sym_name_to_string(FieldName),
        % Pieces = [words("Error: field"), quote(FieldString),
        %     words("multiply defined."), nl],
        % HereMsg = simplest_msg(Context, Pieces),
        % PrevPieces = [words("Here is the previous definition of field"),
        %     quote(FieldString), suffix("."), nl],
        % PrevMsg = simplest_msg(OrigContext, PrevPieces),
        % Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
        %     [HereMsg, PrevMsg]),
        % !:Specs = [Spec | !.Specs]
        true
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

:- pred is_conflicting_field_defn(hlds_ctor_field_defn::in,
    hlds_ctor_field_defn::in) is semidet.

is_conflicting_field_defn(FieldDefnA, FieldDefnB) :-
    FieldDefnA = hlds_ctor_field_defn(_ContextA, _TypeStatusA, TypeCtor,
        _ConsIdA, _FieldNumberA),
    FieldDefnB = hlds_ctor_field_defn(_ContextB, _TypeStatusB, TypeCtor,
        _ConsIdB, _FieldNumberB).

:- pred do_add_ctor_field(string::in, hlds_ctor_field_defn::in,
    module_name::in, ctor_field_table::in, ctor_field_table::out) is det.

do_add_ctor_field(FieldName, FieldNameDefn, ModuleName, !FieldNameTable) :-
    multi_map.set(qualified(ModuleName, FieldName), FieldNameDefn,
        !FieldNameTable).

%---------------------------------------------------------------------------%

    % check_foreign_type_for_current_target checks whether a foreign type
    % has a representation for the backend we are generating code for.
    % If it does not, we generate an error message.
    %
:- pred check_foreign_type_for_current_target(type_ctor::in,
    foreign_type_body::in, type_defn_prev_errors::in, prog_context::in,
    found_invalid_type::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_type_for_current_target(TypeCtor, ForeignTypeBody, PrevErrors,
        Context, FoundInvalidType, !ModuleInfo, !Specs) :-
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
        LangStr = compilation_target_string(Target),
        MainPieces = [words("Error: the type"), unqual_type_ctor(TypeCtor),
            words("has no definition that is valid when targeting"),
            fixed(LangStr), suffix(";"),
            words("neither a Mercury definition,"),
            words("nor a"), pragma_decl("foreign_type"), words("declaration"),
            words("for"), fixed(LangStr), suffix("."), nl],
        VerbosePieces = [words("There are representations for this type"),
            words("on other back-ends, but none for this back-end."), nl],
        Msg = simple_msg(Context,
            [always(MainPieces), verbose_only(verbose_always, VerbosePieces)]),
        Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
            [Msg]),
        !:Specs = [Spec | !.Specs],
        FoundInvalidType = found_invalid_type
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type maybe_set_subtype_noncanonical
    --->    do_not_set_subtype_noncanon
    ;       set_subtype_noncanon.

:- pred check_subtype_defn(type_table::in, tvarset::in, type_ctor::in,
    hlds_type_defn::in, type_body_du::in, mer_type::in,
    maybe_set_subtype_noncanonical::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_subtype_defn(TypeTable, TVarSet, TypeCtor, TypeDefn, TypeBodyDu,
        SuperType, MaybeSetSubtypeNoncanon, !FoundInvalidType, !Specs) :-
    ( if type_to_ctor_and_args(SuperType, SuperTypeCtor, SuperTypeArgs) then
        search_super_type_ctor_defn(TypeTable, TypeCtor, TypeDefn,
            SuperTypeCtor, [], SearchResult),
        (
            SearchResult = ok2(SuperTypeDefn, SuperTypeBodyDu),
            check_supertypes_up_to_base_type(TypeTable, TypeCtor, TypeDefn,
                SuperTypeCtor, SuperTypeDefn, SuperTypeBodyDu,
                [], MaybeBaseMaybeCanon),
            (
                MaybeBaseMaybeCanon = ok1(BaseMaybeCanon),
                (
                    BaseMaybeCanon = canon,
                    MaybeSetSubtypeNoncanon = do_not_set_subtype_noncanon
                ;
                    BaseMaybeCanon = noncanon(_),
                    MaybeSetSubtypeNoncanon = set_subtype_noncanon
                ),
                check_subtype_ctors(TypeTable, TypeCtor, TypeDefn, TypeBodyDu,
                    SuperTypeCtor, SuperTypeDefn, SuperTypeBodyDu,
                    SuperTypeArgs, !FoundInvalidType, !Specs)
            ;
                MaybeBaseMaybeCanon = error1(UpToBaseSpecs),
                !:Specs = UpToBaseSpecs ++ !.Specs,
                !:FoundInvalidType = found_invalid_type,
                MaybeSetSubtypeNoncanon = do_not_set_subtype_noncanon
            )
        ;
            SearchResult = error2(SearchSpecs),
            !:Specs = SearchSpecs ++ !.Specs,
            !:FoundInvalidType = found_invalid_type,
            MaybeSetSubtypeNoncanon = do_not_set_subtype_noncanon
        )
    else
        SuperTypeStr = mercury_type_to_string(TVarSet, print_name_only,
            SuperType),
        Pieces = [words("Error: expected type constructor in"),
            words("supertype part of subtype definition, got"),
            quote(SuperTypeStr), suffix("."), nl],
        hlds_data.get_type_defn_context(TypeDefn, Context),
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs],
        !:FoundInvalidType = found_invalid_type,
        MaybeSetSubtypeNoncanon = do_not_set_subtype_noncanon
    ).

%---------------------%

:- pred check_supertypes_up_to_base_type(type_table::in,
    type_ctor::in, hlds_type_defn::in,
    type_ctor::in, hlds_type_defn::in, type_body_du::in,
    list(type_ctor)::in, maybe1(maybe_canonical)::out) is det.

check_supertypes_up_to_base_type(TypeTable, OrigTypeCtor, OrigTypeDefn,
        CurSuperTypeCtor, CurSuperTypeDefn, CurSuperTypeBodyDu,
        PrevSuperTypeCtors0, MaybeBaseMaybeCanon) :-
    CurSuperTypeBodyDu = type_body_du(_, MaybeNextSuperType, MaybeCanon, _, _),
    (
        MaybeNextSuperType = not_a_subtype,
        MaybeBaseMaybeCanon = ok1(MaybeCanon)
    ;
        MaybeNextSuperType = subtype_of(NextSuperType),
        ( if type_to_ctor(NextSuperType, NextSuperTypeCtor) then
            PrevSuperTypeCtors1 = [CurSuperTypeCtor | PrevSuperTypeCtors0],
            search_super_type_ctor_defn(TypeTable, OrigTypeCtor, OrigTypeDefn,
                NextSuperTypeCtor, PrevSuperTypeCtors1, SearchResult),
            (
                SearchResult = ok2(NextSuperTypeDefn, NextSuperTypeBodyDu),
                check_supertypes_up_to_base_type(TypeTable,
                    OrigTypeCtor, OrigTypeDefn,
                    NextSuperTypeCtor, NextSuperTypeDefn, NextSuperTypeBodyDu,
                    PrevSuperTypeCtors1, MaybeBaseMaybeCanon)
            ;
                SearchResult = error2(SearchSpecs),
                MaybeBaseMaybeCanon = error1(SearchSpecs)
            )
        else
            hlds_data.get_type_defn_tvarset(CurSuperTypeDefn, TVarSet),
            PrevSuperTypeCtors1 = [CurSuperTypeCtor | PrevSuperTypeCtors0],
            Pieces = report_non_du_supertype(TVarSet, OrigTypeCtor,
                PrevSuperTypeCtors1, NextSuperType),
            get_type_defn_context(OrigTypeDefn, OrigTypeContext),
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, OrigTypeContext, Pieces),
            MaybeBaseMaybeCanon = error1([Spec])
        )
    ).

%---------------------%

:- type search_type_ctor_defn_error
    --->    supertype_is_abstract
    ;       supertype_is_not_defined
    ;       circularity_detected.

:- pred search_super_type_ctor_defn(type_table::in, type_ctor::in,
    hlds_type_defn::in, type_ctor::in,
    list(type_ctor)::in, maybe2(hlds_type_defn, type_body_du)::out) is det.

search_super_type_ctor_defn(TypeTable, OrigTypeCtor, OrigTypeDefn,
        SuperTypeCtor, PrevSuperTypeCtors, MaybeSuperTypeDefn) :-
    ( if
        ( SuperTypeCtor = OrigTypeCtor
        ; list.contains(PrevSuperTypeCtors, SuperTypeCtor)
        )
    then
        Spec = supertype_ctor_defn_error_to_spec(OrigTypeCtor, OrigTypeDefn,
            PrevSuperTypeCtors, SuperTypeCtor, circularity_detected),
        MaybeSuperTypeDefn = error2([Spec])
    else
        ( if
            search_type_ctor_defn(TypeTable, SuperTypeCtor, SuperTypeDefn)
        then
            hlds_data.get_type_defn_status(OrigTypeDefn, OrigTypeStatus),
            hlds_data.get_type_defn_status(SuperTypeDefn, SuperTypeStatus),
            ( if
                subtype_defn_int_supertype_defn_impl(OrigTypeStatus,
                    SuperTypeStatus)
            then
                Spec = supertype_ctor_defn_error_to_spec(OrigTypeCtor,
                    OrigTypeDefn, PrevSuperTypeCtors, SuperTypeCtor,
                    supertype_is_abstract),
                MaybeSuperTypeDefn = error2([Spec])
            else
                check_supertype_is_du_not_foreign(OrigTypeDefn,
                    SuperTypeCtor, SuperTypeDefn, MaybeSuperTypeBodyDu),
                (
                    MaybeSuperTypeBodyDu = ok1(SuperTypeBodyDu),
                    MaybeSuperTypeDefn = ok2(SuperTypeDefn, SuperTypeBodyDu)
                ;
                    MaybeSuperTypeBodyDu = error1(SuperSpecs),
                    MaybeSuperTypeDefn = error2(SuperSpecs)
                )
            )
        else
            Spec = supertype_ctor_defn_error_to_spec(OrigTypeCtor,
                OrigTypeDefn, PrevSuperTypeCtors, SuperTypeCtor,
                supertype_is_not_defined),
            MaybeSuperTypeDefn = error2([Spec])
        )
    ).

:- pred subtype_defn_int_supertype_defn_impl(type_status::in, type_status::in)
    is semidet.

subtype_defn_int_supertype_defn_impl(SubTypeStatus, SuperTypeStatus) :-
    % If the subtype is defined in the interface section of this module,
    % then its supertype(s) must not be defined in the implementation section,
    % i.e. abstractly exported. Other visibility rules are enforced during
    % module qualification.
    type_status_defined_in_this_module(SubTypeStatus) = yes,
    type_status_defined_in_impl_section(SubTypeStatus) = no,

    type_status_defined_in_this_module(SuperTypeStatus) = yes,
    type_status_defined_in_impl_section(SuperTypeStatus) = yes.

:- pred check_supertype_is_du_not_foreign(hlds_type_defn::in,
    type_ctor::in, hlds_type_defn::in, maybe1(type_body_du)::out) is det.

check_supertype_is_du_not_foreign(TypeDefn, SuperTypeCtor, SuperTypeDefn,
        MaybeSuperTypeBodyDu) :-
    hlds_data.get_type_defn_body(SuperTypeDefn, SuperTypeBody),
    (
        SuperTypeBody = hlds_du_type(SuperTypeBodyDu),
        SuperTypeBodyDu = type_body_du(_, _, _, _, IsForeign),
        (
            IsForeign = no,
            MaybeSuperTypeBodyDu = ok1(SuperTypeBodyDu)
        ;
            IsForeign = yes(_),
            Pieces = [words("Error:"), unqual_type_ctor(SuperTypeCtor),
                words("cannot be a supertype"),
                words("because it has a foreign type definition."), nl],
            hlds_data.get_type_defn_context(TypeDefn, Context),
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            MaybeSuperTypeBodyDu = error1([Spec])
        )
    ;
        (
            SuperTypeBody = hlds_eqv_type(_),
            SuperTypeDesc = "an equivalence type"
        ;
            SuperTypeBody = hlds_foreign_type(_),
            SuperTypeDesc = "a foreign type"
        ;
            SuperTypeBody = hlds_solver_type(_),
            SuperTypeDesc = "a solver type"
        ;
            SuperTypeBody = hlds_abstract_type(_),
            SuperTypeDesc = "an abstract type"
        ),
        Pieces = [words("Error:"), unqual_type_ctor(SuperTypeCtor),
            words("cannot be a supertype because it is"),
            words(SuperTypeDesc), suffix("."), nl],
        hlds_data.get_type_defn_context(TypeDefn, Context),
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        MaybeSuperTypeBodyDu = error1([Spec])
    ).

:- func supertype_ctor_defn_error_to_spec(type_ctor, hlds_type_defn,
    list(type_ctor), type_ctor, search_type_ctor_defn_error) = error_spec.

supertype_ctor_defn_error_to_spec(OrigTypeCtor, OrigTypeDefn,
        PrevSuperTypeCtors, LastSuperTypeCtor, Error) = Spec :-
    (
        Error = supertype_is_abstract,
        Pieces = [words("Error: the type definition for")] ++
            describe_supertype_chain(OrigTypeCtor, PrevSuperTypeCtors,
                LastSuperTypeCtor) ++
            [suffix(","), nl, words("is not visible here."), nl]
    ;
        Error = supertype_is_not_defined,
        ( if special_type_ctor_not_du(LastSuperTypeCtor) then
            Pieces = [words("Error:")] ++
                describe_supertype_chain(OrigTypeCtor, PrevSuperTypeCtors,
                    LastSuperTypeCtor) ++
                [suffix(","), nl,
                words("is not a discriminated union type."), nl]
        else
            Pieces = [words("Error: the type")] ++
                describe_supertype_chain(OrigTypeCtor, PrevSuperTypeCtors,
                    LastSuperTypeCtor) ++
                [suffix(","), nl, words("is not defined."), nl]
        )
    ;
        Error = circularity_detected,
        Pieces = [words("Error: circularity in subtype definition chain."), nl,
            words("The chain is:"), nl] ++
            describe_supertype_chain(OrigTypeCtor, PrevSuperTypeCtors,
                LastSuperTypeCtor) ++
            [suffix("."), nl]
    ),
    hlds_data.get_type_defn_context(OrigTypeDefn, OrigTypeContext),
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        OrigTypeContext, Pieces).

:- pred special_type_ctor_not_du(type_ctor::in) is semidet.

special_type_ctor_not_du(TypeCtor) :-
    % XXX This could use classify_type_ctor_if_special but that predicate is
    % currently in check_hlds.
    (
        TypeCtor = type_ctor(SymName, Arity),
        Arity = 0,
        (
            SymName = unqualified(TypeName)
        ;
            SymName = qualified(mercury_public_builtin_module, TypeName)
        ),
        is_builtin_type_name(TypeName)
    ;
        type_ctor_is_higher_order(TypeCtor, _, _, _)
    ;
        type_ctor_is_tuple(TypeCtor)
    ).

%---------------------%

:- func report_non_du_supertype(tvarset, type_ctor,
    list(type_ctor), mer_type) = list(format_component).

report_non_du_supertype(TVarSet, OrigTypeCtor, PrevSuperTypeCtors1,
        NextSuperType) = Pieces :-
    NextSuperTypeStr = mercury_type_to_string(TVarSet,
        print_name_only, NextSuperType),
    Pieces = [words("Error:"), quote(NextSuperTypeStr)] ++
        describe_which_is_supertype_of_chain(is_first,
            OrigTypeCtor, PrevSuperTypeCtors1) ++
        [suffix(","), nl, words("is not a discriminated union type."), nl].

%---------------------%

:- func describe_supertype_chain(type_ctor, list(type_ctor), type_ctor)
    = list(format_component).

describe_supertype_chain(OrigTypeCtor, PrevSuperTypeCtors, LastSuperTypeCtor)
        = Pieces :-
    Pieces = [unqual_type_ctor(LastSuperTypeCtor), suffix(","), nl] ++
        describe_which_is_supertype_of_chain(is_first, OrigTypeCtor,
            PrevSuperTypeCtors).

:- type maybe_first
    --->    is_not_first
    ;       is_first.

:- func describe_which_is_supertype_of_chain(maybe_first, type_ctor,
    list(type_ctor)) = list(format_component).

describe_which_is_supertype_of_chain(First, OrigTypeCtor, SuperTypeCtors)
        = Pieces :-
    ( First = is_first,     WhichIsPieces = []
    ; First = is_not_first, WhichIsPieces = [words("which is")]
    ),
    (
        SuperTypeCtors = [],
        Pieces = WhichIsPieces ++ [words("the declared super type of"),
            unqual_type_ctor(OrigTypeCtor)]
    ;
        SuperTypeCtors = [HeadSuperTypeCtor | TailSuperTypeCtors],
        Pieces = WhichIsPieces ++ [words("the declared super type of"),
            unqual_type_ctor(HeadSuperTypeCtor), suffix(","), nl] ++
            describe_which_is_supertype_of_chain(is_not_first, OrigTypeCtor,
                TailSuperTypeCtors)
    ).

%---------------------%

:- pred check_subtype_ctors(type_table::in,
    type_ctor::in, hlds_type_defn::in, type_body_du::in,
    type_ctor::in, hlds_type_defn::in, type_body_du::in,
    list(mer_type)::in, found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_subtype_ctors(TypeTable, TypeCtor, TypeDefn, TypeBodyDu,
        SuperTypeCtor, SuperTypeDefn, SuperTypeBodyDu, SuperTypeArgs,
        !FoundInvalidType, !Specs) :-
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet0),
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    hlds_data.get_type_defn_tvarset(SuperTypeDefn, SuperTVarSet),
    hlds_data.get_type_defn_tparams(SuperTypeDefn, SuperTypeParams0),

    % Merge type variables in the subtype and supertype definitions into a
    % common tvarset.
    tvarset_merge_renaming(TVarSet0, SuperTVarSet, NewTVarSet, Renaming),
    apply_variable_renaming_to_tvar_list(Renaming, SuperTypeParams0,
        SuperTypeParams),

    % Create a substitution from the supertype's type parameters to the
    % argument types in the declared supertype part of the subtype definition.
    map.from_corresponding_lists(SuperTypeParams, SuperTypeArgs, TSubst),

    % Apply the type substitution to the supertype constructors' arguments.
    SuperTypeBodyDu = type_body_du(OoMSuperCtors, _, _, _, _),
    SuperCtors0 = one_or_more_to_list(OoMSuperCtors),
    list.map(rename_and_rec_subst_in_constructor(Renaming, TSubst),
        SuperCtors0, SuperCtors),

    % Check each subtype constructor against the supertype's constructors.
    TypeBodyDu = type_body_du(OoMCtors, _, _, _, _),
    Ctors = one_or_more_to_list(OoMCtors),
    list.foldl2(
        look_up_and_check_subtype_ctor(TypeTable, NewTVarSet, TypeStatus,
            SuperTypeCtor, SuperCtors),
        Ctors, !FoundInvalidType, !Specs),

    % Check order of subtype constructors relative to supertype constructors.
    hlds_data.get_type_defn_context(TypeDefn, Context),
    check_subtype_ctors_order(TypeCtor, Ctors, SuperTypeCtor, SuperCtors,
        Context, !Specs).

:- pred look_up_and_check_subtype_ctor(type_table::in, tvarset::in,
    type_status::in, type_ctor::in, list(constructor)::in, constructor::in,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

look_up_and_check_subtype_ctor(TypeTable, TVarSet, TypeStatus,
        SuperTypeCtor, SuperCtors, Ctor, !FoundInvalidType, !Specs) :-
    Ctor = ctor(_, _, CtorName, _, Arity, Context),
    UnqualCtorName = unqualify_name(CtorName),
    ( if
        search_ctor_by_unqual_name(SuperCtors, UnqualCtorName, Arity,
            SuperCtor)
    then
        check_subtype_ctor(TypeTable, TVarSet, TypeStatus, Ctor, SuperCtor,
            !FoundInvalidType, !Specs)
    else
        Pieces = [words("Error:"),
            unqual_sym_name_arity(sym_name_arity(CtorName, Arity)),
            words("is not a constructor of the supertype"),
            unqual_type_ctor(SuperTypeCtor), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs],
        !:FoundInvalidType = found_invalid_type
    ).

:- pred search_ctor_by_unqual_name(list(constructor)::in, string::in, int::in,
    constructor::out) is semidet.

search_ctor_by_unqual_name([HeadCtor | TailCtors], UnqualName, Arity, Ctor) :-
    ( if
        HeadCtor = ctor(_, _, HeadName, _, Arity, _),
        unqualify_name(HeadName) = UnqualName
    then
        Ctor = HeadCtor
    else
        search_ctor_by_unqual_name(TailCtors, UnqualName, Arity, Ctor)
    ).

%---------------------%

:- pred check_subtype_ctor(type_table::in, tvarset::in, type_status::in,
    constructor::in, constructor::in,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_subtype_ctor(TypeTable, TVarSet, TypeStatus, Ctor, SuperCtor,
        !FoundInvalidType, !Specs) :-
    Ctor = ctor(_, MaybeExistConstraints, CtorSymName, Args, Arity, Context),
    SuperCtor = ctor(_, MaybeSuperExistConstraints, _SuperCtorName, SuperArgs,
        _SuperArity, _SuperContext),
    list.foldl4_corresponding(
        check_subtype_ctor_arg(TypeTable, TVarSet, TypeStatus,
            CtorSymName, MaybeExistConstraints, MaybeSuperExistConstraints),
        Args, SuperArgs,
        1, _, map.init, ExistQVarsMapping,
        did_not_find_invalid_type, FoundInvalidType, !Specs),
    (
        FoundInvalidType = did_not_find_invalid_type,
        (
            MaybeExistConstraints = no_exist_constraints,
            MaybeSuperExistConstraints = no_exist_constraints
        ;
            MaybeExistConstraints = exist_constraints(Constraints),
            MaybeSuperExistConstraints = exist_constraints(SuperConstraints),
            CtorSymNameArity = sym_name_arity(CtorSymName, Arity),
            check_subtype_ctor_exist_constraints(CtorSymNameArity,
                Constraints, SuperConstraints, ExistQVarsMapping, Context,
                !FoundInvalidType, !Specs)
        ;
            MaybeExistConstraints = no_exist_constraints,
            MaybeSuperExistConstraints = exist_constraints(_),
            unexpected($pred, "exist_constraints mismatch")
        ;
            MaybeExistConstraints = exist_constraints(_),
            MaybeSuperExistConstraints = no_exist_constraints,
            unexpected($pred, "exist_constraints mismatch")
        )
    ;
        FoundInvalidType = found_invalid_type,
        !:FoundInvalidType = FoundInvalidType
    ).

%---------------------%

    % A map from existential type variable in the supertype constructor
    % to an existential type variable in the subtype constructor.
    %
:- type existq_tvar_mapping == map(tvar, tvar).

:- pred check_subtype_ctor_arg(type_table::in, tvarset::in, type_status::in,
    sym_name::in,
    maybe_cons_exist_constraints::in, maybe_cons_exist_constraints::in,
    constructor_arg::in, constructor_arg::in,
    int::in, int::out, existq_tvar_mapping::in, existq_tvar_mapping::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_subtype_ctor_arg(TypeTable, TVarSet, OrigTypeStatus, CtorSymName,
        MaybeExistConstraints, MaybeSuperExistConstraints,
        CtorArg, SuperCtorArg,
        ArgNum, ArgNum + 1, !ExistQVarsMapping, !FoundInvalidType, !Specs) :-
    CtorArg = ctor_arg(_FieldName, ArgType, Context),
    SuperCtorArg = ctor_arg(_SuperFieldName, SuperArgType, _SuperContext),
    ( if
        check_is_subtype(TypeTable, TVarSet, OrigTypeStatus,
            ArgType, SuperArgType,
            MaybeExistConstraints, MaybeSuperExistConstraints,
            !ExistQVarsMapping)
    then
        true
    else
        ArgTypeStr =
            mercury_type_to_string(TVarSet, print_name_only, ArgType),
        SuperArgTypeStr =
            mercury_type_to_string(TVarSet, print_name_only, SuperArgType),
        Pieces = [words("Error: the"), nth_fixed(ArgNum), words("argument"),
            words("of"), quote(unqualify_name(CtorSymName)),
            words("has a type,"), quote(ArgTypeStr), suffix(","),
            words("which is not a subtype of the corresponding argument type"),
            quote(SuperArgTypeStr), words("in the supertype."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs],
        !:FoundInvalidType = found_invalid_type
    ).

%---------------------%

:- pred check_is_subtype(type_table::in, tvarset::in, type_status::in,
    mer_type::in, mer_type::in,
    maybe_cons_exist_constraints::in, maybe_cons_exist_constraints::in,
    existq_tvar_mapping::in, existq_tvar_mapping::out) is semidet.

check_is_subtype(TypeTable, TVarSet0, OrigTypeStatus, TypeA, TypeB,
        MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping) :-
    require_complete_switch [TypeA]
    (
        TypeA = builtin_type(BuiltinType),
        TypeB = builtin_type(BuiltinType)
    ;
        TypeA = type_variable(VarA, Kind),
        TypeB = type_variable(VarB, Kind),
        check_is_subtype_var_var(VarA, VarB,
            MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping)
    ;
        TypeA = defined_type(NameA, ArgTypesA, Kind),
        TypeB = defined_type(NameB, ArgTypesB, Kind),
        list.length(ArgTypesA, ArityA),
        list.length(ArgTypesB, ArityB),
        ( if
            NameA = NameB,
            ArityA = ArityB
        then
            % TypeA and TypeB have the same type constructor.
            % Check their corresponding argument types.
            check_corresponding_args_are_subtype(TypeTable, TVarSet0,
                OrigTypeStatus, ArgTypesA, ArgTypesB,
                MaybeExistConstraintsA, MaybeExistConstraintsB,
                !ExistQVarsMapping)
        else
            % TypeA and TypeB have different type constructors.
            % Find a subtype definition s(S1, ..., Sn) =< t(T1, ..., Tk)
            % where s/n is the type constructor of TypeA.
            TypeCtorA = type_ctor(NameA, ArityA),
            search_type_ctor_defn(TypeTable, TypeCtorA, TypeDefnA),
            hlds_data.get_type_defn_body(TypeDefnA, TypeBodyA),
            TypeBodyA = hlds_du_type(TypeBodyDuA),
            TypeBodyDuA = type_body_du(_, subtype_of(SuperTypeA), _, _, _),

            hlds_data.get_type_defn_status(TypeDefnA, TypeStatusA),
            not subtype_defn_int_supertype_defn_impl(OrigTypeStatus,
                TypeStatusA),

            % The variables S1, ..., Sn must be distinct.
            % Create a substitution from S1, ..., Sn to the types in ArgTypesA.
            hlds_data.get_type_defn_tvarset(TypeDefnA, TVarSetA),
            hlds_data.get_type_defn_tparams(TypeDefnA, TypeParamsA0),
            tvarset_merge_renaming(TVarSet0, TVarSetA, TVarSet, RenamingA),
            apply_variable_renaming_to_tvar_list(RenamingA,
                TypeParamsA0, TypeParamsA),
            map.from_corresponding_lists(TypeParamsA, ArgTypesA, TSubstA),

            % Apply the substitution to t(T1, ..., Tk) to give
            % t(T1', ..., Tk').
            rename_and_rec_subst_in_type(RenamingA, TSubstA,
                SuperTypeA, RenamedSuperTypeA),

            % Check that t(T1', ..., Tk') =< TypeB.
            check_is_subtype(TypeTable, TVarSet, OrigTypeStatus,
                RenamedSuperTypeA, TypeB,
                MaybeExistConstraintsA, MaybeExistConstraintsB,
                !ExistQVarsMapping)
        )
    ;
        TypeA = tuple_type(ArgTypesA, Kind),
        TypeB = tuple_type(ArgTypesB, Kind),
        list.length(ArgTypesA, Arity),
        list.length(ArgTypesB, Arity),
        check_corresponding_args_are_subtype(TypeTable, TVarSet0,
            OrigTypeStatus, ArgTypesA, ArgTypesB,
            MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping)
    ;
        TypeA = higher_order_type(PredOrFunc, ArgTypesA, HOInstInfoA, Purity,
            EvalMethod),
        TypeB = higher_order_type(PredOrFunc, ArgTypesB, HOInstInfoB, Purity,
            EvalMethod),
        list.length(ArgTypesA, Arity),
        list.length(ArgTypesB, Arity),
        (
            HOInstInfoA = higher_order(PredInfoInfoA),
            HOInstInfoB = higher_order(PredInfoInfoB),
            PredInfoInfoA = pred_inst_info(PredOrFunc, ArgModesA, _RegTypesA,
                Detism),
            PredInfoInfoB = pred_inst_info(PredOrFunc, ArgModesB, _RegTypesB,
                Detism),
            MaybeArgModesA = yes(ArgModesA),
            MaybeArgModesB = yes(ArgModesB)
        ;
            HOInstInfoA = none_or_default_func,
            HOInstInfoB = none_or_default_func,
            MaybeArgModesA = no,
            MaybeArgModesB = no
        ),
        check_is_subtype_higher_order(TypeTable, TVarSet0, OrigTypeStatus,
            ArgTypesA, ArgTypesB, MaybeArgModesA, MaybeArgModesB,
            MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping)
    ;
        TypeA = apply_n_type(_, _, _),
        fail
    ;
        TypeA = kinded_type(TypeA1, Kind),
        TypeB = kinded_type(TypeB1, Kind),
        check_is_subtype(TypeTable, TVarSet0, OrigTypeStatus, TypeA1, TypeB1,
            MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping)
    ).

:- pred check_is_subtype_var_var(tvar::in, tvar::in,
    maybe_cons_exist_constraints::in, maybe_cons_exist_constraints::in,
    existq_tvar_mapping::in, existq_tvar_mapping::out) is semidet.

check_is_subtype_var_var(VarA, VarB,
        MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping) :-
    ( if VarA = VarB then
        true
    else if map.search(!.ExistQVarsMapping, VarB, VarB1) then
        VarB1 = VarA
    else
        MaybeExistConstraintsA = exist_constraints(ExistConstraintsA),
        MaybeExistConstraintsB = exist_constraints(ExistConstraintsB),
        ExistConstraintsA = cons_exist_constraints(_ExistQVarsA,
            _ConstraintsA, UnconstrainedExistQVarsA, ConstrainedExistQVarsA),
        ExistConstraintsB = cons_exist_constraints(_ExistQVarsB,
            _ConstraintsB, UnconstrainedExistQVarsB, ConstrainedExistQVarsB),
        (
            list.contains(UnconstrainedExistQVarsA, VarA),
            list.contains(UnconstrainedExistQVarsB, VarB)
        ;
            list.contains(ConstrainedExistQVarsA, VarA),
            list.contains(ConstrainedExistQVarsB, VarB)
        ),
        map.insert(VarB, VarA, !ExistQVarsMapping)
    ).

:- pred check_corresponding_args_are_subtype(type_table::in, tvarset::in,
    type_status::in, list(mer_type)::in, list(mer_type)::in,
    maybe_cons_exist_constraints::in, maybe_cons_exist_constraints::in,
    existq_tvar_mapping::in, existq_tvar_mapping::out) is semidet.

check_corresponding_args_are_subtype(_TypeTable, _TVarSet, _OrigTypeStatus,
        [], [],
        _MaybeExistConstraintsA, _MaybeExistConstraintsB, !ExistQVarsMapping).
check_corresponding_args_are_subtype(TypeTable, TVarSet, OrigTypeStatus,
        [TypeA | TypesA], [TypeB | TypesB],
        MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping) :-
    check_is_subtype(TypeTable, TVarSet, OrigTypeStatus, TypeA, TypeB,
        MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping),
    check_corresponding_args_are_subtype(TypeTable, TVarSet, OrigTypeStatus,
        TypesA, TypesB, MaybeExistConstraintsA, MaybeExistConstraintsB,
        !ExistQVarsMapping).

:- pred check_is_subtype_higher_order(type_table::in, tvarset::in,
    type_status::in, list(mer_type)::in, list(mer_type)::in,
    maybe(list(mer_mode))::in, maybe(list(mer_mode))::in,
    maybe_cons_exist_constraints::in, maybe_cons_exist_constraints::in,
    existq_tvar_mapping::in, existq_tvar_mapping::out) is semidet.

check_is_subtype_higher_order(_TypeTable, _TVarSet, _OrigTypeStatus,
        [], [], MaybeModesA, MaybeModesB,
        _MaybeExistConstraintsA, _MaybeExistConstraintsB, !ExistQVarsMapping)
        :-
    (
        MaybeModesA = no,
        MaybeModesB = no
    ;
        MaybeModesA = yes([]),
        MaybeModesB = yes([])
    ).
check_is_subtype_higher_order(TypeTable, TVarSet, OrigTypeStatus,
        [TypeA | TypesA], [TypeB | TypesB], MaybeModesA0, MaybeModesB0,
        MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping) :-
    % Check arguments of higher order term have the same type.
    % This could be more efficient, but should be rarely used anyway.
    check_is_subtype(TypeTable, TVarSet, OrigTypeStatus, TypeA, TypeB,
        MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping),
    check_is_subtype(TypeTable, TVarSet, OrigTypeStatus, TypeB, TypeA,
        MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping),

    % Argument modes, if available, must match exactly.
    (
        MaybeModesA0 = no,
        MaybeModesB0 = no,
        MaybeModesA = no,
        MaybeModesB = no
    ;
        MaybeModesA0 = yes([ModeA | ModesA]),
        MaybeModesB0 = yes([ModeB | ModesB]),
        % XXX Currently we require term equality.
        ModeA = ModeB,
        MaybeModesA = yes(ModesA),
        MaybeModesB = yes(ModesB)
    ),

    check_is_subtype_higher_order(TypeTable, TVarSet, OrigTypeStatus,
        TypesA, TypesB, MaybeModesA, MaybeModesB,
        MaybeExistConstraintsA, MaybeExistConstraintsB, !ExistQVarsMapping).

%---------------------%

:- pred check_subtype_ctor_exist_constraints(sym_name_arity::in,
    cons_exist_constraints::in, cons_exist_constraints::in,
    existq_tvar_mapping::in, prog_context::in,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_subtype_ctor_exist_constraints(CtorSymNameArity,
        ExistConstraints, SuperExistConstraints, ExistQVarsMapping, Context,
        !FoundInvalidType, !Specs) :-
    ExistConstraints = cons_exist_constraints(_, Constraints, _, _),
    SuperExistConstraints = cons_exist_constraints(_, SuperConstraints0, _, _),
    apply_variable_renaming_to_prog_constraint_list(ExistQVarsMapping,
        SuperConstraints0, SuperConstraints),
    list.sort(Constraints, SortedConstraints),
    list.sort(SuperConstraints, SortedSuperConstraints),
    ( if SortedConstraints = SortedSuperConstraints then
        true
    else
        Pieces = [words("Error: existential class constraints for"),
            unqual_sym_name_arity(CtorSymNameArity),
            words("differ in the subtype and supertype."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs],
        !:FoundInvalidType = found_invalid_type
    ).

%---------------------%

:- pred check_subtype_ctors_order(type_ctor::in, list(constructor)::in,
    type_ctor::in, list(constructor)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_subtype_ctors_order(TypeCtor, Ctors, SuperTypeCtor, SuperCtors, Context,
        !Specs) :-
    compute_subtype_ctors_diff(Ctors, SuperCtors, ChangeHunkPieces),
    (
        ChangeHunkPieces = []
    ;
        ChangeHunkPieces = [_ | _],
        Pieces = [words("Warning:"), unqual_type_ctor(TypeCtor),
            words("declares some constructors"),
            words("in a different order to its supertype"),
            unqual_type_ctor(SuperTypeCtor), suffix(","),
            words("as shown by this diff"),
            words("against those of the supertype's constructors"),
            words("which are present in the subtype:"), nl,
            blank_line] ++
            ChangeHunkPieces,
        Spec = simplest_spec($pred, severity_warning,
            phase_parse_tree_to_hlds, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred compute_subtype_ctors_diff(list(constructor)::in,
    list(constructor)::in, list(format_component)::out) is det.

compute_subtype_ctors_diff(Ctors, SuperCtors, ChangeHunkPieces) :-
    (
        ( Ctors = []
        ; Ctors = [_]
        ),
        ChangeHunkPieces = []
    ;
        Ctors = [_, _ | _],
        list.map(ctor_to_string, Ctors, CtorStrs0),
        list.map(ctor_to_string, SuperCtors, SuperCtorStrs0),
        list.filter(list.contains(SuperCtorStrs0), CtorStrs0, CtorStrs),
        list.filter(list.contains(CtorStrs0), SuperCtorStrs0, SuperCtorStrs),
        EditParams = edit_params(1, 1, 1),
        find_shortest_edit_seq(EditParams, SuperCtorStrs, CtorStrs, EditSeq),
        find_diff_seq(SuperCtorStrs, EditSeq, DiffSeq),
        find_change_hunks(3, DiffSeq, ChangeHunks),
        list.map(change_hunk_to_pieces, ChangeHunks, ChangeHunkPieceLists),
        list.condense(ChangeHunkPieceLists, ChangeHunkPieces)
    ).

:- pred ctor_to_string(constructor::in, string::out) is det.

ctor_to_string(Ctor, Str) :-
    Ctor = ctor(_, _, SymName, _, Arity, _),
    UnqualName = unqualify_name(SymName),
    SNA = sym_name_arity(unqualified(UnqualName), Arity),
    Str = sym_name_arity_to_string(SNA).

%---------------------%

:- pred rename_and_rec_subst_in_constructor(tvar_renaming::in, tsubst::in,
    constructor::in, constructor::out) is det.

rename_and_rec_subst_in_constructor(Renaming, TSubst, Ctor0, Ctor) :-
    Ctor0 = ctor(Ordinal, MaybeExistConstraints0, Name, Args0, NumArgs,
        Context),
    (
        MaybeExistConstraints0 = no_exist_constraints,
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints0 = exist_constraints(ExistConstraints0),
        rename_and_rec_subst_in_exist_constraints(Renaming, TSubst,
            ExistConstraints0, ExistConstraints),
        MaybeExistConstraints = exist_constraints(ExistConstraints)
    ),
    list.map(rename_and_rec_subst_in_constructor_arg(Renaming, TSubst),
        Args0, Args),
    Ctor = ctor(Ordinal, MaybeExistConstraints, Name, Args, NumArgs,
        Context).

:- pred rename_and_rec_subst_in_exist_constraints(tvar_renaming::in,
    tsubst::in, cons_exist_constraints::in, cons_exist_constraints::out)
    is det.

rename_and_rec_subst_in_exist_constraints(Renaming, TSubst,
        ExistConstraints0, ExistConstraints) :-
    ExistConstraints0 = cons_exist_constraints(ExistQVars0, Constraints0,
        UnconstrainedExistQVars0, ConstrainedExistQVars0),

    apply_variable_renaming_to_tvar_list(Renaming,
        ExistQVars0, ExistQVars),

    apply_variable_renaming_to_prog_constraint_list(Renaming,
        Constraints0, Constraints1),
    apply_rec_subst_to_prog_constraint_list(TSubst,
        Constraints1, Constraints),

    apply_variable_renaming_to_tvar_list(Renaming,
        UnconstrainedExistQVars0, UnconstrainedExistQVars),

    apply_variable_renaming_to_tvar_list(Renaming,
        ConstrainedExistQVars0, ConstrainedExistQVars),

    ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
        UnconstrainedExistQVars, ConstrainedExistQVars).

:- pred rename_and_rec_subst_in_constructor_arg(tvar_renaming::in, tsubst::in,
    constructor_arg::in, constructor_arg::out) is det.

rename_and_rec_subst_in_constructor_arg(Renaming, TSubst, Arg0, Arg) :-
    Arg0 = ctor_arg(MaybeFieldName, Type0, Context),
    rename_and_rec_subst_in_type(Renaming, TSubst, Type0, Type),
    Arg = ctor_arg(MaybeFieldName, Type, Context).

:- pred rename_and_rec_subst_in_type(tvar_renaming::in, tsubst::in,
    mer_type::in, mer_type::out) is det.

rename_and_rec_subst_in_type(Renaming, TSubst, Type0, Type) :-
    apply_variable_renaming_to_type(Renaming, Type0, Type1),
    apply_rec_subst_to_type(TSubst, Type1, Type).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_type.
%---------------------------------------------------------------------------%
