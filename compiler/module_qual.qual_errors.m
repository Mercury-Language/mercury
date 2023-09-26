%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module implements the first pass of module_qual.m; it records
% what entities are available from which modules and with what permissions.
%

:- module parse_tree.module_qual.qual_errors.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module set.

%---------------------------------------------------------------------------%
%
% Facilities for recording the contexts of errors.
%

:- type mq_constraint_error_context
    --->    mqcec_class_defn(prog_context,
                % The name of the type class beging defined, and its arity.
                class_id
            )
    ;       mqcec_class_method(prog_context,
                % The identity of the class method the constraint is on:
                % whether it is predicate or function, and its name.
                % Its arity would be nice, but it is tricky to calculate
                % in the presence of with_type.
                pred_or_func,
                string
            )
    ;       mqcec_instance_defn(prog_context,
                % The name of the class the instance is for, and the
                % instance type vector.
                class_name,
                list(mer_type)
            )
    ;       mqcec_type_defn_constructor(prog_context,
                % The name of the type whose definition the constraint is in.
                type_ctor,

                % The function symbol the constraint is on.
                string
            )
    ;       mqcec_pred_decl(prog_context,
                % The identity of the entity the constraint is on:
                % whether it is predicate or function, its name, and its arity.
                pf_sym_name_arity
            ).

:- type mq_error_context
    --->    mqec_type_defn(prog_context,
                % The name of the type constructor whose definition we are in.
                type_ctor
            )
    ;       mqec_inst_defn(prog_context,
                % The name of the inst constructor whose definition we are in.
                inst_ctor
            )
    ;       mqec_mode_defn(prog_context,
                % The name of the mode constructor whose definition we are in.
                mode_ctor
            )
    ;       mqec_constructor_arg(prog_context,
                % The name of the type constructor whose definition we are in.
                type_ctor,

                % The name of the function symbol.
                string,

                % The argument number of the type.
                int,

                % The name of the field, if it has one.
                maybe(ctor_field_name)
            )
    ;       mqec_typeclass_constraint_name(
                % The context the constraint is in.
                mq_constraint_error_context
            )
    ;       mqec_typeclass_constraint(
                % The name and arity of the typeclass the constraint is for.
                sym_name,
                int,

                % The context the constraint is in.
                mq_constraint_error_context
            )
    ;       mqec_pred_or_func(prog_context,
                % Whether it is a predicate or function declaration, ...
                pred_or_func,

                % and its name.
                mq_id
            )
    ;       mqec_pred_or_func_mode(prog_context,
                maybe(pred_or_func),
                mq_id
            )
    ;       mqec_foreign_proc(prog_context)
    ;       mqec_foreign_enum(prog_context)
    ;       mqec_foreign_export_enum(prog_context)
    ;       mqec_pragma_decl(prog_context,
                item_decl_pragma_info
            )
    ;       mqec_pragma_impl(prog_context,
                item_impl_pragma_info
            )
    ;       mqec_lambda_expr(prog_context)
    ;       mqec_clause_mode_annotation(prog_context)
    ;       mqec_type_qual(prog_context)
    ;       mqec_class(prog_context,
                class_id
            )
    ;       mqec_instance(prog_context,
                class_id
            )
    ;       mqec_mutable(prog_context,
                string
            )
    ;       mqec_type_repn(prog_context,
                type_ctor
            )
    ;       mqec_event_spec_attr(prog_context,
                % The event name.
                string,

                % The attribute name.
                string
            ).

%---------------------------------------------------------------------------%

    % Report an undefined type, inst or mode.
    %
:- pred report_undefined_mq_id(mq_info::in, mq_error_context::in,
    mq_id::in, qual_id_kind::in, module_name::in,
    list(module_name)::in, list(module_name)::in, set(int)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Report an error where a type, inst, mode or typeclass had
    % multiple possible matches.
    %
:- pred report_ambiguous_match(mq_error_context::in, mq_id::in,
    qual_id_kind::in,
    list(module_name)::in, list(module_name)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Output an error message about an ill-formed user_inst.
    %
:- pred report_invalid_user_inst(sym_name::in, list(mer_inst)::in,
    mq_error_context::in, list(error_spec)::in, list(error_spec)::out) is det.

    % Warn about a module imported in the interface that is not used
    % in the interface.
    %
:- pred warn_unused_interface_import(module_name::in,
    pair(module_name, one_or_more(prog_context))::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.item_util.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_util.

:- import_module string.

%---------------------------------------------------------------------------%
%
% Error reporting predicates.
%

report_undefined_mq_id(Info, ErrorContext, Id, IdType, ThisModuleName,
        IntMismatches0, QualMismatches0, PossibleAritiesSet, !Specs) :-
    mq_error_context_to_pieces(ErrorContext, Context, ShouldUnqualId,
        ErrorContextPieces),
    InPieces = [words("In")] ++ ErrorContextPieces ++ [suffix(":"), nl,
        words("error:")],
    qual_id_kind_to_string(IdType, IdTypeStr),
    Id = mq_id(IdSymName, IdArity),
    IdBaseName = unqualify_name(IdSymName),
    list.sort_and_remove_dups(IntMismatches0, IntMismatches),
    list.sort_and_remove_dups(QualMismatches0, QualMismatches),
    ( if
        list.delete_first(IntMismatches, ThisModuleName,
            OtherIntMismatchesPrime)
    then
        OtherIntMismatches = OtherIntMismatchesPrime,
        ThisModulesSN = qualified(ThisModuleName, IdBaseName),
        ThisModuleSNA = sym_name_arity(ThisModulesSN, IdArity),
        UndefPieces = [],
        ThisIntPieces = [words("the"), fixed(IdTypeStr),
            unqual_sym_name_arity(ThisModuleSNA),
            words("is not exported,"),
            words("and thus it may not be used in the interface."), nl]
    else
        OtherIntMismatches = IntMismatches,
        (
            ShouldUnqualId = no,
            SNA = sym_name_arity(IdSymName, IdArity)
        ;
            ShouldUnqualId = yes,
            SNA = sym_name_arity(unqualified(IdBaseName), IdArity)
        ),
        UndefPieces = [words("undefined"), fixed(IdTypeStr),
            qual_sym_name_arity(SNA), suffix("."), nl],
        ThisIntPieces = []
    ),
    (
        OtherIntMismatches = [],
        OtherIntPieces = []
    ;
        OtherIntMismatches = [_ | OtherIntMismatchesTail],
        (
            OtherIntMismatchesTail = [],
            OtherIntModuleWord = "module",
            OtherIntHasWord = "has"
        ;
            OtherIntMismatchesTail = [_ | _],
            OtherIntModuleWord = "modules",
            OtherIntHasWord = "have"
        ),
        OtherIntSymNames = list.map(wrap_module_name, OtherIntMismatches),
        OtherIntPieces = [words("(The"), fixed(OtherIntModuleWord)] ++
            component_list_to_pieces("and", OtherIntSymNames) ++
            [fixed(OtherIntHasWord),
                words("not been imported in the interface.)"), nl]
    ),
    (
        QualMismatches = [],
        QualPieces = []
    ;
        QualMismatches = [_ | QualMismatchesTail],
        (
            QualMismatchesTail = [],
            QualModuleWord = "module"
        ;
            QualMismatchesTail = [_ | _],
            QualModuleWord = "modules"
        ),
        QualSymNames = list.map(wrap_module_name, QualMismatches),
        QualPieces = [words("(Only fully module qualified names"),
            words("may refer to the entities defined in"),
            fixed(QualModuleWord)] ++
            component_list_to_pieces("and", QualSymNames) ++
            [suffix(".)"), nl]
    ),
    ( if
        % If IdSymName is a qualified symbol, then check whether the module
        % name it specifies has been imported.

        IdSymName = qualified(IdModuleName, _),
        mq_info_get_this_module(Info, ThisModuleName),
        mq_info_get_imported_modules(Info, ImportedModuleNames),
        AvailModuleNames =
            [ThisModuleName | set_tree234.to_sorted_list(ImportedModuleNames)],
        module_name_matches_some(IdModuleName, AvailModuleNames) = no
    then
        % This used to say "The module IdModuleName has not been imported.".
        % However, a module with that name may not even exist, since it may be
        % that IdModuleName is only *partially* qualified. We now generate
        % wording that does not imply that IdModuleName must exist.
        NonImportedPieces = [words("(No module named"),
            qual_sym_name(IdModuleName), words("has been imported.)"), nl]
    else
        NonImportedPieces = []
    ),
    set.to_sorted_list(PossibleAritiesSet, PossibleArities),
    ( if
        PossibleArities = [_ | _],
        ThisIntPieces = [],
        OtherIntPieces = [],
        QualPieces = [],
        NonImportedPieces = []
    then
        qual_id_kinds_to_string(IdType, IdTypesStr),
        IsAre = choose_number(PossibleArities, "is a", "are"),
        KindKinds = choose_number(PossibleArities, IdTypeStr, IdTypesStr),
        ArityArities = choose_number(PossibleArities, "arity", "arities"),
        list.map(string.int_to_string, PossibleArities, PossibleArityStrs),
        PossibleAritiesPieces = list_to_pieces(PossibleArityStrs),
        OtherArityPieces = [words("(There"), words(IsAre), words(KindKinds),
            words("named"), quote(unqualify_name(IdSymName)),
            words("with"), words(ArityArities)] ++
            PossibleAritiesPieces ++ [suffix(".)"), nl]
    else
        OtherArityPieces = []
    ),
    % Don't suggest any other names instead of IdBaseName if there are
    % plausible ways that IdBaseName could actually be a correct name.
    ( if
        ThisIntPieces = [],
        OtherIntPieces = [],
        QualPieces = [],
        NonImportedPieces = [],
        OtherArityPieces = []
    then
        (
            IdType = qual_id_type,
            mq_info_get_types(Info, IdSet),
            get_names_in_id_set(IdSet, KnownNames)
        ;
            IdType = qual_id_inst,
            mq_info_get_insts(Info, IdSet),
            get_names_in_id_set(IdSet, KnownNames)
        ;
            IdType = qual_id_mode,
            mq_info_get_modes(Info, IdSet),
            get_names_in_id_set(IdSet, KnownNames)
        ;
            IdType = qual_id_class,
            mq_info_get_classes(Info, IdSet),
            get_names_in_id_set(IdSet, KnownNames)
        ),
        maybe_construct_did_you_mean_pieces(IdBaseName, KnownNames,
            DidYouMeanPieces)
    else
        DidYouMeanPieces = []
    ),
    AllPieces = InPieces ++ UndefPieces ++ ThisIntPieces ++ OtherIntPieces ++
        QualPieces ++ NonImportedPieces ++ OtherArityPieces ++
        DidYouMeanPieces,
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, AllPieces),
    !:Specs = [Spec | !.Specs].

:- func module_name_matches_some(module_name, list(module_name)) = bool.

module_name_matches_some(_SearchModuleName, []) = no.
module_name_matches_some(SearchModuleName, [ModuleName | ModuleNames]) =
        Matches :-
    ( if partial_sym_name_matches_full(SearchModuleName, ModuleName) then
        Matches = yes
    else
        Matches = module_name_matches_some(SearchModuleName, ModuleNames)
    ).

report_ambiguous_match(ErrorContext, Id, IdType,
        UsableModuleNames, UnusableModuleNames, !Specs) :-
    mq_error_context_to_pieces(ErrorContext, Context, _ShouldUnqualId,
        ErrorContextPieces),
    qual_id_kind_to_string(IdType, IdTypeStr),
    UsableModuleSymNames = list.map(wrap_module_name, UsableModuleNames),
    MainPieces = [words("In")] ++ ErrorContextPieces ++ [suffix(":"), nl,
        words("ambiguity error: multiple possible matches for"),
        fixed(IdTypeStr), wrap_qual_id(Id), suffix("."), nl,
        words("The possible matches are in modules")] ++
        component_list_to_pieces("and", UsableModuleSymNames) ++
        [suffix("."), nl],
    (
        UnusableModuleNames = [],
        UnusablePieces = []
    ;
        (
            UnusableModuleNames = [_],
            MatchWord = "match"
        ;
            UnusableModuleNames = [_, _ | _],
            MatchWord = "matches"
        ),
        UnusableModuleSymNames =
            list.map(wrap_module_name, UnusableModuleNames),
        UnusablePieces = [words("The"), words(MatchWord),
            words("in modules")] ++ UnusableModuleSymNames ++
            [words("may not be used in the interface."), nl]
    ),
    VerbosePieces = [words("An explicit module qualifier"),
        words("may be necessary."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), always(UnusablePieces),
        verbose_only(verbose_always, VerbosePieces)]),
    Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

report_invalid_user_inst(_SymName, _Insts, ErrorContext, !Specs) :-
    mq_error_context_to_pieces(ErrorContext, Context, _ShouldUnqualId,
        ErrorContextPieces),
    Pieces = [words("In")] ++ ErrorContextPieces ++ [suffix(":"), nl,
        words("error: variable used as inst constructor."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

warn_unused_interface_import(ParentModuleName,
        ImportedModuleName - ImportContexts, !Specs) :-
    % UNUSED_IMPORT Harmonize the operation of this predicate with
    % the operation of generate_unused_warning in unused_imports.m.
    ImportContexts = one_or_more(HeadContext, TailContexts),
    HeadPieces =
        [words("In module"), qual_sym_name(ParentModuleName), suffix(":"), nl,
        words("warning: module"), qual_sym_name(ImportedModuleName),
        words("is imported in the interface,"),
        words("but it is not used in the interface."), nl],
    HeadMsg = simplest_msg(HeadContext, HeadPieces),
    % TailContexts is almost always [], we add TailMsgs just in case it isn't.
    list.map(warn_redundant_import_context(ImportedModuleName),
        TailContexts, TailMsgs),
    % If the warn_unused_imports option is set to yes, then
    % unused_imports.m will also generate a warning for this import,
    % and it will be more precise than we can do here, because it will know
    % whether the imported module is used in the *implementation* section.
    Spec = conditional_spec($pred, warn_unused_interface_imports, yes,
        severity_warning, phase_parse_tree_to_hlds, [HeadMsg | TailMsgs]),
    !:Specs = [Spec | !.Specs].

:- pred warn_redundant_import_context(module_name::in, prog_context::in,
    error_msg::out) is det.

warn_redundant_import_context(ImportedModuleName, Context, Msg) :-
    Pieces = [words("Module"), qual_sym_name(ImportedModuleName),
        words("is also redundantly imported here."), nl],
    Msg = simplest_msg(Context, Pieces).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred mq_constraint_error_context_to_pieces(mq_constraint_error_context::in,
    prog_context::out, string::out, list(format_piece)::out) is det.

mq_constraint_error_context_to_pieces(ConstraintErrorContext,
        Context, Start, Pieces) :-
    (
        ConstraintErrorContext = mqcec_class_defn(Context, ClassId),
        Start = "in",
        Pieces = [words("definition of type class"), qual_class_id(ClassId)]
    ;
        ConstraintErrorContext = mqcec_class_method(Context,
            PredOrFunc, MethodName),
        Start = "on",
        Pieces = [words("class method"), p_or_f(PredOrFunc), quote(MethodName)]
    ;
        ConstraintErrorContext = mqcec_instance_defn(Context,
            ClassName, ArgTypes),
        Start = "on",
        list.length(ArgTypes, NumArgTypes),
        Pieces = [words("instance definition for"),
            qual_class_id(class_id(ClassName, NumArgTypes))]
    ;
        ConstraintErrorContext = mqcec_type_defn_constructor(Context,
            TypeCtor, FunctionSymbol),
        Start = "on",
        Pieces = [words("function symbol"), quote(FunctionSymbol),
            words("for type constructor"), unqual_type_ctor(TypeCtor)]
    ;
        ConstraintErrorContext = mqcec_pred_decl(Context, PFSymNameArity),
        Start = "on",
        Pieces = [words("declaration of"),
            unqual_pf_sym_name_pred_form_arity(PFSymNameArity)]
    ).

:- pred mq_error_context_to_pieces(mq_error_context::in,
    prog_context::out, bool::out, list(format_piece)::out) is det.

mq_error_context_to_pieces(ErrorContext, Context, ShouldUnqualId, Pieces) :-
    (
        ErrorContext = mqec_type_defn(Context, TypeCtor),
        ShouldUnqualId = no,
        Pieces = [words("definition of type"), unqual_type_ctor(TypeCtor)]
    ;
        ErrorContext = mqec_inst_defn(Context, InstCtor),
        ShouldUnqualId = no,
        Pieces = [words("definition of inst"), unqual_inst_ctor(InstCtor)]
    ;
        ErrorContext = mqec_mode_defn(Context, ModeCtor),
        ShouldUnqualId = no,
        Pieces = [words("definition of mode"), unqual_mode_ctor(ModeCtor)]
    ;
        ErrorContext = mqec_constructor_arg(Context, ContainingTypeCtor,
            FunctionSymbol, ArgNum, MaybeCtorFieldName),
        ShouldUnqualId = no,
        (
            MaybeCtorFieldName = no,
            FieldNamePieces = []
        ;
            MaybeCtorFieldName = yes(CtorFieldName),
            CtorFieldName = ctor_field_name(FieldSymName, _FieldContext),
            FieldNamePieces = [words("(field name"),
                quote(unqualify_name(FieldSymName)), suffix(")")]
        ),
        Pieces = [words("the"), nth_fixed(ArgNum), words("argument of"),
            words("function symbol"), quote(FunctionSymbol)] ++
            FieldNamePieces ++
            [words("of the type"), unqual_type_ctor(ContainingTypeCtor)]
    ;
        ErrorContext = mqec_typeclass_constraint_name(ConstraintErrorContext),
        ShouldUnqualId = no,
        mq_constraint_error_context_to_pieces(ConstraintErrorContext,
            Context, _Start, Pieces)
    ;
        ErrorContext = mqec_typeclass_constraint(ClassName, Arity,
            ConstraintErrorContext),
        ShouldUnqualId = no,
        mq_constraint_error_context_to_pieces(ConstraintErrorContext,
            Context, Start, ConstraintErrorContextPieces),
        Pieces = [words("type class constraint for"),
            unqual_sym_name_arity(sym_name_arity(ClassName, Arity)),
            words(Start) | ConstraintErrorContextPieces]
    ;
        ErrorContext = mqec_pred_or_func(Context, PredOrFunc, Id),
        ShouldUnqualId = no,
        Id = mq_id(SymName, OrigArity),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        Pieces = [words("declaration of"),
            fixed(pred_or_func_to_full_str(PredOrFunc)),
            unqual_sym_name_arity(sym_name_arity(SymName, Arity))]
    ;
        ErrorContext = mqec_pred_or_func_mode(Context, MaybePredOrFunc, Id),
        ShouldUnqualId = no,
        Id = mq_id(SymName, OrigArity),
        (
            MaybePredOrFunc = yes(PredOrFunc),
            adjust_func_arity(PredOrFunc, OrigArity, Arity),
            Pieces = [words("mode declaration for"),
                fixed(pred_or_func_to_full_str(PredOrFunc)),
                unqual_sym_name_arity(sym_name_arity(SymName, Arity))]
        ;
            MaybePredOrFunc = no,
            Pieces = [words("mode declaration for"),
                unqual_sym_name_arity(sym_name_arity(SymName, OrigArity))]
        )
    ;
        ErrorContext = mqec_lambda_expr(Context),
        ShouldUnqualId = no,
        Pieces = [words("mode declaration for lambda expression")]
    ;
        ErrorContext = mqec_clause_mode_annotation(Context),
        ShouldUnqualId = no,
        Pieces = [words("clause mode annotation")]
    ;
        ErrorContext = mqec_foreign_proc(Context),
        ShouldUnqualId = no,
        Pieces = [pragma_decl("foreign_proc"), words("declaration")]
    ;
        ErrorContext = mqec_foreign_enum(Context),
        ShouldUnqualId = yes,
        Pieces = [pragma_decl("foreign_enum"), words("declaration")]
    ;
        ErrorContext = mqec_foreign_export_enum(Context),
        ShouldUnqualId = no,
        Pieces = [pragma_decl("foreign_export_enum"), words("declaration")]
    ;
        ErrorContext = mqec_pragma_decl(Context, DeclPragma),
        ShouldUnqualId = no,
        Pieces = decl_pragma_desc_pieces(DeclPragma)
    ;
        ErrorContext = mqec_pragma_impl(Context, ImplPragma),
        ShouldUnqualId = no,
        Pieces = impl_pragma_desc_pieces(ImplPragma)
    ;
        ErrorContext = mqec_type_qual(Context),
        ShouldUnqualId = no,
        Pieces = [words("explicit type qualification")]
    ;
        ErrorContext = mqec_class(Context, ClassId),
        ShouldUnqualId = no,
        Pieces = [words("declaration of typeclass"), unqual_class_id(ClassId)]
    ;
        ErrorContext = mqec_instance(Context, ClassId),
        ShouldUnqualId = no,
        Pieces = [words("declaration of instance of typeclass"),
            qual_class_id(ClassId)]
    ;
        ErrorContext = mqec_mutable(Context, Name),
        ShouldUnqualId = no,
        Pieces = [words("declaration for mutable"), quote(Name)]
    ;
        ErrorContext = mqec_type_repn(Context, TypeCtor),
        ShouldUnqualId = no,
        Pieces = [words("representation information for type"),
            unqual_type_ctor(TypeCtor)]
    ;
        ErrorContext = mqec_event_spec_attr(Context, EventName, AttrName),
        ShouldUnqualId = no,
        Pieces = [words("attribute"), quote(AttrName),
            words("for"), quote(EventName)]
    ).

:- pred qual_id_kind_to_string(qual_id_kind::in, string::out) is det.

qual_id_kind_to_string(qual_id_type, "type").
qual_id_kind_to_string(qual_id_mode, "mode").
qual_id_kind_to_string(qual_id_inst, "inst").
qual_id_kind_to_string(qual_id_class, "typeclass").

:- pred qual_id_kinds_to_string(qual_id_kind::in, string::out) is det.

qual_id_kinds_to_string(qual_id_type, "types").
qual_id_kinds_to_string(qual_id_mode, "modes").
qual_id_kinds_to_string(qual_id_inst, "insts").
qual_id_kinds_to_string(qual_id_class, "typeclasses").

%---------------------------------------------------------------------------%

:- func wrap_module_name(module_name) = format_piece.

wrap_module_name(SymName) = qual_sym_name(SymName).

:- func wrap_qual_id(mq_id) = format_piece.

wrap_qual_id(mq_id(SymName, Arity)) =
    qual_sym_name_arity(sym_name_arity(SymName, Arity)).

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.qual_errors.
%---------------------------------------------------------------------------%
