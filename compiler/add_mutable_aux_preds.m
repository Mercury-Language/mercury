%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_mutable_aux_preds.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.error_util.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Handle mutable items during pass 1 of making the HLDS.
    %
:- pred add_mutable_aux_pred_decls(item_mutable_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Handle mutable items during pass 2 of making the HLDS.
    %
:- pred do_mutable_checks(item_mutable_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Handle mutable items during pass 3 of making the HLDS.
    %
:- pred add_mutable_aux_pred_defns(item_mutable_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_pragma.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_mutable.
:- import_module parse_tree.prog_util.

:- import_module varset.
:- import_module pair.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

add_mutable_aux_pred_decls(ItemMutable, Status, !ModuleInfo, !Specs) :-
    ItemMutable = item_mutable_info(Name, Type, _InitValue, Inst, MutAttrs,
        _VarSet, Context, _SeqNum),
    module_info_get_name(!.ModuleInfo, ModuleName),

    % The predicate declarations we produce depends on the compilation
    % target, which use different source-to-source transformations for
    % mutables.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        WantPreInitDecl = yes,
        WantLockDecls = yes,
        WantUnsafeAccessDecls = yes
    ;
        CompilationTarget = target_csharp,
        IsThreadLocal = mutable_var_thread_local(MutAttrs),
        (
            IsThreadLocal = mutable_thread_local,
            WantPreInitDecl = yes
        ;
            IsThreadLocal = mutable_not_thread_local,
            WantPreInitDecl = no
        ),
        WantLockDecls = no,
        WantUnsafeAccessDecls = yes
    ;
        CompilationTarget = target_java,
        WantPreInitDecl = no,
        WantLockDecls = no,
        WantUnsafeAccessDecls = yes
    ;
        CompilationTarget = target_erlang,
        WantPreInitDecl = no,
        WantLockDecls = no,
        WantUnsafeAccessDecls = no
    ;
        CompilationTarget = target_il,
        % Not supported yet.
        WantPreInitDecl = yes,
        WantLockDecls = yes,
        WantUnsafeAccessDecls = yes
    ),

    % Create the mutable initialisation predicate.
    add_mutable_init_pred_decl(ModuleName, Name, Status,
        Context, !ModuleInfo, !Specs),

    IsConstant = mutable_var_constant(MutAttrs),
    (
        IsConstant = no,

        % Create the pre-initialisation predicate. This is called
        % by the mutable initialisation predicate.
        (
            WantPreInitDecl = yes,
            add_mutable_pre_init_pred_decl(ModuleName, Name, Status,
                Context, !ModuleInfo, !Specs)
        ;
            WantPreInitDecl = no
        ),

        % Create the primitive access and locking predicates.
        (
            WantLockDecls = yes,
            add_mutable_lock_pred_decl(ModuleName, Name,
                Status, Context, !ModuleInfo, !Specs),
            add_mutable_unlock_pred_decl(ModuleName, Name,
                Status, Context, !ModuleInfo, !Specs)
        ;
            WantLockDecls = no
        ),
        (
            WantUnsafeAccessDecls = yes,
            add_mutable_unsafe_get_pred_decl(ModuleName, Name, Type, Inst,
                Status, Context, !ModuleInfo, !Specs),
            add_mutable_unsafe_set_pred_decl(ModuleName, Name, Type, Inst,
                Status, Context, !ModuleInfo, !Specs)
        ;
            WantUnsafeAccessDecls = no
        ),

        % Create the standard, non-pure access predicates. These are
        % always created for non-constant mutables, even if the
        % `attach_to_io_state' attribute has been specified.
        StdGetPredDecl = std_get_pred_decl(ModuleName, Name,
            Type, Inst, Context),
        StdSetPredDecl = std_set_pred_decl(ModuleName, Name,
            Type, Inst, Context),
        add_pred_decl_info_for_mutable_aux_pred(StdGetPredDecl,
            ModuleName, Name, mutable_pred_std_get, Status,
            !ModuleInfo, !Specs),
        add_pred_decl_info_for_mutable_aux_pred(StdSetPredDecl,
            ModuleName, Name, mutable_pred_std_set, Status,
            !ModuleInfo, !Specs),

        % If requested, create the pure access predicates using
        % the I/O state as well.
        CreateIOInterface = mutable_var_attach_to_io_state(MutAttrs),
        (
            CreateIOInterface = yes,
            IOGetPredDecl = io_get_pred_decl(ModuleName, Name,
                Type, Inst, Context),
            IOSetPredDecl = io_set_pred_decl(ModuleName, Name,
                Type, Inst, Context),
            add_pred_decl_info_for_mutable_aux_pred(IOGetPredDecl,
                ModuleName, Name, mutable_pred_io_get, Status,
                !ModuleInfo, !Specs),
            add_pred_decl_info_for_mutable_aux_pred(IOSetPredDecl,
                ModuleName, Name, mutable_pred_io_set, Status,
                !ModuleInfo, !Specs)
        ;
            CreateIOInterface = no
        )
    ;
        IsConstant = yes,

        % We create the "get" access predicate, which is pure since
        % it always returns the same value, but we must also create
        % a secret "set" predicate for use by the initialization code.
        ConstantGetPredDecl = constant_get_pred_decl(ModuleName, Name,
            Type, Inst, Context),
        ConstantSetPredDecl = constant_set_pred_decl(ModuleName, Name,
            Type, Inst, Context),
        add_pred_decl_info_for_mutable_aux_pred(ConstantGetPredDecl,
            ModuleName, Name, mutable_pred_constant_get, Status,
            !ModuleInfo, !Specs),
        add_pred_decl_info_for_mutable_aux_pred(ConstantSetPredDecl,
            ModuleName, Name, mutable_pred_constant_secret_set, Status,
            !ModuleInfo, !Specs)
    ).

%-----------------------------------------------------------------------------%

    % Add predmode declarations for the four primitive operations.
    %
:- pred add_mutable_unsafe_get_pred_decl(module_name::in, string::in,
    mer_type::in, mer_inst::in, item_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred add_mutable_unsafe_set_pred_decl(module_name::in, string::in,
    mer_type::in, mer_inst::in, item_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred add_mutable_lock_pred_decl(module_name::in, string::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred add_mutable_unlock_pred_decl(module_name::in, string::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mutable_unsafe_get_pred_decl(ModuleName, Name, Type, Inst, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_unsafe_get_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_unsafe_get,
        PredName, ArgTypesAndModes, purity_semipure, Status, Context,
        !ModuleInfo, !Specs).

add_mutable_unsafe_set_pred_decl(ModuleName, Name, Type, Inst, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_unsafe_set_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_unsafe_set,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

add_mutable_lock_pred_decl(ModuleName, Name, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_lock_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_lock,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

add_mutable_unlock_pred_decl(ModuleName, Name, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_unlock_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_unlock,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

    % Add a predmode declaration for the mutable initialisation predicate.
    %
:- pred add_mutable_init_pred_decl(module_name::in, string::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mutable_init_pred_decl(ModuleName, Name, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_init_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_pre_init,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

    % Add a predmode declaration for the mutable pre-initialisation
    % predicate. For normal mutables, this initialises the mutex protecting
    % the mutable. For thread-local mutables, this allocates an index
    % into an array of thread-local mutable values.
    %
:- pred add_mutable_pre_init_pred_decl(module_name::in, string::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mutable_pre_init_pred_decl(ModuleName, Name, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_pre_init_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_pre_init,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred add_mutable_aux_pred_decl(module_name::in, string::in,
    mutable_pred_kind::in, sym_name::in, list(type_and_mode)::in, purity::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mutable_aux_pred_decl(ModuleName, Name, Kind, PredName, ArgTypesAndModes,
        Purity, Status, Context, !ModuleInfo, !Specs) :-
    PredOrigin = origin_mutable(ModuleName, Name, Kind),
    TypeVarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    marker_list_to_markers([marker_mutable_access_pred], Markers),
    module_add_pred_or_func(PredOrigin, TypeVarSet, InstVarSet, ExistQVars,
        pf_predicate, PredName, ArgTypesAndModes, yes(detism_det),
        Purity, Constraints, Markers, Context, Status, _, !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred add_pred_decl_info_for_mutable_aux_pred(item_pred_decl_info::in,
    module_name::in, string::in, mutable_pred_kind::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pred_decl_info_for_mutable_aux_pred(ItemPredDecl, ModuleName, Name, Kind,
        Status, !ModuleInfo, !Specs) :-
    PredOrigin = origin_mutable(ModuleName, Name, Kind),
    ItemPredDecl = item_pred_decl_info(_Origin, TypeVarSet, InstVarSet,
        ExistQVars, PredOrFunc, PredName, TypesAndModes, WithType, WithInst,
        MaybeDet, Purity, Constraints, Context, _SeqNum),
    expect(unify(TypeVarSet, varset.init), $module, $pred,
        "TypeVarSet != varset.init"),
    expect(unify(InstVarSet, varset.init), $module, $pred,
        "InstVarSet != varset.init"),
    expect(unify(ExistQVars, []), $module, $pred, "ExistQVars != []"),
    expect(unify(PredOrFunc, pf_predicate), $module, $pred,
        "PredOrFunc != pf_predicate"),
    expect(unify(WithType, no), $module, $pred, "WithType != no"),
    expect(unify(WithInst, no), $module, $pred, "WithInst != no"),
    expect(unify(MaybeDet, yes(detism_det)), $module, $pred,
        "MaybeDet != yes(detism_det)"),
    expect(unify(Constraints, constraints([], [])), $module, $pred,
        "Constraints != constraints([], [])"),
    marker_list_to_markers([marker_mutable_access_pred], Markers),
    module_add_pred_or_func(PredOrigin, TypeVarSet, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes, MaybeDet, Purity, Constraints,
        Markers, Context, Status, _, !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

do_mutable_checks(ItemMutable, Status, !ModuleInfo, !Specs) :-
    ItemMutable = item_mutable_info(Name, _Type, _InitTerm, Inst,
        MutAttrs, _VarSet, Context, _SeqNum),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, CompilationTarget),

    % XXX We don't currently support the foreign_name attribute
    % for all languages.
    (
        % If we are creating the I/O version of the set predicate then we
        % need to add a promise_pure pragma for it. This needs to be done
        % here (in stage 2) rather than in stage 3 where the rest of the
        % mutable transformation is.
        % ZZZ The code that generates the io set predicate for the C
        % backend has been updated not to need this pragma, by putting
        % a promise_pure scope around a conjunction that both updates the
        % global variable and updates the I/O state. The other backends
        % still need it, but the right fix for that is NOT to update them
        % too, but to factor out the common code, so that the required
        % purity promises, and much else, are generated in just one place,
        % and not several.
        (
            CompilationTarget = target_c,
            ForeignLanguage = lang_c,
            NeedPromisePurePragma = no
        ;
            CompilationTarget = target_java,
            ForeignLanguage = lang_java,
            NeedPromisePurePragma =
                mutable_var_attach_to_io_state(MutAttrs)
        ;
            CompilationTarget = target_csharp,
            ForeignLanguage = lang_csharp,
            NeedPromisePurePragma =
                mutable_var_attach_to_io_state(MutAttrs)
        ;
            CompilationTarget = target_erlang,
            ForeignLanguage = lang_erlang,
            NeedPromisePurePragma =
                mutable_var_attach_to_io_state(MutAttrs)
        ),
        mutable_var_maybe_foreign_names(MutAttrs) = MaybeForeignNames,
        module_info_get_name(!.ModuleInfo, ModuleName),
        (
            MaybeForeignNames = no
        ;
            MaybeForeignNames = yes(ForeignNames),
            % Report any errors with the foreign_name attributes
            % during this pass.
            get_global_name_from_foreign_names(!.ModuleInfo, Context,
                ModuleName, Name, ForeignLanguage, ForeignNames,
                _TargetMutableName, !Specs)
        ),

        (
            NeedPromisePurePragma = yes,
            SetPredName = mutable_set_pred_sym_name(ModuleName, Name),
            SetPredNameArity = pred_name_arity(SetPredName, 3),
            IOSetPromisePurePragma = pragma_promise_pure(SetPredNameArity),
            IOSetPromisePureItemPragma = item_pragma_info(
                item_origin_compiler(item_compiler_attributes(
                    do_allow_export, is_mutable)),
                IOSetPromisePurePragma, Context, -1),
            add_pass_2_pragma(IOSetPromisePureItemPragma, Status,
                !ModuleInfo, !Specs)
        ;
            NeedPromisePurePragma = no
        )
    ;
        CompilationTarget = target_il,
        Pieces = [words("Error: foreign_name mutable attribute"),
            words("is not yet implemented for the"),
            fixed(compilation_target_string(CompilationTarget)),
            words("backend."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ),

    % Check that the inst in the mutable declaration is a valid inst for a
    % mutable declaration.
    ( if is_valid_mutable_inst(!.ModuleInfo, Inst) then
        true
    else
        % It is okay to pass a dummy varset in here since any attempt
        % to use inst variables in a mutable declaration should already
        % been dealt with when the mutable declaration was parsed.
        DummyInstVarset = varset.init,
        InstStr = mercury_expanded_inst_to_string(Inst, DummyInstVarset,
            !.ModuleInfo),
        InvalidInstPieces = [words("Error: the inst"), quote(InstStr),
            words("is not a valid inst for a"),
            decl("mutable"), words("declaration.")],
        % XXX We could provide more information about exactly *why* the
        % inst was not valid here as well.
        InvalidInstMsg = simple_msg(Context, [always(InvalidInstPieces)]),
        InvalidInstSpec = error_spec(severity_error,
            phase_parse_tree_to_hlds, [InvalidInstMsg]),
        !:Specs = [InvalidInstSpec | !.Specs]
    ).

    % Check to see if there is a valid foreign_name attribute for this backend.
    % If so, use it as the name of the global variable in the target code,
    % otherwise take the Mercury name for the mutable and mangle it into
    % an appropriate variable name.
    %
 :- pred get_global_name_from_foreign_names(module_info::in,
    prog_context::in, module_name::in, string::in, foreign_language::in,
    list(foreign_name)::in, string::out,
    list(error_spec)::in, list(error_spec)::out) is det.

get_global_name_from_foreign_names(ModuleInfo, Context,
        ModuleName, MercuryMutableName, ForeignLanguage, ForeignNames,
        TargetMutableName, !Specs) :-
    get_matching_foreign_names(ForeignNames, ForeignLanguage,
        TargetMutableNames),
    (
        TargetMutableNames = [],
        % This works for Erlang as well.
        TargetMutableName = mutable_c_var_name(ModuleName, MercuryMutableName)
    ;
        TargetMutableNames = [foreign_name(_, TargetMutableName)]
        % XXX We should really check that this is a valid identifier
        % in the target language here.
    ;
        TargetMutableNames = [_, _ | _],
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, CompilationTarget),
        Pieces = [words("Error: multiple foreign_name attributes"),
            words("specified for the"),
            fixed(compilation_target_string(CompilationTarget)),
            words("backend."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs],

        % This works for Erlang as well.
        TargetMutableName = mutable_c_var_name(ModuleName, MercuryMutableName)
    ).

:- pred get_matching_foreign_names(list(foreign_name)::in,
    foreign_language::in, list(foreign_name)::out) is det.

get_matching_foreign_names([], _TargetForeignLanguage, []).
get_matching_foreign_names([ForeignName | ForeignNames], TargetForeignLanguage,
        MatchingForeignNames) :-
    get_matching_foreign_names(ForeignNames, TargetForeignLanguage,
        TailMatchingForeignNames),
    ForeignName = foreign_name(ForeignLanguage, _),
    ( if ForeignLanguage = TargetForeignLanguage then
        MatchingForeignNames = [ForeignName | TailMatchingForeignNames]
    else
        MatchingForeignNames = TailMatchingForeignNames
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

add_mutable_aux_pred_defns(ItemMutable, Status, !ModuleInfo,
        !QualInfo, !Specs) :-
    ItemMutable = item_mutable_info(MercuryMutableName, Type, _InitTerm, _Inst,
        MutAttrs, _VarSet, Context, _SeqNum),
    IsConstant = mutable_var_constant(MutAttrs),
    module_info_get_name(!.ModuleInfo, ModuleName),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,

        % Work out what name to give the global in the target language.
        decide_mutable_target_var_name(!.ModuleInfo, MutAttrs,
            ModuleName, MercuryMutableName, lang_c, Context,
            TargetMutableName),

        % Add foreign_decl and foreign_code items that declare/define the
        % global variable used to implement the mutable. If the mutable is
        % not constant, then add a mutex to synchronize access to it as well.
        IsThreadLocal = mutable_var_thread_local(MutAttrs),
        add_c_mutable_defn_and_decl(TargetMutableName, Type, IsConstant,
            IsThreadLocal, Context, !ModuleInfo),

        % Add all the predicates related to mutables.
        add_c_mutable_preds(ItemMutable, TargetMutableName,
            Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        (
            CompilationTarget = target_java,
            Lang = lang_java
        ;
            CompilationTarget = target_csharp,
            Lang = lang_csharp
        ),

        % Work out what name to give the global in the target language.
        decide_mutable_target_var_name(!.ModuleInfo, MutAttrs,
            ModuleName, MercuryMutableName, Lang, Context,
            TargetMutableName),

        % Add foreign_code item that defines the global variable used to
        % implement the mutable.
        IsThreadLocal = mutable_var_thread_local(MutAttrs),
        add_csharp_java_mutable_defn(Lang, TargetMutableName, Type,
            IsThreadLocal, Context, !ModuleInfo),

        % Add all the predicates related to mutables.
        add_csharp_java_mutable_preds(ItemMutable, Lang, TargetMutableName,
            Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        CompilationTarget = target_erlang,

        % Work out what name to give the global in the target language.
        decide_mutable_target_var_name(!.ModuleInfo, MutAttrs,
            ModuleName, MercuryMutableName, lang_erlang, Context,
            TargetMutableName),

        % Add all the predicates related to mutables.
        add_erlang_mutable_preds(ItemMutable, TargetMutableName,
            Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        CompilationTarget = target_il
        % Not supported yet.
    ).

    % Decide what the name of the underlying global used to implement the
    % mutable should be. If there is a foreign_name attribute then use that
    % otherwise construct one based on the Mercury name for the mutable.
    %
:- pred decide_mutable_target_var_name(module_info::in,
    mutable_var_attributes::in, module_name::in, string::in,
    foreign_language::in, prog_context::in, string::out) is det.

decide_mutable_target_var_name(ModuleInfo, MutAttrs, ModuleName, Name,
        ForeignLanguage, Context, TargetMutableName) :-
    mutable_var_maybe_foreign_names(MutAttrs) = MaybeForeignNames,
    (
        MaybeForeignNames = no,
        % This works for Erlang as well.
        TargetMutableName = mutable_c_var_name(ModuleName, Name)
    ;
        MaybeForeignNames = yes(ForeignNames),
        % We have already any errors during pass 2, so ignore them here.
        get_global_name_from_foreign_names(ModuleInfo, Context,
            ModuleName, Name, ForeignLanguage, ForeignNames, TargetMutableName,
            [], _Specs)
    ).

:- pred add_pass_3_initialise_for_mutable(sym_name::in, arity::in,
    prog_context::in, import_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_initialise_for_mutable(SymName, Arity, Context, _Status,
        !ModuleInfo, !Specs) :-
    % The compiler introduces initialise declarations that call impure
    % predicates as part of the source-to-source transformation for mutable
    % variables. These predicates *must* be impure in order to prevent the
    % compiler optimizing them away.

    module_info_get_globals(!.ModuleInfo, Globals),
    % ZZZ our caller should know the language
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        MaybeExportLang = yes(lang_c)
    ;
        CompilationTarget = target_java,
        MaybeExportLang = yes(lang_java)
    ;
        CompilationTarget = target_csharp,
        MaybeExportLang = yes(lang_csharp)
    ;
        CompilationTarget = target_erlang,
        MaybeExportLang = yes(lang_erlang)
    ;
        % Untested.
        % ZZZ
        CompilationTarget = target_il,
        MaybeExportLang = no
    ),
    (
        MaybeExportLang = yes(ExportLang),
        module_info_new_user_init_pred(SymName, Arity, CName, !ModuleInfo),
        PredNameModesPF = pred_name_modes_pf(SymName, [], pf_predicate),
        FPEInfo = pragma_info_foreign_proc_export(ExportLang,
            PredNameModesPF, CName),
        Attrs = item_compiler_attributes(do_allow_export, is_mutable),
        Origin = item_origin_compiler(Attrs),
        add_pragma_foreign_proc_export(Origin, FPEInfo, Context,
            !ModuleInfo, !Specs)
        % ZZZ Should get PredId
    ;
        MaybeExportLang = no
    ).

%---------------------------------------------------------------------------%
%
% C mutables.
%

    % Add the foreign_decl and foreign_code items that declare/define
    % the global variable used to hold the mutable. The bool argument says
    % whether the mutable is a constant mutable or not.
    %
:- pred add_c_mutable_defn_and_decl(string::in, mer_type::in, bool::in,
    mutable_thread_local::in, prog_context::in,
    module_info::in, module_info::out) is det.

add_c_mutable_defn_and_decl(TargetMutableName, Type, IsConstant, IsThreadLocal,
        Context, !ModuleInfo) :-
    % We add the foreign code declaration and definition here rather than
    % in pass 2 because the target-language-specific type name depends on
    % whether there are any foreign_type declarations for Type.

    % The declaration we construct will be included in the .mh files. Since
    % these are grade independent, we need to output both the high- and
    % low-level C declarations for the global used to implement the mutable,
    % and make the choice conditional on whether MR_HIGHLEVEL_CODE is defined.
    (
        IsThreadLocal = mutable_not_thread_local,
        % The only difference between the high- and low-level C backends
        % is that in the latter, mutables are *always* boxed, whereas
        % in the former they may not be.
        HighLevelTypeName = global_foreign_type_name(no, lang_c, !.ModuleInfo,
            Type),
        LowLevelTypeName = global_foreign_type_name(yes, lang_c, !.ModuleInfo,
            Type),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = no,
            TypeName = LowLevelTypeName
        ;
            HighLevelCode = yes,
            TypeName = HighLevelTypeName
        )
    ;
        IsThreadLocal = mutable_thread_local,
        % For thread-local mutables, the variable holds an index into an
        % array.
        TypeName = "MR_Unsigned",
        HighLevelTypeName = TypeName,
        LowLevelTypeName  = TypeName
    ),

    % Constant mutables do not require mutexes, as their values are never
    % updated. Thread-local mutables do not require mutexes either.
    ( if
        ( IsConstant = yes
        ; IsThreadLocal = mutable_thread_local
        )
    then
        LockDeclStrs = [],
        LockDefnStrs = []
    else
        MutexVarName = mutable_mutex_var_name(TargetMutableName),
        LockDeclStrs = [
            "#ifdef MR_THREAD_SAFE\n",
            "    extern MercuryLock ", MutexVarName, ";\n",
            "#endif\n"
        ],
        LockDefnStrs = [
            "#ifdef MR_THREAD_SAFE\n",
            "    MercuryLock ", MutexVarName, ";\n",
            "#endif\n"
        ]
    ),

    DeclBody = string.append_list([
        "#ifdef MR_HIGHLEVEL_CODE\n",
        "    extern ", HighLevelTypeName, " ", TargetMutableName, ";\n",
        "#else\n",
        "    extern ", LowLevelTypeName, " ", TargetMutableName, ";\n",
        "#endif\n" | LockDeclStrs]),
    ForeignDeclCode = foreign_decl_code(lang_c, foreign_decl_is_exported,
        literal(DeclBody), Context),
    module_add_foreign_decl_code(ForeignDeclCode, !ModuleInfo),

    DefnBody = string.append_list([
        TypeName, " ", TargetMutableName, ";\n" | LockDefnStrs]),
    ForeignBodyCode = foreign_body_code(lang_c, literal(DefnBody), Context),
    module_add_foreign_body_code(ForeignBodyCode, !ModuleInfo).

    % The first argument of global_foreign_type_name says whether the mutable
    % should always be boxed or not. The only difference between the high- and
    % low-level C backends is that in the latter mutables are *always* boxed,
    % whereas in the former they may not be.
    %
:- func global_foreign_type_name(bool, foreign_language, module_info, mer_type)
    = string.

global_foreign_type_name(yes, _, _, _) = "MR_Word".
global_foreign_type_name(no, Lang, ModuleInfo, Type) =
    mercury_exported_type_to_string(ModuleInfo, Lang, Type).

%---------------------------------------------------------------------------%

:- pred add_c_mutable_preds(item_mutable_info::in, string::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_c_mutable_preds(ItemMutableInfo, TargetMutableName, Status, !ModuleInfo,
        !QualInfo, !Specs) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ItemMutableInfo = item_mutable_info(MercuryMutableName, Type, InitTerm,
        Inst, MutAttrs, VarSet, Context, _SeqNum),
    IsConstant = mutable_var_constant(MutAttrs),
    IsThreadLocal = mutable_var_thread_local(MutAttrs),

    % Set up the default attributes for the foreign_procs used for the
    % access predicates.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    (
        HighLevelCode = no,
        AlwaysBoxed = yes,
        BoxPolicy = always_boxed
    ;
        HighLevelCode = yes,
        AlwaysBoxed = no,
        BoxPolicy = native_if_possible
    ),
    Attrs0 = default_attributes(lang_c),
    set_box_policy(BoxPolicy, Attrs0, Attrs1),
    set_may_call_mercury(proc_will_not_call_mercury, Attrs1, Attrs),

    (
        IsConstant = yes,
        InitSetPredName = mutable_secret_set_pred_sym_name(ModuleName,
            MercuryMutableName),
        add_ccsj_constant_mutable_access_preds(TargetMutableName,
            ModuleName, MercuryMutableName, Attrs, Inst, BoxPolicy,
            Context, Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        IsConstant = no,
        InitSetPredName = mutable_set_pred_sym_name(ModuleName,
            MercuryMutableName),
        TypeName = global_foreign_type_name(AlwaysBoxed, lang_c,
            !.ModuleInfo, Type),
        add_c_mutable_primitive_preds(TargetMutableName, ModuleName,
            MercuryMutableName, MutAttrs, Attrs, Inst, BoxPolicy, TypeName,
            Context, Status, !ModuleInfo, !QualInfo, !Specs),
        add_ccsj_mutable_user_access_preds(ModuleName, MercuryMutableName,
            MutAttrs, lang_c, Context, Status, !ModuleInfo, !QualInfo, !Specs)
    ),
    add_c_mutable_initialisation(IsConstant, IsThreadLocal,
        TargetMutableName, ModuleName, MercuryMutableName, VarSet,
        InitSetPredName, InitTerm, Attrs,
        Context, Status, !ModuleInfo, !QualInfo, !Specs).

    % Add the access predicates for constant mutables.
    % Shared between C, C# and Java.
    %
:- pred add_ccsj_constant_mutable_access_preds(string::in, module_name::in,
    string::in, pragma_foreign_proc_attributes::in, mer_inst::in,
    box_policy::in, prog_context::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_ccsj_constant_mutable_access_preds(TargetMutableName,
        ModuleName, MutableName, Attrs, Inst, BoxPolicy, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("X", X, varset.init, VarSet),
    InstVarSet = varset.init,
    set_purity(purity_pure, Attrs, ConstantGetAttrs0),
    set_thread_safe(proc_thread_safe, ConstantGetAttrs0, ConstantGetAttrs),
    ConstantGetFCInfo = pragma_info_foreign_proc(
        ConstantGetAttrs,
        mutable_get_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
        VarSet,
        InstVarSet,
        fp_impl_ordinary("X = " ++ TargetMutableName ++ ";\n", yes(Context))
    ),
    add_pragma_foreign_proc(ConstantGetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs),

    % NOTE: we don't need to trail the set action, since it is executed
    % only once at initialization time.

    ConstantSetFCInfo = pragma_info_foreign_proc(Attrs,
        mutable_secret_set_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), BoxPolicy)],
        VarSet,
        InstVarSet,
        fp_impl_ordinary(TargetMutableName ++ " = X;\n", yes(Context))
    ),
    add_pragma_foreign_proc(ConstantSetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs).

    % Add the foreign clauses for the mutable's primitive access and
    % locking predicates.
    %
:- pred add_c_mutable_primitive_preds(string::in, module_name::in, string::in,
    mutable_var_attributes::in, pragma_foreign_proc_attributes::in,
    mer_inst::in, box_policy::in, string::in, prog_context::in,
    import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_c_mutable_primitive_preds(TargetMutableName, ModuleName, MutableName,
        MutAttrs, Attrs, Inst, BoxPolicy, TypeName, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    IsThreadLocal = mutable_var_thread_local(MutAttrs),
    set_thread_safe(proc_thread_safe, Attrs, LockAndUnlockAttrs),

    % Construct the lock predicate.

    MutableMutexVarName = mutable_mutex_var_name(TargetMutableName),
    % XXX the second argument should be the name of the mercury predicate,
    % with chars escaped as appropriate.
    (
        IsThreadLocal = mutable_not_thread_local,
        LockForeignProcBody = string.append_list([
            "#ifdef MR_THREAD_SAFE\n",
            "  MR_LOCK(&" ++ MutableMutexVarName ++ ",
                \"" ++ MutableMutexVarName ++ "\");\n" ++
            "#endif\n"
        ])
    ;
        IsThreadLocal = mutable_thread_local,
        LockForeignProcBody = ""
    ),
    LockFCInfo = pragma_info_foreign_proc(LockAndUnlockAttrs,
        mutable_lock_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [],
        varset.init,    % Prog varset.
        varset.init,    % Inst varset.
        fp_impl_ordinary(LockForeignProcBody, yes(Context))
    ),
    add_pragma_foreign_proc(LockFCInfo, Status, Context, no,
        !ModuleInfo, !Specs),

    % Construct the unlock predicate.
    % XXX as above regarding the second argument to MR_UNLOCK.

    (
        IsThreadLocal = mutable_not_thread_local,
        UnlockForeignProcBody = string.append_list([
            "#ifdef MR_THREAD_SAFE\n",
            "  MR_UNLOCK(&" ++ MutableMutexVarName ++ ",
                \"" ++ MutableMutexVarName ++ "\");\n" ++
            "#endif\n"
        ])
    ;
        IsThreadLocal = mutable_thread_local,
        UnlockForeignProcBody = ""
    ),
    UnlockFCInfo = pragma_info_foreign_proc(LockAndUnlockAttrs,
        mutable_unlock_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [],
        varset.init,    % Prog varset.
        varset.init,    % Inst varset.
        fp_impl_ordinary(UnlockForeignProcBody, yes(Context))
    ),
    add_pragma_foreign_proc(UnlockFCInfo, Status, Context, no,
        !ModuleInfo, !Specs),

    % Construct the semipure unsafe_get_predicate.

    set_purity(purity_semipure, Attrs, UnsafeGetAttrs0),
    set_thread_safe(proc_thread_safe, UnsafeGetAttrs0, UnsafeGetAttrs),
    varset.new_named_var("X", X, varset.init, VarSet),
    (
        IsThreadLocal = mutable_not_thread_local,
        UnsafeGetCode = "X = " ++ TargetMutableName ++ ";\n"
    ;
        IsThreadLocal = mutable_thread_local,
        UnsafeGetCode = "MR_get_thread_local_mutable(" ++
            TypeName ++ ", X, " ++ TargetMutableName ++ ");\n"
    ),
    UnsafeGetFCInfo = pragma_info_foreign_proc(UnsafeGetAttrs,
        mutable_unsafe_get_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
        VarSet,
        varset.init, % Inst varset.
        fp_impl_ordinary(UnsafeGetCode, yes(Context))
    ),
    add_pragma_foreign_proc(UnsafeGetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs),

    % Construct the impure unsafe_set_predicate.

    set_thread_safe(proc_thread_safe, Attrs, UnsafeSetAttrs),
    TrailMutableUpdates = mutable_var_trailed(MutAttrs),
    (
        TrailMutableUpdates = mutable_untrailed,
        TrailCode = ""
    ;
        TrailMutableUpdates = mutable_trailed,

        % If we require that the mutable to be trailed then we need to be
        % compiling in a trailing grade.
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, use_trail, UseTrail),
        (
            UseTrail = yes,
            TrailCode = "MR_trail_current_value(&" ++
                TargetMutableName ++ ");\n"
        ;
            UseTrail = no,
            Pieces =
                [words("Error: trailed mutable in non-trailing grade."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs],

            % This is just a dummy value.
            TrailCode = ""
        )
    ),
    (
        IsThreadLocal = mutable_not_thread_local,
        SetCode = TargetMutableName ++ " = X;\n"
    ;
        IsThreadLocal = mutable_thread_local,
        SetCode = "MR_set_thread_local_mutable(" ++
            TypeName ++ ", X, " ++ TargetMutableName ++ ");\n"
    ),
    UnsafeSetFCInfo = pragma_info_foreign_proc(UnsafeSetAttrs,
        mutable_unsafe_set_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), BoxPolicy)],
        VarSet,
        varset.init, % Inst varset.
        fp_impl_ordinary(TrailCode ++ SetCode, yes(Context))
    ),
    add_pragma_foreign_proc(UnsafeSetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs).

:- inst lang_ccsj
    --->    lang_c
    ;       lang_csharp
    ;       lang_java.

    % Add the access predicates for a non-constant mutable.
    % If the mutable has the `attach_to_io_state' attribute, then add the
    % versions of the access preds that take the I/O state as well.
    % Shared between C, C# and Java.
    %
:- pred add_ccsj_mutable_user_access_preds(module_name::in, string::in,
    mutable_var_attributes::in, foreign_language::in(lang_ccsj),
    prog_context::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_ccsj_mutable_user_access_preds(ModuleName, MutableName, MutAttrs,
        Lang, Context, Status, !ModuleInfo, !QualInfo, !Specs) :-
    varset.new_named_var("X", X, varset.init, VarSet0),

    LockPredName   = mutable_lock_pred_sym_name(ModuleName, MutableName),
    UnlockPredName = mutable_unlock_pred_sym_name(ModuleName, MutableName),
    CallLockExpr   = call_expr(Context, LockPredName, [], purity_impure),
    CallUnlockExpr = call_expr(Context, UnlockPredName, [], purity_impure),

    % ZZZ Ctxt
    GetterPredName = mutable_unsafe_get_pred_sym_name(ModuleName, MutableName),
    SetterPredName = mutable_unsafe_set_pred_sym_name(ModuleName, MutableName),
    CallGetterExpr = call_expr(Context, GetterPredName,
        [variable(X, Context)], purity_semipure),
    CallSetterExpr = call_expr(Context, SetterPredName,
        [variable(X, context_init)], purity_impure),

    GetPredName = mutable_get_pred_sym_name(ModuleName, MutableName),
    SetPredName = mutable_set_pred_sym_name(ModuleName, MutableName),

    (
        Lang = lang_c,
        ImpureGetExpr = goal_list_to_conj(Context,
            [CallLockExpr, CallGetterExpr, CallUnlockExpr]),
        ImpureSetExpr = goal_list_to_conj(Context,
            [CallLockExpr, CallSetterExpr, CallUnlockExpr])
    ;
        ( Lang = lang_java
        ; Lang = lang_csharp
        ),
        % There are no separate lock predicates for Java; the synchronisation
        % is performed within the "unsafe" predicates.
        % XXX C# needs investigation
        ImpureGetExpr = CallGetterExpr,
        ImpureSetExpr = CallSetterExpr
    ),

    % Construct the semipure get predicate.
    StdGetPredName = GetPredName,
    StdGetPredArgs = [variable(X, context_init)],
    StdGetPredExpr = promise_purity_expr(Context, purity_semipure,
        ImpureGetExpr),
    module_add_clause(VarSet0, pf_predicate, StdGetPredName, StdGetPredArgs,
        StdGetPredExpr, Status, Context, no, goal_type_none,
        !ModuleInfo, !QualInfo, !Specs),

    % Construct the impure set predicate.
    StdSetPredName = SetPredName,
    StdSetPredArgs = [variable(X, context_init)],
    StdSetPredExpr = ImpureSetExpr,
    module_add_clause(VarSet0, pf_predicate, StdSetPredName, StdSetPredArgs,
        StdSetPredExpr, Status, Context, no, goal_type_none,
        !ModuleInfo, !QualInfo, !Specs),

    IOStateInterface = mutable_var_attach_to_io_state(MutAttrs),
    (
        IOStateInterface = yes,
        varset.new_named_var("IO0", IO0, VarSet0, VarSet1),
        varset.new_named_var("IO", IO, VarSet1, VarSet),
        IOPredArgs = [variable(X, Context),
            variable(IO0, Context), variable(IO, Context)],
        CopyIOExpr = unify_expr(Context,
            variable(IO0, Context),
            variable(IO, Context),
            purity_impure),

        % Construct the pure get predicate.
        IOGetPredName = GetPredName,
        IOGetPredExpr = conj_expr(Context, ImpureGetExpr, CopyIOExpr),
        PureIOGetPredExpr =
            promise_purity_expr(Context, purity_pure, IOGetPredExpr),
        module_add_clause(VarSet, pf_predicate, IOGetPredName, IOPredArgs,
            PureIOGetPredExpr, Status, Context, no, goal_type_none,
            !ModuleInfo, !QualInfo, !Specs),

        % Construct the pure set predicate.
        %
        % It is important to have the copy of the I/O state *inside*
        % the promise_pure scope. If it were outside, then the scope would
        % not bind any variables, and since it is promised pure, the compiler
        % would be allowed to delete it.
        IOSetPredName = SetPredName,
        IOSetPredExpr = conj_expr(Context, ImpureSetExpr, CopyIOExpr),
        PureIOSetPredExpr =
            promise_purity_expr(Context, purity_pure, IOSetPredExpr),
        module_add_clause(VarSet, pf_predicate, IOSetPredName, IOPredArgs,
            PureIOSetPredExpr, Status, Context, no, goal_type_none,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        IOStateInterface = no
    ).

    % Add the code required to initialise a mutable.
    %
:- pred add_c_mutable_initialisation(bool::in, mutable_thread_local::in,
    string::in, module_name::in, string::in, prog_varset::in,
    sym_name::in, prog_term::in, pragma_foreign_proc_attributes::in,
    prog_context::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_c_mutable_initialisation(IsConstant, IsThreadLocal, TargetMutableName,
        ModuleName, MutableName, VarSet0, InitSetPredName, InitTerm, Attrs,
        Context, Status, !ModuleInfo, !QualInfo, !Specs) :-
    % Add the `:- initialise' declaration for the mutable initialisation
    % predicate.
    InitPredName = mutable_init_pred_sym_name(ModuleName, MutableName),
    InitPredArity = 0,
    add_pass_3_initialise_for_mutable(InitPredName, InitPredArity, Context,
        Status, !ModuleInfo, !Specs),

    % Add the clause for the mutable initialisation predicate.
    varset.new_named_var("X", X, VarSet0, VarSet),
    UnifyExpr =
        unify_expr(Context, variable(X, Context), InitTerm, purity_impure),
    (
        IsConstant = yes,
        CallExpr =
            call_expr(Context, InitSetPredName, [variable(X, Context)],
                purity_impure),
        InitPredExpr = conj_expr(Context, UnifyExpr, CallExpr)
    ;
        IsConstant = no,
        (
            IsThreadLocal = mutable_not_thread_local,
            % Construct the clause for the mutex initialisation predicate.
            PreInitCode = string.append_list([
                "#ifdef MR_THREAD_SAFE\n",
                "   pthread_mutex_init(&",
                        mutable_mutex_var_name(TargetMutableName),
                        ", MR_MUTEX_ATTR);\n",
                "#endif\n"
            ])
        ;
            IsThreadLocal = mutable_thread_local,
            PreInitCode = string.append_list([
                TargetMutableName,
                " = MR_new_thread_local_mutable_index();\n"
            ])
        ),
        PreInitPredName = mutable_pre_init_pred_sym_name(ModuleName,
            MutableName),
        PreInitFCInfo = pragma_info_foreign_proc(Attrs,
            PreInitPredName,
            pf_predicate,
            [],
            varset.init,    % VarSet
            varset.init,    % InstVarSet
            fp_impl_ordinary(PreInitCode, yes(Context))
        ),
        add_pragma_foreign_proc(PreInitFCInfo, Status, Context, no,
            !ModuleInfo, !Specs),

        CallPreInitExpr =
            call_expr(Context, PreInitPredName, [], purity_impure),
        CallSetPredExpr =
            call_expr(Context, InitSetPredName, [variable(X, Context)],
                purity_impure),
        InitPredExpr = goal_list_to_conj(Context,
            [CallPreInitExpr, UnifyExpr, CallSetPredExpr])
    ),

    % See the comments for prog_io.parse_mutable_decl for the reason
    % why we _must_ use Varset here.
    module_add_clause(VarSet, pf_predicate, InitPredName, [], InitPredExpr,
        Status, Context, no, goal_type_none, !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%
%
% C#/Java mutables.
%

:- inst lang_csharp_java
    --->    lang_csharp
    ;       lang_java.

    % Add foreign_code item that defines the global variable used to hold the
    % mutable.
    %
:- pred add_csharp_java_mutable_defn(foreign_language::in(lang_csharp_java),
    string::in, mer_type::in, mutable_thread_local::in, prog_context::in,
    module_info::in, module_info::out) is det.

add_csharp_java_mutable_defn(Lang, TargetMutableName, Type, IsThreadLocal,
        Context, !ModuleInfo) :-
    (
        Lang = lang_csharp,
        (
            IsThreadLocal = mutable_not_thread_local,
            ( if Type = int_type then
                TypeStr = "int"
            else
                TypeStr = "object"
            )
        ;
            IsThreadLocal = mutable_thread_local,
            TypeStr = "int"
        ),
        DefnBody = string.append_list([
            "static ", TypeStr, " ", TargetMutableName, ";\n"])
    ;
        Lang = lang_java,
        IsThreadLocal = mutable_not_thread_local,
        % Synchronization is only required for double and long values, which
        % Mercury does not expose. We could also use the volatile keyword.
        % (Java Language Specification, 2nd Ed., 17.4).
        ( if Type = int_type then
            TypeStr = "int"
        else
            TypeStr = "java.lang.Object"
        ),
        DefnBody = string.append_list([
            "static ", TypeStr, " ", TargetMutableName, ";\n"])
    ;
        Lang = lang_java,
        IsThreadLocal = mutable_thread_local,
        ( if Type = int_type then
            TypeStr = "java.lang.Integer"
        else
            TypeStr = "java.lang.Object"
        ),
        DefnBody = string.append_list([
            "static java.lang.ThreadLocal<", TypeStr, "> ",
            TargetMutableName,
            " = new java.lang.InheritableThreadLocal<", TypeStr, ">();\n"
        ])
    ),
    DefnForeignBodyCode = foreign_body_code(Lang, literal(DefnBody), Context),
    module_add_foreign_body_code(DefnForeignBodyCode, !ModuleInfo).

:- pred add_csharp_java_mutable_preds(item_mutable_info::in,
    foreign_language::in(lang_csharp_java), string::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_csharp_java_mutable_preds(ItemMutable, Lang, TargetMutableName, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ItemMutable = item_mutable_info(MercuryMutableName, Type, InitTerm, Inst,
        MutAttrs, Varset, Context, _SeqNum),
    IsConstant = mutable_var_constant(MutAttrs),
    Attrs0 = default_attributes(Lang),
    % The mutable variable name is not module-qualified so cannot be exported
    % to `.opt' files. We could add the qualification but it would be better
    % to move the mutable code generation into the backends first.
    set_may_duplicate(yes(proc_may_not_duplicate), Attrs0, Attrs),
    BoxPolicy = native_if_possible,
    (
        IsConstant = yes,
        InitSetPredName = mutable_secret_set_pred_sym_name(ModuleName,
            MercuryMutableName),
        add_ccsj_constant_mutable_access_preds(TargetMutableName,
            ModuleName, MercuryMutableName, Attrs, Inst, BoxPolicy,
            Context, Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        IsConstant = no,
        InitSetPredName = mutable_set_pred_sym_name(ModuleName,
            MercuryMutableName),
        add_csharp_java_mutable_primitive_preds(Lang, TargetMutableName,
            ModuleName, MercuryMutableName, Type, MutAttrs, Attrs, Inst,
            BoxPolicy, Context, Status, !ModuleInfo, !QualInfo, !Specs),
        add_ccsj_mutable_user_access_preds(ModuleName, MercuryMutableName,
            MutAttrs, Lang, Context, Status, !ModuleInfo, !QualInfo, !Specs)
    ),
    % The C# thread-local mutable implementation requires array indices to be
    % allocated in pre-init predicates.
    ( if
        Lang = lang_csharp,
        mutable_var_thread_local(MutAttrs) = mutable_thread_local
    then
        add_csharp_thread_local_mutable_pre_init_pred(TargetMutableName,
            ModuleName, MercuryMutableName, Attrs, CallPreInitExpr,
            Context, Status, !ModuleInfo, !QualInfo, !Specs)
    else
        CallPreInitExpr = true_expr(Context)
    ),
    add_csharp_java_mutable_initialisation(ModuleName, MercuryMutableName,
        Varset, CallPreInitExpr, InitSetPredName, InitTerm,
        Context, Status, !ModuleInfo, !QualInfo, !Specs).

:- pred add_csharp_thread_local_mutable_pre_init_pred(string::in,
    module_name::in, string::in, pragma_foreign_proc_attributes::in, goal::out,
    prog_context::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_csharp_thread_local_mutable_pre_init_pred(TargetMutableName,
        ModuleName, MutableName, Attrs, CallPreInitExpr, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    PreInitCode = string.append_list([
        TargetMutableName, " = runtime.ThreadLocalMutables.new_index();\n"
    ]),
    PreInitPredName = mutable_pre_init_pred_sym_name(ModuleName,
        MutableName),
    PreInitFCInfo = pragma_info_foreign_proc(Attrs,
        PreInitPredName,
        pf_predicate,
        [],
        varset.init,    % VarSet
        varset.init,    % InstVarSet
        fp_impl_ordinary(PreInitCode, yes(Context))
    ),
    add_pragma_foreign_proc(PreInitFCInfo, Status, Context, no,
        !ModuleInfo, !Specs),

    CallPreInitExpr =
        call_expr(Context, PreInitPredName, [], purity_impure).

    % Add the foreign clauses for the mutable's primitive access and
    % locking predicates.
    %
:- pred add_csharp_java_mutable_primitive_preds(
    foreign_language::in(lang_csharp_java), string::in, module_name::in,
    string::in, mer_type::in, mutable_var_attributes::in,
    pragma_foreign_proc_attributes::in, mer_inst::in, box_policy::in,
    prog_context::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_csharp_java_mutable_primitive_preds(Lang, TargetMutableName, ModuleName,
        MutableName, Type, MutAttrs, Attrs, Inst, BoxPolicy, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    IsThreadLocal = mutable_var_thread_local(MutAttrs),

    % Construct the semipure get predicate.

    set_purity(purity_semipure, Attrs, GetAttrs0),
    set_thread_safe(proc_thread_safe, GetAttrs0, GetAttrs),
    varset.new_named_var("X", X, varset.init, VarSet),
    (
        IsThreadLocal = mutable_not_thread_local,
        GetCode = "\tX = " ++ TargetMutableName ++ ";\n"
    ;
        IsThreadLocal = mutable_thread_local,
        Lang = lang_java,
        IsThreadLocal = mutable_thread_local,
        GetCode = "\tX = " ++ TargetMutableName ++ ".get();\n"
    ;
        IsThreadLocal = mutable_thread_local,
        Lang = lang_csharp,
        ( if Type = int_type then
            Cast = "(int) "
        else
            Cast = ""
        ),
        GetCode = string.append_list([
            "\tX = ", Cast, "runtime.ThreadLocalMutables.get(",
            TargetMutableName, ");\n"
        ])
    ),
    GetFCInfo = pragma_info_foreign_proc(GetAttrs,
        mutable_unsafe_get_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
        VarSet,
        varset.init, % Inst varset.
        fp_impl_ordinary(GetCode, yes(Context))
    ),
    add_pragma_foreign_proc(GetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs),

    % Construct the impure set predicate.

    set_thread_safe(proc_thread_safe, Attrs, SetAttrs),
    TrailMutableUpdates = mutable_var_trailed(MutAttrs),
    (
        TrailMutableUpdates = mutable_untrailed,
        TrailCode = ""
    ;
        TrailMutableUpdates = mutable_trailed,
        Pieces = [words("Error: trailed mutable in non-trailed grade."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs],
        % This is just a dummy value.
        TrailCode = ""
    ),
    (
        IsThreadLocal = mutable_not_thread_local,
        SetCode = "\t" ++ TargetMutableName ++ " = X;\n"
    ;
        IsThreadLocal = mutable_thread_local,
        Lang = lang_java,
        SetCode = "\t" ++ TargetMutableName ++ ".set(X);\n"
    ;
        IsThreadLocal = mutable_thread_local,
        Lang = lang_csharp,
        SetCode = "\truntime.ThreadLocalMutables.set(" ++
            TargetMutableName ++ ", X);\n"
    ),
    SetFCInfo = pragma_info_foreign_proc(SetAttrs,
        mutable_unsafe_set_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), BoxPolicy)], VarSet,
        varset.init, % Inst varset.
        fp_impl_ordinary(TrailCode ++ SetCode, yes(Context))
    ),
    add_pragma_foreign_proc(SetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs).

    % Add the code required to initialise a mutable.
    %
:- pred add_csharp_java_mutable_initialisation(module_name::in, string::in,
    prog_varset::in, goal::in, sym_name::in, prog_term::in,
    prog_context::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_csharp_java_mutable_initialisation(ModuleName, MutableName, VarSet0,
        CallPreInitExpr, InitSetPredName, InitTerm, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    % Add the `:- initialise' declaration for the mutable initialisation
    % predicate.
    InitPredName = mutable_init_pred_sym_name(ModuleName, MutableName),
    InitPredArity = 0,
    add_pass_3_initialise_for_mutable(InitPredName, InitPredArity, Context,
        Status, !ModuleInfo, !Specs),

    % Add the clause for the mutable initialisation predicate.
    varset.new_named_var("X", X, VarSet0, VarSet),
    UnifyExpr =
        unify_expr(Context, variable(X, Context), InitTerm, purity_impure),
    CallSetPredExpr =
        call_expr(Context, InitSetPredName, [variable(X, Context)],
            purity_impure),
    InitPredExpr = goal_list_to_conj(Context,
        [CallPreInitExpr, UnifyExpr, CallSetPredExpr]),

    % See the comments for prog_io.parse_mutable_decl for the reason
    % why we _must_ use Varset here.
    module_add_clause(VarSet, pf_predicate, InitPredName, [], InitPredExpr,
        Status, Context, no, goal_type_none, !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%
%
% Erlang mutables.
%

:- pred add_erlang_mutable_preds(item_mutable_info::in, string::in,
    import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_erlang_mutable_preds(ItemMutable, TargetMutableName,
        Status, !ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ItemMutable = item_mutable_info(MutableName, _Type, InitTerm, Inst,
        MutAttrs, Varset, Context, _SeqNum),
    IsConstant = mutable_var_constant(MutAttrs),
    (
        IsConstant = yes,
        InitSetPredName = mutable_secret_set_pred_sym_name(ModuleName,
            MutableName),
        add_erlang_constant_mutable_access_preds(TargetMutableName,
            ModuleName, MutableName, Inst,
            Context, Status, !ModuleInfo, !Specs)
    ;
        IsConstant = no,
        InitSetPredName = mutable_set_pred_sym_name(ModuleName,
            MutableName),
        add_erlang_mutable_user_access_preds(TargetMutableName,
            ModuleName, MutableName, MutAttrs, Inst,
            Context, Status, !ModuleInfo, !QualInfo, !Specs)
    ),
    add_erlang_mutable_initialisation(ModuleName, MutableName,
        Varset, InitSetPredName, InitTerm,
        Context, Status, !ModuleInfo, !QualInfo, !Specs).

    % Add the access predicates for constant mutables.
    %
:- pred add_erlang_constant_mutable_access_preds(string::in,
    module_name::in, string::in, mer_inst::in, prog_context::in,
    import_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_erlang_constant_mutable_access_preds(TargetMutableName,
        ModuleName, MutableName, Inst, Context, Status,
        !ModuleInfo, !Specs) :-
    varset.new_named_var("X", X, varset.init, VarSet),
    InstVarSet = varset.init,
    Attrs = default_attributes(lang_erlang),
    set_purity(purity_pure, Attrs, ConstantGetAttrs0),
    set_thread_safe(proc_thread_safe, ConstantGetAttrs0, ConstantGetAttrs),

    % Getter.
    GetCode = erlang_mutable_get_code(TargetMutableName),
    ConstantGetFCInfo = pragma_info_foreign_proc(
        ConstantGetAttrs,
        mutable_get_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), native_if_possible)],
        VarSet,
        InstVarSet,
        fp_impl_ordinary(GetCode, yes(Context))
    ),
    add_pragma_foreign_proc(ConstantGetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs),

    % Secret setter.
    SetCode = erlang_mutable_set_code(TargetMutableName),
    ConstantSetFCInfo = pragma_info_foreign_proc(Attrs,
        mutable_secret_set_pred_sym_name(ModuleName, MutableName),
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), native_if_possible)],
        VarSet,
        InstVarSet,
        fp_impl_ordinary(SetCode, yes(Context))
    ),
    add_pragma_foreign_proc(ConstantSetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs).

    % Add the access predicates for a non-constant mutable.
    % If the mutable has the `attach_to_io_state' attribute then add the
    % versions of the access preds that take the I/O state as well.
    %
:- pred add_erlang_mutable_user_access_preds(string::in,
    module_name::in, string::in, mutable_var_attributes::in, mer_inst::in,
    prog_context::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_erlang_mutable_user_access_preds(TargetMutableName,
        ModuleName, MutableName, MutAttrs, Inst, Context,
        Status, !ModuleInfo, !QualInfo, !Specs) :-
    IsThreadLocal = mutable_var_thread_local(MutAttrs),
    Attrs = default_attributes(lang_erlang),
    varset.new_named_var("X", X, varset.init, VarSet0),

    % Construct the semipure get predicate.
    set_purity(purity_semipure, Attrs, GetAttrs0),
    set_thread_safe(proc_thread_safe, GetAttrs0, GetAttrs),
    (
        IsThreadLocal = mutable_not_thread_local,
        GetCode = erlang_mutable_get_code(TargetMutableName)
    ;
        IsThreadLocal = mutable_thread_local,
        % XXX this will need to change. `thread_local' mutables are supposed
        % to be inherited when a child process is spawned, but Erlang process
        % dictionary values are not automatically inherited. Hence we will
        % probably need another level of indirection.
        GetCode = "X = get({'MR_thread_local_mutable', " ++
            TargetMutableName ++ "})"
    ),
    GetPredName = mutable_get_pred_sym_name(ModuleName, MutableName),
    GetFCInfo = pragma_info_foreign_proc(GetAttrs,
        GetPredName,
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), native_if_possible)],
        VarSet0,
        varset.init, % Inst varset.
        fp_impl_ordinary(GetCode, yes(Context))
    ),
    add_pragma_foreign_proc(GetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs),

    % Construct the impure set predicate.
    set_purity(purity_impure, Attrs, SetAttrs0),
    set_thread_safe(proc_thread_safe, SetAttrs0, SetAttrs),
    (
        IsThreadLocal = mutable_not_thread_local,
        SetCode = erlang_mutable_set_code(TargetMutableName)
    ;
        IsThreadLocal = mutable_thread_local,
        % XXX this will need to change (see the comment for the getter)
        SetCode = "put({'MR_thread_local_mutable', " ++
            TargetMutableName ++ "}, X)"
    ),
    SetPredName = mutable_set_pred_sym_name(ModuleName, MutableName),
    SetFCInfo = pragma_info_foreign_proc(SetAttrs,
        SetPredName,
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), native_if_possible)],
        VarSet0,
        varset.init, % Inst varset.
        fp_impl_ordinary(SetCode, yes(Context))
    ),
    add_pragma_foreign_proc(SetFCInfo, Status, Context, no,
        !ModuleInfo, !Specs),

    IOStateInterface = mutable_var_attach_to_io_state(MutAttrs),
    (
        IOStateInterface = yes,
        varset.new_named_var("IO", IO, VarSet0, VarSet),
        Ctxt = context_init,

        % Construct the pure get predicate.
        % This just calls the semipure get predicate with a promise_pure
        % around it.
        GetPredArgs =
            [variable(X, Ctxt), variable(IO, Ctxt), variable(IO, Ctxt)],
        CallSemipureGetExpr =
            call_expr(Context, GetPredName, [variable(X, Context)],
                purity_semipure),
        GetPredExpr = promise_purity_expr(Context, purity_pure,
            CallSemipureGetExpr),
        module_add_clause(VarSet, pf_predicate, GetPredName, GetPredArgs,
            GetPredExpr, Status, Context, no, goal_type_none,
            !ModuleInfo, !QualInfo, !Specs),

        % Construct the pure set predicate.
        %
        % We just call the impure version and attach a promise_pure pragma
        % to the predicate. (The purity pragma was added during pass 2.)
        SetPredArgs =
            [variable(X, Ctxt), variable(IO, Ctxt), variable(IO, Ctxt)],
        SetPredExpr =
            call_expr(Context, SetPredName, [variable(X, Context)],
                purity_impure),
        module_add_clause(VarSet, pf_predicate, SetPredName, SetPredArgs,
            SetPredExpr, Status, Context, no, goal_type_none,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        IOStateInterface = no
    ).

:- func erlang_mutable_get_code(string) = string.

erlang_mutable_get_code(TargetMutableName) =
    string.append_list([
        "'ML_erlang_global_server' ! {get_mutable, ",
            TargetMutableName, ", self()},\n",
        "receive\n",
        "   {get_mutable_ack, Value} ->\n",
        "       X = Value\n",
        "end\n"
    ]).

:- func erlang_mutable_set_code(string) = string.

erlang_mutable_set_code(TargetMutableName) =
    "'ML_erlang_global_server' ! {set_mutable, " ++
        TargetMutableName ++ ", X}".

    % Add the code required to initialise a mutable.
    %
:- pred add_erlang_mutable_initialisation(module_name::in, string::in,
    prog_varset::in, sym_name::in, prog_term::in, prog_context::in,
    import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_erlang_mutable_initialisation(ModuleName, MutableName,
        VarSet0, InitSetPredName, InitTerm, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    % Add the `:- initialise' declaration for the mutable initialisation
    % predicate.
    InitPredName = mutable_init_pred_sym_name(ModuleName, MutableName),
    InitPredArity = 0,
    add_pass_3_initialise_for_mutable(InitPredName, InitPredArity, Context,
        Status, !ModuleInfo, !Specs),

    % Add the clause for the mutable initialisation predicate.
    %
    % See the comments for prog_io_mutable.parse_mutable_decl_info
    % for the reason why we _must_ start with Varset0 here.
    varset.new_named_var("X", X, VarSet0, VarSet),
    UnifyExpr =
        unify_expr(Context, variable(X, Context), InitTerm, purity_impure),
    CallExpr =
        call_expr(Context, InitSetPredName, [variable(X, Context)],
            purity_impure),
    InitPredArgs = [],
    InitPredExpr = conj_expr(Context, UnifyExpr, CallExpr),
    module_add_clause(VarSet, pf_predicate, InitPredName, InitPredArgs,
        InitPredExpr, Status, Context, no, goal_type_none,
        !ModuleInfo, !QualInfo, !Specs).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_mutable_aux_preds.
%-----------------------------------------------------------------------------%
