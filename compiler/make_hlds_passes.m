%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_passes.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.qual_info.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

    % When adding an item to the HLDS we need to know both its
    % import_status and whether uses of it must be module qualified.
:- type item_status
    ---> item_status(import_status, need_qualifier).

    % do_parse_tree_to_hlds(ParseTree, MQInfo, EqvMap, UsedModules
    %   HLDS, QualInfo, InvalidTypes, InvalidModes):
    %
    % Given MQInfo (returned by module_qual.m) and EqvMap and UsedModules
    % (both returned by equiv_type.m), converts ParseTree to HLDS. Any errors
    % found are recorded in the HLDS num_errors field.
    % Returns InvalidTypes = yes if undefined types found.
    % Returns InvalidModes = yes if undefined or cyclic insts or modes
    % found. QualInfo is an abstract type that is then passed back to
    % produce_instance_method_clauses (see below).
    %
:- pred do_parse_tree_to_hlds(compilation_unit::in, mq_info::in, eqv_map::in,
    used_modules::in, module_info::out, qual_info::out,
    bool::out, bool::out, io::di, io::uo) is det.

    % The bool records whether any cyclic insts or modes were detected.
    %
:- pred add_item_decl_pass_1(item::in, prog_context::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    bool::out, list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_item_clause(item::in, import_status::in, import_status::out,
    prog_context::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_stratified_pred(string::in, sym_name::in, arity::in,
    term.context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % add_pred_marker(PragmaName, Name, Arity, Status,
    %   Context, Marker, ConflictMarkers, !ModuleInfo, !Specs):
    %
    % Adds Marker to the marker list of the pred(s) with give Name and Arity,
    % updating the ModuleInfo. If the named pred does not exist, or the pred
    % already has a marker in ConflictMarkers, report an error.
    %
:- pred add_pred_marker(string::in, sym_name::in, arity::in, import_status::in,
    prog_context::in, marker::in, list(marker)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- type add_marker_pred_info == pred(pred_info, pred_info).
:- inst add_marker_pred_info == (pred(in, out) is det).

:- pred do_add_pred_marker(string::in, sym_name::in, arity::in,
    import_status::in, bool::in, term.context::in,
    add_marker_pred_info::in(add_marker_pred_info),
    module_info::in, module_info::out, list(pred_id)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred module_mark_as_external(sym_name::in, int::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred maybe_check_field_access_function(sym_name::in, arity::in,
    import_status::in, prog_context::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.clause_to_proc.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_out.
:- import_module hlds.make_hlds.add_class.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_mode.
:- import_module hlds.make_hlds.add_pragma.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.add_solver.
:- import_module hlds.make_hlds.add_special_pred.
:- import_module hlds.make_hlds.add_type.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_mutable.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module recompilation.

:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

do_parse_tree_to_hlds(unit_module(Name, Items), MQInfo0, EqvMap, UsedModules,
        ModuleInfo, QualInfo, InvalidTypes, InvalidModes, !IO) :-
    some [!ModuleInfo, !Specs] (
        globals.io_get_globals(Globals, !IO),
        mq_info_get_partial_qualifier_info(MQInfo0, PQInfo),
        module_info_init(Name, Items, Globals, PQInfo, no, !:ModuleInfo),
        module_info_set_used_modules(UsedModules, !ModuleInfo),
        !:Specs = [],
        add_item_list_decls_pass_1(Items,
            item_status(status_local, may_be_unqualified), !ModuleInfo,
            no, InvalidModes0, !Specs),
        globals.io_lookup_bool_option(statistics, Statistics, !IO),
        maybe_write_string(Statistics, "% Processed all items in pass 1\n",
            !IO),
        maybe_report_stats(Statistics, !IO),

        add_item_list_decls_pass_2(Items,
            item_status(status_local, may_be_unqualified),
            !ModuleInfo, [], Pass2Specs),
        (
            Pass2Specs = [],
            InvalidTypes1 = no
        ;
            Pass2Specs = [_ | _],
            InvalidTypes1 = yes
        ),
        !:Specs = Pass2Specs ++ !.Specs,

        % Add constructors and special preds to the HLDS. This must be done
        % after adding all type and `:- pragma foreign_type' declarations.
        % If there were errors in foreign type type declarations, doing this
        % may cause a compiler abort.
        (
            InvalidTypes1 = no,
            module_info_get_type_table(!.ModuleInfo, Types),
            map.foldl3(process_type_defn, Types, no, InvalidTypes2,
                !ModuleInfo, !Specs)
        ;
            InvalidTypes1 = yes,
            InvalidTypes2 = yes
        ),

        % Add the special preds for the builtin types which don't have a
        % type declaration, hence no hlds_type_defn is generated for them.
        (
            Name = mercury_public_builtin_module,
            compiler_generated_rtti_for_builtins(!.ModuleInfo)
        ->
            list.foldl(add_builtin_type_ctor_special_preds,
                builtin_type_ctors_with_no_hlds_type_defn, !ModuleInfo)
        ;
            true
        ),

        % Balance any data structures that need it.
        module_info_optimize(!ModuleInfo),
        maybe_write_string(Statistics, "% Processed all items in pass 2\n",
            !IO),
        maybe_report_stats(Statistics, !IO),
        init_qual_info(MQInfo0, EqvMap, QualInfo0),
        add_item_list_clauses(Items, status_local, !ModuleInfo,
            QualInfo0, QualInfo, !Specs),

        qual_info_get_mq_info(QualInfo, MQInfo),
        mq_info_get_type_error_flag(MQInfo, InvalidTypes3),
        InvalidTypes = InvalidTypes1 `or` InvalidTypes2 `or` InvalidTypes3,
        mq_info_get_mode_error_flag(MQInfo, InvalidModes1),
        InvalidModes = InvalidModes0 `or` InvalidModes1,

        % The predid list is constructed in reverse order, for efficiency,
        % so we return it to the correct order here.
        module_info_reverse_predids(!ModuleInfo),

        sort_error_specs(!.Specs, SortedSpecs),
        module_info_get_globals(!.ModuleInfo, CurGlobals),
        write_error_specs(SortedSpecs, CurGlobals, 0, _NumWarnings,
            0, NumErrors, !IO),
        module_info_incr_num_errors(NumErrors, !ModuleInfo),

        ModuleInfo = !.ModuleInfo
    ).

:- pred add_builtin_type_ctor_special_preds(type_ctor::in,
    module_info::in, module_info::out) is det.

add_builtin_type_ctor_special_preds(TypeCtor, !ModuleInfo) :-
    varset.init(TVarSet),
    Body = hlds_abstract_type(non_solver_type),
    term.context_init(Context),
    Status = status_local,
    construct_type(TypeCtor, [], Type),
    add_special_preds(TVarSet, Type, TypeCtor, Body, Context, Status,
        !ModuleInfo).

%-----------------------------------------------------------------------------%

    % pass 1:
    % Add the declarations one by one to the module,
    % except for type definitions and pragmas.
    %
    % The `InvalidModes' bool records whether we detected
    % any cyclic insts or modes.
    %
:- pred add_item_list_decls_pass_1(item_list::in, item_status::in,
    module_info::in, module_info::out, bool::in, bool::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_list_decls_pass_1([], _, !ModuleInfo, !InvalidModes, !Specs).
add_item_list_decls_pass_1([Item - Context | Items], !.Status, !ModuleInfo,
        !InvalidModes, !Specs) :-
    add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo,
        NewInvalidModes, !Specs),
    !:InvalidModes = bool.or(!.InvalidModes, NewInvalidModes),
    add_item_list_decls_pass_1(Items, !.Status, !ModuleInfo, !InvalidModes,
        !Specs).

    % pass 2:
    % Add the type definitions and pragmas one by one to the module,
    % and add default modes for functions with no mode declaration.
    %
    % Adding type definitions needs to come after we have added the pred
    % declarations, since we need to have the pred_id for `index/2' and
    % `compare/3' when we add compiler-generated clauses for `compare/3'.
    % (And similarly for other compiler-generated predicates like that.)
    %
    % Adding pragmas needs to come after we have added the
    % pred declarations, in order to allow the pragma declarations
    % for a predicate to syntactically precede the pred declaration.
    %
    % Adding default modes for functions needs to come after we have
    % processed all the mode declarations, since otherwise we can't be
    % sure that there isn't a mode declaration for the function.
    %
:- pred add_item_list_decls_pass_2(item_list::in, item_status::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_list_decls_pass_2([], _, !ModuleInfo, !Specs).
add_item_list_decls_pass_2([Item - Context | Items], !.Status, !ModuleInfo,
        !Specs) :-
    add_item_decl_pass_2(Item, Context, !Status, !ModuleInfo, !Specs),
    add_item_list_decls_pass_2(Items, !.Status, !ModuleInfo, !Specs).

    % pass 3:
    % Add the clauses one by one to the module.
    %
    % Check that the declarations for field extraction and update functions
    % are sensible.
    %
    % Check that predicates listed in `:- initialise' and `:- finalise'
    % declarations exist and have the correct signature, introduce
    % pragma export declarations for them and record their exported name in
    % the module_info so that we can tell the code generator to call it at
    % initialisation/finalisation time.
    %
:- pred add_item_list_clauses(item_list::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_list_clauses([], _Status, !ModuleInfo, !QualInfo, !Specs).
add_item_list_clauses([Item - Context | Items], Status0,
        !ModuleInfo, !QualInfo, !Specs) :-
    add_item_clause(Item, Status0, Status1, Context, !ModuleInfo, !QualInfo,
        !Specs),
    add_item_list_clauses(Items, Status1, !ModuleInfo, !QualInfo, !Specs).

%-----------------------------------------------------------------------------%

add_item_decl_pass_1(item_clause(_, _, _, _, _, _), _, !Status, !ModuleInfo,
        no, !Specs).
    % Skip clauses.
add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo, no, !Specs) :-
    % If this is a solver type then we need to also add the declarations
    % for the compiler generated construction function and deconstruction
    % predicate for the special constrained data constructor.
    %
    % In pass 3 we add the corresponding clauses.
    %
    % Before switch detection, we turn calls to these functions/predicates
    % into ordinary constructions/deconstructions, but preserve the
    % corresponding impurity annotations.
    Item = item_type_defn(TVarSet, SymName, TypeParams, TypeDefn, _Cond),
    ( TypeDefn = parse_tree_solver_type(SolverTypeDetails, _MaybeUserEqComp) ->
        add_solver_type_decl_items(TVarSet, SymName, TypeParams,
            SolverTypeDetails, Context, !Status, !ModuleInfo, !Specs),
        add_solver_type_mutable_items_pass_1(SolverTypeDetails ^ mutable_items,
            Context, !Status, !ModuleInfo, !Specs)
    ;
        true
    ).
add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo, InvalidMode,
        !Specs) :-
    Item = item_inst_defn(VarSet, Name, Params, InstDefn, Cond),
    module_add_inst_defn(VarSet, Name, Params, InstDefn, Cond, Context,
        !.Status, !ModuleInfo, InvalidMode, !Specs).
add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo, InvalidMode, !Specs) :-
    Item = item_mode_defn(VarSet, Name, Params, ModeDefn, Cond),
    module_add_mode_defn(VarSet, Name, Params, ModeDefn,
        Cond, Context, !.Status, !ModuleInfo, InvalidMode, !Specs).
add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo, no, !Specs) :-
    Item = item_pred_or_func(Origin, TypeVarSet, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes, _WithType, _WithInst, MaybeDet,
        _Cond, Purity, ClassContext),
    init_markers(Markers0),
    %
    % If this predicate was added as a result of the mutable transformation
    % then mark this predicate as a mutable access pred.  We do this
    % so that we can tell optimizations, like inlining, to treat it
    % specially.
    %
    (
        Origin = compiler(Reason),
        (
            Reason = mutable_decl,
            add_marker(marker_mutable_access_pred, Markers0, Markers)
        ;
            ( Reason = initialise_decl
            ; Reason = finalise_decl
            ; Reason = solver_type
            ; Reason = pragma_memo_attribute
            ; Reason = foreign_imports
            ),
            Markers = Markers0
        )
    ;
        Origin = user,
        Markers = Markers0
    ),
    module_add_pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes, MaybeDet, Purity, ClassContext,
        Markers, Context, !.Status, _, !ModuleInfo, !Specs).
add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo, no, !Specs) :-
    Item = item_pred_or_func_mode(VarSet, MaybePredOrFunc, PredName, Modes,
        _WithInst, MaybeDet, _Cond),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        !.Status = item_status(ImportStatus, _),
        IsClassMethod = no,
        module_add_mode(VarSet, PredName, Modes, MaybeDet, ImportStatus,
            Context, PredOrFunc, IsClassMethod, _, !ModuleInfo, !Specs)
    ;
        MaybePredOrFunc = no,
        % equiv_type.m should have either set the pred_or_func
        % or removed the item from the list.
        unexpected(this_file, "add_item_decl_pass_1: " ++
            "no pred_or_func on mode declaration")
    ).
add_item_decl_pass_1(Item, _, !Status, !ModuleInfo, no, !Specs) :-
    Item = item_pragma(_, _).
add_item_decl_pass_1(Item, _, !Status, !ModuleInfo, no, !Specs) :-
    Item = item_promise(_, _, _, _).
add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo, no, !Specs) :-
    Item = item_module_defn(_VarSet, ModuleDefn),
    ( module_defn_update_import_status(ModuleDefn, StatusPrime) ->
        !:Status = StatusPrime
    ; ModuleDefn = md_import(list_module(Specifiers)) ->
        !.Status = item_status(IStat, _),
        add_module_specifiers(Specifiers, IStat, !ModuleInfo)
    ; ModuleDefn = md_use(list_module(Specifiers)) ->
        !.Status = item_status(IStat, _),
        add_module_specifiers(Specifiers, IStat, !ModuleInfo)
    ; ModuleDefn = md_include_module(_) ->
        true
    ; ModuleDefn = md_external(MaybeBackend, External) ->
        ( External = name_arity(Name, Arity) ->
            module_info_get_globals(!.ModuleInfo, Globals),
            CurrentBackend = lookup_current_backend(Globals),
            (
                (
                    MaybeBackend = no
                ;
                    MaybeBackend = yes(Backend),
                    Backend = CurrentBackend
                )
            ->
                module_mark_as_external(Name, Arity, Context, !ModuleInfo,
                    !Specs)
            ;
                true
            )
        ;
            Pieces = [words("Warning:"), quote("external"),
                words("declaration requires arity."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ; ModuleDefn = md_module(_ModuleName) ->
        report_unexpected_decl("module", Context, !Specs)
    ; ModuleDefn = md_end_module(_ModuleName) ->
        report_unexpected_decl("end_module", Context, !Specs)
    ; ModuleDefn = md_version_numbers(_, _) ->
        true
    ; ModuleDefn = md_transitively_imported ->
        true
    ;
        Pieces = [words("Warning: declaration not yet implemented."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_warning, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).
add_item_decl_pass_1(Item, _, !Status, !ModuleInfo, no, !Specs) :-
    Item = item_nothing(_).
add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo, no, !Specs) :-
    Item = item_typeclass(Constraints, FunDeps, Name, Vars, Interface, VarSet),
    module_add_class_defn(Constraints, FunDeps, Name, Vars, Interface,
        VarSet, Context, !.Status, !ModuleInfo, !Specs).
add_item_decl_pass_1(Item, _, !Status, !ModuleInfo, no, !Specs) :-
    % We add instance declarations on the second pass so that we don't add
    % an instance declaration before its class declaration.
    Item = item_instance(_, _, _, _, _,_).
add_item_decl_pass_1(Item, _, !Status, !ModuleInfo, no, !Specs) :-
    % We add initialise declarations on the third pass.
    Item = item_initialise(_, _, _).
add_item_decl_pass_1(Item, _, !Status, !ModuleInfo, no, !Specs) :-
    % We add finalise declarations on the third pass.
    Item = item_finalise(_, _, _).
add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo, no, !Specs) :-
    % We add the initialise decl and the foreign_decl on the second pass and
    % the foreign_proc clauses on the third pass.
    Item = item_mutable(Name, Type, _InitValue, Inst, MutAttrs, _MutVarset),
    !.Status = item_status(ImportStatus, _),
    DefinedThisModule = status_defined_in_this_module(ImportStatus),
    (
        DefinedThisModule = yes,
        module_info_get_name(!.ModuleInfo, ModuleName),
        %
        % Create the initialisation predicate and the mutex initialisation
        % predicate.  The latter is called by the former.
        %
        InitPredDecl = mutable_init_pred_decl(ModuleName, Name),
        add_item_decl_pass_1(InitPredDecl, Context, !Status, !ModuleInfo, _,
            !Specs),
        IsConstant = mutable_var_constant(MutAttrs),
        (
            IsConstant = no,

            % Create the mutex initialisation predicate.  This is called
            % by the mutable initialisation predicate.
            InitMutexPredDecl = mutable_init_mutex_pred_decl(ModuleName, Name),
            add_item_decl_pass_1(InitMutexPredDecl, Context, !Status,
                !ModuleInfo, _, !Specs),

            % Create the primitive access and locking predicates.
            LockPredDecl = lock_pred_decl(ModuleName, Name),
            add_item_decl_pass_1(LockPredDecl, Context, !Status,
                !ModuleInfo, _, !Specs),
            UnlockPredDecl = unlock_pred_decl(ModuleName, Name),
            add_item_decl_pass_1(UnlockPredDecl, Context, !Status,
                !ModuleInfo, _, !Specs),
            UnsafeGetPredDecl = unsafe_get_pred_decl(ModuleName, Name,
                Type, Inst),
            add_item_decl_pass_1(UnsafeGetPredDecl, Context, !Status,
                !ModuleInfo, _, !Specs),
            UnsafeSetPredDecl = unsafe_set_pred_decl(ModuleName, Name,
                Type, Inst),
            add_item_decl_pass_1(UnsafeSetPredDecl, Context, !Status,
                !ModuleInfo, _, !Specs),

            % Create the standard, non-pure access predicates. These are
            % always created for non-constant mutables, even if the
            % `attach_to_io_state' attribute has been specified.
            StdGetPredDecl = std_get_pred_decl(ModuleName, Name, Type, Inst),
            add_item_decl_pass_1(StdGetPredDecl, Context, !Status,
                !ModuleInfo, _, !Specs),
            StdSetPredDecl = std_set_pred_decl(ModuleName, Name, Type, Inst),
            add_item_decl_pass_1(StdSetPredDecl, Context, !Status,
                !ModuleInfo, _, !Specs),

            % If requested, create the pure access predicates using
            % the I/O state as well.
            CreateIOInterface = mutable_var_attach_to_io_state(MutAttrs),
            (
                CreateIOInterface = yes,
                IOGetPredDecl = io_get_pred_decl(ModuleName, Name, Type, Inst),
                add_item_decl_pass_1(IOGetPredDecl, Context, !Status,
                    !ModuleInfo, _, !Specs),
                IOSetPredDecl = io_set_pred_decl(ModuleName, Name, Type, Inst),
                add_item_decl_pass_1(IOSetPredDecl, Context, !Status,
                    !ModuleInfo, _, !Specs)
            ;
                CreateIOInterface = no
            )
        ;
            IsConstant = yes,

            % We create the "get" access predicate, which is pure since
            % it always returns the same value, but we must also create
            % a secret "set" predicate for use by the initialization code.
            ConstantGetPredDecl = constant_get_pred_decl(ModuleName, Name,
                Type, Inst),
            add_item_decl_pass_1(ConstantGetPredDecl, Context, !Status,
                !ModuleInfo, _, !Specs),
            ConstantSetPredDecl = constant_set_pred_decl(ModuleName, Name,
                Type, Inst),
            add_item_decl_pass_1(ConstantSetPredDecl, Context, !Status,
                !ModuleInfo, _, !Specs)
        )
    ;
        DefinedThisModule = no
    ).

:- pred add_solver_type_mutable_items_pass_1(list(item)::in, prog_context::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_mutable_items_pass_1([], _Context, !Status, !ModuleInfo,
        !Specs).
add_solver_type_mutable_items_pass_1([Item | Items], Context, !Status,
        !ModuleInfo, !Specs) :-
    add_item_decl_pass_1(Item, Context, !Status, !ModuleInfo, _, !Specs),
    add_solver_type_mutable_items_pass_1(Items, Context, !Status, !ModuleInfo,
        !Specs).

:- pred add_module_specifiers(list(module_specifier)::in, import_status::in,
    module_info::in, module_info::out) is det.

add_module_specifiers(Specifiers, IStat, !ModuleInfo) :-
    ( status_defined_in_this_module(IStat) = yes ->
        module_add_imported_module_specifiers(IStat, Specifiers, !ModuleInfo)
    ; IStat = status_imported(import_locn_ancestor_private_interface) ->
        module_add_imported_module_specifiers(IStat, Specifiers, !ModuleInfo),

            % Any import_module which comes from a private interface
            % must by definition be a module used by the parent module.
        module_info_add_parents_to_used_modules(Specifiers, !ModuleInfo)
    ;
        module_add_indirectly_imported_module_specifiers(Specifiers,
            !ModuleInfo)
    ).

%-----------------------------------------------------------------------------%

:- pred add_item_decl_pass_2(item::in, prog_context::in, item_status::in,
    item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_decl_pass_2(Item, _Context, !Status, !ModuleInfo, !Specs) :-
    Item = item_module_defn(_VarSet, ModuleDefn),
    ( module_defn_update_import_status(ModuleDefn, StatusPrime) ->
        !:Status = StatusPrime
    ;
        true
    ).
add_item_decl_pass_2(Item, Context, !Status, !ModuleInfo, !Specs) :-
    Item = item_type_defn(VarSet, Name, Args, TypeDefn, Cond),
    module_add_type_defn(VarSet, Name, Args, TypeDefn, Cond, Context,
        !.Status, !ModuleInfo, !Specs),
    ( TypeDefn = parse_tree_solver_type(SolverTypeDetails, _MaybeUserEqComp) ->
        add_solver_type_mutable_items_pass_2(SolverTypeDetails ^ mutable_items,
            Context, !Status, !ModuleInfo, !Specs)
    ;
        true
    ).

add_item_decl_pass_2(Item, Context, !Status, !ModuleInfo, !Specs) :-
    Item = item_pragma(Origin, Pragma),
    add_pragma(Origin, Pragma, Context, !Status, !ModuleInfo, !Specs).
add_item_decl_pass_2(Item, _Context, !Status, !ModuleInfo, !Specs) :-
    Item = item_pred_or_func(_Origin, _TypeVarSet, _InstVarSet, _ExistQVars,
        PredOrFunc, SymName, TypesAndModes, _WithType, _WithInst,
        _MaybeDet, _Cond, _Purity, _ClassContext),
    %
    % Add default modes for function declarations, if necessary.
    %
    (
        PredOrFunc = predicate
    ;
        PredOrFunc = function,
        list.length(TypesAndModes, Arity),
        adjust_func_arity(function, FuncArity, Arity),
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        (
            predicate_table_search_func_sym_arity(PredTable0,
                is_fully_qualified, SymName, FuncArity, PredIds)
        ->
            predicate_table_get_preds(PredTable0, Preds0),
            maybe_add_default_func_modes(PredIds, Preds0, Preds),
            predicate_table_set_preds(Preds, PredTable0, PredTable),
            module_info_set_predicate_table(PredTable, !ModuleInfo)
        ;
            unexpected(this_file, "can't find func declaration")
        )
    ).
add_item_decl_pass_2(Item, _, !Status, !ModuleInfo, !Specs) :-
    Item = item_promise(_, _, _, _).
add_item_decl_pass_2(Item, _, !Status, !ModuleInfo, !Specs) :-
    Item = item_clause(_, _, _, _, _, _).
add_item_decl_pass_2(Item, _, !Status, !ModuleInfo, !Specs) :-
    Item = item_inst_defn(_, _, _, _, _).
add_item_decl_pass_2(Item, _, !Status, !ModuleInfo, !Specs) :-
    Item = item_mode_defn(_, _, _, _, _).
add_item_decl_pass_2(Item, _, !Status, !ModuleInfo, !Specs) :-
    Item = item_pred_or_func_mode(_, _, _, _, _, _, _).
add_item_decl_pass_2(Item, _, !Status, !ModuleInfo, !Specs) :-
    Item = item_nothing(_).
add_item_decl_pass_2(Item, _, !Status, !ModuleInfo, !Specs) :-
    Item = item_typeclass(_, _, _, _, _, _).
add_item_decl_pass_2(Item, Context, !Status, !ModuleInfo, !Specs) :-
    Item = item_instance(Constraints, Name, Types, Body, VarSet,
        InstanceModuleName),
    !.Status = item_status(ImportStatus, _),
    (
        Body = instance_body_abstract,
        make_status_abstract(ImportStatus, BodyStatus)
    ;
        Body = instance_body_concrete(_),
        BodyStatus = ImportStatus
    ),
    module_add_instance_defn(InstanceModuleName, Constraints, Name, Types,
        Body, VarSet, BodyStatus, Context, !ModuleInfo, !Specs).
add_item_decl_pass_2(Item, Context, !Status, !ModuleInfo, !Specs) :-
    % These are processed properly during pass 3, we just do some
    % error checking at this point.
    Item = item_initialise(Origin, _, _),
    !.Status = item_status(ImportStatus, _),
    ( ImportStatus = status_exported ->
        (
            Origin = user,
            error_is_exported(Context, "`initialise' declaration", !Specs)
        ;
            Origin = compiler(Details),
            (
                % Ignore the error if this initialise declaration was
                % introduced because of a mutable declaration.
                Details = mutable_decl
            ;
                ( Details = initialise_decl
                ; Details = finalise_decl
                ; Details = solver_type
                ; Details = foreign_imports
                ; Details = pragma_memo_attribute
                ),
                unexpected(this_file, "Bad introduced initialise declaration.")
            )
        )
    ;
        true
    ).
add_item_decl_pass_2(Item, Context, !Status, !ModuleInfo, !Specs) :-
    % There are processed properly during pass 3, we just do some error
    % checking at this point.
    Item = item_finalise(Origin, _, _),
    !.Status = item_status(ImportStatus, _),
    ( ImportStatus = status_exported ->
        (
            Origin = user,
            error_is_exported(Context, "`finalise' declaration", !Specs)
        ;
            % There are no source-to-source transformations that introduce
            % finalise declarations.
            Origin = compiler(_),
            unexpected(this_file, "Bad introduced finalise declaration.")
        )
    ;
        true
    ).
add_item_decl_pass_2(Item, Context, !Status, !ModuleInfo, !Specs) :-
    Item = item_mutable(Name, _Type, _InitTerm, Inst, MutAttrs, _MutVarset),
    !.Status = item_status(ImportStatus, _),
    ( ImportStatus = status_exported ->
        error_is_exported(Context, "`mutable' declaration", !Specs)
    ;
        true
    ),
    %
    % We don't implement the `mutable' declaration unless it is defined in
    % this module.  Not having this check means that we might end up up
    % duplicating the definition of the global variable in any submodules.
    %
    DefinedThisModule = status_defined_in_this_module(ImportStatus),
    (
        DefinedThisModule = yes,
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.get_target(Globals, CompilationTarget),

        % XXX We don't currently support the foreign_name attribute
        % for languages other than C.
        ( 
            CompilationTarget = target_c,
            mutable_var_maybe_foreign_names(MutAttrs) = MaybeForeignNames,
            module_info_get_name(!.ModuleInfo, ModuleName),
            (
                MaybeForeignNames = no
            ;
                MaybeForeignNames = yes(ForeignNames),

                % Report any errors with the foreign_name attributes
                % during this pass.
                ReportErrors = yes,
                get_global_name_from_foreign_names(!.ModuleInfo, ReportErrors,
                    Context, ModuleName, Name, ForeignNames,
                    _TargetMutableName, !Specs)
            ),

            % If we are creating the I/O version of the set predicate then we
            % need to add a promise_pure pragma for it.  This needs to be done
            % here (in stage 2) rather than in stage 3 where the rest of the
            % mutable transformation is.

            IOStateInterface = mutable_var_attach_to_io_state(MutAttrs),
            (
                IOStateInterface = yes,
                SetPredName = mutable_set_pred_sym_name(ModuleName, Name),
                IOSetPromisePurePragma = pragma_promise_pure(SetPredName, 3),
                add_pragma(compiler(mutable_decl), IOSetPromisePurePragma,
                    Context, !Status, !ModuleInfo, !Specs)
            ;
                IOStateInterface = no
            )
        ;
            ( CompilationTarget = target_il
            ; CompilationTarget = target_java
            ; CompilationTarget = target_asm
            ),
            Pieces = [words("Error: foreign_name mutable attribute not yet"),
                words("implemented for the"),
                fixed(compilation_target_string(CompilationTarget)),
                words("backend."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ),
        %
        % Check that the inst in the mutable declaration is a valid inst for a
        % mutable declaration.
        %
        ( is_valid_mutable_inst(!.ModuleInfo, Inst) ->
            true
        ;
            % It is okay to pass a dummy varset in here since any attempt
            % to use inst variables in a mutable declaration should already
            % been dealt with when the mutable declaration was parsed.
            DummyInstVarset = varset.init,  
            InstStr = mercury_expanded_inst_to_string(Inst, DummyInstVarset,
                !.ModuleInfo),
            InvalidInstPieces = [
                words("Error: the inst"),
                quote(InstStr),
                words("is not a valid inst for a mutable declaration.")     
            ],
            % XXX we could provide more information about exactly *why* the
            % inst was not valid here as well.
            InvalidInstMsg = simple_msg(Context, [always(InvalidInstPieces)]),
            InvalidInstSpec = error_spec(severity_error,
                phase_parse_tree_to_hlds, [InvalidInstMsg]),
            !:Specs = [ InvalidInstSpec | !.Specs ]
        )
    ;
        DefinedThisModule = no
    ).

:- pred add_solver_type_mutable_items_pass_2(list(item)::in, prog_context::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_mutable_items_pass_2([], _Context, !Status,
        !ModuleInfo, !Specs).
add_solver_type_mutable_items_pass_2([Item | Items], Context, !Status,
        !ModuleInfo, !Specs) :-
    add_item_decl_pass_2(Item, Context, !Status, !ModuleInfo, !Specs),
    add_solver_type_mutable_items_pass_2(Items, Context, !Status,
        !ModuleInfo, !Specs).

    % Check to see if there is a valid foreign_name attribute for this backend.
    % If so, use it as the name of the global variable in the target code,
    % otherwise take the Mercury name for the mutable and mangle it into
    % an appropriate variable name.
    %
 :- pred get_global_name_from_foreign_names(module_info::in, bool::in,
    prog_context::in, module_name::in, string::in, list(foreign_name)::in,
    string::out, list(error_spec)::in, list(error_spec)::out) is det.

get_global_name_from_foreign_names(ModuleInfo, ReportErrors, Context,
        ModuleName, MercuryMutableName, ForeignNames, TargetMutableName,
        !Specs) :-
    solutions(get_matching_foreign_name(ForeignNames, lang_c),
        TargetMutableNames),
    (
        TargetMutableNames = [],
        TargetMutableName = mutable_c_var_name(ModuleName, MercuryMutableName)
    ;
        TargetMutableNames = [foreign_name(_, TargetMutableName)]
        % XXX We should really check that this is a valid identifier
        % in the target language here.
    ;
        TargetMutableNames = [_, _ | _],
        (
            ReportErrors = yes,
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, CompilationTarget),
            Pieces = [words("Error: multiple foreign_name attributes"),
                words("specified for the"),
                fixed(compilation_target_string(CompilationTarget)),
                words("backend."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            ReportErrors = no
        ),
        TargetMutableName = mutable_c_var_name(ModuleName, MercuryMutableName)
    ).

:- pred get_matching_foreign_name(list(foreign_name)::in,
    foreign_language::in, foreign_name::out) is nondet.

get_matching_foreign_name(ForeignNames, ForeignLanguage, ForeignName) :-
    list.member(ForeignName, ForeignNames),
    ForeignName = foreign_name(ForeignLanguage, _).

%-----------------------------------------------------------------------------%

add_item_clause(Item, !Status, Context, !ModuleInfo, !QualInfo, !Specs) :-
    Item = item_clause(Origin, VarSet, PredOrFunc, PredName, Args, Body),
    ( !.Status = status_exported ->
        (
            Origin = user,
            list.length(Args, Arity),

            % There is no point printing out the qualified name since that
            % information is already in the context.
            UnqualifiedPredName = unqualify_name(PredName),
            ClauseId = simple_call_id_to_string(PredOrFunc,
                unqualified(UnqualifiedPredName) / Arity),
            error_is_exported(Context, "clause for " ++ ClauseId, !Specs)
        ;
            Origin = compiler(Details),
            (
                % Ignore clauses that are introduced as a result of
                % `initialise', `finalise' or `mutable' declarations
                % or pragma memos.
                ( Details = initialise_decl
                ; Details = finalise_decl
                ; Details = mutable_decl
                ; Details = pragma_memo_attribute
                )
            ;
                ( Details = solver_type
                ; Details = foreign_imports
                ),
                unexpected(this_file, "Bad introduced clauses.")
            )
        )
    ;
        true
    ),
    % At this stage we only need know that it's not a promise declaration.
    module_add_clause(VarSet, PredOrFunc, PredName, Args, Body, !.Status,
        Context, goal_type_none, !ModuleInfo, !QualInfo, !Specs).
add_item_clause(Item, !Status, Context, !ModuleInfo, !QualInfo, !Specs) :-
    Item = item_type_defn(_TVarSet, SymName, TypeParams, TypeDefn, _Cond),
    % If this is a solver type then we need to also add clauses
    % the compiler generated inst cast predicate (the declaration
    % for which was added in pass 1).  We should only add the clauses
    % if this is the module in which the solver type was defined though.
    (
        TypeDefn = parse_tree_solver_type(SolverTypeDetails, _MaybeUserEqComp),
        status_defined_in_this_module(!.Status) = yes
    ->
        add_solver_type_clause_items(SymName, TypeParams, SolverTypeDetails,
            !Status, Context, !ModuleInfo, !QualInfo, !Specs),
        MutableItems = SolverTypeDetails ^ mutable_items,
        add_solver_type_mutable_items_clauses(MutableItems,
            !Status, Context, !ModuleInfo, !QualInfo, !Specs)
    ;
        true
    ).
add_item_clause(Item, !Status, _, !ModuleInfo, !QualInfo, !Specs) :-
    Item = item_inst_defn(_, _, _, _, _).
add_item_clause(Item, !Status, _, !ModuleInfo, !QualInfo, !Specs) :-
    Item = item_mode_defn(_, _, _, _, _).
add_item_clause(Item, !Status, Context, !ModuleInfo, !QualInfo, !Specs) :-
    Item = item_pred_or_func(_, _, _, _, PredOrFunc, SymName, TypesAndModes,
        _WithType, _WithInst, _, _, _, _),
    (
        PredOrFunc = predicate
    ;
        PredOrFunc = function,
        list.length(TypesAndModes, PredArity),
        adjust_func_arity(function, FuncArity, PredArity),
        maybe_check_field_access_function(SymName, FuncArity, !.Status,
            Context, !.ModuleInfo, !Specs)
    ).
add_item_clause(Item, !Status, _, !ModuleInfo, !QualInfo, !Specs) :-
    Item = item_pred_or_func_mode(_, _, _, _, _, _, _).
add_item_clause(Item, !Status, _, !ModuleInfo, !QualInfo, !Specs) :-
    Item = item_module_defn(_, Defn),
    ( Defn = md_version_numbers(ModuleName, ModuleVersionNumbers) ->
        % Record the version numbers for each imported module
        % if smart recompilation is enabled.
        RecordPred = (pred(RecompInfo0::in, RecompInfo::out) is det :-
            RecompInfo = RecompInfo0 ^ version_numbers ^
                map.elem(ModuleName) := ModuleVersionNumbers
        ),
        apply_to_recompilation_info(RecordPred, !QualInfo)
    ; module_defn_update_import_status(Defn, ItemStatus1) ->
        ItemStatus1 = item_status(!:Status, NeedQual),
        qual_info_get_mq_info(!.QualInfo, MQInfo0),
        mq_info_set_need_qual_flag(NeedQual, MQInfo0, MQInfo),
        qual_info_set_mq_info(MQInfo, !QualInfo)
    ;
        true
    ).
add_item_clause(Item, !Status, Context, !ModuleInfo, !QualInfo, !Specs) :-
    Item = item_pragma(Origin, Pragma),
    (
        Pragma = pragma_foreign_proc(Attributes, Pred, PredOrFunc,
            Vars, ProgVarSet, InstVarSet, PragmaImpl),
        module_add_pragma_foreign_proc(Attributes, Pred, PredOrFunc,
            Vars, ProgVarSet, InstVarSet, PragmaImpl, !.Status, Context,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        Pragma = pragma_import(Name, PredOrFunc, Modes, Attributes,
            C_Function),
        module_add_pragma_import(Name, PredOrFunc, Modes, Attributes,
            C_Function, !.Status, Context, !ModuleInfo, !QualInfo, !Specs)
    ;
        Pragma = pragma_fact_table(Pred, Arity, File),
        module_add_pragma_fact_table(Pred, Arity, File, !.Status,
            Context, !ModuleInfo, !QualInfo, !Specs)
    ;
        Pragma = pragma_tabled(Type, Name, Arity, PredOrFunc, MaybeModes,
            MaybeAttributes),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, type_layout, TypeLayout),
        (
            TypeLayout = yes,
            module_add_pragma_tabled(Type, Name, Arity, PredOrFunc, MaybeModes,
                MaybeAttributes, !Status, Context, !ModuleInfo, !QualInfo,
                !Specs)
        ;
            TypeLayout = no,
            Pieces = [words("Error:"),
                quote(":- pragma " ++ eval_method_to_string(Type)),
                words("declaration requires type_ctor_layout structures."),
                words("Don't use --no-type-layout to disable them."),
                nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Pragma = pragma_type_spec(_, _, _, _, _, _, _, _),
        % XXX For the Java back-end, `pragma type_spec' can result in
        % class names that exceed the limits on file name length.
        % So we ignore these pragmas for the Java back-end.
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        ( 
            Target = target_java
        ;
            ( Target = target_c
            ; Target = target_il
            ; Target = target_asm
            ),
            add_pragma_type_spec(Pragma, Context, !ModuleInfo, !QualInfo,
                !Specs)
        )
    ;
        Pragma = pragma_termination_info(PredOrFunc, SymName, ModeList,
            MaybeArgSizeInfo, MaybeTerminationInfo),
        add_pragma_termination_info(PredOrFunc, SymName, ModeList,
            MaybeArgSizeInfo, MaybeTerminationInfo, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_termination2_info(PredOrFunc, SymName, ModeList,
            MaybeSuccessArgSizeInfo, MaybeFailureArgSizeInfo,
            MaybeTerminationInfo),
        add_pragma_termination2_info(PredOrFunc, SymName, ModeList,
            MaybeSuccessArgSizeInfo, MaybeFailureArgSizeInfo,
            MaybeTerminationInfo, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_structure_sharing(PredOrFunc, SymName, ModeList,
            HeadVars, Types, SharingDomain),
        add_pragma_structure_sharing(PredOrFunc, SymName, ModeList,
            HeadVars, Types, SharingDomain, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_structure_reuse(PredOrFunc, SymName, ModeList,
            HeadVars, Types, MaybeReuseDomain),
        add_pragma_structure_reuse(PredOrFunc, SymName, ModeList,
            HeadVars, Types, MaybeReuseDomain, Context, !ModuleInfo, !Specs)
    ;
        Pragma = pragma_reserve_tag(TypeName, TypeArity),
        add_pragma_reserve_tag(TypeName, TypeArity, !.Status, Context,
            !ModuleInfo, !Specs)
    ;
        Pragma = pragma_foreign_export(Lang, Name, PredOrFunc, Modes,
            C_Function),
        add_pragma_foreign_export(Origin, Lang, Name, PredOrFunc, Modes,
            C_Function, Context, !ModuleInfo, !Specs)
    ;
        % Don't worry about any pragma declarations other than the
        % clause-like pragmas (c_code, tabling and fact_table),
        % foreign_type and the termination_info pragma here,
        % since they've already been handled earlier, in pass 2.
        ( Pragma = pragma_check_termination(_, _)
        ; Pragma = pragma_does_not_terminate(_, _)
        ; Pragma = pragma_exceptions(_, _, _, _, _)
        ; Pragma = pragma_foreign_code(_, _)
        ; Pragma = pragma_foreign_decl(_, _, _)
        ; Pragma = pragma_foreign_import_module(_, _)
        ; Pragma = pragma_inline(_, _)
        ; Pragma = pragma_mm_tabling_info(_, _, _, _, _)
        ; Pragma = pragma_mode_check_clauses(_, _)
        ; Pragma = pragma_no_inline(_, _)
        ; Pragma = pragma_obsolete(_, _)
        ; Pragma = pragma_promise_equivalent_clauses(_, _)
        ; Pragma = pragma_promise_pure(_, _)
        ; Pragma = pragma_promise_semipure(_, _)
        ; Pragma = pragma_source_file(_)
        ; Pragma = pragma_terminates(_, _)
        ; Pragma = pragma_trailing_info(_, _, _, _, _)
        ; Pragma = pragma_unused_args(_, _, _, _, _)
        )
    ).
add_item_clause(item_promise(PromiseType, Goal, VarSet, UnivVars),
        !Status, Context, !ModuleInfo, !QualInfo, !Specs) :-
    % If the outermost universally quantified variables are placed in the head
    % of the dummy predicate, the typechecker will avoid warning about unbound
    % type variables as this implicitly adds a universal quantification of the
    % type variables needed.
    term.var_list_to_term_list(UnivVars, HeadVars),
    (
        % Extra error checking for promise ex declarations.
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        check_promise_ex_decl(UnivVars, PromiseType, Goal, Context, !Specs)
    ;
        PromiseType = promise_type_true
    ),
    % Add as dummy predicate.
    add_promise_clause(PromiseType, HeadVars, VarSet, Goal, Context,
        !.Status, !ModuleInfo, !QualInfo, !Specs).
add_item_clause(item_nothing(_), !Status, _, !ModuleInfo, !QualInfo, !Specs).
add_item_clause(item_typeclass(_, _, _, _, _, _), !Status, _, !ModuleInfo,
        !QualInfo, !Specs).
add_item_clause(item_instance(_, _, _, _, _, _), !Status, _, !ModuleInfo,
        !QualInfo, !Specs).
add_item_clause(item_initialise(user, SymName, Arity), !Status, Context,
        !ModuleInfo, !QualInfo, !Specs) :-
    % To handle a `:- initialise initpred.' declaration we need to:
    % (1) construct a new C function name, CName, to use to export initpred,
    % (2) add the export pragma that does this
    % (3) record the initpred/cname pair in the ModuleInfo so that
    % code generation can ensure cname is called during module initialisation.

    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    (
        predicate_table_search_pred_sym_arity(PredTable,
            may_be_partially_qualified, SymName, Arity, PredIds)
    ->
        ( PredIds = [PredId] ->
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            pred_info_get_procedures(PredInfo, ProcTable),
            ProcInfos = map.values(ProcTable),
            ExportLang = lang_c,
            (
                ArgTypes = [Arg1Type, Arg2Type],
                type_is_io_state(Arg1Type),
                type_is_io_state(Arg2Type),
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [ di_mode, uo_mode ],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det ; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_pure
            ->
                module_info_new_user_init_pred(SymName, CName, !ModuleInfo),
                PragmaExportItem =
                    item_pragma(compiler(initialise_decl),
                        pragma_foreign_export(ExportLang, SymName, predicate,
                            [di_mode, uo_mode], CName)),
                add_item_clause(PragmaExportItem, !Status, Context,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                ArgTypes = [],
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_impure
            ->
                module_info_new_user_init_pred(SymName, CName, !ModuleInfo),
                PragmaExportedItem =
                    item_pragma(compiler(initialise_decl),
                        pragma_foreign_export(ExportLang, SymName, predicate,
                            [], CName)),
                add_item_clause(PragmaExportedItem, !Status, Context,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                    words("used in initialise declaration has"),
                    words("invalid signature."), nl],
                % TODO: provide verbose error information here.
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                words("used in initialise declaration"),
                words("multiple pred declarations."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
            words("used in initialise declaration"),
            words("does not have a corresponding pred declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).
add_item_clause(item_initialise(compiler(Details), SymName, _Arity),
        !Status, Context, !ModuleInfo, !QualInfo, !Specs) :-
    % The compiler introduces initialise declarations that call impure
    % predicates as part of the source-to-source transformation for mutable
    % variables.  These predicates *must* be impure in order to prevent the
    % compiler optimizing them away.

    (
        Details = mutable_decl,
        module_info_new_user_init_pred(SymName, CName, !ModuleInfo),
        ExportLang = lang_c,    % XXX Implement for other backends.
        PragmaExportItem =
            item_pragma(compiler(mutable_decl),
                pragma_foreign_export(ExportLang, SymName, predicate, [],
                    CName)),
        add_item_clause(PragmaExportItem, !Status, Context,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        ( Details = initialise_decl
        ; Details = finalise_decl
        ; Details = solver_type
        ; Details = pragma_memo_attribute
        ; Details = foreign_imports
        ),
        unexpected(this_file, "Bad introduced initialise declaration.")
    ).
add_item_clause(item_finalise(Origin, SymName, Arity),
        !Status, Context, !ModuleInfo, !QualInfo, !Specs) :-
    % To handle a `:- finalise finalpred.' declaration we need to:
    % (1) construct a new C function name, CName, to use to export finalpred,
    % (2) add `:- pragma foreign_export("C", finalpred(di, uo), CName).',
    % (3) record the finalpred/cname pair in the ModuleInfo so that
    % code generation can ensure cname is called during module finalisation.
    
    ( 
        Origin = compiler(_),
        unexpected(this_file, "Bad introduced finalise declaration.")
    ;
        Origin = user 
    ),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    (
        predicate_table_search_pred_sym_arity(PredTable,
            may_be_partially_qualified, SymName, Arity, PredIds)
    ->
        (
            PredIds = [PredId],
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            pred_info_get_procedures(PredInfo, ProcTable),
            ProcInfos = map.values(ProcTable),
            % XXX We currently only support finalise declarations for the C
            % backends.
            ExportLang = lang_c,
            (
                ArgTypes = [Arg1Type, Arg2Type],
                type_is_io_state(Arg1Type),
                type_is_io_state(Arg2Type),
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [ di_mode, uo_mode ],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det ; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_pure
            ->
                module_info_new_user_final_pred(SymName, CName, !ModuleInfo),
                PragmaExportItem =
                    item_pragma(compiler(finalise_decl),
                        pragma_foreign_export(ExportLang, SymName, predicate,
                            [di_mode, uo_mode], CName)),
                add_item_clause(PragmaExportItem, !Status, Context,
                    !ModuleInfo, !QualInfo, !Specs)
            ;
                ArgTypes = [],
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_impure
            ->
                module_info_new_user_final_pred(SymName, CName, !ModuleInfo),
                PragmaExportItem =
                    item_pragma(compiler(finalise_decl),
                        pragma_foreign_export(ExportLang, SymName, predicate,
                            [], CName)),
                add_item_clause(PragmaExportItem, !Status, Context,
                    !ModuleInfo, !QualInfo, !Specs)
            ;

                Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                    words("used in finalise declaration"),
                    words("has invalid signature."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            PredIds = [],
            Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                words("used in finalise declaration"),
                words("has no pred declarations."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        ;
            PredIds = [_, _ | _],
            Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                words("used in finalise declaration"),
                words("has multiple pred declarations."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
            words("used in finalise declaration"),
            words("does not have a corresponding pred declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).
add_item_clause(Item, !Status, Context, !ModuleInfo, !QualInfo, !Specs) :-
    Item = item_mutable(MercuryMutableName, Type, InitTerm, Inst,
        MutAttrs, MutVarset),

    % The transformation here is documented in the comments at the
    % beginning of prog_mutable.m.

    DefinedThisModule = status_defined_in_this_module(!.Status),
    (
        DefinedThisModule = yes,
        module_info_get_name(!.ModuleInfo, ModuleName),
        IsConstant = mutable_var_constant(MutAttrs),

        % Work out what name to give the global in the target language.
        decide_mutable_target_var_name(!.ModuleInfo, MutAttrs, ModuleName,
            MercuryMutableName, Context, TargetMutableName, !Specs),

        % Add foreign_decl and foreign_code items that declare/define the
        % global variable used to implement the mutable.  If the mutable is
        % not constant then add a mutex to synchronize access to it as well.
        add_mutable_defn_and_decl(TargetMutableName, Type, IsConstant,
            Context, !QualInfo, !ModuleInfo, !Specs),

        % Set up the default attributes for the foreign_procs used for the
        % access predicates.
        % XXX Handle target languages other than C here.
        Attrs0 = default_attributes(lang_c),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, mutable_always_boxed, AlwaysBoxed),
        (
            AlwaysBoxed = yes,
            BoxPolicy = always_boxed
        ;
            AlwaysBoxed = no,
            BoxPolicy = native_if_possible
        ),
        set_box_policy(BoxPolicy, Attrs0, Attrs1),
        set_may_call_mercury(proc_will_not_call_mercury, Attrs1, Attrs),

        (
            IsConstant = yes,
            InitSetPredName = mutable_secret_set_pred_sym_name(ModuleName,
                MercuryMutableName),
            add_constant_mutable_access_preds(TargetMutableName,
                ModuleName, MercuryMutableName, Attrs, Inst, BoxPolicy,
                Context, !Status, !QualInfo, !ModuleInfo, !Specs)
        ;
            IsConstant = no,
            InitSetPredName = mutable_set_pred_sym_name(ModuleName,
                MercuryMutableName),
            add_mutable_primitive_preds(TargetMutableName, ModuleName,
                MercuryMutableName, MutAttrs, Attrs, Inst, BoxPolicy, Context,
                !Status, !QualInfo, !ModuleInfo, !Specs),
            add_mutable_user_access_preds(ModuleName, MercuryMutableName,
                MutAttrs, Context, !Status, !QualInfo, !ModuleInfo, !Specs)
        ),
        add_mutable_initialisation(IsConstant, TargetMutableName, ModuleName,
            MercuryMutableName, MutVarset, InitSetPredName, InitTerm, Attrs,
            !Status, Context, !ModuleInfo, !QualInfo, !Specs)
    ;
        DefinedThisModule = no
    ).

    % Decide what the name of the underlying global used to implement the
    % mutable should be.  If there is a foreign_name attribute then use that
    % otherwise construct one based on the Mercury name for the mutable
    %
:- pred decide_mutable_target_var_name(module_info::in,
    mutable_var_attributes::in, module_name::in, string::in, prog_context::in,
    string::out, list(error_spec)::in, list(error_spec)::out) is det.

decide_mutable_target_var_name(ModuleInfo, MutAttrs, ModuleName, Name, Context,
        TargetMutableName, !Specs) :-
    mutable_var_maybe_foreign_names(MutAttrs) = MaybeForeignNames,
    (
        MaybeForeignNames = no,
        TargetMutableName = mutable_c_var_name(ModuleName, Name)
    ;
        MaybeForeignNames = yes(ForeignNames),
        ReportErrors = no, % We've already reported them during pass 2.
        get_global_name_from_foreign_names(ModuleInfo, ReportErrors, Context,
            ModuleName, Name, ForeignNames, TargetMutableName, !Specs)
    ).
    
    % Add the foreign_decl and foreign_code items that declare/define
    % the global variable used to hold the mutable.
    %
:- pred add_mutable_defn_and_decl(string::in, mer_type::in, bool::in,
    prog_context::in, qual_info::in, qual_info::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mutable_defn_and_decl(TargetMutableName, Type, IsConstant, Context,
        !QualInfo, !ModuleInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_target(Globals, CompilationTarget),

    % We add the foreign code declaration and definition here rather than
    % in pass 2 because the target-language-specific type name depends on
    % whether there are any foreign_type declarations for Type.
    (
        CompilationTarget = target_c,
        get_mutable_global_foreign_decl_defn(!.ModuleInfo, Type,
            TargetMutableName, IsConstant, ForeignDecl, ForeignDefn),
        ItemStatus0 = item_status(status_local, may_be_unqualified),
        add_item_decl_pass_2(ForeignDecl, Context, ItemStatus0, _,
            !ModuleInfo, !Specs),
        add_item_decl_pass_2(ForeignDefn, Context, ItemStatus0, _,
            !ModuleInfo, !Specs)
    ;
        % The error message was printed in pass 2.
        ( CompilationTarget = target_il
        ; CompilationTarget = target_java
        ; CompilationTarget = target_asm
        ),
        true
    ).

    % Add the access predicates for constant mutables.
    %
:- pred add_constant_mutable_access_preds(string::in, module_name::in,
    string::in, pragma_foreign_proc_attributes::in, mer_inst::in,
    box_policy::in, prog_context::in, import_status::in, import_status::out,
    qual_info::in, qual_info::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_constant_mutable_access_preds(TargetMutableName, ModuleName, Name,
        Attrs, Inst, BoxPolicy, Context, !Status, !QualInfo, !ModuleInfo,
        !Specs) :-
    varset.new_named_var(varset.init, "X", X, ProgVarSet),
    InstVarSet = varset.init,
    set_purity(purity_pure, Attrs, ConstantGetAttrs0),
    set_thread_safe(proc_thread_safe, ConstantGetAttrs0, ConstantGetAttrs),
    ConstantGetForeignProc = pragma_foreign_proc(
        ConstantGetAttrs,
        mutable_get_pred_sym_name(ModuleName, Name),
        predicate,
        [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
        ProgVarSet,
        InstVarSet,
        fc_impl_ordinary("X = " ++ TargetMutableName ++ ";", yes(Context))
    ),
    ConstantGetClause = item_pragma(compiler(mutable_decl),
        ConstantGetForeignProc),
    add_item_clause(ConstantGetClause, !Status, Context, !ModuleInfo,
        !QualInfo, !Specs),

    % NOTE: we don't need to trail the set action, since it is executed
    % only once at initialization time.

    ConstantSetForeignProc = pragma_foreign_proc(Attrs,
        mutable_secret_set_pred_sym_name(ModuleName, Name),
        predicate,
        [pragma_var(X, "X", in_mode(Inst), BoxPolicy)],
        ProgVarSet,
        InstVarSet,
        fc_impl_ordinary(TargetMutableName ++ " = X;", yes(Context))
    ),
    ConstantSetClause = item_pragma(compiler(mutable_decl),
        ConstantSetForeignProc),
    add_item_clause(ConstantSetClause, !Status, Context, !ModuleInfo,
        !QualInfo, !Specs).
            
    % Add the foreign clauses for the mutable's primitive access and 
    % locking predicates.
    %
:- pred add_mutable_primitive_preds(string::in, module_name::in, string::in,
    mutable_var_attributes::in, pragma_foreign_proc_attributes::in,
    mer_inst::in, box_policy::in, prog_context::in,
    import_status::in, import_status::out, qual_info::in, qual_info::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mutable_primitive_preds(TargetMutableName, ModuleName, Name,
        MutAttrs, Attrs, Inst, BoxPolicy, Context, !Status, !QualInfo,
        !ModuleInfo, !Specs) :-
    set_thread_safe(proc_thread_safe, Attrs, LockAndUnlockAttrs),

    % Construct the lock predicate.

    MutableMutexVarName = mutable_mutex_var_name(TargetMutableName),
    % XXX the second argument should be the name of the mercury predicate,
    % with chars escaped as appropriate.
    LockForeignProcBody = string.append_list([
        "#ifdef MR_THREAD_SAFE\n",
        "  MR_LOCK(&" ++ MutableMutexVarName ++ ",
            \"" ++ MutableMutexVarName ++ "\");\n" ++
        "#endif\n"
    ]),
    LockForeignProc = pragma_foreign_proc(LockAndUnlockAttrs,
        mutable_lock_pred_sym_name(ModuleName, Name),
        predicate,
        [],
        varset.init,    % Prog varset.
        varset.init,    % Inst varset.
        fc_impl_ordinary(LockForeignProcBody, yes(Context))
    ),
    LockClause = item_pragma(compiler(mutable_decl), LockForeignProc),
    add_item_clause(LockClause, !Status, Context, !ModuleInfo, !QualInfo,
        !Specs),

    % Construct the unlock predicate.
    % XXX as above regarding the second argument to MR_UNLOCK.

    UnlockForeignProcBody = string.append_list([
        "#ifdef MR_THREAD_SAFE\n",
        "  MR_UNLOCK(&" ++ MutableMutexVarName ++ ",
            \"" ++ MutableMutexVarName ++ "\");\n" ++
        "#endif\n"
    ]),
    UnlockForeignProc = pragma_foreign_proc(LockAndUnlockAttrs,
        mutable_unlock_pred_sym_name(ModuleName, Name),
        predicate,
        [],
        varset.init,    % Prog varset.
        varset.init,    % Inst varset.
        fc_impl_ordinary(UnlockForeignProcBody, yes(Context))
    ),
    UnlockClause = item_pragma(compiler(mutable_decl), UnlockForeignProc),
    add_item_clause(UnlockClause, !Status, Context, !ModuleInfo, !QualInfo,
        !Specs),

    % Construct the semipure unsafe_get_predicate.

    set_purity(purity_semipure, Attrs, UnsafeGetAttrs0),
    set_thread_safe(proc_thread_safe, UnsafeGetAttrs0, UnsafeGetAttrs), 
    varset.new_named_var(varset.init, "X", X, ProgVarSet),
    UnsafeGetForeignProc = pragma_foreign_proc(UnsafeGetAttrs,
        mutable_unsafe_get_pred_sym_name(ModuleName, Name),
        predicate,
        [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
        ProgVarSet,
        varset.init, % Inst varset.
        fc_impl_ordinary("X = " ++ TargetMutableName ++ ";", yes(Context))
    ),
    UnsafeGetClause = item_pragma(compiler(mutable_decl),
        UnsafeGetForeignProc),
    add_item_clause(UnsafeGetClause, !Status, Context, !ModuleInfo, !QualInfo,
        !Specs),

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
    UnsafeSetForeignProc = pragma_foreign_proc(UnsafeSetAttrs,
        mutable_unsafe_set_pred_sym_name(ModuleName, Name),
        predicate,
        [pragma_var(X, "X", in_mode(Inst), BoxPolicy)],
        ProgVarSet,
        varset.init, % Inst varset.
        fc_impl_ordinary(TrailCode ++ TargetMutableName ++ "= X;",
            yes(Context))
    ),
    UnsafeSetClause = item_pragma(compiler(mutable_decl),
        UnsafeSetForeignProc),
    add_item_clause(UnsafeSetClause, !Status, Context, !ModuleInfo, !QualInfo,
        !Specs).

    % Add the access predicates for a non-constant mutable.
    % If the mutable has the `attach_to_io_state' attribute then add the
    % versions of the access preds that take the I/O state as well.
    %
:- pred add_mutable_user_access_preds(module_name::in, string::in,
    mutable_var_attributes::in, prog_context::in,
    import_status::in, import_status::out, qual_info::in, qual_info::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
 
add_mutable_user_access_preds(ModuleName, Name, MutAttrs, Context,
        !Status, !QualInfo, !ModuleInfo, !Specs) :-
    varset.new_named_var(varset.init, "X", X, ProgVarSet0),
    LockPredName   = mutable_lock_pred_sym_name(ModuleName, Name),
    UnlockPredName = mutable_unlock_pred_sym_name(ModuleName, Name),
    SetPredName = mutable_set_pred_sym_name(ModuleName, Name),
    GetPredName = mutable_get_pred_sym_name(ModuleName, Name),
    CallLock   = call_expr(LockPredName, [], purity_impure) - Context,
    CallUnlock = call_expr(UnlockPredName, [], purity_impure) - Context,
    % 
    % Construct the semipure get predicate.
    %
    UnsafeGetPredName = mutable_unsafe_get_pred_sym_name(ModuleName, Name),
    UnsafeGetCallArgs = [variable(X, Context)],
    CallUnsafeGet = call_expr(UnsafeGetPredName, UnsafeGetCallArgs,
        purity_semipure) - Context,
    
    GetBody = goal_list_to_conj(Context,
        [CallLock, CallUnsafeGet, CallUnlock]),
    StdGetBody = promise_purity_expr(dont_make_implicit_promises,
        purity_semipure, GetBody) - Context,

    StdGetClause = item_clause(
        compiler(mutable_decl),
        ProgVarSet0,
        predicate,
        GetPredName,
        [variable(X, context_init)],
        StdGetBody
    ),
    
    add_item_clause(StdGetClause, !Status, Context, !ModuleInfo, !QualInfo,
        !Specs),
    %
    % Construct the impure set predicate.
    %
    UnsafeSetPredName = mutable_unsafe_set_pred_sym_name(ModuleName, Name),
    UnsafeSetCallArgs = [variable(X, context_init)],
    StdSetCallUnsafeSet = call_expr(UnsafeSetPredName, UnsafeSetCallArgs,
        purity_impure) - Context,

    StdSetBody = goal_list_to_conj(Context,
        [CallLock, StdSetCallUnsafeSet, CallUnlock]),

    StdSetClause = item_clause(
        compiler(mutable_decl),
        ProgVarSet0,
        predicate,
        SetPredName,
        [variable(X, context_init)],
        StdSetBody
    ),
    
    add_item_clause(StdSetClause, !Status, Context, !ModuleInfo, !QualInfo,
        !Specs),
    
    IOStateInterface = mutable_var_attach_to_io_state(MutAttrs),
    (
        IOStateInterface = yes,
        varset.new_named_var(ProgVarSet0, "IO", IO, ProgVarSet),

        % Construct the pure get predicate.
        % 
        IOGetBody = promise_purity_expr(dont_make_implicit_promises,
            purity_pure, GetBody) - Context,
    
        Ctxt = context_init,
        IOGetClause = item_clause(
            compiler(mutable_decl),
            ProgVarSet,
            predicate,
            GetPredName,
            [variable(X, Ctxt), variable(IO, Ctxt), variable(IO, Ctxt)],
            IOGetBody
        ),
    
        add_item_clause(IOGetClause, !Status, Context, !ModuleInfo, !QualInfo,
            !Specs),

        % Construct the pure set predicate.
        %
        % We just use the body of impure version and attach a promise_pure
        % pragma to the predicate.  (The purity pragma was added during
        % stage 2.)
        %
        IOSetBody = StdSetBody, 
        
        IOSetClause = item_clause(
            compiler(mutable_decl),
            ProgVarSet,
            predicate,
            SetPredName,
            [variable(X, Ctxt), variable(IO, Ctxt), variable(IO, Ctxt)],
            IOSetBody
        ),
        
        add_item_clause(IOSetClause, !Status, Context, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        IOStateInterface = no
    ).

    % Add the code required to initialise a mutable.
    %
:- pred add_mutable_initialisation(bool::in, string::in, module_name::in,
    string::in, prog_varset::in, sym_name::in, prog_term::in,
    pragma_foreign_proc_attributes::in, import_status::in, import_status::out,
    prog_context::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mutable_initialisation(IsConstant, TargetMutableName, ModuleName, Name,
    MutVarset, InitSetPredName, InitTerm, Attrs, !Status, Context,
    !ModuleInfo, !QualInfo, !Specs) :-
    %
    % Add the `:- initialise' declaration and clause for the
    % mutable initialise predicate.
    %
    add_item_clause(item_initialise(compiler(mutable_decl),
            mutable_init_pred_sym_name(ModuleName, Name), 0 /* Arity */),
        !Status, Context, !ModuleInfo, !QualInfo, !Specs),
    (
        IsConstant = yes,
        %
        % See the comments for prog_io.parse_mutable_decl for the reason
        % why we _must_ use MutVarset here.
        %
        InitClause = item_clause(compiler(mutable_decl),
            MutVarset,
            predicate,
            mutable_init_pred_sym_name(ModuleName, Name), [],
            call_expr(InitSetPredName, [InitTerm], purity_impure) 
                - Context)
    ;
        IsConstant = no,
        % Construct the clause for the mutex initialisation predicate.
        InitMutexCode = string.append_list([
            "#ifdef MR_THREAD_SAFE\n",
            "   pthread_mutex_init(&",
                    mutable_mutex_var_name(TargetMutableName),
                    ", MR_MUTEX_ATTR);\n",
            "#endif\n"
        ]),
        InitMutexPredName = mutable_init_mutex_pred_sym_name(ModuleName,
            Name),
        InitMutexForeignProc = pragma_foreign_proc(Attrs,
            InitMutexPredName,
            predicate,
            [],
            varset.init,    % ProgVarSet
            varset.init,    % InstVarSet
            fc_impl_ordinary(InitMutexCode, yes(Context))
        ),
        InitMutexClause = item_pragma(compiler(mutable_decl),
            InitMutexForeignProc),
        add_item_clause(InitMutexClause, !Status, Context, !ModuleInfo,
            !QualInfo, !Specs),
      
        CallInitMutexExpr =
            call_expr(InitMutexPredName, [], purity_impure) - Context,
        CallSetPredExpr = 
            call_expr(InitSetPredName, [InitTerm], purity_impure)
                - Context,
        InitClauseExpr = conj_expr(CallInitMutexExpr, CallSetPredExpr)
            - Context,
        %
        % See the comments for prog_io.parse_mutable_decl for the reason
        % why we _must_ use MutVarset here.
        %
        InitClause = item_clause(compiler(mutable_decl),
            MutVarset,
            predicate,
            mutable_init_pred_sym_name(ModuleName, Name),
            [],
            InitClauseExpr
        )
    ),
    add_item_clause(InitClause, !Status, Context, !ModuleInfo, !QualInfo,
        !Specs).
    
    % Create the foreign_decl for the mutable.
    % The bool argument says whether the mutable is a constant mutable
    % or not.
    %
:- pred get_mutable_global_foreign_decl_defn(module_info::in, mer_type::in,
    string::in, bool::in, item::out, item::out) is det.

get_mutable_global_foreign_decl_defn(ModuleInfo, Type, TargetMutableName,
        IsConstant, Decl, Defn) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, mutable_always_boxed, AlwaysBoxed),
    globals.get_target(Globals, Backend),
    (
        Backend = target_c,
        TypeName = global_foreign_type_name(AlwaysBoxed, lang_c, ModuleInfo,
            Type),
        %
        % Constant mutables do not require mutexes as their values are never
        % updated.
        %
        ( 
            IsConstant = yes,
            LockDecl = []
        ;
            IsConstant = no,
            LockDecl = [
                "#ifdef MR_THREAD_SAFE\n",
                "    extern MercuryLock ",
                mutable_mutex_var_name(TargetMutableName), ";\n",
                "#endif\n"
            ]
        ),
        DeclBody = string.append_list([
            "extern ", TypeName, " ", TargetMutableName, ";\n" | LockDecl]),
        Decl = item_pragma(compiler(mutable_decl),
            pragma_foreign_decl(lang_c, foreign_decl_is_exported, DeclBody)),
        
        (
            IsConstant = yes,
            LockDefn = []
        ;
            IsConstant = no,
            LockDefn = [
                "#ifdef MR_THREAD_SAFE\n",
                "    MercuryLock ",
                mutable_mutex_var_name(TargetMutableName), ";\n",
                "#endif\n"
            ]
        ),
        DefnBody = string.append_list([
            TypeName, " ", TargetMutableName, ";\n" | LockDefn]),
        Defn = item_pragma(compiler(mutable_decl),
            pragma_foreign_code(lang_c, DefnBody))
    ;
        ( Backend = target_il
        ; Backend = target_java
        ; Backend = target_asm
        ),
        sorry(this_file, "we don't yet support mutables for non-C backends")
    ).
    
:- func global_foreign_type_name(bool, foreign_language, module_info, mer_type)
    = string.

global_foreign_type_name(yes, _, _, _) = "MR_Word".
global_foreign_type_name(no, Lang, ModuleInfo, Type) =
    to_type_string(Lang, ModuleInfo, Type).

:- pred add_solver_type_mutable_items_clauses(list(item)::in,
    import_status::in, import_status::out, prog_context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_mutable_items_clauses([], !Status, _Context,
        !ModuleInfo, !QualInfo, !Specs).
add_solver_type_mutable_items_clauses([Item | Items], !Status, Context,
        !ModuleInfo, !QualInfo, !Specs) :-
    add_item_clause(Item, !Status, Context, !ModuleInfo, !QualInfo, !Specs),
    add_solver_type_mutable_items_clauses(Items, !Status, Context,
        !ModuleInfo, !QualInfo, !Specs).

    % If a module_defn updates the import_status, return the new status
    % and whether uses of the following items must be module qualified,
    % otherwise fail.
    %
:- pred module_defn_update_import_status(module_defn::in, item_status::out)
    is semidet.

module_defn_update_import_status(md_interface,
        item_status(status_exported, may_be_unqualified)).
module_defn_update_import_status(md_implementation,
        item_status(status_local, may_be_unqualified)).
module_defn_update_import_status(md_private_interface,
        item_status(status_exported_to_submodules, may_be_unqualified)).
module_defn_update_import_status(md_imported(Section),
        item_status(status_imported(Section), may_be_unqualified)).
module_defn_update_import_status(md_used(Section),
        item_status(status_imported(Section), must_be_qualified)).
module_defn_update_import_status(md_opt_imported,
        item_status(status_opt_imported, must_be_qualified)).
module_defn_update_import_status(md_abstract_imported,
        item_status(status_abstract_imported, must_be_qualified)).

%-----------------------------------------------------------------------------%

:- pred add_promise_clause(promise_type::in, list(term(prog_var_type))::in,
    prog_varset::in, goal::in, prog_context::in, import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_promise_clause(PromiseType, HeadVars, VarSet, Goal, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    term.context_line(Context, Line),
    term.context_file(Context, File),
    string.format(prog_out.promise_to_string(PromiseType) ++
        "__%d__%s", [i(Line), s(File)], Name),

    % Promise declarations are recorded as a predicate with a goal_type
    % of goal_type_promise(X), where X is of promise_type. This allows us
    % to leverage off all the other checks in the compiler that operate
    % on predicates.
    %
    % :- promise all [A,B,R] ( R = A + B <=> R = B + A ).
    %
    % becomes
    %
    % promise.lineno_filename(A, B, R) :-
    %   ( R = A + B <=> R = B + A ).
    %
    module_info_get_name(!.ModuleInfo, ModuleName),
    module_add_clause(VarSet, predicate, qualified(ModuleName, Name), HeadVars,
        Goal, Status, Context, goal_type_promise(PromiseType),
        !ModuleInfo, !QualInfo, !Specs).

add_stratified_pred(PragmaName, Name, Arity, Context, !ModuleInfo, !Specs) :-
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    (
        predicate_table_search_sym_arity(PredTable0, is_fully_qualified,
            Name, Arity, PredIds)
    ->
        module_info_get_stratified_preds(!.ModuleInfo, StratPredIds0),
        set.insert_list(StratPredIds0, PredIds, StratPredIds),
        module_info_set_stratified_preds(StratPredIds, !ModuleInfo)
    ;
        string.append_list(["`:- pragma ", PragmaName, "' declaration"],
            Description),
        undefined_pred_or_func_error(Name, Arity, Context, Description,
            !Specs)
    ).

%-----------------------------------------------------------------------------%

add_pred_marker(PragmaName, Name, Arity, Status, Context, Marker,
        ConflictMarkers, !ModuleInfo, !Specs) :-
    ( marker_must_be_exported(Marker) ->
        MustBeExported = yes
    ;
        MustBeExported = no
    ),
    do_add_pred_marker(PragmaName, Name, Arity, Status, MustBeExported,
        Context, add_marker_pred_info(Marker), !ModuleInfo, PredIds, !Specs),
    module_info_preds(!.ModuleInfo, Preds),
    pragma_check_markers(Preds, PredIds, ConflictMarkers, Conflict),
    (
        Conflict = yes,
        pragma_conflict_error(Name, Arity, Context, PragmaName, !Specs)
    ;
        Conflict = no
    ).

do_add_pred_marker(PragmaName, Name, Arity, Status, MustBeExported, Context,
        UpdatePredInfo, !ModuleInfo, PredIds, !Specs) :-
    ( get_matching_pred_ids(!.ModuleInfo, Name, Arity, PredIds0) ->
        PredIds = PredIds0,
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_get_preds(PredTable0, Preds0),

        pragma_add_marker(PredIds, UpdatePredInfo, Status,
            MustBeExported, Preds0, Preds, WrongStatus),
        (
            WrongStatus = yes,
            pragma_status_error(Name, Arity, Context, PragmaName, !Specs)
        ;
            WrongStatus = no
        ),

        predicate_table_set_preds(Preds, PredTable0, PredTable),
        module_info_set_predicate_table(PredTable, !ModuleInfo)
    ;
        PredIds = [],
        string.append_list(["`:- pragma ", PragmaName, "' declaration"],
            Description),
        undefined_pred_or_func_error(Name, Arity, Context, Description, !Specs)
    ).

:- pred get_matching_pred_ids(module_info::in, sym_name::in, arity::in,
    list(pred_id)::out) is semidet.

get_matching_pred_ids(Module0, Name, Arity, PredIds) :-
    module_info_get_predicate_table(Module0, PredTable0),
    % Check that the pragma is module qualified.
    (
        Name = unqualified(_),
        unexpected(this_file, "get_matching_pred_ids: unqualified name")
    ;
        Name = qualified(_, _),
        predicate_table_search_sym_arity(PredTable0, is_fully_qualified,
            Name, Arity, PredIds)
    ).

module_mark_as_external(PredName, Arity, Context, !ModuleInfo, !Specs) :-
    % `external' declarations can only apply to things defined in this module,
    % since everything else is already external.
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    (
        predicate_table_search_sym_arity(PredicateTable0, is_fully_qualified,
            PredName, Arity, PredIdList)
    ->
        module_mark_preds_as_external(PredIdList, !ModuleInfo)
    ;
        undefined_pred_or_func_error(PredName, Arity, Context,
            "`:- external' declaration", !Specs)
    ).

:- pred module_mark_preds_as_external(list(pred_id)::in,
    module_info::in, module_info::out) is det.

module_mark_preds_as_external([], !ModuleInfo).
module_mark_preds_as_external([PredId | PredIds], !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    pred_info_mark_as_external(PredInfo0, PredInfo),
    map.det_update(Preds0, PredId, PredInfo, Preds),
    module_info_set_preds(Preds, !ModuleInfo),
    module_mark_preds_as_external(PredIds, !ModuleInfo).

    % For each pred_id in the list, check whether markers present in the list
    % of conflicting markers are also present in the corresponding pred_info.
    % The bool indicates whether there was a conflicting marker present.
    %
:- pred pragma_check_markers(pred_table::in, list(pred_id)::in,
    list(marker)::in, bool::out) is det.

pragma_check_markers(_, [], _, no).
pragma_check_markers(PredTable, [PredId | PredIds], ConflictList,
        WasConflict) :-
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    (
        list.member(Marker, ConflictList),
        check_marker(Markers, Marker)
    ->
        WasConflict = yes
    ;
        pragma_check_markers(PredTable, PredIds, ConflictList, WasConflict)
    ).

    % For each pred_id in the list, add the given markers to the
    % list of markers in the corresponding pred_info.
    %
:- pred pragma_add_marker(list(pred_id)::in,
    add_marker_pred_info::in(add_marker_pred_info), import_status::in,
    bool::in, pred_table::in, pred_table::out, bool::out) is det.

pragma_add_marker([], _, _, _, !PredTable, no).
pragma_add_marker([PredId | PredIds], UpdatePredInfo, Status, MustBeExported,
        !PredTable, WrongStatus) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    call(UpdatePredInfo, PredInfo0, PredInfo),
    (
        pred_info_is_exported(PredInfo),
        MustBeExported = yes,
        Status \= status_exported
    ->
        WrongStatus0 = yes
    ;
        WrongStatus0 = no
    ),
    map.det_update(!.PredTable, PredId, PredInfo, !:PredTable),
    pragma_add_marker(PredIds, UpdatePredInfo, Status,
        MustBeExported, !PredTable, WrongStatus1),
    bool.or(WrongStatus0, WrongStatus1, WrongStatus).

:- pred add_marker_pred_info(marker::in, pred_info::in, pred_info::out) is det.

add_marker_pred_info(Marker, !PredInfo) :-
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(Marker, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo).

    % Succeed if a marker for an exported procedure must also be exported.
    %
:- pred marker_must_be_exported(marker::in) is semidet.

marker_must_be_exported(_) :-
    semidet_fail.

maybe_check_field_access_function(FuncName, FuncArity, Status, Context,
        ModuleInfo, !Specs) :-
    (
        is_field_access_function_name(ModuleInfo, FuncName, FuncArity,
            AccessType, FieldName)
    ->
        check_field_access_function(AccessType, FieldName, FuncName,
            FuncArity, Status, Context, ModuleInfo, !Specs)
    ;
        true
    ).

:- pred check_field_access_function(field_access_type::in, ctor_field_name::in,
    sym_name::in, arity::in, import_status::in, prog_context::in,
    module_info::in, list(error_spec)::in, list(error_spec)::out) is det.

check_field_access_function(_AccessType, FieldName, FuncName, FuncArity,
        FuncStatus, Context, ModuleInfo, !Specs) :-
    adjust_func_arity(function, FuncArity, PredArity),
    FuncCallId = simple_call_id(function, FuncName, PredArity),

    % Check that a function applied to an exported type is also exported.
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    (
        % Abstract types have status `abstract_exported', so errors won't be
        % reported for local field access functions for them.
        map.search(CtorFieldTable, FieldName, [FieldDefn]),
        FieldDefn = hlds_ctor_field_defn(_, DefnStatus, _, _, _),
        DefnStatus = status_exported,
        FuncStatus \= status_exported
    ->
        report_field_status_mismatch(Context, FuncCallId, !Specs)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred report_field_status_mismatch(prog_context::in, simple_call_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_field_status_mismatch(Context, CallId, !Specs) :-
    Pieces = [words("In declaration of"), simple_call(CallId), suffix(":"), nl,
        words("error: a field access function for an exported field"),
        words("must also be exported."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred report_unexpected_decl(string::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unexpected_decl(Descr, Context, !Specs) :-
    Pieces = [words("Error: unexpected or incorrect"),
        quote(Descr), words("declaration."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred pragma_status_error(sym_name::in, int::in, prog_context::in,
    string::in, list(error_spec)::in, list(error_spec)::out) is det.

pragma_status_error(Name, Arity, Context, PragmaName, !Specs) :-
    Pieces = [words("Error: `:- pragma " ++ PragmaName ++ "'"),
        words("declaration for exported predicate or function"),
        sym_name_and_arity(Name / Arity),
        words("must also be exported."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred pragma_conflict_error(sym_name::in, int::in, prog_context::in,
    string::in, list(error_spec)::in, list(error_spec)::out) is det.

pragma_conflict_error(Name, Arity, Context, PragmaName, !Specs) :-
    Pieces = [words("Error: `:- pragma " ++ PragmaName ++ "'"),
        words("declaration conflicts with previous pragma for"),
        sym_name_and_arity(Name / Arity), suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "make_hlds_passes.m".

%-----------------------------------------------------------------------------%
