%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds__make_hlds__add_pragma.
:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__make_hlds__make_hlds_passes.
:- import_module hlds__make_hlds__qual_info.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__mercury_to_mercury.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module std_util.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred add_pragma(item_origin::in, pragma_type::in, prog_context::in,
    item_status::in, item_status::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

:- pred add_pragma_export(item_origin::in, sym_name::in, pred_or_func::in,
    list(mode)::in, string::in, prog_context::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred add_pragma_reserve_tag(sym_name::in, arity::in, import_status::in,
    prog_context::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

:- pred add_pragma_type_spec(pragma_type::in(type_spec), term__context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    io::di, io::uo) is det.

:- pred add_pragma_termination2_info(pred_or_func::in, sym_name::in,
    list(mode)::in, maybe(pragma_constr_arg_size_info)::in,
    maybe(pragma_constr_arg_size_info)::in,
    maybe(pragma_termination_info)::in, prog_context::in, module_info::in,
    module_info::out, io::di, io::uo) is det.

:- pred add_pragma_termination_info(pred_or_func::in, sym_name::in,
    list(mode)::in, maybe(pragma_arg_size_info)::in,
    maybe(pragma_termination_info)::in, prog_context::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

    % module_add_pragma_import:
    %
    % Handles `pragma import' declarations, by figuring out which predicate
    % the `pragma import' declaration applies to, and adding a clause
    % for that predicate containing an appropriate HLDS `pragma_c_code'
    % instruction.
    %
    % NB. Any changes here might also require similar changes to the
    % handling of `pragma export' declarations, in export.m.
    %
:- pred module_add_pragma_import(sym_name::in, pred_or_func::in,
    list(mode)::in, pragma_foreign_proc_attributes::in, string::in,
    import_status::in, prog_context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    io::di, io::uo) is det.

:- pred module_add_pragma_foreign_proc(pragma_foreign_proc_attributes::in,
    sym_name::in, pred_or_func::in, list(pragma_var)::in, prog_varset::in,
    pragma_foreign_code_impl::in, import_status::in, prog_context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    io::di, io::uo) is det.

:- pred module_add_pragma_tabled(eval_method::in, sym_name::in, int::in,
    maybe(pred_or_func)::in, maybe(list(mode))::in, import_status::in,
    prog_context::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

    % module_add_pragma_fact_table(PredName, Arity, FileName,
    %   Status, Context, Module0, Module, !Info):
    %
    % Add a `pragma fact_table' declaration to the HLDS. This predicate calls
    % the fact table compiler (fact_table_compile_facts) to create a separate
    % `.o' file for the fact_table and then creates separate pieces of
    % `pragma c_code' to access the table in each mode of the fact table
    % predicate.
    %
:- pred module_add_pragma_fact_table(sym_name::in, arity::in, string::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out, io::di, io::uo) is det.

:- pred lookup_current_backend(backend::out, io::di, io::uo) is det.

    % Find the procedure with declared argmodes which match the ones we want.
    % If there was no mode declaration, then use the inferred argmodes.
    %
:- pred get_procedure_matching_declmodes(assoc_list(proc_id, proc_info)::in,
    list(mode)::in, module_info::in, proc_id::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs__foreign.
:- import_module check_hlds__mode_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_out.
:- import_module hlds__make_hlds__add_aditi.
:- import_module hlds__make_hlds__add_pred.
:- import_module hlds__make_hlds__make_hlds_error.
:- import_module hlds__make_hlds__make_hlds_passes.
:- import_module hlds__make_hlds__make_hlds_warn.
:- import_module hlds__make_hlds__qual_info.
:- import_module hlds__make_tags.
:- import_module hlds__quantification.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend.
:- import_module ll_backend__fact_table.
:- import_module parse_tree__error_util.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_foreign.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_type.
:- import_module parse_tree__prog_util.
:- import_module recompilation.
:- import_module transform_hlds__term_constr_main.
:- import_module transform_hlds__term_constr_util.
:- import_module transform_hlds__term_util.

:- import_module bag.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module varset.

%-----------------------------------------------------------------------------%

add_pragma(Origin, Pragma, Context, !Status, !ModuleInfo, !IO) :-
    %
    % check for invalid pragmas in the `interface' section
    %
    !.Status = item_status(ImportStatus, _),
    pragma_allowed_in_interface(Pragma, Allowed),
    (
        Allowed = no,
        (
            Origin = user,
            error_if_exported(ImportStatus, Context, "`pragma' declaration",
                !IO)
        ;
            % We don't report this as an error as it just clutters up
            % the compiler output - the *real* error is whatever caused
            % the compiler to create this pragma.
            Origin = compiler(_)
        )
    ;
        Allowed = yes
    ),
    (
        % Ignore `pragma source_file' declarations - they're dealt
        % with elsewhere.
        Pragma = source_file(_)
    ;
        Pragma = foreign_code(Lang, Body_Code),
        module_add_foreign_body_code(Lang, Body_Code, Context, !ModuleInfo)
    ;
        Pragma  = foreign_decl(Lang, IsLocal, C_Header),
        module_add_foreign_decl(Lang, IsLocal, C_Header, Context, !ModuleInfo)
    ;
        Pragma  = foreign_import_module(Lang, Import),
        module_add_foreign_import_module(Lang, Import, Context, !ModuleInfo)
    ;
        % Handle pragma foreign procs later on (when we process clauses).
        Pragma = foreign_proc(_, _, _, _, _, _)
    ;
        % Handle pragma tabled decls later on (when we process clauses).
        Pragma = tabled(_, _, _, _, _)
    ;
        Pragma = inline(Name, Arity),
        add_pred_marker("inline", Name, Arity, ImportStatus, Context,
            inline, [no_inline], !ModuleInfo, !IO)
    ;
        Pragma = no_inline(Name, Arity),
        add_pred_marker("no_inline", Name, Arity, ImportStatus, Context,
            no_inline, [inline], !ModuleInfo, !IO)
    ;
        Pragma = obsolete(Name, Arity),
        add_pred_marker("obsolete", Name, Arity, ImportStatus,
            Context, obsolete, [], !ModuleInfo, !IO)
    ;
        % Handle pragma import decls later on (when we process
        % clauses and pragma c_code).
        Pragma = import(_, _, _, _, _)
    ;
        % Handle pragma export decls later on, after default
        % function modes have been added.
        Pragma = export(_, _, _, _)
    ;
        % Used for inter-module unused argument elimination.
        % This can only appear in .opt files.
        Pragma = unused_args(PredOrFunc, SymName, Arity, ModeNum,
            UnusedArgs),
        ( ImportStatus \= opt_imported ->
            module_info_incr_errors(!ModuleInfo),
            Pieces = [words("Error: illegal use of pragma `unused_args'.")],
            write_error_pieces(Context, 0, Pieces, !IO)
        ;
            add_pragma_unused_args(PredOrFunc, SymName, Arity, ModeNum,
                UnusedArgs, Context, !ModuleInfo, !IO)
        )
    ;
        Pragma = exceptions(PredOrFunc, SymName, Arity, ModeNum, ThrowStatus),
        ( ImportStatus \= opt_imported ->
            module_info_incr_errors(!ModuleInfo),
            Pieces = [words("Error: illegal use of pragma `exceptions'.")],
            write_error_pieces(Context, 0, Pieces, !IO)
        ;
            add_pragma_exceptions(PredOrFunc, SymName, Arity, ModeNum,
                ThrowStatus, Context, !ModuleInfo, !IO)
        )
    ;
        % Handle pragma type_spec decls later on (when we process clauses).
        Pragma = type_spec(_, _, _, _, _, _, _, _)
    ;
        % Handle pragma fact_table decls later on (when we process clauses
        % -- since these decls take the place of clauses).
        Pragma = fact_table(_, _, _)
    ;
        % Handle pragma reserve_tag decls later on (when we process clauses
        % -- they need to be handled after the type definitions
        % have been added).
        Pragma = reserve_tag(_, _)
    ;
        Pragma = aditi(PredName, Arity),
        maybe_enable_aditi_compilation(!.Status, Context, !ModuleInfo, !IO),
        add_pred_marker("aditi", PredName, Arity, ImportStatus, Context,
            aditi, [], !ModuleInfo, !IO),
        add_stratified_pred("aditi", PredName, Arity, Context, !ModuleInfo, !IO)
    ;
        Pragma = base_relation(PredName, Arity),
        maybe_enable_aditi_compilation(!.Status, Context, !ModuleInfo, !IO),
        add_pred_marker("aditi", PredName, Arity, ImportStatus, Context, aditi,
            [], !ModuleInfo, !IO),
        add_pred_marker("base_relation", PredName, Arity, ImportStatus,
            Context, base_relation, [], !ModuleInfo, !IO),
        module_mark_as_external(PredName, Arity, Context, !ModuleInfo, !IO)
    ;
        Pragma = aditi_index(PredName, Arity, Index),
        add_base_relation_index(PredName, Arity, Index, ImportStatus,
            Context, !ModuleInfo, !IO)
    ;
        Pragma = naive(PredName, Arity),
        add_pred_marker("naive", PredName, Arity, ImportStatus,
            Context, naive, [psn], !ModuleInfo, !IO)
    ;
        Pragma = psn(PredName, Arity),
        add_pred_marker("psn", PredName, Arity, ImportStatus,
            Context, psn, [naive], !ModuleInfo, !IO)
    ;
        Pragma = aditi_memo(Name, Arity),
        add_pred_marker("aditi_memo", Name, Arity, ImportStatus,
            Context, aditi_memo, [aditi_no_memo], !ModuleInfo, !IO)
    ;
        Pragma = aditi_no_memo(PredName, Arity),
        add_pred_marker("aditi_no_memo", PredName, Arity, ImportStatus,
            Context, aditi_no_memo, [aditi_memo], !ModuleInfo, !IO)
    ;
        Pragma = supp_magic(PredName, Arity),
        add_pred_marker("supp_magic", PredName, Arity, ImportStatus,
            Context, supp_magic, [context], !ModuleInfo, !IO)
    ;
        Pragma = context(PredName, Arity),
        add_pred_marker("context", PredName, Arity, ImportStatus,
            Context, context, [supp_magic], !ModuleInfo, !IO)
    ;
        Pragma = owner(PredName, Arity, Owner),
        set_pred_owner(PredName, Arity, Owner, ImportStatus,
            Context, !ModuleInfo, !IO)
    ;
        Pragma = promise_pure(Name, Arity),
        add_pred_marker("promise_pure", Name, Arity, ImportStatus,
            Context, promised_pure, [], !ModuleInfo, !IO)
    ;
        Pragma = promise_semipure(Name, Arity),
        add_pred_marker("promise_semipure", Name, Arity, ImportStatus,
            Context, promised_semipure, [], !ModuleInfo, !IO)
    ;
        % Handle pragma termination_info decls later on, in pass 3 --
        % we need to add function default modes before handling
        % these pragmas
        Pragma = termination_info(_, _, _, _, _)
    ;
        % As for termination_info pragmas
        Pragma = termination2_info(_, _, _, _, _, _)
    ;
        Pragma = terminates(Name, Arity),
        add_pred_marker("terminates", Name, Arity, ImportStatus, Context,
            terminates, [check_termination, does_not_terminate], !ModuleInfo,
            !IO)
    ;
        Pragma = does_not_terminate(Name, Arity),
        add_pred_marker("does_not_terminate", Name, Arity, ImportStatus,
            Context, does_not_terminate, [check_termination, terminates],
            !ModuleInfo, !IO)
    ;
        Pragma = check_termination(Name, Arity),
        add_pred_marker("check_termination", Name, Arity, ImportStatus,
            Context, check_termination, [terminates, does_not_terminate],
            !ModuleInfo, !IO)
    ;
        Pragma = mode_check_clauses(Name, Arity),
        add_pred_marker("mode_check_clauses", Name, Arity, ImportStatus,
            Context, mode_check_clauses, [], !ModuleInfo, !IO),

        % Allowing the predicate to be inlined could lead to code generator
        % aborts. This is because the caller that inlines this predicate may
        % then push other code into the disjunction or switch's branches,
        % which would invalidate the instmap_deltas that the mode_check_clauses
        % feature prevents the recomputation of.
        add_pred_marker("mode_check_clauses", Name, Arity, ImportStatus,
            Context, no_inline, [inline], !ModuleInfo, !IO)
    ).

add_pragma_export(Origin, Name, PredOrFunc, Modes, C_Function, Context,
        !ModuleInfo, !IO) :-
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    list__length(Modes, Arity),
    (
        predicate_table_search_pf_sym_arity(PredTable,
            may_be_partially_qualified, PredOrFunc, Name, Arity, [PredId])
    ->
        predicate_table_get_preds(PredTable, Preds),
        map__lookup(Preds, PredId, PredInfo),
        pred_info_procedures(PredInfo, Procs),
        map__to_assoc_list(Procs, ExistingProcs),
        (
            get_procedure_matching_declmodes(ExistingProcs, Modes,
                !.ModuleInfo, ProcId)
        ->
            map__lookup(Procs, ProcId, ProcInfo),
            proc_info_declared_determinism(ProcInfo, MaybeDet),
            % We cannot catch those multi or nondet procedures that
            % don't have a determinism declaration until after
            % determinism analysis.
            (
                MaybeDet = yes(Det),
                ( Det = nondet ; Det = multidet )
            ->
                Pieces = [words("Error: "),
                    fixed("`:- pragma export' declaration"),
                    words("for a procedure that has"),
                    words("a declared determinism of"),
                    fixed(hlds_out.determinism_to_string(Det) ++ ".")
                ],
                error_util.write_error_pieces(Context, 0, Pieces, !IO),
                module_info_incr_errors(!ModuleInfo)
            ;
                module_info_get_pragma_exported_procs(!.ModuleInfo,
                    PragmaExportedProcs0),
                NewExportedProc = pragma_exported_proc(PredId, ProcId,
                    C_Function, Context),
                PragmaExportedProcs = [NewExportedProc | PragmaExportedProcs0],
                module_info_set_pragma_exported_procs(PragmaExportedProcs,
                    !ModuleInfo)
            )
        ;
            % We warn about errors in export pragmas created by the compiler
            % as part of a source-to-source transformation.
            (
                Origin = user,
                undefined_mode_error(Name, Arity, Context,
                    "`:- pragma export' declaration", !IO),
                module_info_incr_errors(!ModuleInfo)
            ;
                Origin = compiler(Details),
                (
                    Details = initialise_decl
                ;
                    Details = mutable_decl
                ;
                    Details = solver_type,
                    unexpected(this_file, "Bad introduced export pragma.")
                ;      
                    Details = foreign_imports,
                    unexpected(this_file, "Bad introduced export pragma.")
                )
            )
        )
    ;   ( 
            Origin = user,
            undefined_pred_or_func_error(Name, Arity, Context,
                "`:- pragma export' declaration", !IO),
            module_info_incr_errors(!ModuleInfo)
        ;
            Origin = compiler(Details),
            (
                Details = initialise_decl
            ;      
                Details = mutable_decl
            ;
                Details = solver_type,
                unexpected(this_file, "Bad introduced export pragma.")
            ;       
                Details = foreign_imports,
                unexpected(this_file, "Bad introduced export pragma.")
            )
        )
    ).

%-----------------------------------------------------------------------------%

add_pragma_reserve_tag(TypeName, TypeArity, PragmaStatus, Context, !ModuleInfo,
        !IO) :-
    TypeCtor = TypeName - TypeArity,
    module_info_types(!.ModuleInfo, Types0),
    TypeStr = error_util__describe_sym_name_and_arity(TypeName / TypeArity),
    ErrorPieces1 = [
        words("In"),
        fixed("`pragma reserve_tag'"),
        words("declaration for"),
        fixed(TypeStr ++ ":")
    ],
    ( map__search(Types0, TypeCtor, TypeDefn0) ->
        hlds_data__get_type_defn_body(TypeDefn0, TypeBody0),
        hlds_data__get_type_defn_status(TypeDefn0, TypeStatus),
        (
            not (
                TypeStatus = PragmaStatus
            ;
                TypeStatus = abstract_exported,
                ( PragmaStatus = local
                ; PragmaStatus = exported_to_submodules
                )
            )
        ->
            write_error_pieces(Context, 0, ErrorPieces1, !IO),
            ErrorPieces2 = [
                words("error: `reserve_tag' declaration must"),
                words("have the same visibility as the"),
                words("type definition.")
            ],
            write_error_pieces_not_first_line(Context, 0, ErrorPieces2, !IO),
            io__set_exit_status(1, !IO),
            module_info_incr_errors(!ModuleInfo)

        ;
            TypeBody0 = du_type(Body, _CtorTags0, _IsEnum0,
                MaybeUserEqComp, ReservedTag0, IsForeign)
        ->
            (
                ReservedTag0 = yes,
                % make doubly sure that we don't get any
                % spurious warnings with intermodule
                % optimization...
                TypeStatus \= opt_imported
            ->
                write_error_pieces(Context, 0, ErrorPieces1, !IO),
                ErrorPieces2 = [
                    words("warning: multiple"),
                    fixed("`pragma reserved_tag'"),
                    words("declarations for the same type.")
                ],
                write_error_pieces_not_first_line(Context, 0, ErrorPieces2,
                    !IO)
            ;
                true
            ),
            %
            % We passed all the semantic checks.
            % Mark the type has having a reserved tag,
            % and recompute the constructor tags.
            %
            ReservedTag = yes,
            module_info_globals(!.ModuleInfo, Globals),
            assign_constructor_tags(Body, TypeCtor, ReservedTag, Globals,
                CtorTags, IsEnum),
            TypeBody = du_type(Body, CtorTags, IsEnum, MaybeUserEqComp,
                ReservedTag, IsForeign),
            hlds_data__set_type_defn_body(TypeBody, TypeDefn0, TypeDefn),
            map__set(Types0, TypeCtor, TypeDefn, Types),
            module_info_set_types(Types, !ModuleInfo)
        ;
            write_error_pieces(Context, 0, ErrorPieces1, !IO),
            ErrorPieces2 = [
                words("error:"),
                fixed(TypeStr),
                words("is not a discriminated union type.")
            ],
            write_error_pieces_not_first_line(Context, 0, ErrorPieces2, !IO),
            io__set_exit_status(1, !IO),
            module_info_incr_errors(!ModuleInfo)
        )
    ;
        write_error_pieces(Context, 0, ErrorPieces1, !IO),
        ErrorPieces2 = [
            words("error: undefined type"),
            fixed(TypeStr ++ ".")
        ],
        write_error_pieces_not_first_line(Context, 0, ErrorPieces2, !IO),
        io__set_exit_status(1, !IO),
        module_info_incr_errors(!ModuleInfo)
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_unused_args(pred_or_func::in, sym_name::in, arity::in,
    mode_num::in, list(int)::in, prog_context::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

add_pragma_unused_args(PredOrFunc, SymName, Arity, ModeNum, UnusedArgs,
        Context, !ModuleInfo, !IO) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, [PredId])
    ->
        module_info_unused_arg_info(!.ModuleInfo, UnusedArgInfo0),
        % convert the mode number to a proc_id
        proc_id_to_int(ProcId, ModeNum),
        map__set(UnusedArgInfo0, proc(PredId, ProcId), UnusedArgs,
            UnusedArgInfo),
        module_info_set_unused_arg_info(UnusedArgInfo, !ModuleInfo)
    ;
        module_info_incr_errors(!ModuleInfo),
        Pieces = [words("Internal compiler error: "),
            words("unknown predicate in `pragma unused_args'.")],
        write_error_pieces(Context, 0, Pieces, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred add_pragma_exceptions(pred_or_func::in, sym_name::in, arity::in,
    mode_num::in, exception_status::in, prog_context::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

add_pragma_exceptions(PredOrFunc, SymName, Arity, ModeNum, ThrowStatus,
        _Context, !ModuleInfo, !IO) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, [PredId])
    ->
        module_info_exception_info(!.ModuleInfo, ExceptionsInfo0),
        % convert the mode number to a proc_id
        proc_id_to_int(ProcId, ModeNum),
        map__set(ExceptionsInfo0, proc(PredId, ProcId), ThrowStatus,
            ExceptionsInfo),
        module_info_set_exception_info(ExceptionsInfo, !ModuleInfo)
    ;
        % XXX We'll just ignore this for the time being -
        % it causes errors with transitive-intermodule optimization.
        %prog_out__write_context(Context, !IO),
        %io__write_string("Internal compiler error: " ++
        %   "unknown predicate in `pragma exceptions'.\n", !IO),
        %module_info_incr_errors(!ModuleInfo)
        true
    ).

%-----------------------------------------------------------------------------%

add_pragma_type_spec(Pragma, Context, !ModuleInfo, !QualInfo, !IO) :-
    Pragma = type_spec(SymName, _, Arity, MaybePredOrFunc, _, _, _, _),
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    (
        (
            MaybePredOrFunc = yes(PredOrFunc),
            adjust_func_arity(PredOrFunc, Arity, PredArity),
            predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
                PredOrFunc, SymName, PredArity, PredIds)
        ;
            MaybePredOrFunc = no,
            predicate_table_search_sym_arity(Preds, is_fully_qualified,
                SymName, Arity, PredIds)
        ),
        PredIds \= []
    ->
        list__foldl3(add_pragma_type_spec_2(Pragma, Context), PredIds,
            !ModuleInfo, !QualInfo, !IO)
    ;
        undefined_pred_or_func_error(SymName, Arity, Context,
            "`:- pragma type_spec' declaration", !IO),
        module_info_incr_errors(!ModuleInfo)
    ).

:- pred add_pragma_type_spec_2(pragma_type::in(type_spec), prog_context::in,
    pred_id::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out, io::di, io::uo) is det.

add_pragma_type_spec_2(Pragma0, Context, PredId, !ModuleInfo, !QualInfo,
        !IO) :-
    Pragma0 = type_spec(SymName, SpecName, Arity, _, MaybeModes, Subst,
        TVarSet0, ExpandedItems),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    handle_pragma_type_spec_subst(Context, Subst, PredInfo0,
        TVarSet0, TVarSet, Types, ExistQVars, ClassContext, SubstOk,
        !ModuleInfo, !IO),
    (
        SubstOk = yes(RenamedSubst),
        pred_info_procedures(PredInfo0, Procs0),
        handle_pragma_type_spec_modes(SymName, Arity, Context, MaybeModes,
            ProcIds, Procs0, Procs, ModesOk, !ModuleInfo, !IO),
        globals__io_lookup_bool_option(user_guided_type_specialization,
            DoTypeSpec, !IO),
        globals__io_lookup_bool_option(smart_recompilation, Smart, !IO),
        (
            ModesOk = yes,
            % Even if we aren't doing type specialization, we need
            % to create the interface procedures for local
            % predicates to check the type-class correctness of
            % the requested specializations.
            %
            % If we're doing smart recompilation we need to record
            % the pragmas even if we aren't doing type
            % specialization to avoid problems with differing
            % output for the recompilation tests in debugging
            % grades.
            %
            ( DoTypeSpec = yes
            ; \+ pred_info_is_imported(PredInfo0)
            ; Smart = yes
            )
        ->
            %
            % Build a clause to call the old predicate with the
            % specified types to force the specialization.
            % For imported predicates this forces the creation
            % of the proper interface.
            %
            PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
            adjust_func_arity(PredOrFunc, Arity, PredArity),
            varset__init(ArgVarSet0),
            make_n_fresh_vars("HeadVar__", PredArity, Args,
                ArgVarSet0, ArgVarSet),
            % XXX We could use explicit type qualifications here
            % for the argument types, but explicit type
            % qualification doesn't work correctly with type
            % inference due to a bug somewhere in typecheck.m
            % -- the explicitly declared types are not kept in
            % sync with the predicate's tvarset after the first
            % pass of type checking.
            % map__from_corresponding_lists(Args, Types, VarTypes0)
            map__init(VarTypes0),
            goal_info_init(GoalInfo0),
            set__list_to_set(Args, NonLocals),
            goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
            goal_info_set_context(Context, GoalInfo1, GoalInfo),

            %
            % We don't record the called predicate as used -- it
            % is only used if there is some other call. This call
            % is only used to make higher_order.m generate
            % the interface for the type specialized procedure, and
            % will be removed by higher_order.m after that is done.
            %
            do_construct_pred_or_func_call(PredId, PredOrFunc,
                SymName, Args, GoalInfo, Goal),
            Clause = clause(ProcIds, Goal, mercury, Context),
            map__init(TVarNameMap),
            rtti_varmaps_init(RttiVarMaps),
            HasForeignClauses = no,
            set_clause_list([Clause], ClausesRep),
            Clauses = clauses_info(ArgVarSet, VarTypes0, TVarNameMap,
                VarTypes0, Args, ClausesRep, RttiVarMaps, HasForeignClauses),
            pred_info_get_markers(PredInfo0, Markers0),
            add_marker(calls_are_fully_qualified, Markers0, Markers),
            map__init(Proofs),
            map__init(ConstraintMap),

            ( pred_info_is_imported(PredInfo0) ->
                Status = opt_imported
            ;
                pred_info_import_status(PredInfo0, Status)
            ),

            ModuleName = pred_info_module(PredInfo0),
            pred_info_get_aditi_owner(PredInfo0, Owner),
            pred_info_get_origin(PredInfo0, OrigOrigin),
            SubstDesc = list__map(subst_desc, Subst),
            Origin = transformed(type_specialization(SubstDesc),
                OrigOrigin, PredId),
            pred_info_init(ModuleName, SpecName, PredArity, PredOrFunc,
                Context, Origin, Status, none, Markers, Types, TVarSet,
                ExistQVars, ClassContext, Proofs, ConstraintMap, Owner,
                Clauses, NewPredInfo0),
            pred_info_set_procedures(Procs, NewPredInfo0, NewPredInfo),
            module_info_get_predicate_table(!.ModuleInfo, PredTable0),
            predicate_table_insert(NewPredInfo, NewPredId,
                PredTable0, PredTable),
            module_info_set_predicate_table(PredTable,
                !ModuleInfo),

            %
            % Record the type specialisation in the module_info.
            %
            module_info_type_spec_info(!.ModuleInfo, TypeSpecInfo0),
            TypeSpecInfo0 = type_spec_info(ProcsToSpec0,
                ForceVersions0, SpecMap0, PragmaMap0),
            list__map((pred(ProcId::in, PredProcId::out) is det :-
                    PredProcId = proc(PredId, ProcId)
                ), ProcIds, PredProcIds),
            set__insert_list(ProcsToSpec0, PredProcIds,
                ProcsToSpec),
            set__insert(ForceVersions0, NewPredId, ForceVersions),

            ( Status = opt_imported ->
                % For imported predicates dead_proc_elim.m
                % needs to know that if the original predicate
                % is used, the predicate to force the
                % production of the specialised interface is
                % also used.
                multi_map__set(SpecMap0, PredId, NewPredId, SpecMap)
            ;
                SpecMap = SpecMap0
            ),
            Pragma = type_spec(SymName, SpecName, Arity, yes(PredOrFunc),
                MaybeModes, map__to_assoc_list(RenamedSubst), TVarSet,
                ExpandedItems),
            multi_map__set(PragmaMap0, PredId, Pragma, PragmaMap),
            TypeSpecInfo = type_spec_info(ProcsToSpec, ForceVersions, SpecMap,
                PragmaMap),
            module_info_set_type_spec_info(TypeSpecInfo,
                !ModuleInfo),

            status_is_imported(Status, IsImported),
            (
                IsImported = yes,
                ItemType = pred_or_func_to_item_type(PredOrFunc),
                apply_to_recompilation_info(
                    recompilation__record_expanded_items(
                        item_id(ItemType, SymName - Arity), ExpandedItems),
                    !QualInfo)
            ;
                IsImported = no
            )
        ;
            true
        )
    ;
        SubstOk = no
    ).

:- func subst_desc(pair(tvar, type)) = pair(int, type).

subst_desc(TVar - Type) = var_to_int(TVar) - Type.

    % Check that the type substitution for a `:- pragma type_spec'
    % declaration is valid.
    % A type substitution is invalid if:
    %   - it substitutes unknown type variables
    %   - it substitutes existentially quantified type variables
    % Type substitutions are also invalid if the replacement types are
    % not ground, however this is a (hopefully temporary) limitation
    % of the current implementation, so it only results in a warning.
:- pred handle_pragma_type_spec_subst(prog_context::in,
    assoc_list(tvar, type)::in, pred_info::in, tvarset::in, tvarset::out,
    list(type)::out, existq_tvars::out, prog_constraints::out,
    maybe(tsubst)::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

handle_pragma_type_spec_subst(Context, Subst, PredInfo0, TVarSet0, TVarSet,
        Types, ExistQVars, ClassContext, SubstOk, !ModuleInfo, !IO) :-
    assoc_list__keys(Subst, VarsToSub),
    (
        Subst = []
    ->
        error("handle_pragma_type_spec_subst: empty substitution")
    ;
        find_duplicate_list_elements(VarsToSub, MultiSubstVars0),
        MultiSubstVars0 \= []
    ->
        list__sort_and_remove_dups(MultiSubstVars0, MultiSubstVars),
        report_multiple_subst_vars(PredInfo0, Context, TVarSet0,
            MultiSubstVars, !IO),
        module_info_incr_errors(!ModuleInfo),
        io__set_exit_status(1, !IO),
        ExistQVars = [],
        Types = [],
        ClassContext = constraints([], []),
        varset__init(TVarSet),
        SubstOk = no
    ;
        pred_info_typevarset(PredInfo0, CalledTVarSet),
        varset__create_name_var_map(CalledTVarSet, NameVarIndex0),
        list__filter((pred(Var::in) is semidet :-
            varset__lookup_name(TVarSet0, Var, VarName),
            \+ map__contains(NameVarIndex0, VarName)
        ), VarsToSub, UnknownVarsToSub),
        (
            UnknownVarsToSub = [],
            % Check that the substitution is not recursive.
            set__list_to_set(VarsToSub, VarsToSubSet),

            assoc_list__values(Subst, SubstTypes0),
            prog_type__vars_list(SubstTypes0, TVarsInSubstTypes0),
            set__list_to_set(TVarsInSubstTypes0,
                TVarsInSubstTypes),

            set__intersect(TVarsInSubstTypes, VarsToSubSet, RecSubstTVars0),
            set__to_sorted_list(RecSubstTVars0, RecSubstTVars),

            ( RecSubstTVars = [] ->
                map__init(TVarRenaming0),
                list__append(VarsToSub, TVarsInSubstTypes0, VarsToReplace),

                get_new_tvars(VarsToReplace, TVarSet0, CalledTVarSet, TVarSet,
                    NameVarIndex0, _, TVarRenaming0, TVarRenaming),

                % Check that none of the existentially
                % quantified variables were substituted.
                map__apply_to_list(VarsToSub, TVarRenaming, RenamedVarsToSub),
                pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
                list__filter((pred(RenamedVar::in) is semidet :-
                    list__member(RenamedVar, ExistQVars)
                ), RenamedVarsToSub, SubExistQVars),
                (
                    SubExistQVars = [],
                    map__init(TypeSubst0),
                    apply_variable_renaming_to_type_list(TVarRenaming,
                        SubstTypes0, SubstTypes),
                    assoc_list__from_corresponding_lists(RenamedVarsToSub,
                        SubstTypes, SubAL),
                    list__foldl(map_set_from_pair, SubAL,
                        TypeSubst0, TypeSubst),

                    % Apply the substitution.
                    pred_info_arg_types(PredInfo0, Types0),
                    pred_info_get_class_context(PredInfo0, ClassContext0),
                    apply_rec_subst_to_type_list(TypeSubst, Types0, Types),
                    apply_rec_subst_to_prog_constraints(TypeSubst,
                        ClassContext0, ClassContext),
                    SubstOk = yes(TypeSubst)
                ;
                    SubExistQVars = [_ | _],
                    report_subst_existq_tvars(PredInfo0, Context,
                        SubExistQVars, !IO),
                    io__set_exit_status(1, !IO),
                    module_info_incr_errors(!ModuleInfo),
                    Types = [],
                    ClassContext = constraints([], []),
                    SubstOk = no
                )
            ;
                report_recursive_subst(PredInfo0, Context, TVarSet0,
                    RecSubstTVars, !IO),
                io__set_exit_status(1, !IO),
                module_info_incr_errors(!ModuleInfo),
                ExistQVars = [],
                Types = [],
                ClassContext = constraints([], []),
                varset__init(TVarSet),
                SubstOk = no
            )
        ;
            UnknownVarsToSub = [_ | _],
            report_unknown_vars_to_subst(PredInfo0, Context, TVarSet0,
                UnknownVarsToSub, !IO),
            module_info_incr_errors(!ModuleInfo),
            io__set_exit_status(1, !IO),
            ExistQVars = [],
            Types = [],
            ClassContext = constraints([], []),
            varset__init(TVarSet),
            SubstOk = no
        )
    ).

:- pred map_set_from_pair(pair(K, V)::in, map(K, V)::in, map(K, V)::out)
    is det.

map_set_from_pair(K - V, !Map) :-
    svmap__set(K, V, !Map).

:- pred find_duplicate_list_elements(list(T)::in, list(T)::out) is det.

find_duplicate_list_elements([], []).
find_duplicate_list_elements([H | T], Vars) :-
    find_duplicate_list_elements(T, Vars0),
    ( list__member(H, T) ->
        Vars = [H | Vars0]
    ;
        Vars = Vars0
    ).

:- pred report_subst_existq_tvars(pred_info::in, prog_context::in,
    list(tvar)::in, io::di, io::uo) is det.

report_subst_existq_tvars(PredInfo, Context, SubExistQVars, !IO) :-
    pred_info_typevarset(PredInfo, TVarSet),
    Pieces = report_pragma_type_spec(PredInfo) ++
        [words("error: the substitution includes"),
        words("the existentially quantified type"),
        words(report_variables(SubExistQVars, TVarSet)), suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO).

:- pred report_recursive_subst(pred_info::in, prog_context::in, tvarset::in,
    list(tvar)::in, io::di, io::uo) is det.

report_recursive_subst(PredInfo, Context, TVarSet, RecursiveVars, !IO) :-
    ( RecursiveVars = [_] ->
        Occurs = "occurs"
    ;
        Occurs = "occur"
    ),
    Pieces = report_pragma_type_spec(PredInfo) ++
        [words("error:"), words(report_variables(RecursiveVars, TVarSet)),
        words(Occurs), words("on both sides of the substitution.")],
    write_error_pieces(Context, 0, Pieces, !IO).

:- pred report_multiple_subst_vars(pred_info::in, prog_context::in,
    tvarset::in, list(tvar)::in, io::di, io::uo) is det.

report_multiple_subst_vars(PredInfo, Context, TVarSet, MultiSubstVars, !IO) :-
    ( MultiSubstVars = [_] ->
        Has = "has"
    ;
        Has = "have"
    ),
    Pieces = report_pragma_type_spec(PredInfo) ++
        [words("error:"), words(report_variables(MultiSubstVars, TVarSet)),
        words(Has), words("multiple replacement types.")],
    write_error_pieces(Context, 0, Pieces, !IO).

:- pred report_unknown_vars_to_subst(pred_info::in, prog_context::in,
    tvarset::in, list(tvar)::in, io::di, io::uo) is det.

report_unknown_vars_to_subst(PredInfo, Context, TVarSet, UnknownVars, !IO) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ( UnknownVars = [_] ->
        DoesNot = "does not"
    ;
        DoesNot = "do not"
    ),
    (
        PredOrFunc = predicate,
        Decl = "`:- pred'"
    ;
        PredOrFunc = function,
        Decl = "`:- func'"
    ),
    Pieces = report_pragma_type_spec(PredInfo) ++
        [words("error:"), words(report_variables(UnknownVars, TVarSet)),
        words(DoesNot), words("occur in the"), fixed(Decl),
        words("declaration.")],
    write_error_pieces(Context, 0, Pieces, !IO).

:- func report_pragma_type_spec(pred_info) = list(format_component).

report_pragma_type_spec(PredInfo) = Pieces :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Pieces = [words("In `:- pragma type_spec' declaration for"),
        simple_call_id(PredOrFunc - qualified(Module, Name)/Arity),
        suffix(":"), nl].

:- func report_variables(list(tvar), tvarset) = string.

report_variables(SubExistQVars, VarSet) = Str :-
    VarsStr = mercury_vars_to_string(SubExistQVars, VarSet, no),
    ( SubExistQVars = [_] ->
        Str = "variable `" ++ VarsStr ++ "'"
    ;
        Str = "variables `" ++ VarsStr ++ "'"
    ).

    % Check that the mode list for a `:- pragma type_spec' declaration
    % specifies a known procedure.
    %
:- pred handle_pragma_type_spec_modes(sym_name::in, arity::in,
    prog_context::in, maybe(list(mode))::in, list(proc_id)::out,
    proc_table::in, proc_table::out, bool::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

handle_pragma_type_spec_modes(SymName, Arity, Context, MaybeModes, ProcIds,
        !Procs, ModesOk, !ModuleInfo, !IO) :-
    (
        MaybeModes = yes(Modes),
        map__to_assoc_list(!.Procs, ExistingProcs),
        (
            get_procedure_matching_argmodes(ExistingProcs, Modes,
                !.ModuleInfo, ProcId)
        ->
            map__lookup(!.Procs, ProcId, ProcInfo),
            map__det_insert(map__init, ProcId, ProcInfo, !:Procs),
            ProcIds = [ProcId],
            ModesOk = yes
        ;
            ProcIds = [],
            module_info_incr_errors(!ModuleInfo),
            undefined_mode_error(SymName, Arity, Context,
                "`:- pragma type_spec' declaration", !IO),
            ModesOk = no
        )
    ;
        MaybeModes = no,
        map__keys(!.Procs, ProcIds),
        ModesOk = yes
    ).

%-----------------------------------------------------------------------------%

add_pragma_termination2_info(PredOrFunc, SymName, ModeList,
        MaybePragmaSuccessArgSizeInfo, MaybePragmaFailureArgSizeInfo,
        MaybePragmaTerminationInfo, Context, !ModuleInfo, !IO) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    list.length(ModeList, Arity),
    (
        predicate_table_search_pf_sym_arity(Preds,
        is_fully_qualified, PredOrFunc, SymName, Arity, PredIds),
        PredIds = [_ | _]
    ->
        ( PredIds = [PredId] ->
            module_info_preds(!.ModuleInfo, PredTable0),
            map.lookup(PredTable0, PredId, PredInfo0),
            pred_info_procedures(PredInfo0, ProcTable0),
            map.to_assoc_list(ProcTable0, ProcList),
            (
                get_procedure_matching_declmodes(ProcList,
                    ModeList, !.ModuleInfo, ProcId)
            ->
                map.lookup(ProcTable0, ProcId, ProcInfo0),
                add_context_to_constr_termination_info(
                    MaybePragmaTerminationInfo, Context,
                    MaybeTerminationInfo),

                some [!TermInfo] (
                    proc_info_get_termination2_info(ProcInfo0, !:TermInfo),

                    !:TermInfo = !.TermInfo ^ import_success :=
                        MaybePragmaSuccessArgSizeInfo,
                    !:TermInfo = !.TermInfo ^ import_failure :=
                        MaybePragmaFailureArgSizeInfo,
                    !:TermInfo = !.TermInfo ^ term_status :=
                        MaybeTerminationInfo,

                    proc_info_set_termination2_info(!.TermInfo,
                        ProcInfo0, ProcInfo)
                ),
                map__det_update(ProcTable0, ProcId, ProcInfo,
                    ProcTable),
                pred_info_set_procedures(ProcTable, PredInfo0,
                    PredInfo),
                map__det_update(PredTable0, PredId, PredInfo,
                    PredTable),
                module_info_set_preds(PredTable, !ModuleInfo)
            ;
                module_info_incr_errors(!ModuleInfo),
                Pieces = [words("Error: `:- pragma termination2_info'"),
                    words("declaration for undeclared mode of"),
                    simple_call_id(PredOrFunc - SymName/Arity), suffix(".")],
                write_error_pieces(Context, 0, Pieces, !IO)
            )
        ;
            module_info_incr_errors(!ModuleInfo),
            Pieces = [words("Error: ambiguous predicate name"),
                simple_call_id(PredOrFunc - SymName/Arity),
                words("in"), fixed("`pragma termination2_info'.")],
            write_error_pieces(Context, 0, Pieces, !IO)
        )
    ;
        % XXX This happens in `.trans_opt' files sometimes --
        % so just ignore it
        true
        %   undefined_pred_or_func_error(
        %        SymName, Arity, Context,
        %        "`:- pragma termination2_info' declaration", !IO),
        %   module_info_incr_errors(!ModuleInfo)
    ).

%------------------------------------------------------------------------------%

add_pragma_termination_info(PredOrFunc, SymName, ModeList,
        MaybePragmaArgSizeInfo, MaybePragmaTerminationInfo,
        Context, !ModuleInfo, !IO) :-
    module_info_get_predicate_table(!.ModuleInfo, Preds),
    list__length(ModeList, Arity),
    (
        predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
            PredOrFunc, SymName, Arity, PredIds),
        PredIds = [_ | _]
    ->
        ( PredIds = [PredId] ->
            module_info_preds(!.ModuleInfo, PredTable0),
            map__lookup(PredTable0, PredId, PredInfo0),
            pred_info_procedures(PredInfo0, ProcTable0),
            map__to_assoc_list(ProcTable0, ProcList),
            (
                get_procedure_matching_declmodes(ProcList, ModeList,
                    !.ModuleInfo, ProcId)
            ->
                add_context_to_arg_size_info(MaybePragmaArgSizeInfo,
                    Context, MaybeArgSizeInfo),
                add_context_to_termination_info(MaybePragmaTerminationInfo,
                    Context, MaybeTerminationInfo),
                map__lookup(ProcTable0, ProcId, ProcInfo0),
                proc_info_set_maybe_arg_size_info(MaybeArgSizeInfo,
                    ProcInfo0, ProcInfo1),
                proc_info_set_maybe_termination_info(MaybeTerminationInfo,
                    ProcInfo1, ProcInfo),
                map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                map__det_update(PredTable0, PredId, PredInfo, PredTable),
                module_info_set_preds(PredTable, !ModuleInfo)
            ;
                module_info_incr_errors(!ModuleInfo),
                Pieces = [words("Error: `:- pragma termination_info'"),
                    words("declaration for undeclared mode of"),
                    simple_call_id(PredOrFunc - SymName/Arity), suffix(".")],
                write_error_pieces(Context, 0, Pieces, !IO)
            )
        ;
            module_info_incr_errors(!ModuleInfo),
            Pieces = [words("Error: ambiguous predicate name"),
                simple_call_id(PredOrFunc - SymName/Arity),
                words("in"), fixed("`pragma termination_info'.")],
            write_error_pieces(Context, 0, Pieces, !IO)
        )
    ;
        % XXX This happens in `.trans_opt' files sometimes --
        % so just ignore it
        true
        %   undefined_pred_or_func_error(SymName, Arity, Context,
        %       "`:- pragma termination_info' declaration",
        %       !IO),
        %   module_info_incr_errors(!ModuleInfo)
    ).

module_add_pragma_import(PredName, PredOrFunc, Modes, Attributes, C_Function,
        Status, Context, !ModuleInfo, !QualInfo, !IO) :-
    module_info_name(!.ModuleInfo, ModuleName),
    list__length(Modes, Arity),

    globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    (
        VeryVerbose = yes,
        io__write_string("% Processing `:- pragma import' for ", !IO),
        write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
        io__write_string("...\n", !IO)
    ;
        VeryVerbose = no
    ),

        % Lookup the pred declaration in the predicate table. (If it's not
        % there, print an error message and insert a dummy declaration
        % for the predicate.)
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    (
        predicate_table_search_pf_sym_arity(PredicateTable0,
            is_fully_qualified, PredOrFunc, PredName, Arity, [PredId0])
    ->
        PredId = PredId0
    ;
        preds_add_implicit_report_error(ModuleName, PredOrFunc,
            PredName, Arity, Status, no, Context, user(PredName),
            "`:- pragma import' declaration", PredId, !ModuleInfo, !IO)
    ),
    % Lookup the pred_info for this pred, and check that it is valid.
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable2),
    predicate_table_get_preds(PredicateTable2, Preds0),
    map__lookup(Preds0, PredId, PredInfo0),
    % Opt_imported preds are initially tagged as imported and are tagged as
    % opt_imported only if/when we see a clause (including a `pragma import'
    % clause) for them.
    ( Status = opt_imported ->
        pred_info_set_import_status(opt_imported, PredInfo0, PredInfo1)
    ;
        PredInfo1 = PredInfo0
    ),
    ( pred_info_is_imported(PredInfo1) ->
        module_info_incr_errors(!ModuleInfo),
        Pieces = [words("Error: `:- pragma import' declaration for imported"),
            simple_call_id(PredOrFunc - PredName/Arity), suffix(".")],
        write_error_pieces(Context, 0, Pieces, !IO)
    ; pred_info_clause_goal_type(PredInfo1) ->
        module_info_incr_errors(!ModuleInfo),
        Pieces = [words("Error: `:- pragma import' declaration for"),
            simple_call_id(PredOrFunc - PredName/Arity),
            words("with preceding clauses.")],
        write_error_pieces(Context, 0, Pieces, !IO)
    ;
        pred_info_update_goal_type(pragmas, PredInfo1, PredInfo2),
            % Add the pragma declaration to the proc_info for this procedure.
        pred_info_procedures(PredInfo2, Procs),
        map__to_assoc_list(Procs, ExistingProcs),
        (
            get_procedure_matching_argmodes(ExistingProcs, Modes,
                !.ModuleInfo, ProcId)
        ->
            pred_add_pragma_import(PredId, ProcId, Attributes, C_Function,
                Context, PredInfo2, PredInfo, !ModuleInfo, !QualInfo, !IO),
            map__det_update(Preds0, PredId, PredInfo, Preds),
            predicate_table_set_preds(Preds,
                PredicateTable2, PredicateTable),
            module_info_set_predicate_table(PredicateTable, !ModuleInfo)
        ;
            module_info_incr_errors(!ModuleInfo),
            Pieces = [words("Error: `:- pragma import' declaration"),
                words("for undeclared mode of"),
                simple_call_id(PredOrFunc - PredName/Arity), suffix(".")],
            write_error_pieces(Context, 0, Pieces, !IO)
        )
    ).

    % Pred_add_pragma_import is a subroutine of module_add_pragma_import
    % which adds the c_code for a `pragma import' declaration to a pred_info.
    %
:- pred pred_add_pragma_import(pred_id::in, proc_id::in,
    pragma_foreign_proc_attributes::in, string::in, prog_context::in,
    pred_info::in, pred_info::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, io::di, io::uo) is det.

pred_add_pragma_import(PredId, ProcId, Attributes, C_Function, Context,
        !PredInfo, !ModuleInfo, !QualInfo, !IO) :-
    pred_info_procedures(!.PredInfo, Procs),
    map__lookup(Procs, ProcId, ProcInfo),
    foreign__make_pragma_import(!.PredInfo, ProcInfo, C_Function, Context,
        PragmaImpl, VarSet, PragmaVars, ArgTypes, Arity, PredOrFunc,
        !ModuleInfo, !IO),

    % Lookup some information we need from the pred_info and proc_info.
    PredName = pred_info_name(!.PredInfo),
    PredModule = pred_info_module(!.PredInfo),
    pred_info_clauses_info(!.PredInfo, Clauses0),
    pred_info_get_purity(!.PredInfo, Purity),

    % Add the code for this `pragma import' to the clauses_info.
    clauses_info_add_pragma_foreign_proc(Purity, Attributes, PredId, ProcId,
        VarSet, PragmaVars, ArgTypes, PragmaImpl, Context, PredOrFunc,
        qualified(PredModule, PredName), Arity, Clauses0, Clauses,
        !ModuleInfo, !IO),

    % Store the clauses_info etc. back into the pred_info.
    pred_info_set_clauses_info(Clauses, !PredInfo).

%-----------------------------------------------------------------------------%

module_add_pragma_foreign_proc(Attributes0, PredName, PredOrFunc, PVars, VarSet,
        PragmaImpl, Status, Context, !ModuleInfo, !QualInfo, !IO) :-
    %
    % Begin by replacing any maybe_thread_safe foreign_proc attributes
    % with the actual thread safety attributes which we get from the
    % `--maybe-thread-safe' option.
    %
    globals__io_get_globals(Globals, !IO),
    globals__get_maybe_thread_safe(Globals, MaybeThreadSafe),
    ThreadSafe = Attributes0 ^ thread_safe,
    ( ThreadSafe = maybe_thread_safe ->
        (
            MaybeThreadSafe = yes,
            set_thread_safe(thread_safe, Attributes0, Attributes)
        ;
            MaybeThreadSafe = no,
            set_thread_safe(not_thread_safe, Attributes0, Attributes)
        )
    ;
        Attributes = Attributes0
    ),
    module_info_name(!.ModuleInfo, ModuleName),
    PragmaForeignLanguage = foreign_language(Attributes),
    list__length(PVars, Arity),
        % print out a progress message
    globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    (
        VeryVerbose = yes,
        io__write_string("% Processing `:- pragma foreign_proc' for ", !IO),
        write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
        io__write_string("...\n", !IO)
    ;
        VeryVerbose = no
    ),

    globals__io_get_backend_foreign_languages(BackendForeignLangs, !IO),

        % Lookup the pred declaration in the predicate table.
        % (If it's not there, print an error message and insert
        % a dummy declaration for the predicate.)
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    (
        predicate_table_search_pf_sym_arity(PredTable0, is_fully_qualified,
            PredOrFunc, PredName, Arity, [PredId0])
    ->
        PredId = PredId0
    ;
        preds_add_implicit_report_error(ModuleName, PredOrFunc,
            PredName, Arity, Status, no, Context, user(PredName),
            "`:- pragma foreign_proc' declaration",
            PredId, !ModuleInfo, !IO)
    ),

        % Lookup the pred_info for this pred, add the pragma to the proc_info
        % in the proc_table in the pred_info, and save the pred_info.
    module_info_get_predicate_table(!.ModuleInfo, PredTable1),
    predicate_table_get_preds(PredTable1, Preds0),
    some [!PredInfo] (
        map__lookup(Preds0, PredId, !:PredInfo),
        PredInfo0 = !.PredInfo,

        % opt_imported preds are initially tagged as imported and are
        % tagged as opt_imported only if/when we see a clause (including
        % a `pragma c_code' clause) for them
        ( Status = opt_imported ->
            pred_info_set_import_status(opt_imported, !PredInfo)
        ;
            true
        ),
        (
            % If this procedure was previously defined as clauses only
            % then we need to turn all the non mode-specific clauses
            % into mode-specific clauses.
            pred_info_clause_goal_type(!.PredInfo)
        ->
            pred_info_clauses_info(!.PredInfo, CInfo0),
            clauses_info_clauses_only(CInfo0, ClauseList0),
            ClauseList = list__map(
                (func(C) = Res :-
                    AllProcIds = pred_info_all_procids(!.PredInfo),
                    ( C = clause([], Goal, mercury, Ctxt) ->
                        Res = clause(AllProcIds, Goal, mercury, Ctxt)
                    ;
                        Res = C
                    )
                ), ClauseList0),
            clauses_info_set_clauses(ClauseList, CInfo0, CInfo),
            pred_info_set_clauses_info(CInfo, !PredInfo)
        ;
            true
        ),
        lookup_current_backend(CurrentBackend, !IO),
        (
            ExtraAttrs = extra_attributes(Attributes),
            is_applicable_for_current_backend(CurrentBackend, ExtraAttrs) = no
        ->
            % Ignore this foreign_proc.
            true
        ;
            pred_info_is_imported(!.PredInfo)
        ->
            module_info_incr_errors(!ModuleInfo),
            Pieces = [words("Error: `:- pragma foreign_proc'"),
                words("(or `pragma c_code')"),
                words("declaration for imported"),
                simple_call_id(PredOrFunc - PredName/Arity), suffix(".")],
            write_error_pieces(Context, 0, Pieces, !IO)
        ;
                % Don't add clauses for foreign languages other
                % than the ones we can generate code for.
            not list__member(PragmaForeignLanguage, BackendForeignLangs)
        ->
            pred_info_update_goal_type(pragmas, PredInfo0, !:PredInfo),
            module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
        ;
            % add the pragma declaration to the proc_info for this procedure
            pred_info_procedures(!.PredInfo, Procs),
            map__to_assoc_list(Procs, ExistingProcs),
            pragma_get_modes(PVars, Modes),
            (
                get_procedure_matching_argmodes(ExistingProcs, Modes,
                    !.ModuleInfo, ProcId)
            ->
                pred_info_clauses_info(!.PredInfo, Clauses0),
                pred_info_arg_types(!.PredInfo, ArgTypes),
                pred_info_get_purity(!.PredInfo, Purity),
                clauses_info_add_pragma_foreign_proc(Purity, Attributes,
                    PredId, ProcId, VarSet, PVars, ArgTypes, PragmaImpl,
                    Context, PredOrFunc, PredName, Arity, Clauses0, Clauses,
                    !ModuleInfo, !IO),
                pred_info_set_clauses_info(Clauses, !PredInfo),
                pred_info_update_goal_type(pragmas, !PredInfo),
                map__det_update(Preds0, PredId, !.PredInfo, Preds),
                predicate_table_set_preds(Preds, PredTable1, PredTable),
                module_info_set_predicate_table(PredTable, !ModuleInfo),
                pragma_get_var_infos(PVars, ArgInfo),
                maybe_warn_pragma_singletons(PragmaImpl, PragmaForeignLanguage,
                    ArgInfo, Context, PredOrFunc - PredName/Arity,
                    !.ModuleInfo, !IO)
            ;
                module_info_incr_errors(!ModuleInfo),
                Pieces = [words("Error: `:- pragma foreign_proc' declaration"),
                    words("for undeclared mode of"),
                    simple_call_id(PredOrFunc - PredName/Arity), suffix(".")],
                write_error_pieces(Context, 0, Pieces, !IO)
            )
        )
    ).

%-----------------------------------------------------------------------------%

module_add_pragma_tabled(EvalMethod, PredName, Arity, MaybePredOrFunc,
        MaybeModes, Status, Context, !ModuleInfo, !IO) :-
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    EvalMethodStr = eval_method_to_one_string(EvalMethod),
    (
        MaybePredOrFunc = yes(PredOrFunc0),
        PredOrFunc = PredOrFunc0,

        % Lookup the pred declaration in the predicate table.
        % (If it is not there, print an error message and insert
        % a dummy declaration for the predicate.)
        (
            predicate_table_search_pf_sym_arity(PredicateTable0,
                is_fully_qualified, PredOrFunc,
                PredName, Arity, PredIds0)
        ->
            PredIds = PredIds0
        ;
            module_info_name(!.ModuleInfo, ModuleName),
            string__format("`:- pragma %s' declaration",
                [s(EvalMethodStr)], Message1),
            preds_add_implicit_report_error(ModuleName, PredOrFunc, PredName,
                Arity, Status, no, Context, user(PredName), Message1, PredId,
                !ModuleInfo, !IO),
            PredIds = [PredId]
        )
    ;
        MaybePredOrFunc = no,
        (
            predicate_table_search_sym_arity(PredicateTable0,
                is_fully_qualified, PredName, Arity, PredIds0)
        ->
            PredIds = PredIds0
        ;
            module_info_name(!.ModuleInfo, ModuleName),
            string__format("`:- pragma %s' declaration",
                [s(EvalMethodStr)], Message1),
            preds_add_implicit_report_error(ModuleName, predicate, PredName,
                Arity, Status, no, Context, user(PredName), Message1, PredId,
                !ModuleInfo, !IO),
            PredIds = [PredId]
        )
    ),
    list__foldl2(
        module_add_pragma_tabled_2(EvalMethod, PredName, Arity,
            MaybePredOrFunc, MaybeModes, Context),
        PredIds, !ModuleInfo, !IO).

:- pred module_add_pragma_tabled_2(eval_method::in, sym_name::in, int::in,
    maybe(pred_or_func)::in, maybe(list(mode))::in, prog_context::in,
    pred_id::in, module_info::in, module_info::out, io::di, io::uo) is det.

module_add_pragma_tabled_2(EvalMethod0, PredName, Arity0, MaybePredOrFunc,
        MaybeModes, Context, PredId, !ModuleInfo, !IO) :-
    ( EvalMethod0 = eval_minimal(_) ->
        globals__io_lookup_bool_option(use_minimal_model_own_stacks,
            OwnStacks, !IO),
        (
            OwnStacks = yes,
            EvalMethod = eval_minimal(own_stacks)
        ;
            OwnStacks = no,
            EvalMethod = eval_minimal(stack_copy)
        )
    ;
        EvalMethod = EvalMethod0
    ),

    % Lookup the pred_info for this pred.
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
    predicate_table_get_preds(PredicateTable, Preds),
    map__lookup(Preds, PredId, PredInfo0),
    (
        MaybePredOrFunc = yes(PredOrFunc0),
        PredOrFunc = PredOrFunc0
    ;
        MaybePredOrFunc = no,
        PredOrFunc = pred_info_is_pred_or_func(PredInfo0)
    ),
    adjust_func_arity(PredOrFunc, Arity0, Arity),

    EvalMethodStr = eval_method_to_one_string(EvalMethod),
    globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    (
        VeryVerbose = yes,
        io__write_string("% Processing `:- pragma ", !IO),
        io__write_string(EvalMethodStr, !IO),
        io__write_string("' for ", !IO),
        write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
        io__write_string("...\n", !IO)
    ;
        VeryVerbose = no
    ),

    % Issue a warning if this predicate/function has a pragma inline
    % declaration. Tabled procedures cannot be inlined.
    pred_info_get_markers(PredInfo0, Markers),
    globals.io_lookup_bool_option(warn_table_with_inline, WarnInline, !IO),
    ( check_marker(Markers, inline), WarnInline = yes ->
        TablePragmaStr = string.format("`:- pragma %s'", [s(EvalMethodStr)]),
        InlineWarning = [
            words("Warning: "), simple_call_id(PredOrFunc - PredName/Arity),
            words("has a"), fixed(TablePragmaStr),
            words("declaration but also has a"),
            fixed("`:- pragma inline'"), words("declaration."), nl,
            words("This inline pragma will be ignored"),
            words("since tabled predicates cannot be inlined."), nl,
            words("You can use the"), fixed("`--no-warn-table-with-inline'"),
            words("option to suppress this warning.")
        ],
        error_util.report_warning(Context, 0, InlineWarning, !IO)
    ;
        true
    ),
    ( pred_info_is_imported(PredInfo0) ->
        module_info_incr_errors(!ModuleInfo),
        Pieces1 = [words("Error: "),
            fixed("`:- pragma " ++ EvalMethodStr ++ "'"),
            words("declaration for imported"),
            simple_call_id(PredOrFunc - PredName/Arity), suffix(".")],
        write_error_pieces(Context, 0, Pieces1, !IO)
    ;
        % Do we have to make sure the tabled preds are stratified?
        ( eval_method_needs_stratification(EvalMethod) = yes ->
            module_info_stratified_preds(!.ModuleInfo, StratPredIds0),
            set__insert(StratPredIds0, PredId, StratPredIds),
            module_info_set_stratified_preds(StratPredIds, !ModuleInfo)
        ;
            true
        ),

        % Add the eval model to the proc_info for this procedure.
        pred_info_procedures(PredInfo0, ProcTable0),
        map__to_assoc_list(ProcTable0, ExistingProcs),
        SimpleCallId = PredOrFunc - PredName/Arity,
        (
            MaybeModes = yes(Modes),
            (
                get_procedure_matching_argmodes(ExistingProcs, Modes,
                    !.ModuleInfo, ProcId)
            ->
                map__lookup(ProcTable0, ProcId, ProcInfo0),
                set_eval_method(ProcId, ProcInfo0, Context, SimpleCallId,
                    EvalMethod, ProcTable0, ProcTable, !ModuleInfo, !IO),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
            ;
                module_info_incr_errors(!ModuleInfo),
                Pieces2 = [words("Error:"),
                    fixed("`:- pragma " ++ EvalMethodStr ++ "'"),
                    words("declaration for undeclared mode of"),
                    simple_call_id(SimpleCallId), suffix(".")],
                write_error_pieces(Context, 0, Pieces2, !IO)
            )
        ;
            MaybeModes = no,
            (
                ExistingProcs = [],
                module_info_incr_errors(!ModuleInfo),
                Pieces3 = [words("Error: "),
                    fixed("`:- pragma " ++ EvalMethodStr ++ "'"),
                    words("declaration for"), simple_call_id(SimpleCallId),
                    words("with no declared modes.")],
                write_error_pieces(Context, 0, Pieces3, !IO)
            ;
                ExistingProcs = [_ | _],
                set_eval_method_list(ExistingProcs, Context, SimpleCallId,
                    EvalMethod, ProcTable0, ProcTable, !ModuleInfo, !IO),
                pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
                module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
            )
        )
    ).

:- pred set_eval_method_list(assoc_list(proc_id, proc_info)::in,
    prog_context::in, simple_call_id::in, eval_method::in,
    proc_table::in, proc_table::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

set_eval_method_list([], _, _, _, !ProcTable, !ModuleInfo, !IO).
set_eval_method_list([ProcId - ProcInfo0 | Rest], Context, SimpleCallId,
        EvalMethod, !ProcTable, !ModuleInfo, !IO) :-
    set_eval_method(ProcId, ProcInfo0, Context, SimpleCallId,
        EvalMethod, !ProcTable, !ModuleInfo, !IO),
    set_eval_method_list(Rest, Context, SimpleCallId,
        EvalMethod, !ProcTable, !ModuleInfo, !IO).

:- pred set_eval_method(proc_id::in, proc_info::in, prog_context::in,
    simple_call_id::in, eval_method::in, proc_table::in, proc_table::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

set_eval_method(ProcId, ProcInfo0, Context, SimpleCallId, EvalMethod,
        !ProcTable, !ModuleInfo, !IO) :-
    proc_info_eval_method(ProcInfo0, OldEvalMethod),
    % NOTE: We don't bother detecting multiple tabling pragmas
    % of the same type here.
    (
        OldEvalMethod \= eval_normal,
        OldEvalMethod \= EvalMethod
    ->
        % If there are conflicting tabling pragmas then emit an error message
        % and do not bother changing the evaluation method.
        OldEvalMethodStr = eval_method_to_one_string(OldEvalMethod),
        EvalMethodStr = eval_method_to_one_string(EvalMethod),
        Pieces = [words("Error:"), simple_call_id(SimpleCallId),
            words("has both"), fixed(OldEvalMethodStr), words("and"),
            fixed(EvalMethodStr), words("pragmas specified."),
            words("Only one kind of tabling pragma may be applied to it.")
        ],
        module_info_incr_errors(!ModuleInfo),
        write_error_pieces(Context, 0, Pieces, !IO)
    ;
        proc_info_maybe_declared_argmodes(ProcInfo0, MaybeDeclaredArgModes),
        (
            MaybeDeclaredArgModes = no,
            EvalMethodStr = eval_method_to_one_string(EvalMethod),
            Pieces = [words("Error:"),
                fixed("`pragma" ++ EvalMethodStr ++ "'"),
                words("declaration for"), simple_call_id(SimpleCallId),
                suffix(","), words("which has no declared modes.")
            ],
            module_info_incr_errors(!ModuleInfo),
            write_error_pieces(Context, 0, Pieces, !IO)
        ;
            MaybeDeclaredArgModes = yes(DeclaredArgModes),
            ( EvalMethod = eval_memo(specified(MaybeArgMethods)) ->
                check_pred_args_against_tabling_methods(DeclaredArgModes,
                    MaybeArgMethods, !.ModuleInfo, 1, MaybeError)
            ;
                check_pred_args_against_tabling(DeclaredArgModes, !.ModuleInfo,
                    1, MaybeError)
            ),
            (
                MaybeError = no,
                proc_info_set_eval_method(EvalMethod, ProcInfo0, ProcInfo),
                svmap__det_update(ProcId, ProcInfo, !ProcTable)
            ;
                MaybeError = yes(ArgMsg - ErrorMsg),
                EvalMethodStr = eval_method_to_one_string(EvalMethod),
                Pieces = [words("Error in"),
                    fixed("`pragma " ++ EvalMethodStr ++ "'"),
                    words("declaration for"), simple_call_id(SimpleCallId),
                    suffix(":"), nl, fixed(ArgMsg), words(ErrorMsg)
                ],
                module_info_incr_errors(!ModuleInfo),
                write_error_pieces(Context, 0, Pieces, !IO)
            )
        )
    ).

:- pred check_pred_args_against_tabling_methods(list(mode)::in,
    list(maybe(arg_tabling_method))::in, module_info::in, int::in,
    maybe(pair(string))::out) is det.

check_pred_args_against_tabling_methods([], [], _, _, no).
check_pred_args_against_tabling_methods([], [_ | _], _, _, MaybeError) :-
    MaybeError = yes("too many argument tabling methods specified." - "").
check_pred_args_against_tabling_methods([_ | _], [], _, _, MaybeError) :-
    MaybeError = yes("not enough argument tabling methods specified." - "").
check_pred_args_against_tabling_methods([Mode | Modes],
        [MaybeArgMethod | MaybeArgMethods], ModuleInfo, ArgNum, MaybeError) :-
    % XXX We should check not just the boundedness of the argument, but also
    % whether it has any uniqueness annotation: tabling destroys uniqueness.
    ( mode_is_fully_input(ModuleInfo, Mode) ->
        (
            MaybeArgMethod = yes(_),
            check_pred_args_against_tabling_methods(Modes, MaybeArgMethods,
                ModuleInfo, ArgNum + 1, MaybeError)
        ;
            MaybeArgMethod = no,
            MaybeError = yes(("argument " ++ int_to_string(ArgNum) ++ ":") -
                ("argument tabling method `" ++
                maybe_arg_tabling_method_to_string(MaybeArgMethod) ++
                "' is not compatible with input modes."))
        )
    ; mode_is_fully_output(ModuleInfo, Mode) ->
        (
            MaybeArgMethod = yes(_),
            MaybeError = yes(("argument " ++ int_to_string(ArgNum) ++ ":") -
                ("argument tabling method `" ++
                maybe_arg_tabling_method_to_string(MaybeArgMethod) ++
                "' is not compatible with output modes."))
        ;
            MaybeArgMethod = no,
            check_pred_args_against_tabling_methods(Modes, MaybeArgMethods,
                ModuleInfo, ArgNum + 1, MaybeError)
        )
    ;
        MaybeError = yes(("argument " ++ int_to_string(ArgNum) ++ ":") -
            "is neither input or output.")
    ).

:- pred check_pred_args_against_tabling(list(mode)::in, module_info::in,
    int::in, maybe(pair(string))::out) is det.

check_pred_args_against_tabling([], _, _, no).
check_pred_args_against_tabling([Mode | Modes], ModuleInfo, ArgNum,
        MaybeError) :-
    ( mode_is_fully_input(ModuleInfo, Mode) ->
        check_pred_args_against_tabling(Modes, ModuleInfo, ArgNum + 1,
            MaybeError)
    ; mode_is_fully_output(ModuleInfo, Mode) ->
        check_pred_args_against_tabling(Modes, ModuleInfo, ArgNum + 1,
            MaybeError)
    ;
        MaybeError = yes(("argument " ++ int_to_string(ArgNum)) -
            "is neither input or output.")
    ).

    % Extract the modes from the list of pragma_vars.
    %
:- pred pragma_get_modes(list(pragma_var)::in, list(mode)::out) is det.

pragma_get_modes([], []).
pragma_get_modes([PragmaVar | Vars], [Mode | Modes]) :-
    PragmaVar = pragma_var(_Var, _Name, Mode),
    pragma_get_modes(Vars, Modes).

%-----------------------------------------------------------------------------%

    % Extract the vars from the list of pragma_vars.
    %
:- pred pragma_get_vars(list(pragma_var)::in, list(prog_var)::out) is det.

pragma_get_vars([], []).
pragma_get_vars([PragmaVar | PragmaVars], [Var | Vars]) :-
    PragmaVar = pragma_var(Var, _Name, _Mode),
    pragma_get_vars(PragmaVars, Vars).

%---------------------------------------------------------------------------%

    % Extract the names from the list of pragma_vars.
    %
:- pred pragma_get_var_infos(list(pragma_var)::in,
    list(maybe(pair(string, mode)))::out) is det.

pragma_get_var_infos([], []).
pragma_get_var_infos([PragmaVar | PragmaVars], [yes(Name - Mode) | Info]) :-
    PragmaVar = pragma_var(_Var, Name, Mode),
    pragma_get_var_infos(PragmaVars, Info).

module_add_pragma_fact_table(Pred, Arity, FileName, Status, Context,
        !ModuleInfo, !QualInfo, !IO) :-
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
    (
        predicate_table_search_sym_arity(PredicateTable, is_fully_qualified,
            Pred, Arity, PredIDs0),
        PredIDs0 = [PredID | PredIDs1]
    ->
        (
            PredIDs1 = [],      % only one predicate found
            module_info_pred_info(!.ModuleInfo, PredID, PredInfo0),

                % Compile the fact table into a separate .o file.
            fact_table_compile_facts(Pred, Arity, FileName,
                PredInfo0, PredInfo, Context, !.ModuleInfo,
                C_HeaderCode, PrimaryProcID, !IO),

            module_info_set_pred_info(PredID, PredInfo, !ModuleInfo),
            pred_info_procedures(PredInfo, ProcTable),
            pred_info_arg_types(PredInfo, ArgTypes),
            ProcIDs = pred_info_procids(PredInfo),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),
            adjust_func_arity(PredOrFunc, Arity, NumArgs),

                % Create foreign_decls to declare extern variables.
            module_add_foreign_decl(c, foreign_decl_is_local,
                C_HeaderCode, Context, !ModuleInfo),

            module_add_fact_table_file(FileName, !ModuleInfo),

            io__get_exit_status(ExitStatus, !IO),
            ( ExitStatus = 1 ->
                true
            ;
                % Create foreign_procs to access the table in each mode.
                module_add_fact_table_procedures(ProcIDs,
                    PrimaryProcID, ProcTable, Pred,
                    PredOrFunc, NumArgs, ArgTypes, Status,
                    Context, !ModuleInfo, !QualInfo, !IO)
            )
        ;
            PredIDs1 = [_ | _],     % >1 predicate found
            io__set_exit_status(1, !IO),
            Pieces = [words("In pragma fact_table for"),
                sym_name_and_arity(Pred/Arity), suffix(":"), nl,
                words("error: ambiguous predicate/function name.")],
            write_error_pieces(Context, 0, Pieces, !IO)
        )
    ;
        undefined_pred_or_func_error(Pred, Arity, Context,
            "`:- pragma fact_table' declaration", !IO)
    ).

    % Add a `pragma c_code' for each mode of the fact table lookup to the
    % HLDS.
    % `pragma fact_table's are represented in the HLDS by a
    % `pragma c_code' for each mode of the predicate.
    %
:- pred module_add_fact_table_procedures(list(proc_id)::in, proc_id::in,
    proc_table::in, sym_name::in, pred_or_func::in, arity::in,
    list(type)::in, import_status::in, prog_context::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    io::di, io::uo) is det.

module_add_fact_table_procedures([],_,_,_,_,_,_,_,_, !ModuleInfo, !QualInfo,
        !IO).
module_add_fact_table_procedures([ProcID | ProcIDs], PrimaryProcID, ProcTable,
        SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
        !ModuleInfo, !QualInfo, !IO) :-
    module_add_fact_table_proc(ProcID, PrimaryProcID, ProcTable, SymName,
        PredOrFunc, Arity, ArgTypes, Status, Context,
        !ModuleInfo, !QualInfo, !IO),
    module_add_fact_table_procedures(ProcIDs, PrimaryProcID, ProcTable,
        SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
        !ModuleInfo, !QualInfo, !IO).

:- pred module_add_fact_table_proc(proc_id::in, proc_id::in, proc_table::in,
    sym_name::in, pred_or_func::in, arity::in, list(type)::in,
    import_status::in, prog_context::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out, io::di, io::uo) is det.

module_add_fact_table_proc(ProcID, PrimaryProcID, ProcTable, SymName,
        PredOrFunc, Arity, ArgTypes, Status, Context, !ModuleInfo, !QualInfo,
        !IO) :-
    map__lookup(ProcTable, ProcID, ProcInfo),
    varset__init(VarSet0),
    varset__new_vars(VarSet0, Arity, Vars, VarSet),
    proc_info_argmodes(ProcInfo, Modes),
    fact_table_pragma_vars(Vars, Modes, VarSet, PragmaVars),
    fact_table_generate_c_code(SymName, PragmaVars, ProcID, PrimaryProcID,
        ProcInfo, ArgTypes, !.ModuleInfo, C_ProcCode, C_ExtraCode, !IO),

    % XXX this should be modified to use nondet pragma c_code.
    Attrs0 = default_attributes(c),
    set_may_call_mercury(will_not_call_mercury, Attrs0, Attrs1),
    set_thread_safe(thread_safe, Attrs1, Attrs2),
        % fact tables procedures should be considered pure
    set_purity(pure, Attrs2, Attrs),
    module_add_pragma_foreign_proc(Attrs, SymName, PredOrFunc, PragmaVars,
        VarSet, ordinary(C_ProcCode, no), Status, Context,
        !ModuleInfo, !QualInfo, !IO),
    ( C_ExtraCode = "" ->
        true
    ;
        module_add_foreign_body_code(c, C_ExtraCode, Context, !ModuleInfo)
    ),
    %
    % The C code for fact tables includes C labels;
    % we cannot inline this code, because if we try,
    % the result may be duplicate labels in the generated code.
    % So we must disable inlining for fact_table procedures.
    %
    add_pred_marker("fact_table", SymName, Arity, Status, Context,
        no_inline, [], !ModuleInfo, !IO).

    % Create a list(pragma_var) that looks like the ones that are created
    % for foreign_proc in prog_io.m.
    % This is required by module_add_pragma_c_code to add the C code for
    % the procedure to the HLDS.
    %
:- pred fact_table_pragma_vars(list(prog_var)::in, list(mode)::in,
    prog_varset::in, list(pragma_var)::out) is det.

fact_table_pragma_vars(Vars0, Modes0, VarSet, PragmaVars0) :-
    (
        Vars0 = [Var | VarsTail],
        Modes0 = [Mode | ModesTail]
    ->
        varset__lookup_name(VarSet, Var, Name),
        PragmaVar = pragma_var(Var, Name, Mode),
        fact_table_pragma_vars(VarsTail, ModesTail, VarSet, PragmaVarsTail),
        PragmaVars0 = [PragmaVar | PragmaVarsTail]
    ;
        PragmaVars0 = []
    ).

    % Add the pragma_foreign_proc goal to the clauses_info for this procedure.
    % To do so, we must also insert unifications between the variables in the
    % pragma foreign_proc declaration and the head vars of the pred. Also
    % return the hlds_goal.
    %
:- pred clauses_info_add_pragma_foreign_proc(purity::in,
    pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
    prog_varset::in, list(pragma_var)::in, list(type)::in,
    pragma_foreign_code_impl::in, prog_context::in, pred_or_func::in,
    sym_name::in, arity::in, clauses_info::in, clauses_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

clauses_info_add_pragma_foreign_proc(Purity, Attributes0, PredId, ProcId,
        PVarSet, PVars, OrigArgTypes, PragmaImpl0, Context, PredOrFunc,
        PredName, Arity, !ClausesInfo, !ModuleInfo, !IO) :-

    !.ClausesInfo = clauses_info(VarSet0, ExplicitVarTypes, TVarNameMap,
        InferredVarTypes, HeadVars, ClauseRep, RttiVarMaps,
        _HasForeignClauses),
    get_clause_list(ClauseRep, ClauseList),

        % Find all the existing clauses for this mode, and
        % extract their implementation language and clause number
        % (that is, their index in the list).
    globals__io_get_globals(Globals, !IO),
    globals__io_get_target(Target, !IO),
    NewLang = foreign_language(Attributes0),
    list__foldl2(decide_action(Globals, Target, NewLang, ProcId), ClauseList,
        add, FinalAction, 1, _),

    globals__io_get_backend_foreign_languages(BackendForeignLanguages, !IO),
    pragma_get_vars(PVars, Args0),
    pragma_get_var_infos(PVars, ArgInfo),

    %
    % If the foreign language not one of the backend languages, we will
    % have to generate an interface to it in a backend language.
    %
    foreign__extrude_pragma_implementation(BackendForeignLanguages,
        PVars, PredName, PredOrFunc, Context, !ModuleInfo,
        Attributes0, Attributes, PragmaImpl0, PragmaImpl),

    %
    % Check for arguments occurring multiple times.
    %
    bag__init(ArgBag0),
    bag__insert_list(ArgBag0, Args0, ArgBag),
    bag__to_assoc_list(ArgBag, ArgBagAL0),
    list__filter(
        (pred(Arg::in) is semidet :-
            Arg = _ - Occurrences,
            Occurrences > 1
        ), ArgBagAL0, ArgBagAL),
    assoc_list__keys(ArgBagAL, MultipleArgs),

    (
        MultipleArgs = [_ | _],
        io__set_exit_status(1, !IO),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        Pieces1 = [words("In `:- pragma foreign_proc' declaration for"),
            simple_call_id(PredOrFunc - PredName/OrigArity), suffix(":"), nl],
        (
            MultipleArgs = [MultipleArg],
            Pieces2 = [words("error: variable `" ++
                mercury_var_to_string(MultipleArg, PVarSet, no) ++
                "' occurs multiple times in the argument list.")]
        ;
            MultipleArgs = [_, _ | _],
            Pieces2 = [words("error: variables `" ++
                mercury_vars_to_string(MultipleArgs, PVarSet, no) ++
                "' occur multiple times in the argument list.")]
        ),
        write_error_pieces(Context, 0, Pieces1 ++ Pieces2, !IO)
    ;
        MultipleArgs = [],
        % Build the foreign_proc.
        goal_info_init(GoalInfo0),
        goal_info_set_context(Context, GoalInfo0, GoalInfo1),
        % Put the purity in the goal_info in case this foreign code is inlined.
        add_goal_info_purity_feature(Purity, GoalInfo1, GoalInfo),
        make_foreign_args(HeadVars, ArgInfo, OrigArgTypes, ForeignArgs),
        HldsGoal0 = foreign_proc(Attributes, PredId, ProcId, ForeignArgs, [],
            PragmaImpl) - GoalInfo,
        map__init(EmptyVarTypes),
        implicitly_quantify_clause_body(HeadVars, _Warnings,
            HldsGoal0, HldsGoal, VarSet0, VarSet, EmptyVarTypes, _),
        NewClause = clause([ProcId], HldsGoal, foreign_language(NewLang),
            Context),
        (
            FinalAction = ignore,
            NewClauseList = ClauseList
        ;
            FinalAction = add,
            NewClauseList = [NewClause | ClauseList]
        ;
            FinalAction = replace(N),
            list__replace_nth_det(ClauseList, N, NewClause, NewClauseList)
        ;
            FinalAction = split_add(N, Clause),
            list__replace_nth_det(ClauseList, N, Clause, NewClauseListTail),
            NewClauseList = [NewClause | NewClauseListTail]
        ),
        HasForeignClauses = yes,
        set_clause_list(NewClauseList, NewClauseRep),
        !:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes, TVarNameMap,
            InferredVarTypes, HeadVars, NewClauseRep, RttiVarMaps,
            HasForeignClauses)
    ).

:- func is_applicable_for_current_backend(backend,
    list(pragma_foreign_proc_extra_attribute)) = bool.

is_applicable_for_current_backend(_CurrentBackend, []) = yes.
is_applicable_for_current_backend(CurrentBackend, [Attr | Attrs]) = Result :-
    (
        Attr = max_stack_size(_),
        Result = is_applicable_for_current_backend(CurrentBackend, Attrs)
    ;
        Attr = backend(Backend),
        ( Backend = CurrentBackend ->
            Result = is_applicable_for_current_backend(CurrentBackend, Attrs)
        ;
            Result = no
        )
    ).

lookup_current_backend(CurrentBackend, !IO) :-
    globals__io_lookup_bool_option(highlevel_code, HighLevel, !IO),
    (
        HighLevel = yes,
        CurrentBackend = high_level_backend
    ;
        HighLevel= no,
        CurrentBackend = low_level_backend
    ).

    % As we traverse the clauses, at each one decide which action to perform.
    %
    % If there are no clauses, we will simply add this clause.
    %
    % If there are matching foreign_proc clauses for this proc_id,
    % we will either replace them or ignore the new clause
    % (depending on the preference of the two foreign languages).
    %
    % If there is a matching Mercury clause for this proc_id, we will either
    %   - replace it if there is only one matching mode in its proc_id list.
    %   - remove the matching proc_id from its proc_id list, and add this
    %     clause as a new clause for this mode.

:- type foreign_proc_action
    --->    ignore
    ;       add
    ;       split_add(int, clause)
    ;       replace(int).

:- pred decide_action(globals::in, compilation_target::in,
    foreign_language::in, proc_id::in, clause::in,
    foreign_proc_action::in, foreign_proc_action::out,
    int::in, int::out) is det.

decide_action(Globals, Target, NewLang, ProcId, Clause, !Action, !ClauseNum) :-
    Clause = clause(ProcIds, Body, ClauseLang, Context),
    (
        ClauseLang = mercury,
        ( ProcIds = [ProcId] ->
            !:Action = replace(!.ClauseNum)
        ; list__delete_first(ProcIds, ProcId, MercuryProcIds) ->
            NewMercuryClause = clause(MercuryProcIds, Body, ClauseLang,
                Context),
            !:Action = split_add(!.ClauseNum, NewMercuryClause)
        ;
            true
        )
    ;
        ClauseLang = foreign_language(OldLang),
        ( list__member(ProcId, ProcIds) ->
            (
                yes = prefer_foreign_language(Globals, Target,
                    OldLang, NewLang)
            ->
                % This language is preferred to the old
                % language, so we should replace it
                !:Action = replace(!.ClauseNum)
            ;
                % Just ignore it.
                !:Action = ignore
            )
        ;
            true
        )
    ),
    !:ClauseNum = !.ClauseNum + 1.

    % Find the procedure with argmodes which match the ones we want.
    %
:- pred get_procedure_matching_argmodes(assoc_list(proc_id, proc_info)::in,
    list(mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_argmodes(Procs, Modes0, ModuleInfo, ProcId) :-
    list__map(constrain_inst_vars_in_mode, Modes0, Modes),
    get_procedure_matching_argmodes_2(Procs, Modes, ModuleInfo, ProcId).

:- pred get_procedure_matching_argmodes_2(assoc_list(proc_id, proc_info)::in,
    list(mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_argmodes_2([P | Procs], Modes, ModuleInfo, OurProcId) :-
    P = ProcId - ProcInfo,
    proc_info_argmodes(ProcInfo, ArgModes),
    ( mode_list_matches(Modes, ArgModes, ModuleInfo) ->
        OurProcId = ProcId
    ;
        get_procedure_matching_argmodes_2(Procs, Modes, ModuleInfo, OurProcId)
    ).

get_procedure_matching_declmodes(Procs, Modes0, ModuleInfo, ProcId) :-
    list__map(constrain_inst_vars_in_mode, Modes0, Modes),
    get_procedure_matching_declmodes_2(Procs, Modes, ModuleInfo, ProcId).

:- pred get_procedure_matching_declmodes_2(assoc_list(proc_id, proc_info)::in,
    list(mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_declmodes_2([P | Procs], Modes, ModuleInfo,
        OurProcId) :-
    P = ProcId - ProcInfo,
    proc_info_declared_argmodes(ProcInfo, ArgModes),
    ( mode_list_matches(Modes, ArgModes, ModuleInfo) ->
        OurProcId = ProcId
    ;
        get_procedure_matching_declmodes_2(Procs, Modes, ModuleInfo, OurProcId)
    ).

:- pred mode_list_matches(list(mode)::in, list(mode)::in, module_info::in)
    is semidet.

mode_list_matches([], [], _).
mode_list_matches([Mode1 | Modes1], [Mode2 | Modes2], ModuleInfo) :-
    % Use mode_get_insts_semidet instead of mode_get_insts to avoid
    % aborting if there are undefined modes.
    mode_get_insts_semidet(ModuleInfo, Mode1, Inst1, Inst2),
    mode_get_insts_semidet(ModuleInfo, Mode2, Inst1, Inst2),
    mode_list_matches(Modes1, Modes2, ModuleInfo).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file =  "add_pragma.m".

%----------------------------------------------------------------------------%
:- end_module add_pragma.
%----------------------------------------------------------------------------%
