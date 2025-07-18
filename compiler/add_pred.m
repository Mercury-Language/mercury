%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: add_pred.m.
%
% This submodule of make_hlds handles the type and mode declarations
% for predicates.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pred.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module maybe.
:- import_module pair.

%---------------------------------------------------------------------------%

    % Add a pred or predmode declaration for a predicate.
    %
    % We return MaybePredMaybeProcId = yes(PredId - MaybeProcId) if we
    % successfully added the predicate to the HLDS. The MaybeProcId part
    % will be yes(ProcId) if the declaration is a predmode declaration,
    % and we successfully added its implied mode declaration to the HLDS.
    %
:- pred module_add_pred_decl(item_mercury_status::in, pred_status::in,
    need_qualifier::in, item_pred_decl_info::in,
    maybe(pair(pred_id, maybe(proc_id)))::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Is the mode declaration we are adding to the HLDS derived from
    % a combined predmode declaration?
:- type part_of_predmode
    --->    not_part_of_predmode
    ;       part_of_predmode.

    % Add a mode declaration for a predicate.
    %
:- pred module_add_mode_decl(part_of_predmode::in, maybe_class_method::in,
    item_mercury_status::in, pred_status::in, item_mode_decl_info::in,
    pred_proc_id::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % report_mode_decl_after_predmode(PFNameArity, Context):
    %
    % Return a diagnostic reporting that PredPFNameArity has a
    % mode declaration at Context which is disallowed by the fact that
    % its predicate or function declaration was a predmode declaration.
    %
    % We export this to add_class.m. Class definitions consist of pred, func
    % and mode declarations, and we want diagnostics for mode declarations
    % that follow pred or func declarations with embedded mode information
    % to be the same inside class definitions as they are outside.
    %
:- func report_mode_decl_after_predmode(pred_pf_name_arity, prog_context)
    = error_spec.

    % Whenever there is a clause or mode declaration for an undeclared
    % predicate, we add an implicit declaration
    %   :- pred p(T1, T2, ..., Tn).
    % for that predicate; the real types will be inferred by type inference.
    %
:- pred add_implicit_pred_decl_report_error(pred_or_func::in,
    module_name::in, string::in, pred_form_arity::in, pred_status::in,
    maybe_class_method::in, prog_context::in, pred_origin::in,
    list(format_piece)::in, pred_id::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_implicit_pred_decl(pred_or_func::in, module_name::in, string::in,
    pred_form_arity::in, pred_status::in, prog_context::in, pred_origin::in,
    goal_type::in, clauses_info::in, pred_id::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_args.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_hlds.check_field_access_functions.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module hlds.var_table_hlds.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_parse_tree.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.

:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

module_add_pred_decl(ItemMercuryStatus, PredStatus, NeedQual, ItemPredDecl,
        MaybePredMaybeProcId, !ModuleInfo, !Specs) :-
    ItemPredDecl = item_pred_decl_info(PredSymName, PredOrFunc,
        ArgTypesAndMaybeModes, WithType, WithInst, MaybeDetism,
        Origin, TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
        Context, SeqNum),
    (
        PredSymName = unqualified(_PredName),
        unexpected($pred, "unqualified PredSymName")
    ;
        PredSymName = qualified(PredModuleName, PredName)
    ),
    % Any WithType and WithInst annotations should have been expanded
    % and the type and/or inst put into TypesAndModes by equiv_type.m.
    expect(unify(WithType, no), $pred, "WithType != no"),
    expect(unify(WithInst, no), $pred, "WithInst != no"),

    ( if PredName = "" then
        % The term parser, when given input strings such as "A(B, C)",
        % in which a variable acts as a function symbol, returns a term
        % such as functor("", [variable(A), variable(B), variable(C)]).
        % The only way PredName could be "" is if this happened in the
        % predicate or function declaration.
        PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        Pieces = [words("Error: you cannot declare")] ++
            color_as_subject([words("a"), words(PredOrFuncStr)]) ++
            color_as_incorrect([words("whose name is a variable.")]) ++ [nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs],
        MaybePredMaybeProcId = no
    else
        compute_arg_types_maybe_modes(PredOrFunc, ArgTypesAndMaybeModes,
            MaybeDetism, UserArity, ArgTypes, MaybeArgModes, PredmodeDecl),
        record_pred_origin(PredOrFunc, PredSymName, UserArity, Origin,
            Context, PredOrigin, Markers),
        add_new_pred(PredOrigin, Context, SeqNum, PredStatus, NeedQual,
            PredOrFunc, PredModuleName, PredName, TypeVarSet, ExistQVars,
            ArgTypes, Constraints, PredmodeDecl, Purity, Markers,
            MaybeNewPredId, !ModuleInfo, !Specs),
        (
            MaybeArgModes = yes(ArgModes),
            (
                MaybeNewPredId = no,
                % Do not try to add the mode declaration part of the predmode
                % declaration to the HLDS if adding the pred declaration part
                % has failed.
                MaybePredMaybeProcId = no
            ;
                MaybeNewPredId = yes(NewPredId),
                ( if marker_is_present(Markers, marker_class_method) then
                    IsClassMethod = is_a_class_method
                else
                    IsClassMethod = is_not_a_class_method
                ),
                ItemModeDecl = item_mode_decl_info(PredSymName,
                    yes(PredOrFunc), ArgModes, WithInst, MaybeDetism,
                    InstVarSet, Context, SeqNum),
                module_add_mode_decl(part_of_predmode, IsClassMethod,
                    ItemMercuryStatus, PredStatus, ItemModeDecl,
                    ModePredProcId, !ModuleInfo, !Specs),
                ModePredProcId = proc(ModePredId, ModeProcId),
                expect(unify(NewPredId, ModePredId), $pred,
                    "NewPredId != ModePredId"),
                MaybePredMaybeProcId = yes(NewPredId - yes(ModeProcId))
            )
        ;
            MaybeArgModes = no,
            (
                MaybeNewPredId = no,
                MaybePredMaybeProcId = no
            ;
                MaybeNewPredId = yes(NewPredId),
                MaybePredMaybeProcId = yes(NewPredId - no)
            ),
            % There is no valid mode declaration part we can add to the HLDS.
            % Check for an invalid mode declaration part anyway.
            check_for_modeless_predmode_decl(PredStatus, PredOrFunc,
                PredSymName, ArgTypes, MaybeDetism, Context, !Specs)
        )
    ).

:- pred compute_arg_types_maybe_modes(pred_or_func::in,
    types_and_maybe_modes::in, maybe(determinism)::in, user_arity::out,
    list(mer_type)::out, maybe(list(mer_mode))::out, maybe_predmode_decl::out)
    is det.

compute_arg_types_maybe_modes(PredOrFunc, ArgTypesAndMaybeModes, MaybeDetism,
        UserArity, ArgTypes, MaybeArgModes, PredmodeDecl) :-
    PredFormArity = types_and_maybe_modes_arity(ArgTypesAndMaybeModes),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    (
        PredOrFunc = pf_predicate,
        (
            ArgTypesAndMaybeModes = no_types_arity_zero,
            ArgTypes = [],
            (
                % If a predicate declaration has no arguments and no
                % determinism, then it has none of the components of
                % a mode declaration.
                MaybeDetism = no,
                MaybeArgModes = no
            ;
                % If a predicate declaration has no arguments but does
                % declare a determinism, then it has all of the components
                % of a mode declaration for an arity-zero predicate.
                %
                % parse_item.m is supposed to set ArgTypesAndMaybeModes
                % to types_and_modes([]) instead of types_only([])
                % in these cases, but just in case we get a pred decl
                % that is constructed elsewhere ...
                MaybeDetism = yes(_),
                MaybeArgModes = yes([])
            )
        ;
            ArgTypesAndMaybeModes = types_only(ArgTypes),
            MaybeArgModes = no
        ;
            ArgTypesAndMaybeModes = types_and_modes(ArgTypesAndModes),
            split_types_and_modes(ArgTypesAndModes, ArgTypes, ArgModes0),
            MaybeArgModes = yes(ArgModes0)
        )
    ;
        PredOrFunc = pf_function,
        (
            ArgTypesAndMaybeModes = no_types_arity_zero,
            % There should be at least one type, the type of the
            % return value.
            unexpected($pred, "no_types_arity_zero")
        ;
            ArgTypesAndMaybeModes = types_only(ArgTypes),
            % A function declaration that contains no argument modes
            % but does specify a determinism is implicitly specifying
            % the default mode.
            (
                MaybeDetism = yes(_),
                UserArity = user_arity(UserArityInt),
                in_mode(InMode),
                list.duplicate(UserArityInt, InMode, InModes),
                out_mode(OutMode),
                MaybeArgModes = yes(InModes ++ [OutMode])
            ;
                MaybeDetism = no,
                MaybeArgModes = no
            )
        ;
            ArgTypesAndMaybeModes = types_and_modes(ArgTypesAndModes),
            split_types_and_modes(ArgTypesAndModes, ArgTypes, ArgModes0),
            MaybeArgModes = yes(ArgModes0)
        )
    ),
    ( MaybeArgModes = no,     PredmodeDecl = no_predmode_decl
    ; MaybeArgModes = yes(_), PredmodeDecl = predmode_decl
    ).

:- pred record_pred_origin(pred_or_func::in, sym_name::in, user_arity::in,
    item_maybe_attrs::in, prog_context::in,
    pred_origin::out, pred_markers::out) is det.

record_pred_origin(PredOrFunc, PredSymName, UserArity, Origin, Context,
        PredOrigin, Markers) :-
    % If this predicate was added as a result of the mutable
    % transformation, then mark this predicate as a mutable access pred.
    % We do this so that we can tell optimizations, like inlining,
    % to treat it specially.
    init_markers(Markers0),
    (
        Origin = item_origin_user,
        PredOrigin = origin_user(
            user_made_pred(PredOrFunc, PredSymName, UserArity)),
        Markers = Markers0
    ;
        Origin = item_origin_compiler(CompilerAttrs),
        CompilerAttrs = item_compiler_attributes(CompilerOrigin),
        (
            CompilerOrigin = compiler_origin_class_method(ClassId, MethodId),
            PredOrigin =
                origin_user(user_made_class_method(ClassId, MethodId)),
            add_marker(marker_class_method, Markers0, Markers)
        ;
            CompilerOrigin = compiler_origin_solver_repn(TypeCtor,
                SolverPredKind),
            PredOrigin = origin_compiler(
                made_for_solver_repn(TypeCtor, SolverPredKind)),
            Markers = Markers0
        ;
            CompilerOrigin = compiler_origin_tabling(PFSymNameArity,
                TablingPredKind),
            PredOrigin = origin_compiler(made_for_tabling(PFSymNameArity,
                TablingPredKind)),
            Markers = Markers0
        ;
            CompilerOrigin = compiler_origin_mutable(ModuleName, MutableName,
                MutablePredKind),
            PredOrigin = origin_compiler(
                made_for_mutable(ModuleName, MutableName, MutablePredKind)),
            add_marker(marker_mutable_access_pred, Markers0, Markers)
        ;
            CompilerOrigin = compiler_origin_initialise,
            Context = context(File, Line),
            PredOrigin = origin_compiler(made_for_initialise(File, Line)),
            Markers = Markers0
        ;
            CompilerOrigin = compiler_origin_finalise,
            Context = context(File, Line),
            PredOrigin = origin_compiler(made_for_finalise(File, Line)),
            Markers = Markers0
        )
    ).

:- pred check_for_modeless_predmode_decl(pred_status::in, pred_or_func::in,
    sym_name::in, list(mer_type)::in, maybe(determinism)::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_modeless_predmode_decl(PredStatus, PredOrFunc,
        PredSymName, ArgTypes, MaybeDetism, Context, !Specs) :-
    ( if
        MaybeDetism = yes(_),
        % Functions are allowed to declare a determinism without declaring
        % argument modes; the determinism will apply to the default mode.
        % Predicates do not have a default mode, so they may NOT declare
        % a determinism without declaring the argument modes, UNLESS
        % there are no arguments whose mode needs to be declared.
        PredOrFunc = pf_predicate,
        ArgTypes = [_ | _],
        % Do not generate an error message unless the predicate
        % is defined in this module.
        pred_status_defined_in_this_module(PredStatus) = yes
    then
        list.length(ArgTypes, PredFormArity),
        SNA = sym_name_arity(PredSymName, PredFormArity),
        Pieces = [words("Error: predicate")] ++
            color_as_subject([unqual_sym_name_arity(SNA)]) ++
            [words("declares a determinism")] ++
            color_as_incorrect([words("without declaring"),
                words("the modes of its arguments.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- pred add_new_pred(pred_origin::in, prog_context::in, item_seq_num::in,
    pred_status::in, need_qualifier::in, pred_or_func::in,
    module_name::in, string::in, tvarset::in, existq_tvars::in,
    list(mer_type)::in, univ_exist_constraints::in, maybe_predmode_decl::in,
    purity::in, pred_markers::in, maybe(pred_id)::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_new_pred(PredOrigin, Context, SeqNum, PredStatus0, NeedQual, PredOrFunc,
        PredModuleName, PredName, TVarSet, ExistQVars, Types, Constraints,
        PredmodeDecl, Purity, Markers0, MaybeNewPredId, !ModuleInfo, !Specs) :-
    % NB. Predicates are also added in lambda.m, which converts
    % lambda expressions into separate predicates, so any changes may need
    % to be reflected there too.

    % Only preds with opt_imported clauses are tagged as opt_imported, so that
    % the compiler doesn't look for clauses for other preds read in from
    % optimization interfaces.
    ( if PredStatus0 = pred_status(status_opt_imported) then
        PredStatus = pred_status(status_imported(import_locn_interface))
    else
        PredStatus = PredStatus0
    ),
    PredFormArity = arg_list_arity(Types),
    PredSymName = qualified(PredModuleName, PredName),
    compute_maybe_cur_user_decl(PredStatus, PredmodeDecl, SeqNum,
        MaybeCurUserDecl),
    GoalType = goal_not_for_promise(np_goal_type_none),
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    % XXX CIT_TYPES should be cit_types(Types),
    clauses_info_init(PredOrFunc, cit_no_types(PredFormArity),
        init_clause_item_numbers_user, ClausesInfo),
    map.init(Proofs),
    map.init(ConstraintMap),
    purity_to_markers(Purity, PurityMarkers),
    add_markers(PurityMarkers, Markers0, Markers),
    map.init(VarNameRemap),
    pred_info_init(PredOrFunc, PredModuleName, PredName, PredFormArity,
        Context, PredOrigin, PredStatus, MaybeCurUserDecl, GoalType,
        Markers, Types, TVarSet, ExistQVars, Constraints, Proofs,
        ConstraintMap, ClausesInfo, VarNameRemap, PredInfo0),
    predicate_table_search_pf_fqm_n_a(PredTable0, PredOrFunc,
        PredModuleName, PredName, PredFormArity, MaybeOrigPredId),
    (
        MaybeOrigPredId = yes(OrigPredId),
        MaybeNewPredId = no,
        module_info_pred_info(!.ModuleInfo, OrigPredId, OrigPredInfo),
        pred_info_get_context(OrigPredInfo, OrigContext),
        ( if PredStatus0 = pred_status(status_opt_imported) then
            true
        else
            PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            report_multiply_defined(PredOrFuncStr, PredSymName, UserArity,
                Context, OrigContext, [], !Specs)
        )
    ;
        MaybeOrigPredId = no,
        module_info_get_partial_qualifier_info(!.ModuleInfo, PQInfo),
        predicate_table_insert_qual(PredInfo0, NeedQual, PQInfo, PredId,
            PredTable0, PredTable1),
        MaybeNewPredId = yes(PredId),
        ( if pred_info_is_builtin(PredInfo0) then
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_target(Globals, CompilationTarget),
            add_builtin(!.ModuleInfo, CompilationTarget, PredId, Types,
                PredInfo0, PredInfo),
            predicate_table_get_pred_id_table(PredTable1, PredIdTable1),
            map.det_update(PredId, PredInfo, PredIdTable1, PredIdTable),
            predicate_table_set_pred_id_table(PredIdTable,
                PredTable1, PredTable)
        else
            PredTable = PredTable1
        ),
        module_info_set_predicate_table(PredTable, !ModuleInfo)
    ),
    maybe_report_any_unqualified_types(PredStatus0, PredSymName, Context,
        Types, !Specs).

:- pred compute_maybe_cur_user_decl(pred_status::in, maybe_predmode_decl::in,
    item_seq_num::in, maybe(cur_user_decl_info)::out) is det.

compute_maybe_cur_user_decl(PredStatus, PredmodeDecl, SeqNum,
        MaybeCurUserDecl) :-
    ( if
        % NOTE This code is duplicating the effect of
        %
        % MaybeItemMercuryStatus = yes(ItemMercuryStatus),
        % ItemMercuryStatus = item_defined_in_this_module(ItemExport)
        %
        % without requiring our caller to pass ItemMercuryStatus here.
        % The reason why this is important is that for compiler-generated
        % predicate declarations, there is no natural ItemMercuryStatus.
        PredStatus = pred_status(OldItemStatus),
        (
            OldItemStatus = status_local,
            ItemExport = item_export_nowhere
        ;
            OldItemStatus = status_exported_to_submodules,
            ItemExport = item_export_only_submodules
        ;
            OldItemStatus = status_exported,
            ItemExport = item_export_anywhere
        )
    then
        DeclSection = item_decl_section(ItemExport),
        MaybeCurUserDecl = yes(cur_user_decl_info(DeclSection,
            PredmodeDecl, SeqNum))
    else
        MaybeCurUserDecl = no
    ).

%---------------------%

:- func item_decl_section(item_export) = decl_section.

item_decl_section(ItemExport) = DeclSection :-
    (
        ItemExport = item_export_anywhere,
        DeclSection = decl_interface
    ;
        ( ItemExport = item_export_nowhere
        ; ItemExport = item_export_only_submodules
        ),
        DeclSection = decl_implementation
    ).

%---------------------%

:- pred maybe_report_any_unqualified_types(pred_status::in, sym_name::in,
    prog_context::in, list(mer_type)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_report_any_unqualified_types(PredStatus0, PredSymName, Context, Types,
        !Specs) :-
    DefnThisModule = pred_status_defined_in_this_module(PredStatus0),
    (
        DefnThisModule = yes
    ;
        DefnThisModule = no,
        % All predicate and function declarations read in from
        % automatically generated interface files should be fully qualified,
        % *provided* that the source files they are derived from
        % import all the modules needed to module qualify them.
        %
        % For now, we look for and report any unqualified types read in
        % from .int files. Once we can guarantee that such things cannot occur,
        % by making --print-errors-warnings-when-generating-interface
        % not just the default but not even an option that can be switched off,
        % this code should not be needed anymore.
        report_any_unqualified_types(PredSymName, Context, Types, !Specs)
    ).

:- pred report_any_unqualified_types(sym_name::in, prog_context::in,
    list(mer_type)::in, list(error_spec)::in, list(error_spec)::out) is det.

report_any_unqualified_types(_PredSymName, _Context, [], !Specs).
report_any_unqualified_types(PredSymName, Context, [Type | Types], !Specs) :-
    report_any_unqualified_type(PredSymName, Context, Type, !Specs),
    report_any_unqualified_types(PredSymName, Context, Types, !Specs).

:- pred report_any_unqualified_type(sym_name::in, prog_context::in,
    mer_type::in, list(error_spec)::in, list(error_spec)::out) is det.

report_any_unqualified_type(PredSymName, Context, Type, !Specs) :-
    (
        Type = defined_type(TypeCtorSymName, ArgTypes, _Kind),
        (
            TypeCtorSymName = qualified(_, _)
        ;
            TypeCtorSymName = unqualified(TypeCtorName),
            (
                PredSymName = qualified(PredModuleName, _),
                Pieces = [words("Error: unqualified type"),
                    quote(TypeCtorName),
                    words("in automatically generated interface file."),
                    words("The problem is that the definition of this type"),
                    words("is not visible in the source file of the"),
                    qual_sym_name(PredModuleName), words("module."),
                    words("The cause is probably"),
                    words("either a typo in the type name,"),
                    words("or a missing"), decl("import_module"),
                    words("declaration."), nl],
                Spec = spec($pred, severity_error, phase_pt2h,
                    Context, Pieces),
                !:Specs = [Spec | !.Specs]
            ;
                PredSymName = unqualified(_)
                % While a module qualification may be missing from a type name
                % in a predicate declaration, it *should not* be missing
                % from the name of the predicate (or function) itself,
                % since the parser implicitly module qualifies such names.
            )
        ),
        report_any_unqualified_types(PredSymName, Context, ArgTypes, !Specs)
    ;
        Type = tuple_type(ArgTypes, _Kind),
        report_any_unqualified_types(PredSymName, Context, ArgTypes, !Specs)
    ;
        Type = higher_order_type(_PorF, ArgTypes, _HOInstInfo, _Purity),
        report_any_unqualified_types(PredSymName, Context, ArgTypes, !Specs)
    ;
        Type = apply_n_type(_TVar, ArgTypes, _Kind),
        report_any_unqualified_types(PredSymName, Context, ArgTypes, !Specs)
    ;
        Type = kinded_type(SubType, _Kind),
        report_any_unqualified_type(PredSymName, Context, SubType, !Specs)
    ;
        ( Type = type_variable(_, _)
        ; Type = builtin_type(_)
        )
    ).

%---------------------------------------------------------------------------%

:- type maybe_stub
    --->    stub
    ;       non_stub(hlds_goal).

    % For most builtin predicates, say foo/2, we add a clause
    %
    %   foo(H1, H2) :- foo(H1, H2).
    %
    % This does not generate an infinite loop! Instead, the compiler will
    % generate the usual builtin inline code for foo/2 in the body. The reason
    % for generating this forwarding code stub is so that things work correctly
    % if you take the address of the predicate.
    %
    % A few builtins are treated specially.
    %
:- pred add_builtin(module_info::in, compilation_target::in,
    pred_id::in, list(mer_type)::in, pred_info::in, pred_info::out) is det.

add_builtin(ModuleInfo, CompilationTarget, PredId, HeadTypes0, !PredInfo) :-
    ModuleName = pred_info_module(!.PredInfo),
    Name = pred_info_name(!.PredInfo),
    pred_info_get_context(!.PredInfo, Context),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_get_varset(ClausesInfo0, VarSet0),
    clauses_info_get_arg_vector(ClausesInfo0, ProcArgVector),
    % XXX ARGVEC - clean this up after the pred_info is converted to use
    % the arg_vector structure.
    HeadVars0 = proc_arg_vector_to_list(ProcArgVector),

    goal_info_init(Context, GoalInfo0),
    NonLocals = set_of_var.list_to_set(HeadVars0),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),
    ( if
        ModuleName = mercury_private_builtin_module,
        % This predicate is incompatible with some backends.
        Name = "store_at_ref_impure",
        require_complete_switch [CompilationTarget]
        (
            ( CompilationTarget = target_java
            ; CompilationTarget = target_csharp
            ),
            SupportsStore = no
        ;
            CompilationTarget = target_c,
            SupportsStore = yes
        ),
        SupportsStore = no
    then
        HeadVars = HeadVars0,
        HeadTypes = HeadTypes0,
        VarSet = VarSet0,
        MaybeStub = stub
    else if
        (
            ModuleName = mercury_private_builtin_module,
            Name = "trace_get_io_state"
        ;
            ModuleName = mercury_io_module,
            Name = "unsafe_get_io_state"
        )
    then
        varset.new_var(ZeroVar, VarSet0, VarSet),
        HeadVars = [ZeroVar | HeadVars0],
        HeadTypes = [int_type | HeadTypes0],

        ConsId = some_int_const(int_const(0)),
        LHS = ZeroVar,
        RHS = rhs_functor(ConsId, is_not_exist_constr, []),
        UnifyMode = unify_modes_li_lf_ri_rf(free, ground_inst,
            ground_inst, ground_inst),
        Unification = construct(ZeroVar, ConsId, [], [UnifyMode],
            construct_dynamically, cell_is_shared, no_construct_sub_info),
        UnifyContext = unify_context(umc_explicit, []),
        AssignExpr = unify(LHS, RHS, UnifyMode, Unification, UnifyContext),
        goal_info_set_nonlocals(set_of_var.make_singleton(ZeroVar),
            GoalInfo0, GoalInfoWithZero),
        AssignGoal = hlds_goal(AssignExpr, GoalInfoWithZero),

        CastExpr = generic_call(cast(unsafe_type_inst_cast), HeadVars,
            [in_mode, uo_mode], arg_reg_types_unset, detism_det),
        goal_info_set_nonlocals(set_of_var.list_to_set(HeadVars),
            GoalInfo0, GoalInfoWithZeroHeadVars),
        CastGoal = hlds_goal(CastExpr, GoalInfoWithZeroHeadVars),

        ConjExpr = conj(plain_conj, [AssignGoal, CastGoal]),
        ConjGoal = hlds_goal(ConjExpr, GoalInfoWithZeroHeadVars),

        Reason = promise_purity(purity_semipure),
        GoalExpr = scope(Reason, ConjGoal),
        GoalInfo = GoalInfo1,
        MaybeStub = non_stub(hlds_goal(GoalExpr, GoalInfo))
    else if
        (
            ModuleName = mercury_private_builtin_module,
            Name = "trace_set_io_state"
        ;
            ModuleName = mercury_io_module,
            Name = "unsafe_set_io_state"
        )
    then
        ConjExpr = conj(plain_conj, []),
        ConjGoal = hlds_goal(ConjExpr, GoalInfo),
        Reason = promise_purity(purity_impure),
        GoalExpr = scope(Reason, ConjGoal),
        GoalInfo = GoalInfo1,
        HeadVars = HeadVars0,
        HeadTypes = HeadTypes0,
        VarSet = VarSet0,
        MaybeStub = non_stub(hlds_goal(GoalExpr, GoalInfo))
    else
        % Construct the pseudo-recursive call to ModuleName.Name(HeadVars).
        SymName = qualified(ModuleName, Name),
        % Mode checking will figure out the mode.
        ModeId = invalid_proc_id,
        MaybeUnifyContext = no,
        % XXX ARGVEC
        GoalExpr = plain_call(PredId, ModeId, HeadVars0, inline_builtin,
            MaybeUnifyContext, SymName),
        pred_info_get_purity(!.PredInfo, Purity),
        goal_info_set_purity(Purity, GoalInfo1, GoalInfo),
        HeadVars = HeadVars0,
        HeadTypes = HeadTypes0,
        VarSet = VarSet0,
        MaybeStub = non_stub(hlds_goal(GoalExpr, GoalInfo))
    ),

    (
        MaybeStub = stub,
        set_clause_list([], ClausesRep)
    ;
        MaybeStub = non_stub(Goal),
        % Construct a clause containing that pseudo-recursive call.
        Clause = clause(all_modes, Goal, impl_lang_mercury, Context,
            [], init_unused_statevar_arg_map, clause_is_not_a_fact),
        set_clause_list([Clause], ClausesRep)
    ),

    % Put the clause we just built (if any) into the pred_info,
    % annotated with the appropriate types.
    vartypes_from_corresponding_lists(HeadVars, HeadTypes, ExplicitVarTypes),
    corresponding_vars_types_to_var_table(ModuleInfo, VarSet,
        HeadVars, HeadTypes, VarTable),
    rtti_varmaps_init(RttiVarMaps),
    map.init(TVarNameMap),
    ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
        VarTable, RttiVarMaps, TVarNameMap, ProcArgVector, ClausesRep,
        init_clause_item_numbers_comp_gen,
        no_foreign_lang_clauses, no_clause_syntax_errors),
    pred_info_set_clauses_info(ClausesInfo, !PredInfo),

    % It is pointless but harmless to inline these clauses. The main purpose
    % of the `no_inline' marker is to stop constraint propagation creating
    % real infinite loops in the generated code when processing calls to these
    % predicates. The code generator will still generate inline code for calls
    % to these predicates.
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(marker_user_marked_no_inline, Markers0, Markers1),
    (
        MaybeStub = stub,
        add_marker(marker_stub, Markers1, Markers2),
        add_marker(marker_builtin_stub, Markers2, Markers)
    ;
        MaybeStub = non_stub(_),
        Markers = Markers1
    ),
    pred_info_set_markers(Markers, !PredInfo).

%---------------------------------------------------------------------------%

module_add_mode_decl(PartOfPredmode, IsClassMethod,
        ItemMercuryStatus, PredStatus, ItemModeDecl, PredProcId,
        !ModuleInfo, !Specs) :-
    ItemModeDecl = item_mode_decl_info(PredSymName, MaybePredOrFunc,
        Modes, WithInst, _MaybeDetism, _InstVarSet, Context, _SeqNum),
    (
        PredSymName = unqualified(_PredName),
        unexpected($pred, "unqualified PredSymName")
    ;
        PredSymName = qualified(PredModuleName, PredName)
    ),
    % The equiv_type pass should have also either set the pred_or_func,
    % or removed the item from the parse tree.
    (
        MaybePredOrFunc = yes(PredOrFunc)
    ;
        MaybePredOrFunc = no,
        unexpected($pred, "no pred_or_func on mode declaration")
    ),
    % Any WithInst annotations should have been expanded
    % and the inst put into Modes by equiv_type.m.
    expect(unify(WithInst, no), $pred, "WithInst != no"),

    ( if PredName = "" then
        % This dummy PredProcId won't be used due to the error.
        PredProcId = proc(invalid_pred_id, invalid_proc_id),
        Pieces = [words("Error: you cannot declare a mode for a")] ++
            color_as_subject([words("predicate")]) ++
            color_as_incorrect([words("whose name is a variable.")]) ++ [nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        % Lookup the pred or func declaration in the predicate table.
        % If it is not there, generate a warning, and insert an implicit
        % declaration for the predicate. We presum it to be local, and
        % will infer its type automatically.
        PredFormArity = arg_list_arity(Modes),
        module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
        predicate_table_search_pf_fqm_n_a(PredicateTable0, PredOrFunc,
            PredModuleName, PredName, PredFormArity, MaybePredId),
        (
            MaybePredId = yes(PredId)
        ;
            MaybePredId = no,
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            Origin = origin_user(
                user_made_pred(PredOrFunc, PredSymName, UserArity)),
            add_implicit_pred_decl_report_error(PredOrFunc,
                PredModuleName, PredName, PredFormArity, PredStatus,
                IsClassMethod, Context, Origin,
                [decl("mode"), words("declaration")], PredId,
                !ModuleInfo, !Specs)
        ),
        module_info_get_predicate_table(!.ModuleInfo, PredicateTable1),
        predicate_table_get_pred_id_table(PredicateTable1, PredIdTable0),
        map.lookup(PredIdTable0, PredId, PredInfo0),
        module_do_add_mode(!.ModuleInfo, PartOfPredmode, IsClassMethod,
            ItemMercuryStatus, ItemModeDecl, PredInfo0, PredInfo, ProcId,
            !Specs),
        map.det_update(PredId, PredInfo, PredIdTable0, PredIdTable),
        predicate_table_set_pred_id_table(PredIdTable,
            PredicateTable1, PredicateTable),
        module_info_set_predicate_table(PredicateTable, !ModuleInfo),
        PredProcId = proc(PredId, ProcId)
    ).

:- pred module_do_add_mode(module_info::in, part_of_predmode::in,
    maybe_class_method::in, item_mercury_status::in, item_mode_decl_info::in,
    pred_info::in, pred_info::out, proc_id::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_do_add_mode(ModuleInfo, PartOfPredmode, IsClassMethod,
        ItemMercuryStatus, ItemModeDecl, !PredInfo, ProcId, !Specs) :-
    PredName = pred_info_name(!.PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
    ItemModeDecl = item_mode_decl_info(_PredSymName, _MaybePredOrFunc,
        Modes, _WithInst, MaybeDetism, InstVarSet, Context, SeqNum),
    PredFormArity = arg_list_arity(Modes),
    check_that_detism_is_declared(!.PredInfo, IsClassMethod, PredOrFunc,
        PredName, PredFormArity, MaybeDetism, Context, DetismDecl, !Specs),
    pred_info_get_cur_user_decl_info(!.PredInfo, MaybeCurUserDecl),
    (
        MaybeCurUserDecl = yes(CurUserDecl),
        CurUserDecl = cur_user_decl_info(PredDeclSection, PredIsPredMode,
            _PredDeclSeqNum),
        ( if
            PartOfPredmode = not_part_of_predmode,
            ItemMercuryStatus = item_defined_in_this_module(ItemExport)
        then
            check_for_mode_decl_in_wrong_section(PredDeclSection, ItemExport,
                PredOrFunc, PredName, PredFormArity, Context, !Specs),
            check_for_mode_decl_after_predmode(PredIsPredMode, PredOrFunc,
                PredName, PredFormArity, Context, !Specs)
        else
            true
        )
    ;
        MaybeCurUserDecl = no
        % We allow mode declarations for predicates (and functions) that have
        % no item_pred_decl. With the right compiler options, the argument
        % types will be inferred.
    ),
    % Add the mode declaration to the pred_info for this procedure.
    ArgLives = no,
    % Before the simplification pass, HasParallelConj is not meaningful.
    HasParallelConj = has_no_parallel_conj,
    add_new_proc(ModuleInfo, Context, SeqNum, InstVarSet,
        Modes, yes(Modes), ArgLives, DetismDecl, MaybeDetism,
        address_is_not_taken, HasParallelConj, !PredInfo, ProcId).

%---------------------%

:- pred check_that_detism_is_declared(pred_info::in, maybe_class_method::in,
    pred_or_func::in, string::in, pred_form_arity::in, maybe(determinism)::in,
    prog_context::in, detism_decl::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_that_detism_is_declared(PredInfo, IsClassMethod, PredOrFunc,
        PredName, PredFormArity, MaybeDetism, Context, DetismDecl, !Specs) :-
    (
        MaybeDetism = no,
        DetismDecl = detism_decl_none,
        pred_info_get_status(PredInfo, PredStatus),
        PredModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(PredModuleName, PredName),
        (
            IsClassMethod = is_a_class_method,
            report_unspecified_det_for_method(PredOrFunc, PredSymName,
                PredFormArity, Context, !Specs)
        ;
            IsClassMethod = is_not_a_class_method,
            IsExported = pred_status_is_exported(PredStatus),
            (
                IsExported = yes,
                report_unspecified_det_for_exported(PredOrFunc, PredSymName,
                    PredFormArity, Context, !Specs)
            ;
                IsExported = no,
                report_unspecified_det_for_local(PredOrFunc, PredSymName,
                    PredFormArity, Context, !Specs)
            )
        )
    ;
        MaybeDetism = yes(_),
        DetismDecl = detism_decl_explicit
    ).

%---------------------%

:- pred check_for_mode_decl_in_wrong_section(decl_section::in, item_export::in,
    pred_or_func::in, string::in, pred_form_arity::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_mode_decl_in_wrong_section(PredDeclSection, ItemExport,
        PredOrFunc, PredName, PredFormArity, Context, !Specs) :-
    ModeDeclSection = item_decl_section(ItemExport),
    ( if PredDeclSection = ModeDeclSection then
        true
    else
        ModeSectionStr = decl_section_to_string(ModeDeclSection),
        PredSectionStr = decl_section_to_string(PredDeclSection),
        user_arity_pred_form_arity(PredOrFunc,
            user_arity(UserArityInt), PredFormArity),
        NA = name_arity(PredName, UserArityInt),
        PredOrFuncDecl = pred_or_func_to_str(PredOrFunc),
        Pieces = [words("Error: mode declaration in the")] ++
            color_as_incorrect([fixed(ModeSectionStr), words("section")]) ++
            [words("for"), p_or_f(PredOrFunc)] ++
            color_as_subject([name_arity(NA), suffix(".")]) ++
            [words("It should be in the")] ++
            color_as_correct([fixed(PredSectionStr), words("section,")]) ++
            [words("because the corresponding"),
            decl(PredOrFuncDecl), words("declaration is there."), nl],
        Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- func decl_section_to_string(decl_section) = string.

decl_section_to_string(decl_interface) = "interface".
decl_section_to_string(decl_implementation) = "implementation".

%---------------------%

:- pred check_for_mode_decl_after_predmode(maybe_predmode_decl::in,
    pred_or_func::in, string::in, pred_form_arity::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_mode_decl_after_predmode(PredIsPredMode, PredOrFunc, PredName,
        PredFormArity, Context, !Specs) :-
    (
        PredIsPredMode = no_predmode_decl
    ;
        PredIsPredMode = predmode_decl,
        user_arity_pred_form_arity(PredOrFunc,
            UserArity, PredFormArity),
        PFNameArity = pred_pf_name_arity(PredOrFunc,
            unqualified(PredName), UserArity),
        PredModeSpec = report_mode_decl_after_predmode(PFNameArity, Context),
        !:Specs = [PredModeSpec | !.Specs]
    ).

report_mode_decl_after_predmode(PFNameArity, Context) = Spec :-
    PFNameArity = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
    UserArity = user_arity(UserArityInt),
    NA = name_arity(unqualify_name(SymName), UserArityInt),
    Pieces = [words("Error:"), p_or_f(PredOrFunc)] ++
        color_as_subject([name_arity(NA)]) ++
        [words("has its"), p_or_f(PredOrFunc), words("declaration"),
        words("combined with a mode declaration, so")] ++
        color_as_incorrect(
            [words("it may not have a separate mode declaration.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces).

%---------------------------------------------------------------------------%

:- pred report_unspecified_det_for_method(pred_or_func::in, sym_name::in,
    pred_form_arity::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unspecified_det_for_method(PorF, SymName, PredFormArity, Context,
        !Specs) :-
    user_arity_pred_form_arity(PorF, user_arity(UserArityInt), PredFormArity),
    SNA = sym_name_arity(SymName, UserArityInt),
    Pieces = [words("Error:")] ++
        color_as_incorrect([words("no determinism declaration")]) ++
        [words("for type class method"), p_or_f(PorF)] ++
        % We used to qualify (the predecessor of) SNA.
        color_as_subject([unqual_sym_name_arity(SNA), suffix(".")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred report_unspecified_det_for_exported(pred_or_func::in, sym_name::in,
    pred_form_arity::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unspecified_det_for_exported(PorF, SymName, PredFormArity, Context,
        !Specs) :-
    user_arity_pred_form_arity(PorF, user_arity(UserArityInt), PredFormArity),
    SNA = sym_name_arity(SymName, UserArityInt),
    Pieces = [words("Error:")] ++
        color_as_incorrect([words("no determinism declaration")]) ++
        [words("for exported"), p_or_f(PorF)] ++
        color_as_subject([unqual_sym_name_arity(SNA), suffix(".")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred report_unspecified_det_for_local(pred_or_func::in, sym_name::in,
    pred_form_arity::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unspecified_det_for_local(PorF, SymName, PredFormArity, Context,
        !Specs) :-
    user_arity_pred_form_arity(PorF, user_arity(UserArityInt), PredFormArity),
    SNA = sym_name_arity(SymName, UserArityInt),
    MainPieces = [words("Error:")] ++
        color_as_incorrect([words("no determinism declaration")]) ++
        [words("for local"), p_or_f(PorF)] ++
        color_as_subject([unqual_sym_name_arity(SNA), suffix(".")]) ++
        [nl],
    VerbosePieces = [words("(This is an error because"),
        words("you specified the"), quote("--no-infer-det"), words("option."),
        words("Use the"), quote("--infer-det"),
        words("option if you want the compiler"),
        words("to automatically infer the determinism"),
        words("of local predicates.)"), nl],
    Msg = simple_msg(Context,
        [always(MainPieces),
        verbose_only(verbose_once, VerbosePieces)]),
    Spec = conditional_spec($pred, infer_det, no, severity_error, phase_pt2h,
        [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

add_implicit_pred_decl_report_error(PredOrFunc, PredModuleName, PredName,
        PredFormArity, Status, IsClassMethod, Context, PredOrigin, DescPieces,
        PredId, !ModuleInfo, !Specs) :-
    PredSymName = qualified(PredModuleName, PredName),
    maybe_report_undefined_pred_error(!.ModuleInfo, PredOrFunc,
        PredSymName, PredFormArity, Status, IsClassMethod, Context,
        DescPieces, !Specs),
    (
        PredOrFunc = pf_function,
        user_arity_pred_form_arity(pf_function, UserArity, PredFormArity),
        % We perform this check here because the invocation of
        % check_preds_if_field_access_function in make_hlds_passes.m
        % will not cover this function, for the simple reason that
        % it processes the list of all pred_decl items, but this function
        % is being declared implicitly precisely because it is *missing*
        % its pred_decl item.
        %
        % We could alter check_preds_if_field_access_function to process
        % all the locally-declared functions in the pred_id_table, but
        % this is solution is just as good.
        maybe_check_field_access_function(!.ModuleInfo, PredSymName, UserArity,
            Status, Context, !Specs)
    ;
        PredOrFunc = pf_predicate
    ),
    clauses_info_init(PredOrFunc, cit_no_types(PredFormArity),
        init_clause_item_numbers_user, ClausesInfo),
    GoalType = goal_not_for_promise(np_goal_type_none),
    add_implicit_pred_decl(PredOrFunc, PredModuleName, PredName, PredFormArity,
        Status, Context, PredOrigin, GoalType, ClausesInfo,
        PredId, !ModuleInfo).

add_implicit_pred_decl(PredOrFunc, PredModuleName, PredName, PredFormArity,
        PredStatus, Context, PredOrigin, GoalType, ClausesInfo,
        PredId, !ModuleInfo) :-
    MaybeCurUserDecl = maybe.no,
    init_markers(Markers0),
    varset.init(TVarSet0),
    PredFormArity = pred_form_arity(PredFormArityInt),
    make_n_fresh_vars("T", PredFormArityInt, TypeVars, TVarSet0, TVarSet),
    prog_type.var_list_to_type_list(map.init, TypeVars, Types),
    % We assume none of the arguments are existentially typed.
    % Existential types must be declared, they won't be inferred.
    ExistQVars = [],
    % The class context is empty since this is an implicit definition.
    % Inference will fill it in.
    Constraints = univ_exist_constraints([], []),
    map.init(Proofs),
    map.init(ConstraintMap),
    map.init(VarNameRemap),
    pred_info_init(PredOrFunc, PredModuleName, PredName, PredFormArity,
        Context, PredOrigin, PredStatus, MaybeCurUserDecl, GoalType, Markers0,
        Types, TVarSet, ExistQVars, Constraints, Proofs, ConstraintMap,
        ClausesInfo, VarNameRemap, PredInfo0),
    add_marker(marker_infer_type, Markers0, Markers1),
    add_marker(marker_no_pred_decl, Markers1, Markers),
    pred_info_set_markers(Markers, PredInfo0, PredInfo),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    predicate_table_search_pf_fqm_n_a(PredicateTable0, PredOrFunc,
        PredModuleName, PredName, PredFormArity, MaybePredId),
    (
        MaybePredId = no,
        module_info_get_partial_qualifier_info(!.ModuleInfo, MQInfo),
        predicate_table_insert_qual(PredInfo, may_be_unqualified, MQInfo,
            PredId, PredicateTable0, PredicateTable),
        module_info_set_predicate_table(PredicateTable, !ModuleInfo)
    ;
        MaybePredId = yes(_),
        ( if PredOrigin = origin_user(user_made_assertion(_, _, _)) then
            % We add promises to the HLDS *after* we add all user predicate
            % declarations.
            PredSymName = qualified(PredModuleName, PredName),
            NameString = sym_name_to_string(PredSymName),
            string.format("%s %s %s (%s).\n",
                [s("Attempted to introduce a predicate for a promise"),
                s("with a name that is identical to the name of"),
                s("an existing predicate"), s(NameString)],
                UnexpectedMsg),
            unexpected($pred, UnexpectedMsg)
        else
            unexpected($pred, "search succeeded")
        )
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pred.
%---------------------------------------------------------------------------%
