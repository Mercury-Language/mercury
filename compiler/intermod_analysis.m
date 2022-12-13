%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021-2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: intermod.m.
% Main author of the original trans_opt.m: crs.
% However, this module has been modified so much that not much of the original
% design is left.
%
% This module writes out the results of program analyses to both
% .opt and .trans_opt files. Those two jobs are both done here
% because the work required is almost the same for those two file kinds.
%
% For .opt files, analysis results make up their second half;
% the first half is written out by intermod.m.
%
% On the other hand, .trans_opt files contain *only* analysis results.
%
% The original comment on .trans_opt files follows. XXX Take it with a grain
% of salt, since many changes that should have updated it did not do so :-(
%
% Transitive intermodule optimization allows the compiler to do intermodule
% optimization that depends on other .trans_opt files. In comparison to .opt
% files, .trans_opt files allow much more accurate optimization to occur,
% but at the cost of an increased number of compilations required. The fact
% that a .trans_opt file may depend on other .trans_opt files introduces
% the possibility of circular dependencies occurring. These circular
% dependencies would occur if the data in A.trans_opt depended on the data
% in B.trans_opt being correct, and vice versa.
%
% We use the following system to ensure that circular dependencies cannot
% occur:
%
%   When mmake <module>.depend is run, mmc calculates a suitable ordering.
%   This ordering is then used to create each of the .d files. This allows
%   make to ensure that all necessary trans_opt files are up to date before
%   creating any other trans_opt files. This same information is used by mmc
%   to decide which trans_opt files may be imported when creating another
%   .trans_opt file. By observing the ordering decided upon when mmake
%   module.depend was run, any circularities which may have been created
%   are avoided.
%
% This module writes out the interface for transitive intermodule optimization.
% The .trans_opt file includes:
%   :- pragma termination_info declarations for all exported preds
%   :- pragma exceptions declarations for all exported preds
%   :- pragma trailing_info declarations for all exported preds.
%
% All these items should be module qualified.
% Constructors should be explicitly type qualified.
%
% Note that the .trans_opt file does not (yet) include clauses, `pragma
% foreign_proc' declarations, or any of the other information that would be
% needed for inlining or other optimizations. Currently it is used only for
% recording the results of program analyses, such as termination analysis,
% exception and trail usage analysis.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.intermod_analysis.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module set.

%---------------------------------------------------------------------------%
%
% This predicate appends the results of program analyses to .opt files
% in the form of pragma items.
% It is called from mercury_compile_middle_passes.m.
%
% All the analysis results we write out come from the proc_infos of the
% procedures to which they apply, with one exception: the results of
% unused args analysis. This is because we detect unused arguments
% in procedures so we can optimize those arguments away. This makes storing
% information about unused arguments in the proc_infos of the procedures
% to which they apply somewhat tricky, since that procedure may,
% immediately after the unused args are discovered, be transformed to
% eliminate the unused arguments, in which case the recorded information
% becomes dangling; it applies to a procedure that no longer exists.
% This should *not* happen to exported procedures, which are the only
% ones we want to write unused arg pragmas about to an optimization file,
% since other modules compiled without the right flags would still call
% the unoptimized original procedure. Nevertheless, to avoid storing
% analysis results in proc_infos that may apply only to a no-longer-existing
% version of the procedure, we pass the info in unused args pragmas
% to append_unused_arg_pragmas_to_opt_file separately.
%

:- pred append_analysis_pragmas_to_opt_file(io.output_stream::in,
    module_info::in, set(pragma_info_unused_args)::in,
    parse_tree_plain_opt::in, parse_tree_plain_opt::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Write out the contents of a module's .trans_opt file.
    %
:- pred write_trans_opt_file(io.output_stream::in, module_info::in,
    parse_tree_trans_opt::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type should_write_for
    --->    for_analysis_framework
    ;       for_pragma.

:- type maybe_should_write
    --->    should_not_write
    ;       should_write.

:- pred should_write_exception_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

:- pred should_write_trailing_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

:- pred should_write_mm_tabling_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

:- pred should_write_reuse_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

:- pred should_write_sharing_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, maybe_should_write::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.lp_rational.
:- import_module libs.polyhedron.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.item_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.intermod_order_pred_info.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_constr_util.
:- import_module transform_hlds.term_util.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module string.
:- import_module term_context.
:- import_module unit.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

append_analysis_pragmas_to_opt_file(Stream, ModuleInfo, UnusedArgsInfosSet,
        !ParseTreePlainOpt, !IO) :-
    module_info_get_proc_analysis_kinds(ModuleInfo, ProcAnalysisKinds),
    ( if
        set.is_empty(ProcAnalysisKinds),
        set.is_empty(UnusedArgsInfosSet)
    then
        % We have nothing to append to the .opt file.
        true
    else
        UnusedArgsInfos = set.to_sorted_list(UnusedArgsInfosSet),
        module_info_get_valid_pred_ids(ModuleInfo, PredIds),
        generate_order_pred_infos(ModuleInfo, PredIds, OrderPredInfos),

        gather_analysis_pragmas(ModuleInfo, ProcAnalysisKinds, OrderPredInfos,
            TermInfos, TermInfos2, Exceptions, TrailingInfos, MMTablingInfos,
            SharingInfos, ReuseInfos),

        maybe_write_block_start_blank_line(Stream, UnusedArgsInfos, !IO),
        list.foldl(mercury_output_pragma_unused_args(Stream),
            UnusedArgsInfos, !IO),
        write_analysis_pragmas(Stream, TermInfos, TermInfos2, Exceptions,
            TrailingInfos, MMTablingInfos, SharingInfos, ReuseInfos, !IO),

        !ParseTreePlainOpt ^ ptpo_unused_args :=
            list.map(wrap_dummy_pragma_item, UnusedArgsInfos),
        !ParseTreePlainOpt ^ ptpo_termination :=
            list.map(wrap_dummy_pragma_item, TermInfos),
        !ParseTreePlainOpt ^ ptpo_termination2 :=
            list.map(wrap_dummy_pragma_item, TermInfos2),
        !ParseTreePlainOpt ^ ptpo_exceptions :=
            list.map(wrap_dummy_pragma_item, Exceptions),
        !ParseTreePlainOpt ^ ptpo_trailing :=
            list.map(wrap_dummy_pragma_item, TrailingInfos),
        !ParseTreePlainOpt ^ ptpo_mm_tabling :=
            list.map(wrap_dummy_pragma_item, MMTablingInfos),
        !ParseTreePlainOpt ^ ptpo_struct_sharing :=
            list.map(wrap_dummy_pragma_item, SharingInfos),
        !ParseTreePlainOpt ^ ptpo_struct_reuse :=
            list.map(wrap_dummy_pragma_item, ReuseInfos)
    ).

%---------------------%

write_trans_opt_file(Stream, ModuleInfo, ParseTreeTransOpt, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    io.format(Stream, ":- module %s.\n", [s(ModuleNameStr)], !IO),

    % Select all the predicates for which something should be written
    % into the .trans_opt file.
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    PredIdsSet = set.list_to_set(PredIds),
    module_info_get_structure_reuse_preds(ModuleInfo, ReusePredsSet),
    PredIdsNoReusePredsSet = set.difference(PredIdsSet, ReusePredsSet),
    PredIdsNoReuseVersions = set.to_sorted_list(PredIdsNoReusePredsSet),
    generate_order_pred_infos(ModuleInfo, PredIdsNoReuseVersions,
        NoReuseOrderPredInfos),

    % Don't try to output pragmas for an analysis unless that analysis
    % was actually run.
    module_info_get_proc_analysis_kinds(ModuleInfo, ProcAnalysisKinds),
    gather_analysis_pragmas(ModuleInfo, ProcAnalysisKinds,
        NoReuseOrderPredInfos,
        TermInfos, TermInfos2, Exceptions, TrailingInfos, MMTablingInfos,
        SharingInfos, ReuseInfos),
    write_analysis_pragmas(Stream, TermInfos, TermInfos2, Exceptions,
        TrailingInfos, MMTablingInfos, SharingInfos, ReuseInfos, !IO),

    ParseTreeTransOpt = parse_tree_trans_opt(ModuleName, dummy_context,
        list.map(wrap_dummy_pragma_item, TermInfos),
        list.map(wrap_dummy_pragma_item, TermInfos2),
        list.map(wrap_dummy_pragma_item, Exceptions),
        list.map(wrap_dummy_pragma_item, TrailingInfos),
        list.map(wrap_dummy_pragma_item, MMTablingInfos),
        list.map(wrap_dummy_pragma_item, SharingInfos),
        list.map(wrap_dummy_pragma_item, ReuseInfos)).

%---------------------------------------------------------------------------%

:- pred gather_analysis_pragmas(module_info::in, set(proc_analysis_kind)::in,
    list(order_pred_info)::in,
    list(pragma_info_termination_info)::out,
    list(pragma_info_termination2_info)::out,
    list(pragma_info_exceptions)::out,
    list(pragma_info_trailing_info)::out,
    list(pragma_info_mm_tabling_info)::out,
    list(pragma_info_structure_sharing)::out,
    list(pragma_info_structure_reuse)::out) is det.

gather_analysis_pragmas(ModuleInfo, ProcAnalysisKinds, OrderPredInfos,
        TermInfos, TermInfos2, Exceptions, TrailingInfos, MMTablingInfos,
        SharingInfos, ReuseInfos) :-
    ( if set.contains(ProcAnalysisKinds, pak_termination) then
        list.foldl(
            gather_pragma_termination_for_pred(ModuleInfo),
            OrderPredInfos, cord.init, TermInfosCord),
        TermInfos = cord.list(TermInfosCord)
    else
        TermInfos = []
    ),
    ( if set.contains(ProcAnalysisKinds, pak_termination2) then
        list.foldl(
            gather_pragma_termination2_for_pred(ModuleInfo),
            OrderPredInfos, cord.init, TermInfos2Cord),
        TermInfos2 = cord.list(TermInfos2Cord)
    else
        TermInfos2 = []
    ),
    ( if set.contains(ProcAnalysisKinds, pak_exception) then
        list.foldl(
            gather_pragma_exceptions_for_pred(ModuleInfo),
            OrderPredInfos, cord.init, ExceptionsCord),
        Exceptions = cord.list(ExceptionsCord)
    else
        Exceptions = []
    ),
    ( if set.contains(ProcAnalysisKinds, pak_trailing) then
        list.foldl(
            gather_pragma_trailing_info_for_pred(ModuleInfo),
            OrderPredInfos, cord.init, TrailingInfosCord),
        TrailingInfos = cord.list(TrailingInfosCord)
    else
        TrailingInfos = []
    ),
    ( if set.contains(ProcAnalysisKinds, pak_mm_tabling) then
        list.foldl(
            gather_pragma_mm_tabling_info_for_pred(ModuleInfo),
            OrderPredInfos, cord.init, MMTablingInfosCord),
        MMTablingInfos = cord.list(MMTablingInfosCord)
    else
        MMTablingInfos = []
    ),
    ( if set.contains(ProcAnalysisKinds, pak_structure_sharing) then
        list.foldl(
            gather_pragma_structure_sharing_for_pred(ModuleInfo),
            OrderPredInfos, cord.init, SharingInfosCord),
        SharingInfos = cord.list(SharingInfosCord)
    else
        SharingInfos = []
    ),
    ( if set.contains(ProcAnalysisKinds, pak_structure_reuse) then
        list.foldl(
            gather_pragma_structure_reuse_for_pred(ModuleInfo),
            OrderPredInfos, cord.init, ReuseInfosCord),
        ReuseInfos = cord.list(ReuseInfosCord)
    else
        ReuseInfos = []
    ).

:- pred write_analysis_pragmas(io.text_output_stream::in,
    list(pragma_info_termination_info)::in,
    list(pragma_info_termination2_info)::in,
    list(pragma_info_exceptions)::in,
    list(pragma_info_trailing_info)::in,
    list(pragma_info_mm_tabling_info)::in,
    list(pragma_info_structure_sharing)::in,
    list(pragma_info_structure_reuse)::in,
    io::di, io::uo) is det.

write_analysis_pragmas(Stream, TermInfos, TermInfos2, Exceptions,
        TrailingInfos, MMTablingInfos, SharingInfos, ReuseInfos, !IO) :-
    maybe_write_block_start_blank_line(Stream, TermInfos, !IO),
    list.foldl(write_pragma_termination_info(Stream, output_mercury),
        TermInfos, !IO),
    maybe_write_block_start_blank_line(Stream, TermInfos2, !IO),
    list.foldl(write_pragma_termination2_info(Stream, output_mercury),
        TermInfos2, !IO),
    maybe_write_block_start_blank_line(Stream, Exceptions, !IO),
    list.foldl(mercury_output_pragma_exceptions(Stream),
        Exceptions, !IO),
    maybe_write_block_start_blank_line(Stream, TrailingInfos, !IO),
    list.foldl(mercury_output_pragma_trailing_info(Stream),
        TrailingInfos, !IO),
    maybe_write_block_start_blank_line(Stream, MMTablingInfos, !IO),
    list.foldl(mercury_output_pragma_mm_tabling_info(Stream),
        MMTablingInfos, !IO),
    maybe_write_block_start_blank_line(Stream, SharingInfos, !IO),
    list.foldl(write_pragma_structure_sharing_info(Stream, output_debug),
        SharingInfos, !IO),
    maybe_write_block_start_blank_line(Stream, ReuseInfos, !IO),
    list.foldl(write_pragma_structure_reuse_info(Stream, output_debug),
        ReuseInfos, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Gather termination_info pragmas for the predicate if it is exported,
    % it is not a builtin, and it is not a predicate used to force type
    % specialization.
    %
:- pred gather_pragma_termination_for_pred(module_info::in,
    order_pred_info::in,
    cord(pragma_info_termination_info)::in,
    cord(pragma_info_termination_info)::out) is det.

gather_pragma_termination_for_pred(ModuleInfo, OrderPredInfo,
        !TermInfosCord) :-
    OrderPredInfo = order_pred_info(_PredName, _PredArity, _PredOrFunc,
        PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
    ( if
        ( PredStatus = pred_status(status_exported)
        ; PredStatus = pred_status(status_opt_exported)
        ),
        not is_unify_index_or_compare_pred(PredInfo),

        % XXX These should be allowed, but the predicate declaration for
        % the specialized predicate is not produced before the termination
        % pragmas are read in, resulting in an undefined predicate error.
        not set.member(PredId, TypeSpecForcePreds)
    then
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.foldl(
            gather_pragma_termination_for_proc(OrderPredInfo),
            ProcTable, !TermInfosCord)
    else
        true
    ).

:- pred gather_pragma_termination_for_proc(order_pred_info::in,
    proc_id::in, proc_info::in,
    cord(pragma_info_termination_info)::in,
    cord(pragma_info_termination_info)::out) is det.

gather_pragma_termination_for_proc(OrderPredInfo, _ProcId, ProcInfo,
        !TermInfosCord) :-
    ( if proc_info_is_valid_mode(ProcInfo) then
        OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
            _PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        proc_info_get_maybe_arg_size_info(ProcInfo, MaybeArgSize),
        proc_info_get_maybe_termination_info(ProcInfo, MaybeTermination),
        PredNameModesPF =
            proc_pf_name_modes(PredOrFunc, PredSymName, ArgModes),
        MaybeParseTreeArgSize =
            maybe_arg_size_info_to_parse_tree(MaybeArgSize),
        MaybeParseTreeTermination =
            maybe_termination_info_to_parse_tree(MaybeTermination),
        TermInfo = pragma_info_termination_info(PredNameModesPF,
            MaybeParseTreeArgSize, MaybeParseTreeTermination),
        cord.snoc(TermInfo, !TermInfosCord)
    else
        true
    ).

:- func maybe_arg_size_info_to_parse_tree(maybe(arg_size_info)) =
    maybe(pragma_arg_size_info).

maybe_arg_size_info_to_parse_tree(MaybeArgSize) = MaybeParseTreeArgSize :-
    (
        MaybeArgSize = no,
        MaybeParseTreeArgSize = no
    ;
        MaybeArgSize = yes(ArgSize),
        (
            ArgSize = finite(Size, UsedArgs),
            ParseTreeArgSize = finite(Size, UsedArgs)
        ;
            ArgSize = infinite(_ErrorInfo),
            ParseTreeArgSize = infinite(unit)
        ),
        MaybeParseTreeArgSize = yes(ParseTreeArgSize)
    ).

:- func maybe_termination_info_to_parse_tree(maybe(termination_info)) =
    maybe(pragma_termination_info).

maybe_termination_info_to_parse_tree(MaybeTermination)
        = MaybeParseTreeTermination :-
    (
        MaybeTermination = no,
        MaybeParseTreeTermination = no
    ;
        MaybeTermination = yes(Termination),
        (
            Termination = cannot_loop(TermInfo),
            ParseTreeTermination = cannot_loop(TermInfo)
        ;
            Termination = can_loop(_ErrorInfo),
            ParseTreeTermination = can_loop(unit)
        ),
        MaybeParseTreeTermination = yes(ParseTreeTermination)
    ).

%---------------------------------------------------------------------------%

    % Gather termination2_info pragmas for the procedures of a predicate if:
    %   - the predicate is exported.
    %   - the predicate is not compiler generated.
    %
:- pred gather_pragma_termination2_for_pred(module_info::in,
    order_pred_info::in,
    cord(pragma_info_termination2_info)::in,
    cord(pragma_info_termination2_info)::out) is det.

gather_pragma_termination2_for_pred(ModuleInfo, OrderPredInfo,
        !TermInfo2sCord) :-
    OrderPredInfo = order_pred_info(_, _, _, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
    ( if
        ( PredStatus = pred_status(status_exported)
        ; PredStatus = pred_status(status_opt_exported)
        ),
        not hlds_pred.is_unify_index_or_compare_pred(PredInfo),
        not set.member(PredId, TypeSpecForcePreds)
    then
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.foldl(
            gather_pragma_termination2_for_proc(OrderPredInfo),
            ProcTable, !TermInfo2sCord)
    else
        true
    ).

:- pred gather_pragma_termination2_for_proc(order_pred_info::in,
    proc_id::in, proc_info::in,
    cord(pragma_info_termination2_info)::in,
    cord(pragma_info_termination2_info)::out) is det.

gather_pragma_termination2_for_proc(OrderPredInfo, _ProcId, ProcInfo,
        !TermInfo2sCord) :-
    ( if proc_info_is_valid_mode(ProcInfo) then
        OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
            _PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),

        proc_info_declared_argmodes(ProcInfo, ArgModes),
        proc_info_get_termination2_info(ProcInfo, Term2Info),
        MaybeSuccessConstraints = term2_info_get_success_constrs(Term2Info),
        MaybeFailureConstraints = term2_info_get_failure_constrs(Term2Info),
        MaybeTermination = term2_info_get_term_status(Term2Info),

        % NOTE: If this predicate is changed, then parse_pragma.m must also
        % be changed, so that it can parse the resulting pragmas.
        PredNameModesPF =
            proc_pf_name_modes(PredOrFunc, PredSymName, ArgModes),

        proc_info_get_headvars(ProcInfo, HeadVars),
        SizeVarMap = term2_info_get_size_var_map(Term2Info),
        HeadSizeVars = prog_vars_to_size_vars(SizeVarMap, HeadVars),
        list.length(HeadVars, NumHeadSizeVars),

        HeadSizeVarIds = 0 .. NumHeadSizeVars - 1,
        map.det_insert_from_corresponding_lists(HeadSizeVars, HeadSizeVarIds,
            map.init, VarToVarIdMap),
        maybe_constr_arg_size_info_to_arg_size_constr(VarToVarIdMap,
            MaybeSuccessConstraints, MaybeSuccessArgSizeInfo),
        maybe_constr_arg_size_info_to_arg_size_constr(VarToVarIdMap,
            MaybeFailureConstraints, MaybeFailureArgSizeInfo),

        (
            MaybeTermination = no,
            MaybePragmaTermination = no
        ;
            MaybeTermination = yes(cannot_loop(_)),
            MaybePragmaTermination = yes(cannot_loop(unit))
        ;
            MaybeTermination = yes(can_loop(_)),
            MaybePragmaTermination = yes(can_loop(unit))
        ),

        TermInfo2 = pragma_info_termination2_info(PredNameModesPF,
            MaybeSuccessArgSizeInfo, MaybeFailureArgSizeInfo,
            MaybePragmaTermination),
        cord.snoc(TermInfo2, !TermInfo2sCord)
    else
        true
    ).

%---------------------%

:- pred maybe_constr_arg_size_info_to_arg_size_constr(map(size_var, int)::in,
    maybe(constr_arg_size_info)::in, maybe(pragma_constr_arg_size_info)::out)
    is det.

maybe_constr_arg_size_info_to_arg_size_constr(VarToVarIdMap,
        MaybeArgSizeConstrs, MaybeArgSizeInfo) :-
    (
        MaybeArgSizeConstrs = no,
        MaybeArgSizeInfo = no
    ;
        MaybeArgSizeConstrs = yes(Polyhedron),
        Constraints0 = polyhedron.non_false_constraints(Polyhedron),
        Constraints1 = list.negated_filter(nonneg_constr, Constraints0),
        Constraints  = list.sort(Constraints1),
        list.map(lp_rational_constraint_to_arg_size_constr(VarToVarIdMap),
            Constraints, ArgSizeInfoConstrs),
        MaybeArgSizeInfo = yes(ArgSizeInfoConstrs)
    ).

:- pred lp_rational_constraint_to_arg_size_constr(map(size_var, int)::in,
    lp_rational.constraint::in, arg_size_constr::out) is det.

lp_rational_constraint_to_arg_size_constr(VarToVarIdMap,
        LPConstraint, ArgSizeConstr) :-
    deconstruct_non_false_constraint(LPConstraint,
        LPTerms, Operator, Constant),
    list.map(lp_term_to_arg_size_term(VarToVarIdMap), LPTerms, ArgSizeTerms),
    (
        Operator = lp_lt_eq,
        ArgSizeConstr = le(ArgSizeTerms, Constant)
    ;
        Operator = lp_eq,
        ArgSizeConstr = eq(ArgSizeTerms, Constant)
    ).

:- pred lp_term_to_arg_size_term(map(size_var, int)::in,
    lp_rational.lp_term::in, arg_size_term::out) is det.

lp_term_to_arg_size_term(VarToVarIdMap, LPTerm, ArgSizeTerm) :-
    LPTerm = Var - Coefficient,
    map.lookup(VarToVarIdMap, Var, VarId),
    ArgSizeTerm = arg_size_term(VarId, Coefficient).

%---------------------------------------------------------------------------%

    % Gather any exception pragmas for this predicate.
    %
:- pred gather_pragma_exceptions_for_pred(module_info::in, order_pred_info::in,
    cord(pragma_info_exceptions)::in, cord(pragma_info_exceptions)::out)
    is det.

gather_pragma_exceptions_for_pred(ModuleInfo, OrderPredInfo,
        !ExceptionsCord) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl(
        gather_pragma_exceptions_for_proc(ModuleInfo, OrderPredInfo),
        ProcTable, !ExceptionsCord).

:- pred gather_pragma_exceptions_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    cord(pragma_info_exceptions)::in, cord(pragma_info_exceptions)::out)
    is det.

gather_pragma_exceptions_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !ExceptionsCord) :-
    OrderPredInfo = order_pred_info(PredName, UserArity, PredOrFunc,
        PredId, PredInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_index_or_compare_pred(PredInfo),

        module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
        not set.member(PredId, TypeSpecForcePreds),

        % XXX Writing out pragmas for the automatically generated class
        % instance methods causes the compiler to abort when it reads them
        % back in.
        pred_info_get_markers(PredInfo, Markers),
        not check_marker(Markers, marker_class_instance_method),
        not check_marker(Markers, marker_named_class_instance_method),

        proc_info_get_exception_info(ProcInfo, MaybeProcExceptionInfo),
        MaybeProcExceptionInfo = yes(ProcExceptionInfo)
    then
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_id_to_int(ProcId, ModeNum),
        PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc,
            PredSymName, UserArity, ModeNum),
        ProcExceptionInfo = proc_exception_info(Status, _),
        ExceptionInfo = pragma_info_exceptions(PredNameArityPFMn, Status),
        cord.snoc(ExceptionInfo, !ExceptionsCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % Gather any trailing_info pragmas for this predicate.
    %
:- pred gather_pragma_trailing_info_for_pred(module_info::in,
    order_pred_info::in,
    cord(pragma_info_trailing_info)::in,
    cord(pragma_info_trailing_info)::out) is det.

gather_pragma_trailing_info_for_pred(ModuleInfo, OrderPredInfo,
        !TrailingInfosCord) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl(
        gather_pragma_trailing_info_for_proc(ModuleInfo,
            OrderPredInfo),
        ProcTable, !TrailingInfosCord).

:- pred gather_pragma_trailing_info_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    cord(pragma_info_trailing_info)::in,
    cord(pragma_info_trailing_info)::out) is det.

gather_pragma_trailing_info_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !TrailingInfosCord) :-
    OrderPredInfo = order_pred_info(PredName, UserArity, PredOrFunc,
        PredId, PredInfo),
    proc_info_get_trailing_info(ProcInfo, MaybeProcTrailingInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        MaybeProcTrailingInfo = yes(ProcTrailingInfo),
        should_write_trailing_info(ModuleInfo, PredId, ProcId, PredInfo,
            for_pragma, ShouldWrite),
        ShouldWrite = should_write
    then
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_id_to_int(ProcId, ModeNum),
        PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc,
            PredSymName, UserArity, ModeNum),
        ProcTrailingInfo = proc_trailing_info(Status, _),
        TrailingInfo = pragma_info_trailing_info(PredNameArityPFMn, Status),
        cord.snoc(TrailingInfo, !TrailingInfosCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % Write out the mm_tabling_info pragma for this predicate.
    %
:- pred gather_pragma_mm_tabling_info_for_pred(module_info::in,
    order_pred_info::in,
    cord(pragma_info_mm_tabling_info)::in,
    cord(pragma_info_mm_tabling_info)::out) is det.

gather_pragma_mm_tabling_info_for_pred(ModuleInfo, OrderPredInfo,
        !MMTablingInfosCord) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl(
        gather_pragma_mm_tabling_info_for_proc(ModuleInfo, OrderPredInfo),
        ProcTable, !MMTablingInfosCord).

:- pred gather_pragma_mm_tabling_info_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    cord(pragma_info_mm_tabling_info)::in,
    cord(pragma_info_mm_tabling_info)::out) is det.

gather_pragma_mm_tabling_info_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !MMTablingInfosCord) :-
    OrderPredInfo = order_pred_info(PredName, PredArity, PredOrFunc,
        PredId, PredInfo),
    proc_info_get_mm_tabling_info(ProcInfo, MaybeProcMMTablingInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        MaybeProcMMTablingInfo = yes(ProcMMTablingInfo),
        should_write_mm_tabling_info(ModuleInfo, PredId, ProcId, PredInfo,
            for_pragma, ShouldWrite),
        ShouldWrite = should_write
    then
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_id_to_int(ProcId, ModeNum),
        PredNameArityPFMn = proc_pf_name_arity_mn(PredOrFunc,
            PredSymName, PredArity, ModeNum),
        ProcMMTablingInfo = proc_mm_tabling_info(Status, _),
        MMTablingInfo =
            pragma_info_mm_tabling_info(PredNameArityPFMn, Status),
        cord.snoc(MMTablingInfo, !MMTablingInfosCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred gather_pragma_structure_sharing_for_pred(module_info::in,
    order_pred_info::in,
    cord(pragma_info_structure_sharing)::in,
    cord(pragma_info_structure_sharing)::out) is det.

gather_pragma_structure_sharing_for_pred(ModuleInfo, OrderPredInfo,
        !SharingInfosCord) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl(
        gather_pragma_structure_sharing_for_proc(ModuleInfo,
            OrderPredInfo),
        ProcTable, !SharingInfosCord).

:- pred gather_pragma_structure_sharing_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    cord(pragma_info_structure_sharing)::in,
    cord(pragma_info_structure_sharing)::out) is det.

gather_pragma_structure_sharing_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !SharingInfosCord) :-
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        should_write_sharing_info(ModuleInfo, PredId, ProcId, PredInfo,
            for_pragma, ShouldWrite),
        ShouldWrite = should_write,
        proc_info_get_structure_sharing(ProcInfo, MaybeSharingStatus),
        MaybeSharingStatus = yes(SharingStatus)
    then
        proc_info_get_var_table(ProcInfo, VarTable),
        split_var_table(VarTable, VarSet, _VarTypes),
        pred_info_get_typevarset(PredInfo, TypeVarSet),
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc,
            PredSymName, ArgModes),
        proc_info_get_headvars(ProcInfo, HeadVars),
        lookup_var_types(VarTable, HeadVars, HeadVarTypes),
        SharingStatus = structure_sharing_domain_and_status(Sharing, _Status),
        SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
            HeadVars, HeadVarTypes, VarSet, TypeVarSet, yes(Sharing)),
        cord.snoc(SharingInfo, !SharingInfosCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred gather_pragma_structure_reuse_for_pred(module_info::in,
    order_pred_info::in,
    cord(pragma_info_structure_reuse)::in,
    cord(pragma_info_structure_reuse)::out) is det.

gather_pragma_structure_reuse_for_pred(ModuleInfo, OrderPredInfo,
        !ReuseInfosCord) :-
    OrderPredInfo = order_pred_info(_, _, _, _, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.foldl(
        gather_pragma_structure_reuse_for_proc(ModuleInfo,
            OrderPredInfo),
        ProcTable, !ReuseInfosCord).

:- pred gather_pragma_structure_reuse_for_proc(module_info::in,
    order_pred_info::in, proc_id::in, proc_info::in,
    cord(pragma_info_structure_reuse)::in,
    cord(pragma_info_structure_reuse)::out) is det.

gather_pragma_structure_reuse_for_proc(ModuleInfo, OrderPredInfo,
        ProcId, ProcInfo, !ReuseInfosCord) :-
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ( if
        proc_info_is_valid_mode(ProcInfo),
        should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo,
            for_pragma, ShouldWrite),
        ShouldWrite = should_write,
        proc_info_get_structure_reuse(ProcInfo, MaybeStructureReuseDomain),
        MaybeStructureReuseDomain = yes(StructureReuseDomain)
    then
        proc_info_get_var_table(ProcInfo, VarTable),
        split_var_table(VarTable, VarSet, _VarTypes),
        pred_info_get_typevarset(PredInfo, TypeVarSet),
        ModuleName = pred_info_module(PredInfo),
        PredSymName = qualified(ModuleName, PredName),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        PredNameModesPF = proc_pf_name_modes(PredOrFunc, PredSymName,
            ArgModes),
        proc_info_get_headvars(ProcInfo, HeadVars),
        lookup_var_types(VarTable, HeadVars, HeadVarTypes),
        StructureReuseDomain =
            structure_reuse_domain_and_status(Reuse, _Status),
        ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
            HeadVars, HeadVarTypes, VarSet, TypeVarSet, yes(Reuse)),
        cord.snoc(ReuseInfo, !ReuseInfosCord)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

should_write_exception_info(ModuleInfo, PredId, ProcId, PredInfo,
        WhatFor, ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_index_or_compare_pred(PredInfo),
        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds),

            % XXX Writing out pragmas for the automatically generated class
            % instance methods causes the compiler to abort when it reads them
            % back in.
            pred_info_get_markers(PredInfo, Markers),
            not check_marker(Markers, marker_class_instance_method),
            not check_marker(Markers, marker_named_class_instance_method)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

should_write_trailing_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_index_or_compare_pred(PredInfo),
        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds),
            %
            % XXX Writing out pragmas for the automatically generated class
            % instance methods causes the compiler to abort when it reads them
            % back in.
            %
            pred_info_get_markers(PredInfo, Markers),
            not check_marker(Markers, marker_class_instance_method),
            not check_marker(Markers, marker_named_class_instance_method)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

should_write_mm_tabling_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_index_or_compare_pred(PredInfo),
        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds),

            % XXX Writing out pragmas for the automatically generated class
            % instance methods causes the compiler to abort when it reads them
            % back in.
            pred_info_get_markers(PredInfo, Markers),
            not check_marker(Markers, marker_class_instance_method),
            not check_marker(Markers, marker_named_class_instance_method)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_index_or_compare_pred(PredInfo),

        % Don't write out info for reuse versions of procedures.
        pred_info_get_origin(PredInfo, PredOrigin),
        PredOrigin \=
            origin_pred_transform(pred_transform_structure_reuse, _, _),

        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            % XXX These should be allowed, but the predicate declaration for
            % the specialized predicate is not produced before the structure
            % reuse pragmas are read in, resulting in an undefined predicate
            % error.
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

should_write_sharing_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    ( if
        % XXX If PredInfo is not a unify or compare pred, then all its
        % procedures must share the same status.
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_index_or_compare_pred(PredInfo),
        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            % XXX These should be allowed, but the predicate declaration for
            % the specialized predicate is not produced before the structure
            % sharing pragmas are read in, resulting in an undefined predicate
            % error.
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            not set.member(PredId, TypeSpecForcePreds)
        )
    then
        ShouldWrite = should_write
    else
        ShouldWrite = should_not_write
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.intermod_analysis.
%---------------------------------------------------------------------------%
