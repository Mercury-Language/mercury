%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: term_constr_initial.m.
% Main author: juliensf.
%
% This module fills in the appropriate argument size information and
% termination property for builtin and compiler generated predicates.
% It also handles the processing of termination pragmas and sets
% the termination properties for foreign procedures.
%
% Handling of pragma terminates/does_not_terminate
%
% At the moment we set the termination status as appropriate, set the arg
% size information to true and fill in the size_var_map with dummy values
% (because intermodule optimization requires these values to be in place).
% If we ever support user specified arg size constraints this scheme
% will need modifying - in particular we will need to make sure that
% the identity of the variables in the size_var_map matches those
% in the constraints.
%
% A lot of this code is based on that in termination.m that does the
% equivalent jobs for the old termination analyser.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.term_constr_initial.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

%----------------------------------------------------------------------------%

    % Prepare a module for running the main termination pass.
    % This involves setting up argument size and termination information
    % for builtin and compiler-generated predicates and also setting
    % the termination status of those predicates that have termination
    % pragmas attached to them.
    %
    % XXX Fix handling of terminates/does_not_terminate foreign proc.
    % attributes.
    %
:- pred term2_preprocess_module(module_info::in, module_info::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.lp_rational.
:- import_module libs.polyhedron.
:- import_module libs.rat.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_errors.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_constr_util.
:- import_module transform_hlds.term_util.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module term.

%----------------------------------------------------------------------------%
%
% Process each predicate and set the termination property.
%

% Sets termination to `cannot_loop' if:
%   - there is a `terminates' pragma defined for the predicate.
%   - there is a `check_termination' pragma defined for the predicate
%     *and* the compiler is not currently generating the intermodule
%     optimization file.
%   - the predicate is builtin or compiler generated. (These are assumed
%     to terminate.)
%
%  Set the termination to `can_loop' if:
%   - there is a `does_not_terminate' pragma defined for this predicate.
%   - the predicate is imported and there is no other source of information
%   - about it (termination_info pragmas, terminates pragmas, builtin).
%
% XXX This does the wrong thing for copy/2 & typed_unify/2. In both
% cases the constraints should that |HeadVar__1| = |HeadVar__2|.
% Also look at builtin_compound_eq, builtin_compound_lt.

term2_preprocess_module(!ModuleInfo) :-
    should_we_believe_check_termination_markers(!.ModuleInfo,
        BelieveCheckTerm),
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    process_builtin_preds(BelieveCheckTerm, PredIds, !ModuleInfo),
    process_imported_preds(PredIds, !ModuleInfo).

%----------------------------------------------------------------------------%
%
% Set the argument size constraints for imported procedures.
%

% When interargument size constraints are imported from other modules there
% are two parts. The first of the these is a list of ints. Each int
% represents one of a procedure's arguments (including typeinfo related ones).
%
% XXX Since typeinfos all became zero sized we don't actually need this list.
% (I forget the reason we did need it, I think it was something to do with
% the fact that the compiler cannot (presently) distinguish between typeinfos
% it has introduced and ones that were there anyway - in some of the library
% modules).
%
% The second piece of information here is the constraints themselves which
% have been using the integer variable ids to represent actual variables.
%
% We now know the actual head_vars so we create size_vars and substitute
% these into the actual constraints.
%

:- pred process_imported_preds(list(pred_id)::in,
    module_info::in, module_info::out) is det.

process_imported_preds(PredIds, !ModuleInfo) :-
    list.foldl(process_imported_pred, PredIds, !ModuleInfo).

:- pred process_imported_pred(pred_id::in, module_info::in, module_info::out)
    is det.

process_imported_pred(PredId, !ModuleInfo) :-
    module_info_get_type_spec_info(!.ModuleInfo, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecPredIds, _, _),
    ( if set.member(PredId, TypeSpecPredIds) then
        true
    else
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
        process_imported_procs(PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ).

:- pred process_imported_procs(pred_info::in, pred_info::out) is det.

process_imported_procs(!PredInfo) :-
    some [!ProcTable] (
        pred_info_get_proc_table(!.PredInfo, !:ProcTable),
        ProcIds = pred_info_valid_procids(!.PredInfo),
        list.foldl(process_imported_proc, ProcIds, !ProcTable),
        pred_info_set_proc_table(!.ProcTable, !PredInfo)
    ).

:- pred process_imported_proc(proc_id::in, proc_table::in, proc_table::out)
    is det.

process_imported_proc(ProcId, !ProcTable) :-
    some [!ProcInfo] (
        map.lookup(!.ProcTable, ProcId, !:ProcInfo),
        proc_info_get_termination2_info(!.ProcInfo, Term2Info0),
        MaybeImportSuccess = term2_info_get_import_success(Term2Info0),
        % Check that there is something to import.
        (
            MaybeImportSuccess = yes(_),
            process_imported_term_info(!.ProcInfo, Term2Info0, Term2Info),
            proc_info_set_termination2_info(Term2Info, !ProcInfo),
            map.det_update(ProcId, !.ProcInfo, !ProcTable)
        ;
            MaybeImportSuccess = no
        )
    ).

:- pred process_imported_term_info(proc_info::in,
    termination2_info::in, termination2_info::out) is det.

process_imported_term_info(ProcInfo, !Term2Info) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    make_size_var_map(HeadVars, _SizeVarset, SizeVarMap),
    list.length(HeadVars, NumHeadVars),
    HeadVarIds = 0 .. NumHeadVars - 1,
    map.from_corresponding_lists(HeadVarIds, HeadVars, IdsToProgVars),
    create_substitution_map(HeadVarIds, IdsToProgVars, SizeVarMap, SubstMap),
    create_arg_size_polyhedron(SubstMap,
        term2_info_get_import_success(!.Term2Info), MaybeSuccessPoly),
    create_arg_size_polyhedron(SubstMap,
        term2_info_get_import_failure(!.Term2Info), MaybeFailurePoly),
    SizeVars = prog_vars_to_size_vars(SizeVarMap, HeadVars),
    term2_info_set_size_var_map(SizeVarMap, !Term2Info),
    term2_info_set_head_vars(SizeVars, !Term2Info),
    term2_info_set_success_constrs(MaybeSuccessPoly, !Term2Info),
    term2_info_set_failure_constrs(MaybeFailurePoly, !Term2Info),

    % We don't use these fields after this point.
    term2_info_set_import_success(no, !Term2Info),
    term2_info_set_import_failure(no, !Term2Info).

:- pred create_substitution_map(list(int)::in, map(int, prog_var)::in,
    size_var_map::in, map(int, size_var)::out) is det.

create_substitution_map(Ids, IdToProgVar, SizeVarMap, IdToSizeVar) :-
    list.foldl((pred(Id::in, !.Map::in, !:Map::out) is det :-
            ProgVar = IdToProgVar ^ det_elem(Id),
            SizeVar = map.lookup(SizeVarMap, ProgVar),
            map.set(Id, SizeVar, !Map)
        ), Ids, map.init, IdToSizeVar).

:- pred create_arg_size_polyhedron(map(int, var)::in,
    maybe(pragma_constr_arg_size_info)::in, maybe(polyhedron)::out) is det.

create_arg_size_polyhedron(_, no, no).
create_arg_size_polyhedron(SubstMap, yes(PragmaArgSizeInfo),
        yes(Polyhedron)) :-
    list.map(create_arg_size_constraint(SubstMap), PragmaArgSizeInfo,
        Constraints),
    Polyhedron = polyhedron.from_constraints(Constraints).

:- pred create_arg_size_constraint(map(int, var)::in, arg_size_constr::in,
    constraint::out) is det.

create_arg_size_constraint(SubstMap, le(Terms0, Constant), Constraint) :-
    list.map(create_lp_term(SubstMap), Terms0, Terms),
    Constraint = construct_constraint(Terms, lp_lt_eq, Constant).
create_arg_size_constraint(SubstMap, eq(Terms0, Constant), Constraint) :-
    list.map(create_lp_term(SubstMap), Terms0, Terms),
    Constraint = construct_constraint(Terms, lp_eq, Constant).

:- pred create_lp_term(map(int, var)::in, arg_size_term::in, lp_term::out)
    is det.

create_lp_term(SubstMap, ArgSizeTerm, Var - Coefficient) :-
    ArgSizeTerm = arg_size_term(VarId, Coefficient),
    Var = SubstMap ^ det_elem(VarId).

%----------------------------------------------------------------------------%
%
% Set up information for builtins.
%

:- pred process_builtin_preds(maybe_believe_check_termination::in,
    list(pred_id)::in, module_info::in, module_info::out) is det.

process_builtin_preds(_, [], !ModuleInfo).
process_builtin_preds(BelieveCheckTerm, [PredId | PredIds], !ModuleInfo) :-
    process_builtin_pred(BelieveCheckTerm, PredId, !ModuleInfo),
    process_builtin_preds(BelieveCheckTerm, PredIds, !ModuleInfo).

:- pred process_builtin_pred(maybe_believe_check_termination::in,
    pred_id::in, module_info::in, module_info::out) is det.

process_builtin_pred(BelieveCheckTerm, PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    process_builtin_procs(BelieveCheckTerm, !.ModuleInfo, PredId,
        PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

    % It is possible for compiler generated/mercury builtin predicates
    % to be imported or locally defined, so they must be covered here
    % separately.
    %
:- pred process_builtin_procs(maybe_believe_check_termination::in,
    module_info::in, pred_id::in, pred_info::in, pred_info::out) is det.

process_builtin_procs(BelieveCheckTerm, ModuleInfo, PredId, !PredInfo) :-
    pred_info_get_status(!.PredInfo, PredStatus),
    pred_info_get_markers(!.PredInfo, Markers),
    pred_info_get_context(!.PredInfo, Context),
    some [!ProcTable] (
        pred_info_get_proc_table(!.PredInfo, !:ProcTable),
        ProcIds = pred_info_valid_procids(!.PredInfo),
        ( if
            set_compiler_gen_terminates(!.PredInfo, ProcIds, PredId,
                ModuleInfo, !ProcTable)
        then
            true
        else if
            % Since we cannot see their definition, we consider procedures
            % which have a `:- pragma external_{pred/func}' to be imported.
            pred_status_defined_in_this_module(PredStatus) = yes
        then
            % XXX At the moment if a procedure has a pragma terminates
            % declaration its argument size information is set to true.
            % If we allow the user to specify the arg size info this will
            % need to change. This also means that the size_var_map for the
            % procedure is never created. This causes problems with
            % intermodule optimization. The current workaround is to set up
            % a dummy size_var_map for each procedure.
            ( if check_marker(Markers, marker_terminates) then
                TermStatus = cannot_loop(term_reason_pragma_supplied),
                change_procs_constr_termination_info(ProcIds, yes, TermStatus,
                    !ProcTable),
                ArgSizeInfo = polyhedron.universe,
                change_procs_constr_arg_size_info(ProcIds, yes, ArgSizeInfo,
                    !ProcTable),
                initialise_size_var_maps(ProcIds, !ProcTable)
            else
                true
            )
        else
            % Not defined in this module.

            % All of the predicates that are processed in this section
            % are imported in some way. With imported predicates, any
            % 'check_termination' pragmas will be checked by the compiler
            % when it compiles the relevant source file (that the predicate
            % was imported from). When making the intermodule optimization
            % interfaces, the check_termination will not be checked when
            % the relevant source file is compiled, so it cannot be
            % depended upon.
            ( if
                (
                    check_marker(Markers, marker_terminates)
                ;
                    BelieveCheckTerm = do_believe_check_termination,
                    check_marker(Markers, marker_check_termination)
                )
            then
                change_procs_constr_termination_info(ProcIds, yes,
                    cannot_loop(term_reason_pragma_supplied), !ProcTable)
            else
                change_procs_constr_termination_info(ProcIds, no,
                    can_loop([]), !ProcTable)
            ),
            ArgSizeInfo = polyhedron.universe,
            change_procs_constr_arg_size_info(ProcIds, yes, ArgSizeInfo,
                !ProcTable)
        ),
        ( if check_marker(Markers, marker_does_not_terminate) then
            TerminationInfo =
                can_loop([term2_error(Context, does_not_term_pragma(PredId))]),
            NonTermArgSizeInfo = polyhedron.universe,
            change_procs_constr_termination_info(ProcIds, yes,
                TerminationInfo, !ProcTable),
            change_procs_constr_arg_size_info(ProcIds, yes,
                NonTermArgSizeInfo, !ProcTable),
            initialise_size_var_maps(ProcIds, !ProcTable)
        else
            true
        ),
        pred_info_set_proc_table(!.ProcTable, !PredInfo)
    ).

:- pred set_compiler_gen_terminates(pred_info::in, list(proc_id)::in,
    pred_id::in, module_info::in, proc_table::in, proc_table::out)
    is semidet.

set_compiler_gen_terminates(PredInfo, ProcIds, PredId, ModuleInfo,
        !ProcTable) :-
    ( if
        hlds_pred.pred_info_is_builtin(PredInfo)
    then
        set_builtin_terminates(ProcIds, PredId, PredInfo, ModuleInfo,
            !ProcTable)
    else if
        ( if
            Name  = pred_info_name(PredInfo),
            Arity = pred_info_orig_arity(PredInfo),
            special_pred_name_arity(SpecialPredId0, Name, _, Arity),
            ModuleName = pred_info_module(PredInfo),
            any_mercury_builtin_module(ModuleName)
        then
            SpecialPredId = SpecialPredId0
        else
            pred_info_get_origin(PredInfo, PredOrigin),
            PredOrigin = origin_special_pred(SpecialPredId, _)
        )
    then
        set_generated_terminates(ProcIds, SpecialPredId, ModuleInfo,
            !ProcTable)
    else
        fail
    ).

:- pred set_generated_terminates(list(proc_id)::in, special_pred_id::in,
    module_info::in, proc_table::in, proc_table::out) is det.

set_generated_terminates([], _, _, !ProcTable).
set_generated_terminates([ProcId | ProcIds], SpecialPredId, ModuleInfo,
        !ProcTable) :-
    ProcInfo0 = !.ProcTable ^ det_elem(ProcId),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    proc_info_get_vartypes(ProcInfo0, VarTypes),
    special_pred_id_to_termination(SpecialPredId, HeadVars, ModuleInfo,
        VarTypes, ArgSize, Termination, VarMap, HeadSizeVars),
    some [!Term2Info] (
        proc_info_get_termination2_info(ProcInfo0, !:Term2Info),
        term2_info_set_success_constrs(yes(ArgSize), !Term2Info),
        TermStatus = yes(Termination),
        term2_info_set_term_status(TermStatus, !Term2Info),
        IntermodStatus = yes(not_mutually_recursive),
        term2_info_set_intermod_status(IntermodStatus, !Term2Info),
        term2_info_set_size_var_map(VarMap, !Term2Info),
        term2_info_set_head_vars(HeadSizeVars, !Term2Info),
        proc_info_set_termination2_info(!.Term2Info, ProcInfo0, ProcInfo)
    ),
    map.det_update(ProcId, ProcInfo, !ProcTable),
    set_generated_terminates(ProcIds, SpecialPredId, ModuleInfo, !ProcTable).

    % Handle the generation of constraints for special predicates.
    % XXX argument size constraints for unify predicates for types
    % with user-defined equality may not be correct.
    %
:- pred special_pred_id_to_termination(special_pred_id::in, prog_vars::in,
    module_info::in, vartypes::in, constr_arg_size_info::out,
    constr_termination_info::out, size_var_map::out, size_vars::out) is det.

special_pred_id_to_termination(SpecialPredId, HeadProgVars, ModuleInfo,
        VarTypes, ArgSizeInfo, Termination, SizeVarMap, HeadSizeVars) :-
    (
        SpecialPredId = spec_pred_compare,
        make_spec_pred_constr_term_info(HeadProgVars, ModuleInfo, VarTypes,
            ArgSizeInfo, Termination, SizeVarMap, HeadSizeVars)
    ;
        SpecialPredId = spec_pred_unify,
        make_size_var_map(HeadProgVars, _SizeVarset, SizeVarMap),
        HeadSizeVars = prog_vars_to_size_vars(SizeVarMap, HeadProgVars),
        Zeros = find_zero_size_vars(ModuleInfo, SizeVarMap, VarTypes),
        NonZeroHeadSizeVars = list.filter(isnt(is_zero_size_var(Zeros)),
            HeadSizeVars),
        % Unify may have more than two input arguments if one of them is a
        % type-info related arg, or some such thing. Since all these have
        % zero size type, after removing them there are two possibilities.
        % The list of non-zero size type head_vars is empty (if the
        % arguments are zero sized) or it contains two elements.
        (
            NonZeroHeadSizeVars = [],
            Constrs  = []
        ;
            NonZeroHeadSizeVars = [VarA, VarB],
            Constrs  = [make_vars_eq_constraint(VarA, VarB)]
        ;
            ( NonZeroHeadSizeVars = [_]
            ; NonZeroHeadSizeVars = [_, _, _ | _]
            ),
            unexpected($pred, "wrong number of args for unify")
        ),
        Polyhedron  = polyhedron.from_constraints(Constrs),
        ArgSizeInfo = Polyhedron,
        Termination = cannot_loop(term_reason_builtin)
    ;
        SpecialPredId = spec_pred_index,
        NumToDrop = list.length(HeadProgVars) - 2,
        ( if list.drop(NumToDrop, HeadProgVars, _ZeroSizeHeadVars) then
            make_spec_pred_constr_term_info(HeadProgVars, ModuleInfo, VarTypes,
                ArgSizeInfo, Termination, SizeVarMap, HeadSizeVars)
        else
            unexpected($pred, "less than two arguments to builtin index")
        )
    ).

    % Sets the termination status and argument size information for
    % those special_preds (compare and index) where the arguments
    % are either zero sized or unconstrained in size.
    %
:- pred make_spec_pred_constr_term_info(list(prog_var)::in, module_info::in,
    vartypes::in, constr_arg_size_info::out, constr_termination_info::out,
    size_var_map::out, size_vars::out) is det.

make_spec_pred_constr_term_info(HeadProgVars, ModuleInfo, VarTypes,
        ArgSize, Termination, SizeVarMap, HeadSizeVars) :-
    make_size_var_map(HeadProgVars, _SizeVarset, SizeVarMap),
    Zeros = find_zero_size_vars(ModuleInfo, SizeVarMap, VarTypes),
    Constraints = create_nonneg_constraints(SizeVarMap, Zeros),
    Polyhedron = polyhedron.from_constraints(Constraints),
    ArgSize = Polyhedron,
    Termination = cannot_loop(term_reason_builtin),
    HeadSizeVars = prog_vars_to_size_vars(SizeVarMap, HeadProgVars).

    % Set the termination information for builtin predicates.
    % The list of proc_ids must refer to builtin predicates.
    %
:- pred set_builtin_terminates(list(proc_id)::in, pred_id::in, pred_info::in,
    module_info::in, proc_table::in, proc_table::out) is det.

set_builtin_terminates([], _, _, _, !ProcTable).
set_builtin_terminates([ProcId | ProcIds], PredId, PredInfo, ModuleInfo,
        !ProcTable) :-
    ProcInfo0 = !.ProcTable ^ det_elem(ProcId),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    PredModule = pred_info_module(PredInfo),
    PredName   = pred_info_name(PredInfo),
    PredArity  = pred_info_orig_arity(PredInfo),
    make_size_var_map(HeadVars, _SizeVarset, SizeVarMap),
    ( if no_type_info_builtin(PredModule, PredName, PredArity) then
        Constrs = process_no_type_info_builtin(PredName, HeadVars, SizeVarMap)
    else if all_args_input_or_zero_size(ModuleInfo, PredInfo, ProcInfo0) then
        Constrs = []
    else
        unexpected($pred, "builtin with non-zero size args")
    ),
    Polyhedron = polyhedron.from_constraints(Constrs),
    ArgSizeInfo = yes(Polyhedron),
    HeadSizeVars = prog_vars_to_size_vars(SizeVarMap, HeadVars),
    some [!Term2Info] (
        proc_info_get_termination2_info(ProcInfo0, !:Term2Info),
        term2_info_set_success_constrs(ArgSizeInfo, !Term2Info),
        TermStatus = yes(cannot_loop(term_reason_builtin)),
        term2_info_set_term_status(TermStatus, !Term2Info),
        IntermodStatus = yes(not_mutually_recursive),
        term2_info_set_intermod_status(IntermodStatus, !Term2Info),
        term2_info_set_size_var_map(SizeVarMap, !Term2Info),
        term2_info_set_head_vars(HeadSizeVars, !Term2Info),
        proc_info_set_termination2_info(!.Term2Info, ProcInfo0, ProcInfo)
    ),
    map.det_update(ProcId, ProcInfo, !ProcTable),
    set_builtin_terminates(ProcIds, PredId, PredInfo, ModuleInfo, !ProcTable).

:- func process_no_type_info_builtin(string, prog_vars, size_var_map)
    = constraints.

process_no_type_info_builtin(PredName, HeadVars, SizeVarMap) = Constraints :-
    % This predicate should handle every predicate listed by
    % no_type_info_builtin in mdbcomp/program_representation.m.
    %
    % NOTE We assume that no PredName occurs in more than one builtin module,
    % which is a fragile assumption.
    (
        HeadVars = [],
        unexpected($pred, "unrecognised arity-0 no_type_info_builtin")
    ;
        HeadVars = [_],
        unexpected($pred, "unrecognised arity-1 no_type_info_builtin")
    ;
        HeadVars = [HeadVar1, HeadVar2],
        ( if
            ( PredName = "builtin_compound_eq"
            ; PredName = "builtin_compound_lt"
            ; PredName = "get_future"
            ; PredName = "increment_size"
            ; PredName = "new_future"
            ; PredName = "partial_inst_copy"
            ; PredName = "signal_future"
            ; PredName = "store_at_ref_impure"
            ; PredName = "unsafe_promise_unique"
            ; PredName = "unsafe_type_cast"
            ; PredName = "wait_future"
            )
        then
            (
                ( PredName = "partial_inst_copy"
                ; PredName = "unsafe_type_cast"
                ; PredName = "unsafe_promise_unique"
                ),
                SizeVar1 = prog_var_to_size_var(SizeVarMap, HeadVar1),
                SizeVar2 = prog_var_to_size_var(SizeVarMap, HeadVar2),
                Constraints = [make_vars_eq_constraint(SizeVar1, SizeVar2)]
            ;
                ( PredName = "builtin_compound_eq"
                ; PredName = "builtin_compound_lt"
                ; PredName = "get_future"
                ; PredName = "increment_size"
                ; PredName = "new_future"
                ; PredName = "signal_future"
                ; PredName = "store_at_ref_impure"
                ; PredName = "wait_future"
                ),
                Constraints = []
            )
        else
            unexpected($pred, "unrecognised arity-2 no_type_info_builtin")
        )
    ;
        HeadVars = [_, _, _],
        ( if
            ( PredName = "compare_local_uint_words"
            ; PredName = "semidet_call_3"
            ; PredName = "superclass_from_typeclass_info"
            ; PredName = "table_lookup_insert_typeclassinfo"
            ; PredName = "table_lookup_insert_typeinfo"
            ; PredName = "table_restore_any_answer"
            ; PredName = "type_info_from_typeclass_info"
            ; PredName = "unconstrained_type_info_from_typeclass_info"
            )
        then
            Constraints = []
        else
            unexpected($pred, "unrecognised arity-3 no_type_info_builtin")
        )
    ;
        HeadVars = [_, _, _, _],
        ( if
            ( PredName = "compare_local_int16_bitfields"
            ; PredName = "compare_local_int32_bitfields"
            ; PredName = "compare_local_int8_bitfields"
            ; PredName = "instance_constraint_from_typeclass_info"
            ; PredName = "result_call_4"
            ; PredName = "semidet_call_4"
            ; PredName = "table_lookup_insert_enum"
            ; PredName = "unify_remote_arg_words"
            )
        then
            Constraints = []
        else
            unexpected($pred, "unrecognised arity-4 no_type_info_builtin")
        )
    ;
        HeadVars = [_, _, _, _, _],
        ( if
            ( PredName = "compare_local_uint_bitfields"
            ; PredName = "compare_remote_uint_words"
            ; PredName = "result_call_5"
            ; PredName = "semidet_call_5"
            )
        then
            Constraints = []
        else
            unexpected($pred, "unrecognised arity-5 no_type_info_builtin")
        )
    ;
        HeadVars = [_, _, _, _, _, _],
        ( if
            ( PredName = "compare_remote_int16_bitfields"
            ; PredName = "compare_remote_int32_bitfields"
            ; PredName = "compare_remote_int8_bitfields"
            ; PredName = "result_call_6"
            ; PredName = "semidet_call_6"
            )
        then
            Constraints = []
        else
            unexpected($pred, "unrecognised arity-6 no_type_info_builtin")
        )
    ;
        HeadVars = [_, _, _, _, _, _, _],
        ( if
            ( PredName = "compare_remote_uint_bitfields"
            ; PredName = "result_call_7"
            ; PredName = "semidet_call_7"
            )
        then
            Constraints = []
        else
            unexpected($pred, "unrecognised arity-7 no_type_info_builtin")
        )
    ;
        HeadVars = [_, _, _, _, _, _, _, _],
        ( if
            ( PredName = "result_call_8"
            ; PredName = "semidet_call_8"
            )
        then
            Constraints = []
        else
            unexpected($pred, "unrecognised arity-8 no_type_info_builtin")
        )
    ;
        HeadVars = [_, _, _, _, _, _, _, _, _],
        ( if
            PredName = "result_call_9"
        then
            Constraints = []
        else
            unexpected($pred, "unrecognised arity-9 no_type_info_builtin")
        )
    ;
        HeadVars = [_, _, _, _, _, _, _, _, _, _ | _],
        unexpected($pred, "unrecognised arity-10+ no_type_info_builtin")
    ).

%----------------------------------------------------------------------------%

:- pred initialise_size_var_maps(list(proc_id)::in,
    proc_table::in, proc_table::out) is det.

initialise_size_var_maps([], !ProcTable).
initialise_size_var_maps([ProcId | ProcIds], !ProcTable) :-
    ProcInfo0 = !.ProcTable ^ det_elem(ProcId),
    proc_info_get_termination2_info(ProcInfo0, Term2Info0),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    make_size_var_map(HeadVars, _SizeVarset, SizeVarMap),
    term2_info_set_size_var_map(SizeVarMap, Term2Info0, Term2Info),
    proc_info_set_termination2_info(Term2Info, ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcTable),
    initialise_size_var_maps(ProcIds, !ProcTable).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_initial.
%----------------------------------------------------------------------------%
