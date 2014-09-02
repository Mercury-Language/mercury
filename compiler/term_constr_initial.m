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

:- import_module hlds.hlds_module.

:- import_module io.

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
:- pred term_constr_initial.preprocess_module(module_info::in,
    module_info::out, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.globals.
:- import_module libs.lp_rational.
:- import_module libs.options.
:- import_module libs.polyhedron.
:- import_module libs.rat.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_errors.
:- import_module transform_hlds.term_constr_main.
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
:- import_module string.
:- import_module term.

%----------------------------------------------------------------------------%
%
% Process each predicate and set the termination property
%

% Sets termination to `cannot_loop' if:
%   - there is a `terminates' pragma defined for the predicate.
%   - there is a `check_termination' pragma defined for the predicate
%     *and* the compiler is not currently generating the intermodule
%     optimization file.
%   - the predicate is builtin or compiler generated.  (These are assumed
%     to terminate.)
%
%  Set the termination to `can_loop' if:
%   - there is a `does_not_terminate' pragma defined for this predicate.
%   - the predicate is imported and there is no other source of information
%   - about it (termination_info pragmas, terminates pragmas, builtin).
%
% XXX This does the wrong thing for copy/2 & typed_unify/2.  In both
% cases the constraints should that |HeadVar__1| = |HeadVar__2|.
% Also look at builtin_compound_eq, builtin_compound_lt.

preprocess_module(!ModuleInfo, !IO) :-
    module_info_get_valid_predids(PredIds, !ModuleInfo),
    process_builtin_preds(PredIds, !ModuleInfo, !IO),
    process_imported_preds(PredIds, !ModuleInfo).

%----------------------------------------------------------------------------%
%
% Set the argument size constraints for imported procedures.
%

% When interargument size constraints are imported from other modules there
% are two parts.  The first of the these is a list of ints.  Each int
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
    some [!PredTable] (
        module_info_get_preds(!.ModuleInfo, !:PredTable),
        module_info_get_type_spec_info(!.ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, TypeSpecPredIds, _, _),
        ( not set.member(PredId, TypeSpecPredIds) ->
            map.lookup(!.PredTable, PredId, PredInfo0),
            process_imported_procs(PredInfo0, PredInfo),
            map.det_update(PredId, PredInfo, !PredTable),
            module_info_set_preds(!.PredTable, !ModuleInfo)
        ;
            true
        )
    ).

:- pred process_imported_procs(pred_info::in, pred_info::out) is det.

process_imported_procs(!PredInfo) :-
    some [!ProcTable] (
        pred_info_get_procedures(!.PredInfo, !:ProcTable),
        ProcIds = pred_info_procids(!.PredInfo),
        list.foldl(process_imported_proc, ProcIds, !ProcTable),
        pred_info_set_procedures(!.ProcTable, !PredInfo)
    ).

:- pred process_imported_proc(proc_id::in, proc_table::in, proc_table::out)
    is det.

process_imported_proc(ProcId, !ProcTable) :-
    some [!ProcInfo] (
        map.lookup(!.ProcTable, ProcId, !:ProcInfo),
        proc_info_get_termination2_info(!.ProcInfo, TermInfo0),
        (
            % Check that there is something to import.
            TermInfo0 ^ import_success = yes(_)
        ->
            process_imported_term_info(!.ProcInfo, TermInfo0, TermInfo),
            proc_info_set_termination2_info(TermInfo, !ProcInfo),
            map.det_update(ProcId, !.ProcInfo, !ProcTable)
        ;
            true
        )
    ).

:- pred process_imported_term_info(proc_info::in,
    termination2_info::in, termination2_info::out) is det.

process_imported_term_info(ProcInfo, !TermInfo) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    make_size_var_map(HeadVars, _SizeVarset, SizeVarMap),
    list.length(HeadVars, NumHeadVars),
    HeadVarIds = 0 .. NumHeadVars - 1,
    map.from_corresponding_lists(HeadVarIds, HeadVars, IdsToProgVars),
    create_substitution_map(HeadVarIds, IdsToProgVars, SizeVarMap, SubstMap),
    create_arg_size_polyhedron(SubstMap, !.TermInfo ^ import_success,
        MaybeSuccessPoly),
    create_arg_size_polyhedron(SubstMap, !.TermInfo ^ import_failure,
        MaybeFailurePoly),
    SizeVars = prog_vars_to_size_vars(SizeVarMap, HeadVars),
    !TermInfo ^ size_var_map := SizeVarMap,
    !TermInfo ^ head_vars := SizeVars,
    !TermInfo ^ success_constrs := MaybeSuccessPoly,
    !TermInfo ^ failure_constrs := MaybeFailurePoly,

    % We don't use these fields after this point.
    !TermInfo ^ import_success := no,
    !TermInfo ^ import_failure := no.

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

:- pred process_builtin_preds(list(pred_id)::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

process_builtin_preds([], !ModuleInfo, !IO).
process_builtin_preds([PredId | PredIds], !ModuleInfo, !IO) :-
    write_pred_progress_message("% Termination checking ", PredId,
        !.ModuleInfo, !IO),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    some [!PredTable] (
        module_info_get_preds(!.ModuleInfo, !:PredTable),
        PredInfo0 = !.PredTable ^ det_elem(PredId),
        process_builtin_procs(MakeOptInt, PredId, !.ModuleInfo,
            PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, !PredTable),
        module_info_set_preds(!.PredTable, !ModuleInfo)
    ),
    process_builtin_preds(PredIds, !ModuleInfo, !IO).

    % It is possible for compiler generated/mercury builtin predicates
    % to be imported or locally defined, so they must be covered here
    % separately.
    %
:- pred process_builtin_procs(bool::in, pred_id::in, module_info::in,
    pred_info::in, pred_info::out) is det.

process_builtin_procs(MakeOptInt, PredId, ModuleInfo, !PredInfo) :-
    pred_info_get_import_status(!.PredInfo, ImportStatus),
    pred_info_get_markers(!.PredInfo, Markers),
    pred_info_get_context(!.PredInfo, Context),
    some [!ProcTable] (
        pred_info_get_procedures(!.PredInfo, !:ProcTable),
        ProcIds = pred_info_procids(!.PredInfo),
        (
            set_compiler_gen_terminates(!.PredInfo, ProcIds, PredId,
                ModuleInfo, !ProcTable)
        ->
            true
        ;
            % Since we cannot see the definition we consider procedures
            % defined using `:- external' to be imported.
            status_defined_in_this_module(ImportStatus) = yes
        ->
            % XXX At the moment if a procedure has a pragma terminates
            % declaration its argument size information is set to true.
            % If we allow the user to specify the arg size info this
            % will need to change.  This also means that the
            % size_var_map for the procedure is never created.  This
            % causes problems with intermodule optimization.  The
            % current workaround  is to set up a dummy size_var_map for
            % each procedure.
            %
            ( check_marker(Markers, marker_terminates) ->
                TermStatus = cannot_loop(term_reason_pragma_supplied),
                change_procs_constr_termination_info(ProcIds, yes, TermStatus,
                    !ProcTable),
                ArgSizeInfo = polyhedron.universe,
                change_procs_constr_arg_size_info(ProcIds, yes, ArgSizeInfo,
                    !ProcTable),
                initialise_size_var_maps(ProcIds, !ProcTable)
            ;
                true
            )
        ;
            % Not defined in this module.

            % All of the predicates that are processed in this section
            % are imported in some way.  With imported predicates, any
            % 'check_termination' pragmas will be checked by the
            % compiler when it compiles the relevant source file (that
            % the predicate was imported from).  When making the
            % intermodule optimization interfaces, the check_termination
            % will not be checked when the relevant source file is compiled,
            % so it cannot be depended upon.
            (
                (
                    check_marker(Markers, marker_terminates)
                ;
                    MakeOptInt = no,
                    check_marker(Markers, marker_check_termination)
                )
            ->
                change_procs_constr_termination_info(ProcIds, yes,
                    cannot_loop(term_reason_pragma_supplied), !ProcTable)
            ;
                change_procs_constr_termination_info(ProcIds, no,
                    can_loop([]), !ProcTable)
            ),
            ArgSizeInfo = polyhedron.universe,
            change_procs_constr_arg_size_info(ProcIds, yes, ArgSizeInfo,
                !ProcTable)
        ),
        ( check_marker(Markers, marker_does_not_terminate) ->
            TerminationInfo =
                can_loop([Context - does_not_term_pragma(PredId)]),
                NonTermArgSizeInfo = polyhedron.universe,
                change_procs_constr_termination_info(ProcIds, yes,
                    TerminationInfo, !ProcTable),
                change_procs_constr_arg_size_info(ProcIds, yes,
                    NonTermArgSizeInfo, !ProcTable),
                initialise_size_var_maps(ProcIds, !ProcTable)
        ;
            true
        ),
        pred_info_set_procedures(!.ProcTable, !PredInfo)
    ).
:- pred set_compiler_gen_terminates(pred_info::in, list(proc_id)::in,
    pred_id::in, module_info::in, proc_table::in, proc_table::out)
    is semidet.

set_compiler_gen_terminates(PredInfo, ProcIds, PredId, ModuleInfo,
        !ProcTable) :-
    ( hlds_pred.pred_info_is_builtin(PredInfo) ->
        set_builtin_terminates(ProcIds, PredId, PredInfo, ModuleInfo,
            !ProcTable)
    ;
        (
            Name  = pred_info_name(PredInfo),
            Arity = pred_info_orig_arity(PredInfo),
            special_pred_name_arity(SpecPredId0, Name, _, Arity),
            ModuleName = pred_info_module(PredInfo),
            any_mercury_builtin_module(ModuleName)
        ->
            SpecialPredId = SpecPredId0
        ;
            pred_info_get_origin(PredInfo, PredOrigin),
            PredOrigin = origin_special_pred(SpecialPredId - _)
        )
    ->
        set_generated_terminates(ProcIds, SpecialPredId, ModuleInfo,
            !ProcTable)
    ;
        fail
    ).

:- pred set_generated_terminates(list(proc_id)::in, special_pred_id::in,
    module_info::in, proc_table::in, proc_table::out) is det.

set_generated_terminates([], _, _, !ProcTable).
set_generated_terminates([ProcId | ProcIds], SpecialPredId, ModuleInfo,
        !ProcTable) :-
    (
        ( SpecialPredId = spec_pred_unify
        ; SpecialPredId = spec_pred_compare
        ; SpecialPredId = spec_pred_index
        ),
        ProcInfo0 = !.ProcTable ^ det_elem(ProcId),
        proc_info_get_headvars(ProcInfo0, HeadVars),
        proc_info_get_vartypes(ProcInfo0, VarTypes),
        special_pred_id_to_termination(SpecialPredId, HeadVars, ModuleInfo,
            VarTypes, ArgSize, Termination, VarMap, HeadSizeVars),
        some [!TermInfo] (
            proc_info_get_termination2_info(ProcInfo0, !:TermInfo),
            !TermInfo ^ success_constrs := yes(ArgSize),
            !TermInfo ^ term_status := yes(Termination),
            IntermodStatus = yes(not_mutually_recursive),
            !TermInfo ^ intermod_status := IntermodStatus,
            !TermInfo ^ size_var_map := VarMap,
            !TermInfo ^ head_vars := HeadSizeVars,
            proc_info_set_termination2_info(!.TermInfo, ProcInfo0, ProcInfo)
        ),
        map.det_update(ProcId, ProcInfo, !ProcTable)
    ;
        SpecialPredId = spec_pred_init
        % We don't need to do anything special for solver type initialisation
        % predicates. Leaving it up to the analyser may result in better
        % argument size information anyway.
    ),
    set_generated_terminates(ProcIds, SpecialPredId, ModuleInfo, !ProcTable).

    % Handle the generation of constraints for special predicates.
    % XXX argument size constraints for unify predicates for types
    % with user-defined equality may not be correct.
    %
:- pred special_pred_id_to_termination(special_pred_id::in, prog_vars::in,
    module_info::in, vartypes::in, constr_arg_size_info::out,
    constr_termination_info::out, size_var_map::out, size_vars::out) is det.

special_pred_id_to_termination(spec_pred_compare, HeadProgVars, ModuleInfo,
        VarTypes, ArgSizeInfo, Termination, SizeVarMap, HeadSizeVars) :-
    make_info(HeadProgVars, ModuleInfo, VarTypes, ArgSizeInfo, Termination,
        SizeVarMap, HeadSizeVars).
special_pred_id_to_termination(spec_pred_unify, HeadProgVars, ModuleInfo,
        VarTypes, ArgSizeInfo, Termination, SizeVarMap, HeadSizeVars) :-
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
        unexpected($module, $pred, "wrong number of args for unify")
    ),
    Polyhedron  = polyhedron.from_constraints(Constrs),
    ArgSizeInfo = Polyhedron,
    Termination = cannot_loop(term_reason_builtin).
special_pred_id_to_termination(spec_pred_index, HeadProgVars, ModuleInfo,
        VarTypes, ArgSize, Termination, SizeVarMap, HeadSizeVars) :-
    NumToDrop = list.length(HeadProgVars) - 2,
    ( list.drop(NumToDrop, HeadProgVars, _ZeroSizeHeadVars) ->
        make_info(HeadProgVars, ModuleInfo, VarTypes, ArgSize,
            Termination, SizeVarMap, HeadSizeVars)
    ;
        unexpected($module, $pred,
            "less than two arguments to builtin index")
    ).
special_pred_id_to_termination(spec_pred_init, _, _, _, _, _, _, _) :-
    unexpected($module, $pred, "initialise predicate").

    % Sets the termination status and argument size information for
    % those special_preds (compare and index) where the arguments
    % are either zero sized or unconstrained in size.
    %
:- pred make_info(list(prog_var)::in, module_info::in, vartypes::in,
    constr_arg_size_info::out, constr_termination_info::out,
    size_var_map::out, size_vars::out) is det.

make_info(HeadProgVars, ModuleInfo, VarTypes, ArgSize, Termination, SizeVarMap,
        HeadSizeVars) :-
    make_size_var_map(HeadProgVars, _SizeVarset, SizeVarMap),
    Zeros = find_zero_size_vars(ModuleInfo, SizeVarMap, VarTypes),
    Constraints = create_nonneg_constraints(SizeVarMap, Zeros),
    Polyhedron = polyhedron.from_constraints(Constraints),
    ArgSize = Polyhedron,
    Termination = cannot_loop(term_reason_builtin),
    HeadSizeVars = prog_vars_to_size_vars(SizeVarMap, HeadProgVars).

    % Set the termination information for builtin
    % predicates.  The list of proc_ids must refer to builtin predicates.
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
    ( no_type_info_builtin(PredModule, PredName, PredArity) ->
        Constrs = process_no_type_info_builtin(PredName, HeadVars,
            SizeVarMap)
    ; all_args_input_or_zero_size(ModuleInfo, PredInfo, ProcInfo0) ->
        Constrs = []
    ;
        unexpected($module, $pred, "builtin with non-zero size args")
    ),
    Polyhedron = polyhedron.from_constraints(Constrs),
    ArgSizeInfo = yes(Polyhedron),
    HeadSizeVars = prog_vars_to_size_vars(SizeVarMap, HeadVars),
    some [!TermInfo] (
        proc_info_get_termination2_info(ProcInfo0, !:TermInfo),
        !TermInfo ^ success_constrs := ArgSizeInfo,
        !TermInfo ^ term_status := yes(cannot_loop(term_reason_builtin)),
        !TermInfo ^ intermod_status := yes(not_mutually_recursive),
        !TermInfo ^ size_var_map := SizeVarMap,
        !TermInfo ^ head_vars := HeadSizeVars,
        proc_info_set_termination2_info(!.TermInfo, ProcInfo0, ProcInfo)
    ),
    map.det_update(ProcId, ProcInfo, !ProcTable),
    set_builtin_terminates(ProcIds, PredId, PredInfo, ModuleInfo, !ProcTable).

:- func process_no_type_info_builtin(string, prog_vars, size_var_map)
    = constraints.

process_no_type_info_builtin(PredName, HeadVars, SizeVarMap) = Constraints :-
    (
        HeadVars = [HVar1, HVar2],
        (
            ( PredName = "unsafe_type_cast"
            ; PredName = "unsafe_promise_unique"
            )
        ->
            SizeVar1 = prog_var_to_size_var(SizeVarMap, HVar1),
            SizeVar2 = prog_var_to_size_var(SizeVarMap, HVar2),
            ConstraintsPrime = [make_vars_eq_constraint(SizeVar1, SizeVar2)]
        ;
            ( PredName = "store_at_ref"
            ; PredName = "store_at_ref_impure"
            ; PredName = "builtin_compound_eq"
            ; PredName = "builtin_compound_lt"
            )
        ->
            ConstraintsPrime = []
        ;
            fail
        )
    ->
        Constraints = ConstraintsPrime
    ;
        unexpected($module, $pred, "unrecognised predicate")
    ).

%----------------------------------------------------------------------------%

:- pred initialise_size_var_maps(list(proc_id)::in,
    proc_table::in, proc_table::out) is det.

initialise_size_var_maps([], !ProcTable).
initialise_size_var_maps([ProcId | ProcIds], !ProcTable) :-
    ProcInfo0 = !.ProcTable ^ det_elem(ProcId),
    proc_info_get_termination2_info(ProcInfo0, TermInfo0),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    make_size_var_map(HeadVars, _SizeVarset, SizeVarMap),
    TermInfo = TermInfo0 ^ size_var_map := SizeVarMap,
    proc_info_set_termination2_info(TermInfo, ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcTable),
    initialise_size_var_maps(ProcIds, !ProcTable).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_initial.
%----------------------------------------------------------------------------%
