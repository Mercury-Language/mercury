%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997,2002-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: term_constr_main.m.
% Main author: juliensf.
%
% This module contains the top level of a termination analyser for Mercury.
% It is responsible for setting up the relevant data structures and invoking
% all the other passes.
%
% --------
% Overview
% --------
%
% The main data structures used by this analysis are defined in
% term_constr_data.m.
%
% The analysis is carried out in three passes:
%
% * Pass 0 - initial pass.
%   Setup information for builtin predicates and process any information
%   from user annotations like termination pragmas and foreign proc.
%   attributes.  Also, set up information imported from `.opt' and
%   `.trans_opt' files.  (See term_constr_initial.m.)
%
% Each of passes 1 and 2 are run consecutively on each SCC of the
% program call-graph.  This is done in bottom-up order.
%
% * Pass 1 - interargument size relationship (IR) analysis.
%
%   (a) Build pass.  Convert HLDS to the abstract representation (AR)
%       defined in term_constr_data.m.  (See term_constr_build.m.)
%
%   (b) Fixpoint pass. Perform fixpoint calculation to derive IR
%       information.  ( See term_constr_fixpoint.m.)
%
% * Pass 2 - termination.
%   Use the information from pass 1 to attempt to find a proof that the
%   procedures in a SCC terminate.  There is an example of such a proof
%   finder in term_constr_pass2.m, although we will probably end up with
%   several such modules, each of which tries a different approach.
%
% In addition there are some utility procedures in term_constr_util.m
% and some predicates for handling error messages in term_constr_error.m.
%
% The machinery for handling convex constraints (polyhedra) is in:
%
%   * polyhedron.m
%   * lp_rational.m
%   * rat.m
%
% TODO:
%   * add support for higher-order code and intermodule
%     mutual recursion.
%
%   * add support for user-defined IR constraints for foreign
%     code, :- pragma arg_size_relation(...) or something.
%
%   * experiment with different types of widening.
%
%   * experiment with different representation for the polyhedra.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.term_constr_main.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.
:- import_module libs.polyhedron.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_errors.

:- import_module io.
:- import_module list.
:- import_module maybe.

%----------------------------------------------------------------------------%
%
% Types that define termination information about procedures
%

    % This type is the interargument size relationships between
    % the arguments of a predicate.
    %
:- type constr_arg_size_info == polyhedron.

    % Whether or not a procedure is terminating and some idea of why
    % this is or is not the case.
    %
:- type constr_termination_info ==
    generic_termination_info(term_reason, term2_errors).

    % Why does the termination analyser think that a procedure is
    % terminating?  This is useful for debugging purposes.
    %
:- type term_reason
    --->    term_reason_builtin
            % Procedure was a builtin.

    ;       term_reason_pragma_supplied
            % Procedure has pragma terminates decl.

    ;       term_reason_foreign_supplied
            % Procedure has foreign code attribute.

    ;       term_reason_import_supplied
            % This procedure was imported and its termination status
            % was read in from a .opt or .trans_opt file.

    ;       term_reason_analysis.
            % Termination info. was derived via analysis.

    % Whether a procedure may be involved in mutual recursion
    % across module boundaries.
    %
    % XXX Termination analysis of mutual recursion across module boundaries
    % NYI.
    %
:- type intermod_status
    --->    not_mutually_recursive
    ;       may_be_mutually_recursive.

%----------------------------------------------------------------------------%
%
% The 'termination2_info' structure.
%

% All the information needed by the termination analysis is stored in
% this structure. There is one such structure attached to every procedure
% in the module.

:- type termination2_info.

:- func term2_info_init = termination2_info.

:- func termination2_info ^ size_var_map = size_var_map.
:- func termination2_info ^ import_success =
    maybe(pragma_constr_arg_size_info).
:- func termination2_info ^ import_failure =
    maybe(pragma_constr_arg_size_info).
:- func termination2_info ^ success_constrs = maybe(constr_arg_size_info).
:- func termination2_info ^ failure_constrs = maybe(constr_arg_size_info).
:- func termination2_info ^ term_status = maybe(constr_termination_info).
:- func termination2_info ^ abstract_rep = maybe(abstract_proc).
:- func termination2_info ^ intermod_status = maybe(intermod_status).
:- func termination2_info ^ head_vars = list(size_var).

:- func termination2_info ^ size_var_map := size_var_map = termination2_info.
:- func termination2_info ^ import_success :=
    maybe(pragma_constr_arg_size_info) = termination2_info.
:- func termination2_info ^ import_failure :=
    maybe(pragma_constr_arg_size_info) = termination2_info.
:- func termination2_info ^ success_constrs := maybe(constr_arg_size_info)
    = termination2_info.
:- func termination2_info ^ failure_constrs := maybe(constr_arg_size_info)
    = termination2_info.
:- func termination2_info ^ term_status := maybe(constr_termination_info)
    = termination2_info.
:- func termination2_info ^ intermod_status := maybe(intermod_status)
    = termination2_info.
:- func termination2_info ^ abstract_rep := maybe(abstract_proc)
    = termination2_info.
:- func termination2_info ^ head_vars := list(size_var)
    = termination2_info.

%----------------------------------------------------------------------------%
%
% Termination analysis
%

    % Perform termination analysis on a module.
    %
:- pred term_constr_main.pass(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Write out a termination2_info pragma for the predicate if:
    %   - it is exported.
    %   - it is not compiler generated.
    %
:- pred term_constr_main.output_pred_termination2_info(module_info::in,
    pred_id::in, io::di, io::uo) is det.

    % This predicate outputs termination_info pragmas;
    % such annotations can be part of .opt and .trans_opt files.
    %
:- pred term_constr_main.output_pragma_termination2_info(pred_or_func::in,
    sym_name::in, list(mer_mode)::in, prog_context::in,
    maybe(constr_arg_size_info)::in, maybe(constr_arg_size_info)::in,
    maybe(constr_termination_info)::in, size_vars::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.lp_rational.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.dependency_graph.
:- import_module transform_hlds.term_constr_build.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_errors.
:- import_module transform_hlds.term_constr_fixpoint.
:- import_module transform_hlds.term_constr_initial.
:- import_module transform_hlds.term_constr_pass2.
:- import_module transform_hlds.term_constr_util.
:- import_module transform_hlds.term_norm.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% The 'termination2_info' structure
%

:- type termination2_info
    --->    term2_info(
                % Map between prog_vars and size_vars for this procedure.
                size_var_map        :: size_var_map,

                % These are the size variables that occur in argument
                % size constraints. For procedures that are imported
                % via a `.opt' or `.trans_opt' file we set these during
                % the initial pass, for procedures in the module we are
                % analysing, pass 1 sets it.
                head_vars           :: size_vars,

                % Arg size info. imported from another module via a
                % `.opt' or `.trans_opt' file. Pass 0  needs to convert
                % these to the proper form. These particular fields are
                % of no use after that.
                import_success      :: maybe(pragma_constr_arg_size_info),
                import_failure      :: maybe(pragma_constr_arg_size_info),

                % The interargument size relationships
                % (expressed as convex constraints)
                % obtained during pass 1.
                success_constrs     :: maybe(constr_arg_size_info),

                % Failure constraints for predicates that can fail
                % (set by pass 1).
                failure_constrs     :: maybe(constr_arg_size_info),

                % The termination status of the procedure as determined
                % by pass 2.
                term_status         :: maybe(constr_termination_info),

                % Is this procedure (possibly) involved in mutual recursion
                % across module boundaries? Set by pass 1.
                intermod_status     :: maybe(intermod_status),

                % The abstract representation of this proc.
                % Set by term_constr_build.m.
                abstract_rep        :: maybe(abstract_proc)
            ).

term2_info_init = term2_info(map.init, [], no, no, no, no, no, no, no).

%----------------------------------------------------------------------------%
%
% Main pass.
%

term_constr_main.pass(!ModuleInfo, !IO) :-
    % Get options required by the build pass.
    % These are:
    %   - which norm we are using.
    %   - whether we are propagating failure constraints.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_termination2_norm(Globals, Norm),
    FunctorInfo = set_functor_info(!.ModuleInfo, Norm),
    globals.lookup_bool_option(Globals, propagate_failure_constrs, Fail),
    globals.lookup_bool_option(Globals, arg_size_analysis_only, ArgSizeOnly),
    BuildOptions = term_build_options_init(FunctorInfo, Fail, ArgSizeOnly),

    % Get options required by the fixpoint pass.
    % These are:
    %   - what cutoff value we are using
    %   - the maximum allowable matrix size
    %     (this is also used in pass 2).
    globals.lookup_int_option(Globals, term2_maximum_matrix_size, MaxSize),
    globals.lookup_int_option(Globals, widening_limit, CutOff),

    % NOTE: We may eventually allow other types of widening.
    Widening = after_fixed_cutoff(CutOff),
    FixpointOptions = fixpoint_options_init(Widening, MaxSize),

    % Get options required by pass 2.
    % Currently this is just the maximum matrix size.
    Pass2Options = pass2_options_init(MaxSize),

    % Setup termination information for builtins and compiler generated
    % predicates.  Setup information from termination pragmas and
    % foreign proc attributes.
    term_constr_initial.preprocess_module(!ModuleInfo, !IO),

    % Analyse the module per SCC in bottom-up fashion.
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
    list.foldl2(analyse_scc(SCCs, BuildOptions, FixpointOptions, Pass2Options),
        SCCs, !ModuleInfo, !IO),

    % Write termination2_info pragmas to `.opt' and `.trans_opt' files.
    maybe_make_optimization_interface(!.ModuleInfo, !IO).

%----------------------------------------------------------------------------%
%
% Analyse a single SCC.
%

    % Analyse a single SCC of the call graph.  This will require results
    % concerning SCCs lower down the call graph.
    %
    % The normal procedure for analysing a SCC is:
    %
    % (1) Build the abstract representation (AR) of the procedures in
    %     the SCC. (term_constr_build.m).
    %
    % (2) Use a fixpoint iteration to derive interargument size
    %     relationships for the procedures in the SCC.
    %     (term_constr_fixpoint.m).
    %
    % (3) Use this information to try and find a proof that the procedures
    %     in the SCC terminate.  (term_constr_pass2.m).
    %
    % Exactly which of the above steps gets carried out depends upon
    % what (if any) additional information the user has supplied.
    %
    % For Mercury procedures:
    % If the procedure is marked as terminating/non-terminating, we still
    % carry out steps (1) and (2). If a procedure has been supplied with
    % an argument size constraint we carry out steps (1) and (3). If both
    % have been supplied we do not carry out any steps.
    %
    % The usual caveats regarding the termination pragmas and mutual
    % recursion apply (see the reference manual).
    %
    % XXX What do we do about pragma arg_size_info and mutual recursion?
    % (Nothing yet, because it isn't implemented :-( )
    %
    % For procedures implemented as foreign code:
    % The usual defaults (see reference manual) apply.
    % If no argument size constraint is supplied then non-zero arguments
    % are assumed to have unbounded size.
    %
:- pred analyse_scc(dependency_ordering::in, term_build_options::in,
    fixpoint_options::in, pass2_options::in,
    list(pred_proc_id)::in, module_info::in, module_info::out, io::di,
    io::uo) is det.

analyse_scc(DepOrder, BuildOpts, FixpointOpts, Pass2Opts, SCC, !ModuleInfo,
        !IO) :-
    % Since all of the passes are potentially optional we need to initialise
    % the size_var_maps separately. If they are left uninitialised intermodule
    % optimization will not work.
    NeedsAR = list.filter(proc_needs_ar_built(!.ModuleInfo), SCC),

    % Build the abstract representation for those procedures that require it.
    % The procedures that require it are those that do not already have
    % both argument size information and termination information.
    term_constr_build_abstract_scc(DepOrder, NeedsAR, BuildOpts,
        BuildErrors, !ModuleInfo, !IO),

    % We only perform the fixpoint computation for those procedures where
    % we can gain meaningful information from it.  We do not do it when:
    % - the information is already known, or
    % - the argument size relationships depend upon higher-order calls.
    NeedsArgSize = list.filter(isnt(has_arg_size_info(!.ModuleInfo)), NeedsAR),
    term_constr_fixpoint.do_fixpoint_calculation(FixpointOpts,
        NeedsArgSize, 1, FixpointErrors, !ModuleInfo),

    % Errors detected during pass 1 always result in the rest of the analysis
    % being aborted for the SCC under consideration.
    Pass1Errors = BuildErrors ++ FixpointErrors,
    (
        Pass1Errors = [],
        MaybeEarlyPass2Result = no
    ;
        Pass1Errors = [_ | _],
        MaybeEarlyPass2Result = yes(can_loop(Pass1Errors))
    ),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, arg_size_analysis_only, ArgSizeOnly),
    (
        ArgSizeOnly = no,
        NeedsTerm = list.filter(isnt(has_term_info(!.ModuleInfo)), NeedsAR),

        % Some procedures may not have arg_size_info, but may have termination
        % info, i.e. those that have a pragma terminates or does_not_terminate
        % declaration.
        (
            MaybeEarlyPass2Result = yes(Pass2Result)
        ;
            MaybeEarlyPass2Result = no,
            term_constr_pass2.prove_termination_in_scc(Pass2Opts,
                NeedsTerm, !.ModuleInfo, Pass2Result, !IO)
        ),

        % Set the termination property for this procedure and
        % report any errors that we found during pass 2.
        set_termination_info_for_procs(NeedsTerm, Pass2Result, !ModuleInfo),
        (
            Pass2Result = can_loop(Errors),
            report_termination2_errors(NeedsTerm, Errors, !ModuleInfo, !IO)
        ;
            Pass2Result = cannot_loop(_)
        )
    ;
        ArgSizeOnly = yes
    ).

%-----------------------------------------------------------------------------%
%
% Procedures for storing 'termination2_info' in the HLDS.
%

:- pred set_termination_info_for_procs(list(pred_proc_id)::in,
    constr_termination_info::in, module_info::in, module_info::out) is det.

set_termination_info_for_procs(PPIds, TerminationInfo, !ModuleInfo) :-
    list.foldl(set_termination_info_for_proc(TerminationInfo), PPIds,
        !ModuleInfo).

:- pred set_termination_info_for_proc(constr_termination_info::in,
    pred_proc_id::in, module_info::in, module_info::out) is det.

set_termination_info_for_proc(TerminationInfo, PPId, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    proc_info_get_termination2_info(ProcInfo0, TermInfo0),
    TermInfo = TermInfo0 ^ term_status := yes(TerminationInfo),
    proc_info_set_termination2_info(TermInfo, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%
%
% Predicates for writing optimization interfaces.
%

:- pred maybe_make_optimization_interface(module_info::in, io::di, io::uo)
    is det.

maybe_make_optimization_interface(ModuleInfo, !IO) :-
    % XXX update this once this analysis supports `--intermodule-analysis'
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    (
        MakeOptInt = yes,
        module_info_get_valid_predids(PredIds, ModuleInfo, _ModuleInfo),
        make_opt_int(PredIds, ModuleInfo, !IO)
    ;
        MakeOptInt = no
    ).

:- pred make_opt_int(list(pred_id)::in, module_info::in, io::di, io::uo)
    is det.

make_opt_int(PredIds, ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, ModuleName, ".opt.tmp",
        do_not_create_dirs, OptFileName, !IO),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(Verbose,
        "% Appending termination2_info pragmas to `", !IO),
    maybe_write_string(Verbose, OptFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        list.foldl(output_pred_termination2_info(ModuleInfo), PredIds, !IO),
        io.set_output_stream(OldStream, _, !IO),
        io.close_output(OptFile, !IO),
        maybe_write_string(Verbose, " done.\n", !IO)
    ;
        OptFileRes = error(IOError),
        maybe_write_string(Verbose, " failed!\n", !IO),
        io.error_message(IOError, IOErrorMessage),
        io.write_strings(["Error opening file `",
            OptFileName, "' for output: ", IOErrorMessage], !IO),
        io.set_exit_status(1, !IO)
    ).

output_pred_termination2_info(ModuleInfo, PredId, !IO) :-
    % Don't try to output termination2_info pragmas unless the analysis
    % was actually run.  Doing otherwise won't work because the necessary
    % information will not have been set up.

    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, termination2, RunningTerm2),
    (
        RunningTerm2 = yes,
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_import_status(PredInfo, ImportStatus),
        module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
        (
            ( ImportStatus = status_exported
            ; ImportStatus = status_opt_exported
            ),
            not hlds_pred.is_unify_or_compare_pred(PredInfo),
            not set.member(PredId, TypeSpecForcePreds)
        ->
            PredName   = pred_info_name(PredInfo),
            pred_info_get_procedures(PredInfo, ProcTable),
            pred_info_get_context(PredInfo, Context),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),
            ModuleName = pred_info_module(PredInfo),
            ProcIds    = pred_info_procids(PredInfo),
            SymName    = qualified(ModuleName, PredName),
            make_opt_int_procs(PredId, ProcIds, ProcTable, PredOrFunc,
                SymName, Context, !IO)
        ;
            true
        )
    ;
        RunningTerm2 = no
    ).

:- pred make_opt_int_procs(pred_id::in, list(proc_id)::in,
    proc_table::in, pred_or_func::in, sym_name::in, prog_context::in,
    io::di, io::uo) is det.

make_opt_int_procs(_PredId, [], _, _, _, _, !IO).
make_opt_int_procs(PredId, [ProcId | ProcIds], ProcTable,
        PredOrFunc, SymName, Context, !IO) :-
    ProcInfo = ProcTable ^ det_elem(ProcId),
    proc_info_get_termination2_info(ProcInfo, TermInfo),
    proc_info_declared_argmodes(ProcInfo, ModeList),
    proc_info_get_headvars(ProcInfo, HeadVars),
    SizeVarMap = TermInfo ^ size_var_map,
    HeadSizeVars = prog_vars_to_size_vars(SizeVarMap, HeadVars),
    output_pragma_termination2_info(PredOrFunc, SymName,
        ModeList, Context, TermInfo ^ success_constrs,
        TermInfo ^ failure_constrs, TermInfo ^ term_status,
        HeadSizeVars, !IO),
    make_opt_int_procs(PredId, ProcIds, ProcTable,
        PredOrFunc, SymName, Context, !IO).

%----------------------------------------------------------------------------%
%
% Predicates for writing 'termination2_info' pragmas.
%

% NOTE: if these predicates are changed then prog_io_pragma.m must
%       also be changed so that it can parse the resulting pragma
%       termination2_info declarations.

output_pragma_termination2_info(PredOrFunc, SymName, ModeList, Context,
        MaybeSuccess, MaybeFailure, MaybeTermination, HeadVars, !IO) :-
    io.write_string(":- pragma termination2_info(", !IO),
    (
        PredOrFunc = pf_predicate,
        mercury_output_pred_mode_subdecl(varset.init, SymName,
            ModeList, no, Context, !IO)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ModeList, FuncModeList, RetMode),
        mercury_output_func_mode_subdecl(varset.init, SymName,
            FuncModeList, RetMode, no, Context, !IO)
    ),

    list.length(HeadVars, NumHeadVars),
    HeadVarIds = 0 .. NumHeadVars - 1,
    map.det_insert_from_corresponding_lists(HeadVars, HeadVarIds,
        map.init, VarToVarIdMap),

    io.write_string(", ", !IO),
    output_maybe_constr_arg_size_info(VarToVarIdMap, MaybeSuccess,
        !IO),
    io.write_string(", ", !IO),
    output_maybe_constr_arg_size_info(VarToVarIdMap, MaybeFailure,
        !IO),
    io.write_string(", ", !IO),
    output_maybe_termination2_info(MaybeTermination, !IO),
    io.write_string(").\n", !IO).

:- pred output_maybe_constr_arg_size_info(map(size_var, int)::in,
    maybe(constr_arg_size_info)::in, io::di, io::uo) is det.

output_maybe_constr_arg_size_info(VarToVarIdMap, MaybeArgSizeConstrs, !IO) :-
    (
        MaybeArgSizeConstrs = no,
        io.write_string("not_set", !IO)
    ;
        MaybeArgSizeConstrs = yes(Polyhedron),
        io.write_string("constraints(", !IO),
        Constraints0 = polyhedron.non_false_constraints(Polyhedron),
        Constraints1 = list.filter(isnt(nonneg_constr), Constraints0),
        Constraints  = list.sort(Constraints1),
        OutputVar = (func(Var) = int_to_string(VarToVarIdMap ^ det_elem(Var))),
        lp_rational.output_constraints(OutputVar, Constraints, !IO),
        io.write_char(')', !IO)
    ).

:- pred output_maybe_termination2_info(maybe(constr_termination_info)::in,
    io::di, io::uo) is det.

output_maybe_termination2_info(MaybeConstrTermInfo, !IO) :-
    (
        MaybeConstrTermInfo = no,
        io.write_string("not_set", !IO)
    ;
        MaybeConstrTermInfo = yes(cannot_loop(_)),
        io.write_string("cannot_loop", !IO)
    ;
        MaybeConstrTermInfo = yes(can_loop(_)),
        io.write_string("can_loop", !IO)
    ).

%----------------------------------------------------------------------------%
%
% Utility predicates.
%

% These test whether various fields in the termination2_info struct have
% been set.

    % Succeeds iff the given procedure has argument size constraints
    % associated with it.
    %
:- pred has_arg_size_info(module_info::in, pred_proc_id::in) is semidet.

has_arg_size_info(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_termination2_info(ProcInfo, TermInfo),
    TermInfo ^ success_constrs = yes(_).

    % Succeeds iff the termination status of the given procedure has been set.
    %
:- pred has_term_info(module_info::in, pred_proc_id::in) is semidet.

has_term_info(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_termination2_info(ProcInfo, TermInfo),
    TermInfo ^ term_status = yes(_).

:- pred proc_needs_ar_built(module_info::in, pred_proc_id::in) is semidet.

proc_needs_ar_built(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_termination2_info(ProcInfo, TermInfo),
    (
        TermInfo ^ success_constrs = no
    ;
        TermInfo ^ term_status = no
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_main.
%-----------------------------------------------------------------------------%
