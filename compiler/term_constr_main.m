%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997,2002-2011 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
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
%   from user annotations like termination pragmas and foreign proc attributes.
%   Also, set up information imported from `.opt' and `.trans_opt' files.
%   (See term_constr_initial.m.)
%
% Each of passes 1 and 2 are run consecutively on each SCC of the
% program call-graph. This is done in bottom-up order.
%
% * Pass 1 - interargument size relationship (IR) analysis.
%
%   (a) Build pass. Convert HLDS to the abstract representation (AR)
%       defined in term_constr_data.m. (See term_constr_build.m.)
%
%   (b) Fixpoint pass. Perform fixpoint calculation to derive IR
%       information. (See term_constr_fixpoint.m.)
%
% * Pass 2 - termination.
%   Use the information from pass 1 to attempt to find a proof that the
%   procedures in a SCC terminate. There is an example of such a proof
%   finder in term_constr_pass2.m, although we will probably end up with
%   several such modules, each of which tries a different approach.
%
% In addition there are some utility procedures in term_constr_util.m
% and some predicates for handling error messages in term_constr_error.m.
%
% The machinery for handling convex constraints (polyhedra) is in:
%
% * polyhedron.m
% * lp_rational.m
% * rat.m
%
% TODO:
%
% * add support for higher-order code and intermodule mutual recursion.
%
% * add support for user-defined IR constraints for foreign code,
%   :- pragma arg_size_relation(...) or something.
%
% * experiment with different types of widening.
%
% * experiment with different representation for the polyhedra.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.term_constr_main.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

%----------------------------------------------------------------------------%

    % Perform termination analysis on a module.
    %
:- pred term2_analyse_module(module_info::in, module_info::out,
    list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data_pragma.
:- import_module transform_hlds.term_constr_build.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_errors.
:- import_module transform_hlds.term_constr_fixpoint.
:- import_module transform_hlds.term_constr_initial.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_constr_pass2.
:- import_module transform_hlds.term_norm.

:- import_module bool.
:- import_module maybe.
:- import_module std_util.
:- import_module set.

%----------------------------------------------------------------------------%
%
% Main pass.
%

term2_analyse_module(!ModuleInfo, Specs) :-
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
    % predicates. Setup information from termination pragmas and foreign_proc
    % attributes.
    term2_preprocess_module(!ModuleInfo),

    % Analyse the module per SCC in bottom-up fashion.
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    SCCs = dependency_info_get_ordering(DepInfo),
    term2_analyse_sccs(BuildOptions, FixpointOptions, Pass2Options, SCCs,
        !ModuleInfo, [], Specs),

    % Write termination2_info pragmas to `.opt' files.
    module_info_get_proc_analysis_kinds(!.ModuleInfo, ProcAnalysisKinds0),
    set.insert(pak_termination2, ProcAnalysisKinds0, ProcAnalysisKinds),
    module_info_set_proc_analysis_kinds(ProcAnalysisKinds, !ModuleInfo).

:- pred term2_analyse_sccs(term_build_options::in, fixpoint_options::in,
    pass2_options::in, list(list(pred_proc_id))::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

term2_analyse_sccs(_BuildOptions, _FixpointOptions, _Pass2Options,
        [], !ModuleInfo, !Specs).
term2_analyse_sccs(BuildOptions, FixpointOptions, Pass2Options,
        [SCC | HigherSCCs], !ModuleInfo, !Specs) :-
    term2_analyse_scc(BuildOptions, FixpointOptions, Pass2Options,
        SCC, HigherSCCs, !ModuleInfo, !Specs),
    term2_analyse_sccs(BuildOptions, FixpointOptions, Pass2Options,
        HigherSCCs, !ModuleInfo, !Specs).

%----------------------------------------------------------------------------%
%
% Analyse a single SCC.
%

    % Analyse a single SCC of the call graph. This will require results
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
    %     in the SCC terminate. (term_constr_pass2.m).
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
:- pred term2_analyse_scc(term_build_options::in, fixpoint_options::in,
    pass2_options::in, list(pred_proc_id)::in, list(list(pred_proc_id))::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

term2_analyse_scc(BuildOpts, FixpointOpts, Pass2Opts, SCC, HigherSCCs,
        !ModuleInfo, !Specs) :-
    % Since all of the passes are potentially optional, we need to initialise
    % the size_var_maps separately. If they are left uninitialised, intermodule
    % optimization will not work.
    NeedsAR = list.filter(proc_needs_ar_built(!.ModuleInfo), SCC),

    % Build the abstract representation for those procedures that require it.
    % The procedures that require it are those that do not already have
    % both argument size information and termination information.
    term_constr_build_abstract_scc(BuildOpts, HigherSCCs, NeedsAR,
        BuildErrors, !ModuleInfo),

    % We only perform the fixpoint computation for those procedures where
    % we can gain meaningful information from it. We do not do it when:
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
                NeedsTerm, !.ModuleInfo, Pass2Result)
        ),

        % Set the termination property for this procedure, and report
        % any errors that we found during pass 2.
        set_termination_info_for_procs(NeedsTerm, Pass2Result, !ModuleInfo),
        (
            Pass2Result = can_loop(Errors),
            maybe_report_term2_errors(!.ModuleInfo, NeedsTerm, Errors, !Specs)
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
    proc_info_get_termination2_info(ProcInfo0, Term2Info0),
    term2_info_set_term_status(yes(TerminationInfo), Term2Info0, Term2Info),
    proc_info_set_termination2_info(Term2Info, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

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
    term2_info_get_success_constrs(TermInfo) = yes(_).

    % Succeeds iff the termination status of the given procedure has been set.
    %
:- pred has_term_info(module_info::in, pred_proc_id::in) is semidet.

has_term_info(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_termination2_info(ProcInfo, TermInfo),
    term2_info_get_term_status(TermInfo) = yes(_).

:- pred proc_needs_ar_built(module_info::in, pred_proc_id::in) is semidet.

proc_needs_ar_built(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_termination2_info(ProcInfo, TermInfo),
    (
        term2_info_get_success_constrs(TermInfo) = no
    ;
        term2_info_get_term_status(TermInfo) = no
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_main.
%-----------------------------------------------------------------------------%
