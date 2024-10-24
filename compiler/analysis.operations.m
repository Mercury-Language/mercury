%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2003-2004, 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: analysis.operations.m.
% Main authors: stayl, wangp.
%
%---------------------------------------------------------------------------%

:- module analysis.operations.
:- interface.

:- import_module analysis.framework.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%
%
% These two types are used by both this module and by analysis.file.m.
% Strangely, values of these types do NOT pass between them as arguments
% or results of function or predicate calls.
%

:- type analysis_request
    --->    some [FuncInfo, Call]
            analysis_request(
                req_call        :: Call,
                req_caller      :: module_name
            )
            => call_pattern(FuncInfo, Call).

:- type imdg_arc
    --->    some [FuncInfo, Call]
            imdg_arc(
                % Call pattern of the analysis result being depended on.
                imdg_call       :: Call,

                % The module that _depends on_ this function's result.
                imdg_caller     :: module_name
            )
            => call_pattern(FuncInfo, Call).

%---------------------------------------------------------------------------%

:- type no_func_info
    --->    no_func_info.

    % A call pattern that can be used by analyses that do not need
    % finer granularity.
    %
:- type any_call
    --->    any_call.

:- instance call_pattern(no_func_info, any_call).
:- instance partial_order(no_func_info, any_call).
:- instance to_term(any_call).

%---------------------------------------------------------------------------%

:- type analysis_info.

:- func init_analysis_info(Compiler, module_name, bool) = analysis_info
    <= compiler(Compiler).

:- some [Compiler] func analysis_info_get_compiler(analysis_info) = Compiler
    => compiler(Compiler).

%---------------------------------------------------------------------------%

    % Record the dependency of the module being analysed on the analysis
    % result of another module.
    % Does nothing if not making the analysis registry or if the result
    % that is depended upon comes from a non-local module.
    % Automatically makes a request if the call pattern hasn't been seen
    % before for that function.
    %
:- pred record_dependency(module_name::in, func_id::in, FuncInfo::in,
    Call::in, Answer::unused, analysis_info::in, analysis_info::out) is det
    <= analysis(FuncInfo, Call, Answer).

    % Record an analysis result for a function.
    % Abort if the function is not from the module being analysed.
    % Does nothing if not making the analysis registry.
    %
:- pred record_result(module_name::in, func_id::in, Call::in, Answer::in,
    analysis_status::in, analysis_info::in, analysis_info::out) is det
    <= analysis(FuncInfo, Call, Answer).

    % Record a request from the module being analysed on a function defined
    % in an imported module.
    % Does nothing if not making the analysis registry or if the function is
    % defined in a non-local module.
    %
:- pred record_request(analysis_name::in, module_name::in,
    func_id::in, Call::in, analysis_info::in, analysis_info::out) is det
    <= call_pattern(FuncInfo, Call).

%---------------------------------------------------------------------------%

    % Look up all results for a given function.
    % If the module is `invalid' then the result list will be empty.
    %
    % N.B. Newly recorded results will NOT be found. This is intended
    % for looking up results from _other_ modules.
    %
:- pred lookup_results(analysis_info::in, module_name::in, func_id::in,
    list(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

    % Look up all results for a given function and call pattern CP such
    % that the results have call patterns CP' that are equivalent to CP
    % or less specific than CP.
    %
    % N.B. Newly recorded results will NOT be found. This is intended
    % for looking up results from _other_ modules.
    %
:- pred lookup_matching_results(analysis_info::in, module_name::in,
    func_id::in, FuncInfo::in, Call::in,
    list(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

    % Look up the best result matching a given call.
    %
    % N.B. Newly recorded results will NOT be found. This is intended
    % for looking up results from _other_ modules.
    %
    % If the returned best result has a call pattern that is different
    % from the given call pattern, then it is the analysis writer's
    % responsibility to request a more precise analysis from the called module,
    % using `record_request'.
    %
:- pred lookup_best_result(analysis_info::in, module_name::in, func_id::in,
    FuncInfo::in, Call::in, maybe(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

    % Look up call patterns for all results for a given function.
    % Even if the module is `invalid' the call patterns will be returned.
    %
    % You should use this when you want to know which call patterns were
    % produced for a procedure defined in the current module in previous
    % passes.
    %
:- pred lookup_existing_call_patterns(analysis_info::in, analysis_name::in,
    module_name::in, func_id::in, list(Call)::out) is det
    <= call_pattern(FuncInfo, Call).

    % Lookup all the requests for a given function.
    % Abort if the function is not from the module being analysed.
    %
:- pred lookup_requests(analysis_info::in, analysis_name::in, module_name::in,
    func_id::in, list(Call)::out) is det
    <= call_pattern(FuncInfo, Call).

%---------------------------------------------------------------------------%

    % prepare_intermodule_analysis(ProgressStream, Globals,
    %   ImportedModuleNames, LocalModuleNames, !Info, !IO)
    %
    % This predicate should be called before any pass begins to use the
    % analysis framework. It ensures that all the analysis files
    % are loaded so that lookups can be satisfied. ImportedModuleNames is the
    % set of all modules that are directly or indirectly imported by the
    % module being analysed. LocalModuleNames is the set of non-"library"
    % modules.
    %
:- pred prepare_intermodule_analysis(io.text_output_stream::in, globals::in,
    set(module_name)::in, set(module_name)::in, list(error_spec)::out,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

     % module_is_local(Info, ModuleName, IsLocal).
     %
     % IsLocal is `yes' if the module is not a "library" module, i.e. we are
     % able to reanalyse the module. The set of local modules is set in
     % `prepare_intermodule_analysis'.
    %
:- pred module_is_local(analysis_info::in, module_name::in, bool::out) is det.

    % Should be called after all analysis is completed to write the
    % requests and results for the current compilation to the
    % analysis files.
    %
:- pred write_analysis_files(io.text_output_stream::in, Compiler::in,
    module_info::in, set(module_name)::in, analysis_info::in,
    list(error_spec)::out, io::di, io::uo) is det <= compiler(Compiler).

    % do_read_module_overall_status(Compiler, Globals, ModuleName,
    %   MaybeModuleStatus, !IO)
    %
    % Attempt to read the overall status from a module `.analysis' file.
    %
:- pred do_read_module_overall_status(Compiler::in, globals::in,
    module_name::in, analysis_status::out, io::di, io::uo) is det
    <= compiler(Compiler).

%---------------------------------------------------------------------------%

:- pred get_analysis_debug_stream(maybe(io.text_output_stream)::out,
    io::di, io::uo) is det.
:- pred set_analysis_debug_stream(maybe(io.text_output_stream)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.file.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.    % XXX unwanted dependency

:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module type_desc.
:- import_module univ.

%---------------------------------------------------------------------------%

:- type analysis_info
    --->    some [Compiler]
            analysis_info(
                compiler                :: Compiler,

                % The module being analysed.
                this_module             :: module_name,

                % Whether we are making the analysis registry or just using
                % results for .analysis files.
                make_analysis_registry  :: make_analysis_registry,

                % The set of local modules, i.e. for which we can issue
                % requests.
                local_module_names      :: set(module_name),

                % Holds outstanding requests for more specialised variants
                % of procedures. Requests are added to this map as analyses
                % proceed and written out to disk at the end of the
                % compilation of this module.
                analysis_requests_map   :: analysis_map(analysis_request),

                % The overall status of each module.
                module_status_map       :: map(module_name, analysis_status),

                % The "old" map stores analysis results read in from disk.
                % New results generated while analysing the current module
                % are added to the "new" map. After all the analyses
                % the two maps are compared to see which analysis results
                % have changed. Other modules may need to be marked or
                % invalidated as a result. Then "new" results are moved
                % into the "old" map, from where they can be written to disk.

                old_analysis_results    :: analysis_map(some_analysis_result),
                new_analysis_results    :: module_analysis_map(
                                            some_analysis_result),

                % The Inter-module Dependency Graph records dependencies
                % of an entire module's analysis results on another module's
                % answer patterns. e.g. assume module M1 contains function F1
                % that has an analysis result that used the answer F2:CP2->AP2
                % from module M2. If AP2 changes then all of M1 will either be
                % marked `suboptimal' or `invalid'. Finer-grained dependency
                % tracking would allow only F1 to be recompiled, instead of
                % all of M1, but we don't do that.
                %
                % IMDGs are loaded from disk into the old map. During analysis,
                % any dependences of the current module on other modules
                % are added into the new map. At the end of analysis, all the
                % arcs which terminate at the current module are cleared
                % from the old map, and replaced by those in the new map.
                %
                % XXX: Check if we really need two maps.

                old_imdg_map            :: analysis_map(imdg_arc),
                new_imdg_map            :: analysis_map(imdg_arc)
            )
            => compiler(Compiler).

:- type make_analysis_registry
    --->    make_analysis_registry
    ;       use_analysis_registry_only.

%---------------------------------------------------------------------------%
%
% The "any" call pattern
%

:- instance call_pattern(no_func_info, any_call) where [].
:- instance partial_order(no_func_info, any_call) where [
    ( more_precise_than(_, _, _) :-
        semidet_fail
    ),
    ( equivalent(no_func_info, _, _) :-
        semidet_succeed
    )
].
:- instance to_term(any_call) where [
    ( to_term(any_call) = Term :-
        Term = term.functor(atom("any"), [], dummy_context)
    ),
    ( from_term(Term, any_call) :-
        Term = term.functor(atom("any"), [], _)
    )
].

%---------------------------------------------------------------------------%

init_analysis_info(Compiler, ThisModuleName, MakeAnalysisRegBool) = Info :-
    (
        MakeAnalysisRegBool = yes,
        MakeAnalysisReg = make_analysis_registry
    ;
        MakeAnalysisRegBool = no,
        MakeAnalysisReg = use_analysis_registry_only
    ),
    Info = 'new analysis_info'(Compiler, ThisModuleName, MakeAnalysisReg,
        set.init, map.init, map.init, map.init, map.init, map.init, map.init).

analysis_info_get_compiler(Info) = Info ^ compiler.

%---------------------------------------------------------------------------%

record_dependency(CalleeModuleName, FuncId, FuncInfo, Call, DummyAnswer,
        !Info) :-
    ThisModule = !.Info ^ this_module,
    ( if ThisModule = CalleeModuleName then
        unexpected($pred, "dependency on self")
    else
        true
    ),

    MakeAnalysisReg = !.Info ^ make_analysis_registry,
    module_is_local(!.Info, CalleeModuleName, IsLocal),
    ( if
        MakeAnalysisReg = make_analysis_registry,
        IsLocal = yes
    then
        AnalysisName = analysis_name(Call, DummyAnswer),
        record_dependency_2(ThisModule, AnalysisName, CalleeModuleName, FuncId,
            Call, !Info),

        % If the call pattern that is being depended on hasn't been analysed
        % before, make a request for it.
        lookup_exactly_matching_result_even_from_invalid_modules(!.Info,
            CalleeModuleName, FuncId, FuncInfo, Call, MaybeResult),
        (
            MaybeResult = no,
            record_request(AnalysisName, CalleeModuleName, FuncId, Call, !Info)
        ;
            MaybeResult = yes(Result),
            same_type(Result, analysis_result(Call, DummyAnswer, _))
        )
    else
        true
    ).

:- pred record_dependency_2(module_name::in, analysis_name::in,
    module_name::in, func_id::in, Call::in,
    analysis_info::in, analysis_info::out) is det
    <= call_pattern(FuncInfo, Call).

record_dependency_2(CallerModuleName, AnalysisName, CalleeModuleName, FuncId,
        Call, !Info) :-
    NewIMDGMap0 = !.Info ^ new_imdg_map,
    ( if map.search(NewIMDGMap0, CalleeModuleName, Analyses0) then
        Analyses1 = Analyses0
    else
        Analyses1 = map.init
    ),
    ( if map.search(Analyses1, AnalysisName, Funcs0) then
        Funcs1 = Funcs0
    else
        Funcs1 = map.init
    ),
    ( if map.search(Funcs1, FuncId, FuncArcs0) then
        FuncArcs1 = FuncArcs0
    else
        FuncArcs1 = []
    ),
    Dep = 'new imdg_arc'(Call, CallerModuleName),
    % XXX This should really be a set to begin with.
    ( if list.member(Dep, FuncArcs1) then
        true
    else
        FuncArcs = [Dep | FuncArcs1],
        map.set(FuncId, FuncArcs, Funcs1, Funcs),
        map.set(AnalysisName, Funcs, Analyses1, Analyses),
        map.set(CalleeModuleName, Analyses, NewIMDGMap0, NewIMDGMap),
        !Info ^ new_imdg_map := NewIMDGMap
    ).

:- pred lookup_exactly_matching_result_even_from_invalid_modules(
    analysis_info::in, module_name::in, func_id::in, FuncInfo::in, Call::in,
    maybe(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

lookup_exactly_matching_result_even_from_invalid_modules(Info, ModuleName,
        FuncId, FuncInfo, Call, MaybeResult) :-
    AllowInvalidModules = yes,
    lookup_results_1(Info, ModuleName, FuncId, AllowInvalidModules,
        AllResultsList),
    ResultList = list.filter(
        ( pred(R::in) is semidet :-
            equivalent(FuncInfo, Call, R ^ ar_call)
        ), AllResultsList),
    (
        ResultList = [],
        MaybeResult = no
    ;
        ResultList = [Result],
        MaybeResult = yes(Result)
    ;
        ResultList = [_, _ | _],
        unexpected($pred, "zero or one exactly matching results expected")
    ).

%---------------------%

record_result(ModuleName, FuncId, CallPattern, AnswerPattern, Status, !Info) :-
    ( if ModuleName = !.Info ^ this_module then
        true
    else
        unexpected($pred,
            "recording result for procedure defined in another module")
    ),
    MakeAnalysisReg = !.Info ^ make_analysis_registry,
    (
        MakeAnalysisReg = make_analysis_registry,
        Map0 = !.Info ^ new_analysis_results,
        record_result_in_analysis_map(FuncId, CallPattern, AnswerPattern,
            Status, Map0, Map),
        !Info ^ new_analysis_results := Map
    ;
        MakeAnalysisReg = use_analysis_registry_only
    ).

:- pred record_result_in_analysis_map(func_id::in,
    Call::in, Answer::in, analysis_status::in,
    module_analysis_map(some_analysis_result)::in,
    module_analysis_map(some_analysis_result)::out) is det
    <= analysis(FuncInfo, Call, Answer).

record_result_in_analysis_map(FuncId, CallPattern, AnswerPattern, Status,
        ModuleResults0, ModuleResults) :-
    AnalysisName = analysis_name(CallPattern, AnswerPattern),
    ( if map.search(ModuleResults0, AnalysisName, AnalysisResults0) then
        AnalysisResults1 = AnalysisResults0
    else
        AnalysisResults1 = map.init
    ),
    ( if map.search(AnalysisResults1, FuncId, FuncResults0) then
        FuncResults1 = FuncResults0
    else
        FuncResults1 = []
    ),
    Result = 'new some_analysis_result'(CallPattern, AnswerPattern, Status),
    FuncResults = [Result | FuncResults1],
    ModuleResults =
        map.set(ModuleResults0, AnalysisName,
            map.set(AnalysisResults1, FuncId, FuncResults)).

%---------------------%

record_request(AnalysisName, ModuleName, FuncId, CallPattern, !Info) :-
    ThisModule = !.Info ^ this_module,
    ( if ThisModule = ModuleName then
        unexpected($pred, "request on self")
    else
        true
    ),
    MakeAnalysisReg = !.Info ^ make_analysis_registry,
    module_is_local(!.Info, ModuleName, IsLocal),
    ( if
        MakeAnalysisReg = make_analysis_registry,
        IsLocal = yes
    then
        record_request_2(ThisModule, AnalysisName, ModuleName, FuncId,
            CallPattern, !Info)
    else
        true
    ).

:- pred record_request_2(module_name::in, analysis_name::in, module_name::in,
    func_id::in, Call::in, analysis_info::in, analysis_info::out) is det
    <= call_pattern(FuncInfo, Call).

record_request_2(CallerModule, AnalysisName, ModuleName, FuncId, CallPattern,
        !Info) :-
    RequestsMap0 = !.Info ^ analysis_requests_map,
    ( if map.search(RequestsMap0, ModuleName, ModuleResults0) then
        ModuleResults1 = ModuleResults0
    else
        ModuleResults1 = map.init
    ),
    ( if map.search(ModuleResults1, AnalysisName, AnalysisResults0) then
        AnalysisResults1 = AnalysisResults0
    else
        AnalysisResults1 = map.init
    ),
    ( if map.search(AnalysisResults1, FuncId, FuncResults0) then
        FuncResults1 = FuncResults0
    else
        FuncResults1 = []
    ),
    Request = 'new analysis_request'(CallPattern, CallerModule),
    FuncResults = [Request | FuncResults1],
    map.set(FuncId, FuncResults, AnalysisResults1, AnalysisResults),
    map.set(AnalysisName, AnalysisResults, ModuleResults1, ModuleResults),
    map.set(ModuleName, ModuleResults, RequestsMap0, RequestsMap),
    !Info ^ analysis_requests_map := RequestsMap.

%---------------------------------------------------------------------------%

lookup_results(Info, ModuleName, FuncId, ResultList) :-
    AllowInvalidModules = no,
    lookup_results_1(Info, ModuleName, FuncId, AllowInvalidModules,
        ResultList).

:- pred lookup_results_1(analysis_info::in, module_name::in, func_id::in,
    bool::in, list(analysis_result(Call, Answer))::out) is det
    <= analysis(FuncInfo, Call, Answer).

lookup_results_1(Info, ModuleName, FuncId, AllowInvalidModules, ResultList) :-
    trace [io(!IO)] (
        get_debug_analysis_stream(MaybeDebugStream, !IO),
        (
            MaybeDebugStream = no
        ;
            MaybeDebugStream = yes(DebugStream),
            ModuleNameStr = sym_name_to_string(ModuleName),
            FuncIdStr = string.string(FuncId),
            io.format(DebugStream,
                "%% Looking up analysis results for %s.%s\n",
                [s(ModuleNameStr), s(FuncIdStr)], !IO)
        )
    ),
    ( if
        AllowInvalidModules = no,
        map.search(Info ^ module_status_map, ModuleName, invalid)
    then
        ResultList = []
    else
        lookup_results_2(Info ^ old_analysis_results, ModuleName, FuncId,
            ResultList),
        trace [io(!IO)] (
            get_debug_analysis_stream(MaybeDebugStream, !IO),
            (
                MaybeDebugStream = no
            ;
                MaybeDebugStream = yes(DebugStream),
                io.write_string(DebugStream, "% Found these results: ", !IO),
                io.print_line(DebugStream, ResultList, !IO)
            )
        )
    ).

:- pred lookup_results_2(analysis_map(some_analysis_result)::in,
    module_name::in, func_id::in, list(analysis_result(Call, Answer))::out)
    is det <= analysis(FuncInfo, Call, Answer).

lookup_results_2(Map, ModuleName, FuncId, ResultList) :-
    AnalysisName = analysis_name(_ : Call, _ : Answer),
    ( if
        map.search(Map, ModuleName, ModuleResults),
        map.search(ModuleResults, AnalysisName, AnalysisResults),
        map.search(AnalysisResults, FuncId, Results)
    then
        % XXX we might have to discard results which are
        % `invalid' or `fixpoint_invalid' if they are written at all
        ResultList = list.map(
            ( func(Result) = analysis_result(Call, Answer, Status) :-
                Result = some_analysis_result(Call0, Answer0, Status),
                det_univ_to_type(univ(Call0), Call),
                det_univ_to_type(univ(Answer0), Answer)
            ), Results)
    else
        ResultList = []
    ).

%---------------------%

lookup_matching_results(Info, ModuleName, FuncId, FuncInfo, Call,
        ResultList) :-
    lookup_results(Info, ModuleName, FuncId, AllResultsList),
    ResultList = list.filter(
        ( pred(Result::in) is semidet :-
            ResultCall = Result ^ ar_call,
            ( more_precise_than(FuncInfo, Call, ResultCall)
            ; equivalent(FuncInfo, Call, ResultCall)
            )
        ), AllResultsList).

%---------------------%

lookup_best_result(Info, ModuleName, FuncId, FuncInfo, Call,
        MaybeBestResult) :-
    trace [io(!IO)] (
        get_debug_analysis_stream(MaybeDebugStream, !IO),
        (
            MaybeDebugStream = no
        ;
            MaybeDebugStream = yes(DebugStream),
            ModuleNameStr = sym_name_to_string(ModuleName),
            FuncIdStr = string.string(FuncId),
            io.format(DebugStream,
                "%% Looking up best analysis result for %s.%s\n",
                [s(ModuleNameStr), s(FuncIdStr)], !IO)
        )
    ),
    lookup_matching_results(Info, ModuleName, FuncId, FuncInfo, Call,
        MatchingResults),
    (
        MatchingResults = [],
        MaybeBestResult = no
    ;
        MatchingResults = [Result | Results],
        list.foldl(more_precise_answer(FuncInfo), Results, Result, BestResult),
        MaybeBestResult = yes(BestResult)
    ).

:- pred more_precise_answer(FuncInfo::in,
    analysis_result(Call, Answer)::in, analysis_result(Call, Answer)::in,
    analysis_result(Call, Answer)::out) is det
    <= analysis(FuncInfo, Call, Answer).

more_precise_answer(FuncInfo, Result, Best0, Best) :-
    ResultAnswer = Result ^ ar_answer,
    BestAnswer0 = Best0 ^ ar_answer,
    ( if more_precise_than(FuncInfo, ResultAnswer, BestAnswer0) then
        Best = Result
    else
        Best = Best0
    ).

%---------------------%

lookup_existing_call_patterns(Info, AnalysisName, ModuleName, FuncId, Calls) :-
    ( if ModuleName = Info ^ this_module then
        true
    else
        unexpected($pred, "not this_module")
    ),
    Map = Info ^ old_analysis_results,
    ( if
        map.search(Map, ModuleName, ModuleResults),
        map.search(ModuleResults, AnalysisName, AnalysisResults),
        map.search(AnalysisResults, FuncId, Results)
    then
        Calls = list.map(
            ( func(Result) = Call :-
                Result = some_analysis_result(Call0, _Answer, _Status),
                det_univ_to_type(univ(Call0), Call)
            ), Results)
    else
        Calls = []
    ).

%---------------------%

lookup_requests(Info, AnalysisName, ModuleName, FuncId, CallPatterns) :-
    ( if ModuleName = Info ^ this_module then
        true
    else
        unexpected($pred, "not this_module")
    ),
    ( if
        map.search(Info ^ analysis_requests_map, ModuleName, ModuleRequests),
        map.search(ModuleRequests, AnalysisName, AnalysisRequests),
        map.search(AnalysisRequests, FuncId, CallPatterns0)
    then
        CallPatterns1 = list.filter_map(
            ( func(analysis_request(Call0, _)) = Call is semidet :-
                univ(Call) = univ(Call0)
            ), CallPatterns0),
        % Requests simply get appended to `.request' files so when we read them
        % back in there may be duplicates.
        list.sort_and_remove_dups(CallPatterns1, CallPatterns)
    else
        CallPatterns = []
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % The algorithm is from Nick's thesis, pp. 108-9.
    % Or my corruption thereof.
    % See the `analysis/README' file for a reference.
    %
    % XXX The lack of indentation beyond the first level makes this hard to
    % read: exactly what range of lines does the "we don't do that" comment
    % refer to?
    %
    % For each new analysis result (P^M:DP --> Ans_new):
    %   Read in the registry of M if necessary
    %   If there is an existing analysis result (P^M:DP --> Ans_old):
    %   if Ans_new \= Ans_old:
    %       Replace the entry in the registry with P^M:DP --> Ans_new
    %       if Ans_new `more_precise_than` Ans_old
    %       Status = suboptimal
    %       else
    %       Status = invalid
    %       For each entry (Q^N:DQ --> P^M:DP) in the IMDG:
    %       % Mark Q^N:DQ --> _ (_) with Status
    %       Actually, we don't do that. We only mark the module N's _overall_
    %       status with the least upper bound of its old status and Status.
    %   Else (P:DP --> Ans_old) did not exist:
    %   Insert result (P:DP --> Ans_new) into the registry.
    %
    % Finally, clear out the "new" analysis results map. When we write out
    % the analysis files we will do it from the "old" results map.
    %
    % In a similar way, any new results which satisfy a request cause the
    % module that made the request to be marked suboptimal.
    %
:- pred update_analysis_registry(module_info::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry(ModuleInfo, !Info, !IO) :-
    get_debug_analysis_stream(MaybeDebugStream, !IO),
    (
        MaybeDebugStream = no
    ;
        MaybeDebugStream = yes(DebugStream),
        io.write_string(DebugStream, "% Updating analysis registry.\n", !IO)
    ),
    NewResults = !.Info ^ new_analysis_results,
    update_analysis_registry_2(ModuleInfo, !.Info ^ this_module, NewResults,
        !Info, !IO),
    !Info ^ new_analysis_results := map.init.

:- pred update_analysis_registry_2(module_info::in, module_name::in,
    module_analysis_map(some_analysis_result)::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry_2(ModuleInfo, ModuleName, ModuleMap, !Info, !IO) :-
    map.foldl2(update_analysis_registry_3(ModuleInfo, ModuleName), ModuleMap,
        !Info, !IO).

:- pred update_analysis_registry_3(module_info::in, module_name::in,
    analysis_name::in, func_analysis_map(some_analysis_result)::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry_3(ModuleInfo, ModuleName, AnalysisName, FuncMap,
        !Info, !IO) :-
    map.foldl2(
        update_analysis_registry_4(ModuleInfo, ModuleName, AnalysisName),
        FuncMap, !Info, !IO).

:- pred update_analysis_registry_4(module_info::in, module_name::in,
    analysis_name::in, func_id::in, list(some_analysis_result)::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry_4(ModuleInfo, ModuleName, AnalysisName, FuncId,
        NewResults, !Info, !IO) :-
    % XXX Currently we do not prevent there being more than one recorded result
    % for a given call pattern.
    list.foldl2(
        update_analysis_registry_5(ModuleInfo, ModuleName, AnalysisName,
            FuncId),
        NewResults, !Info, !IO).

:- pred update_analysis_registry_5(module_info::in, module_name::in,
    analysis_name::in, func_id::in, some_analysis_result::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

update_analysis_registry_5(ModuleInfo, ModuleName, AnalysisName, FuncId,
        NewResult, !Info, !IO) :-
    NewResult = some_analysis_result(Call, NewAnswer, NewStatus),
    get_func_info(ModuleInfo, ModuleName, FuncId, Call, NewAnswer, FuncInfo),
    lookup_exactly_matching_result_even_from_invalid_modules(!.Info,
        ModuleName, FuncId, FuncInfo, Call, MaybeResult),
    module_info_get_globals(ModuleInfo, Globals),
    (
        MaybeResult = yes(OldResult),
        % There was a previous answer for this call pattern.

        OldResult = analysis_result(_OldCall, OldAnswer, OldStatus),
        ( if equivalent(FuncInfo, NewAnswer, OldAnswer) then
            get_debug_analysis_stream(MaybeDebugStream, !IO),
            (
                MaybeDebugStream = no
            ;
                MaybeDebugStream = yes(DebugStream),
                write_no_change_in_result(DebugStream, ModuleName, FuncId,
                    Call, NewAnswer, !IO)
            ),
            ( if NewStatus = OldStatus then
                true
            else
                OldMap0 = !.Info ^ old_analysis_results,
                replace_result_in_analysis_map(ModuleName, FuncId, FuncInfo,
                    Call, NewAnswer, NewStatus, OldMap0, OldMap),
                !Info ^ old_analysis_results := OldMap
            )
        else
            % Answer has changed.
            % Replace the old answer in the registry with the new answer.
            OldMap0 = !.Info ^ old_analysis_results,
            replace_result_in_analysis_map(ModuleName, FuncId, FuncInfo,
                Call, NewAnswer, NewStatus, OldMap0, OldMap),
            !Info ^ old_analysis_results := OldMap,

            % If the answer is more precise than before then dependent modules
            % should be marked suboptimal. Otherwise the answer is less precise
            % than it was before, so dependent modules should be invalidated.
            ( if more_precise_than(FuncInfo, NewAnswer, OldAnswer) then
                Status = suboptimal
            else
                Status = invalid
            ),
            map.lookup(!.Info ^ old_imdg_map, ModuleName, OldArcs),
            DepModules = imdg_dependent_modules(OldArcs, AnalysisName,
                FuncId, FuncInfo, Call),
            get_debug_analysis_stream(MaybeDebugStream, !IO),
            (
                MaybeDebugStream = no
            ;
                MaybeDebugStream = yes(DebugStream),
                write_changed_answer(DebugStream, OldAnswer, NewAnswer,
                    Status, DepModules, !IO)
            ),
            set.fold2(taint_module_overall_status(Globals, Status), DepModules,
                !Info, !IO)
        )
    ;
        MaybeResult = no,
        % There was no previous answer for this call pattern.
        % Just add this result to the registry.
        OldAnalysisResults0 = !.Info ^ old_analysis_results,
        map.lookup(OldAnalysisResults0, ModuleName, OldMap0),
        record_result_in_analysis_map(FuncId, Call, NewAnswer, NewStatus,
            OldMap0, OldMap),
        map.det_update(ModuleName, OldMap,
            OldAnalysisResults0, OldAnalysisResults),
        !Info ^ old_analysis_results := OldAnalysisResults
    ),

    % If this new result satisfies a request, then mark the requesting modules
    % as suboptimal so they can be reanalysed.
    %
    % Ideally, we could compare the new answer with either a default answer
    % that the calling module probably used, or each request could optionally
    % record what answer the caller assumed. Then we could avoid reanalysing
    % the calling module unnecessarily. (This only reason we don't implement
    % the former is that the structure reuse analysis doesn't implement
    % the `top' typeclass method.)
    ( if
        map.search(!.Info ^ analysis_requests_map, ModuleName, ModuleRequests),
        map.search(ModuleRequests, AnalysisName, ModuleAnalysisMap),
        map.search(ModuleAnalysisMap, FuncId, Requests),
        Requests = [_ | _]
    then
        Callers0 = list.filter_map(
            ( func(analysis_request(Call0, Caller)) = Caller is semidet :-
                univ(Call0) = univ(Call0)
            ), Requests),
        list.sort_and_remove_dups(Callers0, Callers),
        list.foldl2(taint_module_overall_status(Globals, suboptimal), Callers,
            !Info, !IO)
    else
        true
    ).

    % Replace an analysis result for the given function/call pattern with a
    % new result. A previous result _must_ already exist in the map with
    % exactly the same call pattern.
    %
:- pred replace_result_in_analysis_map(module_name::in, func_id::in,
    FuncInfo::in, Call::in, Answer::in, analysis_status::in,
    analysis_map(some_analysis_result)::in,
    analysis_map(some_analysis_result)::out) is det
    <= analysis(FuncInfo, Call, Answer).

replace_result_in_analysis_map(ModuleName, FuncId, FuncInfo,
        CallPattern, AnswerPattern, Status, Map0, Map) :-
    AnalysisName = analysis_name(CallPattern, AnswerPattern),
    ModuleResults0 = map.lookup(Map0, ModuleName),
    AnalysisResults0 = map.lookup(ModuleResults0, AnalysisName),
    FuncResults0 = map.lookup(AnalysisResults0, FuncId),
    replace_result_in_list(FuncInfo, CallPattern, AnswerPattern, Status,
    FuncResults0, FuncResults),
    Map = map.det_update(Map0, ModuleName,
    map.det_update(ModuleResults0, AnalysisName,
    map.det_update(AnalysisResults0, FuncId, FuncResults))).

:- pred replace_result_in_list(FuncInfo::in, Call::in, Answer::in,
    analysis_status::in,
    list(some_analysis_result)::in, list(some_analysis_result)::out) is det
    <= analysis(FuncInfo, Call, Answer).

replace_result_in_list(FuncInfo, Call, Answer, Status, Results0, Results) :-
    (
        Results0 = [],
        unexpected($pred, "found no result to replace")
    ;
        Results0 = [H0 | T0],
        det_univ_to_type(univ(H0 ^ some_ar_call), HCall),
        ( if equivalent(FuncInfo, Call, HCall) then
            H = 'new some_analysis_result'(Call, Answer, Status),
            T = T0
        else
            H = H0,
            replace_result_in_list(FuncInfo, Call, Answer, Status, T0, T)
        ),
        Results = [H | T]
    ).

:- func imdg_dependent_modules(module_analysis_map(imdg_arc), analysis_name,
    func_id, FuncInfo, Call) = set(module_name)
    <= call_pattern(FuncInfo, Call).

imdg_dependent_modules(ModuleMap, AnalysisName, FuncId, FuncInfo, Call)
        = ModuleNameSet :-
    ( if
        map.search(ModuleMap, AnalysisName, FuncAnalysisMap),
        map.search(FuncAnalysisMap, FuncId, IMDGEntries)
    then
        list.filter_map(arc_module_name(FuncInfo, Call),
            IMDGEntries, ModuleNames),
        set.list_to_set(ModuleNames, ModuleNameSet)
    else
        set.init(ModuleNameSet)
    ).

:- pred arc_module_name(FuncInfo::in, Call::in, imdg_arc::in, module_name::out)
    is semidet <= call_pattern(FuncInfo, Call).

arc_module_name(FuncInfo, CallA, imdg_arc(CallB0, ModuleName), ModuleName) :-
    det_univ_to_type(univ(CallB0), CallB),
    equivalent(FuncInfo, CallA, CallB).

:- pred taint_module_overall_status(globals::in, analysis_status::in,
    module_name::in, analysis_info::in, analysis_info::out,
    io::di, io::uo) is det.

taint_module_overall_status(Globals, Status, ModuleName, !Info, !IO) :-
    (
        Status = optimal
    ;
        ( Status = suboptimal
        ; Status = invalid
        ),

        % We may not have read the overall status for this module yet.
        % Even though we loaded all the analysis files of modules reachable
        % from the initial module beforehand, a _caller_ of the initial module
        % may not be part of that set.
        ensure_module_status_loaded(Globals, ModuleName, !Info, !IO),

        ModuleStatusMap0 = !.Info ^ module_status_map,
        map.lookup(ModuleStatusMap0, ModuleName, ModuleStatus0),
        ModuleStatus = lub(ModuleStatus0, Status),
        get_debug_analysis_stream(MaybeDebugStream, !IO),
        (
            MaybeDebugStream = no
        ;
            MaybeDebugStream = yes(DebugStream),
            write_tainting_module(DebugStream, ModuleName, ModuleStatus, !IO)
        ),
        map.set(ModuleName, ModuleStatus, ModuleStatusMap0, ModuleStatusMap),
        !Info ^ module_status_map := ModuleStatusMap
    ).

:- pred ensure_module_status_loaded(globals::in, module_name::in,
    analysis_info::in, analysis_info::out, io::di, io::uo) is det.

ensure_module_status_loaded(Globals, ModuleName, !Info, !IO) :-
    ModuleStatusMap0 = !.Info ^ module_status_map,
    ( if map.contains(ModuleStatusMap0, ModuleName) then
        true
    else
        do_read_module_overall_status(!.Info ^ compiler, Globals, ModuleName,
            ModuleStatus, !IO),
        map.det_insert(ModuleName, ModuleStatus,
            ModuleStatusMap0, ModuleStatusMap),
        !Info ^ module_status_map := ModuleStatusMap
    ).

:- pred write_no_change_in_result(io.text_output_stream::in,
    module_name::in, func_id::in, Call::in, Answer::in, io::di, io::uo) is det.

write_no_change_in_result(OutStream, ModuleName, FuncId,
        Call, NewAnswer, !IO) :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    FuncIdStr = string.string(FuncId),
    CallStr = string.string(Call),
    NewAnswerStr = string.string(NewAnswer),
    io.format(OutStream, "%% No change in the result %s.%s: %s --> %s\n",
        [s(ModuleNameStr), s(FuncIdStr), s(CallStr), s(NewAnswerStr)], !IO).

:- pred write_changed_answer(io.text_output_stream::in, Answer::in, Answer::in,
    analysis_status::in, set(module_name)::in, io::di, io::uo) is det.

write_changed_answer(OutStream, OldAnswer, NewAnswer, Status,
        DepModules, !IO) :-
    OldAnswerStr = string.string(OldAnswer),
    NewAnswerStr = string.string(NewAnswer),
    StatusStr = string.string(Status),
    DepModulesStr = string.string(DepModules),
    io.format(OutStream, "%% %s changed to %s\n",
        [s(OldAnswerStr), s(NewAnswerStr)], !IO),
    io.format(OutStream, "Mark dependent modules as %s\n",
        [s(StatusStr)], !IO),
    % XXX This will be hard to read.
    io.format(OutStream, "The modules to mark are: %s\n",
        [s(DepModulesStr)], !IO).

:- pred write_tainting_module(io.text_output_stream::in,
    module_name::in, analysis_status::in, io::di, io::uo) is det.

write_tainting_module(OutStream, ModuleName, ModuleStatus, !IO) :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    ModuleStatusStr = string.string(ModuleStatus),
    io.format(OutStream,
        "%% Tainting the overall module status of %s with %s\n",
        [s(ModuleNameStr), s(ModuleStatusStr)], !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % For each module N imported by M:
    %   Delete all entries leading to module M from N's IMDG:
    %   For each P^M:DP in S (call patterns to analyse):
    %       add P^M:DP --> Q^N:DQ to N's IMDG
    %
:- pred update_intermodule_dependencies(module_name::in, set(module_name)::in,
    analysis_info::in, analysis_info::out) is det.

update_intermodule_dependencies(ModuleName, LocalImportedModules, !Info) :-
    set.fold(update_intermodule_dependencies_2(ModuleName),
        LocalImportedModules, !Info).

:- pred update_intermodule_dependencies_2(module_name::in, module_name::in,
    analysis_info::in, analysis_info::out) is det.

update_intermodule_dependencies_2(ModuleName, ImportedModuleName, !Info) :-
    map.lookup(!.Info ^ old_imdg_map, ImportedModuleName, IMDG0),
    trace [io(!IO)] (
        get_debug_analysis_stream(MaybeDebugStream, !IO),
        (
            MaybeDebugStream = no
        ;
            MaybeDebugStream = yes(DebugStream),
            write_clearing_entries(DebugStream,
                ModuleName, ImportedModuleName, !IO)
        )
    ),
    clear_imdg_entries_pointing_at(ModuleName, IMDG0, IMDG1),

    ( if map.search(!.Info ^ new_imdg_map, ImportedModuleName, NewArcs) then
        map.union(combine_func_imdg, IMDG1, NewArcs, IMDG)
    else
        IMDG = IMDG1
    ),
    OldIMDGMap0 = !.Info ^ old_imdg_map,
    NewIMDGMap0 = !.Info ^ new_imdg_map,
    map.set(ImportedModuleName, IMDG, OldIMDGMap0, OldIMDGMap),
    map.delete(ImportedModuleName, NewIMDGMap0, NewIMDGMap),
    !Info ^ old_imdg_map := OldIMDGMap,
    !Info ^ new_imdg_map := NewIMDGMap.

:- pred write_clearing_entries(io.text_output_stream::in,
    module_name::in, module_name::in, io::di, io::uo) is det.

write_clearing_entries(OutStream, ModuleName, ImportedModuleName, !IO) :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    ImportedModuleNameStr = sym_name_to_string(ImportedModuleName),
    io.format(OutStream, "%% Clearing entries involving %s from %s's IMDG.",
        [s(ModuleNameStr), s(ImportedModuleNameStr)], !IO).

:- pred clear_imdg_entries_pointing_at(module_name::in,
    module_analysis_map(imdg_arc)::in,
    module_analysis_map(imdg_arc)::out) is det.

clear_imdg_entries_pointing_at(ModuleName, Map0, Map) :-
    map.map_values_only(clear_imdg_entries_pointing_at_2(ModuleName),
        Map0, Map).

:- pred clear_imdg_entries_pointing_at_2(module_name::in,
    func_analysis_map(imdg_arc)::in,
    func_analysis_map(imdg_arc)::out) is det.

clear_imdg_entries_pointing_at_2(ModuleName, FuncMap0, FuncMap) :-
    map.map_values_only(clear_imdg_entries_pointing_at_3(ModuleName),
        FuncMap0, FuncMap).

:- pred clear_imdg_entries_pointing_at_3(module_name::in,
    list(imdg_arc)::in, list(imdg_arc)::out) is det.

clear_imdg_entries_pointing_at_3(ModuleName, Arcs0, Arcs) :-
    list.filter((pred(Arc::in) is semidet :- Arc ^ imdg_caller \= ModuleName),
        Arcs0, Arcs).

:- pred combine_func_imdg(func_analysis_map(imdg_arc)::in,
    func_analysis_map(imdg_arc)::in, func_analysis_map(imdg_arc)::out) is det.

combine_func_imdg(FuncImdgA, FuncImdgB, FuncImdg) :-
    map.union(combine_imdg_lists, FuncImdgA, FuncImdgB, FuncImdg).

:- pred combine_imdg_lists(list(imdg_arc)::in, list(imdg_arc)::in,
    list(imdg_arc)::out) is det.

combine_imdg_lists(ArcsA, ArcsB, ArcsA ++ ArcsB).

%---------------------------------------------------------------------------%

prepare_intermodule_analysis(ProgressStream, Globals,
        ImportedModuleNames0, LocalModuleNames, Specs, !Info, !IO) :-
    % NOTE The original code of the analysis package threw exceptions
    % when it found version number mismatches or syntax errors in the files
    % it tried to read. There was no recovery from these exceptions,
    % so a single compiler invocation could "report" only one such problem.
    % (If you can call a message immediately preceding a compiler abort
    % a "report".)
    %
    % The current code of this package, when it finds those same problems,
    % just creates an error_spec, adds it to the list, and continues.
    % This can report an unbounded number of errors per compiler invocation,
    % and unlike the old system, it also prints the context of each problem.
    % However, the current system assumes that the problems whose error_specs
    % we gather will not cause later computations to abort (e.g. by causing
    % map.lookups to fail) by breaking what were assumed to be invariants.
    % This is mostly because I (zs) *cannot find* any mention of any such
    % invariants.
    ThisModule = !.Info ^ this_module,
    ImportedModuleNames = set.delete(ImportedModuleNames0, ThisModule),

    !Info ^ local_module_names := LocalModuleNames,

    % Read in results for imported modules.
    set.fold3(load_module_analysis_results(ProgressStream, Globals),
        ImportedModuleNames, !Info, [], Specs0, !IO),

    % Read in results and requests for the module being analysed.
    load_module_analysis_results(ProgressStream, Globals, ThisModule, !Info,
        Specs0, Specs1, !IO),
    read_module_analysis_requests(!.Info, Globals, ThisModule,
        ThisModuleRequests, Specs1, Specs, !IO),
    (
        Specs = [],
        RequestsMap0 = !.Info ^ analysis_requests_map,
        map.set(ThisModule, ThisModuleRequests, RequestsMap0, RequestsMap),
        !Info ^ analysis_requests_map := RequestsMap
    ;
        Specs = [_ | _]
    ).

:- pred load_module_analysis_results(io.text_output_stream::in, globals::in,
    module_name::in, analysis_info::in, analysis_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

load_module_analysis_results(ProgressStream, Globals, ModuleName,
        !Info, !Specs, !IO) :-
    OldResultsMap0 = !.Info ^ old_analysis_results,
    ModuleStatusMap0 = !.Info ^ module_status_map,
    ( if
        ( map.contains(OldResultsMap0, ModuleName)
        ; map.contains(ModuleStatusMap0, ModuleName)
        )
    then
        unexpected($pred, "ensure_old_module_analysis_results_loaded")
    else
        do_read_module_overall_status(!.Info ^ compiler, Globals, ModuleName,
            ModuleStatus, !IO),
        read_module_analysis_results(ProgressStream, !.Info, Globals,
            ModuleName, ModuleResults, ModuleSpecs, !IO),
        (
            ModuleSpecs = [],
            map.det_insert(ModuleName, ModuleStatus,
                ModuleStatusMap0, ModuleStatusMap),
            map.det_insert(ModuleName, ModuleResults,
                OldResultsMap0, OldResultsMap),
            !Info ^ module_status_map := ModuleStatusMap,
            !Info ^ old_analysis_results := OldResultsMap
        ;
            ModuleSpecs = [_ | _],
            !:Specs = ModuleSpecs ++ !.Specs
        )
    ).

module_is_local(Info, ModuleName, IsLocal) :-
    ( if set.contains(Info ^ local_module_names, ModuleName) then
        IsLocal = yes
    else
        IsLocal = no
    ).

%---------------------------------------------------------------------------%

write_analysis_files(ProgressStream, Compiler, ModuleInfo, ImportedModules0,
        !.Info, Specs, !IO) :-
    % In this procedure we have just finished compiling module ModuleName
    % and will write out data currently cached in the analysis_info structure
    % out to disk.
    % XXX Where exactly "on disk"?

    ThisModule = !.Info ^ this_module,
    ImportedModules = set.delete(ImportedModules0, ThisModule),

    LocalModules = !.Info ^ local_module_names,
    set.intersect(LocalModules, ImportedModules, LocalImportedModules),

    % Load IMDG files for local modules.
    module_info_get_globals(ModuleInfo, Globals),
    set.fold3(load_module_imdg(Globals), LocalModules, !Info, [], Specs, !IO),

    (
        Specs = [_ | _]
        % Do not go any further if we had any errors while reading IMDG
        % information. Return Specs to be printed by one of our ancestors.
    ;
        Specs = [],
        update_analysis_registry(ModuleInfo, !Info, !IO),

        % The current module was just compiled, so we set its status to the
        % lub of all the new analysis results generated.
        ModuleStatus = lub_result_statuses(!.Info ^ new_analysis_results),
        ModuleStatusMap0 = !.Info ^ module_status_map,
        map.set(ThisModule, ModuleStatus, ModuleStatusMap0, ModuleStatusMap),
        !Info ^ module_status_map := ModuleStatusMap,

        update_intermodule_dependencies(ThisModule, LocalImportedModules,
            !Info),
        ( if map.is_empty(!.Info ^ new_analysis_results) then
            true
        else
            unexpected($pred, "new_analysis_results is not empty")
        ),

        % Write the module statuses for all local modules (not necessarily
        % imported).
        set.fold(maybe_write_module_overall_status(!.Info, Globals),
            LocalModules, !IO),

        % Write the analysis results for the current module.
        map.lookup(!.Info ^ old_analysis_results, ThisModule, ModuleResults),
        write_module_analysis_results(ProgressStream, !.Info, Globals,
            ThisModule, ModuleResults, !IO),

        % Write the requests for imported local modules.
        set.fold(maybe_write_module_requests(!.Info, Globals),
            LocalImportedModules, !IO),

        % Remove the requests for the current module since we (should have)
        % fulfilled them in this pass.
        empty_request_file(!.Info, Globals, ThisModule, !IO),

        % Write the intermodule dependency graphs.
        set.fold(maybe_write_module_imdg(!.Info, Globals),
            LocalImportedModules, !IO),

        % Touch a timestamp file to indicate the last time that
        % this module was analysed.
        % XXX A grep for this extension shows that the compiler
        % never actually even tries to look at the files we write here.
        ExtDate = ext_cur_ngs_gs(ext_cur_ngs_gs_an_ds_date),
        module_name_to_write_file_name(Compiler, Globals, ExtDate,
            ThisModule, TimestampFileName, !IO),
        touch_file_datestamp(Globals, ProgressStream, TimestampFileName,
            _Succeeded, !IO)
    ).

:- pred load_module_imdg(globals::in, module_name::in,
    analysis_info::in, analysis_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

load_module_imdg(Globals, ModuleName, !Info, !Specs, !IO) :-
    read_module_imdg(!.Info, Globals, ModuleName, IMDG, ModuleSpecs, !IO),
    (
        ModuleSpecs = [],
        OldIMDGMap0 = !.Info ^ old_imdg_map,
        map.det_insert(ModuleName, IMDG, OldIMDGMap0, OldIMDGMap),
        !Info ^ old_imdg_map := OldIMDGMap
    ;
        ModuleSpecs = [_ | _],
        !:Specs = ModuleSpecs ++ !.Specs
    ).

:- pred maybe_write_module_overall_status(analysis_info::in, globals::in,
    module_name::in, io::di, io::uo) is det.

maybe_write_module_overall_status(Info, Globals, ModuleName, !IO) :-
    ( if map.search(Info ^ module_status_map, ModuleName, Status) then
        write_module_overall_status(Info, Globals, ModuleName, Status, !IO)
    else
        % We didn't have any reason to read in the status of this module
        % so we have no reason to touch it either.
        true
    ).

:- pred maybe_write_module_requests(analysis_info::in, globals::in,
    module_name::in, io::di, io::uo) is det.

maybe_write_module_requests(Info, Globals, ModuleName, !IO) :-
    ( if map.search(Info ^ analysis_requests_map, ModuleName, Requests) then
        write_module_analysis_requests(Info, Globals, ModuleName,
            Requests, !IO)
    else
        true
    ).

:- pred maybe_write_module_imdg(analysis_info::in, globals::in,
    module_name::in, io::di, io::uo) is det.

maybe_write_module_imdg(Info, Globals, ModuleName, !IO) :-
    ( if map.search(Info ^ old_imdg_map, ModuleName, ModuleEntries) then
        write_module_imdg(Info, Globals, ModuleName, ModuleEntries, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

do_read_module_overall_status(Compiler, Globals, ModuleName,
        ModuleStatus, !IO) :-
    read_module_overall_status(Compiler, Globals, ModuleName,
        ModuleStatus, !IO).

%---------------------------------------------------------------------------%

:- func lub_result_statuses(module_analysis_map(some_analysis_result))
    = analysis_status.

lub_result_statuses(ModuleMap) =
    map.foldl(lub_result_statuses_2, ModuleMap, optimal).

:- func lub_result_statuses_2(analysis_name,
    func_analysis_map(some_analysis_result), analysis_status) =
    analysis_status.

lub_result_statuses_2(_AnalysisName, FuncMap, Acc) =
    map.foldl(lub_result_statuses_3, FuncMap, Acc).

:- func lub_result_statuses_3(func_id, list(some_analysis_result),
    analysis_status) = analysis_status.

lub_result_statuses_3(_FuncId, Results, Acc) =
    list.foldl(lub_result_statuses_4, Results, Acc).

:- func lub_result_statuses_4(some_analysis_result, analysis_status)
    = analysis_status.

lub_result_statuses_4(Result, Acc) = lub(Result ^ some_ar_status, Acc).

%---------------------------------------------------------------------------%

:- mutable(debug_analysis_stream, maybe(io.text_output_stream), no, ground,
    [untrailed, attach_to_io_state]).

get_analysis_debug_stream(MaybeDebugStream, !IO) :-
    get_debug_analysis_stream(MaybeDebugStream, !IO).

set_analysis_debug_stream(MaybeDebugStream, !IO) :-
    set_debug_analysis_stream(MaybeDebugStream, !IO).

%---------------------------------------------------------------------------%
:- end_module analysis.operations.
%---------------------------------------------------------------------------%
