%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: format_call.m.
% Author: zs.
%
% This module has two related jobs.
%
% - The first job is to generate warnings about calls to string.format,
%   io.format and stream.string_writer.format in which the format string
%   and the supplied lists of values do not agree.
%
%   The difficult part of this job is actually finding the values of the
%   variables representing the format string and the list of values to be
%   printed.
%
% - The second job is to try to transform well formed calls into code
%   that interprets the format string at compile time, rather than runtime.
% 
% Our general approach to the first job is a backwards traversal of the
% procedure body. During this traversal, we assign an id to every conjunction
% (considering a cond and then parts of an if-then-else to be a conjunction).
% When we find a call to a recognized format predicate or function, we remember
% the call site together with the identities of the variables holding the
% format string and the values to be printed, and include both variables
% in the set of variables whose values we want to track. As we traverse
% unifications that bind variables we want to track, we record their value
% in a map specific to the conjunction containing the unification. Actually,
% we keep four such maps. Three record information about bindings to function
% symbols: one for format strings, one for the skeletons of the lists of
% values, and one for the elements of those lists. The fourth map is
% for variable equivalences.
%
% We also record relationships between the conjunctions. Consider the code
% structure below, which contains two relevant conjunctions: the outer one,
% and the one containing the cond and then parts of the inner if-then-else.
% Any attempt to trace the value of V2 requires also knowing the value of V1.
% We therefore record that if you can't find the value of a variable such as V1
% in the inner conjunction, you should continue the search in the outer
% conjunction. We call this relationship "predecessor", since the only relevant
% part of the outer conjunction is the one that appears before the inner one.
% This is enforced by the mode system.
%
%   (
%       ...,
%       V1 = ...,
%       ...,
%       (
%           ...
%       ->
%           V2 = ... V1 ...,
%           string.format(..., V2, ...)
%       ;
%           V3 = ... V1 ...,
%           string.format(..., V3, ...)
%       ),
%       ...
%   )
%
% This design is about as cheap in terms of compilation time as we can make it.
% Its cost has two components. The first component is the traversal, and its
% cost is roughly proportional to the size of the procedure body. The second
% cost is the checking of each call to string.format or io.format. The expected
% complexity of this part is proportional to the number of such calls
% multiplied by the average number of arguments they print. In the worst case,
% this can be multiplied again by the number of conjunctions in the procedure
% body, but I expect that in most cases the variables involved in the relevant
% calls will be found in the same conjunction as the call itself, so the
% typical number of conjunctions that has to be searched will in fact be one.
%
% Note that if the value of e.g. a format string is an input to the procedure
% or is computed by a call rather than a unification, we won't be able to check
% whether the values match the format string. Whether we give a warning in such
% cases is controlled by a separate option, which is consulted in det_report.m.
%
% We could in theory track e.g. format strings through calls to library
% functions such as string.append. However, there is no convenient way to
% evaluate the extent of a need for this capability until this change is
% bootstrapped, so that is left for future work.
%
% The second job (optimizing the calls) starts by gathering the information
% we need during the first pass through the code. If we find that a format_call
% can be executed by string.format on dummy values of the appropriate type
% without throwing an exception, we check to see if the format string is simple
% enough for us to interpret it at compile time. At the moment, it is simple
% enough if it consists of only raw text to be printed and uses of the %d,
% %c and %s specifiers without any flags, width or precision specifications
% or anything like that. If the format string falls into this category,
% then we construct code to replace the call right away.
%
% If there are any such replacements, we perform a second backward traversal of
% the procedure body, looking for the goals to be replaced, which we identity
% by goal_id.
%
% For each call we want to optimize, we also want to delete the code that
% constructs the format string and the lists of poly_types. The first pass
% records the identities of the variables involved, so that we can delete the
% construct unifications that produce them (if they were produced by calls, we
% would not have been able to know at compile time *what* they produce).
% Of course, some of these variables may be used elsewhere, both before and
% after the format call we are optimizing. That is why this second backwards
% traversal passes along two sets of variables: the set of variables we want to
% remove (ToDeleteVars), and the set of variables known to be needed later
% (NeededVars). Construction unifications that create one of the ToDeleteVars
% are deleted, unless the variable is also in NeededVars.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.simplify.format_call.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred is_format_call(module_name::in, string::in, list(prog_var)::in)
    is semidet.

:- pred analyze_and_optimize_format_calls(module_info::in,
    hlds_goal::in, maybe(hlds_goal)::out, list(error_spec)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module char.
:- import_module counter.
:- import_module exception.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module univ.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type format_call_site
    --->    format_call_site(
                fcs_goal_id                 :: goal_id,
                fcs_string_var              :: prog_var,
                fcs_values_var              :: prog_var,
                fcs_call_kind               :: format_call_kind,
                fcs_called_pred_module      :: module_name,
                fcs_called_pred_name        :: string,
                fcs_called_pred_arity       :: arity,
                fcs_call_context            :: prog_context,
                fcs_containing_conj         :: conj_id
            ).

:- type list_skeleton_state
    --->    list_skeleton_nil
    ;       list_skeleton_cons(
                head        :: prog_var,
                tail        :: prog_var
            ).

    % Maps each variable representing a format string to the format string
    % itself.
:- type string_map          == map(prog_var, string).

    % Maps each variable participating in the skeleton of the list of values to
    % be printed to its value.
:- type list_skeleton_map   == map(prog_var, list_skeleton_state).

    % Maps each variable representing a polytype in the list of values to be
    % printed to the variable whose value is to be printed, and a dummy value
    % of the same kind. We don't include the actual value to be printed, since
    % (a) in almost all cases that won't be available statically in the
    % program, and (b) we don't actually need it.
:- type what_to_print
    --->    what_to_print(
                var_to_print        :: prog_var,
                dummy_to_print      :: string.poly_type
            ).
:- type list_element_map    == map(prog_var, what_to_print).

    % Maps each variable defined in terms of another variable to the variable
    % it is assigned from.
:- type fc_eqv_map          == map(prog_var, prog_var).

    % The knowledge we have recorded from assign and construct unifications in
    % a given conjunction.
    %
:- type conj_map
    --->    conj_map(
                cm_string_map           :: string_map,
                cm_list_skeleton_map    :: list_skeleton_map,
                cm_list_element_map     :: list_element_map,
                cm_eqv_map              :: fc_eqv_map
            ).

:- type conj_id
    --->    conj_id(int).

    % Maps the id of each conjunction to the knowledge we have derived from
    % unifications in that conjunction.
    %
:- type conj_maps == map(conj_id, conj_map).

    % Maps each conjunction to its predecessor (if any) in the sense documented
    % above.
    %
:- type conj_pred_map == map(conj_id, conj_id).

    % Records the information about each call site that is not common
    % to all calls to recognized predicates and function.
    %
:- type format_call_kind
    --->    kind_string_format(
                sf_result_var           :: prog_var
            )
    ;       kind_io_format_nostream(
                iofns_io_in_var         :: prog_var,
                iofns_io_out_var        :: prog_var
            )
    ;       kind_io_format_stream(
                iofs_stream_var         :: prog_var,
                iofs_io_in_var          :: prog_var,
                iofs_io_out_var         :: prog_var
            )
    ;       kind_stream_string_writer(
                ssw_tc_info_var         :: prog_var,
                ssw_stream_var          :: prog_var,
                ssw_in_var              :: prog_var,
                ssw_out_var             :: prog_var
            ).

%-----------------------------------------------------------------------------%

is_format_call(ModuleName, Name, Args) :-
    is_format_call_kind_and_vars(ModuleName, Name, Args, _Kind,
        _FormatStringVar, _FormattedValuesVar).

:- pred is_format_call_kind_and_vars(module_name::in, string::in,
    list(prog_var)::in, format_call_kind::out, prog_var::out, prog_var::out)
    is semidet.

is_format_call_kind_and_vars(ModuleName, Name, Args, Kind,
        FormatStringVar, FormattedValuesVar) :-
    Name = "format",
    (
        ModuleName = mercury_string_module
    ->
        % We have these arguments regardless of whether we call the
        % predicate or function version of string.format.
        Args = [FormatStringVar, FormattedValuesVar, ResultVar],
        Kind = kind_string_format(ResultVar)
    ;
        ModuleName = mercury_io_module
    ->
        (
            Args = [FormatStringVar, FormattedValuesVar, IOIn, IOOut],
            Kind = kind_io_format_nostream(IOIn, IOOut)
        ;
            Args = [StreamVar, FormatStringVar, FormattedValuesVar,
                IOIn, IOOut],
            Kind = kind_io_format_stream(StreamVar, IOIn, IOOut)
        )
    ;
        ModuleName = mercury_std_lib_module_name(
            qualified(unqualified("stream"), "string_writer"))
    ->
        % Since we do this check after polymorphism, there will have been
        % a typeclassinfo inserted at the front of the argument list.
        Args = [TC_InfoVarForStream, StreamVar, FormatStringVar,
            FormattedValuesVar, StateInVar, StateOutVar],
        Kind = kind_stream_string_writer(TC_InfoVarForStream, StreamVar,
            StateInVar, StateOutVar)
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

analyze_and_optimize_format_calls(ModuleInfo, Goal0, MaybeGoal, Specs,
        !VarSet, !VarTypes) :-
    map.init(ConjMaps0),
    counter.init(0, Counter0),
    fill_goal_id_slots_in_proc_body(ModuleInfo, !.VarTypes, _, Goal0, Goal1),
    format_call_traverse_goal(ModuleInfo, Goal1, _, [], FormatCallSites,
        Counter0, _Counter, ConjMaps0, ConjMaps, map.init, PredMap,
        set_of_var.init, _),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, optimize_format_calls, OptFormatCalls),
    list.foldl4(
        check_format_call_site(ModuleInfo, OptFormatCalls, ConjMaps, PredMap),
        FormatCallSites, map.init, GoalIdMap, [], Specs, !VarSet, !VarTypes),
    ( map.is_empty(GoalIdMap) ->
        % We have not found anything to improve in Goal1.
        MaybeGoal = no
    ;
        % We want to set NeededVars0 to be the set of the procedure's
        % output arguments, but it is ok to add into it some non-output
        % arguments whose insts happen to change as well.
        Goal1 = hlds_goal(_, GoalInfo1),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo1),
        instmap_delta_changed_vars(InstMapDelta, NeededVars0),
        ToDeleteVars0 = set_of_var.init,
        opt_format_call_sites_in_goal(Goal1, Goal, GoalIdMap, _,
            NeededVars0, _NeededVars, ToDeleteVars0, _ToDeleteVars),
        MaybeGoal = yes(Goal)
    ).

:- pred check_format_call_site(module_info::in, bool::in, conj_maps::in,
    conj_pred_map::in, format_call_site::in,
    fc_goal_id_map::in, fc_goal_id_map::out,
    list(error_spec)::in, list(error_spec)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

check_format_call_site(ModuleInfo, OptFormatCalls, ConjMaps, PredMap,
        FormatCallSite, !GoalIdMap, !Specs, !VarSet, !VarTypes) :-
    FormatCallSite = format_call_site(GoalId, StringVar, ValuesVar, Kind,
        ModuleName, Name, Arity, Context, CurId),
    SymName = qualified(ModuleName, Name),

    (
        follow_format_string(ConjMaps, PredMap, CurId, StringVar,
            MaybeFormatString0),
        MaybeFormatString0 = yes(FormatString0)
    ->
        MaybeFormatStringVar = yes({FormatString0, StringVar})
    ;
        MaybeFormatStringVar = no,
        UnknownFormatPieces = [words("Unknown format string in call to"),
            sym_name_and_arity(SymName / Arity), suffix("."), nl],
        UnknownFormatSeverity =
            severity_conditional(warn_unknown_format_calls, yes,
                severity_warning, no),
        UnknownFormatMsg = simple_msg(Context,
            [option_is_set(warn_unknown_format_calls, yes,
                [always(UnknownFormatPieces)])]),
        UnknownFormatSpec = error_spec(UnknownFormatSeverity,
            phase_detism_check, [UnknownFormatMsg]),
        !:Specs = [UnknownFormatSpec | !.Specs]
    ),

    (
        follow_list_skeleton(ConjMaps, PredMap, CurId, ValuesVar,
            SkeletonResult),
        SkeletonResult = follow_skeleton_result(PolytypeVars0, SkeletonVars0),
        list.map(follow_list_value(ConjMaps, PredMap, CurId), PolytypeVars0,
            WhatToPrintMaybes0),
        project_all_yes(WhatToPrintMaybes0, WhatToPrints0)
    ->
        ToDeleteVars0 = [ValuesVar | SkeletonVars0] ++ PolytypeVars0,
        MaybeSkeletonInfo = yes({ToDeleteVars0, WhatToPrints0})
    ;
        MaybeSkeletonInfo = no,
        UnknownFormatValuesPieces =
            [words("Unknown format values in call to"),
            sym_name_and_arity(SymName / Arity), suffix("."), nl],
        UnknownFormatValuesSeverity =
            severity_conditional(warn_unknown_format_calls, yes,
                severity_warning, no),
        UnknownFormatValuesMsg = simple_msg(Context,
            [option_is_set(warn_unknown_format_calls, yes,
                [always(UnknownFormatValuesPieces)])]),
        UnknownFormatValuesSpec = error_spec(UnknownFormatValuesSeverity,
            phase_detism_check, [UnknownFormatValuesMsg]),
        !:Specs = [UnknownFormatValuesSpec | !.Specs]
    ),

    (
        MaybeFormatStringVar = yes({FormatString, StringVar1}),
        MaybeSkeletonInfo = yes({ValuesToDeleteVars, WhatToPrints})
    ->
        DummiesToPrint = list.map(project_dummy_to_print, WhatToPrints),
        promise_equivalent_solutions [Result] (
            try(string.format(FormatString, DummiesToPrint), Result)
        ),
        (
            Result = exception(ExceptionUniv),
            ( univ_to_type(ExceptionUniv, ExceptionError) ->
                ExceptionError = software_error(ExceptionMsg0),
                ( string.append("string.format: ", Msg, ExceptionMsg0) ->
                    ExceptionMsg = Msg
                ;
                    ExceptionMsg = ExceptionMsg0
                ),
                BadFormatPieces =
                    [words("Mismatched format and values in call to"),
                    sym_name_and_arity(SymName / Arity), suffix(":"), nl,
                    words(ExceptionMsg)],
                BadFormatMsg = simple_msg(Context,
                    [option_is_set(warn_known_bad_format_calls, yes,
                        [always(BadFormatPieces)])]),
                BadFormatSeverity = severity_conditional(
                    warn_known_bad_format_calls, yes, severity_warning, no),
                BadFormatSpec = error_spec(BadFormatSeverity,
                    phase_simplify(report_in_any_mode), [BadFormatMsg]),
                !:Specs = [BadFormatSpec | !.Specs]
            ;
                % We can't decode arbitrary exception values, but string.m
                % shouldn't throw anything but software_errors, so ignoring
                % the exception should be ok.
                true
            )
        ;
            % There is no need for any error message; the format works.
            Result = succeeded(_),
            (
                OptFormatCalls = no
            ;
                OptFormatCalls = yes,
                try_create_replacement_goal(ModuleInfo, GoalId,
                    Kind, FormatString, StringVar1,
                    ValuesToDeleteVars, WhatToPrints,
                    !GoalIdMap, !VarSet, !VarTypes)
            )
        )
    ;
        % Any error message has already been generated, if asked for.
        true
    ).

:- pred try_create_replacement_goal(module_info::in, goal_id::in,
    format_call_kind::in, string::in, prog_var::in,
    list(prog_var)::in, list(what_to_print)::in,
    fc_goal_id_map::in, fc_goal_id_map::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

try_create_replacement_goal(ModuleInfo, GoalId, Kind,
        FormatString, StringVar, ValuesToDeleteVars,
        WhatToPrints, !GoalIdMap, !VarSet, !VarTypes) :-
    string.to_char_list(FormatString, FormatStringChars),
    VarsToPrint = list.map(project_var_to_print, WhatToPrints),
    % Note that every predicate or function that this code generates calls to
    % needs to be listed in simplify_may_introduce_calls, in order to prevent
    % its definition from being thrown away by dead_pred_elim before execution
    % gets here.
    (
        Kind = kind_string_format(ResultVar),
        (
            create_string_format_replacement(ModuleInfo,
                FormatStringChars, ResultVar, VarsToPrint,
                ReplacementGoal, !VarSet, !VarTypes)
        ->
            AllToDeleteVars = [StringVar | ValuesToDeleteVars],
            FCOptGoalInfo = fc_opt_goal_info(ReplacementGoal,
                set_of_var.list_to_set(AllToDeleteVars)),
            map.det_insert(GoalId, FCOptGoalInfo, !GoalIdMap)
        ;
            % create_string_format_replacement does not (yet) recognize
            % all possible format strings. We cannot optimize the ones
            % it cannot recognize.
            true
        )
    ;
        (
            Kind = kind_io_format_nostream(IOInVar, IOOutVar),
            MaybeStreamVar = no
        ;
            Kind = kind_io_format_stream(StreamVar, IOInVar, IOOutVar),
            MaybeStreamVar = yes(StreamVar)
        ),
        (
            create_io_format_replacement(ModuleInfo, FormatStringChars,
                MaybeStreamVar, IOInVar, IOOutVar, VarsToPrint,
                ReplacementGoal, !VarSet, !VarTypes)
        ->
            AllToDeleteVars = [StringVar | ValuesToDeleteVars],
            FCOptGoalInfo = fc_opt_goal_info(ReplacementGoal,
                set_of_var.list_to_set(AllToDeleteVars)),
            map.det_insert(GoalId, FCOptGoalInfo, !GoalIdMap)
        ;
            % create_string_format_replacement does not (yet) recognize
            % all possible format strings. We cannot optimize the ones
            % it cannot recognize.
            true
        )
    ;
        Kind = kind_stream_string_writer(_, _, _, _)
        % XXX Optimize these.
    ).

:- pred follow_format_string(conj_maps::in, conj_pred_map::in, conj_id::in,
    prog_var::in, maybe(string)::out) is det.

follow_format_string(ConjMaps, PredMap, CurId, StringVar, MaybeString) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(StringMap, _, _, EqvMap),
    ( map.search(EqvMap, StringVar, EqvVar) ->
        follow_format_string(ConjMaps, PredMap, CurId, EqvVar, MaybeString)
    ; map.search(StringMap, StringVar, String) ->
        MaybeString = yes(String)
    ; map.search(PredMap, CurId, PredId) ->
        follow_format_string(ConjMaps, PredMap, PredId, StringVar, MaybeString)
    ;
        MaybeString = no
    ).

:- type follow_skeleton_result
    --->    follow_skeleton_result(
                fsr_polytype_vars       :: list(prog_var),
                fsr_skeleton_vars       :: list(prog_var)
            )
    ;       no_follow_skeleton_result.

:- pred follow_list_skeleton(conj_maps::in, conj_pred_map::in, conj_id::in,
    prog_var::in, follow_skeleton_result::out) is det.

follow_list_skeleton(ConjMaps, PredMap, CurId, ListVar, Result) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(_, ListMap, _, EqvMap),
    ( map.search(EqvMap, ListVar, EqvVar) ->
        follow_list_skeleton(ConjMaps, PredMap, CurId, EqvVar, Result)
    ; map.search(ListMap, ListVar, ListState) ->
        (
            ListState = list_skeleton_nil,
            Result = follow_skeleton_result([], [ListVar])
        ;
            ListState = list_skeleton_cons(HeadVar, TailVar),
            follow_list_skeleton(ConjMaps, PredMap, CurId, TailVar,
                TailResult),
            (
                TailResult = no_follow_skeleton_result,
                Result = no_follow_skeleton_result
            ;
                TailResult = follow_skeleton_result(TailPolytypeVars,
                    TailSkeletonVars),
                PolytypeVars = [HeadVar | TailPolytypeVars],
                SkeletonVars = [TailVar | TailSkeletonVars],
                Result = follow_skeleton_result(PolytypeVars,
                    SkeletonVars)
            )
        )
    ; map.search(PredMap, CurId, PredId) ->
        follow_list_skeleton(ConjMaps, PredMap, PredId, ListVar, Result)
    ;
        Result = no_follow_skeleton_result
    ).

:- pred follow_list_value(conj_maps::in, conj_pred_map::in,
    conj_id::in, prog_var::in, maybe(what_to_print)::out) is det.

follow_list_value(ConjMaps, PredMap, CurId, PolytypeVar, MaybeResult) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(_, _, ElementMap, EqvMap),
    ( map.search(EqvMap, PolytypeVar, EqvVar) ->
        follow_list_value(ConjMaps, PredMap, CurId, EqvVar, MaybeResult)
    ; map.search(ElementMap, PolytypeVar, WhatToPrint) ->
        MaybeResult = yes(WhatToPrint)
    ; map.search(PredMap, CurId, PredId) ->
        follow_list_value(ConjMaps, PredMap, PredId, PolytypeVar, MaybeResult)
    ;
        MaybeResult = no
    ).

:- func project_dummy_to_print(what_to_print) = string.poly_type.

project_dummy_to_print(what_to_print(_VarToPrint, DummyToPrint))
    = DummyToPrint.

:- func project_var_to_print(what_to_print) = prog_var.

project_var_to_print(what_to_print(VarToPrint, _DummyToPrint)) = VarToPrint.

:- pred project_all_yes(list(maybe(T))::in, list(T)::out) is semidet.

project_all_yes([], []).
project_all_yes([yes(Value) | TailMaybes], [Value | Tail]) :-
    project_all_yes(TailMaybes, Tail).

%-----------------------------------------------------------------------------%

:- pred format_call_traverse_goal(module_info::in, hlds_goal::in, conj_id::out,
    list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    set_of_progvar::in, set_of_progvar::out) is det.

format_call_traverse_goal(ModuleInfo, Goal, CurId, !FormatCallSites, !Counter,
        !ConjMaps, !PredMap, !RelevantVars) :-
    alloc_id(CurId, !Counter),
    goal_to_conj_list(Goal, GoalConj),
    format_call_traverse_conj(ModuleInfo, GoalConj, CurId, !FormatCallSites,
        !Counter, !ConjMaps, !PredMap, !RelevantVars).

:- pred format_call_traverse_conj(module_info::in, list(hlds_goal)::in,
    conj_id::in, list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    set_of_progvar::in, set_of_progvar::out) is det.

format_call_traverse_conj(_ModuleInfo, [], _CurId, !FormatCallSites, !Counter,
        !ConjMaps, !PredMap, !RelevantVars).
format_call_traverse_conj(ModuleInfo, [Goal | Goals], CurId, !FormatCallSites,
        !Counter, !ConjMaps, !PredMap, !RelevantVars) :-
    format_call_traverse_conj(ModuleInfo, Goals, CurId, !FormatCallSites,
        !Counter, !ConjMaps, !PredMap, !RelevantVars),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = conj(_, Conjuncts),
        format_call_traverse_conj(ModuleInfo, Conjuncts, CurId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars)
    ;
        GoalExpr = disj(Disjuncts),
        format_call_traverse_disj(ModuleInfo, Disjuncts, CurId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars)
    ;
        GoalExpr = switch(_, _, Cases),
        Disjuncts = list.map(project_case_goal, Cases),
        format_call_traverse_disj(ModuleInfo, Disjuncts, CurId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),

        format_call_traverse_goal(ModuleInfo, Else, ElseId, !FormatCallSites,
            !Counter, !ConjMaps, !PredMap, !RelevantVars),
        map.det_insert(ElseId, CurId, !PredMap),

        alloc_id(CondThenId, !Counter),
        goal_to_conj_list(Then, ThenConj),
        goal_to_conj_list(Cond, CondConj),
        format_call_traverse_conj(ModuleInfo, CondConj ++ ThenConj, CondThenId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars),
        map.det_insert(CondThenId, CurId, !PredMap)
    ;
        GoalExpr = negation(SubGoal),
        format_call_traverse_goal(ModuleInfo, SubGoal, SubGoalId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars),
        map.det_insert(SubGoalId, CurId, !PredMap)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = from_ground_term(TermVar, from_ground_term_construct),
            % These scopes cannot build the format string (since that is
            % either a single constant, or the result of an operation on
            % strings, neither of which are things for which we build fgt
            % scopes. It can build the term to print, but that will happen
            % only in degenerate cases. However, we do have some degenerate 
            % cases in the test suite.
            not set_of_var.member(!.RelevantVars, TermVar)
        ->
            % It is ok not to traverse the subgoal. The scope cannot contain
            % any calls, and the unifications it does contain are apparently
            % not of interest to any later formal call.
            true
        ;
            format_call_traverse_conj(ModuleInfo, [SubGoal], CurId,
                !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars)
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = plain_call(PredId, _ProcId, Args, _, _, _),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        (
            is_format_call_kind_and_vars(ModuleName, Name, Args,
                Kind, StringVar, ValuesVar)
        ->
            Arity = pred_info_orig_arity(PredInfo),
            GoalId = goal_info_get_goal_id(GoalInfo),
            Context = goal_info_get_context(GoalInfo),
            FormatCallSite = format_call_site(GoalId, StringVar, ValuesVar,
                Kind, ModuleName, Name, Arity, Context, CurId),
            !:FormatCallSites = [FormatCallSite | !.FormatCallSites],
            set_of_var.insert_list([StringVar, ValuesVar], !RelevantVars)
        ;
            true
        )
    ;
        GoalExpr = unify(_, RHS, _, Unification, _),
        format_call_traverse_unify(Unification, CurId, !ConjMaps, !PredMap,
            !RelevantVars),
        (
            RHS = rhs_lambda_goal(_Purity, _HOGroundness, _PredFunc,
                _EvalMethod, _LambdaNonLocals, _LambdaQuantVars, _LambdaModes,
                _LambdaDetism, LambdaGoal),
            format_call_traverse_goal(ModuleInfo, LambdaGoal, LambdaGoalId,
                !FormatCallSites, !Counter, !ConjMaps, !PredMap,
                !RelevantVars),
            map.det_insert(LambdaGoalId, CurId, !PredMap)
        ;
            ( RHS = rhs_var(_)
            ; RHS = rhs_functor(_, _, _)
            )
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            format_call_traverse_disj(ModuleInfo, [MainGoal | OrElseGoals],
                CurId, !FormatCallSites, !Counter, !ConjMaps, !PredMap,
                !RelevantVars)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            format_call_traverse_goal(ModuleInfo, SubGoal, SubGoalId,
                !FormatCallSites, !Counter, !ConjMaps, !PredMap,
                !RelevantVars),
            map.det_insert(SubGoalId, CurId, !PredMap)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded by now.
            unexpected($module, $pred, "bi_implication")
        )
    ).

:- pred format_call_traverse_unify(unification::in, conj_id::in,
    conj_maps::in, conj_maps::out, conj_pred_map::in, conj_pred_map::out,
    set_of_progvar::in, set_of_progvar::out) is det.

format_call_traverse_unify(Unification, CurId, !ConjMaps, !PredMap,
        !RelevantVars) :-
    (
        Unification = assign(TargetVar, SourceVar),
        ( set_of_var.member(!.RelevantVars, TargetVar) ->
            set_of_var.delete(TargetVar, !RelevantVars),
            set_of_var.insert(SourceVar, !RelevantVars),
            ConjMap0 = get_conj_map(!.ConjMaps, CurId),
            ConjMap0 = conj_map(StringMap, ListMap, ElementMap, EqvMap0),
            map.det_insert(TargetVar, SourceVar, EqvMap0, EqvMap),
            ConjMap = conj_map(StringMap, ListMap, ElementMap, EqvMap),
            map.set(CurId, ConjMap, !ConjMaps)
        ;
            true
        )
    ;
        Unification = construct(CellVar, ConsId, ArgVars, _, _, _, _),
        ( set_of_var.member(!.RelevantVars, CellVar) ->
            ConjMap0 = get_conj_map(!.ConjMaps, CurId),
            ConjMap0 = conj_map(StringMap0, ListMap0, ElementMap0, EqvMap0),
            (
                ConsId = string_const(StringConst)
            ->
                set_of_var.delete(CellVar, !RelevantVars),
                map.det_insert(CellVar, StringConst, StringMap0, StringMap),
                ConjMap = conj_map(StringMap, ListMap0, ElementMap0, EqvMap0)
            ;
                ConsId = cons(SymName, Arity, TypeCtor),
                TypeCtor = list_type_ctor,
                Functor = unqualify_name(SymName),
                (
                    Functor = "[|]",
                    Arity = 2,
                    ArgVars = [ArgVar1, ArgVar2],
                    List = list_skeleton_cons(ArgVar1, ArgVar2)
                ;
                    Functor = "[]",
                    Arity = 0,
                    ArgVars = [],
                    List = list_skeleton_nil
                )
            ->
                set_of_var.delete(CellVar, !RelevantVars),
                set_of_var.insert_list(ArgVars, !RelevantVars),
                map.det_insert(CellVar, List, ListMap0, ListMap),
                ConjMap = conj_map(StringMap0, ListMap, ElementMap0, EqvMap0)
            ;
                ConsId = cons(SymName, Arity, TypeCtor),
                TypeCtor = poly_type_type_ctor,
                Arity = 1,
                Functor = unqualify_name(SymName),
                (
                    Functor = "f",
                    Dummy = f(0.0)
                ;
                    Functor = "i",
                    Dummy = i(0)
                ;
                    Functor = "s",
                    Dummy = s("0")
                ;
                    Functor = "c",
                    Dummy = c('0')
                )
            ->
                set_of_var.delete(CellVar, !RelevantVars),
                ( ArgVars = [ArgVar] ->
                    WhatToPrint = what_to_print(ArgVar, Dummy)
                ;
                    unexpected($module, $pred, "arity mismatch")
                ),
                map.det_insert(CellVar, WhatToPrint, ElementMap0, ElementMap),
                ConjMap = conj_map(StringMap0, ListMap0, ElementMap, EqvMap0)
            ;
                ConjMap = ConjMap0
            ),
            map.set(CurId, ConjMap, !ConjMaps)
        ;
            true
        )
    ;
        ( Unification = deconstruct(_, _, _, _, _, _)
        ; Unification = simple_test(_, _)
        ; Unification = complicated_unify(_, _, _)
        )
    ).

:- func project_case_goal(case) = hlds_goal.

project_case_goal(case(_, _, Goal)) = Goal.

:- pred format_call_traverse_disj(module_info::in, list(hlds_goal)::in,
    conj_id::in, list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    set_of_progvar::in, set_of_progvar::out) is det.

format_call_traverse_disj(ModuleInfo, Disjuncts, CurId, !FormatCallSites,
        !Counter, !ConjMaps, !PredMap, !RelevantVars) :-
    format_call_traverse_disj_arms(ModuleInfo, Disjuncts, CurId,
        DisjFormatCallSitesLists, !Counter, !ConjMaps, !PredMap,
        DisjRelevantVarSets),
    list.condense(DisjFormatCallSitesLists, DisjFormatCallSites),
    !:FormatCallSites = !.FormatCallSites ++ DisjFormatCallSites,
    DisjRelevantVars = set_of_var.union_list(DisjRelevantVarSets),
    set_of_var.union(DisjRelevantVars, !RelevantVars).

:- pred format_call_traverse_disj_arms(module_info::in, list(hlds_goal)::in,
    conj_id::in, list(list(format_call_site))::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    list(set_of_progvar)::out) is det.

format_call_traverse_disj_arms(_, [], _, [], !Counter, !ConjMaps, !PredMap,
        []).
format_call_traverse_disj_arms(ModuleInfo, [Goal | Goals], ContainingId,
        [GoalFormatCallSites | GoalsFormatCallSites], !Counter,
        !ConjMaps, !PredMap, [GoalRelevantVars | GoalsRelevantVars]) :-
    format_call_traverse_goal(ModuleInfo, Goal, DisjId, [],
        GoalFormatCallSites, !Counter, !ConjMaps, !PredMap,
        set_of_var.init, GoalRelevantVars),
    map.det_insert(DisjId, ContainingId, !PredMap),
    format_call_traverse_disj_arms(ModuleInfo, Goals, ContainingId,
        GoalsFormatCallSites, !Counter, !ConjMaps, !PredMap,
        GoalsRelevantVars).

:- func get_conj_map(conj_maps, conj_id) = conj_map.

get_conj_map(ConjMaps, ConjId) = ConjMap :-
    ( map.search(ConjMaps, ConjId, ConjMapPrime) ->
        ConjMap = ConjMapPrime
    ;
        ConjMap = conj_map(map.init, map.init, map.init, map.init)
    ).

:- pred alloc_id(conj_id::out, counter::in, counter::out) is det.

alloc_id(ConjId, !Counter) :-
    counter.allocate(N, !Counter),
    ConjId = conj_id(N).

%-----------------------------------------------------------------------------%

:- type fc_opt_goal_info
    --->    fc_opt_goal_info(
                fcogi_replacement_goal  :: hlds_goal,
                fcogi_unneeded_vars     :: set_of_progvar
            ).

:- type fc_goal_id_map == map(goal_id, fc_opt_goal_info).

    % Traverse the goal, looking for call sites in !.GoalIdMap. If we
    % find them, we replace them with the corresponding goal.
    %
:- pred opt_format_call_sites_in_goal(hlds_goal::in, hlds_goal::out,
    fc_goal_id_map::in, fc_goal_id_map::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

opt_format_call_sites_in_goal(Goal0, Goal, !GoalIdMap,
        !NeededVars, !ToDeleteVars) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        GoalId = goal_info_get_goal_id(GoalInfo),
        ( map.remove(GoalId, OptGoalInfo, !GoalIdMap) ->
            OptGoalInfo = fc_opt_goal_info(ReplacementGoal, GoalToDeleteVars),
            Goal = ReplacementGoal,
            set_of_var.union(!.ToDeleteVars, GoalToDeleteVars, !:ToDeleteVars)
        ;
            Goal = Goal0,
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            % Assume that all nonlocals are needed.
            set_of_var.union(!.NeededVars, NonLocals, !:NeededVars),
            set_of_var.difference(!.ToDeleteVars, NonLocals, !:ToDeleteVars)
        )
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0,
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        % Assume that all nonlocals are needed.
        set_of_var.union(!.NeededVars, NonLocals, !:NeededVars),
        set_of_var.difference(!.ToDeleteVars, NonLocals, !:ToDeleteVars)
    ;
        GoalExpr0 = unify(_LHS, _RHS, _UnifyModes, Unification, _UnifyContext),
        (
            Unification = construct(LHSVar, _ConsId, _RHSVars, _ArgModes, 
                _How, _Unique, _SubInfo),
            not set_of_var.member(!.NeededVars, LHSVar),
            % If this succeeds, then the backward traversal cannot encounter
            % any more producers of LHSVar.
            set_of_var.remove(LHSVar, !ToDeleteVars)
        ->
            % This effectively deletes the unification.
            Goal = true_goal
        ;
            % If _RHS = rhs_lambda_goal, we should optimize any occurrences
            % of format calls inside the lambda goal. Unfortunately,
            % some of the fields of rhs_lambda_goal, specifically the lambda
            % nonlocals, the lambda quantified variables and the lambda modes,
            % can be affected by that optimization, and it is not at all clear
            % how those fields should be updated. Our normal course of action,
            % calling requantify and rebuilding instmap deltas, does not work,
            % because quantification.m generates a compiler abort.

            Goal = Goal0,
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            % Assume that all nonlocals are needed.
            set_of_var.union(!.NeededVars, NonLocals, !:NeededVars),
            set_of_var.difference(!.ToDeleteVars, NonLocals, !:ToDeleteVars)
        )
    ;
        % XXX Check that this works for parallel conjunctions.
        GoalExpr0 = conj(ConjType, Conjuncts0),
        opt_format_call_sites_in_conj(Conjuncts0, Conjuncts,
            !GoalIdMap, !NeededVars, !ToDeleteVars),
        GoalExpr = conj(ConjType, Conjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = disj(Disjuncts0),
        opt_format_call_sites_in_disj(Disjuncts0, Disjuncts, !GoalIdMap,
            !.NeededVars, [], NeededVarsSets,
            !.ToDeleteVars, [], ToDeleteVarsSets),
        !:NeededVars = set_of_var.union_list(NeededVarsSets),
        !:ToDeleteVars = set_of_var.intersect_list(ToDeleteVarsSets),
        GoalExpr = disj(Disjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        opt_format_call_sites_in_switch(Cases0, Cases, !GoalIdMap,
            !.NeededVars, [], NeededVarsSets,
            !.ToDeleteVars, [], ToDeleteVarsSets),
        !:NeededVars = set_of_var.union_list(NeededVarsSets),
        !:ToDeleteVars = set_of_var.intersect_list(ToDeleteVarsSets),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        opt_format_call_sites_in_goal(Else0, Else, !GoalIdMap,
            !.NeededVars, NeededVarsBeforeElse,
            !.ToDeleteVars, ToDeleteVarsBeforeElse),
        opt_format_call_sites_in_goal(Then0, Then, !GoalIdMap,
            !.NeededVars, NeededVarsBeforeThen,
            !.ToDeleteVars, ToDeleteVarsBeforeThen),
        opt_format_call_sites_in_goal(Cond0, Cond, !GoalIdMap,
            NeededVarsBeforeThen, NeededVarsBeforeCond,
            ToDeleteVarsBeforeThen, ToDeleteVarsBeforeCond),
        set_of_var.union(NeededVarsBeforeCond, NeededVarsBeforeElse,
            !:NeededVars),
        set_of_var.intersect(ToDeleteVarsBeforeCond, ToDeleteVarsBeforeElse,
            !:ToDeleteVars),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = negation(SubGoal0),
        % SubGoal0 cannot generate anything in !.ToDeleteVars, but it can add
        % to both !:NeededVars and !:ToDeleteVars.
        opt_format_call_sites_in_goal(SubGoal0, SubGoal,
            !GoalIdMap, !NeededVars, !ToDeleteVars),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            Reason = from_ground_term(TermVar, from_ground_term_construct),
            not set_of_var.member(!.NeededVars, TermVar),
            % If this succeeds, then the backward traversal cannot encounter
            % any more producers of LHSVar.
            set_of_var.remove(TermVar, !ToDeleteVars)
        ->
            % We cannot guarantee that the modified version of SubGoal0
            % meets the invariants required of a goal in a
            % from_ground_term_construct scope, so we remove the scope.
            opt_format_call_sites_in_goal(SubGoal0, Goal,
                !GoalIdMap, !NeededVars, !ToDeleteVars)
        ;
            opt_format_call_sites_in_goal(SubGoal0, SubGoal,
                !GoalIdMap, !NeededVars, !ToDeleteVars),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo)
        )
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(AtomicType, OuterVars, InnerVars,
                OutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            opt_format_call_sites_in_goal(MainGoal0, MainGoal,
                !GoalIdMap, !.NeededVars, NeededVarsMain,
                !.ToDeleteVars, ToDeleteVarsMain),
            opt_format_call_sites_in_disj(OrElseGoals0, OrElseGoals,
                !GoalIdMap, !.NeededVars, [], NeededVarsSets,
                !.ToDeleteVars, [], ToDeleteVarsSets),
            !:NeededVars =
                set_of_var.union_list([NeededVarsMain | NeededVarsSets]),
            !:ToDeleteVars =
                set_of_var.intersect_list(
                    [ToDeleteVarsMain | ToDeleteVarsSets]),
            ShortHand = atomic_goal(AtomicType, OuterVars, InnerVars,
                OutputVars, MainGoal, OrElseGoals, OrElseInners),
            GoalExpr = shorthand(ShortHand)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            opt_format_call_sites_in_goal(SubGoal0, SubGoal,
                !GoalIdMap, !NeededVars, !ToDeleteVars),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
            GoalExpr = shorthand(ShortHand)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded by now.
            unexpected($module, $pred, "bi_implication")
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

:- pred opt_format_call_sites_in_conj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    fc_goal_id_map::in, fc_goal_id_map::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

opt_format_call_sites_in_conj([], [], !GoalIdMap,
        !NeededVars, !ToDeleteVars).
opt_format_call_sites_in_conj([Goal0 | Goals0], [Goal | Goals], !GoalIdMap,
        !NeededVars, !ToDeleteVars) :-
    % We traverse conjunctions backwards.
    opt_format_call_sites_in_conj(Goals0, Goals, !GoalIdMap,
        !NeededVars, !ToDeleteVars),
    opt_format_call_sites_in_goal(Goal0, Goal, !GoalIdMap,
        !NeededVars, !ToDeleteVars).

:- pred opt_format_call_sites_in_disj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    fc_goal_id_map::in, fc_goal_id_map::out,
    set_of_progvar::in, list(set_of_progvar)::in, list(set_of_progvar)::out,
    set_of_progvar::in, list(set_of_progvar)::in, list(set_of_progvar)::out)
    is det.

opt_format_call_sites_in_disj([], [], !GoalIdMap,
        _, !NeededVarsSets, _, !ToDeleteVarsSets).
opt_format_call_sites_in_disj([Goal0 | Goals0], [Goal | Goals], !GoalIdMap,
        NeededVars0, !NeededVarsSets, ToDeleteVars0, !ToDeleteVarsSets) :-
    % The order of traversal does not matter for disjunctions, since the
    % disjuncts are independent. This order is more efficient.
    opt_format_call_sites_in_goal(Goal0, Goal, !GoalIdMap,
        NeededVars0, NeededVars, ToDeleteVars0, ToDeleteVars),
    !:NeededVarsSets = [NeededVars | !.NeededVarsSets],
    !:ToDeleteVarsSets = [ToDeleteVars | !.ToDeleteVarsSets],
    opt_format_call_sites_in_disj(Goals0, Goals, !GoalIdMap,
        NeededVars0, !NeededVarsSets, ToDeleteVars0, !ToDeleteVarsSets).

:- pred opt_format_call_sites_in_switch(list(case)::in, list(case)::out,
    fc_goal_id_map::in, fc_goal_id_map::out,
    set_of_progvar::in, list(set_of_progvar)::in, list(set_of_progvar)::out,
    set_of_progvar::in, list(set_of_progvar)::in, list(set_of_progvar)::out)
    is det.

opt_format_call_sites_in_switch([], [], !GoalIdMap,
        _, !NeededVarsSets, _, !ToDeleteVarsSets).
opt_format_call_sites_in_switch([Case0 | Cases0], [Case | Cases], !GoalIdMap,
        NeededVars0, !NeededVarsSets, ToDeleteVars0, !ToDeleteVarsSets) :-
    % The order of traversal does not matter for switches, since the
    % switch arms are independent. This order is more efficient.
    Case0 = case(FirstConsId, LaterConsIds, Goal0),
    opt_format_call_sites_in_goal(Goal0, Goal, !GoalIdMap,
        NeededVars0, NeededVars, ToDeleteVars0, ToDeleteVars),
    !:NeededVarsSets = [NeededVars | !.NeededVarsSets],
    !:ToDeleteVarsSets = [ToDeleteVars | !.ToDeleteVarsSets],
    Case = case(FirstConsId, LaterConsIds, Goal),
    opt_format_call_sites_in_switch(Cases0, Cases, !GoalIdMap,
        NeededVars0, !NeededVarsSets, ToDeleteVars0, !ToDeleteVarsSets).

%-----------------------------------------------------------------------------%

:- pred create_string_format_replacement(module_info::in, list(char)::in,
    prog_var::in, list(prog_var)::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is semidet.

create_string_format_replacement(ModuleInfo, FormatStringChars, ResultVar,
        VarsToPrint, Goal, !VarSet, !VarTypes) :-
    interpret_format_string(FormatStringChars, [], VarsToPrint, Components),
    replace_string_format(ModuleInfo, Components, yes(ResultVar),
        ActualResultVar, Goals, !VarSet, !VarTypes),
    ( ActualResultVar = ResultVar ->
        AllGoals = Goals
    ;
        make_simple_assign(ResultVar, ActualResultVar, umc_explicit, [],
            AssignGoal),
        AllGoals = Goals ++ [AssignGoal]
    ),
    NonLocals = set_of_var.list_to_set([ResultVar | VarsToPrint]),
    InstMapDelta = instmap_delta_bind_var(ResultVar),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
        term.context_init, GoalInfo),
    conj_list_to_goal(AllGoals, GoalInfo, Goal).

% For optimizing e.g. io.format("%d_%d", [i(X), i(Y), !IO), this diff currently
% generates
% 
%         V1 = int_to_string(X),
%         V2 = "_"
%         V3 = V2 ++ V1
%         V4 = int_to_string(Y),
%         V5 = V4 ++ V3
%         io.write_string(V5, !IO)
% 
% It could instead generate
% 
%         V1 = int_to_string(X),
%         io.write_string(V1, !IO),
%         V2 = "_"
%         io.write_string(V2, !IO),
%         V3 = int_to_string(Y),
%         io.write_string(V3, !IO)
%
% The latter avoid allocating memory for the results of concatenation,
% but those concatenations could be done at compile-time is the values
% of X and Y were known statically. The latter also retrieves the current
% stream more than once, but this could be factored out, and is in any case
% not an issue for io.format/5.
%
% For the time being, we always generate the first form. Later, we could
% try to switch to the second form in cases where this seems profitable.

:- pred create_io_format_replacement(module_info::in, list(char)::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in, list(prog_var)::in,
    hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is semidet.

create_io_format_replacement(ModuleInfo, FormatStringChars,
        MaybeStreamVar, IOInVar, IOOutVar, VarsToPrint, Goal,
        !VarSet, !VarTypes) :-
    interpret_format_string(FormatStringChars, [], VarsToPrint, Components),
    replace_string_format(ModuleInfo, Components, no, ResultVar, Goals,
        !VarSet, !VarTypes),
    (
        MaybeStreamVar = yes(StreamVar),
        ArgVars = [StreamVar, ResultVar, IOInVar, IOOutVar]
    ;
        MaybeStreamVar = no,
        ArgVars = [ResultVar, IOInVar, IOOutVar]
    ),
    InstMapDelta = instmap_delta_from_assoc_list(
        [IOOutVar - ground(unique, none)]),
    generate_simple_call(mercury_io_module, "write_string",
        pf_predicate, only_mode, detism_det, purity_pure, ArgVars, [],
        InstMapDelta, ModuleInfo, term.context_init, CallGoal),

    AllGoals = Goals ++ [CallGoal],
    NonLocals = set_of_var.list_to_set(ArgVars ++ VarsToPrint),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
        term.context_init, GoalInfo),
    conj_list_to_goal(AllGoals, GoalInfo, Goal).

%-----------------------------------------------------------------------------%

:- type string_component
    --->    string_constant(string)
    ;       var_to_print_int(prog_var)
    ;       var_to_print_float(prog_var)
    ;       var_to_print_string(prog_var)
    ;       var_to_print_char(prog_var).

:- pred interpret_format_string(list(char)::in, list(char)::in,
    list(prog_var)::in, list(string_component)::out) is semidet.

interpret_format_string([], RevConstChars, [], Components) :-
    (
        RevConstChars = [],
        Components = []
    ;
        RevConstChars = [_ | _],
        list.reverse(RevConstChars, ConstChars),
        string.from_char_list(ConstChars, ConstString),
        Components = [string_constant(ConstString)]
    ).
interpret_format_string([Char0 | Chars0], !.RevConstChars, Vars0,
        Components) :-
    ( Char0 = '%' ->
        % A valid format string cannot end on an unescaped percent sign.
        Chars0 = [Char1 | Chars1],
        ( Char1 = '%' ->
            % Char0 escapes Char1. Keep Char1, but throw away
            !:RevConstChars = [Char1 | !.RevConstChars],
            interpret_format_string(Chars1, !.RevConstChars, Vars0, Components)
        ;
            Vars0 = [Var0 | Vars1],
            (
                !.RevConstChars = [],
                ConstComponents = []
            ;
                !.RevConstChars = [_ | _],
                list.reverse(!.RevConstChars, ConstChars),
                string.from_char_list(ConstChars, ConstString),
                ConstComponents = [string_constant(ConstString)]
            ),
            (
                Char1 = 'd',
                VarComponent = var_to_print_int(Var0)
            ;
                Char1 = 'f',
                VarComponent = var_to_print_float(Var0),
                % Currently, string.m does not export the predicate it uses
                % by default to format float values. This predicate generates
                % strings of six characters in the absence of an explicit
                % precision specification, so it often pads numbers on the
                % right with zeros, whereas plain old string.float_to_string
                % does no such thing. Under these circumstances, replacing
                % an invocation of string.format with one of float_to_string
                % would change the output.
                fail
            ;
                Char1 = 's',
                VarComponent = var_to_print_string(Var0)
            ;
                Char1 = 'c',
                VarComponent = var_to_print_char(Var0)
            ),
            interpret_format_string(Chars1, [], Vars1, TailComponents),
            Components = ConstComponents ++ [VarComponent | TailComponents]
        )
    ;
        % We do not want to look for Char0 = '\\', because any escape sequences
        % started that way have already been processed. If we did throw away
        % backslashes in favor of the character they supposedly escaped,
        % this would change the output.
        %
        % XXX Do we need to watch out for any escape mechanisms besides
        % percent signs and backslashes?

        !:RevConstChars = [Char0 | !.RevConstChars],
        interpret_format_string(Chars0, !.RevConstChars, Vars0, Components)
    ).

:- pred replace_string_format(module_info::in, list(string_component)::in,
    maybe(prog_var)::in, prog_var::out, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

replace_string_format(ModuleInfo, Components, MaybeResultVar, ResultVar, Goals,
        !VarSet, !VarTypes) :-
    (
        Components = [],
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        make_string_const_construction(ResultVar, "", Goal),
        Goals = [Goal]
    ;
        Components = [FirstComponent | LaterComponents],
        (
            LaterComponents = [],
            represent_component(ModuleInfo, FirstComponent,
                MaybeResultVar, ResultVar, Goals, !VarSet, !VarTypes)
        ;
            LaterComponents = [_ | _],
            replace_string_format(ModuleInfo, LaterComponents,
                no, LaterResultVar, LaterGoals, !VarSet, !VarTypes),
            represent_component(ModuleInfo, FirstComponent,
                no, FirstResultVar, FirstGoals,!VarSet, !VarTypes),
            make_result_var_if_needed(MaybeResultVar, ResultVar,
                !VarSet, !VarTypes),
            generate_simple_call(mercury_string_module, "++", pf_function,
                only_mode, detism_det, purity_pure,
                [FirstResultVar, LaterResultVar, ResultVar], [],
                instmap_delta_from_assoc_list(
                    [ResultVar - ground(unique, none)]),
                ModuleInfo, term.context_init, AppendGoal),
            Goals = LaterGoals ++ FirstGoals ++ [AppendGoal]
        )
    ).

:- pred make_result_var_if_needed(maybe(prog_var)::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_result_var_if_needed(MaybeResultVar, ResultVar, !VarSet, !VarTypes) :-
    (
        MaybeResultVar = yes(ResultVar)
    ;
        MaybeResultVar = no,
        varset.new_var(ResultVar, !VarSet),
        add_var_type(ResultVar, string_type, !VarTypes)
    ).

:- pred represent_component(module_info::in, string_component::in,
    maybe(prog_var)::in, prog_var::out, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

represent_component(ModuleInfo, Component, MaybeResultVar, ResultVar,
        Goals, !VarSet, !VarTypes) :-
    (
        Component = string_constant(StringConstant),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        make_string_const_construction(ResultVar, StringConstant, Goal),
        Goals = [Goal]
    ;
        Component = var_to_print_int(IntVar),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        generate_simple_call(mercury_string_module, "int_to_string",
            pf_function, only_mode, detism_det, purity_pure,
            [IntVar, ResultVar], [],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none)]),
            ModuleInfo, term.context_init, Goal),
        Goals = [Goal]
    ;
        Component = var_to_print_float(FloatVar),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        generate_simple_call(mercury_string_module, "float_to_string",
            pf_function, only_mode, detism_det, purity_pure,
            [FloatVar, ResultVar], [],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none)]),
            ModuleInfo, term.context_init, Goal),
        Goals = [Goal]
    ;
        Component = var_to_print_char(CharVar),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        generate_simple_call(mercury_string_module, "char_to_string",
            pf_function, only_mode, detism_det, purity_pure,
            [CharVar, ResultVar], [],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none)]),
            ModuleInfo, term.context_init, Goal),
        Goals = [Goal]
    ;
        Component = var_to_print_string(StringVar),
        ResultVar = StringVar,
        Goals = []
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.simplify.format_call.
%-----------------------------------------------------------------------------%
