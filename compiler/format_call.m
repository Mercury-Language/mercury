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
% cost is the checking of each call to string.format, io.format and similar
% predicates. The expected complexity of this part is proportional to the
% number of such calls multiplied by the average number of arguments
% they print. In the worst case, this can be multiplied again by the number
% of conjunctions in the procedure body, but I expect that in most cases
% the variables involved in the relevant calls will be found in the same
% conjunction as the call itself, so the typical number of conjunctions
% that has to be searched will in fact be one.
%
% Note that if the value of e.g. a format string is an input to the procedure
% or is computed by a call rather than a unification, we won't be able to check
% whether the values match the format string. Whether we give a warning in such
% cases is controlled by a separate option, which is consulted in det_report.m.
%
% We could in theory track e.g. format strings through calls to library
% functions such as string.append. However, we have not yet felt the need
% for this in practice.
%
% The second job (optimizing the calls) starts by procesing the information
% gathered by the first pass through the code. For each call site, we
% systematically convert each component of the format string and its associated
% value to be printed (if any) to a string, and then either append the
% resulting strings together (if the original call was to string.format),
% or print the resulting strings as they are produced (if the original call
% was to io.format). We do not yet optimize calls to the predicate
% stream.string_writer.format. For each call site that we could optimize,
% we record its replacement in a map.
%
% If there are any such replacements, we perform a second backward traversal of
% the procedure body, looking for the goals to be replaced (which we identity
% by goal_id), and replace them.
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

:- include_module parse_string_format.

:- import_module check_hlds.simplify.format_call.parse_string_format.
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
:- import_module string.parse_util.
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
    % printed to an abtract representation of that polytype, with the
    % actual value to be printed replaced by the variable that will hold
    % that value at runtime.
    %
    % For example, when we find the unification X = string.poly_type.s(Y),
    % we add to the list_element_map an entry mapping X to apt_s(Y).
:- type list_element_map    == map(prog_var, abstract_poly_type).

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
    % If you modify this code to recognize any previously unrecognized
    % predicates, then you also need to update the call tree of
    % get_implicit_dependencies in module_imports.m. That code tests whether
    % a list of items calls one of these predicates, so that it can record
    % the need to implicitly import the modules that contain the predicates
    % that implement their optimized versions.
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
    FormatCallSite = format_call_site(GoalId, StringVar, ValuesVar, CallKind,
        ModuleName, Name, Arity, Context, CurId),
    SymName = qualified(ModuleName, Name),
    module_info_get_globals(ModuleInfo, Globals),

    (
        follow_format_string(ConjMaps, PredMap, CurId, StringVar,
            MaybeFormatString0),
        MaybeFormatString0 = yes(FormatString0)
    ->
        MaybeFormatString = yes(FormatString0)
    ;
        MaybeFormatString = no,
        globals.lookup_bool_option(Globals, warn_unknown_format_calls,
            WarnUnknownFormatCallsA),
        (
            WarnUnknownFormatCallsA = no
        ;
            WarnUnknownFormatCallsA = yes,
            UnknownFormatPieces = [words("Unknown format string in call to"),
                sym_name_and_arity(SymName / Arity), suffix("."), nl],
            UnknownFormatMsg = simple_msg(Context,
                [always(UnknownFormatPieces)]),
            UnknownFormatSpec = error_spec(severity_warning,
                phase_simplify(report_in_any_mode), [UnknownFormatMsg]),
            !:Specs = [UnknownFormatSpec | !.Specs]
        )
    ),

    (
        follow_list_skeleton(ConjMaps, PredMap, CurId, ValuesVar,
            SkeletonResult),
        SkeletonResult = follow_skeleton_result(PolytypeVars0, SkeletonVars0),
        list.map(follow_list_value(ConjMaps, PredMap, CurId), PolytypeVars0,
            MaybeAbstractPolyTypes0),
        project_all_yes(MaybeAbstractPolyTypes0, AbstractPolyTypes0)
    ->
        ToDeleteVars0 =
            [StringVar, ValuesVar | SkeletonVars0] ++ PolytypeVars0,
        MaybeSkeletonInfo = yes({ToDeleteVars0, AbstractPolyTypes0})
    ;
        MaybeSkeletonInfo = no,
        globals.lookup_bool_option(Globals, warn_unknown_format_calls,
            WarnUnknownFormatCallsB),
        (
            WarnUnknownFormatCallsB = no
        ;
            WarnUnknownFormatCallsB = yes,
            UnknownFormatValuesPieces =
                [words("Unknown format values in call to"),
                sym_name_and_arity(SymName / Arity), suffix("."), nl],
            UnknownFormatValuesMsg = simple_msg(Context,
                [always(UnknownFormatValuesPieces)]),
            UnknownFormatValuesSpec = error_spec(severity_warning,
                phase_simplify(report_in_any_mode), [UnknownFormatValuesMsg]),
            !:Specs = [UnknownFormatValuesSpec | !.Specs]
        )
    ),

    (
        MaybeFormatString = yes(FormatString),
        MaybeSkeletonInfo = yes({ToDeleteVars, AbstractPolyTypes})
    ->
        string.to_char_list(FormatString, FormatStringChars),
        parse_and_flatten_format_string(FormatStringChars, AbstractPolyTypes,
            MaybeComponents),
        (
            MaybeComponents = error(HeadError - TailErrors),
            globals.lookup_bool_option(Globals, warn_known_bad_format_calls,
                WarnKnownBadFormatCalls),
            (
                WarnKnownBadFormatCalls = no
            ;
                WarnKnownBadFormatCalls = yes,
                PrefixPieces = [words("Mismatched format and values"),
                    words("in call to"),
                    sym_name_and_arity(SymName / Arity), suffix(":"), nl],
                globals.lookup_bool_option(Globals,
                    warn_only_one_format_string_error,
                    WarnOnlyOneFormatStringError),
                (
                    WarnOnlyOneFormatStringError = yes,
                    ErrorPieces = [string_format_error_to_words(HeadError)]
                ;
                    WarnOnlyOneFormatStringError = no,
                    ErrorPieces = [string_format_error_to_words(HeadError) |
                        list.map(string_format_error_to_words, TailErrors)]
                ),

                BadFormatMsg = simple_msg(Context,
                    [always(PrefixPieces), always(ErrorPieces)]),
                BadFormatSpec = error_spec(severity_warning,
                    phase_simplify(report_in_any_mode), [BadFormatMsg]),
                !:Specs = [BadFormatSpec | !.Specs]
            )
        ;
            MaybeComponents = ok(Components),
            (
                OptFormatCalls = no
            ;
                OptFormatCalls = yes,
                create_replacement_goal(ModuleInfo, GoalId, CallKind,
                    Components, ToDeleteVars, !GoalIdMap, !VarSet, !VarTypes)
            )
        )
    ;
        % Any error message has already been generated, if asked for.
        true
    ).

:- func string_format_error_to_words(string_format_error) = format_component.

string_format_error_to_words(Error) =
    words(string_format_error_to_msg(Error)).

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
    --->    no_follow_skeleton_result
    ;       follow_skeleton_result(
                fsr_polytype_vars       :: list(prog_var),
                fsr_skeleton_vars       :: list(prog_var)
            ).

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
                Result = follow_skeleton_result(PolytypeVars, SkeletonVars)
            )
        )
    ; map.search(PredMap, CurId, PredId) ->
        follow_list_skeleton(ConjMaps, PredMap, PredId, ListVar, Result)
    ;
        Result = no_follow_skeleton_result
    ).

:- pred follow_list_value(conj_maps::in, conj_pred_map::in,
    conj_id::in, prog_var::in, maybe(abstract_poly_type)::out) is det.

follow_list_value(ConjMaps, PredMap, CurId, PolytypeVar,
        MaybeAbstractPolyType) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(_, _, ElementMap, EqvMap),
    ( map.search(EqvMap, PolytypeVar, EqvVar) ->
        follow_list_value(ConjMaps, PredMap, CurId, EqvVar,
            MaybeAbstractPolyType)
    ; map.search(ElementMap, PolytypeVar, AbstractPolyType) ->
        MaybeAbstractPolyType = yes(AbstractPolyType)
    ; map.search(PredMap, CurId, PredId) ->
        follow_list_value(ConjMaps, PredMap, PredId, PolytypeVar,
            MaybeAbstractPolyType)
    ;
        MaybeAbstractPolyType = no
    ).

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
            set_of_var.member(!.RelevantVars, TermVar)
        ->
            format_call_traverse_conj(ModuleInfo, [SubGoal], CurId,
                !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars)
        ;
            % It is ok not to traverse the subgoal. The scope cannot contain
            % any calls, and the unifications it does contain are apparently
            % not of interest to any later format call.
            true
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
                expect(unify(ArgVars, []), $module, $pred,
                    "string constant with args"),
                set_of_var.delete(CellVar, !RelevantVars),
                map.det_insert(CellVar, StringConst, StringMap0, StringMap),
                ConjMap = conj_map(StringMap, ListMap0, ElementMap0, EqvMap0)
            ;
                ConsId = cons(SymName, Arity, TypeCtor),
                TypeCtor = list_type_ctor
            ->
                Functor = unqualify_name(SymName),
                (
                    (
                        Functor = "[|]",
                        Arity = 2,
                        ArgVars = [ArgVar1, ArgVar2],
                        ListPrime = list_skeleton_cons(ArgVar1, ArgVar2)
                    ;
                        Functor = "[]",
                        Arity = 0,
                        ArgVars = [],
                        ListPrime = list_skeleton_nil
                    )
                ->
                    List = ListPrime
                ;
                    unexpected($module, $pred, "unexpected list functor")
                ),
                set_of_var.delete(CellVar, !RelevantVars),
                set_of_var.insert_list(ArgVars, !RelevantVars),
                map.det_insert(CellVar, List, ListMap0, ListMap),
                ConjMap = conj_map(StringMap0, ListMap, ElementMap0, EqvMap0)
            ;
                ConsId = cons(SymName, Arity, TypeCtor),
                TypeCtor = poly_type_type_ctor
            ->
                (
                    Arity = 1,
                    ArgVars = [ArgVar]
                ->
                    Functor = unqualify_name(SymName),
                    (
                        (
                            Functor = "f",
                            VarPolyTypePrime = apt_f(ArgVar)
                        ;
                            Functor = "i",
                            VarPolyTypePrime = apt_i(ArgVar)
                        ;
                            Functor = "s",
                            VarPolyTypePrime = apt_s(ArgVar)
                        ;
                            Functor = "c",
                            VarPolyTypePrime = apt_c(ArgVar)
                        )
                    ->
                        VarPolyType = VarPolyTypePrime
                    ;
                        unexpected($module, $pred,
                            "unexpected poly_type functor")
                    )
                ;
                    unexpected($module, $pred, "poly_type arity mismatch")
                ),
                set_of_var.delete(CellVar, !RelevantVars),
                map.det_insert(CellVar, VarPolyType, ElementMap0, ElementMap),
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

:- pred create_replacement_goal(module_info::in, goal_id::in,
    format_call_kind::in, list(flat_component)::in,
    list(prog_var)::in, fc_goal_id_map::in, fc_goal_id_map::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

create_replacement_goal(ModuleInfo, GoalId, CallKind, Components,
        ToDeleteVars, !GoalIdMap, !VarSet, !VarTypes) :-
    % Note that every predicate or function that this code generates calls to
    % must be listed in simplify_may_introduce_calls, in order to prevent
    % its definition from being thrown away by dead_pred_elim before execution
    % gets here.
    (
        CallKind = kind_string_format(ResultVar),
        create_string_format_replacement(ModuleInfo, Components, ResultVar,
            ReplacementGoal, !VarSet, !VarTypes),
        FCOptGoalInfo = fc_opt_goal_info(ReplacementGoal,
            set_of_var.list_to_set(ToDeleteVars)),
        map.det_insert(GoalId, FCOptGoalInfo, !GoalIdMap)
    ;
        (
            CallKind = kind_io_format_nostream(IOInVar, IOOutVar),
            MaybeStreamVar = no
        ;
            CallKind = kind_io_format_stream(StreamVar, IOInVar, IOOutVar),
            MaybeStreamVar = yes(StreamVar)
        ),
        create_io_format_replacement(ModuleInfo, Components,
            MaybeStreamVar, IOInVar, IOOutVar, ReplacementGoal,
            !VarSet, !VarTypes),
        FCOptGoalInfo = fc_opt_goal_info(ReplacementGoal,
            set_of_var.list_to_set(ToDeleteVars)),
        map.det_insert(GoalId, FCOptGoalInfo, !GoalIdMap)
    ;
        CallKind = kind_stream_string_writer(_, _, _, _)
        % XXX Optimize these.
    ).

%-----------------------------------------------------------------------------%

    % For optimizing e.g. string.format("%3d_%.5x", [i(X), i(Y)], Result),
    % generate code that looks like this:
    %
    %   ... set up Flags1 ...
    %   Prec2 = 5,
    %   format_signed_int_component_nowidth_prec(Flags1, Prec2, Y, Str3),
    %   Str4 = "_",
    %   Str5 = Str5 ++ Str4,
    %   ... set up Flags6 ...
    %   Width7 = 3,
    %   Base8 = base_hex_lc,
    %   format_unsigned_int_component_width_noprec(Flags5, Width7, Base8, X,
    %       Str9),
    %   Result = Str9 ++ Str5
    %
    % We build the string back to front to minimize the amount of
    % re-re-recopying that the calls to append (++) have to do.
    % Since we execute the appends back-to-front, we create their
    % arguments back-to-front as well. This way, each component's
    % variable is used immediately after it is constructed, so
    % it shouldn't need to be stored on the stack.
    %
:- pred create_string_format_replacement(module_info::in,
    list(flat_component)::in, prog_var::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

create_string_format_replacement(ModuleInfo, Components, ResultVar, Goal,
        !VarSet, !VarTypes) :-
    replace_string_format(ModuleInfo, Components, yes(ResultVar),
        ActualResultVar, Goals, ValueVars, !VarSet, !VarTypes),
    ( ActualResultVar = ResultVar ->
        AllGoals = Goals
    ;
        % Since replace_string_format can always put the result
        % in the desired variable, this code point is never actually reached.
        % This code is here just in case that ever changes.
        make_simple_assign(ResultVar, ActualResultVar,
            umc_implicit("replace_string_format"), [], AssignGoal),
        AllGoals = Goals ++ [AssignGoal]
    ),
    set_of_var.insert(ResultVar, ValueVars, NonLocals),
    InstMapDelta = instmap_delta_bind_var(ResultVar),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
        term.context_init, GoalInfo),
    conj_list_to_goal(AllGoals, GoalInfo, Goal).

:- pred replace_string_format(module_info::in, list(flat_component)::in,
    maybe(prog_var)::in, prog_var::out, list(hlds_goal)::out,
    set_of_progvar::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

replace_string_format(ModuleInfo, Components, MaybeResultVar, ResultVar, Goals,
        !:ValueVars, !VarSet, !VarTypes) :-
    set_of_var.init(!:ValueVars),
    (
        Components = [],
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        make_string_const_construction(ResultVar, "", Goal),
        Goals = [Goal]
    ;
        Components = [HeadComponent | TailComponents],
        replace_string_format_nonempty(ModuleInfo,
            HeadComponent, TailComponents, MaybeResultVar, ResultVar, Goals,
            !ValueVars, !VarSet, !VarTypes)
    ).

:- pred replace_string_format_nonempty(module_info::in,
    flat_component::in, list(flat_component)::in,
    maybe(prog_var)::in, prog_var::out, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

replace_string_format_nonempty(ModuleInfo, HeadComponent, TailComponents,
        MaybeResultVar, ResultVar, Goals, !ValueVars, !VarSet, !VarTypes) :-
    (
        TailComponents = [],
        represent_component(ModuleInfo, HeadComponent, MaybeResultVar,
            ResultVar, Goals, !ValueVars, !VarSet, !VarTypes)
    ;
        TailComponents = [FirstTailComponent | LaterTailComponents],
        replace_string_format_nonempty(ModuleInfo,
            FirstTailComponent, LaterTailComponents,
            no, TailComponentsVar, TailComponentsGoals,
            !ValueVars, !VarSet, !VarTypes),
        represent_component(ModuleInfo, HeadComponent,
            no, HeadComponentVar, HeadComponentGoals,
            !ValueVars, !VarSet, !VarTypes),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        generate_simple_call(mercury_string_module, "++", pf_function,
            only_mode, detism_det, purity_pure,
            [HeadComponentVar, TailComponentsVar, ResultVar], [],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none)]),
            ModuleInfo, term.context_init, AppendGoal),
        Goals = TailComponentsGoals ++ HeadComponentGoals ++ [AppendGoal]
    ).

%-----------------------------------------------------------------------------%

    % For optimizing e.g. io.format(Stream, "%3d_%.5x", [i(X), i(Y)], IO0, IO),
    % generate code that looks like this:
    %
    %   ... set up Flags1 ...
    %   Width2 = 3,
    %   Base3 = base_hex_lc,
    %   format_unsigned_int_component_width_noprec(Flags1, Width2, Base3, X,
    %       Str4),
    %   io.write_string(Stream, Str4, IO0, IO5),
    %   Str5 = "_",
    %   io.write_string(Stream, Str5, IO5, IO6),
    %   ... set up Flags7 ...
    %   Prec8 = 5,
    %   format_signed_int_component_nowidth_prec(Flags1, Prec2, Y, Str9),
    %   io.write_string(Stream, Str9, IO5, IO),
    %
    % We convert the components in the original order, and print each string
    % resulting from converting a component as soon as it is ready.
    % These strings should therefore never need to be stored in stack slots.
    %
    % If the original call was to io.format/4 instead of io.format/5,
    % i.e. if the stream to be printed on was implicit, then the calls
    % to io.write_string that we generate will also have the stream to be
    % printed on implicit. The runtime system will retrieve the current
    % output stream in each of those calls to io.write_string/3. We could
    % test to see whether factoring out this hidden repetition would be
    % worthwhile.
    %
:- pred create_io_format_replacement(module_info::in, list(flat_component)::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

create_io_format_replacement(ModuleInfo, Components,
        MaybeStreamVar, IOInVar, IOOutVar, Goal, !VarSet, !VarTypes) :-
    replace_io_format(ModuleInfo, Components, MaybeStreamVar,
        IOInVar, IOOutVar, Goals, ValueVars, !VarSet, !VarTypes),

    Uniq = ground(unique, none),
    Clobbered = ground(clobbered, none),
    InstMapDelta = instmap_delta_from_assoc_list(
        [IOInVar - Clobbered, IOOutVar - Uniq]),
    set_of_var.insert_list([IOInVar, IOOutVar], ValueVars, NonLocals),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
        term.context_init, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred replace_io_format(module_info::in, list(flat_component)::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in, list(hlds_goal)::out,
    set_of_progvar::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

replace_io_format(ModuleInfo, Components, MaybeStreamVar, IOInVar, IOOutVar,
        Goals, !:ValueVars, !VarSet, !VarTypes) :-
    set_of_var.init(!:ValueVars),
    (
        Components = [],
        Unification = assign(IOOutVar, IOInVar),
        Uniq = ground(unique, none),
        Clobbered = ground(clobbered, none),
        UniMode = ((free -> Uniq) - (Uniq -> Clobbered)),
        UnifyMainContext = umc_implicit("replace_io_format"),
        UnifyContext = unify_context(UnifyMainContext, []),
        GoalExpr = unify(IOOutVar, rhs_var(IOInVar), UniMode, Unification,
            UnifyContext),
        InstMapDelta = instmap_delta_from_assoc_list(
            [IOInVar - Clobbered, IOOutVar - Uniq]),
        goal_info_init(set_of_var.list_to_set([IOInVar, IOOutVar]),
            InstMapDelta, detism_det, purity_pure, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        Goals = [Goal]
    ;
        Components = [HeadComponent | TailComponents],
        replace_io_format_nonempty(ModuleInfo, HeadComponent, TailComponents,
            MaybeStreamVar, IOInVar, IOOutVar, Goals,
            !ValueVars, !VarSet, !VarTypes)
    ).

:- pred replace_io_format_nonempty(module_info::in,
    flat_component::in, list(flat_component)::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

replace_io_format_nonempty(ModuleInfo, HeadComponent, TailComponents,
        MaybeStreamVar, IOInVar, IOOutVar, Goals,
        !ValueVars, !VarSet, !VarTypes) :-
    (
        TailComponents = [],
        replace_one_io_format(ModuleInfo, HeadComponent,
            MaybeStreamVar, IOInVar, IOOutVar, Goals,
            !ValueVars, !VarSet, !VarTypes)
    ;
        TailComponents = [FirstTailComponent | LaterTailComponents],
        varset.new_var(IOMidVar, !VarSet),
        add_var_type(IOMidVar, io_state_type, !VarTypes),
        replace_one_io_format(ModuleInfo, HeadComponent,
            MaybeStreamVar, IOInVar, IOMidVar, HeadComponentGoals,
            !ValueVars, !VarSet, !VarTypes),
        replace_io_format_nonempty(ModuleInfo,
            FirstTailComponent, LaterTailComponents,
            MaybeStreamVar, IOMidVar, IOOutVar, TailComponentsGoals,
            !ValueVars, !VarSet, !VarTypes),
        Goals = HeadComponentGoals ++ TailComponentsGoals
    ).

:- pred replace_one_io_format(module_info::in, flat_component::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

replace_one_io_format(ModuleInfo, Component, MaybeStreamVar,
        IOInVar, IOOutVar, Goals, !ValueVars, !VarSet, !VarTypes) :-
    represent_component(ModuleInfo, Component,
        no, ComponentVar, ComponentGoals, !ValueVars, !VarSet, !VarTypes),
    (
        MaybeStreamVar = yes(StreamVar),
        ArgVars = [StreamVar, ComponentVar, IOInVar, IOOutVar]
    ;
        MaybeStreamVar = no,
        ArgVars = [ComponentVar, IOInVar, IOOutVar]
    ),
    Uniq = ground(unique, none),
    Clobbered = ground(clobbered, none),
    InstMapDelta = instmap_delta_from_assoc_list(
        [IOInVar - Clobbered, IOOutVar - Uniq]),
    generate_simple_call(mercury_io_module, "write_string",
        pf_predicate, only_mode, detism_det, purity_pure, ArgVars, [],
        InstMapDelta, ModuleInfo, term.context_init, CallGoal),
    Goals = ComponentGoals ++ [CallGoal].

%-----------------------------------------------------------------------------%

:- pred represent_component(module_info::in, flat_component::in,
    maybe(prog_var)::in, prog_var::out, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

represent_component(ModuleInfo, Component, MaybeResultVar, ResultVar,
        Goals, !ValueVars, !VarSet, !VarTypes) :-
    (
        Component = flat_string_const(StringConstant),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        make_string_const_construction(ResultVar, StringConstant, Goal),
        Goals = [Goal]
    ;
        Component = flat_format_char(Flags, MaybeWidth, ValueVar),
        set_of_var.insert(ValueVar, !ValueVars),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        build_flags_arg(Flags, FlagsVar, FlagsGoals, !VarSet, !VarTypes),
        maybe_build_width_arg(MaybeWidth, WidthSuffix, WidthVars, WidthGoals,
            !VarSet, !VarTypes),
        generate_simple_call(mercury_string_format_module,
            "format_char_component" ++ WidthSuffix,
            pf_predicate, only_mode, detism_det, purity_pure,
            [FlagsVar] ++ WidthVars ++ [ValueVar, ResultVar], [],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none)]),
            ModuleInfo, term.context_init, CallGoal),
        Goals = FlagsGoals ++ WidthGoals ++ [CallGoal]
    ;
        Component = flat_format_string(Flags, MaybeWidth, MaybePrec, ValueVar),
        set_of_var.insert(ValueVar, !ValueVars),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        build_flags_arg(Flags, FlagsVar, FlagsGoals, !VarSet, !VarTypes),
        maybe_build_width_arg(MaybeWidth, WidthSuffix, WidthVars, WidthGoals,
            !VarSet, !VarTypes),
        maybe_build_prec_arg(MaybePrec, PrecSuffix, PrecVars, PrecGoals,
            !VarSet, !VarTypes),
        generate_simple_call(mercury_string_format_module,
            "format_string_component" ++ WidthSuffix ++ PrecSuffix,
            pf_predicate, only_mode, detism_det, purity_pure,
            [FlagsVar] ++ WidthVars ++ PrecVars ++ [ValueVar, ResultVar], [],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none)]),
            ModuleInfo, term.context_init, CallGoal),
        Goals = FlagsGoals ++ WidthGoals ++ PrecGoals ++ [CallGoal]
    ;
        Component = flat_format_signed_int(Flags, MaybeWidth, MaybePrec,
            ValueVar),
        set_of_var.insert(ValueVar, !ValueVars),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        build_flags_arg(Flags, FlagsVar, FlagsGoals, !VarSet, !VarTypes),
        maybe_build_width_arg(MaybeWidth, WidthSuffix, WidthVars, WidthGoals,
            !VarSet, !VarTypes),
        maybe_build_prec_arg(MaybePrec, PrecSuffix, PrecVars, PrecGoals,
            !VarSet, !VarTypes),
        generate_simple_call(mercury_string_format_module,
            "format_signed_int_component" ++ WidthSuffix ++ PrecSuffix,
            pf_predicate, only_mode, detism_det, purity_pure,
            [FlagsVar] ++ WidthVars ++ PrecVars ++ [ValueVar, ResultVar], [],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none)]),
            ModuleInfo, term.context_init, CallGoal),
        Goals = FlagsGoals ++ WidthGoals ++ PrecGoals ++ [CallGoal]
    ;
        Component = flat_format_unsigned_int(Flags, MaybeWidth, MaybePrec,
            Base, ValueVar),
        set_of_var.insert(ValueVar, !ValueVars),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        build_flags_arg(Flags, FlagsVar, FlagsGoals, !VarSet, !VarTypes),
        maybe_build_width_arg(MaybeWidth, WidthSuffix, WidthVars, WidthGoals,
            !VarSet, !VarTypes),
        maybe_build_prec_arg(MaybePrec, PrecSuffix, PrecVars, PrecGoals,
            !VarSet, !VarTypes),
        build_int_base_arg(Base, BaseVar, BaseGoal, !VarSet, !VarTypes),
        generate_simple_call(mercury_string_format_module,
            "format_unsigned_int_component" ++ WidthSuffix ++ PrecSuffix,
            pf_predicate, only_mode, detism_det, purity_pure,
            [FlagsVar] ++ WidthVars ++ PrecVars ++
                [BaseVar, ValueVar, ResultVar], [],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none)]),
            ModuleInfo, term.context_init, CallGoal),
        Goals = FlagsGoals ++ WidthGoals ++ PrecGoals ++ [BaseGoal, CallGoal]
    ;
        Component = flat_format_float(Flags, MaybeWidth, MaybePrec,
            Kind, ValueVar),
        set_of_var.insert(ValueVar, !ValueVars),
        make_result_var_if_needed(MaybeResultVar, ResultVar,
            !VarSet, !VarTypes),
        build_flags_arg(Flags, FlagsVar, FlagsGoals, !VarSet, !VarTypes),
        maybe_build_width_arg(MaybeWidth, WidthSuffix, WidthVars, WidthGoals,
            !VarSet, !VarTypes),
        maybe_build_prec_arg(MaybePrec, PrecSuffix, PrecVars, PrecGoals,
            !VarSet, !VarTypes),
        build_float_kind_arg(Kind, KindVar, KindGoal, !VarSet, !VarTypes),
        generate_simple_call(mercury_string_format_module,
            "format_float_component" ++ WidthSuffix ++ PrecSuffix,
            pf_predicate, only_mode, detism_det, purity_pure,
            [FlagsVar] ++ WidthVars ++ PrecVars ++
                [KindVar, ValueVar, ResultVar], [],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none)]),
            ModuleInfo, term.context_init, CallGoal),
        Goals = FlagsGoals ++ WidthGoals ++ PrecGoals ++ [KindGoal, CallGoal]
    ).

    % This predicate generates code of the form
    %
    %   VarHash  = flag_hash_clear,
    %   VarSpace = flag_hash_set,
    %   VarZero  = flag_hash_clear,
    %   VarMinus = flag_hash_clear,
    %   VarPlus  = flag_hash_clear,
    %   Flags = string_format_flags(VarHash, VarSpace, VarZero, VarMinus,
    %       VarPlus)
    %
    % While this looke like a lot, it should actually compile down into
    % a single machine instruction, due to the packing of enums inside
    % structures.
    %
:- pred build_flags_arg(string_format_flags::in, prog_var::out,
    list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

build_flags_arg(Flags, Var, Goals, !VarSet, !VarTypes) :-
    Flags = string_format_flags(FlagHash, FlagSpace, FlagZero, 
        FlagMinus, FlagPlus),
    varset.new_var(VarHash,  !VarSet),
    varset.new_var(VarSpace, !VarSet),
    varset.new_var(VarZero,  !VarSet),
    varset.new_var(VarMinus, !VarSet),
    varset.new_var(VarPlus,  !VarSet),
    ParseUtil = mercury_string_parse_util_module,
    TypeSymNameHash  = qualified(ParseUtil, "string_format_flag_hash"),
    TypeSymNameSpace = qualified(ParseUtil, "string_format_flag_space"),
    TypeSymNameZero  = qualified(ParseUtil, "string_format_flag_zero"),
    TypeSymNameMinus = qualified(ParseUtil, "string_format_flag_minus"),
    TypeSymNamePlus  = qualified(ParseUtil, "string_format_flag_plus"),
    TypeCtorHash  = type_ctor(TypeSymNameHash,  0),
    TypeCtorSpace = type_ctor(TypeSymNameSpace, 0),
    TypeCtorZero  = type_ctor(TypeSymNameZero,  0),
    TypeCtorMinus = type_ctor(TypeSymNameMinus, 0),
    TypeCtorPlus  = type_ctor(TypeSymNamePlus,  0),
    TypeHash  = defined_type(TypeSymNameHash,  [], kind_star),
    TypeSpace = defined_type(TypeSymNameSpace, [], kind_star),
    TypeZero  = defined_type(TypeSymNameZero,  [], kind_star),
    TypeMinus = defined_type(TypeSymNameMinus, [], kind_star),
    TypePlus  = defined_type(TypeSymNamePlus,  [], kind_star),
    add_var_type(VarHash,  TypeHash,  !VarTypes),
    add_var_type(VarSpace, TypeSpace, !VarTypes),
    add_var_type(VarZero,  TypeZero,  !VarTypes),
    add_var_type(VarMinus, TypeMinus, !VarTypes),
    add_var_type(VarPlus,  TypePlus,  !VarTypes),
    (
        FlagHash = flag_hash_clear,
        ConsNameHash = "flag_hash_clear"
    ;
        FlagHash = flag_hash_set,
        ConsNameHash = "flag_hash_set"
    ),
    (
        FlagSpace = flag_space_clear,
        ConsNameSpace = "flag_space_clear"
    ;
        FlagSpace = flag_space_set,
        ConsNameSpace = "flag_space_set"
    ),
    (
        FlagZero = flag_zero_clear,
        ConsNameZero = "flag_zero_clear"
    ;
        FlagZero = flag_zero_set,
        ConsNameZero = "flag_zero_set"
    ),
    (
        FlagMinus = flag_minus_clear,
        ConsNameMinus = "flag_minus_clear"
    ;
        FlagMinus = flag_minus_set,
        ConsNameMinus = "flag_minus_set"
    ),
    (
        FlagPlus = flag_plus_clear,
        ConsNamePlus = "flag_plus_clear"
    ;
        FlagPlus = flag_plus_set,
        ConsNamePlus = "flag_plus_set"
    ),
    ConsIdHash  = cons(qualified(ParseUtil, ConsNameHash),  0, TypeCtorHash),
    ConsIdSpace = cons(qualified(ParseUtil, ConsNameSpace), 0, TypeCtorSpace),
    ConsIdZero  = cons(qualified(ParseUtil, ConsNameZero),  0, TypeCtorZero),
    ConsIdMinus = cons(qualified(ParseUtil, ConsNameMinus), 0, TypeCtorMinus),
    ConsIdPlus  = cons(qualified(ParseUtil, ConsNamePlus),  0, TypeCtorPlus),
    make_const_construction(VarHash,  ConsIdHash,  GoalHash),
    make_const_construction(VarSpace, ConsIdSpace, GoalSpace),
    make_const_construction(VarZero,  ConsIdZero,  GoalZero),
    make_const_construction(VarMinus, ConsIdMinus, GoalMinus),
    make_const_construction(VarPlus,  ConsIdPlus,  GoalPlus),

    TypeNameCombine = qualified(ParseUtil, "string_format_flags"),
    TypeCombine = defined_type(TypeNameCombine, [], kind_star),
    varset.new_var(Var, !VarSet),
    add_var_type(Var, TypeCombine, !VarTypes),

    TypeCtorCombine = type_ctor(TypeNameCombine, 0),
    ConsSymNameCombine = qualified(ParseUtil, "string_format_flags"),
    ConsIdCombine = cons(ConsSymNameCombine, 5, TypeCtorCombine),
    ComponentVars = [VarHash, VarSpace, VarZero, VarMinus, VarPlus],
    construct_functor(Var, ConsIdCombine, ComponentVars, GoalCombine),

    Goals = [GoalHash, GoalSpace, GoalZero, GoalMinus, GoalPlus, GoalCombine].

    % Decide whether we have a specified width.
    %
    % If yes, return both the variable that represents the specified width
    % and the goals that construct it.
    %
:- pred maybe_build_width_arg(compiler_format_maybe_width::in,
    string::out, list(prog_var)::out, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

maybe_build_width_arg(MaybeWidth, PredNameSuffix, MaybeWidthVar,
        MaybeWidthGoals, !VarSet, !VarTypes) :-
    (
        MaybeWidth = compiler_no_specified_width,
        PredNameSuffix = "_nowidth",
        MaybeWidthVar = [],
        MaybeWidthGoals = []
    ;
        MaybeWidth = compiler_manifest_width(WidthInt),
        PredNameSuffix = "_width",
        make_int_const_construction_alloc(WidthInt, no, WidthGoal, WidthVar,
            !VarSet, !VarTypes),
        MaybeWidthVar = [WidthVar],
        MaybeWidthGoals = [WidthGoal]
    ;
        MaybeWidth = compiler_var_width(WidthVar),
        PredNameSuffix = "_width",
        MaybeWidthVar = [WidthVar],
        MaybeWidthGoals = []
    ).

    % Decide whether we have a specified precision.
    %
    % If yes, return both the variable that represents the specified precision
    % and the goals that construct it.
    %
:- pred maybe_build_prec_arg(compiler_format_maybe_prec::in,
    string::out, list(prog_var)::out, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

maybe_build_prec_arg(MaybePrec, PredNameSuffix, MaybePrecVar,
        MaybePrecGoals, !VarSet, !VarTypes) :-
    (
        MaybePrec = compiler_no_specified_prec,
        PredNameSuffix = "_noprec",
        MaybePrecVar = [],
        MaybePrecGoals = []
    ;
        MaybePrec = compiler_manifest_prec(PrecInt),
        PredNameSuffix = "_prec",
        make_int_const_construction_alloc(PrecInt, no, PrecGoal, PrecVar,
            !VarSet, !VarTypes),
        MaybePrecVar = [PrecVar],
        MaybePrecGoals = [PrecGoal]
    ;
        MaybePrec = compiler_var_prec(PrecVar),
        PredNameSuffix = "_prec",
        MaybePrecVar = [PrecVar],
        MaybePrecGoals = []
    ).

:- pred build_int_base_arg(string_format_int_base::in,
    prog_var::out, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

build_int_base_arg(Base, Var, Goal, !VarSet, !VarTypes) :-
    ParseUtil = mercury_string_parse_util_module,
    TypeName = qualified(ParseUtil, "string_format_int_base"),
    TypeCtor = type_ctor(TypeName, 0),
    Type = defined_type(TypeName, [], kind_star),
    (
        Base = base_octal,
        ConsName = "base_octal"
    ;
        Base = base_decimal,
        ConsName = "base_decimal"
    ;
        Base = base_hex_lc,
        ConsName = "base_hex_lc"
    ;
        Base = base_hex_uc,
        ConsName = "base_hex_uc"
    ;
        Base = base_hex_p,
        ConsName = "base_hex_p"
    ),
    ConsId = cons(qualified(ParseUtil, ConsName), 0, TypeCtor),
    make_const_construction_alloc(ConsId, Type, no, Goal, Var,
        !VarSet, !VarTypes).

:- pred build_float_kind_arg(string_format_float_kind::in,
    prog_var::out, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

build_float_kind_arg(Kind, Var, Goal, !VarSet, !VarTypes) :-
    ParseUtil = mercury_string_parse_util_module,
    TypeName = qualified(ParseUtil, "string_format_float_kind"),
    TypeCtor = type_ctor(TypeName, 0),
    Type = defined_type(TypeName, [], kind_star),
    (
        Kind = kind_e_scientific_lc,
        ConsName = "kind_e_scientific_lc"
    ;
        Kind = kind_e_scientific_uc,
        ConsName = "kind_e_scientific_uc"
    ;
        Kind = kind_f_plain_lc,
        ConsName = "kind_f_plain_lc"
    ;
        Kind = kind_f_plain_uc,
        ConsName = "kind_f_plain_uc"
    ;
        Kind = kind_g_flexible_lc,
        ConsName = "kind_g_flexible_lc"
    ;
        Kind = kind_g_flexible_uc,
        ConsName = "kind_g_flexible_uc"
    ),
    ConsId = cons(qualified(ParseUtil, ConsName), 0, TypeCtor),
    make_const_construction_alloc(ConsId, Type, no, Goal, Var,
        !VarSet, !VarTypes).

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

%-----------------------------------------------------------------------------%
:- end_module check_hlds.simplify.format_call.
%-----------------------------------------------------------------------------%
