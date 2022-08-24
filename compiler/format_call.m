%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% Copyright (C) 2015-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
% We therefore record that if you can't find the value of a variable such as
% V1 in the inner conjunction, you should continue the search in the outer
% conjunction. We call this relationship "predecessor", since the only
% relevant part of the outer conjunction is the one that appears before
% the inner one. This is enforced by the mode system.
%
%   (
%       ...,
%       V1 = ...,
%       ...,
%       ( if
%           ...
%       then
%           V2 = ... V1 ...,
%           string.format(..., V2, ...)
%       else
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
% Note that if the value of e.g. a format string is an input to the procedure,
% we won't be able to check whether the values match the format string.
% Whether we give a warning in such cases is controlled by a separate option,
% which is consulted in det_report.m.
%
% Likewise, we cannot do our checks if the value of a format string
% is computed by a call to an unknown procedure rather than a unification
% or one of a small number of known procedures (such as string.append).
% The same option governs whether we give a warning in this case as well.
%
% The second job (optimizing the calls) starts by processing the information
% gathered by the first pass through the code. For each call site, we
% systematically convert each component of the format string and its
% associated value to be printed (if any) to a string, and then either append
% the resulting strings together (if the original call was to string.format),
% or print the resulting strings as they are produced (if the original call
% was to io.format). We optimize calls to stream.string_writer.format by
% constructing the string to be written the same way as we do for
% string.format, and then printing the result. For each call site that
% we could optimize, we record its replacement in a map.
%
% If there are any such replacements, we perform a second backward traversal
% of the procedure body, looking for the goals to be replaced (which we
% identity by goal_id), and replace them.
%
% For each call we want to optimize, we also want to delete the code that
% constructs the format string and the lists of poly_types. The first pass
% records the identities of the variables involved, so that we can delete the
% construct unifications that produce them (if they were produced by calls,
% we would not have been able to know at compile time *what* they produce).
% Of course, some of these variables may be used elsewhere, both before and
% after the format call we are optimizing. That is why this second backwards
% traversal passes along two sets of variables: the set of variables we want
% to remove (ToDeleteVars), and the set of variables known to be needed later
% (NeededVars). Construction unifications that create one of the ToDeleteVars
% are deleted, unless the variable is also in NeededVars.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.format_call.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type maybe_generate_implicit_stream_warnings
    --->    do_not_generate_implicit_stream_warnings
    ;       generate_implicit_stream_warnings.

:- pred is_format_call(module_name::in, string::in, list(prog_var)::in)
    is semidet.

:- pred analyze_and_optimize_format_calls(module_info::in,
    maybe_generate_implicit_stream_warnings::in,
    hlds_goal::in, maybe(hlds_goal)::out, list(error_spec)::out,
    var_table::in, var_table::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- include_module parse_string_format.

:- import_module check_hlds.simplify.format_call.parse_string_format.
:- import_module check_hlds.simplify.simplify_goal_call.
:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.

:- import_module bool.
:- import_module counter.
:- import_module getopt.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.
:- import_module string.parse_util.
:- import_module term.

%---------------------------------------------------------------------------%

:- type format_call_site
    --->    format_call_site(
                fcs_goal_id                 :: goal_id,
                fcs_goal_info               :: hlds_goal_info,
                fcs_string_var              :: prog_var,
                fcs_values_var              :: prog_var,
                fcs_call_kind               :: format_call_kind,
                fcs_called_pred_id          :: pred_id,
                fcs_called_pred_module      :: module_name,
                fcs_called_pred_name        :: string,
                fcs_called_pred_arity       :: arity,
                fcs_call_context            :: prog_context,
                fcs_containing_conj         :: conj_id,
                fcs_warn_unknown_format     :: maybe_warn_unknown_format
            ).

:- type maybe_warn_unknown_format
    --->    do_not_warn_unknown_format
    ;       warn_unknown_format.

%---------------------------------------------------------------------------%

:- type conj_id
    --->    conj_id(int).

    % Maps each conjunction to its predecessor (if any) in the sense
    % documented above.
    %
:- type conj_pred_map == map(conj_id, conj_id).

%---------------------------------------------------------------------------%

:- type string_state
    --->    string_const(string)
    ;       string_append(goal_id, prog_var, prog_var)
            % The string variables being appended together,
            % and the id of the goal that does the appending.
    ;       string_append_list(goal_id, prog_var).
            % The variables holding the list skeleton of the list of string
            % variables being appended together,
            % and the id of the goal that does the appending.

    % Maps each variable representing a format string
    % - either to the format string itself,
    % - or to the method of its construction.
:- type string_map          == map(prog_var, string_state).

:- type list_skeleton_state
    --->    list_skeleton_nil
    ;       list_skeleton_cons(
                head        :: prog_var,
                tail        :: prog_var
            ).

    % Maps each variable participating in the skeleton of the list of values
    % to be printed to its value.
:- type list_skeleton_map   == map(prog_var, list_skeleton_state).

    % Maps each variable representing a polytype in the list of values to be
    % printed to an abstract representation of that polytype, with the
    % actual value to be printed replaced by the variable that will hold
    % that value at runtime.
    %
    % For example, when we find the unification X = string.poly_type.s(Y),
    % we add to the list_element_map an entry mapping X to apt_s(Y).
:- type list_element_map    == map(prog_var, abstract_poly_type).

    % Maps each variable defined in terms of another variable to the variable
    % it is assigned from.
:- type fc_eqv_map          == map(prog_var, prog_var).

    % The knowledge we have recorded from assign and construct unifications
    % in a given conjunction.
    %
:- type conj_map
    --->    conj_map(
                cm_string_map           :: string_map,
                cm_list_skeleton_map    :: list_skeleton_map,
                cm_list_element_map     :: list_element_map,
                cm_eqv_map              :: fc_eqv_map
            ).

    % Maps the id of each conjunction to the knowledge we have derived from
    % unifications in that conjunction.
    %
:- type conj_maps == map(conj_id, conj_map).

%---------------------------------------------------------------------------%

    % Records the information about each call site that is not common
    % to all calls to recognized predicates and function.
    %
:- type format_call_kind
    --->    kind_string_format(
                sf_context              :: prog_context,
                sf_result_var           :: prog_var
            )
    ;       kind_io_format_nostream(
                iofns_context           :: prog_context,
                iofns_io_in_var         :: prog_var,
                iofns_io_out_var        :: prog_var
            )
    ;       kind_io_format_stream(
                iofs_context            :: prog_context,
                iofs_stream_var         :: prog_var,
                iofs_io_in_var          :: prog_var,
                iofs_io_out_var         :: prog_var
            )
    ;       kind_stream_string_writer(
                ssw_context             :: prog_context,
                ssw_tc_info_var         :: prog_var,
                ssw_stream_var          :: prog_var,
                ssw_in_var              :: prog_var,
                ssw_out_var             :: prog_var
            ).

%---------------------------------------------------------------------------%

is_format_call(ModuleName, Name, Args) :-
    % NOTE The logic here must match the test logic of
    % is_format_call_kind_and_vars below.
    Name = "format",
    ( if
        ModuleName = mercury_string_module
    then
        Args = [_FormatStringVar, _FormattedValuesVar, _ResultVar]
    else if
        ModuleName = mercury_io_module
    then
        (
            Args = [_FormatStringVar, _FormattedValuesVar, _IOIn, _IOOut]
        ;
            Args = [_StreamVar, _FormatStringVar, _FormattedValuesVar,
                _IOIn, _IOOut]
        )
    else if
        ModuleName = mercury_std_lib_module_name(
            qualified(unqualified("stream"), "string_writer"))
    then
        Args = [_TC_InfoVarForStream, _StreamVar, _FormatStringVar,
            _FormattedValuesVar, _StateInVar, _StateOutVar]
    else
        fail
    ).

:- pred is_format_call_kind_and_vars(module_name::in, string::in,
    list(prog_var)::in, hlds_goal_info::in,
    format_call_kind::out, prog_var::out, prog_var::out) is semidet.

is_format_call_kind_and_vars(ModuleName, Name, Args, GoalInfo,
        Kind, FormatStringVar, FormattedValuesVar) :-
    % If you modify this code to recognize any previously unrecognized
    % predicates, then you also need to update the call tree of
    % get_implicit_dependencies_* in module_imports.m. That code tests whether
    % a list of items calls one of these predicates, so that it can record
    % the need to implicitly import the modules that contain the predicates
    % that implement their optimized versions.
    %
    % NOTE The test logic here must be duplicated in is_format_call above.
    Name = "format",
    ( if
        ModuleName = mercury_string_module
    then
        % We have these arguments regardless of whether we call the
        % predicate or function version of string.format.
        Args = [FormatStringVar, FormattedValuesVar, ResultVar],
        Context = goal_info_get_context(GoalInfo),
        Kind = kind_string_format(Context, ResultVar)
    else if
        ModuleName = mercury_io_module
    then
        (
            Args = [FormatStringVar, FormattedValuesVar, IOIn, IOOut],
            Context = goal_info_get_context(GoalInfo),
            Kind = kind_io_format_nostream(Context, IOIn, IOOut)
        ;
            Args = [StreamVar, FormatStringVar, FormattedValuesVar,
                IOIn, IOOut],
            Context = goal_info_get_context(GoalInfo),
            Kind = kind_io_format_stream(Context, StreamVar, IOIn, IOOut)
        )
    else if
        ModuleName = mercury_std_lib_module_name(
            qualified(unqualified("stream"), "string_writer"))
    then
        % Since we do this check after polymorphism, there will have been
        % a typeclassinfo inserted at the front of the argument list.
        Args = [TC_InfoVarForStream, StreamVar, FormatStringVar,
            FormattedValuesVar, StateInVar, StateOutVar],
        Context = goal_info_get_context(GoalInfo),
        Kind = kind_stream_string_writer(Context, TC_InfoVarForStream,
            StreamVar, StateInVar, StateOutVar)
    else
        fail
    ).

%---------------------------------------------------------------------------%

analyze_and_optimize_format_calls(ModuleInfo, GenImplicitStreamWarnings,
        Goal0, MaybeGoal, Specs, !VarTable) :-
    map.init(ConjMaps0),
    counter.init(0, Counter0),
    fill_goal_id_slots_in_proc_body(ModuleInfo, !.VarTable, _, Goal0, Goal1),

    module_info_get_globals(ModuleInfo, Globals0),
    globals.set_option(dump_hlds_options, string("vxP"), Globals0, Globals),
    OutInfo = init_hlds_out_info(Globals, output_debug),
    trace [io(!IO), compiletime(flag("debug_format_call"))] (
        io.output_stream(Stream, !IO),
        io.write_string(Stream, "\n\nBEFORE TRANSFORM:\n", !IO),
        write_goal(OutInfo, Stream, ModuleInfo, vns_var_table(!.VarTable),
            print_name_and_num, 0, "\n", Goal1, !IO)
    ),

    globals.lookup_bool_option(Globals, warn_unknown_format_calls,
        WarnUnknownFormatBool),
    (
        WarnUnknownFormatBool = no,
        WarnUnknownFormat = do_not_warn_unknown_format
    ;
        WarnUnknownFormatBool = yes,
        WarnUnknownFormat = warn_unknown_format
    ),
    Params = format_call_traverse_params(ModuleInfo, WarnUnknownFormat),
    format_call_traverse_goal(Params, Goal1, _, [], FormatCallSites,
        Counter0, _Counter, ConjMaps0, ConjMaps, map.init, PredMap,
        set_of_var.init, _),
    globals.get_opt_tuple(Globals, OptTuple),
    OptFormatCalls = OptTuple ^ ot_opt_format_calls,
    globals.lookup_bool_option(Globals, exec_trace, ExecTrace),
    % If users write a call to e.g. to string.format, they expect the debugger
    % to show them calls to string.format. Showing a sequence of calls to
    % lower level predicates instead of that higher level call would probably
    % confuse debugger users.
    ( if
        OptFormatCalls = opt_format_calls,
        ExecTrace = no
    then
        ShouldOptFormatCalls = opt_format_calls
    else
        ShouldOptFormatCalls = do_not_opt_format_calls
    ),
    list.foldl3(
        check_format_call_site(ModuleInfo, GenImplicitStreamWarnings,
            ShouldOptFormatCalls, ConjMaps, PredMap),
        FormatCallSites, map.init, GoalIdMap, [], Specs, !VarTable),
    ( if map.is_empty(GoalIdMap) then
        % We have not found anything to improve in Goal1.
        MaybeGoal = no
    else
        % We want to set NeededVars0 to be the set of the procedure's
        % output arguments, but it is ok to add into it some non-output
        % arguments whose insts happen to change as well.
        Goal1 = hlds_goal(_, GoalInfo1),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo1),
        instmap_delta_changed_vars(InstMapDelta, NeededVars0),
        ToDeleteVars0 = set_of_var.init,
        ToDeleteGoals0 = set_tree234.init,
        opt_format_call_sites_in_goal(Goal1, Goal, GoalIdMap, _,
            NeededVars0, _NeededVars, ToDeleteVars0, _ToDeleteVars,
            ToDeleteGoals0, _ToDeleteGoals),
        trace [io(!IO), compiletime(flag("debug_format_call"))] (
            io.output_stream(Stream, !IO),
            io.write_string(Stream, "\n\nAFTER TRANSFORM:\n", !IO),
            write_goal(OutInfo, Stream, ModuleInfo, vns_var_table(!.VarTable),
                print_name_and_num, 0, "\n", Goal, !IO)
        ),
        MaybeGoal = yes(Goal)
    ).

:- pred check_format_call_site(module_info::in,
    maybe_generate_implicit_stream_warnings::in, maybe_opt_format_calls::in,
    conj_maps::in, conj_pred_map::in, format_call_site::in,
    fc_goal_id_map::in, fc_goal_id_map::out,
    list(error_spec)::in, list(error_spec)::out,
    var_table::in, var_table::out) is det.

check_format_call_site(ModuleInfo, ImplicitStreamWarnings, OptFormatCalls,
        ConjMaps, PredMap, FormatCallSite, !GoalIdMap, !Specs, !VarTable) :-
    FormatCallSite = format_call_site(GoalId, GoalInfo, StringVar, ValuesVar,
        CallKind, PredId, ModuleName, Name, Arity, Context, CurId,
        WarnUnknownFormat),
    (
        ImplicitStreamWarnings = do_not_generate_implicit_stream_warnings
    ;
        ImplicitStreamWarnings = generate_implicit_stream_warnings,
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        maybe_generate_warning_for_implicit_stream_predicate(ModuleInfo,
            PredId, PredInfo, GoalInfo, MaybeImplicitStreamSpec),
        (
            MaybeImplicitStreamSpec = no
        ;
            MaybeImplicitStreamSpec = yes(ImplicitStreamSpec),
            !:Specs = [ImplicitStreamSpec | !.Specs]
        )
    ),

    SymName = qualified(ModuleName, Name),
    module_info_get_globals(ModuleInfo, Globals),

    follow_format_string(ConjMaps, PredMap, CurId, StringVar,
        FormatStringResult),
    (
        FormatStringResult = follow_string_result(_, _, _)
    ;
        FormatStringResult = no_follow_string_result,
        (
            WarnUnknownFormat = do_not_warn_unknown_format
        ;
            WarnUnknownFormat = warn_unknown_format,
            UnknownFormatPieces = [words("Unknown format string in call to"),
                qual_sym_name_arity(sym_name_arity(SymName, Arity)),
                suffix("."), nl],
            UnknownFormatSpec = simplest_spec($pred, severity_warning,
                phase_simplify(report_in_any_mode),
                Context, UnknownFormatPieces),
            !:Specs = [UnknownFormatSpec | !.Specs]
        )
    ),

    follow_list_skeleton(ConjMaps, PredMap, CurId, ValuesVar, SkeletonResult),
    ( if
        SkeletonResult = follow_skeleton_result(PolytypeVars0, SkeletonVars0),
        list.map(follow_poly_type(ConjMaps, PredMap, CurId), PolytypeVars0,
            MaybeAbstractPolyTypes0),
        project_all_yes(MaybeAbstractPolyTypes0, AbstractPolyTypes0)
    then
        PolyTypesToDeleteVars0 =
            [StringVar, ValuesVar | SkeletonVars0] ++ PolytypeVars0,
        MaybePolyTypesInfo = yes({AbstractPolyTypes0, PolyTypesToDeleteVars0})
    else
        MaybePolyTypesInfo = no,
        (
            WarnUnknownFormat = do_not_warn_unknown_format
        ;
            WarnUnknownFormat = warn_unknown_format,
            UnknownFormatValuesPieces =
                [words("Unknown format values in call to"),
                qual_sym_name_arity(sym_name_arity(SymName, Arity)),
                suffix("."), nl],
            UnknownFormatValuesSpec = simplest_spec($pred, severity_warning,
                phase_simplify(report_in_any_mode),
                Context, UnknownFormatValuesPieces),
            !:Specs = [UnknownFormatValuesSpec | !.Specs]
        )
    ),

    ( if
        FormatStringResult = follow_string_result(FormatString,
            FormatStringToDeleteVars, ToDeleteGoals),
        MaybePolyTypesInfo = yes({AbstractPolyTypes, PolyTypeToDeleteVars})
    then
        string.to_char_list(FormatString, FormatStringChars),
        parse_and_optimize_format_string(FormatStringChars, AbstractPolyTypes,
            Context, MaybeSpecs),
        (
            MaybeSpecs = error(HeadError, TailErrors),
            globals.lookup_bool_option(Globals, warn_known_bad_format_calls,
                WarnKnownBadFormatCalls),
            (
                WarnKnownBadFormatCalls = no
            ;
                WarnKnownBadFormatCalls = yes,
                PrefixPieces = [words("Mismatched format and values"),
                    words("in call to"),
                    qual_sym_name_arity(sym_name_arity(SymName, Arity)),
                    suffix(":"), nl],
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

                BadFormatSpec = simplest_spec($pred, severity_warning,
                    phase_simplify(report_in_any_mode),
                    Context, PrefixPieces ++ ErrorPieces),
                !:Specs = [BadFormatSpec | !.Specs]
            )
        ;
            MaybeSpecs = ok(Specs),
            (
                OptFormatCalls = do_not_opt_format_calls
            ;
                OptFormatCalls = opt_format_calls,
                ToDeleteVars =
                    FormatStringToDeleteVars ++ PolyTypeToDeleteVars,
                create_replacement_goal(ModuleInfo, GoalId, CallKind,
                    Specs, ToDeleteVars, ToDeleteGoals, !GoalIdMap, !VarTable)
            )
        )
    else
        % Any error message has already been generated, if asked for.
        true
    ).

:- pred project_all_yes(list(maybe(T))::in, list(T)::out) is semidet.

project_all_yes([], []).
project_all_yes([yes(Value) | TailMaybes], [Value | Tail]) :-
    project_all_yes(TailMaybes, Tail).

:- func string_format_error_to_words(string_format_error) = format_component.

string_format_error_to_words(Error) =
    words(string_format_error_to_msg(Error)).

%---------------------------------------------------------------------------%

:- type follow_string_result
    --->    no_follow_string_result
    ;       follow_string_result(
                % The string variable is known to be bound to this string.
                fsr_string              :: string,

                % The string variables that hold this string, and if it was
                % constructed using string append or append list operations,
                % the variables holding the pieces that were glued together
                % as well as any list skeletons holding those pieces.
                fsr_to_delete_vars      :: list(prog_var),

                % The identities of the calls to string append or append list
                % predicates that do the gluing. Does not list unification
                % goals, since those are deleted solely based on whether
                % they construct a to-delete variable.
                fsr_to_delete_goals     :: list(goal_id)
            ).

:- pred follow_format_string(conj_maps::in, conj_pred_map::in, conj_id::in,
    prog_var::in, follow_string_result::out) is det.

follow_format_string(ConjMaps, PredMap, CurId, StringVar, Result) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(StringMap, _, _, EqvMap),
    ( if map.search(EqvMap, StringVar, EqvVar) then
        follow_format_string(ConjMaps, PredMap, CurId, EqvVar, Result)
    else if map.search(StringMap, StringVar, StringState) then
        (
            StringState = string_const(String),
            Result = follow_string_result(String, [StringVar], [])
        ;
            StringState = string_append(AppendGoalId, StringVarA, StringVarB),
            follow_format_string(ConjMaps, PredMap, CurId, StringVarA,
                ResultA),
            follow_format_string(ConjMaps, PredMap, CurId, StringVarB,
                ResultB),
            ( if
                ResultA = follow_string_result(StringA,
                    ToDeleteVarsA, ToDeleteGoalsA),
                ResultB = follow_string_result(StringB,
                    ToDeleteVarsB, ToDeleteGoalsB)
            then
                Result = follow_string_result(StringA ++ StringB,
                    ToDeleteVarsA ++ ToDeleteVarsB,
                    [AppendGoalId] ++ ToDeleteGoalsA ++ ToDeleteGoalsB)
            else
                Result = no_follow_string_result
            )
        ;
            StringState = string_append_list(AppendListGoalId, SkeletonVar),
            follow_list_skeleton(ConjMaps, PredMap, CurId, SkeletonVar,
                SkeletonResult),
            (
                SkeletonResult = no_follow_skeleton_result,
                Result = no_follow_string_result
            ;
                SkeletonResult = follow_skeleton_result(SubStringVars,
                    SkeletonVars),
                list.map(follow_format_string(ConjMaps, PredMap, CurId),
                    SubStringVars, SubStringResults),
                ( if
                    project_all_follow_string_results(SubStringResults,
                        String, SubStringToDeleteVars, SubStringToDeleteGoals)
                then
                    Result = follow_string_result(String,
                        SkeletonVars ++ SubStringToDeleteVars,
                        [AppendListGoalId | SubStringToDeleteGoals])
                else
                    Result = no_follow_string_result
                )
            )
        )
    else if map.search(PredMap, CurId, PredId) then
        follow_format_string(ConjMaps, PredMap, PredId, StringVar, Result)
    else
        Result = no_follow_string_result
    ).

:- pred project_all_follow_string_results(list(follow_string_result)::in,
    string::out, list(prog_var)::out, list(goal_id)::out) is semidet.

project_all_follow_string_results([], "", [], []).
project_all_follow_string_results([HeadResult | TailResults],
        String, ToDeleteVars, ToDeleteGoals) :-
    HeadResult = follow_string_result(HeadString,
        HeadToDeleteVars, HeadToDeleteGoals),
    project_all_follow_string_results(TailResults,
        TailString, TailToDeleteVars, TailToDeleteGoals),
    String = HeadString ++ TailString,
    ToDeleteVars = HeadToDeleteVars ++ TailToDeleteVars,
    ToDeleteGoals = HeadToDeleteGoals ++ TailToDeleteGoals.

:- type follow_skeleton_result
    --->    no_follow_skeleton_result
    ;       follow_skeleton_result(
                fsr_element_vars        :: list(prog_var),
                fsr_skeleton_vars       :: list(prog_var)
            ).

:- pred follow_list_skeleton(conj_maps::in, conj_pred_map::in, conj_id::in,
    prog_var::in, follow_skeleton_result::out) is det.

follow_list_skeleton(ConjMaps, PredMap, CurId, ListVar, Result) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(_, ListMap, _, EqvMap),
    ( if map.search(EqvMap, ListVar, EqvVar) then
        follow_list_skeleton(ConjMaps, PredMap, CurId, EqvVar, Result)
    else if map.search(ListMap, ListVar, ListState) then
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
                TailResult = follow_skeleton_result(TailElementVars,
                    TailSkeletonVars),
                ElementVars = [HeadVar | TailElementVars],
                ( if list.member(TailVar, TailSkeletonVars) then
                    true
                else
                    unexpected($pred, "TailVar not in TailSkeletonVars")
                ),
                SkeletonVars = [ListVar | TailSkeletonVars],
                Result = follow_skeleton_result(ElementVars, SkeletonVars)
            )
        )
    else if map.search(PredMap, CurId, PredId) then
        follow_list_skeleton(ConjMaps, PredMap, PredId, ListVar, Result)
    else
        Result = no_follow_skeleton_result
    ).

:- pred follow_poly_type(conj_maps::in, conj_pred_map::in,
    conj_id::in, prog_var::in, maybe(abstract_poly_type)::out) is det.

follow_poly_type(ConjMaps, PredMap, CurId, PolytypeVar,
        MaybeAbstractPolyType) :-
    ConjMap = get_conj_map(ConjMaps, CurId),
    ConjMap = conj_map(_, _, ElementMap, EqvMap),
    ( if map.search(EqvMap, PolytypeVar, EqvVar) then
        follow_poly_type(ConjMaps, PredMap, CurId, EqvVar,
            MaybeAbstractPolyType)
    else if map.search(ElementMap, PolytypeVar, AbstractPolyType) then
        MaybeAbstractPolyType = yes(AbstractPolyType)
    else if map.search(PredMap, CurId, PredId) then
        follow_poly_type(ConjMaps, PredMap, PredId, PolytypeVar,
            MaybeAbstractPolyType)
    else
        MaybeAbstractPolyType = no
    ).

%---------------------------------------------------------------------------%

:- type format_call_traverse_params
    --->    format_call_traverse_params(
                module_info,
                maybe_warn_unknown_format
            ).

:- pred format_call_traverse_goal(format_call_traverse_params::in,
    hlds_goal::in, conj_id::out,
    list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    set_of_progvar::in, set_of_progvar::out) is det.

format_call_traverse_goal(Params, Goal, CurId, !FormatCallSites, !Counter,
        !ConjMaps, !PredMap, !RelevantVars) :-
    alloc_id(CurId, !Counter),
    goal_to_conj_list(Goal, GoalConj),
    format_call_traverse_conj(Params, GoalConj, CurId, !FormatCallSites,
        !Counter, !ConjMaps, !PredMap, !RelevantVars).

:- pred format_call_traverse_conj(format_call_traverse_params::in,
    list(hlds_goal)::in, conj_id::in,
    list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    set_of_progvar::in, set_of_progvar::out) is det.

format_call_traverse_conj(_, [], _CurId, !FormatCallSites,
        !Counter, !ConjMaps, !PredMap, !RelevantVars).
format_call_traverse_conj(Params, [Goal | Goals], CurId, !FormatCallSites,
        !Counter, !ConjMaps, !PredMap, !RelevantVars) :-
    format_call_traverse_conj(Params, Goals, CurId, !FormatCallSites,
        !Counter, !ConjMaps, !PredMap, !RelevantVars),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = conj(_, Conjuncts),
        format_call_traverse_conj(Params, Conjuncts, CurId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars)
    ;
        GoalExpr = disj(Disjuncts),
        format_call_traverse_disj(Params, Disjuncts, CurId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars)
    ;
        GoalExpr = switch(_, _, Cases),
        Disjuncts = list.map(project_case_goal, Cases),
        format_call_traverse_disj(Params, Disjuncts, CurId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),

        format_call_traverse_goal(Params, Else, ElseId, !FormatCallSites,
            !Counter, !ConjMaps, !PredMap, !RelevantVars),
        map.det_insert(ElseId, CurId, !PredMap),

        alloc_id(CondThenId, !Counter),
        goal_to_conj_list(Then, ThenConj),
        goal_to_conj_list(Cond, CondConj),
        format_call_traverse_conj(Params, CondConj ++ ThenConj, CondThenId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars),
        map.det_insert(CondThenId, CurId, !PredMap)
    ;
        GoalExpr = negation(SubGoal),
        format_call_traverse_goal(Params, SubGoal, SubGoalId,
            !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars),
        map.det_insert(SubGoalId, CurId, !PredMap)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = from_ground_term(TermVar, FromGroundTermKind),
            ( if
                FromGroundTermKind = from_ground_term_construct,
                % These scopes cannot build the format string (since that is
                % either a single constant, or the result of an operation on
                % strings, neither of which are things for which we build fgt
                % scopes). However, it can build the term to print. That will
                % happen only in degenerate cases, but we do have some
                % degenerate cases in the test suite.
                not set_of_var.member(!.RelevantVars, TermVar)
            then
                % It is ok not to traverse the subgoal. The scope cannot
                % contain any calls, and the unifications it does contain
                % are apparently not of interest to any later format call.
                true
            else
                format_call_traverse_conj(Params, [SubGoal], CurId,
                    !FormatCallSites, !Counter, !ConjMaps, !PredMap,
                    !RelevantVars)
            )
        ;
            Reason = disable_warnings(HeadWarning, TailWarnings),
            Warnings = [HeadWarning | TailWarnings],
            ( if list.member(goal_warning_unknown_format_calls, Warnings) then
                Params = format_call_traverse_params(ModuleInfo, _),
                NewParams = format_call_traverse_params(ModuleInfo,
                    do_not_warn_unknown_format),
                format_call_traverse_conj(NewParams, [SubGoal], CurId,
                    !FormatCallSites, !Counter, !ConjMaps, !PredMap,
                    !RelevantVars)
            else
                format_call_traverse_conj(Params, [SubGoal], CurId,
                    !FormatCallSites, !Counter, !ConjMaps, !PredMap,
                    !RelevantVars)
            )
        ;
            ( Reason = exist_quant(_)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            format_call_traverse_conj(Params, [SubGoal], CurId,
                !FormatCallSites, !Counter, !ConjMaps, !PredMap, !RelevantVars)
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = plain_call(PredId, _ProcId, ArgVars, _, _, _),
        Params = format_call_traverse_params(ModuleInfo, WarnUnknownFormat),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        ( if
            is_format_call_kind_and_vars(ModuleName, Name, ArgVars, GoalInfo,
                Kind, StringVar, ValuesVar)
        then
            Arity = pred_info_orig_arity(PredInfo),
            GoalId = goal_info_get_goal_id(GoalInfo),
            Context = goal_info_get_context(GoalInfo),
            FormatCallSite = format_call_site(GoalId, GoalInfo,
                StringVar, ValuesVar, Kind, PredId,
                ModuleName, Name, Arity, Context, CurId, WarnUnknownFormat),
            !:FormatCallSites = [FormatCallSite | !.FormatCallSites],
            set_of_var.insert_list([StringVar, ValuesVar], !RelevantVars)
        else if
            ModuleName = mercury_string_module
        then
            ( if
                ( Name = "append"
                ; Name = "++"
                ),
                ArgVars = [ArgVarA, ArgVarB, ResultVar],
                set_of_var.member(!.RelevantVars, ResultVar)
            then
                set_of_var.delete(ResultVar, !RelevantVars),
                set_of_var.insert(ArgVarA, !RelevantVars),
                set_of_var.insert(ArgVarB, !RelevantVars),
                GoalId = goal_info_get_goal_id(GoalInfo),
                StringState = string_append(GoalId, ArgVarA, ArgVarB),
                add_to_string_map(CurId, ResultVar, StringState, !ConjMaps)
            else if
                Name = "append_list",
                ArgVars = [ListSkeletonVar, ResultVar],
                set_of_var.member(!.RelevantVars, ResultVar)
            then
                set_of_var.delete(ResultVar, !RelevantVars),
                set_of_var.insert(ListSkeletonVar, !RelevantVars),
                GoalId = goal_info_get_goal_id(GoalInfo),
                StringState = string_append_list(GoalId, ListSkeletonVar),
                add_to_string_map(CurId, ResultVar, StringState, !ConjMaps)
            else
                true
            )
        else
            true
        )
    ;
        GoalExpr = unify(_, RHS, _, Unification, _),
        format_call_traverse_unify(Unification, GoalInfo, CurId,
            !ConjMaps, !PredMap, !RelevantVars),
        (
            RHS = rhs_lambda_goal(_Purity, _HOGroundness, _PredFunc,
                _EvalMethod, _LambdaNonLocals, _LambdaArgVarsModes,
                _LambdaDetism, LambdaGoal),
            format_call_traverse_goal(Params, LambdaGoal, LambdaGoalId,
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
            format_call_traverse_disj(Params, [MainGoal | OrElseGoals],
                CurId, !FormatCallSites, !Counter, !ConjMaps, !PredMap,
                !RelevantVars)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            format_call_traverse_goal(Params, SubGoal, SubGoalId,
                !FormatCallSites, !Counter, !ConjMaps, !PredMap,
                !RelevantVars),
            map.det_insert(SubGoalId, CurId, !PredMap)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred format_call_traverse_unify(unification::in, hlds_goal_info::in,
    conj_id::in, conj_maps::in, conj_maps::out, conj_pred_map::in,
    conj_pred_map::out, set_of_progvar::in, set_of_progvar::out) is det.

format_call_traverse_unify(Unification, GoalInfo, CurId, !ConjMaps, !PredMap,
        !RelevantVars) :-
    (
        Unification = assign(TargetVar, SourceVar),
        ( if set_of_var.member(!.RelevantVars, TargetVar) then
            set_of_var.delete(TargetVar, !RelevantVars),
            set_of_var.insert(SourceVar, !RelevantVars),
            add_to_fc_eqv_map(CurId, TargetVar, SourceVar, !ConjMaps)
        else
            true
        )
    ;
        Unification = construct(CellVar, ConsId, ArgVars, _, _, _, _),
        ( if set_of_var.member(!.RelevantVars, CellVar) then
            ( if
                ConsId = string_const(StringConst)
            then
                expect(unify(ArgVars, []), $pred, "string constant with args"),
                set_of_var.delete(CellVar, !RelevantVars),
                add_to_string_map(CurId, CellVar, string_const(StringConst),
                    !ConjMaps)
            else if
                ConsId = cons(SymName, Arity, TypeCtor),
                TypeCtor = list_type_ctor
            then
                Functor = unqualify_name(SymName),
                ( if
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
                then
                    List = ListPrime
                else
                    unexpected($pred, "unexpected list functor")
                ),
                set_of_var.delete(CellVar, !RelevantVars),
                set_of_var.insert_list(ArgVars, !RelevantVars),
                add_to_list_map(CurId, CellVar, List, !ConjMaps)
            else if
                ConsId = cons(SymName, Arity, TypeCtor),
                TypeCtor = poly_type_type_ctor
            then
                ( if
                    Arity = 1,
                    ArgVars = [ArgVar]
                then
                    Context = goal_info_get_context(GoalInfo),
                    Functor = unqualify_name(SymName),
                    ( if
                        (
                            Functor = "f",
                            VarPolyTypePrime = apt_f(ArgVar, Context)
                        ;
                            Functor = "i",
                            VarPolyTypePrime = apt_i(ArgVar, Context)
                        ;
                            Functor = "i8",
                            VarPolyTypePrime = apt_i8(ArgVar, Context)
                        ;
                            Functor = "i16",
                            VarPolyTypePrime = apt_i16(ArgVar, Context)
                        ;
                            Functor = "i32",
                            VarPolyTypePrime = apt_i32(ArgVar, Context)
                        ;
                            Functor = "i64",
                            VarPolyTypePrime = apt_i64(ArgVar, Context)
                        ;
                            Functor = "u",
                            VarPolyTypePrime = apt_u(ArgVar, Context)
                        ;
                            Functor = "u8",
                            VarPolyTypePrime = apt_u8(ArgVar, Context)
                        ;
                            Functor = "u16",
                            VarPolyTypePrime = apt_u16(ArgVar, Context)
                        ;
                            Functor = "u32",
                            VarPolyTypePrime = apt_u32(ArgVar, Context)
                        ;
                            Functor = "u64",
                            VarPolyTypePrime = apt_u64(ArgVar, Context)
                        ;
                            Functor = "s",
                            VarPolyTypePrime = apt_s(ArgVar, Context)
                        ;
                            Functor = "c",
                            VarPolyTypePrime = apt_c(ArgVar, Context)
                        )
                    then
                        VarPolyType = VarPolyTypePrime
                    else
                        unexpected($pred, "unexpected poly_type functor")
                    )
                else
                    unexpected($pred, "poly_type arity mismatch")
                ),
                set_of_var.delete(CellVar, !RelevantVars),
                add_to_element_map(CurId, CellVar, VarPolyType, !ConjMaps)
            else
                true
            )
        else
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

:- pred format_call_traverse_disj(format_call_traverse_params::in,
    list(hlds_goal)::in, conj_id::in,
    list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out,
    set_of_progvar::in, set_of_progvar::out) is det.

format_call_traverse_disj(Params, Disjuncts, CurId, !FormatCallSites,
        !Counter, !ConjMaps, !PredMap, !RelevantVars) :-
    format_call_traverse_disj_arms(Params, Disjuncts, CurId,
        !FormatCallSites, !Counter, !ConjMaps, !PredMap, DisjRelevantVarSets),
    DisjRelevantVars = set_of_var.union_list(DisjRelevantVarSets),
    set_of_var.union(DisjRelevantVars, !RelevantVars).

:- pred format_call_traverse_disj_arms(format_call_traverse_params::in,
    list(hlds_goal)::in, conj_id::in,
    list(format_call_site)::in, list(format_call_site)::out,
    counter::in, counter::out, conj_maps::in, conj_maps::out,
    conj_pred_map::in, conj_pred_map::out, list(set_of_progvar)::out) is det.

format_call_traverse_disj_arms(_, [], _,
        !FormatCallSites, !Counter, !ConjMaps, !PredMap, []).
format_call_traverse_disj_arms(Params, [Goal | Goals], ContainingId,
        !FormatCallSites, !Counter, !ConjMaps, !PredMap, GoalRelevantVars) :-
    format_call_traverse_goal(Params, Goal, DisjId,
        !FormatCallSites, !Counter, !ConjMaps, !PredMap,
        set_of_var.init, HeadRelevantVars),
    map.det_insert(DisjId, ContainingId, !PredMap),
    format_call_traverse_disj_arms(Params, Goals, ContainingId,
        !FormatCallSites, !Counter, !ConjMaps, !PredMap, TailRelevantVars),
    GoalRelevantVars = [HeadRelevantVars | TailRelevantVars].

%---------------------------------------------------------------------------%

:- pred alloc_id(conj_id::out, counter::in, counter::out) is det.

alloc_id(ConjId, !Counter) :-
    counter.allocate(N, !Counter),
    ConjId = conj_id(N).

:- func get_conj_map(conj_maps, conj_id) = conj_map.

get_conj_map(ConjMaps, ConjId) = ConjMap :-
    ( if map.search(ConjMaps, ConjId, ConjMapPrime) then
        ConjMap = ConjMapPrime
    else
        ConjMap = conj_map(map.init, map.init, map.init, map.init)
    ).

:- pred add_to_string_map(conj_id::in, prog_var::in, string_state::in,
    conj_maps::in, conj_maps::out) is det.

add_to_string_map(ConjId, Var, StringState, !ConjMaps) :-
    trace [io(!IO), compiletime(flag("debug_format_call"))] (
        io.output_stream(Stream, !IO),
        io.write_string(Stream, "adding to string map: ", !IO),
        io.write(Stream, Var, !IO),
        io.write_string(Stream, " -> ", !IO),
        io.write(Stream, StringState, !IO),
        io.nl(Stream, !IO)
    ),
    ( if map.search(!.ConjMaps, ConjId, ConjMap0) then
        ConjMap0 = conj_map(StringMap0, ListMap, ElementMap, EqvMap),
        map.det_insert(Var, StringState, StringMap0, StringMap),
        ConjMap = conj_map(StringMap, ListMap, ElementMap, EqvMap),
        map.det_update(ConjId, ConjMap, !ConjMaps)
    else
        StringMap = map.singleton(Var, StringState),
        ConjMap = conj_map(StringMap, map.init, map.init, map.init),
        map.det_insert(ConjId, ConjMap, !ConjMaps)
    ).

:- pred add_to_list_map(conj_id::in, prog_var::in, list_skeleton_state::in,
    conj_maps::in, conj_maps::out) is det.

add_to_list_map(ConjId, Var, ListState, !ConjMaps) :-
    trace [io(!IO), compiletime(flag("debug_format_call"))] (
        io.output_stream(Stream, !IO),
        io.write_string(Stream, "adding to list map: ", !IO),
        io.write(Stream, Var, !IO),
        io.write_string(Stream, " -> ", !IO),
        io.write(Stream, ListState, !IO),
        io.nl(Stream, !IO)
    ),
    ( if map.search(!.ConjMaps, ConjId, ConjMap0) then
        ConjMap0 = conj_map(StringMap, ListMap0, ElementMap, EqvMap),
        map.det_insert(Var, ListState, ListMap0, ListMap),
        ConjMap = conj_map(StringMap, ListMap, ElementMap, EqvMap),
        map.det_update(ConjId, ConjMap, !ConjMaps)
    else
        ListMap = map.singleton(Var, ListState),
        ConjMap = conj_map(map.init, ListMap, map.init, map.init),
        map.det_insert(ConjId, ConjMap, !ConjMaps)
    ).

:- pred add_to_element_map(conj_id::in, prog_var::in, abstract_poly_type::in,
    conj_maps::in, conj_maps::out) is det.

add_to_element_map(ConjId, Var, Element, !ConjMaps) :-
    trace [io(!IO), compiletime(flag("debug_format_call"))] (
        io.output_stream(Stream, !IO),
        io.write_string(Stream, "adding to elemnt map: ", !IO),
        io.write(Stream, Var, !IO),
        io.write_string(Stream, " -> ", !IO),
        io.write(Stream, Element, !IO),
        io.nl(Stream, !IO)
    ),
    ( if map.search(!.ConjMaps, ConjId, ConjMap0) then
        ConjMap0 = conj_map(StringMap, ListMap, ElementMap0, EqvMap),
        map.det_insert(Var, Element, ElementMap0, ElementMap),
        ConjMap = conj_map(StringMap, ListMap, ElementMap, EqvMap),
        map.det_update(ConjId, ConjMap, !ConjMaps)
    else
        ElementMap = map.singleton(Var, Element),
        ConjMap = conj_map(map.init, map.init, ElementMap, map.init),
        map.det_insert(ConjId, ConjMap, !ConjMaps)
    ).

:- pred add_to_fc_eqv_map(conj_id::in, prog_var::in, prog_var::in,
    conj_maps::in, conj_maps::out) is det.

add_to_fc_eqv_map(ConjId, Var, EqvVar, !ConjMaps) :-
    ( if map.search(!.ConjMaps, ConjId, ConjMap0) then
        ConjMap0 = conj_map(StringMap, ListMap, ElementMap, EqvMap0),
        map.det_insert(Var, EqvVar, EqvMap0, EqvMap),
        ConjMap = conj_map(StringMap, ListMap, ElementMap, EqvMap),
        map.det_update(ConjId, ConjMap, !ConjMaps)
    else
        EqvMap = map.singleton(Var, EqvVar),
        ConjMap = conj_map(map.init, map.init, map.init, EqvMap),
        map.det_insert(ConjId, ConjMap, !ConjMaps)
    ).

%---------------------------------------------------------------------------%

:- type fc_opt_goal_info
    --->    fc_opt_goal_info(
                % The goal identified by the key that lead us here
                % should be replaced by this replacement goal.

                fcogi_replacement_goal  :: hlds_goal,
                fcogi_unneeded_vars     :: list(prog_var),
                fcogi_unneeded_goals    :: list(goal_id)
            ).

:- type fc_goal_id_map == map(goal_id, fc_opt_goal_info).

    % Traverse the goal, looking for call sites in !.GoalIdMap. If we
    % find them, we replace them with the corresponding goal.
    %
:- pred opt_format_call_sites_in_goal(hlds_goal::in, hlds_goal::out,
    fc_goal_id_map::in, fc_goal_id_map::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    set_tree234(goal_id)::in, set_tree234(goal_id)::out) is det.

opt_format_call_sites_in_goal(Goal0, Goal, !GoalIdMap,
        !NeededVars, !ToDeleteVars, !ToDeleteGoals) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        GoalId = goal_info_get_goal_id(GoalInfo),
        ( if map.remove(GoalId, OptGoalInfo, !GoalIdMap) then
            OptGoalInfo = fc_opt_goal_info(ReplacementGoal,
                GoalToDeleteVars, GoalToDeleteGoals),
            Goal = ReplacementGoal,
            set_of_var.insert_list(GoalToDeleteVars, !ToDeleteVars),
            set_tree234.insert_list(GoalToDeleteGoals, !ToDeleteGoals)
        else
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            ( if
                set_tree234.remove(GoalId, !.ToDeleteGoals, NewToDeleteGoals),
                set_of_var.intersect(NonLocals, !.NeededVars, NeededNonLocals),
                set_of_var.is_empty(NeededNonLocals)
            then
                !:ToDeleteGoals = NewToDeleteGoals,
                Goal = true_goal
            else
                Goal = Goal0,
                % Assume that all nonlocals are needed.
                set_of_var.union(NonLocals, !NeededVars),
                set_of_var.difference(!.ToDeleteVars, NonLocals,
                    !:ToDeleteVars)
            )
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
        ( if
            Unification = construct(LHSVar, _ConsId, _RHSVars, _ArgModes,
                _How, _Unique, _SubInfo),
            not set_of_var.member(!.NeededVars, LHSVar),
            % If this succeeds, then the backward traversal cannot encounter
            % any more producers of LHSVar.
            set_of_var.remove(LHSVar, !ToDeleteVars)
        then
            % This effectively deletes the unification.
            Goal = true_goal
        else
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
            !GoalIdMap, !NeededVars, !ToDeleteVars, !ToDeleteGoals),
        GoalExpr = conj(ConjType, Conjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = disj(Disjuncts0),
        opt_format_call_sites_in_disj(Disjuncts0, Disjuncts, !GoalIdMap,
            !.NeededVars, [], NeededVarsSets,
            !.ToDeleteVars, [], ToDeleteVarsSets,
            !.ToDeleteGoals, [], ToDeleteGoalsSets),
        !:NeededVars = set_of_var.union_list(NeededVarsSets),
        !:ToDeleteVars = set_of_var.intersect_list(ToDeleteVarsSets),
        !:ToDeleteGoals = set_tree234.union_list(ToDeleteGoalsSets),
        GoalExpr = disj(Disjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        opt_format_call_sites_in_switch(Cases0, Cases, !GoalIdMap,
            !.NeededVars, [], NeededVarsSets,
            !.ToDeleteVars, [], ToDeleteVarsSets,
            !.ToDeleteGoals, [], ToDeleteGoalsSets),
        !:NeededVars = set_of_var.union_list(NeededVarsSets),
        !:ToDeleteVars = set_of_var.intersect_list(ToDeleteVarsSets),
        !:ToDeleteGoals = set_tree234.union_list(ToDeleteGoalsSets),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        opt_format_call_sites_in_goal(Else0, Else, !GoalIdMap,
            !.NeededVars, NeededVarsBeforeElse,
            !.ToDeleteVars, ToDeleteVarsBeforeElse,
            !.ToDeleteGoals, ToDeleteGoalsBeforeElse),
        opt_format_call_sites_in_goal(Then0, Then, !GoalIdMap,
            !.NeededVars, NeededVarsBeforeThen,
            !.ToDeleteVars, ToDeleteVarsBeforeThen,
            !.ToDeleteGoals, ToDeleteGoalsBeforeThen),
        opt_format_call_sites_in_goal(Cond0, Cond, !GoalIdMap,
            NeededVarsBeforeThen, NeededVarsBeforeCond,
            ToDeleteVarsBeforeThen, ToDeleteVarsBeforeCond,
            ToDeleteGoalsBeforeThen, ToDeleteGoalsBeforeCond),
        set_of_var.union(NeededVarsBeforeCond, NeededVarsBeforeElse,
            !:NeededVars),
        set_of_var.intersect(ToDeleteVarsBeforeCond, ToDeleteVarsBeforeElse,
            !:ToDeleteVars),
        set_tree234.union(ToDeleteGoalsBeforeCond, ToDeleteGoalsBeforeElse,
            !:ToDeleteGoals),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = negation(SubGoal0),
        % SubGoal0 cannot generate anything in !.ToDeleteVars, but it can add
        % to all of !:NeededVars, !:ToDeleteVars and !:ToDeleteGoals.
        opt_format_call_sites_in_goal(SubGoal0, SubGoal,
            !GoalIdMap, !NeededVars, !ToDeleteVars, !ToDeleteGoals),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(TermVar, from_ground_term_construct),
            not set_of_var.member(!.NeededVars, TermVar),
            % If this succeeds, then the backward traversal cannot encounter
            % any more producers of LHSVar.
            set_of_var.remove(TermVar, !ToDeleteVars)
        then
            % We cannot guarantee that the modified version of SubGoal0
            % meets the invariants required of a goal in a
            % from_ground_term_construct scope, so we remove the scope.
            opt_format_call_sites_in_goal(SubGoal0, Goal,
                !GoalIdMap, !NeededVars, !ToDeleteVars, !ToDeleteGoals)
        else
            opt_format_call_sites_in_goal(SubGoal0, SubGoal,
                !GoalIdMap, !NeededVars, !ToDeleteVars, !ToDeleteGoals),
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
                !.ToDeleteVars, ToDeleteVarsMain,
                !.ToDeleteGoals, ToDeleteGoalsMain),
            opt_format_call_sites_in_disj(OrElseGoals0, OrElseGoals,
                !GoalIdMap, !.NeededVars, [], NeededVarsSets,
                !.ToDeleteVars, [], ToDeleteVarsSets,
                !.ToDeleteGoals, [], ToDeleteGoalsSets),
            !:NeededVars =
                set_of_var.union_list([NeededVarsMain | NeededVarsSets]),
            !:ToDeleteVars =
                set_of_var.intersect_list(
                    [ToDeleteVarsMain | ToDeleteVarsSets]),
            !:ToDeleteGoals =
                set_tree234.union_list(
                    [ToDeleteGoalsMain | ToDeleteGoalsSets]),
            ShortHand = atomic_goal(AtomicType, OuterVars, InnerVars,
                OutputVars, MainGoal, OrElseGoals, OrElseInners),
            GoalExpr = shorthand(ShortHand)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            opt_format_call_sites_in_goal(SubGoal0, SubGoal,
                !GoalIdMap, !NeededVars, !ToDeleteVars, !ToDeleteGoals),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
            GoalExpr = shorthand(ShortHand)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded by now.
            unexpected($pred, "bi_implication")
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

:- pred opt_format_call_sites_in_conj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    fc_goal_id_map::in, fc_goal_id_map::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    set_tree234(goal_id)::in, set_tree234(goal_id)::out) is det.

opt_format_call_sites_in_conj([], [], !GoalIdMap,
        !NeededVars, !ToDeleteVars, !ToDeleteGoals).
opt_format_call_sites_in_conj([HeadGoal0 | TailGoals0], Goals, !GoalIdMap,
        !NeededVars, !ToDeleteVars, !ToDeleteGoals) :-
    % We traverse conjunctions backwards.
    opt_format_call_sites_in_conj(TailGoals0, TailGoals, !GoalIdMap,
        !NeededVars, !ToDeleteVars, !ToDeleteGoals),
    opt_format_call_sites_in_goal(HeadGoal0, HeadGoal, !GoalIdMap,
        !NeededVars, !ToDeleteVars, !ToDeleteGoals),
    HeadGoal = hlds_goal(HeadGoalExpr, _),
    ( if HeadGoalExpr = conj(plain_conj, HeadSubGoals) then
        % Flatten out nested conjunctions. This will improve the HLDS
        % - when HeadSubGoals is an empty list, i.e. when we simply
        %   deleted HeadGoal0, and
        % - when HeadSubGoals is a non-empty list, i.e. when we
        %   replaced HeadGoal0 with its optimized version.
        Goals = HeadSubGoals ++ TailGoals
    else
        Goals = [HeadGoal | TailGoals]
    ).

:- pred opt_format_call_sites_in_disj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    fc_goal_id_map::in, fc_goal_id_map::out,
    set_of_progvar::in, list(set_of_progvar)::in, list(set_of_progvar)::out,
    set_of_progvar::in, list(set_of_progvar)::in, list(set_of_progvar)::out,
    set_tree234(goal_id)::in,
    list(set_tree234(goal_id))::in, list(set_tree234(goal_id))::out) is det.

opt_format_call_sites_in_disj([], [], !GoalIdMap,
        _, !NeededVarsSets, _, !ToDeleteVarsSets, _, !ToDeleteGoalSets).
opt_format_call_sites_in_disj([Goal0 | Goals0], [Goal | Goals], !GoalIdMap,
        NeededVars0, !NeededVarsSets, ToDeleteVars0, !ToDeleteVarsSets,
        ToDeleteGoals0, !ToDeleteGoalSets) :-
    % The order of traversal does not matter for disjunctions, since the
    % disjuncts are independent. This order is more efficient.
    opt_format_call_sites_in_goal(Goal0, Goal, !GoalIdMap,
        NeededVars0, NeededVars, ToDeleteVars0, ToDeleteVars,
        ToDeleteGoals0, ToDeleteGoals),
    !:NeededVarsSets = [NeededVars | !.NeededVarsSets],
    !:ToDeleteVarsSets = [ToDeleteVars | !.ToDeleteVarsSets],
    !:ToDeleteGoalSets = [ToDeleteGoals | !.ToDeleteGoalSets],
    opt_format_call_sites_in_disj(Goals0, Goals, !GoalIdMap,
        NeededVars0, !NeededVarsSets, ToDeleteVars0, !ToDeleteVarsSets,
        ToDeleteGoals0, !ToDeleteGoalSets).

:- pred opt_format_call_sites_in_switch(list(case)::in, list(case)::out,
    fc_goal_id_map::in, fc_goal_id_map::out,
    set_of_progvar::in, list(set_of_progvar)::in, list(set_of_progvar)::out,
    set_of_progvar::in, list(set_of_progvar)::in, list(set_of_progvar)::out,
    set_tree234(goal_id)::in,
    list(set_tree234(goal_id))::in, list(set_tree234(goal_id))::out) is det.

opt_format_call_sites_in_switch([], [], !GoalIdMap,
        _, !NeededVarsSets, _, !ToDeleteVarsSets, _, !ToDeleteGoalSets).
opt_format_call_sites_in_switch([Case0 | Cases0], [Case | Cases], !GoalIdMap,
        NeededVars0, !NeededVarsSets, ToDeleteVars0, !ToDeleteVarsSets,
        ToDeleteGoals0, !ToDeleteGoalSets) :-
    % The order of traversal does not matter for switches, since the
    % switch arms are independent. This order is more efficient.
    Case0 = case(FirstConsId, LaterConsIds, Goal0),
    opt_format_call_sites_in_goal(Goal0, Goal, !GoalIdMap,
        NeededVars0, NeededVars, ToDeleteVars0, ToDeleteVars,
        ToDeleteGoals0, ToDeleteGoals),
    !:NeededVarsSets = [NeededVars | !.NeededVarsSets],
    !:ToDeleteVarsSets = [ToDeleteVars | !.ToDeleteVarsSets],
    !:ToDeleteGoalSets = [ToDeleteGoals | !.ToDeleteGoalSets],
    Case = case(FirstConsId, LaterConsIds, Goal),
    opt_format_call_sites_in_switch(Cases0, Cases, !GoalIdMap,
        NeededVars0, !NeededVarsSets, ToDeleteVars0, !ToDeleteVarsSets,
        ToDeleteGoals0, !ToDeleteGoalSets).

%---------------------------------------------------------------------------%

:- pred create_replacement_goal(module_info::in, goal_id::in,
    format_call_kind::in, list(compiler_format_spec)::in,
    list(prog_var)::in, list(goal_id)::in,
    fc_goal_id_map::in, fc_goal_id_map::out,
    var_table::in, var_table::out) is det.

create_replacement_goal(ModuleInfo, GoalId, CallKind, Specs,
        ToDeleteVars, ToDeleteGoals, !GoalIdMap, !VarTable) :-
    % Note that every predicate or function that this code generates calls to
    % must be listed in simplify_may_introduce_calls, in order to prevent
    % its definition from being thrown away by dead_pred_elim before execution
    % gets here.
    (
        CallKind = kind_string_format(Context, ResultVar),
        create_string_format_replacement(ModuleInfo, Specs, Context,
            ResultVar, ReplacementGoal, !VarTable)
    ;
        (
            CallKind = kind_io_format_nostream(Context, IOInVar, IOOutVar),
            MaybeStreamVar = no
        ;
            CallKind = kind_io_format_stream(Context, StreamVar,
                IOInVar, IOOutVar),
            MaybeStreamVar = yes(StreamVar)
        ),
        create_io_format_replacement(ModuleInfo, Specs, Context,
            MaybeStreamVar, IOInVar, IOOutVar, ReplacementGoal, !VarTable)
    ;
        CallKind = kind_stream_string_writer(Context,
            TC_InfoVarForStream, StreamVar, StateInVar, StateOutVar),
        create_stream_string_writer_format_replacement(ModuleInfo, Specs,
            Context, TC_InfoVarForStream, StreamVar, StateInVar, StateOutVar,
            ReplacementGoal, !VarTable)
    ),
    FCOptGoalInfo = fc_opt_goal_info(ReplacementGoal,
        list.sort(ToDeleteVars), list.sort(ToDeleteGoals)),
    map.det_insert(GoalId, FCOptGoalInfo, !GoalIdMap).

%---------------------------------------------------------------------------%

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
    list(compiler_format_spec)::in, prog_context::in, prog_var::in,
    hlds_goal::out, var_table::in, var_table::out) is det.

create_string_format_replacement(ModuleInfo, Specs, Context, ResultVar,
        ReplacementGoal, !VarTable) :-
    replace_string_format(ModuleInfo, Specs, Context, yes(ResultVar),
        ActualResultVar, Goals, ValueVars, !VarTable),
    ( if ActualResultVar = ResultVar then
        AllGoals = Goals
    else
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
        Context, GoalInfo),
    conj_list_to_goal(AllGoals, GoalInfo, ReplacementGoal).

:- pred replace_string_format(module_info::in, list(compiler_format_spec)::in,
    prog_context::in, maybe(prog_var)::in, prog_var::out, list(hlds_goal)::out,
    set_of_progvar::out, var_table::in, var_table::out) is det.

replace_string_format(ModuleInfo, Specs, Context, MaybeResultVar, ResultVar,
        Goals, !:ValueVars, !VarTable) :-
    set_of_var.init(!:ValueVars),
    (
        Specs = [],
        make_result_var_if_needed(MaybeResultVar, ResultVar, !VarTable),
        make_string_const_construction(Context, ResultVar, "", Goal),
        Goals = [Goal]
    ;
        Specs = [HeadSpec | TailSpecs],
        replace_string_format_nonempty(ModuleInfo,
            HeadSpec, TailSpecs, Context, MaybeResultVar, ResultVar, Goals,
            !ValueVars, !VarTable)
    ).

:- pred replace_string_format_nonempty(module_info::in,
    compiler_format_spec::in, list(compiler_format_spec)::in,
    prog_context::in, maybe(prog_var)::in, prog_var::out, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    var_table::in, var_table::out) is det.

replace_string_format_nonempty(ModuleInfo, HeadSpec, TailSpecs,
        Context, MaybeResultVar, ResultVar, Goals, !ValueVars, !VarTable) :-
    (
        TailSpecs = [],
        represent_spec(ModuleInfo, HeadSpec, MaybeResultVar,
            ResultVar, Goals, _HeadSpecContext, !ValueVars, !VarTable)
    ;
        TailSpecs = [FirstTailSpec | LaterTailSpecs],
        replace_string_format_nonempty(ModuleInfo,
            FirstTailSpec, LaterTailSpecs, Context,
            no, TailSpecsVar, TailSpecsGoals, !ValueVars, !VarTable),
        represent_spec(ModuleInfo, HeadSpec, no, HeadSpecVar, HeadSpecGoals,
            _HeadSpecContext, !ValueVars, !VarTable),
        make_result_var_if_needed(MaybeResultVar, ResultVar, !VarTable),
        generate_plain_call(ModuleInfo, pf_function,
            mercury_string_module, "++",
            [], [HeadSpecVar, TailSpecsVar, ResultVar],
            instmap_delta_from_assoc_list(
                [ResultVar - ground(unique, none_or_default_func)]),
            only_mode, detism_det, purity_pure, [], Context, AppendGoal),
        Goals = TailSpecsGoals ++ HeadSpecGoals ++ [AppendGoal]
    ).

%---------------------------------------------------------------------------%

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
    % output stream in each of those calls to io.write_string/3. Factoring out
    % this hidden repetition could be worthwhile, but if it is, then
    % it should also be worth doing for code explicitly written by the user,
    % so this is not the place to worry about it; instead, it could be done
    % by a later optimization pass.
    %
    % Passing streams explicitly makes the code more robust against changes
    % anyway, which is another reason why the --warn-implicit-stream-calls
    % option encourages programmers to do that.
    %
:- pred create_io_format_replacement(module_info::in,
    list(compiler_format_spec)::in, prog_context::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in, hlds_goal::out,
    var_table::in, var_table::out) is det.

create_io_format_replacement(ModuleInfo, Specs, Context, MaybeStreamVar,
        IOInVar, IOOutVar, ReplacementGoal, !VarTable) :-
    replace_io_format(ModuleInfo, Specs, MaybeStreamVar,
        IOInVar, IOOutVar, Goals, ValueVars, !VarTable),

    make_di_uo_instmap_delta(IOInVar, IOOutVar, InstMapDelta),
    set_of_var.insert_list([IOInVar, IOOutVar], ValueVars, NonLocals),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
        Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, ReplacementGoal).

:- pred replace_io_format(module_info::in, list(compiler_format_spec)::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in, list(hlds_goal)::out,
    set_of_progvar::out, var_table::in, var_table::out) is det.

replace_io_format(ModuleInfo, Specs, MaybeStreamVar, IOInVar, IOOutVar,
        Goals, !:ValueVars, !VarTable) :-
    set_of_var.init(!:ValueVars),
    (
        Specs = [],
        Unification = assign(IOOutVar, IOInVar),
        Uniq = ground(unique, none_or_default_func),
        Clobbered = ground(clobbered, none_or_default_func),
        UnifyMode = unify_modes_li_lf_ri_rf(free, Uniq, Uniq, Clobbered),
        UnifyMainContext = umc_implicit("replace_io_format"),
        UnifyContext = unify_context(UnifyMainContext, []),
        GoalExpr = unify(IOOutVar, rhs_var(IOInVar), UnifyMode, Unification,
            UnifyContext),
        make_di_uo_instmap_delta(IOInVar, IOOutVar, InstMapDelta),
        goal_info_init(set_of_var.list_to_set([IOInVar, IOOutVar]),
            InstMapDelta, detism_det, purity_pure, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        Goals = [Goal]
    ;
        Specs = [HeadSpec | TailSpecs],
        replace_io_format_nonempty(ModuleInfo, HeadSpec, TailSpecs,
            MaybeStreamVar, IOInVar, IOOutVar, Goals,
            !ValueVars, !VarTable)
    ).

:- pred replace_io_format_nonempty(module_info::in,
    compiler_format_spec::in, list(compiler_format_spec)::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    var_table::in, var_table::out) is det.

replace_io_format_nonempty(ModuleInfo, HeadSpec, TailSpecs, MaybeStreamVar,
        IOInVar, IOOutVar, Goals, !ValueVars, !VarTable) :-
    (
        TailSpecs = [],
        replace_one_io_format(ModuleInfo, HeadSpec, MaybeStreamVar,
            IOInVar, IOOutVar, Goals, !ValueVars, !VarTable)
    ;
        TailSpecs = [FirstTailSpec | LaterTailSpecs],
        IOMidVarEntry = vte("", io_state_type, is_dummy_type),
        add_var_entry(IOMidVarEntry, IOMidVar, !VarTable),
        replace_one_io_format(ModuleInfo, HeadSpec, MaybeStreamVar,
            IOInVar, IOMidVar, HeadSpecGoals, !ValueVars, !VarTable),
        replace_io_format_nonempty(ModuleInfo, FirstTailSpec, LaterTailSpecs,
            MaybeStreamVar, IOMidVar, IOOutVar, TailSpecsGoals,
            !ValueVars, !VarTable),
        Goals = HeadSpecGoals ++ TailSpecsGoals
    ).

:- pred replace_one_io_format(module_info::in, compiler_format_spec::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in, list(hlds_goal)::out,
    set_of_progvar::in, set_of_progvar::out,
    var_table::in, var_table::out) is det.

replace_one_io_format(ModuleInfo, Spec, MaybeStreamVar,
        IOInVar, IOOutVar, Goals, !ValueVars, !VarTable) :-
    represent_spec(ModuleInfo, Spec, no, SpecVar, SpecGoals, SpecContext,
        !ValueVars, !VarTable),
    (
        MaybeStreamVar = yes(StreamVar),
        ArgVars = [StreamVar, SpecVar, IOInVar, IOOutVar]
    ;
        MaybeStreamVar = no,
        ArgVars = [SpecVar, IOInVar, IOOutVar]
    ),
    make_di_uo_instmap_delta(IOInVar, IOOutVar, InstMapDelta),
    generate_plain_call(ModuleInfo, pf_predicate,
        mercury_io_module, "write_string", [], ArgVars, InstMapDelta,
        only_mode, detism_det, purity_pure,
        [feature_do_not_warn_implicit_stream], SpecContext, CallGoal),
    Goals = SpecGoals ++ [CallGoal].

%---------------------------------------------------------------------------%

    % For optimizing e.g.
    %   stream.string_writer.format(Stream, "%3d_%.5x",
    %       [i(X), i(Y)], State0, State),
    % generate code that looks like this:
    %
    %   ... optimized code for string.format("%dd_%.5x", [i(X), i(Y)], FS) ...
    %   stream.put(Stream, FS, State0, State)
    %
    % This mimics the current implementation of stream.string_writer.format
    % in the runtime.
    %
    % XXX We should investigate whether switching to an approach similar
    % to the one we follow for io.format, which in this case would mean
    % invoking put on each component as soon as it is formatted, would
    % yield faster code. We can answer that question only with access
    % to a representative sample of code that uses stream.string_writer.format.
    %
:- pred create_stream_string_writer_format_replacement(module_info::in,
    list(compiler_format_spec)::in, prog_context::in,
    prog_var::in, prog_var::in, prog_var::in, prog_var::in, hlds_goal::out,
    var_table::in, var_table::out) is det.

create_stream_string_writer_format_replacement(ModuleInfo, Specs, Context,
        TC_InfoVarForStream, StreamVar, StateInVar, StateOutVar,
        ReplacementGoal, !VarTable) :-
    replace_string_format(ModuleInfo, Specs, Context, no, ResultVar,
        StringFormatGoals, ValueVars, !VarTable),
    make_di_uo_instmap_delta(StateInVar, StateOutVar, InstMapDelta),
    generate_plain_call(ModuleInfo, pf_predicate, mercury_stream_module, "put",
        [TC_InfoVarForStream], [StreamVar, ResultVar, StateInVar, StateOutVar],
        InstMapDelta, only_mode, detism_det, purity_pure,
        [feature_do_not_warn_implicit_stream], Context, CallGoal),
    Goals = StringFormatGoals ++ [CallGoal],

    set_of_var.insert_list([TC_InfoVarForStream, StreamVar,
        StateInVar, StateOutVar], ValueVars, NonLocals),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
        Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, ReplacementGoal).

%---------------------------------------------------------------------------%

:- pred represent_spec(module_info::in, compiler_format_spec::in,
    maybe(prog_var)::in, prog_var::out, list(hlds_goal)::out,
    prog_context::out, set_of_progvar::in, set_of_progvar::out,
    var_table::in, var_table::out) is det.

represent_spec(ModuleInfo, Spec, MaybeResultVar, ResultVar, Goals, Context,
        !ValueVars, !VarTable) :-
    (
        Spec = compiler_const_string(Context, StringConstant),
        make_result_var_if_needed(MaybeResultVar, ResultVar, !VarTable),
        make_string_const_construction(Context, ResultVar, StringConstant,
            Goal),
        Goals = [Goal]
    ;
        Spec = compiler_spec_char(Context, Flags, MaybeWidth, ValueVar),
        set_of_var.insert(ValueVar, !ValueVars),
        make_result_var_if_needed(MaybeResultVar, ResultVar, !VarTable),
        build_flags_arg(Context, Flags, FlagsVar, FlagsGoals, !VarTable),
        maybe_build_width_arg(MaybeWidth, WidthSuffix, WidthVars, WidthGoals,
            !VarTable),
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_string_format_module,
            "format_char_component" ++ WidthSuffix,
            [], [FlagsVar] ++ WidthVars ++ [ValueVar, ResultVar],
            instmap_delta_bind_var(ResultVar), only_mode,
            detism_det, purity_pure, [], Context, CallGoal),
        Goals = FlagsGoals ++ WidthGoals ++ [CallGoal]
    ;
        Spec = compiler_spec_string(Context, Flags,
            MaybeWidth, MaybePrec, ValueVar),
        set_of_var.insert(ValueVar, !ValueVars),
        ( if
            % Optimize a common case, "%s", which can be handled *without*
            % invoking the component formatter on the string value at all.
            Flags = string_format_flags(flag_hash_clear, flag_space_clear,
                flag_zero_clear, flag_minus_clear, flag_plus_clear),
            MaybeWidth = compiler_no_specified_width,
            MaybePrec = compiler_no_specified_prec
        then
            (
                MaybeResultVar = no,
                ResultVar = ValueVar,
                Goals = []
            ;
                MaybeResultVar = yes(ResultVar),
                make_simple_assign(ResultVar, ValueVar,
                    umc_implicit("represent_spec"), [], AssignGoal),
                Goals = [AssignGoal]
            )
        else
            make_result_var_if_needed(MaybeResultVar, ResultVar, !VarTable),
            build_flags_arg(Context, Flags, FlagsVar, FlagsGoals, !VarTable),
            maybe_build_width_arg(MaybeWidth, WidthSuffix, WidthVars,
                WidthGoals, !VarTable),
            maybe_build_prec_arg(MaybePrec, PrecSuffix, PrecVars, PrecGoals,
                !VarTable),
            generate_plain_call(ModuleInfo, pf_predicate,
                mercury_string_format_module,
                "format_string_component" ++ WidthSuffix ++ PrecSuffix, [],
                [FlagsVar] ++ WidthVars ++ PrecVars ++ [ValueVar, ResultVar],
                instmap_delta_bind_var(ResultVar), only_mode,
                detism_det, purity_pure, [], Context, CallGoal),
            Goals = FlagsGoals ++ WidthGoals ++ PrecGoals ++ [CallGoal]
        )
    ;
        (
            Spec = compiler_spec_signed_int(Context, Flags,
                MaybeWidth, MaybePrec, IntSize, OrigValueVar),
            % Format a signed int as signed int.
            BaseVars = [],
            BaseGoals = [],
            ( if IntSize = int_size_64 then
                FormatPredBase = "format_signed_int64_component"
            else
                FormatPredBase = "format_signed_int_component"
            ),
            cast_int_value_var_if_needed(ModuleInfo, Context, IntSize,
                OrigValueVar, ValueVar, ValueCastGoals, !VarTable)
        ;
            Spec = compiler_spec_unsigned_int(Context, Flags,
                MaybeWidth, MaybePrec, Base, IntSize, OrigValueVar),
            % Format a signed int as unsigned int.
            build_int_base_arg(Base, BaseVars, BaseGoals, !VarTable),
            ( if IntSize = int_size_64 then
                FormatPredBase = "format_unsigned_int64_component"
            else
                FormatPredBase = "format_unsigned_int_component"
            ),
            cast_int_value_var_if_needed(ModuleInfo, Context, IntSize,
                OrigValueVar, ValueVar, ValueCastGoals, !VarTable)
        ;
            Spec = compiler_spec_uint(Context, Flags,
                MaybeWidth, MaybePrec, Base, UIntSize, OrigValueVar),
            % Format an unsigned int as unsigned int.
            build_int_base_arg(Base, BaseVars, BaseGoals, !VarTable),
            ( if UIntSize = uint_size_64 then
                FormatPredBase = "format_uint64_component"
            else
                FormatPredBase = "format_uint_component"
            ),
            cast_uint_value_var_if_needed(ModuleInfo, Context, UIntSize,
                OrigValueVar, ValueVar, ValueCastGoals, !VarTable)
        ),
        set_of_var.insert(OrigValueVar, !ValueVars),
        make_result_var_if_needed(MaybeResultVar, ResultVar, !VarTable),
        build_flags_arg(Context, Flags, FlagsVar, FlagsGoals, !VarTable),
        maybe_build_width_arg(MaybeWidth, WidthSuffix, WidthVars, WidthGoals,
            !VarTable),
        maybe_build_prec_arg(MaybePrec, PrecSuffix, PrecVars, PrecGoals,
            !VarTable),
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_string_format_module,
            FormatPredBase ++ WidthSuffix ++ PrecSuffix,
            [], [FlagsVar] ++ WidthVars ++ PrecVars ++ BaseVars ++
                [ValueVar, ResultVar],
            instmap_delta_bind_var(ResultVar), only_mode,
            detism_det, purity_pure, [], Context, CallGoal),
        Goals = ValueCastGoals ++ FlagsGoals ++ WidthGoals ++ PrecGoals ++
            BaseGoals ++ [CallGoal]
    ;
        Spec = compiler_spec_float(Context, Flags,
            MaybeWidth, MaybePrec, Kind, ValueVar),
        set_of_var.insert(ValueVar, !ValueVars),
        make_result_var_if_needed(MaybeResultVar, ResultVar, !VarTable),
        build_flags_arg(Context, Flags, FlagsVar, FlagsGoals, !VarTable),
        maybe_build_width_arg(MaybeWidth, WidthSuffix, WidthVars, WidthGoals,
            !VarTable),
        maybe_build_prec_arg(MaybePrec, PrecSuffix, PrecVars, PrecGoals,
            !VarTable),
        build_float_kind_arg(Kind, KindVar, KindGoal, !VarTable),
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_string_format_module,
            "format_float_component" ++ WidthSuffix ++ PrecSuffix,
            [], [FlagsVar] ++ WidthVars ++ PrecVars ++
                [KindVar, ValueVar, ResultVar],
            instmap_delta_bind_var(ResultVar), only_mode,
            detism_det, purity_pure, [], Context, CallGoal),
        Goals = FlagsGoals ++ WidthGoals ++ PrecGoals ++ [KindGoal, CallGoal]
    ).

:- pred cast_int_value_var_if_needed(module_info::in, prog_context::in,
    int_size::in, prog_var::in, prog_var::out, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

cast_int_value_var_if_needed(ModuleInfo, Context, IntSize,
        OrigValueVar, ValueVar, ValueCastGoals, !VarTable) :-
    (
        ( IntSize = int_size_word
        ; IntSize = int_size_64
        ),
        ValueVar = OrigValueVar,
        ValueCastGoals = []
    ;
        ( IntSize = int_size_8, Size = "8"
        ; IntSize = int_size_16, Size = "16"
        ; IntSize = int_size_32, Size = "32"
        ),
        ValueVarEntry = vte("", int_type, is_not_dummy_type),
        add_var_entry(ValueVarEntry, ValueVar, !VarTable),
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_string_format_module,
            "format_cast_int" ++ Size ++ "_to_int",
            [], [OrigValueVar, ValueVar],
            instmap_delta_bind_var(ValueVar), only_mode,
            detism_det, purity_pure, [], Context, ValueCastGoal),
        ValueCastGoals = [ValueCastGoal]
    ).

:- pred cast_uint_value_var_if_needed(module_info::in, prog_context::in,
    uint_size::in, prog_var::in, prog_var::out, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

cast_uint_value_var_if_needed(ModuleInfo, Context, UIntSize,
        OrigValueVar, ValueVar, ValueCastGoals, !VarTable) :-
    (
        ( UIntSize = uint_size_word
        ; UIntSize = uint_size_64
        ),
        ValueVar = OrigValueVar,
        ValueCastGoals = []
    ;
        ( UIntSize = uint_size_8, Size = "8"
        ; UIntSize = uint_size_16, Size = "16"
        ; UIntSize = uint_size_32, Size = "32"
        ),
        ValueVarEntry = vte("", uint_type, is_not_dummy_type),
        add_var_entry(ValueVarEntry, ValueVar, !VarTable),
        generate_plain_call(ModuleInfo, pf_predicate,
            mercury_string_format_module,
            "format_cast_uint" ++ Size ++ "_to_uint",
            [], [OrigValueVar, ValueVar],
            instmap_delta_bind_var(ValueVar), only_mode,
            detism_det, purity_pure, [], Context, ValueCastGoal),
        ValueCastGoals = [ValueCastGoal]
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
    % While this looks like a lot, it should actually compile down into
    % a single machine instruction, due to the packing of enums inside
    % structures.
    %
:- pred build_flags_arg(prog_context::in, string_format_flags::in,
    prog_var::out, list(hlds_goal)::out, var_table::in, var_table::out) is det.

build_flags_arg(Context, Flags, Var, Goals, !VarTable) :-
    Flags = string_format_flags(FlagHash, FlagSpace, FlagZero,
        FlagMinus, FlagPlus),
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
    EntryHash  = vte("", TypeHash,  is_not_dummy_type),
    EntrySpace = vte("", TypeSpace, is_not_dummy_type),
    EntryZero  = vte("", TypeZero,  is_not_dummy_type),
    EntryMinus = vte("", TypeMinus, is_not_dummy_type),
    EntryPlus  = vte("", TypePlus,  is_not_dummy_type),
    add_var_entry(EntryHash,  VarHash,  !VarTable),
    add_var_entry(EntrySpace, VarSpace, !VarTable),
    add_var_entry(EntryZero,  VarZero,  !VarTable),
    add_var_entry(EntryMinus, VarMinus, !VarTable),
    add_var_entry(EntryPlus,  VarPlus,  !VarTable),
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
    make_const_construction(Context, VarHash,  ConsIdHash,  GoalHash),
    make_const_construction(Context, VarSpace, ConsIdSpace, GoalSpace),
    make_const_construction(Context, VarZero,  ConsIdZero,  GoalZero),
    make_const_construction(Context, VarMinus, ConsIdMinus, GoalMinus),
    make_const_construction(Context, VarPlus,  ConsIdPlus,  GoalPlus),

    TypeNameCombine = qualified(ParseUtil, "string_format_flags"),
    TypeCombine = defined_type(TypeNameCombine, [], kind_star),
    CombineVarEntry = vte("", TypeCombine, is_not_dummy_type),
    add_var_entry(CombineVarEntry, Var, !VarTable),

    TypeCtorCombine = type_ctor(TypeNameCombine, 0),
    ConsSymNameCombine = qualified(ParseUtil, "string_format_flags"),
    ConsIdCombine = cons(ConsSymNameCombine, 5, TypeCtorCombine),
    SpecVars = [VarHash, VarSpace, VarZero, VarMinus, VarPlus],
    construct_functor(Var, ConsIdCombine, SpecVars, GoalCombine),

    Goals = [GoalHash, GoalSpace, GoalZero, GoalMinus, GoalPlus, GoalCombine].

    % Decide whether we have a specified width.
    %
    % If yes, return both the variable that represents the specified width
    % and the goals that construct it.
    %
:- pred maybe_build_width_arg(compiler_format_maybe_width::in,
    string::out, list(prog_var)::out, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

maybe_build_width_arg(MaybeWidth, PredNameSuffix, MaybeWidthVar,
        MaybeWidthGoals, !VarTable) :-
    (
        MaybeWidth = compiler_no_specified_width,
        PredNameSuffix = "_nowidth",
        MaybeWidthVar = [],
        MaybeWidthGoals = []
    ;
        MaybeWidth = compiler_manifest_width(WidthInt),
        PredNameSuffix = "_width",
        make_int_const_construction_alloc(WidthInt, "", WidthGoal, WidthVar,
            !VarTable),
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
    var_table::in, var_table::out) is det.

maybe_build_prec_arg(MaybePrec, PredNameSuffix, MaybePrecVar,
        MaybePrecGoals, !VarTable) :-
    (
        MaybePrec = compiler_no_specified_prec,
        PredNameSuffix = "_noprec",
        MaybePrecVar = [],
        MaybePrecGoals = []
    ;
        MaybePrec = compiler_manifest_prec(PrecInt),
        PredNameSuffix = "_prec",
        make_int_const_construction_alloc(PrecInt, "", PrecGoal, PrecVar,
            !VarTable),
        MaybePrecVar = [PrecVar],
        MaybePrecGoals = [PrecGoal]
    ;
        MaybePrec = compiler_var_prec(PrecVar),
        PredNameSuffix = "_prec",
        MaybePrecVar = [PrecVar],
        MaybePrecGoals = []
    ).

:- pred build_int_base_arg(string_format_int_base::in,
    list(prog_var)::out, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

build_int_base_arg(Base, [Var], [Goal], !VarTable) :-
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
    make_const_construction_alloc(ConsId, Type, is_not_dummy_type, "",
        Goal, Var, !VarTable).

:- pred build_float_kind_arg(string_format_float_kind::in,
    prog_var::out, hlds_goal::out, var_table::in, var_table::out) is det.

build_float_kind_arg(Kind, Var, Goal, !VarTable) :-
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
    make_const_construction_alloc(ConsId, Type, is_not_dummy_type, "",
        Goal, Var, !VarTable).

:- pred make_result_var_if_needed(maybe(prog_var)::in, prog_var::out,
    var_table::in, var_table::out) is det.

make_result_var_if_needed(MaybeResultVar, ResultVar, !VarTable) :-
    (
        MaybeResultVar = yes(ResultVar)
    ;
        MaybeResultVar = no,
        Entry = vte("", string_type, is_not_dummy_type),
        add_var_entry(Entry, ResultVar, !VarTable)
    ).

:- pred make_di_uo_instmap_delta(prog_var::in, prog_var::in,
    instmap_delta::out) is det.

make_di_uo_instmap_delta(InVar, OutVar, InstMapDelta) :-
    Uniq = ground(unique, none_or_default_func),
    Clobbered = ground(clobbered, none_or_default_func),
    InstMapDelta = instmap_delta_from_assoc_list(
        [InVar - Clobbered, OutVar - Uniq]).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.format_call.
%---------------------------------------------------------------------------%
