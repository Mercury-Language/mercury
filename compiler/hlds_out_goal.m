%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_out_goal.m.
% Main authors: conway, fjh.
%
% There is quite a bit of overlap between the following modules:
%
%   the submodules of hlds_out.m, especially this one
%   parse_tree_out_*.m
%   term_io.m
%
% parse_tree_out_*.m prints the parse tree data structure defined
% in prog_data.m. hlds_out.m does a similar task, but for the data structure
% defined in hlds.m. term_io.m prints terms.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_goal.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module libs.
:- import_module libs.indent.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module string.
:- import_module string.builder.

%---------------------------------------------------------------------------%

    % Print a goal in a way that is suitable for debugging the compiler
    % (but necessarily for anything else).
    %
    % If what you are after is a short, less-than-one-line description
    % of a goal, then look in hlds_desc.m.
    %
:- pred dump_goal(io.text_output_stream::in, module_info::in,
    var_name_source::in, tvarset::in, inst_varset::in, hlds_goal::in,
    io::di, io::uo) is det.

    % Print a goal in a way that is suitable for debugging the compiler
    % (but necessarily for anything else), followed by a newline.
    %
    % If what you are after is a short, less-than-one-line description
    % of a goal, then look in hlds_desc.m.
    %
:- pred dump_goal_nl(io.text_output_stream::in, module_info::in,
    var_name_source::in, tvarset::in, inst_varset::in, hlds_goal::in,
    io::di, io::uo) is det.

    % Print out an HLDS goal. The integer gives the level of indentation
    % to be used within the goal. The string says what should end the line
    % containing the goal; it should include a newline character, but may
    % also contain other characters before that.
    %
:- pred write_goal(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, var_name_print::in,
    tvarset::in, inst_varset::in, indent::in, string::in, hlds_goal::in,
    io::di, io::uo) is det.
:- pred format_goal(hlds_out_info::in, module_info::in,
    var_name_source::in, var_name_print::in,
    tvarset::in, inst_varset::in, indent::in, string::in, hlds_goal::in,
    string.builder.state::di, string.builder.state::uo) is det.

    % As write_goal, but add a newline at the end.
    %
:- pred write_goal_nl(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, var_name_print::in,
    tvarset::in, inst_varset::in, indent::in, string::in, hlds_goal::in,
    io::di, io::uo) is det.
:- pred format_goal_nl(hlds_out_info::in, module_info::in,
    var_name_source::in, var_name_print::in,
    tvarset::in, inst_varset::in, indent::in, string::in, hlds_goal::in,
    string.builder.state::di, string.builder.state::uo) is det.

    % As write_goal, but for a list of goals.
    %
:- pred write_goal_list(hlds_out_info_goal::in, io.text_output_stream::in,
    indent::in, string::in, list(hlds_goal)::in, io::di, io::uo) is det.
:- pred format_goal_list(hlds_out_info_goal::in,
    indent::in, string::in, list(hlds_goal)::in,
    string.builder.state::di, string.builder.state::uo) is det.

%---------------------------------------------------------------------------%

:- type hlds_out_info_goal
    --->    hlds_out_info_goal(
                hoig_out_info       :: hlds_out_info,
                hoig_module_info    :: module_info,
                hoig_var_name_src   :: var_name_source,
                hoig_var_name_print :: var_name_print,
                hoig_tvarset        :: tvarset,
                hoig_inst_varset    :: inst_varset,
                % This should be tvarset_var_table(TVarset, VarTable)
                % if all constructors should be module qualified.
                %
                % XXX Having the tvarset potentially be present twice
                % (always in the hoig_tvarset field, and possibly in the
                % hoig_type_qual field) is not ideal, but the two fields
                % roles are quite separate.
                hoig_type_qual      :: type_qual
            ).

:- pred do_write_goal(hlds_out_info_goal::in, io.text_output_stream::in,
    indent::in, string::in, hlds_goal::in, io::di, io::uo) is det.
:- pred do_format_goal(hlds_out_info_goal::in, indent::in, string::in,
    hlds_goal::in, string.builder.state::di, string.builder.state::uo) is det.

%---------------------------------------------------------------------------%

    % Write out the mapping of variables to their abstract locations.
    %
:- pred write_var_to_abs_locns(io.text_output_stream::in,
    var_name_source::in, var_name_print::in, indent::in,
    assoc_list(prog_var, abs_locn)::in, io::di, io::uo) is det.
:- pred format_var_to_abs_locns(var_name_source::in, var_name_print::in,
    indent::in, assoc_list(prog_var, abs_locn)::in,
    string.builder.state::di, string.builder.state::uo) is det.

%---------------------------------------------------------------------------%

    % Print out the right-hand-side of a unification. The module_info and
    % the varsets give the context of the rhs. The boolean says whether
    % variables should have their numbers appended to them. The integer
    % gives the level of indentation to be used within the rhs.
    %
:- pred write_unify_rhs(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, var_name_print::in,
    tvarset::in, inst_varset::in, indent::in, unify_rhs::in,
    io::di, io::uo) is det.
:- pred format_unify_rhs(hlds_out_info::in, module_info::in,
    var_name_source::in, var_name_print::in,
    tvarset::in, inst_varset::in, indent::in, unify_rhs::in,
    string.builder.state::di, string.builder.state::uo) is det.

    % Converts the right-hand-side of a unification to a string, similarly to
    % write_unify_rhs, but doesn't print any details for lambda goals.
    % The module_info and the varset give the context of the rhs. The boolean
    % says whether variables should have their numbers appended to them.
    %
:- func unify_rhs_to_string(module_info, var_table, var_name_print, unify_rhs)
    = string.

:- func case_to_string(hlds_out_info_goal, indent, prog_var, case) = string.

%---------------------------------------------------------------------------%

    % Given a tagged cons_id, return the name of the cons_id and the tag.
    %
:- pred project_cons_name_and_tag(tagged_cons_id::in, string::out,
    cons_tag::out) is det.

    % case_comment(VarName, MainConsName, OtherConsNames) = Comment:
    %
    % Create a comment describing the arm of the switch on VarName that covers
    % MainConsName and OtherConsNames.
    %
:- func case_comment(string, string, list(string)) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_ordering.
:- import_module hlds.error_msg_inst.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_tree_out_clause.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.parse_tree_output.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.write_error_spec.
:- import_module transform_hlds.
:- import_module transform_hlds.ctgc.
:- import_module transform_hlds.ctgc.util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module term.
:- import_module term_context.
:- import_module term_int.
:- import_module term_subst.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%

dump_goal(Stream, ModuleInfo, VarNameSrc, TVarSet, InstVarSet, Goal, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    Info = init_hlds_out_info(Globals, output_debug),
    VarNamePrint = print_name_and_num,
    TypeQual = no_tvarset_var_table,
    InfoGoal = hlds_out_info_goal(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, TypeQual),
    Indent = 0u,
    Follow = "",
    do_write_goal(InfoGoal, Stream, Indent, Follow, Goal, !IO).

dump_goal_nl(Stream, ModuleInfo, VarNameSrc, TVarSet, InstVarSet, Goal, !IO) :-
    dump_goal(Stream, ModuleInfo, VarNameSrc, TVarSet, InstVarSet, Goal, !IO),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%

write_goal(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, !IO) :-
    State0 = string.builder.init,
    format_goal(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_goal(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, !State) :-
    % Do not type qualify everything.
    TypeQual = no_tvarset_var_table,
    InfoGoal = hlds_out_info_goal(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, TypeQual),
    do_format_goal(InfoGoal, Indent, Follow, Goal, !State).

%---------------------%

write_goal_nl(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, !IO) :-
    State0 = string.builder.init,
    format_goal_nl(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_goal_nl(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, !State) :-
    format_goal(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, !State),
    string.builder.append_string("\n", !State).

%---------------------%

write_goal_list(InfoGoal, Stream, Indent, Separator, Goals, !IO) :-
    State0 = string.builder.init,
    format_goal_list(InfoGoal, Indent, Separator, Goals, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_goal_list(InfoGoal, Indent, Separator, Goals, !State) :-
    (
        Goals = []
    ;
        Goals = [HeadGoal | TailGoals],
        format_indent2(Indent, !State),
        string.builder.append_string(Separator, !State),
        do_format_goal(InfoGoal, Indent + 1u, "\n", HeadGoal, !State),
        format_goal_list(InfoGoal, Indent, Separator, TailGoals, !State)
    ).

%---------------------------------------------------------------------------%

do_write_goal(InfoGoal, Stream, Indent, Follow, Goal, !IO) :-
    State0 = string.builder.init,
    do_format_goal(InfoGoal, Indent, Follow, Goal, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

do_format_goal(InfoGoal, Indent, Follow, Goal, !State) :-
    % Write out goal_infos in the form of annotations around goal expressions.
    IndentStr = indent2_string(Indent),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,

    DumpContexts = DumpOptions ^ dump_goal_type_contexts,
    (
        DumpContexts = yes,
        Context = goal_info_get_context(GoalInfo),
        maybe_format_context_comment(Indent, "", Context, !State)
    ;
        DumpContexts = no
    ),

    DumpGoalIdsPaths = DumpOptions ^ dump_goal_ids_paths,
    (
        DumpGoalIdsPaths = yes,
        GoalId = goal_info_get_goal_id(GoalInfo),
        ( if is_valid_goal_id(GoalId) then
            GoalId = goal_id(GoalIdNum),
            string.builder.format("%s%% goal id: %u\n",
                [s(IndentStr), u(GoalIdNum)], !State)
        else
            true
        )
    ;
        DumpGoalIdsPaths = no
    ),

    DumpNonLocals = DumpOptions ^ dump_goal_nonlocals,
    (
        DumpNonLocals = yes,
        NonLocalsSet = goal_info_get_nonlocals(GoalInfo),
        set_of_var.to_sorted_list(NonLocalsSet, NonLocalsList),
        (
            NonLocalsList = [_ | _],
            NonLocalsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                NonLocalsList),
            string.builder.format("%s%% nonlocals: %s\n",
                [s(IndentStr), s(NonLocalsStr)], !State)
        ;
            NonLocalsList = []
        )
    ;
        DumpNonLocals = no
    ),

    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    DumpGoalBirthsDeaths = DumpOptions ^ dump_goal_birth_death_sets,
    (
        DumpGoalBirthsDeaths = yes,
        ( if
            goal_info_maybe_get_pre_deaths(GoalInfo, PreDeaths),
            PreDeathList = set_of_var.to_sorted_list(PreDeaths),
            PreDeathList = [_ | _]
        then
            PreDeathStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                PreDeathList),
            string.builder.format("%s%% pre-deaths: %s\n",
                [s(IndentStr), s(PreDeathStr)], !State)
        else
            true
        ),
        ( if
            goal_info_maybe_get_pre_births(GoalInfo, PreBirths),
            PreBirthList = set_of_var.to_sorted_list(PreBirths),
            PreBirthList = [_ | _]
        then
            PreBirthStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                PreBirthList),
            string.builder.format("%s%% pre-births: %s\n",
                [s(IndentStr), s(PreBirthStr)], !State)
        else
            true
        )
    ;
        DumpGoalBirthsDeaths = no
    ),

    DumpModeConstraints = DumpOptions ^ dump_mode_constraints,
    (
        DumpModeConstraints = yes,
        goal_info_get_producing_vars(GoalInfo, ProducingVars),
        ( if set_of_var.is_non_empty(ProducingVars) then
            set_of_var.to_sorted_list(ProducingVars, ProducingVarsList),
            ProducingVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, ProducingVarsList),
            string.builder.format("%s%% producing vars: %s\n",
                [s(IndentStr), s(ProducingVarsStr)], !State)
        else
            true
        ),

        goal_info_get_consuming_vars(GoalInfo, ConsumingVars),
        ( if set_of_var.is_non_empty(ConsumingVars) then
            set_of_var.to_sorted_list(ConsumingVars, ConsumingVarsList),
            ConsumingVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, ConsumingVarsList),
            string.builder.format("%s%% consuming vars: %s\n",
                [s(IndentStr), s(ConsumingVarsStr)], !State)
        else
            true
        ),

        goal_info_get_make_visible_vars(GoalInfo, MakeVisibleVars),
        ( if set_of_var.is_non_empty(MakeVisibleVars) then
            set_of_var.to_sorted_list(MakeVisibleVars, MakeVisibleVarsList),
            MakeVisibleVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, MakeVisibleVarsList),
            string.builder.format("%s%% make_visible vars: %s\n",
                [s(IndentStr), s(MakeVisibleVarsStr)], !State)
        else
            true
        ),

        goal_info_get_need_visible_vars(GoalInfo, NeedVisibleVars),
        ( if set_of_var.is_non_empty(NeedVisibleVars) then
            set_of_var.to_sorted_list(NeedVisibleVars, NeedVisibleVarsList),
            NeedVisibleVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, NeedVisibleVarsList),
            string.builder.format("%s%% need_visible vars: %s\n",
                [s(IndentStr), s(NeedVisibleVarsStr)], !State)
        else
            true
        )
    ;
        DumpModeConstraints = no
    ),

    DumpDetism = DumpOptions ^ dump_goal_determinism,
    (
        DumpDetism = yes,
        Determinism = goal_info_get_determinism(GoalInfo),
        string.builder.format("%s%% determinism: %s\n",
            [s(IndentStr), s(determinism_to_string(Determinism))], !State)
    ;
        DumpDetism = no
    ),

    DumpRegions = DumpOptions ^ dump_region_annotations,
    (
        DumpRegions = yes,
        MaybeRbmmInfo = goal_info_get_maybe_rbmm(GoalInfo),
        (
            MaybeRbmmInfo = yes(RbmmInfo),
            RbmmInfo = rbmm_goal_info(Created, Removed, Carried, Alloc, Used),

            CreatedList = set.to_sorted_list(Created),
            RemovedList = set.to_sorted_list(Removed),
            CarriedList = set.to_sorted_list(Carried),
            AllocList = set.to_sorted_list(Alloc),
            UsedList = set.to_sorted_list(Used),

            CreatedStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                CreatedList),
            RemovedStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                RemovedList),
            CarriedStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                CarriedList),
            AllocStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                AllocList),
            UsedStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                UsedList),

            string.builder.format("%s%% Created regions: %s\n",
                [s(IndentStr), s(CreatedStr)], !State),
            string.builder.format("%s%% Removed regions: %s\n",
                [s(IndentStr), s(RemovedStr)], !State),
            string.builder.format("%s%% Carried regions: %s\n",
                [s(IndentStr), s(CarriedStr)], !State),
            string.builder.format("%s%% Allocated into regions: %s\n",
                [s(IndentStr), s(AllocStr)], !State),
            string.builder.format("%s%% Used regions: %s\n",
                [s(IndentStr), s(UsedStr)], !State)
        ;
            MaybeRbmmInfo = no
        )
    ;
        DumpRegions = no
    ),

    DumpPurity = DumpOptions ^ dump_goal_purity_markers,
    (
        DumpPurity = yes,
        Purity = goal_info_get_purity(GoalInfo),
        (
            Purity = purity_pure
        ;
            Purity = purity_semipure,
            string.builder.format("%s%% semipure\n", [s(IndentStr)], !State)
        ;
            Purity = purity_impure,
            string.builder.format("%s%% impure\n", [s(IndentStr)], !State)
        )
    ;
        DumpPurity = no
    ),

    DumpDeepProf = DumpOptions ^ dump_goal_purity_markers,
    (
        DumpDeepProf = yes,
        MaybeDPInfo = goal_info_get_maybe_dp_info(GoalInfo),
        (
            MaybeDPInfo = yes(dp_goal_info(MdprofInst, MaybeDPCoverageInfo)),
            (
                MdprofInst = goal_is_mdprof_inst,
                string.builder.format("%s%% mdprof instrumentation\n",
                    [s(IndentStr)], !State)
            ;
                MdprofInst = goal_is_not_mdprof_inst
            ),
            (
                MaybeDPCoverageInfo = yes(CoverageInfo),
                CoverageInfo = dp_coverage_goal_info(IsTrivial,
                    PortCountsGiveCoverageAfter),
                (
                    IsTrivial = goal_is_trivial,
                    string.builder.format("%s%% trivial goal\n",
                        [s(IndentStr)], !State)
                ;
                    IsTrivial = goal_is_nontrivial,
                    string.builder.format("%s%% nontrivial goal\n",
                        [s(IndentStr)], !State)
                ),
                (
                    PortCountsGiveCoverageAfter =
                        port_counts_give_coverage_after,
                    string.builder.format(
                        "%s%% port counts give coverage after\n",
                        [s(IndentStr)], !State)
                ;
                    PortCountsGiveCoverageAfter =
                        no_port_counts_give_coverage_after,
                    string.builder.format(
                        "%s%% no port counts give coverage after\n",
                        [s(IndentStr)], !State)
                )
            ;
                MaybeDPCoverageInfo = no
            )
        ;
            MaybeDPInfo = no
        )
    ;
        DumpDeepProf = no
    ),

    format_goal_expr(InfoGoal, Indent, Follow, GoalExpr, !State),

    DumpInstMapVars = DumpOptions ^ dump_goal_instmap_vars,
    (
        DumpInstMapVars = yes,
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_changed_vars(InstMapDelta, Vars),
        ( if
            % InstMapDelta will almost always be reachable,
            % so any failure is far more likely to come from is_empty failing.
            set_of_var.is_empty(Vars),
            instmap_delta_is_reachable(InstMapDelta)
        then
            true
        else
            DumpInstMapDeltas = DumpOptions ^ dump_goal_instmap_deltas,
            DumpInstMapIndent = DumpOptions ^ dump_structured_insts,
            (
                DumpInstMapDeltas = yes,
                ( if instmap_delta_is_unreachable(InstMapDelta) then
                    string.builder.format("%s%% new insts: unreachable\n",
                        [s(IndentStr)], !State)
                else
                    (
                        DumpInstMapIndent = yes,
                        instmap_delta_to_assoc_list(InstMapDelta, NewVarInsts),
                        NewVarInstStrs = list.map(
                            new_var_inst_msg_to_string(InfoGoal, IndentStr),
                            NewVarInsts),
                        string.builder.format("%s%% new insts:\n",
                            [s(IndentStr)], !State),
                        string.builder.append_strings(NewVarInstStrs, !State)
                    ;
                        DumpInstMapIndent = no,
                        instmap_delta_to_assoc_list(InstMapDelta, NewVarInsts),
                        NewVarInstStrs = list.map(
                            new_var_inst_to_string(InfoGoal, IndentStr),
                            NewVarInsts),
                        string.builder.format("%s%% new insts:\n",
                            [s(IndentStr)], !State),
                        string.builder.append_strings(NewVarInstStrs, !State)
                    )
                )
            ;
                DumpInstMapDeltas = no,
                ( if instmap_delta_is_unreachable(InstMapDelta) then
                    NewVarsStr = "unreachable"
                else
                    instmap_delta_to_assoc_list(InstMapDelta, NewVarInsts),
                    assoc_list.keys(NewVarInsts, NewVars),
                    NewVarStrs = list.map(
                        mercury_var_to_string_src(VarNameSrc, VarNamePrint),
                        NewVars),
                    NewVarsStr = string.join_list(", ", NewVarStrs)
                ),
                string.builder.format("%s%% vars with new insts: %s\n",
                    [s(IndentStr), s(NewVarsStr)], !State)
            )
        )

        % XXX Until work starts on the new constraint based mode system,
        % printing goal_modes would be just clutter.
%       GoalMode = goal_info_get_goal_mode(GoalInfo),
%       PrefixStr = indent_string(Indent) ++ "% ",
%       GoalModeStrs = dump_goal_mode(PrefixStr, VarNameSrc, GoalMode),
%       list.foldl(io.write_string(Stream), GoalModeStrs, !State)
    ;
        DumpInstMapVars = no
    ),

    (
        DumpGoalBirthsDeaths = yes,
        ( if
            goal_info_maybe_get_post_deaths(GoalInfo, PostDeaths),
            PostDeathList = set_of_var.to_sorted_list(PostDeaths),
            PostDeathList = [_ | _]
        then
            PostDeathStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                PostDeathList),
            string.builder.format("%s%% post-deaths: %s\n",
                [s(IndentStr), s(PostDeathStr)], !State)
        else
            true
        ),
        ( if
            goal_info_maybe_get_post_births(GoalInfo, PostBirths),
            PostBirthList = set_of_var.to_sorted_list(PostBirths),
            PostBirthList = [_ | _]
        then
            PostBirthStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                PostBirthList),
            string.builder.format("%s%% post-births: %s\n",
                [s(IndentStr), s(PostBirthStr)], !State)
        else
            true
        )
    ;
        DumpGoalBirthsDeaths = no
    ),

    DumpUseReuse = DumpOptions ^ dump_use_reuse_info,
    (
        DumpUseReuse = yes,
        ( if
            yes(LFU) = goal_info_get_maybe_lfu(GoalInfo),
            yes(LBU) = goal_info_get_maybe_lbu(GoalInfo),
            yes(ReuseDescription) = goal_info_get_maybe_reuse(GoalInfo)
        then
            set_of_var.to_sorted_list(LFU, ListLFU),
            set_of_var.to_sorted_list(LBU, ListLBU),
            StrLFU = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                ListLFU),
            StrLBU = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                ListLBU),

            string.builder.format("%s%% LFU: %s\n",
                [s(IndentStr), s(StrLFU)], !State),
            string.builder.format("%s%% LBU: %s\n",
                [s(IndentStr), s(StrLBU)], !State),

            string.builder.format("%s%% Reuse: ", [s(IndentStr)], !State),
            (
                ReuseDescription = no_reuse_info,
                string.builder.append_string("no reuse info", !State)
            ;
                ReuseDescription = no_possible_reuse,
                string.builder.append_string("no possible reuse", !State)
            ;
                ReuseDescription = missed_reuse(Messages),
                string.builder.append_string("missed (", !State),
                string.builder.append_strings_sep(", ", Messages, !State),
                string.builder.append_string(")", !State)
            ;
                ReuseDescription = potential_reuse(ShortReuseDescr),
                string.builder.append_string("potential reuse (", !State),
                format_short_reuse_description(ShortReuseDescr,
                    VarNameSrc, VarNamePrint, !State),
                string.builder.append_string(")", !State)
            ;
                ReuseDescription = reuse(ShortReuseDescr),
                string.builder.append_string("reuse (", !State),
                format_short_reuse_description(ShortReuseDescr,
                    VarNameSrc, VarNamePrint, !State),
                string.builder.append_string(")", !State)
            ),
            string.builder.append_string("\n", !State)
        else
            true
        )
    ;
        DumpUseReuse = no
    ),

    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    (
        CodeGenInfo = no_code_gen_info
    ;
        CodeGenInfo = llds_code_gen_info(_CodeGenDetails),
        format_llds_code_gen_info(Info, GoalInfo, VarNameSrc, VarNamePrint,
            Indent, !State)
    ),

    DumpGoalFeatures = DumpOptions ^ dump_goal_features,
    (
        DumpGoalFeatures = yes,
        Features = goal_info_get_features(GoalInfo),
        set.to_sorted_list(Features, FeatureList),
        (
            FeatureList = []
        ;
            FeatureList = [_ | _],
            string.builder.format("%s%% Goal features: %s\n",
                [s(IndentStr), s(string.string(FeatureList))], !State)
        )
    ;
        DumpGoalFeatures = no
    ).

:- func new_var_inst_to_string(hlds_out_info_goal, string,
    pair(prog_var, mer_inst)) = string.

new_var_inst_to_string(InfoGoal, IndentStr, Var - Inst) = Str :-
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
    varset.init(InstVarSet),
    InstStr = mercury_inst_to_string(output_debug, InstVarSet, Inst),
    string.format("%s%%   %s -> %s\n",
        [s(IndentStr), s(VarStr), s(InstStr)], Str).

:- func new_var_inst_msg_to_string(hlds_out_info_goal, string,
    pair(prog_var, mer_inst)) = string.

new_var_inst_msg_to_string(InfoGoal, IndentStr, Var - Inst) = Str :-
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
    ModuleInfo = InfoGoal ^ hoig_module_info,
    TVarSet = InfoGoal ^ hoig_tvarset,
    InstVarSet = InfoGoal ^ hoig_inst_varset,
    ShortInstPrefix = [],
    ShortInstSuffix = [nl],
    LongInstPrefix = [nl],
    LongInstSuffix = [nl],
    InstPieces = error_msg_inst(ModuleInfo, InstVarSet, expand_named_insts,
        uod_developer(TVarSet), fixed_short_inst,
        ShortInstPrefix, ShortInstSuffix,
        LongInstPrefix, LongInstSuffix, Inst),

    InstLines = error_pieces_to_std_lines(InstPieces),
    ( if do_lines_fit_in_n_code_points(40, InstLines) then
        ShortInstStr = error_lines_to_one_line_string(InstLines),
        string.format("%s%%   %s -> %s\n",
            [s(IndentStr), s(VarStr), s(ShortInstStr)], Str)
    else
        Prefix = IndentStr ++ "%   ",
        LongInstStr = error_lines_to_multi_line_string(Prefix, InstLines),
        string.format("%s%%   %s ->\n%s",
            [s(IndentStr), s(VarStr), s(LongInstStr)], Str)
    ).

:- pred format_llds_code_gen_info(hlds_out_info::in, hlds_goal_info::in,
    var_name_source::in, var_name_print::in, indent::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_llds_code_gen_info(Info, GoalInfo, VarNameSrc, VarNamePrint,
        Indent, !State) :-
    IndentStr = indent2_string(Indent),
    DumpOptions = Info ^ hoi_dump_hlds_options,

    DumpFollowVars = DumpOptions ^ dump_follow_vars,
    (
        DumpFollowVars = yes,
        goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
        (
            MaybeFollowVars = yes(FollowVars),
            FollowVars = abs_follow_vars(FollowVarsMap, NextRegR, NextRegF),
            map.to_assoc_list(FollowVarsMap, FVlist),

            string.builder.format("%s%% follow vars: r%d, f%d\n",
                [s(IndentStr), i(NextRegR), i(NextRegF)], !State),
            format_var_to_abs_locns(VarNameSrc, VarNamePrint, Indent,
                FVlist, !State)
        ;
            MaybeFollowVars = no
        )
    ;
        DumpFollowVars = no
    ),

    DumpResumePoints = DumpOptions ^ dump_goal_resume_points,
    (
        DumpResumePoints = yes,
        goal_info_get_resume_point(GoalInfo, Resume),
        (
            Resume = no_resume_point
        ;
            Resume = resume_point(ResumeVars, Locs),
            ResumeVarList = set_of_var.to_sorted_list(ResumeVars),
            ResumeVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, ResumeVarList),
            (
                Locs = resume_locs_orig_only,
                LocsStr = "orig only"
            ;
                Locs = resume_locs_stack_only,
                LocsStr = "stack only"
            ;
                Locs = resume_locs_orig_then_stack,
                LocsStr = "orig then stack"
            ;
                Locs = resume_locs_stack_then_orig,
                LocsStr = "stack then orig"
            ),
            string.builder.format("%s%% resume point %s %s\n",
                [s(IndentStr), s(LocsStr), s(ResumeVarsStr)], !State)
        )
    ;
        DumpResumePoints = no
    ),

    DumpStoreMap = DumpOptions ^ dump_goal_store_maps,
    (
        DumpStoreMap = yes,
        goal_info_get_store_map(GoalInfo, StoreMap),
        map.to_assoc_list(StoreMap, StoreMapList),
        (
            StoreMapList = [_ | _],
            string.builder.format("%s%% store map:\n", [s(IndentStr)], !State),
            format_var_to_abs_locns(VarNameSrc, VarNamePrint, Indent,
                StoreMapList, !State)
        ;
            StoreMapList = []
        ),
        goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall),
        (
            MaybeNeedAcrossCall = yes(NeedAcrossCall),
            NeedAcrossCall = need_across_call(CallForwardSet, CallResumeSet,
                CallNondetSet),
            CallForwardList = set_of_var.to_sorted_list(CallForwardSet),
            CallResumeList = set_of_var.to_sorted_list(CallResumeSet),
            CallNondetList = set_of_var.to_sorted_list(CallNondetSet),
            string.builder.format("%s%% need across call forward vars: ",
                [s(IndentStr)], !State),
            (
                CallForwardList = [],
                string.builder.append_string("none\n", !State)
            ;
                CallForwardList = [_ | _],
                mercury_format_vars_src(VarNameSrc, VarNamePrint,
                    CallForwardList, string.builder.handle, !State),
                string.builder.append_string("\n", !State)
            ),

            string.builder.format("%s%% need across call resume vars: ",
                [s(IndentStr)], !State),
            (
                CallResumeList = [],
                string.builder.append_string("none\n", !State)
            ;
                CallResumeList = [_ | _],
                mercury_format_vars_src(VarNameSrc, VarNamePrint,
                    CallResumeList, string.builder.handle, !State),
                string.builder.append_string("\n", !State)
            ),

            string.builder.format("%s%% need across call nondet vars: ",
                [s(IndentStr)], !State),
            (
                CallNondetList = [],
                string.builder.append_string("none\n", !State)
            ;
                CallNondetList = [_ | _],
                mercury_format_vars_src(VarNameSrc, VarNamePrint,
                    CallNondetList, string.builder.handle, !State),
                string.builder.append_string("\n", !State)
            )
        ;
            MaybeNeedAcrossCall = no
        ),
        goal_info_get_maybe_need_in_resume(GoalInfo, MaybeNeedInResume),
        (
            MaybeNeedInResume = yes(NeedInResume),
            NeedInResume = need_in_resume(ResumeOnStack, ResumeResumeSet,
                ResumeNondetSet),
            ResumeResumeList = set_of_var.to_sorted_list(ResumeResumeSet),
            ResumeNondetList = set_of_var.to_sorted_list(ResumeNondetSet),

            (
                ResumeOnStack = yes,
                string.builder.format("%s%% resume point has stack label\n",
                    [s(IndentStr)], !State)
            ;
                ResumeOnStack = no,
                string.builder.format("%s%% resume point has no stack label\n",
                    [s(IndentStr)], !State)
            ),
            string.builder.format("%s%% need in resume resume vars: ",
                [s(IndentStr)], !State),
            (
                ResumeResumeList = [],
                string.builder.append_string("none\n", !State)
            ;
                ResumeResumeList = [_ | _],
                mercury_format_vars_src(VarNameSrc, VarNamePrint,
                    ResumeResumeList, string.builder.handle, !State),
                string.builder.append_string("\n", !State)
            ),

            string.builder.format("%s%% need in resume nondet vars: ",
                [s(IndentStr)], !State),
            (
                ResumeNondetList = [],
                string.builder.append_string("none\n", !State)
            ;
                ResumeNondetList = [_ | _],
                mercury_format_vars_src(VarNameSrc, VarNamePrint,
                    ResumeNondetList, string.builder.handle, !State),
                string.builder.append_string("\n", !State)
            )
        ;
            MaybeNeedInResume = no
        ),
        goal_info_get_maybe_need_in_par_conj(GoalInfo, MaybeNeedInParConj),
        (
            MaybeNeedInParConj = yes(NeedInParConj),
            NeedInParConj = need_in_par_conj(ParConjSet),
            ParConjList = set_of_var.to_sorted_list(ParConjSet),
            string.builder.format("%s%% need in par_conj vars: ",
                [s(IndentStr)], !State),
            mercury_format_vars_src(VarNameSrc, VarNamePrint, ParConjList,
                string.builder.handle, !State),
            string.builder.append_string("\n", !State)
        ;
            MaybeNeedInParConj = no
        )
    ;
        DumpStoreMap = no
    ).

write_var_to_abs_locns(Stream, VarNameSrc, VarNamePrint, Indent,
        VarLocs, !IO) :-
    State0 = string.builder.init,
    format_var_to_abs_locns(VarNameSrc, VarNamePrint, Indent,
        VarLocs, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_var_to_abs_locns(_, _, _, [], !State).
format_var_to_abs_locns(VarNameSrc, VarNamePrint, Indent,
        [Var - Loc | VarLocs], !State) :-
    IndentStr = indent2_string(Indent),
    VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
    abs_locn_to_string(Loc, LocnStr, MaybeWidth),
    (
        MaybeWidth = no,
        WidthStr = ""
    ;
        MaybeWidth = yes(Width),
        WidthStr = " " ++ Width
    ),
    string.builder.format("%s%%\t%s\t-> %s%s\n",
        [s(IndentStr), s(VarStr), s(LocnStr), s(WidthStr)], !State),
    format_var_to_abs_locns(VarNameSrc, VarNamePrint, Indent,
        VarLocs, !State).

:- pred format_short_reuse_description(short_reuse_description::in,
    var_name_source::in, var_name_print::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_short_reuse_description(ShortDescription, VarNameSrc, VarNamePrint,
        !State):-
    (
        ShortDescription = cell_died,
        string.builder.append_string("cell died", !State)
    ;
        ShortDescription = cell_reused(Var, IsConditional, _, _),
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        string.builder.format("cell reuse - %s - %s",
            [s(VarStr), s(is_conditional_to_string(IsConditional))], !State)
    ;
        ShortDescription = reuse_call(IsConditional, NoClobbers),
        string.builder.format("reuse call - %s, no clobbers = %s",
            [s(is_conditional_to_string(IsConditional)),
            s(string.string(NoClobbers))], !State)
    ).

:- func is_conditional_to_string(is_conditional) = string.

is_conditional_to_string(IsConditional) = Str :-
    (
        IsConditional = conditional_reuse,
        Str = "with condition"
    ;
        IsConditional = unconditional_reuse,
        Str = "always safe"
    ).

%---------------------------------------------------------------------------%
%
% Write out goal expressions.
%

:- pred format_goal_expr(hlds_out_info_goal::in,
    indent::in, string::in, hlds_goal_expr::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_expr(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    (
        GoalExpr = unify(_, _, _, _, _),
        format_goal_unify(InfoGoal, Indent, Follow, GoalExpr, !State)
    ;
        GoalExpr = plain_call(_, _, _, _, _, _),
        format_goal_plain_call(InfoGoal, Indent, Follow, GoalExpr, !State)
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        format_goal_generic_call(InfoGoal, Indent, Follow,
            GoalExpr, !State)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        format_goal_foreign_proc(InfoGoal, Indent, Follow,
            GoalExpr, !State)
    ;
        GoalExpr = conj(_, _),
        format_goal_conj(InfoGoal, Indent, Follow, GoalExpr, !State)
    ;
        GoalExpr = disj(_),
        format_goal_disj(InfoGoal, Indent, Follow, GoalExpr, !State)
    ;
        GoalExpr = switch(_, _, _),
        format_goal_switch(InfoGoal, Indent, Follow, GoalExpr, !State)
    ;
        GoalExpr = scope(_, _),
        format_goal_scope(InfoGoal, Indent, Follow, GoalExpr, !State)
    ;
        GoalExpr = if_then_else(_, _, _, _),
        format_goal_if_then_else(InfoGoal, Indent, Follow,
            GoalExpr, !State)
    ;
        GoalExpr = negation(_),
        format_goal_negation(InfoGoal, Indent, Follow, GoalExpr, !State)
    ;
        GoalExpr = shorthand(_),
        format_goal_shorthand(InfoGoal, Indent, Follow, GoalExpr, !State)
    ).

%---------------------------------------------------------------------------%
%
% Write out unifications.
%

    % format_goal_unify(InfoGoal, Indent, Follow, GoalExpr, !State):
    %
    % Write out a unification.
    %
:- pred format_goal_unify(hlds_out_info_goal::in,
    indent::in, string::in, hlds_goal_expr::in(goal_expr_unify),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_unify(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = unify(LHS, RHS, _, Unification, _),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    IndentStr = indent2_string(Indent),

    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    LHSStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, LHS),
    string.builder.format("%s%s = ", [s(IndentStr), s(LHSStr)], !State),
    TypeQual = InfoGoal ^ hoig_type_qual,
    (
        TypeQual = tvarset_var_table(_, VarTable),
        lookup_var_type(VarTable, LHS, UniType),
        VarType = yes(UniType)
    ;
        TypeQual = no_tvarset_var_table,
        VarType = no
    ),
    format_unify_rhs_2(InfoGoal, Indent, VarType, RHS, !State),
    string.builder.append_string(Follow, !State),

    DumpUnifyDetails = DumpOptions ^ dump_unification_details,
    DumpGoalBirthsDeaths = DumpOptions ^ dump_goal_birth_death_sets,
    ( if ( DumpUnifyDetails = yes ; DumpGoalBirthsDeaths = yes ) then
        ( if
            % Don not output bogus info if we haven't been through
            % mode analysis yet.
            Unification = complicated_unify(ComplMode, CanFail, TypeInfoVars),
            CanFail = can_fail,
            ComplMode = unify_modes_li_lf_ri_rf(free, free, free, free),
            TypeInfoVars = []
        then
            true
        else
            % XXX While Follow strings should never contain newlines,
            % some callers do pass them.
            ( if string.contains_char(Follow, '\n') then
                true
            else
                string.builder.append_string("\n", !State)
            ),
            write_unification(InfoGoal, Indent, Unification, !State)
        )
    else
        true
    ).

write_unify_rhs(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, RHS, !IO) :-
    State0 = string.builder.init,
    format_unify_rhs(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, RHS, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_unify_rhs(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, RHS, !State) :-
    TypeQual = no_tvarset_var_table,
    InfoGoal = hlds_out_info_goal(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, TypeQual),
    format_unify_rhs_2(InfoGoal, Indent, no, RHS, !State).

:- pred format_unify_rhs_2(hlds_out_info_goal::in,
    indent::in, maybe(mer_type)::in, unify_rhs::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_unify_rhs_2(InfoGoal, Indent, MaybeType, RHS, !State) :-
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    (
        RHS = rhs_var(Var),
        mercury_format_var_src(VarNameSrc, VarNamePrint, Var,
            string.builder.handle, !State)
    ;
        RHS = rhs_functor(ConsId0, IsExistConstruct, ArgVars),
        ( if
            IsExistConstruct = is_exist_constr,
            ConsId0 = du_data_ctor(du_ctor(SymName0, Arity, TypeCtor))
        then
            add_new_prefix(SymName0, SymName),
            ConsId = du_data_ctor(du_ctor(SymName, Arity, TypeCtor))
        else
            ConsId = ConsId0
        ),
        ModuleInfo = InfoGoal ^ hoig_module_info,
        RHSStr = functor_cons_id_to_string(ModuleInfo, VarNameSrc,
            VarNamePrint, ConsId, ArgVars),
        string.builder.append_string(RHSStr, !State),
        ( if
            MaybeType = yes(Type),
            InfoGoal ^ hoig_type_qual = tvarset_var_table(TVarSet, _)
        then
            string.builder.append_string(" : ", !State),
            mercury_format_type(TVarSet, VarNamePrint, Type,
                string.builder.handle, !State)
        else
            true
        )
    ;
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc,
            NonLocals, VarsModes, Det, Goal),
        IndentStr = indent2_string(Indent),
        Indent1 = Indent + 1u,
        string.builder.append_string(purity_prefix_to_string(Purity), !State),
        Info = InfoGoal ^ hoig_out_info,
        Lang = get_output_lang(Info ^ hoi_merc_out_info),
        (
            PredOrFunc = pf_predicate,
            (
                Groundness = ho_ground,
                Functor = "pred"
            ;
                Groundness = ho_any,
                Functor = "any_pred"
            ),
            string.builder.append_string("(", !State),
            (
                VarsModes = [],
                string.builder.format("(%s)", [s(Functor)], !State)
            ;
                VarsModes = [_ | _],
                InstVarSet = InfoGoal ^ hoig_inst_varset,
                ModesStr = var_modes_to_string(Lang, VarNameSrc, InstVarSet,
                    VarNamePrint, VarsModes),
                string.builder.format("%s(%s)",
                    [s(Functor), s(ModesStr)], !State)
            ),
            DetStr = mercury_det_to_string(Det),
            string.builder.format(" is %s :-\n", [s(DetStr)], !State),
            do_format_goal(InfoGoal, Indent1, "\n", Goal, !State),
            string.builder.format("%s)", [s(IndentStr)], !State)
        ;
            PredOrFunc = pf_function,
            (
                Groundness = ho_ground,
                Functor = "func"
            ;
                Groundness = ho_any,
                Functor = "any_func"
            ),
            pred_args_to_func_args(VarsModes, ArgVarsModes, RetVarMode),
            string.builder.append_string("(", !State),
            InstVarSet = InfoGoal ^ hoig_inst_varset,
            (
                ArgVarsModes = [],
                string.builder.format("(%s)", [s(Functor)], !State)
            ;
                ArgVarsModes = [_ | _],
                ArgModesStr = var_modes_to_string(Lang, VarNameSrc, InstVarSet,
                    VarNamePrint, ArgVarsModes),
                string.builder.format("%s(%s)",
                    [s(Functor), s(ArgModesStr)], !State)
            ),
            RetModeStr = var_mode_to_string(Lang, VarNameSrc, InstVarSet,
                VarNamePrint, RetVarMode),
            DetStr = mercury_det_to_string(Det),
            string.builder.format(" = (%s) is %s :-\n",
                [s(RetModeStr), s(DetStr)], !State),
            do_format_goal(InfoGoal, Indent1, "\n", Goal, !State),
            string.builder.format("%s)", [s(IndentStr)], !State)
        ),
        ( if
            MaybeType = yes(Type),
            InfoGoal ^ hoig_type_qual = tvarset_var_table(TVarSet, _)
        then
            string.builder.append_string(" : ", !State),
            mercury_format_type(TVarSet, VarNamePrint, Type,
                string.builder.handle, !State)
        else
            true
        ),

        DumpOptions = Info ^ hoi_dump_hlds_options,
        DumpNonLocals = DumpOptions ^ dump_goal_nonlocals,
        (
            DumpNonLocals = yes,
            (
                NonLocals = [_ | _],
                NonLocalsStr = mercury_vars_to_string_src(VarNameSrc,
                    VarNamePrint, NonLocals),
                string.builder.format("\n%s%% lambda nonlocals: %s",
                    [s(IndentStr), s(NonLocalsStr)], !State)
            ;
                NonLocals = []
            )
        ;
            DumpNonLocals = no
        )
    ).

unify_rhs_to_string(ModuleInfo, VarTable, VarNamePrint, RHS) = Str :-
    (
        RHS = rhs_var(Var),
        Str = mercury_var_to_string(VarTable, VarNamePrint, Var)
    ;
        RHS = rhs_functor(ConsId0, IsExistConstruct, ArgVars),
        ( if
            IsExistConstruct = is_exist_constr,
            ConsId0 = du_data_ctor(du_ctor(SymName0, Arity, TypeCtor))
        then
            add_new_prefix(SymName0, SymName),
            ConsId = du_data_ctor(du_ctor(SymName, Arity, TypeCtor))
        else
            ConsId = ConsId0
        ),
        Str = functor_cons_id_to_string(ModuleInfo, vns_var_table(VarTable),
            VarNamePrint, ConsId, ArgVars)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _),
        Str = "lambda goal"
    ).

:- pred write_unification(hlds_out_info_goal::in, indent::in, unification::in,
    string.builder.state::di, string.builder.state::uo) is det.

write_unification(InfoGoal, Indent, Unification, !State) :-
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    IndentStr = indent2_string(Indent),
    (
        Unification = assign(X, Y),
        XStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, X),
        YStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Y),
        string.builder.format("%s%% %s := %s\n",
            [s(IndentStr), s(XStr), s(YStr)], !State)
    ;
        Unification = simple_test(X, Y),
        XStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, X),
        YStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Y),
        string.builder.format("%s%% %s == %s\n",
            [s(IndentStr), s(XStr), s(YStr)], !State)
    ;
        Unification = construct(Var, ConsId, ArgVars, ArgModes, ConstructHow,
            Uniqueness, SubInfo),
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        string.builder.format("%s%% %s <= ",
            [s(IndentStr), s(VarStr)], !State),
        format_functor_and_submodes(InfoGoal, Indent, ConsId,
            ArgVars, ArgModes, !State),

        Info = InfoGoal ^ hoig_out_info,
        DumpOptions = Info ^ hoi_dump_hlds_options,
        DumpUnifyDetails = DumpOptions ^ dump_unification_details,
        (
            DumpUnifyDetails = yes,
            ( if ConsId = du_data_ctor(du_ctor(_, _, TypeCtor)) then
                TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
                TypeCtorSymNameStr = sym_name_to_string(TypeCtorSymName),
                string.builder.format("%s%% cons_id type_ctor: %s/%d\n",
                    [s(IndentStr), s(TypeCtorSymNameStr), i(TypeCtorArity)],
                    !State)
            else
                true
            ),
            (
                Uniqueness = cell_is_unique,
                string.builder.format("%s%% cell_is_unique\n",
                    [s(IndentStr)], !State)
            ;
                Uniqueness = cell_is_shared
            ),
            (
                SubInfo = no_construct_sub_info
            ;
                SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize),
                (
                    MaybeTakeAddr = yes(TakeAddressFields),
                    string.builder.format("%s%% take address fields: %s\n",
                        [s(IndentStr), s(string.string(TakeAddressFields))],
                        !State)
                ;
                    MaybeTakeAddr = no
                ),
                (
                    MaybeSize = yes(SizeSource),
                    string.builder.format("%s%% term size ",
                        [s(IndentStr)], !State),
                    (
                        SizeSource = known_size(KnownSize),
                        string.builder.format("const %d\n",
                            [i(KnownSize)], !State)
                    ;
                        SizeSource = dynamic_size(SizeVar),
                        SizeVarStr = mercury_var_to_string_src(VarNameSrc,
                            VarNamePrint, SizeVar),
                        string.builder.format("var %s\n",
                            [s(SizeVarStr)], !State)
                    )
                ;
                    MaybeSize = no
                )
            ),
            (
                ConstructHow = construct_dynamically
            ;
                ConstructHow = construct_statically(born_static),
                string.builder.format(
                    "%s%% construct statically (born static)\n",
                    [s(IndentStr)], !State)
            ;
                ConstructHow = construct_statically(marked_static),
                string.builder.format(
                    "%s%% construct statically (marked static)\n",
                    [s(IndentStr)], !State)
            ;
                ConstructHow = reuse_cell(CellToReuse),
                CellToReuse = cell_to_reuse(ReuseVar, _ReuseConsIds,
                    _FieldAssigns),
                ReuseVarStr = mercury_var_to_string_src(VarNameSrc,
                    VarNamePrint, ReuseVar),
                string.builder.format("%s%% reuse cell: %s\n",
                    [s(IndentStr), s(ReuseVarStr)], !State)
            ;
                ConstructHow = construct_in_region(RegVar),
                RegVarStr = mercury_var_to_string_src(VarNameSrc,
                    VarNamePrint, RegVar),
                string.builder.format("%s%% construct in region: %s\n",
                    [s(IndentStr), s(RegVarStr)], !State)
            )
        ;
            DumpUnifyDetails = no
        )
    ;
        Unification = deconstruct(Var, ConsId, ArgVars, ArgModes, CanFail,
            CanCGC),
        Info = InfoGoal ^ hoig_out_info,
        DumpOptions = Info ^ hoi_dump_hlds_options,
        DumpCtgc = DumpOptions ^ dump_ctgc,
        (
            DumpCtgc = yes,
            string.builder.format("%s%% Compile time garbage collect: %s\n",
                [s(IndentStr), s(string.string(CanCGC))], !State)
        ;
            DumpCtgc = no
        ),
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        (
            CanFail = can_fail,
            OpStr = "?="
        ;
            CanFail = cannot_fail,
            OpStr = "=>"
        ),
        string.builder.format("%s%% %s %s ",
            [s(IndentStr), s(VarStr), s(OpStr)], !State),
        format_functor_and_submodes(InfoGoal, Indent, ConsId, ArgVars,
            ArgModes, !State)
    ;
        Unification = complicated_unify(Mode, CanFail, TypeInfoVars),
        (
            CanFail = can_fail,
            CanFailStr = "can_fail"
        ;
            CanFail = cannot_fail,
            CanFailStr = "cannot_fail"
        ),
        InstVarSet = InfoGoal ^ hoig_inst_varset,
        ModeStr = mercury_unify_mode_to_string(InstVarSet, Mode),
        string.builder.format("%s%% %s, mode: %s\n",
            [s(IndentStr), s(CanFailStr), s(ModeStr)], !State),
        (
            TypeInfoVars = []
        ;
            TypeInfoVars = [_ | _],
            TypeInfoVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, TypeInfoVars),
            string.builder.format("%s%% type-info vars: %s\n",
                [s(IndentStr), s(TypeInfoVarsStr)], !State)
        )
    ).

:- pred format_functor_and_submodes(hlds_out_info_goal::in,
    indent::in,
    cons_id::in, list(prog_var)::in, list(unify_mode)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_functor_and_submodes(InfoGoal, Indent, ConsId, ArgVars,
        ArgUnifyModes0, !State) :-
    ConsIdStr = cons_id_and_arity_to_string(ConsId),
    (
        ArgVars = [],
        string.builder.format("%s\n", [s(ConsIdStr)], !State)
    ;
        ArgVars = [_ | _],
        VarNameSrc = InfoGoal ^ hoig_var_name_src,
        VarNamePrint = InfoGoal ^ hoig_var_name_print,
        ArgVarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
            ArgVars),
        string.builder.format("%s(%s)\n",
            [s(ConsIdStr), s(ArgVarsStr)], !State),
        Info = InfoGoal ^ hoig_out_info,
        DumpOptions = Info ^ hoi_dump_hlds_options,
        DumpUnifyArgmodes = DumpOptions ^ dump_unify_argmodes,
        (
            DumpUnifyArgmodes = yes,
            InstVarSet = InfoGoal ^ hoig_inst_varset,
            IndentStr = indent2_string(Indent),
            list.map(limit_size_of_unify_mode, ArgUnifyModes0, ArgUnifyModes),
            DumpUnifyArgmodesStruct = DumpOptions ^ dump_unify_argmodes_struct,
            (
                DumpUnifyArgmodesStruct = yes,
                string.builder.format("%s%% arg-modes\n",
                    [s(IndentStr)], !State),
                mercury_format_structured_unify_mode_list(output_debug,
                    InstVarSet, do_incl_addr, Indent, ArgUnifyModes,
                    string.builder.handle, !State)
            ;
                DumpUnifyArgmodesStruct = no,
                format_arg_modes(InstVarSet, IndentStr, 1,
                    ArgUnifyModes, !State)
            )
        ;
            DumpUnifyArgmodes = no
        )
    ).

:- pred format_arg_modes(inst_varset::in, string::in, int::in,
    list(unify_mode)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_arg_modes(__InstVarSet, _Indent, _ArgNum, [], !State).
format_arg_modes(InstVarSet, IndentStr, ArgNum,
        [UnifyMode | UnifyModes], !State) :-
    UnifyModeStr = mercury_unify_mode_to_string(InstVarSet, UnifyMode),
    string.builder.format("%s%% arg-mode %d %s\n",
        [s(IndentStr), i(ArgNum), s(UnifyModeStr)], !State),
    format_arg_modes(InstVarSet, IndentStr, ArgNum + 1,
        UnifyModes, !State).

%---------------------------------------------------------------------------%

:- pred limit_size_of_unify_mode(unify_mode::in, unify_mode::out) is det.

limit_size_of_unify_mode(UnifyMode0, UnifyMode) :-
    UnifyMode0 = unify_modes_li_lf_ri_rf(LI0, LF0, RI0, RF0),
    % XXX We could, and maybe should, make this an option that the
    % programmer may set. However, that would incur the cost of an
    % option lookup for every unification. It is probably better to wait
    % until we find a real-life need for deeper unify_modes before
    % we take that step.
    Levels = 3,
    limit_size_of_inst(Levels, LI0, LI),
    limit_size_of_inst(Levels, LF0, LF),
    limit_size_of_inst(Levels, RI0, RI),
    limit_size_of_inst(Levels, RF0, RF),
    UnifyMode = unify_modes_li_lf_ri_rf(LI, LF, RI, RF).

:- pred limit_size_of_inst(int::in, mer_inst::in, mer_inst::out) is det.

limit_size_of_inst(Levels, Inst0, Inst) :-
    (
        ( Inst0 = any(_, _)
        ; Inst0 = ground(_, _)
        ; Inst0 = bound(_, _, _)
        ; Inst0 = constrained_inst_vars(_, _)
        ),
        ( if Levels < 1 then
            Inst = defined_inst(user_inst(unqualified("..."), []))
        else
            (
                Inst0 = any(Uniq, HOInstInfo0),
                limit_size_of_pred_inst_info(Levels, HOInstInfo0, HOInstInfo),
                Inst = any(Uniq, HOInstInfo)
            ;
                Inst0 = ground(Uniq, HOInstInfo0),
                limit_size_of_pred_inst_info(Levels, HOInstInfo0, HOInstInfo),
                Inst = ground(Uniq, HOInstInfo)
            ;
                Inst0 = bound(Uniq, TestResults, BoundFunctors0),
                limit_size_of_bound_functors(Levels - 1,
                    BoundFunctors0, BoundFunctors),
                Inst = bound(Uniq, TestResults, BoundFunctors)
            ;
                Inst0 = constrained_inst_vars(Vars, SubInst0),
                limit_size_of_inst(Levels - 1, SubInst0, SubInst),
                Inst = constrained_inst_vars(Vars, SubInst)
            )
        )
    ;
        ( Inst0 = free
        ; Inst0 = inst_var(_Var)
        ; Inst0 = defined_inst(_InstName)
        ; Inst0 = not_reached
        ),
        Inst = Inst0
    ).

:- pred limit_size_of_pred_inst_info(int::in,
    ho_inst_info::in, ho_inst_info::out) is det.

limit_size_of_pred_inst_info(Levels, HOInstInfo0, HOInstInfo) :-
    (
        HOInstInfo0 = none_or_default_func,
        HOInstInfo = none_or_default_func
    ;
        HOInstInfo0 = higher_order(PredInstInfo0),
        PredInstInfo0 = pred_inst_info(PredOrFunc, ArgModes0, RegInfo, Detism),
        list.map(limit_size_of_mode(Levels - 1), ArgModes0, ArgModes),
        PredInstInfo = pred_inst_info(PredOrFunc, ArgModes, RegInfo, Detism),
        HOInstInfo = higher_order(PredInstInfo)
    ).

:- pred limit_size_of_mode(int::in, mer_mode::in, mer_mode::out) is det.

limit_size_of_mode(Levels, Mode0, Mode) :-
    (
        Mode0 = from_to_mode(InitInst0, FinalInst0),
        limit_size_of_inst(Levels, InitInst0, InitInst),
        limit_size_of_inst(Levels, FinalInst0, FinalInst),
        Mode = from_to_mode(InitInst, FinalInst)
    ;
        Mode0 = user_defined_mode(Name, ArgInsts0),
        list.map(limit_size_of_inst(Levels), ArgInsts0, ArgInsts),
        Mode = user_defined_mode(Name, ArgInsts)
    ).

:- pred limit_size_of_bound_functors(int::in,
    list(bound_functor)::in, list(bound_functor)::out) is det.

limit_size_of_bound_functors(_, [], []).
limit_size_of_bound_functors(Levels,
        [BoundFunctor0 | BoundFunctors0], [BoundFunctor | BoundFunctors]) :-
    BoundFunctor0 = bound_functor(ConsId, ArgInsts0),
    list.map(limit_size_of_inst(Levels), ArgInsts0, ArgInsts),
    BoundFunctor = bound_functor(ConsId, ArgInsts),
    limit_size_of_bound_functors(Levels, BoundFunctors0, BoundFunctors).

%---------------------------------------------------------------------------%
%
% Write out ordinary first-order calls.
%

:- pred format_goal_plain_call(hlds_out_info_goal::in, indent::in, string::in,
    hlds_goal_expr::in(goal_expr_plain_call),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_plain_call(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = plain_call(PredId, ProcId, ArgVars, Builtin,
        MaybeUnifyContext, PredName),
    IndentStr = indent2_string(Indent),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    DumpBuiltinStatus = DumpOptions ^ dump_call_builtin_status,
    (
        DumpBuiltinStatus = yes,
        (
            Builtin = inline_builtin,
            string.builder.format("%s%% inline builtin\n",
                [s(IndentStr)], !State)
        ;
            Builtin = not_builtin
        )
    ;
        DumpBuiltinStatus = no
    ),
    ( if PredId = invalid_pred_id then
        % If we do not know the id of the callee yet, then treat the call
        % as being to a pure predicate. This may be misleading, but any
        % other assumption has a significantly higher chance of being
        % misleading.
        PredOrFunc = pf_predicate,
        PrefixStr = ""
    else if
        ModuleInfo = InfoGoal ^ hoig_module_info,
        module_info_get_pred_id_table(ModuleInfo, PredIdTable),
        map.search(PredIdTable, PredId, PredInfo)
    then
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        pred_info_get_purity(PredInfo, Purity),
        % The prefix includes a trailing space character.
        PrefixStr = purity_prefix_to_string(Purity)
    else
        % We should know the id of the callee, but the callee has been
        % deleted *without* this call to it (and maybe others) being
        % adjusted accordingly. This is a bug, so we want to draw attention
        % to it, but we cannot do so effectively if this code aborts
        % before we finish writing out the HLDS dump.
        PredOrFunc = pf_predicate,
        PrefixStr = "CALL TO DELETED "
    ),
    string.builder.format("%s%s", [s(IndentStr), s(PrefixStr)], !State),
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    (
        PredOrFunc = pf_predicate,
        InParenArgVars = ArgVars
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, InParenArgVars, LHSVar),
        mercury_format_var_src(VarNameSrc, VarNamePrint, LHSVar,
            string.builder.handle, !State),
        string.builder.append_string(" = ", !State)
    ),
    InParenArgVarsStr = sym_name_and_args_to_string(VarNameSrc, VarNamePrint,
        PredName, InParenArgVars),
    string.builder.format("%s%s", [s(InParenArgVarsStr), s(Follow)], !State),
    DumpInstMapVars = DumpOptions ^ dump_goal_instmap_vars,
    (
        DumpInstMapVars = yes,
        pred_id_to_int(PredId, PredNum),
        proc_id_to_int(ProcId, ProcNum),
        string.builder.format("%s%% pred id: %i, proc id: %d%s",
            [s(IndentStr), i(PredNum), i(ProcNum), s(Follow)], !State),
        (
            MaybeUnifyContext = yes(CallUnifyContext),
            TypeQual = InfoGoal ^ hoig_type_qual,
            (
                TypeQual = tvarset_var_table(_, VarTable),
                lookup_var_type(VarTable, Var, UniType),
                VarType = yes(UniType)
            ;
                TypeQual = no_tvarset_var_table,
                VarType = no
            ),
            CallUnifyContext = call_unify_context(Var, RHS, _UnifyContext),
            VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
            string.builder.format("%s%% unify context: %s = ",
                [s(IndentStr), s(VarStr)], !State),
            format_unify_rhs_2(InfoGoal, Indent, VarType, RHS, !State),
            % XXX If we print Follow here, we shouldn't have printed it above.
            string.builder.append_string(Follow, !State)
        ;
            MaybeUnifyContext = no
        )
    ;
        DumpInstMapVars = no
    ).

:- func sym_name_and_args_to_string(var_name_source, var_name_print, sym_name,
    list(prog_var)) = string.

sym_name_and_args_to_string(VarNameSrc, VarNamePrint, PredName, ArgVars)
        = Str :-
    (
        PredName = qualified(ModuleName, Name),
        Str = qualified_functor_to_string(VarNameSrc, VarNamePrint,
            ModuleName, term.atom(Name), ArgVars)
    ;
        PredName = unqualified(Name),
        Str = functor_to_string_maybe_needs_quotes(VarNameSrc, VarNamePrint,
            next_to_graphic_token, term.atom(Name), ArgVars)
    ).

%---------------------------------------------------------------------------%
%
% Write out generic calls.
%

:- pred format_goal_generic_call(hlds_out_info_goal::in, indent::in,
    string::in, hlds_goal_expr::in(goal_expr_generic_call),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_generic_call(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = generic_call(GenericCall, ArgVars, Modes, MaybeArgRegs, _),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    DumpCallPredIds = DumpOptions ^ dump_call_pred_ids,
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    IndentStr = indent2_string(Indent),
    % XXX We should print more info here.
    (
        GenericCall = higher_order(PredVar, Purity, PredOrFunc, _, _),
        PurityPrefix = purity_prefix_to_string(Purity),
        (
            PredOrFunc = pf_predicate,
            (
                DumpCallPredIds = yes,
                string.builder.format("%s%% higher-order predicate call\n",
                    [s(IndentStr)], !State),
                format_ho_arg_regs(Indent, MaybeArgRegs, !State)
            ;
                DumpCallPredIds = no
            ),
            CallStr = functor_to_string(VarNameSrc, VarNamePrint,
                term.atom("call"), [PredVar | ArgVars]),
            string.builder.format("%s%s%s",
                [s(IndentStr), s(PurityPrefix), s(CallStr)], !State)
        ;
            PredOrFunc = pf_function,
            (
                DumpCallPredIds = yes,
                string.builder.format(
                    "%s%% higher-order function application\n",
                    [s(IndentStr)], !State),
                format_ho_arg_regs(Indent, MaybeArgRegs, !State)
            ;
                DumpCallPredIds = no
            ),
            pred_args_to_func_args([PredVar | ArgVars],
                FuncArgVars, FuncRetVar),
            FuncRetVarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint,
                FuncRetVar),
            ApplyStr = functor_to_string(VarNameSrc, VarNamePrint,
                term.atom("apply"), FuncArgVars),
            string.builder.format("%s%s%s = %s",
                [s(IndentStr), s(PurityPrefix), s(FuncRetVarStr), s(ApplyStr)],
                !State)
        ),
        string.builder.append_string(Follow, !State)
    ;
        GenericCall = class_method(TCInfoVar, method_proc_num(MethodNum),
            _ClassId, _MethodId),
        (
            DumpCallPredIds = yes,
            string.builder.format("%s%% class method call\n",
                [s(IndentStr)], !State),
            format_ho_arg_regs(Indent, MaybeArgRegs, !State)
        ;
            DumpCallPredIds = no
        ),
        Context = dummy_context,
        Functor = term.atom("class_method_call"),
        TCInfoTerm = term.variable(TCInfoVar, Context),
        MethodNumTerm = term_int.int_to_decimal_term(MethodNum, Context),
        term_subst.var_list_to_term_list(ArgVars, ArgTerms),
        Term = term.functor(Functor, [TCInfoTerm, MethodNumTerm | ArgTerms],
            Context),
        string.builder.append_string(IndentStr, !State),
        mercury_format_term_src(VarNameSrc, VarNamePrint, Term,
            string.builder.handle, !State),
        string.builder.append_string(Follow, !State)
    ;
        GenericCall = event_call(EventName),
        (
            DumpCallPredIds = yes,
            string.builder.format("%s%% event call\n", [s(IndentStr)], !State),
            format_ho_arg_regs(Indent, MaybeArgRegs, !State)
        ;
            DumpCallPredIds = no
        ),
        Functor = term.atom(EventName),
        term_subst.var_list_to_term_list(ArgVars, ArgTerms),
        Term = term.functor(Functor, ArgTerms, dummy_context),
        string.builder.format("%sevent ", [s(IndentStr)], !State),
        mercury_format_term_src(VarNameSrc, VarNamePrint, Term,
            string.builder.handle, !State),
        string.builder.append_string(Follow, !State)
    ;
        GenericCall = cast(CastType),
        (
            ( CastType = unsafe_type_cast
            ; CastType = unsafe_type_inst_cast
            ; CastType = equiv_type_cast
            ; CastType = exists_cast
            ),
            CastTypeString = cast_type_to_string(CastType)
        ;
            CastType = subtype_coerce,
            % We must produce valid Mercury code for coerce casts that are
            % written to .opt files.
            CastTypeString = "coerce"
        ),
        (
            DumpCallPredIds = yes,
            string.builder.format("%s%% %s\n",
                [s(IndentStr), s(CastTypeString)], !State),
            format_ho_arg_regs(Indent, MaybeArgRegs, !State)
        ;
            DumpCallPredIds = no
        ),
        DumpInstMapDeltas = DumpOptions ^ dump_goal_instmap_deltas,
        (
            DumpInstMapDeltas = yes,
            varset.init(InstVarSet),
            ModesStr = mercury_mode_list_to_string(output_debug, InstVarSet,
                Modes),
            string.builder.format("%s%% modes: %s\n",
                [s(IndentStr), s(ModesStr)], !State)
        ;
            DumpInstMapDeltas = no
        ),
        PredOrFunc = write_cast_as_pred_or_func(CastType),
        (
            PredOrFunc = pf_predicate,
            CallStr = functor_to_string(VarNameSrc, VarNamePrint,
                term.atom(CastTypeString), ArgVars),
            string.builder.format("%s%s",
                [s(IndentStr), s(CallStr)], !State)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(ArgVars, FuncArgVars, FuncRetVar),
            FuncRetVarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint,
                FuncRetVar),
            CallStr = functor_to_string(VarNameSrc, VarNamePrint,
                term.atom(CastTypeString), FuncArgVars),
            string.builder.format("%s%s = %s",
                [s(IndentStr), s(FuncRetVarStr), s(CallStr)], !State)
        ),
        string.builder.append_string(Follow, !State)
    ).

:- pred format_ho_arg_regs(indent::in, arg_reg_type_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_ho_arg_regs(Indent, MaybeArgRegs, !State) :-
    (
        MaybeArgRegs = arg_reg_types(ArgRegs),
        IndentStr = indent2_string(Indent),
        ArgRegStrs = list.map(ho_arg_reg_to_string, ArgRegs),
        ArgRegsStr = string.join_list(", ", ArgRegStrs),
        string.builder.format("%s%% arg regs: %s\n",
            [s(IndentStr), s(ArgRegsStr)], !State)
    ;
        MaybeArgRegs = arg_reg_types_unset
    ).

:- func ho_arg_reg_to_string(ho_arg_reg) = string.

ho_arg_reg_to_string(ArgReg) = Str :-
    (
        ArgReg = ho_arg_reg_r,
        Str = "reg_r"
    ;
        ArgReg = ho_arg_reg_f,
        Str = "reg_f"
    ).

:- func write_cast_as_pred_or_func(cast_kind) = pred_or_func.

write_cast_as_pred_or_func(CastType) = PredOrFunc :-
    (
        ( CastType = unsafe_type_cast
        ; CastType = unsafe_type_inst_cast
        ; CastType = equiv_type_cast
        ; CastType = exists_cast
        ),
        PredOrFunc = pf_predicate
    ;
        CastType = subtype_coerce,
        PredOrFunc = pf_function
    ).

%---------------------------------------------------------------------------%
%
% Write out calls to foreign procs.
%

:- pred format_goal_foreign_proc(hlds_out_info_goal::in,
    indent::in, string::in, hlds_goal_expr::in(goal_expr_foreign_proc),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_foreign_proc(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = call_foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
        MaybeTraceRuntimeCond, PragmaCode),
    ForeignLang = get_foreign_language(Attributes),
    ForeignLangStr = foreign_language_string(ForeignLang),
    ModuleInfo = InfoGoal ^ hoig_module_info,
    PredStr = pred_id_to_dev_string(ModuleInfo, PredId),
    pred_id_to_int(PredId, PredIdInt),
    proc_id_to_int(ProcId, ProcIdInt),

    IndentStr = indent2_string(Indent),
    string.builder.format(
        "%s$pragma_foreign_proc(/* %s */, %s pred %d proc %d,\n",
        [s(IndentStr), s(ForeignLangStr),
        s(PredStr), i(PredIdInt), i(ProcIdInt)], !State),
    (
        MaybeTraceRuntimeCond = no
    ;
        MaybeTraceRuntimeCond = yes(TraceRuntimeCond),
        string.builder.format("%s%% trace_runtime_cond(",
            [s(IndentStr)], !State),
        mercury_format_trace_expr(string.builder.handle,
            mercury_format_trace_runtime, TraceRuntimeCond, !State),
        string.builder.append_string(")\n", !State)
    ),
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    TypeVarSet = InfoGoal ^ hoig_tvarset,
    InstVarSet = InfoGoal ^ hoig_inst_varset,
    ArgsStr = foreign_args_to_string(VarNameSrc, VarNamePrint,
        TypeVarSet, InstVarSet, IndentStr, "[", "],", Args),
    string.builder.append_string(ArgsStr, !State),
    (
        ExtraArgs = []
    ;
        ExtraArgs = [_ | _],
        ExtraArgsStr = foreign_args_to_string(VarNameSrc, VarNamePrint,
            TypeVarSet, InstVarSet, IndentStr, "{", "},", ExtraArgs),
        string.builder.append_string(ExtraArgsStr, !State)
    ),
    PragmaCode = fp_impl_ordinary(Code, _),
    string.builder.format("""%s"")%s", [s(Code), s(Follow)], !State).

:- func foreign_args_to_string(var_name_source, var_name_print,
    tvarset, inst_varset, string, string, string, list(foreign_arg)) = string.

foreign_args_to_string(VarNameSrc, VarNamePrint, TypeVarSet, InstVarSet,
        IndentStr, LParen, RParen, Args) = Str :-
    (
        Args = [],
        string.format("%s%s%s\n", [s(IndentStr), s(LParen), s(RParen)], Str)
    ;
        Args = [HeadArg | TailArgs],
        LineStrs = foreign_args_to_string_lag(VarNameSrc, VarNamePrint,
            TypeVarSet, InstVarSet, IndentStr, LParen, RParen,
            HeadArg, TailArgs),
        string.append_list(LineStrs, Str)
    ).

:- func foreign_args_to_string_lag(var_name_source, var_name_print,
    tvarset, inst_varset, string, string, string,
    foreign_arg, list(foreign_arg)) = list(string).

foreign_args_to_string_lag(VarNameSrc, VarNamePrint, TypeVarSet, InstVarSet,
        IndentStr, MaybeLParen0, RParen, Arg, Args) = LineStrs :-
    Arg = foreign_arg(Var, MaybeNameMode, Type, BoxPolicy),
    VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
    (
        MaybeNameMode = yes(foreign_arg_name_mode(Name, Mode)),
        % For HLDS dumps, we need clarity mode than round-trippability,
        % which is why we specify output_debug.
        ModeStr = mercury_mode_to_string(output_debug, InstVarSet, Mode),
        string.format("/%s(%s)", [s(Name), s(ModeStr)], NameModeStr)
    ;
        MaybeNameMode = no,
        NameModeStr = ""
    ),
    (
        BoxPolicy = bp_native_if_possible,
        BoxPolicyStr = ""
    ;
        BoxPolicy = bp_always_boxed,
        BoxPolicyStr = "$alwaysboxed"
    ),
    TypeStr = mercury_type_to_string(TypeVarSet, VarNamePrint, Type),
    (
        Args = [],
        string.format("%s%s%s%s%s@%s%s\n",
            [s(IndentStr), s(MaybeLParen0), s(VarStr), s(NameModeStr),
            s(BoxPolicyStr), s(TypeStr), s(RParen)], ArgLineStr),
        LineStrs = [ArgLineStr]
    ;
        Args = [HeadArg | TailArgs],
        string.format("%s%s%s%s%s@%s,\n",
            [s(IndentStr), s(MaybeLParen0), s(VarStr), s(NameModeStr),
            s(BoxPolicyStr), s(TypeStr)], ArgLineStr),
        % We have already printed the left parenthesis, either in this call,
        % or in one of our ancestors.
        ArgsMaybeLParen = "",
        ArgsLineStrs = foreign_args_to_string_lag(VarNameSrc, VarNamePrint,
            TypeVarSet, InstVarSet, IndentStr, ArgsMaybeLParen, RParen,
            HeadArg, TailArgs),
        LineStrs = [ArgLineStr | ArgsLineStrs]
    ).

%---------------------------------------------------------------------------%
%
% Write out disjunctions.
%

:- pred format_goal_conj(hlds_out_info_goal::in,
    indent::in, string::in, hlds_goal_expr::in(goal_expr_conj),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_conj(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = conj(ConjType, List),
    (
        List = [Goal | Goals],
        (
            ConjType = plain_conj,
            Info = InfoGoal ^ hoig_out_info,
            DumpOptions = Info ^ hoi_dump_hlds_options,
            ( if DumpOptions = empty_dump_options then
                write_conj(InfoGoal, Indent, Follow, ",\n",
                    Goal, Goals, !State)
            else
                IndentStr = indent2_string(Indent),
                string.builder.format("%s( %% conjunction\n",
                    [s(IndentStr)], !State),
                write_conj(InfoGoal, Indent + 1u, "\n", ",\n",
                    Goal, Goals, !State),
                string.builder.format("%s)%s",
                    [s(IndentStr), s(Follow)], !State)
            )
        ;
            ConjType = parallel_conj,
            IndentStr = indent2_string(Indent),
            string.builder.format("%s( %% parallel conjunction\n",
                [s(IndentStr)], !State),
            do_format_goal(InfoGoal, Indent + 1u, "\n", Goal, !State),
            % See comments at format_goal_list.
            format_goal_list(InfoGoal, Indent, "&\n", Goals, !State),
            string.builder.format("%s)%s", [s(IndentStr), s(Follow)], !State)
        )
    ;
        List = [],
        IndentStr = indent2_string(Indent),
        (
            ConjType = plain_conj,
            ParStr = ""
        ;
            ConjType = parallel_conj,
            ParStr = "/* parallel */"
        ),
        string.builder.format("%s%strue%s",
            [s(IndentStr), s(ParStr), s(Follow)], !State)
    ).

:- pred write_conj(hlds_out_info_goal::in,
    indent::in, string::in, string::in, hlds_goal::in, list(hlds_goal)::in,
    string.builder.state::di, string.builder.state::uo) is det.

write_conj(InfoGoal, Indent, Follow, Separator, Goal1, Goals1, !State) :-
    (
        Goals1 = [Goal2 | Goals2],
        Info = InfoGoal ^ hoig_out_info,
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if DumpOptions = empty_dump_options then
            do_format_goal(InfoGoal, Indent, Separator, Goal1, !State)
        else
            % When generating verbose dumps, we want the comma on its own line,
            % since that way it visually separates the lines after one goal
            % and the lines before the next.
            do_format_goal(InfoGoal, Indent, "\n", Goal1, !State),
            IndentStr = indent2_string(Indent),
            string.builder.format("%s%s", [s(IndentStr), s(Separator)], !State)
        ),
        write_conj(InfoGoal, Indent, Follow, Separator,
            Goal2, Goals2, !State)
    ;
        Goals1 = [],
        do_format_goal(InfoGoal, Indent, Follow, Goal1, !State)
    ).

%---------------------------------------------------------------------------%
%
% Write out disjunctions.
%

:- pred format_goal_disj(hlds_out_info_goal::in,
    indent::in, string::in, hlds_goal_expr::in(goal_expr_disj),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_disj(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = disj(Disjuncts),
    IndentStr = indent2_string(Indent),
    (
        Disjuncts = [Goal | Goals],
        string.builder.format("%s( %% disjunction\n", [s(IndentStr)], !State),
        do_format_goal(InfoGoal, Indent + 1u, "\n", Goal, !State),
        format_goal_list(InfoGoal, Indent, ";\n", Goals, !State),
        string.builder.format("%s)%s", [s(IndentStr), s(Follow)], !State)
    ;
        Disjuncts = [],
        string.builder.format("%sfail%s", [s(IndentStr), s(Follow)], !State)
    ).

%---------------------------------------------------------------------------%
%
% Write out switches.
%

:- pred format_goal_switch(hlds_out_info_goal::in,
    indent::in, string::in, hlds_goal_expr::in(goal_expr_switch),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_switch(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = switch(Var, CanFail, CasesList),
    IndentStr = indent2_string(Indent),
    CanFailStr = can_fail_to_string(CanFail),
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
    string.builder.format("%s( %% %s switch on %s\n",
        [s(IndentStr), s(CanFailStr), s(VarStr)], !State),
    (
        CasesList = [Case | Cases],
        format_case(InfoGoal, Indent, Var, Case, !State),
        format_cases(InfoGoal, Indent, Var, Cases, !State)
    ;
        CasesList = [],
        string.builder.format("%sfail\n", [s(IndentStr)], !State)
    ),
    string.builder.format("%s)%s", [s(IndentStr), s(Follow)], !State).

:- pred format_cases(hlds_out_info_goal::in,
    indent::in, prog_var::in, list(case)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_cases(InfoGoal, Indent, Var, CasesList, !State) :-
    (
        CasesList = [Case | Cases],
        IndentStr = indent2_string(Indent),
        string.builder.format("%s;\n", [s(IndentStr)], !State),
        format_case(InfoGoal, Indent, Var, Case, !State),
        format_cases(InfoGoal, Indent, Var, Cases, !State)
    ;
        CasesList = []
    ).

case_to_string(InfoGoal, Indent, Var, Case) = Str :-
    State0 = string.builder.init,
    format_case(InfoGoal, Indent, Var, Case, State0, State),
    Str = string.builder.to_string(State).

:- pred format_case(hlds_out_info_goal::in,
    indent::in, prog_var::in, case::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_case(InfoGoal, Indent, Var, Case, !State) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    IndentStr = indent2_string(Indent),
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
    % Any module qualifications on the cons_ids would be copies of
    % the module qualification on the type they are from, which can be
    % looked up in the procedure's type table.
    ConsIdStrs = list.map(unqual_cons_id_and_arity_to_string,
        [MainConsId | OtherConsIds]),
    ConsIdsStr = string.join_list(" or ", ConsIdStrs),
    % Align the line listing the case's functors with the parentheses
    % around the switch, having them stand out from the case arm goals.
    string.builder.format("%s%% %s has functor %s\n",
        [s(IndentStr), s(VarStr), s(ConsIdsStr)], !State),
    % XXX if the output of this is to be used, e.g. in
    % inter-module optimization, output a unification to bind the
    % Var to the functor, since simplify.m and unused_args.m remove
    % the unification. At the moment this is not a problem, since
    % intermod.m works on the unoptimized clauses.
    do_format_goal(InfoGoal, Indent + 1u, "\n", Goal, !State).

project_cons_name_and_tag(TaggedConsId, ConsName, ConsTag) :-
    TaggedConsId = tagged_cons_id(ConsId, ConsTag),
    ConsName = cons_id_and_arity_to_string(ConsId).

case_comment(VarName, MainConsName, OtherConsNames) = Comment :-
    (
        OtherConsNames = [],
        Comment = VarName ++ " has the functor " ++ MainConsName
    ;
        OtherConsNames = [_ | _],
        Comment = VarName ++ " has one of the functors " ++
            string.join_list(", ", [MainConsName | OtherConsNames])
    ).

%---------------------------------------------------------------------------%
%
% Write out negations.
%

:- pred format_goal_negation(hlds_out_info_goal::in,
    indent::in, string::in, hlds_goal_expr::in(goal_expr_neg),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_negation(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = negation(Goal),
    IndentStr = indent2_string(Indent),
    string.builder.format("%snot (\n", [s(IndentStr)], !State),
    do_format_goal(InfoGoal, Indent + 1u, "\n", Goal, !State),
    string.builder.format("%s)%s", [s(IndentStr), s(Follow)], !State).

%---------------------------------------------------------------------------%
%
% Write out if-then-elses.
%

:- pred format_goal_if_then_else(hlds_out_info_goal::in, indent::in,
    string::in, hlds_goal_expr::in(goal_expr_ite),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_if_then_else(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = if_then_else(Vars, Cond, Then, Else),
    IndentStr = indent2_string(Indent),
    (
        Vars = [],
        SomeVarsStr = ""
    ;
        Vars = [_ | _],
        VarNameSrc = InfoGoal ^ hoig_var_name_src,
        VarNamePrint = InfoGoal ^ hoig_var_name_print,
        VarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint, Vars),
        string.format(" some [%s]", [s(VarsStr)], SomeVarsStr)
    ),
    string.builder.format("%s( if%s\n",
        [s(IndentStr), s(SomeVarsStr)], !State),

    Indent1 = Indent + 1u,
    do_format_goal(InfoGoal, Indent1, "\n", Cond, !State),
    string.builder.format("%sthen\n", [s(IndentStr)], !State),
    do_format_goal(InfoGoal, Indent1, "\n", Then, !State),
    string.builder.format("%selse\n", [s(IndentStr)], !State),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if
        DumpOptions \= empty_dump_options,
        Else = hlds_goal(if_then_else(_, _, _, _), _)
    then
        ElseIndent = Indent
    else
        ElseIndent = Indent1
    ),
    do_format_goal(InfoGoal, ElseIndent, "\n", Else, !State),
    string.builder.format("%s)%s", [s(IndentStr), s(Follow)], !State).

%---------------------------------------------------------------------------%
%
% Write out scope goals.
%

:- pred format_goal_scope(hlds_out_info_goal::in,
    indent::in, string::in, hlds_goal_expr::in(goal_expr_scope),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_scope(!.InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = scope(Reason, Goal),
    IndentStr = indent2_string(Indent),
    string.builder.append_string(IndentStr, !State),
    (
        Reason = exist_quant(Vars, Creator),
        VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
        VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
        VarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint, Vars),
        ( Creator = user_quant,     CreatorStr = "user"
        ; Creator = compiler_quant, CreatorStr = "compiler"
        ),
        string.builder.format("some [%s] ( %% %s\n",
            [s(VarsStr), s(CreatorStr)], !State)
    ;
        Reason = disable_warnings(HeadWarning, TailWarnings),
        string.builder.append_string("disable_warnings [", !State),
        mercury_format_goal_warnings(string.builder.handle,
            HeadWarning, TailWarnings, !State),
        string.builder.append_string("] (\n", !State)
    ;
        Reason = promise_purity(Purity),
        ( Purity = purity_pure,         PromiseStr = "promise_pure"
        ; Purity = purity_semipure,     PromiseStr = "promise_semipure"
        ; Purity = purity_impure,       PromiseStr = "promise_impure"
        ),
        string.builder.format("%s (\n", [s(PromiseStr)], !State)
    ;
        Reason = promise_solutions(Vars, Kind),
        VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
        VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
        VarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint, Vars),
        (
            Kind = equivalent_solutions,
            PromiseKindStr = "promise_equivalent_solutions"
        ;
            Kind = equivalent_solution_sets,
            PromiseKindStr = "promise_equivalent_solution_sets"
        ;
            Kind = equivalent_solution_sets_arbitrary,
            PromiseKindStr = "arbitrary"
        ),
        string.builder.format("%s [%s] (\n",
            [s(PromiseKindStr), s(VarsStr)], !State)
    ;
        Reason = require_detism(Detism),
        ( Detism = detism_det,          ReqStr = "require_det"
        ; Detism = detism_semi,         ReqStr = "require_semidet"
        ; Detism = detism_non,          ReqStr = "require_nondet"
        ; Detism = detism_multi,        ReqStr = "require_multi"
        ; Detism = detism_cc_multi,     ReqStr = "require_cc_multi"
        ; Detism = detism_cc_non,       ReqStr = "require_cc_nondet"
        ; Detism = detism_failure,      ReqStr = "require_failure"
        ; Detism = detism_erroneous,    ReqStr = "require_erroneous"
        ),
        string.builder.format("%s (\n", [s(ReqStr)], !State)
    ;
        Reason = require_complete_switch(Var),
        VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
        VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        string.builder.format("require_complete_switch [%s] (\n",
            [s(VarStr)], !State)
    ;
        Reason = require_switch_arms_detism(Var, Detism),
        (
            Detism = detism_det,
            ReqStr = "require_switch_arms_det"
        ;
            Detism = detism_semi,
            ReqStr = "require_switch_arms_semidet"
        ;
            Detism = detism_non,
            ReqStr = "require_switch_arms_nondet"
        ;
            Detism = detism_multi,
            ReqStr = "require_switch_arms_multi"
        ;
            Detism = detism_cc_multi,
            ReqStr = "require_switch_arms_cc_multi"
        ;
            Detism = detism_cc_non,
            ReqStr = "require_switch_arms_cc_nondet"
        ;
            Detism = detism_failure,
            ReqStr = "require_switch_arms_failure"
        ;
            Detism = detism_erroneous,
            ReqStr = "require_switch_arms_erroneous"
        ),
        VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
        VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        string.builder.format("%s [%s] (\n", [s(ReqStr), s(VarStr)], !State)
    ;
        Reason = barrier(removable),
        string.builder.append_string("(\n", !State),
        format_indent2(Indent, !State),
        string.builder.append_string("% barrier(removable)\n", !State)
    ;
        Reason = barrier(not_removable),
        string.builder.append_string("(\n", !State),
        format_indent2(Indent, !State),
        string.builder.append_string("% barrier(not_removable)\n", !State)
    ;
        Reason = commit(force_pruning),
        string.builder.append_string("(\n", !State),
        format_indent2(Indent, !State),
        string.builder.append_string("% commit(force_pruning)\n", !State)
    ;
        Reason = commit(do_not_force_pruning),
        string.builder.append_string("(\n", !State),
        format_indent2(Indent, !State),
        string.builder.append_string("% commit(do_not_force_pruning)\n",
            !State)
    ;
        Reason = from_ground_term(Var, Kind),
        VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
        VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        ( Kind = from_ground_term_initial,      KindStr = "initial"
        ; Kind = from_ground_term_construct,    KindStr = "construct"
        ; Kind = from_ground_term_deconstruct,  KindStr = "deconstruct"
        ; Kind = from_ground_term_other,        KindStr = "other"
        ),
        string.builder.append_string("(\n", !State),
        string.builder.format("%s%% from_ground_term [%s, %s]\n",
            [s(IndentStr), s(VarStr), s(KindStr)], !State),
        % The goals inside from_ground_term scopes are created with
        % all of the fields of goal infos already filled in.
        % This means printing them is meaningful, and sometimes
        % it is needed to diagnose problems.
        DumpOptionsBackup =
            !.InfoGoal ^ hoig_out_info ^ hoi_dump_hlds_options_backup,
        !InfoGoal ^ hoig_out_info ^ hoi_dump_hlds_options := DumpOptionsBackup
    ;
        Reason = trace_goal(MaybeCompileTime, MaybeRunTime, MaybeIO0,
            MutableVars0, QuantVars),
        string.builder.append_string("trace [\n", !State),
        some [!AddCommaNewline] (
            !:AddCommaNewline = no,
            Indent1Str = indent2_string(Indent + 1u),
            (
                MaybeCompileTime = yes(CompileTime),
                string.builder.format("%scompile_time(",
                    [s(Indent1Str)], !State),
                mercury_format_trace_expr(string.builder.handle,
                    mercury_format_trace_compiletime, CompileTime, !State),
                string.builder.append_string(")", !State),
                !:AddCommaNewline = yes
            ;
                MaybeCompileTime = no
            ),
            (
                MaybeRunTime = yes(RunTime),
                maybe_add_comma_newline(!.AddCommaNewline, !State),
                string.builder.format("%sruntime(", [s(Indent1Str)], !State),
                mercury_format_trace_expr(string.builder.handle,
                    mercury_format_trace_runtime, RunTime, !State),
                string.builder.append_string(")", !State),
                !:AddCommaNewline = yes
            ;
                MaybeRunTime = no
            ),
            Lang = get_output_lang(!.InfoGoal ^ hoig_out_info
                ^ hoi_merc_out_info),
            (
                Lang = output_mercury,
                % After we have read in trace goals as expressions,
                % goal_expr_to_goal.m, in the process of converting
                % those goal_exprs to HLDS goals, wraps the goal
                % in the scope with code to get and set the I/O state
                % and/or any mutables mentioned by the scope.
                %
                % Therefore when we generate Mercury code, we don't write
                % these parts of the trace goal out. If we did, then
                % two problems would arise.
                %
                % - The obvious problem is that we would get and set
                %   the I/O state and any mutables twice.
                %
                % - In fact, we never get there, because another problem
                %   arises first, which is that goal_expr_to_goal.m generates
                %   the calls to getter and setter predicates in an
                %   unqualified form. When reading the compiler reads such
                %   code from .opt files, it expects every call in that code
                %   to have been fully module qualified by the compiler
                %   invocation that created the .opt file, but obviously,
                %   that compiler invocation cannot do that on code that
                %   only a later compiler invocation will create.
                %
                % Omitting any io() and state() components from .opt files
                % works just fine. The other situation in which we write
                % Mercury code out with output_mercury is when we generate
                % .ugly files. In that use case, we would prefer to
                % write out io() and state() components while also
                % (a) deleting the calls to the get and set predicates
                % they introduce, and (b) undoing the expansion of the
                % state variables they specify. While (a) could be done
                % relatively easily with the help of a goal feature
                % introduced specifically for this purpose, (b) is much
                % harder. Since .ugly files are not an important use case,
                % we don't bother.
                MaybeIO = no,
                MutableVars = []
            ;
                Lang = output_debug,
                % When generating HLDS dumps, it does not matter what
                % goal_expr_to_goal.m would do with our output, so we prefer
                % to generate code that reflects the original source code
                % as far as possible, accepting that, as explained above,
                % undoing the transformations that goal_expr_to_goal.m
                % has already done to the goal in the scope is impractical.
                MaybeIO = MaybeIO0,
                MutableVars = MutableVars0
            ),
            (
                MaybeIO = yes(IOStateVarName),
                maybe_add_comma_newline(!.AddCommaNewline, !State),
                string.builder.format("%sio(!%s)",
                    [s(Indent1Str), s(IOStateVarName)], !State),
                !:AddCommaNewline = yes
            ;
                MaybeIO = no
            ),
            list.foldl2(write_trace_mutable_var_hlds(Indent1Str),
                MutableVars, !AddCommaNewline, !State),
            (
                !.AddCommaNewline = no
            ;
                !.AddCommaNewline = yes,
                % There is nothing following that requires a comma.
                string.builder.append_string("\n", !State)
            ),
            (
                Lang = output_mercury
            ;
                Lang = output_debug,
                VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
                VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
                QuantVarsStr = mercury_vars_to_string_src(VarNameSrc,
                    VarNamePrint, QuantVars),
                string.builder.format("%s%% quantified vars [%s]\n",
                    [s(Indent1Str), s(QuantVarsStr)], !State)
            ),
            format_indent2(Indent, !State),
            string.builder.append_string("] (\n", !State)
        )
    ;
        Reason = loop_control(LCVar, LCSVar, UseParentStack),
        (
            UseParentStack = lc_use_parent_stack_frame,
            UseParentStackStr = "using_parent_stack_frame"
        ;
            UseParentStack = lc_create_frame_on_child_stack,
            UseParentStackStr = "using_child_stack"
        ),
        VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
        VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
        LCVarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
            [LCVar, LCSVar]),
        string.builder.format("%s%% loop_control_spawn_off_%s(%s) (\n",
            [s(IndentStr), s(UseParentStackStr), s(LCVarsStr)], !State)
    ),
    do_format_goal(!.InfoGoal, Indent + 1u, "\n", Goal, !State),
    string.builder.format("%s)%s", [s(IndentStr), s(Follow)], !State).

:- pred write_trace_mutable_var_hlds(string::in, trace_mutable_var_hlds::in,
    bool::in, bool::out,
    string.builder.state::di, string.builder.state::uo) is det.

write_trace_mutable_var_hlds(IndentStr, MutableVar,
        !AddCommaNewline, !State) :-
    MutableVar = trace_mutable_var_hlds(MutableName, StateVarName),
    maybe_add_comma_newline(!.AddCommaNewline, !State),
    string.builder.format("%sstate(%s, !%s)",
        [s(IndentStr), s(MutableName), s(StateVarName)], !State),
    !:AddCommaNewline = yes.

:- pred maybe_add_comma_newline(bool::in,
    string.builder.state::di, string.builder.state::uo) is det.

maybe_add_comma_newline(AddCommaNewline, !State) :-
    (
        AddCommaNewline = no
    ;
        AddCommaNewline = yes,
        string.builder.append_string(",\n", !State)
    ).

%---------------------------------------------------------------------------%
%
% Write out shorthand goals.
%

:- pred format_goal_shorthand(hlds_out_info_goal::in,
    indent::in, string::in, hlds_goal_expr::in(goal_expr_shorthand),
    string.builder.state::di, string.builder.state::uo) is det.

format_goal_shorthand(InfoGoal, Indent, Follow, GoalExpr, !State) :-
    GoalExpr = shorthand(ShortHand),
    IndentStr = indent2_string(Indent),
    Indent1 = Indent + 1u,
    (
        ShortHand = atomic_goal(_GoalType, Outer, Inner, MaybeOutputVars,
            MainGoal, OrElseGoals, _OrElseInners),
        VarNameSrc = InfoGoal ^ hoig_var_name_src,
        VarNamePrint = InfoGoal ^ hoig_var_name_print,
        OuterStr = atomic_interface_vars_to_string(VarNameSrc, VarNamePrint,
            "outer", Outer),
        InnerStr = atomic_interface_vars_to_string(VarNameSrc, VarNamePrint,
            "inner", Inner),
        (
            MaybeOutputVars = no,
            MaybeOutputVarsStr = "no_vars"
        ;
            MaybeOutputVars = yes(OutputVars),
            OutputVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, OutputVars),
            string.format("vars([%s])", [s(OutputVarsStr)], MaybeOutputVarsStr)
        ),
        string.builder.format("%satomic [%s %s %s] (\n",
            [s(IndentStr), s(OuterStr), s(InnerStr), s(MaybeOutputVarsStr)],
            !State),
        do_format_goal(InfoGoal, Indent1, "\n", MainGoal, !State),
        format_goal_list(InfoGoal, Indent, "or_else\n",
            OrElseGoals, !State),
        string.builder.format("%s)%s", [s(IndentStr), s(Follow)], !State)
    ;
        ShortHand = try_goal(MaybeIO, _, SubGoal),
        string.builder.format("%s( %% try\n", [s(IndentStr)], !State),
        (
            MaybeIO = yes(try_io_state_vars(IOVarA, IOVarB)),
            Indent1Str = indent2_string(Indent1),
            VarNameSrc = InfoGoal ^ hoig_var_name_src,
            VarNamePrint = InfoGoal ^ hoig_var_name_print,
            IOVarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                [IOVarA, IOVarB]),
            string.builder.format("%s%% io(%s)\n",
                [s(Indent1Str), s(IOVarsStr)], !State)
        ;
            MaybeIO = no
        ),
        do_format_goal(InfoGoal, Indent1, "\n", SubGoal, !State),
        string.builder.format("%s)%s", [s(IndentStr), s(Follow)], !State)
    ;
        ShortHand = bi_implication(GoalA, GoalB),
        string.builder.format("%s( %% bi-implication\n",
            [s(IndentStr)], !State),
        do_format_goal(InfoGoal, Indent1, "\n", GoalA, !State),
        string.builder.format("%s<=>\n", [s(IndentStr)], !State),
        do_format_goal(InfoGoal, Indent1, "\n", GoalB, !State),
        string.builder.format("%s)%s", [s(IndentStr), s(Follow)], !State)
    ).

:- func atomic_interface_vars_to_string(var_name_source, var_name_print,
    string, atomic_interface_vars) = string.

atomic_interface_vars_to_string(VarNameSrc, VarNamePrint,
        CompName, CompState) = Str :-
    CompState = atomic_interface_vars(Var1, Var2),
    Var1Str = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var1),
    Var2Str = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var2),
    string.format("%s(%s, %s)", [s(CompName), s(Var1Str), s(Var2Str)], Str).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_goal.
%---------------------------------------------------------------------------%
