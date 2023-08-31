%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
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
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Print a goal in a way that is suitable for debugging the compiler
    % (but necessarily for anything else).
    %
:- pred dump_goal(io.text_output_stream::in, module_info::in,
    var_name_source::in, tvarset::in, inst_varset::in, hlds_goal::in,
    io::di, io::uo) is det.

    % Print a goal in a way that is suitable for debugging the compiler
    % (but necessarily for anything else), followed by a newline.
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
    tvarset::in, inst_varset::in, int::in, string::in, hlds_goal::in,
    io::di, io::uo) is det.

    % As write_goal, but add a newline at the end.
    %
:- pred write_goal_nl(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, var_name_print::in,
    tvarset::in, inst_varset::in, int::in, string::in, hlds_goal::in,
    io::di, io::uo) is det.

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
    int::in, string::in, hlds_goal::in, io::di, io::uo) is det.

    % write_goal_list is used to write both disjunctions and parallel
    % conjunctions. The boolean says whether variables should have
    % their numbers appended to them. The integer gives the level of
    % indentation to be used within the goal. The string says what should be
    % on the line between each goal; it should include a newline character,
    % but may also contain other characters before that.
    %
:- pred write_goal_list(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, list(hlds_goal)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Write out the mapping of variables to their abstract locations.
    %
:- pred write_var_to_abs_locns(io.text_output_stream::in,
    var_name_source::in, var_name_print::in, int::in,
    assoc_list(prog_var, abs_locn)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Print out the right-hand-side of a unification. The module_info and
    % the varsets give the context of the rhs. The boolean says whether
    % variables should have their numbers appended to them. The integer
    % gives the level of indentation to be used within the rhs.
    %
:- pred write_unify_rhs(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, var_name_print::in,
    tvarset::in, inst_varset::in, int::in, unify_rhs::in,
    io::di, io::uo) is det.

    % Converts the right-hand-side of a unification to a string, similarly to
    % write_unify_rhs, but doesn't print any details for lambda goals.
    % The module_info and the varset give the context of the rhs. The boolean
    % says whether variables should have their numbers appended to them.
    %
:- func unify_rhs_to_string(module_info, var_table, var_name_print, unify_rhs)
    = string.

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

:- import_module hlds.error_msg_inst.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.indent.
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
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module term_int.
:- import_module term_subst.
:- import_module varset.

%---------------------------------------------------------------------------%

dump_goal(Stream, ModuleInfo, VarNameSrc, TVarSet, InstVarSet, Goal, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    Info = init_hlds_out_info(Globals, output_debug),
    VarNamePrint = print_name_and_num,
    TypeQual = no_tvarset_var_table,
    InfoGoal = hlds_out_info_goal(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, TypeQual),
    Indent = 0,
    Follow = "",
    do_write_goal(InfoGoal, Stream, Indent, Follow, Goal, !IO).

dump_goal_nl(Stream, ModuleInfo, VarNameSrc, TVarSet, InstVarSet, Goal, !IO) :-
    dump_goal(Stream, ModuleInfo, VarNameSrc, TVarSet, InstVarSet, Goal, !IO),
    io.nl(Stream, !IO).

write_goal(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, !IO) :-
    % Do not type qualify everything.
    TypeQual = no_tvarset_var_table,
    InfoGoal = hlds_out_info_goal(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, TypeQual),
    do_write_goal(InfoGoal, Stream, Indent, Follow, Goal, !IO).

write_goal_nl(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, !IO) :-
    write_goal(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, Follow, Goal, !IO),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%

do_write_goal(InfoGoal, Stream, Indent, Follow, Goal, !IO) :-
    % Write out goal_infos in the form of annotations around goal expressions.

    IndentStr = indent2_string(Indent),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'c') then
        Context = goal_info_get_context(GoalInfo),
        maybe_output_context_comment(Stream, Indent, "", Context, !IO)
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'P') then
        GoalId = goal_info_get_goal_id(GoalInfo),
        GoalId = goal_id(GoalIdNum),
        ( if GoalIdNum < 0 then
            true
        else
            io.format(Stream, "%s%% goal id: %d\n",
                [s(IndentStr), i(GoalIdNum)], !IO)
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'n') then
        NonLocalsSet = goal_info_get_nonlocals(GoalInfo),
        set_of_var.to_sorted_list(NonLocalsSet, NonLocalsList),
        (
            NonLocalsList = [_ | _],
            NonLocalsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                NonLocalsList),
            io.format(Stream, "%s%% nonlocals: %s\n",
                [s(IndentStr), s(NonLocalsStr)], !IO)
        ;
            NonLocalsList = []
        )
    else
        true
    ),
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    ( if string.contains_char(DumpOptions, 'p') then
        ( if
            goal_info_maybe_get_pre_deaths(GoalInfo, PreDeaths),
            PreDeathList = set_of_var.to_sorted_list(PreDeaths),
            PreDeathList = [_ | _]
        then
            PreDeathStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                PreDeathList),
            io.format(Stream, "%s%% pre-deaths: %s\n",
                [s(IndentStr), s(PreDeathStr)], !IO)
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
            io.format(Stream, "%s%% pre-births: %s\n",
                [s(IndentStr), s(PreBirthStr)], !IO)
        else
            true
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'B') then
        ProducingVars = GoalInfo ^ producing_vars,
        ( if set_of_var.is_non_empty(ProducingVars) then
            set_of_var.to_sorted_list(ProducingVars, ProducingVarsList),
            ProducingVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, ProducingVarsList),
            io.format(Stream, "%s%% producing vars: %s\n",
                [s(IndentStr), s(ProducingVarsStr)], !IO)
        else
            true
        ),

        ConsumingVars = GoalInfo ^ consuming_vars,
        ( if set_of_var.is_non_empty(ConsumingVars) then
            set_of_var.to_sorted_list(ConsumingVars, ConsumingVarsList),
            ConsumingVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, ConsumingVarsList),
            io.format(Stream, "%s%% consuming vars: %s\n",
                [s(IndentStr), s(ConsumingVarsStr)], !IO)
        else
            true
        ),

        MakeVisibleVars = GoalInfo ^ make_visible_vars,
        ( if set_of_var.is_non_empty(MakeVisibleVars) then
            set_of_var.to_sorted_list(MakeVisibleVars, MakeVisibleVarsList),
            MakeVisibleVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, MakeVisibleVarsList),
            io.format(Stream, "%s%% make_visible vars: %s\n",
                [s(IndentStr), s(MakeVisibleVarsStr)], !IO)
        else
            true
        ),

        NeedVisibleVars = GoalInfo ^ need_visible_vars,
        ( if set_of_var.is_non_empty(NeedVisibleVars) then
            set_of_var.to_sorted_list(NeedVisibleVars, NeedVisibleVarsList),
            NeedVisibleVarsStr = mercury_vars_to_string_src(VarNameSrc,
                VarNamePrint, NeedVisibleVarsList),
            io.format(Stream, "%s%% need_visible vars: %s\n",
                [s(IndentStr), s(NeedVisibleVarsStr)], !IO)
        else
            true
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'd') then
        Determinism = goal_info_get_determinism(GoalInfo),
        io.format(Stream, "%s%% determinism: %s\n",
            [s(IndentStr), s(determinism_to_string(Determinism))], !IO)
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'e') then
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

            io.format(Stream, "%s%% Created regions: %s\n",
                [s(IndentStr), s(CreatedStr)], !IO),
            io.format(Stream, "%s%% Removed regions: %s\n",
                [s(IndentStr), s(RemovedStr)], !IO),
            io.format(Stream, "%s%% Carried regions: %s\n",
                [s(IndentStr), s(CarriedStr)], !IO),
            io.format(Stream, "%s%% Allocated into regions: %s\n",
                [s(IndentStr), s(AllocStr)], !IO),
            io.format(Stream, "%s%% Used regions: %s\n",
                [s(IndentStr), s(UsedStr)], !IO)
        ;
            MaybeRbmmInfo = no
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'z') then
        Purity = goal_info_get_purity(GoalInfo),
        (
            Purity = purity_pure
        ;
            Purity = purity_semipure,
            io.format(Stream, "%s%% semipure\n", [s(IndentStr)], !IO)
        ;
            Purity = purity_impure,
            io.format(Stream, "%s%% impure\n", [s(IndentStr)], !IO)
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'E') then
        MaybeDPInfo = goal_info_get_maybe_dp_info(GoalInfo),
        (
            MaybeDPInfo = yes(dp_goal_info(MdprofInst, MaybeDPCoverageInfo)),
            (
                MdprofInst = goal_is_mdprof_inst,
                io.format(Stream, "%s%% mdprof instrumentation\n",
                    [s(IndentStr)], !IO)
            ;
                MdprofInst = goal_is_not_mdprof_inst
            ),
            (
                MaybeDPCoverageInfo = yes(CoverageInfo),
                CoverageInfo = dp_coverage_goal_info(IsTrivial,
                    PortCountsGiveCoverageAfter),
                (
                    IsTrivial = goal_is_trivial,
                    io.format(Stream, "%s%% trivial goal\n",
                        [s(IndentStr)], !IO)
                ;
                    IsTrivial = goal_is_nontrivial,
                    io.format(Stream, "%s%% nontrivial goal\n",
                        [s(IndentStr)], !IO)
                ),
                (
                    PortCountsGiveCoverageAfter =
                        port_counts_give_coverage_after,
                    io.format(Stream,
                        "%s%% port counts give coverage after\n",
                        [s(IndentStr)], !IO)
                ;
                    PortCountsGiveCoverageAfter =
                        no_port_counts_give_coverage_after,
                    io.format(Stream,
                        "%s%% no port counts give coverage after\n",
                        [s(IndentStr)], !IO)
                )
            ;
                MaybeDPCoverageInfo = no
            )
        ;
            MaybeDPInfo = no
        )
    else
        true
    ),
    write_goal_expr(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO),
    ( if string.contains_char(DumpOptions, 'i') then
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
            ( if string.contains_char(DumpOptions, 'D') then
                ( if instmap_delta_is_unreachable(InstMapDelta) then
                    io.format(Stream, "%s%% new insts: unreachable\n",
                        [s(IndentStr)], !IO)
                else if string.contains_char(DumpOptions, 'Y') then
                    instmap_delta_to_assoc_list(InstMapDelta, NewVarInsts),
                    NewVarInstStrs = list.map(
                        new_var_inst_msg_to_string(InfoGoal, IndentStr),
                        NewVarInsts),
                    io.format(Stream, "%s%% new insts:\n",
                        [s(IndentStr)], !IO),
                    list.foldl(io.write_string(Stream), NewVarInstStrs, !IO)
                else
                    instmap_delta_to_assoc_list(InstMapDelta, NewVarInsts),
                    NewVarInstStrs = list.map(
                        new_var_inst_to_string(InfoGoal, IndentStr),
                        NewVarInsts),
                    io.format(Stream, "%s%% new insts:\n",
                        [s(IndentStr)], !IO),
                    list.foldl(io.write_string(Stream), NewVarInstStrs, !IO)
                )
            else
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
                io.format(Stream, "%s%% vars with new insts: %s\n",
                    [s(IndentStr), s(NewVarsStr)], !IO)
            )
        )

        % XXX Until work starts on the new constraint based mode system,
        % printing goal_modes would be just clutter.
%       GoalMode = goal_info_get_goal_mode(GoalInfo),
%       PrefixStr = indent_string(Indent) ++ "% ",
%       GoalModeStrs = dump_goal_mode(PrefixStr, VarNameSrc, GoalMode),
%       list.foldl(io.write_string(Stream), GoalModeStrs, !IO)
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'p') then
        ( if
            goal_info_maybe_get_post_deaths(GoalInfo, PostDeaths),
            PostDeathList = set_of_var.to_sorted_list(PostDeaths),
            PostDeathList = [_ | _]
        then
            PostDeathStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                PostDeathList),
            io.format(Stream, "%s%% post-deaths: %s\n",
                [s(IndentStr), s(PostDeathStr)], !IO)
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
            io.format(Stream, "%s%% post-births: %s\n",
                [s(IndentStr), s(PostBirthStr)], !IO)
        else
            true
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'R') then
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

            io.format(Stream, "%s%% LFU: %s\n",
                [s(IndentStr), s(StrLFU)], !IO),
            io.format(Stream, "%s%% LBU: %s\n",
                [s(IndentStr), s(StrLBU)], !IO),

            io.format(Stream, "%s%% Reuse: ", [s(IndentStr)], !IO),
            (
                ReuseDescription = no_reuse_info,
                io.write_string(Stream, "no reuse info", !IO)
            ;
                ReuseDescription = no_possible_reuse,
                io.write_string(Stream, "no possible reuse", !IO)
            ;
                ReuseDescription = missed_reuse(Messages),
                io.write_string(Stream, "missed (", !IO),
                write_out_list(add_string, ", ", Messages, Stream, !IO),
                io.write_string(Stream, ")", !IO)
            ;
                ReuseDescription = potential_reuse(ShortReuseDescr),
                io.write_string(Stream, "potential reuse (", !IO),
                write_short_reuse_description(Stream, ShortReuseDescr,
                    VarNameSrc, VarNamePrint, !IO),
                io.write_string(Stream, ")", !IO)
            ;
                ReuseDescription = reuse(ShortReuseDescr),
                io.write_string(Stream, "reuse (", !IO),
                write_short_reuse_description(Stream, ShortReuseDescr,
                    VarNameSrc, VarNamePrint, !IO),
                io.write_string(Stream, ")", !IO)
            ),
            io.write_string(Stream, "\n", !IO)
        else
            true
        )
    else
        true
    ),
    CodeGenInfo = goal_info_get_code_gen_info(GoalInfo),
    (
        CodeGenInfo = no_code_gen_info
    ;
        CodeGenInfo = llds_code_gen_info(_CodeGenDetails),
        write_llds_code_gen_info(Info, Stream, GoalInfo, VarNameSrc,
            VarNamePrint, Indent, !IO)
    ),
    ( if string.contains_char(DumpOptions, 'g') then
        Features = goal_info_get_features(GoalInfo),
        set.to_sorted_list(Features, FeatureList),
        (
            FeatureList = []
        ;
            FeatureList = [_ | _],
            io.format(Stream, "%s%% Goal features: %s\n",
                [s(IndentStr), s(string.string(FeatureList))], !IO)
        )
    else
        true
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
    ShortInstSuffix = [nl],
    LongInstPrefix = [nl],
    LongInstSuffix = [nl],
    InstPieces = error_msg_inst(ModuleInfo, InstVarSet, expand_named_insts,
        uod_developer(TVarSet), fixed_short_inst, ShortInstSuffix,
        LongInstPrefix, LongInstSuffix, Inst),
    ShortInstStr = error_pieces_to_one_line_string(InstPieces),
    ( if string.count_code_points(ShortInstStr) < 40 then
        string.format("%s%%   %s -> %s\n",
            [s(IndentStr), s(VarStr), s(ShortInstStr)], Str)
    else
        Prefix = IndentStr ++ "%   ",
        LongInstStr = error_pieces_to_multi_line_string(Prefix, InstPieces),
        string.format("%s%%   %s ->\n%s",
            [s(IndentStr), s(VarStr), s(LongInstStr)], Str)
    ).

write_goal_list(InfoGoal, Stream, Indent, Separator, Goals, !IO) :-
    (
        Goals = [HeadGoal | TailGoals],
        write_indent2(Stream, Indent, !IO),
        io.write_string(Stream, Separator, !IO),
        do_write_goal(InfoGoal, Stream, Indent + 1, "\n", HeadGoal, !IO),
        write_goal_list(InfoGoal, Stream, Indent, Separator, TailGoals, !IO)
    ;
        Goals = []
    ).

:- pred write_llds_code_gen_info(hlds_out_info::in, io.text_output_stream::in,
    hlds_goal_info::in, var_name_source::in, var_name_print::in, int::in,
    io::di, io::uo) is det.

write_llds_code_gen_info(Info, Stream, GoalInfo, VarNameSrc, VarNamePrint,
        Indent, !IO) :-
    DumpOptions = Info ^ hoi_dump_hlds_options,
    IndentStr = indent2_string(Indent),
    ( if string.contains_char(DumpOptions, 'f') then
        goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
        (
            MaybeFollowVars = yes(FollowVars),
            FollowVars = abs_follow_vars(FollowVarsMap, NextRegR, NextRegF),
            map.to_assoc_list(FollowVarsMap, FVlist),

            io.format(Stream, "%s%% follow vars: r%d, f%d\n",
                [s(IndentStr), i(NextRegR), i(NextRegF)], !IO),
            write_var_to_abs_locns(Stream, VarNameSrc, VarNamePrint, Indent,
                FVlist, !IO)
        ;
            MaybeFollowVars = no
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'r') then
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
            io.format(Stream, "%s%% resume point %s %s\n",
                [s(IndentStr), s(LocsStr), s(ResumeVarsStr)], !IO)
        )
    else
        true
    ),
    ( if
        string.contains_char(DumpOptions, 's'),
        goal_info_get_store_map(GoalInfo, StoreMap),
        map.to_assoc_list(StoreMap, StoreMapList),
        StoreMapList = [_ | _]
    then
        io.format(Stream, "%s%% store map:\n", [s(IndentStr)], !IO),
        write_var_to_abs_locns(Stream, VarNameSrc, VarNamePrint, Indent,
            StoreMapList, !IO)
    else
        true
    ),
    ( if
        string.contains_char(DumpOptions, 's'),
        goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall),
        MaybeNeedAcrossCall = yes(NeedAcrossCall)
    then
        NeedAcrossCall = need_across_call(CallForwardSet, CallResumeSet,
            CallNondetSet),
        CallForwardList = set_of_var.to_sorted_list(CallForwardSet),
        CallResumeList = set_of_var.to_sorted_list(CallResumeSet),
        CallNondetList = set_of_var.to_sorted_list(CallNondetSet),
        io.format(Stream, "%s%% need across call forward vars: ",
            [s(IndentStr)], !IO),
        (
            CallForwardList = [],
            io.write_string(Stream, "none\n", !IO)
        ;
            CallForwardList = [_ | _],
            write_vars(Stream, VarNameSrc, VarNamePrint, CallForwardList, !IO),
            io.write_string(Stream, "\n", !IO)
        ),

        io.format(Stream, "%s%% need across call resume vars: ",
            [s(IndentStr)], !IO),
        (
            CallResumeList = [],
            io.write_string(Stream, "none\n", !IO)
        ;
            CallResumeList = [_ | _],
            write_vars(Stream, VarNameSrc, VarNamePrint, CallResumeList, !IO),
            io.write_string(Stream, "\n", !IO)
        ),

        io.format(Stream, "%s%% need across call nondet vars: ",
            [s(IndentStr)], !IO),
        (
            CallNondetList = [],
            io.write_string(Stream, "none\n", !IO)
        ;
            CallNondetList = [_ | _],
            write_vars(Stream, VarNameSrc, VarNamePrint, CallNondetList, !IO),
            io.write_string(Stream, "\n", !IO)
        )
    else
        true
    ),
    ( if
        string.contains_char(DumpOptions, 's'),
        goal_info_get_maybe_need_in_resume(GoalInfo, MaybeNeedInResume),
        MaybeNeedInResume = yes(NeedInResume)
    then
        NeedInResume = need_in_resume(ResumeOnStack, ResumeResumeSet,
            ResumeNondetSet),
        ResumeResumeList = set_of_var.to_sorted_list(ResumeResumeSet),
        ResumeNondetList = set_of_var.to_sorted_list(ResumeNondetSet),

        (
            ResumeOnStack = yes,
            io.format(Stream, "%s%% resume point has stack label\n",
                [s(IndentStr)], !IO)
        ;
            ResumeOnStack = no,
            io.format(Stream, "%s%% resume point has no stack label\n",
                [s(IndentStr)], !IO)
        ),
        io.format(Stream, "%s%% need in resume resume vars: ",
            [s(IndentStr)], !IO),
        (
            ResumeResumeList = [],
            io.write_string(Stream, "none\n", !IO)
        ;
            ResumeResumeList = [_ | _],
            write_vars(Stream, VarNameSrc, VarNamePrint,
                ResumeResumeList, !IO),
            io.write_string(Stream, "\n", !IO)
        ),

        io.format(Stream, "%s%% need in resume nondet vars: ",
            [s(IndentStr)], !IO),
        (
            ResumeNondetList = [],
            io.write_string(Stream, "none\n", !IO)
        ;
            ResumeNondetList = [_ | _],
            write_vars(Stream, VarNameSrc, VarNamePrint,
                ResumeNondetList, !IO),
            io.write_string(Stream, "\n", !IO)
        )
    else
        true
    ),
    ( if
        string.contains_char(DumpOptions, 's'),
        goal_info_get_maybe_need_in_par_conj(GoalInfo, MaybeNeedInParConj),
        MaybeNeedInParConj = yes(NeedInParConj)
    then
        NeedInParConj = need_in_par_conj(ParConjSet),
        ParConjList = set_of_var.to_sorted_list(ParConjSet),
        io.format(Stream, "%s%% need in par_conj vars: ",
            [s(IndentStr)], !IO),
        write_vars(Stream, VarNameSrc, VarNamePrint, ParConjList, !IO),
        io.write_string(Stream, "\n", !IO)
    else
        true
    ).

write_var_to_abs_locns(_, _, _, _, [], !IO).
write_var_to_abs_locns(Stream, VarNameSrc, VarNamePrint, Indent,
        [Var - Loc | VarLocs], !IO) :-
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
    io.format(Stream, "%s%%\t%s\t-> %s%s\n",
        [s(IndentStr), s(VarStr), s(LocnStr), s(WidthStr)], !IO),
    write_var_to_abs_locns(Stream, VarNameSrc, VarNamePrint, Indent,
        VarLocs, !IO).

:- pred write_vars(io.text_output_stream::in, var_name_source::in,
    var_name_print::in, list(prog_var)::in, io::di, io::uo) is det.

write_vars(_, _, _, [], !IO).
write_vars(Stream, VarNameSrc, VarNamePrint, [Var], !IO) :-
    mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO).
write_vars(Stream, VarNameSrc, VarNamePrint, [Var1, Var2 | Vars], !IO) :-
    mercury_output_var_src(VarNameSrc, VarNamePrint, Var1, Stream, !IO),
    io.write_string(Stream, ", ", !IO),
    write_vars(Stream, VarNameSrc, VarNamePrint, [Var2 | Vars], !IO).

:- pred write_short_reuse_description(io.text_output_stream::in,
    short_reuse_description::in, var_name_source::in, var_name_print::in,
    io::di, io::uo) is det.

write_short_reuse_description(Stream, ShortDescription, VarNameSrc,
        VarNamePrint, !IO):-
    (
        ShortDescription = cell_died,
        io.write_string(Stream, "cell died", !IO)
    ;
        ShortDescription = cell_reused(Var, IsConditional, _, _),
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        io.format(Stream, "cell reuse - %s - %s",
            [s(VarStr), s(is_conditional_to_string(IsConditional))], !IO)
    ;
        ShortDescription = reuse_call(IsConditional, NoClobbers),
        io.format(Stream, "reuse call - %s, no clobbers = %s",
            [s(is_conditional_to_string(IsConditional)),
            s(string.string(NoClobbers))], !IO)
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

:- pred write_goal_expr(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, hlds_goal_expr::in, io::di, io::uo) is det.

write_goal_expr(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    (
        GoalExpr = unify(_, _, _, _, _),
        write_goal_unify(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = plain_call(_, _, _, _, _, _),
        write_goal_plain_call(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        write_goal_generic_call(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        write_goal_foreign_proc(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = conj(_, _),
        write_goal_conj(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = disj(_),
        write_goal_disj(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = switch(_, _, _),
        write_goal_switch(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = scope(_, _),
        write_goal_scope(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = if_then_else(_, _, _, _),
        write_goal_if_then_else(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = negation(_),
        write_goal_negation(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = shorthand(_),
        write_goal_shorthand(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out unifications.
%

    % write_goal_unify(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO):
    %
    % Write out a unification.
    %
:- pred write_goal_unify(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_unify),
    io::di, io::uo) is det.

write_goal_unify(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = unify(LHS, RHS, _, Unification, _),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    IndentStr = indent2_string(Indent),

    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    LHSStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, LHS),
    io.format(Stream, "%s%s = ", [s(IndentStr), s(LHSStr)], !IO),
    TypeQual = InfoGoal ^ hoig_type_qual,
    (
        TypeQual = tvarset_var_table(_, VarTable),
        lookup_var_type(VarTable, LHS, UniType),
        VarType = yes(UniType)
    ;
        TypeQual = no_tvarset_var_table,
        VarType = no
    ),
    write_unify_rhs_2(InfoGoal, Stream, Indent, VarType, RHS, !IO),
    io.write_string(Stream, Follow, !IO),
    ( if
        ( string.contains_char(DumpOptions, 'u')
        ; string.contains_char(DumpOptions, 'p')
        )
    then
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
                io.nl(Stream, !IO)
            ),
            write_unification(InfoGoal, Stream, Indent, Unification, !IO)
        )
    else
        true
    ).

write_unify_rhs(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, Indent, RHS, !IO) :-
    TypeQual = no_tvarset_var_table,
    InfoGoal = hlds_out_info_goal(Info, ModuleInfo, VarNameSrc, VarNamePrint,
        TVarSet, InstVarSet, TypeQual),
    write_unify_rhs_2(InfoGoal, Stream, Indent, no, RHS, !IO).

:- pred write_unify_rhs_2(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, maybe(mer_type)::in, unify_rhs::in, io::di, io::uo) is det.

write_unify_rhs_2(InfoGoal, Stream, Indent, MaybeType, RHS, !IO) :-
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    (
        RHS = rhs_var(Var),
        mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO)
    ;
        RHS = rhs_functor(ConsId0, IsExistConstruct, ArgVars),
        ( if
            IsExistConstruct = is_exist_constr,
            ConsId0 = cons(SymName0, Arity, TypeCtor)
        then
            add_new_prefix(SymName0, SymName),
            ConsId = cons(SymName, Arity, TypeCtor)
        else
            ConsId = ConsId0
        ),
        ModuleInfo = InfoGoal ^ hoig_module_info,
        RHSStr = functor_cons_id_to_string(ModuleInfo, VarNameSrc,
            VarNamePrint, ConsId, ArgVars),
        io.write_string(Stream, RHSStr, !IO),
        ( if
            MaybeType = yes(Type),
            InfoGoal ^ hoig_type_qual = tvarset_var_table(TVarSet, _)
        then
            io.write_string(Stream, " : ", !IO),
            mercury_output_type(TVarSet, VarNamePrint, Type, Stream, !IO)
        else
            true
        )
    ;
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, _EvalMethod,
            NonLocals, VarsModes, Det, Goal),
        IndentStr = indent2_string(Indent),
        Indent1 = Indent + 1,
        io.write_string(Stream, purity_prefix_to_string(Purity), !IO),
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
            io.write_string(Stream, "(", !IO),
            (
                VarsModes = [],
                io.format(Stream, "(%s)", [s(Functor)], !IO)
            ;
                VarsModes = [_ | _],
                InstVarSet = InfoGoal ^ hoig_inst_varset,
                ModesStr = var_modes_to_string(Lang, VarNameSrc, InstVarSet,
                    VarNamePrint, VarsModes),
                io.format(Stream, "%s(%s)", [s(Functor), s(ModesStr)], !IO)
            ),
            DetStr = mercury_det_to_string(Det),
            io.format(Stream, " is %s :-\n", [s(DetStr)], !IO),
            do_write_goal(InfoGoal, Stream, Indent1, "\n", Goal, !IO),
            io.format(Stream, "%s)", [s(IndentStr)], !IO)
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
            io.write_string(Stream, "(", !IO),
            InstVarSet = InfoGoal ^ hoig_inst_varset,
            (
                ArgVarsModes = [],
                io.format(Stream, "(%s)", [s(Functor)], !IO)
            ;
                ArgVarsModes = [_ | _],
                ArgModesStr = var_modes_to_string(Lang, VarNameSrc, InstVarSet,
                    VarNamePrint, ArgVarsModes),
                io.format(Stream, "%s(%s)",
                    [s(Functor), s(ArgModesStr)], !IO)
            ),
            RetModeStr = var_mode_to_string(Lang, VarNameSrc, InstVarSet,
                VarNamePrint, RetVarMode),
            DetStr = mercury_det_to_string(Det),
            io.format(Stream, " = (%s) is %s :-\n",
                [s(RetModeStr), s(DetStr)], !IO),
            do_write_goal(InfoGoal, Stream, Indent1, "\n", Goal, !IO),
            io.format(Stream, "%s)", [s(IndentStr)], !IO)
        ),
        ( if
            MaybeType = yes(Type),
            InfoGoal ^ hoig_type_qual = tvarset_var_table(TVarSet, _)
        then
            io.write_string(Stream, " : ", !IO),
            mercury_output_type(TVarSet, VarNamePrint, Type, Stream, !IO)
        else
            true
        ),
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'n') then
            (
                NonLocals = [_ | _],
                NonLocalsStr = mercury_vars_to_string_src(VarNameSrc,
                    VarNamePrint, NonLocals),
                io.format(Stream, "\n%s%% lambda nonlocals: %s",
                    [s(IndentStr), s(NonLocalsStr)], !IO)
            ;
                NonLocals = []
            )
        else
            true
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
            ConsId0 = cons(SymName0, Arity, TypeCtor)
        then
            add_new_prefix(SymName0, SymName),
            ConsId = cons(SymName, Arity, TypeCtor)
        else
            ConsId = ConsId0
        ),
        Str = functor_cons_id_to_string(ModuleInfo, vns_var_table(VarTable),
            VarNamePrint, ConsId, ArgVars)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _),
        Str = "lambda goal"
    ).

:- pred write_unification(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, unification::in, io::di, io::uo) is det.

write_unification(InfoGoal, Stream, Indent, Unification, !IO) :-
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    IndentStr = indent2_string(Indent),
    (
        Unification = assign(X, Y),
        XStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, X),
        YStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Y),
        io.format(Stream, "%s%% %s := %s\n",
            [s(IndentStr), s(XStr), s(YStr)], !IO)
    ;
        Unification = simple_test(X, Y),
        XStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, X),
        YStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Y),
        io.format(Stream, "%s%% %s == %s\n",
            [s(IndentStr), s(XStr), s(YStr)], !IO)
    ;
        Unification = construct(Var, ConsId, ArgVars, ArgModes, ConstructHow,
            Uniqueness, SubInfo),
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        io.format(Stream, "%s%% %s <= ", [s(IndentStr), s(VarStr)], !IO),
        write_functor_and_submodes(InfoGoal, Stream, Indent, ConsId,
            ArgVars, ArgModes, !IO),

        Info = InfoGoal ^ hoig_out_info,
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'u') then
            ( if ConsId = cons(_, _, TypeCtor) then
                TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
                TypeCtorSymNameStr = sym_name_to_string(TypeCtorSymName),
                io.format(Stream, "%s%% cons_id type_ctor: %s/%d\n",
                    [s(IndentStr), s(TypeCtorSymNameStr), i(TypeCtorArity)],
                    !IO)
            else
                true
            ),
            (
                Uniqueness = cell_is_unique,
                io.format(Stream, "%s%% cell_is_unique\n", [s(IndentStr)], !IO)
            ;
                Uniqueness = cell_is_shared
            ),
            (
                SubInfo = no_construct_sub_info
            ;
                SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize),
                (
                    MaybeTakeAddr = yes(TakeAddressFields),
                    io.format(Stream, "%s%% take address fields: %s\n",
                        [s(IndentStr), s(string.string(TakeAddressFields))],
                        !IO)
                ;
                    MaybeTakeAddr = no
                ),
                (
                    MaybeSize = yes(SizeSource),
                    io.format(Stream, "%s%% term size ", [s(IndentStr)], !IO),
                    (
                        SizeSource = known_size(KnownSize),
                        io.format(Stream, "const %d\n", [i(KnownSize)], !IO)
                    ;
                        SizeSource = dynamic_size(SizeVar),
                        SizeVarStr = mercury_var_to_string_src(VarNameSrc,
                            VarNamePrint, SizeVar),
                        io.format(Stream, "var %s\n", [s(SizeVarStr)], !IO)
                    )
                ;
                    MaybeSize = no
                )
            ),
            (
                ConstructHow = construct_dynamically
            ;
                ConstructHow = construct_statically(born_static),
                io.format(Stream,
                    "%s%% construct statically (born static)\n",
                    [s(IndentStr)], !IO)
            ;
                ConstructHow = construct_statically(marked_static),
                io.format(Stream,
                    "%s%% construct statically (marked static)\n",
                    [s(IndentStr)], !IO)
            ;
                ConstructHow = reuse_cell(CellToReuse),
                CellToReuse = cell_to_reuse(ReuseVar, _ReuseConsIds,
                    _FieldAssigns),
                ReuseVarStr = mercury_var_to_string_src(VarNameSrc,
                    VarNamePrint, ReuseVar),
                io.format(Stream, "%s%% reuse cell: %s\n",
                    [s(IndentStr), s(ReuseVarStr)], !IO)
            ;
                ConstructHow = construct_in_region(RegVar),
                RegVarStr = mercury_var_to_string_src(VarNameSrc,
                    VarNamePrint, RegVar),
                io.format(Stream, "%s%% construct in region: %s\n",
                    [s(IndentStr), s(RegVarStr)], !IO)
            )
        else
            true
        )
    ;
        Unification = deconstruct(Var, ConsId, ArgVars, ArgModes, CanFail,
            CanCGC),
        Info = InfoGoal ^ hoig_out_info,
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'G') then
            io.format(Stream, "%s%% Compile time garbage collect: %s\n",
                [s(IndentStr), s(string.string(CanCGC))], !IO)
        else
            true
        ),
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        (
            CanFail = can_fail,
            OpStr = "?="
        ;
            CanFail = cannot_fail,
            OpStr = "=>"
        ),
        io.format(Stream, "%s%% %s %s ",
            [s(IndentStr), s(VarStr), s(OpStr)], !IO),
        write_functor_and_submodes(InfoGoal, Stream, Indent, ConsId, ArgVars,
            ArgModes, !IO)
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
        io.format(Stream, "%s%% %s, mode: %s\n",
            [s(IndentStr), s(CanFailStr), s(ModeStr)], !IO),

        TypeInfoVarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
            TypeInfoVars),
        io.format(Stream, "%s%% type-info vars: %s\n",
            [s(IndentStr), s(TypeInfoVarsStr)], !IO)
    ).

:- pred write_functor_and_submodes(hlds_out_info_goal::in,
    io.text_output_stream::in, int::in,
    cons_id::in, list(prog_var)::in, list(unify_mode)::in,
    io::di, io::uo) is det.

write_functor_and_submodes(InfoGoal, Stream, Indent, ConsId, ArgVars,
        ArgUnifyModes0, !IO) :-
    ConsIdStr = cons_id_and_arity_to_string(ConsId),
    (
        ArgVars = [],
        io.format(Stream, "%s\n", [s(ConsIdStr)], !IO)
    ;
        ArgVars = [_ | _],
        VarNameSrc = InfoGoal ^ hoig_var_name_src,
        VarNamePrint = InfoGoal ^ hoig_var_name_print,
        ArgVarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
            ArgVars),
        io.format(Stream, "%s(%s)\n", [s(ConsIdStr), s(ArgVarsStr)], !IO),
        Info = InfoGoal ^ hoig_out_info,
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'a') then
            InstVarSet = InfoGoal ^ hoig_inst_varset,
            IndentStr = indent2_string(Indent),
            list.map(limit_size_of_unify_mode, ArgUnifyModes0, ArgUnifyModes),
            ( if string.contains_char(DumpOptions, 'y') then
                io.format(Stream, "%s%% arg-modes\n", [s(IndentStr)], !IO),
                mercury_output_structured_unify_mode_list(Stream, output_debug,
                    InstVarSet, do_incl_addr, Indent, ArgUnifyModes, !IO)
            else
                write_arg_modes(Stream, InstVarSet, IndentStr, 1,
                    ArgUnifyModes, !IO)
            )
        else
            true
        )
    ).

:- pred write_arg_modes(io.text_output_stream::in, inst_varset::in,
    string::in, int::in, list(unify_mode)::in, io::di, io::uo) is det.

write_arg_modes(_Stream, _InstVarSet, _Indent, _ArgNum, [], !IO).
write_arg_modes(Stream, InstVarSet, IndentStr, ArgNum,
        [UnifyMode | UnifyModes], !IO) :-
    UnifyModeStr = mercury_unify_mode_to_string(InstVarSet, UnifyMode),
    io.format(Stream, "%s%% arg-mode %d %s\n",
        [s(IndentStr), i(ArgNum), s(UnifyModeStr)], !IO),
    write_arg_modes(Stream, InstVarSet, IndentStr, ArgNum + 1,
        UnifyModes, !IO).

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
                Inst0 = bound(Uniq, TestResults, BoundInsts0),
                limit_size_of_bound_insts(Levels - 1, BoundInsts0, BoundInsts),
                Inst = bound(Uniq, TestResults, BoundInsts)
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

:- pred limit_size_of_bound_insts(int::in,
    list(bound_inst)::in, list(bound_inst)::out) is det.

limit_size_of_bound_insts(_, [], []).
limit_size_of_bound_insts(Levels,
        [BoundInst0 | BoundInsts0], [BoundInst | BoundInsts]) :-
    BoundInst0 = bound_functor(ConsId, ArgInsts0),
    list.map(limit_size_of_inst(Levels), ArgInsts0, ArgInsts),
    BoundInst = bound_functor(ConsId, ArgInsts),
    limit_size_of_bound_insts(Levels, BoundInsts0, BoundInsts).

%---------------------------------------------------------------------------%
%
% Write out ordinary first-order calls.
%

:- pred write_goal_plain_call(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_plain_call),
    io::di, io::uo) is det.

write_goal_plain_call(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = plain_call(PredId, ProcId, ArgVars, Builtin,
        MaybeUnifyContext, PredName),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    IndentStr = indent2_string(Indent),
    ( if string.contains_char(DumpOptions, 'b') then
        (
            Builtin = inline_builtin,
            io.format(Stream, "%s%% inline builtin\n", [s(IndentStr)], !IO)
        ;
            Builtin = not_builtin
        )
    else
        true
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
    io.format(Stream, "%s%s", [s(IndentStr), s(PrefixStr)], !IO),
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    (
        PredOrFunc = pf_predicate,
        InParenArgVars = ArgVars
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, InParenArgVars, LHSVar),
        mercury_output_var_src(VarNameSrc, VarNamePrint, LHSVar, Stream, !IO),
        io.write_string(Stream, " = ", !IO)
    ),
    InParenArgVarsStr = sym_name_and_args_to_string(VarNameSrc, VarNamePrint,
        PredName, InParenArgVars),
    io.format(Stream, "%s%s", [s(InParenArgVarsStr), s(Follow)], !IO),
    ( if string.contains_char(DumpOptions, 'l') then
        pred_id_to_int(PredId, PredNum),
        proc_id_to_int(ProcId, ProcNum),
        io.format(Stream, "%s%% pred id: %i, proc id: %d%s",
            [s(IndentStr), i(PredNum), i(ProcNum), s(Follow)], !IO),
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
            io.format(Stream, "%s%% unify context: %s = ",
                [s(IndentStr), s(VarStr)], !IO),
            write_unify_rhs_2(InfoGoal, Stream, Indent, VarType, RHS, !IO),
            % XXX If we print Follow here, we shouldn't have printed it above.
            io.write_string(Stream, Follow, !IO)
        ;
            MaybeUnifyContext = no
        )
    else
        true
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

:- pred write_goal_generic_call(hlds_out_info_goal::in,
    io.text_output_stream::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_generic_call), io::di, io::uo) is det.

write_goal_generic_call(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = generic_call(GenericCall, ArgVars, Modes, MaybeArgRegs, _),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    IndentStr = indent2_string(Indent),
    % XXX We should print more info here.
    (
        GenericCall = higher_order(PredVar, Purity, PredOrFunc, _),
        PurityPrefix = purity_prefix_to_string(Purity),
        (
            PredOrFunc = pf_predicate,
            ( if string.contains_char(DumpOptions, 'l') then
                io.format(Stream, "%s%% higher-order predicate call\n",
                    [s(IndentStr)], !IO),
                write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
            else
                true
            ),
            CallStr = functor_to_string(VarNameSrc, VarNamePrint,
                term.atom("call"), [PredVar | ArgVars]),
            io.format(Stream, "%s%s%s",
                [s(IndentStr), s(PurityPrefix), s(CallStr)], !IO)
        ;
            PredOrFunc = pf_function,
            ( if string.contains_char(DumpOptions, 'l') then
                io.format(Stream, "%s%% higher-order function application\n",
                    [s(IndentStr)], !IO),
                write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
            else
                true
            ),
            pred_args_to_func_args([PredVar | ArgVars],
                FuncArgVars, FuncRetVar),
            FuncRetVarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint,
                FuncRetVar),
            ApplyStr = functor_to_string(VarNameSrc, VarNamePrint,
                term.atom("apply"), FuncArgVars),
            io.format(Stream, "%s%s%s = %s",
                [s(IndentStr), s(PurityPrefix), s(FuncRetVarStr), s(ApplyStr)],
                !IO)
        ),
        io.write_string(Stream, Follow, !IO)
    ;
        GenericCall = class_method(TCInfoVar, method_proc_num(MethodNum),
            _ClassId, _MethodId),
        ( if string.contains_char(DumpOptions, 'l') then
            io.format(Stream, "%s%% class method call\n", [s(IndentStr)], !IO),
            write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
        else
            true
        ),
        Context = dummy_context,
        Functor = term.atom("class_method_call"),
        TCInfoTerm = term.variable(TCInfoVar, Context),
        MethodNumTerm = term_int.int_to_decimal_term(MethodNum, Context),
        term_subst.var_list_to_term_list(ArgVars, ArgTerms),
        Term = term.functor(Functor, [TCInfoTerm, MethodNumTerm | ArgTerms],
            Context),
        io.write_string(Stream, IndentStr, !IO),
        mercury_output_term_src(VarNameSrc, VarNamePrint, Term, Stream, !IO),
        io.write_string(Stream, Follow, !IO)
    ;
        GenericCall = event_call(EventName),
        ( if string.contains_char(DumpOptions, 'l') then
            io.format(Stream, "%s%% event call\n", [s(IndentStr)], !IO),
            write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
        else
            true
        ),
        Functor = term.atom(EventName),
        term_subst.var_list_to_term_list(ArgVars, ArgTerms),
        Term = term.functor(Functor, ArgTerms, dummy_context),
        io.format(Stream, "%sevent ", [s(IndentStr)], !IO),
        mercury_output_term_src(VarNameSrc, VarNamePrint, Term, Stream, !IO),
        io.write_string(Stream, Follow, !IO)
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
        ( if string.contains_char(DumpOptions, 'l') then
            io.format(Stream, "%s%% %s\n",
                [s(IndentStr), s(CastTypeString)], !IO),
            write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
        else
            true
        ),
        ( if string.contains_char(DumpOptions, 'D') then
            varset.init(InstVarSet),
            ModesStr = mercury_mode_list_to_string(output_debug, InstVarSet,
                Modes),
            io.format(Stream, "%s%% modes: %s\n",
                [s(IndentStr), s(ModesStr)], !IO)
        else
            true
        ),
        PredOrFunc = write_cast_as_pred_or_func(CastType),
        (
            PredOrFunc = pf_predicate,
            CallStr = functor_to_string(VarNameSrc, VarNamePrint,
                term.atom(CastTypeString), ArgVars),
            io.format(Stream, "%s%s",
                [s(IndentStr), s(CallStr)], !IO)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(ArgVars, FuncArgVars, FuncRetVar),
            FuncRetVarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint,
                FuncRetVar),
            CallStr = functor_to_string(VarNameSrc, VarNamePrint,
                term.atom(CastTypeString), FuncArgVars),
            io.format(Stream, "%s%s = %s",
                [s(IndentStr), s(FuncRetVarStr), s(CallStr)], !IO)
        ),
        io.write_string(Stream, Follow, !IO)
    ).

:- pred write_ho_arg_regs(io.text_output_stream::in, int::in,
    arg_reg_type_info::in, io::di, io::uo) is det.

write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO) :-
    (
        MaybeArgRegs = arg_reg_types(ArgRegs),
        IndentStr = indent2_string(Indent),
        ArgRegStrs = list.map(ho_arg_reg_to_string, ArgRegs),
        ArgRegsStr = string.join_list(", ", ArgRegStrs),
        io.format(Stream, "%s%% arg regs: %s\n",
            [s(IndentStr), s(ArgRegsStr)], !IO)
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

:- pred write_goal_foreign_proc(hlds_out_info_goal::in,
    io.text_output_stream::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_foreign_proc), io::di, io::uo) is det.

write_goal_foreign_proc(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = call_foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
        MaybeTraceRuntimeCond, PragmaCode),
    ForeignLang = get_foreign_language(Attributes),
    ForeignLangStr = foreign_language_string(ForeignLang),
    ModuleInfo = InfoGoal ^ hoig_module_info,
    PredStr = pred_id_to_dev_string(ModuleInfo, PredId),
    pred_id_to_int(PredId, PredIdInt),
    proc_id_to_int(ProcId, ProcIdInt),

    IndentStr = indent2_string(Indent),
    io.format(Stream, "%s$pragma_foreign_proc(/* %s */, %s pred %d proc %d,\n",
        [s(IndentStr), s(ForeignLangStr),
        s(PredStr), i(PredIdInt), i(ProcIdInt)], !IO),
    (
        MaybeTraceRuntimeCond = no
    ;
        MaybeTraceRuntimeCond = yes(TraceRuntimeCond),
        io.format(Stream, "%s%% trace_runtime_cond(", [s(IndentStr)], !IO),
        mercury_output_trace_expr(Stream, mercury_output_trace_runtime,
            TraceRuntimeCond, !IO),
        io.write_string(Stream, ")\n", !IO)
    ),
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    TypeVarSet = InfoGoal ^ hoig_tvarset,
    InstVarSet = InfoGoal ^ hoig_inst_varset,
    ArgsStr = foreign_args_to_string(VarNameSrc, VarNamePrint,
        TypeVarSet, InstVarSet, IndentStr, "[", "],", Args),
    io.write_string(Stream, ArgsStr, !IO),
    (
        ExtraArgs = []
    ;
        ExtraArgs = [_ | _],
        ExtraArgsStr = foreign_args_to_string(VarNameSrc, VarNamePrint,
            TypeVarSet, InstVarSet, IndentStr, "{", "},", ExtraArgs),
        io.write_string(Stream, ExtraArgsStr, !IO)
    ),
    PragmaCode = fp_impl_ordinary(Code, _),
    io.format(Stream, """%s"")%s", [s(Code), s(Follow)], !IO).

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

:- pred write_goal_conj(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_conj), io::di,
    io::uo) is det.

write_goal_conj(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = conj(ConjType, List),
    (
        List = [Goal | Goals],
        (
            ConjType = plain_conj,
            Info = InfoGoal ^ hoig_out_info,
            DumpOptions = Info ^ hoi_dump_hlds_options,
            ( if DumpOptions = "" then
                write_conj(InfoGoal, Stream, Indent, Follow, ",\n",
                    Goal, Goals, !IO)
            else
                IndentStr = indent2_string(Indent),
                io.format(Stream, "%s( %% conjunction\n", [s(IndentStr)], !IO),
                write_conj(InfoGoal, Stream, Indent + 1, "\n", ",\n",
                    Goal, Goals, !IO),
                io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO)
            )
        ;
            ConjType = parallel_conj,
            IndentStr = indent2_string(Indent),
            io.format(Stream, "%s( %% parallel conjunction\n",
                [s(IndentStr)], !IO),
            do_write_goal(InfoGoal, Stream, Indent + 1, "\n", Goal, !IO),
            % See comments at write_goal_list.
            write_goal_list(InfoGoal, Stream, Indent, "&\n", Goals, !IO),
            io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO)
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
        io.format(Stream, "%s%strue%s",
            [s(IndentStr), s(ParStr), s(Follow)], !IO)
    ).

:- pred write_conj(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, string::in, hlds_goal::in, list(hlds_goal)::in,
    io::di, io::uo) is det.

write_conj(InfoGoal, Stream, Indent, Follow, Separator, Goal1, Goals1, !IO) :-
    (
        Goals1 = [Goal2 | Goals2],
        Info = InfoGoal ^ hoig_out_info,
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if DumpOptions = "" then
            do_write_goal(InfoGoal, Stream, Indent, Separator, Goal1, !IO)
        else
            % When generating verbose dumps, we want the comma on its own line,
            % since that way it visually separates the lines after one goal
            % and the lines before the next.
            do_write_goal(InfoGoal, Stream, Indent, "\n", Goal1, !IO),
            IndentStr = indent2_string(Indent),
            io.format(Stream, "%s%s", [s(IndentStr), s(Separator)], !IO)
        ),
        write_conj(InfoGoal, Stream, Indent, Follow, Separator,
            Goal2, Goals2, !IO)
    ;
        Goals1 = [],
        do_write_goal(InfoGoal, Stream, Indent, Follow, Goal1, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out disjunctions.
%

:- pred write_goal_disj(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_disj),
    io::di, io::uo) is det.

write_goal_disj(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = disj(Disjuncts),
    IndentStr = indent2_string(Indent),
    (
        Disjuncts = [Goal | Goals],
        io.format(Stream, "%s( %% disjunction\n", [s(IndentStr)], !IO),
        do_write_goal(InfoGoal, Stream, Indent + 1, "\n", Goal, !IO),
        write_goal_list(InfoGoal, Stream, Indent, ";\n", Goals, !IO),
        io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO)
    ;
        Disjuncts = [],
        io.format(Stream, "%sfail%s", [s(IndentStr), s(Follow)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out switches.
%

:- pred write_goal_switch(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_switch),
    io::di, io::uo) is det.

write_goal_switch(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = switch(Var, CanFail, CasesList),
    IndentStr = indent2_string(Indent),
    CanFailStr = can_fail_to_string(CanFail),
    VarNameSrc = InfoGoal ^ hoig_var_name_src,
    VarNamePrint = InfoGoal ^ hoig_var_name_print,
    VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
    io.format(Stream, "%s( %% %s switch on `%s'\n",
        [s(IndentStr), s(CanFailStr), s(VarStr)], !IO),
    Indent1 = Indent + 1,
    (
        CasesList = [Case | Cases],
        write_case(InfoGoal, Stream, Indent1, Var, Case, !IO),
        write_cases(InfoGoal, Stream, Indent, Var, Cases, !IO)
    ;
        CasesList = [],
        io.format(Stream, "%sfail\n", [s(IndentStr)], !IO)
    ),
    io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO).

:- pred write_cases(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, prog_var::in, list(case)::in, io::di, io::uo) is det.

write_cases(InfoGoal, Stream, Indent, Var, CasesList, !IO) :-
    (
        CasesList = [Case | Cases],
        IndentStr = indent2_string(Indent),
        io.format(Stream, "%s;\n", [s(IndentStr)], !IO),
        write_case(InfoGoal, Stream, Indent + 1, Var, Case, !IO),
        write_cases(InfoGoal, Stream, Indent, Var, Cases, !IO)
    ;
        CasesList = []
    ).

:- pred write_case(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, prog_var::in, case::in, io::di, io::uo) is det.

write_case(InfoGoal, Stream, Indent, Var, Case, !IO) :-
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
    io.format(Stream, "%s%% %s has functor %s\n",
        [s(IndentStr), s(VarStr), s(ConsIdsStr)], !IO),
    % XXX if the output of this is to be used, e.g. in
    % inter-module optimization, output a unification to bind the
    % Var to the functor, since simplify.m and unused_args.m remove
    % the unification. At the moment this is not a problem, since
    % intermod.m works on the unoptimized clauses.
    do_write_goal(InfoGoal, Stream, Indent, "\n", Goal, !IO).

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

:- pred write_goal_negation(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_neg),
    io::di, io::uo) is det.

write_goal_negation(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = negation(Goal),
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%snot (\n", [s(IndentStr)], !IO),
    do_write_goal(InfoGoal, Stream, Indent + 1, "\n", Goal, !IO),
    io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO).

%---------------------------------------------------------------------------%
%
% Write out if-then-elses.
%

:- pred write_goal_if_then_else(hlds_out_info_goal::in,
    io.text_output_stream::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_ite), io::di, io::uo) is det.

write_goal_if_then_else(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
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
    io.format(Stream, "%s( if%s\n", [s(IndentStr), s(SomeVarsStr)], !IO),

    Indent1 = Indent + 1,
    do_write_goal(InfoGoal, Stream, Indent1, "\n", Cond, !IO),
    io.format(Stream, "%sthen\n", [s(IndentStr)], !IO),
    do_write_goal(InfoGoal, Stream, Indent1, "\n", Then, !IO),
    io.format(Stream, "%selse\n", [s(IndentStr)], !IO),
    Info = InfoGoal ^ hoig_out_info,
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if
        DumpOptions \= "",
        Else = hlds_goal(if_then_else(_, _, _, _), _)
    then
        ElseIndent = Indent
    else
        ElseIndent = Indent1
    ),
    do_write_goal(InfoGoal, Stream, ElseIndent, "\n", Else, !IO),
    io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO).

%---------------------------------------------------------------------------%
%
% Write out scope goals.
%

:- pred write_goal_scope(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_scope),
    io::di, io::uo) is det.

write_goal_scope(!.InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = scope(Reason, Goal),
    IndentStr = indent2_string(Indent),
    io.write_string(Stream, IndentStr, !IO),
    (
        Reason = exist_quant(Vars, Creator),
        VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
        VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
        VarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint, Vars),
        ( Creator = user_quant,     CreatorStr = "user"
        ; Creator = compiler_quant, CreatorStr = "compiler"
        ),
        io.format(Stream, "some [%s] ( %% %s\n",
            [s(VarsStr), s(CreatorStr)], !IO)
    ;
        Reason = disable_warnings(HeadWarning, TailWarnings),
        io.write_string(Stream, "disable_warnings [", !IO),
        write_goal_warnings(Stream, HeadWarning, TailWarnings, !IO),
        io.write_string(Stream, "] (\n", !IO)
    ;
        Reason = promise_purity(Purity),
        ( Purity = purity_pure,         PromiseStr = "promise_pure"
        ; Purity = purity_semipure,     PromiseStr = "promise_semipure"
        ; Purity = purity_impure,       PromiseStr = "promise_impure"
        ),
        io.format(Stream, "%s (\n", [s(PromiseStr)], !IO)
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
        io.format(Stream, "%s [%s] (\n", [s(PromiseKindStr), s(VarsStr)], !IO)
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
        io.format(Stream, "%s (\n", [s(ReqStr)], !IO)
    ;
        Reason = require_complete_switch(Var),
        VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
        VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        io.format(Stream, "require_complete_switch [%s] (\n", [s(VarStr)], !IO)
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
        io.format(Stream, "%s [%s] (\n", [s(ReqStr), s(VarStr)], !IO)
    ;
        Reason = barrier(removable),
        io.write_string(Stream, "(\n", !IO),
        write_indent2(Stream, Indent, !IO),
        io.write_string(Stream, "% barrier(removable)\n", !IO)
    ;
        Reason = barrier(not_removable),
        io.write_string(Stream, "(\n", !IO),
        write_indent2(Stream, Indent, !IO),
        io.write_string(Stream, "% barrier(not_removable)\n", !IO)
    ;
        Reason = commit(force_pruning),
        io.write_string(Stream, "(\n", !IO),
        write_indent2(Stream, Indent, !IO),
        io.write_string(Stream, "% commit(force_pruning)\n", !IO)
    ;
        Reason = commit(dont_force_pruning),
        io.write_string(Stream, "(\n", !IO),
        write_indent2(Stream, Indent, !IO),
        io.write_string(Stream, "% commit(dont_force_pruning)\n", !IO)
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
        io.write_string(Stream, "(\n", !IO),
        io.format(Stream, "%s%% from_ground_term [%s, %s]\n",
            [s(IndentStr), s(VarStr), s(KindStr)], !IO),
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
        io.write_string(Stream, "trace [\n", !IO),
        some [!AddCommaNewline] (
            !:AddCommaNewline = no,
            Indent1Str = indent2_string(Indent + 1),
            (
                MaybeCompileTime = yes(CompileTime),
                io.format(Stream, "%scompile_time(", [s(Indent1Str)], !IO),
                mercury_output_trace_expr(Stream,
                    mercury_output_trace_compiletime, CompileTime, !IO),
                io.write_string(Stream, ")", !IO),
                !:AddCommaNewline = yes
            ;
                MaybeCompileTime = no
            ),
            (
                MaybeRunTime = yes(RunTime),
                maybe_add_comma_newline(Stream, !.AddCommaNewline, !IO),
                io.format(Stream, "%sruntime(", [s(Indent1Str)], !IO),
                mercury_output_trace_expr(Stream, mercury_output_trace_runtime,
                    RunTime, !IO),
                io.write_string(Stream, ")", !IO),
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
                maybe_add_comma_newline(Stream, !.AddCommaNewline, !IO),
                io.format(Stream, "%sio(!%s)",
                    [s(Indent1Str), s(IOStateVarName)], !IO),
                !:AddCommaNewline = yes
            ;
                MaybeIO = no
            ),
            list.foldl2(write_trace_mutable_var_hlds(Stream, Indent1Str),
                MutableVars, !AddCommaNewline, !IO),
            (
                !.AddCommaNewline = no
            ;
                !.AddCommaNewline = yes,
                % There is nothing following that requires a comma.
                io.nl(Stream, !IO)
            ),
            (
                Lang = output_mercury
            ;
                Lang = output_debug,
                VarNameSrc = !.InfoGoal ^ hoig_var_name_src,
                VarNamePrint = !.InfoGoal ^ hoig_var_name_print,
                QuantVarsStr = mercury_vars_to_string_src(VarNameSrc,
                    VarNamePrint, QuantVars),
                io.format(Stream, "%s%% quantified vars [%s]\n",
                    [s(Indent1Str), s(QuantVarsStr)], !IO)
            ),
            write_indent2(Stream, Indent, !IO),
            io.write_string(Stream, "] (\n", !IO)
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
        io.format(Stream, "%s%% loop_control_spawn_off_%s(%s) (\n",
            [s(IndentStr), s(UseParentStackStr), s(LCVarsStr)], !IO)
    ),
    do_write_goal(!.InfoGoal, Stream, Indent + 1, "\n", Goal, !IO),
    io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO).

:- pred write_trace_mutable_var_hlds(io.text_output_stream::in, string::in,
    trace_mutable_var_hlds::in, bool::in, bool::out, io::di, io::uo) is det.

write_trace_mutable_var_hlds(Stream, IndentStr, MutableVar,
        !AddCommaNewline, !IO) :-
    MutableVar = trace_mutable_var_hlds(MutableName, StateVarName),
    maybe_add_comma_newline(Stream, !.AddCommaNewline, !IO),
    io.format(Stream, "%sstate(%s, !%s)",
        [s(IndentStr), s(MutableName), s(StateVarName)], !IO),
    !:AddCommaNewline = yes.

:- pred maybe_add_comma_newline(io.text_output_stream::in, bool::in,
    io::di, io::uo) is det.

maybe_add_comma_newline(Stream, AddCommaNewline, !IO) :-
    (
        AddCommaNewline = no
    ;
        AddCommaNewline = yes,
        io.write_string(Stream, ",\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out shorthand goals.
%

:- pred write_goal_shorthand(hlds_out_info_goal::in, io.text_output_stream::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_shorthand),
    io::di, io::uo) is det.

write_goal_shorthand(InfoGoal, Stream, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = shorthand(ShortHand),
    IndentStr = indent2_string(Indent),
    Indent1 = Indent + 1,
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
        io.format(Stream, "%satomic [%s %s %s] (\n",
            [s(IndentStr), s(OuterStr), s(InnerStr), s(MaybeOutputVarsStr)],
            !IO),
        do_write_goal(InfoGoal, Stream, Indent1, "\n", MainGoal, !IO),
        write_goal_list(InfoGoal, Stream, Indent, "or_else\n",
            OrElseGoals, !IO),
        io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO)
    ;
        ShortHand = try_goal(MaybeIO, _, SubGoal),
        io.format(Stream, "%s( %% try\n", [s(IndentStr)], !IO),
        (
            MaybeIO = yes(try_io_state_vars(IOVarA, IOVarB)),
            Indent1Str = indent2_string(Indent1),
            VarNameSrc = InfoGoal ^ hoig_var_name_src,
            VarNamePrint = InfoGoal ^ hoig_var_name_print,
            IOVarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                [IOVarA, IOVarB]),
            io.format(Stream, "%s%% io(%s)\n",
                [s(Indent1Str), s(IOVarsStr)], !IO)
        ;
            MaybeIO = no
        ),
        do_write_goal(InfoGoal, Stream, Indent1, "\n", SubGoal, !IO),
        io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO)
    ;
        ShortHand = bi_implication(GoalA, GoalB),
        io.format(Stream, "%s( %% bi-implication\n", [s(IndentStr)], !IO),
        do_write_goal(InfoGoal, Stream, Indent1, "\n", GoalA, !IO),
        io.format(Stream, "%s<=>\n", [s(IndentStr)], !IO),
        do_write_goal(InfoGoal, Stream, Indent1, "\n", GoalB, !IO),
        io.format(Stream, "%s)%s", [s(IndentStr), s(Follow)], !IO)
    ).

:- func atomic_interface_vars_to_string(var_name_source, var_name_print, string,
    atomic_interface_vars) = string.

atomic_interface_vars_to_string(VarNameSrc, VarNamePrint,
        CompName, CompState) = Str :-
    CompState = atomic_interface_vars(Var1, Var2),
    Var1Str = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var1),
    Var2Str = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var2),
    string.format("%s(%s, %s)", [s(CompName), s(Var1Str), s(Var2Str)], Str).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_goal.
%---------------------------------------------------------------------------%
