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
%   mercury_to_mercury.m
%   term_io.m
%
% mercury_to_mercury.m prints the parse tree data structure defined
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
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Print a goal in a way that is suitable for debugging the compiler
    % (but necessarily for anything else).
    %
:- pred dump_goal(io.text_output_stream::in, module_info::in,
    var_name_source::in, hlds_goal::in, io::di, io::uo) is det.

    % Print a goal in a way that is suitable for debugging the compiler
    % (but necessarily for anything else), followed by a newline.
    %
:- pred dump_goal_nl(io.text_output_stream::in, module_info::in,
    var_name_source::in, hlds_goal::in, io::di, io::uo) is det.

    % Print out an HLDS goal. The integer gives the level of indentation
    % to be used within the goal. The string says what should end the line
    % containing the goal; it should include a newline character, but may
    % also contain other characters before that.
    %
:- pred write_goal(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, var_name_print::in, int::in,
    string::in, hlds_goal::in, io::di, io::uo) is det.

    % As write_goal, but add a newline at the end.
    %
:- pred write_goal_nl(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, var_name_print::in, int::in,
    string::in, hlds_goal::in, io::di, io::uo) is det.

    % TypeQual is tvarset_var_table(TVarset, VarTable)
    % if all constructors should be module qualified.
    %
:- pred do_write_goal(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in, hlds_goal::in,
    io::di, io::uo) is det.

    % write_goal_list is used to write both disjunctions and parallel
    % conjunctions. The boolean says whether variables should have
    % their numbers appended to them. The integer gives the level of
    % indentation to be used within the goal. The string says what should be
    % on the line between each goal; it should include a newline character,
    % but may also contain other characters before that.
    %
:- pred write_goal_list(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in, list(hlds_goal)::in,
    io::di, io::uo) is det.

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
    module_info::in, var_name_source::in, inst_varset::in, var_name_print::in,
    int::in, unify_rhs::in, io::di, io::uo) is det.

    % Converts the right-hand-side of a unification to a string, similarly to
    % write_unify_rhs, but doesn't print any details for lambda goals.
    % The module_info and the varset give the context of the rhs. The boolean
    % says whether variables should have their numbers appended to them.
    %
:- func unify_rhs_to_string(module_info, var_name_source, var_name_print,
    unify_rhs) = string.

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

:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_clause.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

dump_goal(Stream, ModuleInfo, VarNameSrc, Goal, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    Info = init_hlds_out_info(Globals, output_debug),
    VarNamePrint = print_name_and_num,
    Indent = 0,
    Follow = "",
    TypeQual = no_tvarset_var_table,
    do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Follow, Goal, !IO).

dump_goal_nl(Stream, ModuleInfo, VarNameSrc, Goal, !IO) :-
    dump_goal(Stream, ModuleInfo, VarNameSrc, Goal, !IO),
    io.nl(Stream, !IO).

write_goal(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint, Indent, Follow,
        Goal, !IO) :-
    % Do not type qualify everything.
    do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, no_tvarset_var_table,
        VarNamePrint, Indent, Follow, Goal, !IO).

write_goal_nl(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint,
        Indent, Follow, Goal, !IO) :-
    write_goal(Info, Stream, ModuleInfo, VarNameSrc, VarNamePrint,
        Indent, Follow, Goal, !IO),
    io.nl(Stream, !IO).

do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Follow, Goal, !IO) :-
    % Write out goal_infos in the form of annotations around goal expressions.

    Goal = hlds_goal(GoalExpr, GoalInfo),
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
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% goal id: ", !IO),
            io.write_int(Stream, GoalIdNum, !IO),
            io.write_string(Stream, "\n", !IO)
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'n') then
        NonLocalsSet = goal_info_get_nonlocals(GoalInfo),
        set_of_var.to_sorted_list(NonLocalsSet, NonLocalsList),
        (
            NonLocalsList = [_ | _],
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% nonlocals: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, NonLocalsList,
                Stream, !IO),
            io.write_string(Stream, "\n", !IO)
        ;
            NonLocalsList = []
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'p') then
        ( if
            goal_info_maybe_get_pre_deaths(GoalInfo, PreDeaths),
            PreDeathList = set_of_var.to_sorted_list(PreDeaths),
            PreDeathList = [_ | _]
        then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% pre-deaths: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, PreDeathList,
                Stream, !IO),
            io.write_string(Stream, "\n", !IO)
        else
            true
        ),
        ( if
            goal_info_maybe_get_pre_births(GoalInfo, PreBirths),
            PreBirthList = set_of_var.to_sorted_list(PreBirths),
            PreBirthList = [_ | _]
        then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% pre-births: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, PreBirthList,
                Stream, !IO),
            io.write_string(Stream, "\n", !IO)
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
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% producing vars: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint,
                ProducingVarsList, Stream, !IO),
            io.write_string(Stream, "\n", !IO)
        else
            true
        ),

        ConsumingVars = GoalInfo ^ consuming_vars,
        ( if set_of_var.is_non_empty(ConsumingVars) then
            set_of_var.to_sorted_list(ConsumingVars, ConsumingVarsList),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% consuming vars: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint,
                ConsumingVarsList, Stream, !IO),
            io.write_string(Stream, "\n", !IO)
        else
            true
        ),

        MakeVisibleVars = GoalInfo ^ make_visible_vars,
        ( if set_of_var.is_non_empty(MakeVisibleVars) then
            set_of_var.to_sorted_list(MakeVisibleVars, MakeVisibleVarsList),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% make_visible vars: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint,
                MakeVisibleVarsList, Stream, !IO),
            io.write_string(Stream, "\n", !IO)
        else
            true
        ),

        NeedVisibleVars = GoalInfo ^ need_visible_vars,
        ( if set_of_var.is_non_empty(NeedVisibleVars) then
            set_of_var.to_sorted_list(NeedVisibleVars, NeedVisibleVarsList),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% need_visible vars: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint,
                NeedVisibleVarsList, Stream, !IO),
            io.write_string(Stream, "\n", !IO)
        else
            true
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'd') then
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% determinism: ", !IO),
        Determinism = goal_info_get_determinism(GoalInfo),
        io.write_string(Stream, determinism_to_string(Determinism), !IO),
        io.write_string(Stream, "\n", !IO)
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

            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% Created regions: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, CreatedList,
                Stream, !IO),
            io.nl(Stream, !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% Removed regions: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, RemovedList,
                Stream, !IO),
            io.nl(Stream, !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% Carried regions: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, CarriedList,
                Stream, !IO),
            io.nl(Stream, !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% Allocated into regions: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, AllocList,
                Stream, !IO),
            io.nl(Stream, !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% Used regions: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, UsedList,
                Stream, !IO),
            io.nl(Stream, !IO)
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
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% semipure\n", !IO)
        ;
            Purity = purity_impure,
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% impure\n", !IO)
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
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% mdprof instrumentation\n", !IO)
            ;
                MdprofInst = goal_is_not_mdprof_inst
            ),
            (
                MaybeDPCoverageInfo = yes(CoverageInfo),
                CoverageInfo = dp_coverage_goal_info(IsTrivial,
                    PortCountsGiveCoverageAfter),
                write_indent(Stream, Indent, !IO),
                (
                    IsTrivial = goal_is_trivial,
                    io.write_string(Stream, "% trivial goal\n", !IO)
                ;
                    IsTrivial = goal_is_nontrivial,
                    io.write_string(Stream, "% nontrivial goal\n", !IO)
                ),
                write_indent(Stream, Indent, !IO),
                (
                    PortCountsGiveCoverageAfter =
                        port_counts_give_coverage_after,
                    io.write_string(Stream,
                        "% port counts give coverage after\n", !IO)
                ;
                    PortCountsGiveCoverageAfter =
                        no_port_counts_give_coverage_after,
                    io.write_string(Stream,
                        "% no port counts give coverage after\n", !IO)
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
    write_goal_expr(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
        VarNamePrint, Indent, Follow, GoalExpr, !IO),
    ( if string.contains_char(DumpOptions, 'i') then
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        ( if
            instmap_delta_is_reachable(InstMapDelta),
            instmap_delta_changed_vars(InstMapDelta, Vars),
            set_of_var.is_empty(Vars)
        then
            true
        else
            write_indent(Stream, Indent, !IO),
            ( if string.contains_char(DumpOptions, 'D') then
                ( if instmap_delta_is_unreachable(InstMapDelta) then
                    io.write_string(Stream, "% new insts: unreachable\n", !IO)
                else
                    io.write_string(Stream, "% new insts:\n", !IO),
                    instmap_delta_to_assoc_list(InstMapDelta, NewVarInsts),
                    write_new_var_inst_list(Stream, VarNameSrc, VarNamePrint,
                        Indent, NewVarInsts, !IO)
                )
            else
                io.write_string(Stream, "% vars with new insts: ", !IO),
                write_instmap_delta_vars(Stream, VarNameSrc, VarNamePrint,
                    InstMapDelta, !IO),
                io.write_string(Stream, "\n", !IO)
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
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% post-deaths: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, PostDeathList,
                Stream, !IO),
            io.write_string(Stream, "\n", !IO)
        else
            true
        ),
        ( if
            goal_info_maybe_get_post_births(GoalInfo, PostBirths),
            PostBirthList = set_of_var.to_sorted_list(PostBirths),
            PostBirthList = [_ | _]
        then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% post-births: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, PostBirthList,
                Stream, !IO),
            io.write_string(Stream, "\n", !IO)
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
            yes(ReuseDescription) = goal_info_get_maybe_reuse(GoalInfo),
            set_of_var.to_sorted_list(LFU, ListLFU),
            set_of_var.to_sorted_list(LBU, ListLBU)
        then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% LFU: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, ListLFU,
                Stream, !IO),
            io.write_string(Stream, "\n", !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% LBU: ", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, ListLBU,
                Stream, !IO),
            io.write_string(Stream, "\n", !IO),

            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% Reuse: ", !IO),
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
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% Goal features:  ", !IO),
            io.write(Stream, FeatureList, !IO),
            io.write_string(Stream, "\n", !IO)
        )
    else
        true
    ).

:- pred write_new_var_inst_list(io.text_output_stream::in, var_name_source::in,
    var_name_print::in, int::in, assoc_list(prog_var, mer_inst)::in,
    io::di, io::uo) is det.

write_new_var_inst_list(_, _, _VarNamePrint, _Indent, [], !IO).
write_new_var_inst_list(Stream, VarNameSrc, VarNamePrint, Indent,
        [Var - Inst | VarInsts], !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "%   ", !IO),
    mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO),
    io.write_string(Stream, " -> ", !IO),
    varset.init(InstVarSet),
    mercury_output_inst(Stream, output_debug, InstVarSet, Inst, !IO),
    io.nl(Stream, !IO),
    write_new_var_inst_list(Stream, VarNameSrc, VarNamePrint, Indent,
        VarInsts, !IO).

write_goal_list(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Separator, Goals, !IO) :-
    (
        Goals = [HeadGoal | TailGoals],
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, Separator, !IO),
        do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent + 1, "\n", HeadGoal, !IO),
        write_goal_list(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Separator, TailGoals, !IO)
    ;
        Goals = []
    ).

:- pred write_llds_code_gen_info(hlds_out_info::in, io.text_output_stream::in,
    hlds_goal_info::in, var_name_source::in, var_name_print::in, int::in,
    io::di, io::uo) is det.

write_llds_code_gen_info(Info, Stream, GoalInfo, VarNameSrc, VarNamePrint,
        Indent, !IO) :-
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'f') then
        goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
        (
            MaybeFollowVars = yes(FollowVars),
            FollowVars = abs_follow_vars(FollowVarsMap, NextRegR, NextRegF),
            map.to_assoc_list(FollowVarsMap, FVlist),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% follow vars: r", !IO),
            io.write_int(Stream, NextRegR, !IO),
            io.write_string(Stream, ", f", !IO),
            io.write_int(Stream, NextRegF, !IO),
            io.write_string(Stream, "\n", !IO),
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
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% resume point ", !IO),
            (
                Locs = resume_locs_orig_only,
                io.write_string(Stream, "orig only ", !IO)
            ;
                Locs = resume_locs_stack_only,
                io.write_string(Stream, "stack only ", !IO)
            ;
                Locs = resume_locs_orig_then_stack,
                io.write_string(Stream, "orig then stack ", !IO)
            ;
                Locs = resume_locs_stack_then_orig,
                io.write_string(Stream, "stack then orig ", !IO)
            ),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, ResumeVarList,
                Stream, !IO),
            io.write_string(Stream, "\n", !IO)
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
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% store map:\n", !IO),
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
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% need across call forward vars: ", !IO),
        (
            CallForwardList = [],
            io.write_string(Stream, "none\n", !IO)
        ;
            CallForwardList = [_ | _],
            write_vars(Stream, VarNameSrc, VarNamePrint, CallForwardList, !IO),
            io.write_string(Stream, "\n", !IO)
        ),

        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% need across call resume vars: ", !IO),
        (
            CallResumeList = [],
            io.write_string(Stream, "none\n", !IO)
        ;
            CallResumeList = [_ | _],
            write_vars(Stream, VarNameSrc, VarNamePrint, CallResumeList, !IO),
            io.write_string(Stream, "\n", !IO)
        ),

        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% need across call nondet vars: ", !IO),
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

        write_indent(Stream, Indent, !IO),
        (
            ResumeOnStack = yes,
            io.write_string(Stream, "% resume point has stack label\n", !IO)
        ;
            ResumeOnStack = no,
            io.write_string(Stream, "% resume point has no stack label\n", !IO)
        ),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% need in resume resume vars: ", !IO),
        (
            ResumeResumeList = [],
            io.write_string(Stream, "none\n", !IO)
        ;
            ResumeResumeList = [_ | _],
            write_vars(Stream, VarNameSrc, VarNamePrint,
                ResumeResumeList, !IO),
            io.write_string(Stream, "\n", !IO)
        ),

        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% need in resume nondet vars: ", !IO),
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
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% need in par_conj vars: ", !IO),
        write_vars(Stream, VarNameSrc, VarNamePrint, ParConjList, !IO),
        io.write_string(Stream, "\n", !IO)
    else
        true
    ).

write_var_to_abs_locns(_, _, _, _, [], !IO).
write_var_to_abs_locns(Stream, VarNameSrc, VarNamePrint, Indent,
        [Var - Loc | VarLocs], !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "%\t", !IO),
    mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO),
    io.write_string(Stream, "\t-> ", !IO),
    abs_locn_to_string(Loc, LocnStr, MaybeWidth),
    io.write_string(Stream, LocnStr, !IO),
    (
        MaybeWidth = yes(Width),
        io.write_string(Stream, " ", !IO),
        io.write_string(Stream, Width, !IO)
    ;
        MaybeWidth = no
    ),
    io.write_string(Stream, "\n", !IO),
    write_var_to_abs_locns(Stream, VarNameSrc, VarNamePrint, Indent,
        VarLocs, !IO).

:- pred write_instmap_delta_vars(io.text_output_stream::in,
    var_name_source::in, var_name_print::in, instmap_delta::in,
    io::di, io::uo) is det.

write_instmap_delta_vars(Stream, VarNameSrc, VarNamePrint, InstMapDelta,
        !IO) :-
    ( if instmap_delta_is_unreachable(InstMapDelta) then
        io.write_string(Stream, "unreachable", !IO)
    else
        instmap_delta_to_assoc_list(InstMapDelta, AssocList),
        assoc_list.keys(AssocList, Vars),
        write_vars(Stream, VarNameSrc, VarNamePrint, Vars, !IO)
    ).

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
        io.format(Stream, "reuse call - %s, no clobbers = ",
            [s(is_conditional_to_string(IsConditional))], !IO),
        io.write_string(Stream, ", no clobbers = ", !IO),
        io.write(Stream, NoClobbers, !IO)
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

:- pred write_goal_expr(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in, hlds_goal_expr::in,
    io::di, io::uo) is det.

write_goal_expr(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    (
        GoalExpr = unify(_, _, _, _, _),
        write_goal_unify(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = plain_call(_, _, _, _, _, _),
        write_goal_plain_call(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        write_goal_generic_call(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        write_goal_foreign_proc(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = conj(_, _),
        write_goal_conj(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = disj(_),
        write_goal_disj(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = switch(_, _, _),
        write_goal_switch(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = scope(_, _),
        write_goal_scope(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = if_then_else(_, _, _, _),
        write_goal_if_then_else(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = negation(_),
        write_goal_negation(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = shorthand(_),
        write_goal_shorthand(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out unifications.
%

    % write_goal_unify(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
    %   VarNamePrint, Indent, Follow, GoalExpr, !IO):
    %
    % Write out a unification.
    %
:- pred write_goal_unify(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_unify), io::di, io::uo) is det.

write_goal_unify(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = unify(LHS, RHS, _, Unification, _),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    write_indent(Stream, Indent, !IO),
    mercury_output_var_src(VarNameSrc, VarNamePrint, LHS, Stream, !IO),
    io.write_string(Stream, " = ", !IO),
    (
        TypeQual = tvarset_var_table(_, VarTable),
        lookup_var_type(VarTable, LHS, UniType),
        VarType = yes(UniType)
    ;
        TypeQual = no_tvarset_var_table,
        VarType = no
    ),
    % XXX Fake the inst varset.
    varset.init(InstVarSet),
    write_unify_rhs_2(Info, Stream, ModuleInfo, VarNameSrc, InstVarSet,
        TypeQual, VarNamePrint, Indent, VarType, RHS, !IO),
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
            write_unification(Info, Stream, ModuleInfo, VarNameSrc, InstVarSet,
                VarNamePrint, Indent, Unification, !IO)
        )
    else
        true
    ).

write_unify_rhs(Info, Stream, ModuleInfo, VarNameSrc, InstVarSet, VarNamePrint,
        Indent, RHS, !IO) :-
    write_unify_rhs_2(Info, Stream, ModuleInfo, VarNameSrc, InstVarSet,
        no_tvarset_var_table, VarNamePrint, Indent, no, RHS, !IO).

:- pred write_unify_rhs_2(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, inst_varset::in, type_qual::in,
    var_name_print::in, int::in, maybe(mer_type)::in, unify_rhs::in,
    io::di, io::uo) is det.

write_unify_rhs_2(Info, Stream, ModuleInfo, VarNameSrc, InstVarSet, TypeQual,
        VarNamePrint, Indent, MaybeType, RHS, !IO) :-
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
        io.write_string(Stream,
            functor_cons_id_to_string(ModuleInfo, VarNameSrc, VarNamePrint,
                ConsId, ArgVars),
            !IO),
        ( if
            MaybeType = yes(Type),
            TypeQual = tvarset_var_table(TVarSet, _)
        then
            io.write_string(Stream, " : ", !IO),
            mercury_output_type(TVarSet, VarNamePrint, Type, Stream, !IO)
        else
            true
        )
    ;
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, _EvalMethod,
            NonLocals, VarsModes, Det, Goal),
        Indent1 = Indent + 1,
        io.write_string(Stream, purity_prefix_to_string(Purity), !IO),
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
                ModesStr = var_modes_to_string(Lang, VarNameSrc, InstVarSet,
                    VarNamePrint, VarsModes),
                io.format(Stream, "%s(%s)",
                    [s(Functor), s(ModesStr)], !IO)
            ),
            DetStr = mercury_det_to_string(Det),
            io.format(Stream, " is %s :-\n", [s(DetStr)], !IO),
            do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
                VarNamePrint, Indent1, "\n", Goal, !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, ")", !IO)
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
            do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
                VarNamePrint, Indent1, "\n", Goal, !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, ")", !IO)
        ),
        ( if
            MaybeType = yes(Type),
            TypeQual = tvarset_var_table(TVarSet, _)
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
                io.nl(Stream, !IO),
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% lambda nonlocals: ", !IO),
                mercury_output_vars_src(VarNameSrc, VarNamePrint, NonLocals,
                    Stream, !IO)
            ;
                NonLocals = []
            )
        else
            true
        )
    ).

unify_rhs_to_string(ModuleInfo, VarNameSrc, VarNamePrint, RHS) = Str :-
    (
        RHS = rhs_var(Var),
        Str = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var)
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
        Str = functor_cons_id_to_string(ModuleInfo, VarNameSrc, VarNamePrint,
            ConsId, ArgVars)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _),
        Str = "lambda goal"
    ).

:- pred write_unification(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, inst_varset::in, var_name_print::in,
    int::in, unification::in, io::di, io::uo) is det.

write_unification(Info, Stream, ModuleInfo, VarNameSrc, InstVarSet,
        VarNamePrint, Indent, Unification, !IO) :-
    (
        Unification = assign(X, Y),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% ", !IO),
        mercury_output_var_src(VarNameSrc, VarNamePrint, X, Stream, !IO),
        io.write_string(Stream, " := ", !IO),
        mercury_output_var_src(VarNameSrc, VarNamePrint, Y, Stream, !IO),
        io.write_string(Stream, "\n", !IO)
    ;
        Unification = simple_test(X, Y),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% ", !IO),
        mercury_output_var_src(VarNameSrc, VarNamePrint, X, Stream, !IO),
        io.write_string(Stream, " == ", !IO),
        mercury_output_var_src(VarNameSrc, VarNamePrint, Y, Stream, !IO),
        io.write_string(Stream, "\n", !IO)
    ;
        Unification = construct(Var, ConsId, ArgVars, ArgModes, ConstructHow,
            Uniqueness, SubInfo),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% ", !IO),
        mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO),
        io.write_string(Stream, " <= ", !IO),
        write_functor_and_submodes(Info, Stream, ModuleInfo,
            VarNameSrc, InstVarSet, VarNamePrint, Indent, ConsId,
            ArgVars, ArgModes, !IO),

        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'u') then
            ( if ConsId = cons(_, _, TypeCtor) then
                TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
                write_indent(Stream, Indent, !IO),
                TypeCtorSymNameStr = sym_name_to_string(TypeCtorSymName),
                io.format(Stream, "%% cons_id type_ctor: %s/%d\n",
                    [s(TypeCtorSymNameStr), i(TypeCtorArity)], !IO)
            else
                true
            ),
            (
                Uniqueness = cell_is_unique,
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% cell_is_unique\n", !IO)
            ;
                Uniqueness = cell_is_shared
            ),
            (
                SubInfo = no_construct_sub_info
            ;
                SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize),
                (
                    MaybeTakeAddr = yes(TakeAddressFields),
                    write_indent(Stream, Indent, !IO),
                    io.write_string(Stream, "% take address fields: ", !IO),
                    write_intlist(Stream, TakeAddressFields, !IO),
                    io.write_string(Stream, "\n", !IO)
                ;
                    MaybeTakeAddr = no
                ),
                (
                    MaybeSize = yes(SizeSource),
                    write_indent(Stream, Indent, !IO),
                    io.write_string(Stream, "% term size ", !IO),
                    (
                        SizeSource = known_size(KnownSize),
                        io.write_string(Stream, "const ", !IO),
                        io.write_int(Stream, KnownSize, !IO),
                        io.write_string(Stream, "\n", !IO)
                    ;
                        SizeSource = dynamic_size(SizeVar),
                        io.write_string(Stream, "var ", !IO),
                        mercury_output_var_src(VarNameSrc, VarNamePrint,
                            SizeVar, Stream, !IO),
                        io.write_string(Stream, "\n", !IO)
                    )
                ;
                    MaybeSize = no
                )
            ),
            (
                ConstructHow = construct_dynamically
            ;
                ConstructHow = construct_statically(born_static),
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream,
                    "% construct statically (born static)\n", !IO)
            ;
                ConstructHow = construct_statically(marked_static),
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream,
                    "% construct statically (marked static)\n", !IO)
            ;
                ConstructHow = reuse_cell(CellToReuse),
                CellToReuse = cell_to_reuse(ReuseVar, _ReuseConsIds,
                    _FieldAssigns),
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% reuse cell: ", !IO),
                mercury_output_var_src(VarNameSrc, VarNamePrint, ReuseVar,
                    Stream, !IO),
                io.write_string(Stream, "\n", !IO)
            ;
                ConstructHow = construct_in_region(RegVar),
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% construct in region: ", !IO),
                mercury_output_var_src(VarNameSrc, VarNamePrint, RegVar,
                    Stream, !IO),
                io.write_string(Stream, "\n", !IO)
            )
        else
            true
        )
    ;
        Unification = deconstruct(Var, ConsId, ArgVars, ArgModes, CanFail,
            CanCGC),
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'G') then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% Compile time garbage collect: ", !IO),
            io.write(Stream, CanCGC, !IO),
            io.nl(Stream, !IO)
        else
            true
        ),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% ", !IO),
        mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO),
        (
            CanFail = can_fail,
            io.write_string(Stream, " ?= ", !IO)
        ;
            CanFail = cannot_fail,
            io.write_string(Stream, " => ", !IO)
        ),
        write_functor_and_submodes(Info, Stream, ModuleInfo,
            VarNameSrc, InstVarSet, VarNamePrint, Indent, ConsId,
            ArgVars, ArgModes, !IO)
    ;
        Unification = complicated_unify(Mode, CanFail, TypeInfoVars),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% ", !IO),
        (
            CanFail = can_fail,
            io.write_string(Stream, "can_fail, ", !IO)
        ;
            CanFail = cannot_fail,
            io.write_string(Stream, "cannot_fail, ", !IO)
        ),
        io.write_string(Stream, "mode: ", !IO),
        mercury_output_unify_mode(Stream, Mode, InstVarSet, !IO),
        io.write_string(Stream, "\n", !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% type-info vars: ", !IO),
        mercury_output_vars_src(VarNameSrc, VarNamePrint, TypeInfoVars,
            Stream, !IO),
        io.write_string(Stream, "\n", !IO)
    ).

:- pred write_functor_and_submodes(hlds_out_info::in,
    io.text_output_stream::in, module_info::in,
    var_name_source::in, inst_varset::in, var_name_print::in, int::in,
    cons_id::in, list(prog_var)::in, list(unify_mode)::in,
    io::di, io::uo) is det.

write_functor_and_submodes(Info, Stream, _ModuleInfo, VarNameSrc, InstVarSet,
        VarNamePrint, Indent, ConsId, ArgVars, ArgModes, !IO) :-
    io.write_string(Stream, cons_id_and_arity_to_string(ConsId), !IO),
    (
        ArgVars = [],
        io.write_string(Stream, "\n", !IO)
    ;
        ArgVars = [_ | _],
        io.write_string(Stream, " (", !IO),
        mercury_output_vars_src(VarNameSrc, VarNamePrint, ArgVars,
            Stream, !IO),
        io.write_string(Stream, ")\n", !IO),
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'a') then
            ( if string.contains_char(DumpOptions, 'y') then
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% arg-modes\n", !IO),
                mercury_output_structured_unify_mode_list(Stream,
                    ArgModes, Indent, output_debug, do_incl_addr, InstVarSet,
                    !IO)
            else
                write_arg_modes(Stream, InstVarSet, Indent, 1, ArgModes, !IO)
            )
        else
            true
        )
    ).

:- pred write_arg_modes(io.text_output_stream::in, inst_varset::in,
    int::in, int::in,
    list(unify_mode)::in, io::di, io::uo) is det.

write_arg_modes(_Stream, _InstVarSet, _Indent, _ArgNum, [], !IO).
write_arg_modes(Stream, InstVarSet, Indent, ArgNum,
        [UnifyMode | UnifyModes], !IO) :-
    write_indent(Stream, Indent, !IO),
    io.format(Stream, "%% arg-mode %d ", [i(ArgNum)], !IO),
    mercury_output_unify_mode(Stream, UnifyMode, InstVarSet, !IO),
    io.nl(Stream, !IO),
    write_arg_modes(Stream, InstVarSet, Indent, ArgNum + 1, UnifyModes, !IO).

%---------------------------------------------------------------------------%
%
% Write out ordinary first-order calls.
%

:- pred write_goal_plain_call(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_plain_call), io::di, io::uo) is det.

write_goal_plain_call(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
        VarNamePrint, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = plain_call(PredId, ProcId, ArgVars, Builtin,
        MaybeUnifyContext, PredName),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'b') then
        (
            Builtin = inline_builtin,
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% inline builtin\n", !IO)
        ;
            Builtin = not_builtin
        )
    else
        true
    ),
    write_indent(Stream, Indent, !IO),
    ( if PredId = invalid_pred_id then
        % If we do not know the id of the callee yet, then treat the call
        % as being to a pure predicate. This may be misleading, but any
        % other assumption has a significantly higher chance of being
        % misleading.
        PredOrFunc = pf_predicate
    else if
        module_info_get_pred_id_table(ModuleInfo, PredIdTable),
        map.search(PredIdTable, PredId, PredInfo)
    then
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        pred_info_get_purity(PredInfo, Purity),
        io.write_string(Stream, purity_prefix_to_string(Purity), !IO)
    else
        % We should know the id of the callee, but the callee has been
        % deleted *without* this call to it (and maybe others) being
        % adjusted accordingly. This is a bug, so we want to draw attention
        % to it, but we cannot do so effectively if this code aborts
        % before we finish writing out the HLDS dump.
        io.write_string(Stream, "CALL TO DELETED ", !IO),
        PredOrFunc = pf_predicate
    ),
    (
        PredOrFunc = pf_predicate,
        NewArgVars = ArgVars
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, NewArgVars, LHSVar),
        mercury_output_var_src(VarNameSrc, VarNamePrint, LHSVar, Stream, !IO),
        io.write_string(Stream, " = ", !IO)
    ),
    io.write_string(Stream,
        sym_name_and_args_to_string(VarNameSrc, VarNamePrint, PredName,
            NewArgVars),
        !IO),
    io.write_string(Stream, Follow, !IO),
    ( if string.contains_char(DumpOptions, 'l') then
        pred_id_to_int(PredId, PredNum),
        proc_id_to_int(ProcId, ProcNum),
        write_indent(Stream, Indent, !IO),
        io.format(Stream, "%% pred id: %i, proc id: %d%s",
            [i(PredNum), i(ProcNum), s(Follow)], !IO),
        (
            MaybeUnifyContext = yes(CallUnifyContext),
            (
                TypeQual = tvarset_var_table(_, VarTable),
                lookup_var_type(VarTable, Var, UniType),
                VarType = yes(UniType)
            ;
                TypeQual = no_tvarset_var_table,
                VarType = no
            ),
            CallUnifyContext = call_unify_context(Var, RHS, _UnifyContext),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% unify context: ", !IO),
            mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO),
            io.write_string(Stream, " = ", !IO),
            % XXX Fake the inst varset.
            varset.init(InstVarSet),
            write_unify_rhs_2(Info, Stream, ModuleInfo, VarNameSrc, InstVarSet,
                TypeQual, VarNamePrint, Indent, VarType, RHS, !IO),
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

:- pred write_goal_generic_call(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_generic_call), io::di, io::uo) is det.

write_goal_generic_call(Info, Stream, _ModuleInfo, VarNameSrc, _TypeQual,
        VarNamePrint, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = generic_call(GenericCall, ArgVars, Modes, MaybeArgRegs, _),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    % XXX We should print more info here.
    (
        GenericCall = higher_order(PredVar, Purity, PredOrFunc, _),
        (
            PredOrFunc = pf_predicate,
            ( if string.contains_char(DumpOptions, 'l') then
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream,
                    "% higher-order predicate call\n", !IO),
                write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
            else
                true
            ),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, purity_prefix_to_string(Purity), !IO),
            io.write_string(Stream,
                functor_to_string(VarNameSrc, VarNamePrint,
                    term.atom("call"), [PredVar | ArgVars]),
                !IO)
        ;
            PredOrFunc = pf_function,
            ( if string.contains_char(DumpOptions, 'l') then
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream,
                    "% higher-order function application\n", !IO),
                write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
            else
                true
            ),
            pred_args_to_func_args([PredVar | ArgVars],
                FuncArgVars, FuncRetVar),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, purity_prefix_to_string(Purity), !IO),
            mercury_output_var_src(VarNameSrc, VarNamePrint, FuncRetVar,
                Stream, !IO),
            io.write_string(Stream, " = ", !IO),
            io.write_string(Stream,
                functor_to_string(VarNameSrc, VarNamePrint,
                    term.atom("apply"), FuncArgVars),
                !IO)
        ),
        io.write_string(Stream, Follow, !IO)
    ;
        GenericCall = class_method(TCInfoVar, MethodNum, _ClassId,
            _MethodId),
        ( if string.contains_char(DumpOptions, 'l') then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% class method call\n", !IO),
            write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
        else
            true
        ),
        term.context_init(Context),
        Functor = term.atom("class_method_call"),
        TCInfoTerm = term.variable(TCInfoVar, Context),
        MethodNumTerm = int_to_decimal_term(MethodNum, Context),
        term.var_list_to_term_list(ArgVars, ArgTerms),
        Term = term.functor(Functor, [TCInfoTerm, MethodNumTerm | ArgTerms],
            Context),
        write_indent(Stream, Indent, !IO),
        mercury_output_term_src(VarNameSrc, VarNamePrint, Term, Stream, !IO),
        io.write_string(Stream, Follow, !IO)
    ;
        GenericCall = event_call(EventName),
        ( if string.contains_char(DumpOptions, 'l') then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% event call\n", !IO),
            write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
        else
            true
        ),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "event ", !IO),
        term.context_init(Context),
        Functor = term.atom(EventName),
        term.var_list_to_term_list(ArgVars, ArgTerms),
        Term = term.functor(Functor, ArgTerms, Context),
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
            write_indent(Stream, Indent, !IO),
            io.write_strings(Stream, ["% ", CastTypeString, "\n"], !IO),
            write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO)
        else
            true
        ),
        ( if string.contains_char(DumpOptions, 'D') then
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% modes: ", !IO),
            varset.init(InstVarSet),
            mercury_output_mode_list(Stream, output_debug, InstVarSet,
                Modes, !IO),
            io.nl(Stream, !IO)
        else
            true
        ),
        PredOrFunc = write_cast_as_pred_or_func(CastType),
        (
            PredOrFunc = pf_predicate,
            Functor = term.atom(CastTypeString),
            term.var_list_to_term_list(ArgVars, ArgTerms),
            term.context_init(Context),
            Term = term.functor(Functor, ArgTerms, Context),
            write_indent(Stream, Indent, !IO),
            mercury_output_term_src(VarNameSrc, VarNamePrint, Term,
                Stream, !IO)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(ArgVars, FuncArgVars, FuncRetVar),
            write_indent(Stream, Indent, !IO),
            mercury_output_var_src(VarNameSrc, VarNamePrint, FuncRetVar,
                Stream, !IO),
            io.write_string(Stream, " = ", !IO),
            io.write_string(Stream,
                functor_to_string(VarNameSrc, VarNamePrint,
                    term.atom(CastTypeString), FuncArgVars),
                !IO)
        ),
        io.write_string(Stream, Follow, !IO)
    ).

:- pred write_ho_arg_regs(io.text_output_stream::in, int::in,
    arg_reg_type_info::in, io::di, io::uo) is det.

write_ho_arg_regs(Stream, Indent, MaybeArgRegs, !IO) :-
    (
        MaybeArgRegs = arg_reg_types(ArgRegs),
        ArgRegStrs = list.map(ho_arg_reg_to_string, ArgRegs),
        ArgRegsStr = string.join_list(", ", ArgRegStrs),
        write_indent(Stream, Indent, !IO),
        io.format(Stream, "%% arg regs: %s\n", [s(ArgRegsStr)], !IO)
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

:- pred write_goal_foreign_proc(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_foreign_proc), io::di, io::uo) is det.

write_goal_foreign_proc(_Info, Stream, ModuleInfo, VarNameSrc, _TypeQual,
        VarNamePrint, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = call_foreign_proc(Attributes, PredId, ProcId,
        Args, ExtraArgs, MaybeTraceRuntimeCond, PragmaCode),
    ForeignLang = get_foreign_language(Attributes),
    ForeignLangStr = foreign_language_string(ForeignLang),
    PredStr = pred_id_to_dev_string(ModuleInfo, PredId),
    pred_id_to_int(PredId, PredIdInt),
    proc_id_to_int(ProcId, ProcIdInt),

    write_indent(Stream, Indent, !IO),
    io.format(Stream, "$pragma_foreign_proc(/* %s */, %s pred %d proc %d,\n",
        [s(ForeignLangStr), s(PredStr), i(PredIdInt), i(ProcIdInt)], !IO),
    (
        MaybeTraceRuntimeCond = no
    ;
        MaybeTraceRuntimeCond = yes(TraceRuntimeCond),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% trace_runtime_cond(", !IO),
        mercury_output_trace_expr(Stream, mercury_output_trace_runtime,
            TraceRuntimeCond, !IO),
        io.write_string(Stream, ")\n", !IO)
    ),
    % XXX We do not have the TypeVarSet or InstVarSet available here,
    % but it is only used for printing out the names of the type and inst
    % variables, which is not essential.
    varset.init(TypeVarSet),
    varset.init(InstVarSet),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "[", !IO),
    write_foreign_args(Stream, VarNameSrc, TypeVarSet, InstVarSet,
        VarNamePrint, Indent, Args, !IO),
    io.write_string(Stream, "],\n", !IO),
    (
        ExtraArgs = []
    ;
        ExtraArgs = [_ | _],
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "{", !IO),
        write_foreign_args(Stream, VarNameSrc, TypeVarSet, InstVarSet,
            VarNamePrint, Indent, ExtraArgs, !IO),
        io.write_string(Stream, "},\n", !IO)
    ),
    PragmaCode = fp_impl_ordinary(Code, _),
    io.format(Stream, """%s"")%s", [s(Code), s(Follow)], !IO).

:- pred write_foreign_args(io.text_output_stream::in,
    var_name_source::in, tvarset::in, inst_varset::in, var_name_print::in,
    int::in, list(foreign_arg)::in, io::di, io::uo) is det.

write_foreign_args(_, _, _, _, _, _, [], !IO).
write_foreign_args(Stream, VarNameSrc, TVarSet, InstVarSet,
        VarNamePrint, Indent, [Arg | Args], !IO) :-
    Arg = foreign_arg(Var, MaybeNameMode, Type, BoxPolicy),
    mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO),
    (
        MaybeNameMode = yes(foreign_arg_name_mode(Name, Mode)),
        % For HLDS dumps, we need clarity mode than round-trippability,
        % which is why we specify output_debug.
        ModeStr = mercury_mode_to_string(output_debug, InstVarSet, Mode),
        io.format(Stream, "/%s(%s)", [s(Name), s(ModeStr)], !IO)
    ;
        MaybeNameMode = no
    ),
    (
        BoxPolicy = bp_native_if_possible
    ;
        BoxPolicy = bp_always_boxed,
        io.write_string(Stream, "$alwaysboxed", !IO)
    ),
    io.write_string(Stream, "@", !IO),
    mercury_output_type(TVarSet, VarNamePrint, Type, Stream, !IO),
    (
        Args = []
    ;
        Args = [_ | _],
        io.write_string(Stream, ",\n", !IO),
        write_indent(Stream, Indent, !IO),
        write_foreign_args(Stream, VarNameSrc, TVarSet, InstVarSet,
            VarNamePrint, Indent, Args, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out disjunctions.
%

:- pred write_goal_conj(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_conj), io::di, io::uo) is det.

write_goal_conj(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = conj(ConjType, List),
    (
        List = [Goal | Goals],
        (
            ConjType = plain_conj,
            DumpOptions = Info ^ hoi_dump_hlds_options,
            ( if DumpOptions = "" then
                write_conj(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
                    VarNamePrint, Indent, Follow, ",\n", Goal, Goals, !IO)
            else
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "( % conjunction\n", !IO),
                write_conj(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
                    VarNamePrint, Indent + 1, "\n", ",\n", Goal, Goals, !IO),
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, ")", !IO),
                io.write_string(Stream, Follow, !IO)
            )
        ;
            ConjType = parallel_conj,
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "( % parallel conjunction\n", !IO),
            do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
                VarNamePrint, Indent + 1, "\n", Goal, !IO),
            % See comments at write_goal_list.
            write_goal_list(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
                VarNamePrint, Indent, "&\n", Goals, !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, ")", !IO),
            io.write_string(Stream, Follow, !IO)
        )
    ;
        List = [],
        write_indent(Stream, Indent, !IO),
        (
            ConjType = plain_conj,
            io.write_string(Stream, "true", !IO)
        ;
            ConjType = parallel_conj,
            io.write_string(Stream, "/* parallel */ true", !IO)
        ),
        io.write_string(Stream, Follow, !IO)
    ).

:- pred write_conj(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in, string::in,
    hlds_goal::in, list(hlds_goal)::in, io::di, io::uo) is det.

write_conj(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Follow, Separator, Goal1, Goals1, !IO) :-
    (
        Goals1 = [Goal2 | Goals2],
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if DumpOptions = "" then
            do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
                VarNamePrint, Indent, Separator, Goal1, !IO)
        else
            % When generating verbose dumps, we want the comma on its own line,
            % since that way it visually separates the lines after one goal
            % and the lines before the next.
            do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
                VarNamePrint, Indent, "\n", Goal1, !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, Separator, !IO)
        ),
        write_conj(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, Separator, Goal2, Goals2, !IO)
    ;
        Goals1 = [],
        do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Follow, Goal1, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out disjunctions.
%

:- pred write_goal_disj(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_disj), io::di, io::uo) is det.

write_goal_disj(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = disj(Disjuncts),
    write_indent(Stream, Indent, !IO),
    (
        Disjuncts = [Goal | Goals],
        io.write_string(Stream, "( % disjunction\n", !IO),
        do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent + 1, "\n", Goal, !IO),
        write_goal_list(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, ";\n", Goals, !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, ")", !IO),
        io.write_string(Stream, Follow, !IO)
    ;
        Disjuncts = [],
        io.write_string(Stream, "fail", !IO),
        io.write_string(Stream, Follow, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out switches.
%

:- pred write_goal_switch(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_switch), io::di, io::uo) is det.

write_goal_switch(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = switch(Var, CanFail, CasesList),
    CanFailStr = can_fail_to_string(CanFail),
    VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
    write_indent(Stream, Indent, !IO),
    io.format(Stream, "( %% %s switch on `%s'\n",
        [s(CanFailStr), s(VarStr)], !IO),
    Indent1 = Indent + 1,
    (
        CasesList = [Case | Cases],
        write_case(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent1, Var, Case, !IO),
        write_cases(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Var, Cases, !IO)
    ;
        CasesList = [],
        write_indent(Stream, Indent1, !IO),
        io.write_string(Stream, "fail\n", !IO)
    ),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, ")", !IO),
    io.write_string(Stream, Follow, !IO).

:- pred write_cases(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, prog_var::in, list(case)::in,
    io::di, io::uo) is det.

write_cases(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
        VarNamePrint, Indent, Var, CasesList, !IO) :-
    (
        CasesList = [Case | Cases],
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, ";\n", !IO),
        write_case(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent + 1, Var, Case, !IO),
        write_cases(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, Var, Cases, !IO)
    ;
        CasesList = []
    ).

:- pred write_case(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, prog_var::in, case::in,
    io::di, io::uo) is det.

write_case(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, Var, Case, !IO) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% ", !IO),
    mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO),
    io.write_string(Stream, " has functor ", !IO),
    io.write_string(Stream, cons_id_and_arity_to_string(MainConsId), !IO),
    list.foldl(write_alternative_cons_id(Stream), OtherConsIds, !IO),
    io.write_string(Stream, "\n", !IO),
    % XXX if the output of this is to be used, e.g. in
    % inter-module optimization, output a unification to bind the
    % Var to the functor, since simplify.m and unused_args.m remove
    % the unification. At the moment this is not a problem, since
    % intermod.m works on the unoptimized clauses.
    do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent, "\n", Goal, !IO).

:- pred write_alternative_cons_id(io.text_output_stream::in, cons_id::in,
    io::di, io::uo) is det.

write_alternative_cons_id(Stream, ConsId, !IO) :-
    io.write_string(Stream, " or ", !IO),
    io.write_string(Stream, cons_id_and_arity_to_string(ConsId), !IO).

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

:- pred write_goal_negation(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in, hlds_goal_expr::in(goal_expr_neg),
    io::di, io::uo) is det.

write_goal_negation(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
        VarNamePrint, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = negation(Goal),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "not (\n", !IO),
    do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent + 1, "\n", Goal, !IO),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, ")", !IO),
    io.write_string(Stream, Follow, !IO).

%---------------------------------------------------------------------------%
%
% Write out if-then-elses.
%

:- pred write_goal_if_then_else(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in, hlds_goal_expr::in(goal_expr_ite),
    io::di, io::uo) is det.

write_goal_if_then_else(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
        VarNamePrint, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = if_then_else(Vars, Cond, Then, Else),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "( if", !IO),
    write_some(Stream, VarNameSrc, Vars, !IO),
    io.write_string(Stream, "\n", !IO),
    Indent1 = Indent + 1,
    do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent1, "\n", Cond, !IO),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "then\n", !IO),
    do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        Indent1, "\n", Then, !IO),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "else\n", !IO),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if
        DumpOptions \= "",
        Else = hlds_goal(if_then_else(_, _, _, _), _)
    then
        ElseIndent = Indent
    else
        ElseIndent = Indent1
    ),
    do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual, VarNamePrint,
        ElseIndent, "\n", Else, !IO),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, ")", !IO),
    io.write_string(Stream, Follow, !IO).

:- pred write_some(io.text_output_stream::in, var_name_source::in,
    list(prog_var)::in, io::di, io::uo) is det.

write_some(Stream, VarNameSrc, Vars, !IO) :-
    (
        Vars = []
    ;
        Vars = [_ | _],
        VarsStr = mercury_vars_to_string_src(VarNameSrc,
            print_name_and_num, Vars),
        io.format(Stream, " some [%s]", [s(VarsStr)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out scope goals.
%

:- pred write_goal_scope(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_scope), io::di, io::uo) is det.

write_goal_scope(!.Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
        VarNamePrint, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = scope(Reason, Goal),
    write_indent(Stream, Indent, !IO),
    (
        Reason = exist_quant(Vars),
        VarsStr = mercury_vars_to_string_src(VarNameSrc, VarNamePrint, Vars),
        io.format(Stream, "some [%s] (\n", [s(VarsStr)], !IO)
    ;
        Reason = disable_warnings(HeadWarning, TailWarnings),
        io.write_string(Stream, "disable_warnings [", !IO),
        write_goal_warnings(Stream, HeadWarning, TailWarnings, !IO),
        io.write_string(Stream, "] (\n", !IO)
    ;
        Reason = promise_purity(Purity),
        (
            Purity = purity_pure,
            PromiseStr = "promise_pure"
        ;
            Purity = purity_semipure,
            PromiseStr = "promise_semipure"
        ;
            Purity = purity_impure,
            PromiseStr = "promise_impure"
        ),
        io.format(Stream, "%s (\n", [s(PromiseStr)], !IO)
    ;
        Reason = promise_solutions(Vars, Kind),
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
        (
            Detism = detism_det,
            ReqStr = "require_det"
        ;
            Detism = detism_semi,
            ReqStr = "require_semidet"
        ;
            Detism = detism_non,
            ReqStr = "require_nondet"
        ;
            Detism = detism_multi,
            ReqStr = "require_multi"
        ;
            Detism = detism_cc_multi,
            ReqStr = "require_cc_multi"
        ;
            Detism = detism_cc_non,
            ReqStr = "require_cc_nondet"
        ;
            Detism = detism_failure,
            ReqStr = "require_failure"
        ;
            Detism = detism_erroneous,
            ReqStr = "require_erroneous"
        ),
        io.format(Stream, "%s (\n", [s(ReqStr)], !IO)
    ;
        Reason = require_complete_switch(Var),
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        io.format(Stream, "require_complete_switch [%s] (\n",
            [s(VarStr)], !IO)
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
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        io.format(Stream, "%s [%s] (\n", [s(ReqStr), s(VarStr)], !IO)
    ;
        Reason = barrier(removable),
        io.write_string(Stream, "(\n", !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% barrier(removable)\n", !IO)
    ;
        Reason = barrier(not_removable),
        io.write_string(Stream, "(\n", !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% barrier(not_removable)\n", !IO)
    ;
        Reason = commit(force_pruning),
        io.write_string(Stream, "(\n", !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% commit(force_pruning)\n", !IO)
    ;
        Reason = commit(dont_force_pruning),
        io.write_string(Stream, "(\n", !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% commit(dont_force_pruning)\n", !IO)
    ;
        Reason = from_ground_term(Var, Kind),
        VarStr = mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var),
        (
            Kind = from_ground_term_initial,
            KindStr = "initial"
        ;
            Kind = from_ground_term_construct,
            KindStr = "construct"
        ;
            Kind = from_ground_term_deconstruct,
            KindStr = "deconstruct"
        ;
            Kind = from_ground_term_other,
            KindStr = "other"
        ),
        io.write_string(Stream, "(\n", !IO),
        write_indent(Stream, Indent, !IO),
        io.format(Stream, "%% from_ground_term [%s, %s]\n",
            [s(VarStr), s(KindStr)], !IO),
        % The goals inside from_ground_term scopes are created with
        % all of the fields of goal infos already filled in.
        % This means printing them is meaningful, and sometimes
        % it is needed to diagnose problems.
        DumpOptionsBackup = !.Info ^ hoi_dump_hlds_options_backup,
        !Info ^ hoi_dump_hlds_options := DumpOptionsBackup
    ;
        Reason = trace_goal(MaybeCompileTime, MaybeRunTime, MaybeIO0,
            MutableVars0, QuantVars),
        io.write_string(Stream, "trace [\n", !IO),
        some [!AddCommaNewline] (
            !:AddCommaNewline = no,
            (
                MaybeCompileTime = yes(CompileTime),
                write_indent(Stream, Indent + 1, !IO),
                io.write_string(Stream, "compile_time(", !IO),
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
                write_indent(Stream, Indent + 1, !IO),
                io.write_string(Stream, "runtime(", !IO),
                mercury_output_trace_expr(Stream, mercury_output_trace_runtime,
                    RunTime, !IO),
                io.write_string(Stream, ")", !IO),
                !:AddCommaNewline = yes
            ;
                MaybeRunTime = no
            ),
            Lang = get_output_lang(!.Info ^ hoi_merc_out_info),
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
                % relatively easily with the help of a goal features
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
                write_indent(Stream, Indent + 1, !IO),
                io.format(Stream, "io(!%s)", [s(IOStateVarName)], !IO),
                !:AddCommaNewline = yes
            ;
                MaybeIO = no
            ),

            list.foldl2(write_trace_mutable_var_hlds(Stream, Indent + 1),
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
                QuantVarsStr = mercury_vars_to_string_src(VarNameSrc,
                    VarNamePrint, QuantVars),
                write_indent(Stream, Indent + 1, !IO),
                io.format(Stream, "%% quantified vars [%s]\n",
                    [s(QuantVarsStr)], !IO)
            ),

            write_indent(Stream, Indent, !IO),
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
        io.format(Stream, "loop_control_spawn_off_%s(%s) (\n",
            [s(UseParentStackStr),
            s(mercury_vars_to_string_src(VarNameSrc, VarNamePrint,
                [LCVar, LCSVar]))],
            !IO)
    ),
    do_write_goal(!.Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
        VarNamePrint, Indent + 1, "\n", Goal, !IO),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, ")", !IO),
    io.write_string(Stream, Follow, !IO).

:- pred write_trace_mutable_var_hlds(io.text_output_stream::in, int::in,
    trace_mutable_var_hlds::in, bool::in, bool::out, io::di, io::uo) is det.

write_trace_mutable_var_hlds(Stream, Indent, MutableVar,
        !AddCommaNewline, !IO) :-
    MutableVar = trace_mutable_var_hlds(MutableName, StateVarName),
    maybe_add_comma_newline(Stream, !.AddCommaNewline, !IO),
    write_indent(Stream, Indent, !IO),
    io.format(Stream, "state(%s, !%s)",
        [s(MutableName), s(StateVarName)], !IO),
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

:- pred write_goal_shorthand(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, var_name_source::in, type_qual::in,
    var_name_print::in, int::in, string::in,
    hlds_goal_expr::in(goal_expr_shorthand), io::di, io::uo) is det.

write_goal_shorthand(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
        VarNamePrint, Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = shorthand(ShortHand),
    (
        ShortHand = atomic_goal(_GoalType, Outer, Inner, MaybeOutputVars,
            MainGoal, OrElseGoals, _OrElseInners),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "atomic [", !IO),
        write_atomic_interface_vars(Stream, VarNameSrc, VarNamePrint,
            "outer", Outer, !IO),
        io.write_string(Stream, " ", !IO),
        write_atomic_interface_vars(Stream, VarNameSrc, VarNamePrint,
            "inner", Inner, !IO),
        io.write_string(Stream, " ", !IO),
        (
            MaybeOutputVars = no
        ;
            MaybeOutputVars = yes(OutputVars),
            io.write_string(Stream, "vars([", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, OutputVars,
                Stream, !IO),
            io.write_string(Stream, "])", !IO)
        ),
        io.write_string(Stream, "] (\n",!IO),

        do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent + 1, "\n", MainGoal, !IO),
        write_goal_list(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent, "or_else\n", OrElseGoals, !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, ")", !IO),
        io.write_string(Stream, Follow, !IO)
    ;
        ShortHand = try_goal(MaybeIO, _, SubGoal),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "( % try\n", !IO),
        (
            MaybeIO = yes(try_io_state_vars(IOVarA, IOVarB)),
            write_indent(Stream, Indent + 1, !IO),
            io.write_string(Stream, "% io(", !IO),
            mercury_output_vars_src(VarNameSrc, VarNamePrint, [IOVarA, IOVarB],
                Stream, !IO),
            io.write_string(Stream, ")\n", !IO)
        ;
            MaybeIO = no
        ),
        do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent + 1, "\n", SubGoal, !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, ")", !IO),
        io.write_string(Stream, Follow, !IO)
    ;
        ShortHand = bi_implication(GoalA, GoalB),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "( % bi-implication\n", !IO),
        Indent1 = Indent + 1,
        do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent1, "\n", GoalA, !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "<=>\n", !IO),
        do_write_goal(Info, Stream, ModuleInfo, VarNameSrc, TypeQual,
            VarNamePrint, Indent1, "\n", GoalB, !IO),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, ")", !IO),
        io.write_string(Stream, Follow, !IO)
    ).

:- pred write_atomic_interface_vars(io.text_output_stream::in,
    var_name_source::in, var_name_print::in, string::in,
    atomic_interface_vars::in, io::di, io::uo) is det.

write_atomic_interface_vars(Stream, VarNameSrc, VarNamePrint,
        CompName, CompState, !IO) :-
    io.write_string(Stream, CompName, !IO),
    io.write_string(Stream, "(", !IO),
    CompState = atomic_interface_vars(Var1, Var2),
    mercury_output_var_src(VarNameSrc, VarNamePrint, Var1, Stream, !IO),
    io.write_string(Stream, ", ", !IO),
    mercury_output_var_src(VarNameSrc, VarNamePrint, Var2, Stream, !IO),
    io.write_string(Stream, ")", !IO).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_goal.
%---------------------------------------------------------------------------%
