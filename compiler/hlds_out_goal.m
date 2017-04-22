%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
%-----------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_goal.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Print a goal in a way that is suitable for debugging the compiler
    % (but necessarily for anything else).
    %
:- pred dump_goal(module_info::in, prog_varset::in, hlds_goal::in,
    io::di, io::uo) is det.

    % Print out an HLDS goal. The module_info and prog_varset give
    % the context of the goal. The integer gives the level of indentation
    % to be used within the goal. The string says what should end the line
    % containing the goal; it should include a newline character, but may
    % also contain other characters before that.
    %
:- pred write_goal(hlds_out_info::in, module_info::in, prog_varset::in,
    var_name_print::in, int::in, string::in, hlds_goal::in,
    io::di, io::uo) is det.

    % TypeQual is yes(TVarset, VarTypes) if all constructors should be
    % module qualified.
    %
:- pred do_write_goal(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in,
    int::in, string::in, hlds_goal::in, io::di, io::uo) is det.

    % write_goal_list is used to write both disjunctions
    % and parallel conjunctions. The module_info, prog_varset and
    % maybe_vartypes give the context of the goal. The boolean
    % says whether variables should have their numbers appended to
    % them. The integer gives the level of indentation to be used
    % within the goal. The string says what should be on the line
    % between each goal; it should include a newline character,
    % but may also contain other characters before that.
    %
:- pred write_goal_list(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in,
    int::in, string::in, list(hlds_goal)::in,io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Write out the mapping of variables to their abstract locations.
    %
:- pred write_var_to_abs_locns(prog_varset::in, var_name_print::in, int::in,
    assoc_list(prog_var, abs_locn)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Print out the right-hand-side of a unification. The module_info and
    % the varsets give the context of the rhs. The boolean says whether
    % variables should have their numbers appended to them. The integer
    % gives the level of indentation to be used within the rhs.
    %
:- pred write_unify_rhs(hlds_out_info::in, module_info::in,
    prog_varset::in, inst_varset::in, var_name_print::in, int::in,
    unify_rhs::in, io::di, io::uo) is det.

    % Converts the right-hand-side of a unification to a string, similarly to
    % write_unify_rhs, but doesn't print any details for lambda goals.
    % The module_info and the varset give the context of the rhs. The boolean
    % says whether variables should have their numbers appended to them.
    %
:- func unify_rhs_to_string(module_info, prog_varset, var_name_print,
    unify_rhs) = string.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
:- import_module parse_tree.prog_mode.
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

%-----------------------------------------------------------------------------%

dump_goal(ModuleInfo, VarSet, Goal, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    Info = init_hlds_out_info(Globals, output_debug),
    VarNamePrint = print_name_and_num,
    Indent = 0,
    Follow = "",
    TypeQual = no_varset_vartypes,
    do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, Goal, !IO).

write_goal(Info, ModuleInfo, VarSet, VarNamePrint, Indent, Follow, Goal,
        !IO) :-
    % Don't type qualify everything.
    do_write_goal(Info, ModuleInfo, VarSet, no_varset_vartypes, VarNamePrint,
        Indent, Follow, Goal, !IO).

do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, Goal, !IO) :-
    % Write out goal_infos in the form of annotations around goal expressions.

    Goal = hlds_goal(GoalExpr, GoalInfo),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'c') then
        Context = goal_info_get_context(GoalInfo),
        term.context_file(Context, FileName),
        term.context_line(Context, LineNumber),
        ( if FileName = "" then
            true
        else
            write_indent(Indent, !IO),
            io.write_string("% context: file `", !IO),
            io.write_string(FileName, !IO),
            io.write_string("', line ", !IO),
            io.write_int(LineNumber, !IO),
            io.write_string("\n", !IO)
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'P') then
        GoalId = goal_info_get_goal_id(GoalInfo),
        GoalId = goal_id(GoalIdNum),
        ( if GoalIdNum < 0 then
            true
        else
            write_indent(Indent, !IO),
            io.write_string("% goal id: ", !IO),
            io.write_int(GoalIdNum, !IO),
            io.write_string("\n", !IO)
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'n') then
        NonLocalsSet = goal_info_get_nonlocals(GoalInfo),
        set_of_var.to_sorted_list(NonLocalsSet, NonLocalsList),
        (
            NonLocalsList = [_ | _],
            write_indent(Indent, !IO),
            io.write_string("% nonlocals: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, NonLocalsList, !IO),
            io.write_string("\n", !IO)
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
            write_indent(Indent, !IO),
            io.write_string("% pre-deaths: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, PreDeathList, !IO),
            io.write_string("\n", !IO)
        else
            true
        ),
        ( if
            goal_info_maybe_get_pre_births(GoalInfo, PreBirths),
            PreBirthList = set_of_var.to_sorted_list(PreBirths),
            PreBirthList = [_ | _]
        then
            write_indent(Indent, !IO),
            io.write_string("% pre-births: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, PreBirthList, !IO),
            io.write_string("\n", !IO)
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
            write_indent(Indent, !IO),
            io.write_string("% producing vars: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, ProducingVarsList, !IO),
            io.write_string("\n", !IO)
        else
            true
        ),

        ConsumingVars = GoalInfo ^ consuming_vars,
        ( if set_of_var.is_non_empty(ConsumingVars) then
            set_of_var.to_sorted_list(ConsumingVars, ConsumingVarsList),
            write_indent(Indent, !IO),
            io.write_string("% consuming vars: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, ConsumingVarsList, !IO),
            io.write_string("\n", !IO)
        else
            true
        ),

        MakeVisibleVars = GoalInfo ^ make_visible_vars,
        ( if set_of_var.is_non_empty(MakeVisibleVars) then
            set_of_var.to_sorted_list(MakeVisibleVars, MakeVisibleVarsList),
            write_indent(Indent, !IO),
            io.write_string("% make_visible vars: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, MakeVisibleVarsList,
                !IO),
            io.write_string("\n", !IO)
        else
            true
        ),

        NeedVisibleVars = GoalInfo ^ need_visible_vars,
        ( if set_of_var.is_non_empty(NeedVisibleVars) then
            set_of_var.to_sorted_list(NeedVisibleVars, NeedVisibleVarsList),
            write_indent(Indent, !IO),
            io.write_string("% need_visible vars: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, NeedVisibleVarsList,
                !IO),
            io.write_string("\n", !IO)
        else
            true
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'd') then
        write_indent(Indent, !IO),
        io.write_string("% determinism: ", !IO),
        Determinism = goal_info_get_determinism(GoalInfo),
        io.write_string(determinism_to_string(Determinism), !IO),
        io.write_string("\n", !IO)
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'e') then
        MaybeRbmmInfo = goal_info_get_maybe_rbmm(GoalInfo),
        (
            MaybeRbmmInfo = yes(RbmmInfo),
            RbmmInfo = rbmm_goal_info(Created, Removed, Carried, Alloc, Used),
            write_indent(Indent, !IO),
            io.write_string("% Created regions: ", !IO),
            io.write_list(set.to_sorted_list(Created), ", ", io.write, !IO),
            io.nl(!IO),
            write_indent(Indent, !IO),
            io.write_string("% Removed regions: ", !IO),
            io.write_list(set.to_sorted_list(Removed), ", ", io.write, !IO),
            io.nl(!IO),
            write_indent(Indent, !IO),
            io.write_string("% Carried regions: ", !IO),
            io.write_list(set.to_sorted_list(Carried), ", ", io.write, !IO),
            io.nl(!IO),
            write_indent(Indent, !IO),
            io.write_string("% Allocated into regions: ", !IO),
            io.write_list(set.to_sorted_list(Alloc), ", ", io.write, !IO),
            io.nl(!IO),
            write_indent(Indent, !IO),
            io.write_string("% Used regions: ", !IO),
            io.write_list(set.to_sorted_list(Used), ", ", io.write, !IO),
            io.nl(!IO)
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
            write_indent(Indent, !IO),
            io.write_string("% semipure\n", !IO)
        ;
            Purity = purity_impure,
            write_indent(Indent, !IO),
            io.write_string("% impure\n", !IO)
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
                write_indent(Indent, !IO),
                io.write_string("% mdprof instrumentation\n", !IO)
            ;
                MdprofInst = goal_is_not_mdprof_inst
            ),
            (
                MaybeDPCoverageInfo = yes(CoverageInfo),
                CoverageInfo = dp_coverage_goal_info(IsTrivial,
                    PortCountsGiveCoverageAfter),
                write_indent(Indent, !IO),
                (
                    IsTrivial = goal_is_trivial,
                    io.write_string("% trivial goal\n", !IO)
                ;
                    IsTrivial = goal_is_nontrivial,
                    io.write_string("% nontrivial goal\n", !IO)
                ),
                write_indent(Indent, !IO),
                (
                    PortCountsGiveCoverageAfter =
                        port_counts_give_coverage_after,
                    io.write_string("% port counts give coverage after\n", !IO)
                ;
                    PortCountsGiveCoverageAfter =
                        no_port_counts_give_coverage_after,
                    io.write_string("% no port counts give coverage after\n",
                        !IO)
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
    write_goal_expr(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO),
    ( if string.contains_char(DumpOptions, 'i') then
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        ( if
            instmap_delta_is_reachable(InstMapDelta),
            instmap_delta_changed_vars(InstMapDelta, Vars),
            set_of_var.is_empty(Vars)
        then
            true
        else
            write_indent(Indent, !IO),
            ( if string.contains_char(DumpOptions, 'D') then
                io.write_string("% new insts: ", !IO),
                write_instmap_delta(VarSet, VarNamePrint, Indent,
                    InstMapDelta, !IO),
                io.write_string("\n", !IO)
            else
                io.write_string("% vars with new insts: ", !IO),
                write_instmap_delta_vars(VarSet, VarNamePrint,
                    InstMapDelta, !IO),
                io.write_string("\n", !IO)
            )
        )
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'p') then
        ( if
            goal_info_maybe_get_post_deaths(GoalInfo, PostDeaths),
            PostDeathList = set_of_var.to_sorted_list(PostDeaths),
            PostDeathList = [_ | _]
        then
            write_indent(Indent, !IO),
            io.write_string("% post-deaths: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, PostDeathList, !IO),
            io.write_string("\n", !IO)
        else
            true
        ),
        ( if
            goal_info_maybe_get_post_births(GoalInfo, PostBirths),
            PostBirthList = set_of_var.to_sorted_list(PostBirths),
            PostBirthList = [_ | _]
        then
            write_indent(Indent, !IO),
            io.write_string("% post-births: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, PostBirthList, !IO),
            io.write_string("\n", !IO)
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
            write_indent(Indent, !IO),
            io.write_string("% LFU: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, ListLFU, !IO),
            io.write_string("\n", !IO),
            write_indent(Indent, !IO),
            io.write_string("% LBU: ", !IO),
            mercury_output_vars(VarSet, VarNamePrint, ListLBU, !IO),
            io.write_string("\n", !IO),

            write_indent(Indent, !IO),
            write_string("% Reuse: ", !IO),
            (
                ReuseDescription = no_reuse_info,
                io.write_string("no reuse info", !IO)
            ;
                ReuseDescription = no_possible_reuse,
                io.write_string("no possible reuse", !IO)
            ;
                ReuseDescription = missed_reuse(Messages),
                io.write_string("missed (", !IO),
                io.write_list(Messages, ", ", io.write_string, !IO),
                io.write_string(")", !IO)
            ;
                ReuseDescription = potential_reuse(ShortReuseDescr),
                io.write_string("potential reuse (", !IO),
                write_short_reuse_description(ShortReuseDescr, VarSet,
                    VarNamePrint, !IO),
                io.write_string(")", !IO)
            ;
                ReuseDescription = reuse(ShortReuseDescr),
                io.write_string("reuse (", !IO),
                write_short_reuse_description(ShortReuseDescr, VarSet,
                    VarNamePrint, !IO),
                io.write_string(")", !IO)
            ),
            io.write_string("\n", !IO)
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
        write_llds_code_gen_info(Info, GoalInfo, VarSet, VarNamePrint, Indent,
            !IO)
    ),
    ( if string.contains_char(DumpOptions, 'g') then
        Features = goal_info_get_features(GoalInfo),
        set.to_sorted_list(Features, FeatureList),
        (
            FeatureList = []
        ;
            FeatureList = [_ | _],
            write_indent(Indent, !IO),
            io.write_string("% Goal features:  ", !IO),
            io.write(FeatureList, !IO),
            io.write_string("\n", !IO)
        )
    else
        true
    ).

:- pred write_llds_code_gen_info(hlds_out_info::in, hlds_goal_info::in,
    prog_varset::in, var_name_print::in, int::in, io::di, io::uo) is det.

write_llds_code_gen_info(Info, GoalInfo, VarSet, VarNamePrint, Indent, !IO) :-
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'f') then
        goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
        (
            MaybeFollowVars = yes(FollowVars),
            FollowVars = abs_follow_vars(FollowVarsMap, NextRegR, NextRegF),
            map.to_assoc_list(FollowVarsMap, FVlist),
            write_indent(Indent, !IO),
            io.write_string("% follow vars: r", !IO),
            io.write_int(NextRegR, !IO),
            io.write_string(", f", !IO),
            io.write_int(NextRegF, !IO),
            io.write_string("\n", !IO),
            write_var_to_abs_locns(VarSet, VarNamePrint, Indent, FVlist, !IO)
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
            write_indent(Indent, !IO),
            io.write_string("% resume point ", !IO),
            (
                Locs = resume_locs_orig_only,
                io.write_string("orig only ", !IO)
            ;
                Locs = resume_locs_stack_only,
                io.write_string("stack only ", !IO)
            ;
                Locs = resume_locs_orig_and_stack,
                io.write_string("orig and stack ", !IO)
            ;
                Locs = resume_locs_stack_and_orig,
                io.write_string("stack and orig ", !IO)
            ),
            mercury_output_vars(VarSet, VarNamePrint, ResumeVarList, !IO),
            io.write_string("\n", !IO)
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
        write_indent(Indent, !IO),
        io.write_string("% store map:\n", !IO),
        write_var_to_abs_locns(VarSet, VarNamePrint, Indent, StoreMapList, !IO)
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
        write_indent(Indent, !IO),
        io.write_string("% need across call forward vars: ", !IO),
        (
            CallForwardList = [],
            io.write_string("none\n", !IO)
        ;
            CallForwardList = [_ | _],
            write_vars(VarSet, VarNamePrint, CallForwardList, !IO),
            io.write_string("\n", !IO)
        ),

        write_indent(Indent, !IO),
        io.write_string("% need across call resume vars: ", !IO),
        (
            CallResumeList = [],
            io.write_string("none\n", !IO)
        ;
            CallResumeList = [_ | _],
            write_vars(VarSet, VarNamePrint, CallResumeList, !IO),
            io.write_string("\n", !IO)
        ),

        write_indent(Indent, !IO),
        io.write_string("% need across call nondet vars: ", !IO),
        (
            CallNondetList = [],
            io.write_string("none\n", !IO)
        ;
            CallNondetList = [_ | _],
            write_vars(VarSet, VarNamePrint, CallNondetList, !IO),
            io.write_string("\n", !IO)
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

        write_indent(Indent, !IO),
        (
            ResumeOnStack = yes,
            io.write_string("% resume point has stack label\n", !IO)
        ;
            ResumeOnStack = no,
            io.write_string("% resume point has no stack label\n", !IO)
        ),
        write_indent(Indent, !IO),
        io.write_string("% need in resume resume vars: ", !IO),
        (
            ResumeResumeList = [],
            io.write_string("none\n", !IO)
        ;
            ResumeResumeList = [_ | _],
            write_vars(VarSet, VarNamePrint, ResumeResumeList, !IO),
            io.write_string("\n", !IO)
        ),

        write_indent(Indent, !IO),
        io.write_string("% need in resume nondet vars: ", !IO),
        (
            ResumeNondetList = [],
            io.write_string("none\n", !IO)
        ;
            ResumeNondetList = [_ | _],
            write_vars(VarSet, VarNamePrint, ResumeNondetList, !IO),
            io.write_string("\n", !IO)
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
        write_indent(Indent, !IO),
        io.write_string("% need in par_conj vars: ", !IO),
        write_vars(VarSet, VarNamePrint, ParConjList, !IO),
        io.write_string("\n", !IO)
    else
        true
    ).

write_var_to_abs_locns(_, _, _, [], !IO).
write_var_to_abs_locns(VarSet, VarNamePrint, Indent, [Var - Loc | VarLocs],
        !IO) :-
    write_indent(Indent, !IO),
    io.write_string("%\t", !IO),
    mercury_output_var(VarSet, VarNamePrint, Var, !IO),
    io.write_string("\t-> ", !IO),
    abs_locn_to_string(Loc, LocnStr, MaybeWidth),
    io.write_string(LocnStr, !IO),
    (
        MaybeWidth = yes(Width),
        io.write_string(" ", !IO),
        io.write_string(Width, !IO)
    ;
        MaybeWidth = no
    ),
    io.write_string("\n", !IO),
    write_var_to_abs_locns(VarSet, VarNamePrint, Indent, VarLocs, !IO).

:- pred write_instmap_delta(prog_varset::in, var_name_print::in,
    int::in, instmap_delta::in, io::di, io::uo) is det.

write_instmap_delta(VarSet, VarNamePrint, Indent, InstMapDelta, !IO) :-
    ( if instmap_delta_is_unreachable(InstMapDelta) then
        io.write_string("unreachable", !IO)
    else
        instmap_delta_to_assoc_list(InstMapDelta, AssocList),
        write_var_inst_list(VarSet, VarNamePrint, Indent, AssocList, !IO)
    ).

:- pred write_instmap_delta_vars(prog_varset::in, var_name_print::in,
    instmap_delta::in, io::di, io::uo) is det.

write_instmap_delta_vars(VarSet, VarNamePrint, InstMapDelta, !IO) :-
    ( if instmap_delta_is_unreachable(InstMapDelta) then
        io.write_string("unreachable", !IO)
    else
        instmap_delta_to_assoc_list(InstMapDelta, AssocList),
        assoc_list.keys(AssocList, Vars),
        write_vars(VarSet, VarNamePrint, Vars, !IO)
    ).

:- pred write_vars(prog_varset::in, var_name_print::in, list(prog_var)::in,
    io::di, io::uo) is det.

write_vars(_, _, [], !IO).
write_vars(VarSet, VarNamePrint, [Var], !IO) :-
    mercury_output_var(VarSet, VarNamePrint, Var, !IO).
write_vars(VarSet, VarNamePrint, [Var1, Var2 | Vars], !IO) :-
    mercury_output_var(VarSet, VarNamePrint, Var1, !IO),
    io.write_string(", ", !IO),
    write_vars(VarSet, VarNamePrint, [Var2 | Vars], !IO).

:- pred write_short_reuse_description(short_reuse_description::in,
    prog_varset::in, var_name_print::in, io::di, io::uo) is det.

write_short_reuse_description(ShortDescription, VarSet, VarNamePrint, !IO):-
    (
        ShortDescription = cell_died,
        io.write_string("cell died", !IO)
    ;
        ShortDescription = cell_reused(Var, IsConditional, _, _),
        io.write_string("cell reuse - ", !IO),
        mercury_output_var(VarSet, VarNamePrint, Var, !IO),
        io.write_string(" - ", !IO),
        write_is_conditional(IsConditional, !IO)
    ;
        ShortDescription = reuse_call(IsConditional, NoClobbers),
        io.write_string("reuse call - ", !IO),
        write_is_conditional(IsConditional, !IO),
        io.write_string(", no clobbers = ", !IO),
        io.write(NoClobbers, !IO)
    ).

:- pred write_is_conditional(is_conditional::in, io::di, io::uo) is det.

write_is_conditional(IsConditional, !IO) :-
    (
        IsConditional = conditional_reuse,
        io.write_string("with condition", !IO)
    ;
        IsConditional = unconditional_reuse,
        io.write_string("always safe", !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out goal expressions.
%

write_goal_list(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Separator, Goals, !IO) :-
    (
        Goals = [HeadGoal | TailGoals],
        write_indent(Indent, !IO),
        io.write_string(Separator, !IO),
        do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent + 1, "\n", HeadGoal, !IO),
        write_goal_list(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent, Separator, TailGoals, !IO)
    ;
        Goals = []
    ).

:- pred write_goal_expr(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in,
    int::in, string::in, hlds_goal_expr::in, io::di, io::uo) is det.

write_goal_expr(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    (
        GoalExpr = unify(_, _, _, _, _),
        write_goal_unify(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = plain_call(_, _, _, _, _, _),
        write_goal_plain_call(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        write_goal_generic_call(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        write_goal_foreign_proc(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = conj(_, _),
        write_goal_conj(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = disj(_),
        write_goal_disj(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = switch(_, _, _),
        write_goal_switch(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = scope(_, _),
        write_goal_scope(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = if_then_else(_, _, _, _),
        write_goal_if_then_else(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = negation(_),
        write_goal_negation(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ;
        GoalExpr = shorthand(_),
        write_goal_shorthand(Info, ModuleInfo, VarSet, TypeQual,
            VarNamePrint, Indent, Follow, GoalExpr, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out unifications.
%

:- pred write_goal_unify(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_unify),
    io::di, io::uo) is det.

write_goal_unify(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = unify(LHS, RHS, _, Unification, _),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    write_indent(Indent, !IO),
    mercury_output_var(VarSet, VarNamePrint, LHS, !IO),
    io.write_string(" = ", !IO),
    (
        TypeQual = varset_vartypes(_, VarTypes),
        lookup_var_type(VarTypes, LHS, UniType),
        VarType = yes(UniType)
    ;
        TypeQual = no_varset_vartypes,
        VarType = no
    ),
    % XXX Fake the inst varset.
    varset.init(InstVarSet),
    write_unify_rhs_2(Info, ModuleInfo, VarSet, InstVarSet, TypeQual,
        VarNamePrint, Indent, VarType, RHS, !IO),
    io.write_string(Follow, !IO),
    ( if
        ( string.contains_char(DumpOptions, 'u')
        ; string.contains_char(DumpOptions, 'p')
        )
    then
        ( if
            % Don't output bogus info if we haven't been through
            % mode analysis yet.
            Unification = complicated_unify(ComplMode, CanFail, TypeInfoVars),
            CanFail = can_fail,
            ComplMode = unify_modes_lhs_rhs(
                from_to_insts(free, free),
                from_to_insts(free, free)),
            TypeInfoVars = []
        then
            true
        else
            write_unification(Info, ModuleInfo, VarSet, InstVarSet,
                VarNamePrint, Indent, Unification, !IO)
        )
    else
        true
    ).

write_unify_rhs(Info, ModuleInfo, VarSet, InstVarSet, VarNamePrint,
        Indent, RHS, !IO) :-
    write_unify_rhs_2(Info, ModuleInfo, VarSet, InstVarSet, no_varset_vartypes,
        VarNamePrint, Indent, no, RHS, !IO).

:- pred write_unify_rhs_2(hlds_out_info::in, module_info::in,
    prog_varset::in, inst_varset::in, maybe_vartypes::in, var_name_print::in,
    int::in, maybe(mer_type)::in, unify_rhs::in, io::di, io::uo) is det.

write_unify_rhs_2(Info, ModuleInfo, VarSet, InstVarSet, TypeQual, VarNamePrint,
        Indent, MaybeType, RHS, !IO) :-
    (
        RHS = rhs_var(Var),
        mercury_output_var(VarSet, VarNamePrint, Var, !IO)
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
        write_functor_cons_id(ModuleInfo, VarSet, VarNamePrint,
            ConsId, ArgVars, !IO),
        ( if
            MaybeType = yes(Type),
            TypeQual = varset_vartypes(TVarSet, _)
        then
            io.write_string(" : ", !IO),
            mercury_output_type(TVarSet, VarNamePrint, Type, !IO)
        else
            true
        )
    ;
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, _EvalMethod,
            NonLocals, Vars, Modes, Det, Goal),
        Indent1 = Indent + 1,
        write_purity_prefix(Purity, !IO),
        (
            PredOrFunc = pf_predicate,
            (
                Groundness = ho_ground,
                Functor = "pred"
            ;
                Groundness = ho_any,
                Functor = "any_pred"
            ),
            io.write_string("(", !IO),
            (
                Vars = [],
                io.write_strings(["(", Functor, ")"], !IO)
            ;
                Vars = [_ | _],
                io.write_strings([Functor, "("], !IO),
                write_var_modes(VarSet, InstVarSet, VarNamePrint, Vars, Modes,
                    !IO),
                io.write_string(")", !IO)
            ),
            io.write_string(" is ", !IO),
            mercury_output_det(Det, !IO),
            io.write_string(" :-\n", !IO),
            do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
                Indent1, "\n", Goal, !IO),
            write_indent(Indent, !IO),
            io.write_string(")\n", !IO)
        ;
            PredOrFunc = pf_function,
            (
                Groundness = ho_ground,
                Functor = "func"
            ;
                Groundness = ho_any,
                Functor = "any_func"
            ),
            pred_args_to_func_args(Modes, ArgModes, RetMode),
            pred_args_to_func_args(Vars, ArgVars, RetVar),
            io.write_string("(", !IO),
            (
                ArgVars = [],
                io.write_strings(["(", Functor, ")"], !IO)
            ;
                ArgVars = [_ | _],
                io.write_strings([Functor, "("], !IO),
                write_var_modes(VarSet, InstVarSet, VarNamePrint,
                    ArgVars, ArgModes, !IO),
                io.write_string(")", !IO)
            ),
            io.write_string(" = (", !IO),
            write_var_mode(VarSet, InstVarSet, VarNamePrint,
                RetVar - RetMode, !IO),
            io.write_string(") is ", !IO),
            mercury_output_det(Det, !IO),
            io.write_string(" :-\n", !IO),
            do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
                Indent1, "\n", Goal, !IO),
            write_indent(Indent, !IO),
            io.write_string(")\n", !IO)
        ),
        ( if
            MaybeType = yes(Type),
            TypeQual = varset_vartypes(TVarSet, _)
        then
            io.write_string(" : ", !IO),
            mercury_output_type(TVarSet, VarNamePrint, Type, !IO)
        else
            true
        ),
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'n') then
            (
                NonLocals = [_ | _],
                write_indent(Indent1, !IO),
                io.write_string("% lambda nonlocals: ", !IO),
                mercury_output_vars(VarSet, VarNamePrint, NonLocals, !IO)
            ;
                NonLocals = []
            )
        else
            true
        )
    ).

unify_rhs_to_string(ModuleInfo, VarSet, VarNamePrint, RHS) = Str :-
    (
        RHS = rhs_var(Var),
        Str = mercury_var_to_string(VarSet, VarNamePrint, Var)
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
        Str = functor_cons_id_to_string(ModuleInfo, VarSet, VarNamePrint,
            ConsId, ArgVars)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, _),
        Str = "lambda goal"
    ).

:- pred write_unification(hlds_out_info::in, module_info::in,
    prog_varset::in, inst_varset::in, var_name_print::in, int::in,
    unification::in, io::di, io::uo) is det.

write_unification(Info, ModuleInfo, ProgVarSet, InstVarSet, VarNamePrint,
        Indent, Unification, !IO) :-
    (
        Unification = assign(X, Y),
        write_indent(Indent, !IO),
        io.write_string("% ", !IO),
        mercury_output_var(ProgVarSet, VarNamePrint, X, !IO),
        io.write_string(" := ", !IO),
        mercury_output_var(ProgVarSet, VarNamePrint, Y, !IO),
        io.write_string("\n", !IO)
    ;
        Unification = simple_test(X, Y),
        write_indent(Indent, !IO),
        io.write_string("% ", !IO),
        mercury_output_var(ProgVarSet, VarNamePrint, X, !IO),
        io.write_string(" == ", !IO),
        mercury_output_var(ProgVarSet, VarNamePrint, Y, !IO),
        io.write_string("\n", !IO)
    ;
        Unification = construct(Var, ConsId, ArgVars, ArgModes, ConstructHow,
            Uniqueness, SubInfo),
        write_indent(Indent, !IO),
        io.write_string("% ", !IO),
        mercury_output_var(ProgVarSet, VarNamePrint, Var, !IO),
        io.write_string(" := ", !IO),
        write_functor_and_submodes(Info, ModuleInfo, ProgVarSet, InstVarSet,
            VarNamePrint, Indent, ConsId, ArgVars, ArgModes, !IO),

        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'u') then
            ( if ConsId = cons(_, _, TypeCtor) then
                TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
                write_indent(Indent, !IO),
                TypeCtorSymNameStr = sym_name_to_string(TypeCtorSymName),
                io.format("%% cons_id type_ctor: %s/%d",
                    [s(TypeCtorSymNameStr), i(TypeCtorArity)], !IO)
            else
                true
            ),
            (
                Uniqueness = cell_is_unique,
                write_indent(Indent, !IO),
                io.write_string("% cell_is_unique\n", !IO)
            ;
                Uniqueness = cell_is_shared
            ),
            (
                SubInfo = no_construct_sub_info
            ;
                SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize),
                (
                    MaybeTakeAddr = yes(TakeAddressFields),
                    write_indent(Indent, !IO),
                    io.write_string("% take address fields: ", !IO),
                    write_intlist(TakeAddressFields, !IO),
                    io.write_string("\n", !IO)
                ;
                    MaybeTakeAddr = no
                ),
                (
                    MaybeSize = yes(SizeSource),
                    write_indent(Indent, !IO),
                    io.write_string("% term size ", !IO),
                    (
                        SizeSource = known_size(KnownSize),
                        io.write_string("const ", !IO),
                        io.write_int(KnownSize, !IO),
                        io.write_string("\n", !IO)
                    ;
                        SizeSource = dynamic_size(SizeVar),
                        io.write_string("var ", !IO),
                        mercury_output_var(ProgVarSet, VarNamePrint, SizeVar,
                            !IO),
                        io.write_string("\n", !IO)
                    )
                ;
                    MaybeSize = no
                )
            ),
            (
                ConstructHow = construct_dynamically
            ;
                ConstructHow = construct_statically,
                write_indent(Indent, !IO),
                io.write_string("% construct statically\n", !IO)
            ;
                ConstructHow = reuse_cell(CellToReuse),
                CellToReuse = cell_to_reuse(ReuseVar, _ReuseConsIds,
                    _FieldAssigns),
                write_indent(Indent, !IO),
                io.write_string("% reuse cell: ", !IO),
                mercury_output_var(ProgVarSet, VarNamePrint, ReuseVar, !IO),
                io.write_string("\n", !IO)
            ;
                ConstructHow = construct_in_region(RegVar),
                write_indent(Indent, !IO),
                io.write_string("% construct in region: ", !IO),
                mercury_output_var(ProgVarSet, VarNamePrint, RegVar, !IO),
                io.write_string("\n", !IO)
            )
        else
            true
        )
    ;
        Unification = deconstruct(Var, ConsId, ArgVars, ArgModes, CanFail,
            CanCGC),
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'G') then
            write_indent(Indent, !IO),
            io.write_string("% Compile time garbage collect: ", !IO),
            io.write(CanCGC, !IO),
            io.nl(!IO)
        else
            true
        ),
        write_indent(Indent, !IO),
        io.write_string("% ", !IO),
        mercury_output_var(ProgVarSet, VarNamePrint, Var, !IO),
        (
            CanFail = can_fail,
            io.write_string(" ?= ", !IO)
        ;
            CanFail = cannot_fail,
            io.write_string(" => ", !IO)
        ),
        write_functor_and_submodes(Info, ModuleInfo, ProgVarSet, InstVarSet,
            VarNamePrint, Indent, ConsId, ArgVars, ArgModes, !IO)
    ;
        Unification = complicated_unify(Mode, CanFail, TypeInfoVars),
        write_indent(Indent, !IO),
        io.write_string("% ", !IO),
        (
            CanFail = can_fail,
            io.write_string("can_fail, ", !IO)
        ;
            CanFail = cannot_fail,
            io.write_string("cannot_fail, ", !IO)
        ),
        io.write_string("mode: ", !IO),
        mercury_output_unify_mode(Mode, InstVarSet, !IO),
        io.write_string("\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% type-info vars: ", !IO),
        mercury_output_vars(ProgVarSet, VarNamePrint, TypeInfoVars, !IO),
        io.write_string("\n", !IO)
    ).

:- pred write_functor_and_submodes(hlds_out_info::in, module_info::in,
    prog_varset::in, inst_varset::in, var_name_print::in, int::in,
    cons_id::in, list(prog_var)::in, list(unify_mode)::in,
    io::di, io::uo) is det.

write_functor_and_submodes(Info, _ModuleInfo, ProgVarSet, InstVarSet,
        VarNamePrint, Indent, ConsId, ArgVars, ArgModes, !IO) :-
    io.write_string(cons_id_and_arity_to_string(ConsId), !IO),
    (
        ArgVars = [],
        io.write_string("\n", !IO)
    ;
        ArgVars = [_ | _],
        io.write_string(" (", !IO),
        mercury_output_vars(ProgVarSet, VarNamePrint, ArgVars, !IO),
        io.write_string(")\n", !IO),
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if string.contains_char(DumpOptions, 'a') then
            ( if string.contains_char(DumpOptions, 'y') then
                write_indent(Indent, !IO),
                io.write_string("% arg-modes\n", !IO),
                mercury_output_structured_unify_mode_list(ArgModes, Indent,
                    output_debug, do_incl_addr, InstVarSet, !IO)
            else
                write_indent(Indent, !IO),
                io.write_string("% arg-modes ", !IO),
                mercury_output_unify_mode_list(ArgModes, InstVarSet, !IO),
                io.write_string("\n", !IO)
            )
        else
            true
        )
    ).

%-----------------------------------------------------------------------------%
%
% Write out ordinary first-order calls.
%

:- pred write_goal_plain_call(hlds_out_info::in, module_info::in,
    prog_varset::in,maybe_vartypes::in, var_name_print::in,
    int::in, string::in, hlds_goal_expr::in(goal_expr_plain_call),
    io::di, io::uo) is det.

write_goal_plain_call(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = plain_call(PredId, ProcId, ArgVars, Builtin,
        MaybeUnifyContext, PredName),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'b') then
        (
            Builtin = inline_builtin,
            write_indent(Indent, !IO),
            io.write_string("% inline builtin\n", !IO)
        ;
            Builtin = not_builtin
        )
    else
        true
    ),
    write_indent(Indent, !IO),
    ( if PredId = invalid_pred_id then
        % If we don't know then the call must be treated as a predicate.
        PredOrFunc = pf_predicate
    else
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_purity(PredInfo, Purity),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        write_purity_prefix(Purity, !IO)
    ),
    (
        PredOrFunc = pf_predicate,
        NewArgVars = ArgVars
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, NewArgVars, LHSVar),
        mercury_output_var(VarSet, VarNamePrint, LHSVar, !IO),
        io.write_string(" = ", !IO)
    ),
    write_sym_name_and_args(VarSet, VarNamePrint, PredName, NewArgVars, !IO),
    io.write_string(Follow, !IO),
    ( if string.contains_char(DumpOptions, 'l') then
        pred_id_to_int(PredId, PredNum),
        proc_id_to_int(ProcId, ProcNum),
        write_indent(Indent, !IO),
        io.write_string("% pred id: ", !IO),
        io.write_int(PredNum, !IO),
        io.write_string(", proc id: ", !IO),
        io.write_int(ProcNum, !IO),
        io.write_string(Follow, !IO),
        (
            MaybeUnifyContext = yes(CallUnifyContext),
            (
                TypeQual = varset_vartypes(_, VarTypes),
                lookup_var_type(VarTypes, Var, UniType),
                VarType = yes(UniType)
            ;
                TypeQual = no_varset_vartypes,
                VarType = no
            ),
            CallUnifyContext = call_unify_context(Var, RHS, _UnifyContext),
            write_indent(Indent, !IO),
            io.write_string("% unify context: ", !IO),
            mercury_output_var(VarSet, VarNamePrint, Var, !IO),
            io.write_string(" = ", !IO),
            % XXX Fake the inst varset.
            varset.init(InstVarSet),
            write_unify_rhs_2(Info, ModuleInfo, VarSet, InstVarSet, TypeQual,
                VarNamePrint, Indent, VarType, RHS, !IO),
            io.write_string(Follow, !IO)
        ;
            MaybeUnifyContext = no
        )
    else
        true
    ).

:- pred write_sym_name_and_args(prog_varset::in, var_name_print::in,
    sym_name::in, list(prog_var)::in, io::di, io::uo) is det.

write_sym_name_and_args(VarSet, VarNamePrint, PredName, ArgVars, !IO) :-
    (
        PredName = qualified(ModuleName, Name),
        write_qualified_functor(VarSet, VarNamePrint,
            ModuleName, term.atom(Name), ArgVars, !IO)
    ;
        PredName = unqualified(Name),
        write_functor_maybe_needs_quotes(VarSet, VarNamePrint,
            next_to_graphic_token, term.atom(Name), ArgVars, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out generic calls.
%

:- pred write_goal_generic_call(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in, int::in,
    string::in, hlds_goal_expr::in(goal_expr_generic_call),
    io::di, io::uo) is det.

write_goal_generic_call(Info, _ModuleInfo, VarSet, _TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = generic_call(GenericCall, ArgVars, Modes, MaybeArgRegs, _),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    % XXX We should print more info here.
    (
        GenericCall = higher_order(PredVar, Purity, PredOrFunc, _),
        (
            PredOrFunc = pf_predicate,
            ( if string.contains_char(DumpOptions, 'l') then
                write_indent(Indent, !IO),
                io.write_string("% higher-order predicate call\n", !IO),
                write_ho_arg_regs(Indent, MaybeArgRegs, !IO)
            else
                true
            ),
            write_indent(Indent, !IO),
            write_purity_prefix(Purity, !IO),
            write_functor(VarSet, VarNamePrint,
                term.atom("call"), [PredVar | ArgVars], !IO)
        ;
            PredOrFunc = pf_function,
            ( if string.contains_char(DumpOptions, 'l') then
                write_indent(Indent, !IO),
                io.write_string("% higher-order function application\n", !IO),
                write_ho_arg_regs(Indent, MaybeArgRegs, !IO)
            else
                true
            ),
            pred_args_to_func_args([PredVar | ArgVars],
                FuncArgVars, FuncRetVar),
            write_indent(Indent, !IO),
            write_purity_prefix(Purity, !IO),
            mercury_output_var(VarSet, VarNamePrint, FuncRetVar, !IO),
            io.write_string(" = ", !IO),
            write_functor(VarSet, VarNamePrint,
                term.atom("apply"), FuncArgVars, !IO)
        ),
        io.write_string(Follow, !IO)
    ;
        GenericCall = class_method(TCInfoVar, MethodNum, _ClassId,
            _MethodId),
        ( if string.contains_char(DumpOptions, 'l') then
            write_indent(Indent, !IO),
            io.write_string("% class method call\n", !IO),
            write_ho_arg_regs(Indent, MaybeArgRegs, !IO)
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
        write_indent(Indent, !IO),
        mercury_output_term(VarSet, VarNamePrint, Term, !IO),
        io.write_string(Follow, !IO)
    ;
        GenericCall = event_call(EventName),
        ( if string.contains_char(DumpOptions, 'l') then
            write_indent(Indent, !IO),
            io.write_string("% event call\n", !IO),
            write_ho_arg_regs(Indent, MaybeArgRegs, !IO)
        else
            true
        ),
        write_indent(Indent, !IO),
        io.write_string("event ", !IO),
        term.context_init(Context),
        Functor = term.atom(EventName),
        term.var_list_to_term_list(ArgVars, ArgTerms),
        Term = term.functor(Functor, ArgTerms, Context),
        mercury_output_term(VarSet, VarNamePrint, Term, !IO),
        io.write_string(Follow, !IO)
    ;
        GenericCall = cast(CastType),
        CastTypeString = cast_type_to_string(CastType),
        ( if string.contains_char(DumpOptions, 'l') then
            write_indent(Indent, !IO),
            io.write_strings(["% ", CastTypeString, "\n"], !IO),
            write_ho_arg_regs(Indent, MaybeArgRegs, !IO)
        else
            true
        ),
        ( if string.contains_char(DumpOptions, 'D') then
            write_indent(Indent, !IO),
            io.write_string("% modes: ", !IO),
            varset.init(InstVarSet),
            mercury_output_mode_list(output_debug, InstVarSet, Modes, !IO),
            io.nl(!IO)
        else
            true
        ),
        Functor = term.atom(CastTypeString),
        term.var_list_to_term_list(ArgVars, ArgTerms),
        term.context_init(Context),
        Term = term.functor(Functor, ArgTerms, Context),
        write_indent(Indent, !IO),
        mercury_output_term(VarSet, VarNamePrint, Term, !IO),
        io.write_string(Follow, !IO)
    ).

:- pred write_ho_arg_regs(int::in, arg_reg_type_info::in,
    io::di, io::uo) is det.

write_ho_arg_regs(Indent, MaybeArgRegs, !IO) :-
    (
        MaybeArgRegs = arg_reg_types(ArgRegs),
        write_indent(Indent, !IO),
        io.write_string("% arg regs: ", !IO),
        io.write_list(ArgRegs, ", ", write_ho_arg_reg, !IO),
        io.nl(!IO)
    ;
        MaybeArgRegs = arg_reg_types_unset
    ).

:- pred write_ho_arg_reg(ho_arg_reg::in, io::di, io::uo) is det.

write_ho_arg_reg(ArgReg, !IO) :-
    (
        ArgReg = ho_arg_reg_r,
        io.write_string("reg_r", !IO)
    ;
        ArgReg = ho_arg_reg_f,
        io.write_string("reg_f", !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out calls to foreign procs.
%

:- pred write_goal_foreign_proc(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in, int::in,
    string::in, hlds_goal_expr::in(goal_expr_foreign_proc),
    io::di, io::uo) is det.

write_goal_foreign_proc(_Info, ModuleInfo, VarSet, _TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = call_foreign_proc(Attributes, PredId, ProcId,
        Args, ExtraArgs, MaybeTraceRuntimeCond, PragmaCode),
    ForeignLang = get_foreign_language(Attributes),
    write_indent(Indent, !IO),
    io.write_string("$pragma_foreign_proc(/* ", !IO),
    io.write_string(foreign_language_string(ForeignLang), !IO),
    io.write_string(" */, ", !IO),
    write_pred_id(ModuleInfo, PredId, !IO),
    io.write_string(" pred ", !IO),
    pred_id_to_int(PredId, PredIdInt),
    io.write_int(PredIdInt, !IO),
    io.write_string(" proc ", !IO),
    proc_id_to_int(ProcId, ProcIdInt),
    io.write_int(ProcIdInt, !IO),
    io.write_string(",\n", !IO),
    (
        MaybeTraceRuntimeCond = no
    ;
        MaybeTraceRuntimeCond = yes(TraceRuntimeCond),
        write_indent(Indent, !IO),
        io.write_string("% trace_runtime_cond(", !IO),
        mercury_output_trace_expr(mercury_output_trace_runtime,
            TraceRuntimeCond, !IO),
        io.write_string(")\n", !IO)
    ),
    write_indent(Indent, !IO),
    % XXX We don't have the TypeVarSet available here, but it is only used
    % for printing out the names of the type variables, which isn't
    % essential.
    varset.init(TypeVarSet),
    io.write_string("[", !IO),
    write_foreign_args(VarSet, TypeVarSet, VarNamePrint, Args, !IO),
    io.write_string("],\n", !IO),
    (
        ExtraArgs = []
    ;
        ExtraArgs = [_ | _],
        write_indent(Indent, !IO),
        io.write_string("{", !IO),
        write_foreign_args(VarSet, TypeVarSet, VarNamePrint, ExtraArgs, !IO),
        io.write_string("},\n", !IO)
    ),
    PragmaCode = fp_impl_ordinary(Code, _),
    io.write_string("""", !IO),
    io.write_string(Code, !IO),
    io.write_string("""", !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

:- pred write_foreign_args(prog_varset::in, tvarset::in, var_name_print::in,
    list(foreign_arg)::in, io::di, io::uo) is det.

write_foreign_args(_, _, _, [], !IO).
write_foreign_args(VarSet, TVarSet, VarNamePrint, [Arg | Args], !IO) :-
    Arg = foreign_arg(Var, MaybeNameMode, Type, BoxPolicy),
    mercury_output_var(VarSet, VarNamePrint, Var, !IO),
    (
        MaybeNameMode = yes(foreign_arg_name_mode(Name, Mode)),
        io.write_string("/" ++ Name ++ "(", !IO),
        ( if Mode = in_mode then
            io.write_string("in", !IO)
        else if Mode = out_mode then
            io.write_string("out", !IO)
        else
            io.write(Mode, !IO)
        ),
        io.write_string(")", !IO)
    ;
        MaybeNameMode = no
    ),
    (
        BoxPolicy = bp_native_if_possible
    ;
        BoxPolicy = bp_always_boxed,
        io.write_string("$alwaysboxed", !IO)
    ),
    io.write_string("@", !IO),
    mercury_output_type(TVarSet, VarNamePrint, Type, !IO),
    (
        Args = []
    ;
        Args = [_ | _],
        io.write_string(", ", !IO),
        write_foreign_args(VarSet, TVarSet, VarNamePrint, Args, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out disjunctions.
%

:- pred write_goal_conj(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in, int::in,
    string::in, hlds_goal_expr::in(goal_expr_conj), io::di, io::uo) is det.

write_goal_conj(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = conj(ConjType, List),
    (
        List = [Goal | Goals],
        (
            ConjType = plain_conj,
            DumpOptions = Info ^ hoi_dump_hlds_options,
            ( if DumpOptions = "" then
                write_conj(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
                    Indent, Follow, ",\n", Goal, Goals, !IO)
            else
                write_indent(Indent, !IO),
                io.write_string("( % conjunction\n", !IO),
                write_conj(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
                    Indent + 1, "\n", ",\n", Goal, Goals, !IO),
                write_indent(Indent, !IO),
                io.write_string(")", !IO),
                io.write_string(Follow, !IO)
            )
        ;
            ConjType = parallel_conj,
            write_indent(Indent, !IO),
            io.write_string("( % parallel conjunction\n", !IO),
            do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
                Indent + 1, "\n", Goal, !IO),
            % See comments at write_goal_list.
            write_goal_list(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
                Indent, "&\n", Goals, !IO),
            write_indent(Indent, !IO),
            io.write_string(")", !IO),
            io.write_string(Follow, !IO)
        )
    ;
        List = [],
        write_indent(Indent, !IO),
        (
            ConjType = plain_conj,
            io.write_string("true", !IO)
        ;
            ConjType = parallel_conj,
            io.write_string("/* parallel */ true", !IO)
        ),
        io.write_string(Follow, !IO)
    ).

:- pred write_conj(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in,
    int::in, string::in, string::in,
    hlds_goal::in, list(hlds_goal)::in,io::di, io::uo) is det.

write_conj(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint, Indent,
        Follow, Separator, Goal1, Goals1, !IO) :-
    (
        Goals1 = [Goal2 | Goals2],
        DumpOptions = Info ^ hoi_dump_hlds_options,
        ( if DumpOptions = "" then
            do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
                Indent, Separator, Goal1, !IO)
        else
            % When generating verbose dumps, we want the comma on its own line,
            % since that way it visually separates the lines after one goal
            % and the lines before the next.
            do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
                Indent, "\n", Goal1, !IO),
            write_indent(Indent, !IO),
            io.write_string(Separator, !IO)
        ),
        write_conj(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent, Follow, Separator, Goal2, Goals2, !IO)
    ;
        Goals1 = [],
        do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent, Follow, Goal1, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out disjunctions.
%

:- pred write_goal_disj(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in, int::in,
    string::in, hlds_goal_expr::in(goal_expr_disj), io::di, io::uo) is det.

write_goal_disj(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = disj(Disjuncts),
    write_indent(Indent, !IO),
    (
        Disjuncts = [Goal | Goals],
        io.write_string("( % disjunction\n", !IO),
        do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent + 1, "\n", Goal, !IO),
        write_goal_list(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent, ";\n", Goals, !IO),
        write_indent(Indent, !IO),
        io.write_string(")", !IO),
        io.write_string(Follow, !IO)
    ;
        Disjuncts = [],
        io.write_string("fail", !IO),
        io.write_string(Follow, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out switches.
%

:- pred write_goal_switch(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in,var_name_print::in, int::in,
    string::in, hlds_goal_expr::in(goal_expr_switch), io::di, io::uo) is det.

write_goal_switch(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = switch(Var, CanFail, CasesList),
    write_indent(Indent, !IO),
    io.write_string("( % ", !IO),
    io.write_string(can_fail_to_string(CanFail), !IO),
    io.write_string(" switch on `", !IO),
    mercury_output_var(VarSet, VarNamePrint, Var, !IO),
    io.write_string("'\n", !IO),
    Indent1 = Indent + 1,
    (
        CasesList = [Case | Cases],
        write_case(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent1, Var, Case, !IO),
        write_cases(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent, Var, Cases, !IO)
    ;
        CasesList = [],
        write_indent(Indent1, !IO),
        io.write_string("fail\n", !IO)
    ),
    write_indent(Indent, !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

:- pred write_cases(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in, int::in,
    prog_var::in, list(case)::in, io::di, io::uo) is det.

write_cases(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint, Indent,
        Var, CasesList, !IO) :-
    (
        CasesList = [Case | Cases],
        write_indent(Indent, !IO),
        io.write_string(";\n", !IO),
        write_case(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent + 1, Var, Case, !IO),
        write_cases(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent, Var, Cases, !IO)
    ;
        CasesList = []
    ).

:- pred write_case(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in, int::in,
    prog_var::in, case::in, io::di, io::uo) is det.

write_case(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint, Indent,
        Var, Case, !IO) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    write_indent(Indent, !IO),
    io.write_string("% ", !IO),
    mercury_output_var(VarSet, VarNamePrint, Var, !IO),
    io.write_string(" has functor ", !IO),
    io.write_string(cons_id_and_arity_to_string(MainConsId), !IO),
    list.foldl(write_alternative_cons_id, OtherConsIds, !IO),
    io.write_string("\n", !IO),
    % XXX if the output of this is to be used, e.g. in
    % inter-module optimization, output a unification to bind the
    % Var to the functor, since simplify.m and unused_args.m remove
    % the unification. At the moment this is not a problem, since
    % intermod.m works on the unoptimized clauses.
    do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, "\n", Goal, !IO).

:- pred write_alternative_cons_id(cons_id::in, io::di, io::uo) is det.

write_alternative_cons_id(ConsId, !IO) :-
    io.write_string(" or ", !IO),
    io.write_string(cons_id_and_arity_to_string(ConsId), !IO).

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

%-----------------------------------------------------------------------------%
%
% Write out negations.
%

:- pred write_goal_negation(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in, int::in,
    string::in, hlds_goal_expr::in(goal_expr_neg), io::di, io::uo) is det.

write_goal_negation(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = negation(Goal),
    write_indent(Indent, !IO),
    io.write_string("not (\n", !IO),
    do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent + 1, "\n", Goal, !IO),
    write_indent(Indent, !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

%-----------------------------------------------------------------------------%
%
% Write out if-then-elses.
%

:- pred write_goal_if_then_else(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in,var_name_print::in, int::in,
    string::in, hlds_goal_expr::in(goal_expr_ite), io::di, io::uo) is det.

write_goal_if_then_else(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = if_then_else(Vars, Cond, Then, Else),
    write_indent(Indent, !IO),
    io.write_string("( if", !IO),
    write_some(VarSet, Vars, !IO),
    io.write_string("\n", !IO),
    Indent1 = Indent + 1,
    do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent1, "\n", Cond, !IO),
    write_indent(Indent, !IO),
    io.write_string("then\n", !IO),
    do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent1, "\n", Then, !IO),
    write_indent(Indent, !IO),
    io.write_string("else\n", !IO),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if
        DumpOptions \= "",
        Else = hlds_goal(if_then_else(_, _, _, _), _)
    then
        ElseIndent = Indent
    else
        ElseIndent = Indent1
    ),
    do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        ElseIndent, "\n", Else, !IO),
    write_indent(Indent, !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

:- pred write_some(prog_varset::in, list(prog_var)::in, io::di, io::uo) is det.

write_some(_VarSet, _Vars, !IO).
    % Quantification is all implicit by the time we get to the HLDS.

%-----------------------------------------------------------------------------%
%
% Write out scope goals.
%

:- pred write_goal_scope(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in, int::in,
    string::in, hlds_goal_expr::in(goal_expr_scope), io::di, io::uo) is det.

write_goal_scope(!.Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = scope(Reason, Goal),
    write_indent(Indent, !IO),
    (
        Reason = exist_quant(Vars),
        io.write_string("some [", !IO),
        mercury_output_vars(VarSet, VarNamePrint, Vars, !IO),
        io.write_string("] (\n", !IO)
    ;
        Reason = disable_warnings(HeadWarning, TailWarnings),
        io.write_string("disable_warnings [", !IO),
        write_goal_warnings(HeadWarning, TailWarnings, !IO),
        io.write_string("] (\n", !IO)
    ;
        Reason = promise_purity(Purity),
        (
            Purity = purity_pure,
            io.write_string("promise_pure (", !IO)
        ;
            Purity = purity_semipure,
            io.write_string("promise_semipure (", !IO)
        ;
            Purity = purity_impure,
            io.write_string("promise_impure (", !IO)
        ),
        io.write_string("\n", !IO)
    ;
        Reason = promise_solutions(Vars, Kind),
        (
            Kind = equivalent_solutions,
            io.write_string("promise_equivalent_solutions", !IO)
        ;
            Kind = equivalent_solution_sets,
            io.write_string("promise_equivalent_solution_sets", !IO)
        ;
            Kind = equivalent_solution_sets_arbitrary,
            io.write_string("arbitrary", !IO)
        ),
        io.write_string(" [", !IO),
        mercury_output_vars(VarSet, VarNamePrint, Vars, !IO),
        io.write_string("] (\n", !IO)
    ;
        Reason = require_detism(Detism),
        (
            Detism = detism_det,
            io.write_string("require_det", !IO)
        ;
            Detism = detism_semi,
            io.write_string("require_semidet", !IO)
        ;
            Detism = detism_non,
            io.write_string("require_nondet", !IO)
        ;
            Detism = detism_multi,
            io.write_string("require_multi", !IO)
        ;
            Detism = detism_cc_multi,
            io.write_string("require_cc_multi", !IO)
        ;
            Detism = detism_cc_non,
            io.write_string("require_cc_nondet", !IO)
        ;
            Detism = detism_failure,
            io.write_string("require_failure", !IO)
        ;
            Detism = detism_erroneous,
            io.write_string("require_erroneous", !IO)
        ),
        io.write_string(" (\n", !IO)
    ;
        Reason = require_complete_switch(Var),
        io.write_string("require_complete_switch [", !IO),
        mercury_output_var(VarSet, VarNamePrint, Var, !IO),
        io.write_string("] (\n", !IO)
    ;
        Reason = require_switch_arms_detism(Var, Detism),
        (
            Detism = detism_det,
            io.write_string("require_switch_arms_det", !IO)
        ;
            Detism = detism_semi,
            io.write_string("require_switch_arms_semidet", !IO)
        ;
            Detism = detism_non,
            io.write_string("require_switch_arms_nondet", !IO)
        ;
            Detism = detism_multi,
            io.write_string("require_switch_arms_multi", !IO)
        ;
            Detism = detism_cc_multi,
            io.write_string("require_switch_arms_cc_multi", !IO)
        ;
            Detism = detism_cc_non,
            io.write_string("require_switch_arms_cc_nondet", !IO)
        ;
            Detism = detism_failure,
            io.write_string("require_switch_arms_failure", !IO)
        ;
            Detism = detism_erroneous,
            io.write_string("require_switch_arms_erroneous", !IO)
        ),
        io.write_string(" [", !IO),
        mercury_output_var(VarSet, VarNamePrint, Var, !IO),
        io.write_string("] (\n", !IO)
    ;
        Reason = barrier(removable),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% barrier(removable)\n", !IO)
    ;
        Reason = barrier(not_removable),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% barrier(not_removable)\n", !IO)
    ;
        Reason = commit(force_pruning),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% commit(force_pruning)\n", !IO)
    ;
        Reason = commit(dont_force_pruning),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% commit(dont_force_pruning)\n", !IO)
    ;
        Reason = from_ground_term(Var, Kind),
        io.write_string("(\n", !IO),
        write_indent(Indent, !IO),
        io.write_string("% from_ground_term [", !IO),
        mercury_output_var(VarSet, VarNamePrint, Var, !IO),
        io.write_string(", ", !IO),
        (
            Kind = from_ground_term_initial,
            io.write_string("initial", !IO)
        ;
            Kind = from_ground_term_construct,
            io.write_string("construct", !IO)
        ;
            Kind = from_ground_term_deconstruct,
            io.write_string("deconstruct", !IO)
        ;
            Kind = from_ground_term_other,
            io.write_string("other", !IO)
        ),
        io.write_string("]\n", !IO),
        % The goals inside from_ground_term scopes are created with
        % all of the fields of goal infos already filled in.
        % This means printing them is meaningful, and sometimes
        % it is needed to diagnose problems.
        DumpOptionsBackup = !.Info ^ hoi_dump_hlds_options_backup,
        !Info ^ hoi_dump_hlds_options := DumpOptionsBackup
    ;
        Reason = trace_goal(MaybeCompileTime, MaybeRunTime, MaybeIO,
            MutableVars, QuantVars),
        io.write_string("trace [\n", !IO),
        some [!AddCommaNewlineIndent] (
            !:AddCommaNewlineIndent = no,
            (
                MaybeCompileTime = yes(CompileTime),
                write_indent(Indent + 1, !IO),
                io.write_string("compile_time(", !IO),
                mercury_output_trace_expr(mercury_output_trace_compiletime,
                    CompileTime, !IO),
                io.write_string(")", !IO),
                !:AddCommaNewlineIndent = yes
            ;
                MaybeCompileTime = no
            ),
            (
                MaybeRunTime = yes(RunTime),
                maybe_add_comma_newline_indent(!.AddCommaNewlineIndent,
                    Indent + 1, !IO),
                io.write_string("runtime(", !IO),
                mercury_output_trace_expr(mercury_output_trace_runtime,
                    RunTime, !IO),
                io.write_string(")", !IO),
                !:AddCommaNewlineIndent = yes
            ;
                MaybeRunTime = no
            ),
            (
                MaybeIO = yes(IOStateVarName),
                maybe_add_newline_indent(!.AddCommaNewlineIndent,
                    Indent + 1, !IO),
                io.write_string("% io(!" ++ IOStateVarName ++ ")", !IO),
                !:AddCommaNewlineIndent = yes
            ;
                MaybeIO = no
            ),

            list.foldl2(write_trace_mutable_var_hlds(Indent + 1), MutableVars,
                !AddCommaNewlineIndent, !IO),

            maybe_add_newline_indent(!.AddCommaNewlineIndent,
                Indent + 1, !IO),
            io.write_string("% quantified vars [", !IO),
            mercury_output_vars(VarSet, VarNamePrint, QuantVars, !IO),
            io.write_string("]", !IO),

            maybe_add_newline_indent(!.AddCommaNewlineIndent, Indent, !IO),
            io.write_string("] (\n", !IO)
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
        io.format("loop_control_spawn_off_%s(", [s(UseParentStackStr)], !IO),
        mercury_output_vars(VarSet, VarNamePrint, [LCVar, LCSVar], !IO),
        io.write_string(") (\n", !IO)
    ),
    do_write_goal(!.Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent + 1, "\n", Goal, !IO),
    write_indent(Indent, !IO),
    io.write_string(")", !IO),
    io.write_string(Follow, !IO).

:- pred write_trace_mutable_var_hlds(int::in, trace_mutable_var_hlds::in,
    bool::in, bool::out, io::di, io::uo) is det.

write_trace_mutable_var_hlds(Indent, MutableVar, !AddCommaNewlineIndent,
        !IO) :-
    MutableVar = trace_mutable_var_hlds(MutableName, StateVarName),
    maybe_add_newline_indent(!.AddCommaNewlineIndent, Indent, !IO),
    io.write_string("% state(" ++ MutableName ++ ", ", !IO),
    io.write_string("!" ++ StateVarName ++ ")", !IO),
    !:AddCommaNewlineIndent = yes.

:- pred maybe_add_comma_newline_indent(bool::in, int::in, io::di, io::uo)
    is det.

maybe_add_comma_newline_indent(AddCommaNewlineIndent, Indent, !IO) :-
    (
        AddCommaNewlineIndent = no
    ;
        AddCommaNewlineIndent = yes,
        io.write_string(",\n", !IO),
        write_indent(Indent, !IO)
    ).

:- pred maybe_add_newline_indent(bool::in, int::in, io::di, io::uo) is det.

maybe_add_newline_indent(AddCommaNewlineIndent, Indent, !IO) :-
    (
        AddCommaNewlineIndent = no
    ;
        AddCommaNewlineIndent = yes,
        io.write_string("\n", !IO),
        write_indent(Indent, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Write out shorthand goals.
%

:- pred write_goal_shorthand(hlds_out_info::in, module_info::in,
    prog_varset::in, maybe_vartypes::in, var_name_print::in, int::in,
    string::in, hlds_goal_expr::in(goal_expr_shorthand),
    io::di, io::uo) is det.

write_goal_shorthand(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent, Follow, GoalExpr, !IO) :-
    GoalExpr = shorthand(ShortHand),
    (
        ShortHand = atomic_goal(_GoalType, Outer, Inner, MaybeOutputVars,
            MainGoal, OrElseGoals, _OrElseInners),
        write_indent(Indent, !IO),
        io.write_string("atomic [", !IO),
        write_atomic_interface_vars(VarSet, VarNamePrint, "outer", Outer,
            !IO),
        io.write_string(" ", !IO),
        write_atomic_interface_vars(VarSet, VarNamePrint, "inner", Inner,
            !IO),
        io.write_string(" ", !IO),
        (
            MaybeOutputVars = no
        ;
            MaybeOutputVars = yes(OutputVars),
            io.write_string("vars([", !IO),
            mercury_output_vars(VarSet, VarNamePrint, OutputVars, !IO),
            io.write_string("])", !IO)
        ),
        io.write_string("] (\n",!IO),

        do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent + 1, "\n", MainGoal, !IO),
        write_goal_list(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent, "or_else\n", OrElseGoals, !IO),
        write_indent(Indent, !IO),
        io.write_string(")", !IO),
        io.write_string(Follow, !IO)
    ;
        ShortHand = try_goal(MaybeIO, _, SubGoal),
        write_indent(Indent, !IO),
        io.write_string("( % try\n", !IO),
        (
            MaybeIO = yes(try_io_state_vars(IOVarA, IOVarB)),
            write_indent(Indent + 1, !IO),
            io.write_string("% io(", !IO),
            mercury_output_vars(VarSet, VarNamePrint, [IOVarA, IOVarB], !IO),
            io.write_string(")\n", !IO)
        ;
            MaybeIO = no
        ),
        do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent + 1, "\n", SubGoal, !IO),
        write_indent(Indent, !IO),
        io.write_string(")", !IO),
        io.write_string(Follow, !IO)
    ;
        ShortHand = bi_implication(GoalA, GoalB),
        write_indent(Indent, !IO),
        io.write_string("( % bi-implication\n", !IO),
        Indent1 = Indent + 1,
        do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent1, "\n", GoalA, !IO),
        write_indent(Indent, !IO),
        io.write_string("<=>\n", !IO),
        do_write_goal(Info, ModuleInfo, VarSet, TypeQual, VarNamePrint,
            Indent1, "\n", GoalB, !IO),
        write_indent(Indent, !IO),
        io.write_string(")", !IO),
        io.write_string(Follow, !IO)
    ).

:- pred write_atomic_interface_vars(prog_varset::in, var_name_print::in,
    string::in, atomic_interface_vars::in, io::di, io::uo) is det.

write_atomic_interface_vars(VarSet, VarNamePrint, CompName, CompState, !IO) :-
    io.write_string(CompName, !IO),
    io.write_string("(", !IO),
    CompState = atomic_interface_vars(Var1, Var2),
    mercury_output_var(VarSet, VarNamePrint, Var1, !IO),
    io.write_string(", ", !IO),
    mercury_output_var(VarSet, VarNamePrint, Var2, !IO),
    io.write_string(")", !IO).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_goal.
%-----------------------------------------------------------------------------%
