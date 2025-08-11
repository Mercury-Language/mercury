%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2017, 2019-2020, 2022-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mmakefiles.m
% Main author: zs.
%
% This file defines
% - a set of types for representing (fragments of) Mmakefiles, and
% - predicates for writing them out.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module libs.mmakefiles.
:- interface.

:- import_module cord.
:- import_module list.
:- import_module one_or_more.

%---------------------------------------------------------------------------%

:- pred start_mmakefile(mmakefile::out) is det.

:- pred add_mmake_fragment(mmake_fragment::in,
    mmakefile::in, mmakefile::out) is det.
:- pred add_mmake_fragments(list(mmake_fragment)::in,
    mmakefile::in, mmakefile::out) is det.
:- pred add_mmake_entry(mmake_entry::in,
    mmakefile::in, mmakefile::out) is det.
:- pred add_mmake_entries(list(mmake_entry)::in,
    mmakefile::in, mmakefile::out) is det.

:- func mmake_entry_to_fragment(mmake_entry) = mmake_fragment.

:- func mmakefile_to_string(mmakefile) = string.

%---------------------------------------------------------------------------%

    % An mmakefile, like a makefile, is a sequence of entries, but
    % conditionals may select between two alternative subsequences of entries.
    % We represent such structure by using mmake_fragments, with each
    % mmake_fragment representing one if-then-else. In general, both arms
    % of the if-then-else are a sequence of other fragments. That sequence
    % may be empty, or it may contain other conditionals, nested arbitrarily
    % deeply. However, the mmake_fragment type has a direct representation
    % of the common pattern where the condition simply selects between
    % two alternative rules.
    %
:- type mmakefile == cord(mmake_fragment).
:- type mmake_fragment
    --->    mmf_entry(mmake_entry)
    ;       mmf_conditional_entry(
                mmfce_test      :: mmake_condition,
                mmfce_if_true   :: mmake_entry,
                mmfce_if_false  :: mmake_entry
            )
    ;       mmf_conditional_fragments(
                mmfcfs_test     :: mmake_condition,
                % The true list should contain at least one fragment;
                % the false list may be empty.
                mmfcfs_if_true  :: list(mmake_fragment),
                mmfcfs_if_false :: list(mmake_fragment)
            ).

    % The mmake_entry type represents the two traditional kinds of
    % makefile entries, variable definitions and rules, but it has
    % significantly more than two function symbols, for two reasons.
    %
    % One reason is that we use this type to represent block comments
    % as well as variable definitions and rules. (This is simpler
    % than allowing the attachment of comments to other constructs,
    % and more expressive than not allowing comments at all.)
    %
    % The other reason is the main reason, which is that the type
    % provides two function symbols to represent variable definitions,
    % and several function symbols to represent rules. For each kind
    % of construct, one function symbol is the fully general form,
    % and the others provide more convenient ways to represent
    % common special forms. For rules, the fully general form
    % allows the sources and targets to be partitioned into named groups.
    % When they are so partitioned, the lists of sources and targets
    % are visibly partitioned in the generated mmakefile, and the
    % names of the groups are printed as comments preceding the rule,
    % so those names can help readers understand where each source or target
    % came from.
    %
    % Every rule has a name which is printed before the rule (unless
    % the "name" is the empty string), and an indication whether the
    % targets are phony.
    %
:- type mmake_entry
    --->    mmake_start_comment(
                msc_contents    :: string,
                msc_module_name :: string,
                msc_source_file :: string,
                msc_version     :: string,
                msc_full_arch   :: string
            )
    ;       mmake_block_comment(
                % A block comment. Each string is one line of that block.
                mbc_comments    :: list(string)
            )
    ;       mmake_var_defn(
                % A variable definition in which the assigned value
                % is specified as a single string.
                mvd_var_name    :: string,
                mvd_var_value   :: string
            )
    ;       mmake_var_defn_list(
                % A variable definition in which the assigned value
                % is specified as a sequence of zero or more strings.
                mvdl_var_name   :: string,
                mvdl_var_value  :: list(string)
            )
    ;       mmake_simple_rule(
                % A rule that has exactly one target.
                % Neither the target nor the sources are in named groups.
                msr_rule_name   :: string,
                msr_flags       :: is_mmake_rule_phony,
                msr_targets     :: mmake_file_name,
                msr_sources     :: list(mmake_file_name),
                msr_actions     :: list(mmake_action)
            )
    ;       mmake_flat_rule(
                % A rule that has one or more targets.
                % Neither the targets nor the sources are in named groups.
                mfr_rule_name   :: string,
                mfr_flags       :: is_mmake_rule_phony,
                mfr_targets     :: one_or_more(mmake_file_name),
                mfr_sources     :: list(mmake_file_name),
                mfr_actions     :: list(mmake_action)
            )
    ;       mmake_deep_rule(
                % A rule that has exactly one target.
                % The target is not in a named group, but the sources are.
                mdr_rule_name   :: string,
                mdr_flags       :: is_mmake_rule_phony,
                mdr_targets     :: mmake_file_name,
                mdr_sources     :: list(mmake_file_name_group),
                mdr_actions     :: list(mmake_action)
            )
    ;       mmake_general_rule(
                % A rule that has one or more targets.
                % Both the targets and the sources are in named groups.
                mgr_rule_name   :: string,
                mgr_flags       :: is_mmake_rule_phony,
                mgr_targets     :: one_or_more(mmake_file_name_group),
                mgr_sources     :: list(mmake_file_name_group),
                mgr_actions     :: list(mmake_action)
            ).

:- type is_mmake_rule_phony
    --->    mmake_rule_is_not_phony
    ;       mmake_rule_is_phony.

    % A "file name" acting as a target or as a source.
    %
    % It can be a literal file name or a make variable that expands to
    % a file name, but it can also be other things. It can be a literal
    % *list* of file names, a make variable that expands to a list of
    % file names, or it can be an invocation of gmake functions
    % that expand to a file name or a list of file names.
    % And any of those file names can be a *phony* file name,
    % i.e. the name of a file that is never supposed to exist.
    %
    % XXX Can anyone think of a better name for this type?
    % (And for the next one.) A name such as mmake_source_or_target
    % would be more accurate, but also much more clumsy.
    %
:- type mmake_file_name == string.

:- type mmake_file_name_group
    --->    mmake_file_name_group(
                fng_group_name  :: string,
                fng_file_names  :: one_or_more(mmake_file_name)
            ).

    % An mmake_action is one line in a makefile action.
    %
    % Mmake_actions should not include the initial tab and the final newline.
    % However, if an action is supposed to be split into two or more lines,
    % then
    %
    % - each line other than the last should end with " \" (" \\" in the
    %   Mercury source code) to escape the automatically added newline
    %   that would follow, and
    %
    % - each line other than the first should have an initial tab. This
    %   would be the *second* tab on the line to indent the continuation line,
    %   not the initial tab that designates the line as an action line
    %   (the initial tab is added automatically by the code that writes
    %   the action out).
    %
    % XXX I (zs) have tried out a representation for multi-line actions
    % which added the " \" to the ends of non-final lines and the extra tab
    % to the start of non-initial lines automatically. It did not work.
    % It did slightly simplify the code for actions that were split across
    % several lines just because the action's command was too long to fit
    % on one line, but it could not handle actions that were effectively
    % embedded shell scripts, such as the action for MmakeRuleLibInstallInts
    % in generate_dep_file_install_targets. That is because the above scheme
    % generates a block of text with uniform indentation after the first line,
    % but such scripts want indentation that reflects their nesting structure,
    % which *won't* be uniform. In addition, such scripts may need "; \" at
    % the ends of command lines even if the command fits on one line, simply
    % to get make to execute them in the same shell instance, thus letting
    % variable bindings flow from earlier commands to later ones.
    %
    % One could devise a structured representation to handle all these use
    % cases, but its complexity would not be worth the cost, either in extra
    % code to both construct and to interpret that structured representation,
    % or in breaking the current direct connection between what is visible
    % in the code that constructs mmake actions, and the code that gets put
    % into Mmakefile fragments.
    %
:- type mmake_action == string.

:- func make_multiline_action(list(string)) = list(string).

%---------------------------------------------------------------------------%

    % The conditions of if-then-elses in mmakefiles.
    %
    % This type needs to represent only the kinds of conditions
    % needed by the Mmakefiles generated by the Mercury compiler;
    % it does need to (and cannot) represent all possible kinds of
    % makefile conditions.
    %
    % At the moment, the only conditions we need are
    %
    % - grade tests, presently implemented crudely as a test for
    %   the presence of a given grade component string in "$(GRADE)", and
    %
    % - tests for the equality or inequality of two strings.
    %
:- type mmake_condition
    --->    mmake_cond_grade_has_component(mmake_grade_component)
    % ;     mmake_cond_not_grade_has_component(mmake_grade_component)
    ;       mmake_cond_strings_equal(string, string)
    ;       mmake_cond_strings_not_equal(string, string).

    % XXX We should have a mechanism for making the presence or absence
    % or *each* individual grade component visible in separate mmake
    % variables. Once we have that, we should use a more structured
    % representation here.
:- type mmake_grade_component == string.

%---------------------------------------------------------------------------%

    % make_file_name_group(GroupName, FileNames):
    %
    % If FileNames names is not the empty list, return a single file name
    % group named GroupName containing FileNames.
    %
    % If FileNames names is the empty list, return an empty list
    % of file name groups.
    %
:- func make_file_name_group(string, list(mmake_file_name))
    = list(mmake_file_name_group).

    % Return a file name group with the given name containing just
    % the given file name.
    %
:- func make_singleton_file_name_group(string, mmake_file_name)
    = mmake_file_name_group.

    % Return an action that prints nothing and does nothing,
    % but *is* nevertheless an action as far as make is concerned.
    % We use it to force GNU Make to recheck the timestamp on the target file.
    % (It is a pity that GNU Make doesn't have a way of handling
    % these sorts of rules in a nicer manner.)
    %
:- func silent_noop_action = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.
:- import_module string.builder.

%---------------------------------------------------------------------------%

start_mmakefile(!:MmakeFile) :-
    !:MmakeFile = cord.init.

%---------------------------------------------------------------------------%

add_mmake_fragment(Fragment, !MmakeFile) :-
    !:MmakeFile = cord.snoc(!.MmakeFile, Fragment).

add_mmake_fragments(Fragments, !MmakeFile) :-
    !:MmakeFile = !.MmakeFile ++ cord.from_list(Fragments).

add_mmake_entry(Entry, !MmakeFile) :-
    !:MmakeFile = cord.snoc(!.MmakeFile, mmake_entry_to_fragment(Entry)).

add_mmake_entries(Entries, !MmakeFile) :-
    !:MmakeFile = !.MmakeFile ++
        cord.from_list(list.map(mmake_entry_to_fragment, Entries)).

mmake_entry_to_fragment(Entry) = mmf_entry(Entry).

%---------------------------------------------------------------------------%

:- type maybe_add_mmake_comments
    --->    do_not_add_mmake_comments
    ;       add_mmake_comments.

mmakefile_to_string(MmakeFile) = MmakeFileStr :-
    SB0 = string.builder.init,
    cord.foldl_pred(
        append_mmake_fragment(add_mmake_comments),
        MmakeFile, SB0, SB),
    MmakeFileStr = string.builder.to_string(SB).

:- pred append_mmake_fragments(maybe_add_mmake_comments::in,
    list(mmake_fragment)::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_fragments(__WriteComments, [], !SB).
append_mmake_fragments(WriteComments, [MmakeFragment | MmakeFragments], !SB) :-
    append_mmake_fragment(WriteComments, MmakeFragment, !SB),
    append_mmake_fragments(WriteComments, MmakeFragments, !SB).

:- pred append_mmake_fragment(maybe_add_mmake_comments::in,
    mmake_fragment::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_fragment(WriteComments, MmakeFragment, !SB) :-
    (
        MmakeFragment = mmf_entry(Entry),
        append_mmake_entry(WriteComments, Entry, !SB)
    ;
        MmakeFragment = mmf_conditional_entry(Cond, ThenEntry, ElseEntry),
        append_mmake_condition(Cond, !SB),
        append_string("\n", !SB),
        append_mmake_entry(WriteComments, ThenEntry, !SB),
        append_string("else\n\n", !SB),
        append_mmake_entry(WriteComments, ElseEntry, !SB),
        append_string("endif # conditional fragment\n\n", !SB)
    ;
        MmakeFragment = mmf_conditional_fragments(Cond,
            ThenFragments, ElseFragments),
        append_mmake_condition(Cond, !SB),
        append_string("\n", !SB),
        append_mmake_fragments(WriteComments, ThenFragments, !SB),
        (
            ElseFragments = []
        ;
            ElseFragments = [_ | _],
            append_string("else\n\n", !SB),
            append_mmake_fragments(WriteComments, ElseFragments, !SB)
        ),
        append_string("endif # conditional fragment\n\n", !SB)
    ).

:- pred append_mmake_condition(mmake_condition::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_condition(Cond, !SB) :-
    (
        Cond = mmake_cond_grade_has_component(GradeComponent),
        string.builder.format("ifeq ($(findstring %s,$(GRADE)),%s)\n",
            [s(GradeComponent), s(GradeComponent)], !SB)
%   ;
%       Cond = mmake_cond_not_grade_has_component(GradeComponent),
%       string.builder.format("ifneq ($(findstring %s,$(GRADE)),%s)\n",
%           [s(GradeComponent), s(GradeComponent)], !SB)
    ;
        Cond = mmake_cond_strings_equal(StrA, StrB),
        string.builder.format("ifeq (%s,%s)\n", [s(StrA), s(StrB)], !SB)
    ;
        Cond = mmake_cond_strings_not_equal(StrA, StrB),
        string.builder.format("ifneq (%s,%s)\n", [s(StrA), s(StrB)], !SB)
    ).

%---------------------------------------------------------------------------%

:- pred append_mmake_entry(maybe_add_mmake_comments::in, mmake_entry::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_entry(_WriteComments, MmakeEntry, !SB) :-
    (
        MmakeEntry = mmake_start_comment(Contents, ModuleName, SourceFile,
            Version, FullArch),
        string.builder.format("# vim: ts=8 sw=8 noexpandtab ft=make\n\n",
            [], !SB),
        string.builder.format("# Automatically generated %s for\n",
            [s(Contents)], !SB),
        string.builder.format("# module `%s' in source file `%s'.\n",
            [s(ModuleName), s(SourceFile)], !SB),
        string.builder.format("# Generated by Mercury compiler version %s\n",
            [s(Version)], !SB),
        string.builder.format("# configured for %s.\n",
            [s(FullArch)], !SB)
    ;
        MmakeEntry = mmake_block_comment(CommentLines),
        list.foldl(append_block_comment_line, CommentLines, !SB)
    ;
        MmakeEntry = mmake_var_defn(VarName, Value),
        string.builder.format("%s = %s\n", [s(VarName), s(Value)], !SB)
    ;
        MmakeEntry = mmake_var_defn_list(VarName, Values),
        (
            Values = [],
            string.builder.format("%s =\n", [s(VarName)], !SB)
        ;
            Values = [HeadValue | TailValues],
            (
                TailValues = [],
                string.builder.format("%s = %s\n",
                    [s(VarName), s(HeadValue)], !SB)
            ;
                TailValues = [_ | _],
                string.builder.format("%s = \\\n", [s(VarName)], !SB),
                append_mmake_var_values(HeadValue, TailValues, !SB)
            )
        )
    ;
        (
            MmakeEntry = mmake_simple_rule(RuleName, IsPhony,
                _, SourceFiles, Actions)
        ;
            MmakeEntry = mmake_flat_rule(RuleName, IsPhony,
                _, SourceFiles, Actions)
        ),

        append_rule_name(RuleName, !SB),

        (
            MmakeEntry = mmake_simple_rule(_, _, TargetFile, _, _),
            maybe_append_phony_rule(IsPhony, [TargetFile], !SB),
            append_mmake_file_name(TargetFile, !SB)
        ;
            MmakeEntry = mmake_flat_rule(_, _,
                one_or_more(HeadTargetFile, TailTargetFiles), _, _),
            maybe_append_phony_rule(IsPhony,
                [HeadTargetFile | TailTargetFiles], !SB),
            append_mmake_file_names_horizontal(
                HeadTargetFile, TailTargetFiles, !SB)
        ),
        (
            SourceFiles = [],
            append_string(" :", !SB)
        ;
            SourceFiles = [HeadSourceFile | TailSourceFiles],
            append_string(" : ", !SB),
            ( if
                (
                    % Always write trans_opt_deps vertically as the list needs
                    % to be parseable by maybe_read_d_file_for_trans_opt_deps.
                    RuleName = "trans_opt_deps"
                ;
                    1 + list.length(TailSourceFiles) > max_horizontal
                )
            then
                append_string("\\\n", !SB),
                append_mmake_file_names_vertical(
                    HeadSourceFile, TailSourceFiles, !SB)
            else
                append_mmake_file_names_horizontal(
                    HeadSourceFile, TailSourceFiles, !SB)
            )
        ),
        append_string("\n", !SB),
        append_mmake_actions(Actions, !SB)
    ;
        (
            MmakeEntry = mmake_deep_rule(RuleName, IsPhony, _,
                SourceGroups, Actions)
        ;
            MmakeEntry = mmake_general_rule(RuleName, IsPhony, _,
                SourceGroups, Actions)
        ),

        append_rule_name(RuleName, !SB),
        (
            MmakeEntry = mmake_deep_rule(_, _, _, _, _)
        ;
            MmakeEntry = mmake_general_rule(_, _, TargetGroups0, _, _),
            maybe_append_group_names("target",
                one_or_more_to_list(TargetGroups0), !SB)
        ),
        maybe_append_group_names("source", SourceGroups, !SB),

        (
            MmakeEntry = mmake_deep_rule(_, _, TargetFile, _, _),
            maybe_append_phony_rule(IsPhony, [TargetFile], !SB),
            append_mmake_file_name(TargetFile, !SB)
        ;
            MmakeEntry = mmake_general_rule(_, _, TargetGroups, _, _),
            maybe_append_phony_rule(IsPhony,
                file_name_groups_files(TargetGroups), !SB),
            TargetGroups = one_or_more(HeadTargetGroup, TailTargetGroups),
            append_mmake_file_name_groups_horizontal(
                HeadTargetGroup, TailTargetGroups, !SB)
        ),

        (
            SourceGroups = [],
            append_string(" :", !SB)
        ;
            SourceGroups = [HeadSourceGroup | TailSourceGroups],
            append_string(" : \\\n", !SB),
            append_mmake_file_name_groups_vertical(
                HeadSourceGroup, TailSourceGroups, !SB)
        ),
        append_string("\n", !SB),

        append_mmake_actions(Actions, !SB)
    ),
    % Provide visual separation from the next entry.
    append_string("\n", !SB).

%---------------------%

:- pred append_block_comment_line(string::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_block_comment_line(Comment, !SB) :-
    string.builder.format("# %s\n", [s(Comment)], !SB).

:- pred maybe_append_group_names(string::in, list(mmake_file_name_group)::in,
    string.builder.state::di, string.builder.state::uo) is det.

maybe_append_group_names(TargetOrSource, Groups, !SB) :-
    ( if all_group_names_are_empty(Groups) then
        % There are no group names to write out.
        true
    else
        string.builder.format("# %s group names:\n", [s(TargetOrSource)], !SB),
        list.foldl(append_group_name, Groups, !SB)
    ).

:- pred all_group_names_are_empty(list(mmake_file_name_group)::in) is semidet.

all_group_names_are_empty([]).
all_group_names_are_empty([Group | Groups]) :-
    Group = mmake_file_name_group(GroupName, _),
    GroupName = "",
    all_group_names_are_empty(Groups).

:- pred append_group_name(mmake_file_name_group::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_group_name(Group, !SB) :-
    Group = mmake_file_name_group(GroupName0, _),
    ( if GroupName0 = "" then
        GroupName = "(unnamed)"
    else
        GroupName = GroupName0
    ),
    string.builder.format("#   %s\n", [s(GroupName)], !SB).

:- func max_horizontal = int.

max_horizontal = 1.

%---------------------%

:- pred append_mmake_var_values(string::in, list(string)::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_var_values(HeadValue, TailValues, !SB) :-
    (
        TailValues = [],
        string.builder.format("\t%s\n", [s(HeadValue)], !SB)
    ;
        TailValues = [HeadTailValue | TailTailValues],
        string.builder.format("\t%s \\\n", [s(HeadValue)], !SB),
        append_mmake_var_values(HeadTailValue, TailTailValues, !SB)
    ).

%---------------------%

:- pred maybe_append_phony_rule(is_mmake_rule_phony::in, list(string)::in,
    string.builder.state::di, string.builder.state::uo) is det.

maybe_append_phony_rule(IsPhony, FileNames, !SB) :-
    (
        IsPhony = mmake_rule_is_not_phony
    ;
        IsPhony = mmake_rule_is_phony,
        FileNamesStr = string.join_list(" ", FileNames),
        string.builder.format(".PHONY: %s\n", [s(FileNamesStr)], !SB)
    ).

%---------------------%

:- pred append_rule_name(string::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_rule_name(RuleName, !SB) :-
    ( if RuleName = "" then
        true
    else
        string.builder.format("# RULE %s\n", [s(RuleName)], !SB)
    ).

%---------------------%

    % Write out a list of file name groups with each file name
    % being written after the previous one.
    %
    % The initial position of the cursor is expected to be
    % where the first file name is to be written.
    %
    % The final position of the cursor will be immediately after
    % the last file name written out. This will be on the same line
    % as the initial position.
    %
:- pred append_mmake_file_name_groups_horizontal(
    mmake_file_name_group::in, list(mmake_file_name_group)::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_file_name_groups_horizontal(FileNameGroup, FileNameGroups, !SB) :-
    append_mmake_file_name_group_horizontal(FileNameGroup, !SB),
    (
        FileNameGroups = []
    ;
        FileNameGroups = [HeadFileNameGroup | TailFileNameGroups],
        append_string(" ", !SB),
        append_mmake_file_name_groups_horizontal(HeadFileNameGroup,
            TailFileNameGroups, !SB)
    ).

    % Write out a list of file name groups with each file name being written
    % below the previous one, writing an empty line (containing only a
    % backslash to indicate "continued below") between successive groups.
    %
    % The initial position of the cursor is expected to be the start of
    % the line on which the first file name is to be written.
    %
    % The final position of the cursor will be at the end of the line
    % containing the last file name written out.
    %
:- pred append_mmake_file_name_groups_vertical(
    mmake_file_name_group::in, list(mmake_file_name_group)::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_file_name_groups_vertical(
        FileNameGroup, FileNameGroups, !SB) :-
    append_mmake_file_name_group_vertical(FileNameGroup, !SB),
    (
        FileNameGroups = []
    ;
        FileNameGroups = [HeadFileNameGroup | TailFileNameGroups],
        append_string(" \\\n", !SB),
        append_string("\t\\\n", !SB),
        append_mmake_file_name_groups_vertical(HeadFileNameGroup,
            TailFileNameGroups, !SB)
    ).

    % Write out a list of file names with each file name being written after
    % the previous one.
    %
    % The initial position of the cursor is expected to be
    % where the first file name is to be written.
    %
    % The final position of the cursor will be immediately after
    % the last file name written out. This will be on the same line
    % as the initial position.
    %
:- pred append_mmake_file_name_group_horizontal(mmake_file_name_group::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_file_name_group_horizontal(FileNameGroup, !SB) :-
    FileNameGroup = mmake_file_name_group(_GroupName,
        one_or_more(FileName, FileNames)),
    append_mmake_file_names_horizontal(FileName, FileNames, !SB).

    % Write out a list of file names with each file name being written below
    % the previous one.
    %
    % The initial position of the cursor is expected to be the start of
    % the line on which the first file name is to be written.
    %
    % The final position of the cursor will be at the end of the line
    % containing the last file name written out.
    %
:- pred append_mmake_file_name_group_vertical(mmake_file_name_group::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_file_name_group_vertical(FileNameGroup, !SB) :-
    FileNameGroup = mmake_file_name_group(_GroupName,
        one_or_more(FileName, FileNames)),
    append_mmake_file_names_vertical(FileName, FileNames, !SB).

%---------------------%

    % Write out a list of file names with each file name being written after
    % the previous one.
    %
    % The initial position of the cursor is expected to be
    % where the first file name is to be written.
    %
    % The final position of the cursor will be immediately after
    % the last file name written out. This will be on the same line
    % as the initial position.
    %
:- pred append_mmake_file_names_horizontal(
    mmake_file_name::in, list(mmake_file_name)::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_file_names_horizontal(FileName, FileNames, !SB) :-
    append_mmake_file_name(FileName, !SB),
    (
        FileNames = []
    ;
        FileNames = [HeadFileName | TailFileNames],
        append_string(" ", !SB),
        append_mmake_file_names_horizontal(HeadFileName, TailFileNames, !SB)
    ).

    % Write out a list of file names with each file name being written below
    % the previous one.
    %
    % The initial position of the cursor is expected to be the start of
    % the line on which the first file name is to be written.
    %
    % The final position of the cursor will be at the end of the line
    % containing the last file name written out.
    %
:- pred append_mmake_file_names_vertical(
    mmake_file_name::in, list(mmake_file_name)::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_file_names_vertical(FileName, FileNames, !SB) :-
    append_string("\t\t", !SB),
    append_mmake_file_name(FileName, !SB),
    (
        FileNames = []
    ;
        FileNames = [HeadFileName | TailFileNames],
        append_string(" \\\n", !SB),
        append_mmake_file_names_vertical(
            HeadFileName, TailFileNames, !SB)
    ).

%---------------------%

:- pred append_mmake_file_name(mmake_file_name::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_file_name(FileName, !SB) :-
    append_string(FileName, !SB).

%---------------------%

:- pred append_mmake_actions(list(mmake_action)::in,
    string.builder.state::di, string.builder.state::uo) is det.

append_mmake_actions([], !SB).
append_mmake_actions([Action | Actions], !SB) :-
    string.builder.format("\t%s\n", [s(Action)], !SB),
    append_mmake_actions(Actions, !SB).

%---------------------------------------------------------------------------%

    % Return the list of file names in a file name group.
    %
:- func file_name_group_files(mmake_file_name_group) = list(mmake_file_name).

file_name_group_files(FileNameGroup) = [HeadFileName | TailFileNames] :-
    FileNameGroup = mmake_file_name_group(_GroupName,
        one_or_more(HeadFileName, TailFileNames)).

    % Return the list of file names in a list of file name groups.
    %
:- func file_name_groups_files(one_or_more(mmake_file_name_group))
    = list(mmake_file_name).

file_name_groups_files(FileNameGroups) = FileNames :-
    FileNameGroups = one_or_more(HeadFileNameGroup, TailFileNameGroups),
    FileNamesList = list.map(file_name_group_files,
        [HeadFileNameGroup | TailFileNameGroups]),
    FileNames = list.condense(FileNamesList).

%---------------------------------------------------------------------------%

make_multiline_action(Lines0) = Lines :-
    (
        Lines0 = [],
        Lines = []
    ;
        Lines0 = [HeadLine0 | TailLines0],
        Lines = make_multiline_action_lag(HeadLine0, TailLines0)
    ).

:- func make_multiline_action_lag(string, list(string)) = list(string).

make_multiline_action_lag(HeadLine0, TailLines0) = Lines :-
    (
        TailLines0 = [],
        Lines = [HeadLine0]
    ;
        TailLines0 = [HeadTailLine0 | TailTailLines0],
        HeadLine = HeadLine0 ++ " \\",
        HeadTailLine1 = "\t" ++ HeadTailLine0,
        TailLines = make_multiline_action_lag(HeadTailLine1, TailTailLines0),
        Lines = [HeadLine | TailLines]
    ).

%---------------------------------------------------------------------------%

make_file_name_group(GroupName, FileNames) = Groups :-
    (
        FileNames = [],
        Groups = []
    ;
        FileNames = [HeadFileName | TailFileNames],
        Groups = [mmake_file_name_group(GroupName,
            one_or_more(HeadFileName, TailFileNames))]
    ).

make_singleton_file_name_group(GroupName, FileName) =
    mmake_file_name_group(GroupName, one_or_more(FileName, [])).

silent_noop_action = "@:".

%---------------------------------------------------------------------------%
:- end_module libs.mmakefiles.
%---------------------------------------------------------------------------%
