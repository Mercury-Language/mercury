%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
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

:- import_module io.
:- import_module list.
:- import_module cord.

%---------------------------------------------------------------------------%
%
% During the transition period from writing out Mmakefiles directly
% to constructing an mmakefile structure and writing *that* out,
% the protocol for writing out Mmakefiles is the following.
%
% - Call start_mmakefile to create the initial mmakefile structure.
%
% - After writing out each piece of Mmakefile directly, call one of the
%   gather_mmake_* predicates below to construct the corresponding piece
%   of the mmakefile structure.

% - Call end_mmakefile to finalize the mmakefile structure.
%
% This is a single interface, but it has four separate implementations
% behind it. The one actually used is selected by the value returned
% by get_mmake_mode.
%
% The four implementations are as follows.
%
% - With mmake_mode_old_only, no mmakefile is ever constructed;
%   the gather_mmake_* predicates do nothing.
%
% - With mmake_mode_old, no mmakefile is ever constructed;
%   the gather_mmake_* print out the mmakefile pieces given to them
%   in an inactive form, as the then-part of an if-then-else whose
%   condition is false, implemented via the always-failing test
%   "ifeq (x,y)".
%
% - With mmake_mode_new_interleave, no mmakefile is ever constructed;
%   the gather_mmake_* print out the mmakefile pieces given to them
%   in an active (i.e. enabled) form. This mode arranges for the
%   mmakefile pieces written *directly* to the given stream to be
%   inactive instead, using the same test as above.
%
% - With mmake_mode_new_at end, the intended mode of operation
%   once transition is finished, the gather_mmake_* predicates
%   *do* construct an mmakefile, which is then printed by end_mmakefile.
%   Again, all the mmakefile pieces written directly to the stream
%   are made inactive using the same test as above.
%

:- pred start_mmakefile(io.text_output_stream::in,
    mmakefile::out, io::di, io::uo) is det.
:- pred end_mmakefile(io.text_output_stream::in,
    mmakefile::in, io::di, io::uo) is det.

:- pred gather_mmake_fragment(io.text_output_stream::in,
    mmake_fragment::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.
:- pred gather_mmake_fragments(io.text_output_stream::in,
    list(mmake_fragment)::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.
:- pred gather_mmake_entry(io.text_output_stream::in,
    mmake_entry::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.
:- pred gather_mmake_entries(io.text_output_stream::in,
    list(mmake_entry)::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

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
    % the "name" is the empty string), and a indication whether the
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
                % The target is not in named group, but the sources are.
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
    % XXX We *could* have a representation for actions such as this
    % to represent potentially multi-line actions more directly:
    %
    % :- type mmake_action
    %   --->    mmake_action(string, list(string)).
    %
    % With that representation, the final " \"s and the initial tabs
    % would be added automatically by the code writing out actions.
    % This would probably be a better representation once we have
    % switched over to the mmakefile representation *exclusively*,
    % but adding the suffixes and prefixes "by hand" is more compatible
    % with the existing "legacy" code.
    %
:- type mmake_action == string.

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

    % Return an anonymous file name group containing just the given file name.
    %
:- func make_singleton_file_name_group(mmake_file_name)
    = mmake_file_name_group.

    % Return an action that prints nothing and does nothing,
    % but *is* nevertheless an action as far as make is concerned.
    %
:- func silent_noop_action = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

:- type mmake_mode
    --->    mmake_mode_old_only
    ;       mmake_mode_old
    ;       mmake_mode_new_interleave
    ;       mmake_mode_new_at_end.

:- func get_mmake_mode = mmake_mode.

get_mmake_mode = mmake_mode_new_interleave.

%---------------------%

start_mmakefile(OutStream, !:MmakeFile, !IO) :-
    Mode = get_mmake_mode,
    !:MmakeFile = cord.init,
    (
        ( Mode = mmake_mode_old_only
        ; Mode = mmake_mode_old
        )
    ;
        ( Mode = mmake_mode_new_interleave
        ; Mode = mmake_mode_new_at_end
        ),
        io.write_string(OutStream, "ifeq (x,y)\n\n", !IO)
    ).

end_mmakefile(OutStream, !.MmakeFile, !IO) :-
    Mode = get_mmake_mode,
    (
        ( Mode = mmake_mode_old_only
        ; Mode = mmake_mode_old
        )
    ;
        Mode = mmake_mode_new_interleave,
        io.write_string(OutStream, "\nendif # ifeq(x,y)\n\n", !IO)
    ;
        Mode = mmake_mode_new_at_end,
        io.write_string(OutStream, "\nendif # ifeq(x,y)\n\n", !IO),
        cord.foldl_pred(
            write_mmake_fragment(OutStream, make_active, write_mmake_comments),
            !.MmakeFile, !IO)
    ).

%---------------------%

gather_mmake_fragment(OutStream, Fragment, !MmakeFile, !IO) :-
    Mode = get_mmake_mode,
    (
        Mode = mmake_mode_old_only
    ;
        Mode = mmake_mode_old,
        write_mmake_fragment(OutStream, comment_out, write_mmake_comments,
            Fragment, !IO)
    ;
        Mode = mmake_mode_new_interleave,
        io.write_string(OutStream, "\nendif # ifeq(x,y)\n\n", !IO),
        write_mmake_fragment(OutStream, make_active, write_mmake_comments,
            Fragment, !IO),
        io.write_string(OutStream, "ifeq (x,y)\n\n", !IO)
    ;
        Mode = mmake_mode_new_at_end,
        add_mmake_fragment(Fragment, !MmakeFile)
    ).

gather_mmake_fragments(OutStream, Fragments, !MmakeFile, !IO) :-
    Mode = get_mmake_mode,
    (
        Mode = mmake_mode_old_only
    ;
        Mode = mmake_mode_old,
        write_mmake_fragments(OutStream, comment_out, write_mmake_comments,
            Fragments, !IO)
    ;
        Mode = mmake_mode_new_interleave,
        io.write_string(OutStream, "\nendif # ifeq(x,y)\n\n", !IO),
        write_mmake_fragments(OutStream, make_active, write_mmake_comments,
            Fragments, !IO),
        io.write_string(OutStream, "ifeq (x,y)\n\n", !IO)
    ;
        Mode = mmake_mode_new_at_end,
        add_mmake_fragments(Fragments, !MmakeFile)
    ).

gather_mmake_entry(OutStream, Entry, !MmakeFile, !IO) :-
    Mode = get_mmake_mode,
    (
        Mode = mmake_mode_old_only
    ;
        Mode = mmake_mode_old,
        write_mmake_entry(OutStream, comment_out, write_mmake_comments,
            Entry, !IO)
    ;
        Mode = mmake_mode_new_interleave,
        io.write_string(OutStream, "\nendif # ifeq(x,y)\n\n", !IO),
        write_mmake_entry(OutStream, make_active, write_mmake_comments,
            Entry, !IO),
        io.write_string(OutStream, "ifeq (x,y)\n\n", !IO)
    ;
        Mode = mmake_mode_new_at_end,
        add_mmake_entry(Entry, !MmakeFile)
    ).

gather_mmake_entries(OutStream, Entries, !MmakeFile, !IO) :-
    Mode = get_mmake_mode,
    (
        Mode = mmake_mode_old_only
    ;
        Mode = mmake_mode_old,
        write_mmake_entries(OutStream, comment_out, write_mmake_comments,
            Entries, !IO)
    ;
        Mode = mmake_mode_new_interleave,
        io.write_string(OutStream, "\nendif # ifeq(x,y)\n\n", !IO),
        write_mmake_entries(OutStream, make_active, write_mmake_comments,
            Entries, !IO),
        io.write_string(OutStream, "ifeq (x,y)\n\n", !IO)
    ;
        Mode = mmake_mode_new_at_end,
        add_mmake_entries(Entries, !MmakeFile)
    ).

%---------------------------------------------------------------------------%

:- pred add_mmake_fragment(mmake_fragment::in,
    mmakefile::in, mmakefile::out) is det.

add_mmake_fragment(Fragment, !MmakeFile) :-
    !:MmakeFile = cord.snoc(!.MmakeFile, Fragment).

:- pred add_mmake_fragments(list(mmake_fragment)::in,
    mmakefile::in, mmakefile::out) is det.

add_mmake_fragments(Fragments, !MmakeFile) :-
    !:MmakeFile = !.MmakeFile ++ cord.from_list(Fragments).

:- pred add_mmake_entry(mmake_entry::in,
    mmakefile::in, mmakefile::out) is det.

add_mmake_entry(Entry, !MmakeFile) :-
    !:MmakeFile = cord.snoc(!.MmakeFile, mmake_entry_to_fragment(Entry)).

:- pred add_mmake_entries(list(mmake_entry)::in,
    mmakefile::in, mmakefile::out) is det.

add_mmake_entries(Entries, !MmakeFile) :-
    !:MmakeFile = !.MmakeFile ++
        cord.from_list(list.map(mmake_entry_to_fragment, Entries)).

:- func mmake_entry_to_fragment(mmake_entry) = mmake_fragment.

mmake_entry_to_fragment(Entry) = mmf_entry(Entry).

%---------------------------------------------------------------------------%

:- type maybe_write_mmake_comments
    --->    do_not_write_mmake_comments
    ;       write_mmake_comments.

:- type maybe_comment_out
    --->    comment_out
    ;       make_active.

:- pred write_mmake_fragments(io.text_output_stream::in,
    maybe_comment_out::in, maybe_write_mmake_comments::in,
    list(mmake_fragment)::in, io::di, io::uo) is det.

write_mmake_fragments(_OutStream, _MaybeCommentOut, _WriteComments, [], !IO).
write_mmake_fragments(OutStream, MaybeCommentOut, WriteComments,
        [MmakeFragment | MmakeFragments], !IO) :-
    write_mmake_fragment(OutStream, MaybeCommentOut, WriteComments,
        MmakeFragment, !IO),
    write_mmake_fragments(OutStream, MaybeCommentOut, WriteComments,
        MmakeFragments, !IO).

:- pred write_mmake_fragment(io.text_output_stream::in,
    maybe_comment_out::in, maybe_write_mmake_comments::in,
    mmake_fragment::in, io::di, io::uo) is det.

write_mmake_fragment(OutStream, MaybeCommentOut, WriteComments,
        MmakeFragment, !IO) :-
    (
        MmakeFragment = mmf_entry(Entry),
        write_mmake_entry(OutStream, MaybeCommentOut, WriteComments,
            Entry, !IO)
    ;
        MmakeFragment = mmf_conditional_entry(Cond, ThenEntry, ElseEntry),
        write_mmake_condition(OutStream, Cond, !IO),
        io.write_string(OutStream, "\n", !IO),
        write_mmake_entry(OutStream, MaybeCommentOut, WriteComments,
            ThenEntry, !IO),
        io.write_string(OutStream, "else\n", !IO),
        io.write_string(OutStream, "\n", !IO),
        write_mmake_entry(OutStream, MaybeCommentOut, WriteComments,
            ElseEntry, !IO),
        io.write_string(OutStream, "endif # conditional fragment\n\n", !IO)
    ;
        MmakeFragment = mmf_conditional_fragments(Cond,
            ThenFragments, ElseFragments),
        write_mmake_condition(OutStream, Cond, !IO),
        io.write_string(OutStream, "\n", !IO),
        write_mmake_fragments(OutStream, MaybeCommentOut, WriteComments,
            ThenFragments, !IO),
        (
            ElseFragments = []
        ;
            ElseFragments = [_ | _],
            io.write_string(OutStream, "else\n", !IO),
            io.write_string(OutStream, "\n", !IO),
            write_mmake_fragments(OutStream, MaybeCommentOut, WriteComments,
                ElseFragments, !IO)
        ),
        io.write_string(OutStream, "endif # conditional fragment\n\n", !IO)
    ).

:- pred write_mmake_condition(io.text_output_stream::in,
    mmake_condition::in, io::di, io::uo) is det.

write_mmake_condition(OutStream, Cond, !IO) :-
    (
        Cond = mmake_cond_grade_has_component(GradeComponent),
        io.format(OutStream, "ifeq ($(findstring %s,$(GRADE)),%s)\n",
            [s(GradeComponent), s(GradeComponent)], !IO)
%   ;
%       Cond = mmake_cond_not_grade_has_component(GradeComponent),
%       io.format(OutStream, "ifneq ($(findstring %s,$(GRADE)),%s)\n",
%           [s(GradeComponent), s(GradeComponent)], !IO)
    ;
        Cond = mmake_cond_strings_equal(StrA, StrB),
        io.format(OutStream, "ifeq (%s,%s)\n", [s(StrA), s(StrB)], !IO)
    ;
        Cond = mmake_cond_strings_not_equal(StrA, StrB),
        io.format(OutStream, "ifneq (%s,%s)\n", [s(StrA), s(StrB)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred write_mmake_entries(io.text_output_stream::in,
    maybe_comment_out::in, maybe_write_mmake_comments::in,
    list(mmake_entry)::in, io::di, io::uo) is det.

write_mmake_entries(_OutStream, _MaybeCommentOut, _WriteComments, [], !IO).
write_mmake_entries(OutStream, MaybeCommentOut, WriteComments,
        [MmakeEntry | MmakeEntries], !IO) :-
    write_mmake_entry(OutStream, MaybeCommentOut, WriteComments,
        MmakeEntry, !IO),
    write_mmake_entries(OutStream, MaybeCommentOut, WriteComments,
        MmakeEntries, !IO).

:- pred write_mmake_entry(io.text_output_stream::in,
    maybe_comment_out::in, maybe_write_mmake_comments::in,
    mmake_entry::in, io::di, io::uo) is det.

write_mmake_entry(OutStream, MaybeCommentOut, _WriteComments,
        MmakeEntry, !IO) :-
    (
        MaybeCommentOut = make_active
    ;
        MaybeCommentOut = comment_out,
        io.write_string(OutStream, "ifeq (x,y)\n", !IO)
    ),
    (
        MmakeEntry = mmake_start_comment(Contents, ModuleName, SourceFile,
            Version, FullArch),
        io.format(OutStream, "# vim: ts=8 sw=8 noexpandtab ft=make\n\n",
            [], !IO),
        io.format(OutStream, "# Automatically generated %s for\n",
            [s(Contents)], !IO),
        io.format(OutStream, "# module `%s' in source file `%s'.\n",
            [s(ModuleName), s(SourceFile)], !IO),
        io.format(OutStream, "# Generated by Mercury compiler version %s\n",
            [s(Version)], !IO),
        io.format(OutStream, "# configured for %s.\n",
            [s(FullArch)], !IO)
    ;
        MmakeEntry = mmake_block_comment(CommentLines),
        list.foldl(write_block_comment_line(OutStream), CommentLines, !IO)
    ;
        MmakeEntry = mmake_var_defn(VarName, Value),
        io.format(OutStream, "%s = %s\n",
            [s(VarName), s(Value)], !IO)
    ;
        MmakeEntry = mmake_var_defn_list(VarName, Values),
        (
            Values = [],
            io.format(OutStream, "%s =\n", [s(VarName)], !IO)
        ;
            Values = [HeadValue | TailValues],
            (
                TailValues = [],
                io.format(OutStream, "%s = %s\n",
                    [s(VarName), s(HeadValue)], !IO)
            ;
                TailValues = [_ | _],
                io.format(OutStream, "%s = \\\n", [s(VarName)], !IO),
                write_mmake_var_values(OutStream, HeadValue, TailValues, !IO)
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

        write_rule_name(OutStream, RuleName, !IO),

        (
            MmakeEntry = mmake_simple_rule(_, _, TargetFile, _, _),
            maybe_write_phony_rule(OutStream, IsPhony, [TargetFile], !IO),
            write_mmake_file_name(OutStream, TargetFile, !IO)
        ;
            MmakeEntry = mmake_flat_rule(_, _,
                one_or_more(HeadTargetFile, TailTargetFiles), _, _),
            maybe_write_phony_rule(OutStream, IsPhony,
                [HeadTargetFile | TailTargetFiles], !IO),
            write_mmake_file_names_horizontal(OutStream,
                HeadTargetFile, TailTargetFiles, !IO)
        ),
        (
            SourceFiles = [],
            io.write_string(OutStream, " :", !IO)
        ;
            SourceFiles = [HeadSourceFile | TailSourceFiles],
            io.write_string(OutStream, " : ", !IO),
            ( if 1 + list.length(TailSourceFiles) > max_horizontal then
                io.write_string(OutStream, "\\\n", !IO),
                write_mmake_file_names_vertical(OutStream,
                    HeadSourceFile, TailSourceFiles, !IO)
            else
                write_mmake_file_names_horizontal(OutStream,
                    HeadSourceFile, TailSourceFiles, !IO)
            )
        ),
        io.nl(OutStream, !IO),
        write_mmake_actions(OutStream, Actions, !IO)
    ;
        (
            MmakeEntry = mmake_deep_rule(RuleName, IsPhony, _,
                SourceGroups, Actions)
        ;
            MmakeEntry = mmake_general_rule(RuleName, IsPhony, _,
                SourceGroups, Actions)
        ),

        write_rule_name(OutStream, RuleName, !IO),
        (
            MmakeEntry = mmake_deep_rule(_, _, _, _, _)
        ;
            MmakeEntry = mmake_general_rule(_, _, TargetGroups0, _, _),
            maybe_write_group_names(OutStream, "target",
                one_or_more_to_list(TargetGroups0), !IO)
        ),
        maybe_write_group_names(OutStream, "source", SourceGroups, !IO),

        (
            MmakeEntry = mmake_deep_rule(_, _, TargetFile, _, _),
            maybe_write_phony_rule(OutStream, IsPhony, [TargetFile], !IO),
            write_mmake_file_name(OutStream, TargetFile, !IO)
        ;
            MmakeEntry = mmake_general_rule(_, _, TargetGroups, _, _),
            maybe_write_phony_rule(OutStream, IsPhony,
                file_name_groups_files(TargetGroups), !IO),
            TargetGroups = one_or_more(HeadTargetGroup, TailTargetGroups),
            write_mmake_file_name_groups_horizontal(OutStream,
                HeadTargetGroup, TailTargetGroups, !IO)
        ),

        (
            SourceGroups = [],
            io.write_string(OutStream, " :", !IO)
        ;
            SourceGroups = [HeadSourceGroup | TailSourceGroups],
            io.write_string(OutStream, " : \\\n", !IO),
            write_mmake_file_name_groups_vertical(OutStream,
                HeadSourceGroup, TailSourceGroups, !IO)
        ),
        io.nl(OutStream, !IO),

        write_mmake_actions(OutStream, Actions, !IO)
    ),
    (
        MaybeCommentOut = make_active
    ;
        MaybeCommentOut = comment_out,
        io.write_string(OutStream, "endif\n", !IO)
    ),
    % Provide visual separation from the next entry.
    io.nl(OutStream, !IO).

%---------------------%

:- pred write_block_comment_line(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

write_block_comment_line(OutStream, Comment, !IO) :-
    io.format(OutStream, "# %s\n", [s(Comment)], !IO).

:- func one_or_more_to_list(one_or_more(T)) = list(T).

one_or_more_to_list(one_or_more(Head, Tail)) = [Head | Tail].

:- pred maybe_write_group_names(io.text_output_stream::in, string::in,
    list(mmake_file_name_group)::in, io::di, io::uo) is det.

maybe_write_group_names(OutStream, TargetOrSource, Groups, !IO) :-
    GroupNames = list.map(project_group_name, Groups),
    ( if string.append_list(GroupNames) = "" then
        % There are no group names to write out.
        true
    else
        io.format(OutStream, "# %s group names:\n", [s(TargetOrSource)], !IO),
        list.foldl(write_group_name(OutStream), GroupNames, !IO)
    ).

:- func project_group_name(mmake_file_name_group) = string.

project_group_name(mmake_file_name_group(GroupName, _)) = GroupName.

:- pred write_group_name(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

write_group_name(OutStream, GroupName0, !IO) :-
    ( if GroupName0 = "" then
        GroupName = "(unnamed)"
    else
        GroupName = GroupName0
    ),
    io.format(OutStream, "#   %s\n", [s(GroupName)], !IO).

:- func max_horizontal = int.

max_horizontal = 1.

%---------------------%

:- pred write_mmake_var_values(io.text_output_stream::in,
    string::in, list(string)::in, io::di, io::uo) is det.

write_mmake_var_values(OutStream, HeadValue, TailValues, !IO) :-
    (
        TailValues = [],
        io.format(OutStream, "\t%s\n", [s(HeadValue)], !IO)
    ;
        TailValues = [HeadTailValue | TailTailValues],
        io.format(OutStream, "\t%s \\\n", [s(HeadValue)], !IO),
        write_mmake_var_values(OutStream, HeadTailValue, TailTailValues, !IO)
    ).

%---------------------%

:- pred maybe_write_phony_rule(io.text_output_stream::in,
    is_mmake_rule_phony::in, list(string)::in, io::di, io::uo) is det.

maybe_write_phony_rule(OutStream, IsPhony, FileNames, !IO) :-
    (
        IsPhony = mmake_rule_is_not_phony
    ;
        IsPhony = mmake_rule_is_phony,
        FileNamesStr = string.join_list(" ", FileNames),
        io.format(OutStream, ".PHONY: %s\n", [s(FileNamesStr)], !IO)
    ).

%---------------------%

:- pred write_rule_name(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

write_rule_name(OutStream, RuleName, !IO) :-
    ( if RuleName = "" then
        true
    else
        io.format(OutStream, "# RULE %s\n", [s(RuleName)], !IO)
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
:- pred write_mmake_file_name_groups_horizontal(io.text_output_stream::in,
    mmake_file_name_group::in, list(mmake_file_name_group)::in,
    io::di, io::uo) is det.

write_mmake_file_name_groups_horizontal(OutStream,
        FileNameGroup, FileNameGroups, !IO) :-
    write_mmake_file_name_group_horizontal(OutStream, FileNameGroup, !IO),
    (
        FileNameGroups = []
    ;
        FileNameGroups = [HeadFileNameGroup | TailFileNameGroups],
        io.write_string(OutStream, " ", !IO),
        write_mmake_file_name_groups_horizontal(OutStream,
            HeadFileNameGroup, TailFileNameGroups, !IO)
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
:- pred write_mmake_file_name_groups_vertical(io.text_output_stream::in,
    mmake_file_name_group::in, list(mmake_file_name_group)::in,
    io::di, io::uo) is det.

write_mmake_file_name_groups_vertical(OutStream,
        FileNameGroup, FileNameGroups, !IO) :-
    write_mmake_file_name_group_vertical(OutStream, FileNameGroup, !IO),
    (
        FileNameGroups = []
    ;
        FileNameGroups = [HeadFileNameGroup | TailFileNameGroups],
        io.write_string(OutStream, " \\\n", !IO),
        io.write_string(OutStream, "\t\\\n", !IO),
        write_mmake_file_name_groups_vertical(OutStream,
            HeadFileNameGroup, TailFileNameGroups, !IO)
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
:- pred write_mmake_file_name_group_horizontal(io.text_output_stream::in,
    mmake_file_name_group::in, io::di, io::uo) is det.

write_mmake_file_name_group_horizontal(OutStream, FileNameGroup, !IO) :-
    FileNameGroup = mmake_file_name_group(_GroupName,
        one_or_more(FileName, FileNames)),
    write_mmake_file_names_horizontal(OutStream, FileName, FileNames, !IO).

    % Write out a list of file names with each file name being written below
    % the previous one.
    %
    % The initial position of the cursor is expected to be the start of
    % the line on which the first file name is to be written.
    %
    % The final position of the cursor will be at the end of the line
    % containing the last file name written out.
    %
:- pred write_mmake_file_name_group_vertical(io.text_output_stream::in,
    mmake_file_name_group::in, io::di, io::uo) is det.

write_mmake_file_name_group_vertical(OutStream, FileNameGroup, !IO) :-
    FileNameGroup = mmake_file_name_group(_GroupName,
        one_or_more(FileName, FileNames)),
    write_mmake_file_names_vertical(OutStream, FileName, FileNames, !IO).

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
:- pred write_mmake_file_names_horizontal(io.text_output_stream::in,
    mmake_file_name::in, list(mmake_file_name)::in, io::di, io::uo) is det.

write_mmake_file_names_horizontal(OutStream, FileName, FileNames, !IO) :-
    write_mmake_file_name(OutStream, FileName, !IO),
    (
        FileNames = []
    ;
        FileNames = [HeadFileName | TailFileNames],
        io.write_string(OutStream, " ", !IO),
        write_mmake_file_names_horizontal(OutStream,
            HeadFileName, TailFileNames, !IO)
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
:- pred write_mmake_file_names_vertical(io.text_output_stream::in,
    mmake_file_name::in, list(mmake_file_name)::in, io::di, io::uo) is det.

write_mmake_file_names_vertical(OutStream, FileName, FileNames, !IO) :-
    io.write_string(OutStream, "\t\t", !IO),
    write_mmake_file_name(OutStream, FileName, !IO),
    (
        FileNames = []
    ;
        FileNames = [HeadFileName | TailFileNames],
        io.write_string(OutStream, " \\\n", !IO),
        write_mmake_file_names_vertical(OutStream,
            HeadFileName, TailFileNames, !IO)
    ).

%---------------------%

:- pred write_mmake_file_name(io.text_output_stream::in,
    mmake_file_name::in, io::di, io::uo) is det.

write_mmake_file_name(OutStream, FileName, !IO) :-
    io.write_string(OutStream, FileName, !IO).

%---------------------%

:- pred write_mmake_actions(io.text_output_stream::in,
    list(mmake_action)::in, io::di, io::uo) is det.

write_mmake_actions(_OutStream, [], !IO).
write_mmake_actions(OutStream, [Action | Actions], !IO) :-
    write_mmake_action(OutStream, Action, !IO),
    write_mmake_actions(OutStream, Actions, !IO).

:- pred write_mmake_action(io.text_output_stream::in,
    mmake_action::in, io::di, io::uo) is det.

write_mmake_action(OutStream, Action, !IO) :-
    io.write_string(OutStream, "\t", !IO),
    io.write_string(OutStream, Action, !IO),
    io.nl(OutStream, !IO).

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

make_file_name_group(GroupName, FileNames) = Groups :-
    (
        FileNames = [],
        Groups = []
    ;
        FileNames = [HeadFileName | TailFileNames],
        Groups = [mmake_file_name_group(GroupName,
            one_or_more(HeadFileName, TailFileNames))]
    ).

make_singleton_file_name_group(FileName) =
    mmake_file_name_group("", one_or_more(FileName, [])).

silent_noop_action = "@:".

%---------------------------------------------------------------------------%
:- end_module libs.mmakefiles.
%---------------------------------------------------------------------------%
