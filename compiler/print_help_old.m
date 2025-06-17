%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: print_help_old.m.
% Main author: zs.

:- module libs.print_help_old.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.

:- type help_section
    --->    help_section(
                hs_section_name             :: string,
                hs_section_comment_lines    :: list(string),
                hs_help_structs             :: list(help)
            )
    ;       unnamed_help_section(
                % If a section has no name, it shouldn't have any
                % comment lines either.
                uhs_help_structs             :: list(help)
            ).

:- type maybe_nested_help_section
    --->    std_help_section(help_section)
    ;       nested_help_section(
                nhs_overall_name            :: string,
                nhs_overall_comment_lines   :: list(string),
                nhs_subsections             :: list(help_section)
            ).

% This structured representation improves on our traditional approach
% of storing just lists of lines in the following ways.
%
% - It does not force us to start every line documenting an option
%   with "\tactual help text".
%
% - It does not require global changes to change indentation levels.
%
% - It allows an optional check, probably inside a trace scope, that tests
%   each first and each later line whether they fit on a standard
%   80 column line, and adds an easily greppable marker if any
%   exceeds that limit.
%
% - It allows taking a line width parameter, and reflowing the later lines
%   to respect that limit.
%
% - It allows adding a version of output_help_messages that outputs
%   the help message not to be read by a compiler user, but with
%   texinfo markup, intended to be copy-and-pasted into doc/user_guide.texi.
%
% - Once all help text is in these structures, and we have a single data
%   structure that holds all those structures, we can add an option that
%   asks for a bijection check, which tests
%
%   - whether all option names mentioned as keys in the short_option and
%     long_option predicates appear in exactly one help structure, and
%
%   - whether all short and long option names mentioned in the set of all
%     help structures have a match among the keys of the short_option and
%     long_option predicates.
%
%   We could even check whether any long option whose documentation has
%   a "no-" prefix is a negatable long option.
%

:- type help
    --->    no_help
    ;       gen_help(
                % The name of the option, minus the "--" prefix, but
                % including any "no-" prefix. If the option takes an argument,
                % then the option name should be followed by " <argdesc>".
                %
                % The reason for including the "no-" prefix is that
                % the description string has to be written differently
                % depending on whether the option is default-on or default-off,
                % and so requiring the documentation writer to add the prefix
                % is a trivially small extra burden.
                %
                % The reason for including the " <argdesc>" suffix if the
                % option has an argument is that for accumulating options,
                % only the positive version takes an argument, but we must
                % document the negative version as well. This makes we cannot
                % add the argdesc suffix to all alternate names willy-nilly,
                % and any automatic addition that is smart enough to always
                % do the right thing would come with an unjustifiable amount
                % of complexity.
                %
                % (Both "no-" prefixes and " <...>" suffixes will need to be
                % be stripped off for any bijection check.)
                gh_long_name            :: string,

                % Should we try to print all the names on one line, or not?
                gh_alt_name_pos         :: alt_name_pos,

                % Any alternate names of the option.
                % We have many options that have both British and American
                % spelling of the option name, and some have both
                % short and long versions.
                % XXX We should decide on a consistent order:
                % - should British or American spelling come first?
                % - should we go from long versions to short, or vice versa?
                %
                % The order of these alt options here will be preserved
                % in the output. They will all follow gh_long_name.
                gh_alt_long_names       :: list(string),

                % Every character in this field is a short name of the option.
                % The order of these short options here will be preserved
                % in the output.
                %
                % We use a char because all options using gen_help
                % have short names that have no argument.
                % (Some options using the other function symbols below
                % *do* have short names that take arguments, and for these,
                % we use strings, the first character of which is the short
                % name, and the rest is the argument description suffix.
                gh_short_names          :: list(char),

                % Is the option's documentation printed for users, or not?
                gh_public_or_private    :: help_public_or_private,

                % The lines describing the effect of the option.
                gh_description          :: list(string)
            )
    % The following function symbols (whose list may grow)
    % all have a subset of the fields of gen_help.
    %
    % The fields they contain have the same semantics as in gen_help.
    % The fields they do not contain implicitly default to "[]" for
    % alternate names, and will get their public vs private status
    % from the function symbol name.
    %
    % This design minimizes clutter in lists of help structures.
    ;       help(
                h_long_name             :: string,
                h_description           :: list(string)
            )
    ;       priv_help(
                ph_long_name            :: string,
                ph_description          :: list(string)
            )
    ;       alt_help(
                ah_long_name            :: string,
                ah_alt_name_pos         :: alt_name_pos,
                ah_alt_long_names       :: list(string),
                ah_description          :: list(string)
            )
    ;       priv_alt_help(
                pah_long_name           :: string,
                pah_alt_name_pos        :: alt_name_pos,
                pah_alt_long_names      :: list(string),
                pah_description         :: list(string)
            )
    ;       short_help(
                sh_short_name           :: char,
                sh_long_name            :: string,
                sh_alt_long_names       :: list(string),
                sh_description          :: list(string)
            )
    ;       priv_short_help(
                psh_short_name          :: char,
                psh_long_name           :: string,
                psh_alt_long_names      :: list(string),
                psh_description         :: list(string)
            )
    ;       short_arg_help(
                sah_short_name          :: string,
                sah_long_name           :: string,
                sah_alt_long_names      :: list(string),
                sah_description         :: list(string)
            )
    ;       priv_short_arg_help(
                psah_short_name         :: string,
                psah_long_name          :: string,
                psah_alt_long_names     :: list(string),
                psah_description        :: list(string)
            )
    ;       unnamed_help(
                % Help for an internal-use-only option that has no visible
                % name, and therefore cannot be given on the command line
                % even by developers.
                uh_description          :: list(string)
            ).

:- type help_public_or_private
    --->    help_public
    ;       help_private.

:- type alt_name_pos
    --->    pos_one_line
    ;       pos_sep_lines.

:- type print_what_help
    --->    print_public_help
    ;       print_public_and_private_help.

:- pred output_maybe_nested_help_section(io.text_output_stream::in,
    print_what_help::in, maybe_nested_help_section::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module maybe.
:- import_module string.

output_maybe_nested_help_section(Stream, What, MaybeNestedSection, !IO) :-
    (
        MaybeNestedSection = std_help_section(Section),
        output_help_section(Stream, What, "", Section, !IO)
    ;
        MaybeNestedSection = nested_help_section(OverallName,
            OverallCommentLines, Subsections),
        % The original code used different indentation
        % from what we generate below.
        %
        % We do NOT put a newline after the OverallName + OverallCommentLines
        % combo, because each of the Subsections will *start* by printing
        % a newline, and we do not want to end up with two.
        io.format(Stream, "\n%s:\n", [s(OverallName)], !IO),
        (
            OverallCommentLines = []
        ;
            OverallCommentLines = [_ | _],
            io.nl(Stream, !IO),
            io.write_prefixed_lines(Stream, single_indent,
                OverallCommentLines, !IO)
        ),
        list.foldl(output_help_section(Stream, What, single_indent),
            Subsections, !IO)
    ).

:- pred output_help_section(io.text_output_stream::in, print_what_help::in,
    string::in, help_section::in, io::di, io::uo) is det.

output_help_section(Stream, What, SectionNameIndent, Section, !IO) :-
    (
        Section = help_section(SectionName0, SectionCommentLines, HelpStructs),
        MaybeSectionName = yes(SectionName0)
    ;
        Section = unnamed_help_section(HelpStructs),
        MaybeSectionName = no,
        SectionCommentLines = []
    ),
    % Both with a section and without it, separate this section
    % from what came before.
    (
        MaybeSectionName = no,
        io.nl(Stream, !IO)
    ;
        MaybeSectionName = yes(SectionName),
        io.format(Stream, "\n%s%s:\n\n",
            [s(SectionNameIndent), s(SectionName)], !IO)
    ),
    (
        SectionCommentLines = []
    ;
        SectionCommentLines = [_ | _],
        io.write_prefixed_lines(Stream, single_indent,
            SectionCommentLines, !IO),
        io.nl(Stream, !IO)
    ),
    output_help_messages(Stream, What, HelpStructs, !IO).

:- pred output_help_messages(io.text_output_stream::in, print_what_help::in,
    list(help)::in, io::di, io::uo) is det.

output_help_messages(_Stream, _What, [], !IO).
output_help_messages(Stream, What, [OptHelp | OptHelps], !IO) :-
    output_help_message(Stream, What, OptHelp, !IO),
    output_help_messages(Stream, What, OptHelps, !IO).

:- pred output_help_message(io.text_output_stream::in, print_what_help::in,
    help::in, io::di, io::uo) is det.

output_help_message(Stream, What, OptHelp, !IO) :-
    % XXX We could automatically add "(This option is not for general use.)"
    % to the start of the description of every private option, to save
    % the repetition of including it in help_private structures.
    %
    % We currently handle this message quite badly. First, we do not include it
    % in many help_private structures (which, to be fair, won't matter
    % until we implement callers that specify print_public_and_private_help.
    % Second, we *do* include it in a few help_public structures, in which
    % cases it gives non-developer readers useless information. To make
    % the message useful, the message would have to say *in what situations*
    % the option may be relevant to non-developers.
    OptNameLineMaxLen = 71,
    (
        OptHelp = no_help,
        PublicOrPrivate = help_private,
        OptNameLines = [],
        DescLines = []
    ;
        OptHelp = gen_help(LongName, AltNamePos, AltLongNames, ShortNames,
            PublicOrPrivate, DescLines),
        LongNames = [LongName | AltLongNames],
        ShortNameStrs = list.map(short_name_to_str, ShortNames),
        LongNameStrs = list.map(long_name_to_str, LongNames),
        OptNameLines = join_options(AltNamePos, OptNameLineMaxLen,
            ShortNameStrs ++ LongNameStrs)
    ;
        (
            OptHelp = help(LongName, DescLines),
            PublicOrPrivate = help_public
        ;
            OptHelp = priv_help(LongName, DescLines),
            PublicOrPrivate = help_private
        ),
        OptNameLines = ["--" ++ LongName]
    ;
        (
            OptHelp = alt_help(LongName, AltNamePos, AltLongNames, DescLines),
            PublicOrPrivate = help_public
        ;
            OptHelp = priv_alt_help(LongName, AltNamePos, AltLongNames,
                DescLines),
            PublicOrPrivate = help_private
        ),
        LongNames = [LongName | AltLongNames],
        LongNameStrs = list.map(long_name_to_str, LongNames),
        OptNameLines =
            join_options(AltNamePos, OptNameLineMaxLen, LongNameStrs)
    ;
        (
            OptHelp = short_help(ShortName, LongName, AltLongNames,
                DescLines),
            ShortNameStr = short_name_to_str(ShortName),
            PublicOrPrivate = help_public
        ;
            OptHelp = short_arg_help(ShortName, LongName, AltLongNames,
                DescLines),
            ShortNameStr = short_name_with_arg_to_str(ShortName),
            PublicOrPrivate = help_public
        ;
            OptHelp = priv_short_help(ShortName, LongName, AltLongNames,
                DescLines),
            ShortNameStr = short_name_to_str(ShortName),
            PublicOrPrivate = help_private
        ;
            OptHelp = priv_short_arg_help(ShortName, LongName, AltLongNames,
                DescLines),
            ShortNameStr = short_name_with_arg_to_str(ShortName),
            PublicOrPrivate = help_private
        ),
        LongNameStr = long_name_to_str(LongName),
        AltLongNameStrs = list.map(long_name_to_str, AltLongNames),
        % We could use pos_sep_lines, but we should use one setting
        % consistently for all <one short, one long> name options.
        OptNameLines0 = join_options(pos_one_line, OptNameLineMaxLen,
            [ShortNameStr, LongNameStr]),
        OptNameLines = OptNameLines0 ++ AltLongNameStrs
    ;
        OptHelp = unnamed_help(DescLines),
        % XXX It is quite likely that many options that do not have entries
        % in the long_table predicate, which therefore should be in optdb
        % with unnamed_help, are there with some other help structure,
        % such as priv_help.
        PublicOrPrivate = help_private,
        OptNameLines = []
    ),
    ( if
        (
            PublicOrPrivate = help_public
        ;
            PublicOrPrivate = help_private,
            What = print_public_and_private_help
        )
    then
        OptNamePrefix = single_indent,
        DescPrefix = double_indent,
        io.write_prefixed_lines(Stream, OptNamePrefix, OptNameLines, !IO),
        io.write_prefixed_lines(Stream, DescPrefix, DescLines, !IO)
    else
        true
    ).

:- func long_name_to_str(string) = string.

long_name_to_str(LongName) =
    string.format("--%s", [s(LongName)]).

:- func short_name_to_str(char) = string.

short_name_to_str(ShortName) =
    string.format("-%c", [c(ShortName)]).

:- func short_name_with_arg_to_str(string) = string.

short_name_with_arg_to_str(ShortName) =
    string.format("-%s", [s(ShortName)]).

    % join_options(AltNamePos, OptNameLineMaxLen, OptionStrs) = Lines :-
    % Given a list of short and/or long option strings, of the form
    % -X or --Y, return one or more lines containing those option strings.
    % If AltNamePos = pos_one_line, *and* all option strings fit together
    % on one when separated by commas, we will return that line.
    % Otherwise we will return one line for each option string,
    % whether short or long.
    %
    % For lists of long option names that "rhyme", i.e. they have
    % common prefixes followed by different suffixes, pos_sep_lines
    % will make that fact pop out at the reader; pos_one_line will not.
    % This makes pos_sep_lines preferable in that case.
    %
:- func join_options(alt_name_pos, int, list(string)) = list(string).

join_options(AltNamePos, OptNameLineMaxLen, OptionStrs) = Lines :-
    OneLine = string.join_list(", ", OptionStrs),
    ( if
        (
            AltNamePos = pos_one_line,
            string.count_code_points(OneLine) > OptNameLineMaxLen
        ;
            AltNamePos = pos_sep_lines
        )
    then
        Lines = OptionStrs
    else
        Lines = [OneLine]
    ).

:- func single_indent = string.
:- func double_indent = string.

% Until all options are documented using help structures,
% maintain the existing indentation.
% single_indent = "    ".
% double_indent = "        ".
single_indent = "\t".
double_indent = "\t\t".

%---------------------------------------------------------------------------%
:- end_module libs.print_help_old.
%---------------------------------------------------------------------------%
