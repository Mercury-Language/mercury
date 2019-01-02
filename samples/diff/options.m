%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1998, 2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: bromage
%
% This module contains all the option handling code for diff.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module options.
:- interface.

:- import_module getopt.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred get_option_ops(option_ops(option)::out(option_ops)) is det.

    % Process the option table, perhaps returning an error message
    % if there was some inconsistency or other error.
    %
:- pred postprocess_options(maybe_option_table::in, maybe(string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Info on the accepted options, displayed in response to --help.
    %
:- pred options_help(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type option_table == option_table(option).
:- type maybe_option_table == maybe_option_table(option).

    % The master list of options.
    %
:- type option
    % Output styles
    --->    help
    ;       version
    ;       context
    ;       context_style_output
    ;       unified
    ;       unified_style_output
    ;       ed
    ;       forward_ed
    ;       rcs
    ;       brief
    ;       ifdef
    ;       side_by_side
    ;       cvs_merge_conflict

    % Output options
    ;       show_c_function     % Not handled (and unlikely to be soon)
    ;       show_function_line  % Not handled (and unlikely to be soon)
    ;       label
    ;       width
    ;       expand_tabs
    ;       initial_tab
    ;       paginate            % Not handled (and unlikely to be soon)
    ;       left_column
    ;       suppress_common_lines
    ;       sdiff_merge_assist  % Accepted but ignored

    % Matching options
    ;       new_file                % Not handled (and unlikely to be soon)
    ;       unidirectional_new_file % Not handled (and unlikely to be soon)
    ;       ignore_case
    ;       ignore_all_space
    ;       ignore_space_change

    % Diff options
    ;       minimal
    ;       speed_large_files       % Accepted but ignored
    ;       file_split_speed_hack   % Accepted but ignored (GNU diff
                                    % ignores this too, so let's not
                                    % feel too bad about it)

    % Change filter options
    ;       ignore_matching_lines   % Not handled (and unlikely to be soon)
    ;       ignore_blank_lines

    % Directory comparison options
    % None of these are likely to be handled in the near future.
    ;       starting_file           % Not handled
    ;       recursive               % Not handled
    ;       report_identical_files  % Not handled
    ;       exclude                 % Not handled
    ;       exclude_from            % Not handled

    % #ifdef format options
    % None of these are likely to be handled in the very near future.
    ;       old_line_format         % Not handled
    ;       new_line_format         % Not handled
    ;       unchanged_line_format   % Not handled
    ;       line_format             % Not handled
    ;       old_group_format        % Not handled
    ;       new_group_format        % Not handled
    ;       unchanged_group_format  % Not handled
    ;       changed_group_format    % Not handled
    ;       horizon_lines           % Not handled

    % File input options
    % Neither of these are likely to be handled in the near future.
    ;       text                    % Not handled
    ;       binary.                 % Not handled

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module diff_out.
:- import_module globals.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%

:- pred long_option(string::in, option::out) is semidet.

long_option("ignore-blank-lines",       ignore_blank_lines).
long_option("context",                  context).
long_option("ifdef",                    ifdef).
long_option("show-function-line",       show_function_line).
long_option("speed-large-files",        speed_large_files).
long_option("ignore-matching-lines",    ignore_matching_lines).
long_option("file-label",               label).
long_option("label",                    label).
long_option("new-file",                 new_file).
long_option("entire-new-file",          new_file).
long_option("unidirectional-new-file",  unidirectional_new_file).
long_option("starting-file",            starting_file).
long_option("initial-tab",              initial_tab).
long_option("unified",                  unified).
long_option("width",                    width).
long_option("exclude-from",             exclude_from).
long_option("text",                     text).
long_option("ascii",                    text).
long_option("ignore-space-change",      ignore_space_change).
long_option("minimal",                  minimal).
long_option("ed",                       ed).
long_option("forward-ed",               forward_ed).
long_option("ignore-case",              ignore_case).
long_option("paginate",                 paginate).
long_option("print",                    paginate).
long_option("rcs",                      rcs).
long_option("show-c-function",          show_c_function).
long_option("brief",                    brief).
long_option("recursive",                recursive).
long_option("report-identical-files",   report_identical_files).
long_option("expand-tabs",              expand_tabs).
long_option("version",                  version).
long_option("ignore-all-space",         ignore_all_space).
long_option("exclude",                  exclude).
long_option("side-by-side",             side_by_side).
long_option("cvs-merge-conflict",       cvs_merge_conflict).
long_option("left-column",              left_column).
long_option("suppress-common-lines",    suppress_common_lines).
long_option("sdiff-merge-assist",       sdiff_merge_assist).
long_option("old-line-format",          old_line_format).
long_option("new-line-format",          new_line_format).
long_option("unchanged-line-format",    unchanged_line_format).
long_option("line-format",              line_format).
long_option("old-group-format",         old_group_format).
long_option("new-group-format",         new_group_format).
long_option("unchanged-group-format",   unchanged_group_format).
long_option("changed-group-format",     changed_group_format).
long_option("horizon-lines",            horizon_lines).
long_option("help",                     help).
long_option("binary",                   binary).

%-----------------------------------------------------------------------------%

:- pred short_option(character::in, option::out) is semidet.

short_option('B', ignore_blank_lines).
short_option('C', context).
short_option('D', ifdef).
short_option('F', show_function_line).
short_option('H', speed_large_files).
short_option('I', ignore_matching_lines).
short_option('L', label).
short_option('N', new_file).
short_option('P', unidirectional_new_file).
short_option('S', starting_file).
short_option('T', initial_tab).
short_option('U', unified).
short_option('W', width).
short_option('X', exclude_from).
short_option('a', text).
short_option('b', ignore_space_change).
short_option('c', context_style_output).
short_option('d', minimal).
short_option('e', ed).
short_option('f', forward_ed).
short_option('h', file_split_speed_hack).
short_option('i', ignore_case).
short_option('l', paginate).
short_option('n', rcs).
short_option('p', show_c_function).
short_option('q', brief).
short_option('r', recursive).
short_option('s', report_identical_files).
short_option('t', expand_tabs).
short_option('u', unified_style_output).
short_option('v', version).
short_option('w', ignore_all_space).
short_option('x', exclude).
short_option('y', side_by_side).

%-----------------------------------------------------------------------------%

:- pred option_defaults(option::out, option_data::out) is multi.

    % Output styles
option_defaults(help,                   bool(no)).
option_defaults(version,                bool(no)).
option_defaults(context,                maybe_int(no)).
option_defaults(context_style_output,   bool_special).
option_defaults(unified,                maybe_int(no)).
option_defaults(unified_style_output,   bool_special).
option_defaults(ed,                     bool(no)).
option_defaults(forward_ed,             bool(no)).
option_defaults(rcs,                    bool(no)).
option_defaults(brief,                  bool(no)).
option_defaults(ifdef,                  maybe_string(no)).
option_defaults(side_by_side,           bool(no)).
option_defaults(cvs_merge_conflict,     bool(no)).

    % Output options
option_defaults(show_c_function,        bool_special).
option_defaults(show_function_line,     string_special).
option_defaults(label,                  accumulating([])).
option_defaults(width,                  int(130)).
option_defaults(expand_tabs,            bool(no)).
option_defaults(initial_tab,            bool(no)).
option_defaults(paginate,               bool_special).
option_defaults(left_column,            bool(no)).
option_defaults(suppress_common_lines,  bool(no)).
option_defaults(sdiff_merge_assist,     bool(no)).

    % Matching options
option_defaults(new_file,                   bool_special).
option_defaults(unidirectional_new_file,    bool_special).
option_defaults(ignore_case,                bool(no)).
option_defaults(ignore_all_space,           bool(no)).
option_defaults(ignore_space_change,        bool(no)).

    % Diff options
option_defaults(minimal,                bool(no)).
option_defaults(speed_large_files,      bool(no)).
option_defaults(file_split_speed_hack,  bool(no)).

    % Change filter options
option_defaults(ignore_matching_lines,  string_special).
option_defaults(ignore_blank_lines,     bool(no)).

    % Directory comparison options (none of these are handled)
option_defaults(starting_file,          string_special).
option_defaults(recursive,              bool_special).
option_defaults(report_identical_files, bool_special).
option_defaults(exclude,                string_special).
option_defaults(exclude_from,           string_special).

    % Format options (none of these are handled)
option_defaults(old_line_format,        string_special).
option_defaults(new_line_format,        string_special).
option_defaults(unchanged_line_format,  string_special).
option_defaults(line_format,            string_special).
option_defaults(old_group_format,       string_special).
option_defaults(new_group_format,       string_special).
option_defaults(unchanged_group_format, string_special).
option_defaults(changed_group_format,   string_special).
option_defaults(horizon_lines,          int_special).

    % File input options (none of these are handled)
option_defaults(text,               bool_special).
option_defaults(binary,             bool_special).

%-----------------------------------------------------------------------------%

:- pred special_handler(option :: in, special_data :: in, option_table :: in,
        maybe_option_table :: out) is semidet.

special_handler(context_style_output, bool(yes), !.Options, ok(!:Options)) :-
    map.lookup(!.Options, context, maybe_int(Context0)),
    (
        Context0 = no,
        Context = yes(3)
    ;
        Context0 = yes(C),
        Context = yes(C)
    ),
    map.set(context, maybe_int(Context), !Options).
special_handler(unified_style_output, bool(yes), !.Options, ok(!:Options)) :-
    map.lookup(!.Options, unified, maybe_int(Unified0)),
    (
        Unified0 = no,
        Unified = yes(3)
    ;
        Unified0 = yes(C),
        Unified = yes(C)
    ),
    map.set(unified, maybe_int(Unified), !Options).

    % Special handlers for unhandled options.
    %
special_handler(show_c_function, _, _, error(Msg)) :-
    Msg = "Option not handled: --show-c-function".
special_handler(show_function_line, _, _, error(Msg)) :-
    Msg = "Option not handled: --show-function-line".
special_handler(paginate, _, _, error(Msg)) :-
    Msg = "Option not handled: --paginate".
special_handler(sdiff_merge_assist, _, _, error(Msg)) :-
    Msg = "Option not handled: --sdiff-merge-assist".
special_handler(new_file, _, _, error(Msg)) :-
    Msg = "Option not handled: --new-file".
special_handler(unidirectional_new_file, _, _, error(Msg)) :-
    Msg = "Option not handled: --unidirectional-new-file".
special_handler(speed_large_files, _, _, error(Msg)) :-
    Msg = "Option not handled: --speed-large-files".
special_handler(ignore_matching_lines, _, _, error(Msg)) :-
    Msg = "Option not handled: --ignore-matching-lines".
special_handler(ignore_blank_lines, _, _, error(Msg)) :-
    Msg = "Option not handled: --ignore-blank-lines".
special_handler(starting_file, _, _, error(Msg)) :-
    Msg = "Option not handled: --starting-file".
special_handler(recursive, _, _, error(Msg)) :-
    Msg = "Option not handled: --recursive".
special_handler(report_identical_files, _, _, error(Msg)) :-
    Msg = "Option not handled: --report-identical-files".
special_handler(exclude, _, _, error(Msg)) :-
    Msg = "Option not handled: --exclude".
special_handler(exclude_from, _, _, error(Msg)) :-
    Msg = "Option not handled: --exclude-from".
special_handler(old_line_format, _, _, error(Msg)) :-
    Msg = "Option not handled: --old-line-format".
special_handler(new_line_format, _, _, error(Msg)) :-
    Msg = "Option not handled: --new-line-format".
special_handler(unchanged_line_format, _, _, error(Msg)) :-
    Msg = "Option not handled: --unchangedline-format".
special_handler(line_format, _, _, error(Msg)) :-
    Msg = "Option not handled: --line-format".
special_handler(old_group_format, _, _, error(Msg)) :-
    Msg = "Option not handled: --old-group-format".
special_handler(new_group_format, _, _, error(Msg)) :-
    Msg = "Option not handled: --new-group-format".
special_handler(unchanged_group_format, _, _, error(Msg)) :-
    Msg = "Option not handled: --unchanged-group-format".
special_handler(changed_group_format, _, _, error(Msg)) :-
    Msg = "Option not handled: --changed-group-format".
special_handler(horizon_lines, _, _, error(Msg)) :-
    Msg = "Option not handled: --horizon-lines".
special_handler(text, _, _, error(Msg)) :-
    Msg = "Option not handled: --text".
special_handler(binary, _, _, error(Msg)) :-
    Msg = "Option not handled: --binary".

%-----------------------------------------------------------------------------%

get_option_ops(OptionOps) :-
    OptionOps = option_ops_multi(
        short_option,
        long_option,
        option_defaults,
        special_handler
    ).

%-----------------------------------------------------------------------------%

    % Postprocess the options.
    %
postprocess_options(ok(OptionTable0), Result, !IO) :-
    ( if postprocess_output_style(OptionTable0, OutputStyle) then
        globals.io_init(OptionTable0, !IO),
        globals.io_set_output_style(OutputStyle, !IO),
        Result = no
    else
        Result = yes("Can't set more than one output style.")
    ).
postprocess_options(error(Msg), yes(Msg), !IO).

    % Determine which output style to use from the provided options.
    %
:- pred postprocess_output_style(option_table::in, output_style::out)
    is semidet.

postprocess_output_style(OptionTable, Style) :-
    ( if
        map.search(OptionTable, help, bool(UseHelp)),
        map.search(OptionTable, version, bool(UseVersion)),
        map.search(OptionTable, context, maybe_int(UseContext)),
        map.search(OptionTable, unified, maybe_int(UseUnified)),
        map.search(OptionTable, ed, bool(UseEd)),
        map.search(OptionTable, forward_ed, bool(UseForwardEd)),
        map.search(OptionTable, rcs, bool(UseRCS)),
        map.search(OptionTable, brief, bool(UseBrief)),
        map.search(OptionTable, ifdef, maybe_string(UseIfdef)),
        map.search(OptionTable, side_by_side, bool(UseSideBySide)),
        map.search(OptionTable, cvs_merge_conflict, bool(CVS))
    then
        postprocess_output_style_2(UseHelp, UseVersion, UseContext,
            UseUnified, UseEd, UseForwardEd, UseRCS, UseBrief,
            UseIfdef, UseSideBySide, CVS, Style)
    else
        error("postprocess_output_style")
    ).

:- pred postprocess_output_style_2(bool::in, bool::in, maybe(int)::in,
    maybe(int)::in, bool::in, bool::in, bool::in, bool::in,
    maybe(string)::in, bool::in, bool::in, output_style::out) is semidet.

postprocess_output_style_2(no, no, no, no, no, no, no, no, no, no, no,
                    normal).
postprocess_output_style_2(yes, no, no, no, no, no, no, no, no, no, no,
                    help_only).
postprocess_output_style_2(no, yes, no, no, no, no, no, no, no, no, no,
                    version_only).
postprocess_output_style_2(no, no, yes(C), no, no, no, no, no, no, no, no,
                    context(C)).
postprocess_output_style_2(no, no, no, yes(U), no, no, no, no, no, no, no,
                    unified(U)).
postprocess_output_style_2(no, no, no, no, yes, no, no, no, no, no, no,
                    ed).
postprocess_output_style_2(no, no, no, no, no, yes, no, no, no, no, no,
                    forward_ed).
postprocess_output_style_2(no, no, no, no, no, no, yes, no, no, no, no,
                    rcs).
postprocess_output_style_2(no, no, no, no, no, no, no, yes, no, no, no,
                    brief).
postprocess_output_style_2(no, no, no, no, no, no, no, no, yes(Sym), no, no,
                    ifdef(Sym)).
postprocess_output_style_2(no, no, no, no, no, no, no, no, no, yes, no,
                    side_by_side).
postprocess_output_style_2(no, no, no, no, no, no, no, no, no, no, yes,
                    cvs_merge_conflict).

%-----------------------------------------------------------------------------%

options_help(!IO) :-
    io.write_strings([
        "Output styles:\n",
        "\t--help\n",
        "\t\tOutput this help.\n",
        "\t-v, --version\n",
        "\t\tOutput version info.\n",
        "\t-c, -C <num>, --context <num>\n",
        "\t\tOutput <num> (default 2) lines of copied context.\n",
        "\t-u, -U <num>, --unified <num>\n",
        "\t\tOutput <num> (default 2) lines of unified context.\n",
        "\t\t-L <label>, --label <label>  Use <label> instead of file name.\n",
        "\t-e, --ed\n",
        "\t\tOutput an ed script.\n",
        "\t-f, --forward-ed\n",
        "\t\tProduce output similar to --ed, not useful to ed(1),\n",
        "\t\tand in the opposite order.\n",
        "\t-n, --rcs\n",
        "\t\tOutput an RCS format diff.\n",
        "\t-q, --brief\n",
        "\t\tOutput only whether or not files differ.\n",
        "\t-D <name>, --ifdef <name>\n",
        "\t\tProduce output in #ifdef <name> format.\n",
        "\t-y, --side-by-side\n",
        "\t\tProduce output in side-by-side format.\n",
        "\t\t-w <num>, --width <num>  Output at most <num> (default 130)\n",
        "\t\t\tcharacters per line.\n",
        "\t\t--left-column  Output only the left column of common lines.\n",
        "\t\t--suppress-common-lines  Do not output common lines.\n",
        "\nMatching options:\n",
        "\t-d, --minimal\n",
        "\t\tTry hard to find as small a set of changes as possible.\n",
        "\t-B, --ignore-blank-lines\n",
        "\t\tIgnore changes whose lines are all blank.\n"
    ], !IO).

%-----------------------------------------------------------------------------%
:- end_module options.
%-----------------------------------------------------------------------------%
