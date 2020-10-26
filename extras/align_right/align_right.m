%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This program is a filter for text streams. The only part of the text stream
% that flows through it that it changes is any sequence of lines that all
% end with the same user-specifiable pattern string, and the only change
% it makes to those lines is to put spaces before those strings as necessary
% to make all the pattern strings align in the rightmost column in which
% any of those lines end with the pattern string.
%
% This is intended to make it easier to work on multiline C macros,
% in which each line ends with a backslash. Programmers can change the code
% of the macros themselves, and then make the macro pretty by invoking this
% filter either on the whole file or just the macro. This is why the default
% string is a single backslash.
%
%---------------------------------------------------------------------------%

:- module align_right.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(short_options, long_options, option_defaults),
    getopt.process_options(OptionOps, Args, _OptionArgs, NonOptionArgs,
        OptionsResult),
    (
        OptionsResult = error(Error),
        set_exit_status(1, !IO),
        io.write_string(option_error_to_string(Error), !IO),
        io.nl(!IO)
    ;
        OptionsResult = ok(OptionTable),
        getopt.lookup_bool_option(OptionTable, help, Help),
        (
            Help = no,
            align_right(OptionTable, NonOptionArgs, !IO)
        ;
            Help = yes,
            usage(!IO)
        )
    ).

:- pred align_right(option_table(option)::in, list(string)::in,
    io::di, io::uo) is det.

align_right(OptionTable, NonOptionArgs, !IO) :-
    (
        NonOptionArgs = [],
        MaybeStream = yes(io.stdin_stream)
    ;
        NonOptionArgs = [FileName],
        io.open_input(FileName, OpenResult, !IO),
        (
            OpenResult = ok(FileStream),
            MaybeStream = yes(FileStream)
        ;
            OpenResult = error(OpenError),
            MaybeStream = no,
            set_exit_status(1, !IO),
            OpenErrorMsg = io.error_message(OpenError),
            io.write_string(OpenErrorMsg, !IO),
            io.nl(!IO)
        )
    ;
        NonOptionArgs = [_, _ | _],
        MaybeStream = no,
        set_exit_status(1, !IO),
        usage(!IO)
    ),
    (
        MaybeStream = no
    ;
        MaybeStream = yes(Stream),
        io.read_file(Stream, ReadResult, !IO),
        (
            ReadResult = ok(FileChars),
            getopt.lookup_int_option(OptionTable, tab_width, TabWidth),
            getopt.lookup_bool_option(OptionTable, squeeze, SqueezeBool),
            ( SqueezeBool = no, Squeeze = do_not_squeeze
            ; SqueezeBool = yes, Squeeze = squeeze
            ),
            getopt.lookup_string_option(OptionTable, pattern, PatternStr),
            string.to_char_list(PatternStr, PatternChars),
            list.reverse(PatternChars, RevPatternChars),
            list.length(PatternChars, PatternLen),
            getopt.lookup_int_option(OptionTable, lo_pattern_start_column,
                LoPatternStartColumn),
            getopt.lookup_int_option(OptionTable, hi_pattern_end_column,
                HiPatternEndColumn),
            LoBeforePatternColumn = LoPatternStartColumn,
            HiBeforePatternColumn = HiPatternEndColumn - PatternLen,
            Params = params(TabWidth, Squeeze,
                PatternStr, RevPatternChars, PatternLen,
                LoBeforePatternColumn, HiBeforePatternColumn),

            break_into_lines(Params, FileChars, [], RevFileLines),
            list.reverse(RevFileLines, FileLines),
            write_out_lines(Params, FileLines, !IO)
        ;
            ReadResult = error(_PartialFileChars, Error),
            set_exit_status(1, !IO),
            ErrorMsg = io.error_message(Error),
            io.write_string(ErrorMsg, !IO),
            io.nl(!IO)
        )
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.progname("align_right", ProgName, !IO),
    io.format(
        "usage: %s [-p<pattern_string>] [-lN] [-hM] [-tT] [-s] [FileName]\n",
        [s(ProgName)], !IO),
    io.write_string(string.join_list("\n", option_desc), !IO).

:- func option_desc = list(string).

option_desc = [
    "",
    "The value of the -p option specifies the string whose presence",
    "at the ends of on two or more consecutive lines asks the program",
    "to right-align the ends of those lines by adding padding before them.",
    "The default is a single backslash, to help align the ends of C macros.",
    "",
    "The value of the -h option specifies the highest column number",
    "that the program will push the end of the pattern string to.",
    "By default, there is no maximum.",
    "",
    "The value of the -l option specifies the lowest column number",
    "that the program will start the pattern string at. Even if",
    "all the lines in a block of lines that all end in the specified pattern",
    "start the pattern before this column, the program will add padding",
    "to push them out to start in this column.",
    "By default, there is no minimum.",
    "",
    "In the absence of the -s (squeeze) option, the program will never",
    "delete any spaces before the pattern string. In its presence, however,",
    "the program will replace any run of white space before the pattern",
    "string with a single space before doing its job. This means that",
    "-s allows the program to remove existing padding before the pattern",
    "string if it is more extensive than necessary.",
    "",
    "The program is designed primarily to work on text in which tabs",
    "do not occur. However, if tabs do occur in the input, the default",
    "setting is to consider tab stops to occur in every 8th column.",
    "However, the value of the -t (tabs) option, if given, overrides",
    "this setting.",
    ""
].

:- type option
    --->    tab_width
    ;       lo_pattern_start_column
    ;       hi_pattern_end_column
    ;       pattern
    ;       squeeze
    ;       help.

:- pred short_options(char::in, option::out) is semidet.

short_options('t', tab_width).
short_options('l', lo_pattern_start_column).
short_options('h', hi_pattern_end_column).
short_options('p', pattern).
short_options('s', squeeze).

:- pred long_options(string::in, option::out) is semidet.

long_options("tab_width", tab_width).
long_options("lo_pattern_start_column", lo_pattern_start_column).
long_options("hi_pattern_end_column", hi_pattern_end_column).
long_options("pattern", pattern).
long_options("squeeze", squeeze).
long_options("help", help).

:- pred option_defaults(option::out, option_data::out) is multi.

option_defaults(tab_width, int(8)).
option_defaults(lo_pattern_start_column, int(-1)).
option_defaults(hi_pattern_end_column, int(-1)).
option_defaults(pattern, string("\\")).
option_defaults(squeeze, bool(no)).
option_defaults(help, bool(no)).

:- type params
    --->    params(
                p_tab_width                 :: int,
                p_squeeze                   :: squeeze,
                p_pattern_str               :: string,
                p_rev_pattern_chars         :: list(char),
                p_pattern_len               :: int,
                p_lo_before_pattern_column  :: int,
                p_hi_before_pattern_column  :: int
            ).

:- type squeeze
    --->    do_not_squeeze
    ;       squeeze.

%---------------------------------------------------------------------------%

:- type pat_line
    --->    non_pattern_line(
                % This line DOES NOT end with the pattern.

                % The list of all the characters on the line,
                % including the final newline, if there is one.
                % (The final line of a file may or may not be
                % terminated by a newline.)
                list(char)
            )
    ;       pattern_line(
                % This line DOES end with the pattern.

                % The list of all the characters on the line,
                % EXCEPT the final pattern and newline.
                % (We expect lines that end with the pattern
                % to also have a newline, since in the use-case that
                % this tool is built for, there will always be one.)
                list(char),

                % The column in which the last character before the
                % pattern was.
                int
            ).

:- inst pat_line
    --->    pattern_line(ground, ground).

:- pred break_into_lines(params::in, list(char)::in,
    list(pat_line)::in, list(pat_line)::out) is det.

break_into_lines(_Params, [], !RevLines).
break_into_lines(Params, [Char | Chars], !RevLines) :-
    get_next_line(Params, Char, 1, Chars, [], Line, LeftOverChars),
    !:RevLines = [Line | !.RevLines],
    break_into_lines(Params, LeftOverChars, !RevLines).

:- pred get_next_line(params::in, char::in, int::in,
    list(char)::in, list(char)::in, pat_line::out, list(char)::out) is det.

get_next_line(Params, FirstChar, FirstCharColumn, LaterChars,
        !.RevCharsSoFar, Line, LeftOverChars) :-
    !:RevCharsSoFar = [FirstChar | !.RevCharsSoFar],
    ( if FirstChar = '\n' then
        make_line(Params, !.RevCharsSoFar, FirstCharColumn, Line),
        LeftOverChars = LaterChars
    else
        (
            LaterChars = [],
            % The column does not matter, since the test in make_line
            % cannot succeed without the final newline.
            make_line(Params, !.RevCharsSoFar, FirstCharColumn, Line),
            LeftOverChars = []
        ;
            LaterChars = [FirstLaterChar | LaterLaterChars],
            FirstLaterCharColumn = update_column(Params ^ p_tab_width,
                FirstCharColumn, FirstChar),
            get_next_line(Params, FirstLaterChar, FirstLaterCharColumn,
                LaterLaterChars, !.RevCharsSoFar, Line, LeftOverChars)
        )
    ).

:- func update_column(int, int, char) = int.

update_column(TabWidth, CurColumn, Char) = NextColumn :-
    ( if Char = '\t' then
        RoundUp = TabWidth * ((CurColumn + TabWidth - 1) / TabWidth),
        NextColumn = RoundUp + 1
    else
        NextColumn = CurColumn + 1
    ).

:- pred make_line(params::in, list(char)::in, int::in, pat_line::out) is det.

make_line(Params, RevCharsSoFar, NextColumn, Line) :-
    ( if
        RevCharsSoFar = ['\n' | RevBeforeNewlineChars],
        RevPattern = Params ^ p_rev_pattern_chars,
        list.append(RevPattern, RevBeforePatternChars0, RevBeforeNewlineChars)
    then
        % Subtract the length of the newline and the pattern.
        MaxColumn0 = NextColumn - 1 - Params ^ p_pattern_len,

        Squeeze = Params ^ p_squeeze,
        (
            Squeeze = do_not_squeeze,
            RevBeforePatternChars = RevBeforePatternChars0,
            MaxColumn = MaxColumn0
        ;
            Squeeze = squeeze,
            squeeze(RevBeforePatternChars0, RevBeforePatternChars,
                MaxColumn0, MaxColumn)
        ),
        list.reverse(RevBeforePatternChars, BeforePatternChars),
        Line = pattern_line(BeforePatternChars, MaxColumn)
    else
        list.reverse(RevCharsSoFar, LineChars),
        Line = non_pattern_line(LineChars)
    ).

    % If the reversed line ends with more than one space character,
    % squeeze out all but one of them.
    %
:- pred squeeze(list(char)::in, list(char)::out, int::in, int::out) is det.

squeeze(Chars, SqueezedChars, !MaxColumn) :-
    ( if Chars = [' ', ' ' | MoreChars] then
        !:MaxColumn = !.MaxColumn - 1,
        squeeze([' ' | MoreChars], SqueezedChars, !MaxColumn)
    else
        SqueezedChars = Chars
    ).

%---------------------------------------------------------------------------%

:- pred write_out_lines(params::in, list(pat_line)::in, io::di, io::uo) is det.

write_out_lines(_Params, [], !IO).
write_out_lines(Params, [Line | Lines], !IO) :-
    (
        Line = non_pattern_line(LineChars),
        write_out_chars(LineChars, !IO),
        write_out_lines(Params, Lines, !IO)
    ;
        Line = pattern_line(_, Column),
        get_pattern_block(Lines, [Line], RevBlockLines, Column, MaxColumn0,
            LeftOverLines),
        LoBeforePatternColumn = Params ^ p_lo_before_pattern_column,
        HiBeforePatternColumn = Params ^ p_hi_before_pattern_column,
        int.max(MaxColumn0, LoBeforePatternColumn, MaxColumn1),
        ( if HiBeforePatternColumn < 0 then
            MaxColumn = MaxColumn1
        else
            int.min(MaxColumn1, HiBeforePatternColumn, MaxColumn)
        ),
        BlockLines = list.inst_preserving_reverse(RevBlockLines),
        write_out_pattern_lines(Params, BlockLines, MaxColumn, !IO),
        write_out_lines(Params, LeftOverLines, !IO)
    ).

:- pred get_pattern_block(list(pat_line)::in,
    list(pat_line)::in(list_skel(pat_line)),
    list(pat_line)::out(list_skel(pat_line)),
    int::in, int::out, list(pat_line)::out) is det.

get_pattern_block([], !RevBlockLines, !MaxColumn, []).
get_pattern_block([Line | Lines], !RevBlockLines, !MaxColumn,
        LeftOverLines) :-
    (
        Line = non_pattern_line(_),
        LeftOverLines = [Line | Lines]
    ;
        Line = pattern_line(_, LineColumn),
        !:RevBlockLines = [Line | !.RevBlockLines],
        !:MaxColumn = int.max(!.MaxColumn, LineColumn),
        get_pattern_block(Lines, !RevBlockLines, !MaxColumn, LeftOverLines)
    ).

:- pred write_out_pattern_lines(params::in,
    list(pat_line)::in(list_skel(pat_line)), int::in, io::di, io::uo) is det.

write_out_pattern_lines(_Params, [], _, !IO).
write_out_pattern_lines(Params, [FirstLine | LaterLine], MaxColumn, !IO) :-
    trace [compile_time(flag("dump_block_lines")), io(!TIO)] (
        io.write(FirstLine, !TIO),
        io.write_char('\n', !TIO)
    ),
    FirstLine = pattern_line(LineCharsBeforePattern, LineColumn),
    write_out_chars(LineCharsBeforePattern, !IO),
    write_out_n_spaces(MaxColumn - LineColumn, !IO),
    io.write_string(Params ^ p_pattern_str, !IO),
    io.write_char('\n', !IO),
    write_out_pattern_lines(Params, LaterLine, MaxColumn, !IO).

:- pred write_out_chars(list(char)::in, io::di, io::uo) is det.

write_out_chars([], !IO).
write_out_chars([Char | Chars], !IO) :-
    io.write_char(Char, !IO),
    write_out_chars(Chars, !IO).

:- pred write_out_n_spaces(int::in, io::di, io::uo) is det.

write_out_n_spaces(N, !IO) :-
    ( if N =< 0 then
        true
    else
        io.write_char(' ', !IO),
        write_out_n_spaces(N - 1, !IO)
    ).

%---------------------------------------------------------------------------%
