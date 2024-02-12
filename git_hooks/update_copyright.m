%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2024 YesLogic Pty. Ltd.
% Copyright (C) 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This program updates the first matching Copyright line of each file to
% include the current year. If one or more file names are specified on the
% command line, then those files will be updated in-place (files already
% containing up-to-date Copyright lines will not be modified).
% If no file names are specified, the input will be read from standard input,
% and the output written to standard output.
%
% Usage: update_copyright [OPTIONS] [FILES]...
%
% Options:
%
%   -s, --suffix STR    Match only Copyright lines with STR in the suffix.
%
% Exit status:
%
%   0   No files modified.
%   1   One or more files modified.
%   2   An error occurred.
%
%---------------------------------------------------------------------------%

:- module update_copyright.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module time.

:- type year_range
    --->    years(int, int).    % can be equal

:- type option
    --->    suffix
    ;       dummy.

:- type mod_state
    --->    unmodified          % no copyright line found yet
    ;       found_unmodified    % found copyright, already up-to-date
    ;       found_modified.     % found copyright, updated

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    getopt.process_options(OptionOps, Args, NonOptionArgs, OptionResult),
    (
        OptionResult = ok(OptionTable),
        current_year(Year, !IO),
        getopt.lookup_maybe_string_option(OptionTable, suffix,
            MaybeExpectSuffix),
        (
            NonOptionArgs = [],
            process_stdin(Year, MaybeExpectSuffix, !IO)
        ;
            NonOptionArgs = [_ | _],
            process_files(Year, MaybeExpectSuffix, NonOptionArgs, !IO)
        )
    ;
        OptionResult = error(Error),
        report_error_message(option_error_to_string(Error), !IO)
    ).

%---------------------------------------------------------------------------%

:- pred short_option(char::in, option::out) is semidet.

short_option('s', suffix).

:- pred long_option(string::in, option::out) is semidet.

long_option("suffix", suffix).

:- pred option_default(option::out, option_data::out) is multi.

option_default(suffix, maybe_string(no)).
option_default(dummy, int(0)).  % force multi

%---------------------------------------------------------------------------%

:- pred current_year(int::out, io::di, io::uo) is det.

current_year(Year, !IO) :-
    time(Time, !IO),
    localtime(Time, TM, !IO),
    Year = 1900 + TM ^ tm_year.

:- pred process_stdin(int::in, maybe(string)::in, io::di, io::uo) is det.

process_stdin(CurrentYear, MaybeExpectSuffix, !IO) :-
    io.input_stream(InputStream, !IO),
    read_lines_loop(InputStream, CurrentYear, MaybeExpectSuffix,
        [], RevLines, unmodified, _ModState, !IO),
    io.output_stream(OutputStream, !IO),
    list.foldl(io.write_string(OutputStream), reverse(RevLines), !IO).

:- pred process_files(int::in, maybe(string)::in, list(string)::in,
    io::di, io::uo) is det.

process_files(_CurrentYear, _MaybeExpectSuffix, [], !IO).
process_files(CurrentYear, MaybeExpectSuffix, [FileName | FileNames], !IO) :-
    process_file(CurrentYear, MaybeExpectSuffix, FileName, Continue, !IO),
    (
        Continue = yes,
        process_files(CurrentYear, MaybeExpectSuffix, FileNames, !IO)
    ;
        Continue = no
    ).

:- pred process_file(int::in, maybe(string)::in, string::in, bool::out,
    io::di, io::uo) is det.

process_file(CurrentYear, MaybeExpectSuffix, FileName, Continue, !IO) :-
    io.open_input(FileName, OpenInputRes, !IO),
    (
        OpenInputRes = ok(InputStream),
        read_lines_loop(InputStream, CurrentYear, MaybeExpectSuffix,
            [], RevLines, unmodified, ModState, !IO),
        io.close_input(InputStream, !IO),
        (
            ( ModState = unmodified
            ; ModState = found_unmodified
            ),
            io.format("Unchanged: %s\n", [s(FileName)], !IO),
            Continue = yes
        ;
            ModState = found_modified,
            % It would be better to write a temporary file then atomically
            % rename over the original file, but that would require extra work
            % to preserve the file ownership and permissions. For simplicity,
            % we forgo atomicity.
            io.open_output(FileName, OpenOutputRes, !IO),
            (
                OpenOutputRes = ok(OutputStream),
                list.foldl(io.write_string(OutputStream),
                    reverse(RevLines), !IO),
                io.close_output(OutputStream, !IO),
                io.format("Modified: %s\n", [s(FileName)], !IO),
                maybe_set_modified_exit_status(!IO),
                Continue = yes
            ;
                OpenOutputRes = error(Error),
                report_io_error("Error opening " ++ FileName, Error, !IO),
                Continue = no
            )
        )
    ;
        OpenInputRes = error(Error),
        report_io_error("Error opening " ++ FileName, Error, !IO),
        Continue = no
    ).

%---------------------------------------------------------------------------%

:- pred read_lines_loop(io.text_input_stream::in, int::in, maybe(string)::in,
    list(string)::in, list(string)::out, mod_state::in, mod_state::out,
    io::di, io::uo) is det.

read_lines_loop(InputStream, CurrentYear, MaybeExpectSuffix,
        !RevLines, !ModState, !IO) :-
    io.read_line_as_string(InputStream, ReadRes, !IO),
    (
        ReadRes = ok(Line),
        ( if
            !.ModState = unmodified,
            parse_copyright_line(Line, MaybeExpectSuffix,
                Prefix, Ranges0, Suffix)
        then
            % We could normalise ranges here.
            sort(Ranges0, Ranges1),
            ( if add_to_ranges(CurrentYear, Ranges1, Ranges) then
                make_copyright_line(Prefix, Ranges, Suffix, NewLine),
                !:RevLines = [NewLine | !.RevLines],
                !:ModState = found_modified
            else
                !:RevLines = [Line | !.RevLines],
                !:ModState = found_unmodified
            )
        else
            !:RevLines = [Line | !.RevLines]
        ),
        read_lines_loop(InputStream, CurrentYear, MaybeExpectSuffix,
            !RevLines, !ModState, !IO)
    ;
        ReadRes = eof
    ;
        ReadRes = error(Error),
        report_io_error("Error reading", Error, !IO)
    ).

:- pred parse_copyright_line(string::in, maybe(string)::in,
    string::out, list(year_range)::out, string::out) is semidet.

parse_copyright_line(Line, MaybeExpectSuffix, Prefix, Ranges, Suffix) :-
    string.sub_string_search(Line, "Copyright ", AfterCopyright),
    find_prefix_end(Line, ' ', AfterCopyright, PrefixEnd),
    find_suffix_start(Line, PrefixEnd, PrefixEnd, SuffixStart),
    (
        MaybeExpectSuffix = no
    ;
        MaybeExpectSuffix = yes(ExpectSuffix),
        string.sub_string_search_start(Line, ExpectSuffix, SuffixStart, _)
    ),
    string.unsafe_between(Line, 0, PrefixEnd, Prefix),
    string.unsafe_between(Line, PrefixEnd, SuffixStart, Mid),
    string.unsafe_between(Line, SuffixStart, length(Line), Suffix),
    parse_ranges(Mid, Ranges).

:- pred find_prefix_end(string::in, char::in, int::in, int::out) is semidet.

find_prefix_end(Str, PrevC, I0, I) :-
    string.unsafe_index_next(Str, I0, I1, C),
    % We'll assume the first digit following whitespace begins the year ranges.
    ( if
        char.is_digit(C),
        char.is_whitespace(PrevC)
    then
        I = I0
    else
        find_prefix_end(Str, C, I1, I)
    ).

:- pred find_suffix_start(string::in, int::in, int::in, int::out) is semidet.

find_suffix_start(Str, I0, LastNonWs, I) :-
    ( if string.unsafe_index_next(Str, I0, I1, C) then
        ( if
            ( char.is_digit(C)
            ; C = ('-')
            ; C = (',')
            )
        then
            find_suffix_start(Str, I1, I1, I)
        else if char.is_whitespace(C) then
            find_suffix_start(Str, I1, LastNonWs, I)
        else
            I = LastNonWs
        )
    else
        I = LastNonWs
    ).

:- pred parse_ranges(string::in, list(year_range)::out) is semidet.

parse_ranges(Str, Ranges) :-
    Words = string.words_separator(is_whitespace_or_comma, Str),
    list.map(parse_range, Words, Ranges).

:- pred parse_range(string::in, year_range::out) is semidet.

parse_range(Str, Range) :-
    Words = string.split_at_char('-', Str),
    (
        Words = [S],
        string.to_int(S, N),
        Range = years(N, N)
    ;
        Words = [S1, S2],
        string.to_int(S1, N1),
        string.to_int(S2, N2),
        N1 =< N2,
        Range = years(N1, N2)
    ).

:- pred is_whitespace_or_comma(char::in) is semidet.

is_whitespace_or_comma(C) :-
    ( char.is_whitespace(C)
    ; C = (',')
    ).

:- pred add_to_ranges(int::in, list(year_range)::in, list(year_range)::out)
    is semidet.

add_to_ranges(Year, Ranges0, Ranges) :-
    (
        Ranges0 = [],
        Ranges = [years(Year, Year)]
    ;
        Ranges0 = [R0 | Ranges1],
        ( if year_in_range(Year, R0) then
            fail
        else if extend_range(Year, R0, R) then
            Ranges = [R | Ranges1]
        else
            add_to_ranges(Year, Ranges1, Ranges2),
            Ranges = [R0 | Ranges2]
        )
    ).

:- pred year_in_range(int::in, year_range::in) is semidet.

year_in_range(Year, Range) :-
    Range = years(Lo, Hi),
    Year >= Lo,
    Year =< Hi.

:- pred extend_range(int::in, year_range::in, year_range::out) is semidet.

extend_range(Year, Range0, Range) :-
    Range0 = years(Lo, Hi),
    Year = Hi + 1,
    Range = years(Lo, Year).

:- pred make_copyright_line(string::in, list(year_range)::in, string::in,
    string::out) is det.

make_copyright_line(Prefix, Ranges, Suffix, Line) :-
    Mid = string.join_list(", ", list.map(range_to_string, Ranges)),
    Line = Prefix ++ Mid ++ Suffix.

:- func range_to_string(year_range) = string.

range_to_string(Range) = Str :-
    Range = years(Lo, Hi),
    ( if Lo = Hi then
        Str = string.from_int(Lo)
    else
        Str = string.from_int(Lo) ++ "-" ++ string.from_int(Hi)
    ).

%---------------------------------------------------------------------------%

:- pred report_io_error(string::in, io.error::in, io::di, io::uo) is det.

report_io_error(Prefix, Error, !IO) :-
    Message = Prefix ++ ": " ++ io.error_message(Error),
    report_error_message(Message, !IO).

:- pred report_error_message(string::in, io::di, io::uo) is det.

report_error_message(Message, !IO) :-
    io.stderr_stream(ErrorStream, !IO),
    io.write_string(ErrorStream, Message, !IO),
    io.nl(ErrorStream, !IO),
    io.set_exit_status(2, !IO).

:- pred maybe_set_modified_exit_status(io::di, io::uo) is det.

maybe_set_modified_exit_status(!IO) :-
    % Don't override exit status for general errors.
    io.get_exit_status(ExitStatus0, !IO),
    ( if ExitStatus0 = 0 then
        io.set_exit_status(1, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module update_copyright.
%---------------------------------------------------------------------------%
