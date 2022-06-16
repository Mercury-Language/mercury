%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% File: mcowsay.m.
% Author: juliensf.
%
% A Mercury version of the cowsay program originally written by Tony Monroe.
% It prints an ASCII art picture of a cow saying/thinking a user-supplied
% message.
%
% Implements most of the functionality of the original cowsay aside from the
% ability to use .cow files (-f option and COWPATH environment variable) and
% the ability to list .cow files in the COWPATH (-l option). Implementing
% those is left as an exercise for the reader.
%
% This source file is hereby placed in the public domain.
%
%---------------------------------------------------------------------------%

:- module mcowsay.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module stream.
:- import_module string.
:- import_module require.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(
        short_option,
        long_option,
        option_default,
        special_option_handler
    ),
    getopt.process_options(OptionOps, Args, NonOptionArgs, OptionResult),
    (
        OptionResult = ok(OptionTable),
        ( if getopt.lookup_bool_option(OptionTable, help, yes) then
            print_help_message(!IO)
        else if getopt.lookup_bool_option(OptionTable, version, yes) then
            print_version_message(!IO)
        else
            (
                NonOptionArgs = [],
                read_and_print_message_from_stdin(OptionTable, !IO)
            ;
                NonOptionArgs = [Message],
                % We replicate the behaviour of the original cowsay here.
                % If the message is empty, then read from standard input.
                ( if Message = "" then
                    read_and_print_message_from_stdin(OptionTable, !IO)
                else
                    read_and_print_message_from_arg(OptionTable, Message, !IO)
                )
            ;
                NonOptionArgs = [_, _ | _],
                print_usage_error(!IO)
            )
        )
    ;
        OptionResult = error(Error),
        print_option_error(Error, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred read_and_print_message_from_stdin(option_table(option)::in,
    io::di, io::uo) is det.

read_and_print_message_from_stdin(OptionTable, !IO) :-
    io.stdin_stream(Stdin, !IO),
    getopt.lookup_bool_option(OptionTable, word_wrap, WordWrap),
    (
        WordWrap = no,
        stream.input_stream_fold(Stdin, add_line, cord.empty, Result, !IO),
        (
            Result = ok(Lines),
            print_cow_and_message(OptionTable, Lines, !IO)
        ;
            Result = error(_, IO_Error),
            print_io_error(IO_Error, !IO)
        )
    ;
        WordWrap = yes,
        io.read_file_as_string(Stdin, Result, !IO),
        (
            Result = ok(Message),
            Lines = wrap_message(OptionTable, Message),
            print_cow_and_message(OptionTable, Lines, !IO)
        ;
            Result = error(_, IO_Error),
            print_io_error(IO_Error, !IO)
        )
    ).

:- pred add_line(line::in, cord(string)::in, cord(string)::out) is det.

add_line(Line, !Lines) :-
    Line = line(String),
    ExpandedString = expand_tabs(String),
    cord.snoc(string.chomp(ExpandedString), !Lines).

%---------------------------------------------------------------------------%

:- pred read_and_print_message_from_arg(option_table(option)::in,
    string::in, io::di, io::uo) is det.

read_and_print_message_from_arg(OptionTable, Message, !IO) :-
    getopt.lookup_bool_option(OptionTable, word_wrap, WordWrap),
    (
        WordWrap = no,
        ExpandedMessage = expand_tabs(Message),
        LineList = string.split_into_lines(ExpandedMessage),
        Lines = cord.from_list(LineList)
    ;
        WordWrap = yes,
        Lines = wrap_message(OptionTable, Message)
    ),
    print_cow_and_message(OptionTable, Lines, !IO).

%---------------------------------------------------------------------------%

:- func wrap_message(option_table(option), string) = cord(string).

wrap_message(OptionTable, Message) = Lines :-
    getopt.lookup_int_option(OptionTable, wrap_width, WrapWidth),
    WrappedMessage = string.word_wrap(Message, WrapWidth),
    LineList = string.split_into_lines(WrappedMessage),
    Lines = cord.from_list(LineList).

:- func expand_tabs(string) = string.

expand_tabs(String) = string.replace_all(String, "\t", "    ").

%---------------------------------------------------------------------------%

:- type cow_action
    --->    speaking
    ;       thinking.

:- pred get_cow_action(cow_action::out, io::di, io::uo) is det.

get_cow_action(Action, !IO) :-
    io.progname_base("mcowsay", ProgName, !IO),
    ( if (ProgName = "cowthink" ; ProgName = "mcowthink") then
        Action = thinking
    else
        Action = speaking
    ).

:- func thoughts_string(cow_action) = string.

thoughts_string(speaking) = "\\".
thoughts_string(thinking) = "o".

%---------------------------------------------------------------------------%

:- pred print_cow_and_message(option_table(option)::in, cord(string)::in,
    io::di, io::uo) is det.

print_cow_and_message(OptionTable, Lines, !IO) :-
    get_cow_action(Action, !IO),
    MaxLineWidth = max_line_width(Lines),
    print_message_bubble(Action, MaxLineWidth, Lines, !IO),
    getopt.lookup_string_option(OptionTable, eyes_string, EyesString),
    getopt.lookup_string_option(OptionTable, tongue_string, TongueString),
    ThoughtsString = thoughts_string(Action),
    io.write_string(cow(EyesString, TongueString, ThoughtsString), !IO).

:- func max_line_width(cord(string)) = int.

max_line_width(Lines) = MaxWidth :-
    cord.foldl_pred(acc_line_width, Lines, 0, MaxWidth).

:- pred acc_line_width(string::in, int::in, int::out) is det.

acc_line_width(Line, !MaxWidth) :-
    string.count_codepoints(Line, LineWidth),
    ( if LineWidth > !.MaxWidth then
        !:MaxWidth = LineWidth
    else
        true
    ).

:- pred print_message_bubble(cow_action::in, int::in, cord(string)::in,
    io::di, io::uo) is det.

print_message_bubble(Action, MaxLineWidth, Lines, !IO) :-
    io.print_line(top_bubble_border(MaxLineWidth), !IO),
    ( if cord.is_empty(Lines) then
        print_single_line_bubble(Action, "", !IO)
    else if cord.is_singleton(Lines, FirstLine) then
        print_single_line_bubble(Action, FirstLine, !IO)
    else
        NumLines = cord.length(Lines),
        (
            Action = speaking,
            cord.foldl2(print_speech_bubble_line(MaxLineWidth, NumLines),
                Lines, 1, _, !IO)
        ;
            Action = thinking,
            cord.foldl_pred(print_thought_bubble_line(MaxLineWidth), Lines,
                !IO)
        )
    ),
    io.print_line(bottom_bubble_border(MaxLineWidth), !IO).

:- pred print_single_line_bubble(cow_action::in, string::in,
    io::di, io::uo) is det.

print_single_line_bubble(Action, Line, !IO) :-
    (
        Action = speaking,
        io.format("< %s >\n", [s(Line)], !IO)
    ;
        Action = thinking,
        io.format("( %s )\n", [s(Line)], !IO)
    ).

:- pred print_speech_bubble_line(int::in, int::in, string::in,
    int::in, int::out, io::di, io::uo) is det.

print_speech_bubble_line(MaxLineWidth, NumLines, Line, !LineCount, !IO) :-
    ( if !.LineCount = 1 then
        Prefix = "/", Suffix = "\\"
    else if !.LineCount < NumLines then
        Prefix = "|", Suffix = "|"
    else
        Prefix = "\\", Suffix = "/"
    ),
    NLine = normalize_line(MaxLineWidth, Line),
    io.format("%s %s %s\n", [s(Prefix), s(NLine), s(Suffix)], !IO),
    !:LineCount = !.LineCount + 1.

:- pred print_thought_bubble_line(int::in, string::in, io::di, io::uo) is det.

print_thought_bubble_line(MaxLineWidth, Line, !IO) :-
    NLine = normalize_line(MaxLineWidth, Line),
    io.format("( %s )\n", [s(NLine)], !IO).

:- func top_bubble_border(int) = string.

top_bubble_border(MaxLineWidth) =
    " " ++ string.duplicate_char('_', MaxLineWidth + 2).

:- func bottom_bubble_border(int) = string.

bottom_bubble_border(MaxLineWidth) =
    " " ++ string.duplicate_char('-', MaxLineWidth + 2).

:- func normalize_line(int, string) = string.

normalize_line(MaxLineWidth, Line) = NormalLine :-
    string.count_codepoints(Line, LineWidth),
    NormalLine = Line ++ string.duplicate_char(' ', MaxLineWidth - LineWidth).

%---------------------------------------------------------------------------%

:- func cow(string, string, string) = string.

cow(Eyes, Tongue, Thoughts) = string.append_list([
"        ", Thoughts, "   ^__^\n",
"         ", Thoughts, "  (", Eyes, ")\\_______\n",
"            (__)\\       )\\/\\\n",
"             ", Tongue, " ||----w |\n",
"                ||     ||\n"
]).

%---------------------------------------------------------------------------%

:- type option
    --->    help
    ;       version

    % Options to control word wrapping.

    ;       user_wrap_width
    ;       no_format

    % Options to control the cow mode.

    ;       borg_mode
    ;       dead_mode
    ;       greedy_mode
    ;       paranoid_mode
    ;       stoned_mode
    ;       tired_mode
    ;       wired_mode
    ;       youthful_mode
    ;       user_eyes
    ;       user_tongue

    % Internal options.

    ;       word_wrap
    ;       wrap_width
    ;       eyes_string
    ;       tongue_string.

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('n', no_format).
short_option('b', borg_mode).
short_option('d', dead_mode).
short_option('g', greedy_mode).
short_option('p', paranoid_mode).
short_option('s', stoned_mode).
short_option('t', tired_mode).
short_option('w', wired_mode).
short_option('y', youthful_mode).
short_option('e', user_eyes).
short_option('T', user_tongue).
short_option('W', user_wrap_width).

:- pred long_option(string::in, option::out) is semidet.

long_option("help", help).
long_option("version", version).
long_option("no-wrap", no_format).
long_option("borg", borg_mode).
long_option("dead", dead_mode).
long_option("greedy", greedy_mode).
long_option("stoned", stoned_mode).
long_option("tired", tired_mode).
long_option("wired", wired_mode).
long_option("youthful", youthful_mode).
long_option("eyes", user_eyes).
long_option("tongue", user_tongue).
long_option("width", user_wrap_width).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help, bool(no)).
option_default(version, bool(no)).
option_default(no_format, special).
option_default(borg_mode, special).
option_default(dead_mode, special).
option_default(greedy_mode, special).
option_default(paranoid_mode, special).
option_default(stoned_mode, special).
option_default(tired_mode, special).
option_default(wired_mode, special).
option_default(youthful_mode, special).
option_default(user_eyes, string_special).
option_default(user_tongue, string_special).
option_default(eyes_string, string("oo")).
option_default(tongue_string, string("  ")).
option_default(word_wrap, bool(yes)).
option_default(user_wrap_width, int_special).
option_default(wrap_width, int(40)).

:- pred special_option_handler(option::in, special_data::in,
    option_table(option)::in, maybe_option_table(option)::out) is semidet.

special_option_handler(no_format, none, !.OptionTable, Result) :-
    map.set(word_wrap, bool(no), !OptionTable),
    Result = ok(!.OptionTable).

special_option_handler(borg_mode, none, OptionTable, Result) :-
    set_cow_mode("==", "  ", OptionTable, Result).
special_option_handler(dead_mode, none, OptionTable, Result) :-
    set_cow_mode("XX", "U ", OptionTable, Result).
special_option_handler(greedy_mode, none, OptionTable, Result) :-
    set_cow_mode("$$", "  ", OptionTable, Result).
special_option_handler(paranoid_mode, none, OptionTable, Result) :-
    set_cow_mode("@@", "  ", OptionTable, Result).
special_option_handler(stoned_mode, none, OptionTable, Result) :-
    set_cow_mode("**", "U ", OptionTable, Result).
special_option_handler(tired_mode, none, OptionTable, Result) :-
    set_cow_mode("--", "  ", OptionTable, Result).
special_option_handler(wired_mode, none, OptionTable, Result) :-
    set_cow_mode("OO", "  ", OptionTable, Result).
special_option_handler(youthful_mode, none, OptionTable, Result) :-
    set_cow_mode("..", "  ", OptionTable, Result).
special_option_handler(user_eyes, string(UserEyes), !.OptionTable,
        Result) :-
    string.count_codepoints(UserEyes, NumEyeCodePoints),
    ( if NumEyeCodePoints < 1 then
        Eyes = "  "
    else if NumEyeCodePoints = 1 then
        Eyes = UserEyes ++ " "
    else
        Eyes = string.left_by_codepoint(UserEyes, 2)
    ),
    map.set(eyes_string, string(Eyes), !OptionTable),
    Result = ok(!.OptionTable).
special_option_handler(user_tongue, string(UserTongue), !.OptionTable,
        Result) :-
    string.count_codepoints(UserTongue, NumTongueCodePoints),
    ( if NumTongueCodePoints < 1 then
        Tongue = "  "
    else if NumTongueCodePoints = 1 then
        Tongue = UserTongue ++ " "
    else
        Tongue = string.left_by_codepoint(UserTongue, 2)
    ),
    map.set(tongue_string, string(Tongue), !OptionTable),
    Result = ok(!.OptionTable).
special_option_handler(user_wrap_width, int(WrapCol), !.OptionTable,
        Result) :-
    ( if WrapCol < 1 then
        Result = error("the value of option --width must be greater than zero")
    else
        map.set(wrap_width, int(WrapCol), !OptionTable),
        Result = ok(!.OptionTable)
    ).

:- pred set_cow_mode(string::in, string::in, option_table(option)::in,
    maybe_option_table(option)::out) is det.

set_cow_mode(Eyes, Tongue, !.OptionTable, Result) :-
    map.set(eyes_string, string(Eyes), !OptionTable),
    map.set(tongue_string, string(Tongue), !OptionTable),
    Result = ok(!.OptionTable).

%---------------------------------------------------------------------------%

:- pred print_help_message(io::di, io::uo) is det.

print_help_message(!IO) :-
    io.write_strings([
        "Name: mcowsay - a Mercury version of cowsay\n",
        "\n",
        "Usage: mcowsay [<options>] [<message>]\n",
        "\n",
        "Description:\n",
        "\tPrints an ASCII art picture of a cow saying a message provided\n",
        "\tby the user. If the message is not provided as an argument on\n",
        "\tthe command line, then it will be read from the standard input.\n",
        "\n",
        "\tAny tab characters in the message will be replaced in the output\n",
        "\tby a sequence of four space characters.\n",
        "\n",
        "\tIf the program is invoked as 'mcowthink' or 'cowthink' then the\n",
        "\tcow will think its message instead of saying it.\n",
        "\n",
        "Options:\n",
        "\t-h, --help\n",
        "\t\tPrint this information and exit.\n",
        "\t--version\n",
        "\t\tPrint version information and exit.\n",
        "\t-n, --no-wrap\n",
        "\t\tDo not wrap lines.\n",
        "\t-W <wrap-col>, --width <wrap-col>\n",
        "\t\tSpecify the column at which to wrap words.\n",
        "\t\t<wrap-col> must be greater than zero and defaults to 40.\n",
        "\t-b, --borg\n",
        "\t\t\"Borg mode\", uses == for the cow's eyes.\n",
        "\t-d, --dead\n",
        "\t\t\"Dead mode\", uses XX for the cow's eyes and U for its tongue.\n",
        "\t-g, --greedy\n",
        "\t\t\"Greedy mode\", uses $$ for the cow's eyes.\n",
        "\t-p, --paranoid\n",
        "\t\t\"Paranoid mode\", uses @@ for the cow's eyes.\n",
        "\t-s, --stoned\n",
        "\t\t\"Stoned mode\", uses ** for the cow's eyes and U for its tongue.\n",
        "\t-t, --tired\n",
        "\t\t\"Tired mode\", uses -- for the cow's eyes.\n",
        "\t-w, --wired\n",
        "\t\t\"Wired mode\", uses OO for the cow's eyes.\n",
        "\t-y, --youthful\n",
        "\t\t\"Youthful mode\", uses .. for the cow's eyes.\n",
        "\t-e <eye-string>, --eyes <eye-string>\n",
        "\t\tSpecifies the cow's eye type. Only the first two characters of\n",
        "\t\t<eye-string> are used.\n",
        "\t-T <tongue-string>, --tongue <tongue-string>\n",
        "\t\tSpecifies the cow's tongue shape. Only the first two characters of\n",
        "\t\t<tongue-string> are used.\n"
    ], !IO).

:- pred print_version_message(io::di, io::uo) is det.

print_version_message(!IO) :-
    io.write_string("Mercury cowsay version 1.0\n", !IO).

%---------------------------------------------------------------------------%

:- pred print_option_error(option_error(option)::in, io::di, io::uo) is det.

print_option_error(Error, !IO) :-
    Msg = option_error_to_string(Error),
    io.stderr_stream(Stderr, !IO),
    io.format(Stderr, "error: %s.\n", [s(Msg)], !IO),
    io.set_exit_status(1, !IO).

:- pred print_io_error(io.error::in, io::di, io::uo) is det.

print_io_error(IO_Error, !IO) :-
    io.error_message(IO_Error, Msg),
    io.stderr_stream(Stderr, !IO),
    io.format(Stderr, "error: %s\n", [s(Msg)], !IO),
    io.set_exit_status(1, !IO).

:- pred print_usage_error(io::di, io::uo) is det.

print_usage_error(!IO) :-
    io.stderr_stream(Stderr, !IO),
    io.print_line(Stderr, "Usage: mcowsay [<options>] [<message>]", !IO),
    io.set_exit_status(1, !IO).

%---------------------------------------------------------------------------%
:- end_module mcowsay.
%---------------------------------------------------------------------------%
