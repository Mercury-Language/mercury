%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module midimon.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- pragma require_feature_set([concurrency]).

:- import_module concurrent_stream.
:- import_module midi.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module thread.

%----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    process_options(
        option_ops_multi(short_option, long_option, option_defaults),
        Args0, _Args, MOpts),
    (
        MOpts = ok(Opts),
        lookup_bool_option(Opts, help, Help),
        (
            Help = yes,
            help(!IO)
        ;
            Help = no,
            lookup_maybe_string_option(Opts, input, MInfile),
            open_input(MInfile, InFileOpened, !IO),
            (
                InFileOpened = yes,
                new(Bytes0, !IO),
                new(Messages, !IO),
                spawn(
                    ( pred(!.IO::di, !:IO::uo) is cc_multi :-
                        read_midi(Bytes0, Messages, !IO)
                    ), !IO),
                spawn(
                    ( pred(!.IO::di, !:IO::uo) is cc_multi :-
                        print_messages(Messages, !IO)
                    ), !IO),
                read_input(Bytes0, !IO)
            ;
                InFileOpened = no
            )
        )
    ;
        MOpts = error(Error),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "%s\n", [s(option_error_to_string(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred open_input(maybe(string)::in, bool::out, io::di, io::uo) is det.

open_input(no, Opened, !IO) :-
    io.open_binary_input("/dev/midi", Res, !IO),
    (
        Res = ok(DevMidi),
        io.set_binary_input_stream(DevMidi, _, !IO),
        Opened = yes
    ;
        Res = error(Err),
        io.error_message(Err, Msg),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "error opening `/dev/midi': %s\n", [s(Msg)], !IO),
        Opened = no
    ).
open_input(yes(FileName), Opened, !IO) :-
    ( if FileName = "-" then
            % use stdin
        Opened = yes
    else
        io.open_binary_input(FileName, Res, !IO),
        (
            Res = ok(File),
            io.set_binary_input_stream(File, _, !IO),
            Opened = yes
        ;
            Res = error(Err),
            io.error_message(Err, Msg),
            io.stderr_stream(StdErr, !IO),
            io.format(StdErr, "error opening `%s': %s\n",
                [s(FileName), s(Msg)], !IO),
            Opened = no
        )
    ).

:- pred read_input(concurrent_stream(byte)::in,
    io::di, io::uo) is det.

read_input(Stream, !IO) :-
    io.read_byte(Res0, !IO),
    (
        Res0 = eof,
        end(Stream, !IO)
    ;
        Res0 = error(Err),
        io.error_message(Err, Msg),
        error(Stream, Msg, !IO)
    ;
        Res0 = ok(Byte),
        put(Stream, Byte, !IO),
        read_input(Stream, !IO)
    ).

:- pred print_messages(concurrent_stream(message)::in,
    io::di, io::uo) is det.

print_messages(Stream, !IO) :-
    get(Stream, Res0, !IO),
    (
        Res0 = ok(Msg),
        io.write(Msg, !IO),
        io.write_string(".\n", !IO),
        print_messages(Stream, !IO)
    ;
        Res0 = end
    ;
        Res0 = error(Msg),
        io.write_string(Msg, !IO),
        io.nl(!IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type option_table == option_table(option).
:- type maybe_option_table == maybe_option_table(option).

    % The master list of options.
    %
:- type option
    --->    help
    ;       input.

:- pred long_option(string::in, option::out) is semidet.

long_option("help",       help).
long_option("input-file", input).

:- pred short_option(char::in, option::out) is semidet.

short_option('h', help).
short_option('i', input).

:- pred option_defaults(option::out, option_data::out) is multi.

option_defaults(help,  bool(no)).
option_defaults(input, maybe_string(no)).

:- pred help(io::di, io::uo) is det.

help(!IO) :-
    io.write_strings([
"usage: midimon [--help|-h] [--input-file|-i <filename>]\n",
"   --help|-h       print this help message.\n",
"   --input-file|-i <file>  read from <file> (default is /dev/midi).\n"
    ], !IO).

%-----------------------------------------------------------------------------%
:- end_module midimon.
%-----------------------------------------------------------------------------%
