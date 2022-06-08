%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

% The top-level

:- module main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module eval.
:- import_module eval_util.
:- import_module exception.
:- import_module globals.
:- import_module gml.
:- import_module peephole.
:- import_module renderer.

:- import_module char.
:- import_module getopt.
:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module prolog.
:- import_module unit.

main(!IO) :-
    io.command_line_arguments(AllArgs, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_defaults),
    getopt.process_options(OptionOps, AllArgs, _OptionArgs, NonOptionArgs,
        OptionResult),
    (
        OptionResult = ok(OptionTable),
        getopt.lookup_int_option(OptionTable, target_parallelism,
            TargetParallelism),
        getopt.lookup_string_option(OptionTable, time_filename,
            TimeFileName),
        set_target_parallelism(TargetParallelism, !IO),
        set_time_filename(TimeFileName, !IO),

        (
            NonOptionArgs = []
            % Take input from stdin.
        ;
            NonOptionArgs = [FileName],
            prolog.see(FileName, SeeResult, !IO),
            (
                SeeResult = ok
            ;
                SeeResult = error(Error),
                io.error_message(Error, Msg),
                io.write_string(Msg, !IO),
                io.set_exit_status(1, !IO)
            )
        ;
            NonOptionArgs = [_, _ | _],
            io.write_string("Expected at most one non-option argument.\n", !IO)
        ),

        try_io(real_main, ExceptionResult, !IO),
        ( ExceptionResult = exception(E) ->
            write_nice_exception(E, !IO),
            io.set_exit_status(1, !IO)
        ;
            true
        )
    ;
        OptionResult = error(Error),
        io.write_string(option_error_to_string(Error), !IO),
        io.nl(!IO),
        io.set_exit_status(1, !IO)
    ).

:- pred real_main(unit::out, io::di, io::uo) is det.

real_main(unit, !IO) :-
    globals.init(!IO),
    tokenize(BasicTokens, !IO),
    parse(BasicTokens, Program),
    peephole(Program, OptProgram),
%   write_prog(0, OptProgram, !IO),
    setup_and_interpret(OptProgram, !IO).

%-----------------------------------------------------------------------------%

:- type option
    --->    target_parallelism
    ;       time_filename.

:- pred short_option(char::in, option::out) is semidet.

short_option('p', target_parallelism).
short_option('f', time_filename).

:- pred long_option(string::in, option::out) is semidet.

long_option("target-parallelism",   target_parallelism).
long_option("time_filename",        time_filename).

:- pred option_defaults(option::out, option_data::out) is multi.

option_defaults(target_parallelism, int(0)).
option_defaults(time_filename, string("")).

%-----------------------------------------------------------------------------%
