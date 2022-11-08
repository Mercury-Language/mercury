%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module getopt_maybe_option.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module getopt.
:- import_module list.
:- import_module maybe.

:- type option
    --->    i1
    ;       i2
    ;       s1
    ;       s2.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.

short_option('i', i1).

:- pred long_option(string::in, option::out) is semidet.

long_option("i1", i1).
long_option("i2", i2).
long_option("s1", s1).
long_option("s2", s2).

:- pred option_default(option::out, option_data::out) is multi.

option_default(i1, maybe_int(no)).
option_default(i2, maybe_int(no)).
option_default(s1, maybe_string(no)).
option_default(s2, maybe_string(no)).

main(!IO) :-
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    Args = [
        "--no-i1",
        "--i2", "123",
        "--no-s1",
        "--s2", "STR"
    ],
    getopt.process_options(OptionOps, Args, _NonOptionArgs, OptionResult),
    (
        OptionResult = ok(OptionTable),
        getopt.lookup_maybe_int_option(OptionTable, i1, I1),
        getopt.lookup_maybe_int_option(OptionTable, i2, I2),
        getopt.lookup_maybe_string_option(OptionTable, s1, S1),
        getopt.lookup_maybe_string_option(OptionTable, s2, S2),

        io.write_string("option i1: ", !IO),
        io.print_line(I1, !IO),
        io.write_string("option i2: ", !IO),
        io.print_line(I2, !IO),

        io.write_string("option s1: ", !IO),
        io.print_line(S1, !IO),
        io.write_string("option s2: ", !IO),
        io.print_line(S2, !IO)
    ;
        OptionResult = error(Error),
        Msg = option_error_to_string(Error),
        io.print_line(Msg, !IO)
    ).
