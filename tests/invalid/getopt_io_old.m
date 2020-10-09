%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module getopt_io_old.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module getopt_io.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    OptionOps = option_ops_multi(short_option, long_option, option_default),
    process_options(OptionOps, Args0, _Args, GetoptResult, !IO),
    (
        GetoptResult = ok(_OptionTable)
    ;
        GetoptResult = error(ErrorMsg),
        io.format("%s\n", [s(ErrorMsg)], !IO)
    ).

%-----------------------------------------------------------------------------%

:- type option
    --->    help
    ;       verbose
    ;       detailed.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.

short_option('?', help).
short_option('h', help).
short_option('v', verbose).
short_option('d', detailed).

:- pred long_option(string::in, option::out) is semidet.

long_option("help",             help).
long_option("verbose",          verbose).
long_option("detailed",         detailed).

:- pred option_default(option::out, option_data::out) is multi.

option_default(help,            bool(no)).
option_default(verbose,         bool(no)).
option_default(detailed,        bool(no)).

%-----------------------------------------------------------------------------%
