%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order_no_detism.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module getopt.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    getopt.process_options(
        option_ops_multi(
            short,
            long,
            defaults
        ), [], _, MaybeOptions),
    (
        MaybeOptions = ok(_),
        io.write_string("ok\n", !IO)
    ;
        MaybeOptions = error(_),
        io.write_string("error\n", !IO)
    ).

:- type option
    --->    opt_a
    ;       opt_b
    ;       opt_c.

:- type option_table == (option_table(option)).

:- pred short(char::in, option::out).               % XXX is semidet.

short('a',  opt_a).
short('b',  opt_b).
short('c',  opt_c).

:- pred long(string::in, option::out).              % XXX is semidet.

long("opt-a",    opt_a).
long("opt-b",    opt_b).
long("opt-c",    opt_c).

:- pred defaults(option::out, option_data::out).    % XXX is multi.

defaults(opt_a,      bool(no)).
defaults(opt_b,      bool(no)).
defaults(opt_c,      bool(no)).

%---------------------------------------------------------------------------%
:- end_module higher_order_no_detism.
%---------------------------------------------------------------------------%
