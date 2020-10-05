%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
%
% Name: getopt_test.m
%
% Description of bug:
%   "-" was not being handled correctly by getopt. It should be left
%   alone, but was being processed by an option.
%
% Symptom(s) of bug:
%   "-" as an option would be silently ignored.
%
% Date bug existed: 8-July-1997
%
% Author: trd
%
% Note: This bug report (and fix) provided by Philip Dart.

%---------------------------------------------------------------------------%

:- module getopt_test.
:- interface.
:- import_module getopt.
:- import_module io.

:- type option
    --->    foo
    ;       bar.

:- type option_table == option_table(option).

:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(option::out, option_data::out) is nondet.
:- pred option_default(option::out, option_data::out) is multidet.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module bool.
:- import_module list.
:- import_module require.
:- import_module std_util.

option_defaults(Option, Default) :-
    semidet_succeed,
    option_default(Option, Default).

option_default(foo, string("happy")).
option_default(bar, bool(yes)).

short_option('f',   foo).
short_option('b',   bar).

long_option("foo",  foo).
long_option("bar",  bar).

main(!IO) :-
    Ops = option_ops(short_option, long_option, option_defaults),
    getopt.process_options(Ops, ["-"], Left, MaybeOptionTable),
    ( if
        MaybeOptionTable = ok(OptionTable)
    then
        getopt.lookup_bool_option(OptionTable, bar, Bar),
        getopt.lookup_string_option(OptionTable, foo, FooStr),
        io.write_string("option bar: `", !IO),
        io.write(Bar, !IO),
        io.write_string("'\n", !IO),
        io.write_string("option foo: `", !IO),
        io.write_string(FooStr, !IO),
        io.write_string("'\n", !IO),

        io.write_line(Left, !IO)
    else
        error("unable to process options")
    ).
