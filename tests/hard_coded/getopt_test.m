%
% Regression test.
%
% Name: getopt_test.m
%
% Description of bug:
%	"-" was not being handled correctly by getopt. It should be left
%	alone, but was being processed by an option.
%
% Symptom(s) of bug:
% 	"-" as an option would be silently ignored.
%
% Date bug existed: 8-July-1997
%
% Author: trd
%
% Note: This bug report (and fix) provided by Philip Dart.

%-----------------------------------------------------------------------------%

:- module getopt_test.
:- interface.
:- import_module io, getopt.

:- type option	
	--->	foo
	;	bar.

:- type option_table	==	option_table(option).
		
:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(option::out, option_data::out) is nondet.
:- pred option_default(option::out, option_data::out) is multidet.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, list, std_util, bool.

option_defaults(Option, Default) :-
	semidet_succeed,
	option_default(Option, Default).

option_default(foo,		string("happy")).
option_default(bar,		bool(yes)).

short_option('f',			foo).
short_option('b',			bar).

long_option("foo",			foo).
long_option("bar",			bar).

main -->
	{ getopt__process_options(option_ops(short_option, long_option, 
		option_defaults), ["-"], Left, MaybeOptionTable) },
	(
		{ MaybeOptionTable = ok(OptionTable) }
	->
		{ getopt__lookup_bool_option(OptionTable, bar,
			Bar) },
		{ getopt__lookup_string_option(OptionTable, foo,
			FooStr) },
		io__write_string("option bar: `"),
		io__write(Bar),
		io__write_string("'\n"),
		io__write_string("option foo: `"),
		io__write_string(FooStr),
		io__write_string("'\n"),

		io__write(Left),
		io__write_string("\n")
		
	;
		{ error("unable to process options") }
	).

