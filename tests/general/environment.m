% Short series of tests for io__get_environment_var and
% io__set_environment_var.
%
% Author: bromage

:- module environment.

:- interface.
:- import_module io.
:- pred main(io__state :: di, io__state :: uo) is det.

:- implementation.
:- import_module list, bool, std_util, string.

:- pred environment__test(string, bool, bool, io__state, io__state).
:- mode environment__test(in, in, in, di, uo) is det.
environment__test(Var, ShouldBeSet, ShouldBePrinted) -->
	io__get_environment_var(Var, MaybeValue),
	io__write_strings(["Variable \"", Var, "\" is set "]),
	( { MaybeValue = yes(Value) } ->
		( { ShouldBePrinted = yes } ->
			io__write_strings(["to \"", Value, "\" "])
		;
			[]
		),
		{ Ok = ShouldBeSet }
	;
		io__write_string("not set "),
		{ bool__not(ShouldBeSet, Ok) }
	),
	( { Ok = yes } ->
		io__write_string("(passed)\n")
	;
		io__write_string("(failed)\n")
	).

main -->
	% PATH should be set on all Unix systems but may differ
	% on the different machines that generate .exp and .out files
	environment__test("PATH", yes, no),

	% This one probably isn't. :-)
	environment__test("SHOULD_NOT_BE_SET", no, yes),

	% So set it...
	io__set_environment_var("SHOULD_NOT_BE_SET", "Hello World!"),

	% Did that work?
	environment__test("SHOULD_NOT_BE_SET", yes, yes).

