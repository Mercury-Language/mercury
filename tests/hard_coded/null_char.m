% Test output of strings containing null characters

% XXX Note that currently we don't handle this correctly;
% we ignore everything after the first null character.
% So the ".exp2" file for this test case allows that output.
% If/when this is fixed, the ".exp2" file for this
% test case should be removed.

:- module null_char.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module string.

main -->
	print("text before null \0 text after null character\n"),
	{ Foo = "before\0&after", Bar = " some more\0&more" },
	print(Foo ++ Bar),
	print("\n").
