% This tests the interaction between type classes, instance declarations,
% and modules.  In particular this test checks that in module `module_test'
% we can use an instance declaration defined in a different module
% `module_test_m2' that defines an instance of a type class which is
% defined in yet another module `module_test_m1'.

:- module module_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module module_test_m1.
:- import_module module_test_m2.

main -->
	run(a_t1),
	run("hello world"),
	run(a_t2),
	run(123.45).
