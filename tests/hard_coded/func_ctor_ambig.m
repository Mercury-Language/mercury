% Mercury 0.7.3 generated incorrect code for this test case.

:- module func_ctor_ambig.
:- interface.
:- import_module io.

:- type t.
:- func bar = int.
:- func baz = t.
:- func bar2 = int.
:- func baz2 = t.

:- pred main(io.state::di, io.state::uo) is det.

:- implementation.

main -->
	print("bar = "), print(bar), nl,
	print("bar2 = "), print(bar2), nl,
	print("baz = "), print(baz), nl,
	print("baz2 = "), print(baz2), nl.

:- type t ---> ambig.

:- func ambig = int.
ambig = 42.

bar = ambig.

baz = ambig.

bar2 = func_ctor_ambig.ambig.

baz2 = func_ctor_ambig.ambig.

