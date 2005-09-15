:- module mutable_parent.

:- interface.

:- import_module io.

:- include_module mutable_child.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- import_module mutable_parent.mutable_child.
:- import_module mutable_parent.mutable_child.mutable_grandchild.

:- mutable(parent_global, int, 100, ground, [untrailed, thread_safe]).

main(!IO) :-
	mutable_parent.run_parent(!IO),
	mutable_parent.mutable_child.run_child(!IO),
	mutable_parent.mutable_child.mutable_grandchild.run_grandchild(!IO),
	io.write_string("Back in parent ...\n", !IO),
	promise_pure (
		semipure get_parent_global(ParentGlobal)
	),
	io.format("    parent_global = %d\n", [i(ParentGlobal)], !IO).

:- pred run_parent(io::di, io::uo) is det.

run_parent(!IO) :-
	io.write_string("In parent ...\n", !IO),
	promise_pure (
		semipure get_parent_global(X)
	),
	io.format("    parent_global = %d\n", [i(X)], !IO),
	promise_pure (
		impure set_parent_global(X + 1)
	).
