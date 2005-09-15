:- module mutable_parent.mutable_child.mutable_grandchild.

:- interface.

:- pred run_grandchild(io::di, io::uo) is det.

:- implementation.

:- mutable(grandchild_global, int, 300, ground, [untrailed, thread_safe]).

run_grandchild(!IO) :-
	io.write_string("In grandchild ...\n", !IO),
	promise_pure (
		semipure get_parent_global(ParentGlobal),
		semipure get_child_global(ChildGlobal),
		semipure get_grandchild_global(GrandChildGlobal)
	),
	io.format("    parent_global      = %d\n", [i(ParentGlobal)], !IO),
	io.format("    child_global       = %d\n", [i(ChildGlobal)], !IO),
	io.format("    grandchild_global  = %d\n", [i(GrandChildGlobal)], !IO),
	promise_pure (
		impure set_parent_global(ParentGlobal + 1)
	).
