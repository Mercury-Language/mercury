:- module mutable_parent.mutable_child.

:- interface.

:- pred run_child(io::di, io::uo) is det.

:- include_module mutable_grandchild.

:- implementation.

:- mutable(child_global, int, 200, ground, [untrailed, thread_safe]).

run_child(!IO) :-
	io.write_string("In child ...\n", !IO),
	promise_pure (
		semipure get_parent_global(ParentGlobal),
		semipure get_child_global(ChildGlobal)
	),
	io.format("    parent_global = %d\n", [i(ParentGlobal)], !IO),
	io.format("    child_global  = %d\n", [i(ChildGlobal)], !IO),
	promise_pure (
		impure set_parent_global(ParentGlobal + 1),
		impure set_child_global(ChildGlobal + 1)
	).
