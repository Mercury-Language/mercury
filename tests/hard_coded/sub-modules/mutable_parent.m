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

:- mutable(parent_global, int, 100, ground,
	[untrailed, attach_to_io_state]).

main(!IO) :-
	mutable_parent.run_parent(!IO),
	mutable_parent.mutable_child.run_child(!IO),
	mutable_parent.mutable_child.mutable_grandchild.run_grandchild(!IO),
	io.write_string("Back in parent ...\n", !IO),
	get_parent_global(ParentGlobal, !IO),
	io.format("    parent_global = %d\n", [i(ParentGlobal)], !IO).

:- pred run_parent(io::di, io::uo) is det.

run_parent(!IO) :-
	io.write_string("In parent ...\n", !IO),
	get_parent_global(X, !IO),
	io.format("    parent_global = %d\n", [i(X)], !IO),
	set_parent_global(X + 1, !IO).
