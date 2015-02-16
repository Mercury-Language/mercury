%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mutable_parent.mutable_child.

:- interface.

:- pred run_child(io::di, io::uo) is det.

:- include_module mutable_grandchild.

:- implementation.

:- mutable(child_global, int, 200, ground,
    [untrailed, attach_to_io_state]).

run_child(!IO) :-
    io.write_string("In child ...\n", !IO),
    get_parent_global(ParentGlobal, !IO),
    get_child_global(ChildGlobal, !IO),
    io.format("    parent_global = %d\n", [i(ParentGlobal)], !IO),
    io.format("    child_global  = %d\n", [i(ChildGlobal)], !IO),
    set_parent_global(ParentGlobal + 1, !IO),
    set_child_global(ChildGlobal + 1, !IO).
