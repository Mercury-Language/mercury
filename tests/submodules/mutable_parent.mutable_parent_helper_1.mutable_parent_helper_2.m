%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mutable_parent.mutable_parent_helper_1.mutable_parent_helper_2.

:- interface.

:- pred run_grandchild(io::di, io::uo) is det.

:- implementation.

:- mutable(grandchild_global, int, 300, ground,
    [untrailed, attach_to_io_state]).

run_grandchild(!IO) :-
    io.write_string("In grandchild ...\n", !IO),
    get_parent_global(ParentGlobal, !IO),
    get_child_global(ChildGlobal, !IO),
    get_grandchild_global(GrandChildGlobal, !IO),
    io.format("    parent_global      = %d\n", [i(ParentGlobal)], !IO),
    io.format("    child_global       = %d\n", [i(ChildGlobal)], !IO),
    io.format("    grandchild_global  = %d\n", [i(GrandChildGlobal)], !IO),
    set_parent_global(ParentGlobal + 1, !IO).
