%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This program checks if performing a commit on a thread causes any issues.
%
% The .exp file is for grades supporting native threads.
% The .exp2 file is for grades not supporting native threads.
%
%---------------------------------------------------------------------------%

:- module thread_commit.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module maybe.
:- import_module thread.
:- import_module thread.semaphore.

:- type tree
    --->    leaf(int)
    ;       branch(tree, tree).

main(!IO) :-
    ( if thread.can_spawn_native then
        semaphore.init(JoinSem, !IO),
        io.write_string("Spawning thread...\n", !IO),
        thread.spawn_native(thread_proc(JoinSem), SpawnRes, !IO),
        (
            SpawnRes = ok(_Thread),
            semaphore.wait(JoinSem, !IO),
            io.write_string("thread done.\n", !IO),
            % Repeat test on main thread so program doesn't exit immediately.
            io.write_string("Running test on main thread...\n", !IO),
            tree_test(!IO),
            io.write_string("done.\n", !IO)
        ;
            SpawnRes = error(Error),
            io.print_line(Error, !IO)
        )
    else
        io.write_string("spawn_native not supported.\n", !IO)
    ).

:- pred thread_proc(semaphore::in, thread::in, io::di, io::uo) is cc_multi.

thread_proc(JoinSem, _Thread, !IO) :-
    cc_multi_equal(!IO),
    io.write_string("Running test on thread...\n", !IO),
    tree_test(!IO),
    semaphore.signal(JoinSem, !IO).

:- pred tree_test(io::di, io::uo) is det.

tree_test(!IO) :-
    build_tree(10, Tree, 0, _Acc),
    ( if all_odd(Tree) then
        io.print_line("All ints are odd.", !IO)
    else
        io.print_line("Not all ints are odd.", !IO)
    ).

:- pred build_tree(int::in, tree::out, int::in, int::out) is det.

build_tree(Height, Tree, !Acc) :-
    ( if Height =< 0 then
        Tree = leaf(!.Acc),
        !:Acc = !.Acc + 2
    else
        build_tree(Height - 1, TreeA, !Acc),
        build_tree(Height - 2, TreeB, !Acc),
        Tree = branch(TreeA, TreeB)
    ).

:- pred all_odd(tree::in) is semidet.

all_odd(Tree) :-
    % This may perform a commit.
    all [Leaf] (
        leaf(Leaf, Tree)
    =>
        not(int.even(Leaf))
    ).

:- pred leaf(int::out, tree::in) is nondet.

leaf(Leaf, Tree) :-
    (
        Tree = leaf(Leaf)
    ;
        Tree = branch(TreeA, TreeB),
        ( leaf(Leaf, TreeA)
        ; leaf(Leaf, TreeB)
        )
    ).

%---------------------------------------------------------------------------%
