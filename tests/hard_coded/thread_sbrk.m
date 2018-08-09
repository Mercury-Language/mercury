%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

% This program performs Mercury allocations in one thread (e.g. using Boehm GC)
% and simulates malloc calls in another thread that indirectly call sbrk.
% If both allocators use sbrk and are invoked simulataneously then
% memory corruption can result.

:- module thread_sbrk.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module thread.
:- import_module thread.semaphore.

:- pragma foreign_decl("C", "
#ifdef MR_HAVE_SBRK
    #include <unistd.h>
#endif
").

:- type tree
    --->    nil
    ;       node(int, tree, tree).

%---------------------------------------------------------------------------%

main(!IO) :-
    ( can_spawn_native ->
        semaphore.init(Sem, !IO),
        thread.spawn_native(alloc_thread(Sem), _, !IO),
        semaphore.wait(Sem, !IO),
        sbrk_loop(Sem, !IO),
        io.write_string("done.\n", !IO)
    ;
        io.write_string("spawn_native not supported.\n", !IO)
    ).

:- pred sbrk_loop(semaphore::in, io::di, io::uo) is det.

sbrk_loop(Sem, !IO) :-
    % io.write_string("sbrk thread\n", !IO),
    semaphore.try_wait(Sem, Success, !IO),
    (
        Success = yes
    ;
        Success = no,
        % It is hard to trigger a crash by calling malloc because not every
        % call ends up calling sbrk.  Therefore we call sbrk directly.
        sbrk(0x100, !IO),
        sbrk_loop(Sem, !IO)
    ).

:- pred sbrk(int::in, io::di, io::uo) is det.

sbrk(_, !IO).

:- pragma foreign_proc("C",
    sbrk(Increment::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#ifdef MR_HAVE_SBRK
    sbrk(Increment);
#endif
").

:- pred alloc_thread(semaphore::in, thread::in, io::di, io::uo) is cc_multi.

alloc_thread(Sem, _Thread, !IO) :-
    semaphore.signal(Sem, !IO),
    alloc_loop(Sem, 1, !IO).

:- pred alloc_loop(semaphore::in, int::in, io::di, io::uo) is cc_multi.

alloc_loop(Sem, Depth, !IO) :-
    % io.write_string("alloc thread\n", !IO),
    ( Depth > 20 ->
        semaphore.signal(Sem, !IO)
    ;
        build(Depth, T, 0, _Id),
        io.format("depth %d, size %d\n", [i(Depth), i(size(T))], !IO),
        alloc_loop(Sem, Depth + 1, !IO)
    ).

:- pred build(int::in, tree::out, int::in, int::out) is det.

build(Depth, T, Id0, Id) :-
    ( Depth = 1 ->
        T = nil,
        Id = Id0
    ;
        build(Depth - 1, L, Id0 + 1, Id2),
        build(Depth - 1, R, Id2, Id),
        T = node(Id0, L, R)
    ).

:- func size(tree) = int.

size(nil) = 1.
size(node(_, L, R)) = 1 + size(L) + size(R).
