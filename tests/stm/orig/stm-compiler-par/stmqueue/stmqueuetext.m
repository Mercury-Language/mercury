%------------------------------------------------------------------------------%
% stmqueuetext.m
% <lmika@csse.unimelb.edu.au>
% Mon Oct 15 21:16:42 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module stmqueuetext.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module stm_builtin.
:- import_module exception.
:- import_module univ.
:- import_module maybe.

:- type bqueue == stm_var(list(int)).

%------------------------------------------------------------------------------%

main(IO0, IO) :-
    stmqueuetext.init([1,2,3,4,5], BQueue, IO0, IO1),
    atomic [outer(IO1, IO2), inner(BLA0, BLA)] (
        put(BQueue, 8, BLA0, BLA)
    or_else
        BLA0 = BLA
    ),
    io.nl(IO2, IO).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
% stmqueue.m
% <lmika@csse.unimelb.edu.au>
% Mon Oct 15 21:02:31 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%   A blocking queue using software transactional memory for synchronisation
%
%------------------------------------------------------------------------------%

    % Creates a new bqueue value.
    %
:- pred init(list(int)::in, bqueue::out, io::di, io::uo) is det.


    % Takes an element from the head of the queue.  If queue is empty, the
    % thread will be blocked.
    %
:- pred take(bqueue::in, int::out, stm::di, stm::uo) is det.


    % Places an element at the tail of the queue.
    %
:- pred put(bqueue::in, int::in, stm::di, stm::uo) is det.


    % Atomically removes all elements in the queue.
    %
:- pred clear(bqueue::in, stm::di, stm::uo) is det.


    % Returns the list of all elements in the queue.
    %
:- pred to_list(bqueue::in, list(int)::out, stm::di, stm::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%


init(List, BQueue, !STM) :-
    new_stm_var(List, BQueue, !STM).


take(BQueue, Val, !STM) :-
    read_stm_var(BQueue, List, !STM),
    (
        List = [],
        retry(!.STM)
    ;
        List = [Val | Vals],
        write_stm_var(BQueue, Vals, !STM)
    ).


put(BQueue, Val, !STM) :-
    read_stm_var(BQueue, List0, !STM),
    List = List0 ++ [Val],
    write_stm_var(BQueue, List, !STM).


clear(BQueue, !STM) :-
    write_stm_var(BQueue, [], !STM).


to_list(BQueue, List, !STM) :-
    read_stm_var(BQueue, List, !STM).


%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
