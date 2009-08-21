%-----------------------------------------------------------------------------%
% Regression test.
% The compiler aborted on this module with --trace shallow.
%
% Software Error: continuation_info.m: Unexpected: find_typeinfos_for_tvars:
% can't find rval for type_info var TypeInfo_for_AtomicTask
%
%-----------------------------------------------------------------------------%

:- module livevars_shallow.
:- interface.

:- pred update({}::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- type net(AtomicTask)
    --->    net.

:- type task(AtomicTask)
    --->    task.

:- type task_type(AtomicTask)
    --->    atomik
    ;       composite.

%-----------------------------------------------------------------------------%

update(AtomicTasks) :-
    Net = net : net(int),
    classify_nodes(Net, AtomicTasks).

:- pred classify_nodes(net(AtomicTask)::in, {}::out) is det.

classify_nodes(_ : net(AtomicTask), AtomicTasks) :-
    Task = task : task(AtomicTask),
    TaskType = get_task_type(Task),
    (
        TaskType = atomik,
        AtomicTasks = {}
    ;
        TaskType = composite,
        AtomicTasks = {}
    ).

:- func get_task_type(task(AtomicTask)) = task_type(AtomicTask).

get_task_type(_) = atomik.

%-----------------------------------------------------------------------------%
% vim: set sts=4 sw=4 et:
