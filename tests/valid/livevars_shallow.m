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

:- import_module list.

:- type node(A)
    --->    place
    ;       task
    .

:- type task_type(A)
    --->    atomick
    ;       composite.

:- pred cancellation_set_updates(list(node(A))::in, list(string)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

cancellation_set_updates(Nodes, L) :-
    classify_nodes(Nodes, L).

:- pred classify_nodes(list(node(A))::in, list(string)::out) is det.

classify_nodes([], []).
classify_nodes([Node | Nodes], L) :-
    (
        Node = place,
        classify_nodes(Nodes, L)
    ;
        Node = task,
        classify_nodes(Nodes, L0),
        TaskType = get_task_type(Node),
        (
            TaskType = atomick,
            L = L0
        ;
            TaskType = composite,
            L = L0
        )
    ).

:- func get_task_type(node(A)) = task_type(A).

get_task_type(_) = composite.

%-----------------------------------------------------------------------------%
% vim: set sts=4 sw=4 et:
