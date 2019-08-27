%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 2004-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prof_info.m
% Main author: petdr.
%
% Declare the main data structures for mercury.profile and their access
% predicates, the actual types are exported as well.  This is because some
% predicates need to access entire data structure.
% XXX Should maybe changed at a later date.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module prof_info.
:- interface.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

    % prof: Data structure which contains ALL the relevant info for use
    % in generating the output.
:- type prof.

    % Maps label names to label addresses.
:- type addrdecl    ==  map(string, int).

    % Maps label addresses to prof nodes.
:- type prof_node_map   ==  map(int, prof_node).

    % Maps predicate names to the cycle the predicate is in.
:- type cycle_map   ==  map(string, int).

    % prof_node: Contains all the info used for output, for a single pred.
:- type prof_node.

:- type pred_info.

:- type prof_node_type
    --->    predicate
    ;       cycle.

%---------------------------------------------------------------------------%

    % Initialise prof predicates.
    %
:- func prof_node_init(string) = prof_node.

:- func prof_node_init_cycle(string, int, int, float, list(pred_info), int,
    int) = prof_node.

    % Get prof_node from via predicate name.
    %
:- pred get_prof_node(string::in, addrdecl::in, prof_node_map::in,
    prof_node::out) is det.

:- pred update_prof_node(string::in, prof_node::in, addrdecl::in,
    prof_node_map::in, prof_node_map::out) is det.

%---------------------------------------------------------------------------%
%
% `prof' access predicates.
%

:- pred prof_get_entire(prof::in, float::out, string::out, int::out,
    addrdecl::out, prof_node_map::out, cycle_map::out) is det.

:- pred prof_get_addrdeclmap(prof::in, addrdecl::out) is det.
:- pred prof_get_profnodemap(prof::in, prof_node_map::out) is det.

%---------------------------------------------------------------------------%
%
% `prof' update predicates.
%

:- pred prof_set_entire(float::in, string::in, int::in, addrdecl::in,
    prof_node_map::in, cycle_map::in, prof::out) is det.

:- pred prof_set_profnodemap(prof_node_map::in, prof::in, prof::out) is det.
:- pred prof_set_cyclemap(cycle_map::in, prof::in, prof::out) is det.

%---------------------------------------------------------------------------%
%
% Special prof_node predicates.
%

:- pred prof_node_type(prof_node::in, prof_node_type::out) is det.

%---------------------------------------------------------------------------%
%
% Access Predicate for prof_node.
%

:- pred prof_node_get_entire_pred(prof_node::in, string::out, int::out,
    int::out, float::out, list(pred_info)::out, list(pred_info)::out,
    int::out, int::out, list(string)::out) is det.

:- pred prof_node_get_entire_cycle(prof_node::in, string::out, int::out,
    int::out, float::out, list(pred_info)::out, int::out, int::out) is det.

:- pred prof_node_get_pred_name(prof_node::in, string::out) is det.
:- pred prof_node_get_cycle_number(prof_node::in, int::out) is det.
:- pred prof_node_get_initial_counts(prof_node::in, int::out) is det.
:- pred prof_node_get_propagated_counts(prof_node::in, float::out) is det.
:- pred prof_node_get_parent_list(prof_node::in, list(pred_info)::out) is det.
:- pred prof_node_get_child_list(prof_node::in, list(pred_info)::out) is det.
:- pred prof_node_get_total_calls(prof_node::in, int::out) is det.
:- pred prof_node_get_self_calls(prof_node::in, int::out) is det.

%---------------------------------------------------------------------------%
%
% Update prof_node predicates.
%

:- pred prof_node_set_cycle_num(int::in, prof_node::in, prof_node::out) is det.

:- pred prof_node_set_initial_counts(int::in, prof_node::in, prof_node::out)
    is det.

:- pred prof_node_set_propagated_counts(float::in, prof_node::in,
    prof_node::out) is det.

:- pred prof_node_concat_to_parent(string::in, int::in,
    prof_node::in, prof_node::out) is det.

:- pred prof_node_concat_to_child(string::in, int::in,
    prof_node::in, prof_node::out) is det.

:- pred prof_node_set_total_calls(int::in,
    prof_node::in, prof_node::out) is det.

:- pred prof_node_set_self_calls(int::in,
    prof_node::in, prof_node::out) is det.

:- pred prof_node_concat_to_name_list(string::in,
    prof_node::in, prof_node::out) is det.

:- pred prof_node_concat_to_member(string::in, int::in,
    prof_node::in, prof_node::out) is det.

%---------------------------------------------------------------------------%
%
% Init predicates for pred_info.
%

:- pred pred_info_init(string::in, int::in, pred_info::out) is det.

%---------------------------------------------------------------------------%
%
% Access predicates for pred_info.
%

:- pred pred_info_get_entire(pred_info::in, string::out, int::out) is det.
:- pred pred_info_get_pred_name(pred_info::in, string::out) is det.
:- pred pred_info_get_counts(pred_info::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- type prof
    --->    prof(
                % Scaling factor.
                scaling_factor      :: float,

                % Units (Each profiling count is equivalent to Scale Units).
                units               :: string,

                % Total counts of the profile run.
                total_count         :: int,

                % Map between label name and label addr used to find key
                % to look up prof_node_map.
                addr_decl_map       :: addrdecl,

                % Map between label addresses and all the relevant data
                % about that predicate.
                prof_node_map       :: prof_node_map,

                % Map between predicate name and its cycle number.
                cycle_map           :: cycle_map
            ).

:- type prof_node
     --->   pred_node(
                % A node which consists of just one predicate.

                pred_name               :: string,
                pred_cycle_number       :: int,
                pred_self_counts        :: int,
                pred_propagated_counts  :: float,

                % Parent pred and the number of times it calls this predicate.
                % XXX
                pred_parent_list        :: list(pred_info),

                % Child pred and the number of times they are called
                % from this predicate.
                pred_child_list         :: list(pred_info),

                % Total count of times this predicate called.
                pred_total_calls        :: int,

                % Number of self recursive calls of this routine.
                pred_self_calls         :: int,

                % Alternative names for this predicate, e.g. labels with
                % different names but the same address.
                prd_name_list           :: list(string)
            )
    ;       cycle_node(
                % A node which is built up with more than one predicate
                % and is a cycle.

                cycle_name              :: string,
                cycle_cycle_number      :: int,
                cycle_self_counts       :: int,
                cycle_propagated_counts :: float,

                % Cycle members plus total calls to that predicate.
                % XXX
                cycle_members           :: list(pred_info),

                % Total count of times this predicate called.
                % XXX
                cycle_total_calls       :: int,

                % Number of calls to fellow cycle members.
                cycle_self_calls        :: int
        ).

:- type pred_info
    --->    pred_info(
                pred_info_name      :: string,  % predicate (label)
                pred_info_count     :: int      % count (to or from)
            ).

%---------------------------------------------------------------------------%
%
% Initialise predicates.
%

prof_node_init(PredName) =
    pred_node(PredName, 0, 0, 0.0, [], [], 0, 0, []).

prof_node_init_cycle(A, B, C, D, E, F, G) =
    cycle_node(A, B, C, D, E, F, G).

%---------------------------------------------------------------------------%

get_prof_node(Pred, AddrMap, ProfNodeMap, ProfNode) :-
    map.lookup(AddrMap, Pred, Key),
    map.lookup(ProfNodeMap, Key, ProfNode).

update_prof_node(Pred, ProfNode, AddrMap, !ProfNodeMap) :-
    map.lookup(AddrMap, Pred, Key),
    map.det_update(Key, ProfNode, !ProfNodeMap).

%---------------------------------------------------------------------------%
%
% Access prof predicates.
%

prof_get_entire(prof(A, B, C, D, E, F), A, B, C, D, E, F).

prof_get_addrdeclmap(Prof, X) :-
    X = Prof ^ addr_decl_map.
prof_get_profnodemap(Prof, X) :-
    X = Prof ^ prof_node_map.

%---------------------------------------------------------------------------%
%
% Update prof predicates.
%

prof_set_entire(A, B, C, D, E, F, prof(A, B, C, D, E, F)).

prof_set_profnodemap(X, !Prof) :-
    !Prof ^ prof_node_map := X.
prof_set_cyclemap(X, !Prof) :-
    !Prof ^ cycle_map := X.

%---------------------------------------------------------------------------%
%
% Special prof_node predicates.
%

prof_node_type(pred_node(_, _, _, _, _, _, _, _, _), predicate).
prof_node_type(cycle_node(_, _, _, _, _, _, _), cycle).

%---------------------------------------------------------------------------%
%
% Access prof_node predicates.
%

prof_node_get_entire_pred(pred_node(A,B,C,D,E,F,G,H,I),A,B,C,D,E,F,G,H,I).
prof_node_get_entire_pred(cycle_node(_,_,_,_,_,_,_),_,_,_,_,_,_,_,_,_) :-
    error("prof_node_get_entire_pred: not a pred\n").

prof_node_get_entire_cycle(cycle_node(A,B,C,D,E,F,G),A,B,C,D,E,F,G).
prof_node_get_entire_cycle(pred_node(_,_,_,_,_,_,_,_,_),_,_,_,_,_,_,_) :-
    error("prof_node_get_entire_cycle: not a cycle\n").

prof_node_get_pred_name(pred_node(Name, _, _, _, _, _, _, _, _), Name).
prof_node_get_pred_name(cycle_node(Name, _, _, _, _, _, _), Name).

prof_node_get_cycle_number(pred_node(_, Cycle, _, _, _, _, _, _, _), Cycle).
prof_node_get_cycle_number(cycle_node(_, Cycle, _, _, _, _, _), Cycle).

prof_node_get_initial_counts(pred_node(_, _, Count, _, _, _, _, _, _), Count).
prof_node_get_initial_counts(cycle_node(_, _, Count, _, _, _, _), Count).

prof_node_get_propagated_counts(pred_node(_, _, _, Count,_,_,_,_,_), Count).
prof_node_get_propagated_counts(cycle_node(_, _, _, Count, _, _, _), Count).

prof_node_get_parent_list(pred_node(_, _, _, _, PList, _, _, _, _), PList).
prof_node_get_parent_list(cycle_node(_, _, _, _, _, _, _), _) :-
    error("prof_node_get_parent_list: cycle_node has no parent list\n").

prof_node_get_child_list(pred_node(_, _, _, _, _, Clist, _, _, _), Clist).
prof_node_get_child_list(cycle_node(_, _, _, _, _, _, _), _) :-
    error("prof_node_get_child_list: cycle_node has no child list\n").

prof_node_get_total_calls(pred_node(_, _, _, _, _, _, Calls, _, _), Calls).
prof_node_get_total_calls(cycle_node(_, _, _, _, _, Calls, _), Calls).

prof_node_get_self_calls(pred_node(_, _, _, _, _, _, _, Calls, _), Calls).
prof_node_get_self_calls(cycle_node(_, _, _, _, _, _, Calls), Calls).

%---------------------------------------------------------------------------%
%
% Update prof_node predicates.
%

prof_node_set_cycle_num(Cycle, pred_node(A, _, C, D, E, F, G, H, I),
    pred_node(A, Cycle, C, D, E, F, G, H, I)).
prof_node_set_cycle_num(Cycle, cycle_node(A, _, C, D, E, F, G),
    cycle_node(A, Cycle, C, D, E, F, G)).

prof_node_set_initial_counts(Count, pred_node(A, B, _, D, E, F, G, H, I),
    pred_node(A, B, Count, D, E, F, G, H, I)).
prof_node_set_initial_counts(Count, cycle_node(A, B, _, D, E, F, G),
    cycle_node(A, B, Count, D, E, F, G)).

prof_node_set_propagated_counts(Count, pred_node(A, B, C, _, E, F, G, H, I),
     pred_node(A, B, C, Count, E, F, G, H, I)).
prof_node_set_propagated_counts(Count, cycle_node(A, B, C, _, E, F, G),
     cycle_node(A, B, C, Count, E, F, G)).

prof_node_concat_to_parent(Name,Count, pred_node(A, B, C, D, PList, F, G, H, I),
    pred_node(A, B, C, D, [pred_info(Name,Count) | PList], F, G, H, I)).
prof_node_concat_to_parent(_, _, cycle_node(_, _, _, _, _, _, _), _) :-
    error("prof_node_concat_to_parent: cycle_node has no parents\n").

prof_node_concat_to_child(Name, Count, pred_node(A, B, C, D, E, CList, G, H, I),
    pred_node(A, B, C, D, E, [pred_info(Name,Count) | CList], G, H, I)).
prof_node_concat_to_child(_, _, cycle_node(_, _, _, _, _, _, _), _) :-
    error("prof_node_concat_to_child: cycle_node has no child\n").

prof_node_set_total_calls(Calls, pred_node(A, B, C, D, E, F, _, H, I),
    pred_node(A, B, C, D, E, F, Calls, H, I)).
prof_node_set_total_calls(Calls, cycle_node(A, B, C, D, E, _, G),
    cycle_node(A, B, C, D, E, Calls, G)).

prof_node_set_self_calls(Calls, pred_node(A, B, C, D, E, F, G, _, I),
    pred_node(A, B, C, D, E, F, G, Calls, I)).
prof_node_set_self_calls(Calls, cycle_node(A, B, C, D, E, F, _),
    cycle_node(A, B, C, D, E, F, Calls)).

prof_node_concat_to_name_list(Name, pred_node(A, B, C, D, E, F, G, H, NL),
    pred_node(A, B, C, D, E, F, G, H, [Name | NL])).
prof_node_concat_to_name_list(_, cycle_node(_, _, _, _, _, _, _), _) :-
    error("prof_node_concat_to_name_list: cycle_node has no namelist\n").

prof_node_concat_to_member(Name, Count, cycle_node(A, B, C, D, CList, F, G),
    cycle_node(A, B, C, D, [pred_info(Name,Count) | CList], F, G)).
prof_node_concat_to_member(_, _, pred_node(_, _, _, _, _, _, _, _, _), _) :-
    error("prof_node_concat_to_member: pred_node has no members\n").

%---------------------------------------------------------------------------%
%
% Init predicates for pred_info.

pred_info_init(Name, Count, pred_info(Name, Count)).

%---------------------------------------------------------------------------%
%
% Access predicates for pred_info.
%

pred_info_get_entire(pred_info(A, B), A, B).

pred_info_get_pred_name(Pred, X) :-
    X = Pred ^ pred_info_name.
pred_info_get_counts(Pred, X) :-
    X = Pred ^ pred_info_count.

%---------------------------------------------------------------------------%
:- end_module prof_info.
%---------------------------------------------------------------------------%
