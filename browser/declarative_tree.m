%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2008, 2010-2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: declarative_tree.m
% Author: Mark Brown
%
% This module defines an instance of mercury_edt/2, the debugging tree.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.declarative_tree.
:- interface.

:- import_module mdb.declarative_edt.
:- import_module mdb.declarative_execution.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.

%---------------------------------------------------------------------------%

    % The type of nodes in our implementation of EDTs. The parameter is meant
    % to be the type of references to trace nodes. In particular, the
    % references should be to trace nodes that could be considered nodes
    % in the EDT, namely those for exit, fail and exception events.
    %
:- type edt_node(R)
    --->    dynamic(R).

:- instance mercury_edt(wrap(S), edt_node(R)) <= annotated_trace(S, R).

    % The wrap/1 around the first argument of the instance is
    % required by the language.
    %
:- type wrap(S)
    --->    wrap(S).

:- pred trace_implicit_tree_info(wrap(S)::in, edt_node(R)::in,
    implicit_tree_info::out) is semidet <= annotated_trace(S, R).

:- pred edt_subtree_details(S::in, edt_node(R)::in, event_number::out,
    sequence_number::out, R::out) is det <= annotated_trace(S, R).

:- pred trace_atom_subterm_is_ground(trace_atom::in, arg_pos::in,
    term_path::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.declarative_debugger.
:- import_module mdb.io_action.
:- import_module mdb.term_rep.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.rtti_access.
:- import_module mdbcomp.sym_name.

:- import_module assoc_list.
:- import_module bool.
:- import_module deconstruct.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module unit.
:- import_module univ.

%---------------------------------------------------------------------------%

:- instance mercury_edt(wrap(S), edt_node(R)) <= annotated_trace(S, R)
    where [
        pred(edt_question/3) is trace_question,
        pred(edt_get_e_bug/3) is trace_get_e_bug,
        pred(edt_get_i_bug/4) is trace_get_i_bug,
        pred(edt_children/3) is trace_children,
        pred(edt_parent/3) is trace_last_parent,
        pred(edt_dependency/6) is trace_dependency,
        pred(edt_subterm_mode/5) is trace_subterm_mode,
        pred(edt_is_implicit_root/2) is trace_is_implicit_root,
        pred(edt_same_nodes/3) is trace_same_event_numbers,
        pred(edt_topmost_node/2) is trace_topmost_node,
        pred(edt_number_of_events/4) is trace_number_of_events,
        pred(edt_subtree_suspicion/4) is trace_subtree_suspicion,
        pred(edt_context/4) is trace_context,
        func(edt_proc_label/2) is trace_node_proc_label,
        func(edt_arg_pos_to_user_arg_num/3) is trace_arg_pos_to_user_arg_num
    ].

%---------------------------------------------------------------------------%

:- func exit_node_decl_atom(S::in,
    trace_node(R)::in(trace_node_exit)) = (final_decl_atom::out) is det
    <= annotated_trace(S, R).

exit_node_decl_atom(Store, ExitNode) = DeclAtom :-
    ExitAtom = get_trace_exit_atom(ExitNode),
    CallId = ExitNode ^ exit_call,
    call_node_from_id(Store, CallId, Call),
    CallIoSeq = Call ^ call_io_seq_num,
    ExitIoSeq = ExitNode ^ exit_io_seq_num,
    ( if CallIoSeq = ExitIoSeq then
        DeclAtom = final_decl_atom(ExitAtom, no)
    else
        DeclAtom = final_decl_atom(ExitAtom,
            yes(io_action_range(CallIoSeq, ExitIoSeq)))
    ).

:- func call_node_decl_atom(S, R) = init_decl_atom <= annotated_trace(S, R).

call_node_decl_atom(Store, CallId) = DeclAtom :-
    call_node_from_id(Store, CallId, CallNode),
    CallAtom = get_trace_call_atom(CallNode),
    DeclAtom = init_decl_atom(CallAtom).

:- pred get_edt_node_initial_atom(S::in, R::in, init_decl_atom::out)
    is det <= annotated_trace(S, R).

get_edt_node_initial_atom(Store, Ref, Atom) :-
    det_edt_return_node_from_id(Store, Ref, Node),
    (
        Node = node_exit(_, CallId, _, _, _, _, _, _),
        Atom = call_node_decl_atom(Store, CallId)
    ;
        Node = node_fail(_, CallId, _, _, _, _),
        Atom = call_node_decl_atom(Store, CallId)
    ;
        Node = node_excp(_, CallId, _, _, _, _, _),
        Atom = call_node_decl_atom(Store, CallId)
    ).

:- pred get_edt_node_event_number(S::in, R::in, event_number::out)
    is det <= annotated_trace(S, R).

get_edt_node_event_number(Store, Ref, Event) :-
    det_edt_return_node_from_id(Store, Ref, Node),
    ( Node = node_exit(_, _, _, _, Event, _, _, _)
    ; Node = node_fail(_, _, _, Event, _, _)
    ; Node = node_excp(_, _, _, _, Event, _, _)
    ).

%---------------------------------------------------------------------------%

:- pred trace_question(wrap(S)::in, edt_node(R)::in,
    decl_question(edt_node(R))::out) is det <= annotated_trace(S, R).

trace_question(wrap(Store), dynamic(Ref), Root) :-
    det_edt_return_node_from_id(Store, Ref, Node),
    (
        Node = node_fail(_, CallId, RedoId, _, _, _),
        DeclAtom = call_node_decl_atom(Store, CallId),
        get_answers(Store, RedoId, [], Answers),
        Root = missing_answer(dynamic(Ref), DeclAtom, Answers)
    ;
        Node = node_exit(_, CallId, _, _, _, _, _, _),
        InitDeclAtom = call_node_decl_atom(Store, CallId),
        FinalDeclAtom = exit_node_decl_atom(Store, Node),
        Root = wrong_answer(dynamic(Ref), InitDeclAtom, FinalDeclAtom)
    ;
        Node = node_excp(_, CallId, _, Exception, _, _, _),
        DeclAtom = call_node_decl_atom(Store, CallId),
        Root = unexpected_exception(dynamic(Ref), DeclAtom, Exception)
    ).

:- pred get_answers(S::in, R::in,
    list(final_decl_atom)::in, list(final_decl_atom)::out) is det
    <= annotated_trace(S, R).

get_answers(Store, RedoId, DeclAtoms0, DeclAtoms) :-
    ( if
        maybe_redo_node_from_id(Store, RedoId, node_redo(_, ExitId, _, _, _))
    then
        exit_node_from_id(Store, ExitId, ExitNode),
        NextId = ExitNode ^ exit_prev_redo,
        DeclAtom = exit_node_decl_atom(Store, ExitNode),
        get_answers(Store, NextId, [DeclAtom | DeclAtoms0], DeclAtoms)
    else
        DeclAtoms = DeclAtoms0
    ).

:- pred trace_get_e_bug(wrap(S)::in, edt_node(R)::in,
    decl_e_bug::out) is det <= annotated_trace(S, R).

trace_get_e_bug(wrap(Store), dynamic(Ref), Bug) :-
    det_edt_return_node_from_id(Store, Ref, Node),
    (
        Node = node_exit(_, CallId, _, _, Event, _, _, _),
        InitDeclAtom = call_node_decl_atom(Store, CallId),
        FinalDeclAtom = exit_node_decl_atom(Store, Node),
        get_exit_atoms_in_contour(Store, Node, Contour),
        Bug = incorrect_contour(InitDeclAtom, FinalDeclAtom, Contour, Event)
    ;
        Node = node_fail(_, CallId, _, Event, _, _),
        DeclAtom = call_node_decl_atom(Store, CallId),
        Bug = partially_uncovered_atom(DeclAtom, Event)
    ;
        Node = node_excp(_, CallId, _, Exception, Event, _, _),
        DeclAtom = call_node_decl_atom(Store, CallId),
        Bug = unhandled_exception(DeclAtom, Exception, Event)
    ).

:- pred trace_get_i_bug(wrap(S)::in, edt_node(R)::in,
    edt_node(R)::in, decl_i_bug::out) is det <= annotated_trace(S, R).

trace_get_i_bug(wrap(Store), dynamic(BugRef), dynamic(InadmissibleRef),
        inadmissible_call(BugAtom, unit, InadmissibleAtom, Event)) :-
    get_edt_node_initial_atom(Store, BugRef, BugAtom),
    get_edt_node_initial_atom(Store, InadmissibleRef, InadmissibleAtom),
    get_edt_node_event_number(Store, BugRef, Event).

    % Finding the parent of a node in the EDT from an EXIT event is in fact
    % not deterministic in the presence of backtracking, since one EXIT event
    % could belong to multiple children if it is in a call which is backtracked
    % over and each of these children could have different parents. We return
    % the last interface event of the parent CALL event as the parent. This is
    % OK since trace_last_parent is only used when an explicit subtree
    % is generated which is above the previous subtree, so it does not
    % really matter which parent we pick.
    %
:- pred trace_last_parent(wrap(S)::in, edt_node(R)::in, edt_node(R)::out)
    is semidet <= annotated_trace(S, R).

trace_last_parent(wrap(Store), dynamic(Ref), dynamic(Parent)) :-
    det_edt_return_node_from_id(Store, Ref, Node),
    ( Node = node_fail(_, CallId, _, _, _, _)
    ; Node = node_exit(_, CallId, _, _, _, _, _, _)
    ; Node = node_excp(_, CallId, _, _, _, _, _)
    ),
    call_node_from_id(Store, CallId, Call),
    CallPrecId = Call ^ call_preceding,
    step_left_to_call(Store, CallPrecId, ParentCallNode),
    Parent = ParentCallNode ^ call_last_interface.

:- pred trace_same_event_numbers(wrap(S)::in, edt_node(R)::in,
    edt_node(R)::in) is semidet <= annotated_trace(S, R).

trace_same_event_numbers(wrap(Store), dynamic(Ref1), dynamic(Ref2)) :-
    det_edt_return_node_from_id(Store, Ref1, Node1),
    det_edt_return_node_from_id(Store, Ref2, Node2),
    (
        Node1 = node_exit(_, _, _, _, Event, _, _, _),
        Node2 = node_exit(_, _, _, _, Event, _, _, _)
    ;
        Node1 = node_fail(_, _, _, Event, _, _),
        Node2 = node_fail(_, _, _, Event, _, _)
    ;
        Node1 = node_excp(_, _, _, _, Event, _, _),
        Node2 = node_excp(_, _, _, _, Event, _, _)
    ).

:- pred trace_topmost_node(wrap(S)::in, edt_node(R)::in) is semidet
    <= annotated_trace(S, R).

trace_topmost_node(wrap(Store), dynamic(Ref)) :-
    det_edt_return_node_from_id(Store, Ref, Node),
    ( Node = node_exit(_, CallId, _, _, _, _, _, _)
    ; Node = node_fail(_, CallId, _, _, _, _)
    ; Node = node_excp(_, CallId, _, _, _, _, _)
    ),
    % XXX This is buggy: see the io_read_bug test case.
    % The node is topmost if the call sequence number is 1.
    call_node_from_id(Store, CallId, node_call(_, _, _, 1, _, _, _, _, _, _)).

:- pred trace_children(wrap(S)::in, edt_node(R)::in, list(edt_node(R))::out)
    is semidet <= annotated_trace(S, R).

trace_children(wrap(Store), dynamic(Ref), Children) :-
    det_edt_return_node_from_id(Store, Ref, Node),
    (
        Node = node_fail(PrecId, CallId, _, _, _, _),
        not_at_depth_limit(Store, CallId),
        stratum_children(Store, PrecId, CallId, [], Children)
    ;
        Node = node_exit(PrecId, CallId, _, _, _, _, _, _),
        Atom = get_trace_exit_atom(Node),
        not_at_depth_limit(Store, CallId),
        ( if missing_answer_special_case(Atom) then
            stratum_children(Store, PrecId, CallId, [], Children)
        else
            contour_children(normal, Store, PrecId, CallId, [], Children)
        )
    ;
        Node = node_excp(PrecId, CallId, _, _, _, _, _),
        not_at_depth_limit(Store, CallId),
        contour_children(exception, Store, PrecId, CallId, [], Children)
    ).

:- pred trace_is_implicit_root(wrap(S)::in, edt_node(R)::in) is semidet
    <= annotated_trace(S, R).

trace_is_implicit_root(wrap(Store), dynamic(Ref)) :-
    get_edt_call_node(Store, Ref, CallId),
    not not_at_depth_limit(Store, CallId).

trace_implicit_tree_info(wrap(Store), dynamic(Ref), ImplicitTreeInfo) :-
    get_edt_call_node(Store, Ref, CallId),
    call_node_from_id(Store, CallId, CallNode),
    CallNode ^ call_at_max_depth = yes(ImplicitTreeInfo).

:- pred trace_number_of_events(wrap(S)::in, edt_node(R)::in, int::out,
    int::out) is det <= annotated_trace(S, R).

trace_number_of_events(Store, NodeId, Events, DuplicatedEvents) :-
    trace_weight(number_of_events, Store, NodeId, 0, Events, no, 0, 0,
        DuplicatedEvents).

:- pred trace_subtree_suspicion(wrap(S)::in, edt_node(R)::in, int::out,
    int::out) is det <= annotated_trace(S, R).

trace_subtree_suspicion(Store, NodeId, Suspicion, Excess) :-
    trace_weight(suspicion, Store, NodeId, 0, Suspicion, no, 0, 0, Excess).

    % trace_weight(Weighting, Store, Node, PrevWeight, Weight, RecordDups,
    %   DupFactor, PrevDupWeight, Excess):
    %
    % Calculate the difference between the value of a field in an EXIT,
    % FAIL or EXCP node and the same field in the corresponding CALL node
    % (the field that is used depends on the value of Weighting).
    % If Node is a FAIL or EXCP, then sum the differences between the first
    % CALL and the first EXIT, subsequent REDOs and EXITs and the final
    % REDO and FAIL/EXCP. If Node is a FAIL or EXCP, then all the previous
    % EXITs will be included in the EDT, and the subtrees rooted at these
    % EXITs will have common annotated trace nodes. Excess is the total
    % weight of all duplicated nodes. PrevWeight and PrevDupWeight are
    % accumulators which should initially be zero. RecordDups keeps track
    % of whether the final node was a FAIL or EXCP; this should be `no'
    % initially. DupFactor keeps track of how many times the nodes before
    % the last REDO could have been duplicated, and should initially be zero.
    %
:- pred trace_weight(weighting_heuristic::in, wrap(S)::in, edt_node(R)::in,
    int::in, int::out, bool::in, int::in, int::in, int::out)
    is det <= annotated_trace(S, R).

trace_weight(Weighting, wrap(Store), dynamic(Ref), PrevWeight, Weight,
        RecordDups, DupFactor, PrevDupWeight, Excess) :-
    det_trace_node_from_id(Store, Ref, Final),
    ( if
        (
            Final = node_exit(_, CallId, RedoId, _, FinalEvent, _, _,
                FinalSuspicion),
            NewRecordDups = RecordDups
        ;
            Final = node_fail(_, CallId, RedoId, FinalEvent, _,
                FinalSuspicion),
            NewRecordDups = yes
        ;
            Final = node_excp(_, CallId, RedoId, _, FinalEvent, _,
                FinalSuspicion),
            NewRecordDups = yes
        )
    then
        ( if
            maybe_redo_node_from_id(Store, RedoId, Redo),
            Redo = node_redo(_, ExitId, RedoEvent, _, RedoSuspicion)
        then
            (
                NewRecordDups = yes,
                (
                    Weighting = number_of_events,
                    NewPrevDupWeight = PrevDupWeight +
                        DupFactor * (FinalEvent - RedoEvent + 1)
                ;
                    Weighting = suspicion,
                    NewPrevDupWeight = PrevDupWeight +
                        DupFactor * (FinalSuspicion - RedoSuspicion)
                )
            ;
                NewRecordDups = no,
                NewPrevDupWeight = 0
            ),
            (
                Weighting = number_of_events,
                NewPrevWeight = PrevWeight + FinalEvent - RedoEvent + 1
            ;
                Weighting = suspicion,
                NewPrevWeight = PrevWeight + FinalSuspicion - RedoSuspicion
            ),
            trace_weight(Weighting, wrap(Store), dynamic(ExitId),
                NewPrevWeight, Weight, NewRecordDups,
                DupFactor + 1, NewPrevDupWeight, Excess)
        else
            call_node_from_id(Store, CallId, Call),
            CallEvent = Call ^ call_event,
            CallSuspicion = Call ^ call_suspicion,
            (
                Weighting = number_of_events,
                Weight = PrevWeight + FinalEvent - CallEvent + 1
            ;
                Weighting = suspicion,
                Weight = PrevWeight + FinalSuspicion - CallSuspicion
            ),
            (
                NewRecordDups = yes,
                (
                    Weighting = number_of_events,
                    Excess = PrevDupWeight +
                        DupFactor * (FinalEvent - CallEvent + 1)
                ;
                    Weighting = suspicion,
                    Excess = PrevDupWeight +
                        DupFactor * (FinalSuspicion - CallSuspicion)
                )
            ;
                NewRecordDups = no,
                Excess = 0
            )
        )
    else
        throw(internal_error($pred, "not a final event"))
    ).

:- pred trace_context(wrap(S)::in, edt_node(R)::in, pair(string, int)::out,
    maybe(pair(string, int))::out) is semidet <= annotated_trace(S, R).

trace_context(wrap(Store), dynamic(Ref), FileName - LineNo,
        MaybeReturnContext) :-
    det_trace_node_from_id(Store, Ref, Final),
    (
        Final = node_exit(_, CallId, _, _, _, Label, _, _)
    ;
        Final = node_fail(_, CallId, _, _, Label, _)
    ;
        Final = node_excp(_, CallId, _, _, _, Label, _)
    ),
    get_context_from_label_layout(Label, FileName, LineNo),
    call_node_from_id(Store, CallId, Call),
    (
        Call ^ call_return_label = yes(ReturnLabel),
        get_context_from_label_layout(ReturnLabel,
            ReturnFileName, ReturnLineNo),
        MaybeReturnContext = yes(ReturnFileName - ReturnLineNo)
    ;
        Call ^ call_return_label = no,
        MaybeReturnContext = no
    ).

:- pred missing_answer_special_case(trace_atom::in) is semidet.

missing_answer_special_case(Atom) :-
    ProcLabel = get_proc_label_from_layout(Atom ^ proc_layout),
    (
        ProcLabel = ordinary_proc_label(StdUtilModule1, pf_predicate,
            StdUtilModule2, "builtin_aggregate", 4, _)
    ;
        ProcLabel = ordinary_proc_label(StdUtilModule1, pf_predicate,
            StdUtilModule2, "builtin_aggregate2", 6, _)
    ),
    possible_sym_library_module_name("solutions", StdUtilModule1),
    possible_sym_library_module_name("solutions", StdUtilModule2).

:- pred possible_sym_library_module_name(string::in, module_name::out)
    is multi.

possible_sym_library_module_name(ModuleStr, unqualified(ModuleStr)).
possible_sym_library_module_name(ModuleStr, qualified(unqualified("library"),
    ModuleStr)).

:- pred not_at_depth_limit(S::in, R::in) is semidet <= annotated_trace(S, R).

not_at_depth_limit(Store, Ref) :-
    call_node_from_id(Store, Ref, CallNode),
    CallNode ^ call_at_max_depth = no.

:- func trace_node_proc_label(wrap(S), edt_node(R)) = proc_label
    <= annotated_trace(S, R).

trace_node_proc_label(wrap(Store), dynamic(Ref)) = ProcLabel :-
    det_edt_return_node_from_id(Store, Ref, Node),
    ( Node = node_fail(_, _, _, _, Label, _)
    ; Node = node_exit(_, _, _, _, _, Label, _, _)
    ; Node = node_excp(_, _, _, _, _, Label, _)
    ),
    ProcLayout = get_proc_layout_from_label_layout(Label),
    ProcLabel = get_proc_label_from_layout(ProcLayout).

:- type contour_type
    --->    normal
            % The contour ends with an EXIT event.

    ;       exception.
            % The contour ends with an EXCP event.

:- pred contour_children(contour_type::in, S::in, R::in, R::in,
    list(edt_node(R))::in, list(edt_node(R))::out)  is det
    <= annotated_trace(S, R).

contour_children(ContourType, Store, NodeId, StartId, Ns0, Ns) :-
    ( if NodeId = StartId then
        Ns = Ns0
    else
        contour_children_2(ContourType, Store, NodeId, StartId, Ns0, Ns)
    ).

:- pred contour_children_2(contour_type::in, S::in, R::in, R::in,
    list(edt_node(R))::in, list(edt_node(R))::out)  is det
    <= annotated_trace(S, R).

contour_children_2(ContourType, Store, NodeId, StartId, Ns0, Ns) :-
    det_trace_node_from_id(Store, NodeId, Node),
    (
        Node = node_call(_, _, _, _, _, _, _, _, _, _),
        throw(internal_error($pred, "unexpected start of contour"))
    ;
        Node = node_exit(_, _, _, _, _, _, _, _),
        % Add a child for this node.
        Ns1 = [dynamic(NodeId) | Ns0]
    ;
        Node = node_fail(_, CallId, _, _, _, _),

        % Fail events can be reached here if there were events missing
        % due to a parent being shallow traced. In this case, we cannot tell
        % whether the call was in a negated context or backtracked over,
        % so we have to assume the former.
        %
        % Fail events can also be reached here if the parent was a variant
        % of solutions/2.
        %
        % If this really is in a negated context, the start of the context
        % would be just before the entry to this failed call, modulo
        % any det/semidet code which succeeded.

        call_node_from_id(Store, CallId, Call),
        NestedStartId = Call ^ call_preceding,
        stratum_children(Store, NodeId, NestedStartId, Ns0, Ns1)
    ;
        Node = node_neg_fail(Prec, NestedStartId, _),

        % There is a nested context. Neg_fail events can be reached here
        % if there were events missing due to a parent being shallow traced.
        % In this case, we cannot tell whether the call was in a negated
        % context or backtracked over, so we have to assume the former.

        contour_children(ContourType, Store, Prec, NestedStartId, Ns0, Ns1)
    ;
        ( Node = node_else(Prec, NestedStartId, _)
        ; Node = node_neg_succ(Prec, NestedStartId, _)
        ),
        % There is a nested context.
        stratum_children(Store, Prec, NestedStartId, Ns0, Ns1)
    ;
        Node = node_excp(_, CallId, _, _, _, _, _),

        % If the contour ends in an exception, then add this exception
        % to the list of contour children and continue along the contour,
        % since in this case we are only interested in nodes that caused
        % the exception to be thrown.
        %
        % If the contour ends with an exit then the exception must have been
        % caught by a try/2 or try_all/3 or similar. In this case we want to
        % add all the exits of the call that threw the exception to the list
        % of children since one of the generated solutions may be incorrect.

        (
            ContourType = exception,
            Ns1 = [dynamic(NodeId) | Ns0]
        ;
            ContourType = normal,
            call_node_from_id(Store, CallId, Call),
            NestedStartId = Call ^ call_preceding,
            stratum_children(Store, NodeId, NestedStartId, Ns0, Ns1)
        )
    ;
        ( Node = node_redo(_, _, _, _, _)
        ; Node = node_switch(_, _)
        ; Node = node_first_disj(_, _)
        ; Node = node_later_disj(_, _, _)
        ; Node = node_then(_, _, _)
        ),

        % We skip neg_succ nodes for the same reason that we skip exit nodes
        % This handles the following cases: redo, switch, first_disj,
        % later_disj, and then. Also handles cond when the status is anything
        % other than failed.
        %
        % Redo events can be reached here if there were missing events
        % due to a shallow tracing. In this case, we have to scan over
        % the entire previous contour, since there is no way to tell
        % how much of it was backtracked over.

        Ns1 = Ns0
    ;
        Node = node_cond(_, _, CondStatus),
        (
            CondStatus = failed,
            throw(internal_error($pred, "unexpected start of contour"))
        ;
            ( CondStatus = succeeded
            ; CondStatus = undecided
            ),
            Ns1 = Ns0
        )
    ;
        Node = node_neg(_, _, NegStatus),
        (
            ContourType = normal,
            throw(internal_error($pred, "unexpected start of contour"))
        ;
            ContourType = exception,
            (
                NegStatus = failed,
                throw(internal_error($pred, "unexpected start of contour"))
            ;
                ( NegStatus = succeeded
                ; NegStatus = undecided
                ),
                % A non-failed NEGE could be encountered when gathering
                % the children of an exception node, since the exception
                % may have been thrown inside the negation.
                Ns1 = Ns0
            )
        )
    ),
    Next = step_left_in_contour(Store, Node),
    contour_children(ContourType, Store, Next, StartId, Ns1, Ns).

:- pred stratum_children(S::in, R::in, R::in, list(edt_node(R))::in,
    list(edt_node(R))::out) is det <= annotated_trace(S, R).

stratum_children(Store, NodeId, StartId, Ns0, Ns) :-
    ( if NodeId = StartId then
        Ns = Ns0
    else
        stratum_children_2(Store, NodeId, StartId, Ns0, Ns)
    ).

:- pred stratum_children_2(S::in, R::in, R::in, list(edt_node(R))::in,
    list(edt_node(R))::out) is det <= annotated_trace(S, R).

stratum_children_2(Store, NodeId, StartId, Ns0, Ns) :-
    det_trace_node_from_id(Store, NodeId, Node),
    (
        ( Node = node_call(_, _, _, _, _, _, _, _, _, _)
        ; Node = node_neg(_, _, _)
        ),
        throw(internal_error($pred, "unexpected start of contour"))
    ;
        ( Node = node_fail(_, _, _, _, _, _)
        ; Node = node_excp(_, _, _, _, _, _, _)
        ),
        % Add a child for this node.
        Ns1 = [dynamic(NodeId) | Ns0]
    ;
        Node = node_neg_fail(Prec, NestedStartId, _),
        % There is a nested successful context.
        contour_children(normal, Store, Prec, NestedStartId, Ns0, Ns1)
    ;
        Node = node_else(Prec, NestedStartId, _),
        % There is a nested failed context.
        stratum_children(Store, Prec, NestedStartId, Ns0, Ns1)
    ;
        Node = node_exit(_, CallId, _, _, _, _, _, _),
        % Only include an exit node as a missing answer child if it
        % produces output. If the exit event does not produce output,
        % then the only way the call could have behaved differently
        % is by failing, which won't change the fail, negs or else event
        % anchoring the end of the current stratum, since the rest of the goal
        % failed anyway.

        ( if calls_arguments_are_all_ground(Store, CallId) then
            Ns1 = Ns0
        else
            Ns1 = [dynamic(NodeId) | Ns0]
        )
    ;
        ( Node = node_redo(_, _, _, _, _)
        ; Node = node_switch(_, _)
        ; Node = node_first_disj(_, _)
        ; Node = node_later_disj(_, _, _)
        ; Node = node_then(_, _, _)
        ; Node = node_neg_succ(_, _, _)
        ),
        % We skip neg_succ nodes for the same reason that we skip exit nodes
        % where there are no outputs (see above).
        Ns1 = Ns0
    ;
        Node = node_cond(_, _, CondStatus),
        (
            ( CondStatus = succeeded
            ; CondStatus = undecided
            ),
            Ns1 = Ns0
        ;
            CondStatus = failed,
            throw(internal_error($pred, "unexpected start of contour"))
        )
    ),
    Next = step_in_stratum(Store, Node),
    stratum_children(Store, Next, StartId, Ns1, Ns).

%---------------------------------------------------------------------------%
%
% Tracking a subterm dependency.
%
% We are given an EDT node, an argument position, and a path to the selected
% subterm. We wish to find the origin of that subterm within the body of the
% given node, or within the body of its parent. We can figure out the mode of
% the top of the selected subterm.
%
% If the mode is `in', the origin could be:
%   - a primitive (unification or foreign_proc) within the body of the parent,
%   - an output subterm in a sibling node, or
%   - an input subterm of the parent node.
% In this case, we look at the contour leading up to the call event associated
% with the given node. This contour will be wholly within the parent call.
%
% If the mode is `out', the origin could be:
%   - a primitive (unification or foreign_proc) within the body of the call,
%   - an output subterm of a child of the node, or
%   - an input subterm of the node itself.
% In this case, we look at the contour leading up to the exit or exception event
% associated with the given node. This contour will be wholly within the
% current call.
%
% Our algorithm for finding the origin has three phases.
%
% In the first phase, we materialize a list of the nodes in the contour.
%
% In the second phase, we use this list of nodes to construct a list of the
% primitive goals along that contour in the body of the relevant procedure,
% leading up to either the call event (if subterm_mode is `in') or the exit
% event (if subterm_mode is `out').
%
% In the third phase, we traverse the list of primitive goals backwards, from
% the most recently executed primitive to the earliest one, keeping track of
% the variable which contains the selected subterm, and the location within
% this variable.

:- type dependency_chain_start(R)
    --->    chain_start(
                % The argument number of the selected position in the full list
                % of arguments, including the compiler-generated ones.
                start_loc(R),

                % The total number of arguments including the compiler
                % generated ones.
                int,

                % XXX document me
                int,

                % The id of the node preceding the exit node if start_loc
                % is cur_goal, and the id of the node preceding the call node
                % if start_loc is parent_goal.
                R,

                % No if start_loc is cur_goal; and yes wrapped around the
                % goal path of the call in the parent procedure if start_loc
                % is parent_goal.
                maybe(reverse_goal_path),

                % The body of the procedure indicated by start_loc.
                maybe(proc_defn_rep)
            )

    ;       require_explicit_subtree.
            % An explicit subtree is required before the
            % chain start can be calculated.

:- type start_loc(R)
    --->    cur_goal
    ;       parent_goal(R, trace_node(R)).

:- type goal_and_path
    --->    goal_and_path(goal_rep, reverse_goal_path).

:- type goal_and_path_list ==   list(goal_and_path).

:- type annotated_primitive(R)
    --->    primitive(
                string,             % filename
                int,                % line number
                list(var_rep),      % vars bound by the atomic goal
                atomic_goal_rep,    % the atomic goal itself
                reverse_goal_path,  % its goal path
                maybe(R)            % if the atomic goal is a call,
                                    % the id of the call's exit event
            ).

:- pred trace_subterm_mode(wrap(S)::in, edt_node(R)::in, arg_pos::in,
    term_path::in, subterm_mode::out) is det <= annotated_trace(S, R).

trace_subterm_mode(wrap(Store), dynamic(Ref), ArgPos, TermPath, Mode) :-
    find_chain_start(Store, Ref, ArgPos, TermPath, ChainStart),
    (
        ChainStart = chain_start(StartLoc, _, _, _, _, _),
        Mode = start_loc_to_subterm_mode(StartLoc)
    ;
        ChainStart = require_explicit_subtree,
        % The only time a subtree will be required is
        % if the mode of the subterm is output.
        Mode = subterm_out
    ).

:- pred trace_dependency(wrap(S)::in, edt_node(R)::in, arg_pos::in,
    term_path::in, subterm_mode::out, subterm_origin(edt_node(R))::out)
    is det <= annotated_trace(S, R).

trace_dependency(wrap(Store), dynamic(Ref), ArgPos, TermPath, Mode, Origin) :-
    find_chain_start(Store, Ref, ArgPos, TermPath, ChainStart),
    (
        ChainStart = chain_start(StartLoc, ArgNum, TotalArgs, NodeId,
            StartPath, MaybeProcDefnRep),
        Mode = start_loc_to_subterm_mode(StartLoc),
        (
            MaybeProcDefnRep = no,
            Origin = origin_not_found
        ;
            MaybeProcDefnRep = yes(ProcDefnRep),
            ( if
                trace_dependency_special_case(Store, ProcDefnRep, Ref,
                    StartLoc, ArgNum, TermPath, NodeId, Origin0)
            then
                Origin = Origin0
            else
                trace_dependency_in_proc_defn_rep(Store, TermPath, StartLoc,
                    ArgNum, TotalArgs, NodeId, StartPath, ProcDefnRep, Origin)
            )
        )
    ;
        ChainStart = require_explicit_subtree,
        Origin = origin_require_explicit_subtree,
        % The only time a subtree will be required is if the subterm is output.
        Mode = subterm_out
    ).

    % trace_dependency_special_case handles special cases not handled
    % by the usual subterm dependency tracking algorithm. At the moment
    % it handles tracking of subterms through catch_impl.
    %
:- pred trace_dependency_special_case(S::in, proc_defn_rep::in, R::in,
    start_loc(R)::in, int::in, term_path::in, R::in,
    subterm_origin(edt_node(R))::out) is semidet <= annotated_trace(S, R).

trace_dependency_special_case(Store, ProcDefnRep, Ref, StartLoc,
        ArgNum, TermPath, NodeId, Origin) :-
    % Catch_impl's body is a single call to builtin_catch. Builtin_catch
    % does not generate any events, so we need to handle catch_impl specially.

    proc_defn_rep_is_catch_impl(ProcDefnRep),
    (
        StartLoc = parent_goal(_, _),
        % The subterm being tracked is an input to builtin_catch so we know
        % the origin will be in the first argument of catch_impl, because
        % builtin_catch is only called from catch_impl.

        Origin = origin_input(user_head_var(1), [ArgNum | TermPath])
    ;
        StartLoc = cur_goal,
        % The subterm being tracked is an output of catch_impl so we know
        % its origin will be the output of the closure passed to try.
        % If the closure succeeded, then we continue to track the subterm
        % in the child call to exception.wrap_success_or_failure, otherwise
        % we stop tracking at the catch_impl.
        % XXX In future we should track exception values to the throw
        % that created them.

        exit_node_from_id(Store, Ref, ExitNode),
        ExitAtom = get_trace_exit_atom(ExitNode),
        ExitAtom = atom(_, Args),
        list.det_index1(Args, ArgNum, TryResultArgInfo),
        TryResultArgInfo = arg_info(_, _, yes(TryResultRep)),
        rep_to_univ(TryResultRep, TryResultUniv),
        univ_value(TryResultUniv) = TryResult,
        deconstruct(TryResult, canonicalize, Functor, _, _),
        ( if Functor = "succeeded" then
            Origin = origin_output(dynamic(NodeId), any_head_var_from_back(1),
                TermPath)
        else
            Origin = origin_primitive_op("exception.m", 0, primop_builtin_call)
        )
    ).

:- pred trace_dependency_in_proc_defn_rep(S::in, term_path::in,
    start_loc(R)::in, int::in, int::in, R::in, maybe(reverse_goal_path)::in,
    proc_defn_rep::in, subterm_origin(edt_node(R))::out) is det
    <= annotated_trace(S, R).

trace_dependency_in_proc_defn_rep(Store, TermPath, StartLoc, ArgNum,
        TotalArgs, NodeId, StartPath, ProcDefnRep, Origin) :-
    det_trace_node_from_id(Store, NodeId, Node),
    materialize_contour(Store, NodeId, Node, [], Contour0),
    (
        StartLoc = parent_goal(CallId, CallNode),
        Contour = list.append(Contour0, [CallId - CallNode])
    ;
        StartLoc = cur_goal,
        Contour = Contour0
    ),
    HeadVars = list.map(head_var_to_var, ProcDefnRep ^ pdr_head_vars),
    GoalRep = ProcDefnRep ^ pdr_goal,
    is_traced_grade(AllTraced),
    MaybePrims = make_primitive_list(Store,
        [goal_and_path(GoalRep, rgp_nil)], Contour, StartPath, ArgNum,
        TotalArgs, HeadVars, AllTraced, []),
    (
        MaybePrims = yes(primitive_list_and_var(Primitives, Var,
            MaybeClosure)),
        % If the subterm is in a closure argument, then the argument number
        % of the closure argument is prefixed to the term path, since the
        % closure is itself a term. This is done here because at the time
        % of the closure call it is not easy to decide if the call is higher
        % order or not, without repeating all the work done in
        % make_primitive_list.
        (
            MaybeClosure = yes,
            AdjustedTermPath = [ArgNum | TermPath]
        ;
            MaybeClosure = no,
            AdjustedTermPath = TermPath
        ),
        traverse_primitives(Primitives, Var, AdjustedTermPath, Store,
            ProcDefnRep, Origin)
    ;
        MaybePrims = no,
        Origin = origin_not_found
    ).

    % proc_defn_rep_is_catch_impl(ProcDefnRep) is true if ProcDefnRep
    % is a representation of exception.catch_impl (the converse is true
    % assuming exception.builtin_catch is only called from
    % exception.catch_impl).
    %
:- pred proc_defn_rep_is_catch_impl(proc_defn_rep::in) is semidet.

proc_defn_rep_is_catch_impl(ProcDefnRep) :-
    GoalRep = ProcDefnRep ^ pdr_goal,
    HeadVars = list.map(head_var_to_var, ProcDefnRep ^ pdr_head_vars),
    GoalExprRep = GoalRep ^ goal_expr_rep,
    HeadVars = [A, B, C, D],
    GoalExprRep = atomic_goal_rep("exception.m", _, [D],
        plain_call_rep("exception", "builtin_catch", [A, B, C, D])).

:- pred find_chain_start(S::in, R::in, arg_pos::in, term_path::in,
    dependency_chain_start(R)::out) is det <= annotated_trace(S, R).

find_chain_start(Store, Ref, ArgPos, TermPath, ChainStart) :-
    det_edt_return_node_from_id(Store, Ref, Node),
    (
        Node = node_exit(_, CallId, _, _, _, _, _, _),
        ExitAtom = get_trace_exit_atom(Node),
        call_node_from_id(Store, CallId, CallNode),
        CallAtom = get_trace_call_atom(CallNode),
        ( if trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) then
            find_chain_start_inside(Store, CallId, CallNode,
                ArgPos, ChainStart)
        else if trace_atom_subterm_is_ground(ExitAtom, ArgPos, TermPath) then
            ( if not_at_depth_limit(Store, CallId) then
                find_chain_start_outside(CallNode, Node, ArgPos, ChainStart)
            else
                ChainStart = require_explicit_subtree
            )
        else
            throw(internal_error($pred, "unbound wrong answer term"))
        )
    ;
        Node = node_fail(_, CallId, _, _, _, _),
        call_node_from_id(Store, CallId, CallNode),
        CallAtom = get_trace_call_atom(CallNode),
        ( if trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) then
            find_chain_start_inside(Store, CallId, CallNode,
                ArgPos, ChainStart)
        else
            throw(internal_error($pred, "unbound missing answer term"))
        )
    ;
        Node = node_excp(_, CallId, _, _, _, _, _),
        call_node_from_id(Store, CallId, CallNode),
        CallAtom = get_trace_call_atom(CallNode),
        % XXX We do not yet handle tracking of the exception value.
        ( if trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) then
            find_chain_start_inside(Store, CallId, CallNode,
                ArgPos, ChainStart)
        else
            throw(internal_error($pred, "unbound exception term"))


        )
    ).

:- pred find_chain_start_inside(S::in, R::in,
    trace_node(R)::in(trace_node_call), arg_pos::in,
    dependency_chain_start(R)::out) is det <= annotated_trace(S, R).

find_chain_start_inside(Store, CallId, CallNode, ArgPos, ChainStart) :-
    CallPrecId = CallNode ^ call_preceding,
    CallAtom = get_trace_call_atom(CallNode),
    CallPathStr = get_goal_path_from_maybe_label(CallNode ^ call_return_label),
    rev_goal_path_from_string_det(CallPathStr, CallPath),
    StartLoc = parent_goal(CallId, CallNode),
    absolute_arg_num(ArgPos, CallAtom, ArgNum),
    TotalArgs = length(CallAtom ^ atom_args),
    StartId = CallPrecId,
    StartPath = yes(CallPath),
    parent_proc_defn_rep(Store, CallId, StartRep),
    ChainStart = chain_start(StartLoc, ArgNum, TotalArgs, StartId,
        StartPath, StartRep).

:- pred find_chain_start_outside(trace_node(R)::in(trace_node_call),
    trace_node(R)::in(trace_node_exit), arg_pos::in,
    dependency_chain_start(R)::out) is det.

find_chain_start_outside(CallNode, ExitNode, ArgPos, ChainStart) :-
    StartLoc = cur_goal,
    ExitAtom = get_trace_exit_atom(ExitNode),
    absolute_arg_num(ArgPos, ExitAtom, ArgNum),
    TotalArgs = length(ExitAtom ^ atom_args),
    StartId = ExitNode ^ exit_preceding,
    StartPath = no,
    call_node_maybe_proc_defn_rep(CallNode, StartRep),
    ChainStart = chain_start(StartLoc, ArgNum, TotalArgs, StartId,
        StartPath, StartRep).

:- pred parent_proc_defn_rep(S::in, R::in, maybe(proc_defn_rep)::out)
    is det <= annotated_trace(S, R).

parent_proc_defn_rep(Store, CallId, ProcDefnRep) :-
    call_node_from_id(Store, CallId, Call),
    CallPrecId = Call ^ call_preceding,
    ( if step_left_to_call(Store, CallPrecId, ParentCallNode) then
        call_node_maybe_proc_defn_rep(ParentCallNode, ProcDefnRep)
    else
        ProcDefnRep = no
    ).

    % Finds the call node of the parent of the given node.
    % Fails if the call node cannot be found because it was
    % not included in the annotated trace.
    %
:- pred step_left_to_call(S::in, R::in, trace_node(R)::out(trace_node_call))
    is semidet <= annotated_trace(S, R).

step_left_to_call(Store, NodeId, ParentCallNode) :-
    trace_node_from_id(Store, NodeId, Node),
    ( if Node = node_call(_, _, _, _, _, _, _, _, _, _) then
        ParentCallNode = Node
    else
        % We wish to step through negated contexts, so we handle NEGE
        % and COND events separately, since step_left_in_contour/2
        % will throw an exception if it reaches the boundary of a
        % negated context.

        ( if Node = node_neg(NegPrec, _, _) then
            PrevNodeId = NegPrec
        else if Node = node_cond(CondPrec, _, _) then
            PrevNodeId = CondPrec
        else
            PrevNodeId = step_left_in_contour(Store, Node)
        ),
        step_left_to_call(Store, PrevNodeId, ParentCallNode)
    ).

:- pred materialize_contour(S::in, R::in, trace_node(R)::in,
    assoc_list(R, trace_node(R))::in, assoc_list(R, trace_node(R))::out)
    is det <= annotated_trace(S, R).

materialize_contour(Store, NodeId, Node, Nodes0, Nodes) :-
    ( if Node = node_call(_, _, _, _, _, _, _, _, _, _) then
        Nodes = Nodes0
    else
        % We include NEGE and (possibly failed) COND events in the contour
        % so we can track input sub-terms through negated contexts.
        ( if Node = node_neg(NegPrec, _, _) then
            PrevNodeId = NegPrec
        else if Node = node_cond(CondPrec, _, _) then
            PrevNodeId = CondPrec
        else
            PrevNodeId = step_left_in_contour(Store, Node)
        ),
        det_trace_node_from_id(Store, PrevNodeId, PrevNode),
        ( if Node = node_then(_, _, _) then
            % The cond node is enough to tell us which way the if-then-else
            % went; the then node would just complicate the job of
            % make_primitive_list.
            Nodes1 = Nodes0
        else
            Nodes1 = [NodeId - Node | Nodes0]
        ),
        materialize_contour(Store, PrevNodeId, PrevNode, Nodes1, Nodes)
    ).

:- pred get_exit_atoms_in_contour(S::in,
    trace_node(R)::in(trace_node_exit),
    list(final_decl_atom)::out) is det <= annotated_trace(S, R).

get_exit_atoms_in_contour(Store, ExitNode, ExitAtoms) :-
    ExitPrecId = ExitNode ^ exit_preceding,
    det_trace_node_from_id(Store, ExitPrecId, ExitPrec),
    materialize_contour(Store, ExitPrecId, ExitPrec, [], Contour),
    list.filter_map(get_exit_atom(Store), Contour, ExitAtoms).

:- pred get_exit_atom(S::in, pair(R, trace_node(R))::in,
    final_decl_atom::out) is semidet <= annotated_trace(S, R).

get_exit_atom(Store, _ - Exit, FinalAtom) :-
    Exit = node_exit(_, _, _, _, _, _, _, _),
    FinalAtom = exit_node_decl_atom(Store, Exit).

:- type primitive_list_and_var(R)
    --->    primitive_list_and_var(
                primitives  :: list(annotated_primitive(R)),

                % The var_rep for the argument which holds the subterm
                % we are trying to find the origin of. If the subterm
                % is in one of the arguments that were passed to a closure
                % when the closure was created, then this will be the var_rep
                % for the variable containing the closure.
                var         :: var_rep,

                % Was the subterm inside a closure argument that was passed
                % in when the closure was created?
                closure     :: bool
            ).

    % Constructs a list of the primitive goals along the given contour
    % if it can. It might not be able to construct the list in the case
    % where there are higher order calls and we are not sure if everything
    % is traced, then there might be extra/missing events on the contour and
    % we need to make sure the primitive atomic goals match up with the
    % contour events, but in the case of higher order calls this is not
    % easily done as the name/module of the higher order call is not
    % available in the goal_rep. If we cannot construct the primitive list
    % reliably, then we return `no'. MaybeEnd is the goal path of the
    % call event that should be at the end of the contour for input subterms.
    %
:- func make_primitive_list(S, goal_and_path_list,
    assoc_list(R, trace_node(R)), maybe(reverse_goal_path), int, int,
    list(var_rep), bool, list(annotated_primitive(R)))
    = maybe(primitive_list_and_var(R)) <= annotated_trace(S, R).

make_primitive_list(Store, GoalPaths, Contour, MaybeEnd, ArgNum, TotalArgs,
        HeadVars, AllTraced, Primitives0) = MaybePrims :-
    ( if
        AllTraced = no,
        (
            next_goal_generates_internal_event(GoalPaths)
        ;
            GoalPaths = []
        )
    then
        % There may be extra exit and fail events in the contour if a call
        % to an untraced module was made, but then something in the untraced
        % module called something in a traced module.
        remove_leading_exit_fail_events(Contour, AdjustedContour)
    else
        AdjustedContour = Contour
    ),
    ( if
        AllTraced = no,
        contour_at_end_path(AdjustedContour, MaybeEnd),
        (
            next_goal_generates_internal_event(GoalPaths)
        ;
            GoalPaths = []
        )
    then
        % We were unable to identify the goal corresponding to this call
        % (it might have been a higher order call) so we return no to indicate
        % this. This is the safest thing to do when we are not sure
        % what has/has not been traced.
        MaybePrims = no
    else
        (
            GoalPaths = [goal_and_path(Goal, Path) | Tail],
            MaybePrims = match_goal_to_contour_event(Store, Goal, Path, Tail,
                AdjustedContour, MaybeEnd, ArgNum, TotalArgs, HeadVars,
                AllTraced, Primitives0)
        ;
            GoalPaths = [],
            decl_require(unify(AdjustedContour, []),
                "make_primitive_list", "nonempty contour at end"),
            decl_require(unify(MaybeEnd, no),
                "make_primitive_list", "found end when looking for call"),
            find_variable_in_args(HeadVars, ArgNum, TotalArgs, Var),
            MaybePrims = yes(primitive_list_and_var(Primitives0, Var, no))
        )
    ).

:- pred contour_at_end_path(assoc_list(R, trace_node(R))::in,
    maybe(reverse_goal_path)::in) is semidet.

contour_at_end_path([_ - Node], yes(EndPath)) :-
    Node = node_call(_, _, _, _, _, _, MaybeReturnLabel, _, _, _),
    CallPathStr = get_goal_path_from_maybe_label(MaybeReturnLabel),
    rev_goal_path_from_string_det(CallPathStr, CallPath),
    CallPath = EndPath.

:- pred next_goal_generates_internal_event(list(goal_and_path)::in) is semidet.

next_goal_generates_internal_event([goal_and_path(NextGoal, _) | _]) :-
    goal_generates_internal_event(NextGoal) = yes.

    % match_goal_to_contour_event(Store, Goal, Path, GoalPaths, Contour,
    %   MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced, Primitives0)
    %   = MaybePrims:
    %
    % Matches the given goal_rep to the first event in the contour for
    % all goal_reps except atomic goal reps which need to be handled
    % differently depending on whether everything is traced (AllTraced).
    % Returns Primitives0 appended to the end of the list of primitive goals
    % along the remaining contour. If it cannot match a higher order call
    % to a contour event and AllTraced is no, then it returns "no".
    %
:- func match_goal_to_contour_event(S, goal_rep, reverse_goal_path,
    goal_and_path_list, assoc_list(R, trace_node(R)), maybe(reverse_goal_path),
    int, int, list(var_rep), bool, list(annotated_primitive(R)))
    = maybe(primitive_list_and_var(R)) <= annotated_trace(S, R).

match_goal_to_contour_event(Store, Goal, Path, GoalPaths, Contour, MaybeEnd,
        ArgNum, TotalArgs, HeadVars, AllTraced, Primitives0) = MaybePrims :-
    Goal = goal_rep(GoalExpr, _, _),
    (
        GoalExpr = conj_rep(Conjs),
        add_paths_to_conjuncts(Conjs, Path, 1, ConjPaths),
        MaybePrims = make_primitive_list(Store, ConjPaths ++ GoalPaths,
            Contour, MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
            Primitives0)
    ;
        GoalExpr = scope_rep(InnerGoal, MaybeCut),
        InnerPath = rev_goal_path_add_at_end(Path, step_scope(MaybeCut)),
        InnerAndPath = goal_and_path(InnerGoal, InnerPath),
        MaybePrims = make_primitive_list(Store, [InnerAndPath | GoalPaths],
            Contour, MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
            Primitives0)
    ;
        GoalExpr = atomic_goal_rep(File, Line, BoundVars, AtomicGoal),
        GeneratesEvent = atomic_goal_generates_event_like_call(AtomicGoal),
        (
            GeneratesEvent = yes(AtomicGoalArgs),
            MaybePrims = match_atomic_goal_to_contour_event(Store, File, Line,
                BoundVars, AtomicGoal, AtomicGoalArgs, Path, GoalPaths,
                Contour, MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
                Primitives0)
        ;
            GeneratesEvent = no,
            Primitive = primitive(File, Line, BoundVars, AtomicGoal, Path, no),
            Primitives1 = [Primitive | Primitives0],
            MaybePrims = make_primitive_list(Store, GoalPaths, Contour,
                MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced, Primitives1)
        )
    ;
        GoalExpr = disj_rep(Disjs),
        ( if
            Contour = [_ - ContourHeadNode | ContourTail],
            ( ContourHeadNode = node_first_disj(_, Label)
            ; ContourHeadNode = node_later_disj(_, Label, _)
            ),
            DisjPathStr = get_goal_path_from_label_layout(Label),
            rev_goal_path_from_string_det(DisjPathStr, DisjPath),
            rev_goal_path_remove_last(DisjPath, DisjInitialPath, DisjLastStep),
            DisjInitialPath = Path,
            DisjLastStep = step_disj(N)
        then
            list.det_index1(Disjs, N, Disj),
            DisjAndPath = goal_and_path(Disj, DisjPath),
            MaybePrims = make_primitive_list(Store, [DisjAndPath | GoalPaths],
                ContourTail, MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
                Primitives0)
        else
            throw(internal_error($pred, "mismatch on disj"))
        )
    ;
        GoalExpr = switch_rep(_SwitchVar, _SwitchCanFail, Cases),
        ( if
            Contour = [_ - ContourHeadNode | ContourTail],
            ContourHeadNode = node_switch(_, Label),
            ArmPathStr = get_goal_path_from_label_layout(Label),
            rev_goal_path_from_string_det(ArmPathStr, ArmPath),
            rev_goal_path_remove_last(ArmPath, ArmInitialPath, ArmLastStep),
            ArmInitialPath = Path,
            ArmLastStep = step_switch(N, _)
        then
            list.det_index1(Cases, N, Case),
            Case = case_rep(_ConsId, _ConsIdArity, Arm),
            ArmAndPath = goal_and_path(Arm, ArmPath),
            MaybePrims = make_primitive_list(Store, [ArmAndPath | GoalPaths],
                ContourTail, MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
                Primitives0)
        else
            throw(internal_error($pred, "mismatch on switch"))
        )
    ;
        GoalExpr = ite_rep(Cond, Then, Else),
        ( if
            Contour = [_ - ContourHeadNode | ContourTail],
            ContourHeadNode = node_cond(_, Label, _),
            CondPathStr = get_goal_path_from_label_layout(Label),
            rev_goal_path_from_string_det(CondPathStr, CondPath),
            rev_goal_path_remove_last(CondPath, CondInitialPath, CondLastStep),
            CondInitialPath = Path,
            CondLastStep = step_ite_cond
        then
            ThenPath = rev_goal_path_add_at_end(Path, step_ite_then),
            CondAndPath = goal_and_path(Cond, CondPath),
            ThenAndPath = goal_and_path(Then, ThenPath),
            MaybePrims = make_primitive_list(Store,
                [CondAndPath, ThenAndPath | GoalPaths], ContourTail,
                MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced, Primitives0)
        else if
            Contour = [_ - ContourHeadNode | ContourTail],
            ContourHeadNode = node_else(_, ElseCondId, _),
            cond_node_from_id(Store, ElseCondId, CondNode),
            CondNode = node_cond(_, Label, _),
            CondPathStr = get_goal_path_from_label_layout(Label),
            rev_goal_path_from_string_det(CondPathStr, CondPath),
            rev_goal_path_remove_last(CondPath, CondInitialPath, CondLastStep),
            CondInitialPath = Path,
            CondLastStep = step_ite_cond
        then
            ElsePath = rev_goal_path_add_at_end(Path, step_ite_else),
            ElseAndPath = goal_and_path(Else, ElsePath),
            MaybePrims = make_primitive_list(Store, [ElseAndPath | GoalPaths],
                ContourTail, MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
                Primitives0)
        else
            throw(internal_error($pred, "mismatch on if-then-else"))
        )
    ;
        GoalExpr = negation_rep(NegGoal),
        ( if
            Contour = [_ - ContourHeadNode | ContourTail],
            ContourHeadNode = node_neg_succ(_, _, _)
        then
            % The negated goal cannot contribute any bindings.
            MaybePrims = make_primitive_list(Store, GoalPaths, ContourTail,
                MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced, Primitives0)
        else if
            Contour = [_ - ContourHeadNode | ContourTail],
            ContourHeadNode = node_neg(_, _, _)
        then
            % The end of the primitive list is somewhere inside
            % NegGoal.
            NegPath = rev_goal_path_add_at_end(Path, step_neg),
            NegAndPath = goal_and_path(NegGoal, NegPath),
            MaybePrims = make_primitive_list(Store, [NegAndPath], ContourTail,
                MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced, Primitives0)
        else
            throw(internal_error($pred, "mismatch on negation"))
        )
    ).

:- pred remove_leading_exit_fail_events(
    assoc_list(R, trace_node(R))::in,
    assoc_list(R, trace_node(R))::out) is det.

remove_leading_exit_fail_events([], []).
remove_leading_exit_fail_events(Contour0, Contour) :-
    Contour0 = [_ - ContourHeadNode | ContourTail],
    ( if
        ( ContourHeadNode = node_exit(_, _, _, _, _, _, _, _)
        ; ContourHeadNode = node_fail(_, _, _, _, _, _)
        )
    then
        remove_leading_exit_fail_events(ContourTail, Contour)
    else
        Contour = Contour0
    ).

    % Trys to match an atomic goal to the first event on the contour.
    % These should match if AllTraced = yes. If AllTraced = no, then
    % if the goal does not match the contour event (i.e. they are for
    % different predicates), then the goal will be treated as a primitive
    % operation with no children. The next atomic goal will then be tried
    % as a match for the first event on the contour. This will
    % continue until a non-atomic goal is reached, at which point all
    % events that could match atomic goals (exit and fail events) are
    % removed from the top of the contour. This strategy will work
    % best when untraced calls do not call traced modules (which seems
    % more likely for the majority of untraced calls).
    %
:- func match_atomic_goal_to_contour_event(S, string, int,
    list(var_rep), atomic_goal_rep, list(var_rep), reverse_goal_path,
    list(goal_and_path), assoc_list(R, trace_node(R)),
    maybe(reverse_goal_path), int, int, list(var_rep), bool,
    list(annotated_primitive(R))) = maybe(primitive_list_and_var(R))
    <= annotated_trace(S, R).

match_atomic_goal_to_contour_event(Store, File, Line, BoundVars, AtomicGoal,
        AtomicGoalArgs, Path, GoalPaths, Contour, MaybeEnd, ArgNum,
        TotalArgs, HeadVars, AllTraced, Primitives0) = MaybePrims :-
    ( if
        Contour = [_ - ContourHeadNode],
        MaybeEnd = yes(EndPath)
    then
        ( if
            ContourHeadNode = node_call(_, _, _, _, _, _,
                MaybeReturnLabel, _, _, _),
            Atom = get_trace_call_atom(ContourHeadNode),
            CallPathStr = get_goal_path_from_maybe_label( MaybeReturnLabel),
            rev_goal_path_from_string_det(CallPathStr, CallPath),
            CallPath = EndPath
        then
            ( if
                ( if
                    atomic_goal_identifiable(AtomicGoal) = yes(AtomicGoalId)
                then
                    atomic_goal_matches_atom(AtomicGoalId, Atom)
                else
                    AllTraced = yes
                )
            then
                ( if
                    % Test to see that the argument is not a closure argument
                    % (passed in when the closure was created).
                    ArgNum > TotalArgs - length(AtomicGoalArgs)
                then
                    find_variable_in_args(AtomicGoalArgs, ArgNum, TotalArgs,
                        Var),
                    MaybePrims = yes(
                        primitive_list_and_var(Primitives0, Var, no))
                else
                    % Perhaps this is a closure and the argument was passed in
                    % when the closure was created.
                    (
                        AtomicGoal = higher_order_call_rep(Closure, _),
                        Var = Closure,
                        MaybePrims = yes(
                            primitive_list_and_var(Primitives0, Var, yes))
                    ;
                        ( AtomicGoal = unify_construct_rep(_, _, _)
                        ; AtomicGoal = unify_deconstruct_rep(_, _, _)
                        ; AtomicGoal = partial_deconstruct_rep(_, _, _)
                        ; AtomicGoal = partial_construct_rep(_, _, _)
                        ; AtomicGoal = unify_assign_rep(_, _)
                        ; AtomicGoal = cast_rep(_, _)
                        ; AtomicGoal = unify_simple_test_rep(_, _)
                        ; AtomicGoal = pragma_foreign_code_rep(_)
                        ; AtomicGoal = method_call_rep(_, _, _)
                        ; AtomicGoal = plain_call_rep(_, _, _)
                        ; AtomicGoal = builtin_call_rep(_, _, _)
                        ; AtomicGoal = event_call_rep(_, _)
                        ),
                        throw(internal_error($pred,
                            "argument number mismatch"))
                    )
                )
            else
                (
                    AllTraced = yes,
                    throw(internal_error($pred, "name mismatch on call"))
                ;
                    AllTraced = no,
                    Primitive = primitive(File, Line, BoundVars, AtomicGoal,
                        Path, no),
                    Primitives1 = [Primitive|Primitives0],
                    MaybePrims = make_primitive_list(Store, GoalPaths, Contour,
                        MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
                        Primitives1)
                )
            )
        else
            throw(internal_error($pred, "goalpath mismatch on call"))
        )
    else
        (
            Contour = [ContourHeadId - ContourHeadNode | ContourTail],
            ( if Atom = get_trace_exit_atom(ContourHeadNode) then
                ( if
                    ( if
                        atomic_goal_identifiable(AtomicGoal) =
                            yes(AtomicGoalId)
                    then
                        atomic_goal_matches_atom(AtomicGoalId, Atom)
                    else
                        AllTraced = yes
                    )
                then
                    CallInfo = yes(ContourHeadId),
                    NewContour = ContourTail
                else
                    (
                        AllTraced = yes,
                        throw(internal_error($pred,
                            "atomic goal does not match exit event\n"))
                    ;
                        AllTraced = no,
                        CallInfo = no,
                        NewContour = Contour
                    )
                )
            else
                (
                    AllTraced = yes,
                    throw(internal_error($pred,
                        "atomic goal with no exit event "
                        ++ "when assuming all traced"))
                ;
                    AllTraced = no,
                    CallInfo = no,
                    NewContour = Contour
                )
            ),
            Primitive = primitive(File, Line, BoundVars, AtomicGoal, Path,
                CallInfo),
            Primitives1 = [Primitive | Primitives0],
            MaybePrims = make_primitive_list(Store, GoalPaths, NewContour,
                MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced, Primitives1)
        ;
            Contour = [],
            ( if
                MaybeEnd = no,
                AllTraced = no
            then
                Primitive = primitive(File, Line, BoundVars, AtomicGoal, Path,
                    no),
                Primitives1 = [Primitive | Primitives0],
                MaybePrims = make_primitive_list(Store, GoalPaths, [],
                    MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
                    Primitives1)
            else
                throw(internal_error($pred, "premature contour end"))
            )
        )
    ).

:- pred atomic_goal_matches_atom(atomic_goal_id::in, trace_atom::in)
    is semidet.

atomic_goal_matches_atom(AtomicGoalId, Atom) :-
    AtomicGoalId = atomic_goal_id(GoalModule, GoalName, GoalArity),
    ProcLabel = get_proc_label_from_layout(Atom ^ proc_layout),
    get_pred_attributes(ProcLabel, EventModule, EventName, _, _),
    EventArity = length(Atom ^ atom_args),
    GoalModule = sym_name_to_string(EventModule),
    EventName = GoalName,
    EventArity = GoalArity.

:- pred find_variable_in_args(list(var_rep)::in, int::in, int::in,
    var_rep::out) is det.

find_variable_in_args(Args, ArgNum, TotalArgs, Var) :-
    % We reverse the arg list in case this is an argument of a closure call
    % that is passed in at the time of the call.
    ( if list.index1(reverse(Args), TotalArgs - ArgNum + 1, FoundVar) then
        Var = FoundVar
    else
        throw(internal_error($pred, "arg not found"))
    ).

:- pred traverse_primitives(list(annotated_primitive(R))::in,
    var_rep::in, term_path::in, S::in, proc_defn_rep::in,
    subterm_origin(edt_node(R))::out) is det <= annotated_trace(S, R).

traverse_primitives([], Var0, TermPath0, _, ProcDefnRep, Origin) :-
    HeadVars = list.map(head_var_to_var, ProcDefnRep ^ pdr_head_vars),
    ArgPos = find_arg_pos(HeadVars, Var0),
    Origin = origin_input(ArgPos, TermPath0).
traverse_primitives([Prim | Prims], Var0, TermPath0, Store, ProcDefnRep,
        Origin) :-
    Prim = primitive(File, Line, BoundVars, AtomicGoal, _GoalPath,
        MaybeNodeId),
    (
        AtomicGoal = unify_construct_rep(_CellVar, _Cons, FieldVars),
        ( if list.member(Var0, BoundVars) then
            (
                TermPath0 = [],
                Origin = origin_primitive_op(File, Line, primop_unification)
            ;
                TermPath0 = [TermPathStep0 | TermPath],
                list.det_index1(FieldVars, TermPathStep0, Var),
                traverse_primitives(Prims, Var, TermPath, Store, ProcDefnRep,
                    Origin)
            )
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ;
        AtomicGoal = unify_deconstruct_rep(CellVar, _Cons, FieldVars),
        ( if list.member(Var0, BoundVars) then
            ( if list.index1_of_first_occurrence(FieldVars, Var0, Pos) then
                traverse_primitives(Prims, CellVar, [Pos | TermPath0],
                    Store, ProcDefnRep, Origin)
            else
                throw(internal_error($pred, "bad deconstruct"))
            )
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ;
        AtomicGoal = partial_deconstruct_rep(_, _, MaybeFieldVars),
        ( if
            list.member(Var0, BoundVars),
            TermPath0 = [TermPathStep0 | TermPath]
        then
            list.det_index1(MaybeFieldVars, TermPathStep0, MaybeVar),
            (
                MaybeVar = yes(Var),
                % This partial deconstruction bound the TermPathStep0'th
                % argument of Var0.
                traverse_primitives(Prims, Var, TermPath, Store, ProcDefnRep,
                    Origin)
            ;
                MaybeVar = no,
                % This partial deconstruction did not bind the TermPathStep0'th
                % argument, so continue looking for the unification which did.
                traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                    Origin)
            )
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ;
        AtomicGoal = partial_construct_rep(_, _, MaybeFieldVars),
        ( if list.member(Var0, BoundVars) then
            (
                TermPath0 = [],
                Origin = origin_primitive_op(File, Line, primop_unification)
            ;
                TermPath0 = [TermPathStep0 | TermPath],
                list.det_index1(MaybeFieldVars, TermPathStep0, MaybeVar),
                (
                    MaybeVar = yes(Var),
                    % The partial construction bound the TermPathStep0'th
                    % argument of Var0.
                    traverse_primitives(Prims, Var, TermPath, Store,
                        ProcDefnRep, Origin)
                ;
                    MaybeVar = no,
                    % We got to the construction which bound the outermost
                    % functor of Var0 without finding the unification which
                    % bound the TermPathStep0'th argument of that functor.
                    % So something has gone wrong.
                    throw(internal_error($pred, "input argument not found"))
                )
            )
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ;
        AtomicGoal = unify_assign_rep(ToVar, FromVar),
        % We handle assigns the same as we handle unsafe casts.
        ( if list.member(Var0, BoundVars) then
            decl_require(unify(Var0, ToVar), "traverse_primitives",
                "bad assign"),
            traverse_primitives(Prims, FromVar, TermPath0, Store, ProcDefnRep,
                Origin)
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ;
        AtomicGoal = cast_rep(ToVar, FromVar),
        % We handle casts the same as we handle assigns.
        ( if list.member(Var0, BoundVars) then
            decl_require(unify(Var0, ToVar), $pred, "bad unsafe_cast"),
            traverse_primitives(Prims, FromVar, TermPath0, Store, ProcDefnRep,
                Origin)
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ;
        AtomicGoal = pragma_foreign_code_rep(_Args),
        ( if list.member(Var0, BoundVars) then
            Origin = origin_primitive_op(File, Line, primop_foreign_proc)
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ;
        AtomicGoal = unify_simple_test_rep(_LVar, _RVar),
        ( if list.member(Var0, BoundVars) then
            throw(internal_error($pred, "bad test"))
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ;
        AtomicGoal = higher_order_call_rep(_, Args),
        traverse_call(BoundVars, File, Line, Args, MaybeNodeId, Prims,
            Var0, TermPath0, Store, ProcDefnRep, Origin)
    ;
        AtomicGoal = method_call_rep(_, _, Args),
        traverse_call(BoundVars, File, Line, Args, MaybeNodeId, Prims,
            Var0, TermPath0, Store, ProcDefnRep, Origin)
    ;
        AtomicGoal = plain_call_rep(Module, Name, Args),
        ( if
            list.member(Var0, BoundVars),
            plain_call_is_special_case(Module, Name, Args, NewVar)
        then
            traverse_primitives(Prims, NewVar, TermPath0, Store, ProcDefnRep,
                Origin)
        else
            traverse_call(BoundVars, File, Line, Args, MaybeNodeId,
                Prims, Var0, TermPath0, Store, ProcDefnRep, Origin)
        )
    ;
        AtomicGoal = builtin_call_rep(_, _, _),
        ( if list.member(Var0, BoundVars) then
            Origin = origin_primitive_op(File, Line, primop_builtin_call)
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ;
        AtomicGoal = event_call_rep(_, _),
        ( if list.member(Var0, BoundVars) then
            throw(internal_error($pred, "bad event"))
        else
            traverse_primitives(Prims, Var0, TermPath0, Store, ProcDefnRep,
                Origin)
        )
    ).

    % Some foreign calls, such as casts, are handled specially
    % to improve the accuracy of the subterm dependency tracking algorithm.
    %
:- pred plain_call_is_special_case(string::in, string::in, list(var_rep)::in,
    var_rep::out) is semidet.

plain_call_is_special_case(Module, Name, Args, NewVar) :-
    % builtin.cc_multi_equal is the same as a unification for the
    % purposes of subterm dependency tracking.
    Module = "builtin",
    Name = "cc_multi_equal",
    list.length(Args, 3),
    list.det_index1(Args, 2) = NewVar.

:- type plain_call_info
    --->    plain_call_info(
                file_name           :: string,
                line_number         :: int,
                flat_module_name    :: string,
                pred_name           :: string
            ).

:- pred traverse_call(list(var_rep)::in, string::in, int::in,
    list(var_rep)::in, maybe(R)::in, list(annotated_primitive(R))::in,
    var_rep::in, term_path::in, S::in, proc_defn_rep::in,
    subterm_origin(edt_node(R))::out) is det <= annotated_trace(S, R).

traverse_call(BoundVars, File, Line, Args, MaybeNodeId,
        Prims, Var, TermPath, Store, ProcDefnRep, Origin) :-
    ( if list.member(Var, BoundVars) then
        Pos = find_arg_pos(Args, Var),
        (
            MaybeNodeId = yes(NodeId),
            Origin = origin_output(dynamic(NodeId), Pos, TermPath)
        ;
            MaybeNodeId = no,
            Origin = origin_primitive_op(File, Line, primop_untraced_call)
        )
    else
        traverse_primitives(Prims, Var, TermPath, Store, ProcDefnRep, Origin)
    ).

%---------------------------------------------------------------------------%

:- pred add_paths_to_conjuncts(list(goal_rep)::in, reverse_goal_path::in,
    int::in, goal_and_path_list::out) is det.

add_paths_to_conjuncts([], _, _, []).
add_paths_to_conjuncts([Goal | Goals], ParentPath, N,
        [goal_and_path(Goal, Path) | GoalAndPaths]) :-
    Path = rev_goal_path_add_at_end(ParentPath, step_conj(N)),
    add_paths_to_conjuncts(Goals, ParentPath, N + 1, GoalAndPaths).

%---------------------------------------------------------------------------%

:- pred is_traced_grade(bool::out) is det.

:- pragma foreign_proc("C",
    is_traced_grade(TracingOn::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    #ifdef MR_EXEC_TRACE
        TracingOn = MR_YES;
    #else
        TracingOn = MR_NO;
    #endif
").

%---------------------------------------------------------------------------%

:- func start_loc_to_subterm_mode(start_loc(R)) = subterm_mode.

start_loc_to_subterm_mode(cur_goal) = subterm_out.
start_loc_to_subterm_mode(parent_goal(_, _)) = subterm_in.

%---------------------------------------------------------------------------%

:- func find_arg_pos(list(var_rep), var_rep) = arg_pos.

find_arg_pos(HeadVars, Var) = ArgPos :-
    find_arg_pos_from_back(HeadVars, Var, length(HeadVars), ArgPos).

:- pred find_arg_pos_from_back(list(var_rep)::in, var_rep::in, int::in,
    arg_pos::out) is det.

find_arg_pos_from_back([], _, _, _) :-
    throw(internal_error($pred, "empty list")).
find_arg_pos_from_back([HeadVar | HeadVars], Var, Pos, ArgPos) :-
    ( if HeadVar = Var then
        ArgPos = any_head_var_from_back(Pos)
    else
        find_arg_pos_from_back(HeadVars, Var, Pos - 1, ArgPos)
    ).

%---------------------------------------------------------------------------%

edt_subtree_details(Store, dynamic(Ref), Event, SeqNo, CallPreceding) :-
    det_edt_return_node_from_id(Store, Ref, Node),
    ( Node = node_exit(_, Call, _, _, Event, _, _, _)
    ; Node = node_fail(_, Call, _, Event, _, _)
    ; Node = node_excp(_, Call, _, _, Event, _, _)
    ),
    call_node_from_id(Store, Call, CallNode),
    SeqNo = CallNode ^ call_seq,
    CallPreceding = CallNode ^ call_preceding.

:- inst edt_return_node for trace_node/1
    --->    node_exit(ground, ground, ground, ground, ground, ground, ground,
                ground)
    ;       node_fail(ground, ground, ground, ground, ground, ground)
    ;       node_excp(ground, ground, ground, ground, ground, ground, ground).

:- pred det_edt_return_node_from_id(S::in, R::in,
    trace_node(R)::out(edt_return_node)) is det <= annotated_trace(S, R).

det_edt_return_node_from_id(Store, Ref, Node) :-
    ( if
        trace_node_from_id(Store, Ref, Node0),
        ( Node0 = node_exit(_, _, _, _, _, _, _, _)
        ; Node0 = node_fail(_, _, _, _, _, _)
        ; Node0 = node_excp(_, _, _, _, _, _, _)
        )
    then
        Node = Node0
    else
        throw(internal_error($pred, "not a return node"))
    ).

:- pred get_edt_call_node(S::in, R::in, R::out) is det
    <= annotated_trace(S, R).

get_edt_call_node(Store, Ref, CallId) :-
    ( if
        trace_node_from_id(Store, Ref, Node0),
        ( Node0 = node_exit(_, CallId0, _, _, _, _, _, _)
        ; Node0 = node_fail(_, CallId0, _, _, _, _)
        ; Node0 = node_excp(_, CallId0, _, _, _, _, _)
        )
    then
        CallId = CallId0
    else
        throw(internal_error($pred, "not a return node"))
    ).

%---------------------------------------------------------------------------%

trace_atom_subterm_is_ground(atom(_, Args), ArgPos, _) :-
    select_arg_at_pos(ArgPos, Args, ArgInfo),
    ArgInfo = arg_info(_, _, MaybeArg),
    MaybeArg = yes(_).

:- func trace_arg_pos_to_user_arg_num(wrap(S), edt_node(R), arg_pos) = int
    <= annotated_trace(S, R).

trace_arg_pos_to_user_arg_num(wrap(Store), dynamic(Ref), ArgPos) = ArgNum :-
    get_edt_call_node(Store, Ref, CallId),
    call_node_from_id(Store, CallId, Call),
    Atom = get_trace_call_atom(Call),
    user_arg_num(ArgPos, Atom, ArgNum).

:- pred calls_arguments_are_all_ground(S::in, R::in) is semidet
    <= annotated_trace(S, R).

calls_arguments_are_all_ground(Store, CallId) :-
    call_node_from_id(Store, CallId, Call),
    Args = Call ^ call_atom_args,

    % XXX The following will not work for partially instantiated arguments.
    all [Arg] (
        list.member(Arg, Args)
    =>
        Arg = arg_info(_, _, yes(_))
    ).

%---------------------------------------------------------------------------%

:- pred decl_require((pred)::in((pred) is semidet), string::in, string::in)
    is det.

decl_require(Goal, Loc, Msg) :-
    ( if call(Goal) then
        true
    else
        throw(internal_error(Loc, Msg))
    ).

%---------------------------------------------------------------------------%
:- end_module mdb.declarative_tree.
%---------------------------------------------------------------------------%
