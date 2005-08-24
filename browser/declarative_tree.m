%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_tree.m
% Author: Mark Brown
%
% This module defines an instance of mercury_edt/2, the debugging tree.
%
%-----------------------------------------------------------------------------%

:- module mdb.declarative_tree.

:- interface.

:- import_module mdb.declarative_edt.
:- import_module mdb.declarative_execution.
:- import_module mdbcomp.program_representation.

	% The type of nodes in our implementation of EDTs.  The parameter
	% is meant to be the type of references to trace nodes.  In
	% particular, the references should be to trace nodes that could
	% be considered nodes in the EDT, namely those for exit, fail
	% and exception events.
	% 
:- type edt_node(R)
	--->	dynamic(R).

:- instance mercury_edt(wrap(S), edt_node(R)) <= annotated_trace(S, R).

	% The wrap/1 around the first argument of the instance is
	% required by the language.
	%
:- type wrap(S) ---> wrap(S).

:- pred edt_subtree_details(S::in, edt_node(R)::in, event_number::out,
	sequence_number::out, R::out) is det <= annotated_trace(S, R).

:- pred trace_atom_subterm_is_ground(trace_atom::in, arg_pos::in, 
	term_path::in) is semidet.

:- pred trace_implicit_tree_info(wrap(S)::in, edt_node(R)::in, 
	implicit_tree_info::out) is semidet <= annotated_trace(S, R).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.declarative_debugger.
:- import_module mdb.io_action.
:- import_module mdb.util.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.rtti_access.

:- import_module assoc_list.
:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module string.

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
		func(edt_arg_pos_to_user_arg_num/3) is
			trace_arg_pos_to_user_arg_num
	].

%-----------------------------------------------------------------------------%

:- func exit_node_decl_atom(S::in,
	trace_node(R)::in(trace_node_exit)) = (final_decl_atom::out) is det
	<= annotated_trace(S, R).

exit_node_decl_atom(Store, ExitNode) = DeclAtom :-
	ExitAtom = get_trace_exit_atom(ExitNode),
	CallId = ExitNode ^ exit_call,
	call_node_from_id(Store, CallId, Call),
	CallIoSeq = Call ^ call_io_seq_num,
	ExitIoSeq = ExitNode ^ exit_io_seq_num,
	(
		CallIoSeq = ExitIoSeq
	->
		DeclAtom = final_decl_atom(ExitAtom, no)
	;
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
		Node = exit(_, CallId, _, _, _, _, _, _),
		Atom = call_node_decl_atom(Store, CallId)
	;
		Node = fail(_, CallId, _, _, _, _),
		Atom = call_node_decl_atom(Store, CallId)
	;
		Node = excp(_, CallId, _, _, _, _, _),
		Atom = call_node_decl_atom(Store, CallId)
	).

:- pred get_edt_node_event_number(S::in, R::in, event_number::out)
	is det <= annotated_trace(S, R).

get_edt_node_event_number(Store, Ref, Event) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, _, _, _, Event, _, _, _)
	;
		Node = fail(_, _, _, Event, _, _)
	;
		Node = excp(_, _, _, _, Event, _, _)
	).

%-----------------------------------------------------------------------------%

:- pred trace_question(wrap(S)::in, edt_node(R)::in,
	decl_question(edt_node(R))::out) is det <= annotated_trace(S, R).

trace_question(wrap(Store), dynamic(Ref), Root) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = fail(_, CallId, RedoId, _, _, _),
		DeclAtom = call_node_decl_atom(Store, CallId),
		get_answers(Store, RedoId, [], Answers),
		Root = missing_answer(dynamic(Ref), DeclAtom, Answers)
	;
		Node = exit(_, CallId, _, _, _, _, _, _),
		InitDeclAtom = call_node_decl_atom(Store, CallId),
		FinalDeclAtom = exit_node_decl_atom(Store, Node),
		Root = wrong_answer(dynamic(Ref), InitDeclAtom, FinalDeclAtom)
	;
		Node = excp(_, CallId, _, Exception, _, _, _),
		DeclAtom = call_node_decl_atom(Store, CallId),
		Root = unexpected_exception(dynamic(Ref), DeclAtom, Exception)
	).

:- pred get_answers(S::in, R::in,
	list(final_decl_atom)::in, list(final_decl_atom)::out) is det
	<= annotated_trace(S, R).

get_answers(Store, RedoId, DeclAtoms0, DeclAtoms) :-
	(
		maybe_redo_node_from_id(Store, RedoId, redo(_, ExitId, _, _,
			_))
	->
		exit_node_from_id(Store, ExitId, ExitNode),
		NextId = ExitNode ^ exit_prev_redo,
		DeclAtom = exit_node_decl_atom(Store, ExitNode),
		get_answers(Store, NextId, [DeclAtom | DeclAtoms0], DeclAtoms)
	;
		DeclAtoms = DeclAtoms0
	).

:- pred trace_get_e_bug(wrap(S)::in, edt_node(R)::in,
	decl_e_bug::out) is det <= annotated_trace(S, R).

trace_get_e_bug(wrap(Store), dynamic(Ref), Bug) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, CallId, _, _, Event, _, _, _),
		InitDeclAtom = call_node_decl_atom(Store, CallId),
		FinalDeclAtom = exit_node_decl_atom(Store, Node),
		get_exit_atoms_in_contour(Store, Node, Contour),
		Bug = incorrect_contour(InitDeclAtom, FinalDeclAtom, Contour, 
			Event)
	;
		Node = fail(_, CallId, _, Event, _, _),
		DeclAtom = call_node_decl_atom(Store, CallId),
		Bug = partially_uncovered_atom(DeclAtom, Event)
	;
		Node = excp(_, CallId, _, Exception, Event, _, _),
		DeclAtom = call_node_decl_atom(Store, CallId),
		Bug = unhandled_exception(DeclAtom, Exception, Event)
	).

:- pred trace_get_i_bug(wrap(S)::in, edt_node(R)::in,
	edt_node(R)::in, decl_i_bug::out) is det <= annotated_trace(S, R).

trace_get_i_bug(wrap(Store), dynamic(BugRef), 
		dynamic(InadmissibleRef), inadmissible_call(BugAtom, unit,
			InadmissibleAtom, Event)) :-
	get_edt_node_initial_atom(Store, BugRef, BugAtom),
	get_edt_node_initial_atom(Store, InadmissibleRef, InadmissibleAtom),
	get_edt_node_event_number(Store, BugRef, Event).

	% Finding the parent of a node in the EDT from an EXIT event is
	% in actual fact not deterministic in the presence of backtracking,
	% since one EXIT event could belong to multiple children if it is in 
	% a call which is backtracked over and each of these children could
	% have different parents.  We return the last interface event of the
	% parent CALL event as the parent.  This is okay since
	% trace_last_parent is only used when an explicit subtree is generated
	% which is above the previous subtree, so it doesn't really matter
	% which parent we pick.
	%
:- pred trace_last_parent(wrap(S)::in, edt_node(R)::in, edt_node(R)::out) 
	is semidet <= annotated_trace(S, R).

trace_last_parent(wrap(Store), dynamic(Ref), dynamic(Parent)) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = fail(_, CallId, _, _, _, _)
	;
		Node = exit(_, CallId, _, _, _, _, _, _)
	;
		Node = excp(_, CallId, _, _, _, _, _)
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
		Node1 = exit(_, _, _, _, Event, _, _, _),
		Node2 = exit(_, _, _, _, Event, _, _, _)
	;
		Node1 = fail(_, _, _, Event, _, _),
		Node2 = fail(_, _, _, Event, _, _)
	;
		Node1 = excp(_, _, _, _, Event, _, _),
		Node2 = excp(_, _, _, _, Event, _, _)
	).

:- pred trace_topmost_node(wrap(S)::in, edt_node(R)::in) is semidet
	<= annotated_trace(S, R).

trace_topmost_node(wrap(Store), dynamic(Ref)) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, CallId, _, _, _, _, _, _)
	;
		Node = fail(_, CallId, _, _, _, _)
	;
		Node = excp(_, CallId, _, _, _, _, _)
	),
	% The node is topmost of the call sequence number is 1.
	call_node_from_id(Store, CallId, call(_, _, _, 1, _, _, _, _, _, _)).

:- pred trace_children(wrap(S)::in, edt_node(R)::in, list(edt_node(R))::out)
	is semidet <= annotated_trace(S, R).

trace_children(wrap(Store), dynamic(Ref), Children) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = fail(PrecId, CallId, _, _, _, _),
		not_at_depth_limit(Store, CallId),
		stratum_children(Store, PrecId, CallId, [], Children)
	;
		Node = exit(PrecId, CallId, _, _, _, _, _, _),
		Atom = get_trace_exit_atom(Node),
		not_at_depth_limit(Store, CallId),
		(
			missing_answer_special_case(Atom)
		->
			stratum_children(Store, PrecId, CallId, [], 
				Children)
		;
			contour_children(normal, Store, PrecId, CallId, 
				[], Children)
		)
	;
		Node = excp(PrecId, CallId, _, _, _, _, _),
		not_at_depth_limit(Store, CallId),
		contour_children(exception, Store, PrecId, CallId, [],
			Children)
	).

:- pred trace_is_implicit_root(wrap(S)::in, edt_node(R)::in) is semidet
	<= annotated_trace(S, R).

trace_is_implicit_root(wrap(Store), dynamic(Ref)) :-
	get_edt_call_node(Store, Ref, CallId),
	\+ not_at_depth_limit(Store, CallId).

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
	%	DupFactor, PrevDupWeight, Excess)
	% Calculate the difference between the value of a field in an EXIT,
	% FAIL or EXCP node and the same field in the corresponding CALL node
	% (the field that is used depends on the value of Weighting).  If Node
	% is a FAIL or EXCP, then sum the differences between the first
	% CALL and the first EXIT, subsequent REDOs and EXITs and the final
	% REDO and FAIL/EXCP.  If Node is a FAIL or EXCP then all the previous
	% EXITS will be included in the EDT and the subtrees rooted at these
	% EXITS will have common annotated trace nodes.  Excess is the total
	% weight of all duplicated nodes.  PrevWeight and PrevDupWeight are
	% accumulators which should initially be zero.  RecordDups keeps track
	% of whether the final node was a FAIL or EXCP. This should be `no'
	% initially.  DupFactor keeps track of how many times the nodes before
	% the last REDO could have been duplicated and should initially be
	% zero.
	%
:- pred trace_weight(weighting_heuristic::in, wrap(S)::in, edt_node(R)::in, 
	int::in, int::out, bool::in, int::in, int::in, int::out) 
	is det <= annotated_trace(S, R).

trace_weight(Weighting, wrap(Store), dynamic(Ref), PrevWeight, Weight,
		RecordDups, DupFactor, PrevDupWeight, Excess) :-
	det_trace_node_from_id(Store, Ref, Final),
	(
		(
			Final = exit(_, CallId, RedoId, _, FinalEvent, _, _, 
				FinalSuspicion),
			NewRecordDups = RecordDups
		;
			Final = fail(_, CallId, RedoId, FinalEvent, _, 
				FinalSuspicion),
			NewRecordDups = yes
		;
			Final = excp(_, CallId, RedoId, _, FinalEvent, _, 
				FinalSuspicion),
			NewRecordDups = yes
		)
	->
		(
			maybe_redo_node_from_id(Store, RedoId, Redo),
			Redo = redo(_, ExitId, RedoEvent, _, RedoSuspicion)
		->
			(
				NewRecordDups = yes,
				(
					Weighting = number_of_events,
					NewPrevDupWeight = PrevDupWeight +
						DupFactor * (FinalEvent -
							RedoEvent + 1)
				;
					Weighting = suspicion,
					NewPrevDupWeight = PrevDupWeight +
						DupFactor * (FinalSuspicion -
							RedoSuspicion)
				)
			;
				NewRecordDups = no,
				NewPrevDupWeight = 0
			),
			(
				Weighting = number_of_events,
				NewPrevWeight = PrevWeight + FinalEvent 
					- RedoEvent + 1
			;
				Weighting = suspicion,
				NewPrevWeight = PrevWeight + FinalSuspicion
					- RedoSuspicion
			),
			trace_weight(Weighting, wrap(Store), dynamic(ExitId),
				NewPrevWeight, Weight, NewRecordDups, 
				DupFactor + 1, NewPrevDupWeight, Excess)
		;
			call_node_from_id(Store, CallId, Call),
			CallEvent = Call ^ call_event,
			CallSuspicion = Call ^ call_suspicion,
			(
				Weighting = number_of_events,
				Weight = PrevWeight + FinalEvent - 
					CallEvent + 1
			;
				Weighting = suspicion,
				Weight = PrevWeight + FinalSuspicion -
					CallSuspicion
			),
			(
				NewRecordDups = yes,
				(
					Weighting = number_of_events,
					Excess = PrevDupWeight + DupFactor *
						(FinalEvent - CallEvent + 1)
				;
					Weighting = suspicion,
					Excess = PrevDupWeight + DupFactor *
						(FinalSuspicion 
							- CallSuspicion)
				)
			;
				NewRecordDups = no,
				Excess = 0
			)
		)
	;
		throw(internal_error("trace_weight", "not a final event"))
	).

:- pred trace_context(wrap(S)::in, edt_node(R)::in, pair(string, int)::out,
	maybe(pair(string, int))::out) is semidet <= annotated_trace(S, R).

trace_context(wrap(Store), dynamic(Ref), FileName - LineNo, MaybeReturnContext)
		:-
	det_trace_node_from_id(Store, Ref, Final),
	(
		Final = exit(_, CallId, _, _, _, Label, _, _)
	;
		Final = fail(_, CallId, _, _, Label, _)
	;
		Final = excp(_, CallId, _, _, _, Label, _)
	),
	get_context_from_label_layout(Label, FileName, LineNo),
	call_node_from_id(Store, CallId, Call),
	(
		Call ^ call_return_label = yes(ReturnLabel),
		get_context_from_label_layout(ReturnLabel, ReturnFileName,
			ReturnLineNo),
		MaybeReturnContext = yes(ReturnFileName - ReturnLineNo)
	;
		Call ^ call_return_label = no,
		MaybeReturnContext = no
	).

:- pred missing_answer_special_case(trace_atom::in) is semidet.

missing_answer_special_case(Atom) :-
	ProcLabel = get_proc_label_from_layout(Atom ^ proc_layout),
	ProcLabel = proc(StdUtilModule1, predicate, StdUtilModule2, 
		"builtin_aggregate", 4, _),
	possible_sym_library_module_name("std_util", StdUtilModule1),
	possible_sym_library_module_name("std_util", StdUtilModule2).

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
	(
		Node = fail(_, _, _, _, Label, _)
	;
		Node = exit(_, _, _, _, _, Label, _, _)
	;
		Node = excp(_, _, _, _, _, Label, _)
	),
	ProcLayout = get_proc_layout_from_label_layout(Label),
	ProcLabel = get_proc_label_from_layout(ProcLayout).

:- type contour_type
			% The contour ends with an EXIT event.
	--->	normal
			% The contour ends with an EXCP event.
	;	exception.

:- pred contour_children(contour_type::in, S::in, R::in, R::in, 
	list(edt_node(R))::in, list(edt_node(R))::out)  is det
	<= annotated_trace(S, R).

contour_children(ContourType, Store, NodeId, StartId, Ns0, Ns) :-
	(
		NodeId = StartId
	->
		Ns = Ns0
	;
		contour_children_2(ContourType, Store, NodeId, StartId, 
			Ns0, Ns)
	).

:- pred contour_children_2(contour_type::in, S::in, R::in, R::in, 
	list(edt_node(R))::in, list(edt_node(R))::out)  is det
	<= annotated_trace(S, R).

contour_children_2(ContourType, Store, NodeId, StartId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		( 
			Node = call(_, _, _, _, _, _, _, _, _, _)
		; 
			%
			% A non-failed NEGE could be encountered when gathering
			% the children of an exception node, since the
			% exception may have been thrown inside the negation.
			%
			(
				ContourType = normal,
				Node = neg(_, _, _)
			;
				ContourType = exception,
				Node = neg(_, _, failed)
			)
		; 
			Node = cond(_, _, failed)
		)
	->
		throw(internal_error("contour_children_2",
			"unexpected start of contour"))
	;
		Node = exit(_, _, _, _, _, _, _, _)
	->
			%
			% Add a child for this node.
			%
		Ns1 = [dynamic(NodeId) | Ns0]
	;
		Node = fail(_, CallId, _, _, _, _)
	->
			%
			% Fail events can be reached here if there
			% were events missing due to a parent being
			% shallow traced.  In this case, we can't tell
			% whether the call was in a negated context
			% or backtracked over, so we have to assume
			% the former.
			%
			% Fail events can also be reached here if the
			% parent was a variant of solutions/2.
			%
			% If this really is in a negated context, the start of
			% the context would be just before the entry to this
			% failed call, modulo any det/semidet code which
			% succeeded.
			%
		call_node_from_id(Store, CallId, Call),
		NestedStartId = Call ^ call_preceding,
		stratum_children(Store, NodeId, NestedStartId, Ns0, Ns1)
	;
		Node = neg_fail(Prec, NestedStartId, _)
	->
			%
			% There is a nested context.  Neg_fail events can be
			% reached here if there were events missing due to a
			% parent being shallow traced.  In this case, we can't
			% tell whether the call was in a negated context or
			% backtracked over, so we have to assume the former.
			%
		contour_children(ContourType, Store, Prec, 
			NestedStartId, Ns0, Ns1)
	;
		( Node = else(Prec, NestedStartId, _)
		; Node = neg_succ(Prec, NestedStartId, _)
		)
	->
			%
			% There is a nested context.
			%
		stratum_children(Store, Prec, NestedStartId, Ns0, Ns1)
	; 
		Node = excp(_, CallId, _, _, _, _, _)
	->
			%
			% If the contour ends in an exception, then add this
			% exception to the list of contour children and 
			% continue along the contour, since in this case we are
			% only interested in nodes that caused the exception to
			% be thrown. 
			%
			% If the contour ends with an exit then the exception
			% must have been caught by a try/2 or try_all/3 or
			% similar.  In this case we want to add all the exits
			% of the call that threw the exception to the list of
			% children since one of the generated solutions may
			% be incorrect.
			%
		(
			ContourType = exception,
			Ns1 = [dynamic(NodeId) | Ns0]
		;
			ContourType = normal,	
			call_node_from_id(Store, CallId, Call),
			NestedStartId = Call ^ call_preceding,
			stratum_children(Store, NodeId, NestedStartId, 
				Ns0, Ns1)
		)
	;
			%
			% This handles the following cases:
			% redo, switch, first_disj, later_disj, and
			% then.  Also handles cond when the status is
			% anything other than failed.
			%
			% Redo events can be reached here if there
			% were missing events due to a shallow tracing.
			% In this case, we have to scan over the entire
			% previous contour, since there is no way to
			% tell how much of it was backtracked over.
			%
		Ns1 = Ns0
	),
	Next = step_left_in_contour(Store, Node),
	contour_children(ContourType, Store, Next, StartId, Ns1, Ns).

:- pred stratum_children(S::in, R::in, R::in, list(edt_node(R))::in, 
	list(edt_node(R))::out) is det <= annotated_trace(S, R).

stratum_children(Store, NodeId, StartId, Ns0, Ns) :-
	(
		NodeId = StartId
	->
		Ns = Ns0
	;
		stratum_children_2(Store, NodeId, StartId, Ns0, Ns)
	).

:- pred stratum_children_2(S::in, R::in, R::in, list(edt_node(R))::in, 
	list(edt_node(R))::out) is det <= annotated_trace(S, R).

stratum_children_2(Store, NodeId, StartId, Ns0, Ns) :-
	det_trace_node_from_id(Store, NodeId, Node),
	(
		( Node = call(_, _, _, _, _, _, _, _, _, _)
		; Node = neg(_, _, _)
		; Node = cond(_, _, failed)
		)
	->
		throw(internal_error("stratum_children_2",
			"unexpected start of contour"))
	;
		( Node = fail(_, _, _, _, _, _)
		; Node = excp(_, _, _, _, _, _, _)
		)
	->
			%
			% Add a child for this node.
			%
		Ns1 = [dynamic(NodeId) | Ns0]
	;
		Node = neg_fail(Prec, NestedStartId, _)
	->
			%
			% There is a nested successful context.
			%
		contour_children(normal, Store, Prec, NestedStartId, Ns0, Ns1)
	;
		Node = else(Prec, NestedStartId, _)
	->
			%
			% There is a nested failed context.
			%
		stratum_children(Store, Prec, NestedStartId, Ns0, Ns1)
	;
		Node = exit(_, CallId, _, _, _, _, _, _)
	->
			%
			% Only include an exit node as a missing answer child
			% if it produces output.  If the exit event doesn't
			% produce output then the only way the call could have
			% behaved differently is by failing, which won't change
			% the fail, negs or else event anchoring the end of the 
			% current stratum, since the rest of the goal failed 
			% anyway.
			%
		( calls_arguments_are_all_ground(Store, CallId) ->
			Ns1 = Ns0
		;
			Ns1 = [dynamic(NodeId) | Ns0]
		)
	;
			%
			% This handles the following cases: redo, switch,
			% first_disj, later_disj, then and neg_succ.  Also
			% handles cond when the status is anything other than
			% failed.  
			% We skip neg_succ nodes for the same reason that we
			% skip exit nodes where there are no outputs (see
			% above).
			%
		Ns1 = Ns0
	),
	Next = step_in_stratum(Store, Node),
	stratum_children(Store, Next, StartId, Ns1, Ns).

%-----------------------------------------------------------------------------%
%
% Tracking a subterm dependency.
%
% We are given an EDT node, an argument position, and a path to the selected
% subterm.  We wish to find the origin of that subterm within the body of the
% given node, or within the body of its parent.  We can figure out the mode of
% the top of the selected subterm.
%
% If the mode is `in', the origin could be:
%	- a primitive (unification or foreign_proc) within the body of the
%	  parent,
%	- an output subterm in a sibling node, or
%	- an input subterm of the parent node.
% In this case we look at the contour leading up to the call event associated
% with the given node. This contour will be wholly within the parent call.
%
% If the mode is `out', the origin could be:
%	- a primitive (unification or foreign_proc) within the body of the
%	  call,
%	- an output subterm of a child of the node, or
%	- an input subterm of the node itself.
% In this case we look at the contour leading up to the exit or exception event
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
	--->	chain_start(
			start_loc(R),
					% The argument number of the selected
					% position in the full list of
					% arguments, including the
					% compiler-generated ones.  
			int,		
					% The total number of arguments
					% including the compiler generated 
					% ones.
			int,
			R,		% The id of the node preceding the exit
					% node, if start_loc is cur_goal
					% and the id of the node preceding the
					% call node if start_loc is
					% parent_goal.
			maybe(goal_path),
					% No if start_loc is cur_goal;
					% and yes wrapped around the goal path
					% of the call in the parent procedure
					% if start_loc is parent_goal.
			maybe(proc_rep)
					% The body of the procedure indicated
					% by start_loc.
		)
			% An explicit subtree is required before the
			% chain start can be calculated.
	;	require_explicit_subtree.

:- type start_loc(R)
	--->	cur_goal
	;	parent_goal(R, trace_node(R)).

:- type goal_and_path	--->	goal_and_path(goal_rep, goal_path).

:- type goal_and_path_list ==	list(goal_and_path).

:- type annotated_primitive(R)
	--->	primitive(
			string,		% filename
			int,		% line number
			list(var_rep),	% vars bound by the atomic goal
			atomic_goal_rep,% the atomic goal itself
			goal_path,	% its goal path
			maybe(R)
					% if the atomic goal is a call,
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
		% The only time a subtree will be required is if the
		% mode of the subterm is output.
		Mode = subterm_out
	).

:- pred trace_dependency(wrap(S)::in, edt_node(R)::in, arg_pos::in,
	term_path::in, subterm_mode::out, subterm_origin(edt_node(R))::out) 
	is det <= annotated_trace(S, R).

trace_dependency(wrap(Store), dynamic(Ref), ArgPos, TermPath, Mode, Origin) :-
	find_chain_start(Store, Ref, ArgPos, TermPath, ChainStart),
	(
		ChainStart = chain_start(StartLoc, ArgNum, TotalArgs, NodeId, 
			StartPath, MaybeProcRep),
		Mode = start_loc_to_subterm_mode(StartLoc),
		(
			MaybeProcRep = no,
			Origin = not_found
		;
			MaybeProcRep = yes(ProcRep),
			det_trace_node_from_id(Store, NodeId, Node),
			materialize_contour(Store, NodeId, Node, [], Contour0),
			(
				StartLoc = parent_goal(CallId, CallNode),
				Contour = list.append(Contour0, 
					[CallId - CallNode])
			;
				StartLoc = cur_goal,
				Contour = Contour0
			),
			ProcRep = proc_rep(HeadVars, GoalRep),
			is_traced_grade(AllTraced),
			MaybePrims = make_primitive_list(Store, 
				[goal_and_path(GoalRep, [])],
				Contour, StartPath, ArgNum, TotalArgs,
				HeadVars, AllTraced, []),
			(
				MaybePrims = yes(primitive_list_and_var(
					Primitives, Var, MaybeClosure)),
				%
				% If the subterm is in a closure argument (i.e.
				% an argument passed to the predicate that
				% originally formed the closure), then the
				% argument number of the closure argument is
				% prefixed to the term path, since the closure
				% is itself a term.  This is done because at
				% the time of the closure call it is not easy
				% to decide if the call is higher order or not,
				% without repeating all the work done in
				% make_primitive_list, so the original TermPath
				% doesn't reflect the closure argument
				% position.
				%
				(
					MaybeClosure = yes,
					AdjustedTermPath = [ArgNum | TermPath]
				;
					MaybeClosure = no,
					AdjustedTermPath = TermPath
				),
				traverse_primitives(Primitives, Var,
					AdjustedTermPath, Store, ProcRep,
					Origin)
			;
				MaybePrims = no,
				Origin = not_found
			)
		)
	;
		ChainStart = require_explicit_subtree,
		Origin = require_explicit_subtree,
		% The only time a subtree will be required is if the
		% mode of the subterm is output.
		Mode = subterm_out
	).

:- pred find_chain_start(S::in, R::in, arg_pos::in, term_path::in,
	dependency_chain_start(R)::out) is det <= annotated_trace(S, R).

find_chain_start(Store, Ref, ArgPos, TermPath, ChainStart) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, CallId, _, _, _, _, _, _),
		ExitAtom = get_trace_exit_atom(Node),
		call_node_from_id(Store, CallId, CallNode),
		CallAtom = get_trace_call_atom(CallNode),
		( trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) ->
			find_chain_start_inside(Store, CallId, CallNode,
				ArgPos, ChainStart)
		; trace_atom_subterm_is_ground(ExitAtom, ArgPos, TermPath) ->
			(
				not_at_depth_limit(Store, CallId)
			->
				find_chain_start_outside(CallNode, Node,
					ArgPos, ChainStart)
			;
				ChainStart = require_explicit_subtree
			)
		;
			throw(internal_error("find_chain_start",
				"unbound wrong answer term"))
		)
	;
		Node = fail(_, CallId, _, _, _, _),
		call_node_from_id(Store, CallId, CallNode),
		CallAtom = get_trace_call_atom(CallNode),
		( trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) ->
			find_chain_start_inside(Store, CallId, CallNode,
				ArgPos, ChainStart)
		;
			throw(internal_error("find_chain_start",
				"unbound missing answer term"))
		)
	;
		Node = excp(_, CallId, _, _, _, _, _),
		call_node_from_id(Store, CallId, CallNode),
		CallAtom = get_trace_call_atom(CallNode),
		%
		% XXX we don't yet handle tracking of the exception value.
		%
		( trace_atom_subterm_is_ground(CallAtom, ArgPos, TermPath) ->
			find_chain_start_inside(Store, CallId, CallNode,
				ArgPos, ChainStart)
		;
			throw(internal_error("find_chain_start",
				"unbound exception term"))
		)
	).

:- pred find_chain_start_inside(S::in, R::in,
	trace_node(R)::in(trace_node_call), arg_pos::in,
	dependency_chain_start(R)::out) is det <= annotated_trace(S, R).

find_chain_start_inside(Store, CallId, CallNode, ArgPos, ChainStart) :-
	CallPrecId = CallNode ^ call_preceding,
	CallAtom = get_trace_call_atom(CallNode),
	CallPathStr = get_goal_path_from_maybe_label(
		CallNode ^ call_return_label),
	path_from_string_det(CallPathStr, CallPath),
	StartLoc = parent_goal(CallId, CallNode),
	absolute_arg_num(ArgPos, CallAtom, ArgNum),
	TotalArgs = length(CallAtom ^ atom_args),
	StartId = CallPrecId,
	StartPath = yes(CallPath),
	parent_proc_rep(Store, CallId, StartRep),
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
	call_node_maybe_proc_rep(CallNode, StartRep),
	ChainStart = chain_start(StartLoc, ArgNum, TotalArgs, StartId,
		StartPath, StartRep).

:- pred parent_proc_rep(S::in, R::in, maybe(proc_rep)::out)
	is det <= annotated_trace(S, R).

parent_proc_rep(Store, CallId, ProcRep) :-
	call_node_from_id(Store, CallId, Call),
	CallPrecId = Call ^ call_preceding,
	(
		step_left_to_call(Store, CallPrecId, ParentCallNode)
	->
		call_node_maybe_proc_rep(ParentCallNode, ProcRep)
	;
		ProcRep = no
	).
	
	%
	% Finds the call node of the parent of the given node.  Fails if
	% the call node cannot be found because it was not included in the
	% annotated trace.
	%
:- pred step_left_to_call(S::in, R::in, trace_node(R)::out(trace_node_call)) 
	is semidet <= annotated_trace(S, R).

step_left_to_call(Store, NodeId, ParentCallNode) :-
	trace_node_from_id(Store, NodeId, Node),
	( Node = call(_, _, _, _, _, _, _, _, _, _) ->
		ParentCallNode = Node
	;
		%
		% We wish to step through negated contexts, so we handle NEGE
		% and COND events seperately, since step_left_in_contour/2
		% will throw an exception if it reaches the boundary of a
		% negated context.
		%
		( 
			Node = neg(NegPrec, _, _)
		->
			PrevNodeId = NegPrec
		;	
			Node = cond(CondPrec, _, _)
		->
			PrevNodeId = CondPrec
		;
			PrevNodeId = step_left_in_contour(Store, Node)
		),
		step_left_to_call(Store, PrevNodeId, ParentCallNode)
	).

:- pred materialize_contour(S::in, R::in, trace_node(R)::in,
	assoc_list(R, trace_node(R))::in, assoc_list(R, trace_node(R))::out)
	is det <= annotated_trace(S, R).

materialize_contour(Store, NodeId, Node, Nodes0, Nodes) :-
	( Node = call(_, _, _, _, _, _, _, _, _, _) ->
		
		Nodes = Nodes0
	;
		%
		% We include NEGE and (possibly failed) COND events in the
		% contour so we can track input sub-terms through negated
		% contexts.
		%
		(
			Node = neg(NegPrec, _, _)
		->
			PrevNodeId = NegPrec
		;
			Node = cond(CondPrec, _, _)
		->
			PrevNodeId = CondPrec
		;
			PrevNodeId = step_left_in_contour(Store, Node)
		),
		det_trace_node_from_id(Store, PrevNodeId, PrevNode),
		( Node = then(_, _, _) ->
			% The cond node is enough to tell us which way the
			% if-then-else went; the then node would just
			% complicate the job of make_primitive_list.
			Nodes1 = Nodes0
		;
			Nodes1 = [NodeId - Node | Nodes0]
		),
		materialize_contour(Store, PrevNodeId, PrevNode,
			Nodes1, Nodes)
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
	Exit = exit(_, _, _, _, _, _, _, _),
	FinalAtom = exit_node_decl_atom(Store, Exit).

:- type primitive_list_and_var(R)
	--->	primitive_list_and_var(
			primitives	:: list(annotated_primitive(R)),
				%
				% The var_rep for the argument which holds the
				% subterm we are trying to find the origin of.
				% If the subterm is in one of the arguments
				% that were passed to a closure when the
				% closure was created, then this will be the
				% var_rep for the variable containing the
				% closure.
				% 
			var		:: var_rep,
				%
				% Was the subterm inside a closure argument
				% that was passed in when the closure was
				% created?
				%
			closure		:: bool
		).

	% Constructs a list of the primitive goals along the given contour if
	% it can.  It might not be able to construct the list in the case where
	% there are higher order calls and we're not sure if everything is
	% traced, then there might be extra/missing events on the contour and
	% we need to make sure the primitive atomic goals match up with the
	% contour events, but in the case of higher order calls this is not
	% easily done as the name/module of the higher order call is not
	% available in the goal_rep.  If it cannot construct the primitive list
	% reliably then `no' is returned.  MaybeEnd is the goal path of the
	% call event that should be at the end of the contour for input
	% subterms.
	%
:- func make_primitive_list(S, goal_and_path_list, 
	assoc_list(R, trace_node(R)), maybe(goal_path), int, int, 
	list(var_rep), bool, list(annotated_primitive(R))) 
	= maybe(primitive_list_and_var(R)) <= annotated_trace(S, R).

make_primitive_list(Store, GoalPaths, Contour, MaybeEnd, ArgNum, TotalArgs,
		HeadVars, AllTraced, Primitives0) = MaybePrims :-
	(
		AllTraced = no,
		(
			next_goal_generates_internal_event(GoalPaths)
		;
			GoalPaths = []
		)
	->
		% There may be extra exit and fail events in the
		% contour if a call to an untraced module was made, but
		% then something in the untraced module called
		% something in a traced module.
		remove_leading_exit_fail_events(Contour, 
			AdjustedContour)
	;
		AdjustedContour = Contour
	),
	(
		AllTraced = no,
		contour_at_end_path(AdjustedContour, MaybeEnd),
		(
			next_goal_generates_internal_event(GoalPaths)
		;
			GoalPaths = []
		)
	->
		% We were unable to identify the goal corresponding to this
		% call (it might have been a higher order call) so we return no
		% to indicate this. This is the safest thing to do when we're
		% not sure what has/hasn't been traced.
		MaybePrims = no
	;
		(
			GoalPaths = [goal_and_path(Goal, Path) | Tail],
			MaybePrims = match_goal_to_contour_event(Store, Goal,
				Path, Tail, AdjustedContour, MaybeEnd,
				ArgNum, TotalArgs, HeadVars, AllTraced,
				Primitives0)
		;
			GoalPaths = [],
			decl_require(unify(AdjustedContour, []),
				"make_primitive_list", 
				"nonempty contour at end"),
			decl_require(unify(MaybeEnd, no),
				"make_primitive_list", 
				"found end when looking for call"),
			find_variable_in_args(HeadVars, ArgNum, TotalArgs, 
				Var),
			MaybePrims = yes(primitive_list_and_var(
				Primitives0, Var, no))
		)
	).

:- pred contour_at_end_path(assoc_list(R, trace_node(R))::in, 
	maybe(goal_path)::in) is semidet.

contour_at_end_path([_ - call(_, _, _, _, _, _, MaybeReturnLabel, _, _, _)], 
		yes(EndPath)) :-
	CallPathStr = get_goal_path_from_maybe_label(MaybeReturnLabel),
	path_from_string_det(CallPathStr, CallPath),
	CallPath = EndPath.

:- pred next_goal_generates_internal_event(list(goal_and_path)::in) is semidet.

next_goal_generates_internal_event([goal_and_path(NextGoal, _) | _]) :-
	goal_generates_internal_event(NextGoal) = yes.

	% match_goal_to_contour_event(Store, Goal, Path, GoalPaths,
	%	Contour, MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
	%	Primitives) = MaybePrims
	% Matches the given goal_rep to the first event in the contour for
	% all goal_reps except atomic goal reps which need to be handled 
	% differently depending on whether everything is traced (AllTraced).
	% Returns the list of Primitives appended to the list of 
	% primitive goals along the remaining contour.  If it cannot match
	% a higher order call to a contour event and AllTraced is no, then
	% no is returned.
	%
:- func match_goal_to_contour_event(S, goal_rep, goal_path, goal_and_path_list, 
	assoc_list(R, trace_node(R)), maybe(goal_path), int, int, 
	list(var_rep), bool, list(annotated_primitive(R))) 
	= maybe(primitive_list_and_var(R)) <= annotated_trace(S, R).

match_goal_to_contour_event(Store, Goal, Path, GoalPaths,
		Contour, MaybeEnd, ArgNum, TotalArgs, HeadVars, AllTraced,
		Primitives0) = MaybePrims :- 
	(
		Goal = conj_rep(Conjs),
		add_paths_to_conjuncts(Conjs, Path, 1, ConjPaths),
		MaybePrims = make_primitive_list(Store, 
			list.append(ConjPaths, GoalPaths),
			Contour, MaybeEnd, ArgNum, TotalArgs, HeadVars, 
			AllTraced, Primitives0)
	;
		Goal = scope_rep(InnerGoal, MaybeCut),
		InnerPath = list.append(Path, [scope(MaybeCut)]),
		InnerAndPath = goal_and_path(InnerGoal, InnerPath),
		MaybePrims = make_primitive_list(Store, 
			[InnerAndPath | GoalPaths],
			Contour, MaybeEnd, ArgNum, TotalArgs, HeadVars, 
			AllTraced, Primitives0)
	;
		Goal = atomic_goal_rep(_, File, Line, BoundVars, AtomicGoal),
		GeneratesEvent = atomic_goal_generates_event(AtomicGoal),
		(
			GeneratesEvent = yes(AtomicGoalArgs),
			MaybePrims = match_atomic_goal_to_contour_event(Store, 
				File, Line, BoundVars, AtomicGoal,
				AtomicGoalArgs, Path, GoalPaths, Contour,
				MaybeEnd, ArgNum, TotalArgs, HeadVars,
				AllTraced, Primitives0)
		;
			GeneratesEvent = no,
			Primitive = primitive(File, Line, BoundVars,
				AtomicGoal, Path, no),
			Primitives1 = [Primitive | Primitives0],
			MaybePrims = make_primitive_list(Store, GoalPaths,
				Contour, MaybeEnd, ArgNum, TotalArgs, HeadVars,
				AllTraced, Primitives1)
		)
	;
		Goal = disj_rep(Disjs),
		(
			Contour = [_ - ContourHeadNode | ContourTail],
			( 
				ContourHeadNode = first_disj(_, Label)
			; 
				ContourHeadNode = later_disj(_, Label, _)
			),
			DisjPathStr = get_goal_path_from_label_layout(Label),
			path_from_string_det(DisjPathStr, DisjPath),
			list.append(Path, PathTail, DisjPath),
			PathTail = [disj(N)]
		->
			list.index1_det(Disjs, N, Disj),
			DisjAndPath = goal_and_path(Disj, DisjPath),
			MaybePrims = make_primitive_list(Store, [DisjAndPath |
				GoalPaths], ContourTail, MaybeEnd, ArgNum,
				TotalArgs, HeadVars, AllTraced, Primitives0)
		;
			throw(internal_error("match_goal_to_contour_event",
				"mismatch on disj"))
		)
	;
		Goal = switch_rep(Arms),
		(
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = switch(_, Label),
			ArmPathStr = get_goal_path_from_label_layout(Label),
			path_from_string_det(ArmPathStr, ArmPath),
			list.append(Path, PathTail, ArmPath),
			PathTail = [switch(N)]
		->
			list.index1_det(Arms, N, Arm),
			ArmAndPath = goal_and_path(Arm, ArmPath),
			MaybePrims = make_primitive_list(Store, [ArmAndPath |
				GoalPaths], ContourTail, MaybeEnd, ArgNum,
				TotalArgs, HeadVars, AllTraced, Primitives0)
		;
			throw(internal_error("match_goal_to_contour_event",
				"mismatch on switch"))
		)
	;
		Goal = ite_rep(Cond, Then, Else),
		(
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = cond(_, Label, _),
			CondPathStr = get_goal_path_from_label_layout(Label),
			path_from_string_det(CondPathStr, CondPath),
			list.append(Path, PathTail, CondPath),
			PathTail = [ite_cond]
		->
			ThenPath = list.append(Path, [ite_then]),
			CondAndPath = goal_and_path(Cond, CondPath),
			ThenAndPath = goal_and_path(Then, ThenPath),
			MaybePrims = make_primitive_list(Store, [CondAndPath,
				ThenAndPath | GoalPaths], ContourTail,
				MaybeEnd, ArgNum, TotalArgs, HeadVars,
				AllTraced, Primitives0)
		;
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = else(_, ElseCondId, _),
			cond_node_from_id(Store, ElseCondId, CondNode),
			CondNode = cond(_, Label, _),
			CondPathStr = get_goal_path_from_label_layout(Label),
			path_from_string_det(CondPathStr, CondPath),
			list.append(Path, PathTail, CondPath),
			PathTail = [ite_cond]
		->
			ElsePath = list.append(Path, [ite_else]),
			ElseAndPath = goal_and_path(Else, ElsePath),
			MaybePrims = make_primitive_list(Store, [ElseAndPath |
				GoalPaths], ContourTail, MaybeEnd, ArgNum,
				TotalArgs, HeadVars, AllTraced, Primitives0)
		;
			throw(internal_error("match_goal_to_contour_event",
				"mismatch on if-then-else"))
		)
	;
		Goal = negation_rep(NegGoal),
		(
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = neg_succ(_, _, _)
		->
			% The negated goal cannot contribute any bindings.
			MaybePrims = make_primitive_list(Store, GoalPaths, 
				ContourTail, MaybeEnd, ArgNum, TotalArgs,
				HeadVars, AllTraced, Primitives0)
		;
			Contour = [_ - ContourHeadNode | ContourTail],
			ContourHeadNode = neg(_, _, _)
		->
			% The end of the primitive list is somewhere inside
			% NegGoal.
			NegPath = list.append(Path, [neg]),
			NegAndPath = goal_and_path(NegGoal, NegPath),
			MaybePrims = make_primitive_list(Store, [NegAndPath], 
				ContourTail, MaybeEnd, ArgNum, TotalArgs,
				HeadVars, AllTraced, Primitives0)
		;
			throw(internal_error("match_goal_to_contour_event",
				"mismatch on negation"))
		)
	).

:- pred remove_leading_exit_fail_events(
	assoc_list(R, trace_node(R))::in,
	assoc_list(R, trace_node(R))::out) is det.
	
remove_leading_exit_fail_events([], []).
remove_leading_exit_fail_events(Contour0, Contour) :-
	Contour0 = [_ - ContourHeadNode | ContourTail],
	(
		(
			ContourHeadNode = exit(_, _, _, _, _, _, _, _)
		;
			ContourHeadNode = fail(_, _, _, _, _, _)
		)
	->
		remove_leading_exit_fail_events(ContourTail, 
			Contour)
	;
		Contour = Contour0
	).

	% Trys to match an atomic goal to the first event on the contour.
	% These should match if AllTraced = yes.  If AllTraced = no, then
	% if the goal doesn't match the contour event (i.e. they are for
	% different predicates), then the goal will be treated as a primitive
	% operation with no children.  The next atomic goal will then be tried
	% as a match for the first event on the contour.  This will
	% continue until a non-atomic goal is reached, at which point all
	% events that could match atomic goals (exit and fail events) are
	% removed from the top of the contour.  This strategy will work
	% best when untraced calls do not call traced modules (which seems
	% more likely for the majority of untraced calls).
	%
:- func match_atomic_goal_to_contour_event(S, string, int, 
	list(var_rep), atomic_goal_rep, list(var_rep), goal_path,
	list(goal_and_path), assoc_list(R, trace_node(R)), maybe(goal_path),
	int, int, list(var_rep), bool, list(annotated_primitive(R))) =
	maybe(primitive_list_and_var(R)) <= annotated_trace(S, R).

match_atomic_goal_to_contour_event(Store, File, Line, BoundVars, AtomicGoal,
		AtomicGoalArgs, Path, GoalPaths, Contour, MaybeEnd, ArgNum,
		TotalArgs, HeadVars, AllTraced, Primitives0) = MaybePrims :- 
	(
		Contour = [_ - ContourHeadNode],
		MaybeEnd = yes(EndPath)
	->
		(
			ContourHeadNode = call(_, _, _, _, _, _, 
				MaybeReturnLabel, _, _, _),
			Atom = get_trace_call_atom(ContourHeadNode),
			CallPathStr = get_goal_path_from_maybe_label(
				MaybeReturnLabel),
			path_from_string_det(CallPathStr, CallPath),
			CallPath = EndPath
		->
			(
				(
					atomic_goal_identifiable(AtomicGoal) =
						yes(AtomicGoalId)
				->
					atomic_goal_matches_atom(AtomicGoalId,
						Atom)
				;
					AllTraced = yes
				)
			->
				(
					% Test to see that the argument is not
					% a closure argument (passed in when
					% the closure was created)
					ArgNum > TotalArgs -
						length(AtomicGoalArgs)
				->
					find_variable_in_args(AtomicGoalArgs, 
						ArgNum, TotalArgs, Var),
					MaybePrims = yes(
						primitive_list_and_var(
							Primitives0, Var, no))
					
				;
					% Perhaps this is a closure and the 
					% argument was passed in when the
					% closure was created.  
					(
						AtomicGoal =
						higher_order_call_rep(
							Closure, _)
					->
						Var = Closure,
						MaybePrims = yes(
						    primitive_list_and_var(
							Primitives0, Var, yes))
					;
						throw(internal_error(
							"make_primitive_list",
							"argument number "++ 
							"mismatch"))
					)
				)
			;
				(
					AllTraced = yes,
					throw(internal_error(
						"match_atomic_goal_to_conto"++
						"ur_event",
						"name mismatch on call"))
				;
					AllTraced = no,
					Primitive = primitive(File, Line, 
						BoundVars, AtomicGoal, Path,
						no),
					Primitives1 = [Primitive|Primitives0],
					MaybePrims = make_primitive_list(Store,
						GoalPaths, Contour, MaybeEnd,
						ArgNum, TotalArgs, HeadVars,
						AllTraced, Primitives1)
				)
			)
		;
			throw(internal_error(
				"match_atomic_goal_to_contour_event",
				"goalpath mismatch on call"))
		)
	;
		(
			Contour = [ContourHeadId - ContourHeadNode |
				ContourTail],
			(
				Atom = get_trace_exit_atom(ContourHeadNode)
			->
				(
					(
						atomic_goal_identifiable(
							AtomicGoal) =
							yes(AtomicGoalId)
					->
						atomic_goal_matches_atom(
							AtomicGoalId, Atom)
					;
						AllTraced = yes
					)
				->
					CallInfo = yes(ContourHeadId),
					NewContour = ContourTail
				;
					(
						AllTraced = yes,
						throw(internal_error(
							"match_atomic_goal_"++
							"to_contour_event",
							"atomic goal doesn't"++
							" match exit event\n"))
					;
						AllTraced = no,
						CallInfo = no,
						NewContour = Contour
					)
				)
			;
				(
					AllTraced = yes,
					throw(internal_error(
					  "match_atomic_goal_to_contour_event",
					  "atomic goal with no exit event "++
					  "when assuming all traced"))
				;
					AllTraced = no,
					CallInfo = no,
					NewContour = Contour
				)
			),
			Primitive = primitive(File, Line, BoundVars,
				AtomicGoal, Path, CallInfo),
			Primitives1 = [Primitive | Primitives0],
			MaybePrims = make_primitive_list(Store, GoalPaths,
				NewContour, MaybeEnd, ArgNum, TotalArgs,
				HeadVars, AllTraced, Primitives1)
		;
			Contour = [],
			(
				AllTraced = no,
				MaybeEnd = no
			->
				Primitive = primitive(File, Line, BoundVars,
					AtomicGoal, Path, no),
				Primitives1 = [Primitive | Primitives0],
				MaybePrims = make_primitive_list(Store,
					GoalPaths, [], MaybeEnd, ArgNum,
					TotalArgs, HeadVars, AllTraced,
					Primitives1)
			;
				throw(internal_error(
					"match_atomic_goal_to_contour_event",
					"premature contour end"))
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
	sym_name_to_string(EventModule, ".", EventModuleStr),
	EventModuleStr = GoalModule, 
	EventName = GoalName, 
	EventArity = GoalArity.

:- pred find_variable_in_args(list(var_rep)::in, int::in, int::in, 
	var_rep::out) is det.

find_variable_in_args(Args, ArgNum, TotalArgs, Var) :-
	% We reverse the arg list in case this is an argument of a closure call
	% that is passed in at the time of the call.
	(
		index1(reverse(Args), TotalArgs - ArgNum + 1, FoundVar)
	->
		Var = FoundVar
	;
		throw(internal_error("find_variable_in_args", "arg not found"))
	).

:- pred traverse_primitives(list(annotated_primitive(R))::in,
	var_rep::in, term_path::in, S::in, proc_rep::in,
	subterm_origin(edt_node(R))::out) is det <= annotated_trace(S, R).

traverse_primitives([], Var0, TermPath0, _, ProcRep, Origin) :-
	ProcRep = proc_rep(HeadVars, _),
	ArgPos = find_arg_pos(HeadVars, Var0),
	Origin = input(ArgPos, TermPath0).
traverse_primitives([Prim | Prims], Var0, TermPath0, Store, ProcRep,
		Origin) :-
	Prim = primitive(File, Line, BoundVars, AtomicGoal, _GoalPath,
		MaybeNodeId),
	(
		AtomicGoal = unify_construct_rep(_CellVar, _Cons, FieldVars),
		( list.member(Var0, BoundVars) ->
			(
				TermPath0 = [],
				Origin = primitive_op(File, Line, unification)
			;
				TermPath0 = [TermPathStep0 | TermPath],
				list.index1_det(FieldVars, TermPathStep0,
					Var),
				traverse_primitives(Prims, Var, TermPath,
					Store, ProcRep, Origin)
			)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = unify_deconstruct_rep(CellVar, _Cons, FieldVars),
		( list.member(Var0, BoundVars) ->
			( list.nth_member_search(FieldVars, Var0, Pos) ->
				traverse_primitives(Prims,
					CellVar, [Pos | TermPath0],
					Store, ProcRep, Origin)
			;
				throw(internal_error("traverse_primitives",
					"bad deconstruct"))
			)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = unify_assign_rep(ToVar, FromVar),
		% We handle assigns the same as we handle unsafe casts.
		( list.member(Var0, BoundVars) ->
			decl_require(unify(Var0, ToVar),
				"traverse_primitives", "bad assign"),
			traverse_primitives(Prims, FromVar, TermPath0,
				Store, ProcRep, Origin)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = unsafe_cast_rep(ToVar, FromVar),
		% We handle unsafe casts the same as we handle assigns.
		( list.member(Var0, BoundVars) ->
			decl_require(unify(Var0, ToVar),
				"traverse_primitives", "bad unsafe_cast"),
			traverse_primitives(Prims, FromVar, TermPath0,
				Store, ProcRep, Origin)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = pragma_foreign_code_rep(_Args),
		( list.member(Var0, BoundVars) ->
			Origin = primitive_op(File, Line, foreign_proc)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = unify_simple_test_rep(_LVar, _RVar),
		( list.member(Var0, BoundVars) ->
			throw(internal_error("traverse_primitives", "bad test"))
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	;
		AtomicGoal = higher_order_call_rep(_, Args),
		traverse_call(BoundVars, File, Line, Args, MaybeNodeId, Prims,
			Var0, TermPath0, Store, ProcRep, Origin)
	;
		AtomicGoal = method_call_rep(_, _, Args),
		traverse_call(BoundVars, File, Line, Args, MaybeNodeId, Prims,
			Var0, TermPath0, Store, ProcRep, Origin)
	;
		AtomicGoal = plain_call_rep(_, _, Args),
		traverse_call(BoundVars, File, Line, Args, MaybeNodeId,
			Prims, Var0, TermPath0, Store, ProcRep, Origin)
	;
		AtomicGoal = builtin_call_rep(_, _, _),
		( list.member(Var0, BoundVars) ->
			Origin = primitive_op(File, Line, builtin_call)
		;
			traverse_primitives(Prims, Var0, TermPath0,
				Store, ProcRep, Origin)
		)
	).

:- type plain_call_info
	--->	plain_call_info(
			file_name	:: string,
			line_number	:: int,
			flat_module_name:: string,
			pred_name	:: string
		).

:- pred traverse_call(list(var_rep)::in, string::in, int::in, 
	list(var_rep)::in, maybe(R)::in, list(annotated_primitive(R))::in,
	var_rep::in, term_path::in, S::in, proc_rep::in,
	subterm_origin(edt_node(R))::out) is det <= annotated_trace(S, R).

traverse_call(BoundVars, File, Line, Args, MaybeNodeId,
		Prims, Var, TermPath, Store, ProcRep, Origin) :-
	( list.member(Var, BoundVars) ->
		Pos = find_arg_pos(Args, Var),
		(
			MaybeNodeId = yes(NodeId),
			Origin = output(dynamic(NodeId), Pos, TermPath)
		;
			MaybeNodeId = no,
			Origin = primitive_op(File, Line, untraced_call)
		)
	;
		traverse_primitives(Prims, Var, TermPath, Store, ProcRep,
			Origin)
	).

%-----------------------------------------------------------------------------%

:- pred add_paths_to_conjuncts(list(goal_rep)::in, goal_path::in, int::in,
	goal_and_path_list::out) is det.

add_paths_to_conjuncts([], _, _, []).
add_paths_to_conjuncts([Goal | Goals], ParentPath, N,
		[goal_and_path(Goal, Path) | GoalAndPaths]) :-
	list.append(ParentPath, [conj(N)], Path),
	add_paths_to_conjuncts(Goals, ParentPath, N + 1, GoalAndPaths).

%-----------------------------------------------------------------------------%

:- pred is_traced_grade(bool::out) is det.

:- pragma foreign_proc("C", is_traced_grade(TracingOn::out), 
	[promise_pure, will_not_call_mercury, thread_safe],
"
	#ifdef MR_EXEC_TRACE
		TracingOn = MR_YES;
	#else
		TracingOn = MR_NO;
	#endif
").


%-----------------------------------------------------------------------------%

:- func start_loc_to_subterm_mode(start_loc(R)) = subterm_mode.

start_loc_to_subterm_mode(cur_goal) = subterm_out.
start_loc_to_subterm_mode(parent_goal(_, _)) = subterm_in.

%-----------------------------------------------------------------------------%

:- func find_arg_pos(list(var_rep), var_rep) = arg_pos.

find_arg_pos(HeadVars, Var) = ArgPos :-
	find_arg_pos_from_back(HeadVars, Var, length(HeadVars), ArgPos).

:- pred find_arg_pos_from_back(list(var_rep)::in, var_rep::in, int::in, 
	arg_pos::out) is det.

find_arg_pos_from_back([], _, _, _) :-
	throw(internal_error("find_arg_pos_2", "empty list")).
find_arg_pos_from_back([HeadVar | HeadVars], Var, Pos, ArgPos) :-
	( HeadVar = Var ->
		ArgPos = any_head_var_from_back(Pos)
	;
		find_arg_pos_from_back(HeadVars, Var, Pos - 1, ArgPos)
	).

%-----------------------------------------------------------------------------%

edt_subtree_details(Store, dynamic(Ref), Event, SeqNo, CallPreceding) :-
	det_edt_return_node_from_id(Store, Ref, Node),
	(
		Node = exit(_, Call, _, _, Event, _, _, _)
	;
		Node = fail(_, Call, _, Event, _, _)
	;
		Node = excp(_, Call, _, _, Event, _, _)
	),
	call_node_from_id(Store, Call, CallNode),
	SeqNo = CallNode ^ call_seq,
	CallPreceding = CallNode ^ call_preceding.

:- inst edt_return_node 
	--->	exit(ground, ground, ground, ground, ground, ground, ground,
			ground)
	;	fail(ground, ground, ground, ground, ground, ground)
	;	excp(ground, ground, ground, ground, ground, ground, ground).

:- pred det_edt_return_node_from_id(S::in, R::in,
	trace_node(R)::out(edt_return_node)) is det <= annotated_trace(S, R).

det_edt_return_node_from_id(Store, Ref, Node) :-
	(
		trace_node_from_id(Store, Ref, Node0),
		(
			Node0 = exit(_, _, _, _, _, _, _, _)
		;
			Node0 = fail(_, _, _, _, _, _)
		;
			Node0 = excp(_, _, _, _, _, _, _)
		)
	->
		Node = Node0
	;
		throw(internal_error("det_edt_return_node_from_id",
			"not a return node"))
	).

:- pred get_edt_call_node(S::in, R::in, R::out) 
	is det <= annotated_trace(S, R).

get_edt_call_node(Store, Ref, CallId) :-
	(
		trace_node_from_id(Store, Ref, Node0),
		(
			Node0 = exit(_, CallId0, _, _, _, _, _, _)
		;
			Node0 = fail(_, CallId0, _, _, _, _)
		;
			Node0 = excp(_, CallId0, _, _, _, _, _)
		)
	->
		CallId = CallId0
	;
		throw(internal_error("get_edt_call_node",
			"not a return node"))
	).

%-----------------------------------------------------------------------------%

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
	%
	% XXX The following won't work for partially instantiated arguments.
	%
	all [Arg] (
		list.member(Arg, Args)
	=> 
		Arg = arg_info(_, _, yes(_))
	).

%-----------------------------------------------------------------------------%

:- pred decl_require((pred)::in((pred) is semidet), string::in, string::in) 
	is det.

decl_require(Goal, Loc, Msg) :-
	(
		call(Goal)
	->
		true
	;
		throw(internal_error(Loc, Msg))
	).
