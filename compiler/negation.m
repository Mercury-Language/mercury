%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% File: negation.nl
% Main author: squirrel (Jane Anna Langley)
%
% This module pushes in negations, and sorts out quantifications along the
% way.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module negation.
:- interface.

:- import_module string, int, set, bintree, list, map, require, std_util.
:- import_module term, term_io, prog_io, varset.
:- import_module prog_util, prog_out, hlds_out.
:- import_module globals, options.
:- import_module make_tags, quantification.
:- import_module unify_proc, type_util.
:- import_module io.

:- pred negation__transform(item_list, item_list, io__state, io__state).
:- mode negation__transform(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% PUBLIC PREDICATE IMPLEMENTATIONS:
%-----------------------------------------------------------------------------%
:- implementation.

negation__transform(Items_In, Items_Out) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Pushing negation inwards..."),
	maybe_flush_output(Verbose),
	{ negation__transform_over_list(Items_In, Items_Out) },
	maybe_write_string(Verbose, " done.\n"),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics).

%-----------------------------------------------------------------------------%
% LOCAL PREDICATE DECLARATIONS:
%-----------------------------------------------------------------------------%
:- pred negation__transform_over_list(item_list, item_list).
:- mode negation__transform_over_list(in, out) is det.

:- pred negation__transform_item_and_context(item_and_context, 
		item_and_context).
:- mode negation__transform_item_and_context(in, out) is det.

:- pred negation__transform_item(item, item).
:- mode negation__transform_item(in, out) is det.

:- pred negation__transform_goal(goal, goal).
:- mode negation__transform_goal(in, out) is det.

%-----------------------------------------------------------------------------%
% LOCAL PREDICATE IMPLEMENTATIONS:
%-----------------------------------------------------------------------------%

negation__transform_over_list([], []).

negation__transform_over_list([In|Ins], [Out|Outs]) :-
	negation__transform_item_and_context(In, Out),
	negation__transform_over_list(Ins, Outs).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %

	% negation__transform_item_and_context/2


	% Note that the Context is unchanged and is just passed
	% straight through.
	% It simply strips off the Context and passes the
	% item on to negation__transform_item/2
negation__transform_item_and_context((Item_In - Context_In),
			(Item_Out - Context_In)) :-
	negation__transform_item(Item_In, Item_Out).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %

	% negation__transform_item/2

	% Pushes negations inwards.
	% If the item is a clause, it pulls out the goal and
	% transforms it, otherwise the item passes through
	% unchanged.
negation__transform_item(Item_In, Item_Out) :-
	(
		Item_In = clause(Varset, Sym_name, Terms, Goal_In)
	->
		(
		negation__transform_goal(Goal_In, Goal_Out),
		Item_Out = clause(Varset, Sym_name, Terms, Goal_Out)
		)
	;
		Item_Out = Item_In
	).	

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
	% negation__transform_goal/2
	
	% Pushes negations inwards.
	% This the predicate that actually does the work!
	% All implication operators should have been removed by
	% the time that this goal is reached.  If there are any
	% encountered then this predicate will report an error.
	% Also transforms universal quantification
	% i.e. all x P becomes not(some x not(P))
negation__transform_goal(Goal_In, Goal_Out) :-
	(
		% recursively remove double negation
		Goal_In  = not(_, not(_, Goal))
	->
		negation__transform_goal(Goal, Goal_Out)
	;
		% all x P becomes not(some x not(P))
		Goal_In  = all(Vars, Goal)
	->
		(
		negation__transform_goal(not([],Goal), Not_Goal),
		negation__transform_goal(not([], some(Vars,Not_Goal)),
			Goal_Out)
		)
	;
		% Push negation inside scope of "some"
		Goal_In  = some(Vars, Goal)
	->
		(
		negation__transform_goal(Goal, Goal1),
		Goal_Out = some(Vars, Goal1)
		)
	;
		% ~(P v Q) ---> ~P ^ ~Q
		Goal_In  = not(_Vars, (P ; Q))
	->
		(
		negation__transform_goal(not([],P), Not_P),
		negation__transform_goal(not([],Q), Not_Q),
		Goal_Out = (Not_P, Not_Q)
		)
	;
		% ~(~P ^ ~Q) ---> (P v Q)
		% A sort of reverse de Morgan
		Goal_In  = not(_V1, (not(_V2,P), not(_V3,Q)))
	->
		(
		negation__transform_goal(P, P1),
		negation__transform_goal(Q, Q1),
		Goal_Out = (P1 ; Q1)
		)
	;
		Goal_In = (P, Q)
	->
		(
		negation__transform_goal(P, P1),
		negation__transform_goal(Q, Q1),
		Goal_Out = (P1, Q1)
		)
	;
		Goal_In = (P; Q)
	->
		(
		negation__transform_goal(P, P1),
		negation__transform_goal(Q, Q1),
		Goal_Out = (P1; Q1)
		)
	;
		Goal_In = not(Vars, P)
	->
		(
		negation__transform_goal(P, P1),
		Goal_Out = not(Vars, P1)
		)
	;
		Goal_Out = Goal_In
	).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
