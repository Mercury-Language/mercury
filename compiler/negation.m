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
%-----------------------------------------------------------------------------%
% PUBLIC PREDICATE DECLARATIONS:
%-----------------------------------------------------------------------------%
% dependencies
:- import_module prog_io, io.


:- pred negation__transform(item_list, item_list, io__state, io__state).
:- mode negation__transform(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
% PUBLIC PREDICATE IMPLEMENTATIONS:
%-----------------------------------------------------------------------------%
:- implementation.

% dependencies
:- import_module string, int, set, bintree, list, map, require, std_util.
:- import_module term, term_io, varset.
:- import_module prog_util, prog_out, hlds_out.
:- import_module globals, options.
:- import_module make_tags, quantification.
:- import_module unify_proc, type_util.

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

:- pred negation__transform_goal_2(goal_expr, goal_expr).
:- mode negation__transform_goal_2(in, out) is det.

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
	% Just calls negation__transform_goal_2, passing the context
	% through unchanged.
	
negation__transform_goal(Goal_In - Context, Goal_Out - Context) :-
	negation__transform_goal_2(Goal_In, Goal_Out).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
	% negation__transform_goal_2/2
	% Pushes negations inwards.
	% This the predicate that actually does the work!
	% All implication operators should have been removed by
	% the time that this goal is reached.  If there are any
	% encountered then this predicate will report an error.

% The transformations we actually want...

negation__transform_goal_2(not(Goal), Goal_Out) :-
	(
	% Eliminate double negatives
		Goal = not(Goal1 - _) - _
	->
		negation__transform_goal_2(Goal1, Goal_Out)
	;

	% A sort of pseudo-de Morgan transformation, these
	% two transformations are not strictly necessary
	% but they do get rid of some ugly dirt thrown
	% up by the implication and negation transformations.
	% If they cause any problems they can be safely removed
	% without any other changes to the code being necessary.
	
% clip from here --------------------------------------
	% (1)   ~(~P ^ ~Q) ----> (P v Q)
		Goal = (not(P) - _, not(Q) - _) - _
	->
		(
		negation__transform_goal(P, P1),
		negation__transform_goal(Q, Q1),
		Goal_Out = (P1 ; Q1)
		)
	;
	% (2)   ~(~P v ~Q) ----> (P ^ Q)

		Goal = (not(P) - _; not(Q) - _) - _
	->
		(
		negation__transform_goal(P, P1),
		negation__transform_goal(Q, Q1),
		Goal_Out = (P1, Q1)
		)
	;
% to here ---------------------------------------------

%	% Apply De Morgan's rule over a conjunction
%	% (Don't do this, it would cause mode errors!)
%		Goal = (P, Q)
%	->
%		(
%		negation__transform_goal(not(P), Not_P),
%		negation__transform_goal(not(Q), Not_Q),
%		Goal_Out = (Not_P ; Not_Q)
%		)
%	;
	% Apply De Morgan's rule over a disjunction
		Goal = (P ; Q) - C
	->
		(
		negation__transform_goal_2(not(P), Not_P),
		negation__transform_goal_2(not(Q), Not_Q),
		Goal_Out = (Not_P - C, Not_Q - C)
		)
	;
	% No other applicable cases so just apply the
	% transformation recursively
		(
		negation__transform_goal(Goal, Goal2),
		Goal_Out = not(Goal2)
		)
	).


% These forms should have already been eliminated!

	% Implication
negation__transform_goal_2(implies(_P, _Q), _Goal_Out) :-
	error("implication encountered when transforming negation").

	% Equivalence
negation__transform_goal_2(equivalent(_P, _Q), _Goal_Out) :-
	error("equivalence encountered when transforming negation").

	% Universal Quantification
negation__transform_goal_2(all(_Vars, _P), _Goal_Out) :-
	error("'all' encountered when transforming negation").

% These forms do not need to be transformed themselves, they
% just continue to transform recursively...

	% Existential Quantification
negation__transform_goal_2(some(Vars, P), Goal_Out) :-
	negation__transform_goal(P, P1),
	Goal_Out = some(Vars, P1).

	% Conjunction
negation__transform_goal_2((P, Q), Goal_Out) :-
	negation__transform_goal(P, P1),
	negation__transform_goal(Q, Q1),
	Goal_Out = (P1, Q1).

	% Disjunction
negation__transform_goal_2((P; Q), Goal_Out) :-
	negation__transform_goal(P, P1),
	negation__transform_goal(Q, Q1),
	Goal_Out = (P1; Q1).

	% If-Then
negation__transform_goal_2(if_then(Vars, P, Q), Goal_Out) :-
	negation__transform_goal(P, P1),
	negation__transform_goal(Q, Q1),
	Goal_Out = if_then(Vars, P1, Q1).

	% If-Then-Else
negation__transform_goal_2(if_then_else(Vars, P, Q, R), Goal_Out) :-
	negation__transform_goal(P, P1),
	negation__transform_goal(Q, Q1),
	negation__transform_goal(R, R1),
	Goal_Out = if_then_else(Vars, P1, Q1, R1).

	% Truth
negation__transform_goal_2(true, true).

	% Falsehood
negation__transform_goal_2(fail, fail).

	% Call
negation__transform_goal_2(call(Term), call(Term)).

	% Unify
negation__transform_goal_2(unify(Term1, Term2), unify(Term1, Term2)).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
