%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_debugger.m
% Author: Mark Brown
% Purpose:
%	This module implements the oracle for a Mercury declarative debugger.
% It is called by the front end of the declarative debugger to provide 
% information about the intended interpretation of the program being
% debugged.
%
% The module has a knowledge base as a sub-component.  This is a cache
% for all the assumptions that the oracle is currently making.  When
% the oracle is queried, it first checks the KB to see if an answer
% is available there.
%
% If no answer is available in the KB, then the oracle uses the UI 
% (in browser/declarative_user.m) to get the required answer from the
% user.  If any new knowledge is obtained, it is added to the KB so
% the user will not be asked the same question twice.
%

:- module declarative_oracle.
:- interface.
:- import_module io.
:- import_module declarative_debugger, declarative_user.

	%
	% The oracle state.  This is threaded around the declarative
	% debugger.
	%
:- type oracle_state.

	%
	% A response that the oracle gives to the caller.  This is
	% a truth value, if available, or else an indication of why
	% the query cannot be answered yet.  For deferred answers
	% the argument is the user response, which will not be a truth
	% value (since, if it was, the truth_value/1 constructor would
	% be used).
	%
:- type oracle_answer
	--->	truth_value(edt_truth)
	;	deferred(user_response).

	%
	% Query the oracle about the program being debugged.  The first
	% argument is a node in the evaluation tree, the second argument
	% is the oracle response.  The oracle state is threaded through
	% so its contents can be updated after user responses.
	%
:- pred query_oracle(edt_node, oracle_answer, oracle_state, oracle_state,
		io__state, io__state).
:- mode query_oracle(in, out, in, out, di, uo) is det.

	%
	% Produce a new oracle state.
	%
:- pred oracle_state_init(oracle_state).
:- mode oracle_state_init(out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, list, char, require, std_util, string, map.
:- import_module declarative_user, util, browse.

:- type oracle_state
	--->	oracle(
%			browser_state,		% XXX not used yet
			oracle_kb
		).

oracle_state_init(oracle(KB)) :-
	oracle_kb_init(KB).


:- pred get_oracle_kb(oracle_state, oracle_kb).
:- mode get_oracle_kb(in, out) is det.

get_oracle_kb(oracle(KB), KB).

:- pred set_oracle_kb(oracle_state, oracle_kb, oracle_state).
:- mode set_oracle_kb(in, in, out) is det.

set_oracle_kb(oracle(_), KB, oracle(KB)).

query_oracle(Node, Answer, Oracle0, Oracle) -->
	(
		{ query_oracle_kb(Node, Oracle0, KBTruth, _KBAssumption) }
	->
		{ Answer = truth_value(KBTruth) },
		{ Oracle = Oracle0 }
	;
		query_user(Node, Answer),
		{
			Answer = truth_value(Truth),
			%
			% We don't need to check the consistency of this
			% assertion because we only get here if the KB
			% didn't know the answer already.
			%
			add_oracle_assumption(Node, Truth, _Assumption,
					Oracle0, Oracle)
		;
			Answer = deferred(_),
			Oracle = Oracle0
		}
	).


%-----------------------------------------------------------------------------%

	%
	% This section implements the oracle knowledge base, which
	% stores anything that the debugger knows about the intended
	% interpretation.  This can be used to check the correctness
	% of an EDT node.  The KB stores items of knowledge, called
	% oracle assumptions, and these can be referred to from outside
	% this module by the use of an identifier.
	%

	%
	% The type of the knowledge base.  Other fields may be added in
	% the future, such as for assertions made on-the-fly by the user,
	% or assertions in the program text.
	%
:- type oracle_kb
	---> oracle_kb(
		%
		% For ground atoms, the knowledge is represented directly
		% with a map.  This is used, for example, in the common
		% case that the user supplies a truth value for a
		% "wrong answer" node.
		%
		map(edt_node, edt_truth)
	).


	%
	% The type of identifiers for the oracle assumptions.
	%
:- type oracle_assumption
			%
			% Truth value came from a table of ground atoms.
			%
	--->	ground(edt_node).


:- pred query_oracle_kb(edt_node, oracle_state, edt_truth, oracle_assumption).
:- mode query_oracle_kb(in, in, out, out) is semidet.

query_oracle_kb(Node, Oracle, Truth, Assumption) :-
	get_oracle_kb(Oracle, oracle_kb(NodeMap)),
	map__search(NodeMap, Node, Truth),
	Assumption = ground(Node).


:- pred add_oracle_assumption(edt_node, edt_truth, oracle_assumption,
		oracle_state, oracle_state).
:- mode add_oracle_assumption(in, in, out, in, out) is det.

add_oracle_assumption(Node, Truth, Assumption, Oracle0, Oracle) :-
	get_oracle_kb(Oracle0, oracle_kb(NodeMap0)),
	map__set(NodeMap0, Node, Truth, NodeMap),
	set_oracle_kb(Oracle0, oracle_kb(NodeMap), Oracle),
	Assumption = ground(Node).


:- pred oracle_kb_init(oracle_kb).
:- mode oracle_kb_init(out) is det.

oracle_kb_init(oracle_kb(NodeMap)) :-
	map__init(NodeMap).


