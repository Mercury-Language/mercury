%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_debugger.m
% Author: Mark Brown
%
% This module has two main purposes:
% 	- to define the interface between the front and back ends of
% 	  a Mercury declarative debugger, and
% 	- to implement a front end.
%
% The interface between the front and back ends is partly defined
% by the evaluation_tree typeclass.  An instance of this typeclass
% implements evaluation dependency trees (EDTs), which are created
% in the back end and passed to the front end for analysis.  The rest
% of the interface is via analyse_edt/7, which is how the front end
% is called from the back end.
%
% The front end implemented in this module analyses the EDT it is
% passed to diagnose a bug.  It does this by a simple top-down search.
%

:- module declarative_debugger.
:- interface.
:- import_module io, list, string, std_util, bool.
:- import_module declarative_oracle.

	%
	% This type represents the possible truth values for nodes
	% in the EDT.
	%
:- type edt_truth == bool.

	%
	% Values of this type represent EDT nodes.  This representation
	% is used by the front end (in this module), as well as the
	% oracle and user interface.
	%
	% There will be nodes other than wrong_answer in future, such
	% as for missing answer analysis.
	%
:- type edt_node
			%
			% The node is a possible wrong answer.  The first
			% argument is the procedure name and the second
			% is the list of arguments at exit.
			%
	--->	wrong_answer(string, list(univ)).

	%
	% See comments above.
	%
:- typeclass evaluation_tree(Tree) where [
	pred edt_root(Tree, edt_node),
	mode edt_root(in, out) is det,

	pred edt_children(Tree, list(Tree)),
	mode edt_children(in, out) is det
].

:- pred analyse_edt(T, io__input_stream, io__output_stream, oracle_state,
		oracle_state, io__state, io__state) <= evaluation_tree(T).
:- mode analyse_edt(in, in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, int, char.
:- import_module declarative_user.

	%
	% This section defines the Mercury instance of the evaluation
	% tree.
	%

:- instance evaluation_tree(mercury_edt) where [
	pred(edt_root/2) is mercury_edt_root,
	pred(edt_children/2) is mercury_edt_children
].

	%
	% This is defined as a "no-tag" type, to avoid problems with
	% equivalence types being used as type class instances.
	%
:- type mercury_edt
	--->	mercury_edt(c_pointer).


:- pred mercury_edt_children(mercury_edt, list(mercury_edt)).
:- mode mercury_edt_children(in, out) is det.

mercury_edt_children(mercury_edt(EDT), Children) :-
	(
		mercury_edt_first_child(EDT, FirstChild)
	->
		mercury_edt_children_2(FirstChild, Children0),
		Children = [mercury_edt(FirstChild) | Children0]
	;
		Children = []
	).


:- pred mercury_edt_children_2(c_pointer, list(mercury_edt)).
:- mode mercury_edt_children_2(in, out) is det.

mercury_edt_children_2(Child, Siblings) :-
	(
		mercury_edt_sibling(Child, Sibling)
	->
		mercury_edt_children_2(Sibling, Siblings0),
		Siblings = [mercury_edt(Sibling) | Siblings0]
	;
		Siblings = []
	).


:- pragma c_header_code("
	#include ""mercury_trace_declarative.h""
	#include ""mercury_type_info.h""
	#include ""mercury_wrapper.h""
").

:- pred mercury_edt_first_child(c_pointer, c_pointer).
:- mode mercury_edt_first_child(in, out) is semidet.

:- pragma c_code(mercury_edt_first_child(Parent::in, Child::out),
	[will_not_call_mercury],
	"
		MR_Edt_Node	*parent;
		MR_Edt_Node	*child;

		parent = (MR_Edt_Node *) Parent;
		child = parent->MR_edt_node_children;
		if (child != NULL) {
			Child = (Word) child;
			SUCCESS_INDICATOR = TRUE;
		} else {
			SUCCESS_INDICATOR = FALSE;
		}
	"
).

:- pred mercury_edt_sibling(c_pointer, c_pointer).
:- mode mercury_edt_sibling(in, out) is semidet.

:- pragma c_code(mercury_edt_sibling(Child::in, Sibling::out),
	[will_not_call_mercury],
	"
		MR_Edt_Node	*child;
		MR_Edt_Node	*sibling;

		child = (MR_Edt_Node *) Child;
		sibling = child->MR_edt_node_sibling;
		if (sibling != NULL) {
			Sibling = (Word) sibling;
			SUCCESS_INDICATOR = TRUE;
		} else {
			SUCCESS_INDICATOR = FALSE;
		}
	"
).

:- pred mercury_edt_root(mercury_edt, edt_node).
:- mode mercury_edt_root(in, out) is det.

mercury_edt_root(mercury_edt(CPtr), Root) :-
	mercury_edt_root_imp(CPtr, Root).


:- pred mercury_edt_root_imp(c_pointer, edt_node).
:- mode mercury_edt_root_imp(in, out) is det.

:- pragma c_code(mercury_edt_root_imp(EDT::in, Root::out),
	[will_not_call_mercury],
	"
		#ifdef MR_USE_DECLARATIVE_DEBUGGER
			/*
			** We wish to call MR_edt_root_node in the trace
			** directory, but due to problems with linking we
			** call it indirectly via a pointer defined in
			** runtime/mercury_wrapper.c.
			*/
			MR_address_of_edt_root_node(EDT, &Root);
		#else
			fatal_error(\"this should never be reached\");
		#endif
	"
).

%-----------------------------------------------------------------------------%

	%
	% This section implements the front end.  It exports the function
	% ML_DD_analyse_edt to C to be called from
	% trace/mercury_trace_declarative.c, and is passed an EDT.
	% This structure is then analysed to find a cause of the bug,
	% which is then presented to the user.
	%
	% The current implementation uses a simple top-down strategy to
	% analyse the EDT.
	%

	%
	% This is what the analysis can currently find.
	%

:- type declarative_bug(T)		% <= evaluation_tree(T)
	--->	not_found
		%
		% An e_bug is an EDT whose root node is incorrect, but
		% whose children are all correct.
		%
	;	e_bug(T).


	%
	% To simplify calling this module from C code, we export
	% a version of analyse_edt which is specifically for the instance
	% used by the current back end.
	%
:- pred analyse_mercury_edt(mercury_edt, io__input_stream, io__output_stream,
		io__state, io__state).
:- mode analyse_mercury_edt(in, in, in, di, uo) is det.

:- pragma export(declarative_debugger__analyse_mercury_edt(in, in, in, di, uo),
		"ML_DD_analyse_edt").

analyse_mercury_edt(EDT, MdbIn, MdbOut) -->
		%
		% XXX this data structure needs to be more
		% persistent.  It really should be saved between
		% calls to this predicate.
		%
	{ oracle_state_init(Oracle0) },
	analyse_edt(EDT, MdbIn, MdbOut, Oracle0, _).


analyse_edt(EDT, MdbIn, MdbOut, Oracle0, Oracle) -->
	io__set_input_stream(MdbIn, OldIn),
	io__set_output_stream(MdbOut, OldOut),
	{ edt_root(EDT, RootNode) },
	query_oracle(RootNode, Answer, Oracle0, Oracle1),
	(
		{ Answer = truth_value(yes) },
		{ Bug = not_found },
		{ Oracle = Oracle1 }
	;
		{ Answer = truth_value(no) },
		analyse_edt_2(EDT, Bug, Oracle1, Oracle)
	;
		{ Answer = deferred(_) },
		{ Bug = not_found },
		{ Oracle = Oracle1 }
	),
	report_bug(Bug),
	io__set_input_stream(OldIn, _),
	io__set_output_stream(OldOut, _).


	%
	% Assumes the root note is not valid.
	%
:- pred analyse_edt_2(T, declarative_bug(T), oracle_state, oracle_state,
		io__state, io__state) <= evaluation_tree(T).
:- mode analyse_edt_2(in, out, in, out, di, uo) is det.

analyse_edt_2(EDT, Bug, Oracle0, Oracle) -->
	{ edt_children(EDT, Children) },
	analyse_children(Children, e_bug(EDT), Bug, Oracle0, Oracle).


:- pred analyse_children(list(T), declarative_bug(T), declarative_bug(T),
		oracle_state, oracle_state, io__state, io__state)
				<= evaluation_tree(T).
:- mode analyse_children(in, in, out, in, out, di, uo) is det.

analyse_children([], Bug, Bug, Oracle, Oracle) -->
	[].
analyse_children([Child | Children], Bug0, Bug, Oracle0, Oracle) -->
	{ edt_root(Child, ChildNode) },
	query_oracle(ChildNode, Answer, Oracle0, Oracle1),
	(
		{ Answer = truth_value(yes) },
		analyse_children(Children, Bug0, Bug, Oracle1, Oracle)
	;
		{ Answer = truth_value(no) },
		analyse_edt_2(Child, Bug, Oracle1, Oracle)
	;
		{ Answer = deferred(_) },
		{ append(Children, [Child], NewChildren) },
		analyse_children(NewChildren, Bug0, Bug, Oracle1, Oracle)
	).


:- pred report_bug(declarative_bug(T), io__state, io__state)
		<= evaluation_tree(T).
:- mode report_bug(in, di, uo) is det.

report_bug(not_found) -->
	io__write_string("Bug not found.\n").
report_bug(e_bug(EDT)) -->
	io__write_string("Incorrect instance found:\n\n"),
	write_root_node(EDT),
	{ edt_children(EDT, Children0) },
	(
		{ Children0 = [Child | Children1] }
	->
		io__write_string(" :-\n"),
		{ list__reverse(Children1, Children) },
		write_children(Children),
		io__write_char('\t'),
		write_root_node(Child)
	;
		[]
	),
	io__write_string(".\n\n").


:- pred write_children(list(T), io__state, io__state) <= evaluation_tree(T).
:- mode write_children(in, di, uo) is det.

write_children([]) -->
	[].
write_children([Child | Children]) -->
	io__write_char('\t'),
	write_root_node(Child),
	io__write_string(",\n"),
	write_children(Children).


:- pred write_root_node(T, io__state, io__state) <= evaluation_tree(T).
:- mode write_root_node(in, di, uo) is det.

write_root_node(EDT) -->
	{ edt_root(EDT, RootNode) },
	write_edt_node(RootNode).

