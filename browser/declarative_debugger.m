%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: declarative_debugger.m
% Author: Mark Brown
% Purpose:
%	This module is the front end of a Mercury declarative debugger.
% It is called by the back end, in trace/mercury_trace_declarative.c, and
% is passed an evaluation dependency tree (EDT).  It then analyses this to
% diagnose a bug.
%
% The implementation is in two sections:
%	- Mercury interface to the main data structure.
%	- Implementation of the analysis algorithm.
%

:- module declarative_debugger.
:- interface.
:- import_module io, list, string, std_util.

:- type evaluation_tree.

:- type edt_node
			%
			% The node is a possible wrong answer.  The first
			% argument is the procedure name and the second
			% is the list of arguments at exit.
			%
	--->	wrong_answer(string, list(univ)).


	%
	% This procedure is exported to C to be called from the back
	% end of the declarative debugger.
	%
:- pred analyse_edt(evaluation_tree, io__input_stream, io__output_stream,
		io__state, io__state).
:- mode analyse_edt(in, in, in, di, uo) is det.


	%
	% This prints the atom to the current output stream.
	%
:- pred write_atom(string, list(univ), io__state, io__state).
:- mode write_atom(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, require, int, char.
:- import_module declarative_oracle.

	%
	% This section contains the Mercury interface to the EDTs that
	% are built by the back end.
	%

:- type evaluation_tree == c_pointer.

:- pred edt_children(evaluation_tree, list(evaluation_tree)).
:- mode edt_children(in, out) is det.

edt_children(EDT, Children) :-
	(
		edt_first_child(EDT, FirstChild)
	->
		edt_children_2(FirstChild, Children0),
		Children = [FirstChild | Children0]
	;
		Children = []
	).


:- pred edt_children_2(evaluation_tree, list(evaluation_tree)).
:- mode edt_children_2(in, out) is det.

edt_children_2(Child, Siblings) :-
	(
		edt_sibling(Child, Sibling)
	->
		edt_children_2(Sibling, Siblings0),
		Siblings = [Sibling | Siblings0]
	;
		Siblings = []
	).

:- pragma c_header_code("
	#include ""mercury_trace_declarative.h""
	#include ""mercury_type_info.h""
").

:- pred edt_first_child(evaluation_tree, evaluation_tree).
:- mode edt_first_child(in, out) is semidet.

:- pragma c_code(edt_first_child(Parent::in, Child::out),
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

:- pred edt_sibling(evaluation_tree, evaluation_tree).
:- mode edt_sibling(in, out) is semidet.

:- pragma c_code(edt_sibling(Child::in, Sibling::out),
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

:- pred edt_root(evaluation_tree, edt_node).
:- mode edt_root(in, out) is det.

:- pragma import(edt_root(in, out),
	[will_not_call_mercury],
	"MR_edt_root_node"
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

:- type declarative_bug
	--->	unknown
	;	wrong(evaluation_tree).


:- pragma export(declarative_debugger__analyse_edt(in, in, in, di, uo),
		"ML_DD_analyse_edt").

analyse_edt(EDT, MdbIn, MdbOut) -->
	io__set_input_stream(MdbIn, OldIn),
	io__set_output_stream(MdbOut, OldOut),
	{ edt_root(EDT, RootNode) },
	query_oracle(RootNode, Valid),
	(
		{ Valid = yes },
		{ Bug = unknown }
	;
		{ Valid = no },
		analyse_edt_2(EDT, Bug)
	),
	report_bug(Bug),
	io__set_input_stream(OldIn, _),
	io__set_output_stream(OldOut, _).


	%
	% Assumes the root note is not valid.
	%
:- pred analyse_edt_2(evaluation_tree, declarative_bug, io__state, io__state).
:- mode analyse_edt_2(in, out, di, uo) is det.

analyse_edt_2(EDT, Bug) -->
	{ edt_children(EDT, Children) },
	analyse_children(Children, wrong(EDT), Bug).


:- pred analyse_children(list(evaluation_tree), declarative_bug,
		declarative_bug, io__state, io__state).
:- mode analyse_children(in, in, out, di, uo) is det.

analyse_children([], Bug, Bug) -->
	[].
analyse_children([Child | Children], Bug0, Bug) -->
	{ edt_root(Child, ChildNode) },
	query_oracle(ChildNode, Valid),
	(
		{ Valid = yes },
		analyse_children(Children, Bug0, Bug)
	;
		{ Valid = no },
		analyse_edt_2(Child, Bug)
	).


:- pred report_bug(declarative_bug, io__state, io__state).
:- mode report_bug(in, di, uo) is det.

report_bug(unknown) -->
	io__write_string("Bug not found.\n").
report_bug(wrong(EDT)) -->
	io__write_string("Incorrect instance found:\n\n"),
	write_root_atom(EDT),
	{ edt_children(EDT, Children0) },
	(
		{ Children0 = [Child | Children1] }
	->
		io__write_string(" :-\n"),
		{ list__reverse(Children1, Children) },
		write_children(Children),
		io__write_char('\t'),
		write_root_atom(Child)
	;
		[]
	),
	io__write_string(".\n\n").


:- pred write_children(list(evaluation_tree), io__state, io__state).
:- mode write_children(in, di, uo) is det.

write_children([]) -->
	[].
write_children([Child | Children]) -->
	io__write_char('\t'),
	write_root_atom(Child),
	io__write_string(",\n"),
	write_children(Children).


:- pred write_root_atom(evaluation_tree, io__state, io__state).
:- mode write_root_atom(in, di, uo) is det.

write_root_atom(EDT) -->
	{ edt_root(EDT, RootNode) },
	{
		RootNode = wrong_answer(Name0, Args0),
		Name = Name0,
		Args = Args0
	},
	write_atom(Name, Args).


write_atom(Name, Args) -->
	io__write_string(Name),
	(
		{ Args = [Arg1 | Args0] }
	->
		io__write_char('('),
		io__print(Arg1),
		write_args_rest(Args0),
		io__write_char(')')
	;
		[]
	).


:- pred write_args_rest(list(univ), io__state, io__state).
:- mode write_args_rest(in, di, uo) is det.

write_args_rest([]) -->
	[].
write_args_rest([Arg | Args]) -->
	io__write_string(", "),
	io__print(Arg),
	write_args_rest(Args).

