%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: help.m.
% Author: zs.
% Stability: low.
%
% This file provides a basic help system that stores information in help nodes
% which are organized as a tree structure of arbitrary depth.
%
% The help system consists of a list of help list entries. Each entry
% has a name, an index (an integer that determines its position in the list),
% and a help node. Each node contains text that should shed some light
% on the topic named by the node's entry. Each node also has an associated
% list of child entries; this list may of course be empty.
%
%-----------------------------------------------------------------------------%

:- module mdb__help.

:- interface.

:- import_module list, io, std_util.

:- type help__system.

:- type help__path	==	list(string).

:- type help__res	--->	help__ok ; help__error(string).

%-----------------------------------------------------------------------------%

	% Initialize an empty help system.
:- pred help__init(help__system::out) is det.

	% Add a node to the given help system, at the given path, and with
	% the given name and index. If successful, return ok and the
	% updated help system; if not, return an error message and the
	% original help system.
:- pred help__add_help_node(help__system::in, help__path::in, int::in,
	string::in, string::in, help__res::out, help__system::out) is det.

	% Print the top-level help nodes. This should give an overview
	% of the main topics for which help is available.
:- pred help__help(help__system::in, io__output_stream::in,
	io__state::di, io__state::uo) is det.

	% Print the help node at the given path. If there is none,
	% print the top-level nodes.
:- pred help__path(help__system::in, help__path::in, io__output_stream::in,
	help__res::out, io__state::di, io__state::uo) is det.

	% Print all help nodes with the given name. If there are none,
	% print the top-level nodes.
:- pred help__name(help__system::in, string::in, io__output_stream::in,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

	% Return the type_info for the type help__system, for use by C code.
:- pred help__help_system_type(type_desc::out) is det.

	% Help interpret a help__res for C code.
:- pred help__result_is_error(help__res::in, string::out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, string, require.

:- type help__system	==	list(help__entry).

:- type help__node
	--->	node(
			help__text,
			list(help__entry)
		).

:- type help__text	==	string. % Should be one or more complete lines.

:- type help__entry
	--->	entry(
			int,		% This integer determines the position
					% of the node in the node list. A node
					% list is always sorted on this field.
			string,		% The name of the node, which should
					% be one word or phrase. It must be
					% unique within the node list, but
					% need not be unique globally.
			node
		).

%-----------------------------------------------------------------------------%

:- pragma export(help__init(out), "ML_HELP_init").
:- pragma export(help__add_help_node(in, in, in, in, in, out, out),
	"ML_HELP_add_help_node").
:- pragma export(help__help(in, in, di, uo), "ML_HELP_help").
:- pragma export(help__path(in, in, in, out, di, uo), "ML_HELP_path").
:- pragma export(help__name(in, in, in, di, uo), "ML_HELP_name").
:- pragma export(help__help_system_type(out), "ML_HELP_help_system_type").
:- pragma export(help__result_is_error(in, out), "ML_HELP_result_is_error").

%-----------------------------------------------------------------------------%

help__init([]).

help__add_help_node(Sys0, Path, Index, Name, Text, Res, Sys) :-
	Node = node(Text, []),
	help__add_node(Sys0, Path, Index, Name, Node, Res, Sys).

:- pred help__add_node(help__system::in, help__path::in, int::in,
	string::in, help__node::in, help__res::out, help__system::out) is det.

help__add_node(Nodes0, [Step | Steps], Index, Name, NewNode, Res, Nodes) :-
	( help__one_path_step(Nodes0, Step, Entry0) ->
		Entry0 = entry(EntryIndex, EntryName, EntryNode0),
		EntryNode0 = node(Text, SubNodes0),
		help__add_node(SubNodes0, Steps, Index, Name, NewNode,
			Res, SubNodes),
		EntryNode = node(Text, SubNodes),
		Entry = entry(EntryIndex, EntryName, EntryNode),
		help__replace_entry(Nodes0, Entry, Nodes)
	;
		string__append("invalid path component ", Step, Msg),
		Res = help__error(Msg),
		Nodes = Nodes0
	).
help__add_node(Nodes0, [], Index, Name, Node, Res, Nodes) :-
	(
		list__member(Entry1, Nodes0),
		Entry1 = entry(Index, _, _)
	->
		Res = help__error("entry with given index already exists"),
		Nodes = Nodes0
	;
		list__member(Entry1, Nodes0),
		Entry1 = entry(_, Name, _)
	->
		Res = help__error("entry with given name already exists"),
		Nodes = Nodes0
	;
		Res = help__ok,
		help__insert_into_entry_list(Nodes0, Index, Name, Node, Nodes)
	).

:- pred help__insert_into_entry_list(list(help__entry)::in,
	int::in, string::in, help__node::in, list(help__entry)::out) is det.

help__insert_into_entry_list([], Index, Name, Node, [Entry]) :-
	Entry = entry(Index, Name, Node).
help__insert_into_entry_list([Head | Tail], Index, Name, Node, List) :-
	Head = entry(HeadIndex, _, _),
	( HeadIndex < Index ->
		help__insert_into_entry_list(Tail, Index, Name, Node, NewTail),
		List = [Head | NewTail]
	;
		Entry = entry(Index, Name, Node),
		List = [Entry, Head | Tail]
	).

%-----------------------------------------------------------------------------%

help__help(Sys, Stream) -->
	help__print_entry_list(Sys, Stream).

help__name(Sys, Name, Stream) -->
	help__search_entry_list(Sys, Name, 0, Count, Stream),
	( { Count = 0 } ->
		io__write_string("There is no such help topic.\n"),
		help__help(Sys, Stream)
	;
		[]
	).

:- pred help__search_entry_list(list(help__entry)::in, string::in,
	int::in, int::out, io__output_stream::in,
	io__state::di, io__state::uo) is det.

help__search_entry_list([], _, C, C, _) --> [].
help__search_entry_list([Entry | Tail], Name, C0, C, Stream) -->
	{ Entry = entry(_, EntryName, Node) },
	( { Name = EntryName } ->
		% We print this node, but don't search its children.
		help__print_node(Node, Stream),
		{ C = C0 + 1 }
	;
		help__search_node(Node, Name, C0, C1, Stream),
		help__search_entry_list(Tail, Name, C1, C, Stream)
	).

:- pred help__search_node(help__node::in, string::in, int::in, int::out,
	io__output_stream::in, io__state::di, io__state::uo) is det.

help__search_node(node(_, SubNodes), Name, C0, C, Stream) -->
	help__search_entry_list(SubNodes, Name, C0, C, Stream).

help__path(Entries, Path, Stream, Result) -->
	( { Path = [Step] } ->
		( { help__one_path_step(Entries, Step, Entry) } ->
			{ Entry = entry(_, _, EntryNode) },
			{ EntryNode = node(Text, _) },
			io__write_string(Stream, Text),
			{ Result = help__ok }
		;
			{ string__append_list(["error at path component """,
				Step, """"], Msg) },
			{ Result = help__error(Msg) }
		)
	; { Path = [Step | Tail] } ->
		( { help__one_path_step(Entries, Step, Entry) } ->
			{ Entry = entry(_, _, EntryNode) },
			{ EntryNode = node(_, SubEntries) },
			help__path(SubEntries, Tail, Stream, Result)
		;
			{ string__append_list(["error at path component """,
				Step, """"], Msg) },
			{ Result = help__error(Msg) }
		)
	;
		{ Result = help__error("the path does not go that deep") }
	).

%-----------------------------------------------------------------------------%

:- pred help__print_entry_list(list(help__entry)::in, io__output_stream::in,
	io__state::di, io__state::uo) is det.

help__print_entry_list([], _) --> [].
help__print_entry_list([entry(_, _, Node) | Nodes], Stream) -->
	help__print_node(Node, Stream),
	help__print_entry_list(Nodes, Stream).

:- pred help__print_node(help__node::in, io__output_stream::in,
	io__state::di, io__state::uo) is det.

help__print_node(node(Text, _Nodes), Stream) -->
	io__write_string(Stream, Text).
	% XXX help__print_entry_list(Nodes, Stream).

%-----------------------------------------------------------------------------%

:- pred help__one_path_step(list(help__entry)::in, string::in,
	help__entry::out) is semidet.

help__one_path_step([Head | Tail], Name, Entry) :-
	Head = entry(_, HeadName, _),
	( HeadName = Name ->
		Entry = Head
	;
		help__one_path_step(Tail, Name, Entry)
	).

:- pred help__replace_entry(list(help__entry)::in, help__entry::in,
	list(help__entry)::out) is det.

help__replace_entry([], _, _) :-
	error("help__replace_entry: entry to be replaced not found").
help__replace_entry([Head | Tail], Entry, List) :-
	Head = entry(HeadIndex, _, _),
	Entry = entry(EntryIndex, _, _),
	( HeadIndex = EntryIndex ->
		List = [Entry | Tail]
	;
		help__replace_entry(Tail, Entry, NewTail),
		List = [Head | NewTail]
	).

%-----------------------------------------------------------------------------%

help__help_system_type(Type) :-
	help__init(HelpInit),
	Type = type_of(HelpInit).

help__result_is_error(help__error(Msg), Msg).

%-----------------------------------------------------------------------------%
