%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.help.

:- interface.

:- import_module io.
:- import_module list.
:- import_module type_desc.

:- type system.

:- type path  ==  list(string).

:- type help_res
    --->    help_ok
    ;       help_error(string).

%---------------------------------------------------------------------------%

    % Initialize an empty help system.
    %
:- pred init(system::out) is det.

    % Add a node to the given help system, at the given path, and with
    % the given name and index. If successful, return help_ok and the
    % updated help system; if not, return an error message and the
    % original help system.
    %
:- pred add_help_node(system::in, path::in, int::in,
    string::in, string::in, help_res::out, system::out) is det.

    % Print the top-level help nodes. This should give an overview
    % of the main topics for which help is available.
    %
:- pred help(system::in, io.output_stream::in, io::di, io::uo) is det.

    % Print the help node at the given path. If there is none,
    % print the top-level nodes.
    %
:- pred path(system::in, path::in, io.output_stream::in,
    help_res::out, io::di, io::uo) is det.

    % Print all help nodes with the given name. If there are none,
    % print the top-level nodes.
    %
:- pred name(system::in, string::in, io.output_stream::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Return the type_info for the type system, for use by C code.
    %
:- pred help_system_type(type_desc::out) is det.

    % Help interpret a help_res for C code.
    %
:- pred result_is_error(help_res::in, string::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

:- type system    ==  list(entry).

:- type node
    --->    node(
                text,
                list(entry)
            ).

:- type text  ==  string. % Should be one or more complete lines.

:- type entry
    --->    entry(
                int,        % This integer determines the position
                            % of the node in the node list. A node
                            % list is always sorted on this field.

                string,     % The name of the node, which should
                            % be one word or phrase. It must be
                            % unique within the node list, but
                            % need not be unique globally.

                node
            ).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C", init(out), "ML_HELP_init").
:- pragma foreign_export("C", add_help_node(in, in, in, in, in, out, out),
    "ML_HELP_add_help_node").
:- pragma foreign_export("C", help(in, in, di, uo), "ML_HELP_help").
:- pragma foreign_export("C", path(in, in, in, out, di, uo), "ML_HELP_path").
:- pragma foreign_export("C", name(in, in, in, di, uo), "ML_HELP_name").
:- pragma foreign_export("C", help_system_type(out),
    "ML_HELP_help_system_type").
:- pragma foreign_export("C", result_is_error(in, out),
    "ML_HELP_result_is_error").

%---------------------------------------------------------------------------%

init([]).

add_help_node(Sys0, Path, Index, Name, Text, Res, Sys) :-
    Node = node(Text, []),
    add_node(Sys0, Path, Index, Name, Node, Res, Sys).

:- pred add_node(system::in, path::in, int::in,
    string::in, node::in, help_res::out, system::out) is det.

add_node(Nodes0, [Step | Steps], Index, Name, NewNode, Res, Nodes) :-
    ( if one_path_step(Nodes0, Step, Entry0) then
        Entry0 = entry(EntryIndex, EntryName, EntryNode0),
        EntryNode0 = node(Text, SubNodes0),
        add_node(SubNodes0, Steps, Index, Name, NewNode, Res, SubNodes),
        EntryNode = node(Text, SubNodes),
        Entry = entry(EntryIndex, EntryName, EntryNode),
        replace_entry(Nodes0, Entry, Nodes)
    else
        string.append("invalid path component ", Step, Msg),
        Res = help_error(Msg),
        Nodes = Nodes0
    ).
add_node(Nodes0, [], Index, Name, Node, Res, Nodes) :-
    ( if
        list.member(Entry1, Nodes0),
        Entry1 = entry(Index, _, _)
    then
        Res = help_error("entry with given index already exists"),
        Nodes = Nodes0
    else if
        list.member(Entry1, Nodes0),
        Entry1 = entry(_, Name, _)
    then
        Res = help_error("entry with given name already exists"),
        Nodes = Nodes0
    else
        Res = help_ok,
        insert_into_entry_list(Nodes0, Index, Name, Node, Nodes)
    ).

:- pred insert_into_entry_list(list(entry)::in, int::in, string::in, node::in,
    list(entry)::out) is det.

insert_into_entry_list([], Index, Name, Node, [Entry]) :-
    Entry = entry(Index, Name, Node).
insert_into_entry_list([Head | Tail], Index, Name, Node, List) :-
    Head = entry(HeadIndex, _, _),
    ( if HeadIndex < Index then
        insert_into_entry_list(Tail, Index, Name, Node, NewTail),
        List = [Head | NewTail]
    else
        Entry = entry(Index, Name, Node),
        List = [Entry, Head | Tail]
    ).

%---------------------------------------------------------------------------%

help(Sys, Stream, !IO) :-
    print_entry_list(Sys, Stream, !IO).

path(Entries, Path, Stream, Result, !IO) :-
    (
        Path = [Step | Tail],
        ( if one_path_step(Entries, Step, Entry) then
            Entry = entry(_, _, EntryNode),
            (
                Tail = [],
                EntryNode = node(Text, _),
                io.write_string(Stream, Text, !IO),
                Result = help_ok
            ;
                Tail = [_ | _],
                EntryNode = node(_, SubEntries),
                path(SubEntries, Tail, Stream, Result, !IO)
            )
        else
            Msg = "error at path component """ ++ Step ++ """",
            Result = help_error(Msg)
        )
    ;
        Path = [],
        Result = help_error("the path does not go that deep")
    ).

name(Sys, Name, Stream, !IO) :-
    search_entry_list(Sys, Name, 0, Count, Stream, !IO),
    ( if Count = 0 then
        io.write_string("There is no such help topic.\n", !IO),
        help(Sys, Stream, !IO)
    else
        true
    ).

:- pred search_entry_list(list(entry)::in, string::in, int::in, int::out,
    io.output_stream::in, io::di, io::uo) is det.

search_entry_list([], _, !C, _, !IO).
search_entry_list([Entry | Tail], Name, !C, Stream, !IO) :-
    Entry = entry(_, EntryName, Node),
    ( if Name = EntryName then
        % We print this node, but don't search its children.
        print_node(Node, Stream, !IO),
        !:C = !.C + 1
    else
        search_node(Node, Name, !C, Stream, !IO),
        search_entry_list(Tail, Name, !C, Stream, !IO)
    ).

:- pred search_node(node::in, string::in, int::in, int::out,
    io.output_stream::in, io::di, io::uo) is det.

search_node(node(_, SubNodes), Name, !C, Stream, !IO) :-
    search_entry_list(SubNodes, Name, !C, Stream, !IO).

%---------------------------------------------------------------------------%

:- pred print_entry_list(list(entry)::in, io.output_stream::in,
    io::di, io::uo) is det.

print_entry_list([], _, !IO).
print_entry_list([entry(_, _, Node) | Nodes], Stream, !IO) :-
    print_node(Node, Stream, !IO),
    print_entry_list(Nodes, Stream, !IO).

:- pred print_node(node::in, io.output_stream::in, io::di, io::uo) is det.

print_node(node(Text, _Nodes), Stream, !IO) :-
    io.write_string(Stream, Text, !IO).
    % XXX print_entry_list(Nodes, Stream, !IO).

%---------------------------------------------------------------------------%

:- pred one_path_step(list(entry)::in, string::in, entry::out) is semidet.

one_path_step([Head | Tail], Name, Entry) :-
    Head = entry(_, HeadName, _),
    ( if HeadName = Name then
        Entry = Head
    else
        one_path_step(Tail, Name, Entry)
    ).

:- pred replace_entry(list(entry)::in, entry::in, list(entry)::out) is det.

replace_entry([], _, _) :-
    error("replace_entry: entry to be replaced not found").
replace_entry([Head | Tail], Entry, List) :-
    Head = entry(HeadIndex, _, _),
    Entry = entry(EntryIndex, _, _),
    ( if HeadIndex = EntryIndex then
        List = [Entry | Tail]
    else
        replace_entry(Tail, Entry, NewTail),
        List = [Head | NewTail]
    ).

%---------------------------------------------------------------------------%

help_system_type(Type) :-
    init(HelpInit),
    Type = type_of(HelpInit).

result_is_error(help_error(Msg), Msg).

%---------------------------------------------------------------------------%
