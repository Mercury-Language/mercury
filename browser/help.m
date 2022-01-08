%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2006 The University of Melbourne.
% Copyright (C) 2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
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

:- type help_system.

:- type path == list(string).

:- type help_res
    --->    help_ok
    ;       help_error(string).

%---------------------------------------------------------------------------%

    % Initialize an empty help system.
    %
:- pred init(help_system::out) is det.

    % Add a node to the given help system, at the given path, and with
    % the given name and index. If successful, return help_ok and the
    % updated help system; if not, return an error message and the
    % original help system.
    %
:- pred add_help_node(path::in, int::in, string::in, string::in,
    help_res::out, help_system::in, help_system::out) is det.

    % Print the top-level help nodes. This should give an overview
    % of the main topics for which help is available.
    %
:- pred print_top_level_help_nodes(io.output_stream::in, help_system::in,
    io::di, io::uo) is det.

    % Print the help node at the given path. If there is none,
    % print the top-level nodes.
    %
:- pred print_help_node_at_path(io.output_stream::in, help_system::in,
    path::in, help_res::out, io::di, io::uo) is det.

    % Print all help nodes with the given name. If there are none,
    % print the top-level nodes.
    %
:- pred print_help_for_name(io.output_stream::in, help_system::in, string::in,
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

:- type help_system == list(help_node).

:- type help_node
    --->    help_node(
                % The index of the node, which determines the position
                % of the node in the node list of its parent (if any) or
                % or in the system node list (if it has no parent).
                % Every node list is always sorted on this field.
                helpnode_index      :: int,

                % The name of the node, which should be one word or phrase.
                % It must be unique within the list of nodes containing it,
                % but need not be unique globally.
                helpnode_name       :: string,

                % The actual help text in the node. Should be one or more
                % complete lines.
                helpnode_text       :: string,

                helpnode_children   :: list(help_node)
            ).

%---------------------------------------------------------------------------%

:- pragma foreign_export("C", init(out), "ML_HELP_init").
:- pragma foreign_export("C", add_help_node(in, in, in, in, out, in, out),
    "ML_HELP_add_help_node").
:- pragma foreign_export("C", print_top_level_help_nodes(in, in, di, uo),
    "ML_HELP_print_top_level_help_nodes").
:- pragma foreign_export("C", print_help_node_at_path(in, in, in, out, di, uo),
    "ML_HELP_print_help_node_at_path").
:- pragma foreign_export("C", print_help_for_name(in, in, in, di, uo),
    "ML_HELP_print_help_for_name").
:- pragma foreign_export("C", help_system_type(out),
    "ML_HELP_help_system_type").
:- pragma foreign_export("C", result_is_error(in, out),
    "ML_HELP_result_is_error").

%---------------------------------------------------------------------------%

init([]).

add_help_node(Path, Index, Name, Text, Res, !Sys) :-
    Node = help_node(Index, Name, Text, []),
    add_node(Path, Node, Res, !Sys).

:- pred add_node(path::in, help_node::in, help_res::out,
    list(help_node)::in, list(help_node)::out) is det.

add_node([Step | Steps], NewNode, Res, Nodes0, Nodes) :-
    ( if one_path_step(Nodes0, Step, Node0) then
        Node0 = help_node(NodeIndex, NodeName, Text, SubNodes0),
        add_node(Steps, NewNode, Res, SubNodes0, SubNodes),
        Node = help_node(NodeIndex, NodeName, Text, SubNodes),
        replace_node_at_index(Node, Nodes0, Nodes)
    else
        Msg = "invalid path component " ++ Step,
        Res = help_error(Msg),
        Nodes = Nodes0
    ).
add_node([], NewNode, Res, Nodes0, Nodes) :-
    ( if
        some [MemberNode] (
            list.member(MemberNode, Nodes0),
            MemberNode ^ helpnode_index = NewNode ^ helpnode_index
        )
    then
        Res = help_error("entry with given index already exists"),
        Nodes = Nodes0
    else if
        some [MemberNode] (
            list.member(MemberNode, Nodes0),
            MemberNode ^ helpnode_name = NewNode ^ helpnode_name
        )
    then
        Res = help_error("entry with given name already exists"),
        Nodes = Nodes0
    else
        Res = help_ok,
        insert_into_node_list(NewNode, Nodes0, Nodes)
    ).

:- pred replace_node_at_index(help_node::in,
    list(help_node)::in, list(help_node)::out) is det.

replace_node_at_index(_, [], _) :-
    unexpected($pred, "node to be replaced not found").
replace_node_at_index(Node, [Head | Tail], List) :-
    ( if Head ^ helpnode_index = Node ^ helpnode_index then
        List = [Node | Tail]
    else
        replace_node_at_index(Node, Tail, NewTail),
        List = [Head | NewTail]
    ).

:- pred insert_into_node_list(help_node::in,
    list(help_node)::in, list(help_node)::out) is det.

insert_into_node_list(Node, [], [Node]).
insert_into_node_list(Node, [Head | Tail], List) :-
    ( if Head ^ helpnode_index < Node ^ helpnode_index then
        insert_into_node_list(Node, Tail, NewTail),
        List = [Head | NewTail]
    else
        List = [Node, Head | Tail]
    ).

%---------------------------------------------------------------------------%

print_top_level_help_nodes(Stream, Sys, !IO) :-
    print_node_list(Stream, Sys, !IO).

print_help_node_at_path(Stream, Nodes, Path, Result, !IO) :-
    (
        Path = [Step | Tail],
        ( if one_path_step(Nodes, Step, Node) then
            (
                Tail = [],
                print_node(Stream, Node, !IO),
                Result = help_ok
            ;
                Tail = [_ | _],
                SubNodes = Node ^ helpnode_children,
                print_help_node_at_path(Stream, SubNodes, Tail, Result, !IO)
            )
        else
            Msg = "error at path component """ ++ Step ++ """",
            Result = help_error(Msg)
        )
    ;
        Path = [],
        Result = help_error("the path does not go that deep")
    ).

print_help_for_name(Stream, Sys, Name, !IO) :-
    search_node_list(Sys, Name, [], RevMatchedNodes),
    (
        RevMatchedNodes = [],
        io.write_string(Stream, "There is no such help topic.\n", !IO),
        print_top_level_help_nodes(Stream, Sys, !IO)
    ;
        RevMatchedNodes = [_ | _],
        list.reverse(RevMatchedNodes, MatchedNodes),
        print_node_list(Stream, MatchedNodes, !IO)
    ).

:- pred search_node_list(list(help_node)::in, string::in,
    list(help_node)::in, list(help_node)::out) is det.

search_node_list([], _, !RevMatchedNodes).
search_node_list([Node | Nodes], Name, !RevMatchedNodes) :-
    ( if Name = Node ^ helpnode_name then
        !:RevMatchedNodes = [Node | !.RevMatchedNodes]
    else
        search_node_list(Node ^ helpnode_children, Name, !RevMatchedNodes),
        search_node_list(Nodes, Name, !RevMatchedNodes)
    ).

%---------------------------------------------------------------------------%

:- pred print_node_list(io.output_stream::in, list(help_node)::in,
    io::di, io::uo) is det.

print_node_list(_, [], !IO).
print_node_list(Stream, [Node | Nodes], !IO) :-
    print_node(Stream, Node, !IO),
    print_node_list(Stream, Nodes, !IO).

:- pred print_node(io.output_stream::in, help_node::in, io::di, io::uo) is det.

print_node(Stream, Node, !IO) :-
    Node = help_node(_, _, Text, _),
    io.write_string(Stream, Text, !IO).

%---------------------------------------------------------------------------%

:- pred one_path_step(list(help_node)::in, string::in, help_node::out)
    is semidet.

one_path_step([Head | Tail], Name, Node) :-
    ( if Head ^ helpnode_name = Name then
        Node = Head
    else
        one_path_step(Tail, Name, Node)
    ).

%---------------------------------------------------------------------------%

help_system_type(Type) :-
    init(HelpInit),
    Type = type_of(HelpInit).

result_is_error(help_error(Msg), Msg).

%---------------------------------------------------------------------------%
:- end_module mdb.help.
%---------------------------------------------------------------------------%
