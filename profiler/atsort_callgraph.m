%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997, 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: atsort_callgraph.m.
% Main author: petdr.
%
% Takes a list of files which contains the callgraph of a Mercury module
% and approximately topologically sorts them to standard output.
%
% XXX This program uses the "relation" module. Since that module was deleted
% from the Mercury standard library several years ago, this program
% must have been nonfunctional for at least as long.
%
% Anyone who wants to make it work again will have make it use the digraph
% module instead of the relation module.
%
%---------------------------------------------------------------------------%

:- module atsort_callgraph.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module read.

:- import_module relation.
:- import_module set.

main(!IO) :-
    io.command_line_arguments(Files, !IO),
    relation.init(CallGraph0),
    build_call_graph(Files, CallGraph0, CallGraph, !IO),
    relation.atsort(CallGraph, Cliques),
    list.reverse(Cliques, RevCliques),
    print_cliques(RevCliques, !IO).

%---------------------------------------------------------------------------%

    % Builds the call graph located in the *.prof files.
    %
:- pred build_call_graph(list(string)::in,
    relation(string)::in, relation(string)::out, io::di, io::uo) is det.

build_call_graph([], !CallGraph, !IO).
build_call_graph([File | Files], !CallGraph, !IO) :-
    process_prof_file(File, !CallGraph, !IO),
    build_call_graph(Files, !CallGraph, !IO).

    % Puts all the Caller and Callee label pairs from File into the
    % call graph relation.
    %
:- pred process_prof_file(string::in,
    relation(string)::in, relation(string)::out, io::di, io::uo) is det.

process_prof_file(File, !CallGraph, !IO) :-
    io.see(File, Result, !IO),
    (
        Result = ok,
        process_prof_file_2(!CallGraph, !IO),
        io.seen(!IO)
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMsg),
        io.stderr_stream(StdErr, !IO),
        io.write_strings(StdErr,
            ["atsort_callgraph: error opening file `",
            File, "': ", ErrorMsg, "\n"], !IO)
    ).

:- pred process_prof_file_2(relation(string)::in, relation(string)::out,
    io::di, io::uo) is det.

process_prof_file_2(!CallGraph, !IO) :-
    maybe_read_label_name(MaybeLabelName, !IO),
    (
        MaybeLabelName = yes(CallerLabel),
        read_label_name(CalleeLabel, !IO),
        svrelation.add(CallerLabel, CalleeLabel, !CallGraph),
        process_prof_file_2(!CallGraph, !IO)
    ;
        MaybeLabelName = no
    ).

%---------------------------------------------------------------------------%

:- pred print_cliques(list(set(string))::in, io::di, io::uo) is det.

print_cliques([], !IO).
print_cliques([Clique | Cliques], !IO) :-
    print_clique(Clique, !IO),
    print_cliques(Cliques, !IO).

:- pred print_clique(set(string)::in, io::di, io::uo) is det.

print_clique(Clique, !IO) :-
    set.to_sorted_list(Clique, CliqueList),
    print_list(CliqueList, !IO).

:- pred print_list(list(string), io.state, io.state).
:- mode print_list(in, di, uo) is det.

print_list([], !IO).
print_list([C | Cs], !IO) :-
    io.write_string(C, !IO),
    io.write_string("\n", !IO),
    print_list(Cs, !IO).

%---------------------------------------------------------------------------%
:- end_module atsort_callgraph.
%---------------------------------------------------------------------------%
