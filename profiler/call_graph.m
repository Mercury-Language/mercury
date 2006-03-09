%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998, 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% call_graph.m
%
% Main author: petdr.
%
% Responsible for building the static call graph. The dynamic call graph is
% built during the processing of 'Prof.CallPair', if the appropiate option
% is set.
%
%-----------------------------------------------------------------------------%

:- module call_graph.

:- interface.

:- import_module relation, io, list, string.

:- pred call_graph.main(list(string)::in,
    relation(string)::in, relation(string)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module read.
:- import_module options.
:- import_module globals.

:- import_module bool.
:- import_module require.
:- import_module std_util.

%-----------------------------------------------------------------------------%

call_graph.main(Args, !StaticCallGraph, !IO) :-
    globals.io_lookup_bool_option(dynamic_cg, Dynamic, !IO),
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    %
    % We can only build the static call graph if the *.prof files are
    % available. NB The dynamic call graph is built as it is read in
    % in process_addr_pair_file.
    %
    (
        Dynamic = yes
    ;
        Dynamic = no,
        build_static_call_graph(Args, VeryVerbose, !StaticCallGraph, !IO)
    ).

    % build_static_call_graph:
    % Builds the static call graph located in the *.prof files.
    %
:- pred build_static_call_graph(list(string)::in, bool::in,
    relation(string)::in, relation(string)::out, io::di, io::uo) is det.

build_static_call_graph(Files, VeryVerbose, !StaticCallGraph, !IO) :-
    list.foldl2(process_prof_file(VeryVerbose), Files, !StaticCallGraph, !IO).

    % process_prof_file:
    % Puts all the Caller and Callee label pairs from File into the
    % static call graph relation.
    %
:- pred process_prof_file(bool::in, string::in,
    relation(string)::in, relation(string)::out, io::di, io::uo) is det.

process_prof_file(VeryVerbose, File, !StaticCallGraph, !IO) :-
    maybe_write_string(VeryVerbose, "\n\tProcessing ", !IO),
    maybe_write_string(VeryVerbose, File, !IO),
    maybe_write_string(VeryVerbose, "...", !IO),
    io.see(File, Result, !IO),
    (
        Result = ok,
        process_prof_file_2(!StaticCallGraph, !IO),
        io.seen(!IO)
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMsg),
        io.stderr_stream(StdErr, !IO),
        io.write_strings(StdErr, ["mprof: error opening file `",
            File, "': ", ErrorMsg, "\n"], !IO)
    ),
    maybe_write_string(VeryVerbose, " done", !IO).

:- pred process_prof_file_2(relation(string)::in, relation(string)::out,
    io::di, io::uo) is det.

process_prof_file_2(!StaticCallGraph, !IO) :-
    maybe_read_label_name(MaybeLabelName, !IO),
    ( MaybeLabelName = yes(CallerLabel) ->
        read_label_name(CalleeLabel, !IO),
        relation.lookup_element(!.StaticCallGraph, CallerLabel, CallerKey),
        relation.lookup_element(!.StaticCallGraph, CalleeLabel, CalleeKey),
        relation.add(!.StaticCallGraph, CallerKey, CalleeKey,
            !:StaticCallGraph),
        process_prof_file_2(!StaticCallGraph, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module call_graph.
%-----------------------------------------------------------------------------%
