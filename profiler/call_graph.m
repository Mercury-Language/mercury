%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998, 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: call_graph.m
% Main author: petdr.
%
% Responsible for building the static call graph. The dynamic call graph is
% built during the processing of 'Prof.CallPair', if the appropriate option
% is set.
%
%---------------------------------------------------------------------------%

:- module call_graph.
:- interface.

:- import_module digraph.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%


:- pred build_call_graph(io.text_output_stream::in, list(string)::in,
    digraph(string)::in, digraph(string)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module read.
:- import_module options.
:- import_module globals.

:- import_module bool.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

build_call_graph(ProgressStream, Args, !StaticCallGraph, !IO) :-
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
        build_static_call_graph(ProgressStream, Args, VeryVerbose,
            !StaticCallGraph, !IO)
    ).

    % build_static_call_graph:
    %
    % Builds the static call graph located in the *.prof files.
    %
:- pred build_static_call_graph(io.text_output_stream::in, list(string)::in,
    bool::in, digraph(string)::in, digraph(string)::out,
    io::di, io::uo) is det.

build_static_call_graph(ProgressStream, Files, VeryVerbose,
        !StaticCallGraph, !IO) :-
    list.foldl2(process_prof_file(ProgressStream, VeryVerbose), Files,
        !StaticCallGraph, !IO).

    % process_prof_file:
    %
    % Puts all the Caller and Callee label pairs from File into the
    % static call graph.
    %
:- pred process_prof_file(io.text_output_stream::in, bool::in, string::in,
    digraph(string)::in, digraph(string)::out, io::di, io::uo) is det.

process_prof_file(ProgressStream, VeryVerbose, File, !StaticCallGraph, !IO) :-
    maybe_write_string(ProgressStream, VeryVerbose, "\n\tProcessing ", !IO),
    maybe_write_string(ProgressStream, VeryVerbose, File, !IO),
    maybe_write_string(ProgressStream, VeryVerbose, "...", !IO),
    io.open_input(File, Result, !IO),
    (
        Result = ok(FileStream),
        process_prof_file_2(FileStream, !StaticCallGraph, !IO),
        io.close_input(FileStream, !IO)
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMsg),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "mprof: error opening file `%s': %s\n",
            [s(File), s(ErrorMsg)], !IO)
    ),
    maybe_write_string(ProgressStream, VeryVerbose, " done", !IO).

:- pred process_prof_file_2(io.text_input_stream::in,
    digraph(string)::in, digraph(string)::out, io::di, io::uo) is det.

process_prof_file_2(InputStream, !StaticCallGraph, !IO) :-
    maybe_read_label_name(InputStream, MaybeLabelName, !IO),
    (
        MaybeLabelName = yes(CallerLabel),
        read_label_name(InputStream, CalleeLabel, !IO),
        digraph.lookup_key(!.StaticCallGraph, CallerLabel, CallerKey),
        digraph.lookup_key(!.StaticCallGraph, CalleeLabel, CalleeKey),
        digraph.add_edge(CallerKey, CalleeKey, !StaticCallGraph),
        process_prof_file_2(InputStream, !StaticCallGraph, !IO)
    ;
        MaybeLabelName = no
    ).

%---------------------------------------------------------------------------%
:- end_module call_graph.
%---------------------------------------------------------------------------%
