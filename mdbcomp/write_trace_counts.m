%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2008, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018, 2021-2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: trace_counts.m.
% Main author: wangp.
% Modifications by zs and maclarty.
%
% This module defines predicates to write out execution trace summaries.
%
%---------------------------------------------------------------------------%

:- module mdbcomp.write_trace_counts.
:- interface.

:- import_module mdbcomp.trace_counts.

:- import_module io.


    % write_trace_counts_to_file(FileType, TraceCounts, FileName, Result, !IO):
    %
    % Write the given trace counts to FileName in a format suitable for
    % reading with read_trace_counts/4.
    %
:- pred write_trace_counts_to_file(trace_count_file_type::in, trace_counts::in,
    string::in, io.res::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.

:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term_io.

%---------------------------------------------------------------------------%

write_trace_counts_to_file(FileType, TraceCounts, FileName, Result, !IO) :-
    io.open_output(FileName, FileResult, !IO),
    (
        FileResult = ok(FileStream),
        Result = ok,
        io.write_string(FileStream, trace_count_file_id, !IO),
        io.nl(FileStream, !IO),
        write_trace_counts(FileStream, FileType, TraceCounts, !IO),
        io.close_output(FileStream, !IO)
    ;
        FileResult = error(Error),
        Result = error(Error)
    ).

:- pred write_trace_counts(io.text_output_stream::in,
    trace_count_file_type::in, trace_counts::in, io::di, io::uo) is det.

write_trace_counts(OutputStream, FileType, TraceCounts, !IO) :-
    io.write(OutputStream, FileType, !IO),
    io.write_string(OutputStream, ".\n", !IO),
    map.foldl3(write_proc_label_and_file_trace_counts(OutputStream),
        TraceCounts, unqualified(""), _, "", _, !IO).

:- pred write_proc_label_and_file_trace_counts(io.text_output_stream::in,
    proc_label_in_context::in, proc_trace_counts::in,
    sym_name::in, sym_name::out, string::in, string::out,
    io::di, io::uo) is det.

write_proc_label_and_file_trace_counts(OutputStream, ProcLabelInContext,
        PathPortCounts, !CurModuleNameSym, !CurFileName, !IO) :-
    ProcLabelInContext = proc_label_in_context(ModuleNameSym, FileName,
        ProcLabel),
    ( if ModuleNameSym = !.CurModuleNameSym then
        true
    else
        ModuleName = sym_name_to_string(ModuleNameSym),
        io.write_string(OutputStream, "module ", !IO),
        term_io.format_quoted_atom(OutputStream, ModuleName, !IO),
        io.write_string(OutputStream, "\n", !IO),
        !:CurModuleNameSym = ModuleNameSym
    ),
    ( if FileName = !.CurFileName then
        true
    else
        io.write_string(OutputStream, "file ", !IO),
        term_io.format_quoted_atom(OutputStream, FileName, !IO),
        io.write_string(OutputStream, "\n", !IO),
        !:CurFileName = FileName
    ),
    write_proc_label_and_check(OutputStream, ModuleNameSym, ProcLabel, !IO),
    map.foldl(write_path_port_count(OutputStream), PathPortCounts, !IO).

:- pred write_proc_label_and_check(io.text_output_stream::in, sym_name::in,
    proc_label::in, io::di, io::uo) is det.

write_proc_label_and_check(OutputStream, ModuleNameSym, ProcLabel, !IO) :-
    (
        ProcLabel = ordinary_proc_label(DefModuleSym, _, _, _, _, _),
        require(unify(ModuleNameSym, DefModuleSym),
            "write_proc_label_and_check: module mismatch")
    ;
        % We don't record trace counts in special preds.
        ProcLabel = special_proc_label(_, _, _, _, _, _),
        error("write_proc_label: special_pred")
    ),
    write_proc_label(OutputStream, ProcLabel, !IO).

    % Write out the given proc_label.
    %
:- pred write_proc_label(io.text_output_stream::in, proc_label::in,
    io::di, io::uo) is det.

write_proc_label(OutputStream, ProcLabel, !IO) :-
    (
        ProcLabel = ordinary_proc_label(DefModuleSym, PredOrFunc,
            DeclModuleSym, Name, Arity, Mode),
        (
            PredOrFunc = pf_predicate,
            ( if DeclModuleSym = DefModuleSym then
                io.write_string(OutputStream, "pproc ", !IO)
            else
                DeclModule = sym_name_to_string(DeclModuleSym),
                io.write_string(OutputStream, "pprocdecl ", !IO),
                term_io.format_quoted_atom(OutputStream, DeclModule, !IO),
                io.write_string(OutputStream, " ", !IO)
            )
        ;
            PredOrFunc = pf_function,
            ( if DeclModuleSym = DefModuleSym then
                io.write_string(OutputStream, "fproc ", !IO)
            else
                DeclModule = sym_name_to_string(DeclModuleSym),
                io.write_string(OutputStream, "fprocdecl ", !IO),
                term_io.format_quoted_atom(OutputStream, DeclModule, !IO),
                io.write_string(OutputStream, " ", !IO)
            )
        ),
        term_io.format_quoted_atom(OutputStream, Name, !IO),
        io.format(OutputStream, " %d %d\n", [i(Arity), i(Mode)], !IO)
    ;
        % We don't record trace counts in special preds.
        ProcLabel = special_proc_label(_, _, _, _, _, _),
        error("write_proc_label: special_pred")
    ).

:- pred write_path_port_count(io.text_output_stream::in,
    path_port::in, line_no_and_count::in, io::di, io::uo) is det.

write_path_port_count(OutputStream, PathPort, LineNoAndCount, !IO) :-
    LineNoAndCount = line_no_and_count(LineNo, ExecCount, NumTests),
    (
        PathPort = port_only(Port),
        string_to_trace_port(PortStr, Port),
        io.format(OutputStream, "%s %d %d %d\n",
            [s(PortStr), i(LineNo), i(ExecCount), i(NumTests)], !IO)
    ;
        PathPort = path_only(Path),
        io.format(OutputStream, "<%s> %d %d %d\n",
            [s(rev_goal_path_to_string(Path)),
            i(LineNo), i(ExecCount), i(NumTests)], !IO)
    ;
        PathPort = port_and_path(Port, Path),
        string_to_trace_port(PortStr, Port),
        io.format(OutputStream, "%s <%s> %d %d %d\n",
            [s(PortStr), s(rev_goal_path_to_string(Path)),
            i(LineNo), i(ExecCount), i(NumTests)], !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module mdbcomp.write_trace_counts.
%---------------------------------------------------------------------------%
