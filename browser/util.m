%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2002, 2004-2007, 2010-2011 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%

:- module mdb.util.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- func is_predicate(pred_or_func) = bool.
:- func is_function(pred_or_func) = bool.

:- type line_number == int.

    % Get user input via the same method used by the internal debugger.
    %
:- pred trace_getline(string::in, io.result(string)::out,
    io::di, io::uo) is det.

:- pred trace_getline(string::in, io.result(string)::out,
    io.input_stream::in, io.output_stream::in, io::di, io::uo) is det.

    % trace_get_command is similar to trace_getline except that it
    % breaks lines into semicolon separated commands, and replaces
    % EOF with the command 'quit'.
    %
:- pred trace_get_command(string::in, string::out, io::di, io::uo)
    is det.

:- pred trace_get_command(string::in, string::out,
    io.input_stream::in, io.output_stream::in, io::di, io::uo) is det.

:- pred zip_with(pred(T1, T2, T3)::in(pred(in, in, out) is det),
    list(T1)::in, list(T2)::in, list(T3)::out) is det.

    % Apply predicate to argument repeatedly until the result
    % remains the same.
    %
:- pred limit(pred(list(T), list(T))::in(pred(in, out) is det),
    list(T)::in, list(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

is_predicate(pf_predicate) = yes.
is_predicate(pf_function) = no.

is_function(pf_predicate) = no.
is_function(pf_function) = yes.

trace_getline(Prompt, Result, !IO) :-
    io.input_stream(MdbIn, !IO),
    io.output_stream(MdbOut, !IO),
    trace_getline(Prompt, Result, MdbIn, MdbOut, !IO).

trace_getline(Prompt, Result, MdbIn, MdbOut, !IO) :-
    call_trace_getline(MdbIn, MdbOut, Prompt, Line, Success, !IO),
    ( if Success = 0 then
        Result = eof
    else
        Result = ok(Line)
    ).

:- pred call_trace_getline(input_stream::in, output_stream::in, string::in,
    string::out, int::out, io.state::di, io.state::uo) is det.

:- pragma foreign_decl("C", "
    #include ""mercury_wrapper.h""
    #include ""mercury_string.h""
    #include ""mercury_trace_base.h""
    #include ""mercury_library_types.h""
").

:- pragma foreign_proc("C",
    call_trace_getline(MdbIn::in, MdbOut::in, Prompt::in, Line::out,
        Success::out, _IO0::di, _IO::uo),
    % We need to use will_not_call_mercury here,
    % because MR_make_aligned_string_copy() references MR_hp,
    % which only works for will_not_call_mercury foreign_procs.
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    char        *line = NULL;
    MercuryFile *mdb_in = (MercuryFile *) MdbIn;
    MercuryFile *mdb_out = (MercuryFile *) MdbOut;

    if (MR_address_of_trace_getline != NULL) {
        line = (*MR_address_of_trace_getline)((char *) Prompt,
                MR_file(*mdb_in), MR_file(*mdb_out));
    } else {
        MR_tracing_not_enabled();
        /* not reached */
    }

    if (line == NULL) {
        /* we copy the null string to avoid warnings about const */
        MR_make_aligned_string_copy(Line, """");
        Success = 0;
    } else {
        MR_make_aligned_string_copy(Line, line);
        MR_free(line);
        Success = 1;
    }
").

call_trace_getline(MdbIn, MdbOut, Prompt, Line, Success, !IO) :-
    io.write_string(MdbOut, Prompt, !IO),
    io.flush_output(MdbOut, !IO),
    io.read_line_as_string(MdbIn, Result, !IO),
    (
        Result = ok(Line),
        Success = 1
    ;
        Result = eof,
        Line = "",
        Success = 0
    ;
        Result = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

trace_get_command(Prompt, Result, !IO) :-
    io.input_stream(MdbIn, !IO),
    io.output_stream(MdbOut, !IO),
    trace_get_command(Prompt, Result, MdbIn, MdbOut, !IO).

:- pragma foreign_proc("C",
    trace_get_command(Prompt::in, Line::out, MdbIn::in,
        MdbOut::in, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    char        *line;
    MercuryFile *mdb_in = (MercuryFile *) MdbIn;
    MercuryFile *mdb_out = (MercuryFile *) MdbOut;

    if (MR_address_of_trace_getline != NULL) {
        line = (*MR_address_of_trace_get_command)(
                (char *) Prompt,
                MR_file(*mdb_in), MR_file(*mdb_out));

        MR_make_aligned_string_copy(Line, line);
        MR_free(line);

    } else {
        ML_BROWSER_trace_get_command_fallback(Prompt, &Line, MdbIn, MdbOut);
    }
").

trace_get_command(Prompt, Line, MdbIn, MdbOut, !IO) :-
    trace_get_command_fallback(Prompt, Line, MdbIn, MdbOut, !IO).

    % This is called by trace_get_command when the trace library is not linked
    % in.
    %
:- pred trace_get_command_fallback(string::in, string::out, io.input_stream::in,
    io.output_stream::in, io::di, io::uo) is det.

:- pragma foreign_export("C",
    trace_get_command_fallback(in, out, in, in, di, uo),
    "ML_BROWSER_trace_get_command_fallback").

trace_get_command_fallback(Prompt, String, MdbIn, MdbOut, !IO) :-
    io.write_string(MdbOut, Prompt, !IO),
    io.flush_output(MdbOut, !IO),
    io.read_line_as_string(MdbIn, Result, !IO),
    (
        Result = ok(String)
    ;
        Result = eof,
        String = "quit"
    ;
        Result = error(Error),
        unexpected($pred, io.error_message(Error))
    ).

zip_with(Pred, XXs, YYs, Zipped) :-
    ( if XXs = [], YYs = [] then
        Zipped = []
    else if XXs = [X | Xs], YYs = [Y | Ys] then
        Pred(X, Y, PXY),
        zip_with(Pred, Xs, Ys, ZippedTail),
        Zipped = [PXY | ZippedTail]
    else
        unexpected($pred, "list arguments are of unequal length")
    ).

limit(Pred, Xs, Ys) :-
    Pred(Xs, Zs),
    ( if Xs = Zs then
        Ys = Zs
    else
        limit(Pred, Zs, Ys)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
