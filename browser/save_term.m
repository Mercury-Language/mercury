%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: save_term.m.
%
% This module implements saving terms to files.
%
%---------------------------------------------------------------------------%

:- module mdb.save_term.
:- interface.

:- import_module mdb.browser_info.
:- import_module mdb.browser_term.

:- import_module io.

%---------------------------------------------------------------------------%

    % save_term_to_file(OutputStream, FileName, Format, BrowserTerm, !IO):
    %
    % Save BrowserTerm to the file FileName. If there is an error,
    % print an error message to OutputStream.
    %
    % The format of the saved term can be influenced by the Format
    % argument, but how this works is not specified.
    %
:- pred save_term_to_file(io.text_output_stream::in, string::in, string::in,
    browser_term::in, io::di, io::uo) is cc_multi.

    % save_term_to_file_xml(OutputStream, FileName, BrowserTerm, !IO):
    %
    % Save BrowserTerm to FileName as an XML document. If there is an error,
    % print an error message to OutputStream.
    %
:- pred save_term_to_file_xml(io.text_output_stream::in, string::in,
    browser_term::in, io::di, io::uo) is cc_multi.

    % save_term_to_file_doc(OutputStream, FileName, BrowserTerm, !IO):
    %
    % Save BrowserTerm to FileName as a document prettyprinted by the
    % pretty_printer module in the Mercury standard library. If there is
    % an error, print an error message to OutputStream.
    %
:- pred save_term_to_file_doc(io.text_output_stream::in, string::in,
    browser_term::in, io::di, io::uo) is det.

    % Save BrowserTerm in an HTML file and launch the web browser specified
    % by the web_browser_cmd field in the browser_persistent_state.
    %
:- pred save_and_browse_browser_term_web(io.text_output_stream::in,
    io.text_output_stream::in, browser_term::in,
    browser_persistent_state::in, io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.term_to_html.

:- import_module deconstruct.
:- import_module dir.
:- import_module int.
:- import_module io.call_system.
:- import_module io.environment.
:- import_module io.file.
:- import_module list.
:- import_module maybe.
:- import_module pretty_printer.
:- import_module stream.
:- import_module string.
:- import_module term_to_xml.
:- import_module type_desc.
:- import_module univ.

%---------------------------------------------------------------------------%
%
% We export these predicates to C for use by the tracer:
% they are used in trace/mercury_trace_browse.c.
%

:- pragma foreign_export("C",
    save_term_to_file(in, in, in, in, di, uo),
    "ML_BROWSE_save_term_to_file").
:- pragma foreign_export("C",
    save_term_to_file_xml(in, in, in, di, uo),
    "ML_BROWSE_save_term_to_file_xml").
:- pragma foreign_export("C",
    save_term_to_file_doc(in, in, in, di, uo),
    "ML_BROWSE_save_term_to_file_doc").
:- pragma foreign_export("C",
    save_and_browse_browser_term_web(in, in, in, in, di, uo),
    "ML_BROWSE_save_and_browse_browser_term_web").

%---------------------------------------------------------------------------%

save_term_to_file(OutputStream, FileName, _Format, BrowserTerm, !IO) :-
    trace [compile_time(flag("debug_save_term_to_file")), io(!TIO)] (
        io.format(OutputStream, "%s\n", [s(FileName)], !TIO),
        io.write_line(OutputStream, BrowserTerm, !TIO)
    ),
    io.open_output(FileName, FileStreamResult, !IO),
    (
        FileStreamResult = ok(FileStream),
        (
            BrowserTerm = plain_term(Term),
            save_univ(FileStream, 0, Term, !IO),
            io.nl(FileStream, !IO)
        ;
            BrowserTerm = synthetic_term(Functor, ArgUnivs, MaybeResultUniv),
            io.write_string(FileStream, Functor, !IO),
            io.write_string(FileStream, "(\n", !IO),
            save_arg_univs(FileStream, 1, ArgUnivs, !IO),
            io.write_string(FileStream, "\n)\n", !IO),
            (
                MaybeResultUniv = no
            ;
                MaybeResultUniv = yes(ResultUniv),
                io.write_string(FileStream, "=\n", !IO),
                save_univ(FileStream, 1, ResultUniv, !IO),
                io.write_string(FileStream, "\n", !IO)
            )
        ),
        io.close_output(FileStream, !IO)
    ;
        FileStreamResult = error(Error),
        io.error_message(Error, Msg),
        io.write_string(OutputStream, Msg, !IO)
    ).

:- pred save_univ(io.text_output_stream::in, int::in, univ::in,
    io::di, io::uo) is cc_multi.

save_univ(OutputStream, Indent, Univ, !IO) :-
    save_term(OutputStream, Indent, univ_value(Univ), !IO).

:- pred save_term(io.text_output_stream::in, int::in, T::in,
    io::di, io::uo) is cc_multi.

save_term(OutputStream, Indent, Term, !IO) :-
    ( if dynamic_cast_to_list(Term, List) then
        (
            List = [],
            write_indent(OutputStream, Indent, !IO),
            io.write_string(OutputStream, "[]", !IO)
        ;
            List = [_ | _],
            MakeUniv =
                ( func(Element) = (ElementUniv) :-
                    ElementUniv = univ(Element)
                ),
            Univs = list.map(MakeUniv, List),
            write_indent(OutputStream, Indent, !IO),
            io.write_string(OutputStream, "[\n", !IO),
            save_arg_univs(OutputStream, Indent + 1, Univs, !IO),
            io.write_string(OutputStream, "\n", !IO),
            write_indent(OutputStream, Indent, !IO),
            io.write_string(OutputStream, "]", !IO)
        )
    else
        deconstruct(Term, include_details_cc, Functor, _Arity, Args),
        write_indent(OutputStream, Indent, !IO),
        io.write_string(OutputStream, Functor, !IO),
        (
            Args = []
        ;
            Args = [_ | _],
            io.write_string(OutputStream, "(\n", !IO),
            save_arg_univs(OutputStream, Indent + 1, Args, !IO),
            io.write_string(OutputStream, "\n", !IO),
            write_indent(OutputStream, Indent, !IO),
            io.write_string(OutputStream, ")", !IO)
        )
    ).

:- pred save_arg_univs(io.text_output_stream::in, int::in, list(univ)::in,
    io::di, io::uo) is cc_multi.

save_arg_univs(_OutputStream, _Indent, [], !IO).
save_arg_univs(OutputStream, Indent, [Univ | Univs], !IO) :-
    save_univ(OutputStream, Indent, Univ, !IO),
    (
        Univs = []
    ;
        Univs = [_ | _],
        io.write_string(OutputStream, ",\n", !IO),
        save_arg_univs(OutputStream, Indent, Univs, !IO)
    ).

:- some [T2] pred dynamic_cast_to_list(T1::in, list(T2)::out) is semidet.

dynamic_cast_to_list(X, L) :-
    % The code of this predicate is copied from pprint.m.
    [ArgTypeDesc] = type_args(type_of(X)),
    (_ `with_type` ArgType) `has_type` ArgTypeDesc,
    dynamic_cast(X, L `with_type` list(ArgType)).

:- pred write_indent(io.text_output_stream::in, int::in, io::di, io::uo)
    is det.

write_indent(OutputStream, Indent, !IO) :-
    ( if Indent =< 0 then
        true
    else
        io.write_char(OutputStream, ' ', !IO),
        write_indent(OutputStream, Indent - 1, !IO)
    ).

%---------------------%

:- type xml_predicate_wrapper
    --->    predicate(
                predicate_name      :: string,
                predicate_arguments :: list(univ)
            ).

:- type xml_function_wrapper
    --->    function(
                function_name       :: string,
                function_arguments  :: list(univ),
                return_value        :: univ
            ).

save_term_to_file_xml(OutputStream, FileName, BrowserTerm, !IO) :-
    io.open_output(FileName, FileStreamResult, !IO),
    (
        FileStreamResult = ok(FileStream),
        % Note that the three calls to write_xml_doc_general_cc cannot be
        % replaced by one call, because the type of the second arguments
        % is different in each of the three calls.
        (
            BrowserTerm = plain_term(Univ),
            Term = univ_value(Univ),
            term_to_xml.write_xml_doc_general_cc(FileStream, Term,
                simple, no_stylesheet,  no_dtd, _, !IO)
        ;
            BrowserTerm = synthetic_term(Functor, ArgUnivs, MaybeResultUniv),
            (
                MaybeResultUniv = no,
                PredicateTerm = predicate(Functor, ArgUnivs),
                term_to_xml.write_xml_doc_general_cc(FileStream, PredicateTerm,
                    simple, no_stylesheet, no_dtd, _, !IO)
            ;
                MaybeResultUniv = yes(ResultUniv),
                FunctionTerm = function(Functor, ArgUnivs, ResultUniv),
                term_to_xml.write_xml_doc_general_cc(FileStream, FunctionTerm,
                    simple, no_stylesheet, no_dtd, _, !IO)
            )
        ),
        io.close_output(FileStream, !IO)
    ;
        FileStreamResult = error(Error),
        io.error_message(Error, Msg),
        io.format(OutputStream, "%s\n", [s(Msg)], !IO)
    ).

%---------------------%

save_term_to_file_doc(OutputStream, FileName, BrowserTerm, !IO) :-
    io.open_output(FileName, FileStreamResult, !IO),
    (
        FileStreamResult = ok(FileStream),
        (
            BrowserTerm = plain_term(Univ),
            Term = univ_value(Univ),
            Doc = pretty_printer.format(Term)
        ;
            BrowserTerm = synthetic_term(Functor, ArgUnivs, MaybeResultUniv),
            (
                MaybeResultUniv = no,
                PredicateTerm = predicate(Functor, ArgUnivs),
                Doc = pretty_printer.format(PredicateTerm)
            ;
                MaybeResultUniv = yes(ResultUniv),
                FunctionTerm = function(Functor, ArgUnivs, ResultUniv),
                Doc = pretty_printer.format(FunctionTerm)
            )
        ),
        Canonicalize = include_details_cc,
        get_default_formatter_map(FMap, !IO),
        Params = pp_params(78, int.max_int, triangular(int.max_int)),
        promise_equivalent_solutions [!:IO] (
            pretty_printer.put_doc(FileStream, Canonicalize, FMap, Params,
                Doc, !IO)
        ),
        io.close_output(FileStream, !IO)
    ;
        FileStreamResult = error(Error),
        io.error_message(Error, Msg),
        io.format(OutputStream, "%s\n", [s(Msg)], !IO)
    ).

%---------------------------------------------------------------------------%

save_and_browse_browser_term_web(OutputStream, ErrorStream, Term, State,
        !IO) :-
    get_mdb_dir(MaybeMdbDir, !IO),
    (
        MaybeMdbDir = yes(MdbDir),
        MaybeBrowserCmd = State ^ web_browser_cmd,
        (
            MaybeBrowserCmd = yes(BrowserCmd),
            io.file.get_temp_directory(TmpDir, !IO),
            io.file.make_temp_file(TmpDir, "mdb", ".html", TmpResult, !IO),
            (
                TmpResult = ok(TmpFileName0),
                ( if string.suffix(TmpFileName0, ".html") then
                    TmpFileName = TmpFileName0
                else
                    % Work around io.make_temp_file ignoring suffix.
                    io.file.remove_file(TmpFileName0, _, !IO),
                    TmpFileName = TmpFileName0 ++ ".html"
                ),
                save_term_to_file_web(TmpFileName, Term, MdbDir,
                    SaveResult, !IO),
                (
                    SaveResult = ok(_),
                    % We should actually quote the file name.
                    CommandStr = BrowserCmd ++ " " ++ TmpFileName,
                    launch_web_browser(OutputStream, ErrorStream,
                        CommandStr, !IO)
                ;
                    SaveResult = error(Error),
                    io.error_message(Error, Msg),
                    io.format(ErrorStream,
                        "Error opening file `%s': %s\n",
                        [s(TmpFileName), s(Msg)], !IO)
                )
            ;
                TmpResult = error(Error),
                io.error_message(Error, Msg),
                io.format(ErrorStream,
                    "Error opening temporary file: %s\n",
                    [s(Msg)], !IO)
            )
        ;
            MaybeBrowserCmd = no,
            io.write_string(ErrorStream,
                "mdb: You need to specify the shell command that launches " ++
                "your preferred web browser, by issuing an mdb command " ++
                "\"web_browser_cmd <command>\".\n", !IO)
        )
    ;
        MaybeMdbDir = no,
        io.write_string(ErrorStream,
            "Could not determine directory containing mdb files.\n", !IO)
    ).

:- pred get_mdb_dir(maybe(string)::out, io::di, io::uo) is det.

get_mdb_dir(Res, !IO) :-
    io.environment.get_environment_var("MERCURY_DEBUGGER_INIT",
        MaybeValue, !IO),
    ( if
        MaybeValue = yes(Path),
        dir.path_name_is_absolute(Path),
        dir.split_name(Path, MdbDir, "mdbrc")
    then
        Res = yes(MdbDir)
    else
        Res = no
    ).

:- pred save_term_to_file_web(string::in, browser_term::in, string::in,
    io.res(io.text_output_stream)::out, io::di, io::uo) is cc_multi.

save_term_to_file_web(FileName, BrowserTerm, MdbDir, FileStreamRes,
        !IO) :-
    io.open_output(FileName, FileStreamRes, !IO),
    (
        FileStreamRes = ok(OutputStream),
        term_to_html.write_html_doc(OutputStream, BrowserTerm, MdbDir, _, !IO),
        io.close_output(OutputStream, !IO)
    ;
        FileStreamRes = error(_)
    ).

:- pred launch_web_browser(io.text_output_stream::in,
    io.text_output_stream::in, string::in, io::di, io::uo) is det.

launch_web_browser(OutputStream, ErrorStream, CommandStr, !IO) :-
    io.write_string(OutputStream, "Launching web browser...\n", !IO),
    io.flush_output(OutputStream, !IO),
    io.call_system.call_system_return_signal(CommandStr, Result, !IO),
    (
        Result = ok(ExitStatus),
        (
            ExitStatus = exited(ExitCode),
            ( if ExitCode = 0 then
                true
            else
                io.format(ErrorStream,
                    "mdb: The command `%s' terminated with " ++
                    "a non-zero exit code.\n",
                    [s(CommandStr)], !IO)
            )
        ;
            ExitStatus = signalled(_),
            io.write_string(ErrorStream, "mdb: The browser was killed.\n", !IO)
        )
    ;
        Result = error(Error),
        io.format(ErrorStream, "mdb: Error launching browser: %s.\n",
            [s(string.string(Error))], !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module mdb.save_term.
%---------------------------------------------------------------------------%
