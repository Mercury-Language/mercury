%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A stress test of the pretty_printer.
%
%---------------------------------------------------------------------------%

:- module pretty_printer_stress_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.
:- import_module io.file.
:- import_module list.
:- import_module mercury_term_parser.
:- import_module pretty_printer.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module term_io.
:- import_module type_desc.
:- import_module univ.

main(!IO) :-
    InputFileName = "pretty_printer_stress_test.data",
    OutputFileName = "pretty_printer_stress_test.data.out",
    io.open_input(InputFileName, InputResult, !IO),
    io.open_output(OutputFileName, OutputResult, !IO),
    ( if
        InputResult = ok(InputStream),
        OutputResult = ok(OutputStream)
    then
        setup_pretty_printer(!IO),
        test_loop(InputStream, OutputStream, !IO),
        io.close_input(InputStream, !IO),
        io.close_output(OutputStream, !IO),
        % We remove OutputFileName because it is big (several Mb)
        % and it is of interest only when someone is actively working
        % on this test case. Anyone whose does so can just comment this out.
        io.file.remove_file(OutputFileName, _RemoveResult, !IO),
        io.write_string("done\n", !IO)
    else
        io.write_string("could not open a file\n", !IO)
    ).

:- pred test_loop(io.text_input_stream::in, io.text_output_stream::in,
    io::di, io::uo) is det.

test_loop(InputStream, OutputStream, !IO) :-
    read_term(InputStream, ReadResult, !IO),
    (
        ReadResult = term(_VarSet, Term : term),
        Params = pp_params(80, 1000000, linear(1000000)),
        get_default_formatter_map(Formatters, !IO),
        Doc = format(Term),
        put_doc(OutputStream, canonicalize, Formatters, Params, Doc, !IO),
        io.nl(OutputStream, !IO),
        test_loop(InputStream, OutputStream, !IO)
    ;
        ReadResult = eof,
        io.write_string(OutputStream, "eof\n", !IO)
    ;
        ReadResult = error(Msg, Line),
        io.format(OutputStream, "error %s on line %d\n",
            [s(Msg), i(Line)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred setup_pretty_printer(io::di, io::uo) is det.

setup_pretty_printer(!IO) :-
    set_default_formatter("builtin", "int",        0, fmt_int,    !IO),
    set_default_formatter("builtin", "float",      0, fmt_float,  !IO),
    set_default_formatter("builtin", "character",  0, fmt_char,   !IO),
    set_default_formatter("builtin", "string",     0, fmt_string, !IO),
    set_default_formatter("list",    "list",       1, fmt_list,   !IO),
    set_default_formatter("term_context", "term_context", 0, fmt_ctxt, !IO).

%---------------------%

:- func fmt_int(univ, list(type_desc)) = doc.

fmt_int(Univ, _) =
    ( if Univ = univ(Int) then
        str(string.int_to_string(Int))
    else
        str("?fmt_int?")
    ).

%---------------------%

:- func fmt_float(univ, list(type_desc)) = doc.

fmt_float(Univ, _) =
    ( if Univ = univ(Float) then
        str(string.float_to_string(Float))
    else
        str("?fmt_float?")
    ).

%---------------------%

:- func fmt_char(univ, list(type_desc)) = doc.

fmt_char(Univ, _) =
    ( if Univ = univ(Char) then
        str(term_io.quoted_char(Char))
    else
        str("?fmt_char?")
    ).

%---------------------%

:- func fmt_string(univ, list(type_desc)) = doc.

fmt_string(Univ, _) =
    ( if Univ = univ(String) then
        docs([str("\""), str(String), str("\"")])
    else
        str("?fmt_string?")
    ).

%---------------------%

:- func fmt_list(univ, list(type_desc)) = doc.

fmt_list(Univ, ArgDescs) = Doc :-
    ( if
        ArgDescs = [ArgDesc],
        has_type(Arg, ArgDesc),
        same_list_type(List, Arg),
        Value = univ_value(Univ),
        dynamic_cast(Value, List),
        UnivList = list.map(make_univ, List)
    then
        Doc = indent([str("["), format_list(UnivList, str(", ")), str("]")])
    else
        Doc = str("?fmt_list?")
    ).

:- pred same_list_type(list(T)::unused, T::unused) is det.

same_list_type(_, _).

:- func make_univ(T) = univ.

make_univ(X) = univ(X).

%---------------------%

:- func fmt_ctxt(univ, list(type_desc)) = doc.

fmt_ctxt(Univ, _) = Doc :-
    ( if Univ = univ(context(_FileName, LineNumber)) then
        string.format("line %d", [i(LineNumber)], Str),
        Doc = str(Str)
    else
        Doc = str("?fmt_ctxt?")
    ).

%---------------------------------------------------------------------------%
