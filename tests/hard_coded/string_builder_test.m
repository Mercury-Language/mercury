%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module string_builder_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module pprint.
:- import_module stream.
:- import_module stream.string_writer.
:- import_module string.
:- import_module string.builder.
:- import_module term_to_xml.

main(!IO) :-
    some [!State] (
        !:State = string.builder.init,
        Stream = string.builder.handle,
        put(Stream, "Hello", !State),
        put(Stream, ',', !State),
        put(Stream, " world!", !State),
        put(Stream, "\n", !State),
        write_xml_doc_general(Stream, [1, 2, 3],
            simple, no_stylesheet, no_dtd, _, !State),
        put(Stream, "\n", !State),
        pprint.write(Stream, 0, to_doc([4, 5, 6]), !State),
        put(Stream, "\n", !State),
        string_writer.format(Stream, "%.2f", [f(3.14)], !State),
        put(Stream, "\n", !State),
        String = string.builder.to_string(!.State),
        io.write_string(String, !IO)
    ).
