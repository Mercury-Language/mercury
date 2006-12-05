%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
:- module stream_util_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module pprint.
:- import_module stream.
:- import_module stream_util.
:- import_module stream_util.string_builder.
:- import_module string.
:- import_module term_to_xml.

main(!IO) :-
    some [!State] (
        string_builder.init(Stream, !:State),
        put(Stream, "Hello", !State),
        put(Stream, ',', !State),
        put(Stream, " world!", !State),
        put(Stream, "\n", !State),
        write_xml_doc_general(Stream, [1, 2, 3],
            simple, no_stylesheet, no_dtd, _, !State),
        put(Stream, "\n", !State),
        pprint.write(Stream, 0, to_doc([4, 5, 6]),
            !State),
        put(Stream, "\n", !State),
        stream.format(Stream, "%.2f", [f(3.14)],
            !State),
        put(Stream, "\n", !State),
        String = string_builder_state_to_string(!.State),
        io.write_string(String, !IO)
    ).
