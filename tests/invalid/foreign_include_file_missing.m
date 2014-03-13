:- module foreign_include_file_missing.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pragma foreign_code("C", include_file("missing_file")).
:- pragma foreign_code("Java", include_file("missing_file")).
:- pragma foreign_code("C#", include_file("missing_file")).
:- pragma foreign_code("Erlang", include_file("missing_file")).

main(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
