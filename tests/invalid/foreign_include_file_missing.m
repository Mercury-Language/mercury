%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .err_exp file is for systems where the directory separator is "/".
% The .err_exp2 file is for systems where the directory separator is "\".

:- module foreign_include_file_missing.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pragma foreign_code("C", include_file("missing_file")).
:- pragma foreign_code("Java", include_file("missing_file")).
:- pragma foreign_code("C#", include_file("missing_file")).

main(!IO).
