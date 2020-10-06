%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_various.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module string.

main(!IO) :-
    io.write_string(remove_suffix_if_present(".gz", "myfile"), !IO),
    io.nl(!IO),
    io.write_string(remove_suffix_if_present(".gz", "myfile.gz"), !IO),
    io.nl(!IO),
    io.write_string(remove_suffix_if_present(".gz", "myfile.gz.gz"), !IO),
    io.nl(!IO).
