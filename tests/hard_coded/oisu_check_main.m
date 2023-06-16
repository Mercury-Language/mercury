%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module oisu_check_main.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module oisu_check_db.

:- import_module bool.
:- import_module pair.

main(!IO) :-
    some [!DBA, !DBB] (
        create_db1(!:DBA),
        mutate_db1(yes, !DBA),
        mutate_db2(2, !DBA),
        destroy_db(!.DBA, BA, NA),

        create_db2(3, !:DBB),
        mutate_db1(yes, !DBB),
        mutate_db2(4, !DBB),
        destroy_db(!.DBB, BB, NB)
    ),
    io.write_line(BA - NA, !IO),
    io.write_line(BB - NB, !IO).
