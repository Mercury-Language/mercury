%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_fact_table_decls.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if table_1(0, no, A) then
        io.write_line(A, !IO)
    else
        io.write_string("no A\n", !IO)
    ),
    ( if table_2(0, 1, 2) then
        io.write_string("B yes\n", !IO)
    else
        io.write_string("B no\n", !IO)
    ),
    ( if table_3(0, 1, 2) then
        io.write_string("C yes\n", !IO)
    else
        io.write_string("C no\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Since all three of these predicates have either a type or a mode
% that is not allowed in fact tables, the fact table files named below
% should never actually be accessed. They therefore do not need to exist,
% and they don't.
%

:- pred table_1(int, bool, int16).
:- mode table_1(in, out, out) is semidet.
:- mode table_1(out, in, out) is semidet.
:- mode table_1(out, out, in) is nondet.
:- pragma fact_table(pred(table_1/3), "fact_table_data_file_1").

:- pred table_2(int, int, int).
:- mode table_2(in, in, in) is semidet.
:- mode table_2(in, in, in) is nondet.
:- pragma fact_table(pred(table_2/3), "fact_table_data_file_2").

:- pred table_3(int, int, int).
:- mode table_3(in, ia, in) is semidet.
:- mode table_3(in, in, any) is nondet.
:- pragma fact_table(pred(table_3/3), "fact_table_data_file_3").

%---------------------------------------------------------------------------%
