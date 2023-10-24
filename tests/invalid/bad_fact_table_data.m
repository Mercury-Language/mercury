%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% We expect to match .err_exp file when targeting C,
% which supports fact tables.
% We expect to match .err_exp file when targeting Java or C#,
% which do not support fact tables.
%
%---------------------------------------------------------------------------%

:- module bad_fact_table_data.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if table_x(0, 1.1, "c") then
        io.write_string("yes", !IO)
    else
        io.write_string("no\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred table_x(int::in, float::in, string::in) is semidet.

:- pragma fact_table(pred(table_x/3), "fact_table_data_file_x").

%---------------------------------------------------------------------------%
