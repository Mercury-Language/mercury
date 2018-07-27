%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module static_term_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- type data
    --->    none
    ;       data(data_info).

:- type data_info
    --->    data_info(side, int).

:- type side
    --->    top
    ;       bottom.

main(!IO) :-
    ( if get_data("top", Data) then
        io.write(Data, !IO),
        io.nl(!IO)
    else
        io.write_string("failed\n", !IO)
    ).

:- pred get_data(string::in, data::out) is semidet.
:- pragma no_inline(get_data/2).

get_data(Side, Data) :-
    Side = "top",
    Data = data(data_info(top, 1234)).
