%-----------------------------------------------------------------------------%

:- module try_bad_params.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

%-----------------------------------------------------------------------------%

main(!IO) :-
    (try [io(!IO), io(!IO)]
        true
    then
        true
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    (try [bloop]
        true
    then
        true
    ).

:- pred main_3(int::in, int::out) is det.

main_3(!Int) :-
    (try [io(!Int)]
        true
    then
        true
    ).

:- pred main_4(io::di) is det.

main_4(IO) :-
    (try [io(IO)]
        true
    then
        true
    ).

:- pred main_5(io::di, io::uo) is det.

main_5(IO0, IO) :-
    (try [io(IO0, IO)]
        true
    then
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
