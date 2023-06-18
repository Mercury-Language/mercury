:- module gh118.
:- interface.
:- include_module gh118_helper.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module gh118.gh118_helper_1.

main(!IO) :-
    bar(!IO).
