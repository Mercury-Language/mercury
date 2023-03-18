:- module gh118.gh118_helper.
:- interface.
:- import_module io.

:- pred bar(io::di, io::uo) is multi.

:- implementation.

bar(!IO) :-
    io.write_string("bar\n", !IO).
