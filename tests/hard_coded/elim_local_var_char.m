% Regression test.
% When --eliminate-local-vars was enabled, we could end up passing an int to a
% function expecting a char.  This is a type error in Java.

:- module elim_local_var_char.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
    io.write_char('a', !IO),
    io.write_char('!', !IO),
    io.write_char('\n', !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
