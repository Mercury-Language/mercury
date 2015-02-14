%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% When --eliminate-local-vars was enabled, we could end up passing an int to a
% function expecting a char.  This is a type error in Java.
% The same problem also occurred with enumerations.

:- module elim_local_var_char.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type rhymes
    --->    orange
    ;       lozenge.

main(!IO) :-
    io.write_char('a', !IO),
    io.write_char('!', !IO),
    io.write_char('\n', !IO),
    write_enum(orange, !IO),
    io.write_char('\n', !IO).

:- pred write_enum(rhymes::in, io::di, io::uo) is det.
:- pragma no_inline(write_enum/3).

write_enum(X, !IO) :-
    io.write(X, !IO).
