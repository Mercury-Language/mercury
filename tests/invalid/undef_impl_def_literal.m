:- module undef_impl_def_literal.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write($nosuchthing, !IO).

% vi:ft=mercury:ts=8:sts=4:sw=4:et
