%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module undef_mod_qual.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- use_module undef_mod_qual.foo.

main(!IO) :-
    foo.bletchx(!IO).

%-------------------------------------------------------------------------
% foo is a submodule

:- module foo.

:- interface.
:- pred bletch(io::di, io::uo) is det.

:- implementation.
bletch(!IO) :-
    io.write_string("Hi There", !IO).

:- end_module foo.

:- end_module undef_mod_qual.
