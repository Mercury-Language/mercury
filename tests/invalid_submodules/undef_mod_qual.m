%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module undef_mod_qual.

:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- use_module undef_mod_qual__foo.

main -->
    foo__bletchx.

%-------------------------------------------------------------------------
% foo is a submodule

:- module foo.

:- interface.
:- pred bletch(io__state::di, io__state::uo) is det.

:- implementation.
bletch -->
    write_string("Hi There").

:- end_module foo.

:- end_module undef_mod_qual.
