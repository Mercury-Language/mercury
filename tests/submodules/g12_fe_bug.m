%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Compilation of this program with rotd-2011-09-04 and before resulted
% in there being an erroneous error message about multiple foreign_enum
% pragmas being emitted when generating the .c file for the generic_search
% sub-module.

:- module g12_fe_bug.
:- interface.

:- import_module io.

:- include_module g12_fe_bug_helper_1.
:- include_module g12_fe_bug_helper_2.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_string("Ok\n", !IO).
