%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test. The compiler was not writing some dummy types to the
% implementation sections of interface files, specifically types with one
% constructor with one argument which is itself a dummy type.

:- module dummy_no_tag.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module dummy_no_tag_helper_1.

main(!IO) :-
    io.write_line(fun, !IO),
    io.write_line(fun_eqv, !IO),
    io.write_line(fun_eqv2, !IO).
