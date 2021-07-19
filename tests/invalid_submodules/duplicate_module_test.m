%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test of modules declared as both nested and separate modules.
% The error occurs in the imported module duplicate_module.m,
% but it is detected when making the dependencies for this module.

:- module duplicate_module_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module duplicate_module.

mainI(!IO) :-
    duplicate_module.do_main(!IO).
