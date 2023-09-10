%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that inter-module optimization works with nested modules.
%
% The compiler of 12/11/1999 did not read the `.int0' files
% for parent modules of a module for which a `.opt' file was read,
% resulting in undefined symbol errors.
%
% This test was originally called intermod_nested_module.
%

:- module read_submod_opt.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module read_submod_opt_helper_1.
:- import_module
    read_submod_opt_helper_1.read_submod_opt_helper_2.

main(!IO) :-
    bar(3, X),
    io.write_line(X, !IO).
