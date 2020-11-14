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

:- module intermod_nested_module.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module intermod_nested_module2.
:- import_module intermod_nested_module2.sub_module.

main(!IO) :-
    bar(3, X),
    io.write_line(X, !IO).
