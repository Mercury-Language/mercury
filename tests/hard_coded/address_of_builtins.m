%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test taking the address of builtins such as '<'/2.
%

:- module address_of_builtins.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module require.

main(!IO) :-
    X = 3,
    require(X > 0, "oops, X is not positive!"),
    require(1 > 0, "oops, 1 > 0 failed!"),
    require(1 < X, "oops, 1 < X failed!"),
    io.write_string("\n", !IO).
