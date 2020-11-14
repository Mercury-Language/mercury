%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_nested_uniq.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module intermod_nested_uniq2.

main(!IO) :-
    init(1, 1, Matrix),
    lookup(1, 1, Matrix, _).
