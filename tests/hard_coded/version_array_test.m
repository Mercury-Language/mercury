%-----------------------------------------------------------------------------%
% version_array_test.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Oct  8 15:00:25 EST 2004
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module version_array_test.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, string, version_array.

%-----------------------------------------------------------------------------%

main(!IO) :-
    A0 = version_array.empty,
    A1 = version_array(0`..`9),
    A2 = int.fold_up(func(I, A) = ( A ^ elem(I) := 9 - I ), 0, 9, A1),
    io.write_list(to_list(A0), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A0))], !IO),
    io.write_list(to_list(A1), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A1))], !IO),
    io.write_list(to_list(A2), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A2))], !IO),
    A3 = unsafe_rewind(A1),
    io.write_list(to_list(A3), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A3))], !IO),
    A4 = version_array.init(7, 7),
    io.write_list(to_list(A4), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A4))], !IO),
    S4 = foldl(func(X, A) = X + A, A4, 0),
    io.format(" (sum %d)\n", [i(S4)], !IO),
    A5 = resize(A4, 4, 4),
    io.write_list(to_list(A5), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A5))], !IO),
    A6 = resize(A5, 9, 9),
    io.write_list(to_list(A6), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A6))], !IO),
    S6 = foldl(func(X, A) = X + A, A6, 0),
    io.format(" (sum %d)\n", [i(S6)], !IO),
    A7 = copy(A5),
    io.write_list(to_list(A7), ", ", io.write_int, !IO),
    io.format(" (size %d)\n", [i(size(A7))], !IO),
    true.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
