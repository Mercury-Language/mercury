%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module array_sort.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module list.
:- import_module solutions.

%---------------------------------------------------------------------------%

main(!IO) :-
    unsorted_aggregate(generate, test, !IO),
    io.write_string("done.\n", !IO).

:- pred generate(list(int)::out) is multi.

generate(L) :-
    L0 = [0, 0, 1, 1, 2, 2, 3, 3],
    sub(L0, L1),
    list.perm(L1, L).

:- pred sub(list(T)::in, list(T)::out) is multi.

sub([], []).
sub([_ | T], L) :-
    sub(T, L).
sub([H | T], [H | L]) :-
    sub(T, L).

:- pred test(list(int)::in, io::di, io::uo) is det.

test(L, !IO) :-
    list.sort(L, LS),
    AS = to_list(array.sort(from_list(L))),
    ( if LS = AS then
        % io.write_string("ok: ", !IO),
        % io.write(L, !IO),
        % io.nl(!IO)
        true
    else
        io.write_string("failed: ", !IO),
        io.write(L, !IO),
        io.write_string(" -> ", !IO),
        io.write(AS, !IO),
        io.nl(!IO)
    ).
