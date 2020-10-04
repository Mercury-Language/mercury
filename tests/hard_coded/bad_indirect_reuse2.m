%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test.
%---------------------------------------------------------------------------%

:- module bad_indirect_reuse2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

main(!IO) :-
    copy([1, 3, 3], U1),
    copy([1, 3, 7], U2),
    H1 = hide(U1),
    H2 = hide(U2),
    my_merge(U1, U2, M),    % bad indirect reuse
    io.write(H1, !IO),
    nl(!IO),
    io.write(H2, !IO),
    nl(!IO),
    io.write(M, !IO),
    io.nl(!IO),

    io.write_string("--------\n", !IO),

    copy([0, 0, 3, 5], U3),
    copy([1, 2, 2, 4], U4),
    my_merge(U3, U4, MB),   % good indirect reuse
    io.write(MB, !IO),
    io.nl(!IO).

:- type hide
    --->    hide(list(int)).

:- pred my_merge(list(int)::in, list(int)::in, list(int)::out) is det.
:- pragma no_inline(my_merge/3).

my_merge([], [], []).
my_merge([A | As], [], [A | As]).
my_merge([], [B | Bs], [B | Bs]).
my_merge([A | As], [B | Bs], [C | Cs]) :-
    ( if compare(>, A, B) then
        C = B,
        my_merge([A | As], Bs, Cs)
    else
        % If compare((=), A, B), take A first.
        C = A,
        my_merge(As, [B | Bs], Cs)
    ).
