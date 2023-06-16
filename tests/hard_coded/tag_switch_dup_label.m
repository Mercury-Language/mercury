%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The compiler could previously generate high-level C code with duplicate
% labels.

:- module tag_switch_dup_label.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type fruit
    --->    apple
    ;       banana(int)
    ;       cherry(string).

main(!IO) :-
    ( if p(apple, "a", N) then
        io.write_int(N, !IO),
        io.nl(!IO)
    else
        true
    ).

:- pred p(fruit::in, string::in, int::out) is semidet.
:- pragma no_inline(p/3).

p(F, S, N) :-
    (
        % A tag switch with multiple primary tags leading to the same case.
        ( F = apple
        ; F = banana(_)
        ),
        % A string switch which is compiled to a binary search.
        % A label is used to jump out of the search.
        ( S = "a", N = 1
        ; S = "b", N = 2
        ; S = "c", N = 3
        ; S = "d", N = 4
        ; S = "e", N = 5
        )
    ;
        F = cherry(_),
        N = 6
    ).
