%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module uniq_unify.
:- interface.
:- import_module io.

:- type f ---> f(int).
:- type fg ---> f(int) ; g.

:- pred p(f::di, f::uo) is det.

:- pred p2(fg::di, fg::uo) is det.

:- implementation.

:- pred q(f::di, f::uo) is det.
:- pragma no_inline(q/2).

q(!F).

:- pred q2(fg::di, fg::uo) is det.
:- pragma no_inline(q2/2).

q2(!FG).

:- pred r(f::di, f::uo) is det.
:- pragma no_inline(r/2).

r(!F).

:- pred r2(fg::di, fg::uo) is det.
:- pragma no_inline(r2/2).

r2(!FG).

% This is a regression test: a previous version of the compiler
% reported a spurious mode error because after `F0 = f(_)' it
% inferred `F0 -> unique(f(ground))' rather than `F0 -> unique(f(unique))'.

p(F0, F) :-
    F0 = f(_),
    q(F0, F).

% This is also a regression test: a previous version of the compiler
% reported a spurious unique mode error, because it thought that F0
% had to be made mostly_unique for the condition of the if-then-else.

p2(F0, F) :-
    ( if F0 = g then
        q2(F0, F)
    else
        r2(F0, F)
    ).
