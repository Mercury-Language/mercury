%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test error messages with problems that arise trying to taking the address
% of multi-moded predicates.

:- module multimode_addr_problems.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    % The compiler can't choose which mode of absolute to use.
    Abs = absolute,
    Abs(3, X),
    io.write_int(X, !IO),
    io.nl(!IO).

:- pred absolute(int, int).
:- mode absolute(in, out) is det.
:- mode absolute(out, in) is multi.

:- pragma promise_equivalent_clauses(absolute/2).

absolute(X::in, Y::out) :-
    Y = ( if X < 0 then -X else X).

absolute(X::out, Y::in) :-
    ( X = Y
    ; X = -Y
    ).

:- func my_foldl(func(L, A) = A, list(L), A) = A.
:- mode my_foldl(in(func(in, in) = out is det), in, in) = out is det.

my_foldl(F, L, A0) = A :-
    % None of the modes of f2p are usable.
    % XXX the error message without this explicit unification is confusing.
    P = f2p(F),
    list.foldl(P, L, A0, A).

:- pred f2p(func(L, A) = A, L, A, A).
:- mode f2p(in(func(in, di) = uo is det), in, di, uo) is det.
% :- mode f2p(in(func(in, in) = out is det), in, in, out) is det.
:- mode f2p(in(func(in, in) = out is semidet), in, in, out) is semidet.

f2p(F, L, A0, A) :-
    F(L, A0) = A.
