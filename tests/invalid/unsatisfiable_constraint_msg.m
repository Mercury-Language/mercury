%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unsatisfiable_constraint_msg.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.

main(!IO) :-
    io.write_string("typeclass_test", !IO),
    ( if test(ta("a"), tb("b")) then
        io.write_string("true", !IO)
    else
        io.write_string("fail", !IO)
    ).

:- pred test(A::in, B::in) is semidet <= (tca(A), tcb(B)).

test(A, B) :-
    pa(A),
    pb(B).

:- typeclass tca(A) where [
    pred pa(A::in) is semidet
].

:- typeclass tcb(B) where [
    pred pb(B::in) is semidet
].

:- typeclass tcc(C) where [
    pred pc(C::in) is semidet
].

:- type ta(A)
    --->    ta(A).
:- type tb(B)
    --->    tb(B).

:- instance tca(ta(A)) <= (tca(A), tcc(A)) where [
    ( pa(ta(A)) :-
        pa(A),
        pc(A)
    )
].

/*
% missing typeclass instance
:- instance tcb(tb(B)) <= tcb(B)
where
[
    (pb(tb(A)) :-
        pb(A)
    )
].
*/

:- instance tca(string) where [
    (pa(String) :-
        length(String) > 0
    )
].

:- instance tcb(string) where [
    (pb(String) :-
        length(String) > 0
    )
].

/*
% missing typeclass instance
:- instance c(string)
where
[
    (c(String) :-
        length(String) > 0
    )
].
*/
:- end_module unsatisfiable_constraint_msg.
