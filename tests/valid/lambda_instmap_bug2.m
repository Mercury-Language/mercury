%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test. When lambdas are turned into separate predicates, the
% non-local sets in the procedure may change, which in turn requires that
% instmaps be updated.  We didn't do that, and the compiler aborted with:
%
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%     Key Type: term.var(parse_tree.prog_data.prog_var_type)
%     Key Value: var(23)
%     Value Type: ll_backend.var_locn.var_state

:- module lambda_instmap_bug2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
    mkthing(Thing),
    docall(
        % Must be lambda.
        (pred(1::out, !.T::in, !:T::out) is det),
        R, Thing, _Thing),
    io.write(R, !IO),
    io.nl(!IO).

:- type thing ---> thing.

:- typeclass tc1(T) where [].
:- typeclass tc2(T) where [].

:- instance tc1(thing) where [].
:- instance tc2(thing) where [].

:- some [T] pred mkthing(T::out) => (tc1(T), tc2(T)).
:- pragma no_inline(mkthing/1).

mkthing(thing).

:- pred docall((pred(int, T, T)::in(pred(out, in, out) is det)),
    int::out, T::in, T::out) is det.
:- pragma no_inline(docall/4).

docall(P, R, !T) :-
    P(R, !T).
