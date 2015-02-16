%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_exist_method.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module require.

:- typeclass toto(T) where
  [
  ].

:- instance toto(float) where [].
:- instance toto(character) where [].

:- some [V] (pred gen_toto_float(V) => toto(V)).
:- mode gen_toto_float(out) is det.

gen_toto_float(42.0).

:- some [V] (pred gen_toto_char(V) => toto(V)).
:- mode gen_toto_char(out) is det.

gen_toto_char('?').

:- typeclass toto2(T) where [
    some[V] (pred gen_toto(T::in, V::out) is det => toto(V))
].

:- instance toto2(int) where [
    pred(gen_toto/2) is int_gen_toto
].

:- some [V] (pred int_gen_toto(int, V) => toto(V)).
:- mode int_gen_toto(in, out) is det.

int_gen_toto(X, Y) :-
    ( compare(=, X, X) ->
        gen_toto_float(Y)
    ;
        error("oops")
    ).

main -->
    { gen_toto(42, Y) },
    write(Y), nl.

:- end_module typeclass_exist_method.

