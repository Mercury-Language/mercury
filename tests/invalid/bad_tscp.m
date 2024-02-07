%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Test the operation of type_spec_constrained_preds pragmas.
% We keep the module name short to make the type_spec pragmas
% that the compiler outputs as informational messages fit on one line.
%---------------------------------------------------------------------------%

:- module bad_tscp.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module term.
:- import_module stream.
:- import_module string.
:- import_module string.builder.

%---------------------------------------------------------------------------%

:- typeclass tc1(E, F, G, H) <= (tc2(E, int, H), tc4(float, G)) where [].
:- typeclass tc2(K, L, M) <= (tc3(K, L), tc3(L, M)) where [].
:- typeclass tc3(P, Q) where [].
:- typeclass tc4(S, T) where [].

:- instance tc1(int, int, int, int) where [].
:- instance tc2(int, int, int) where [].
:- instance tc3(int, int) where [].
:- instance tc4(float, int) where [].

%---------------------------------------------------------------------------%

main(!IO) :-
    p1(41, 42, 43, 44, N),
    io.write_int(N, !IO),
    io.nl(!IO),

    io.input_stream(InputStream, !IO),
    p2({InputStream, 'a', make_io_error("xyzzy")}, !IO).

%---------------------------------------------------------------------------%

:- pragma type_spec_constrained_preds(
    [tc1(X1, X2, X3, X4)],
    apply_to_superclasses,
    [subst([X2 => char, X3 => bool]),
        subst([X3 => int, X4 => var(Y)])]).

:- pred p1(A::in, B::in, C::in, D::in, D::out) is det <= tc1(A, B, C, D).

p1(_A, _B, _C, !D).

%---------------------------------------------------------------------------%

:- pragma type_spec_constrained_preds(
    [stream.line_oriented(Stream, State),
        stream.unboxed_reader(Stream, char, State, Error),
        stream.putback(Stream, char, State, Error)],
    apply_to_superclasses,
    [subst([Stream => io.text_input_stream, Unit => list(T),
        State => io.state, Error => io.error]),
    subst([Stream => set(U), Unit => map(K, V),
        State => io.state, Error => io.error, Xyzzy => float]),
    subst([set(U) => map(K, V)])]).

:- pred p2({Stream, Unit, Error}::in, State::di, State::uo) is det
    <= (stream.line_oriented(Stream, State),
    stream.unboxed_reader(Stream, char, State, Error),
    stream.putback(Stream, Unit, State, Error)).

p2({_Stream, _Unit, _Errors}, !State).
