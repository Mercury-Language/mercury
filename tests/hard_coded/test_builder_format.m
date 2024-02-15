:- module test_builder_format.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- import_module string.
:- import_module string.builder.

main(!IO) :-
    State0 = string.builder.init,
    xyzzy(42, 55.5, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Str, !IO).

:- pred xyzzy(int::in, float::in,
    string.builder.state::di, string.builder.state::uo) is det.

xyzzy(I, F, !State) :-
    string.builder.format("%d_abc_%f_%s\n", [i(I), f(F), s("def")], !State).
