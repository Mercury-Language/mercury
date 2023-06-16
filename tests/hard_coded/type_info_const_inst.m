%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% There was a bug in the inst for static type_infos when --const-struct was
% enabled: the arity was off by one as it did not count the type_ctor_info
% argument.
%
% This made unification with a dynamic type_info (with the correct inst)
% fail when it shouldn't, at compile-time when --optimise-constant-pbpagation
% was enabled.

:- module type_info_const_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module require.
:- import_module string.
:- import_module type_desc.

:- type thing(T, U)
    --->    thing(T, U, string).

:- type a
    --->    a(int, string).

:- type b
    --->    b(int, int).

main(!IO) :-
    A = thing(a(1, "2"), a(3, "4"), "A"),
    describe(A, DescA),
    io.write_string(DescA, !IO),
    io.nl(!IO),

    B = thing(a(1, "2"), b(3, 4), "B"),
    describe(B, DescB),
    io.write_string(DescB, !IO),
    io.nl(!IO).

:- pred describe(thing(T, U)::in, string::out) is det.

describe(X, What) :-
    ( if dynamic_cast(X, _ : thing(a, a)) then
        What = "a, a"
    else if dynamic_cast(X, _ : thing(a, b)) then
        What = "a, b"
    else
        unexpected($module, $pred, string(type_of(X)))
    ).
