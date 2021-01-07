%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A regression test for a bug with the printing
% of type_ctor_descs via io.write.

:- module type_ctor_desc.

:- interface.
:- use_module io.

:- pred main(io.state::di, io.state::uo) is det.

:- implementation.
:- import_module int.
:- import_module integer.
:- import_module type_desc.

main(!IO) :-
    Type = type_of(test),
    type_ctor_and_args(Type, TypeCtor, TypeArgs),
    io.write(TypeCtor, !IO),
    io.print(" ", !IO),
    io.write(TypeArgs, !IO),
    io.nl(!IO).

:- func test(int) = int.
:- mode test(in) = out is det.
test(X) = Y :-
    Y = X + 1.
