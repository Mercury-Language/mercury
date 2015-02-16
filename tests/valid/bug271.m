%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug271.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- type a_or_b
    --->    a
    ;       b.

main(!IO) :-
    get_a_or_b(AB),
    (
        AB = a,
        obj_to_num(obj, X),
        S = to_string(X)
    ;
        AB = b,
        obj_to_num(obj, X),
        S = to_string(X)
    ),
    format("%s\n", [s(S)], !IO).

:- pred get_a_or_b(a_or_b::out) is det.

:- typeclass literal(X) where [
    func literal_type(X) = literal_type
].

:- typeclass num(X) where [
    func to_string(X) = string
].

:- some [Y] (pred obj_to_num(object(X)::in, Y::out) is det
    => num(Y)) <= literal(X).

% We need a clause so that Y gets a type assignment.
obj_to_num(_, 15).

:- type object(L)
    --->    object(L).
:- type literal
    --->    literal.
:- type literal_type
    --->    literal_type.

:- func obj = object(literal).

:- instance literal(literal) where [
    (literal_type(literal) = literal_type)
].

:- instance num(int) where [
    (to_string(X) = string(X))
].
