%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is the core of the hard_coded/typeclasses/complicated_constraint test
% case. This code tests the compiler and the debugger's handling of predicates
% whose signature includes a type variable which occurs in a typeclass
% constraint but is not a direct argument of the typeclass constraint.
% As a consequence, the typeinfo describing the type variable is available
% neither as an argument nor directly inside a typeclassinfo, but only inside
% a typeinfo inside a typeclassinfo. Our RTTI design doesn't (yet) have
% a mechanism for expressing this.
%
% There is also another reason why we can't handle this test case: the blah
% predicate's type_info_varmap contains incorrect information: it implies
% that the typeinfo for T is directly inside TypeClassInfo_for_foo, when the
% only typeinfo directly inside TypeClassInfo_for_foo is the typeinfo for
% list(T). This misleads the term size profiling transformation as well as
% the parts of the compiler generating the debugger's RTTI.
%
% The relevant part of the HLDS dump:
%
% variable types map:
% TypeClassInfo_for_foo (number 12):
%   (private_builtin.typeclass_info(
%       (private_builtin.constraint((x.foo), (list.list(T))))))
% type_info varmap:
% T (number 1) -> typeclass_info(TypeClassInfo_for_foo, 1)  (number 12)
% typeclass_info varmap:
% x.foo((list.list(T))) -> TypeClassInfo_for_foo

:- module deeply_nested_typeinfo.

:- interface.

:- import_module io.
:- import_module list.

:- typeclass foo(A) where [
    pred b(A::in) is semidet
].

:- instance foo(int).
:- instance foo(list(T)) <= foo(T).

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.
:- import_module int.

:- instance foo(int) where [
    pred(b/1) is int_b
].
:- instance foo(list(T)) <= foo(T) where [
    pred(b/1) is list_b
].

main -->
    blah(101).

:- pred list_b(list(T)::in) is semidet <= foo(T).

list_b(List) :-
    list.map((pred(A::in, A::out) is semidet :- b(A)), List, _).

:- pred int_b(int::in) is semidet.

int_b(1).

:- pred blah(T, io, io) <= foo(list(T)).
:- mode blah(in, di, uo) is det.

blah(X, !IO) :-
    ( if b([X]) then
        io.write_string("true\n", !IO)
    else
        io.write_string("false\n", !IO)
    ).
