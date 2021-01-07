%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for get_functor on a functor with existentially typed arguments.
%
% Author: zs

:- module construct_test_exist.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module construct.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module type_desc.

:- typeclass tc1(V) where [
    func m1(V) = int
].

:- type t1
    --->    f11
    ;       f12(int)
    ;       some [T] f13(int, T, list(T))
    ;       some [T, U] f14(T, list(U))
    ;       some [T, U] f15(T, list(U), U) => tc1(T).

:- type t2
    --->    f21
    ;       some [T] f22(
                f21name :: int,
                f22name :: T,
                f23name :: list(T),
                f24name :: T,
                f25name :: float
            ).

:- type t3(T, U)
    --->    f31(T, U)
    ;       some [V, W] f32(
                f31name :: int,
                f32name :: pair(T, V),
                f33name :: map(T, pair(U, pair(V, W))),
                f34name :: U
            ).

%---------------------------------------------------------------------------%

main(!IO) :-
    test_all(f11, !IO),
    test_all(f21, !IO),
    test_all(f31(3, "three"), !IO),
    test_all(f31([3], 3.0), !IO).

:- pred test_all(T::in, io::di, io::uo) is det.

test_all(T, !IO) :-
    TypeInfo = type_desc.type_of(T),
    ( N = construct.num_functors(TypeInfo) ->
        io.write_int(N, !IO),
        io.write_string(" functors in this type", !IO),
        io.nl(!IO),
        test_all_functors(TypeInfo, N, !IO),
        io.nl(!IO)
    ;
        io.write_string("no functors in this type\n", !IO)
    ).

:- pred test_all_functors(type_desc.type_desc::in, int::in, io::di, io::uo)
    is det.

test_all_functors(TypeInfo, N, !IO) :-
    ( N =< 0 ->
        true
    ;
        test_nth_functor(TypeInfo, N - 1, !IO),
        test_all_functors(TypeInfo, N - 1, !IO)
    ).

:- pred test_nth_functor(type_desc.type_desc::in, int::in, io::di, io::uo)
    is det.

test_nth_functor(TypeInfo, N, !IO) :-
    io.write_int(N, !IO),
    (
        construct.get_functor_with_names(TypeInfo, N, Name, Arity,
            ArgTypes, Names)
    ->
        io.write_string(" - ", !IO),
        io.write_string(Name, !IO),
        io.write_string("/", !IO),
        io.write_int(Arity, !IO),
        io.write_string(" [", !IO),
        io.write_list(ArgTypes, ", ", print_arg_type, !IO),
        io.write_string("] ", !IO),
        io.write_string(" [", !IO),
        io.write_list(Names, ", ", print_maybe_name, !IO),
        io.write_string("]\n", !IO)
    ;
        io.write_string(" failed ", !IO),
        io.nl(!IO)
    ).

:- pred print_arg_type(type_desc.pseudo_type_desc::in, io::di, io::uo)
    is det.

print_arg_type(PseudoTypeDesc, !IO) :-
    PseudoTypeRep = pseudo_type_desc_to_rep(PseudoTypeDesc),
    (
        PseudoTypeRep = bound(TypeCtorDesc, ArgPseudoTypeInfos),
        io.write_string(type_desc.type_ctor_name(TypeCtorDesc), !IO),
        (
            ArgPseudoTypeInfos = []
        ;
            ArgPseudoTypeInfos = [_ | _],
            io.write_string("(", !IO),
            io.write_list(ArgPseudoTypeInfos, ", ", print_arg_type, !IO),
            io.write_string(")", !IO)
        )
    ;
        PseudoTypeRep = univ_tvar(TypeVarNum),
        io.write_string("U", !IO),
        io.write_int(TypeVarNum, !IO)
    ;
        PseudoTypeRep = exist_tvar(TypeVarNum),
        io.write_string("E", !IO),
        io.write_int(TypeVarNum, !IO)
    ).

:- pred print_maybe_name(maybe(string)::in, io::di, io::uo) is det.

print_maybe_name(MaybeName, !IO) :-
    (
        MaybeName = yes(FieldName),
        io.write_string(FieldName, !IO)
    ;
        MaybeName = no,
        io.write_string("_", !IO)
    ).

%---------------------------------------------------------------------------%
