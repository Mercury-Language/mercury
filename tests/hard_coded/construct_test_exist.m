% Test case for get_functor on a functor with existentially typed arguments.
% 
% Author: zs

:- module construct_test_exist.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, maybe, pair, term, map, string, require.
:- import_module type_desc, construct.

:- typeclass tc1(V) where [
	func m1(V) = int
].

:- type t1
	--->	f11
	;	f12(int)
	;	some [T] f13(int, T, list(T))
	;	some [T, U] f14(T, list(U))
	;	some [T, U] f15(T, list(U), U) => tc1(T).

:- type t2
	--->	f21
	;	some [T] f22(
			f21name :: int,
			f22name :: T,
			f23name :: list(T),
			f24name :: T,
			f25name :: float
		).

:- type t3(T, U)
	--->	f31(T, U)
	;	some [V, W] f32(
			f31name :: int,
			f32name :: pair(T, V),
			f33name :: map(T, pair(U, pair(V, W))),
			f34name :: U
		).

%----------------------------------------------------------------------------%

main(!IO) :-
	test_all(f11, !IO),
	test_all(f21, !IO),
	test_all(f31(3, "three"), !IO),
	test_all(f31([3], 3.0), !IO).

:- pred test_all(T::in, io::di, io::uo) is det.

test_all(T, !IO) :-
	TypeInfo = type_desc__type_of(T),
	N = construct__num_functors(TypeInfo),
	io__write_int(N, !IO),
	io__write_string(" functors in this type", !IO),
	io__nl(!IO),
	test_all_functors(TypeInfo, N, !IO),
	io__nl(!IO).

:- pred test_all_functors(type_desc__type_desc::in, int::in, io::di, io::uo)
	is det.

test_all_functors(TypeInfo, N, !IO) :-
	( N =< 0 ->
		true
	;
		test_nth_functor(TypeInfo, N - 1, !IO),
		test_all_functors(TypeInfo, N - 1, !IO)
	).

:- pred test_nth_functor(type_desc__type_desc::in, int::in, io::di, io::uo)
	is det.

test_nth_functor(TypeInfo, N, !IO) :-
	io__write_int(N, !IO),
	(
		construct__get_functor_with_names(TypeInfo, N, Name, Arity,
			ArgTypes, Names)
	->
		io__write_string(" - ", !IO),
		io__write_string(Name, !IO),
		io__write_string("/", !IO),
		io__write_int(Arity, !IO),
		io__write_string(" [", !IO),
		io__write_list(ArgTypes, ", ", print_arg_type, !IO),
		io__write_string("] ", !IO),
		io__write_string(" [", !IO),
		io__write_list(Names, ", ", print_maybe_name, !IO),
		io__write_string("]\n", !IO)
	;
		io__write_string(" failed ", !IO),
		io__nl(!IO)
	).

:- pred print_arg_type(type_desc__pseudo_type_desc::in, io::di, io::uo)
	is det.

print_arg_type(PseudoTypeDesc, !IO) :-
	PseudoTypeRep = pseudo_type_desc_to_rep(PseudoTypeDesc),
	(
		PseudoTypeRep = bound(TypeCtorDesc, ArgPseudoTypeInfos),
		io__write_string(type_desc__type_ctor_name(TypeCtorDesc), !IO),
		(
			ArgPseudoTypeInfos = []
		;
			ArgPseudoTypeInfos = [_ | _],
			io__write_string("(", !IO),
			io__write_list(ArgPseudoTypeInfos, ", ",
				print_arg_type, !IO),
			io__write_string(")", !IO)
		)
	;
		PseudoTypeRep = univ_tvar(TypeVarNum),
		io__write_string("U", !IO),
		io__write_int(TypeVarNum, !IO)
	;
		PseudoTypeRep = exist_tvar(TypeVarNum),
		io__write_string("E", !IO),
		io__write_int(TypeVarNum, !IO)
	).

:- pred print_maybe_name(maybe(string)::in, io::di, io::uo) is det.

print_maybe_name(MaybeName, !IO) :-
	(
		MaybeName = yes(FieldName),
		io__write_string(FieldName, !IO)
	;
		MaybeName = no,
		io__write_string("_", !IO)
	).

%----------------------------------------------------------------------------%
