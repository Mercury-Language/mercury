%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: rtti_implementation.m.
% Main author: trd.
% Stability: low.

% This file is intended to provide portable RTTI functionality by implementing
% most of Mercury's RTTI functionality in Mercury.
%
% This is simpler writing large amounts of low-level C code, and is much
% easier to maintain and port to new platforms.
%
% This module is not complete, currently only enough functionality is
% present to implement type_info comparisons and unifications (which is enough
% to get univ working).
%
% The plan is to have RTTI functions in std_util.m call into this module
% as they are implemented in Mercury.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module rtti_implementation.

:- interface.

:- import_module list.

:- use_module std_util.

	% Our type_info and type_ctor_info implementations are both
	% abstract types.
:- type type_info.
:- type type_ctor_info.

:- func get_type_info(T::unused) = (type_info::out) is det.

:- pred generic_unify(T::in, T::in) is semidet.

:- pred generic_compare(comparison_result::out, T::in, T::in) is det.

:- pred compare_type_infos(comparison_result::out,
		type_info::in, type_info::in) is det.

:- pred type_ctor_and_args(type_info::in,
		type_ctor_info::out,
		list(type_info)::out) is det.

:- pred type_ctor_name_and_arity(type_ctor_info::in,
		string::out, string::out, int::out) is det.

:- pred deconstruct(T::in, string::out, int::out,
		list(std_util__univ)::out) is det.

	% This is useful in a few places, so we'd like to share the code, but
	% it's better to put it into an implementation module such as this one.
:- func unsafe_cast(T1::in) = (T2::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, string, int.

	% std_util has a lot of types and functions with the same names,
	% so we prefer to keep the namespace separate.
:- use_module std_util.

	% It is convenient to represent the type_ctor_rep as a Mercury 
	% enumeration, so 
	%
	% The type_ctor_rep needs to be kept up to date with the real
	% definition in runtime/mercury_type_info.h.
:- type type_ctor_rep 
	---> 	enum 
	; 	enum_usereq
	;	du
	;	du_usereq
	;	notag
	;	notag_usereq
	;	equiv
	;	(func)
	;	int
	;	char
	;	float
	;	string
	;	(pred)
	;	univ
	;	void
	;	c_pointer
	;	typeinfo
	;	typeclassinfo
	;	array
	;	succip
	;	hp
	;	curfr
	;	maxfr
	;	redofr
	;	redoip
	;	trail_ptr
	;	ticket
	;	notag_ground
	;	notag_ground_usereq
	;	equiv_ground
	;	tuple
	;	reserved_addr
	;	reserved_addr_usereq
	;	type_ctor_info
	;	base_typeclass_info
	;	unknown.

	% We keep all the other types abstract.

:- type type_ctor_info ---> type_ctor_info(c_pointer).
:- type type_info ---> type_info(c_pointer).
:- type compare_pred ---> compare_pred(c_pointer).
:- type type_functors ---> type_functors(c_pointer).
:- type type_layout ---> type_layout(c_pointer).
:- type pred_type ---> pred_type(c_pointer).
:- type pseudo_type_info ---> pred_type(c_pointer).

:- pragma foreign_proc("C#",
	get_type_info(_T::unused) = (TypeInfo::out),
	[will_not_call_mercury, promise_pure, thread_safe],
" 
	TypeInfo = TypeInfo_for_T;
").

:- pragma foreign_proc("C",
	get_type_info(_T::unused) = (TypeInfo::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeInfo = TypeInfo_for_T;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generic_compare(Res, X, Y) :-
	TypeInfo = get_type_info(X),
	TypeCtorInfo = get_type_ctor_info(TypeInfo),
	TypeCtorRep = TypeCtorInfo ^ type_ctor_rep,
	( 
		TypeCtorRep = tuple 
	->
		compare_tuple(TypeInfo, Res, X, Y)
	; 	
		( TypeCtorRep = (pred) ; TypeCtorRep = (func) )
	->
		error("rtti_implementation.m: unimplemented: higher order comparisons")
	;	
		Arity = TypeCtorInfo ^ type_ctor_arity,
		ComparePred = TypeCtorInfo ^ type_ctor_compare_pred,
		( Arity = 0 ->
			result_call_4(ComparePred, Res, X, Y)
		; Arity = 1 ->
			result_call_5(ComparePred, Res,
				TypeInfo ^ type_info_index(1), X, Y)
		; Arity = 2 ->
			result_call_6(ComparePred, Res,  
				TypeInfo ^ type_info_index(1),
				TypeInfo ^ type_info_index(2), 
				X, Y)
		; Arity = 3 ->
			result_call_7(ComparePred, Res,
				TypeInfo ^ type_info_index(1),
				TypeInfo ^ type_info_index(2), 
				TypeInfo ^ type_info_index(3),
				X, Y)
		; Arity = 4 ->
			result_call_8(ComparePred, Res,
				TypeInfo ^ type_info_index(1),
				TypeInfo ^ type_info_index(2), 
				TypeInfo ^ type_info_index(3),
				TypeInfo ^ type_info_index(4),
				X, Y)
		; Arity = 5 ->
			result_call_9(ComparePred, Res,
				TypeInfo ^ type_info_index(1),
				TypeInfo ^ type_info_index(2), 
				TypeInfo ^ type_info_index(3),
				TypeInfo ^ type_info_index(4),
				TypeInfo ^ type_info_index(5),
				X, Y)
		;
			error("compare/3: type arity > 5 not supported")
		)
	).


generic_unify(X, Y) :-
	TypeInfo = get_type_info(X),
	TypeCtorInfo = get_type_ctor_info(TypeInfo),
	TypeCtorRep = TypeCtorInfo ^ type_ctor_rep,
	( 
		TypeCtorRep = tuple 
	->
		unify_tuple(TypeInfo, X, Y)
	; 	
		( TypeCtorRep = (pred) ; TypeCtorRep = (func) )
	->
		error("rtti_implementation.m: unimplemented: higher order unifications")
	;	
		Arity = TypeCtorInfo ^ type_ctor_arity,
		UnifyPred = TypeCtorInfo ^ type_ctor_unify_pred,
		( Arity = 0 ->
			semidet_call_3(UnifyPred, X, Y)
		; Arity = 1 ->
			semidet_call_4(UnifyPred,
				TypeInfo ^ type_info_index(1), X, Y)
		; Arity = 2 ->
			semidet_call_5(UnifyPred, 
				TypeInfo ^ type_info_index(1),
				TypeInfo ^ type_info_index(2), 
				X, Y)
		; Arity = 3 ->
			semidet_call_6(UnifyPred, 
				TypeInfo ^ type_info_index(1),
				TypeInfo ^ type_info_index(2), 
				TypeInfo ^ type_info_index(3),
				X, Y)
		; Arity = 4 ->
			semidet_call_7(UnifyPred, 
				TypeInfo ^ type_info_index(1),
				TypeInfo ^ type_info_index(2), 
				TypeInfo ^ type_info_index(3),
				TypeInfo ^ type_info_index(4),
				X, Y)
		; Arity = 5 ->
			semidet_call_8(UnifyPred, 
				TypeInfo ^ type_info_index(1),
				TypeInfo ^ type_info_index(2), 
				TypeInfo ^ type_info_index(3),
				TypeInfo ^ type_info_index(4),
				TypeInfo ^ type_info_index(5),
				X, Y)
		;
			error("unify/2: type arity > 5 not supported")
		)
	).

	% check for tuple and higher order cases

:- pred unify_tuple(type_info::in, T::in, T::in) is semidet.

unify_tuple(_, _, _) :- 
	semidet_unimplemented("tuple unifications").

:- pred compare_tuple(type_info::in, comparison_result::out, T::in, T::in)
	is det.

compare_tuple(_, (=), _, _) :- 
	det_unimplemented("tuple comparisons").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Implement generic calls -- we could use call/N but then we would
	% have to create a real closure.
	%
	% We first give "unimplemented" definitions in Mercury, which will be
	% used by default.

:- pred semidet_call_3(P::in, T::in, U::in) is semidet.
semidet_call_3(_::in, _::in, _::in) :-
	semidet_unimplemented("semidet_call_3").

:- pred semidet_call_4(P::in, A::in, T::in, U::in) is semidet.
semidet_call_4(_::in, _::in, _::in, _::in) :-
	semidet_unimplemented("semidet_call_4").

:- pred semidet_call_5(P::in, A::in, B::in, T::in, U::in) is semidet.
semidet_call_5(_::in, _::in, _::in, _::in, _::in) :-
	semidet_unimplemented("semidet_call_5").

:- pred semidet_call_6(P::in, A::in, B::in, C::in, T::in, U::in) is semidet.
semidet_call_6(_::in, _::in, _::in, _::in, _::in, _::in) :-
	semidet_unimplemented("semidet_call_6").

:- pred semidet_call_7(P::in, A::in, B::in, C::in, D::in, T::in, U::in)
	is semidet.
semidet_call_7(_::in, _::in, _::in, _::in, _::in, _::in, _::in) :-
	semidet_unimplemented("semidet_call_7").

:- pred semidet_call_8(P::in, A::in, B::in, C::in, D::in, E::in, T::in, U::in)
	is semidet.
semidet_call_8(_::in, _::in, _::in, _::in, _::in, _::in, _::in, _::in) :-
	semidet_unimplemented("semidet_call_8").


:- pred result_call_4(P::in, comparison_result::out,
		T::in, U::in) is det.
result_call_4(_::in, (=)::out, _::in, _::in) :-
	det_unimplemented("result_call_4").

:- pred result_call_5(P::in, comparison_result::out,
		A::in, T::in, U::in) is det.
result_call_5(_::in, (=)::out, _::in, _::in, _::in) :-
	det_unimplemented("comparison_result").

:- pred result_call_6(P::in, comparison_result::out,
		A::in, B::in, T::in, U::in) is det.
result_call_6(_::in, (=)::out, _::in, _::in, _::in, _::in) :-
	det_unimplemented("comparison_result").

:- pred result_call_7(P::in, comparison_result::out,
		A::in, B::in, C::in, T::in, U::in) is det.
result_call_7(_::in, (=)::out, _::in, _::in, _::in, _::in, _::in) :-
	det_unimplemented("comparison_result").

:- pred result_call_8(P::in, comparison_result::out,
		A::in, B::in, C::in, D::in, T::in, U::in) is det.
result_call_8(_::in, (=)::out, _::in, _::in, _::in, _::in, _::in, _::in) :-
	det_unimplemented("comparison_result").

:- pred result_call_9(P::in, comparison_result::out,
		A::in, B::in, C::in, D::in, E::in, T::in, U::in) is det.
result_call_9(_::in, (=)::out, _::in, _::in, _::in, _::in, _::in,
		_::in, _::in) :-
	det_unimplemented("result_call_9").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


	% We override the above definitions in the .NET backend.

:- pragma foreign_proc("MC++",
	semidet_call_3(Pred::in, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_3(Pred, X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_4(Pred::in, A::in, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_4(Pred, A, X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_5(Pred::in, A::in, B::in, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_5(Pred, A, B, X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_6(Pred::in, A::in, B::in, C::in, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_6(Pred, A, B, C,
			X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_7(Pred::in, A::in, B::in, C::in, D::in, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_7(Pred, A, B, C, D,
			X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_8(Pred::in, A::in, B::in, C::in, D::in, E::in,
		X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_8(Pred, A, B, C, D,
			E, X, Y);
").



:- pragma foreign_proc("C#",
	result_call_4(Pred::in, Res::out, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	mercury.runtime.GenericCall.result_call_4(Pred, ref Res, X, Y);
").

:- pragma foreign_proc("C#",
	result_call_5(Pred::in, Res::out, A::in, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	mercury.runtime.GenericCall.result_call_5(Pred, A, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
	result_call_6(Pred::in, Res::out, A::in, B::in, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	mercury.runtime.GenericCall.result_call_6(Pred, A, B, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
	result_call_7(Pred::in, Res::out, A::in, B::in, C::in, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	mercury.runtime.GenericCall.result_call_7(Pred, A, B, C, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
	result_call_8(Pred::in, Res::out, A::in, B::in, C::in, D::in, X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	mercury.runtime.GenericCall.result_call_8(Pred, A, B, C, D,
		ref Res, X, Y);
").
:- pragma foreign_proc("C#",
	result_call_9(Pred::in, Res::out, A::in, B::in, C::in, D::in, E::in,
		X::in, Y::in), 
		[will_not_call_mercury, promise_pure, thread_safe],
"
	mercury.runtime.GenericCall.result_call_9(Pred, 
		A, B, C, D, E, ref Res, X, Y);
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

compare_type_infos(Res, TypeInfo1, TypeInfo2) :-
	( same_pointer_value(TypeInfo1, TypeInfo2) ->
		Res = (=)
	;
		NewTypeInfo1 = collapse_equivalences(TypeInfo1),
		NewTypeInfo2 = collapse_equivalences(TypeInfo2),
		( same_pointer_value(NewTypeInfo1, NewTypeInfo2) ->
			Res = (=)
		;
			compare_collapsed_type_infos(Res, TypeInfo1, TypeInfo2)
		)
	).

:- pred compare_collapsed_type_infos(comparison_result::out,
		type_info::in, type_info::in) is det.
compare_collapsed_type_infos(Res, TypeInfo1, TypeInfo2) :-
	TypeCtorInfo1 = get_type_ctor_info(TypeInfo1),
	TypeCtorInfo2 = get_type_ctor_info(TypeInfo2),

		% The comparison here is arbitrary.
		% In the past we just compared pointers of the type_c
	compare(NameRes, TypeCtorInfo1 ^ type_ctor_name,
		TypeCtorInfo2 ^ type_ctor_name),
	( NameRes = (=) ->
		compare(Res, 
			TypeCtorInfo1 ^ type_ctor_module_name,
			TypeCtorInfo2 ^ type_ctor_module_name),
		( 
			Res = (=),
			type_ctor_is_variable_arity(TypeCtorInfo1)
		->
			% XXX code to handle tuples and higher order
			error("rtti_implementation.m: unimplemented: tuples and higher order type comparisons")
		;
			true
		)
	;
		Res = NameRes
	).

:- pred type_ctor_is_variable_arity(type_ctor_info::in) is semidet.
type_ctor_is_variable_arity(TypeCtorInfo) :-
	( TypeCtorInfo ^ type_ctor_rep = (pred)
	; TypeCtorInfo ^ type_ctor_rep = (func)
	; TypeCtorInfo ^ type_ctor_rep = tuple
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% In the .NET backend, we don't generally have to collapse equivalences
	% because they are already collapsed (il grades require
	% intermodule optimization, which will collapse them for us).
	% 
	% XXX For other backends this code may have to be completed.

:- func collapse_equivalences(type_info) = type_info.
collapse_equivalences(TypeInfo) = NewTypeInfo :-
	TypeCtorInfo = get_type_ctor_info(TypeInfo),
	( 
		( 
		  TypeCtorInfo ^ type_ctor_rep = equiv_ground 
		;
		  TypeCtorInfo ^ type_ctor_rep = equiv 
		)
	->
		error("rtti_implementation.m: unimplemented: collapsing equivalence types")
	;
		NewTypeInfo = TypeInfo
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

type_ctor_name_and_arity(TypeCtorInfo, ModuleName, Name, Arity) :-
	ModuleName = type_ctor_module_name(TypeCtorInfo),
	Name = type_ctor_name(TypeCtorInfo),
	Arity = type_ctor_arity(TypeCtorInfo).

type_ctor_and_args(TypeInfo0, TypeCtorInfo, TypeArgs) :-
	TypeInfo = collapse_equivalences(TypeInfo0),
	TypeCtorInfo = get_type_ctor_info(TypeInfo),
	( 
		type_ctor_is_variable_arity(TypeCtorInfo)
	->
		error("rtti_implementation.m: unimplemented: tuples and higher order type comparisons")
	;
		Arity = type_ctor_arity(TypeCtorInfo),
		TypeArgs = iterate(1, Arity,
			(func(X) = Y :-
				Y = TypeInfo ^ type_info_index(X)
			)
		)
	).

:- func iterate(int, int, func(int, T)) = list(T).
iterate(Start, Max, Func) = Results :-
	( Start =< Max ->
		Res = Func(Start),
		Results = [Res | iterate(Start + 1, Max, Func)]
	;
		Results = []
	).

:- pred iterate_foldl(int, int, pred(int, T, T), T, T).
:- mode iterate_foldl(in, in, pred(in, in, out) is det, in, out) is det.
iterate_foldl(Start, Max, Pred) -->
	( { Start =< Max } ->
		Pred(Start),
		iterate_foldl(Start + 1, Max, Pred)
	;
		[]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


	% Code to perform deconstructions (XXX not yet complete).
	%
	% There are many cases to implement here, only the ones that were
	% immediately useful (e.g. called by io__write) have been implemented
	% so far.

deconstruct(Term, Functor, Arity, Arguments) :-
	TypeInfo = get_type_info(Term),
	TypeCtorInfo = get_type_ctor_info(TypeInfo),
	TypeCtorRep = type_ctor_rep(TypeCtorInfo),
	( 
		TypeCtorRep = enum_usereq,
		Functor = "some_enum_usereq", 
		Arity = 0,
		Arguments = []
	; 	
		TypeCtorRep = enum,
		Functor = "some_enum", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = du_usereq,
		Functor = "some_du_usereq", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = du,

		LayoutInfo = type_layout(TypeCtorInfo),
		PTag = get_primary_tag(Term),
		PTagEntry = LayoutInfo ^ ptag_index(PTag),
		SecTagLocn = PTagEntry ^ sectag_locn,
		(
			SecTagLocn = none,
			FunctorDesc = PTagEntry ^ du_sectag_alternatives(0),
			Functor = FunctorDesc ^ functor_name,
			Arity = FunctorDesc ^ functor_arity,
			Arguments = iterate(0, Arity - 1, 
				(func(X) = std_util__univ(
					get_arg(Term, X, SecTagLocn,
						FunctorDesc, TypeInfo))
				))
		;
			SecTagLocn = local,
			Functor = "some_du_local_sectag",
			Arity = 0,
			Arguments = []
		;
			SecTagLocn = remote,
			SecTag = get_remote_secondary_tag(Term),
			FunctorDesc = PTagEntry ^
				du_sectag_alternatives(SecTag),
			Functor = FunctorDesc ^ functor_name,
			Arity = FunctorDesc ^ functor_arity,
			Arguments = iterate(0, Arity - 1, 
				(func(X) = std_util__univ(
					get_arg(Term, X, SecTagLocn,
						FunctorDesc, TypeInfo))
				))
		;
			SecTagLocn = variable,
			Functor = "some_du_variable_sectag",
			Arity = 0,
			Arguments = []
		)
	;
		TypeCtorRep = notag_usereq,
		Functor = "some_notag_usereq", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = notag,
		Functor = "some_notag", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = notag_ground_usereq,
		Functor = "some_notag_ground_usereq", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = notag_ground,
		Functor = "some_notag_ground", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = equiv_ground,
		Functor = "some_equiv_ground", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = (func),
		Functor = "some_func", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = equiv,
		Functor = "some_equiv", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = int,
		Functor = "some_int", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = char,
		Functor = "some_char", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = float,
		Functor = "some_float", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = string,
		Functor = "some_string", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = (pred),
		Functor = "some_pred", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = tuple,
		Functor = "some_tuple", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = univ,
		Functor = "some_univ", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = void,
		Functor = "some_void", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = c_pointer,
		Functor = "some_c_pointer", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = typeinfo,
		Functor = "some_typeinfo", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = typeclassinfo,
		Functor = "some_typeclassinfo", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = array,
		Functor = "some_array", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = succip,
		Functor = "some_succip", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = hp,
		Functor = "some_hp", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = curfr,
		Functor = "some_curfr", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = maxfr,
		Functor = "some_maxfr", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = redofr,
		Functor = "some_redofr", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = redoip,
		Functor = "some_redoip", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = trail_ptr,
		Functor = "some_trail_ptr", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = ticket,
		Functor = "some_ticket", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = reserved_addr,
		Functor = "some_reserved_addr", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = reserved_addr_usereq,
		Functor = "some_reserved_addr_usereq", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = type_ctor_info,
		Functor = "some_typectorinfo", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = base_typeclass_info,
		Functor = "some_base_typeclass_info", 
		Arity = 0,
		Arguments = []
	;
		TypeCtorRep = unknown,
		Functor = "some_unknown", 
		Arity = 0,
		Arguments = []
	).
	


	% Retrieve an argument number from a term, given the functor
	% descriptor.

:- some [T] func get_arg(
		U, int, sectag_locn, du_functor_descriptor, type_info) = T.

get_arg(Term, Index, SecTagLocn, FunctorDesc, TypeInfo) = (Arg) :-
	ArgTypes = FunctorDesc ^ functor_arg_types,
	PseudoTypeInfo = get_pti_from_arg_types(ArgTypes, Index),
	get_type_and_extra_args(TypeInfo, PseudoTypeInfo, Term,
		FunctorDesc, ExtraArgs, ArgTypeInfo),
	( SecTagLocn = none ->
		TagOffset = 0
	;
		TagOffset = 1
	),
	RealArgsOffset = TagOffset + ExtraArgs,
	Arg = get_subterm(ArgTypeInfo, Term, Index, RealArgsOffset).

:- pred get_type_and_extra_args(type_info::in, P::in, T::in,
	du_functor_descriptor::in, int::out, type_info::out) is det.

get_type_and_extra_args(TypeInfoParams, PseudoTypeInfo, Term,
		FunctorDesc, ExtraArgs, ArgTypeInfo) :-
	( 
		typeinfo_is_variable(PseudoTypeInfo, VarNum)
	->
		get_type_info_for_var(TypeInfoParams,
			VarNum, Term, FunctorDesc, ExtraArgs, ExpandedTypeInfo),
		( typeinfo_is_variable(ExpandedTypeInfo, _) ->
			error("get_type_and_extra_args: unbound type variable")
		;
			ArgTypeInfo = ExpandedTypeInfo
		)
	;
		CastTypeInfo = type_info_cast(PseudoTypeInfo),
		TypeCtorInfo = get_type_ctor_info(CastTypeInfo),
		( 
			type_ctor_is_variable_arity(TypeCtorInfo)
		->
			Arity = pseudotypeinfo_get_higher_order_arity(
				CastTypeInfo),
			StartRegionSize = 2
		;
			Arity = TypeCtorInfo ^ type_ctor_arity,
			StartRegionSize = 1
		),
		ArgTypeInfo0 = std_util__no,
		UpperBound = Arity + StartRegionSize - 1,

		iterate_foldl(StartRegionSize, UpperBound,
			(pred(I::in, TI0::in, TI::out) is det :-

				PTI = get_pti_from_type_info(CastTypeInfo, I),
				get_type_and_extra_args(TypeInfoParams, PTI,
					Term, FunctorDesc, _ExtraArgs,
					ETypeInfo),
				( 
					same_pointer_value_untyped(
						ETypeInfo, PTI)
				->
					TI = TI0
				;
					TI0 = std_util__yes(TypeInfo0)
				->
					unsafe_promise_unique(TypeInfo0,
						TypeInfo1),
					update_type_info_index(I, 
						ETypeInfo, TypeInfo1, TypeInfo),
					TI = std_util__yes(TypeInfo)
				;
					NewTypeInfo0 = new_type_info(
						CastTypeInfo, UpperBound),
					update_type_info_index(I, 
						ETypeInfo, NewTypeInfo0, 
						NewTypeInfo),
					TI = std_util__yes(NewTypeInfo)
				)
			), ArgTypeInfo0, MaybeArgTypeInfo),
		( MaybeArgTypeInfo = std_util__yes(ArgTypeInfo1) ->
			ArgTypeInfo = ArgTypeInfo1
		;
			ArgTypeInfo = CastTypeInfo
		),
		ExtraArgs = 0
	).


	% XXX this is completely unimplemented.
:- func pseudotypeinfo_get_higher_order_arity(type_info) = int.
pseudotypeinfo_get_higher_order_arity(_) = 1 :-
	det_unimplemented("pseudotypeinfo_get_higher_order_arity").


	% Make a new type-info with the given arity, using the given type_info
	% as the basis.

:- func new_type_info(type_info::in, int::in) = (type_info::uo) is det.
new_type_info(TypeInfo::in, _::in) = (NewTypeInfo::uo) :- 
	unsafe_promise_unique(TypeInfo, NewTypeInfo),
	det_unimplemented("new_type_info").

:- pragma foreign_proc("C#",
	new_type_info(OldTypeInfo::in, Arity::in) = (NewTypeInfo::uo), [], "
	NewTypeInfo = new object[Arity + 1];
	System.Array.Copy(OldTypeInfo, NewTypeInfo, OldTypeInfo.Length);
").


	% Get the pseudo-typeinfo at the given index from the argument types.
	
:- some [T] func get_pti_from_arg_types(arg_types, int) = T.

get_pti_from_arg_types(_::in, _::in) = (42::out) :-
	det_unimplemented("get_pti_from_arg_types").

:- pragma foreign_proc("C#",
	get_pti_from_arg_types(ArgTypes::in, Index::in) =
		(ArgTypeInfo::out), [], "
	ArgTypeInfo = ArgTypes[Index];
").


	% Get the pseudo-typeinfo at the given index from a type-info.

:- some [T] func get_pti_from_type_info(type_info, int) = T.

get_pti_from_type_info(_::in, _::in) = (42::out) :-
	det_unimplemented("get_pti_from_type_info").

:- pragma foreign_proc("C#",
	get_pti_from_type_info(TypeInfo::in, Index::in) = (PTI::out), [], "
	PTI = TypeInfo[Index];
").



	% Get the type info for a particular type variable number
	% (it might be in the type_info or in the term itself).
	%
	% XXX existentially quantified vars are not yet handled.
	
:- pred get_type_info_for_var(
		type_info::in, int::in, T::in, du_functor_descriptor::in,
		int::out, type_info::out) is det.

get_type_info_for_var(TypeInfo, VarNum, Term, FunctorDesc,
			ExtraArgs, ArgTypeInfo) :-
	(
		type_variable_is_univ_quant(VarNum) 
	->
		ArgTypeInfo = TypeInfo ^ type_info_index(VarNum),
		ExtraArgs = 0
	;
		ExistInfo = FunctorDesc ^ functor_exist_info,
		ExtraArgs = (ExistInfo ^ exist_info_typeinfos_plain) + 
			(ExistInfo ^ exist_info_tcis),

		ExistVarNum = VarNum - pseudotypeinfo_exist_var_base - 1,
		ExistLocn = ExistInfo ^ typeinfo_locns_index(ExistVarNum),
		Slot = ExistLocn ^ exist_arg_num,
		Offset = ExistLocn ^ exist_offset_in_tci,
		
		SlotMaybeTypeInfo = get_typeinfo_from_term(Term, Slot),
		( Offset < 0 ->
			ArgTypeInfo = SlotMaybeTypeInfo
		;
			ArgTypeInfo = typeclass_info_type_info(
				SlotMaybeTypeInfo, Offset)
		)
	).


	% An unchecked cast to type_info (for pseudo-typeinfos).

:- func type_info_cast(T) = type_info.

type_info_cast(X) = unsafe_cast(X).

	% Get a subterm term, given its type_info, the original term, its
	% index and the start region size.

:- some [T] func get_subterm(type_info, U, int, int) = T.

get_subterm(_::in, _::in, _::in, _::in) = (42::out) :-
	det_unimplemented("get_subterm").

:- pragma foreign_proc("C#",
	get_subterm(TypeInfo::in, Term::in, Index::in,
		TagOffset::in) = (Arg::out), [], "
	Arg = ((object[]) Term)[Index + TagOffset];
	TypeInfo_for_T = TypeInfo;
").


	% Test whether a type info is variable.

:- pred typeinfo_is_variable(T::in, int::out) is semidet.

typeinfo_is_variable(_::in, 42::out) :-
	semidet_unimplemented("typeinfo_is_variable").

:- pragma foreign_proc("MC++",
	typeinfo_is_variable(TypeInfo::in, VarNum::out), [], "
	SUCCESS_INDICATOR = (dynamic_cast<MR_Word>(TypeInfo) == NULL);
	if (SUCCESS_INDICATOR) {
		VarNum = System::Convert::ToInt32(TypeInfo);
	}
").


	% Tests for universal and existentially quantified variables.

:- pred type_variable_is_univ_quant(int::in) is semidet.
:- pred type_variable_is_exist_quant(int::in) is semidet.

type_variable_is_exist_quant(X) :- X > pseudotypeinfo_exist_var_base.
type_variable_is_univ_quant(X) :- X =< pseudotypeinfo_exist_var_base.

:- func pseudotypeinfo_exist_var_base = int.
:- func pseudotypeinfo_max_var = int.

pseudotypeinfo_exist_var_base = 512.
pseudotypeinfo_max_var = 1024.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% XXX we have only implemented the .NET backend for the low-level data case.

:- func get_type_ctor_info(type_info) = type_ctor_info is det.

:- pragma foreign_code("C#", "

	// The field numbers of the contents of type_ctor_infos.
	// Fill this in as you add new field accessors.

	enum type_ctor_info_field_nums {
		type_ctor_arity 	= 0,
		// type_ctor_version	= 1,
		type_ctor_rep		= 2,
		type_ctor_num_ptags	= 3,
		type_ctor_unify_pred 	= 4,
		type_ctor_compare_pred	= 5,
		type_ctor_module_name	= 6,
		type_ctor_name		= 7,
		type_functors		= 8,
		type_layout		= 9,
		type_ctor_num_functors	= 10
	}

	enum ptag_layout_field_nums {
		sectag_sharers		= 0,
		sectag_locn		= 1,
		sectag_alternatives	= 2
	}

	enum du_functor_field_nums {
		du_functor_name		= 0,
		du_functor_orig_arity	= 1,
		du_functor_arg_type_contains_var = 2,
		du_functor_sectag_locn	= 3,
		du_functor_primary	= 4,
		du_functor_secondary	= 5,
		du_functor_ordinal	= 6,
		du_functor_arg_types	= 7,
		du_functor_arg_names	= 8,
		du_functor_exist_info	= 9
	}

	enum exist_info_field_nums {
		typeinfos_plain		= 0,
		typeinfos_in_tci	= 1,
		tcis			= 2,
		typeinfo_locns		= 3
	}

	enum exist_locn_field_nums {
		exist_arg_num			= 0,
		exist_offset_in_tci		= 1
	}

").

:- pragma foreign_proc("C#",
	get_type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	try {
		TypeCtorInfo = (object[]) TypeInfo[0];
	} catch (System.InvalidCastException) {
		TypeCtorInfo = TypeInfo;
	}
").

:- pragma foreign_proc("C",
	get_type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeCtorInfo = (MR_Word) MR_TYPEINFO_GET_TYPE_CTOR_INFO(
		(MR_TypeInfo) TypeInfo);
").


:- pred same_pointer_value(T::in, T::in) is semidet.
:- pred same_pointer_value_untyped(T::in, U::in) is semidet.

same_pointer_value(X, Y) :- same_pointer_value_untyped(X, Y).

:- pragma foreign_proc("MC++",
	same_pointer_value_untyped(T1::in, T2::in),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	SUCCESS_INDICATOR = (T1 == T2);
").
:- pragma foreign_proc("C",
	same_pointer_value_untyped(T1::in, T2::in), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	SUCCESS_INDICATOR = (T1 == T2);
").


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- func get_primary_tag(T) = int.
:- func get_remote_secondary_tag(T) = int.

get_primary_tag(_::in) = (0::out) :- 
	det_unimplemented("get_primary_tag").

get_remote_secondary_tag(_::in) = (0::out) :- 
	det_unimplemented("get_remote_secondary_tag").

:- pragma foreign_proc("C#",
	get_primary_tag(X::in) = (Tag::out), [], "
	// We don't look at X to find the tag, for .NET low-level data
	// there is no primary tag, so we always return zero.
	Tag = 0;
").

:- pragma foreign_proc("C#",
	get_remote_secondary_tag(X::in) = (Tag::out), [], "
	object[] data = (object[]) X;
	Tag = (int) data[0];
").



:- type sectag_locn ---> none ; local ; remote ; variable.

:- type du_sectag_alternatives ---> du_sectag_alternatives(c_pointer).

:- type ptag_entry ---> ptag_entry(c_pointer).

:- type du_functor_descriptor ---> du_functor_descriptor(c_pointer).

:- type arg_types ---> arg_types(c_pointer).

:- type exist_info ---> exist_info(c_pointer).

:- type typeinfo_locn ---> typeinfo_locn(c_pointer).

:- func ptag_index(int, type_layout) = ptag_entry.

	% This is an "unimplemented" definition in Mercury, which will be
	% used by default.

ptag_index(_::in, TypeLayout::in) = (unsafe_cast(TypeLayout)::out) :- 
	det_unimplemented("ptag_index").

:- pragma foreign_proc("C#",
	ptag_index(X::in, TypeLayout::in) = (PtagEntry::out), [], "
	PtagEntry = (object[]) TypeLayout[X];
").

:- func sectag_locn(ptag_entry) = sectag_locn.

sectag_locn(PTagEntry::in) = (unsafe_cast(PTagEntry)::out) :- 
	det_unimplemented("sectag_locn").

:- pragma foreign_proc("C#",
	sectag_locn(PTagEntry::in) = (SectagLocn::out), [], "
	SectagLocn = mercury.runtime.LowLevelData.make_enum((int)
		PTagEntry[(int) ptag_layout_field_nums.sectag_locn]);
").

:- func du_sectag_alternatives(int, ptag_entry) = du_functor_descriptor.

du_sectag_alternatives(_::in, PTagEntry::in) = (unsafe_cast(PTagEntry)::out) :- 
	det_unimplemented("sectag_alternatives").

:- pragma foreign_proc("C#",
	du_sectag_alternatives(X::in, PTagEntry::in) =
		(FunctorDescriptor::out), [], "
	object[] sectag_alternatives;
	sectag_alternatives = (object []) 
		PTagEntry[(int) ptag_layout_field_nums.sectag_alternatives];
	FunctorDescriptor = (object []) sectag_alternatives[X];
").

:- func functor_name(du_functor_descriptor) = string.

functor_name(FunctorDescriptor::in) = (unsafe_cast(FunctorDescriptor)::out) :- 
	det_unimplemented("functor_name").

:- pragma foreign_proc("C#",
	functor_name(FunctorDescriptor::in) = (Name::out), [], "
	Name = (string)
		FunctorDescriptor[(int) du_functor_field_nums.du_functor_name];
").

:- func functor_arity(du_functor_descriptor) = int.

functor_arity(FunctorDescriptor::in) = (unsafe_cast(FunctorDescriptor)::out) :- 
	det_unimplemented("functor_arity").

:- pragma foreign_proc("C#",
	functor_arity(FunctorDescriptor::in) = (Name::out), [], "
	Name = (int)
		FunctorDescriptor[(int)
			du_functor_field_nums.du_functor_orig_arity];
		
").

:- func functor_arg_types(du_functor_descriptor) = arg_types.

functor_arg_types(X::in) = (unsafe_cast(X)::out) :- 
	det_unimplemented("functor_arg_types").

:- pragma foreign_proc("C#",
	functor_arg_types(FunctorDescriptor::in) = (ArgTypes::out), [], "
	ArgTypes = (object[])
		FunctorDescriptor[(int)
			du_functor_field_nums.du_functor_arg_types];
		
").

:- func functor_exist_info(du_functor_descriptor) = exist_info.

functor_exist_info(X::in) = (unsafe_cast(X)::out) :- 
	det_unimplemented("functor_exist_info").

:- pragma foreign_proc("C#",
	functor_exist_info(FunctorDescriptor::in) = (ExistInfo::out), [], "
	ExistInfo = (object[])
		FunctorDescriptor[(int)
			du_functor_field_nums.du_functor_exist_info];
		
").


:- func typeinfo_locns_index(int, exist_info) = typeinfo_locn.

typeinfo_locns_index(X::in, _::in) = (unsafe_cast(X)::out) :- 
	det_unimplemented("typeinfo_locns_index").

:- pragma foreign_proc("C#",
	typeinfo_locns_index(X::in, ExistInfo::in) = (TypeInfoLocn::out), [], "

	TypeInfoLocn = (object[]) ((object[]) ExistInfo[(int)
			exist_info_field_nums.typeinfo_locns])[X];
		
").


:- func exist_info_typeinfos_plain(exist_info) = int.

exist_info_typeinfos_plain(X::in) = (unsafe_cast(X)::out) :- 
	det_unimplemented("exist_info_typeinfos_plain").

:- pragma foreign_proc("C#",
	exist_info_typeinfos_plain(ExistInfo::in) = (TypeInfosPlain::out), [], "
	TypeInfosPlain = (int)
		ExistInfo[(int)
			exist_info_field_nums.typeinfos_plain];
").

:- func exist_info_tcis(exist_info) = int.

exist_info_tcis(X::in) = (unsafe_cast(X)::out) :- 
	det_unimplemented("exist_info_tcis").

:- pragma foreign_proc("C#",
	exist_info_tcis(ExistInfo::in) = (TCIs::out), [], "
	TCIs = (int) ExistInfo[(int)
			exist_info_field_nums.tcis];
").





:- func exist_arg_num(typeinfo_locn) = int.

exist_arg_num(X::in) = (unsafe_cast(X)::out) :- 
	det_unimplemented("exist_arg_num").

:- pragma foreign_proc("C#",
	exist_arg_num(TypeInfoLocn::in) = (ArgNum::out), [], "
	ArgNum = (int) TypeInfoLocn[(int) exist_locn_field_nums.exist_arg_num];
		
").

:- func exist_offset_in_tci(typeinfo_locn) = int.

exist_offset_in_tci(X::in) = (unsafe_cast(X)::out) :- 
	det_unimplemented("exist_arg_num").

:- pragma foreign_proc("C#",
	exist_offset_in_tci(TypeInfoLocn::in) = (ArgNum::out), [], "
	ArgNum = (int)
		TypeInfoLocn[(int) exist_locn_field_nums.exist_offset_in_tci];
		
").

:- func get_typeinfo_from_term(U, int) = type_info.

get_typeinfo_from_term(_::in, X::in) = (unsafe_cast(X)::out) :- 
	det_unimplemented("get_typeinfo_from_term").

:- pragma foreign_proc("C#",
	get_typeinfo_from_term(Term::in, Index::in) = (TypeInfo::out), [], "
	TypeInfo = (object[]) ((object[]) Term)[Index];
").

:- func typeclass_info_type_info(type_info, int) = type_info.

typeclass_info_type_info(TypeClassInfo, Index) = unsafe_cast(TypeInfo) :-
	private_builtin__type_info_from_typeclass_info(
		unsafe_cast(TypeClassInfo)
			`with_type` private_builtin__typeclass_info(int),
		Index, TypeInfo
			`with_type` private_builtin__type_info(int)).



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func type_info_index(int, type_info) = type_info.

	% This is an "unimplemented" definition in Mercury, which will be
	% used by default.

type_info_index(_::in, TypeInfo::in) = (TypeInfo::out) :- 
	det_unimplemented("type_info_index").

:- pragma foreign_proc("C#",
	type_info_index(X::in, TypeInfo::in) = (TypeInfoAtIndex::out),
		[will_not_call_mercury, promise_pure], "
	TypeInfoAtIndex = (object[]) TypeInfo[X];
").

:- pred update_type_info_index(int::in, type_info::in, type_info::di,
		type_info::uo) is det.

update_type_info_index(_::in, _::in, X::di, X::uo) :- 
	det_unimplemented("type_info_index").

:- pragma foreign_proc("C#",
	update_type_info_index(X::in, NewValue::in, OldTypeInfo::di,
		NewTypeInfo::uo), [will_not_call_mercury, promise_pure], "
	OldTypeInfo[X] = NewValue;
	NewTypeInfo = OldTypeInfo;
").



:- pred semidet_unimplemented(string::in) is semidet.
semidet_unimplemented(S) :-
	( std_util__semidet_succeed ->
		error("rtti_implementation: unimplemented: " ++ S)
	;
		std_util__semidet_succeed
	).

:- pred det_unimplemented(string::in) is det.
det_unimplemented(S) :-
	( std_util__semidet_succeed ->
		error("rtti_implementation: unimplemented: " ++ S)
	;
		true
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func type_ctor_arity(type_ctor_info) = int.
:- pragma foreign_proc("C#",
	type_ctor_arity(TypeCtorInfo::in) = (Arity::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Arity = (int) TypeCtorInfo[
			(int) type_ctor_info_field_nums.type_ctor_arity];
").
:- pragma foreign_proc("C",
	type_ctor_arity(TypeCtorInfo::in) = (Arity::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	Arity = tci->MR_type_ctor_arity;
").

:- some [P] func type_ctor_unify_pred(type_ctor_info) = P.
:- pragma foreign_proc("C#",
	type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	UnifyPred = TypeCtorInfo[
			(int) type_ctor_info_field_nums.type_ctor_unify_pred];
").
:- pragma foreign_proc("C",
	type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	UnifyPred = (MR_Integer) tci->MR_type_ctor_unify_pred;
").

:- some [P] func type_ctor_compare_pred(type_ctor_info) = P.
:- pragma foreign_proc("C#",
	type_ctor_compare_pred(TypeCtorInfo::in) = (UnifyPred::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	UnifyPred = TypeCtorInfo[
			(int) type_ctor_info_field_nums.type_ctor_compare_pred];
").
:- pragma foreign_proc("C",
	type_ctor_compare_pred(TypeCtorInfo::in) = (UnifyPred::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	UnifyPred = (MR_Integer) tci->MR_type_ctor_compare_pred;
").



:- func type_ctor_rep(type_ctor_info) = type_ctor_rep.
:- pragma foreign_proc("C#",
	type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	int rep;
	rep = (int) TypeCtorInfo[
		(int) type_ctor_info_field_nums.type_ctor_rep];
	TypeCtorRep = mercury.runtime.LowLevelData.make_enum(rep);
").
:- pragma foreign_proc("C",
	type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	TypeCtorRep = MR_type_ctor_rep(tci);
").


:- func type_ctor_module_name(type_ctor_info) = string.

:- pragma foreign_proc("C#",
	type_ctor_module_name(TypeCtorInfo::in) = (Name::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Name = (string)
		TypeCtorInfo[(int)
		type_ctor_info_field_nums.type_ctor_module_name];
").

:- pragma foreign_proc("C",
	type_ctor_module_name(TypeCtorInfo::in) = (Name::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	Name = (MR_String) MR_type_ctor_module_name(tci);
").



:- func type_ctor_name(type_ctor_info) = string.

:- pragma foreign_proc("C#",
	type_ctor_name(TypeCtorInfo::in) = (Name::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Name = (string)
		TypeCtorInfo[(int) type_ctor_info_field_nums.type_ctor_name];
").
:- pragma foreign_proc("C",
	type_ctor_name(TypeCtorInfo::in) = (Name::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	Name = (MR_String) MR_type_ctor_name(tci);
").


:- func type_layout(type_ctor_info) = type_layout.

:- pragma foreign_proc("C#",
	type_layout(TypeCtorInfo::in) = (TypeLayout::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	TypeLayout = (object[])
		TypeCtorInfo[(int) type_ctor_info_field_nums.type_layout];
").
:- pragma foreign_proc("C",
	type_layout(TypeCtorInfo::in) = (TypeLayout::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	TypeLayout = (MR_Word) &(MR_type_ctor_layout(tci));
").

:- pragma foreign_proc("C",
	unsafe_cast(VarIn::in) = (VarOut::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	VarOut = VarIn;
").
:- pragma foreign_proc("C#",
	unsafe_cast(VarIn::in) = (VarOut::out),
		[will_not_call_mercury, promise_pure, thread_safe],
"
	VarOut = VarIn;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
