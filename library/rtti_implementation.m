%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
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

	% Our type_info and type_ctor_info implementations are both
	% abstract types.
:- type type_info.
:- type type_ctor_info.

:- func get_type_info(T::unused) = (type_info::out) is det.

:- pred generic_unify(T::in, T::in) is semidet.

:- pred generic_compare(comparison_result::out, T::in, T::in) is det.

:- pred compare_type_infos(comparison_result::out,
		type_info::in, type_info::in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, string.

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
	;	equiv_var
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
	;	currfr
	;	maxfr
	;	redofr
	;	redoip
	;	trail_ptr
	;	ticket
	;	notag_ground
	;	notag_ground_usereq
	;	equiv_ground
	;	tuple
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
	get_type_info(_T::unused) = (TypeInfo::out), [], " 
	TypeInfo = TypeInfo_for_T;
").

:- pragma foreign_proc("C",
	get_type_info(_T::unused) = (TypeInfo::out), [], "
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
		TypeCtorRep = (pred)
	->
		error("rtti_implementation.m: unimplemented: higher order comparisons")
	;	
		Arity = TypeCtorInfo ^ type_ctor_arity,
		ComparePred = TypeCtorInfo ^ type_ctor_compare_pred,
		( Arity = 0 ->
			result_call_4(ComparePred, Res, X, Y)
		; Arity = 1 ->
			result_call_5(ComparePred, Res,
				TypeInfo ^ index(1), X, Y)
		; Arity = 2 ->
			result_call_6(ComparePred, Res,  
				TypeInfo ^ index(1), TypeInfo ^ index(2), 
				X, Y)
		; Arity = 3 ->
			result_call_7(ComparePred, Res,
				TypeInfo ^ index(1), TypeInfo ^ index(2), 
				TypeInfo ^ index(3),
				X, Y)
		; Arity = 4 ->
			result_call_8(ComparePred, Res,
				TypeInfo ^ index(1), TypeInfo ^ index(2), 
				TypeInfo ^ index(3), TypeInfo ^ index(4),
				X, Y)
		; Arity = 5 ->
			result_call_9(ComparePred, Res,
				TypeInfo ^ index(1), TypeInfo ^ index(2), 
				TypeInfo ^ index(3), TypeInfo ^ index(4),
				TypeInfo ^ index(5),
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
		TypeCtorRep = (pred)
	->
		error("rtti_implementation.m: unimplemented: higher order unifications")
	;	
		Arity = TypeCtorInfo ^ type_ctor_arity,
		UnifyPred = TypeCtorInfo ^ type_ctor_unify_pred,
		( Arity = 0 ->
			semidet_call_3(UnifyPred, X, Y)
		; Arity = 1 ->
			semidet_call_4(UnifyPred, TypeInfo ^ index(1), X, Y)
		; Arity = 2 ->
			semidet_call_5(UnifyPred, 
				TypeInfo ^ index(1), TypeInfo ^ index(2), 
				X, Y)
		; Arity = 3 ->
			semidet_call_6(UnifyPred, 
				TypeInfo ^ index(1), TypeInfo ^ index(2), 
				TypeInfo ^ index(3),
				X, Y)
		; Arity = 4 ->
			semidet_call_7(UnifyPred, 
				TypeInfo ^ index(1), TypeInfo ^ index(2), 
				TypeInfo ^ index(3), TypeInfo ^ index(4),
				X, Y)
		; Arity = 5 ->
			semidet_call_8(UnifyPred, 
				TypeInfo ^ index(1), TypeInfo ^ index(2), 
				TypeInfo ^ index(3), TypeInfo ^ index(4),
				TypeInfo ^ index(5),
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
		[will_not_call_mercury, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_3(Pred, X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_4(Pred::in, A::in, X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_4(Pred, A, X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_5(Pred::in, A::in, B::in, X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_5(Pred, A, B, X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_6(Pred::in, A::in, B::in, C::in, X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_6(Pred, A, B, C,
			X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_7(Pred::in, A::in, B::in, C::in, D::in, X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_7(Pred, A, B, C, D,
			X, Y);
").
:- pragma foreign_proc("MC++",
	semidet_call_8(Pred::in, A::in, B::in, C::in, D::in, E::in,
		X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	SUCCESS_INDICATOR =
		mercury::runtime::GenericCall::semidet_call_8(Pred, A, B, C, D,
			E, X, Y);
").



:- pragma foreign_proc("C#",
	result_call_4(Pred::in, Res::out, X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	mercury.runtime.GenericCall.result_call_4(Pred, ref Res, X, Y);
").

:- pragma foreign_proc("C#",
	result_call_5(Pred::in, Res::out, A::in, X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	mercury.runtime.GenericCall.result_call_5(Pred, A, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
	result_call_6(Pred::in, Res::out, A::in, B::in, X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	mercury.runtime.GenericCall.result_call_6(Pred, A, B, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
	result_call_7(Pred::in, Res::out, A::in, B::in, C::in, X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	mercury.runtime.GenericCall.result_call_7(Pred, A, B, C, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
	result_call_8(Pred::in, Res::out, A::in, B::in, C::in, D::in, X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
"
	mercury.runtime.GenericCall.result_call_8(Pred, A, B, C, D,
		ref Res, X, Y);
").
:- pragma foreign_proc("C#",
	result_call_9(Pred::in, Res::out, A::in, B::in, C::in, D::in, E::in,
		X::in, Y::in), 
		[will_not_call_mercury, thread_safe],
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
			TypeCtorInfo1 ^ type_ctor_module_name = "builtin",
			( TypeCtorInfo1 ^ type_ctor_name = "tuple" 
			; TypeCtorInfo1 ^ type_ctor_name = "pred" 
			; TypeCtorInfo1 ^ type_ctor_name = "func" 
			)
		->
			% XXX code to handle tuples and higher order
			error("rtti_implementation.m: unimplemented: tuples and higher order type comparisons")
		;
			true
		)
	;
		Res = NameRes
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
%
% XXX we have only implemented the .NET backend for the low-level data case.

:- func get_type_ctor_info(type_info) = type_ctor_info is det.

:- pragma foreign_code("C#", "

	// The field numbers of the contents of type_ctor_infos.
	// Fill this in as you add new field accessors.

	enum type_ctor_info_field_nums {
		type_ctor_arity = 0,
		type_ctor_unify_pred = 1,
		type_ctor_compare_pred = 3,
		type_ctor_rep = 4,
		type_ctor_module_name = 7,
		type_ctor_name = 8,
		type_functors = 10,
		type_layout = 11,
		type_ctor_num_functors = 12,
		type_ctor_num_ptags = 13
	}

").

:- pragma foreign_proc("C#",
	get_type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out), [],
"
	try {
		TypeCtorInfo = (object[]) TypeInfo[0];
	} catch (System.InvalidCastException) {
		TypeCtorInfo = TypeInfo;
	}
").

:- pragma foreign_proc("C",
	get_type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out), [],
"
	TypeCtorInfo = (MR_Word) MR_TYPEINFO_GET_TYPE_CTOR_INFO(
		(MR_TypeInfo) TypeInfo);
").


:- pred same_pointer_value(T::in, T::in) is semidet.

:- pragma foreign_proc("MC++",
	same_pointer_value(T1::in, T2::in), [], "
	SUCCESS_INDICATOR = (T1 == T2);
").
:- pragma foreign_proc("C",
	same_pointer_value(T1::in, T2::in), [], "
	SUCCESS_INDICATOR = (T1 == T2);
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func index(int, type_info) = type_info.
:- pragma foreign_proc("C#",
	index(X::in, TypeInfo::in) = (TypeInfoAtIndex::out), [], "
	TypeInfoAtIndex = (object[]) TypeInfo[X];
").

	% This is an "unimplemented" definition in Mercury, which will be
	% used by default.

index(_::in, TypeInfo::in) = (TypeInfo::out) :- 
	det_unimplemented("index").


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
	type_ctor_arity(TypeCtorInfo::in) = (Arity::out), [], "
	Arity = (int) TypeCtorInfo[
			(int) type_ctor_info_field_nums.type_ctor_arity];
").
:- pragma foreign_proc("C",
	type_ctor_arity(TypeCtorInfo::in) = (Arity::out), [], "
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	Arity = tci->arity;
").

:- some [P] func type_ctor_unify_pred(type_ctor_info) = P.
:- pragma foreign_proc("C#",
	type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out), [], "
	UnifyPred = TypeCtorInfo[
			(int) type_ctor_info_field_nums.type_ctor_unify_pred];
").
:- pragma foreign_proc("C",
	type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out), [], "
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	UnifyPred = (MR_Integer) tci->unify_pred;
").

:- some [P] func type_ctor_compare_pred(type_ctor_info) = P.
:- pragma foreign_proc("C#",
	type_ctor_compare_pred(TypeCtorInfo::in) = (UnifyPred::out), [], "
	UnifyPred = TypeCtorInfo[
			(int) type_ctor_info_field_nums.type_ctor_compare_pred];
").
:- pragma foreign_proc("C",
	type_ctor_compare_pred(TypeCtorInfo::in) = (UnifyPred::out), [], "
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	UnifyPred = (MR_Integer) tci->compare_pred;
").



:- func type_ctor_rep(type_ctor_info) = type_ctor_rep.
:- pragma foreign_proc("C#",
	type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out), [], "
	int rep;
	rep = (int) TypeCtorInfo[
		(int) type_ctor_info_field_nums.type_ctor_rep];
	TypeCtorRep = mercury.runtime.LowLevelData.make_enum(rep);
").
:- pragma foreign_proc("C",
	type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out), [], "
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	TypeCtorRep = tci->type_ctor_rep;
").


:- func type_ctor_module_name(type_ctor_info) = string.

:- pragma foreign_proc("C#",
	type_ctor_module_name(TypeCtorInfo::in) = (Name::out), [], "
	Name = (string)
		TypeCtorInfo[(int)
		type_ctor_info_field_nums.type_ctor_module_name];
").

:- pragma foreign_proc("C",
	type_ctor_module_name(TypeCtorInfo::in) = (Name::out), [], "
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	Name = (MR_String) tci->type_ctor_module_name;
").



:- func type_ctor_name(type_ctor_info) = string.

:- pragma foreign_proc("C#",
	type_ctor_name(TypeCtorInfo::in) = (Name::out), [], "
	Name = (string)
		TypeCtorInfo[(int) type_ctor_info_field_nums.type_ctor_name];
").
:- pragma foreign_proc("C",
	type_ctor_name(TypeCtorInfo::in) = (Name::out), [], "
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	Name = (MR_String) tci->type_ctor_name;
").


:- func type_layout(type_ctor_info) = type_layout.

:- pragma foreign_proc("C#",
	type_layout(TypeCtorInfo::in) = (TypeLayout::out), [], "
	TypeLayout = (object[])
		TypeCtorInfo[(int) type_ctor_info_field_nums.type_layout];
").
:- pragma foreign_proc("C",
	type_layout(TypeCtorInfo::in) = (TypeLayout::out), [], "
	MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
	TypeLayout = tci->type_layout;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
