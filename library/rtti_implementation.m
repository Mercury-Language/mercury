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

:- pred compare_type_infos(comparison_result::out,
		type_info::in, type_info::in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

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
	get_type_ctor_info(TypeInfo1, TypeCtorInfo1),
	get_type_ctor_info(TypeInfo2, TypeCtorInfo2),

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
			error("rtti_implementation.m: unimplemented: tuples and higher order type comparisons")
		;
			true
		)

		% XXX code to handle tuples and higher order
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
	get_type_ctor_info(TypeInfo, TypeCtorInfo),
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

:- pred get_type_ctor_info(type_info::in, type_ctor_info::out) is det.

:- pragma foreign_code("C#", "

	// The field numbers of the contents of type_ctor_infos.
	// Fill this in as you add new field accessors.

	enum type_ctor_info_field_nums {
		type_ctor_rep = 4,
		type_ctor_module_name = 7,
		type_ctor_name = 8,
		type_layout = 11
	}

").



:- pragma foreign_proc("C#",
	get_type_ctor_info(TypeInfo::in, TypeCtorInfo::out), [],
"
	try {
		TypeCtorInfo = (object[]) TypeInfo[0];
	} catch (System.InvalidCastException) {
		TypeCtorInfo = TypeInfo;
	}
").

:- pragma foreign_proc("C",
	get_type_ctor_info(TypeInfo::in, TypeCtorInfo::out), [],
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
