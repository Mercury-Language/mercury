:- module simplify_bug2.

:- interface.

:- type type_info ---> type_info(c_pointer).
:- type du_functor_descriptor ---> du_functor_descriptor(c_pointer).

:- pred get_type_and_extra_args(type_info::in, P::in,
	type_info::out) is det.

:- implementation.

:- import_module require.
:- use_module std_util.

get_type_and_extra_args(TypeInfoParams, PseudoTypeInfo, ArgTypeInfo) :-
	( 
		typeinfo_is_variable(PseudoTypeInfo, VarNum)
	->
		get_type_info_for_var(TypeInfoParams,
			VarNum, ExpandedTypeInfo),
		( typeinfo_is_variable(ExpandedTypeInfo, _) ->
			error("get_type_and_extra_args: unbound type variable")
		;
			ArgTypeInfo = ExpandedTypeInfo
		)
	;
		error("get_type_and_extra_args")
	).

:- pred get_type_info_for_var(type_info::in, int::in, type_info::out) is det.
:- pragma no_inline(get_type_info_for_var/3).

get_type_info_for_var(X, _, X).

:- pred typeinfo_is_variable(T::in, int::out) is semidet.
:- pragma no_inline(typeinfo_is_variable/2).

typeinfo_is_variable(_, 42) :-
	std_util__semidet_succeed.

