%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prog_util.
:- interface.
:- import_module prog_io, list.

%-----------------------------------------------------------------------------%

	% XXX change the `goedel_' prefixes

	% The following predicate goedel_expand_eqv_types traverses
	% through the list of items.  Each time it finds an eqv_type
	% definition, it replaces all occurrences of the type (both
	% before and after it in the list of items) with type that it
	% is equivalent to.  This has the effect of eliminating all the
	% equivalence types from the source code.  Circular equivalence
	% types in the input will cause references to undefined types
	% in the output.

:- pred goedel_expand_eqv_types(list(item_and_context), list(item_and_context)).
:- mode goedel_expand_eqv_types(input, output).
	
	% The following predicate goedel_replace_eqv_type_list
	% performs substititution of a single type on a list
	% of items.  It is used in mercury_to_goedel to rename
	% type `int' as `integer'.

:- pred goedel_replace_eqv_type_list(list(item_and_context), varset, string,
			list(type_param), type, list(item_and_context)).
:- mode goedel_replace_eqv_type_list(input, input, input, input, input, output).

%-----------------------------------------------------------------------------%

	% Convert a (possibly module-qualified) sym_name into a string.

:- pred unqualify_name(sym_name, string).
:- mode unqualify_name(input, output) is det.

%-----------------------------------------------------------------------------%

	% A pred declaration may contains just types, as in
	%	:- pred append(list(T), list(T), list(T)).
	% or it may contain both types and modes, as in
	%	:- pred append(list(T)::input, list(T)::input,
	%			list(T)::output).
	%
	% This predicate takes the argument list of a pred declaration,
	% splits it into two separate lists for the types and (if present)
	% the modes.

:- type maybe_modes ---> yes(list(mode)) ; no.

:- pred split_types_and_modes(list(type_and_mode), list(type), maybe_modes).
:- mode split_types_and_modes(input, output, output).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

goedel_expand_eqv_types(Items0, Items) :-
	goedel_replace_all_eqv_types(Items0, [], Items1),
	reverse(Items1, Items).

:- pred goedel_replace_all_eqv_types(list(item_and_context),
		list(item_and_context), list(item_and_context)).
:- mode goedel_replace_all_eqv_types(input, input, output).

goedel_replace_all_eqv_types([], Items, Items).
goedel_replace_all_eqv_types([Item - Context | Items0], ItemList0, ItemList) :-
	( Item = type_defn(VarSet, eqv_type(Name, Args, Body), _Cond) ->
		unqualify_name(Name, Name2),
		goedel_replace_eqv_type_list(ItemList0, VarSet, Name2, Args,
				Body, ItemList1),
		goedel_replace_eqv_type_list(Items0, VarSet, Name2, Args, Body,				Items1),
		goedel_replace_all_eqv_types(Items1, ItemList1, ItemList)
	;
		goedel_replace_all_eqv_types(Items0,
				[Item - Context | ItemList0], ItemList)
	).

goedel_replace_eqv_type_list([], _, _, _, _, []).
goedel_replace_eqv_type_list([Item0 - Context| Items0], VarSet, Name, Args,
				Body, [Item - Context| Items]) :-
	(
		%some [Item1]
		goedel_replace_eqv_type(Item0, VarSet, Name, Args, Body, Item1)
	->
		Item = Item1
	;
		Item = Item0
	),
	goedel_replace_eqv_type_list(Items0, VarSet, Name, Args, Body, Items).

:- pred goedel_replace_eqv_type(item, varset, string, list(type_param), type,
			item).
:- mode goedel_replace_eqv_type(input, input, input, input, input, output).

goedel_replace_eqv_type(type_defn(VarSet0, TypeDefn0, Cond),
			TVarSet, Name, Args0, Body0,
			type_defn(VarSet0, TypeDefn, Cond)) :-
	varset__merge(VarSet0, TVarSet, [Body0 | Args0], _, [Body | Args]),
	goedel_replace_eqv_type_defn(TypeDefn0, Name, Args, Body, TypeDefn).

goedel_replace_eqv_type(pred(VarSet0, PredName, TypesAndModes0, Det, Cond),
			TVarSet, Name, Args0, Body0,
			pred(VarSet0, PredName, TypesAndModes, Det, Cond)) :-
	varset__merge(VarSet0, TVarSet, [Body0 | Args0], _, [Body | Args]),
	goedel_replace_eqv_type_pred(TypesAndModes0, Name, Args, Body,
		no, TypesAndModes, yes).
	
:- pred goedel_replace_eqv_type_defn(type_defn, string, list(type_param),
					type, type_defn).
:- mode goedel_replace_eqv_type_defn(input, input, input, input, output).

goedel_replace_eqv_type_defn(eqv_type(TName, TArgs, TBody0),
				Name, Args, Body,
				eqv_type(TName, TArgs, TBody)) :-
	goedel_replace_eqv_type_type(TBody0, Name, Args, Body, no, TBody, yes).
goedel_replace_eqv_type_defn(uu_type(TName, TArgs, TBody0),
				Name, Args, Body,
				uu_type(TName, TArgs, TBody)) :-
	goedel_replace_eqv_type_uu(TBody0, Name, Args, Body, no, TBody, yes).
goedel_replace_eqv_type_defn(du_type(TName, TArgs, TBody0),
				Name, Args, Body,
				du_type(TName, TArgs, TBody)) :-
	goedel_replace_eqv_type_du(TBody0, Name, Args, Body, no, TBody, yes).


:- pred goedel_replace_eqv_type_uu(list(type), string, list(type_param),
					type, bool, list(type), bool).
:- mode goedel_replace_eqv_type_uu(input, input, input, input, input,
					output, output).

goedel_replace_eqv_type_uu([], _Name, _Args, _Body, Found, [], Found).
goedel_replace_eqv_type_uu([T0|Ts0], Name, Args, Body, Found0, [T|Ts], Found) :-
	goedel_replace_eqv_type_type(T0, Name, Args, Body, Found0, T, Found1),
	goedel_replace_eqv_type_uu(Ts0, Name, Args, Body, Found1, Ts, Found).

:- pred goedel_replace_eqv_type_du(list(constructor), string, list(type_param),
				type, bool, list(constructor), bool).
:- mode goedel_replace_eqv_type_du(input, input, input, input, input,
					output, output).

goedel_replace_eqv_type_du([], _Name, _Args, _Body, Found, [], Found).
goedel_replace_eqv_type_du([T0|Ts0], Name, Args, Body, Found0, [T|Ts], Found) :-
	goedel_replace_eqv_type_ctor(T0, Name, Args, Body, Found0, T, Found1),
	goedel_replace_eqv_type_du(Ts0, Name, Args, Body, Found1, Ts, Found).

:- pred goedel_replace_eqv_type_ctor(constructor, string, list(type_param),
				type, bool, constructor, bool).
:- mode goedel_replace_eqv_type_ctor(input, input, input, input, input,
					output, output).

goedel_replace_eqv_type_ctor(TName - Targs0, Name, Args, Body, Found0,
		TName - Targs, Found) :-
	goedel_replace_eqv_type_uu(Targs0, Name, Args, Body, Found0,
		Targs, Found).

:- pred goedel_replace_eqv_type_type(type, string, list(type_param),
				type, bool, type, bool).
:- mode goedel_replace_eqv_type_type(input, input, input, input, input,
					output, output).

goedel_replace_eqv_type_type(term_variable(V), _Name, _Args, _Body, Found,
		term_variable(V), Found).
goedel_replace_eqv_type_type(term_functor(F, TArgs0, Context), Name, Args,
		Body, Found0, Type, Found) :- 
	(	
		F = term_atom(Name),
		same_length(TArgs0, Args)
	->
		type_param_to_var_list(Args, Args2),
		term__substitute_corresponding(Args2, TArgs0, Body, Type),
		Found = yes
	;
		goedel_replace_eqv_type_uu(TArgs0, Name, Args, Body, Found0,
			TArgs, Found),
		Type = term_functor(F, TArgs, Context)
	).

:- pred type_param_to_var_list(list(type_param), list(var)).
:- mode type_param_to_var_list(input, output).

type_param_to_var_list([], []).
type_param_to_var_list([T | Ts], [V | Vs]) :-
	type_param_to_var(T, V),
	type_param_to_var_list(Ts, Vs).

:- pred type_param_to_var(type_param, var).
:- mode type_param_to_var(input, output).

type_param_to_var(term_variable(V), V).

:- pred goedel_replace_eqv_type_pred(list(type_and_mode), string,
	list(type_param), type, bool, list(type_and_mode), bool).
:- mode goedel_replace_eqv_type_pred(input, input, input, input, input,
					output, output).

goedel_replace_eqv_type_pred([], _Name, _Args, _Body, Found, [], Found).
goedel_replace_eqv_type_pred([TM0|TMs0], Name, Args, Body, Found0,
				[TM|TMs], Found) :-
	goedel_replace_eqv_type_tm(TM0, Name, Args, Body, Found0, TM, Found1),
	goedel_replace_eqv_type_pred(TMs0, Name, Args, Body, Found1,
					TMs, Found).
:- pred goedel_replace_eqv_type_tm(type_and_mode, string, list(type_param),
				type, bool, type_and_mode, bool).
:- mode goedel_replace_eqv_type_tm(input, input, input, input, input,
					output, output).

goedel_replace_eqv_type_tm(type_only(Type0), Name, Args, Body, Found0,
				type_only(Type), Found) :-
	goedel_replace_eqv_type_type(Type0, Name, Args, Body, Found0, Type,
		Found).
goedel_replace_eqv_type_tm(type_and_mode(Type0, Mode), Name, Args, Body, Found0,
				type_and_mode(Type, Mode), Found) :-
	goedel_replace_eqv_type_type(Type0, Name, Args, Body, Found0, Type,
		Found).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

unqualify_name(unqualified(Name), Name).
unqualify_name(qualified(_Module, Name), Name).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

split_types_and_modes(TypesAndModes, Types, MaybeModes) :-
	split_types_and_modes_2(TypesAndModes, yes, Types, Modes, Result),
	(
		Result = yes
	->
		MaybeModes = yes(Modes)
	;
		MaybeModes = no
	).

:- type maybe ---> yes ; no.
:- pred split_types_and_modes_2(list(type_and_mode), maybe,
				list(type), list(mode), maybe).
:- mode split_types_and_modes_2(input, input, output, output, output).

	% T = type, M = mode, TM = combined type and mode
split_types_and_modes_2([], Result, [], [], Result).
split_types_and_modes_2([TM|TMs], Result0, [T|Ts], [M|Ms], Result) :-
	split_type_and_mode(TM, Result0, T, M, Result1),
	split_types_and_modes_2(TMs, Result1, Ts, Ms, Result).

	% if a pred declaration specifies modes for some but
	% not all of the arguments, then the modes are ignored
	% - should this be an error instead?

:- pred split_type_and_mode(type_and_mode, maybe, type, mode, maybe).
:- mode split_type_and_mode(input, input, output, output, output).

split_type_and_mode(type_only(T), _, T, (free -> free), no).
split_type_and_mode(type_and_mode(T,M), R, T, M, R).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
