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
:- mode goedel_expand_eqv_types(in, out) is det.
	
	% The following predicate goedel_replace_eqv_type_list
	% performs substititution of a single type on a list
	% of items.  It is used in mercury_to_goedel to rename
	% type `int' as `integer'.

:- pred goedel_replace_eqv_type_list(list(item_and_context), varset, string,
			list(type_param), type, list(item_and_context)).
:- mode goedel_replace_eqv_type_list(in, in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% Convert a (possibly module-qualified) sym_name into a string.

:- pred unqualify_name(sym_name, string).
:- mode unqualify_name(in, out) is det.

:- pred sym_name_get_module_name(sym_name, module_name, module_name).
:- mode sym_name_get_module_name(in, in, out) is det.

%-----------------------------------------------------------------------------%

	% A pred declaration may contains just types, as in
	%	:- pred list__append(list(T), list(T), list(T)).
	% or it may contain both types and modes, as in
	%	:- pred list__append(list(T)::in, list(T)::in,
	%			list(T)::output).
	%
	% This predicate takes the argument list of a pred declaration,
	% splits it into two separate lists for the types and (if present)
	% the modes.

:- type maybe_modes == maybe(list(mode)).

:- pred split_types_and_modes(list(type_and_mode), list(type), maybe_modes).
:- mode split_types_and_modes(in, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, varset, term.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

goedel_expand_eqv_types(Items0, Items) :-
	goedel_replace_all_eqv_types(Items0, [], Items1),
	list__reverse(Items1, Items).

:- pred goedel_replace_all_eqv_types(list(item_and_context),
		list(item_and_context), list(item_and_context)).
:- mode goedel_replace_all_eqv_types(in, in, out) is det.

goedel_replace_all_eqv_types([], Items, Items).
goedel_replace_all_eqv_types([Item - Context | Items0], ItemList0, ItemList) :-
	( Item = type_defn(VarSet, eqv_type(Name, Args, Body), _Cond) ->
		unqualify_name(Name, Name2),
		goedel_replace_eqv_type_list(ItemList0, VarSet, Name2, Args,
				Body, ItemList1),
		goedel_replace_eqv_type_list(Items0, VarSet, Name2, Args, Body,				Items1)
	;
		Items1 = Items0,
		ItemList1 = [Item - Context | ItemList0]
	),
	goedel_replace_all_eqv_types(Items1, ItemList1, ItemList).

goedel_replace_eqv_type_list([], _, _, _, _, []).
goedel_replace_eqv_type_list([Item0 - Context| Items0], VarSet, Name, Args,
				Body, [Item - Context| Items]) :-
	% Attempting to replace an equivalence type can cause
	% quite a bit of memory allocation.  If it turns out that
	% we don't need to replace anything, then we fail so that
	% we can quickly reclaim this memory.
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
:- mode goedel_replace_eqv_type(in, in, in, in, in, out) is det.

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
:- mode goedel_replace_eqv_type_defn(in, in, in, in, out) is det.

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
:- mode goedel_replace_eqv_type_uu(in, in, in, in, in, out, out) is det.

goedel_replace_eqv_type_uu([], _Name, _Args, _Body, Found, [], Found).
goedel_replace_eqv_type_uu([T0|Ts0], Name, Args, Body, Found0, [T|Ts], Found) :-
	goedel_replace_eqv_type_type(T0, Name, Args, Body, Found0, T, Found1),
	goedel_replace_eqv_type_uu(Ts0, Name, Args, Body, Found1, Ts, Found).

:- pred goedel_replace_eqv_type_du(list(constructor), string, list(type_param),
				type, bool, list(constructor), bool).
:- mode goedel_replace_eqv_type_du(in, in, in, in, in, out, out) is det.

goedel_replace_eqv_type_du([], _Name, _Args, _Body, Found, [], Found).
goedel_replace_eqv_type_du([T0|Ts0], Name, Args, Body, Found0, [T|Ts], Found) :-
	goedel_replace_eqv_type_ctor(T0, Name, Args, Body, Found0, T, Found1),
	goedel_replace_eqv_type_du(Ts0, Name, Args, Body, Found1, Ts, Found).

:- pred goedel_replace_eqv_type_ctor(constructor, string, list(type_param),
				type, bool, constructor, bool).
:- mode goedel_replace_eqv_type_ctor(in, in, in, in, in, out, out) is det.

goedel_replace_eqv_type_ctor(TName - Targs0, Name, Args, Body, Found0,
		TName - Targs, Found) :-
	goedel_replace_eqv_type_uu(Targs0, Name, Args, Body, Found0,
		Targs, Found).

:- pred goedel_replace_eqv_type_type(type, string, list(type_param),
				type, bool, type, bool).
:- mode goedel_replace_eqv_type_type(in, in, in, in, in, out, out) is det.

goedel_replace_eqv_type_type(term__variable(V), _Name, _Args, _Body, Found,
		term__variable(V), Found).
goedel_replace_eqv_type_type(term__functor(F, TArgs0, Context), Name, Args,
		Body, Found0, Type, Found) :- 
	goedel_replace_eqv_type_uu(TArgs0, Name, Args, Body, Found0,
		TArgs1, Found1),
	(	
		F = term__atom(Name),
		list__same_length(TArgs1, Args)
	->
		term__term_list_to_var_list(Args, Args2),
		term__substitute_corresponding(Args2, TArgs1, Body, Type),
		Found = yes
	;
		% could we improve efficiency here by reclaiming
		% garbage (or avoiding allocating it in the first place)?
		Found = Found1,
		Type = term__functor(F, TArgs1, Context)
	).

:- pred goedel_replace_eqv_type_pred(list(type_and_mode), string,
	list(type_param), type, bool, list(type_and_mode), bool).
:- mode goedel_replace_eqv_type_pred(in, in, in, in, in, out, out) is det.

goedel_replace_eqv_type_pred([], _Name, _Args, _Body, Found, [], Found).
goedel_replace_eqv_type_pred([TM0|TMs0], Name, Args, Body, Found0,
				[TM|TMs], Found) :-
	goedel_replace_eqv_type_tm(TM0, Name, Args, Body, Found0, TM, Found1),
	goedel_replace_eqv_type_pred(TMs0, Name, Args, Body, Found1,
					TMs, Found).
:- pred goedel_replace_eqv_type_tm(type_and_mode, string, list(type_param),
				type, bool, type_and_mode, bool).
:- mode goedel_replace_eqv_type_tm(in, in, in, in, in, out, out) is det.

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

unqualify_name(unqualified(PredName), PredName).
unqualify_name(qualified(_ModuleName, PredName), PredName).

sym_name_get_module_name(unqualified(_), ModuleName, ModuleName).
sym_name_get_module_name(qualified(ModuleName, _PredName), _, ModuleName).

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

:- pred split_types_and_modes_2(list(type_and_mode), bool,
				list(type), list(mode), bool).
:- mode split_types_and_modes_2(in, in, out, out, out) is det.

	% T = type, M = mode, TM = combined type and mode
split_types_and_modes_2([], Result, [], [], Result).
split_types_and_modes_2([TM|TMs], Result0, [T|Ts], [M|Ms], Result) :-
	split_type_and_mode(TM, Result0, T, M, Result1),
	split_types_and_modes_2(TMs, Result1, Ts, Ms, Result).

	% if a pred declaration specifies modes for some but
	% not all of the arguments, then the modes are ignored
	% - should this be an error instead?

:- pred split_type_and_mode(type_and_mode, bool, type, mode, bool).
:- mode split_type_and_mode(in, in, out, out, out) is det.

split_type_and_mode(type_only(T), _, T, (free -> free), no).
split_type_and_mode(type_and_mode(T,M), R, T, M, R).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
