%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: type_util.nl.
% Main author: fjh.

% This file provides some utility predicates which operate on types.
% It is used by various stages of the compilation after type-checking,
% include the mode checker and the code generator.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module type_util.
:- interface.
:- import_module prog_io, hlds.

%-----------------------------------------------------------------------------%

	% Succeed iff type is an "atomic" type - one which can be
	% unified using a simple_unify (register comparison) rather
	% than a complicated_unify.

:- pred type_is_atomic(type, module_info).
:- mode type_is_atomic(in, in) is semidet.

%-----------------------------------------------------------------------------%

	% Given a type, determine what sort of type it is.

:- pred classify_type(type, module_info, builtin_type).
:- mode classify_type(in, in, out) is det.

:- type builtin_type	--->	inttype
			;	chartype
			;	strtype
			;	enumtype
			;	usertype(type).

%-----------------------------------------------------------------------------%

	% Given a non-variable type, return it's type-id and argument types.

:- pred type_to_type_id(type, type_id, list(type)).
:- mode type_to_type_id(in, out, out) is det.

	% Given a constant and an arity, return a type_id.
	% Fails if the constant is not an atom.

:- pred make_type_id(const, int, type_id).
:- mode make_type_id(in, in, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, term, require, map, std_util.

type_is_atomic(Type, ModuleInfo) :-
	classify_type(Type, ModuleInfo, BuiltinType),
		% XXX we can't use \= until the compiler handles
		% scopes for \= properly.
	\+ BuiltinType = usertype(_).

%-----------------------------------------------------------------------------%

	% Given a type, determine what sort of type it is.

classify_type(VarType, ModuleInfo, Type) :-
	(
		VarType = term__functor(term__atom("character"), [], _)
	->
		Type = chartype
	;
		VarType = term__functor(term__atom("int"), [], _)
	->
		Type = inttype
	;
		VarType = term__functor(term__atom("string"), [], _)
	->
		Type = strtype
	;
		type_is_enumeration(VarType, ModuleInfo)
	->
		Type = enumtype
	;
		Type = usertype(VarType)
	).

:- pred type_is_enumeration(type, module_info).
:- mode type_is_enumeration(in, in) is semidet.

type_is_enumeration(Type, ModuleInfo) :-
	type_to_type_id(Type, TypeId, _),
	module_info_types(ModuleInfo, TypeDefnTable),
	map__lookup(TypeDefnTable, TypeId, TypeDefn),
	TypeDefn = hlds__type_defn(_, _, TypeBody, _, _),
	TypeBody = du_type(_, _, IsEnum),
	IsEnum = yes.

type_to_type_id(term__functor(Name, Args, _), TypeId, Args) :-
	list__length(Args, Arity),
	make_type_id(Name, Arity, TypeId).

%-----------------------------------------------------------------------------%

	% Given a constant and an arity, return a type_id.
	% This really ought to take a name and an arity -
	% use of integers/floats/strings as type names should
	% be rejected by the parser in prog_io.nl, not in undef_types.nl.

make_type_id(term__atom(Name), Arity, unqualified(Name) - Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
