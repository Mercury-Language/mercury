%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: type_util.nl.
% Main author: fjh.

% This file provides some utility predicates which operate on types.
% It is used by various stages of the compilation after type-checking,
% include the mode checker and the code generator.

% XXX TODO: implement type_is_enumeration.

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

	% Given a constant and an arity, return a type_id.

:- pred make_type_id(const, int, type_id).
:- mode make_type_id(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

type_is_atomic(Type, ModuleInfo) :-
	classify_type(Type, ModuleInfo, BuiltinType),
	BuiltinType \= usertype(_).

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
	type_to_type_id(Type, TypeId),
	module_info_types(ModuleInfo, TypeDefnTable),
	map__lookup(TypeDefnTable, TypeId, TypeDefn),
	TypeDefn = hlds__type_defn(_, _, TypeBody, _, _),
	TypeBody = du_type(Constructors),
	constructors_are_all_constants(Constructors).

:- pred constructors_are_all_constants(list(constructor)).
:- mode constructors_are_all_constants(in) is semidet.

constructors_are_all_constants([]).
constructors_are_all_constants([_Name - ArgTypes | Constructors]) :-
	ArgTypes = [],
	constructors_are_all_constants(Constructors).

:- pred type_to_type_id(type, type_id).
:- mode type_to_type_id(in, out) is det.

type_to_type_id(term__functor(Name, Args, _), TypeId) :-
	length(Args, Arity),
	make_type_id(Name, Arity, TypeId).

%-----------------------------------------------------------------------------%

	% Given a constant and an arity, return a type_id.
	% XXX this should take a name and an arity;
	% use of integers/floats/strings as type names should
	% be rejected by the parser in prog_io.nl, not here.

make_type_id(term__atom(Name), Arity, unqualified(Name) - Arity).
make_type_id(term__integer(_), _, unqualified("<error>") - 0) :-
	error("atom expected").
make_type_id(term__float(_), _, unqualified("<error>") - 0) :-
	error("atom expected").
make_type_id(term__string(_), _, unqualified("<error>") - 0) :-
	error("atom expected").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
