%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% file: hlds_code_util.m.
%
% various utilities routines for use during hlds generation.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds__hlds_code_util.

:- interface.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module parse_tree__prog_data.

	% Are equivalence types fully expanded on this backend?
:- pred are_equivalence_types_expanded(module_info::in) is semidet.

	% Find out how a function symbol (constructor) is represented
	% in the given type.

:- func cons_id_to_tag(cons_id, type, module_info) = cons_tag.

:- implementation.

:- import_module check_hlds__type_util.
:- import_module libs__globals.
:- import_module libs__options.

:- import_module bool, char, string, require, list, map, std_util, term.

%-----------------------------------------------------------------------------%

are_equivalence_types_expanded(ModuleInfo) :-
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, highlevel_data, HighLevelData),
	HighLevelData = yes,
	globals__get_target(Globals, Target),
	( Target = il ; Target = java).

%-----------------------------------------------------------------------------%

cons_id_to_tag(int_const(X), _, _) = int_constant(X).
cons_id_to_tag(float_const(X), _, _) = float_constant(X).
cons_id_to_tag(string_const(X), _, _) = string_constant(X).
cons_id_to_tag(pred_const(P,M,E), _, _) = pred_closure_tag(P,M,E).
cons_id_to_tag(type_ctor_info_const(M,T,A), _, _) =
		type_ctor_info_constant(M,T,A).
cons_id_to_tag(base_typeclass_info_const(M,C,_,N), _, _) =
		base_typeclass_info_constant(M,C,N).
cons_id_to_tag(type_info_cell_constructor(_), _, _) = unshared_tag(0).
cons_id_to_tag(typeclass_info_cell_constructor, _, _) = unshared_tag(0).
cons_id_to_tag(tabling_pointer_const(PredId,ProcId), _, _) =
		tabling_pointer_constant(PredId,ProcId).
cons_id_to_tag(deep_profiling_proc_static(PPId), _, _) =
		deep_profiling_proc_static_tag(PPId).
cons_id_to_tag(table_io_decl(PPId), _, _) = table_io_decl_tag(PPId).
cons_id_to_tag(cons(Name, Arity), Type, ModuleInfo) = Tag :-
	(
			% handle the `character' type specially
		Type = term__functor(term__atom("character"), [], _),
		Name = unqualified(ConsName),
	 	string__char_to_string(Char, ConsName)
	->
		char__to_int(Char, CharCode),
		Tag = int_constant(CharCode)
	;
		% Tuples do not need a tag. Note that unary tuples are not
		% treated as no_tag types. There's no reason why they
		% couldn't be, it's just not worth the effort.
		type_is_tuple(Type, _)
	->
		Tag = single_functor
	;
			% Use the type to determine the type_ctor
		( type_to_ctor_and_args(Type, TypeCtor0, _) ->
			TypeCtor = TypeCtor0
		;
			% the type-checker should ensure that this never happens
			error("cons_id_to_tag: invalid type")
		),
			% Given the type_ctor, lookup up the constructor tag
			% table for that type
		module_info_types(ModuleInfo, TypeTable),
		map__lookup(TypeTable, TypeCtor, TypeDefn),
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		(
			ConsTable0 = TypeBody ^ du_type_cons_tag_values
		->
			ConsTable = ConsTable0
		;
			% this should never happen
			error("cons_id_to_tag: type is not d.u. type?")
		),
			% Finally look up the cons_id in the table
		map__lookup(ConsTable, cons(Name, Arity), Tag)
	).

%-----------------------------------------------------------------------------%
