%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates the LLDS code that defines global constants
% to hold the base_type_layout structures of the types defined by the
% current module.
%
% It requires that type_infos are generated using the
% shared-one-of-two-cells option. This layout structures will
% not be generated if this option is not specified.
%
% These global constants are needed only with when we are using accurate
% garbage collection. It is up to the caller to check this. 
% (When using other garbage collectors, defining these global constants 
% is harmless except for adding to compilation time and executable size.)
%
% The tables generated have a number of `create' rvals within them,
% these are removed by llds_common.m to create static structures.
%
% Author: trd.
%
%---------------------------------------------------------------------------%
%
% NOTE: If the representation of base_type_layouts changes, the
% following modules must also be examined to see whether they need to
% be updated. 
%
% library:	std_util.m		- functor, arg, expand,
% 					  solutions
% 		uniq_array.m		- uniq_array type
% 		io.m			- io__stream type
% 		mercury_builtin.m	- builtin types
%
% runtime:	type_info.h		- defines layout macros
% 		deep_copy.{c,h}		- deep_copy
% 		
% Any module that uses base_type_layouts should register itself here.
% Changes can by minimized by using the macros in type_info.h.
%
%---------------------------------------------------------------------------%
% Data representation: Layout Tables
%
% Arrays are created, designed to allow indexing from actual data words.
%
% First index on tag:
%
% Tag 0 - 	CONST  Word = type of constant
%
% CONST: No further indexing required, look at word in layout 
%        to find what sort of constant is here. The data word contains
%        a representation of this sort of constant.
%
% Tag 0 - 	CONST   Word = 0	- unassigned
% Tag 0 - 	CONST   Word = 1	- unused tag
% Tag 0 - 	CONST   Word = 2	- string
% Tag 0 - 	CONST   Word = 3	- float
% Tag 0 - 	CONST   Word = 4	- int
% Tag 0 - 	CONST   Word = 5	- character
% Tag 0 - 	CONST   Word = 6	- univ
% Tag 0 - 	CONST   Word = 7	- pred
% 			Words 8 - 1024 reserved for future use
% Tag 0 - 	CONST   Word = 1024+	- constant(s) 
% 					  word is pointer to 
% 					      - 1 or 0 (enumeration or not)
% 					      - S, the number of constants
%	 					sharing this tag
%					      - S strings - functor names
%
% Note that tag 0 value 0 is presently unassigned. This may be used
% in future for some common purpose.
%
% Tag 1 - 	SIMPLE  Word = pointer to argument vector
%
% SIMPLE: Argument vector contains N, then N pointers to pseudo-typeinfos,
% 	  finally, a pointer to a string containing the name of this
% 	  functor.
%         No further indexing is required. The data word points to a
%         vector of its argument data.
%
% Tag 2 - 	COMPLIC Word = pointer to multiple argument vector
%
% COMPLIC: Multi-argument vector contains F, the number of sharing
% 	   functors, then F pointers to argument vectors. Data word
% 	   contains a pointer to a secondary tag word, then argument
% 	   data. Use the secondary tag to index into the F argument
% 	   pointers, which point to an argument vector just as in
% 	   SIMPLE above. 
%
% Tag 3 - 	VAR/EQ  Word = type variable number, or pointer to 
% 		 		indicator, equivalent base_type_info
% 		 		and maybe functor.
%
% VAR/EQ:  There are 3 cases covered by this tag, all of them forms
% 	   of equivalences.
% 	   If under 1024, the rest of the word is a type variable number,
% 	   that is, the polymophic argument number (starting at 1) of
% 	   the type. Substitute that variable, and you have the type
% 	   this type is equivalent to.
%
% 	   If over 1024, it's just a pointer to a vector, containing
% 	   	- an indicator whether this is a no_tag or not
% 	   	- a pseudo-typeinfo 
% 	   	- possibly a string constant - the name of the 
% 	   	  functor if it is a no_tag.
%	   If the indicator says it isn't a no_tag, the pseudo-typeinfo
%	   is the type this type is equivalent to. There is no string
%	   constant.
%	   If the indicator says it is a no_tag, then the string
%	   contains the functor, and the pseudo-typeinfo is the
%	   type of the argument. Except for the functor, this is much 
%	   the same as an equivalence - the data word has a type
%	   given by the pseudo-typeinfo (it might be worth taking
%	   advantage of the fact that the vector for a no_tag type
%	   is exactly the same vector that a simple tag type would
%	   have - one argument, type of argument, functor).
%
% 	   In any case, you need to look at the equivalent type
% 	   to find out what the data word represents.
%
% Argument vector 	- arity, then pointers to pseudo-typeinfos.
% Multi-Argument vector - number of functors sharing this tag, then
% 			  pointers to argument vectors.
% Pseudo-Typeinfo	- same as a typeinfo (see polymorphism.m) but
% 	may also store free type variables, represented as integers.
% 	We reserve the first 1024 positive integers for type variables,
% 	as this is often not addressable memory, so cannot be used for
% 	pointers. On architectures where this is possible to address, we
% 	should protect the low page of memory.
%
% If there are less than 4 tags available, we don't use tags, we use
% a two word representation, where the first word is the tag value,
% and the second is the contents of the rest of the word. 
%
%---------------------------------------------------------------------------%

:- module base_type_layout.

:- interface.

:- import_module hlds_module.

:- pred base_type_layout__generate_hlds(module_info, module_info).
:- mode base_type_layout__generate_hlds(in, out) is det.

:- pred base_type_layout__generate_llds(module_info, list(c_module)).
:- mode base_type_layout__generate_llds(in, out) is det.

:- implementation.

:- import_module prog_data, hlds_data, hlds_pred, hlds_out, type_util.
:- import_module llds, code_util, globals, options, special_pred, prog_util.
:- import_module assoc_list, bool, string, int, list, map, std_util, require.

:- type layout_info 	--->	
	layout_info(
		string,		% module name
		cons_table,	% ctor table
		int,		% number of tags available
		int,		% next available label 
		type_id,	% type_id of type being currently examined
		list(c_module)	% generated data
	).

:- type tag_category	--->	simple 		% tagged pointer
			; 	complicated 	% shared tagged pointer
			;	comp_const	% shared constants
			; 	no_tag 		% special case of equivalence 
			; 	unused.		% unused tag

%---------------------------------------------------------------------------%

	% Generate a list of base_gen_layouts from the HLDS for later
	% use.

base_type_layout__generate_hlds(ModuleInfo0, ModuleInfo) :-
	module_info_name(ModuleInfo0, ModuleName),
	module_info_types(ModuleInfo0, TypeTable),
	map__keys(TypeTable, TypeIds),
	base_type_layout__gen_base_gen_layouts(TypeIds, TypeTable, ModuleName,
		ModuleInfo0, BaseGenInfos),
	module_info_set_base_gen_layouts(ModuleInfo0, BaseGenInfos,
		ModuleInfo).

	% Given a list of the ids of all the types in the type table,
	% find the types defined in this module, and return a
	% base_gen_layout for each.

:- pred base_type_layout__gen_base_gen_layouts(list(type_id), type_table, 
	string, module_info, list(base_gen_layout)).
:- mode base_type_layout__gen_base_gen_layouts(in, in, in, in, out) is det.

base_type_layout__gen_base_gen_layouts([], _, _, _, []).
base_type_layout__gen_base_gen_layouts([TypeId | TypeIds], TypeTable, 
		ModuleName, ModuleInfo, BaseGenInfos) :-
	base_type_layout__gen_base_gen_layouts(TypeIds, TypeTable, ModuleName,
		ModuleInfo, BaseGenInfos1),
	TypeId = SymName - TypeArity,
	(
		% Is this type defined in this module? 
			
		SymName = qualified(TypeModuleName, TypeName),
		( TypeModuleName = ModuleName ->
			map__lookup(TypeTable, TypeId, TypeDefn),
			hlds_data__get_type_defn_status(TypeDefn, Status),

			Info = base_gen_layout(TypeId, ModuleName, TypeName,
				TypeArity, Status, TypeDefn),
			
			BaseGenInfos = [Info | BaseGenInfos1]
		;
			BaseGenInfos = BaseGenInfos1
		)
	;
		% unqualified types should not occur here

		SymName = unqualified(TypeName),
		string__append_list(["unqualified type ", TypeName,
			"found in base_type_layout"], Msg),
		error(Msg)
	).

%---------------------------------------------------------------------------%

	% Initialize the LayoutInfo, and begin processing BaseGenInfos.
base_type_layout__generate_llds(ModuleInfo, CModules) :-
	module_info_base_gen_layouts(ModuleInfo, BaseGenInfos),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_int_option(Globals, num_tag_bits, NumTagBits),
	int__pow(2, NumTagBits, MaxTags),
	module_info_name(ModuleInfo, ModuleName),
	module_info_ctors(ModuleInfo, ConsTable),
	LayoutInfo0 = layout_info(ModuleName, ConsTable, MaxTags, 0, 
		unqualified("") - 0, []),
	base_type_layout__construct_base_type_layouts(BaseGenInfos, 
		LayoutInfo0, LayoutInfo),
	LayoutInfo = layout_info(_, _, _, _, _, CModules).


	% For each type, generate the required CModule
	
:- pred base_type_layout__construct_base_type_layouts(list(base_gen_layout),
	layout_info, layout_info).
:- mode base_type_layout__construct_base_type_layouts(in, in, out) is det.

base_type_layout__construct_base_type_layouts([], LayoutInfo, LayoutInfo).
base_type_layout__construct_base_type_layouts([BaseGenInfo | BaseGenInfos],
		LayoutInfo0, LayoutInfo) :-
	BaseGenInfo = base_gen_layout(TypeId, ModuleName, TypeName, TypeArity,
		Status, HldsType),
	base_type_layout__set_type_id(LayoutInfo0, TypeId, LayoutInfo1),
	hlds_data__get_type_defn_body(HldsType, TypeBody),
	(
		TypeBody = uu_type(_Alts),
		error("base_type_layout: undiscriminated union unimplemented\n")
	;
		TypeBody = eqv_type(Type),
		base_type_layout__construct_eqv_type(Type, LayoutInfo1, 
			LayoutInfo2, TypeData)
	;
		TypeBody = abstract_type,
		LayoutInfo2 = LayoutInfo1,
		TypeData = []
	;
		TypeBody = du_type(Ctors, ConsTagMap, Enum),

			% sort list on tags, so that 
			% enums, complicated constants and
			% complicated tags have their shared
			% functors in the right order.
		map__to_assoc_list(ConsTagMap, ConsTags0),
		assoc_list__reverse_members(ConsTags0, RevConsList),
		list__sort(RevConsList, SortedRevConsList), 
		assoc_list__reverse_members(SortedRevConsList, ConsTags),
		(
			Enum = yes,
			base_type_layout__construct_enum(ConsTags,
				LayoutInfo1, LayoutInfo2, TypeData)
		;
			Enum = no,
			( 
				type_is_no_tag_type(Ctors, Name, TypeArg)
			->
				base_type_layout__construct_no_tag_type(Name,
					TypeArg, LayoutInfo1, LayoutInfo2,
					TypeData)
			;
				base_type_layout__construct_du_type(ConsTags, 
					LayoutInfo1, LayoutInfo2, TypeData)
			)
		)
	),	
	(
		( Status = exported ; Status = abstract_exported )
	->
		Exported = yes
	;
		Exported = no
	),

		% pure abstract types have no layout definition.
	( 
		TypeData = []
	->
		LayoutInfo3 = LayoutInfo2
	;
		CModule = c_data(ModuleName, base_type_layout(TypeName, 
			TypeArity), Exported, TypeData, []),
		base_type_layout__add_cmodule(LayoutInfo2, CModule, 
			LayoutInfo3)
	),
	base_type_layout__construct_base_type_layouts(BaseGenInfos, LayoutInfo3,
		LayoutInfo).


%---------------------------------------------------------------------------%

	% Constants - these should be kept in check with the runtime
	% definition.

:- type const_sort 	--->	unassigned
			;	unused
			;	string
			;	float
			;	int
			;	character
			;	univ
			;	predicate.

:- pred base_type_layout__const_value(const_sort::in, int::out) is det.
base_type_layout__const_value(unassigned, 0).
base_type_layout__const_value(unused, 1).
base_type_layout__const_value(string, 2).
base_type_layout__const_value(float, 3).
base_type_layout__const_value(int, 4).
base_type_layout__const_value(character, 5).
base_type_layout__const_value(univ, 6).
base_type_layout__const_value(predicate, 7).

	% The value we use to indicate whether a type is an no_tag type
	
:- pred base_type_layout__no_tag_indicator(bool::in, int::out) is det.
base_type_layout__no_tag_indicator(no, 0).
base_type_layout__no_tag_indicator(yes, 1).

	% The value we use to indicate whether a type is an enum
	
:- pred base_type_layout__enum_indicator(bool::in, int::out) is det.
base_type_layout__enum_indicator(no, 0).
base_type_layout__enum_indicator(yes, 1).

	% Maximum value of a integer representation of a variable.
	
:- pred base_type_layout__max_varint(int::out) is det.
base_type_layout__max_varint(1024).

	% Tag values
	
:- pred base_type_layout__tag_value(tag_category::in, int::out) is det.
base_type_layout__tag_value(comp_const, 0).
base_type_layout__tag_value(simple, 1).
base_type_layout__tag_value(complicated, 2).
base_type_layout__tag_value(no_tag, 3).
base_type_layout__tag_value(unused, 0).

:- pred base_type_layout__tag_value_const(int::out) is det.
base_type_layout__tag_value_const(0).

:- pred base_type_layout__tag_value_equiv(int::out) is det.
base_type_layout__tag_value_equiv(3).

%---------------------------------------------------------------------------%

	% Encoding

	% If we don't have enough tags, encode mkword using two words.
	% Tag word and value word.
	
:- pred base_type_layout__encode_mkword(layout_info, int, rval, 
		list(maybe(rval))).
:- mode base_type_layout__encode_mkword(in, in, in, out) is det.
base_type_layout__encode_mkword(LayoutInfo, Tag, Rval, Rvals) :-
	base_type_layout__get_max_tags(LayoutInfo, MaxTags),
	(
		MaxTags < 4
	->
		Rvals = [yes(const(int_const(Tag))), yes(Rval)]
	;
		Rvals = [yes(mkword(Tag, unop(mkbody, Rval)))]
	).

	% If we don't have enough tags, encode create using two words, one
	% containing the tag and one containing the pointer.
	
:- pred base_type_layout__encode_create(layout_info, int, list(maybe(rval)),
		bool, int, list(maybe(rval))).
:- mode base_type_layout__encode_create(in, in, in, in, in, out) is det.
base_type_layout__encode_create(LayoutInfo, Tag, Rvals0, Unique, Label, 
		Rvals) :-
	base_type_layout__get_max_tags(LayoutInfo, MaxTags),
	(
		MaxTags < 4
	->
		Rvals = [yes(const(int_const(Tag))), 
			yes(create(0, Rvals0, Unique, Label))]
	;
		Rvals = [yes(create(Tag, Rvals0, Unique, Label))]
	).

%---------------------------------------------------------------------------%

	% If the type is reduced to some sort of constant or special,
	% handle it seperately.
	
:- pred base_type_layout__construct_special_tagged(pair(cons_id, cons_tag), 
	layout_info, int, list(maybe(rval))).
:- mode base_type_layout__construct_special_tagged(in, in, in, out) is semidet.
base_type_layout__construct_special_tagged(_ConsId - ConsTag, LayoutInfo, 
		MaxTags, Rvals) :-
	base_type_layout__tag_value_const(Tag),
	(
		ConsTag = string_constant(_),
		base_type_layout__const_value(string, Value),
		base_type_layout__encode_mkword(LayoutInfo, Tag, 
			const(int_const(Value)), Rval)
	;
		ConsTag = float_constant(_),
		base_type_layout__const_value(float, Value),
		base_type_layout__encode_mkword(LayoutInfo, Tag, 
			const(int_const(Value)), Rval)
	;
		ConsTag = int_constant(_),
		base_type_layout__const_value(int, Value),
		base_type_layout__encode_mkword(LayoutInfo, Tag, 
			const(int_const(Value)), Rval)
	;
		ConsTag = pred_closure_tag(_, _),
		error("base_type_layout: Unexpected tag - pred_closure_tag/2")
	;
		ConsTag = code_addr_constant(_, _),
		error("base_type_layout: Unexpected constant - code_addr_constant/2")
	;
		ConsTag = base_type_info_constant(_, _, _),
		error("base_type_layout: Unexpected constant - base_type_into_constant/3")
	),
	list__duplicate(MaxTags, Rval, RvalsList),
	list__condense(RvalsList, Rvals).


	% For enumerations:
	%
	% tag is 0, rest of word is pointer to 
	% 	- enum indicator
	% 	- S, the number of constants in this enum
	% 	- S strings of constant names
	
:- pred base_type_layout__construct_enum(assoc_list(cons_id, cons_tag),
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__construct_enum(in, in, out, out) is det.
base_type_layout__construct_enum(ConsList, LayoutInfo0, LayoutInfo, Rvals) :-
	list__map(
		lambda([ConsId::in, CtorRval::out] is det, (
		    ( ConsId = cons(SymName, _Arity) - _ConsTag ->
			unqualify_name(SymName, CtorName),
			CtorRval = yes(const(string_const(CtorName)))
		    ;
			error("base_type_layout: constant has no constructor")
		    )
		)),
		ConsList, CtorNameRvals),

	base_type_layout__enum_indicator(yes, EnumIndicator),
	Rval0 = yes(const(int_const(EnumIndicator))),

	list__length(ConsList, NumCtors),
	Rval1 = yes(const(int_const(NumCtors))),

	base_type_layout__get_next_label(LayoutInfo0, NextLabel),
	base_type_layout__incr_next_label(LayoutInfo0, LayoutInfo),
	base_type_layout__tag_value_const(Tag),
	base_type_layout__encode_create(LayoutInfo, Tag, 
		[Rval0, Rval1 | CtorNameRvals], no, NextLabel, Rval),
	base_type_layout__get_max_tags(LayoutInfo, MaxTags),
	list__duplicate(MaxTags, Rval, RvalList),
	list__condense(RvalList, Rvals).
	
	% For no_tag types:
	%
	% Tag is 3, rest of word is pointer to vector of
	% no_tag indicator, pseudo_type_info and functor name.

:- pred base_type_layout__construct_no_tag_type(sym_name, type, layout_info, 
		layout_info, list(maybe(rval))).
:- mode base_type_layout__construct_no_tag_type(in, in, in, out, out) is det.
base_type_layout__construct_no_tag_type(SymName, Type, LayoutInfo0, 
		LayoutInfo, Rvals) :-

		% indicator of tag_type
	base_type_layout__no_tag_indicator(yes, NoTagIndicator),
	Rval0 = yes(const(int_const(NoTagIndicator))),

		% generate pseudo_type_info
	base_type_layout__generate_pseudo_type_info(Type, LayoutInfo0, 
		LayoutInfo1, Rval1),

		% functor name
	unqualify_name(SymName, Name),
	Rval2 = yes(const(string_const(Name))),

	base_type_layout__get_next_label(LayoutInfo1, NextLabel),
	base_type_layout__incr_next_label(LayoutInfo1, LayoutInfo),
	base_type_layout__tag_value_equiv(Tag),

	base_type_layout__encode_create(LayoutInfo, Tag, 
			[Rval0, Rval1, Rval2], no, NextLabel, Rval),

	base_type_layout__get_max_tags(LayoutInfo, MaxTags),
	list__duplicate(MaxTags, Rval, RvalsList),
	list__condense(RvalsList, Rvals).


	% For equivalences:
	%
	% Tag is 3, rest of word is pointer to vector or
	% variable number

:- pred base_type_layout__construct_eqv_type(type, layout_info, 
		layout_info, list(maybe(rval))).
:- mode base_type_layout__construct_eqv_type(in, in, out, out) is det.
base_type_layout__construct_eqv_type(Type, LayoutInfo0, LayoutInfo, Rvals) :-

		% generate rest of word, remove a level of creates
	base_type_layout__generate_pseudo_type_info(Type, LayoutInfo0, 
		LayoutInfo1, Rval0),
	base_type_layout__tag_value_equiv(Tag),
	( 
		% If it was a constant (a type variable), then tag it
		
		Rval0 = yes(const(Const))
	->
		base_type_layout__encode_mkword(LayoutInfo, Tag, 
				const(Const), Rval),
		LayoutInfo = LayoutInfo1
	;
		% Otherwise, package it into a vector

		base_type_layout__no_tag_indicator(no, NoTagIndicator),
		IndicatorRval = yes(const(int_const(NoTagIndicator))),
		
		base_type_layout__get_next_label(LayoutInfo1, NextLabel),
		base_type_layout__incr_next_label(LayoutInfo1, LayoutInfo),
		base_type_layout__encode_create(LayoutInfo, Tag, 
			[IndicatorRval, Rval0], no, NextLabel, Rval)
	),
	base_type_layout__get_max_tags(LayoutInfo, MaxTags),
	list__duplicate(MaxTags, Rval, RvalsList),
	list__condense(RvalsList, Rvals).


	% For discriminated unions:
	%
	% Mixture of simple, complicated and complicated_constant
	% tags. For each primary tag value, we have a word that
	% describes what it represents. The list of words will
	% form an array that can be indexed by primary tag.
	
:- pred base_type_layout__construct_du_type(assoc_list(cons_id, cons_tag), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__construct_du_type(in, in, out, out) is det.
base_type_layout__construct_du_type([], _, _, []) :-
	error("base_type_layout: type with no cons_tag information").
base_type_layout__construct_du_type(ConsList, LayoutInfo0, LayoutInfo, Rvals) :-
	ConsList = [ConsPair | _],
	base_type_layout__get_max_tags(LayoutInfo0, MaxTags),
	(
		base_type_layout__construct_special_tagged(ConsPair, 
			LayoutInfo0, MaxTags, Rvals0)
	->
		LayoutInfo0 = LayoutInfo,
		Rvals = Rvals0
	;
		MaxTagValue is MaxTags - 1,
		base_type_layout__gather_tags(ConsList, MaxTagValue, TagGroups),
		base_type_layout__generate_rvals(TagGroups, LayoutInfo0, 
			LayoutInfo, [], Rvals)
	).


:- pred base_type_layout__generate_rvals(list(pair(tag_category, 
	list(pair(cons_id, cons_tag)))), layout_info, layout_info, 
	list(maybe(rval)), list(maybe(rval))).
:- mode base_type_layout__generate_rvals(in, in, out, in, out) is det.

base_type_layout__generate_rvals([], LayoutInfo, LayoutInfo, Rvals, Rvals).
base_type_layout__generate_rvals([Tag - ConsList | Rest], LayoutInfo0,
		LayoutInfo, Rvals0, Rvals) :-
	(
		Tag = simple,
		base_type_layout__handle_simple(ConsList, LayoutInfo0, 
			LayoutInfo1, Rval),
		list__append(Rval, Rvals0, Rvals1)
	;
		Tag = complicated,
		base_type_layout__handle_complicated(ConsList, LayoutInfo0, 
			LayoutInfo1, Rval),
		list__append(Rval, Rvals0, Rvals1)

	;
		Tag = comp_const,
		base_type_layout__handle_comp_const(ConsList, LayoutInfo0, 
			LayoutInfo1, Rval),
		list__append(Rval, Rvals0, Rvals1)
	;
		Tag = no_tag,
		error("base_type_layout: unexpected no_tag")
	;
		Tag = unused,
		LayoutInfo1 = LayoutInfo0,
		base_type_layout__const_value(unused, Value),
		base_type_layout__tag_value(unused, TagValue),
		base_type_layout__encode_mkword(LayoutInfo1, TagValue, 
			const(int_const(Value)), Rval),
		list__append(Rval, Rvals0, Rvals1)
	),
	base_type_layout__generate_rvals(Rest, LayoutInfo1, LayoutInfo, 
		Rvals1, Rvals).


	% For complicated constants:
	%
	% tag is 0, rest of word is pointer to 
	% 	- enum indicator
	% 	- S, the number of constants sharing this tag 
	% 	- S strings of constant names
	

:- pred base_type_layout__handle_comp_const(list(pair(cons_id, cons_tag)), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__handle_comp_const(in, in, out, out) is det.

base_type_layout__handle_comp_const([], _, _, _) :-
	error("base_type_layout: no constructors for complicated constant tag").
base_type_layout__handle_comp_const([C | Cs], LayoutInfo0, LayoutInfo, Rval) :-
	list__length([C | Cs], NumCtors), 		% Number of sharers
	Rval1 = yes(const(int_const(NumCtors))),

	base_type_layout__enum_indicator(no, EnumIndicator),
	Rval0 = yes(const(int_const(EnumIndicator))),

	list__map(
	    lambda([ConsId::in, CtorRval::out] is det, (
		( ConsId = cons(SymName, _Arity) - _ConsTag ->
			unqualify_name(SymName, CtorName),
			CtorRval = yes(const(string_const(CtorName)))
		;
			error("base_type_layout: constant has no constructor")
		))),
	    [C | Cs], CtorNameRvals),

	base_type_layout__get_next_label(LayoutInfo0, NextLabel),
	base_type_layout__incr_next_label(LayoutInfo0, LayoutInfo),
	base_type_layout__tag_value(comp_const, Tag),
	base_type_layout__encode_create(LayoutInfo, Tag, 
		[Rval0, Rval1 | CtorNameRvals], no, NextLabel, Rval).


	% For simple tags:
	%
	% Tag 1, with a pointer to an array containing:
	%	N - the arity of this functor 
	%	N argument pseudo-typeinfos 
	%	- a string constant (the name of the functor)

:- pred base_type_layout__handle_simple(list(pair(cons_id, cons_tag)), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__handle_simple(in, in, out, out) is det.

base_type_layout__handle_simple([], _, _, _) :-
	error("base_type_layout: no constructors for simple tag").
base_type_layout__handle_simple([ConsId - _ConsTag | _], LayoutInfo0, 
		LayoutInfo, Rval) :-
	( 
		ConsId = cons(SymName, _Arity)
	->
		unqualify_name(SymName, ConsString)
	;
		error("base_type_layout: simple tag with no constructor")
	),
	base_type_layout__get_cons_args(LayoutInfo0, ConsId, ConsArgs),
	base_type_layout__get_next_label(LayoutInfo0, NextLabel),
	base_type_layout__incr_next_label(LayoutInfo0, LayoutInfo1),
	list__length(ConsArgs, NumArgs),
	base_type_layout__generate_pseudo_type_infos(ConsArgs, 
		LayoutInfo1, LayoutInfo, PseudoTypeInfos),
	list__append(PseudoTypeInfos, 
		[yes(const(string_const(ConsString)))], EndRvals),
	base_type_layout__tag_value(simple, Tag),
	base_type_layout__encode_create(LayoutInfo, Tag, 
		[yes(const(int_const(NumArgs))) | EndRvals], no, NextLabel, 
		Rval).

	% For complicated tags:
	%
	% Tag 2, with a pointer to an array containing:
	% 	F - the number of functors sharing this tag
	% 	F pointers to arrays containing:
	%		N - the arity of this functor
	%		N argument pseudo-typeinfos

:- pred base_type_layout__handle_complicated(list(pair(cons_id, cons_tag)), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__handle_complicated(in, in, out, out) is det.

base_type_layout__handle_complicated([], _, _, _) :-
	error("base_type_layout: no constructors for complicated tag").
base_type_layout__handle_complicated([C | Cs], LayoutInfo0, LayoutInfo, Rval) :-
	base_type_layout__get_next_label(LayoutInfo0, NextLabel),
	base_type_layout__incr_next_label(LayoutInfo0, LayoutInfo1),

		% Number of sharers
	list__length([C | Cs], NumCtors),
	NumSharersRval = yes(const(int_const(NumCtors))),

		% Create rvals for sharers
		% (just like a lot of simples)
	list__foldr(
		lambda([Cons::in, Acc::in, NewAcc::out] is det, (
			Acc = Rvals0 - LayoutInfoA,
			base_type_layout__handle_simple([Cons], LayoutInfoA,
				LayoutInfoB, Rval1),
			list__append(Rval1, Rvals0, Rvals1),
			NewAcc = Rvals1 - LayoutInfoB)),
		[C | Cs],
		[] - LayoutInfo1, 
		SharedRvals - LayoutInfo),

	base_type_layout__tag_value(complicated, Tag),
	base_type_layout__encode_create(LayoutInfo, Tag, 
		[NumSharersRval | SharedRvals], no, NextLabel, Rval).


%---------------------------------------------------------------------------%

	% Generate some pseudo-typeinfos as create() rvals.
	%
	% Pseudo-typeinfos are just like typeinfos, but can also
	% store type variables. We store type variables as integers,
	% which will always have low values.
	
:- pred base_type_layout__generate_pseudo_type_infos(list(type), layout_info,
	layout_info, list(maybe(rval))).
:- mode base_type_layout__generate_pseudo_type_infos(in, in, out, out) is det.

base_type_layout__generate_pseudo_type_infos([], LayoutInfo, LayoutInfo, []).
base_type_layout__generate_pseudo_type_infos([Type | Types], LayoutInfo0, 
		LayoutInfo, [PseudoTypeInfo | PseudoTypeInfos]) :-
	base_type_layout__generate_pseudo_type_info(Type, LayoutInfo0, 
		LayoutInfo1, PseudoTypeInfo),
	base_type_layout__generate_pseudo_type_infos(Types, LayoutInfo1, 
		LayoutInfo, PseudoTypeInfos).

	% generate a single pseudo-typeinfo as a create() rval.
	
:- pred base_type_layout__generate_pseudo_type_info(type, layout_info,
	layout_info, maybe(rval)).
:- mode base_type_layout__generate_pseudo_type_info(in, in, out, out) is det.

base_type_layout__generate_pseudo_type_info(Type, LayoutInfo0, LayoutInfo,
		Pseudo) :-
	(
		type_to_type_id(Type, TypeId, TypeArgs)
	->
		( 
			% XXX higher order types are tricky. For the moment,
			% we'll just make them refer to the defined pred_0
			% base_type_info, with argument according to
			% their types. The arity will need to be
			% obtained from elsewhere - for most
			% applications the closure contains the number
			% of arguments present in the closure, which
			% is enough to copy them.

			type_is_higher_order(Type, _PredFunc, _TypeArgs)
		->
			TypeModule = "",
			TypeName = "pred",
			Arity = 0,
			TypeId = _QualTypeName - RealArity,
			RealArityArg = [yes(const(int_const(RealArity)))]
		;
			TypeId = QualTypeName - Arity,
			unqualify_name(QualTypeName, TypeName),
			sym_name_get_module_name(QualTypeName, "", TypeModule),
			RealArityArg = []
		),
		base_type_layout__get_next_label(LayoutInfo0, NextLabel),
		base_type_layout__incr_next_label(LayoutInfo0, LayoutInfo1),
		Pseudo0 = yes(const(data_addr_const(data_addr(TypeModule,
			base_type_info(TypeName, Arity))))),

			% generate args, but remove one level of create()s.
		base_type_layout__generate_pseudo_type_infos(TypeArgs,
			LayoutInfo1, LayoutInfo, PseudoArgs0),
		base_type_layout__remove_creates(PseudoArgs0, PseudoArgs1),

		list__append(RealArityArg, PseudoArgs1, PseudoArgs),

		Pseudo = yes(create(0, [Pseudo0 | PseudoArgs], no, 
			NextLabel))
	;
		type_util__var(Type, Var)
	->
		term__var_to_int(Var, VarInt),
		base_type_layout__max_varint(MaxVarInt),
		require(VarInt < MaxVarInt, 
			"base_type_layout: type variable representation exceeds limit"),
		Pseudo = yes(const(int_const(VarInt))),
		LayoutInfo = LayoutInfo0
	;
		error("base_type_layout: type neither var nor non-var")
	).

	% Remove a level of create() from a list of rvals.
	
:- pred base_type_layout__remove_creates(list(maybe(rval)), list(maybe(rval))).
:- mode base_type_layout__remove_creates(in, out) is det.

base_type_layout__remove_creates([], []).
base_type_layout__remove_creates([MaybeRval0 | Rest0], [MaybeRval | Rest]) :-
	base_type_layout__remove_create(MaybeRval0, MaybeRval),
	base_type_layout__remove_creates(Rest0, Rest).

	% Remove a create() from an rval, if present.
	
:- pred base_type_layout__remove_create(maybe(rval), maybe(rval)).
:- mode base_type_layout__remove_create(in, out) is det.

base_type_layout__remove_create(MaybeRval0, MaybeRval) :-
	(
		MaybeRval0 = yes(create(_, [PTI], _, _))
	->
		MaybeRval = PTI
	;
		MaybeRval = MaybeRval0
	).

	% Gather the constructors for each tag into a list, and
	% generate a list of these lists, in reverse order of tag
	% value.

:- pred base_type_layout__gather_tags(list(pair(cons_id, cons_tag)), 
	int, list(pair(tag_category, list(pair(cons_id, cons_tag))))).
:- mode base_type_layout__gather_tags(in, in, out) is det.

base_type_layout__gather_tags(ConsList0, Tag0, TagGroups) :-
	( 
		Tag0 < 0 
	->
		TagGroups = []
	;
		base_type_layout__get_tags(ConsList0, Tag0, TagGroup, TagType),
		list__delete_elems(ConsList0, TagGroup, ConsList),
		Tag is Tag0 - 1,
		base_type_layout__gather_tags(ConsList, Tag, TagGroups0),
		TagGroups = [TagType - TagGroup | TagGroups0]
	).

	% For a given tag, get all (cons_id, cons_tag) pairs with that tag.
	
:- pred base_type_layout__get_tags(list(pair(cons_id, cons_tag)), 
	int, list(pair(cons_id, cons_tag)), tag_category).
:- mode base_type_layout__get_tags(in, in, out, out) is det.

base_type_layout__get_tags([], _Tag, [], unused).
base_type_layout__get_tags([ConsPair | ConsRest], Tag, TagList, TagType) :-
	ConsPair = _ConsId - ConsTag,
	base_type_layout__get_tags(ConsRest, Tag, TagList0, TagType0),
	base_type_layout__tag_type_and_value(ConsTag, Tag1, TagType1),
	(
		Tag1 = Tag
	->
		TagList = [ConsPair | TagList0],
		TagType = TagType1
	;
		TagList = TagList0,
		TagType = TagType0
	).


	% Classify this type of tag, and its numeric tag value.

:- pred base_type_layout__tag_type_and_value(cons_tag, int, tag_category).
:- mode base_type_layout__tag_type_and_value(in, out, out) is det.

base_type_layout__tag_type_and_value(simple_tag(Tag), Tag, simple).
base_type_layout__tag_type_and_value(complicated_tag(Tag, _), Tag, complicated).
base_type_layout__tag_type_and_value(complicated_constant_tag(Tag, _), Tag, 
	comp_const).
base_type_layout__tag_type_and_value(no_tag, -1, no_tag). 
base_type_layout__tag_type_and_value(string_constant(_), -1, unused). 
base_type_layout__tag_type_and_value(float_constant(_), -1, unused). 
base_type_layout__tag_type_and_value(int_constant(_), -1, unused). 
base_type_layout__tag_type_and_value(pred_closure_tag(_, _), -1, unused). 
base_type_layout__tag_type_and_value(code_addr_constant(_, _), -1, unused).
base_type_layout__tag_type_and_value(base_type_info_constant(_, _, _), -1,unused). 

	% Get the arguments of this constructor of the current type.
	
:- pred base_type_layout__get_cons_args(layout_info, cons_id, list(type)).
:- mode base_type_layout__get_cons_args(in, in, out) is det.

base_type_layout__get_cons_args(LayoutInfo, ConsId, TypeArgs) :-
	base_type_layout__get_cons_table(LayoutInfo, ConsTable),
	base_type_layout__get_type_id(LayoutInfo, TypeId),
	(
		map__search(ConsTable, ConsId, HldsConsList),
		list__filter(lambda([X::in] is semidet, (
				X = hlds__cons_defn(_, TypeId, _))),
			HldsConsList,
			[hlds__cons_defn(TypeArgs0, _, _)])
	->
		TypeArgs = TypeArgs0
	;
		error("base_type_layout: no matching cons definition")
	).

%---------------------------------------------------------------------------%

	% access to the base_type_layout data structure.

:- pred base_type_layout__get_module_name(layout_info, string).
:- mode base_type_layout__get_module_name(in, out) is det.
base_type_layout__get_module_name(LayoutInfo, ModuleName) :-
	LayoutInfo = layout_info(ModuleName, _, _, _, _, _).

:- pred base_type_layout__get_cons_table(layout_info, cons_table).
:- mode base_type_layout__get_cons_table(in, out) is det.
base_type_layout__get_cons_table(LayoutInfo, ConsTable) :-
	LayoutInfo = layout_info(_, ConsTable, _, _, _, _).

:- pred base_type_layout__get_max_tags(layout_info, int).
:- mode base_type_layout__get_max_tags(in, out) is det.
base_type_layout__get_max_tags(LayoutInfo, MaxTags) :-
	LayoutInfo = layout_info(_, _, MaxTags, _, _, _).

:- pred base_type_layout__get_next_label(layout_info, int).
:- mode base_type_layout__get_next_label(in, out) is det.
base_type_layout__get_next_label(LayoutInfo, NextLabel) :-
	LayoutInfo = layout_info(_, _, _, NextLabel, _, _).

:- pred base_type_layout__get_type_id(layout_info, type_id).
:- mode base_type_layout__get_type_id(in, out) is det.
base_type_layout__get_type_id(LayoutInfo, TypeId) :-
	LayoutInfo = layout_info(_, _, _, _, TypeId, _).

:- pred base_type_layout__get_cmodules(layout_info, list(c_module)).
:- mode base_type_layout__get_cmodules(in, out) is det.
base_type_layout__get_cmodules(LayoutInfo, CModules) :-
	LayoutInfo = layout_info(_, _, _, _, _, CModules).

:- pred base_type_layout__add_cmodule(layout_info, c_module, layout_info).
:- mode base_type_layout__add_cmodule(in, in, out) is det.
base_type_layout__add_cmodule(LayoutInfo0, CModule, LayoutInfo) :-
	LayoutInfo0 = layout_info(A, B, C, D, E, CModules0),
	CModules = [CModule | CModules0],
	LayoutInfo = layout_info(A, B, C, D, E, CModules).

:- pred base_type_layout__incr_next_label(layout_info, layout_info).
:- mode base_type_layout__incr_next_label(in, out) is det.
base_type_layout__incr_next_label(LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = layout_info(A, B, C, NextLabel0, E, F),
	NextLabel = NextLabel0 + 1,
	LayoutInfo = layout_info(A, B, C, NextLabel, E, F).

:- pred base_type_layout__set_type_id(layout_info, type_id, layout_info).
:- mode base_type_layout__set_type_id(in, in, out) is det.
base_type_layout__set_type_id(LayoutInfo0, TypeId, LayoutInfo) :-
	LayoutInfo0 = layout_info(A, B, C, D, _, F),
	LayoutInfo = layout_info(A, B, C, D, TypeId, F).

