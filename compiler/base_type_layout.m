%---------------------------------------------------------------------------%
% Copyright (C) 1996-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates the LLDS code that defines global constants
% to hold the `type_ctor_layout' and `type_ctor_functors' structures 
% of the types defined by the current module.
%
% It requires that type_infos are generated using the
% shared-one-or-two-cells option.  The layout structures will
% not be generated if this option is not specified.
%
% These global constants are used by the predicates functor/3, arg/3 and
% expand/4 in std_util.m, and by deep_copy(), which is used by solutions
% in non conservative-gc grades.  They will also be used for accurate
% garbage collection.
%
% The tables generated have a number of `create' rvals within them,
% these are removed by llds_common.m to create static structures.
%
% Author: trd.
%
%---------------------------------------------------------------------------%
%
% NOTE: If the representation of type_ctor_layouts changes, the
% following modules must also be examined to see whether they need to
% be updated. 
%
% library:	std_util.m		- functor, arg, expand,
% 					  solutions
% 		array.m			- array type
% 		io.m			- io__stream type
% 		builtin.m		- builtin types
%
% runtime:	mercury_type_info.h	- defines layout macros
% 		mercury_deep_copy.{c,h}	- deep_copy
%		mercury_table_any.c	- tabling
% 		
% Any module that uses type_ctor_layouts should register itself here.
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
% Tag 0 - 	CONST   Word = 8	- void
% Tag 0 - 	CONST   Word = 9	- array
% Tag 0 - 	CONST   Word = 10	- type_info
% Tag 0 - 	CONST   Word = 11	- c_pointer
% Tag 0 - 	CONST   Word = 12	- type_class_info
% 			Words 13 - 1024 reserved for future use
% Tag 0 - 	CONST   Word = 1024+	- constant(s) 
% 					  word is pointer to enum
% 					  vector.
%
% enum vector:
%	- 1 or 0 (1 = enumeration, 0 = shared local)
%	- S, the number of constants sharing this tag
%	- S strings (functor names)
%
% Note that tag 0 value 0 is presently unassigned. This may be used
% in future for some common case optimization.
%
% Tag 1 - 	UNSHARED  Word = pointer to functor descriptor
%
% UNSHARED: Functor descriptor contains 
% 	  - the arity of the functor (N)
% 	  - N pointers to pseudo-typeinfos (of each argument),
% 	  - a pointer to a string containing the name of this
% 	    functor.
%	  - A word containing a representation of the primary and
%	    secondary tags of this functor
%
%	  Note, this ordering is better for operations such as
%	  deep_copy, which don't require the functor name or the
%	  tags, so they are less likely to be occupying the cache.
%	  For operations that need the functor name and tags, it
%	  is likely they will be associated with I/O, so they will
%	  probably be less sensitive to cache misses.
%
%         No further indexing is required. The data word points to a
%         vector of its argument data.
%
% Tag 2 - 	SHARED Word = pointer to functor descriptor vector
%
% SHARED:  Multi-argument vector contains F, the number of sharing
% 	   functors, then F pointers to functor descriptors.
%	   Data word contains a pointer to a secondary tag word, then
%	   argument data. Use the secondary tag to index into the F
%	   argument pointers, which point to a functors descriptors just
%	   as in UNSHARED above. 
%
% Tag 3 - 	VAR/EQ  Word = type variable number, or pointer to 
% 		 		indicator, equivalent type_ctor_info
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
%	   is exactly the same as a functors descriptor).
%
% 	   In any case, you need to look at the equivalent type
% 	   to find out what the data word represents.
%
%---------------------------------------------------------------------------%
%
% Definitions:
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
%
% Data representation: Functors Tables
%
% type_ctor_functors tables are generated, one for each type. These
% contain information about the functors of discriminated union (du)
% types. The same information is available in the type_ctor_layouts, but
% is quite difficult to obtain, because the functors tables are designed
% for easy indexing via primary and secondary tag. 
%
% The first word of any functors table is an indicator of whether this
% type is a du, no_tag, equivalence, enumeration or special (e.g. builtin).
%
% For discriminated unions, the tables contain:
% 	- disc. union indicator
% 	- number of functors this type has
% 	- vector of pointers to functor descriptors, one for each functor
%
% For no_tag types, the tables contain:
% 	- no_tag indicator
% 	- pointers to a functor descriptor, for the functor
%
% For enumerations, the tables contain:
% 	- enumeration indicator
% 	- pointer to enumeration vector
%
% For equivalences, the tables contain:
% 	- equivalence indicator
% 	- pointer to pseudo-type-info of equivalent type
%
% For special or builtin types (not discriminated unions), the tables contain:
%	- special indicator
%
% Note: Future Work
%
% 	Because the indicator in the type_ctor_functors duplicates
% 	information present in the type_ctor_layouts, it would be good
% 	to simplify the encoding of type_ctor_layouts by removing the
% 	redundant information. For example:
% 	
% 	- You don't need any information about equivalences,
% 	  no_tag types, or enumerations in the type_ctor_layouts,
% 	  since the same information is already in the 
% 	  type_ctor_functors.
% 	- If you add a little more information about specials
% 	  to the type_ctor_functors, you would no longer need the
% 	  information in the layout tables (eg, is it an int, a float,
% 	  etc).
%
% 	This means the only information needed in the layout tables is
% 	the unshared and shared tag meanings for discriminated unions
% 	that don't fall into any of the above categories (not no_tag and
% 	not enum). In addition, the code for testing which category a 
% 	particular type belongs to could be greatly simplified.
%
%---------------------------------------------------------------------------%

:- module base_type_layout.

:- interface.

:- import_module hlds_module, list, llds, prog_data.

:- pred base_type_layout__generate_hlds(module_info, module_info).
:- mode base_type_layout__generate_hlds(in, out) is det.

:- pred base_type_layout__generate_llds(module_info, module_info,
		list(comp_gen_c_data)).
:- mode base_type_layout__generate_llds(in, out, out) is det.

	% Given a Mercury type, this predicate returns an rval giving the
	% pseudo type info for that type, plus the llds_type of that rval.
	% The int arguments are label numbers for generating `create' rvals
	% with.
:- pred base_type_layout__construct_typed_pseudo_type_info(type,
	rval, llds_type, int, int).
:- mode base_type_layout__construct_typed_pseudo_type_info(in,
	out, out, in, out) is det.

:- implementation.

:- import_module hlds_data, hlds_pred, hlds_out, type_util.
:- import_module code_util, globals, options, special_pred, prog_util.
:- import_module term.
:- import_module assoc_list, bool, string, int, map, std_util, require.

:- type layout_info 	--->	
	layout_info(
		module_name,	% module name
		cons_table,	% ctor table
		int,		% number of tags available
		int,		% next available label 
		type_id,	% type_id of type being currently examined
		list(comp_gen_c_data)	% generated data
	).

:- type tag_category	--->	unshared 	% tagged pointer
			; 	shared_remote 	% shared tagged pointer
			;	shared_local	% shared constants
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
	module_name, module_info, list(base_gen_layout)).
:- mode base_type_layout__gen_base_gen_layouts(in, in, in, in, out) is det.

base_type_layout__gen_base_gen_layouts([], _, _, _, []).
base_type_layout__gen_base_gen_layouts([TypeId | TypeIds], TypeTable, 
		ModuleName, ModuleInfo, BaseGenInfos) :-
	base_type_layout__gen_base_gen_layouts(TypeIds, TypeTable, ModuleName,
		ModuleInfo, BaseGenInfos1),
	TypeId = SymName - TypeArity,
	(
		% Is this type defined in this module and not hand
		% defined?
			
		SymName = qualified(TypeModuleName, TypeName),
		( 
			TypeModuleName = ModuleName,
			\+ type_id_is_hand_defined(TypeId)
		->
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
			"found in type_ctor_layout"], Msg),
		error(Msg)
	).

%---------------------------------------------------------------------------%

	% Initialize the LayoutInfo, and begin processing BaseGenInfos.
base_type_layout__generate_llds(ModuleInfo0, ModuleInfo, CModules) :-
	module_info_base_gen_layouts(ModuleInfo0, BaseGenInfos),
	module_info_globals(ModuleInfo0, Globals),
	globals__lookup_int_option(Globals, num_tag_bits, NumTagBits),
	int__pow(2, NumTagBits, MaxTags),
	module_info_name(ModuleInfo0, ModuleName),
	module_info_ctors(ModuleInfo0, ConsTable),
	module_info_get_cell_count(ModuleInfo0, CellCount),
	LayoutInfo0 = layout_info(ModuleName, ConsTable, MaxTags, CellCount, 
		unqualified("") - 0, []),
	base_type_layout__construct_base_type_data(BaseGenInfos, 
		LayoutInfo0, LayoutInfo),
	LayoutInfo = layout_info(_, _, _, FinalCellCount, _, CModules),
	module_info_set_cell_count(ModuleInfo0, FinalCellCount, ModuleInfo).

%---------------------------------------------------------------------------%


%---------------------------------------------------------------------------%

	% For each type, generate the required CModules, one for
	% functors, one for layout.
	
:- pred base_type_layout__construct_base_type_data(list(base_gen_layout),
	layout_info, layout_info).
:- mode base_type_layout__construct_base_type_data(in, in, out) is det.

base_type_layout__construct_base_type_data([], LayoutInfo, LayoutInfo).
base_type_layout__construct_base_type_data([BaseGenInfo | BaseGenInfos],
		LayoutInfo0, LayoutInfo) :-
	BaseGenInfo = base_gen_layout(TypeId, ModuleName, TypeName, TypeArity,
		_Status, HldsType),
	base_type_layout__set_type_id(LayoutInfo0, TypeId, LayoutInfo1),
	hlds_data__get_type_defn_body(HldsType, TypeBody),
	(
		TypeBody = uu_type(_Alts),
		error("type_ctor_layout: sorry, undiscriminated union unimplemented\n")
	;
		TypeBody = eqv_type(Type),
		base_type_layout__layout_eqv(Type, LayoutInfo1, 
			LayoutInfo2, LayoutTypeData),
		base_type_layout__functors_eqv(Type, LayoutInfo2, LayoutInfo3,
			FunctorsTypeData)
	;
		TypeBody = abstract_type,
		LayoutInfo3 = LayoutInfo1,
		LayoutTypeData = [],
		FunctorsTypeData = []
	;
		TypeBody = du_type(Ctors, ConsTagMap, Enum, _EqualityPred),

			% sort list on tags, so that 
			% enums, shared local tag and
			% shared remote tags have their shared
			% functors in the right order.
		map__to_assoc_list(ConsTagMap, UnsortedConsTags),
		assoc_list__reverse_members(UnsortedConsTags, RevConsList),
		list__sort(RevConsList, SortedRevConsList), 
		assoc_list__reverse_members(SortedRevConsList, SortedConsTags),
		(
			Enum = yes,
			base_type_layout__layout_enum(SortedConsTags,
				LayoutInfo1, LayoutInfo2, LayoutTypeData),
			base_type_layout__functors_enum(SortedConsTags,
				LayoutInfo2, LayoutInfo3, FunctorsTypeData)
		;
			Enum = no,
			( 
				type_is_no_tag_type(Ctors, Name, TypeArg)
			->
				base_type_layout__layout_no_tag(Name,
					TypeArg, LayoutInfo1, LayoutInfo2,
					LayoutTypeData),
				base_type_layout__functors_no_tag(Name,
					TypeArg, LayoutInfo2, LayoutInfo3,
					FunctorsTypeData)
			;
				base_type_layout__layout_du(
					SortedConsTags, LayoutInfo1, 
					LayoutInfo2, LayoutTypeData),
				base_type_layout__functors_du(
					UnsortedConsTags, LayoutInfo2, 
					LayoutInfo3, FunctorsTypeData)
			)
		)
	),	

	%
	% Note: type_ctor_layouts and type_ctor_functors are never exported,
	% because they should only be accessed via the type_ctor_info in
	% the same module.
	% Accesses to the type_ctor_layout for a type exported from a
	% different module should be done via that type's type_ctor_info,
	% which will be exported if the type was exported/abstract_exported.
	%
	Exported = no,

		% pure abstract types have no layout definition.
	( 
		LayoutTypeData = []
	->
		LayoutInfo5 = LayoutInfo3
	;
		LayoutDataName = type_ctor(layout, TypeName, TypeArity),
		LayoutCData = comp_gen_c_data(ModuleName, LayoutDataName,
			Exported, LayoutTypeData, uniform(no), []),
		FunctorsDataName = type_ctor(functors, TypeName, TypeArity),
		FunctorsCData = comp_gen_c_data(ModuleName, FunctorsDataName,
			Exported, FunctorsTypeData, uniform(no), []),
		base_type_layout__add_c_data(LayoutInfo3, LayoutCData, 
			LayoutInfo4),
		base_type_layout__add_c_data(LayoutInfo4, FunctorsCData, 
			LayoutInfo5)
	),
	base_type_layout__construct_base_type_data(BaseGenInfos, LayoutInfo5,
		LayoutInfo).


%---------------------------------------------------------------------------%

	% Constants - these should be kept in check with the runtime
	% definitions.

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
base_type_layout__tag_value(shared_local, 0).
base_type_layout__tag_value(unshared, 1).
base_type_layout__tag_value(shared_remote, 2).
base_type_layout__tag_value(no_tag, 3).
base_type_layout__tag_value(unused, 0).

:- pred base_type_layout__tag_value_const(int::out) is det.
base_type_layout__tag_value_const(0).

:- pred base_type_layout__tag_value_equiv(int::out) is det.
base_type_layout__tag_value_equiv(3).

	% Constants for type_ctor_functors

:- type functors_category 	--->	du
				;	enum
				;	equiv
				;	special
				;	no_tag.

:- pred base_type_layout__functors_value(functors_category::in, int::out) 
	is det.
base_type_layout__functors_value(du, 0).
base_type_layout__functors_value(enum, 1).
base_type_layout__functors_value(equiv, 2).
base_type_layout__functors_value(special, 3).
base_type_layout__functors_value(no_tag, 4).

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
	static_or_dynamic, int, list(maybe(rval))).
:- mode base_type_layout__encode_create(in, in, in, in, in, out) is det.
base_type_layout__encode_create(LayoutInfo, Tag, Rvals0, StatDyn, CellNumber, 
		Rvals) :-
	base_type_layout__get_max_tags(LayoutInfo, MaxTags),
	(
		MaxTags < 4
	->
		Rvals = [yes(const(int_const(Tag))), 
			yes(create(0, Rvals0, uniform(no), StatDyn,
				CellNumber, "type_layout"))]
	;
		Rvals = [yes(create(Tag, Rvals0, uniform(no), StatDyn,
			CellNumber, "type_layout"))]
	).

	% Encode a cons tag (unshared or shared) in rvals.

:- pred base_type_layout__encode_cons_tag(cons_tag, list(maybe(rval)), 
	layout_info, layout_info).
:- mode base_type_layout__encode_cons_tag(in, out, in, out) is det.
base_type_layout__encode_cons_tag(ConsTag, ConsTagRval, LayoutInfo, 
		LayoutInfo) :-
	( 
		ConsTag = unshared_tag(Tag0) 
	->
		SecTag = 0, Tag = Tag0
	; 
		ConsTag = shared_remote_tag(Tag0, SecTag0) 
	->
		SecTag = SecTag0, Tag = Tag0
	;
		ConsTag = shared_local_tag(Tag0, SecTag0) 
	->
		SecTag = SecTag0, Tag = Tag0
	; 
		error(
		"type_ctor_layout: cons_tag not shared or unshared in du")
	),
	base_type_layout__encode_mkword(LayoutInfo, Tag, 
		const(int_const(SecTag)), ConsTagRval).

%---------------------------------------------------------------------------%

	% If the type is reduced to some sort of constant or special,
	% handle it seperately.
	
:- pred base_type_layout__layout_special(pair(cons_id, cons_tag), 
	layout_info, int, list(maybe(rval))).
:- mode base_type_layout__layout_special(in, in, in, out) is semidet.
base_type_layout__layout_special(_ConsId - ConsTag, LayoutInfo, 
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
		error("type_ctor_layout: Unexpected tag - pred_closure_tag/2")
	;
		ConsTag = code_addr_constant(_, _),
		error("type_ctor_layout: Unexpected constant - code_addr_constant/2")
	;
		ConsTag = type_ctor_info_constant(_, _, _),
		error("type_ctor_layout: Unexpected constant - type_ctor_info_constant/3")
	),
	list__duplicate(MaxTags, Rval, RvalsList),
	list__condense(RvalsList, Rvals).


	% For enumerations:
	%
	% tag is 0, rest of word is pointer to enumeration vector.
	
:- pred base_type_layout__layout_enum(assoc_list(cons_id, cons_tag),
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__layout_enum(in, in, out, out) is det.
base_type_layout__layout_enum(ConsList, LayoutInfo0, LayoutInfo, Rvals) :-
	
		% Construct the vector
	base_type_layout__layout_enum_vector(ConsList, VectorRvals),

		% Create a tagged pointer to it
	base_type_layout__get_next_cell_number(NextCellNumber, LayoutInfo0,
		LayoutInfo),
	base_type_layout__tag_value_const(Tag),
	base_type_layout__encode_create(LayoutInfo, Tag, 
		VectorRvals, must_be_static, NextCellNumber, Rval),

		% Duplicate it MaxTags times.
	base_type_layout__get_max_tags(LayoutInfo, MaxTags),
	list__duplicate(MaxTags, Rval, RvalList),
	list__condense(RvalList, Rvals).

	% Construct enumeration vector, which contains:
	% 	- enum indicator (that is, yes, this is an enum)
	% 	- S, the number of constants in this enum
	% 	- S strings of constant names

:- pred base_type_layout__layout_enum_vector(assoc_list(cons_id, cons_tag),
	list(maybe(rval))).
:- mode base_type_layout__layout_enum_vector(in, out) is det.
base_type_layout__layout_enum_vector(ConsList, Rvals) :-
	list__map(
		lambda([ConsId::in, CtorRval::out] is det, (
		    ( ConsId = cons(SymName, _Arity) - _ConsTag ->
			unqualify_name(SymName, CtorName),
			CtorRval = yes(const(string_const(CtorName)))
		    ;
			error("type_ctor_layout: constant has no constructor")
		    )
		)),
		ConsList, CtorNameRvals),
	base_type_layout__enum_indicator(yes, EnumIndicator),
	Rval0 = yes(const(int_const(EnumIndicator))),
	list__length(ConsList, NumCtors),
	Rval1 = yes(const(int_const(NumCtors))),
	Rvals = [Rval0, Rval1 | CtorNameRvals].

	
	% For no_tag types:
	%
	% Tag is 3, rest of word is pointer to no_tag vector.

:- pred base_type_layout__layout_no_tag(sym_name, type, layout_info, 
		layout_info, list(maybe(rval))).
:- mode base_type_layout__layout_no_tag(in, in, in, out, out) is det.
base_type_layout__layout_no_tag(SymName, Type, LayoutInfo0, 
		LayoutInfo, Rvals) :-

	base_type_layout__layout_no_tag_vector(SymName, Type,
		LayoutInfo0, LayoutInfo1, VectorRvals),

	base_type_layout__get_next_cell_number(NextCellNumber, LayoutInfo1,
		LayoutInfo),
	base_type_layout__tag_value_equiv(Tag),

	base_type_layout__encode_create(LayoutInfo, Tag, 
			VectorRvals, must_be_static, NextCellNumber, Rval),

	base_type_layout__get_max_tags(LayoutInfo, MaxTags),
	list__duplicate(MaxTags, Rval, RvalsList),
	list__condense(RvalsList, Rvals).

	% no_tag vector:
	% 	- no_tag indicator 
	% 	- pseudo_type_info (of the argument)
	% 	- functor name.
	% 	- tag information (bogus, see comment in code below)

:- pred base_type_layout__layout_no_tag_vector(sym_name, type, 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__layout_no_tag_vector(in, in, in, out, out) 
	is det.
base_type_layout__layout_no_tag_vector(SymName, Type, LayoutInfo0, 
		LayoutInfo, Rvals) :-

		% indicator of tag_type
	base_type_layout__no_tag_indicator(yes, NoTagIndicator),
	Rval0 = yes(const(int_const(NoTagIndicator))),

		% generate pseudo_type_info
	base_type_layout__generate_pseudo_type_info(Type, Rval1, LayoutInfo0, 
		LayoutInfo1),

		% functor name
	unqualify_name(SymName, Name),
	Rval2 = yes(const(string_const(Name))),

		% create tag information
		% since it's a no_tag, we'll give it a tag value of 0
		% to be consistent, but this doesn't really have any
		% meaning.
	base_type_layout__encode_cons_tag(unshared_tag(0), ConsTagRvals, 
		LayoutInfo1, LayoutInfo),

	Rvals = [Rval0, Rval1, Rval2 | ConsTagRvals].

	% For equivalences:
	%
	% Tag is 3, rest of word is pointer to pseudo_type_info or
	% variable number

:- pred base_type_layout__layout_eqv(type, layout_info, 
		layout_info, list(maybe(rval))).
:- mode base_type_layout__layout_eqv(in, in, out, out) is det.
base_type_layout__layout_eqv(Type, LayoutInfo0, LayoutInfo, Rvals) :-

		% generate rest of word, remove a level of creates
	base_type_layout__generate_pseudo_type_info(Type, Rval0, LayoutInfo0, 
		LayoutInfo1),
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
		
		base_type_layout__get_next_cell_number(NextCellNumber, 
			LayoutInfo1, LayoutInfo),
		base_type_layout__encode_create(LayoutInfo, Tag, 
			[IndicatorRval, Rval0], must_be_static,
			NextCellNumber, Rval)
	),
	base_type_layout__get_max_tags(LayoutInfo, MaxTags),
	list__duplicate(MaxTags, Rval, RvalsList),
	list__condense(RvalsList, Rvals).


	% For discriminated unions:
	%
	% Mixture of unshared, shared_local and shared_remote
	% tags. For each primary tag value, we have a word that
	% describes what it represents. The list of words will
	% form an array that can be indexed by primary tag.
	
:- pred base_type_layout__layout_du(assoc_list(cons_id, cons_tag), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__layout_du(in, in, out, out) is det.
base_type_layout__layout_du([], _, _, []) :-
	error("type_ctor_layout: type with no cons_tag information").
base_type_layout__layout_du(ConsList, LayoutInfo0, LayoutInfo, Rvals) :-
	ConsList = [ConsPair | _],
	base_type_layout__get_max_tags(LayoutInfo0, MaxTags),
	(
		base_type_layout__layout_special(ConsPair, 
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


	% Generate an rval for each primary tag value.

:- pred base_type_layout__generate_rvals(list(pair(tag_category, 
	list(pair(cons_id, cons_tag)))), layout_info, layout_info, 
	list(maybe(rval)), list(maybe(rval))).
:- mode base_type_layout__generate_rvals(in, in, out, in, out) is det.

base_type_layout__generate_rvals([], LayoutInfo, LayoutInfo, Rvals, Rvals).
base_type_layout__generate_rvals([Tag - ConsList | Rest], LayoutInfo0,
		LayoutInfo, Rvals0, Rvals) :-
	(
		Tag = unshared,
		base_type_layout__handle_unshared(ConsList, LayoutInfo0, 
			LayoutInfo1, Rval),
		list__append(Rval, Rvals0, Rvals1)
	;
		Tag = shared_remote,
		base_type_layout__handle_shared_remote(ConsList, LayoutInfo0, 
			LayoutInfo1, Rval),
		list__append(Rval, Rvals0, Rvals1)

	;
		Tag = shared_local,
		base_type_layout__handle_shared_local(ConsList, LayoutInfo0, 
			LayoutInfo1, Rval),
		list__append(Rval, Rvals0, Rvals1)
	;
		Tag = no_tag,
		error("type_ctor_layout: unexpected no_tag")
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


	% For shared local tags:
	%
	% tag is 0, rest of word is pointer to 
	% 	- enum indicator (no, this isn't an enum)
	% 	- S, the number of constants sharing this tag 
	% 	- S strings of constant names

:- pred base_type_layout__handle_shared_local(list(pair(cons_id, cons_tag)), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__handle_shared_local(in, in, out, out) is det.

base_type_layout__handle_shared_local([], _, _, _) :-
	error("type_ctor_layout: no constructors for shared local tag").
base_type_layout__handle_shared_local([C | Cs], LayoutInfo0, LayoutInfo,
		Rval) :-
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
			error("type_ctor_layout: constant has no constructor")
		))),
	    [C | Cs], CtorNameRvals),

	base_type_layout__get_next_cell_number(NextCellNumber, LayoutInfo0,
		LayoutInfo),
	base_type_layout__tag_value(shared_local, Tag),
	base_type_layout__encode_create(LayoutInfo, Tag, 
		[Rval0, Rval1 | CtorNameRvals], must_be_static,
		NextCellNumber, Rval).

	% For unshared tags:
	%
	% Tag 1, with a pointer to a functor descriptor

:- pred base_type_layout__handle_unshared(list(pair(cons_id, cons_tag)), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__handle_unshared(in, in, out, out) is det.

base_type_layout__handle_unshared(ConsList, LayoutInfo0, LayoutInfo, Rval) :-
	base_type_layout__functor_descriptor(ConsList, LayoutInfo0, LayoutInfo1,
		EndRvals),
	base_type_layout__get_next_cell_number(NextCellNumber, LayoutInfo1,
		LayoutInfo),
	base_type_layout__tag_value(unshared, Tag),
	base_type_layout__encode_create(LayoutInfo, Tag, EndRvals,
		must_be_static, NextCellNumber, Rval).

	% Create a functor descriptor.
	%
	%	N - the arity of this functor 
	%	N pseudo-typeinfos (of the arguments)
	%	- a string constant (the name of the functor)
	%	- tag information

:- pred base_type_layout__functor_descriptor(list(pair(cons_id, cons_tag)), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__functor_descriptor(in, in, out, out) is det.

base_type_layout__functor_descriptor([], _, _, _) :-
	error("type_ctor_layout: no constructors for unshared tag").
base_type_layout__functor_descriptor([ConsId - ConsTag | _], LayoutInfo0, 
		LayoutInfo, EndRvals) :-
	( 
		ConsId = cons(SymName, _Arity)
	->
		unqualify_name(SymName, ConsString)
	;
		error("type_ctor_layout: unshared tag with no constructor")
	),
	base_type_layout__get_cons_args(LayoutInfo0, ConsId, ConsArgs),
	list__length(ConsArgs, NumArgs),
	list__map_foldl(base_type_layout__generate_pseudo_type_info,
		ConsArgs, PseudoTypeInfos, LayoutInfo0, LayoutInfo1),
	base_type_layout__encode_cons_tag(ConsTag, ConsTagRvals, LayoutInfo1,
		LayoutInfo),
	list__append([yes(const(int_const(NumArgs))) | PseudoTypeInfos], 
		[yes(const(string_const(ConsString))) | ConsTagRvals], 
		EndRvals).


	% For shared remote tags:
	%
	% Tag 2, with a pointer to an array containing:
	% 	F - the number of functors sharing this tag
	% 	F pointers to functor descriptors

:- pred base_type_layout__handle_shared_remote(list(pair(cons_id, cons_tag)), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__handle_shared_remote(in, in, out, out) is det.

base_type_layout__handle_shared_remote([], _, _, _) :-
	error("type_ctor_layout: no constructors for shared remote tag").
base_type_layout__handle_shared_remote([C | Cs], LayoutInfo0, LayoutInfo,
		Rval) :-
	base_type_layout__get_next_cell_number(NextCellNumber, LayoutInfo0,
		LayoutInfo1),

		% Number of sharers
	list__length([C | Cs], NumCtors),
	NumSharersRval = yes(const(int_const(NumCtors))),

		% Create rvals for sharers
		% (just like a series of unshared tags)
	list__foldr(
		lambda([Cons::in, Acc::in, NewAcc::out] is det, (
			Acc = Rvals0 - LayoutInfoA,
			base_type_layout__handle_unshared([Cons], LayoutInfoA,
				LayoutInfoB, Rval1),
			list__append(Rval1, Rvals0, Rvals1),
			NewAcc = Rvals1 - LayoutInfoB)),
		[C | Cs],
		[] - LayoutInfo1, 
		SharedRvals - LayoutInfo),

	base_type_layout__tag_value(shared_remote, Tag),
	base_type_layout__encode_create(LayoutInfo, Tag, 
		[NumSharersRval | SharedRvals], must_be_static,
		NextCellNumber, Rval).

%---------------------------------------------------------------------------%

	% Code to create the contents of type_ctor_functors.


	% type_ctor_functors of an equivalence type:
	%
	% - equivalence indicator
	% - pointer to equivalent pseudo_type_info

:- pred base_type_layout__functors_eqv(type, layout_info, layout_info, 
	list(maybe(rval))).
:- mode base_type_layout__functors_eqv(in, in, out, out) is det.

base_type_layout__functors_eqv(Type, LayoutInfo0, LayoutInfo, Rvals) :-

		% Construct pseudo
	base_type_layout__generate_pseudo_type_info(Type, Rvals0, LayoutInfo0, 
		LayoutInfo),
	base_type_layout__functors_value(equiv, EqvIndicator),
	EqvRval = yes(const(int_const(EqvIndicator))),
	Rvals = [EqvRval, Rvals0].

	% type_ctor_functors of an enumeration:
	%
	% - enumeration indicator
	% - pointer to enumeration vector

:- pred base_type_layout__functors_enum(assoc_list(cons_id, cons_tag), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__functors_enum(in, in, out, out) is det.

base_type_layout__functors_enum(ConsList, LayoutInfo0, LayoutInfo, Rvals) :-

		% Construct the vector
	base_type_layout__layout_enum_vector(ConsList, VectorRvals),

		% Create a pointer to it
	base_type_layout__get_next_cell_number(NextCellNumber, LayoutInfo0,
		LayoutInfo),
	base_type_layout__functors_value(enum, EnumIndicator),
	EnumRval = yes(const(int_const(EnumIndicator))),
	CreateRval = yes(create(0, VectorRvals, uniform(no), must_be_static,
		NextCellNumber, "type_layout")),
	Rvals = [EnumRval, CreateRval].

	% type_ctor_functors of a no_tag:
	%
	% - no_tag indicator
	% - pointer to functor descriptor

:- pred base_type_layout__functors_no_tag(sym_name, type, layout_info, 
		layout_info, list(maybe(rval))).
:- mode base_type_layout__functors_no_tag(in, in, in, out, out) is det.
base_type_layout__functors_no_tag(SymName, Type, LayoutInfo0, 
		LayoutInfo, Rvals) :-

	base_type_layout__layout_no_tag_vector(SymName, Type,
		LayoutInfo0, LayoutInfo1, VectorRvals),

	base_type_layout__get_next_cell_number(NextCellNumber, LayoutInfo1,
		LayoutInfo),
	CreateRval = yes(create(0, VectorRvals, uniform(no), must_be_static,
		NextCellNumber, "type_layout")),

	base_type_layout__functors_value(no_tag, NoTagIndicator),
	NoTagRval = yes(const(int_const(NoTagIndicator))),

	Rvals = [NoTagRval, CreateRval].

	% type_ctor_functors of a du:
	%
	% - du indicator
	% - number of functors
	% - vector of pointers to functor descriptor 

:- pred base_type_layout__functors_du(assoc_list(cons_id, cons_tag), 
	layout_info, layout_info, list(maybe(rval))).
:- mode base_type_layout__functors_du(in, in, out, out) is det.
base_type_layout__functors_du(ConsList, LayoutInfo0, LayoutInfo, Rvals) :-
	base_type_layout__functors_value(du, DuIndicator),
	DuIndicatorRval = yes(const(int_const(DuIndicator))),
	list__length(ConsList, Length),
	LengthRval = yes(const(int_const(Length))),
	list__foldr(
		lambda([ConsPair::in, Acc::in, NewAcc::out] is det, (
			Acc = Rvals0 - LayoutInfoA,
			base_type_layout__functor_descriptor([ConsPair],
				LayoutInfoA, LayoutInfoB, VectorRvalList),
			base_type_layout__get_next_cell_number(NextCellNumber,
				LayoutInfoB, LayoutInfoC),
			VectorRval = yes(create(0, VectorRvalList, uniform(no),
				must_be_static, NextCellNumber,
				"type_layout")),
			Rvals1 = [VectorRval | Rvals0],
			NewAcc = Rvals1 - LayoutInfoC)),
		ConsList, [] - LayoutInfo0, VectorRvals - LayoutInfo),
	Rvals = [DuIndicatorRval, LengthRval | VectorRvals].

	% type_ctor_functors of a special:
	%
	% - special indicator
	
:- pred base_type_layout__functors_special(pair(cons_id, cons_tag), 
	list(maybe(rval))).
:- mode base_type_layout__functors_special(in, out) is semidet.
base_type_layout__functors_special(_ConsId - ConsTag, Rvals) :-
	(
		ConsTag = string_constant(_) ;
	  	ConsTag = float_constant(_) ; 
	  	ConsTag = int_constant(_) 
	),
	base_type_layout__functors_value(special, BuiltinIndicator),
	BuiltinRval = yes(const(int_const(BuiltinIndicator))),
	Rvals = [BuiltinRval].

%---------------------------------------------------------------------------%

:- pred base_type_layout__generate_pseudo_type_info(type, maybe(rval), 
	layout_info, layout_info).
:- mode base_type_layout__generate_pseudo_type_info(in, out, in, out) is det.

base_type_layout__generate_pseudo_type_info(Type, yes(Rval), LayoutInfo0, 
		LayoutInfo) :-
	base_type_layout__get_cell_number(LayoutInfo0, CellNumber0),
	base_type_layout__construct_pseudo_type_info(Type, Rval,
		CellNumber0, CellNumber),
	base_type_layout__set_cell_number(CellNumber, LayoutInfo0, LayoutInfo).

:- pred base_type_layout__construct_pseudo_type_info(type, rval, int, int).
:- mode base_type_layout__construct_pseudo_type_info(in, out, in, out) is det.

base_type_layout__construct_pseudo_type_info(Type, Pseudo, CNum0, CNum) :-
	base_type_layout__construct_typed_pseudo_type_info(Type, Pseudo, _,
		CNum0, CNum).

base_type_layout__construct_typed_pseudo_type_info(Type, Pseudo, LldsType,
		CNum0, CNum) :-
	(
		type_to_type_id(Type, TypeId, TypeArgs0)
	->
		(
			% The argument to typeclass_info types is not
			% a type - it encodes the class constraint.
			mercury_private_builtin_module(PrivateBuiltin),
			TypeId = qualified(PrivateBuiltin, TName) - _,
			( TName = "typeclass_info"
			; TName = "base_typeclass_info"	
			)
		->
			TypeArgs = []
		;
			TypeArgs = TypeArgs0
		),
		( 
			% For higher order types: they all refer to the
			% defined pred_0 type_ctor_info, have an extra
			% argument for their real arity, and then type
			% arguments according to their types. 
			% polymorphism.m has a detailed explanation.

			type_is_higher_order(Type, _PredFunc, _TypeArgs)
		->
			TypeModule = unqualified(""),
			TypeName = "pred",
			Arity = 0,
			TypeId = _QualTypeName - RealArity,
			RealArityArg = [yes(const(int_const(RealArity)))]
		;
			TypeId = QualTypeName - Arity,
			unqualify_name(QualTypeName, TypeName),
			sym_name_get_module_name(QualTypeName, unqualified(""),
					TypeModule),
			RealArityArg = []
		),
		Pseudo0 = yes(const(data_addr_const(data_addr(TypeModule,
			type_ctor(info, TypeName, Arity))))),
		LldsType = data_ptr,
		CNum1 = CNum0 + 1,

			% generate args, but remove one level of create()s.
		list__map_foldl(base_type_layout__construct_pseudo_type_info,
			TypeArgs, PseudoArgs0, CNum1, CNum),
		list__map(base_type_layout__remove_create, PseudoArgs0,
			PseudoArgs1),

		list__append(RealArityArg, PseudoArgs1, PseudoArgs),

		Pseudo = create(0, [Pseudo0 | PseudoArgs], uniform(no),
			must_be_static, CNum0, "type_layout")
	;
		type_util__var(Type, Var)
	->
		term__var_to_int(Var, VarInt),
		base_type_layout__max_varint(MaxVarInt),
		require(VarInt < MaxVarInt, 
			"type_ctor_layout: type variable representation exceeds limit"),
		Pseudo = const(int_const(VarInt)),
		LldsType = integer,
		CNum = CNum0
	;
		error("type_ctor_layout: type neither var nor non-var")
	).


	% Remove a create() from an rval, if present.
	
:- pred base_type_layout__remove_create(rval, maybe(rval)).
:- mode base_type_layout__remove_create(in, out) is det.

base_type_layout__remove_create(Rval0, Rval) :-
	(
		Rval0 = create(_, [PTI], _, _, _, _)
	->
		Rval = PTI
	;
		Rval = yes(Rval0)
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

base_type_layout__tag_type_and_value(unshared_tag(Tag), Tag, unshared).
base_type_layout__tag_type_and_value(shared_remote_tag(Tag, _), Tag,
	shared_remote).
base_type_layout__tag_type_and_value(shared_local_tag(Tag, _), Tag, 
	shared_local).
base_type_layout__tag_type_and_value(no_tag, -1, no_tag). 
base_type_layout__tag_type_and_value(string_constant(_), -1, unused). 
base_type_layout__tag_type_and_value(float_constant(_), -1, unused). 
base_type_layout__tag_type_and_value(int_constant(_), -1, unused). 
base_type_layout__tag_type_and_value(pred_closure_tag(_, _), -1, unused). 
base_type_layout__tag_type_and_value(code_addr_constant(_, _), -1, unused).
base_type_layout__tag_type_and_value(type_ctor_info_constant(_, _, _), -1,
	unused). 
base_type_layout__tag_type_and_value(base_typeclass_info_constant(_, _, _), -1,
	unused). 
base_type_layout__tag_type_and_value(tabling_pointer_constant(_, _), -1,
	unused). 

	% Get the arguments of this constructor of the current type.
	
:- pred base_type_layout__get_cons_args(layout_info, cons_id, list(type)).
:- mode base_type_layout__get_cons_args(in, in, out) is det.

base_type_layout__get_cons_args(LayoutInfo, ConsId, TypeArgs) :-
	base_type_layout__get_cons_table(LayoutInfo, ConsTable),
	base_type_layout__get_type_id(LayoutInfo, TypeId),
	(
		map__search(ConsTable, ConsId, HldsConsList),
		list__filter(lambda([X::in] is semidet, (
				X = hlds_cons_defn(_, _, _, TypeId, _))),
			HldsConsList,
			[hlds_cons_defn(_, _, TypeArgs0, _, _)])
	->
		TypeArgs = TypeArgs0
	;
		error("type_ctor_layout: no matching cons definition")
	).

%---------------------------------------------------------------------------%

	% access to the type_ctor_layout data structure.

:- pred base_type_layout__get_module_name(layout_info, module_name).
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

:- pred base_type_layout__get_cell_number(layout_info, int).
:- mode base_type_layout__get_cell_number(in, out) is det.
base_type_layout__get_cell_number(LayoutInfo, NextCNum) :-
	LayoutInfo = layout_info(_, _, _, NextCNum, _, _).

:- pred base_type_layout__get_type_id(layout_info, type_id).
:- mode base_type_layout__get_type_id(in, out) is det.
base_type_layout__get_type_id(LayoutInfo, TypeId) :-
	LayoutInfo = layout_info(_, _, _, _, TypeId, _).

:- pred base_type_layout__get_c_data(layout_info, list(comp_gen_c_data)).
:- mode base_type_layout__get_c_data(in, out) is det.
base_type_layout__get_c_data(LayoutInfo, CModules) :-
	LayoutInfo = layout_info(_, _, _, _, _, CModules).

:- pred base_type_layout__add_c_data(layout_info, comp_gen_c_data,
	layout_info).
:- mode base_type_layout__add_c_data(in, in, out) is det.
base_type_layout__add_c_data(LayoutInfo0, CModule, LayoutInfo) :-
	LayoutInfo0 = layout_info(A, B, C, D, E, CModules0),
	CModules = [CModule | CModules0],
	LayoutInfo = layout_info(A, B, C, D, E, CModules).

:- pred base_type_layout__get_next_cell_number(int, layout_info, layout_info).
:- mode base_type_layout__get_next_cell_number(out, in, out) is det.
base_type_layout__get_next_cell_number(CNum0, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = layout_info(A, B, C, CNum0, E, F),
	CNum = CNum0 + 1,
	LayoutInfo = layout_info(A, B, C, CNum, E, F).

:- pred base_type_layout__set_cell_number(int, layout_info, layout_info).
:- mode base_type_layout__set_cell_number(in, in, out) is det.
base_type_layout__set_cell_number(NextLabel, LayoutInfo0, LayoutInfo) :-
	LayoutInfo0 = layout_info(A, B, C, _, E, F),
	LayoutInfo = layout_info(A, B, C, NextLabel, E, F).

:- pred base_type_layout__set_type_id(layout_info, type_id, layout_info).
:- mode base_type_layout__set_type_id(in, in, out) is det.
base_type_layout__set_type_id(LayoutInfo0, TypeId, LayoutInfo) :-
	LayoutInfo0 = layout_info(A, B, C, D, _, F),
	LayoutInfo = layout_info(A, B, C, D, TypeId, F).

