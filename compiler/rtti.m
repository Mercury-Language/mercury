%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Definitions of data structures for representing run-time type information
% within the compiler. When output by rtti_out.m, values of most these types
% will correspond to the types defined in runtime/mercury_type_info.h;
% the documentation of those types can be found there.
% The code to generate the structures is in type_ctor_info.m.
% See also pseudo_type_info.m.
%
% This module is independent of whether we are compiling to LLDS or MLDS.
% It is used as an intermediate data structure that we generate from the
% HLDS, and which we can then convert to either LLDS or MLDS.
% The LLDS actually incorporates this data structure unchanged.
%
% Authors: zs, fjh.

%-----------------------------------------------------------------------------%

:- module backend_libs__rtti.

:- interface.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module bool, list, set, map, std_util.

%-----------------------------------------------------------------------------%
%
% The data structures representing types, both ground (typeinfos) and
% nonground (pseudo-typeinfos).

	% An rtti_type_info identifies a ground type.
:- type rtti_type_info
	--->	plain_arity_zero_type_info(
			rtti_type_ctor
		)
	;	plain_type_info(
			rtti_type_ctor,
			% This list should not be empty; if it is, one should
			% use plain_arity_zero_type_info instead.
			list(rtti_type_info)
		)
	;	var_arity_type_info(
			var_arity_ctor_id,
			list(rtti_type_info)
		).

	% An rtti_pseudo_type_info identifies a possibly non-ground type.
:- type rtti_pseudo_type_info
	--->	plain_arity_zero_pseudo_type_info(
			rtti_type_ctor
		)
	;	plain_pseudo_type_info(
			rtti_type_ctor,
			% This list should not be empty; if it is, one should
			% use plain_arity_zero_pseudo_type_info instead.
			list(rtti_maybe_pseudo_type_info)
		)
	;	var_arity_pseudo_type_info(
			var_arity_ctor_id,
			list(rtti_maybe_pseudo_type_info)
		)
	;	type_var(int).

	% An rtti_maybe_pseudo_type_info identifies a type. If the type is
	% ground, it should be bound to plain; if it is non-ground, it should
	% be bound to pseudo.
:- type rtti_maybe_pseudo_type_info
	--->	pseudo(rtti_pseudo_type_info)
	;	plain(rtti_type_info).

	% An rtti_type_ctor uniquely identifies a fixed arity type constructor.
:- type rtti_type_ctor
	--->	rtti_type_ctor(
			module_name,		% module name
			string,			% type ctor's name
			arity			% type ctor's arity
		).

	% A var_arity_ctor_id uniquely identifies a variable arity type
	% constructor.
:- type var_arity_ctor_id
	--->	pred_type_info
	;	func_type_info
	;	tuple_type_info.

%-----------------------------------------------------------------------------%
%
% The data structures representing type constructors.

	% A type_ctor_data structure contains all the information that the
	% runtime system needs to know about a type constructor.
:- type type_ctor_data
	--->	type_ctor_data(
			tcr_version		:: int,
			tcr_module_name		:: module_name,
			tcr_type_name		:: string,
			tcr_arity		:: int,
			tcr_unify_pred		:: univ,
			tcr_compare_pred	:: univ,
			tcr_flags		:: set(type_ctor_flag),
			tcr_rep_details		:: type_ctor_details
		).

	% Each of the following values corresponds to one of the
	% MR_TYPE_CTOR_FLAG_* macros in runtime/mercury_type_info.h.
	% Their meanings are documented there.
:- type type_ctor_flag
	--->	reserve_tag_flag
	;	variable_arity_flag
	;	kind_of_du_flag
	;	typeinfo_fake_arity_flag.

	% A type_ctor_details structure contains all the information that the
	% runtime system needs to know about the data representation scheme
	% used by a type constructor.
	%
	% There are four alternatives that correspond to discriminated union:
	% enum, du, reserved and notag. Enum is for types that define only
	% constants. Notag is for types that define only one unary functor.
	% Reserved is for types in which at least one functor is represented
	% using a reserved value, which may be the address of an object or a
	% small integer (including zero). Du is for all other types.
	%
	% All four alternatives have four kinds of information.
	%
	% First, an indication of whether the type has user-defined equality or
	% not.
	%
	% Second, a list of descriptors containing all the function symbols
	% defined by the type, in declaration order.
	%
	% Third, a table that allows the runtime system to map a value in
	% memory to a printable representation (i.e. to implement the
	% deconstruct operation).
	%
	% Fourth, a table that allows the runtime system to map a printable
	% representation to a value in memory (i.e. to implement the
	% construct operation).
	%
	% For types in which some function symbols are represented by reserved
	% addresses, the third component is in two parts: a list of function
	% symbols so represented, and a table indexed by the primary tag for
	% all the other function symbols. The runtime system must check every
	% element on the list before looking at the primary tag.
	%
	% For notag types, the single functor descriptor fills the roles of
	% the second, third and fourth components.

:- type type_ctor_details
	--->	enum(
			enum_axioms		:: equality_axioms,
			enum_functors		:: list(enum_functor),
			enum_value_table	:: map(int, enum_functor),
			enum_name_table		:: map(string, enum_functor)
		)
	;	du(
			du_axioms		:: equality_axioms,
			du_functors		:: list(du_functor),
			du_value_table		:: ptag_map,
			du_name_table		:: map(string, map(int,
							du_functor))
		)
	;	reserved(
			res_axioms		:: equality_axioms,
			res_functors		:: list(maybe_reserved_functor),
			res_value_table_res	:: list(reserved_functor),
			res_value_table_du	:: ptag_map,
			res_name_table		:: map(string, map(int,
							maybe_reserved_functor))
		)
	;	notag(
			notag_axioms		:: equality_axioms,
			notag_functor		:: notag_functor
		)
	;	eqv(
			eqv_type		:: rtti_maybe_pseudo_type_info
		)
	;	builtin(
			builtin_ctor		:: builtin_ctor
		)
	;	impl_artifact(
			impl_ctor		:: impl_ctor
		)
	;	foreign(
			is_stable		:: is_stable
		).

	% For a given du family type, this says whether the user has defined
	% their own unification predicate for the type.
:- type equality_axioms
	--->	standard
	;	user_defined.

	% Descriptor for a functor in an enum type.
	%
	% This type corresponds to the C type MR_EnumFunctorDesc.
:- type enum_functor
	--->	enum_functor(
			enum_name		:: string,
			enum_ordinal		:: int
		).

	% Descriptor for a functor in a notag type.
	%
	% This type corresponds to the C type MR_NotagFunctorDesc.
:- type notag_functor
	--->	notag_functor(
			nt_name			:: string,
			nt_arg_type		:: rtti_maybe_pseudo_type_info,
			nt_arg_name		:: maybe(string)
		).

	% Descriptor for a functor in a du type. Also used for functors in
	% reserved address types which are not represented by a reserved
	% address.
	%
	% This type mostly corresponds to the C type MR_DuFunctorDesc.
:- type du_functor
	--->	du_functor(
			du_name			:: string,
			du_orig_arity		:: int,
			du_ordinal		:: int,
			du_rep			:: du_rep,
			du_arg_infos		:: list(du_arg_info),
			du_exist_info		:: maybe(exist_info)
		).

	% Descriptor for a functor represented by a reserved address.
	%
	% This type corresponds to the C type MR_ReservedAddrFunctorDesc.
:- type reserved_functor
	--->	reserved_functor(
			res_name		:: string,
			res_ordinal		:: int,
			res_rep			:: reserved_address
		).

	% Descriptor for a functor in reserved address type.
	%
	% This type corresponds to the C type MR_MaybeResAddrFunctorDesc,
	% although their structure is slightly different in order to make
	% searches on an array of the C structures as convenient as searches
	% on a list of values of this Mercury type.
:- type maybe_reserved_functor
	--->	res_func(
			mrf_res			:: reserved_functor
		)
	;	du_func(
			mrf_du			:: du_functor
		).

	% Describes the representation of a functor in a general
	% discriminated union type.
	%
	% Will probably need modification for the Java and IL back ends.
:- type du_rep
	--->	du_ll_rep(
			du_ll_ptag		:: int,
			du_ll_sec_tag		:: sectag_and_locn
		)
	;	du_hl_rep(
			remote_sec_tag		:: int
		).

	% Describes the types of the existentially typed arguments of a
	% discriminated union functor.
	%
	% This type corresponds to the C type MR_DuExistInfo.
:- type	exist_info
	--->	exist_info(
			exist_num_plain_typeinfos	:: int,
			exist_num_typeinfos_in_tcis	:: int,
			exist_typeclass_constraints	:: list(tc_constraint),
			exist_typeinfo_locns		::
						list(exist_typeinfo_locn)
		).

	% Describes the location at which one can find the typeinfo for the
	% type bound to an existentially quantified type variable in a
	% discriminated union functor.
	%
	% This type corresponds to the C type MR_DuExistLocn.
:- type exist_typeinfo_locn
	--->	plain_typeinfo(
			int			% The typeinfo is stored
						% directly in the cell, at this
						% offset.
		)
	;	typeinfo_in_tci(
			int,			% The typeinfo is stored
						% indirectly in the typeclass
						% info stored at this offset
						% in the cell.
			int			% To find the typeinfo inside
						% the typeclass info structure,
						% give this integer to the
						% MR_typeclass_info_type_info
						% macro.
		).

	% These tables let the runtime system interpret values in memory
	% of general discriminated union types.
	%
	% The runtime system should first use the primary tag to index into
	% the type's ptag_map. It can then find the location (if any) of the
	% secondary tag, and use the secondary tag (or zero if there isn't one)
	% to index into the stag_map to find the functor descriptor.
	%
	% The type sectag_table corresponds to the C type MR_DuPtagLayout.
	% The two maps are implemented in C as simple arrays.

:- type ptag_map	== map(int, sectag_table).	% key is primary tag
:- type stag_map	== map(int, du_functor).	% key is secondary tag

:- type	sectag_table
	--->	sectag_table(
			sectag_locn		:: sectag_locn,
			sectag_num_sharers	:: int,
			sectag_map		:: stag_map
		).

	% Describes the location of the secondary tag for a given primary tag
	% value in a given type.
:- type sectag_locn
	--->	sectag_none
	;	sectag_local
	;	sectag_remote.

	% Describes the location of the secondary tag and its value for a
	% given functor in a given type.
:- type sectag_and_locn
	--->	sectag_none
	;	sectag_local(int)
	;	sectag_remote(int).

	% Information about an argument of a functor in a discriminated union
	% type.
:- type du_arg_info
	--->	du_arg_info(
			du_arg_name	:: maybe(string),
			du_arg_type	:: rtti_maybe_pseudo_type_info_or_self
		).

	% An rtti_maybe_pseudo_type_info identifies the type of a function
	% symbol's argument. If the type of the argument is the same as the
	% type of the whole term, it should be bound to self. Otherwise, if
	% the argument's type is ground, it should be bound to plain; if it
	% is non-ground, it should be bound to pseudo.
:- type rtti_maybe_pseudo_type_info_or_self
	--->	pseudo(rtti_pseudo_type_info)
	;	plain(rtti_type_info)
	;	self.

	% The list of type constructors for types that are built into the
	% Mercury language or the Mercury standard library.
:- type builtin_ctor
	--->	int
	;	float
	;	char
	;	string
	;	void
	;	c_pointer(is_stable)
	;	pred_ctor
	;	func_ctor
	;	tuple
	;	ref
	;	type_desc
	;	pseudo_type_desc
	;	type_ctor_desc.

	% The list of type constructors that are used behind the scenes by
	% the Mercury implementation.
:- type impl_ctor
	--->	hp
	;	succip
	;	maxfr
	;	curfr
	;	redofr
	;	redoip
	;	ticket
	;	trail_ptr
	;	type_info
	;	type_ctor_info
	;	typeclass_info
	;	base_typeclass_info
	;	subgoal.			% coming soon

:- type is_stable
	--->	is_stable
	;	is_not_stable.

%-----------------------------------------------------------------------------%
%
% The data structures representing type class dictionaries.

	% A base_typeclass_info holds information about a typeclass instance.
	% See notes/type_class_transformation.html for details.
:- type base_typeclass_info --->
	base_typeclass_info(
			% num_extra = num_unconstrained + num_constraints,
			% where num_unconstrained is the number of
			% unconstrained type variables from the head
			% of the instance declaration.
		num_extra :: int,
			% num_constraints is the number of constraints
			% on the instance declaration
		num_constraints :: int,
			% num_superclasses is the number of constraints
			% on the typeclass declaration.
		num_superclasses :: int,
			% class_arity is the number of type variables
			% in the head of the class declaration
		class_arity :: int,
			% num_methods is the number of procedures
			% in the typeclass declaration
		num_methods :: int,
			% methods is a list of length num_methods
			% containing the addresses of the methods
			% for this instance declaration.
		methods :: list(rtti_proc_label)
	).

%-----------------------------------------------------------------------------%

% The types in this block (until the next horizontal line) will eventually
% replace base_typeclass_infos. For now, the C data structures they describe
% are generated only on request, and used only by the debugger.

	% This type corresponds to the C type MR_TypeClassMethod.
:- type tc_method_id
	--->	tc_method_id(
			tcm_name		:: string,
			tcm_arity		:: int,
			tcm_pred_or_func	:: pred_or_func
		).

	% Uniquely identifies a type class.
:- type tc_name
	--->	tc_name(
			tcn_module		:: module_name,
			tcn_name		:: string,
			tcn_arity		:: int
		).

	% Values of the tc_id and tc_decl types contain the information about
	% a type class declaration that we need to interpret other data
	% structures related to the type class.
	%
	% The tc_id type corresponds to the C type MR_TypeClassId, while
	% the tc_decl type corresponds to the C type MR_TypeClassDecl.
	%
	% The reason for splitting the information between two C structures
	% is to make it easier to allow us to maintain binary compatibility
	% even if the amount of information we want to record about type class
	% declarations changes.
:- type tc_id
	--->	tc_id(
			tc_id_name		:: tc_name,
			tc_id_type_var_names	:: list(string),
			tc_id_methods		:: list(tc_method_id)
		).

:- type tc_decl
	--->	tc_decl(
			tc_decl_id		:: tc_id,
			tc_decl_version_number	:: int,
			tc_decl_supers		:: list(tc_constraint)
		).

:- type tc_type == rtti_maybe_pseudo_type_info.

	% This type corresponds to the C type MR_TypeClassConstraint_NStruct,
	% where N is the length of the list in the tcc_types field.
:- type tc_constraint
	--->	tc_constraint(
			tcc_class_name		:: tc_name,
			tcc_types		:: list(tc_type)
		).

	% Uniquely identifies an instance declaration, and gives information
	% about the declaration that we need to interpret other data
	% structures related to the type class.
	%
	% This type corresponds to the C type MR_Instance.
:- type tc_instance
	--->	tc_instance(
			tci_type_class		:: tc_name,
			tci_types		:: list(tc_type),
			tci_num_type_vars	:: int,
			tci_constraints		:: list(tc_constraint),
			tci_methods		:: list(rtti_proc_label)
		).

	% This type corresponds to the C type MR_ClassDict.
	%
	% XXX We don't yet use this type.
:- type tc_dict
	--->	tc_dict(
			tcd_class		:: tc_name,
			tcd_types		:: list(rtti_type_info),
			tcd_methods		:: list(rtti_proc_label)
		).

%-----------------------------------------------------------------------------%
%
% The data structures representing the top-level global data structures
% generated by the Mercury compiler. Usually readonly, with one exception:
% data containing code addresses must be initialized at runtime in grades
% that don't support static code initializers.

:- type rtti_data
	--->	type_ctor_info(
			type_ctor_data
		)
	;	type_info(
			rtti_type_info
		)
	;	pseudo_type_info(
			rtti_pseudo_type_info
		)
	;	base_typeclass_info(
			tc_name,	% identifies the type class
			module_name,	% module containing instance decl.
			string,		% encodes the names and arities of the
					% types in the instance declaration

			base_typeclass_info
		)
	;	type_class_decl(
			tc_decl
		)
	;	type_class_instance(
			tc_instance
		)

		% A procedure to be called top-down by Aditi when
		% evaluating a join condition. These procedures
		% only have one input and one output argument,
		% both of which must have a ground {}/N type.
	;       aditi_proc_info(
			rtti_proc_label,	% The procedure to call.
			rtti_type_info,		% Type of the input argument.
			rtti_type_info		% Type of the output argument.
		).

% All rtti_data data structures and all their components are identified
% by an rtti_id. For data structures that are part of the description
% of a single type constructor, we use the ctor_rtti_id functor, and make the
% id of that type constructor part of the id of the data structure.
% For data structures that are not necessarily associated with a single type,
% which for the foreseeable future are all associated with typeclasses,
% we use the tc_rtti_id functor.

:- type rtti_id
	--->	ctor_rtti_id(rtti_type_ctor, ctor_rtti_name)
	;	tc_rtti_id(tc_name, tc_rtti_name)
	;	aditi_rtti_id(rtti_proc_label).

:- type ctor_rtti_name
	--->	exist_locns(int)		% functor ordinal
	;	exist_locn
	;	exist_tc_constr(int, int, int)	% functor ordinal,
						% constraint ordinal,
						% constraint arity
	;	exist_tc_constrs(int)		% functor ordinal
	;	exist_info(int)			% functor ordinal
	;	field_names(int)		% functor ordinal
	;	field_types(int)		% functor ordinal
	;	res_addrs
	;	res_addr_functors
	;	enum_functor_desc(int)		% functor ordinal
	;	notag_functor_desc
	;	du_functor_desc(int)		% functor ordinal
	;	res_functor_desc(int)		% functor ordinal
	;	enum_name_ordered_table
	;	enum_value_ordered_table
	;	du_name_ordered_table
	;	du_stag_ordered_table(int)	% primary tag
	;	du_ptag_ordered_table
	;	du_ptag_layout(int)		% primary tag
	;	res_value_ordered_table
	;	res_name_ordered_table
	;	maybe_res_addr_functor_desc
	;	type_functors
	;	type_layout
	;	type_ctor_info
	;	type_info(rtti_type_info)
	;	pseudo_type_info(rtti_pseudo_type_info)
	;	type_hashcons_pointer.

:- type tc_rtti_name
	--->	base_typeclass_info(
			module_name,	% module containing instance decl.
			string		% encodes the names and arities of the
					% types in the instance declaration
		)
	;	type_class_id
	;	type_class_id_var_names
	;	type_class_id_method_ids
	;	type_class_decl
	;	type_class_decl_super(int, int)
			% superclass ordinal, constraint arity
	;	type_class_decl_supers
	;	type_class_instance(list(tc_type))
	;	type_class_instance_tc_type_vector(list(tc_type))
	;	type_class_instance_constraint(list(tc_type),
			int, int)
			% constraint ordinal, constraint arity
	;	type_class_instance_constraints(list(tc_type))
	;	type_class_instance_methods(list(tc_type)).

%-----------------------------------------------------------------------------%
%
% The functions operating on RTTI data.

:- func encode_type_ctor_flags(set(type_ctor_flag)) = int.

	% Return the id of the type constructor.
:- func tcd_get_rtti_type_ctor(type_ctor_data) = rtti_type_ctor.

	% Convert a rtti_data to an rtti_id.
	% This calls error/1 if the argument is a type_var/1 rtti_data,
	% since there is no rtti_id to return in that case.
:- pred rtti_data_to_id(rtti_data::in, rtti_id::out) is det.

	% Convert an id that specifies a kind of variable arity type_info
	% or pseudo_type_info into the type_ctor of the canonical (arity-zero)
	% type of that kind.
:- func var_arity_id_to_rtti_type_ctor(var_arity_ctor_id) = rtti_type_ctor.

:- type rtti_id_maybe_element
	--->	item_type(rtti_id)
		% The type is the type of the data structure identified by the
		% rtti_id.
	;	element_type(rtti_id).
		% The type is the type of the elements of the data structure
		% identified by the rtti_id, which must be an array.

	% Return yes iff the specified entity is an array.
:- func rtti_id_maybe_element_has_array_type(rtti_id_maybe_element) = bool.
:- func rtti_id_has_array_type(rtti_id) = bool.
:- func ctor_rtti_name_has_array_type(ctor_rtti_name) = bool.
:- func tc_rtti_name_has_array_type(tc_rtti_name) = bool.

	% Return yes iff the specified entity should be exported
	% for use by other modules.
:- func rtti_id_is_exported(rtti_id) = bool.
:- func ctor_rtti_name_is_exported(ctor_rtti_name) = bool.
:- func tc_rtti_name_is_exported(tc_rtti_name) = bool.

	% Construct an rtti_proc_label for a given procedure.
:- func rtti__make_rtti_proc_label(module_info, pred_id, proc_id)
	= rtti_proc_label.

	% The inverse of rtti__make_rtti_proc_label.
:- pred rtti__proc_label_pred_proc_id(rtti_proc_label::in,
	pred_id::out, proc_id::out) is det.

	% Construct an aditi_proc_info for a given procedure.
:- func make_aditi_proc_info(module_info, pred_id, proc_id) = rtti_data.

	% Return the C variable name of the RTTI data structure identified
	% by the input argument.
:- pred rtti__id_to_c_identifier(rtti_id::in, string::out) is det.

	% Return the C representation of a pred_or_func indication.
:- pred rtti__pred_or_func_to_string(pred_or_func::in, string::out) is det.

	% Return the C representation of a secondary tag location.
:- pred rtti__sectag_locn_to_string(sectag_locn::in, string::out) is det.

	% Return the C representation of a secondary tag location.
:- pred rtti__sectag_and_locn_to_locn_string(sectag_and_locn::in, string::out)
	is det.

	% Return the C representation of the type_ctor_rep value of the given
	% type_ctor.
:- pred rtti__type_ctor_rep_to_string(type_ctor_data::in, string::out)
	is det.

	% Return the rtti_data containing the given type_info.
:- func type_info_to_rtti_data(rtti_type_info) = rtti_data.

	% Return the rtti_data containing the given type_info or
	% pseudo_type_info.
:- func maybe_pseudo_type_info_to_rtti_data(rtti_maybe_pseudo_type_info)
	= rtti_data.

	% Return the rtti_data containing the given type_info or
	% pseudo_type_info or self.
:- func maybe_pseudo_type_info_or_self_to_rtti_data(
	rtti_maybe_pseudo_type_info_or_self) = rtti_data.

	% Given a type constructor with the given details, return the number
	% of primary tag values used by the type. The return value will be
	% negative if the type constructor doesn't reserve primary tags.
:- func type_ctor_details_num_ptags(type_ctor_details) = int.

	% Given a type constructor with the given details, return the number
	% of function symbols defined by the type. The return value will be
	% negative if the type constructor doesn't define any function symbols.
:- func type_ctor_details_num_functors(type_ctor_details) = int.

	% Extract the argument name (if any) from a du_arg_info.
:- func du_arg_info_name(du_arg_info) = maybe(string).

	% Extract the argument type from a du_arg_info.
:- func du_arg_info_type(du_arg_info) = rtti_maybe_pseudo_type_info_or_self.

	% If the given value is bound to yes, return its argument.
:- func project_yes(maybe(T)) = T is semidet.

	% Return the symbolic representation of the address of the given
	% functor descriptor.
:- func enum_functor_rtti_name(enum_functor) = ctor_rtti_name.
:- func du_functor_rtti_name(du_functor) = ctor_rtti_name.
:- func res_functor_rtti_name(reserved_functor) = ctor_rtti_name.
:- func maybe_res_functor_rtti_name(maybe_reserved_functor) = ctor_rtti_name.

	% Extract the reserved address from a reserved address functor
	% descriptor.
:- func res_addr_rep(reserved_functor) = reserved_address.

	% Reserved addresses can be numeric or symbolic. Succeed if the
	% one passed is numeric.
:- pred res_addr_is_numeric(reserved_address::in) is semidet.

        % Return true iff the given type of RTTI data structure includes
	% code addresses.
:- func rtti_id_would_include_code_addr(rtti_id) = bool.
:- func ctor_rtti_name_would_include_code_addr(ctor_rtti_name) = bool.
:- func tc_rtti_name_would_include_code_addr(tc_rtti_name) = bool.

        % Return true iff the given type_info's or pseudo_type_info's RTTI
	% data structure includes code addresses.
:- func type_info_would_incl_code_addr(rtti_type_info) = bool.
:- func pseudo_type_info_would_incl_code_addr(rtti_pseudo_type_info) = bool.

	% rtti_id_c_type(RttiId, Type, IsArray):
	%	To declare a variable of the type specified by RttiId,
	%	put Type before the name of the variable; if IsArray is true,
	%	also put "[]" after the name.
:- pred rtti_id_maybe_element_c_type(rtti_id_maybe_element::in, string::out,
	bool::out) is det.
:- pred rtti_id_c_type(rtti_id::in, string::out, bool::out) is det.
:- pred ctor_rtti_name_c_type(ctor_rtti_name::in, string::out, bool::out)
	is det.
:- pred tc_rtti_name_c_type(tc_rtti_name::in, string::out, bool::out)
	is det.

	% Analogous to rtti_id_c_type.
:- pred rtti_id_maybe_element_java_type(rtti_id_maybe_element::in, string::out,
	bool::out) is det.
:- pred rtti_id_java_type(rtti_id::in, string::out, bool::out) is det.
:- pred ctor_rtti_name_java_type(ctor_rtti_name::in, string::out, bool::out)
	is det.
:- pred tc_rtti_name_java_type(tc_rtti_name::in, string::out, bool::out)
	is det.

	% Given a type in a type vector in a type class instance declaration,
	% return its string encoding for use in RTTI data structures, e.g. as
	% part of C identifiers.
:- func rtti__encode_tc_instance_type(tc_type) = string.

	% Return yes iff the name of the given data structure should be module
	% qualified.
:- func module_qualify_name_of_rtti_id(rtti_id) = bool.
:- func module_qualify_name_of_ctor_rtti_name(ctor_rtti_name) = bool.
:- func module_qualify_name_of_tc_rtti_name(tc_rtti_name) = bool.

	% If the given rtti_id is implemented as a single MR_TypeCtorInfo,
	% return the identity of the type constructor.
:- pred rtti_id_emits_type_ctor_info(rtti_id::in, rtti_type_ctor::out)
	is semidet.

:- implementation.

:- import_module backend_libs__name_mangle.
:- import_module backend_libs__proc_label.
:- import_module backend_libs__pseudo_type_info.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module parse_tree__prog_util.	% for mercury_public_builtin_module
:- import_module parse_tree__prog_out.

:- import_module int, string, require, varset.

encode_type_ctor_flags(FlagSet) =
		list__foldl(encode_type_ctor_flag, FlagList, 0) :-
	set__to_sorted_list(FlagSet, FlagList).

:- func encode_type_ctor_flag(type_ctor_flag, int) = int.

	% The encoding here must match the one in runtime/mercury_type_info.h.
encode_type_ctor_flag(reserve_tag_flag, N) 		= N + 1.
encode_type_ctor_flag(variable_arity_flag, N)		= N + 2.
encode_type_ctor_flag(kind_of_du_flag, N)		= N + 4.
encode_type_ctor_flag(typeinfo_fake_arity_flag, N)	= N + 8.

rtti_data_to_id(type_ctor_info(TypeCtorData),
		ctor_rtti_id(RttiTypeCtor, type_ctor_info)) :-
	RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData).
rtti_data_to_id(type_info(TypeInfo),
		ctor_rtti_id(RttiTypeCtor, type_info(TypeInfo))) :-
	RttiTypeCtor = ti_get_rtti_type_ctor(TypeInfo).
rtti_data_to_id(pseudo_type_info(PseudoTypeInfo),
		ctor_rtti_id(RttiTypeCtor, pseudo_type_info(PseudoTypeInfo))) :-
	RttiTypeCtor = pti_get_rtti_type_ctor(PseudoTypeInfo).
rtti_data_to_id(base_typeclass_info(TCName, Module, Instance, _),
		tc_rtti_id(TCName, base_typeclass_info(Module, Instance))).
rtti_data_to_id(type_class_decl(tc_decl(TCId, _, _)),
		tc_rtti_id(TCName, type_class_decl)) :-
	TCId = tc_id(TCName, _, _).
rtti_data_to_id(type_class_instance(tc_instance(TCName, TCTypes, _, _, _)),
		tc_rtti_id(TCName, type_class_instance(TCTypes))).
rtti_data_to_id(aditi_proc_info(ProcLabel, _, _), aditi_rtti_id(ProcLabel)).

tcd_get_rtti_type_ctor(TypeCtorData) = RttiTypeCtor :-
	ModuleName = TypeCtorData ^ tcr_module_name,
	TypeName = TypeCtorData ^ tcr_type_name,
	Arity = TypeCtorData ^ tcr_arity,
	RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, Arity).

:- func maybe_pseudo_get_rtti_type_ctor(rtti_maybe_pseudo_type_info)
	= rtti_type_ctor.

maybe_pseudo_get_rtti_type_ctor(plain(TypeInfo)) =
	ti_get_rtti_type_ctor(TypeInfo).
maybe_pseudo_get_rtti_type_ctor(pseudo(PseudoTypeInfo)) =
	pti_get_rtti_type_ctor(PseudoTypeInfo).

:- func ti_get_rtti_type_ctor(rtti_type_info) = rtti_type_ctor.

ti_get_rtti_type_ctor(plain_arity_zero_type_info(RttiTypeCtor))
	= RttiTypeCtor.
ti_get_rtti_type_ctor(plain_type_info(RttiTypeCtor, _))
	= RttiTypeCtor.
ti_get_rtti_type_ctor(var_arity_type_info(RttiVarArityId, _)) =
	var_arity_id_to_rtti_type_ctor(RttiVarArityId).

:- func pti_get_rtti_type_ctor(rtti_pseudo_type_info) = rtti_type_ctor.

pti_get_rtti_type_ctor(plain_arity_zero_pseudo_type_info(RttiTypeCtor))
	= RttiTypeCtor.
pti_get_rtti_type_ctor(plain_pseudo_type_info(RttiTypeCtor, _))
	= RttiTypeCtor.
pti_get_rtti_type_ctor(var_arity_pseudo_type_info(RttiVarArityId, _)) =
	var_arity_id_to_rtti_type_ctor(RttiVarArityId).
pti_get_rtti_type_ctor(type_var(_)) = _ :-
	% there's no rtti_type_ctor associated with a type_var
	error("rtti_data_to_name: type_var").

var_arity_id_to_rtti_type_ctor(pred_type_info) = Ctor :-
	mercury_public_builtin_module(Builtin),
	Ctor = rtti_type_ctor(Builtin, "pred", 0).
var_arity_id_to_rtti_type_ctor(func_type_info) = Ctor :-
	mercury_public_builtin_module(Builtin),
	Ctor = rtti_type_ctor(Builtin, "func", 0).
var_arity_id_to_rtti_type_ctor(tuple_type_info) = Ctor :-
	mercury_public_builtin_module(Builtin),
	Ctor = rtti_type_ctor(Builtin, "tuple", 0).

rtti_id_maybe_element_has_array_type(item_type(RttiId)) =
	rtti_id_has_array_type(RttiId).
rtti_id_maybe_element_has_array_type(element_type(RttiId)) = no :-
	require(unify(rtti_id_has_array_type(RttiId), yes),
		"rtti_id_maybe_element_has_array_type: base is not array").

rtti_id_has_array_type(ctor_rtti_id(_, RttiName)) =
	ctor_rtti_name_has_array_type(RttiName).
rtti_id_has_array_type(tc_rtti_id(_, TCRttiName)) =
	tc_rtti_name_has_array_type(TCRttiName).
rtti_id_has_array_type(aditi_rtti_id(_)) = no.

ctor_rtti_name_has_array_type(RttiName) = IsArray :-
	ctor_rtti_name_type(RttiName, _, IsArray).

tc_rtti_name_has_array_type(TCRttiName) = IsArray :-
	tc_rtti_name_type(TCRttiName, _, IsArray).

rtti_id_is_exported(ctor_rtti_id(_, RttiName)) =
	ctor_rtti_name_is_exported(RttiName).
rtti_id_is_exported(tc_rtti_id(_, TCRttiName)) =
	tc_rtti_name_is_exported(TCRttiName).
% MR_AditiProcInfos must be exported to be visible to dlsym().
rtti_id_is_exported(aditi_rtti_id(_)) = yes.

ctor_rtti_name_is_exported(exist_locns(_))		= no.
ctor_rtti_name_is_exported(exist_locn)			= no.
ctor_rtti_name_is_exported(exist_tc_constr(_, _, _))	= no.
ctor_rtti_name_is_exported(exist_tc_constrs(_))		= no.
ctor_rtti_name_is_exported(exist_info(_))       	= no.
ctor_rtti_name_is_exported(field_names(_))      	= no.
ctor_rtti_name_is_exported(field_types(_))      	= no.
ctor_rtti_name_is_exported(res_addrs)           	= no.
ctor_rtti_name_is_exported(res_addr_functors)   	= no.
ctor_rtti_name_is_exported(enum_functor_desc(_))	= no.
ctor_rtti_name_is_exported(notag_functor_desc)  	= no.
ctor_rtti_name_is_exported(du_functor_desc(_))  	= no.
ctor_rtti_name_is_exported(res_functor_desc(_)) 	= no.
ctor_rtti_name_is_exported(enum_name_ordered_table)     = no.
ctor_rtti_name_is_exported(enum_value_ordered_table)    = no.
ctor_rtti_name_is_exported(du_name_ordered_table)       = no.
ctor_rtti_name_is_exported(du_stag_ordered_table(_))    = no.
ctor_rtti_name_is_exported(du_ptag_ordered_table)       = no.
ctor_rtti_name_is_exported(du_ptag_layout(_))   	= no.
ctor_rtti_name_is_exported(res_value_ordered_table)     = no.
ctor_rtti_name_is_exported(res_name_ordered_table)      = no.
ctor_rtti_name_is_exported(maybe_res_addr_functor_desc) = no.
ctor_rtti_name_is_exported(type_functors)       	= no.
ctor_rtti_name_is_exported(type_layout)         	= no.
ctor_rtti_name_is_exported(type_ctor_info)      	= yes.
ctor_rtti_name_is_exported(type_info(TypeInfo)) =
	type_info_is_exported(TypeInfo).
ctor_rtti_name_is_exported(pseudo_type_info(PseudoTypeInfo)) =
	pseudo_type_info_is_exported(PseudoTypeInfo).
ctor_rtti_name_is_exported(type_hashcons_pointer)       = no.

tc_rtti_name_is_exported(base_typeclass_info(_, _)) = yes.
tc_rtti_name_is_exported(type_class_id) = no.
tc_rtti_name_is_exported(type_class_id_var_names) = no.
tc_rtti_name_is_exported(type_class_id_method_ids) = no.
tc_rtti_name_is_exported(type_class_decl) = yes.
tc_rtti_name_is_exported(type_class_decl_super(_, _)) = no.
tc_rtti_name_is_exported(type_class_decl_supers) = no.
tc_rtti_name_is_exported(type_class_instance(_)) = yes.
tc_rtti_name_is_exported(type_class_instance_tc_type_vector(_)) = no.
tc_rtti_name_is_exported(type_class_instance_constraint(_, _, _)) = no.
tc_rtti_name_is_exported(type_class_instance_constraints(_)) = no.
tc_rtti_name_is_exported(type_class_instance_methods(_)) = no.

:- func type_info_is_exported(rtti_type_info) = bool.

type_info_is_exported(plain_arity_zero_type_info(_)) = yes.
type_info_is_exported(plain_type_info(_, _))	     = no.
type_info_is_exported(var_arity_type_info(_, _))     = no.

:- func pseudo_type_info_is_exported(rtti_pseudo_type_info) = bool.

pseudo_type_info_is_exported(plain_arity_zero_pseudo_type_info(_)) = yes.
pseudo_type_info_is_exported(plain_pseudo_type_info(_, _))	= no.
pseudo_type_info_is_exported(var_arity_pseudo_type_info(_, _))	= no.
pseudo_type_info_is_exported(type_var(_))			= no.

rtti__make_rtti_proc_label(ModuleInfo, PredId, ProcId) = ProcLabel :-
	module_info_name(ModuleInfo, ThisModule),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	PredModule = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),
	pred_info_arg_types(PredInfo, ArgTypes),
	proc_info_varset(ProcInfo, ProcVarSet),
	proc_info_headvars(ProcInfo, ProcHeadVars),
	proc_info_argmodes(ProcInfo, ProcModes),
	proc_info_interface_determinism(ProcInfo, ProcDetism),
	modes_to_arg_modes(ModuleInfo, ProcModes, ArgTypes, ProcArgModes),
	PredIsImported = (pred_info_is_imported(PredInfo) -> yes ; no),
	PredIsPseudoImp = (pred_info_is_pseudo_imported(PredInfo) -> yes ; no),
	ProcIsExported = (procedure_is_exported(ModuleInfo, PredInfo, ProcId)
		-> yes ; no),
	pred_info_get_maybe_special_pred(PredInfo, PredMaybeSpecial),
	ProcHeadVarsWithNames = list__map((func(Var) = Var - Name :-
			Name = varset__lookup_name(ProcVarSet, Var)
		), ProcHeadVars),
	(
		(
			PredIsImported = yes
		;
			PredIsPseudoImp = yes,
			hlds_pred__in_in_unification_proc_id(ProcId)
		)
	->
		ProcIsImported = yes
	;
		ProcIsImported = no
	),
	ProcLabel = rtti_proc_label(PredOrFunc, ThisModule, PredModule,
		PredName, Arity, ArgTypes, PredId, ProcId,
		ProcHeadVarsWithNames, ProcArgModes, ProcDetism,
		PredIsImported, PredIsPseudoImp, PredMaybeSpecial,
		ProcIsExported, ProcIsImported).

rtti__proc_label_pred_proc_id(ProcLabel, PredId, ProcId) :-
	PredId = ProcLabel ^ pred_id,
	ProcId = ProcLabel ^ proc_id.

make_aditi_proc_info(ModuleInfo, PredId, ProcId) =
		aditi_proc_info(ProcLabel, InputTypeInfo, OutputTypeInfo) :-
	ProcLabel = rtti__make_rtti_proc_label(ModuleInfo, PredId, ProcId),

	% The types of the arguments must be ground.
	( ProcLabel ^ proc_arg_types = [InputArgType, OutputArgType] ->
		pseudo_type_info__construct_type_info(
			InputArgType, InputTypeInfo),
		pseudo_type_info__construct_type_info(
			OutputArgType, OutputTypeInfo)
	;
		error("make_aditi_proc_info: incorrect number of arguments")
	).

rtti__id_to_c_identifier(ctor_rtti_id(RttiTypeCtor, RttiName), Str) :-
	rtti__name_to_string(RttiTypeCtor, RttiName, Str).
rtti__id_to_c_identifier(tc_rtti_id(TCName, TCRttiName), Str) :-
	rtti__tc_name_to_string(TCName, TCRttiName, Str).
rtti__id_to_c_identifier(aditi_rtti_id(RttiProcLabel), Str) :-
    Str = "AditiProcInfo_For_" ++
	proc_label_to_c_string(make_proc_label_from_rtti(RttiProcLabel), no).

:- pred rtti__name_to_string(rtti_type_ctor::in, ctor_rtti_name::in,
	string::out) is det.

rtti__name_to_string(RttiTypeCtor, RttiName, Str) :-
	rtti__mangle_rtti_type_ctor(RttiTypeCtor, ModuleName, TypeName, A_str),
	(
		RttiName = exist_locns(Ordinal),
		string__int_to_string(Ordinal, O_str),
		string__append_list([ModuleName, "__exist_locns_",
			TypeName, "_", A_str, "_", O_str], Str)
	;
		RttiName = exist_locn,
		string__append_list([ModuleName, "__exist_locn_",
			TypeName, "_", A_str], Str)
	;
		RttiName = exist_tc_constr(Ordinal, TCCNum, _),
		string__int_to_string(Ordinal, O_str),
		string__int_to_string(TCCNum, N_str),
		string__append_list([ModuleName, "__exist_tc_constr_",
			TypeName, "_", A_str, "_", O_str, "_", N_str], Str)
	;
		RttiName = exist_tc_constrs(Ordinal),
		string__int_to_string(Ordinal, O_str),
		string__append_list([ModuleName, "__exist_tc_constrs_",
			TypeName, "_", A_str, "_", O_str], Str)
	;
		RttiName = exist_info(Ordinal),
		string__int_to_string(Ordinal, O_str),
		string__append_list([ModuleName, "__exist_info_",
			TypeName, "_", A_str, "_", O_str], Str)
	;
		RttiName = field_names(Ordinal),
		string__int_to_string(Ordinal, O_str),
		string__append_list([ModuleName, "__field_names_",
			TypeName, "_", A_str, "_", O_str], Str)
	;
		RttiName = field_types(Ordinal),
		string__int_to_string(Ordinal, O_str),
		string__append_list([ModuleName, "__field_types_",
			TypeName, "_", A_str, "_", O_str], Str)
	;
		RttiName = res_addrs,
		string__append_list([ModuleName, "__reserved_addrs_",
			TypeName, "_", A_str], Str)
	;
		RttiName = res_addr_functors,
		string__append_list([ModuleName, "__reserved_addr_functors_",
			TypeName, "_", A_str], Str)
	;
		RttiName = enum_functor_desc(Ordinal),
		string__int_to_string(Ordinal, O_str),
		string__append_list([ModuleName, "__enum_functor_desc_",
			TypeName, "_", A_str, "_", O_str], Str)
	;
		RttiName = notag_functor_desc,
		string__append_list([ModuleName, "__notag_functor_desc_",
			TypeName, "_", A_str], Str)
	;
		RttiName = du_functor_desc(Ordinal),
		string__int_to_string(Ordinal, O_str),
		string__append_list([ModuleName, "__du_functor_desc_",
			TypeName, "_", A_str, "_", O_str], Str)
	;
		RttiName = res_functor_desc(Ordinal),
		string__int_to_string(Ordinal, O_str),
		string__append_list([ModuleName,
			"__reserved_addr_functor_desc_",
			TypeName, "_", A_str, "_", O_str], Str)
	;
		RttiName = enum_name_ordered_table,
		string__append_list([ModuleName, "__enum_name_ordered_",
			TypeName, "_", A_str], Str)
	;
		RttiName = enum_value_ordered_table,
		string__append_list([ModuleName, "__enum_value_ordered_",
			TypeName, "_", A_str], Str)
	;
		RttiName = du_name_ordered_table,
		string__append_list([ModuleName, "__du_name_ordered_",
			TypeName, "_", A_str], Str)
	;
		RttiName = du_stag_ordered_table(Ptag),
		string__int_to_string(Ptag, P_str),
		string__append_list([ModuleName, "__du_stag_ordered_",
			TypeName, "_", A_str, "_", P_str], Str)
	;
		RttiName = du_ptag_ordered_table,
		string__append_list([ModuleName, "__du_ptag_ordered_",
			TypeName, "_", A_str], Str)
	;
		RttiName = du_ptag_layout(Ptag),
		string__int_to_string(Ptag, P_str),
		string__append_list([ModuleName,
			"__du_ptag_layout_",
			TypeName, "_", A_str, "_", P_str], Str)
	;
		RttiName = res_value_ordered_table,
		string__append_list([ModuleName, "__res_layout_ordered_table_",
			TypeName, "_", A_str], Str)
	;
		RttiName = res_name_ordered_table,
		string__append_list([ModuleName, "__res_name_ordered_table_",
			TypeName, "_", A_str], Str)
	;
		RttiName = maybe_res_addr_functor_desc,
		string__append_list([ModuleName,
			"__maybe_res_addr_functor_desc_",
			TypeName, "_", A_str], Str)
	;
		RttiName = type_functors,
		string__append_list([ModuleName, "__type_functors",
			TypeName, "_", A_str], Str)
	;
		RttiName = type_layout,
		string__append_list([ModuleName, "__type_layout",
			TypeName, "_", A_str], Str)
	;
		RttiName = type_ctor_info,
		string__append_list([ModuleName, "__type_ctor_info_",
			TypeName, "_", A_str], Str)
	;
		RttiName = type_info(TypeInfo),
		Str = rtti__type_info_to_string(TypeInfo)
	;
		RttiName = pseudo_type_info(PseudoTypeInfo),
		Str = rtti__pseudo_type_info_to_string(PseudoTypeInfo)
	;
		RttiName = type_hashcons_pointer,
		string__append_list([ModuleName, "__hashcons_ptr_",
			TypeName, "_", A_str], Str)
	).

:- pred rtti__tc_name_to_string(tc_name::in, tc_rtti_name::in, string::out)
	is det.

rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = base_typeclass_info(_ModuleName, InstanceStr),
	Str = make_base_typeclass_info_name(TCName, InstanceStr).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_id,
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	string__append_list([ModuleName, "__type_class_id_",
		ClassName, "_", ArityStr], Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_id_method_ids,
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	string__append_list([ModuleName, "__type_class_id_method_ids_",
		ClassName, "_", ArityStr], Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_id_var_names,
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	string__append_list([ModuleName, "__type_class_id_var_names_",
		ClassName, "_", ArityStr], Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_decl,
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	string__append_list([ModuleName, "__type_class_decl_",
		ClassName, "_", ArityStr], Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_decl_supers,
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	string__append_list([ModuleName, "__type_class_decl_supers_",
		ClassName, "_", ArityStr], Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_decl_super(Ordinal, _),
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	string__int_to_string(Ordinal, OrdinalStr),
	string__append_list([ModuleName, "__type_class_decl_super_",
		ClassName, "_", ArityStr, "_", OrdinalStr], Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_instance(TCTypes),
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	TypeStrs = list__map(rtti__encode_tc_instance_type, TCTypes),
	TypeVectorStr = string__append_list(TypeStrs),
	string__append_list([ModuleName, "__type_class_instance_",
		ClassName, "_", ArityStr, "_", TypeVectorStr], Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_instance_tc_type_vector(TCTypes),
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	TypeStrs = list__map(rtti__encode_tc_instance_type, TCTypes),
	TypeVectorStr = string__append_list(TypeStrs),
	string__append_list([ModuleName,
		"__type_class_instance_tc_type_vector_",
		ClassName, "_", ArityStr, "_", TypeVectorStr], Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_instance_constraint(TCTypes, Ordinal, _),
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	TypeStrs = list__map(rtti__encode_tc_instance_type, TCTypes),
	TypeVectorStr = string__append_list(TypeStrs),
	string__int_to_string(Ordinal, OrdinalStr),
	string__append_list([ModuleName, "__type_class_instance_constraint_",
		ClassName, "_", ArityStr, "_", OrdinalStr, "_", TypeVectorStr],
		Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_instance_constraints(TCTypes),
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	TypeStrs = list__map(rtti__encode_tc_instance_type, TCTypes),
	TypeVectorStr = string__append_list(TypeStrs),
	string__append_list([ModuleName, "__type_class_instance_constraints_",
		ClassName, "_", ArityStr, "_", TypeVectorStr], Str).
rtti__tc_name_to_string(TCName, TCRttiName, Str) :-
	TCRttiName = type_class_instance_methods(TCTypes),
	rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName,
		ArityStr),
	TypeStrs = list__map(rtti__encode_tc_instance_type, TCTypes),
	TypeVectorStr = string__append_list(TypeStrs),
	string__append_list([ModuleName, "__type_class_instance_methods_",
		ClassName, "_", ArityStr, "_", TypeVectorStr], Str).

% The encoding we use here depends on the types in instance declarations
% being type constructors applied to vectors of distinct variables. When
% we lift that restriction, we will have to change this scheme.
%
% The code here is based on the code of base_typeclass_info__type_to_string,
% but its input is of type `maybe_pseudo_type_info', not of type `type'.

rtti__encode_tc_instance_type(TCType) = Str :-
	(
		TCType = plain(TI),
		(
			TI = plain_arity_zero_type_info(RttiTypeCtor),
			ArgTIs = []
		;
			TI = plain_type_info(RttiTypeCtor, ArgTIs)
		;
			TI = var_arity_type_info(VarArityId, ArgTIs),
			RttiTypeCtor =
				var_arity_id_to_rtti_type_ctor(VarArityId)
		),
		Arity = list__length(ArgTIs)
		% XXX We may wish to check that all arguments are variables.
		% (possible only if Arity = 0)
	;
		TCType = pseudo(PTI),
		(
			PTI = plain_arity_zero_pseudo_type_info(RttiTypeCtor),
			ArgPTIs = []
		;
			PTI = plain_pseudo_type_info(RttiTypeCtor, ArgPTIs)
		;
			PTI = var_arity_pseudo_type_info(VarArityId, ArgPTIs),
			RttiTypeCtor =
				var_arity_id_to_rtti_type_ctor(VarArityId)
		;
			PTI = type_var(_),
			error("rtti__encode_tc_instance_type: type_var")
		),
		Arity = list__length(ArgPTIs)
		% XXX We may wish to check that all arguments are variables.
	),
	RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, _CtorArity),
	prog_out__sym_name_to_string(qualified(ModuleName, TypeName), "__",
		TypeStr),
	string__int_to_string(Arity, ArityStr),
	% XXX This naming scheme is the same as for base_typeclass_infos.
	% We should think about
	% - whether encoding guarantees different names for different instance
	%   declarations;
	% - whether the encoding is uniquely invertible, and
	% - whether the encoding may ever need to be uniquely invertible.
	string__append_list([TypeStr, "__arity", ArityStr, "__"], Str).

:- pred rtti__mangle_rtti_type_ctor(rtti_type_ctor::in,
	string::out, string::out, string::out) is det.

rtti__mangle_rtti_type_ctor(RttiTypeCtor, ModuleName, TypeName, ArityStr) :-
	RttiTypeCtor = rtti_type_ctor(ModuleNameSym0, TypeName0, TypeArity),
	% This predicate will be invoked only at stages of compilation
	% that are after everything has been module qualified. The only
	% things with an empty module name should be the builtins.
	( ModuleNameSym0 = unqualified("") ->
		mercury_public_builtin_module(ModuleNameSym)
	;
		ModuleNameSym = ModuleNameSym0
	),
	ModuleName = sym_name_mangle(ModuleNameSym),
	TypeName = name_mangle(TypeName0),
	string__int_to_string(TypeArity, ArityStr).

:- pred rtti__mangle_rtti_type_class_name(tc_name::in,
	string::out, string::out, string::out) is det.

rtti__mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr) :-
	TCName = tc_name(ModuleNameSym, ClassName0, Arity),
	ModuleName = sym_name_mangle(ModuleNameSym),
	ClassName = name_mangle(ClassName0),
	string__int_to_string(Arity, ArityStr).

%-----------------------------------------------------------------------------%

:- func rtti__type_info_to_string(rtti_type_info) = string.

rtti__type_info_to_string(TypeInfo) = Str :-
	(
		TypeInfo = plain_arity_zero_type_info(RttiTypeCtor),
		rtti__id_to_c_identifier(
			ctor_rtti_id(RttiTypeCtor, type_ctor_info), Str)
	;
		TypeInfo = plain_type_info(RttiTypeCtor, Args),
		rtti__mangle_rtti_type_ctor(RttiTypeCtor,
			ModuleName, TypeName, ArityStr),
		ArgsStr = type_info_list_to_string(Args),
		string__append_list([ModuleName, "__ti_",
			TypeName, "_", ArityStr, ArgsStr], Str)
	;
		TypeInfo = var_arity_type_info(VarArityId, Args),
		RealArity = list__length(Args),
		ArgsStr = type_info_list_to_string(Args),
		string__int_to_string(RealArity, RealArityStr),
		IdStr = var_arity_ctor_id_to_string(VarArityId),
		string__append_list(["__vti_", IdStr, "_",
			RealArityStr, ArgsStr], Str)
	).

:- func rtti__pseudo_type_info_to_string(rtti_pseudo_type_info) = string.

rtti__pseudo_type_info_to_string(PseudoTypeInfo) = Str :-
	(
		PseudoTypeInfo =
			plain_arity_zero_pseudo_type_info(RttiTypeCtor),
		rtti__id_to_c_identifier(
			ctor_rtti_id(RttiTypeCtor, type_ctor_info), Str)
	;
		PseudoTypeInfo = plain_pseudo_type_info(RttiTypeCtor, Args),
		rtti__mangle_rtti_type_ctor(RttiTypeCtor,
			ModuleName, TypeName, ArityStr),
		ArgsStr = maybe_pseudo_type_info_list_to_string(Args),
		string__append_list([ModuleName, "__pti_",
			TypeName, "_", ArityStr, ArgsStr], Str)
	;
		PseudoTypeInfo = var_arity_pseudo_type_info(VarArityId, Args),
		RealArity = list__length(Args),
		ArgsStr = maybe_pseudo_type_info_list_to_string(Args),
		string__int_to_string(RealArity, RealArityStr),
		IdStr = var_arity_ctor_id_to_string(VarArityId),
		string__append_list(["__vpti_", IdStr, "_",
			RealArityStr, ArgsStr], Str)
	;
		PseudoTypeInfo = type_var(VarNum),
		string__int_to_string(VarNum, Str)
	).

:- func maybe_pseudo_type_info_to_string(rtti_maybe_pseudo_type_info) = string.

maybe_pseudo_type_info_to_string(plain(TypeInfo)) =
	string__append("__plain_", type_info_to_string(TypeInfo)).
maybe_pseudo_type_info_to_string(pseudo(PseudoTypeInfo)) =
	string__append("__pseudo_", pseudo_type_info_to_string(PseudoTypeInfo)).

:- func var_arity_ctor_id_to_string(var_arity_ctor_id) = string.

var_arity_ctor_id_to_string(pred_type_info) = "pred".
var_arity_ctor_id_to_string(func_type_info) = "func".
var_arity_ctor_id_to_string(tuple_type_info) = "tuple".

%-----------------------------------------------------------------------------%

:- func maybe_pseudo_type_info_list_to_string(list(rtti_maybe_pseudo_type_info))
	= string.

maybe_pseudo_type_info_list_to_string(MaybePseudoTypeInfoList) =
	string__append_list(
		list__map(maybe_pseudo_type_info_to_string,
			MaybePseudoTypeInfoList)).

:- func pseudo_type_info_list_to_string(list(rtti_pseudo_type_info)) = string.

pseudo_type_info_list_to_string(PseudoTypeInfoList) =
	string__append_list(
		list__map(pseudo_type_info_to_string, PseudoTypeInfoList)).

:- func type_info_list_to_string(list(rtti_type_info)) = string.

type_info_list_to_string(TypeInfoList) =
	string__append_list(list__map(type_info_to_string, TypeInfoList)).

%-----------------------------------------------------------------------------%

rtti__pred_or_func_to_string(predicate, "MR_PREDICATE").
rtti__pred_or_func_to_string(function,  "MR_FUNCTION").

rtti__sectag_locn_to_string(sectag_none,   "MR_SECTAG_NONE").
rtti__sectag_locn_to_string(sectag_local,  "MR_SECTAG_LOCAL").
rtti__sectag_locn_to_string(sectag_remote, "MR_SECTAG_REMOTE").

rtti__sectag_and_locn_to_locn_string(sectag_none,      "MR_SECTAG_NONE").
rtti__sectag_and_locn_to_locn_string(sectag_local(_),  "MR_SECTAG_LOCAL").
rtti__sectag_and_locn_to_locn_string(sectag_remote(_), "MR_SECTAG_REMOTE").

rtti__type_ctor_rep_to_string(TypeCtorData, RepStr) :-
	TypeCtorDetails = TypeCtorData ^ tcr_rep_details,
	(
		TypeCtorDetails = enum(TypeCtorUserEq, _, _, _),
		(
			TypeCtorUserEq = standard,
			RepStr = "MR_TYPECTOR_REP_ENUM"
		;
			TypeCtorUserEq = user_defined,
			RepStr = "MR_TYPECTOR_REP_ENUM_USEREQ"
		)
	;
		TypeCtorDetails = du(TypeCtorUserEq, _, _, _),
		(
			TypeCtorUserEq = standard,
			RepStr = "MR_TYPECTOR_REP_DU"
		;
			TypeCtorUserEq = user_defined,
			RepStr = "MR_TYPECTOR_REP_DU_USEREQ"
		)
	;
		TypeCtorDetails = reserved(TypeCtorUserEq, _, _, _, _),
		(
			TypeCtorUserEq = standard,
			RepStr = "MR_TYPECTOR_REP_RESERVED_ADDR"
		;
			TypeCtorUserEq = user_defined,
			RepStr = "MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ"
		)
	;
		TypeCtorDetails = notag(TypeCtorUserEq, NotagFunctor),
		NotagEqvType = NotagFunctor ^ nt_arg_type,
		(
			TypeCtorUserEq = standard,
			(
				NotagEqvType = pseudo(_),
				RepStr = "MR_TYPECTOR_REP_NOTAG"
			;
				NotagEqvType = plain(_),
				RepStr = "MR_TYPECTOR_REP_NOTAG_GROUND"
			)
		;
			TypeCtorUserEq = user_defined,
			(
				NotagEqvType = pseudo(_),
				RepStr = "MR_TYPECTOR_REP_NOTAG_USEREQ"
			;
				NotagEqvType = plain(_),
				RepStr = "MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ"
			)
		)
	;
		TypeCtorDetails = eqv(EqvType),
		(
			EqvType = pseudo(_),
			RepStr = "MR_TYPECTOR_REP_EQUIV"
		;
			EqvType = plain(_),
			RepStr = "MR_TYPECTOR_REP_EQUIV_GROUND"
		)
	;
		TypeCtorDetails = builtin(BuiltinCtor),
		builtin_ctor_rep_to_string(BuiltinCtor, RepStr)
	;
		TypeCtorDetails = impl_artifact(ImplCtor),
		impl_ctor_rep_to_string(ImplCtor, RepStr)
	;
		TypeCtorDetails = foreign(IsStable),
		(
			type_ctor_is_array(
				qualified(TypeCtorData ^ tcr_module_name,
					TypeCtorData ^ tcr_type_name) -
					TypeCtorData ^ tcr_arity)
		->
			% XXX This is a kludge to allow accurate GC
			% to trace arrays. We should allow users to
			% provide tracing functions for foreign types.
			RepStr = "MR_TYPECTOR_REP_ARRAY"
		;
			(
				IsStable = is_stable,
				RepStr = "MR_TYPECTOR_REP_STABLE_FOREIGN"
			;
				IsStable = is_not_stable,
				RepStr = "MR_TYPECTOR_REP_FOREIGN"
			)
		)
	).

:- pred builtin_ctor_rep_to_string(builtin_ctor::in, string::out) is det.

builtin_ctor_rep_to_string(int, "MR_TYPECTOR_REP_INT").
builtin_ctor_rep_to_string(string, "MR_TYPECTOR_REP_STRING").
builtin_ctor_rep_to_string(float, "MR_TYPECTOR_REP_FLOAT").
builtin_ctor_rep_to_string(char, "MR_TYPECTOR_REP_CHAR").
builtin_ctor_rep_to_string(void, "MR_TYPECTOR_REP_VOID").
builtin_ctor_rep_to_string(c_pointer(is_not_stable),
	"MR_TYPECTOR_REP_C_POINTER").
builtin_ctor_rep_to_string(c_pointer(is_stable),
	"MR_TYPECTOR_REP_STABLE_C_POINTER").
builtin_ctor_rep_to_string(pred_ctor, "MR_TYPECTOR_REP_PRED").
builtin_ctor_rep_to_string(func_ctor, "MR_TYPECTOR_REP_FUNC").
builtin_ctor_rep_to_string(tuple, "MR_TYPECTOR_REP_TUPLE").
builtin_ctor_rep_to_string(ref, "MR_TYPECTOR_REP_REFERENCE").
builtin_ctor_rep_to_string(type_ctor_desc, "MR_TYPECTOR_REP_TYPECTORDESC").
builtin_ctor_rep_to_string(pseudo_type_desc, "MR_TYPECTOR_REP_PSEUDOTYPEDESC").
builtin_ctor_rep_to_string(type_desc, "MR_TYPECTOR_REP_TYPEDESC").

:- pred impl_ctor_rep_to_string(impl_ctor::in, string::out) is det.

impl_ctor_rep_to_string(type_ctor_info, "MR_TYPECTOR_REP_TYPECTORINFO").
impl_ctor_rep_to_string(type_info, "MR_TYPECTOR_REP_TYPEINFO").
impl_ctor_rep_to_string(typeclass_info, "MR_TYPECTOR_REP_TYPECLASSINFO").
impl_ctor_rep_to_string(base_typeclass_info,
	"MR_TYPECTOR_REP_BASETYPECLASSINFO").
impl_ctor_rep_to_string(hp, "MR_TYPECTOR_REP_HP").
impl_ctor_rep_to_string(succip, "MR_TYPECTOR_REP_SUCCIP").
impl_ctor_rep_to_string(curfr, "MR_TYPECTOR_REP_CURFR").
impl_ctor_rep_to_string(maxfr, "MR_TYPECTOR_REP_MAXFR").
impl_ctor_rep_to_string(redofr, "MR_TYPECTOR_REP_REDOFR").
impl_ctor_rep_to_string(redoip, "MR_TYPECTOR_REP_REDOIP").
impl_ctor_rep_to_string(trail_ptr, "MR_TYPECTOR_REP_TRAIL_PTR").
impl_ctor_rep_to_string(ticket, "MR_TYPECTOR_REP_TICKET").
impl_ctor_rep_to_string(subgoal, "MR_TYPECTOR_REP_SUBGOAL").

type_info_to_rtti_data(TypeInfo) = type_info(TypeInfo).

maybe_pseudo_type_info_to_rtti_data(pseudo(PseudoTypeInfo)) =
	pseudo_type_info(PseudoTypeInfo).
maybe_pseudo_type_info_to_rtti_data(plain(TypeInfo)) =
	type_info(TypeInfo).

maybe_pseudo_type_info_or_self_to_rtti_data(pseudo(PseudoTypeInfo)) =
	pseudo_type_info(PseudoTypeInfo).
maybe_pseudo_type_info_or_self_to_rtti_data(plain(TypeInfo)) =
	type_info(TypeInfo).
maybe_pseudo_type_info_or_self_to_rtti_data(self) =
	pseudo_type_info(type_var(0)).

type_ctor_details_num_ptags(enum(_, _, _, _)) = -1.
type_ctor_details_num_ptags(du(_, _, PtagMap, _)) = LastPtag + 1 :-
	map__keys(PtagMap, Ptags),
	list__last_det(Ptags, LastPtag).
type_ctor_details_num_ptags(reserved(_, _, _, PtagMap, _)) = NumPtags :-
	map__keys(PtagMap, Ptags),
	( Ptags = [] ->
		NumPtags = -1
	;
		list__last_det(Ptags, LastPtag),
		NumPtags = LastPtag + 1
	).
type_ctor_details_num_ptags(notag(_, _)) = -1.
type_ctor_details_num_ptags(eqv(_)) = -1.
type_ctor_details_num_ptags(builtin(_)) = -1.
type_ctor_details_num_ptags(impl_artifact(_)) = -1.
type_ctor_details_num_ptags(foreign(_)) = -1.

type_ctor_details_num_functors(enum(_, Functors, _, _)) =
	list__length(Functors).
type_ctor_details_num_functors(du(_, Functors, _, _)) =
	list__length(Functors).
type_ctor_details_num_functors(reserved(_, Functors, _, _, _)) =
	list__length(Functors).
type_ctor_details_num_functors(notag(_, _)) = 1.
type_ctor_details_num_functors(eqv(_)) = -1.
type_ctor_details_num_functors(builtin(_)) = -1.
type_ctor_details_num_functors(impl_artifact(_)) = -1.
type_ctor_details_num_functors(foreign(_)) = -1.

du_arg_info_name(ArgInfo) = ArgInfo ^ du_arg_name.

du_arg_info_type(ArgInfo) = ArgInfo ^ du_arg_type.

project_yes(yes(X)) = X.

enum_functor_rtti_name(EnumFunctor) =
	enum_functor_desc(EnumFunctor ^ enum_ordinal).

du_functor_rtti_name(DuFunctor) = du_functor_desc(DuFunctor ^ du_ordinal).

res_functor_rtti_name(ResFunctor) =
	res_functor_desc(ResFunctor ^ res_ordinal).

maybe_res_functor_rtti_name(du_func(DuFunctor)) =
	du_functor_desc(DuFunctor ^ du_ordinal).
maybe_res_functor_rtti_name(res_func(ResFunctor)) =
	res_functor_desc(ResFunctor ^ res_ordinal).

res_addr_rep(ResFunctor) = ResFunctor ^ res_rep.

res_addr_is_numeric(null_pointer).
res_addr_is_numeric(small_pointer(_)).

rtti_id_would_include_code_addr(ctor_rtti_id(_, RttiName)) =
	ctor_rtti_name_would_include_code_addr(RttiName).
rtti_id_would_include_code_addr(tc_rtti_id(_, TCRttiName)) =
	tc_rtti_name_would_include_code_addr(TCRttiName).
rtti_id_would_include_code_addr(aditi_rtti_id(_)) = yes.

ctor_rtti_name_would_include_code_addr(exist_locns(_)) =		no.
ctor_rtti_name_would_include_code_addr(exist_locn) 	=		no.
ctor_rtti_name_would_include_code_addr(exist_tc_constr(_, _, _)) =	no.
ctor_rtti_name_would_include_code_addr(exist_tc_constrs(_)) =		no.
ctor_rtti_name_would_include_code_addr(exist_info(_)) =			no.
ctor_rtti_name_would_include_code_addr(field_names(_)) =		no.
ctor_rtti_name_would_include_code_addr(field_types(_)) =		no.
ctor_rtti_name_would_include_code_addr(res_addrs) =			no.
ctor_rtti_name_would_include_code_addr(res_addr_functors) =		no.
ctor_rtti_name_would_include_code_addr(enum_functor_desc(_)) =		no.
ctor_rtti_name_would_include_code_addr(notag_functor_desc) =		no.
ctor_rtti_name_would_include_code_addr(du_functor_desc(_)) =		no.
ctor_rtti_name_would_include_code_addr(res_functor_desc(_)) = 		no.
ctor_rtti_name_would_include_code_addr(enum_name_ordered_table) =	no.
ctor_rtti_name_would_include_code_addr(enum_value_ordered_table) =	no.
ctor_rtti_name_would_include_code_addr(du_name_ordered_table) =		no.
ctor_rtti_name_would_include_code_addr(du_stag_ordered_table(_)) =	no.
ctor_rtti_name_would_include_code_addr(du_ptag_ordered_table) =		no.
ctor_rtti_name_would_include_code_addr(du_ptag_layout(_)) =		no.
ctor_rtti_name_would_include_code_addr(res_value_ordered_table) =	no.
ctor_rtti_name_would_include_code_addr(res_name_ordered_table) =	no.
ctor_rtti_name_would_include_code_addr(maybe_res_addr_functor_desc) =	no.
ctor_rtti_name_would_include_code_addr(type_hashcons_pointer) =		no.
ctor_rtti_name_would_include_code_addr(type_functors) =			no.
ctor_rtti_name_would_include_code_addr(type_layout) =			no.
ctor_rtti_name_would_include_code_addr(type_ctor_info) =		yes.
ctor_rtti_name_would_include_code_addr(type_info(TypeInfo)) =
	type_info_would_incl_code_addr(TypeInfo).
ctor_rtti_name_would_include_code_addr(pseudo_type_info(PseudoTypeInfo)) =
	pseudo_type_info_would_incl_code_addr(PseudoTypeInfo).

tc_rtti_name_would_include_code_addr(base_typeclass_info(_, _)) = yes.
tc_rtti_name_would_include_code_addr(type_class_id) = no.
tc_rtti_name_would_include_code_addr(type_class_id_var_names) = no.
tc_rtti_name_would_include_code_addr(type_class_id_method_ids) = no.
tc_rtti_name_would_include_code_addr(type_class_decl) = no.
tc_rtti_name_would_include_code_addr(type_class_decl_super(_, _)) = no.
tc_rtti_name_would_include_code_addr(type_class_decl_supers) = no.
tc_rtti_name_would_include_code_addr(type_class_instance(_)) = no.
tc_rtti_name_would_include_code_addr(type_class_instance_tc_type_vector(_))
	= no.
tc_rtti_name_would_include_code_addr(type_class_instance_constraint(_, _, _))
	= no.
tc_rtti_name_would_include_code_addr(type_class_instance_constraints(_)) = no.
tc_rtti_name_would_include_code_addr(type_class_instance_methods(_)) = no.

type_info_would_incl_code_addr(plain_arity_zero_type_info(_)) = yes.
type_info_would_incl_code_addr(plain_type_info(_, _)) =	no.
type_info_would_incl_code_addr(var_arity_type_info(_, _)) = no.

pseudo_type_info_would_incl_code_addr(plain_arity_zero_pseudo_type_info(_))
	= yes.
pseudo_type_info_would_incl_code_addr(plain_pseudo_type_info(_, _)) = no.
pseudo_type_info_would_incl_code_addr(var_arity_pseudo_type_info(_, _))	= no.
pseudo_type_info_would_incl_code_addr(type_var(_)) = no.

rtti_id_maybe_element_c_type(item_type(RttiId), CTypeName, IsArray) :-
	rtti_id_c_type(RttiId, CTypeName, IsArray).
rtti_id_maybe_element_c_type(element_type(RttiId), CTypeName, IsArray) :-
	rtti_id_c_type(RttiId, CTypeName, IsArray0),
	(
		IsArray0 = no,
		error("rtti_id_maybe_element_c_type: base is not array")
	;
		IsArray0 = yes,
		IsArray = no
	).

rtti_id_c_type(ctor_rtti_id(_, RttiName), CTypeName, IsArray) :-
	ctor_rtti_name_c_type(RttiName, CTypeName, IsArray).
rtti_id_c_type(tc_rtti_id(_, TCRttiName), CTypeName, IsArray) :-
	tc_rtti_name_c_type(TCRttiName, CTypeName, IsArray).
rtti_id_c_type(aditi_rtti_id(_), "MR_Aditi_Proc_Info", no).

ctor_rtti_name_c_type(RttiName, CTypeName, IsArray) :-
	ctor_rtti_name_type(RttiName, GenTypeName, IsArray),
	CTypeName = string__append("MR_", GenTypeName).

tc_rtti_name_c_type(TCRttiName, CTypeName, IsArray) :-
	tc_rtti_name_type(TCRttiName, GenTypeName, IsArray),
	CTypeName = string__append("MR_", GenTypeName).

rtti_id_maybe_element_java_type(item_type(RttiId), CTypeName, IsArray) :-
	rtti_id_java_type(RttiId, CTypeName, IsArray).
rtti_id_maybe_element_java_type(element_type(RttiId), CTypeName, IsArray) :-
	rtti_id_java_type(RttiId, CTypeName, IsArray0),
	(
		IsArray0 = no,
		error("rtti_id_maybe_element_java_type: base is not array")
	;
		IsArray0 = yes,
		IsArray = no
	).

rtti_id_java_type(ctor_rtti_id(_, RttiName), JavaTypeName, IsArray) :-
	ctor_rtti_name_java_type(RttiName, JavaTypeName, IsArray).
rtti_id_java_type(tc_rtti_id(_, TCRttiName), JavaTypeName, IsArray) :-
	tc_rtti_name_java_type(TCRttiName, JavaTypeName, IsArray).
rtti_id_java_type(aditi_rtti_id(_), _, _) :-
	error("Aditi not supported for the Java back-end").

ctor_rtti_name_java_type(RttiName, JavaTypeName, IsArray) :-
	ctor_rtti_name_type(RttiName, GenTypeName0, IsArray),
	(
		% Java doesn't have typedefs (or "const"),
		% so we need to use "String" rather than "ConstString"
		GenTypeName0 = "ConstString"
	->
		JavaTypeName = "java.lang.String"
	;
		% In Java, every non-builtin type is a pointer,
		% so there's no need for the "Ptr" suffixes.
		string__remove_suffix(GenTypeName0, "Ptr", GenTypeName1)
	->
		JavaTypeName = string__append("mercury.runtime.", GenTypeName1)
	;
		% In C, we do some nasty hacks to represent type class
		% constraints of different arities as different structures
		% ending with arrays of the appropriate length, but in
		% Java we just use a single type for all of them
		% (with an extra level of indirection for the array).
		string__prefix(GenTypeName0, "TypeClassConstraint_")
	->
		JavaTypeName = "mercury.runtime.TypeClassConstraint"
	;
		% In C, we do some nasty hacks to represent type infos
		% different arities as different structures
		% ending with arrays of the appropriate length, but in
		% Java we just use a single type for all of them
		% (with an extra level of indirection for the array).
		( string__prefix(GenTypeName0, "FA_PseudoTypeInfo_Struct")
		; string__prefix(GenTypeName0, "FA_TypeInfo_Struct")
		; string__prefix(GenTypeName0, "VA_PseudoTypeInfo_Struct")
		; string__prefix(GenTypeName0, "VA_TypeInfo_Struct")
		)
	->
		JavaTypeName = "mercury.runtime.TypeInfo_Struct"
	;
		JavaTypeName = string__append("mercury.runtime.", GenTypeName0)
	).

tc_rtti_name_java_type(TCRttiName, JavaTypeName, IsArray) :-
	tc_rtti_name_type(TCRttiName, GenTypeName, IsArray),
	(
		% BaseTypeClassInfo in C is represented using a
		% variable-length array as the last field,
		% so we need to handle it specially in Java
		GenTypeName = "BaseTypeclassInfo"
	->
		JavaTypeName = "java.lang.Object" /* & IsArray = yes */
	;
		% Java doesn't have typedefs (or "const"),
		% so we need to use "String" rather than "ConstString"
		GenTypeName = "ConstString"
	->
		JavaTypeName = "java.lang.String"
	;
		% In C, we do some nasty hacks to represent type class
		% constraints of different arities as different structures
		% ending with arrays of the appropriate length, but in
		% Java we just use a single type for all of them
		% (with an extra level of indirection for the array).
		string__prefix(GenTypeName, "TypeClassConstraint_")
	->
		JavaTypeName = "mercury.runtime.TypeClassConstraint"
	;
		% The rest are all defined in Mercury's Java runtime
		% (java/runtime/*.java).
		JavaTypeName = string__append("mercury.runtime.",
			GenTypeName)
	).

	% ctor_rtti_name_type(RttiName, Type, IsArray):
:- pred ctor_rtti_name_type(ctor_rtti_name::in, string::out, bool::out) is det.

ctor_rtti_name_type(exist_locns(_),             "DuExistLocn", yes).
ctor_rtti_name_type(exist_locn,             	"DuExistLocn", no).
ctor_rtti_name_type(exist_tc_constr(_, _, N), TypeName, no) :-
	TypeName = tc_constraint_type_name(N).
ctor_rtti_name_type(exist_tc_constrs(_),        "TypeClassConstraint", yes).
ctor_rtti_name_type(exist_info(_),              "DuExistInfo", no).
ctor_rtti_name_type(field_names(_),             "ConstString", yes).
ctor_rtti_name_type(field_types(_),             "PseudoTypeInfo", yes).
ctor_rtti_name_type(res_addrs,                  "ReservedAddr", yes).
ctor_rtti_name_type(res_addr_functors,          "ReservedAddrFunctorDescPtr",
	yes).
ctor_rtti_name_type(enum_functor_desc(_),       "EnumFunctorDesc", no).
ctor_rtti_name_type(notag_functor_desc,         "NotagFunctorDesc", no).
ctor_rtti_name_type(du_functor_desc(_),         "DuFunctorDesc", no).
ctor_rtti_name_type(res_functor_desc(_),        "ReservedAddrFunctorDesc", no).
ctor_rtti_name_type(enum_name_ordered_table,    "EnumFunctorDescPtr", yes).
ctor_rtti_name_type(enum_value_ordered_table,   "EnumFunctorDescPtr", yes).
ctor_rtti_name_type(du_name_ordered_table,      "DuFunctorDescPtr", yes).
ctor_rtti_name_type(du_stag_ordered_table(_),   "DuFunctorDescPtr", yes).
ctor_rtti_name_type(du_ptag_ordered_table,      "DuPtagLayout", yes).
ctor_rtti_name_type(du_ptag_layout(_),      	"DuPtagLayout", no).
ctor_rtti_name_type(res_value_ordered_table,    "ReservedAddrTypeLayout", no).
ctor_rtti_name_type(res_name_ordered_table,     "MaybeResAddrFunctorDesc", yes).
ctor_rtti_name_type(maybe_res_addr_functor_desc,
						"MaybeResAddrFunctorDesc", no).
ctor_rtti_name_type(type_functors,              "TypeFunctors", no).
ctor_rtti_name_type(type_layout,                "TypeLayout", no).
ctor_rtti_name_type(type_ctor_info,             "TypeCtorInfo_Struct", no).
ctor_rtti_name_type(type_hashcons_pointer,      "TrieNodePtr", no).
ctor_rtti_name_type(type_info(TypeInfo), TypeName, no) :-
	TypeName = type_info_name_type(TypeInfo).
ctor_rtti_name_type(pseudo_type_info(PseudoTypeInfo), TypeName, no) :-
	TypeName = pseudo_type_info_name_type(PseudoTypeInfo).

	% tc_rtti_name_type(RttiName, Type, IsArray):
:- pred tc_rtti_name_type(tc_rtti_name::in, string::out, bool::out) is det.

tc_rtti_name_type(base_typeclass_info(_, _),    "BaseTypeclassInfo", yes).
tc_rtti_name_type(type_class_id,		"TypeClassId", no).
tc_rtti_name_type(type_class_id_var_names,	"ConstString", yes).
tc_rtti_name_type(type_class_id_method_ids,	"TypeClassMethod", yes).
tc_rtti_name_type(type_class_decl,		"TypeClassDeclStruct", no).
tc_rtti_name_type(type_class_decl_super(_, N), TypeName, no) :-
	TypeName = tc_constraint_type_name(N).
tc_rtti_name_type(type_class_decl_supers,	"TypeClassConstraint", yes).
tc_rtti_name_type(type_class_instance(_),	"InstanceStruct", no).
tc_rtti_name_type(type_class_instance_tc_type_vector(_),
						"PseudoTypeInfo", yes).
tc_rtti_name_type(type_class_instance_constraint(_, _, N), TypeName, no) :-
	TypeName = tc_constraint_type_name(N).
tc_rtti_name_type(type_class_instance_constraints(_),
						"TypeClassConstraint", yes).
tc_rtti_name_type(type_class_instance_methods(_),
						"CodePtr", yes).

:- func tc_constraint_type_name(int) = string.

tc_constraint_type_name(N) = TypeName :-
	string__int_to_string(N, NStr),
	string__append_list(["TypeClassConstraint_", NStr, "Struct"],
		TypeName).

:- func type_info_name_type(rtti_type_info) = string.

type_info_name_type(plain_arity_zero_type_info(_)) =
	"TypeCtorInfo_Struct".
type_info_name_type(plain_type_info(_, ArgTypes)) =
	string__format("FA_TypeInfo_Struct%d", [i(list__length(ArgTypes))]).
type_info_name_type(var_arity_type_info(_, ArgTypes)) =
	string__format("VA_TypeInfo_Struct%d", [i(list__length(ArgTypes))]).

:- func pseudo_type_info_name_type(rtti_pseudo_type_info) = string.

pseudo_type_info_name_type(plain_arity_zero_pseudo_type_info(_)) =
	"TypeCtorInfo_Struct".
pseudo_type_info_name_type(plain_pseudo_type_info(_TypeCtor, ArgTypes)) =
	string__format("FA_PseudoTypeInfo_Struct%d",
		[i(list__length(ArgTypes))]).
pseudo_type_info_name_type(var_arity_pseudo_type_info(_TypeCtor, ArgTypes)) =
	string__format("VA_PseudoTypeInfo_Struct%d",
		[i(list__length(ArgTypes))]).
pseudo_type_info_name_type(type_var(_)) = _ :-
	% we use small integers to represent type_vars,
	% rather than pointers, so there is no pointed-to type
	error("pseudo_type_info_name_type: type_var").

module_qualify_name_of_rtti_id(RttiId) = ShouldModuleQualify :-
	(
		RttiId = ctor_rtti_id(_, CtorRttiName),
		ShouldModuleQualify =
			module_qualify_name_of_ctor_rtti_name(CtorRttiName)
	;
		RttiId = tc_rtti_id(_, TCRttiName),
		ShouldModuleQualify =
			module_qualify_name_of_tc_rtti_name(TCRttiName)
	;
		RttiId = aditi_rtti_id(_),
		ShouldModuleQualify = yes
	).

module_qualify_name_of_ctor_rtti_name(_) = yes.

% We don't want to include the module name as part of the name for
% base_typeclass_infos, since we _want_ to cause a link error for
% overlapping instance decls, even if they are in a different modules.
%
% When we start generating data structures replacing base_typeclass_infos,
% we should include their names here.
%
% This decision is implemented separately in rtti__tc_name_to_string.

module_qualify_name_of_tc_rtti_name(TCRttiName) =
	( TCRttiName = base_typeclass_info(_, _) ->
		no
	;
		yes
	).

rtti_id_emits_type_ctor_info(RttiId, TypeCtor) :-
	RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
	(
		RttiName = type_ctor_info,
		TypeCtor = RttiTypeCtor
	;
		RttiName = type_info(TypeInfo),
		TypeInfo = plain_arity_zero_type_info(TypeCtor)
	;
		RttiName = pseudo_type_info(PseudoTypeInfo),
		PseudoTypeInfo = plain_arity_zero_pseudo_type_info(TypeCtor)
	).

%-----------------------------------------------------------------------------%
