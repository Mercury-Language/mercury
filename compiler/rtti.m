%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
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

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_module, hlds__hlds_pred, hlds__hlds_data.
:- import_module backend_libs__code_model.

:- import_module bool, list, std_util, map.

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
			tcr_rep_details		:: type_ctor_details
		).

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
	% element on the list before looking at tn /var/spool/htdig.  Also
	% improve error reporting.        than /var/spool/htdig.  Also improve
	% error reporting.he primary tag.
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
	;	foreign.

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
			exist_num_typeclass_infos	:: int,
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
	% Mercury language. The compiler never creates type_ctor_datas for
	% these, but RTTI predicates implemented in Mercury will need to
	% know about them.
:- type builtin_ctor
	--->	int
	;	float
	;	char
	;	string
	;	univ
	;	void
	;	c_pointer.	% maybe more to come later

	% The list of type constructors that are used behind the scenes by
	% the Mercury implementation. The compiler never creates
	% type_ctor_datas for these, but RTTI predicates implemented
	% in Mercury will need to know about them.
:- type impl_ctor
	--->	sp
	;	hp
	;	maxfr
	;	curfr.		% maybe more to come later

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

	% The rtti_proc_label type holds all the information about a procedure
	% that we need to compute the entry label for that procedure
	% in the target language (the llds__code_addr or mlds__code_addr).
:- type rtti_proc_label
	--->	rtti_proc_label(
			pred_or_func		::	pred_or_func,
			this_module		::	module_name,
			pred_module		::	module_name,
			pred_name		::	string,
			arity			::	arity,
			arg_types		::	list(type),
			pred_id			::	pred_id,
			proc_id			::	proc_id,
			proc_varset		::	prog_varset,
			proc_headvars		::	list(prog_var),
			proc_arg_modes		::	list(arg_mode),
			proc_interface_code_model ::	code_model,
			%
			% The following booleans hold values computed from the
			% pred_info, using procedures
			%	pred_info_is_imported/1,
			%	pred_info_is_pseudo_imported/1,
			%	procedure_is_exported/2, and
			%	pred_info_is_compiler_generated/1
			% respectively.
			% We store booleans here, rather than storing the
			% pred_info, to avoid retaining a reference to the
			% parts of the pred_info that we aren't interested in,
			% so that those parts can be garbage collected.
			% We use booleans rather than an import_status
			% so that we can continue to use the above-mentioned
			% abstract interfaces rather than hard-coding tests
			% on the import_status.
			%
			is_imported			::	bool,
			is_pseudo_imported		::	bool,
			is_exported			::	bool,
			is_special_pred_instance	::	bool
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
			module_name,	% module containing instance decl.
			class_id,	% specifies class name & class arity
			string,		% encodes the names and arities of the
					% types in the instance declaration

			base_typeclass_info
		).

:- type rtti_name
	--->	exist_locns(int)		% functor ordinal
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
	;	res_value_ordered_table
	;	res_name_ordered_table
	;	type_ctor_info
	;	type_info(rtti_type_info)
	;	pseudo_type_info(rtti_pseudo_type_info)
	;	base_typeclass_info(
			module_name,	% module containing instance decl.
			class_id,	% specifies class name & class arity
			string		% encodes the names and arities of the
					% types in the instance declaration
		)
	;	type_hashcons_pointer.

%-----------------------------------------------------------------------------%
%
% The functions operating on RTTI data.

	% Return the id of the type constructor.
:- func tcd_get_rtti_type_ctor(type_ctor_data) = rtti_type_ctor.

	% Convert a rtti_data to a rtti_type_ctor and a rtti_name.
	% This calls error/1 if the argument is a type_var/1 rtti_data,
	% since there is no rtti_type_ctor to return in that case.
:- pred rtti_data_to_name(rtti_data::in, rtti_type_ctor::out, rtti_name::out)
	is det.

	% Convert an id that specifies a kind of variable arity type_info
	% or pseudo_type_info into the type_ctor of the canonical (arity-zero)
	% type of that kind.
:- func var_arity_id_to_rtti_type_ctor(var_arity_ctor_id) = rtti_type_ctor.

	% return yes iff the specified rtti_name is an array
:- func rtti_name_has_array_type(rtti_name) = bool.

	% Return yes iff the specified rtti_name should be exported
	% for use by other modules.
:- func rtti_name_is_exported(rtti_name) = bool.

	% Construct an rtti_proc_label for a given procedure.
:- func rtti__make_proc_label(module_info, pred_id, proc_id) = rtti_proc_label.

	% Construct an rtti_proc_label for a given procedure.
:- pred rtti__proc_label_pred_proc_id(rtti_proc_label::in,
	pred_id::out, proc_id::out) is det.

	% Return the C variable name of the RTTI data structure identified
	% by the input arguments.
:- pred rtti__addr_to_string(rtti_type_ctor::in, rtti_name::in, string::out)
	is det.

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
:- func enum_functor_rtti_name(enum_functor) = rtti_name.
:- func du_functor_rtti_name(du_functor) = rtti_name.
:- func res_functor_rtti_name(reserved_functor) = rtti_name.
:- func maybe_res_functor_rtti_name(maybe_reserved_functor) = rtti_name.

	% Extract the reserved address from a reserved address functor
	% descriptor.
:- func res_addr_rep(reserved_functor) = reserved_address.

	% Reserved addresses can be numeric or symbolic. Succeed if the
	% one passed is numeric.
:- pred res_addr_is_numeric(reserved_address::in) is semidet.

        % Return true iff the given type of RTTI data structure includes
	% code addresses.
:- func rtti_name_would_include_code_addr(rtti_name) = bool.

        % Return true iff the given type_info's RTTI data structure includes
	% code addresses.
:- func type_info_would_incl_code_addr(rtti_type_info) = bool.

        % Return true iff the given pseudo_type_info's RTTI data structure
	% includes code addresses.
:- func pseudo_type_info_would_incl_code_addr(rtti_pseudo_type_info) = bool.

	% rtti_name_c_type(RttiName, Type, IsArray):
	%	To declare a variable of the type specified by RttiName,
	%	put Type before the name of the variable; if IsArray is true,
	%	also put "[]" after the name.
:- pred rtti_name_c_type(rtti_name::in, string::out, bool::out) is det.

	% Analogous to rtti_name_c_type.
:- pred rtti_name_java_type(rtti_name::in, string::out, bool::out) is det.

:- implementation.

:- import_module parse_tree__prog_util.	% for mercury_public_builtin_module
:- import_module hlds__hlds_data.
:- import_module check_hlds__type_util, check_hlds__mode_util.
:- import_module ll_backend__code_util.	% for code_util__compiler_generated
:- import_module ll_backend__llds_out.	% for name_mangle and sym_name_mangle

:- import_module int, string, require.

rtti_data_to_name(type_ctor_info(TypeCtorData), RttiTypeCtor,
		type_ctor_info) :-
	RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData).
rtti_data_to_name(type_info(TypeInfo), RttiTypeCtor, type_info(TypeInfo)) :-
	RttiTypeCtor = ti_get_rtti_type_ctor(TypeInfo).
rtti_data_to_name(pseudo_type_info(PseudoTypeInfo), RttiTypeCtor,
		pseudo_type_info(PseudoTypeInfo)) :-
	RttiTypeCtor = pti_get_rtti_type_ctor(PseudoTypeInfo).
rtti_data_to_name(base_typeclass_info(_, _, _, _), _, _) :-
	% there's no rtti_type_ctor associated with a base_typeclass_info
	error("rtti_data_to_name: base_typeclass_info").

tcd_get_rtti_type_ctor(TypeCtorData) = RttiTypeCtor :-
	ModuleName = TypeCtorData ^ tcr_module_name,
	TypeName = TypeCtorData ^ tcr_type_name,
	Arity = TypeCtorData ^ tcr_arity,
	RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, Arity).

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

rtti_name_has_array_type(RttiName) = IsArray :-
	rtti_name_type(RttiName, _, IsArray).

rtti_name_is_exported(exist_locns(_))		= no.
rtti_name_is_exported(exist_info(_))            = no.
rtti_name_is_exported(field_names(_))           = no.
rtti_name_is_exported(field_types(_))           = no.
rtti_name_is_exported(res_addrs)           	= no.
rtti_name_is_exported(res_addr_functors)   	= no.
rtti_name_is_exported(enum_functor_desc(_))     = no.
rtti_name_is_exported(notag_functor_desc)       = no.
rtti_name_is_exported(du_functor_desc(_))       = no.
rtti_name_is_exported(res_functor_desc(_)) 	= no.
rtti_name_is_exported(enum_name_ordered_table)  = no.
rtti_name_is_exported(enum_value_ordered_table) = no.
rtti_name_is_exported(du_name_ordered_table)    = no.
rtti_name_is_exported(du_stag_ordered_table(_)) = no.
rtti_name_is_exported(du_ptag_ordered_table)    = no.
rtti_name_is_exported(res_value_ordered_table)  = no.
rtti_name_is_exported(res_name_ordered_table)   = no.
rtti_name_is_exported(type_ctor_info)           = yes.
rtti_name_is_exported(type_info(TypeInfo)) =
	type_info_is_exported(TypeInfo).
rtti_name_is_exported(pseudo_type_info(PseudoTypeInfo)) =
	pseudo_type_info_is_exported(PseudoTypeInfo).
rtti_name_is_exported(base_typeclass_info(_, _, _)) = yes.
rtti_name_is_exported(type_hashcons_pointer)    = no.

:- func type_info_is_exported(rtti_type_info) = bool.

type_info_is_exported(plain_arity_zero_type_info(_)) = yes.
type_info_is_exported(plain_type_info(_, _))	     = no.
type_info_is_exported(var_arity_type_info(_, _))     = no.

:- func pseudo_type_info_is_exported(rtti_pseudo_type_info) = bool.

pseudo_type_info_is_exported(plain_arity_zero_pseudo_type_info(_)) = yes.
pseudo_type_info_is_exported(plain_pseudo_type_info(_, _))	= no.
pseudo_type_info_is_exported(var_arity_pseudo_type_info(_, _))	= no.
pseudo_type_info_is_exported(type_var(_))			= no.

rtti__make_proc_label(ModuleInfo, PredId, ProcId) = ProcLabel :-
	module_info_name(ModuleInfo, ThisModule),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_module(PredInfo, PredModule),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, Arity),
	pred_info_arg_types(PredInfo, ArgTypes),
	proc_info_varset(ProcInfo, ProcVarSet),
	proc_info_headvars(ProcInfo, ProcHeadVars),
	proc_info_argmodes(ProcInfo, ProcModes),
	proc_info_interface_code_model(ProcInfo, ProcCodeModel),
	modes_to_arg_modes(ModuleInfo, ProcModes, ArgTypes, ProcArgModes),
	IsImported = (pred_info_is_imported(PredInfo) -> yes ; no),
	IsPseudoImp = (pred_info_is_pseudo_imported(PredInfo) -> yes ; no),
	IsExported = (procedure_is_exported(PredInfo, ProcId) -> yes ; no),
	IsSpecialPredInstance =
		(code_util__compiler_generated(PredInfo) -> yes ; no),
	ProcLabel = rtti_proc_label(PredOrFunc, ThisModule, PredModule,
		PredName, Arity, ArgTypes, PredId, ProcId,
		ProcVarSet, ProcHeadVars, ProcArgModes, ProcCodeModel,
		IsImported, IsPseudoImp, IsExported, IsSpecialPredInstance).

rtti__proc_label_pred_proc_id(ProcLabel, PredId, ProcId) :-
	ProcLabel = rtti_proc_label(_, _, _, _, _, _, PredId, ProcId,
		_, _, _, _, _, _, _, _).

rtti__addr_to_string(RttiTypeCtor, RttiName, Str) :-
	rtti__mangle_rtti_type_ctor(RttiTypeCtor, ModuleName, TypeName, A_str),
	(
		RttiName = exist_locns(Ordinal),
		string__int_to_string(Ordinal, O_str),
		string__append_list([ModuleName, "__exist_locns_",
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
		RttiName = res_value_ordered_table,
		string__append_list([ModuleName, "__res_layout_ordered_table_",
			TypeName, "_", A_str], Str)
	;
		RttiName = res_name_ordered_table,
		string__append_list([ModuleName, "__res_name_ordered_table_",
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
		RttiName = base_typeclass_info(_ModuleName, ClassId,
			InstanceStr),
		ClassId = class_id(ClassSym, ClassArity),
		llds_out__sym_name_mangle(ClassSym, MangledClassString),
		string__int_to_string(ClassArity, ArityString),
		llds_out__name_mangle(InstanceStr, MangledTypeNames),
		string__append_list(["base_typeclass_info_",
			MangledClassString, "__arity", ArityString, "__",
			MangledTypeNames], Str)
	;
		RttiName = type_hashcons_pointer,
		string__append_list([ModuleName, "__hashcons_ptr_",
			TypeName, "_", A_str], Str)
	).

:- pred rtti__mangle_rtti_type_ctor(rtti_type_ctor::in,
	string::out, string::out, string::out) is det.

rtti__mangle_rtti_type_ctor(RttiTypeCtor, ModuleName, TypeName, ArityStr) :-
	RttiTypeCtor = rtti_type_ctor(ModuleName0, TypeName0, TypeArity),
	llds_out__sym_name_mangle(ModuleName0, ModuleName),
	llds_out__name_mangle(TypeName0, TypeName),
	string__int_to_string(TypeArity, ArityStr).

%-----------------------------------------------------------------------------%

:- func rtti__type_info_to_string(rtti_type_info) = string.

rtti__type_info_to_string(TypeInfo) = Str :-
	(
		TypeInfo = plain_arity_zero_type_info(RttiTypeCtor),
		rtti__addr_to_string(RttiTypeCtor, type_ctor_info, Str)
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
		rtti__addr_to_string(RttiTypeCtor, type_ctor_info, Str)
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
		TypeCtorDetails = builtin(_),
		error("rtti__type_ctor_rep_to_string: builtin")
	;
		TypeCtorDetails = impl_artifact(_),
		error("rtti__type_ctor_rep_to_string: impl_artifact")
	;
		TypeCtorDetails = foreign,
		RepStr = "MR_TYPECTOR_REP_FOREIGN"
	).

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
type_ctor_details_num_ptags(reserved(_, _, _, PtagMap, _)) = LastPtag + 1 :-
	map__keys(PtagMap, Ptags),
	list__last_det(Ptags, LastPtag).
type_ctor_details_num_ptags(notag(_, _)) = -1.
type_ctor_details_num_ptags(eqv(_)) = -1.
type_ctor_details_num_ptags(builtin(_)) = -1.
type_ctor_details_num_ptags(impl_artifact(_)) = -1.
type_ctor_details_num_ptags(foreign) = -1.

type_ctor_details_num_functors(enum(_, EnumFunctors, _, _)) =
	list__length(EnumFunctors).
type_ctor_details_num_functors(du(_, DuFunctors, _, _)) =
	list__length(DuFunctors).
type_ctor_details_num_functors(reserved(_, ResFunctors, _, _, _)) =
	list__length(ResFunctors).
type_ctor_details_num_functors(notag(_, _)) = 1.
type_ctor_details_num_functors(eqv(_)) = -1.
type_ctor_details_num_functors(builtin(_)) = -1.
type_ctor_details_num_functors(impl_artifact(_)) = -1.
type_ctor_details_num_functors(foreign) = -1.

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

rtti_name_would_include_code_addr(exist_locns(_)) =		no.
rtti_name_would_include_code_addr(exist_info(_)) =		no.
rtti_name_would_include_code_addr(field_names(_)) =		no.
rtti_name_would_include_code_addr(field_types(_)) =		no.
rtti_name_would_include_code_addr(res_addrs) =			no.
rtti_name_would_include_code_addr(res_addr_functors) =		no.
rtti_name_would_include_code_addr(enum_functor_desc(_)) =	no.
rtti_name_would_include_code_addr(notag_functor_desc) =		no.
rtti_name_would_include_code_addr(du_functor_desc(_)) =		no.
rtti_name_would_include_code_addr(res_functor_desc(_)) = 	no.
rtti_name_would_include_code_addr(enum_name_ordered_table) =	no.
rtti_name_would_include_code_addr(enum_value_ordered_table) =	no.
rtti_name_would_include_code_addr(du_name_ordered_table) =	no.
rtti_name_would_include_code_addr(du_stag_ordered_table(_)) =	no.
rtti_name_would_include_code_addr(du_ptag_ordered_table) =	no.
rtti_name_would_include_code_addr(res_value_ordered_table) =	no.
rtti_name_would_include_code_addr(res_name_ordered_table) =	no.
rtti_name_would_include_code_addr(type_hashcons_pointer) =	no.
rtti_name_would_include_code_addr(type_ctor_info) =		yes.
rtti_name_would_include_code_addr(base_typeclass_info(_, _, _)) = yes.
rtti_name_would_include_code_addr(type_info(TypeInfo)) =
	type_info_would_incl_code_addr(TypeInfo).
rtti_name_would_include_code_addr(pseudo_type_info(PseudoTypeInfo)) =
	pseudo_type_info_would_incl_code_addr(PseudoTypeInfo).

type_info_would_incl_code_addr(plain_arity_zero_type_info(_)) = yes.
type_info_would_incl_code_addr(plain_type_info(_, _)) =	no.
type_info_would_incl_code_addr(var_arity_type_info(_, _)) = no.

pseudo_type_info_would_incl_code_addr(plain_arity_zero_pseudo_type_info(_))
	= yes.
pseudo_type_info_would_incl_code_addr(plain_pseudo_type_info(_, _)) = no.
pseudo_type_info_would_incl_code_addr(var_arity_pseudo_type_info(_, _))	= no.
pseudo_type_info_would_incl_code_addr(type_var(_)) = no.

rtti_name_c_type(RttiName, CTypeName, IsArray) :-
	rtti_name_type(RttiName, GenTypeName, IsArray),
	CTypeName = string__append("MR_", GenTypeName).

rtti_name_java_type(RttiName, JavaTypeName, IsArray) :-
	rtti_name_type(RttiName, GenTypeName, IsArray),
	JavaTypeName = string__append("mercury.runtime.", GenTypeName).

	% rtti_name_type(RttiName, Type, IsArray):
:- pred rtti_name_type(rtti_name::in, string::out, bool::out) is det.

rtti_name_type(exist_locns(_),             "DuExistLocn", yes).
rtti_name_type(exist_info(_),              "DuExistInfo", no).
rtti_name_type(field_names(_),             "ConstString", yes).
rtti_name_type(field_types(_),             "PseudoTypeInfo", yes).
rtti_name_type(res_addrs,                  "ReservedAddr", yes).
rtti_name_type(res_addr_functors,          "ReservedAddrFunctorDescPtr", yes).
rtti_name_type(enum_functor_desc(_),       "EnumFunctorDesc", no).
rtti_name_type(notag_functor_desc,         "NotagFunctorDesc", no).
rtti_name_type(du_functor_desc(_),         "DuFunctorDesc", no).
rtti_name_type(res_functor_desc(_),        "ReservedAddrFunctorDesc", no).
rtti_name_type(enum_name_ordered_table,    "EnumFunctorDescPtr", yes).
rtti_name_type(enum_value_ordered_table,   "EnumFunctorDescPtr", yes).
rtti_name_type(du_name_ordered_table,      "DuFunctorDescPtr", yes).
rtti_name_type(du_stag_ordered_table(_),   "DuFunctorDescPtr", yes).
rtti_name_type(du_ptag_ordered_table,      "DuPtagLayout", yes).
rtti_name_type(res_value_ordered_table,    "ReservedAddrTypeLayout", no).
rtti_name_type(res_name_ordered_table,     "MaybeResAddrFunctorDesc", yes).
rtti_name_type(type_ctor_info,             "TypeCtorInfo_Struct", no).
rtti_name_type(base_typeclass_info(_,_,_), "BaseTypeclassInfo", yes).
rtti_name_type(type_hashcons_pointer,      "TrieNodePtr", no).
rtti_name_type(type_info(TypeInfo), TypeName, no) :-
	TypeName = type_info_name_type(TypeInfo).
rtti_name_type(pseudo_type_info(PseudoTypeInfo), TypeName, no) :-
	TypeName = pseudo_type_info_name_type(PseudoTypeInfo).

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

%-----------------------------------------------------------------------------%
