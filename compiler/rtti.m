%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rtti.m.
% Authors: zs, fjh.
%
% Definitions of data structures for representing run-time type information
% within the compiler. When output by rtti_out.m, values of most these types
% will correspond to the types defined in runtime/mercury_type_info.h; the
% documentation of those types can be found there.
% The code to generate the structures is in type_ctor_info.m.
% See also pseudo_type_info.m.
%
% This module is independent of whether we are compiling to LLDS or MLDS.
% It is used as an intermediate data structure that we generate from the HLDS,
% and which we can then convert to either LLDS or MLDS. The LLDS actually
% incorporates this data structure unchanged.
%
%-----------------------------------------------------------------------------%

:- module backend_libs.rtti.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module univ.

%-----------------------------------------------------------------------------%
%
% The data structures representing types, both ground (typeinfos) and
% nonground (pseudo-typeinfos).

    % An rtti_type_info identifies a ground type.
    %
:- type rtti_type_info
    --->    plain_arity_zero_type_info(
                rtti_type_ctor
            )
    ;       plain_type_info(
                rtti_type_ctor,
                % This list should not be empty; if it is, one should
                % use plain_arity_zero_type_info instead.
                list(rtti_type_info)
            )
    ;       var_arity_type_info(
                var_arity_ctor_id,
                list(rtti_type_info)
            ).

    % An rtti_pseudo_type_info identifies a possibly non-ground type.
    %
:- type rtti_pseudo_type_info
    --->    plain_arity_zero_pseudo_type_info(
                rtti_type_ctor
            )
    ;       plain_pseudo_type_info(
                rtti_type_ctor,
                % This list should not be empty; if it is, one should
                % use plain_arity_zero_pseudo_type_info instead.
                list(rtti_maybe_pseudo_type_info)
            )
    ;       var_arity_pseudo_type_info(
                var_arity_ctor_id,
                list(rtti_maybe_pseudo_type_info)
            )
    ;       type_var(int).

    % An rtti_maybe_pseudo_type_info identifies a type. If the type is
    % ground, it should be bound to plain; if it is non-ground, it should
    % be bound to pseudo.
    %
:- type rtti_maybe_pseudo_type_info
    --->    pseudo(rtti_pseudo_type_info)
    ;       plain(rtti_type_info).

    % An rtti_type_ctor uniquely identifies a fixed arity type constructor.
    %
:- type rtti_type_ctor
    --->    rtti_type_ctor(
                module_name,        % module name
                string,             % type ctor's name
                uint16              % type ctor's arity
            ).

    % A var_arity_ctor_id uniquely identifies a variable arity type
    % constructor.
:- type var_arity_ctor_id
    --->    pred_type_info
    ;       func_type_info
    ;       tuple_type_info.

%-----------------------------------------------------------------------------%
%
% The data structures representing type constructors.
%

    % A type_ctor_data structure contains all the information that the
    % runtime system needs to know about a type constructor.
    %
:- type type_ctor_data
    --->    type_ctor_data(
                tcr_version         :: uint8,
                tcr_module_name     :: module_name,
                tcr_type_name       :: string,
                tcr_arity           :: uint16,
                tcr_unify_pred      :: univ,
                tcr_compare_pred    :: univ,
                tcr_flags           :: set(type_ctor_flag),
                tcr_rep_details     :: type_ctor_details
            ).

    % Each of the following values corresponds to one of the
    % MR_TYPE_CTOR_FLAG_* macros in runtime/mercury_type_info.h.
    % Their meanings are documented there.
    %
:- type type_ctor_flag
    --->    variable_arity_flag
    ;       kind_of_du_flag
    ;       layout_indexable_flag.

    % A type_ctor_details structure contains all the information that the
    % runtime system needs to know about the data representation scheme
    % used by a type constructor.
    %
    % There are three alternatives that correspond to discriminated union:
    % enum, du, and notag. Enum is for types that define only
    % constants. Notag is for types that define only one unary functor.
    % Du is for all other types.
    %
    % All three alternatives have four kinds of information.
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
    % For notag types, the single functor descriptor fills the roles of
    % the second, third and fourth components.
    %
:- type type_ctor_details
    --->    tcd_enum(
                enum_axioms         :: equality_axioms,
                enum_is_dummy       :: enum_maybe_dummy,
                enum_functors       :: list(enum_functor),
                enum_ordinal_table  :: map(uint32, enum_functor),
                enum_name_table     :: map(string, enum_functor),
                enum_functor_number_mapping
                                    :: list(uint32),
                enum_base_type_ctor :: maybe(type_ctor)
            )
    ;       tcd_foreign_enum(
                foreign_enum_language      :: foreign_language,
                foreign_enum_axioms        :: equality_axioms,
                foreign_enum_functors      :: list(foreign_enum_functor),
                foreign_enum_ordinal_table :: map(uint32, foreign_enum_functor),
                foreign_enum_name_table    :: map(string,
                                                foreign_enum_functor),
                foreign_enum_functor_number_mapping
                                           :: list(uint32)
            )
    ;       tcd_du(
                du_axioms           :: equality_axioms,
                du_functors         :: list(du_functor),
                du_value_table      :: ptag_map,
                du_name_table       :: map(string, map(uint16, du_functor)),
                du_functor_number_mapping
                                    :: list(uint32),
                du_base_type_ctor   :: maybe(type_ctor)
            )
    ;       tcd_notag(
                notag_axioms        :: equality_axioms,
                notag_functor       :: notag_functor,
                notag_base_type_ctor
                                    :: maybe(type_ctor)
            )
    ;       tcd_eqv(
                eqv_type            :: rtti_maybe_pseudo_type_info
            )
    ;       tcd_builtin(
                builtin_ctor        :: builtin_ctor
            )
    ;       tcd_impl_artifact(
                impl_ctor           :: impl_ctor
            )
    ;       tcd_foreign(
                is_stable           :: is_stable
            ).

:- type enum_maybe_dummy
    --->    enum_is_not_dummy
    ;       enum_is_dummy.

    % For a given du family type, this says whether the user has defined
    % their own unification predicate for the type.
    %
:- type equality_axioms
    --->    standard
    ;       user_defined.

    % Descriptor for a functor in an enum type.
    %
    % This type corresponds to the C type MR_EnumFunctorDesc.
    %
:- type enum_functor
    --->    enum_functor(
                enum_name           :: string,
                enum_ordinal        :: uint32,
                enum_value          :: enum_value
            ).

:- type enum_value
    --->    enum_value(uint32).

    % Descriptor for a functor in a foreign enum type.
    %
    % This type corresponds to the C Type MR_ForeignEnumFunctorDesc.
    %
:- type foreign_enum_functor
    --->    foreign_enum_functor(
                foreign_enum_name    :: string,
                foreign_enum_ordinal :: uint32,
                foreign_enum_value   :: string
            ).

    % Descriptor for a functor in a notag type.
    %
    % This type corresponds to the C type MR_NotagFunctorDesc.
    %
:- type notag_functor
    --->    notag_functor(
                nt_name             :: string,
                nt_arg_type         :: rtti_maybe_pseudo_type_info,
                nt_arg_name         :: maybe(string),
                nt_subtype_info     :: functor_subtype_info
            ).

    % Descriptor for a functor in a du type.
    %
    % This type mostly corresponds to the C type MR_DuFunctorDesc.
    %
:- type du_functor
    --->    du_functor(
                du_name             :: string,
                du_orig_arity       :: uint16,
                du_ordinal          :: uint32,
                du_rep              :: du_rep,
                du_arg_infos        :: list(du_arg_info),
                du_exist_info       :: maybe(exist_info),
                du_subtype_info     :: functor_subtype_info
            ).

    % Describes the representation of a functor in a general
    % discriminated union type.
    %
    % Will probably need modification for the Java and C# back-ends.
    %
:- type du_rep
    --->    du_ll_rep(
                du_ll_ptag          :: ptag,
                du_ll_sec_tag       :: sectag_and_locn
            )
    ;       du_hl_rep(
                remote_sec_tag      :: uint
            ).

    % Describes the types of the existentially typed arguments of a
    % discriminated union functor.
    %
    % This type corresponds to the C type MR_DuExistInfo.
    %
:- type exist_info
    --->    exist_info(
                exist_num_plain_typeinfos   :: uint16,
                exist_num_typeinfos_in_tcis :: uint16,
                exist_typeclass_constraints :: list(tc_constraint),
                exist_typeinfo_locns        :: list(exist_typeinfo_locn)
            ).

    % Describes the location at which one can find the typeinfo for the
    % type bound to an existentially quantified type variable in a
    % discriminated union functor.
    %
    % This type corresponds to the C type MR_DuExistLocn.
    %
:- type exist_typeinfo_locn
    --->    plain_typeinfo(
                % The typeinfo is stored directly in the cell, at this offset.
                uint16
            )
    ;       typeinfo_in_tci(
                % The typeinfo is stored indirectly in the typeclass info
                % stored at this offset in the cell.
                uint16,

                % To find the typeinfo inside the typeclass info structure,
                % give this integer to the MR_typeclass_info_type_info macro.
                uint16
            ).

    % These tables let the runtime system interpret values in memory
    % of general discriminated union types.
    %
    % The runtime system should first use the primary tag to index/search into
    % the type's ptag_map. It can then find the location (if any) of the
    % secondary tag, and use the secondary tag (or zero if there isn't one)
    % to index/search into the stag_map to find the functor descriptor.
    %
    % The type sectag_table corresponds to the C type MR_DuPtagLayout.
    % The two maps are implemented in C as simple arrays.
    %
:- type ptag_map == map(ptag, sectag_table).  % key is primary tag
:- type stag_map == map(uint, du_functor).    % key is secondary tag

    % Each of the following fields corresponds to one of the
    % MR_DU_PTAG_FLAG_* macros in runtime/mercury_type_info.h.
    % Their meanings are documented there.
    %
:- type du_ptag_layout_flags
    --->    du_ptag_layout_flags(
                sectag_alternatives_indexable :: bool
            ).

:- type sectag_table
    --->    sectag_table(
                sectag_locn         :: sectag_locn,
                sectag_num_bits     :: int8,
                sectag_num_sharers  :: uint32,
                sectag_map          :: stag_map
            ).

    % Describes the location of the secondary tag for a given primary tag
    % value in a given type.
    %
:- type sectag_locn
    --->    sectag_none
    ;       sectag_none_direct_arg
    ;       sectag_local_rest_of_word
    ;       sectag_local_bits(uint8, uint)              % #bits, mask
    ;       sectag_remote_word
    ;       sectag_remote_bits(uint8, uint).            % #bits, mask

    % Describes the location, maybe size, and value of the secondary tag,
    % for a given functor in a given type.
    %
:- type sectag_and_locn
    --->    sectag_locn_none
    ;       sectag_locn_none_direct_arg
    ;       sectag_locn_local_rest_of_word(uint)        % value
    ;       sectag_locn_local_bits(uint, uint8, uint)   % value, #bits, mask
    ;       sectag_locn_remote_word(uint)               % value
    ;       sectag_locn_remote_bits(uint, uint8, uint). % value, #bits, mask

    % Information about an argument of a functor in a discriminated union type.
    %
:- type du_arg_info
    --->    du_arg_info(
                du_arg_name         :: maybe(string),
                du_arg_type         :: rtti_maybe_pseudo_type_info_or_self,
                du_arg_pos_width    :: arg_pos_width
            ).

    % Information about subtype constraints on the arguments of a functor
    % due to inst information provided in the type definition. This is not
    % related to the subtypes introduced by ':- type SUBTYPE =< SUPERTYPE'
    % definitions.
    %
    % XXX rename this type and constants to avoid confusion with subtype
    % type definitions
    %
:- type functor_subtype_info
    --->    functor_subtype_none
    ;       functor_subtype_exists.

    % An rtti_maybe_pseudo_type_info identifies the type of a function
    % symbol's argument. If the type of the argument is the same as the
    % type of the whole term, it should be bound to self. Otherwise, if
    % the argument's type is ground, it should be bound to plain; if it
    % is non-ground, it should be bound to pseudo.
    %
:- type rtti_maybe_pseudo_type_info_or_self
    --->    pseudo(rtti_pseudo_type_info)
    ;       plain(rtti_type_info)
    ;       self.

    % The list of type constructors for types that are built into the
    % Mercury language or the Mercury standard library.
    %
:- type builtin_ctor
    --->    builtin_ctor_int
    ;       builtin_ctor_uint
    ;       builtin_ctor_int8
    ;       builtin_ctor_uint8
    ;       builtin_ctor_int16
    ;       builtin_ctor_uint16
    ;       builtin_ctor_int32
    ;       builtin_ctor_uint32
    ;       builtin_ctor_int64
    ;       builtin_ctor_uint64
    ;       builtin_ctor_float
    ;       builtin_ctor_char
    ;       builtin_ctor_string
    ;       builtin_ctor_void
    ;       builtin_ctor_c_pointer(is_stable)
    ;       builtin_ctor_pred_ctor
    ;       builtin_ctor_func_ctor
    ;       builtin_ctor_tuple
    ;       builtin_ctor_ref
    ;       builtin_ctor_type_desc
    ;       builtin_ctor_pseudo_type_desc
    ;       builtin_ctor_type_ctor_desc.

    % The list of type constructors that are used behind the scenes by
    % the Mercury implementation.
    %
:- type impl_ctor
    --->    impl_ctor_hp
    ;       impl_ctor_succip
    ;       impl_ctor_maxfr
    ;       impl_ctor_curfr
    ;       impl_ctor_redofr
    ;       impl_ctor_redoip
    ;       impl_ctor_ticket
    ;       impl_ctor_trail_ptr
    ;       impl_ctor_type_info
    ;       impl_ctor_type_ctor_info
    ;       impl_ctor_typeclass_info
    ;       impl_ctor_base_typeclass_info
    ;       impl_ctor_subgoal.

:- type is_stable
    --->    is_stable
    ;       is_not_stable.

%-----------------------------------------------------------------------------%
%
% The data structures representing type class dictionaries.
%

    % A base_typeclass_info holds information about a typeclass instance.
    % See notes/type_class_transformation.html for details.
    %
:- type base_typeclass_info
    --->    base_typeclass_info(
                % Num_extra = num_unconstrained + num_constraints,
                % where num_unconstrained is the number of unconstrained
                % type variables from the head of the instance declaration.
                num_extra           :: int,

                % Num_constraints is the number of constraints
                % on the instance declaration.
                num_constraints     :: int,

                % Num_superclasses is the number of constraints
                % on the typeclass declaration.
                num_superclasses    :: int,

                % Class_arity is the number of type variables in the head
                % of the class declaration.
                class_arity         :: int,

                % Num_methods is the number of procedures in the typeclass
                % declaration.
                num_methods         :: int,

                % Methods is a list of length num_methods containing the
                % addresses of the methods for this instance declaration.
                methods             :: list(rtti_proc_label)
            ).

%-----------------------------------------------------------------------------%
%
% The types in this block (until the next horizontal line) will eventually
% replace base_typeclass_infos. For now, the C data structures they describe
% are generated only on request, and used only by the debugger.

    % This type corresponds to the C type MR_TypeClassMethod.
    %
:- type tc_method_id
    --->    tc_method_id(
                tcm_name                :: string,
                tcm_arity               :: int,
                tcm_pred_or_func        :: pred_or_func
            ).

    % Uniquely identifies a type class.
    %
:- type tc_name
    --->    tc_name(
                tcn_module              :: module_name,
                tcn_name                :: string,
                tcn_arity               :: int
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
    %
:- type tc_id
    --->    tc_id(
                tc_id_name              :: tc_name,
                tc_id_type_var_names    :: list(string),
                tc_id_methods           :: list(tc_method_id)
            ).

:- type tc_decl
    --->    tc_decl(
                tc_decl_id              :: tc_id,
                tc_decl_version_number  :: int,
                tc_decl_supers          :: list(tc_constraint)
            ).

:- type tc_type == rtti_maybe_pseudo_type_info.

    % This type corresponds to the C type MR_TypeClassConstraint_NStruct,
    % where N is the length of the list in the tcc_types field.
    %
:- type tc_constraint
    --->    tc_constraint(
                tcc_class_name          :: tc_name,
                tcc_types               :: list(tc_type)
            ).

    % Uniquely identifies an instance declaration, and gives information
    % about the declaration that we need to interpret other data
    % structures related to the type class.
    %
    % This type corresponds to the C type MR_Instance.
    %
:- type tc_instance
    --->    tc_instance(
                tci_type_class          :: tc_name,
                tci_types               :: list(tc_type),
                tci_num_type_vars       :: int,
                tci_constraints         :: list(tc_constraint),
                tci_methods             :: list(rtti_proc_label)
            ).

    % This type corresponds to the C type MR_ClassDict.
    %
    % XXX We don't yet use this type.
:- type tc_dict
    --->    tc_dict(
                tcd_class               :: tc_name,
                tcd_types               :: list(rtti_type_info),
                tcd_methods             :: list(rtti_proc_label)
            ).

%-----------------------------------------------------------------------------%
%
% The data structures representing the top-level global data structures
% generated by the Mercury compiler. Usually readonly, with one exception:
% data containing code addresses must be initialized at runtime in grades
% that don't support static code initializers.

:- type rtti_data
    --->    rtti_data_type_ctor_info(
                type_ctor_data
            )
    ;       rtti_data_type_info(
                rtti_type_info
            )
    ;       rtti_data_pseudo_type_info(
                rtti_pseudo_type_info
            )
    ;       rtti_data_base_typeclass_info(
                % The id of the type class.
                tc_name,

                % The module containing the instance declaration.
                module_name,

                % A string that uniquely and reversibly encodes
                % the names and arities of the types in the
                % instance declaration.
                string,

                base_typeclass_info
            )
    ;       rtti_data_type_class_decl(
                tc_decl
            )
    ;       rtti_data_type_class_instance(
                tc_instance
            ).

% All rtti_data data structures and all their components are identified
% by an rtti_id. For data structures that are part of the description
% of a single type constructor, we use the ctor_rtti_id functor, and make the
% id of that type constructor part of the id of the data structure.
% For data structures that are not necessarily associated with a single type,
% which for the foreseeable future are all associated with typeclasses,
% we use the tc_rtti_id functor.

:- type rtti_id
    --->    ctor_rtti_id(rtti_type_ctor, ctor_rtti_name)
    ;       tc_rtti_id(tc_name, tc_rtti_name).

:- type ctor_rtti_name
    --->    type_ctor_exist_locns(uint32)               % functor ordinal
    ;       type_ctor_exist_locn
    ;       type_ctor_exist_tc_constr(uint32, int, int) % functor ordinal,
                                                        % constraint ordinal,
                                                        % constraint arity
    ;       type_ctor_exist_tc_constrs(uint32)          % functor ordinal
    ;       type_ctor_exist_info(uint32)                % functor ordinal
    ;       type_ctor_field_names(uint32)               % functor ordinal
    ;       type_ctor_field_types(uint32)               % functor ordinal
    ;       type_ctor_field_locns(uint32)               % functor ordinal
    ;       type_ctor_enum_functor_desc(uint32)         % functor ordinal
    ;       type_ctor_foreign_enum_functor_desc(uint32) % functor ordinal
    ;       type_ctor_notag_functor_desc
    ;       type_ctor_du_functor_desc(uint32)           % functor ordinal
    ;       type_ctor_enum_name_ordered_table
    ;       type_ctor_enum_ordinal_ordered_table
    ;       type_ctor_foreign_enum_name_ordered_table
    ;       type_ctor_foreign_enum_ordinal_ordered_table
    ;       type_ctor_du_name_ordered_table
    ;       type_ctor_du_stag_ordered_table(ptag)
    ;       type_ctor_du_ptag_ordered_table
    ;       type_ctor_du_ptag_layout(ptag)
    ;       type_ctor_functor_number_map
    ;       type_ctor_type_functors
    ;       type_ctor_type_layout
    ;       type_ctor_type_ctor_info
    ;       type_ctor_type_info(rtti_type_info)
    ;       type_ctor_pseudo_type_info(rtti_pseudo_type_info)
    ;       type_ctor_type_hashcons_pointer.

:- type tc_rtti_name
    --->    type_class_base_typeclass_info(
                % The name of the module containing the instance declaration.
                module_name,

                % A string that uniquely and reversibly encodes
                % the names and arities of the types in the
                % instance declaration.
                string
            )
    ;       type_class_id
    ;       type_class_id_var_names
    ;       type_class_id_method_ids
    ;       type_class_decl
    ;       type_class_decl_super(int, int)
                % superclass ordinal, constraint arity
    ;       type_class_decl_supers
    ;       type_class_instance(list(tc_type))
    ;       type_class_instance_tc_type_vector(list(tc_type))
    ;       type_class_instance_constraint(list(tc_type), int, int)
                % constraint ordinal, constraint arity
    ;       type_class_instance_constraints(list(tc_type))
    ;       type_class_instance_methods(list(tc_type)).

%-----------------------------------------------------------------------------%
%
% Functions operating on RTTI data.
%

:- func encode_type_ctor_flags(set(type_ctor_flag)) = uint16.

:- func encode_du_ptag_layout_flags(du_ptag_layout_flags) = uint8.

    % Convert a rtti_data to an rtti_id.
    % This calls error/1 if the argument is a type_var/1 rtti_data,
    % since there is no rtti_id to return in that case.
    %
:- pred rtti_data_to_id(rtti_data::in, rtti_id::out) is det.

    % Return the id of the type constructor.
    %
:- func tcd_get_rtti_type_ctor(type_ctor_data) = rtti_type_ctor.

    % Convert an id that specifies a kind of variable arity type_info
    % or pseudo_type_info into the type_ctor of the canonical (arity-zero)
    % type of that kind.
    %
:- func var_arity_id_to_rtti_type_ctor(var_arity_ctor_id) = rtti_type_ctor.

:- type rtti_id_maybe_element
    --->    item_type(rtti_id)
            % The type is the type of the data structure identified by the
            % rtti_id.

    ;       element_type(rtti_id).
            % The type is the type of the elements of the data structure
            % identified by the rtti_id, which must be an array.

:- type is_array
    --->    is_array
    ;       not_array.

    % Return is_array iff the specified entity is an array.
    %
:- func rtti_id_maybe_element_has_array_type(rtti_id_maybe_element) = is_array.
:- func rtti_id_has_array_type(rtti_id) = is_array.
:- func ctor_rtti_name_has_array_type(ctor_rtti_name) = is_array.
:- func tc_rtti_name_has_array_type(tc_rtti_name) = is_array.

    % Return yes iff the specified entity should be exported
    % for use by other modules.
    %
:- func rtti_id_is_exported(rtti_id) = bool.
:- func ctor_rtti_name_is_exported(ctor_rtti_name) = bool.
:- func tc_rtti_name_is_exported(tc_rtti_name) = bool.

    % Return the C variable name of the RTTI data structure identified
    % by the input argument.
    %
:- pred id_to_c_identifier(rtti_id::in, string::out) is det.

    % Given a type in a type vector in a type class instance declaration,
    % return its string encoding for use in RTTI data structures, e.g. as
    % part of C identifiers.
    %
:- func encode_tc_instance_type(tc_type) = string.

    % Return the C representation of the type_ctor_rep value of the given
    % type_ctor.
    %
:- pred type_ctor_rep_to_string(type_ctor_data::in,
    target_prefixes::out, string::out) is det.

    % Return a name which identifies the rtti_type_info
    %
:- func type_info_to_string(rtti_type_info) = string.

    % Return a name which identifies the pseudo_type_info
    %
:- func pseudo_type_info_to_string(rtti_pseudo_type_info) = string.

:- type target_prefixes
    --->    target_prefixes(
                % The prefixes that mlds_to_{java,cs}.m respectively
                % need to put in front of the attached string.
                java_prefix     :: string,
                csharp_prefix   :: string
            ).

    % Return the C representation of a pred_or_func indication.
    %
:- pred pred_or_func_to_string(pred_or_func::in,
    target_prefixes::out, string::out) is det.

    % Return the C representation of a secondary tag location.
    %
:- pred sectag_locn_to_string(sectag_locn::in,
    target_prefixes::out, string::out) is det.

    % Return the C representation of a secondary tag location.
    %
:- pred sectag_and_locn_to_locn_string(sectag_and_locn::in,
    target_prefixes::out, string::out)
    is det.

    % Return the C representation of a functor's subtype constraints info.
    %
:- pred functor_subtype_info_to_string(functor_subtype_info::in,
    target_prefixes::out, string::out) is det.

    % Return the rtti_data containing the given type_info.
    %
:- func type_info_to_rtti_data(rtti_type_info) = rtti_data.

    % Return the rtti_data containing the given type_info or
    % pseudo_type_info.
    %
:- func maybe_pseudo_type_info_to_rtti_data(rtti_maybe_pseudo_type_info)
    = rtti_data.

    % Return the rtti_data containing the given type_info or
    % pseudo_type_info or self.
    %
:- func maybe_pseudo_type_info_or_self_to_rtti_data(
    rtti_maybe_pseudo_type_info_or_self) = rtti_data.

    % Given a type constructor with the given details, return `yes(NumPtags)'
    % where NumPtags is the number of primary tag values used by the type,
    % or `no' if the type constructor doesn't use primary tags.
    %
:- func type_ctor_details_num_ptags(type_ctor_details) = maybe(int).

    % Given a type constructor with the given details, return
    % `yes(NumFunctors)' where NumFunctors is the number of function symbols
    % defined by the type, or `no' if the type constructor doesn't define any
    % function symbols.
    %
:- func type_ctor_details_num_functors(type_ctor_details) = maybe(int).

    % Extract the argument name (if any) from a du_arg_info.
    %
:- func du_arg_info_name(du_arg_info) = maybe(string).

    % Extract the argument type from a du_arg_info.
    %
:- func du_arg_info_type(du_arg_info) = rtti_maybe_pseudo_type_info_or_self.

    % Extract the argument position and width from the du_arg_info.
    %
:- func du_arg_info_pos_width(du_arg_info) = arg_pos_width.

    % Return the symbolic representation of the address of the given
    % functor descriptor.
    %
:- func enum_functor_rtti_name(enum_functor) = ctor_rtti_name.
:- func foreign_enum_functor_rtti_name(foreign_enum_functor) = ctor_rtti_name.
:- func du_functor_rtti_name(du_functor) = ctor_rtti_name.

    % Return true iff the given type of RTTI data structure includes
    % code addresses.
    %
:- func rtti_id_would_include_code_addr(rtti_id) = bool.
:- func ctor_rtti_name_would_include_code_addr(ctor_rtti_name) = bool.
:- func tc_rtti_name_would_include_code_addr(tc_rtti_name) = bool.

    % Return true iff the given type_info's or pseudo_type_info's RTTI
    % data structure includes code addresses.
    %
:- func type_info_would_incl_code_addr(rtti_type_info) = bool.
:- func pseudo_type_info_would_incl_code_addr(rtti_pseudo_type_info) = bool.

    % rtti_id_c_type(RttiId, Type, IsArray):
    %
    % To declare a variable of the type specified by RttiId, put Type
    % before the name of the variable; if IsArray is true, also put "[]"
    % after the name.
    %
:- pred rtti_id_maybe_element_c_type(rtti_id_maybe_element::in, string::out,
    is_array::out) is det.
:- pred rtti_id_c_type(rtti_id::in, string::out, is_array::out) is det.
:- pred ctor_rtti_name_c_type(ctor_rtti_name::in, string::out, is_array::out)
    is det.
:- pred tc_rtti_name_c_type(tc_rtti_name::in, string::out, is_array::out)
    is det.

    % Analogous to rtti_id_c_type.
    %
:- pred rtti_id_maybe_element_java_type(rtti_id_maybe_element::in, string::out,
    is_array::out) is det.
:- pred rtti_id_java_type(rtti_id::in, string::out, is_array::out) is det.
:- pred ctor_rtti_name_java_type(ctor_rtti_name::in, string::out,
    is_array::out) is det.
:- pred tc_rtti_name_java_type(tc_rtti_name::in, string::out, is_array::out)
    is det.

    % Analogous to rtti_id_c_type.
    %
:- pred rtti_id_maybe_element_csharp_type(rtti_id_maybe_element::in,
    string::out, is_array::out) is det.
:- pred rtti_id_csharp_type(rtti_id::in, string::out, is_array::out) is det.
:- pred ctor_rtti_name_csharp_type(ctor_rtti_name::in, string::out,
    is_array::out) is det.
:- pred tc_rtti_name_csharp_type(tc_rtti_name::in, string::out, is_array::out)
    is det.

    % Return yes iff the name of the given data structure should be module
    % qualified.
    %
:- func module_qualify_name_of_rtti_id(rtti_id) = bool.
:- func module_qualify_name_of_ctor_rtti_name(ctor_rtti_name) = bool.
:- func module_qualify_name_of_tc_rtti_name(tc_rtti_name) = bool.

    % If the given rtti_id is implemented as a single MR_TypeCtorInfo,
    % return the identity of the type constructor.
    %
:- pred rtti_id_emits_type_ctor_info(rtti_id::in, rtti_type_ctor::out)
    is semidet.

%----------------------------------------------------------------------------%

:- type call_or_answer_table
    --->    call_table
    ;       answer_table.

:- type curr_or_prev_table
    --->    curr_table
    ;       prev_table.

:- type proc_tabling_struct_id
    --->    tabling_info
            % A reference to the main structure containing the call table
            % used to implement memoization, loop checking or minimal model
            % semantics for the given procedure.

    ;       tabling_ptis
            % A reference to the part of the tabling structure for the given
            % procedure that contains pointers to the pseudotypeinfos
            % describing the procedure's arguments.

    ;       tabling_type_param_locns
            % A reference to the part of the tabling structure for the given
            % procedure that contains pointers to the locations of the
            % typeinfos that give the parameters of the pseudotypeinfos
            % in the tabling_ptis array.

    ;       tabling_root_node
            % A reference to the part of the tabling structure for the given
            % procedure that contains the root of the call table.

    ;       tabling_steps_desc(call_or_answer_table)
            % A reference to the part of the tabling structure for the given
            % procedure that gives the nature of each step in the call or
            % answer table.

    ;       tabling_stats(call_or_answer_table, curr_or_prev_table)
            % A reference to the part of the tabling structure for the given
            % procedure that refers to the either the current or the previous
            % versions of the statistics about overall operations on the
            % call or answer table.

    ;       tabling_stat_steps(call_or_answer_table, curr_or_prev_table)
            % A reference to the part of the tabling structure for the given
            % procedure that refers to the either the current or the previous
            % versions of the statistics about operations on the steps of the
            % call or answer table.

    ;       tabling_tips.
            % A reference to the part of the tabling structure for the given
            % procedure that contains pointers to the current set of call table
            % tips, for use as a pool of replacements with limited size tables.

:- func tabling_info_id_str(proc_tabling_struct_id) = string.

    % tabling_id_c_type(TablingId, Type, IsArray):
    %
    % To declare a variable of the type specified by TablingId, put Type
    % before the name of the variable; if IsArray = is_array, also put "[]"
    % after the name.
    %
:- pred tabling_id_c_type(proc_tabling_struct_id::in, string::out,
    is_array::out) is det.

:- pred tabling_id_java_type(proc_tabling_struct_id::in, string::out,
    is_array::out) is det.

:- func tabling_id_has_array_type(proc_tabling_struct_id) = is_array.

:- pred table_trie_step_to_c(table_trie_step::in, string::out, maybe(int)::out)
    is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.name_mangle.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module table_builtin.
:- import_module uint16.
:- import_module uint8.

%----------------------------------------------------------------------------%

encode_type_ctor_flags(FlagSet) = Encoding :-
    set.to_sorted_list(FlagSet, FlagList),
    list.foldl(encode_type_ctor_flag, FlagList, 0u16, Encoding).

    % NOTE: the encoding here must match the one in
    % runtime/mercury_type_info.h.
    %
    % Also note: we used to use 1 to encode types that reserved a tag
    % for constraint solvers.
    %
:- pred encode_type_ctor_flag(type_ctor_flag::in, uint16::in, uint16::out)
    is det.

encode_type_ctor_flag(variable_arity_flag, !Encoding) :-
    !:Encoding = !.Encoding + 2u16.
encode_type_ctor_flag(kind_of_du_flag, !Encoding) :-
    !:Encoding = !.Encoding + 4u16.
encode_type_ctor_flag(layout_indexable_flag, !Encoding) :-
    !:Encoding = !.Encoding + 8u16.

    % NOTE: the encoding here must match the one in
    % runtime/mercury_type_info.h.
    %
encode_du_ptag_layout_flags(Flags) = Encoding :-
    Flags = du_ptag_layout_flags(SectagAltsIndexable),
    (
        SectagAltsIndexable = yes,
        Encoding = 1u8
    ;
        SectagAltsIndexable = no,
        Encoding = 0u8
    ).

rtti_data_to_id(RttiData, RttiId) :-
    (
        RttiData = rtti_data_type_ctor_info(TypeCtorData),
        RttiTypeCtor = tcd_get_rtti_type_ctor(TypeCtorData),
        RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info)
    ;
        RttiData = rtti_data_type_info(TypeInfo),
        RttiTypeCtor = ti_get_rtti_type_ctor(TypeInfo),
        RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_info(TypeInfo))
    ;
        RttiData = rtti_data_pseudo_type_info(PseudoTypeInfo),
        RttiTypeCtor = pti_get_rtti_type_ctor(PseudoTypeInfo),
        RttiId = ctor_rtti_id(RttiTypeCtor,
            type_ctor_pseudo_type_info(PseudoTypeInfo))
    ;
        RttiData = rtti_data_base_typeclass_info(TCName, Module, Instance, _),
        TCId = type_class_base_typeclass_info(Module, Instance),
        RttiId = tc_rtti_id(TCName, TCId)
    ;
        RttiData = rtti_data_type_class_decl(tc_decl(TCId, _, _)),
        TCId = tc_id(TCName, _, _),
        RttiId = tc_rtti_id(TCName, type_class_decl)
    ;
        RttiData = rtti_data_type_class_instance(
            tc_instance(TCName, TCTypes, _, _, _)),
        RttiId = tc_rtti_id(TCName, type_class_instance(TCTypes))
    ).

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
    % There is no rtti_type_ctor associated with a type_var.
    unexpected($pred, "type_var").

var_arity_id_to_rtti_type_ctor(pred_type_info) = Ctor :-
    Builtin = mercury_public_builtin_module,
    Ctor = rtti_type_ctor(Builtin, "pred", 0u16).
var_arity_id_to_rtti_type_ctor(func_type_info) = Ctor :-
    Builtin = mercury_public_builtin_module,
    Ctor = rtti_type_ctor(Builtin, "func", 0u16).
var_arity_id_to_rtti_type_ctor(tuple_type_info) = Ctor :-
    Builtin = mercury_public_builtin_module,
    Ctor = rtti_type_ctor(Builtin, "tuple", 0u16).

rtti_id_maybe_element_has_array_type(item_type(RttiId)) =
    rtti_id_has_array_type(RttiId).
rtti_id_maybe_element_has_array_type(element_type(RttiId)) = not_array :-
    expect(unify(rtti_id_has_array_type(RttiId), is_array), $pred,
        "base is not array").

rtti_id_has_array_type(ctor_rtti_id(_, RttiName)) =
    ctor_rtti_name_has_array_type(RttiName).
rtti_id_has_array_type(tc_rtti_id(_, TCRttiName)) =
    tc_rtti_name_has_array_type(TCRttiName).

ctor_rtti_name_has_array_type(RttiName) = IsArray :-
    ctor_rtti_name_type(RttiName, _, IsArray).

tc_rtti_name_has_array_type(TCRttiName) = IsArray :-
    tc_rtti_name_type(TCRttiName, _, IsArray).

rtti_id_is_exported(ctor_rtti_id(_, RttiName)) =
    ctor_rtti_name_is_exported(RttiName).
rtti_id_is_exported(tc_rtti_id(_, TCRttiName)) =
    tc_rtti_name_is_exported(TCRttiName).

ctor_rtti_name_is_exported(CtorRttiName) = IsExported :-
    (
        ( CtorRttiName = type_ctor_exist_locns(_)
        ; CtorRttiName = type_ctor_exist_locn
        ; CtorRttiName = type_ctor_exist_tc_constr(_, _, _)
        ; CtorRttiName = type_ctor_exist_tc_constrs(_)
        ; CtorRttiName = type_ctor_exist_info(_)
        ; CtorRttiName = type_ctor_field_names(_)
        ; CtorRttiName = type_ctor_field_types(_)
        ; CtorRttiName = type_ctor_field_locns(_)
        ; CtorRttiName = type_ctor_enum_functor_desc(_)
        ; CtorRttiName = type_ctor_foreign_enum_functor_desc(_)
        ; CtorRttiName = type_ctor_notag_functor_desc
        ; CtorRttiName = type_ctor_du_functor_desc(_)
        ; CtorRttiName = type_ctor_enum_name_ordered_table
        ; CtorRttiName = type_ctor_enum_ordinal_ordered_table
        ; CtorRttiName = type_ctor_foreign_enum_name_ordered_table
        ; CtorRttiName = type_ctor_foreign_enum_ordinal_ordered_table
        ; CtorRttiName = type_ctor_du_name_ordered_table
        ; CtorRttiName = type_ctor_du_stag_ordered_table(_)
        ; CtorRttiName = type_ctor_du_ptag_ordered_table
        ; CtorRttiName = type_ctor_du_ptag_layout(_)
        ; CtorRttiName = type_ctor_functor_number_map
        ; CtorRttiName = type_ctor_type_functors
        ; CtorRttiName = type_ctor_type_layout
        ; CtorRttiName = type_ctor_type_hashcons_pointer
        ),
        IsExported = no
    ;
        CtorRttiName = type_ctor_type_ctor_info,
        IsExported = yes
    ;
        CtorRttiName = type_ctor_type_info(TypeInfo),
        IsExported = type_info_is_exported(TypeInfo)
    ;
        CtorRttiName = type_ctor_pseudo_type_info(PseudoTypeInfo),
        IsExported = pseudo_type_info_is_exported(PseudoTypeInfo)
    ).

tc_rtti_name_is_exported(TCName) = IsExported :-
    (
        ( TCName = type_class_base_typeclass_info(_, _)
        ; TCName = type_class_instance(_)
        ; TCName = type_class_decl
        ),
        IsExported = yes
    ;
        ( TCName = type_class_id
        ; TCName = type_class_id_var_names
        ; TCName = type_class_id_method_ids
        ; TCName = type_class_decl_super(_, _)
        ; TCName = type_class_decl_supers
        ; TCName = type_class_instance_tc_type_vector(_)
        ; TCName = type_class_instance_constraint(_, _, _)
        ; TCName = type_class_instance_constraints(_)
        ; TCName = type_class_instance_methods(_)
        ),
        IsExported = no
    ).

:- func type_info_is_exported(rtti_type_info) = bool.

type_info_is_exported(TypeInfo) = IsExported :-
    (
        TypeInfo = plain_arity_zero_type_info(_),
        IsExported = yes
    ;
        ( TypeInfo = plain_type_info(_, _)
        ; TypeInfo = var_arity_type_info(_, _)
        ),
        IsExported = no
    ).

:- func pseudo_type_info_is_exported(rtti_pseudo_type_info) = bool.

pseudo_type_info_is_exported(PseudoTypeInfo) = IsExported :-
    (
        PseudoTypeInfo = plain_arity_zero_pseudo_type_info(_),
        IsExported = yes
    ;
        ( PseudoTypeInfo = plain_pseudo_type_info(_, _)
        ; PseudoTypeInfo = var_arity_pseudo_type_info(_, _)
        ; PseudoTypeInfo = type_var(_)
        ),
        IsExported = no
    ).

id_to_c_identifier(ctor_rtti_id(RttiTypeCtor, RttiName), Str) :-
    Str = name_to_string(RttiTypeCtor, RttiName).
id_to_c_identifier(tc_rtti_id(TCName, TCRttiName), Str) :-
    tc_name_to_string(TCName, TCRttiName, Str).

:- func name_to_string(rtti_type_ctor, ctor_rtti_name) = string.

name_to_string(RttiTypeCtor, RttiName) = Str :-
    mangle_rtti_type_ctor(RttiTypeCtor, ModuleName, TypeName, A_str),
    (
        RttiName = type_ctor_exist_locns(Ordinal),
        O_str = string.uint32_to_string(Ordinal),
        string.append_list([ModuleName, "__exist_locns_",
            TypeName, "_", A_str, "_", O_str], Str)
    ;
        RttiName = type_ctor_exist_locn,
        string.append_list([ModuleName, "__exist_locn_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_exist_tc_constr(Ordinal, TCCNum, _),
        O_str = string.uint32_to_string(Ordinal),
        N_str = string.int_to_string(TCCNum),
        string.append_list([ModuleName, "__exist_tc_constr_",
            TypeName, "_", A_str, "_", O_str, "_", N_str], Str)
    ;
        RttiName = type_ctor_exist_tc_constrs(Ordinal),
        O_str = string.uint32_to_string(Ordinal),
        string.append_list([ModuleName, "__exist_tc_constrs_",
            TypeName, "_", A_str, "_", O_str], Str)
    ;
        RttiName = type_ctor_exist_info(Ordinal),
        O_str = string.uint32_to_string(Ordinal),
        string.append_list([ModuleName, "__exist_info_",
            TypeName, "_", A_str, "_", O_str], Str)
    ;
        RttiName = type_ctor_field_names(Ordinal),
        O_str = string.uint32_to_string(Ordinal),
        string.append_list([ModuleName, "__field_names_",
            TypeName, "_", A_str, "_", O_str], Str)
    ;
        RttiName = type_ctor_field_types(Ordinal),
        O_str = string.uint32_to_string(Ordinal),
        string.append_list([ModuleName, "__field_types_",
            TypeName, "_", A_str, "_", O_str], Str)
    ;
        RttiName = type_ctor_field_locns(Ordinal),
        O_str = string.uint32_to_string(Ordinal),
        string.append_list([ModuleName, "__field_locns_",
            TypeName, "_", A_str, "_", O_str], Str)
    ;
        RttiName = type_ctor_enum_functor_desc(Ordinal),
        O_str = string.uint32_to_string(Ordinal),
        string.append_list([ModuleName, "__enum_functor_desc_",
            TypeName, "_", A_str, "_", O_str], Str)
    ;
        RttiName = type_ctor_foreign_enum_functor_desc(Ordinal),
        O_str = string.uint32_to_string(Ordinal),
        string.append_list([ModuleName, "__foreign_enum_functor_desc_",
            TypeName, "_", A_str, "_", O_str], Str)
    ;
        RttiName = type_ctor_notag_functor_desc,
        string.append_list([ModuleName, "__notag_functor_desc_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_du_functor_desc(Ordinal),
        O_str = string.uint32_to_string(Ordinal),
        string.append_list([ModuleName, "__du_functor_desc_",
            TypeName, "_", A_str, "_", O_str], Str)
    ;
        RttiName = type_ctor_enum_name_ordered_table,
        string.append_list([ModuleName, "__enum_name_ordered_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_enum_ordinal_ordered_table,
        string.append_list([ModuleName, "__enum_ordinal_ordered_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_foreign_enum_name_ordered_table,
        string.append_list([ModuleName, "__foreign_enum_name_ordered_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_foreign_enum_ordinal_ordered_table,
        string.append_list([ModuleName, "__foreign_enum_ordinal_ordered_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_du_name_ordered_table,
        string.append_list([ModuleName, "__du_name_ordered_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_du_stag_ordered_table(Ptag),
        Ptag = ptag(PtagUint8),
        P_str = string.uint8_to_string(PtagUint8),
        string.append_list([ModuleName, "__du_stag_ordered_",
            TypeName, "_", A_str, "_", P_str], Str)
    ;
        RttiName = type_ctor_du_ptag_ordered_table,
        string.append_list([ModuleName, "__du_ptag_ordered_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_du_ptag_layout(Ptag),
        Ptag = ptag(PtagUint8),
        P_str = string.uint8_to_string(PtagUint8),
        string.append_list([ModuleName, "__du_ptag_layout_",
            TypeName, "_", A_str, "_", P_str], Str)
    ;
        RttiName = type_ctor_functor_number_map,
        string.append_list([ModuleName, "__functor_number_map_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_type_functors,
        string.append_list([ModuleName, "__type_functors",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_type_layout,
        string.append_list([ModuleName, "__type_layout",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_type_ctor_info,
        string.append_list([ModuleName, "__type_ctor_info_",
            TypeName, "_", A_str], Str)
    ;
        RttiName = type_ctor_type_info(TypeInfo),
        Str = type_info_to_string(TypeInfo)
    ;
        RttiName = type_ctor_pseudo_type_info(PseudoTypeInfo),
        Str = pseudo_type_info_to_string(PseudoTypeInfo)
    ;
        RttiName = type_ctor_type_hashcons_pointer,
        string.append_list([ModuleName, "__hashcons_ptr_",
            TypeName, "_", A_str], Str)
    ).

:- pred tc_name_to_string(tc_name::in, tc_rtti_name::in, string::out) is det.

tc_name_to_string(TCName, TCRttiName, Str) :-
    (
        TCRttiName = type_class_base_typeclass_info(_ModuleName, InstanceStr),
        Str = make_base_typeclass_info_name(TCName, InstanceStr)
    ;
        TCRttiName = type_class_id,
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        Str = ModuleName ++ "__type_class_id_" ++ ClassName ++ "_" ++ ArityStr
    ;
        TCRttiName = type_class_id_method_ids,
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        Str = ModuleName ++ "__type_class_id_method_ids_" ++ ClassName
            ++ "_" ++ ArityStr
    ;
        TCRttiName = type_class_id_var_names,
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        Str = ModuleName ++ "__type_class_id_var_names_" ++ ClassName
            ++ "_" ++ ArityStr
    ;
        TCRttiName = type_class_decl,
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        Str = ModuleName ++ "__type_class_decl_" ++ ClassName
            ++ "_" ++ ArityStr
    ;
        TCRttiName = type_class_decl_supers,
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        Str = ModuleName ++ "__type_class_decl_supers_" ++ ClassName
            ++ "_" ++ ArityStr
    ;
        TCRttiName = type_class_decl_super(Ordinal, _),
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        string.int_to_string(Ordinal, OrdinalStr),
        Str = ModuleName ++ "__type_class_decl_super_" ++ ClassName ++
            "_" ++ ArityStr ++ "_" ++ OrdinalStr
    ;
        TCRttiName = type_class_instance(TCTypes),
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        TypeStrs = list.map(encode_tc_instance_type, TCTypes),
        TypeVectorStr = string.append_list(TypeStrs),
        Str = ModuleName ++ "__type_class_instance_" ++ ClassName
            ++ "_" ++ ArityStr ++ "_" ++ TypeVectorStr
    ;
        TCRttiName = type_class_instance_tc_type_vector(TCTypes),
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        TypeStrs = list.map(encode_tc_instance_type, TCTypes),
        TypeVectorStr = string.append_list(TypeStrs),
        Str = ModuleName ++ "__type_class_instance_tc_type_vector_"
            ++ ClassName ++ "_" ++ ArityStr ++ "_" ++ TypeVectorStr
    ;
        TCRttiName = type_class_instance_constraint(TCTypes, Ordinal, _),
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        TypeStrs = list.map(encode_tc_instance_type, TCTypes),
        TypeVectorStr = string.append_list(TypeStrs),
        string.int_to_string(Ordinal, OrdinalStr),
        Str = ModuleName ++ "__type_class_instance_constraint_" ++ ClassName
            ++ "_" ++ ArityStr ++ "_" ++ OrdinalStr ++ "_" ++ TypeVectorStr
    ;
        TCRttiName = type_class_instance_constraints(TCTypes),
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        TypeStrs = list.map(encode_tc_instance_type, TCTypes),
        TypeVectorStr = string.append_list(TypeStrs),
        Str = ModuleName ++ "__type_class_instance_constraints_"
            ++ ClassName ++ "_" ++ ArityStr ++ "_" ++ TypeVectorStr
    ;
        TCRttiName = type_class_instance_methods(TCTypes),
        mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr),
        TypeStrs = list.map(encode_tc_instance_type, TCTypes),
        TypeVectorStr = string.append_list(TypeStrs),
        Str = ModuleName ++ "__type_class_instance_methods_"
            ++ ClassName ++ "_" ++ ArityStr ++ "_" ++ TypeVectorStr
    ).

encode_tc_instance_type(TCType) = Str :-
    % The encoding we use here depends on the types in instance declarations
    % being type constructors applied to vectors of distinct variables. When
    % we lift that restriction, we will have to change this scheme.
    %
    % The code here is based on the code of
    % base_typeclass_info.type_to_string, but its input is of type
    % `maybe_pseudo_type_info', not of type `type'.
    (
        TCType = plain(TI),
        (
            TI = plain_arity_zero_type_info(RttiTypeCtor),
            ArgTIs = []
        ;
            TI = plain_type_info(RttiTypeCtor, ArgTIs)
        ;
            TI = var_arity_type_info(VarArityId, ArgTIs),
            RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId)
        ),
        Arity = list.length(ArgTIs)
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
            RttiTypeCtor = var_arity_id_to_rtti_type_ctor(VarArityId)
        ;
            PTI = type_var(_),
            unexpected($pred, "type_var")
        ),
        Arity = list.length(ArgPTIs)
        % XXX We may wish to check that all arguments are variables.
    ),
    RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, _CtorArity),
    TypeStr = sym_name_to_string_sep(qualified(ModuleName, TypeName), "__"),
    % XXX This naming scheme is the same as for base_typeclass_infos.
    % We should think about
    % - whether encoding guarantees different names for different instance
    %   declarations;
    % - whether the encoding is uniquely invertible, and
    % - whether the encoding may ever need to be uniquely invertible.
    Str = TypeStr ++ "__arity" ++ int_to_string(Arity) ++ "__".

:- pred mangle_rtti_type_ctor(rtti_type_ctor::in,
    string::out, string::out, string::out) is det.

mangle_rtti_type_ctor(RttiTypeCtor, ModuleName, TypeName, ArityStr) :-
    RttiTypeCtor = rtti_type_ctor(ModuleNameSym0, TypeName0, TypeArity),
    % This predicate will be invoked only at stages of compilation
    % that are after everything has been module qualified. The only
    % things with an empty module name should be the builtins.
    ( if ModuleNameSym0 = unqualified("") then
        ModuleNameSym = mercury_public_builtin_module
    else
        ModuleNameSym = ModuleNameSym0
    ),
    ModuleName = sym_name_mangle(ModuleNameSym),
    TypeName = name_mangle(TypeName0),
    ArityStr = string.uint16_to_string(TypeArity).

:- pred mangle_rtti_type_class_name(tc_name::in,
    string::out, string::out, string::out) is det.

mangle_rtti_type_class_name(TCName, ModuleName, ClassName, ArityStr) :-
    TCName = tc_name(ModuleNameSym, ClassName0, Arity),
    ModuleName = sym_name_mangle(ModuleNameSym),
    ClassName = name_mangle(ClassName0),
    string.int_to_string(Arity, ArityStr).

%-----------------------------------------------------------------------------%

type_ctor_rep_to_string(TypeCtorData, TargetPrefixes, RepStr) :-
    TargetPrefixes = target_prefixes("jmercury.runtime.TypeCtorRep.",
        "runtime.TypeCtorRep."),
    TypeCtorDetails = TypeCtorData ^ tcr_rep_details,
    (
        TypeCtorDetails = tcd_enum(TypeCtorUserEq, IsDummy, _, _, _, _, _),
        (
            IsDummy = enum_is_dummy,
            expect(unify(TypeCtorUserEq, standard), $pred,
                "dummy type with user equality"),
            RepStr = "MR_TYPECTOR_REP_DUMMY"
        ;
            IsDummy = enum_is_not_dummy,
            (
                TypeCtorUserEq = standard,
                RepStr = "MR_TYPECTOR_REP_ENUM"
            ;
                TypeCtorUserEq = user_defined,
                RepStr = "MR_TYPECTOR_REP_ENUM_USEREQ"
            )
        )
    ;
        TypeCtorDetails = tcd_foreign_enum(_, TypeCtorUserEq, _, _, _, _),
        (
            TypeCtorUserEq = standard,
            RepStr = "MR_TYPECTOR_REP_FOREIGN_ENUM"
        ;
            TypeCtorUserEq = user_defined,
            RepStr = "MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ"
        )
    ;
        TypeCtorDetails = tcd_du(TypeCtorUserEq, _, _, _, _, _),
        (
            TypeCtorUserEq = standard,
            RepStr = "MR_TYPECTOR_REP_DU"
        ;
            TypeCtorUserEq = user_defined,
            RepStr = "MR_TYPECTOR_REP_DU_USEREQ"
        )
    ;
        TypeCtorDetails = tcd_notag(TypeCtorUserEq, NotagFunctor, _),
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
        TypeCtorDetails = tcd_eqv(EqvType),
        (
            EqvType = pseudo(_),
            RepStr = "MR_TYPECTOR_REP_EQUIV"
        ;
            EqvType = plain(_),
            RepStr = "MR_TYPECTOR_REP_EQUIV_GROUND"
        )
    ;
        TypeCtorDetails = tcd_builtin(BuiltinCtor),
        builtin_ctor_rep_to_string(BuiltinCtor, RepStr)
    ;
        TypeCtorDetails = tcd_impl_artifact(ImplCtor),
        impl_ctor_rep_to_string(ImplCtor, RepStr)
    ;
        TypeCtorDetails = tcd_foreign(IsStable),
        ModuleName = TypeCtorData ^ tcr_module_name,
        TypeName = TypeCtorData ^ tcr_type_name,
        TypeArity = uint16.to_int(TypeCtorData ^ tcr_arity),
        TypeCtor = type_ctor(qualified(ModuleName, TypeName), TypeArity),
        ( if type_ctor_is_array(TypeCtor) then
            % XXX This is a kludge to allow accurate GC to trace arrays.
            % We should allow users to provide tracing functions for
            % foreign types.
            RepStr = "MR_TYPECTOR_REP_ARRAY"
        else if type_ctor_is_bitmap(TypeCtor) then
            % bitmaps are handled much like strings.
            RepStr = "MR_TYPECTOR_REP_BITMAP"
        else
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

builtin_ctor_rep_to_string(builtin_ctor_int, "MR_TYPECTOR_REP_INT").
builtin_ctor_rep_to_string(builtin_ctor_uint, "MR_TYPECTOR_REP_UINT").
builtin_ctor_rep_to_string(builtin_ctor_int8, "MR_TYPECTOR_REP_INT8").
builtin_ctor_rep_to_string(builtin_ctor_uint8, "MR_TYPECTOR_REP_UINT8").
builtin_ctor_rep_to_string(builtin_ctor_int16, "MR_TYPECTOR_REP_INT16").
builtin_ctor_rep_to_string(builtin_ctor_uint16, "MR_TYPECTOR_REP_UINT16").
builtin_ctor_rep_to_string(builtin_ctor_int32, "MR_TYPECTOR_REP_INT32").
builtin_ctor_rep_to_string(builtin_ctor_uint32, "MR_TYPECTOR_REP_UINT32").
builtin_ctor_rep_to_string(builtin_ctor_int64, "MR_TYPECTOR_REP_INT64").
builtin_ctor_rep_to_string(builtin_ctor_uint64, "MR_TYPECTOR_REP_UINT64").
builtin_ctor_rep_to_string(builtin_ctor_string, "MR_TYPECTOR_REP_STRING").
builtin_ctor_rep_to_string(builtin_ctor_float, "MR_TYPECTOR_REP_FLOAT").
builtin_ctor_rep_to_string(builtin_ctor_char, "MR_TYPECTOR_REP_CHAR").
builtin_ctor_rep_to_string(builtin_ctor_void, "MR_TYPECTOR_REP_VOID").
builtin_ctor_rep_to_string(builtin_ctor_c_pointer(is_not_stable),
    "MR_TYPECTOR_REP_C_POINTER").
builtin_ctor_rep_to_string(builtin_ctor_c_pointer(is_stable),
    "MR_TYPECTOR_REP_STABLE_C_POINTER").
builtin_ctor_rep_to_string(builtin_ctor_pred_ctor, "MR_TYPECTOR_REP_PRED").
builtin_ctor_rep_to_string(builtin_ctor_func_ctor, "MR_TYPECTOR_REP_FUNC").
builtin_ctor_rep_to_string(builtin_ctor_tuple, "MR_TYPECTOR_REP_TUPLE").
builtin_ctor_rep_to_string(builtin_ctor_ref, "MR_TYPECTOR_REP_REFERENCE").
builtin_ctor_rep_to_string(builtin_ctor_type_ctor_desc,
    "MR_TYPECTOR_REP_TYPECTORDESC").
builtin_ctor_rep_to_string(builtin_ctor_pseudo_type_desc,
    "MR_TYPECTOR_REP_PSEUDOTYPEDESC").
builtin_ctor_rep_to_string(builtin_ctor_type_desc, "MR_TYPECTOR_REP_TYPEDESC").

:- pred impl_ctor_rep_to_string(impl_ctor::in, string::out) is det.

impl_ctor_rep_to_string(impl_ctor_type_ctor_info,
    "MR_TYPECTOR_REP_TYPECTORINFO").
impl_ctor_rep_to_string(impl_ctor_type_info, "MR_TYPECTOR_REP_TYPEINFO").
impl_ctor_rep_to_string(impl_ctor_typeclass_info,
    "MR_TYPECTOR_REP_TYPECLASSINFO").
impl_ctor_rep_to_string(impl_ctor_base_typeclass_info,
    "MR_TYPECTOR_REP_BASETYPECLASSINFO").
impl_ctor_rep_to_string(impl_ctor_hp, "MR_TYPECTOR_REP_HP").
impl_ctor_rep_to_string(impl_ctor_succip, "MR_TYPECTOR_REP_SUCCIP").
impl_ctor_rep_to_string(impl_ctor_curfr, "MR_TYPECTOR_REP_CURFR").
impl_ctor_rep_to_string(impl_ctor_maxfr, "MR_TYPECTOR_REP_MAXFR").
impl_ctor_rep_to_string(impl_ctor_redofr, "MR_TYPECTOR_REP_REDOFR").
impl_ctor_rep_to_string(impl_ctor_redoip, "MR_TYPECTOR_REP_REDOIP").
impl_ctor_rep_to_string(impl_ctor_trail_ptr, "MR_TYPECTOR_REP_TRAIL_PTR").
impl_ctor_rep_to_string(impl_ctor_ticket, "MR_TYPECTOR_REP_TICKET").
impl_ctor_rep_to_string(impl_ctor_subgoal, "MR_TYPECTOR_REP_SUBGOAL").

%-----------------------------------------------------------------------------%

type_info_to_string(TypeInfo) = Str :-
    (
        TypeInfo = plain_arity_zero_type_info(RttiTypeCtor),
        RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        id_to_c_identifier(RttiId, Str)
    ;
        TypeInfo = plain_type_info(RttiTypeCtor, Args),
        mangle_rtti_type_ctor(RttiTypeCtor, ModuleName, TypeName, ArityStr),
        ArgsStr = type_info_list_to_string(Args),
        Str = ModuleName ++ "__ti_" ++ TypeName ++ "_" ++ ArityStr ++ ArgsStr
    ;
        TypeInfo = var_arity_type_info(VarArityId, Args),
        RealArity = list.length(Args),
        ArgsStr = type_info_list_to_string(Args),
        IdStr = var_arity_ctor_id_to_string(VarArityId),
        Str = "__vti_" ++ IdStr ++ "_" ++ int_to_string(RealArity) ++ ArgsStr
    ).

pseudo_type_info_to_string(PseudoTypeInfo) = Str :-
    (
        PseudoTypeInfo = plain_arity_zero_pseudo_type_info(RttiTypeCtor),
        RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        id_to_c_identifier(RttiId, Str)
    ;
        PseudoTypeInfo = plain_pseudo_type_info(RttiTypeCtor, Args),
        mangle_rtti_type_ctor(RttiTypeCtor, ModuleName, TypeName, ArityStr),
        ArgsStr = maybe_pseudo_type_info_list_to_string(Args),
        Str = ModuleName ++ "__pti_" ++ TypeName ++ "_" ++ ArityStr ++ ArgsStr
    ;
        PseudoTypeInfo = var_arity_pseudo_type_info(VarArityId, Args),
        RealArity = list.length(Args),
        ArgsStr = maybe_pseudo_type_info_list_to_string(Args),
        IdStr = var_arity_ctor_id_to_string(VarArityId),
        Str = "__vpti_" ++ IdStr ++ "_" ++ int_to_string(RealArity) ++ ArgsStr
    ;
        PseudoTypeInfo = type_var(VarNum),
        string.int_to_string(VarNum, Str)
    ).

:- func maybe_pseudo_type_info_to_string(rtti_maybe_pseudo_type_info) = string.

maybe_pseudo_type_info_to_string(plain(TypeInfo)) =
    "__plain_" ++ type_info_to_string(TypeInfo).
maybe_pseudo_type_info_to_string(pseudo(PseudoTypeInfo)) =
    "__pseudo_" ++ pseudo_type_info_to_string(PseudoTypeInfo).

:- func var_arity_ctor_id_to_string(var_arity_ctor_id) = string.

var_arity_ctor_id_to_string(pred_type_info) = "pred".
var_arity_ctor_id_to_string(func_type_info) = "func".
var_arity_ctor_id_to_string(tuple_type_info) = "tuple".

%-----------------------------------------------------------------------------%

:- func maybe_pseudo_type_info_list_to_string(
    list(rtti_maybe_pseudo_type_info)) = string.

maybe_pseudo_type_info_list_to_string(MaybePseudoTypeInfoList) =
    string.append_list(
        list.map(maybe_pseudo_type_info_to_string, MaybePseudoTypeInfoList)).

:- func type_info_list_to_string(list(rtti_type_info)) = string.

type_info_list_to_string(TypeInfoList) =
    string.append_list(list.map(type_info_to_string, TypeInfoList)).

%-----------------------------------------------------------------------------%

pred_or_func_to_string(PredOrFunc, TargetPrefixes, String) :-
    TargetPrefixes = target_prefixes("private_builtin.", "runtime.Constants."),
    (
        PredOrFunc = pf_predicate,
        String = "MR_PREDICATE"
    ;
        PredOrFunc = pf_function,
        String = "MR_FUNCTION"
    ).

sectag_locn_to_string(SecTag, TargetPrefixes, String) :-
    % The code of this predicate should produce output using the same scheme
    % as sectag_and_locn_to_locn_string.
    TargetPrefixes =
        target_prefixes("private_builtin.", "runtime.Sectag_Locn."),
    (
        SecTag = sectag_none,
        String = "MR_SECTAG_NONE"
    ;
        SecTag = sectag_none_direct_arg,
        String = "MR_SECTAG_NONE_DIRECT_ARG"
    ;
        SecTag = sectag_local_rest_of_word,
        String = "MR_SECTAG_LOCAL_REST_OF_WORD"
    ;
        SecTag = sectag_local_bits(_, _),
        String = "MR_SECTAG_LOCAL_BITS"
    ;
        SecTag = sectag_remote_word,
        String = "MR_SECTAG_REMOTE_FULL_WORD"
    ;
        SecTag = sectag_remote_bits(_, _),
        String = "MR_SECTAG_REMOTE_BITS"
    ).

sectag_and_locn_to_locn_string(SecTag, TargetPrefixes, String) :-
    % The code of this predicate should produce output using the same scheme
    % as sectag_locn_to_string.
    TargetPrefixes =
        target_prefixes("private_builtin.", "runtime.Sectag_Locn."),
    (
        SecTag = sectag_locn_none,
        String = "MR_SECTAG_NONE"
    ;
        SecTag = sectag_locn_none_direct_arg,
        String = "MR_SECTAG_NONE_DIRECT_ARG"
    ;
        SecTag = sectag_locn_local_rest_of_word(_),
        String = "MR_SECTAG_LOCAL_REST_OF_WORD"
    ;
        SecTag = sectag_locn_local_bits(_, _, _),
        String = "MR_SECTAG_LOCAL_BITS"
    ;
        SecTag = sectag_locn_remote_word(_),
        String = "MR_SECTAG_REMOTE_FULL_WORD"
    ;
        SecTag = sectag_locn_remote_bits(_, _, _),
        String = "MR_SECTAG_REMOTE_BITS"
    ).

functor_subtype_info_to_string(FunctorSubtypeInfo, TargetPrefixes, String) :-
    TargetPrefixes =
        target_prefixes("private_builtin.", "runtime.FunctorSubtypeInfo."),
    (
        FunctorSubtypeInfo = functor_subtype_none,
        String = "MR_FUNCTOR_SUBTYPE_NONE"
    ;
        FunctorSubtypeInfo = functor_subtype_exists,
        String = "MR_FUNCTOR_SUBTYPE_EXISTS"
    ).

type_info_to_rtti_data(TypeInfo) = rtti_data_type_info(TypeInfo).

maybe_pseudo_type_info_to_rtti_data(pseudo(PseudoTypeInfo)) =
    rtti_data_pseudo_type_info(PseudoTypeInfo).
maybe_pseudo_type_info_to_rtti_data(plain(TypeInfo)) =
    rtti_data_type_info(TypeInfo).

maybe_pseudo_type_info_or_self_to_rtti_data(pseudo(PseudoTypeInfo)) =
    rtti_data_pseudo_type_info(PseudoTypeInfo).
maybe_pseudo_type_info_or_self_to_rtti_data(plain(TypeInfo)) =
    rtti_data_type_info(TypeInfo).
maybe_pseudo_type_info_or_self_to_rtti_data(self) =
    rtti_data_pseudo_type_info(type_var(0)).

type_ctor_details_num_ptags(TypeCtorDetails) = MaybeNumPtags :-
    (
        ( TypeCtorDetails = tcd_enum(_, _, _, _, _, _, _)
        ; TypeCtorDetails = tcd_foreign_enum(_, _, _, _, _, _)
        ; TypeCtorDetails = tcd_notag(_, _, _)
        ; TypeCtorDetails = tcd_eqv(_)
        ; TypeCtorDetails = tcd_builtin(_)
        ; TypeCtorDetails = tcd_impl_artifact(_)
        ; TypeCtorDetails = tcd_foreign(_)
        ),
        MaybeNumPtags = no
    ;
        TypeCtorDetails = tcd_du(_, _, PtagMap, _, _, _),
        map.keys(PtagMap, Ptags),
        list.det_last(Ptags, LastPtag),
        LastPtag = ptag(LastPtagUint8),
        NumPtags = uint8.to_int(LastPtagUint8) + 1,
        MaybeNumPtags = yes(NumPtags)
    ).

type_ctor_details_num_functors(TypeCtorDetails) = MaybeNumFunctors :-
    (
        (
            TypeCtorDetails = tcd_enum(_, _, EnumFunctors, _, _, _, _),
            list.length(EnumFunctors, NumFunctors)
        ;
            TypeCtorDetails = tcd_foreign_enum(_, _, ForeignFunctors, _, _, _),
            list.length(ForeignFunctors, NumFunctors)
        ;
            TypeCtorDetails = tcd_du(_, DuFunctors, _, _, _, _),
            list.length(DuFunctors, NumFunctors)
        ;
            TypeCtorDetails = tcd_notag(_, _, _),
            NumFunctors = 1
        ),
        MaybeNumFunctors = yes(NumFunctors)
    ;
        ( TypeCtorDetails = tcd_eqv(_)
        ; TypeCtorDetails = tcd_builtin(_)
        ; TypeCtorDetails = tcd_impl_artifact(_)
        ; TypeCtorDetails = tcd_foreign(_)
        ),
        MaybeNumFunctors = no
    ).

du_arg_info_name(ArgInfo) = ArgInfo ^ du_arg_name.

du_arg_info_type(ArgInfo) = ArgInfo ^ du_arg_type.

du_arg_info_pos_width(ArgInfo) = ArgInfo ^ du_arg_pos_width.

enum_functor_rtti_name(EnumFunctor) =
    type_ctor_enum_functor_desc(EnumFunctor ^ enum_ordinal).

foreign_enum_functor_rtti_name(EnumFunctor) =
    type_ctor_foreign_enum_functor_desc(EnumFunctor ^ foreign_enum_ordinal).

du_functor_rtti_name(DuFunctor) =
    type_ctor_du_functor_desc(DuFunctor ^ du_ordinal).

rtti_id_would_include_code_addr(ctor_rtti_id(_, RttiName)) =
    ctor_rtti_name_would_include_code_addr(RttiName).
rtti_id_would_include_code_addr(tc_rtti_id(_, TCRttiName)) =
    tc_rtti_name_would_include_code_addr(TCRttiName).

ctor_rtti_name_would_include_code_addr(RttiName) = InclCodeAddr :-
    (
        ( RttiName = type_ctor_exist_locns(_)
        ; RttiName = type_ctor_exist_locn
        ; RttiName = type_ctor_exist_tc_constr(_, _, _)
        ; RttiName = type_ctor_exist_tc_constrs(_)
        ; RttiName = type_ctor_exist_info(_)
        ; RttiName = type_ctor_field_names(_)
        ; RttiName = type_ctor_field_types(_)
        ; RttiName = type_ctor_field_locns(_)
        ; RttiName = type_ctor_enum_functor_desc(_)
        ; RttiName = type_ctor_foreign_enum_functor_desc(_)
        ; RttiName = type_ctor_notag_functor_desc
        ; RttiName = type_ctor_du_functor_desc(_)
        ; RttiName = type_ctor_enum_name_ordered_table
        ; RttiName = type_ctor_enum_ordinal_ordered_table
        ; RttiName = type_ctor_foreign_enum_name_ordered_table
        ; RttiName = type_ctor_foreign_enum_ordinal_ordered_table
        ; RttiName = type_ctor_du_name_ordered_table
        ; RttiName = type_ctor_du_stag_ordered_table(_)
        ; RttiName = type_ctor_du_ptag_ordered_table
        ; RttiName = type_ctor_du_ptag_layout(_)
        ; RttiName = type_ctor_functor_number_map
        ; RttiName = type_ctor_type_hashcons_pointer
        ; RttiName = type_ctor_type_functors
        ; RttiName = type_ctor_type_layout
        ),
        InclCodeAddr = no
    ;
        RttiName = type_ctor_type_ctor_info,
        InclCodeAddr = yes
    ;
        RttiName = type_ctor_type_info(TypeInfo),
        InclCodeAddr = type_info_would_incl_code_addr(TypeInfo)
    ;
        RttiName = type_ctor_pseudo_type_info(PseudoTypeInfo),
        InclCodeAddr = pseudo_type_info_would_incl_code_addr(PseudoTypeInfo)
    ).

tc_rtti_name_would_include_code_addr(TCName) = InclCodeAddr :-
    (
        TCName = type_class_base_typeclass_info(_, _),
        InclCodeAddr = yes
    ;
        ( TCName = type_class_id
        ; TCName = type_class_id_var_names
        ; TCName = type_class_id_method_ids
        ; TCName = type_class_decl
        ; TCName = type_class_decl_super(_, _)
        ; TCName = type_class_decl_supers
        ; TCName = type_class_instance(_)
        ; TCName = type_class_instance_tc_type_vector(_)
        ; TCName = type_class_instance_constraint(_, _, _)
        ; TCName = type_class_instance_constraints(_)
        ; TCName = type_class_instance_methods(_)
        ),
        InclCodeAddr = no
    ).

type_info_would_incl_code_addr(TypeInfo) = InclCodeAddr :-
    (
        TypeInfo = plain_arity_zero_type_info(_),
        InclCodeAddr = yes
    ;
        ( TypeInfo = plain_type_info(_, _)
        ; TypeInfo = var_arity_type_info(_, _)
        ),
        InclCodeAddr = no
    ).

pseudo_type_info_would_incl_code_addr(PseudoTypeInfo) = InclCodeAddr :-
    (
        PseudoTypeInfo = plain_arity_zero_pseudo_type_info(_),
        InclCodeAddr = yes
    ;
        ( PseudoTypeInfo = plain_pseudo_type_info(_, _)
        ; PseudoTypeInfo = var_arity_pseudo_type_info(_, _)
        ; PseudoTypeInfo = type_var(_)
        ),
        InclCodeAddr = no
    ).

rtti_id_maybe_element_c_type(item_type(RttiId), CTypeName, IsArray) :-
    rtti_id_c_type(RttiId, CTypeName, IsArray).
rtti_id_maybe_element_c_type(element_type(RttiId), CTypeName, IsArray) :-
    rtti_id_c_type(RttiId, CTypeName, IsArray0),
    (
        IsArray0 = not_array,
        unexpected($pred, "base is not array")
    ;
        IsArray0 = is_array,
        IsArray = not_array
    ).

rtti_id_c_type(ctor_rtti_id(_, RttiName), CTypeName, IsArray) :-
    ctor_rtti_name_c_type(RttiName, CTypeName, IsArray).
rtti_id_c_type(tc_rtti_id(_, TCRttiName), CTypeName, IsArray) :-
    tc_rtti_name_c_type(TCRttiName, CTypeName, IsArray).

ctor_rtti_name_c_type(RttiName, CTypeName, IsArray) :-
    ctor_rtti_name_type(RttiName, GenTypeName, IsArray),
    CTypeName = "MR_" ++ GenTypeName.

tc_rtti_name_c_type(TCRttiName, CTypeName, IsArray) :-
    tc_rtti_name_type(TCRttiName, GenTypeName, IsArray),
    CTypeName = string.append("MR_", GenTypeName).

rtti_id_maybe_element_java_type(item_type(RttiId), CTypeName, IsArray) :-
    rtti_id_java_type(RttiId, CTypeName, IsArray).
rtti_id_maybe_element_java_type(element_type(RttiId), CTypeName, IsArray) :-
    rtti_id_java_type(RttiId, CTypeName, IsArray0),
    (
        IsArray0 = not_array,
        unexpected($pred, "base is not array")
    ;
        IsArray0 = is_array,
        IsArray = not_array
    ).

rtti_id_java_type(ctor_rtti_id(_, RttiName), JavaTypeName, IsArray) :-
    ctor_rtti_name_java_type(RttiName, JavaTypeName, IsArray).
rtti_id_java_type(tc_rtti_id(_, TCRttiName), JavaTypeName, IsArray) :-
    tc_rtti_name_java_type(TCRttiName, JavaTypeName, IsArray).

ctor_rtti_name_java_type(RttiName, JavaTypeName, IsArray) :-
    % Changes here may need similar changes in tc_rtti_name_java_type.
    ctor_rtti_name_type(RttiName, GenTypeName0, IsArray),
    ( if
        % Java doesn't have typedefs (or "const"), so we need to use "String"
        % rather than "ConstString".
        GenTypeName0 = "ConstString"
    then
        JavaTypeName = "java.lang.String"
    else if
        GenTypeName0 = "Integer"
    then
        JavaTypeName = "int"
    else if
        % In Java, every non-builtin type is a pointer, so there is no need
        % for the "Ptr" suffixes.
        string.remove_suffix(GenTypeName0, "Ptr", GenTypeName1)
    then
        JavaTypeName = "jmercury.runtime." ++ GenTypeName1
    else if
        % In C, we do some nasty hacks to represent type class constraints
        % of different arities as different structures ending with arrays
        % of the appropriate length, but in Java we just use a single type
        % for all of them (with an extra level of indirection for the array).
        string.prefix(GenTypeName0, "TypeClassConstraint_")
    then
        JavaTypeName = "jmercury.runtime.TypeClassConstraint"
    else if
        % In C, we do some nasty hacks to represent type infos
        % different arities as different structures
        % ending with arrays of the appropriate length, but in
        % Java we just use a single type for all of them
        % (with an extra level of indirection for the array).
        ( string.prefix(GenTypeName0, "FA_PseudoTypeInfo_Struct")
        ; string.prefix(GenTypeName0, "FA_TypeInfo_Struct")
        ; string.prefix(GenTypeName0, "VA_PseudoTypeInfo_Struct")
        ; string.prefix(GenTypeName0, "VA_TypeInfo_Struct")
        )
    then
        JavaTypeName = "jmercury.runtime.TypeInfo_Struct"
    else
        JavaTypeName = "jmercury.runtime." ++ GenTypeName0
    ).

tc_rtti_name_java_type(TCRttiName, JavaTypeName, IsArray) :-
    % Changes here may need similar changes in ctor_rtti_name_java_type.
    tc_rtti_name_type(TCRttiName, GenTypeName, IsArray),
    ( if
        % BaseTypeClassInfo in C is represented using a variable-length array
        % as the last field, so we need to handle it specially in Java.
        GenTypeName = "BaseTypeclassInfo"
    then
        JavaTypeName = "java.lang.Object"   % & IsArray = yes
    else if
        % Java doesn't have typedefs (or "const"), so we need to use "String"
        % rather than "ConstString".
        GenTypeName = "ConstString"
    then
        JavaTypeName = "java.lang.String"
    else if
        % In C, we do some nasty hacks to represent type class constraints
        % of different arities as different structures ending with arrays
        % of the appropriate length, but in Java we just use a single type
        % for all of them (with an extra level of indirection for the array).
        string.prefix(GenTypeName, "TypeClassConstraint_")
    then
        JavaTypeName = "jmercury.runtime.TypeClassConstraint"
    else
        % The rest are all defined in Mercury's Java runtime
        % (java/runtime/*.java).
        JavaTypeName = "jmercury.runtime." ++ GenTypeName
    ).

rtti_id_maybe_element_csharp_type(item_type(RttiId), CTypeName, IsArray) :-
    rtti_id_csharp_type(RttiId, CTypeName, IsArray).
rtti_id_maybe_element_csharp_type(element_type(RttiId), CTypeName, IsArray) :-
    rtti_id_csharp_type(RttiId, CTypeName, IsArray0),
    (
        IsArray0 = not_array,
        unexpected($pred, "base is not array")
    ;
        IsArray0 = is_array,
        IsArray = not_array
    ).

rtti_id_csharp_type(ctor_rtti_id(_, RttiName), CsharpTypeName, IsArray) :-
    ctor_rtti_name_csharp_type(RttiName, CsharpTypeName, IsArray).
rtti_id_csharp_type(tc_rtti_id(_, TCRttiName), CsharpTypeName, IsArray) :-
    tc_rtti_name_csharp_type(TCRttiName, CsharpTypeName, IsArray).

ctor_rtti_name_csharp_type(RttiName, CsharpTypeName, IsArray) :-
    ctor_rtti_name_type(RttiName, GenTypeName0, IsArray),
    ( if GenTypeName0 = "ConstString" then
        CsharpTypeName = "string"
    else if GenTypeName0 = "Integer" then
        CsharpTypeName = "int"
    else if string.remove_suffix(GenTypeName0, "Ptr", GenTypeName1) then
        CsharpTypeName = "runtime." ++ GenTypeName1
    else if string.prefix(GenTypeName0, "TypeClassConstraint_") then
        CsharpTypeName = "runtime.TypeClassConstraint"
    else if
        ( string.prefix(GenTypeName0, "FA_PseudoTypeInfo_Struct")
        ; string.prefix(GenTypeName0, "FA_TypeInfo_Struct")
        ; string.prefix(GenTypeName0, "VA_PseudoTypeInfo_Struct")
        ; string.prefix(GenTypeName0, "VA_TypeInfo_Struct")
        )
    then
        CsharpTypeName = "runtime.TypeInfo_Struct"
    else
        CsharpTypeName = "runtime." ++ GenTypeName0
    ).

tc_rtti_name_csharp_type(TCRttiName, CsharpTypeName, IsArray) :-
    tc_rtti_name_type(TCRttiName, GenTypeName, IsArray),
    ( if GenTypeName = "BaseTypeclassInfo" then
        CsharpTypeName = "object"           % & IsArray = yes
    else if GenTypeName = "ConstString" then
        CsharpTypeName = "string"
    else if string.prefix(GenTypeName, "TypeClassConstraint_") then
        CsharpTypeName = "runtime.TypeClassConstraint"
    else
        CsharpTypeName = "runtime." ++ GenTypeName
    ).

    % ctor_rtti_name_type(RttiName, Type, IsArray)
    %
:- pred ctor_rtti_name_type(ctor_rtti_name::in, string::out, is_array::out)
    is det.

ctor_rtti_name_type(type_ctor_exist_locns(_),
        "DuExistLocn", is_array).
ctor_rtti_name_type(type_ctor_exist_locn,
        "DuExistLocn", not_array).
ctor_rtti_name_type(type_ctor_exist_tc_constr(_, _, N),
        tc_constraint_type_name(N), not_array).
ctor_rtti_name_type(type_ctor_exist_tc_constrs(_),
        "TypeClassConstraint", is_array).
ctor_rtti_name_type(type_ctor_exist_info(_),
        "DuExistInfo", not_array).
ctor_rtti_name_type(type_ctor_field_names(_),
        "ConstString", is_array).
ctor_rtti_name_type(type_ctor_field_types(_),
        "PseudoTypeInfo", is_array).
ctor_rtti_name_type(type_ctor_field_locns(_),
        "DuArgLocn", is_array).
ctor_rtti_name_type(type_ctor_enum_functor_desc(_),
        "EnumFunctorDesc", not_array).
ctor_rtti_name_type(type_ctor_foreign_enum_functor_desc(_),
        "ForeignEnumFunctorDesc", not_array).
ctor_rtti_name_type(type_ctor_notag_functor_desc,
        "NotagFunctorDesc", not_array).
ctor_rtti_name_type(type_ctor_du_functor_desc(_),
        "DuFunctorDesc", not_array).
ctor_rtti_name_type(type_ctor_enum_name_ordered_table,
        "EnumFunctorDescPtr", is_array).
ctor_rtti_name_type(type_ctor_enum_ordinal_ordered_table,
        "EnumFunctorDescPtr", is_array).
ctor_rtti_name_type(type_ctor_foreign_enum_name_ordered_table,
        "ForeignEnumFunctorDescPtr", is_array).
ctor_rtti_name_type(type_ctor_foreign_enum_ordinal_ordered_table,
        "ForeignEnumFunctorDescPtr", is_array).
ctor_rtti_name_type(type_ctor_du_name_ordered_table,
        "DuFunctorDescPtr", is_array).
ctor_rtti_name_type(type_ctor_du_stag_ordered_table(_),
        "DuFunctorDescPtr", is_array).
ctor_rtti_name_type(type_ctor_du_ptag_ordered_table,
        "DuPtagLayout", is_array).
ctor_rtti_name_type(type_ctor_du_ptag_layout(_),
        "DuPtagLayout", not_array).
ctor_rtti_name_type(type_ctor_functor_number_map,
        "Integer", is_array).
ctor_rtti_name_type(type_ctor_type_functors,
        "TypeFunctors", not_array).
ctor_rtti_name_type(type_ctor_type_layout,
        "TypeLayout", not_array).
ctor_rtti_name_type(type_ctor_type_ctor_info,
        "TypeCtorInfo_Struct", not_array).
ctor_rtti_name_type(type_ctor_type_hashcons_pointer,
        "TrieNodePtr", not_array).
ctor_rtti_name_type(type_ctor_type_info(TypeInfo),
        type_info_name_type(TypeInfo), not_array).
ctor_rtti_name_type(type_ctor_pseudo_type_info(PseudoTypeInfo),
        pseudo_type_info_name_type(PseudoTypeInfo), not_array).

    % tc_rtti_name_type(RttiName, Type, IsArray)
    %
:- pred tc_rtti_name_type(tc_rtti_name::in, string::out, is_array::out) is det.

tc_rtti_name_type(type_class_base_typeclass_info(_, _),
        "BaseTypeclassInfo", is_array).
tc_rtti_name_type(type_class_id,
        "TypeClassId", not_array).
tc_rtti_name_type(type_class_id_var_names,
        "ConstString", is_array).
tc_rtti_name_type(type_class_id_method_ids,
        "TypeClassMethod", is_array).
tc_rtti_name_type(type_class_decl,
        "TypeClassDeclStruct", not_array).
tc_rtti_name_type(type_class_decl_super(_, N), TypeName, not_array) :-
    TypeName = tc_constraint_type_name(N).
tc_rtti_name_type(type_class_decl_supers,
        "TypeClassConstraint", is_array).
tc_rtti_name_type(type_class_instance(_),
        "InstanceStruct", not_array).
tc_rtti_name_type(type_class_instance_tc_type_vector(_),
        "PseudoTypeInfo", is_array).
tc_rtti_name_type(type_class_instance_constraint(_, _, N),
        TypeName, not_array) :-
    TypeName = tc_constraint_type_name(N).
tc_rtti_name_type(type_class_instance_constraints(_),
        "TypeClassConstraint", is_array).
tc_rtti_name_type(type_class_instance_methods(_), "CodePtr", is_array).

:- func tc_constraint_type_name(int) = string.

tc_constraint_type_name(N) =
    "TypeClassConstraint_" ++ int_to_string(N) ++ "Struct".

:- func type_info_name_type(rtti_type_info) = string.

type_info_name_type(plain_arity_zero_type_info(_)) =
    "TypeCtorInfo_Struct".
type_info_name_type(plain_type_info(_, ArgTypes)) =
    string.format("FA_TypeInfo_Struct%d", [i(list.length(ArgTypes))]).
type_info_name_type(var_arity_type_info(_, ArgTypes)) =
    string.format("VA_TypeInfo_Struct%d", [i(list.length(ArgTypes))]).

:- func pseudo_type_info_name_type(rtti_pseudo_type_info) = string.

pseudo_type_info_name_type(plain_arity_zero_pseudo_type_info(_)) =
    "TypeCtorInfo_Struct".
pseudo_type_info_name_type(plain_pseudo_type_info(_TypeCtor, ArgTypes)) =
    string.format("FA_PseudoTypeInfo_Struct%d",
        [i(list.length(ArgTypes))]).
pseudo_type_info_name_type(var_arity_pseudo_type_info(_TypeCtor, ArgTypes)) =
    string.format("VA_PseudoTypeInfo_Struct%d",
        [i(list.length(ArgTypes))]).
pseudo_type_info_name_type(type_var(_)) = _ :-
    % we use small integers to represent type_vars,
    % rather than pointers, so there is no pointed-to type
    unexpected($pred, "type_var").

module_qualify_name_of_rtti_id(RttiId) = ShouldModuleQualify :-
    (
        RttiId = ctor_rtti_id(_, CtorRttiName),
        ShouldModuleQualify =
            module_qualify_name_of_ctor_rtti_name(CtorRttiName)
    ;
        RttiId = tc_rtti_id(_, TCRttiName),
        ShouldModuleQualify =
            module_qualify_name_of_tc_rtti_name(TCRttiName)
    ).

module_qualify_name_of_ctor_rtti_name(_) = yes.

% We don't want to include the module name as part of the name for
% base_typeclass_infos, since we _want_ to cause a link error for
% overlapping instance decls, even if they are in a different modules.
%
% When we start generating data structures replacing base_typeclass_infos,
% we should include their names here.
%
% This decision is implemented separately in tc_name_to_string.

module_qualify_name_of_tc_rtti_name(TCRttiName) = ModuleQualify :-
    (
        TCRttiName = type_class_base_typeclass_info(_, _),
        ModuleQualify = no
    ;
        ( TCRttiName = type_class_id
        ; TCRttiName = type_class_id_var_names
        ; TCRttiName = type_class_id_method_ids
        ; TCRttiName = type_class_decl
        ; TCRttiName = type_class_decl_super(_, _)
        ; TCRttiName = type_class_decl_supers
        ; TCRttiName = type_class_instance(_)
        ; TCRttiName = type_class_instance_tc_type_vector(_)
        ; TCRttiName = type_class_instance_constraint(_, _, _)
        ; TCRttiName = type_class_instance_constraints(_)
        ; TCRttiName = type_class_instance_methods(_)
        ),
        ModuleQualify = yes
    ).

rtti_id_emits_type_ctor_info(RttiId, TypeCtor) :-
    RttiId = ctor_rtti_id(RttiTypeCtor, RttiName),
    (
        RttiName = type_ctor_type_ctor_info,
        TypeCtor = RttiTypeCtor
    ;
        RttiName = type_ctor_type_info(TypeInfo),
        TypeInfo = plain_arity_zero_type_info(TypeCtor)
    ;
        RttiName = type_ctor_pseudo_type_info(PseudoTypeInfo),
        PseudoTypeInfo = plain_arity_zero_pseudo_type_info(TypeCtor)
    ).

%-----------------------------------------------------------------------------%

tabling_info_id_str(tabling_info) = "table_info".
tabling_info_id_str(tabling_ptis) = "table_ptis".
tabling_info_id_str(tabling_type_param_locns) = "tabling_type_param_locns".
tabling_info_id_str(tabling_root_node) = "table_root_node".
tabling_info_id_str(tabling_steps_desc(call_table)) = "table_input_steps".
tabling_info_id_str(tabling_steps_desc(answer_table)) = "table_output_steps".
tabling_info_id_str(tabling_stats(call_table, curr_table)) =
    "table_call_stats".
tabling_info_id_str(tabling_stats(call_table, prev_table)) =
    "table_prev_call_stats".
tabling_info_id_str(tabling_stats(answer_table, curr_table)) =
    "table_answer_stats".
tabling_info_id_str(tabling_stats(answer_table, prev_table)) =
    "table_prev_answer_stats".
tabling_info_id_str(tabling_stat_steps(call_table, curr_table)) =
    "table_call_step_stats".
tabling_info_id_str(tabling_stat_steps(call_table, prev_table)) =
    "table_prev_call_step_stats".
tabling_info_id_str(tabling_stat_steps(answer_table, curr_table)) =
    "table_answer_step_stats".
tabling_info_id_str(tabling_stat_steps(answer_table, prev_table)) =
    "table_prev_answer_step_stats".
tabling_info_id_str(tabling_tips) = "table_tips".

tabling_id_c_type(Id, JavaTypeName, IsArray) :-
    % Since tabling is not yet implemented for Java, this is only provisional.
    tabling_id_base_type(Id, CTypeName, IsArray),
    JavaTypeName = "MR_" ++ CTypeName.

tabling_id_java_type(Id, JavaTypeName, IsArray) :-
    % Since tabling is not yet implemented for Java, this is only provisional.
    tabling_id_base_type(Id, CTypeName, IsArray),
    JavaTypeName = "jmercury.runtime." ++ CTypeName.

:- pred tabling_id_base_type(proc_tabling_struct_id::in, string::out,
    is_array::out) is det.

% These should be without the MR_ prefix.
tabling_id_base_type(tabling_info, "ProcTableInfo", not_array).
tabling_id_base_type(tabling_ptis, "PseudoTypeInfo", is_array).
tabling_id_base_type(tabling_type_param_locns, "TypeParamLocns", is_array).
tabling_id_base_type(tabling_root_node, "TableNode", not_array).
tabling_id_base_type(tabling_steps_desc(_), "TableStepDesc", is_array).
tabling_id_base_type(tabling_stats(_, _), "TableStats", not_array).
tabling_id_base_type(tabling_stat_steps(_, _), "TableStepStats", is_array).
tabling_id_base_type(tabling_tips, "TrieNode", is_array).

tabling_id_has_array_type(Id) = IsArray :-
    tabling_id_base_type(Id, _, IsArray).

table_trie_step_to_c(table_trie_step_dummy, "MR_TABLE_STEP_DUMMY", no).
table_trie_step_to_c(table_trie_step_int(int_type_int),
    "MR_TABLE_STEP_INT", no).
table_trie_step_to_c(table_trie_step_int(int_type_uint),
    "MR_TABLE_STEP_UINT", no).
table_trie_step_to_c(table_trie_step_int(int_type_int8),
    "MR_TABLE_STEP_INT8", no).
table_trie_step_to_c(table_trie_step_int(int_type_uint8),
    "MR_TABLE_STEP_UINT8", no).
table_trie_step_to_c(table_trie_step_int(int_type_int16),
    "MR_TABLE_STEP_INT16", no).
table_trie_step_to_c(table_trie_step_int(int_type_uint16),
    "MR_TABLE_STEP_UINT16", no).
table_trie_step_to_c(table_trie_step_int(int_type_int32),
    "MR_TABLE_STEP_INT32", no).
table_trie_step_to_c(table_trie_step_int(int_type_uint32),
    "MR_TABLE_STEP_UINT32", no).
table_trie_step_to_c(table_trie_step_int(int_type_int64),
    "MR_TABLE_STEP_INT64", no).
table_trie_step_to_c(table_trie_step_int(int_type_uint64),
    "MR_TABLE_STEP_UINT64", no).
table_trie_step_to_c(table_trie_step_char, "MR_TABLE_STEP_CHAR", no).
table_trie_step_to_c(table_trie_step_string, "MR_TABLE_STEP_STRING", no).
table_trie_step_to_c(table_trie_step_float, "MR_TABLE_STEP_FLOAT", no).
table_trie_step_to_c(table_trie_step_enum(EnumRange), "MR_TABLE_STEP_ENUM",
    yes(EnumRange)).
table_trie_step_to_c(table_trie_step_foreign_enum,
    "MR_TABLE_STEP_FOREIGN_ENUM", no).
table_trie_step_to_c(table_trie_step_general(_, table_is_mono, table_value),
    "MR_TABLE_STEP_GEN", no).
table_trie_step_to_c(table_trie_step_general(_, table_is_poly, table_value),
    "MR_TABLE_STEP_GEN_POLY", no).
table_trie_step_to_c(table_trie_step_general(_, table_is_mono, table_addr),
    "MR_TABLE_STEP_GEN_ADDR", no).
table_trie_step_to_c(table_trie_step_general(_, table_is_poly, table_addr),
    "MR_TABLE_STEP_GEN_POLY_ADDR", no).
table_trie_step_to_c(table_trie_step_typeinfo, "MR_TABLE_STEP_TYPEINFO", no).
table_trie_step_to_c(table_trie_step_typeclassinfo,
    "MR_TABLE_STEP_TYPECLASSINFO", no).
table_trie_step_to_c(table_trie_step_promise_implied,
    "MR_TABLE_STEP_PROMISE_IMPLIED", no).

%-----------------------------------------------------------------------------%
:- end_module backend_libs.rtti.
%-----------------------------------------------------------------------------%
