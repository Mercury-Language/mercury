%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_item_type.m.
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_item_type.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item_inst_mode.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.

%---------------------------------------------------------------------------%

:- type item_type_defn_info == item_type_defn_info_general(type_defn).

:- type item_type_defn_info_abstract
    == item_type_defn_info_general(type_details_abstract).
:- type item_type_defn_info_solver
    == item_type_defn_info_general(type_details_solver).
:- type item_type_defn_info_eqv
    == item_type_defn_info_general(type_details_eqv).
:- type item_type_defn_info_du
    == item_type_defn_info_general(type_details_du).
:- type item_type_defn_info_sub
    == item_type_defn_info_general(type_details_sub).
:- type item_type_defn_info_foreign
    == item_type_defn_info_general(type_details_foreign_generic).

:- type item_type_defn_info_general(T)
    --->    item_type_defn_info(
                % `:- type ...':
                % a definition of a type, or a declaration of an abstract type.
                td_ctor_name                    :: sym_name,
                td_ctor_args                    :: list(type_param),
                td_ctor_defn                    :: T,
                td_tvarset                      :: tvarset,
                td_context                      :: prog_context,
                td_seq_num                      :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_foreign_enum_info
    --->    item_foreign_enum_info(
                fe_language                     :: foreign_language,
                fe_type_ctor                    :: type_ctor,
                fe_values                       :: one_or_more(
                                                    pair(sym_name, string)),
                fe_context                      :: prog_context,
                fe_seq_num                      :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_type_repn_info_eqv
    == item_type_repn_info_general(mer_type).
:- type item_type_repn_info_subtype
    == item_type_repn_info_general(type_ctor).
:- type item_type_repn_info
    == item_type_repn_info_general(type_ctor_repn_info).

:- type item_type_repn_info_general(T)
    --->    item_type_repn_info(
                % `:- type_representation ...':
                % An item added by the compiler to a .int3 file
                % to tell readers of that file the information they need
                % to correctly reconstruct the representation of the given
                % type constructor, even when that information is supposed
                % to be invisible to them semantically.
                % There should be at most one such item for any type_ctor
                % in the .int3 file of its defining module.
                % The sym_name should be fully qualified.
                tr_ctor                         :: sym_name,
                tr_ctor_arg_tvars               :: list(tvar),
                tr_ctor_repn_info               :: T,
                tr_tvarset                      :: tvarset,
                tr_context                      :: prog_context,
                tr_seq_num                      :: item_seq_num
            ).

%---------------------------------------------------------------------------%
%
% The intended semantics of a type_ctor_defn_map is a map of
% all the type constructors defined in a given SECTION of a given
% interface file to all its definitions in that section.
%
% There are four intended uses of a type_ctor_defn_map. The most
% important is the fourth one.
%
% One is to eliminate unnecessary items from interface files.
% For example, library/set.m contains two definitions of the set
% type constructor: an abstract definition in the publicly documented
% interface section, and an actual du definition in another interface
% section that we do not include in the automatically generated
% documentation but we *do* export to other modules. In situations
% like this, the abstract definition is redundant. Never including it
% in an interface file lets that interface file to remain unchanged
% in the event that the user deletes the abstract definition from
% the source file as well.
%
% The second use is to canonicalize the parts of interface files
% containing type definitions.
%
% The third use is to help deal with sets of definitions that
% don't make sense. There are many rules that a set of definitions
% for a given type constructor must meet (such as "there may be at most one
% definition for a type constructor that is a du, equivalence or solver
% definition), and bugs may manifest themselves as violations of these rules.
%
% We have a choice in when these violations are detected.
%
% - If we allow the inclusion of inconsistent sets of type definitions
%   in interface files, then we must detect and handle these
%   inconsistencies every time a compiler invocation reads that interface
%   file. These invocations won't generate error messages for these
%   inconsistencies since the type constructor won't be local, but
%   they may generate messages for other "errors" that look like errors
%   only because the compiler's resolution of the inconsistency (i.e.
%   its choice of which type definitions to keep and which to throw out)
%   differs from the programmer's choice.
%
% - If we do NOT allow the inclusion of inconsistent sets of type
%   definitions in interface files, then we must report any violations
%   at interface file construction time, and make them cause that
%   construction to fail. Printing such error messages to stdout
%   instead of the module's .err file is less than ideal, but
%   this early detection can avoid avalanches of misleading diagnostics
%   of the kind mentioned in the previous point. It can also save
%   recompilations. If a module's source file contains inconsistent
%   definitions for a type constructor, then the programmer will
%   have to delete the unintended ones. Once this is done, the
%   interface file will have to be rebuilt. If we allow inconsistent
%   definitions in the interface file, its new contents will differ
%   from its old contents, which means that all the compilations
%   of *other* modules that read the old contents will have been wasted.
%   If we cause the construction of the interface file to fail instead,
%   those compilations won't have taken place.
%
% We implement the first choice by checking whether each entry in
% a type_ctor_defn_map makes sense, and generating error messages
% when they don't. This is done by code in check_type_inst_mode_defns.m.
%
% The fourth and most motivating use is that having all the definitions
% of a type_ctor, *and* all the foreign_enum pragmas that apply to that
% type_ctor, all together at once will make the code that decides
% the proper representation of that type significantly simpler.
%
% Everything above except the fourth use also applies to the inst_
% and mode_ctor_defn_maps, though for those, the consistency rules are
% much simpler: that each inst and mode constructor must have at most one
% non-abstract definition.
%

:- type type_ctor_defn_map == map(type_ctor, type_ctor_all_defns).

:- type type_ctor_all_defns
    --->    type_ctor_all_defns(
                % Abstract and nonabstract solver type definitions.
                tcad_abstract_solver    :: list(item_type_defn_info_abstract),
                tcad_solver             :: list(item_type_defn_info_solver),

                % Abstract and nonabstract nonsolver type definitions.
                tcad_abstract_std       :: list(item_type_defn_info_abstract),
                tcad_eqv                :: list(item_type_defn_info_eqv),
                tcad_du                 :: list(item_type_defn_info_du),
                tcad_sub                :: list(item_type_defn_info_sub),
                tcad_foreign            :: c_j_cs_defns
            ).

:- type type_ctor_maybe_defn
    --->    type_ctor_maybe_defn(
                % Abstract and nonabstract solver type definitions.
                tcmd_abstract_solver    :: maybe(item_type_defn_info_abstract),
                tcmd_solver             :: maybe(item_type_defn_info_solver),

                % Abstract and nonabstract nonsolver type definitions.
                tcmd_abstract_std       :: maybe(item_type_defn_info_abstract),
                tcmd_eqv                :: maybe(item_type_defn_info_eqv),
                tcmd_du                 :: maybe(item_type_defn_info_du),
                tcmd_sub                :: maybe(item_type_defn_info_sub),
                tcmd_foreign            :: c_j_cs_maybe_defn
            ).

    % We support foreign type definitions in all three of our target languages,
    % C, Java and C#. Likewise, we allow foreign enum declarations
    % in these three languages.
    %
    % There are several kinds of info that we may want to store for every
    % one of these foreign languages. This can be done in instances
    % of this type, whose fields always contain the info for C, Java and C#
    % (in that order).
:- type c_java_csharp(T)
    --->    c_java_csharp(T, T, T).

:- type c_j_cs_defns ==
    c_java_csharp(list(item_type_defn_info_foreign)).
:- type c_j_cs_maybe_defn ==
    c_java_csharp(maybe(item_type_defn_info_foreign)).
:- type c_j_cs_enums ==
    c_java_csharp(list(item_foreign_enum_info)).
:- type c_j_cs_maybe_enum ==
    c_java_csharp(maybe(item_foreign_enum_info)).
:- type c_j_cs_repn ==
    c_java_csharp(maybe(foreign_type_repn)).
:- type c_j_cs_enum_repn ==
    c_java_csharp(maybe(enum_foreign_repn)).

:- type inst_ctor_defn_map == map(inst_ctor, inst_ctor_all_defns).
:- type inst_ctor_all_defns
    --->    inst_ctor_all_defns(
                icad_abstract           :: list(item_inst_defn_info_abstract),
                icad_eqv                :: list(item_inst_defn_info_eqv)
            ).

:- type mode_ctor_defn_map == map(mode_ctor, mode_ctor_all_defns).
:- type mode_ctor_all_defns
    --->    mode_ctor_all_defns(
                mcad_abstract           :: list(item_mode_defn_info_abstract),
                mcad_eqv                :: list(item_mode_defn_info_eqv)
            ).

:- type type_ctor_foreign_enum_map == map(type_ctor, c_j_cs_enums).

:- type type_ctor_repn_map == map(type_ctor, item_type_repn_info).

%---------------------------------------------------------------------------%
%
% The representation of a checked-to-be-consistent set of type and
% foreign enum definitions for every type constructor defined in a module.
%

:- type type_ctor_checked_map == map(type_ctor, type_ctor_checked_defn).

    % A type is either a solver type, or not.
:- type type_ctor_checked_defn
    --->    checked_defn_solver(solver_type_defn, src_defns_solver)
    ;       checked_defn_std(std_type_defn, src_defns_std).

%---------------------%

    % Replace this one general type with one type for each function symbol
    % in solver_type_defn.
:- type src_defns_solver
    --->    src_defns_solver(
                % The item_type_defn_info (if any) in the interface section.
                maybe(item_type_defn_info),

                % The item_type_defn_info (if any) in the impl section.
                maybe(item_type_defn_info)
            ).

    % Replace this one general type with one type for each function symbol
    % in std_type_defn.
:- type src_defns_std
    --->    src_defns_std(
                % The item_type_defn_infos in the interface section.
                list(item_type_defn_info),

                % The item_type_defn_infos and item_foreign_enum_infos
                % in the implementation section.
                list(item_type_defn_info),
                list(item_foreign_enum_info)
            ).

%---------------------%

:- type solver_type_defn
    --->    solver_type_abstract(
                abstract_solver_type_status,

                % The abstract definition. It may be in either section;
                % the status specifies the section.
                item_type_defn_info_abstract
            )
    ;       solver_type_full(
                % The abstract definition in the interface section,
                % if one exists.
                maybe(item_type_defn_info_abstract),

                % The full solver type definition, which must be in the
                % implementation section.
                item_type_defn_info_solver
            ).

:- type abstract_solver_type_status
    --->    abstract_solver_type_exported
            % The type name is exported. The abstract definition
            % is in the interface section.
    ;       abstract_solver_type_private.
            % The type name is not exported. The abstract definition
            % is in the implementation section.

%---------------------%

:- type std_type_defn
    --->    std_mer_type_eqv(
                std_eqv_type_status,

                % The equivalence type definition.
                item_type_defn_info_eqv
            )
    ;       std_mer_type_subtype(
                std_subtype_status,

                % The subtype definition.
                item_type_defn_info_sub
            )
    ;       std_mer_type_du_all_plain_constants(
                std_du_type_status,

                % The discriminated union type definition which represents
                % either a direct dummy type or an enum.
                item_type_defn_info_du,

                % The first functor name in the type, and any later functor
                % names. If there are no later functor names, then the type
                % is a direct dummy type, and must satisfy the requirements
                % of non_sub_du_type_is_dummy; if there are, then the type
                % is an enum type, and must satisfy the requirements of
                % non_sub_du_type_is_enum. (Function symbols that do not meet
                % the relevant requirements may be constants, but we
                % don't consider them *plain* constants.)
                string,
                list(string),

                % For each of our target foreign languages, this field
                % specifies whether we have either a foreign language
                % definition for this type, or a foreign enum definition.
                %
                % While the Mercury representation uses small integers
                % allocated consecutively from 0 to represent function symbols,
                % this is not true even for foreign enum definitions,
                % much less foreign type definitions.
                c_j_cs_maybe_defn_or_enum
            )
    ;       std_mer_type_du_not_all_plain_constants(
                std_du_type_status,

                % The discriminated union type definition which represents
                % a type *other* than a direct dummy type or an enum.
                item_type_defn_info_du,

                % For each of our target foreign languages, this field
                % specifies whether we have a foreign language type definition
                % for this type.
                c_j_cs_maybe_defn
            )
    ;       std_mer_type_abstract(
                std_abs_type_status,

                % The abstract declaration of the type (not a subtype).
                item_type_defn_info_abstract,

                % For each of our target foreign languages, this field
                % specifies whether we have a foreign language type definition
                % for this type.
                c_j_cs_maybe_defn
            ).

:- type maybe_only_constants
    --->    not_only_plain_constants
    ;       only_plain_constants(
                % The names of the constants, in the order of declaration.
                opc_head_name       :: string,
                opc_tail_names      :: list(string)
            ).

:- type std_eqv_type_status
    --->    std_eqv_type_mer_exported
            % The Mercury definition (i.e. the equivalence) is exported.
    ;       std_eqv_type_abstract_exported
            % Only the type name is exported. The Mercury definition
            % is private.
    ;       std_eqv_type_all_private.
            % Everything about the type is private.

:- type std_du_type_status
    --->    std_du_type_mer_ft_exported
            % Both the Mercury and any foreign type definitions are exported.
            % Any foreign enum definitions are private, as they have to be.
            % This status is not applicable to equivalence types or subtypes,
            % since they may not have foreign type definitions.
    ;       std_du_type_mer_exported
            % The Mercury definition is exported. Any foreign type definitions
            % and/or foreign enum definitions are private.
    ;       std_du_type_abstract_exported
            % Only the type name is exported. The Mercury definition and
            % any foreign type definitions and/or foreign enum definitions
            % are private.
    ;       std_du_type_all_private.
            % Everything about the type is private.

    % A version of std_du_type_status for subtypes, which may not have
    % any foreign type definitions, and for which therefore the question of
    % whether any foreign type definitions are exported is moot.
:- type std_subtype_status
    --->    std_sub_type_mer_exported
    ;       std_sub_type_abstract_exported
    ;       std_sub_type_all_private.

:- type std_abs_type_status
    --->    std_abs_type_ft_exported
            % The type has foreign type definitions that are exported.
            % Any foreign enum definitions are private, as they have to be.
    ;       std_abs_type_abstract_exported
            % Only the type name is exported. Any foreign type definitions
            % and/or foreign enum definitions are private.
    ;       std_abs_type_all_private.
            % Everything about the type is private.

%---------------------%

:- type c_j_cs_maybe_defn_or_enum ==
    c_java_csharp(maybe(foreign_type_or_enum)).

:- type foreign_type_or_enum
    --->    foreign_type_or_enum_type(item_type_defn_info_foreign)
    ;       foreign_type_or_enum_enum(checked_foreign_enum).

    % Part of checking a foreign enum definition is checking whether
    % the correspondence it describes between the Mercury functors
    % of the type on the one hand and their foreign language counterparts
    % on the other hand is a bijection. If it is, then the second argument
    % of the checked_foreign_enum we construct gives the foreign language
    % counterpart of each Mercury function symbol in the type in the order
    % in which the Mercury function symbols are defined.
    %
    % For example, given
    %
    %   :- type t ---> m1 ; m2 ; m3.
    %
    % and a foreign enum definition that gives the correspondence correctly
    % but in a different order, such as
    %
    %   :- pragma foreign_enum("C", t/0, [m2 - "f2", m3 - "f3", m1 - "f1"]).
    %
    % the second argument will contain the (nonempty) list "f1", "f2", "f3".
    %
    % On the other hand, if the mapping in the foreign enum definition is
    % *not* a bijection, then we will not generate a checked_foreign_enum
    % structure for it.
    %
:- type checked_foreign_enum
    --->    checked_foreign_enum(item_foreign_enum_info, one_or_more(string)).

%---------------------------------------------------------------------------%
%
% Information about the representations of types.
%

    % This type and type_ctor_checked_defn are closely related.
    % The principal differences are the following.
    %
    % - type_ctor_checked_defn deals with solver types. Since solver types
    %   have no representation information themselves (they are represented
    %   by values of another type), this type does not deal with them.
    %
    % - One of the purposes of type_ctor_checked_defn is to decide
    %   what items to include in interface files, for use by code using
    %   the compiler's ancient approach to deciding type representation,
    %   where each compiler invocation that generated code decided for itself
    %   how every type it had access to was represented, including the types
    %   imported from other modules. This means that it needs to contain
    %   either whole items (of particular kinds), or information from which
    %   whole items can be reconstructed.
    %
    % - The above consideration also requires a type_ctor_checked_defn
    %   to specify the status of the type. On the other hand, values of
    %   this type have no use for status information. Status information
    %   is used only for checking whether an access to a type should be
    %   allowed or not; the only use of values of this type is to help
    %   compute type representations.
    %
    % - Only this type needs to contain representation information.
    %   A value of the type_ctor_checked_defn type needs to contain *part*
    %   of the information from which this representation information is
    %   computed for its type, but not *all* of it; some of that information
    %   comes from information about the representation of *other* types.
    %
    % One sort-of difference is while both contain information that has been
    % checked by a compiler invocation, values of this type that have been
    % read in from an interface file, while checked by another compiler
    % invocation before being written out, may be corrupted in the filesystem.
    % However, while this danger is always present, we need not take any
    % special steps to guard against it, precisely because no perfect defense
    % is possible.
    %
    % XXX TYPE_REPN Consider whether we can split this type into two,
    % one for the tcrepns that can occur in .int3 files, and one for the
    % tcrepns that can occur in .int/.int2 files.
    %
:- type type_ctor_repn_info
    --->    tcrepn_is_word_aligned_ptr
    ;       tcrepn_is_eqv_to(mer_type)
    ;       tcrepn_is_subtype_of(type_ctor)
    ;       tcrepn_du(du_repn)
    ;       tcrepn_foreign(c_j_cs_repn).

    % A type that has a discriminated union definition in Mercury
    % may also have a definition in each of our foreign languages,
    % If it is an direct_dummy or enum type, that definition may be
    % either a foreign type definition or a foreign enum definition;
    % otherwise, it can only be a foreign type definition.
:- type du_repn
    --->    dur_direct_dummy(direct_dummy_repn)
    ;       dur_enum(enum_repn)
    ;       dur_notag(notag_repn)
    ;       dur_gen_only_functor(gen_du_only_functor_repn)
    ;       dur_gen_more_functors(gen_du_more_functors_repn).

    % When targeting C, many argument packing decisions depend on
    % three properties of the target platform, i.e. on the combination
    % of the target hardware and the target grade:
    %
    % - whether the target is 64 or 32 bit;
    % - whether the grade is an spf (single-precision float) grade; and
    % - whether the grade allows the direct arg optimization.
    %
    % These have eight combinations, but the spf grade component has
    % no effect on argument packing on 64 bit targets (a float is one word
    % either way), so only six are meaningful.
    %
    % If the decision represented by the T parameter happens to be the same
    % on all six platforms, that decision can be represented by c_repns_same.
    %
    % If they are different on 64 vs 32 bit platforms, but are consistent
    % for each word size, then they can be represented by c_repns_64_32.
    %
    % If neither is the case, we can record all six decisions using
    % c_repns_all.
    %
    % XXX We should look for other partitions of the set of six platforms
    % which often have identical decision results; one could be da vs noda.
    %
    % The name of this type is c_repns because argument packing applies
    % only to the low level data representation, which is applicable only
    % when targeting C.
:- type c_repns(T)
    --->    c_repns_same(
                c_repn_same             :: T
            )
    ;       c_repns_64_32(
                c_repn_all_64           :: T,
                c_repn_all_32           :: T
            )
    ;       c_repns_all(
                c_repn_64_nospf_noda    :: T,
                c_repn_64_nospf_da      :: T,
                % c_repn_64_spf_noda    :: T,   % not needed; see above
                % c_repn_64_spf_da      :: T,   % not needed; see above
                c_repn_32_nospf_noda    :: T,
                c_repn_32_nospf_da      :: T,
                c_repn_32_spf_noda      :: T,
                c_repn_32_spf_da        :: T
            ).

%---------------------%

:- type direct_dummy_repn
    --->    direct_dummy_repn(
                % The type is a direct dummy type that satisfies the
                % requirements of du_type_is_dummy.

                % The name of the one functor in the type, which must be
                % arity 0. Its representation will be dummy_tag.
                dummy_functor_name      :: string,

                % Any foreign type or foreign enum definitions for the type.
                dummy_foreign           :: c_j_cs_enum_repn
            ).

%---------------------%

:- type enum_repn
    --->    enum_repn(
                % The type is an enum type that satisfies the requirements
                % of non_sub_du_type_is_enum.

                % The list of the functor names (all arity 0). We store
                % the first two separately to enforce the structural invariant
                % that an enum must have at least two functors.
                %
                % The representation of functor #N in Mercury will be
                % int_tag(int_tag_int(N)), with counting starting at 0.
                %
                % We do not care about the 32 vs 64 bit distinction here,
                % because the definition of an enum type with more than 2^32
                % function symbols will cause a compiler to run out of memory
                % for a *very* long time to come.
                enum_functor1           :: string,
                enum_functor2           :: string,
                enum_functors3plus      :: list(string),

                % Any foreign type or foreign enum definitions for the type.
                enum_foreign            :: c_j_cs_enum_repn
            ).

%---------------------%

:- type notag_repn
    --->    notag_repn(
                % The name of the one functor in the type, which must be
                % arity 1. Its representation will be no_tag.
                % The representation of the argument be *recorded*
                % as a full word at offset 0, but this should never be
                % looked up, since the argument will actually be stored
                % wherever the whole term is stored.
                notag_functor_name      :: string,

                % The type of the one functor's one argument.
                % We record this because without this information,
                % we cannot recognize that a notag type whose argument size
                % is less than one word can itself be stored in less than
                % one word.
                notag_functor_arg_type  :: mer_type,

                % The foreign language definitions for this type, if any.
                notag_foreign           :: c_j_cs_repn
            ).

%---------------------%

:- type gen_du_only_functor_repn
    --->    gen_du_only_functor_repn(
                % The name of the data constructor. The arity is given by
                % the length of list of argument types. The lists of argument
                % representations in all of the nonconstant_repns inside
                % the c_repns must also ave this length.
                only_functor            :: string,

                % The types of the constructor's arguments, after
                % the expansion of both equivalence types and notag types.
                only_deref_arg_types    :: list(mer_type),

                % The representation of this functor for each possible
                % target platform with the low level data representation.
                % The nonconstant_repn cannot be ncr_direct_arg.
                % XXX TYPE_REPN could we encode that invariant in the type?
                only_arg_repns          :: c_repns(only_nonconstant_repn),

                % The foreign language definitions for this type, if any.
                only_foreign            :: c_j_cs_repn
            ).

:- type gen_du_more_functors_repn
    --->    gen_du_more_functors_repn(
                % The first, second and any later functors in the type,
                % in declaration order, i.e. ordered on the functors'
                % original ordinal numbers.
                more_functor1           :: gen_du_functor_repn,
                more_functor2           :: gen_du_functor_repn,
                more_functors3plus      :: list(gen_du_functor_repn),

                % The foreign language definitions for this type, if any.
                more_foreign            :: c_j_cs_repn
            ).

%---------------------%

:- type gen_du_functor_repn
    --->    gen_du_constant_functor_repn(
                % The name of the data constructor. The arity is zero.
                gducf_functor           :: string,

                % The representation of this functor for each possible
                % target platform with the low level data representation.
                gducf_functor_repn      :: c_repns(constant_repn)
            )
    ;       gen_du_nonconstant_functor_repn(
                % The name of the data constructor. The arity is given by
                % the length of list of argument types. The lists of argument
                % representations in all of the nonconstant_repns inside
                % the c_repns must also ave this length.
                gduncf_functor          :: string,

                % The types of the constructor's arguments, after
                % the expansion of both equivalence types and notag types.
                %
                % Logically, the type of each argument belongs with
                % the representation of that argument, but we have to store
                % up to six versions of the representation, and we don't want
                % a duplicate copy of the type next to each version.
                gduncf_deref_arg_types  :: list(mer_type),

                % The representation of this functor for each possible
                % target platform with the low level data representation.
                gduncf_functor_repn     :: c_repns(more_nonconstant_repn)
            ).

:- type constant_repn
    --->    constant_repn(
                % The ptag is 0. The next two fields specify the value
                % and the size of the local secondary tag.
                cr_sectag               :: uint,
                cr_sectag_size          :: lsectag_word_or_size
            ).

:- type only_nonconstant_repn
    --->    oncr_local_cell(only_nonconstant_local_cell_repn)
    ;       oncr_remote_cell(only_nonconstant_remote_cell_repn).

:- type more_nonconstant_repn
    --->    mncr_local_cell(more_nonconstant_local_cell_repn)
    ;       mncr_remote_cell(more_nonconstant_remote_cell_repn)
    ;       mncr_direct_arg(ptag).

:- type only_nonconstant_local_cell_repn
    --->    only_nonconstant_local_cell_repn(
                % The ptag and local sectag are both implicitly 0u.
                onclcr_arg_repns        :: one_or_more(local_arg_repn)
            ).

:- type more_nonconstant_local_cell_repn
    --->    more_nonconstant_local_cell_repn(
                % The ptag is implicitly 0u.
                mnclcr_sectag           :: cell_local_sectag,
                mnclcr_arg_repns        :: one_or_more(local_arg_repn)
            ).

:- type only_nonconstant_remote_cell_repn
    --->    only_nonconstant_remote_cell_repn(
                % The ptag is both implicitly 0u, and there is
                % no remote sectag.
                ncrcr_arg_repns         :: one_or_more(remote_arg_repn)
            ).

:- type more_nonconstant_remote_cell_repn
    --->    more_nonconstant_remote_cell_repn(
                ncrcr_ptag              :: ptag,
                ncrcr_sectag            :: cell_remote_sectag,
                ncrcr_arg_repns         :: one_or_more(remote_arg_repn)
            ).

:- type cell_local_sectag
    --->    cell_local_sectag(
                clss_sectag             :: uint,
                clss_sectag_size        :: uint8
            ).

:- type cell_remote_sectag
    --->    cell_remote_no_sectag
    ;       cell_remote_sectag(
                crss_sectag             :: uint,
                crss_sectag_size        :: rsectag_word_or_size
            ).

:- type lsectag_word_or_size
    --->    lsectag_rest_of_word(uint8)
    ;       lsectag_part_of_word(uint8).

:- type rsectag_word_or_size
    --->    rsectag_full_word
    ;       rsectag_part_of_word(uint8).

:- type local_arg_repn
    --->    local_partial(
                lp_shift                :: uint,
                lp_fill                 :: fill_kind_size
            )
    ;       local_none.

:- type remote_arg_repn
    --->    remote_full(
                rf_arg_only_offset      :: arg_only_offset,
                rf_cell_offset          :: cell_offset
            )
    ;       remote_double(
                rd_arg_only_offset      :: arg_only_offset,
                rd_cell_offset          :: cell_offset,
                rd_kind                 :: double_word_kind
            )
    ;       remote_partial_first(
                rpf_arg_only_offset     :: arg_only_offset,
                rpf_cell_offset         :: cell_offset,
                rpf_shift               :: uint8,
                rpf_fill                :: fill_kind_size
            )
    ;       remote_partial_shifted(
                rps_arg_only_offset     :: arg_only_offset,
                rps_cell_offset         :: cell_offset,
                rps_shift               :: uint8,
                rps_fill                :: fill_kind_size
            )
    ;       remote_none_shifted(
                rns_arg_only_offset     :: arg_only_offset,
                rns_cell_offset         :: cell_offset
            )
    ;       remote_none_nowhere.

:- type fill_kind_size
    --->    fk_enum(uint)   % XXX TYPE_REPN should be uint8
    ;       fk_int8
    ;       fk_int16
    ;       fk_int32
    ;       fk_uint8
    ;       fk_uint16
    ;       fk_uint32
    ;       fk_char21.

    % XXX TYPE_REPN should return uint8
:- func fill_kind_size_num_bits(fill_kind_size) = uint.

%---------------------%

:- type foreign_type_lang_repn
    --->    foreign_type_lang_repn(
                ftlr_lang               :: foreign_language,
                ftlr_foreign_type       :: foreign_type_repn
            ).

:- type foreign_type_repn
    --->    foreign_type_repn(
                % The name of the foreign type that represents values
                % of this Mercury type.
                ftr_foreign_type        :: string,

                % The assertions about this foreign type.
                ftr_assertions          :: foreign_type_assertions
            ).

:- type enum_foreign_repn
    --->    enum_foreign_type(foreign_type_repn)
    ;       enum_foreign_enum(one_or_more(string)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

fill_kind_size_num_bits(FillKindSize) = NumBits :-
    (
        FillKindSize = fk_enum(NumBits)
    ;
        ( FillKindSize = fk_int8
        ; FillKindSize = fk_uint8
        ),
        NumBits = 8u
    ;
        ( FillKindSize = fk_int16
        ; FillKindSize = fk_uint16
        ),
        NumBits = 16u
    ;
        ( FillKindSize = fk_int32
        ; FillKindSize = fk_uint32
        ),
        NumBits = 32u
    ;
        FillKindSize = fk_char21,
        NumBits = 21u
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_item_type.
%---------------------------------------------------------------------------%
