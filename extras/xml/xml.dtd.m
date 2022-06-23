%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% Copyright (C) 2014, 2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
%---------------------------------------------------------------------------%

:- module xml.dtd.
:- interface.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- type name == string.

:- type token == string.

:- type dtd
    --->    dtd(
                dtd_root        :: name,
                dtd_elements    :: map(name, element),
                dtd_entities    :: map(name, entity_def),
                dtd_p_entities  :: map(name, entity_def)
            ).

:- type element
    --->    element(
                elt_name        :: name,
                elt_attrs       :: map(name, attribute),
                elt_content     :: content
            ).

:- type content
    --->    empty
    ;       any
    ;       children(content_particle)
    ;       mixed(mixed).

:- type content_particle
    --->    children_reps(children, multiplicity).

:- type children
    --->    seq(list(content_particle))
    ;       alt(list(content_particle))
    ;       element(name).

:- type mixed
    --->    mixed(list(name)).

:- type multiplicity
    --->    one
    ;       zero_or_more
    ;       one_or_more
    ;       zero_or_one.

:- type attribute
    --->    attribute(
                attr_name    :: name,
                attr_type    :: attr_type,
                attr_default :: default
            ).

:- type attr_type
    --->    attr_cdata
    ;       attr_id
    ;       attr_id_ref
    ;       attr_id_refs
    ;       attr_entity
    ;       attr_entities
    ;       attr_nm_token
    ;       attr_nm_tokens
    ;       attr_notation(list(token))
    ;       attr_enum(list(token)).

:- type default
    --->    required
    ;       implied
    ;       defaulted(string)
    ;       fixed(string).

:- type entity_def
    --->    entity_internal(entity)
    ;       entity_external(external_id).

:- type entity == string.

:- type external_id
    --->    system(string)
    ;       public(string, string).
