%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007, 2011 The University of Melbourne.
% Copyright (C) 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module defines the types that the smart recompilation system uses
% that must be parts of the parse trees of .intN files.
%
%-----------------------------------------------------------------------------%

:- module recompilation.item_types.
:- interface.

:- import_module libs.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module map.
:- import_module term.

%-----------------------------------------------------------------------------%

% XXX ITEM_LIST Choose a base name for these types that DOESN'T clash
% with the item type in the parse tree. While the types here are closely
% related to prog_item.item, they are NOT the same. Using the same name
% here encourages thinking that they are, which may lead to bugs.
%
% XXX ITEM_LIST Document what prog_item.item, or what sequence of
% prog_item.items, each item_type may correspond to.

:- type recomp_item_id
    --->    recomp_item_id(recomp_item_type, recomp_item_name).

    % XXX RECOMP Consider splitting this type into two or more types,
    % one for each separate purpose. We use this, amongst other things,
    % for selecting one field from several data structures (module_versions,
    % used_items, resolved_used_items, module_imported_items, and
    % gathered_items) which have similar but NOT IDENTICAL sets of fields,
    % so some of these item_types *have* no corresponding field in some
    % of those structures.
    %
:- type recomp_item_type
    --->    recomp_type_name
            % Just the name of the type, not its body. It is common
            % for a value of a type to be passed through a predicate without
            % inspecting the value -- such predicates do not need to be
            % recompiled if the body of the type changes (except for
            % equivalence types).
    ;       recomp_type_defn
    ;       recomp_inst
    ;       recomp_mode
    ;       recomp_typeclass
    ;       recomp_functor        % The RHS of a var-functor unification.
    ;       recomp_predicate
    ;       recomp_function
            % XXX ARITY The arity we record next to function_items *seems*
            % to be the user_arity (though its type is just "arity"),
            % but in the presence of with_type annotations, even that is
            % in question.
    ;       recomp_mutable
    ;       recomp_foreign_proc.
            % XXX ARITY This does not say whether the foreign_proc is for
            % a predicate or a function, which affects the interpretation
            % of the associated arity.

:- inst recomp_simple for recomp_item_type/0
    --->    recomp_type_name
    ;       recomp_type_defn
    ;       recomp_inst
    ;       recomp_mode
    ;       recomp_typeclass.

:- inst recomp_pred_or_func for recomp_item_type/0
    --->    recomp_predicate
    ;       recomp_function.

:- type recomp_item_name
    --->    recomp_item_name(sym_name, arity).
            % For predicates and functions, the arity is the pred_form_arity,
            % I think (zs).

%-----------------------------------------------------------------------------%

:- func pred_or_func_to_recomp_item_type(pred_or_func::in)
    = (recomp_item_type::out(recomp_pred_or_func)) is det.

:- pred string_to_recomp_item_type(string, recomp_item_type).
:- mode string_to_recomp_item_type(in, out) is semidet.
:- mode string_to_recomp_item_type(out, in) is det.

:- func type_ctor_to_recomp_item_name(type_ctor) = recomp_item_name.
:- func inst_ctor_to_recomp_item_name(inst_ctor) = recomp_item_name.
:- func mode_ctor_to_recomp_item_name(mode_ctor) = recomp_item_name.

:- func recomp_item_name_to_type_ctor(recomp_item_name) = type_ctor.
:- func recomp_item_name_to_inst_ctor(recomp_item_name) = inst_ctor.
:- func recomp_item_name_to_mode_ctor(recomp_item_name) = mode_ctor.

%-----------------------------------------------------------------------------%

    % Map modules' names to their version number info.
:- type module_item_version_numbers_map ==
    map(module_name, module_item_version_numbers).

    % Values of this type specify the version number of each visible item
    % in a module.
    %
    % XXX The comment on the type of the predecessor of the vn_instances field
    % said: "For each interface file, we keep a version number for each class",
    % which is quite confusing.
    %
:- type module_item_version_numbers
    --->    module_item_version_numbers(
                mivn_type_names     :: name_arity_version_map,
                mivn_type_defns     :: name_arity_version_map,
                mivn_insts          :: name_arity_version_map,
                mivn_modes          :: name_arity_version_map,
                mivn_typeclasses    :: name_arity_version_map,
                mivn_instances      :: recomp_item_name_version_map,
                mivn_predicates     :: name_arity_version_map,
                mivn_functions      :: name_arity_version_map
            ).

:- type name_arity_version_map == map(name_arity, version_number).

    % Identify a particular version of a program item.
    % This could be done using a timestamp or a hash value.
    %
    % XXX RECOMP We had a thread on m-rev on .used files starting on 2021-04-19
    % on replacing this with two separate representations of time: a binary one
    % containing seconds since the epoch, plus any sub-second-precision
    % information the OS may offer, and a text one for the readability
    % of .used files.
    %
:- type version_number == timestamp.

:- type recomp_item_name_version_map == map(recomp_item_name, version_number).

%-----------------------------------------------------------------------------%

:- func init_module_item_version_numbers = module_item_version_numbers.

%-----------------------------------------------------------------------------%

:- pred parse_version_number_term(term(T)::in, version_number::out) is semidet.

:- pred parse_timestamp_term(term(T)::in, timestamp::out) is semidet.

:- func version_number_to_string(version_number) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

pred_or_func_to_recomp_item_type(pf_predicate) = recomp_predicate.
pred_or_func_to_recomp_item_type(pf_function) = recomp_function.

% The mismatch between the string and the recomp_item_type for type and
% type_body is for historical reasons.
string_to_recomp_item_type("type",          recomp_type_name).
string_to_recomp_item_type("type_body",     recomp_type_defn).
string_to_recomp_item_type("inst",          recomp_inst).
string_to_recomp_item_type("mode",          recomp_mode).
string_to_recomp_item_type("typeclass",     recomp_typeclass).
string_to_recomp_item_type("functor",       recomp_functor).
string_to_recomp_item_type("predicate",     recomp_predicate).
string_to_recomp_item_type("function",      recomp_function).
string_to_recomp_item_type("mutable",       recomp_mutable).
string_to_recomp_item_type("foreign_proc",  recomp_foreign_proc).

type_ctor_to_recomp_item_name(type_ctor(SymName, Arity))
    = recomp_item_name(SymName, Arity).
inst_ctor_to_recomp_item_name(inst_ctor(SymName, Arity))
    = recomp_item_name(SymName, Arity).
mode_ctor_to_recomp_item_name(mode_ctor(SymName, Arity))
    = recomp_item_name(SymName, Arity).

recomp_item_name_to_type_ctor(recomp_item_name(SymName, Arity))
    = type_ctor(SymName, Arity).
recomp_item_name_to_inst_ctor(recomp_item_name(SymName, Arity))
    = inst_ctor(SymName, Arity).
recomp_item_name_to_mode_ctor(recomp_item_name(SymName, Arity))
    = mode_ctor(SymName, Arity).

%-----------------------------------------------------------------------------%

init_module_item_version_numbers =
    module_item_version_numbers(map.init, map.init, map.init, map.init,
        map.init, map.init, map.init, map.init).

%-----------------------------------------------------------------------------%

parse_version_number_term(Term, Timestamp) :-
    parse_timestamp_term(Term, Timestamp).

parse_timestamp_term(Term, Timestamp) :-
    Term = term.functor(term.string(Str), [], _),
    parse_timestamp_string(Str, Timestamp).

version_number_to_string(VersionNumber) = VersionNumberStr :-
    string.format("""%s""", [s(timestamp_to_string(VersionNumber))],
        VersionNumberStr).

%-----------------------------------------------------------------------------%
:- end_module recompilation.item_types.
%-----------------------------------------------------------------------------%
