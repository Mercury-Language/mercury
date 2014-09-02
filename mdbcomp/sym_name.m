%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: sym_name.m.
%
% This module contains the types that define symbol names, and predicates
% that operate on them.
%
% This functionality is shared between the compiler and the debugger.
%
%-----------------------------------------------------------------------------%

:- module mdbcomp.sym_name.
:- interface.

:- import_module list.

    % The order that the sym_name function symbols appear in can be significant
    % for module dependency ordering.
    %
:- type sym_name
    --->    unqualified(string)
    ;       qualified(sym_name, string).

:- type module_name == sym_name.

    % get_ancestors(ModuleName) = ParentDeps:
    %
    % ParentDeps is the list of ancestor modules for this module, oldest first;
    % e.g. if the ModuleName is `foo.bar.baz', then ParentDeps would be
    % [`foo', `foo.bar'].
    %
:- func get_ancestors(module_name) = list(module_name).

    % string_to_sym_name_sep(String, Separator) = SymName:
    %
    % Convert a string, possibly prefixed with module qualifiers (separated
    % by Separator), into a symbol name.
    %
:- func string_to_sym_name_sep(string, string) = sym_name.

    % string_to_sym_name(String) = SymName:
    %
    % Convert a string, possibly prefixed with module qualifiers (separated
    % by the standard Mercury module qualifier separator), into a symbol name.
    %
:- func string_to_sym_name(string) = sym_name.

    % sym_name_to_string_sep(SymName, Separator) = String:
    %
    % Convert a symbol name to a string, with module qualifiers separated
    % by Separator.
    %
:- func sym_name_to_string_sep(sym_name, string) = string.

    % sym_name_to_string(SymName) = String:
    %
    % Convert a symbol name to a string, with module qualifiers separated by
    % the standard Mercury module qualifier operator.
    %
:- func sym_name_to_string(sym_name) = string.

    % sym_name_to_list(SymName) = List:
    %
    % Convert a symbol name to a list of its component strings,
    % qualifiers first, actual name last.
    %
:- func sym_name_to_list(sym_name) = list(string).

    % sym_name_to_qualifier_list_and_name(SymName, QualifierList, Name):
    %
    % Convert a symbol name to a list of its component strings,
    % returning the list of qualifiers separately from the actual name.
    %
:- pred sym_name_to_qualifier_list_and_name(sym_name::in,
    list(string)::out, string::out) is det.

    % is_submodule(SymName1, SymName2):
    %
    % True iff SymName1 is a submodule of SymName2.
    % For example mod1.mod2.mod3 is a submodule of mod1.mod2.
    %
:- pred is_submodule(module_name::in, module_name::in) is semidet.

    % Given a symbol name, return its unqualified name.
    %
:- func unqualify_name(sym_name) = string.

    % sym_name_get_module_name(SymName) = ModName:
    %
    % Given a symbol name, return the module qualifiers(s).
    % Fails if the symbol is unqualified.
    %
:- pred sym_name_get_module_name(sym_name::in, module_name::out) is semidet.

    % sym_name_get_module_name_default(SymName, DefaultModName, ModName):
    %
    % Given a symbol name, return the module qualifier(s).
    % If the symbol is unqualified, then return the specified default
    % module name.
    %
:- pred sym_name_get_module_name_default(sym_name::in, module_name::in,
    module_name::out) is det.

    % sym_name_get_module_name_default_name(SymName,
    %   DefaultModName, ModName, Name):
    % Return the ModName sym_name_get_module_name_default would,
    % and the Name unqualify_name would.
    %
:- pred sym_name_get_module_name_default_name(sym_name::in, module_name::in,
    module_name::out, string::out) is det.

    % match_sym_name(PartialSymName, CompleteSymName):
    %
    % Succeeds iff there is some sequence of module qualifiers
    % which when prefixed to PartialSymName gives CompleteSymName.
    %
:- pred match_sym_name(sym_name::in, sym_name::in) is semidet.

    % remove_sym_name_prefix(SymName0, Prefix, SymName)
    % succeeds iff
    %   SymName and SymName0 have the same module qualifier
    %   and the unqualified part of SymName0 has the given prefix
    %   and the unqualified part of SymName is the unqualified
    %       part of SymName0 with the prefix removed.
    %
:- pred remove_sym_name_prefix(sym_name, string, sym_name).
:- mode remove_sym_name_prefix(in, in, out) is semidet.
:- mode remove_sym_name_prefix(out, in, in) is det.

    % remove_sym_name_suffix(SymName0, Suffix, SymName)
    % succeeds iff
    %   SymName and SymName0 have the same module qualifier
    %   and the unqualified part of SymName0 has the given suffix
    %   and the unqualified part of SymName is the unqualified
    %       part of SymName0 with the suffix removed.
    %
:- pred remove_sym_name_suffix(sym_name::in, string::in, sym_name::out)
    is semidet.

    % add_sym_name_suffix(SymName0, Suffix, SymName)
    % succeeds iff
    %   SymName and SymName0 have the same module qualifier
    %   and the unqualified part of SymName is the unqualified
    %       part of SymName0 with the suffix added.
    %
:- pred add_sym_name_suffix(sym_name::in, string::in, sym_name::out) is det.

    % transform_sym_base_name(TransformFunc, SymName0) = SymName
    % succeeds iff
    %   SymName and SymName0 have the same module qualifier
    %   and the unqualified part of SymName is the result of applying
    %   TransformFunc to the unqualified part of SymName0.
    %
:- func transform_sym_base_name(func(string) = string, sym_name) = sym_name.

    % Given a sym_name return the top level qualifier of that name.
    %
:- func outermost_qualifier(sym_name) = string.

    % add_outermost_qualifier(ModuleName, SymName0) = SymName:
    %
    % Prepend the specified ModuleName onto the module qualifiers in SymName0,
    % giving SymName.
    %
:- func add_outermost_qualifier(string, sym_name) = sym_name.

    % Remove and return the top level qualifier of a sym_name.
    %
:- pred strip_outermost_qualifier(sym_name::in,
    string::out, sym_name::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module library.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

get_ancestors(ModuleName) = get_ancestors_2(ModuleName, []).

:- func get_ancestors_2(module_name, list(module_name)) = list(module_name).

get_ancestors_2(unqualified(_), Ancestors) = Ancestors.
get_ancestors_2(qualified(Parent, _), Ancestors0) =
    get_ancestors_2(Parent, [Parent | Ancestors0]).

%-----------------------------------------------------------------------------%

string_to_sym_name_sep(String, ModuleSeparator) = Result :-
    % This would be simpler if we had a string.rev_sub_string_search/3 pred.
    % With that, we could search for underscores right-to-left, and construct
    % the resulting symbol directly. Instead, we search for them left-to-right,
    % and then call add_outermost_qualifier to fix things up.
    (
        string.sub_string_search(String, ModuleSeparator, LeftLength),
        LeftLength > 0
    ->
        string.left(String, LeftLength, ModuleName),
        string.length(String, StringLength),
        string.length(ModuleSeparator, SeparatorLength),
        RightLength = StringLength - LeftLength - SeparatorLength,
        string.right(String, RightLength, Name),
        NameSym = string_to_sym_name_sep(Name, ModuleSeparator),
        Result = add_outermost_qualifier(ModuleName, NameSym)
    ;
        Result = unqualified(String)
    ).

string_to_sym_name(String) = string_to_sym_name_sep(String, ".").

sym_name_to_string_sep(unqualified(Name), _Separator) = Name.
sym_name_to_string_sep(qualified(ModuleSym, Name), Separator) = QualName :-
    ModuleName = sym_name_to_string_sep(ModuleSym, Separator),
    string.append_list([ModuleName, Separator, Name], QualName).

sym_name_to_string(SymName) = sym_name_to_string_sep(SymName, ".").

sym_name_to_list(unqualified(Name)) = [Name].
sym_name_to_list(qualified(Module, Name))
    = sym_name_to_list(Module) ++ [Name].

sym_name_to_qualifier_list_and_name(unqualified(Name), [], Name).
sym_name_to_qualifier_list_and_name(qualified(Module, Name),
        Qualifiers, Name) :-
    Qualifiers = sym_name_to_list(Module).

unqualify_name(unqualified(Name)) = Name.
unqualify_name(qualified(_ModuleName, Name)) = Name.

sym_name_get_module_name(unqualified(_), _) :- fail.
sym_name_get_module_name(qualified(ModuleName, _), ModuleName).

sym_name_get_module_name_default(SymName, DefaultModuleName, ModuleName) :-
    (
        SymName = unqualified(_),
        ModuleName = DefaultModuleName
    ;
        SymName = qualified(ModuleName, _)
    ).

sym_name_get_module_name_default_name(SymName, DefaultModuleName, ModuleName,
        Name) :-
    (
        SymName = unqualified(Name),
        ModuleName = DefaultModuleName
    ;
        SymName = qualified(ModuleName, Name)
    ).

match_sym_name(qualified(Module1, Name), qualified(Module2, Name)) :-
    match_sym_name(Module1, Module2).
match_sym_name(unqualified(Name), unqualified(Name)).
match_sym_name(unqualified(Name), qualified(_, Name)).

remove_sym_name_prefix(SymName0, Prefix, SymName) :-
    (
        SymName0 = unqualified(Name0),
        string.append(Prefix, Name, Name0),
        SymName = unqualified(Name)
    ;
        SymName0 = qualified(Module, Name0),
        string.append(Prefix, Name, Name0),
        SymName = qualified(Module, Name)
    ).

remove_sym_name_suffix(SymName0, Suffix, SymName) :-
    (
        SymName0 = unqualified(Name0),
        string.remove_suffix(Name0, Suffix, Name),
        SymName = unqualified(Name)
    ;
        SymName0 = qualified(Module, Name0),
        string.remove_suffix(Name0, Suffix, Name),
        SymName = qualified(Module, Name)
    ).

add_sym_name_suffix(SymName0, Suffix, SymName) :-
    (
        SymName0 = unqualified(Name0),
        string.append(Name0, Suffix, Name),
        SymName = unqualified(Name)
    ;
        SymName0 = qualified(Module, Name0),
        string.append(Name0, Suffix, Name),
        SymName = qualified(Module, Name)
    ).

transform_sym_base_name(TransformFunc, SymName0) = SymName :-
    (
        SymName0 = unqualified(Name0),
        SymName = unqualified(TransformFunc(Name0))
    ;
        SymName0 = qualified(Module, Name0),
        SymName = qualified(Module, TransformFunc(Name0))
    ).

outermost_qualifier(SymName) = Name :-
    (
        SymName = unqualified(Name)
    ;
        SymName = qualified(ModuleSymName, _),
        Name = outermost_qualifier(ModuleSymName)
    ).

add_outermost_qualifier(ModuleName, SymName0) = SymName :-
    (
        SymName0 = unqualified(Name),
        SymName = qualified(unqualified(ModuleName), Name)
    ;
        SymName0 = qualified(ModuleSymName0, Name),
        ModuleSymName = add_outermost_qualifier(ModuleName, ModuleSymName0),
        SymName = qualified(ModuleSymName, Name)
    ).

strip_outermost_qualifier(SymName0, OuterModuleName, SymName) :-
    SymName0 = qualified(ModuleSymName0, Name),
    (
        ModuleSymName0 = unqualified(OuterModuleName),
        SymName = unqualified(Name)
    ;
        ModuleSymName0 = qualified(_, _),
        strip_outermost_qualifier(ModuleSymName0, OuterModuleName,
            ModuleSymName),
        SymName = qualified(ModuleSymName, Name)
    ).

%-----------------------------------------------------------------------------%

is_submodule(SymName, SymName).
is_submodule(qualified(SymNameA, _), SymNameB) :-
    is_submodule(SymNameA, SymNameB).

%-----------------------------------------------------------------------------%
:- end_module mdbcomp.sym_name.
%-----------------------------------------------------------------------------%
