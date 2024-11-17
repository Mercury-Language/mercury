%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: warn_unread_modules.m.
%
%---------------------------------------------------------------------------%

:- module parse_tree.warn_unread_modules.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.

:- import_module list.
:- import_module set_tree234.

:- pred warn_about_any_unread_modules_with_read_ancestors(
    set_tree234(module_name)::in, set_tree234(module_name)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

warn_about_any_unread_modules_with_read_ancestors(ReadModules, UnreadModules0,
        !Specs) :-
    % When module mod_a.mod_b is nested inside mod_a.m, the source file
    % containing module mod_a, then it is possible for an attempt to read
    % mod_a.mod_b.m to fail (since module mod_a.mod_b is not there),
    % but for the module to be later found by reading mod_a.m.
    % This would result in mod_a.mod_b being included in both
    % ReadModules and UnreadModules0.
    set_tree234.difference(UnreadModules0, ReadModules, UnreadModules),
    set_tree234.to_sorted_list(UnreadModules, UnreadModuleList),
    find_read_ancestors_of_unread_modules(ReadModules, UnreadModuleList,
        set_tree234.init, Parents, set_tree234.init, Ancestors, 
        set_tree234.init, BadUnreads),
    set_tree234.to_sorted_list(BadUnreads, BadUnreadList),
    (
        BadUnreadList = []
    ;
        BadUnreadList = [_ | _],
        BadUnreadModulePieces = list.map(wrap_module_name, BadUnreadList),
        list.intersperse(nl, BadUnreadModulePieces, BadUnreadModuleListPieces),
        TheModules = choose_number(BadUnreadList, "the module", "the modules"),
        BadUnreadPieces =
            [words(TheModules), nl_indent_delta(1), blank_line] ++
            BadUnreadModuleListPieces ++ [nl_indent_delta(-1), blank_line],

        set_tree234.to_sorted_list(Parents, ParentList),
        set_tree234.to_sorted_list(Ancestors, AncestorList),
        ParentModulePieces = list.map(wrap_module_name, ParentList),
        AncestorModulePieces = list.map(wrap_module_name, AncestorList),
        (
            ParentModulePieces = [],
            ParentPieces = []
        ;
            ParentModulePieces = [_ | TailParentModulePieces],
            (
                TailParentModulePieces = [],
                ParentWords = "a parent module,"
            ;
                TailParentModulePieces = [_ | _],
                ParentWords = "parent modules,"
            ),
            list.intersperse(nl, ParentModulePieces, ParentModuleListPieces),
            ParentPieces =
                [words(ParentWords), words("specifically"),
                    nl_indent_delta(1), blank_line] ++
                ParentModuleListPieces ++ [nl_indent_delta(-1), blank_line]
        ),
        (
            AncestorModulePieces = [],
            AncestorPieces = []
        ;
            AncestorModulePieces = [_ | TailAncestorModulePieces],
            (
                TailAncestorModulePieces = [],
                AncestorWords = "an ancestor module,"
            ;
                TailAncestorModulePieces = [_ | _],
                AncestorWords = "ancestor modules,"
            ),
            list.intersperse(nl, AncestorModulePieces,
                AncestorModuleListPieces),
            AncestorPieces0 =
                [words(AncestorWords), words("specifically"),
                    nl_indent_delta(1), blank_line] ++
                AncestorModuleListPieces ++ [nl_indent_delta(-1), blank_line],
            (
                ParentModulePieces = [],
                AncestorPieces = AncestorPieces0
            ;
                ParentModulePieces = [_ | _],
                AncestorPieces = [words("and") | AncestorPieces0]
            )
        ),
        Pieces =
            [words("Warning:")] ++ BadUnreadPieces ++
            [words("which the compiler could not find"),
            words("in the current directory,"), words("have")] ++
            ParentPieces ++ AncestorPieces ++
            [words("which the compiler *did* find in the current directory."),
            blank_line,
            words("This usually indicates that the Mercury.modules file,"),
            words("which contains the module name to source file name map,"),
            words("is either missing or out-of-date."),
            words("You need to rebuild it."),
            words("This is usually done using a command such as"),
            quote("mmc -f *.m"), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_warning, phase_read_files, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- func wrap_module_name(module_name) = format_piece.

wrap_module_name(ModuleName) = fixed(sym_name_to_string(ModuleName)).

:- pred find_read_ancestors_of_unread_modules(set_tree234(module_name)::in,
    list(module_name)::in,
    set_tree234(module_name)::in, set_tree234(module_name)::out,
    set_tree234(module_name)::in, set_tree234(module_name)::out,
    set_tree234(module_name)::in, set_tree234(module_name)::out) is det.

find_read_ancestors_of_unread_modules(_ReadModules, [],
        !Parents, !Ancestors, !BadUnreads).
find_read_ancestors_of_unread_modules(ReadModules,
        [UnreadModule | UnreadModules],
        !Parents, !Ancestors, !BadUnreads) :-
    (
        UnreadModule = unqualified(_)
    ;
        UnreadModule = qualified(ParentModule, _),
        ( if
            find_first_read_ancestor(ReadModules, ParentModule, parent,
                AncestorModule, PoA)
        then
            set_tree234.insert(UnreadModule, !BadUnreads),
            (
                PoA = parent,
                set_tree234.insert(AncestorModule, !Parents)
            ;
                PoA = ancestor,
                set_tree234.insert(AncestorModule, !Ancestors)
            )
        else
            true
        )
    ),
    find_read_ancestors_of_unread_modules(ReadModules, UnreadModules,
        !Parents, !Ancestors, !BadUnreads).

:- type parent_or_ancestor
    --->    parent
    ;       ancestor.

:- pred find_first_read_ancestor(set_tree234(module_name)::in,
    module_name::in, parent_or_ancestor::in,
    module_name::out, parent_or_ancestor::out) is semidet.

find_first_read_ancestor(ReadModules, Module, CurPoA, AncestorModule, PoA) :-
    ( if set_tree234.contains(ReadModules, Module) then
        AncestorModule = Module,
        PoA = CurPoA
    else
        Module = qualified(ParentModule, _),
        find_first_read_ancestor(ReadModules, ParentModule, ancestor,
            AncestorModule, PoA)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.warn_unread_modules.
%---------------------------------------------------------------------------%
