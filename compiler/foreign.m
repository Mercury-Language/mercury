%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2011 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: foreign.m.
% Main authors: trd, dgj.
%
% This module defines predicates for interfacing with foreign languages.
% In particular, this module supports interfacing with languages
% other than the target of compilation.
%
% Parts of this code were originally written by dgj, and have since been moved
% here.
%
%-----------------------------------------------------------------------------%

:- module backend_libs.foreign.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type foreign_type_and_assertions
    --->    foreign_type_and_assertions(sym_name, foreign_type_assertions).
            % A type defined by a pragma foreign_type, and the assertions
            % on that foreign_type.

    % Given an arbitrary mercury type, get the exported_type representation
    % of that type on the current backend.
    %
:- func is_this_a_foreign_type(module_info, mer_type)
    = maybe(foreign_type_and_assertions).

    % Given a type, determine the string which corresponds to that type
    % in the specified foreign language, for use with foreign language
    % interfacing (`pragma export' or `pragma foreign_proc').
    %
:- func exported_type_to_string(module_info, foreign_language, mer_type)
    = string.
:- func exported_type_to_c_string(module_info, mer_type) = string.

:- func maybe_foreign_type_to_string(foreign_language, mer_type,
    maybe(foreign_type_and_assertions)) = string.
:- func maybe_foreign_type_to_c_string(mer_type,
    maybe(foreign_type_and_assertions)) = string.
:- func foreign_type_to_c_string(foreign_type_and_assertions) = string.
:- func maybe_foreign_type_to_csharp_string(mer_type,
    maybe(foreign_type_and_assertions)) = string.
:- func maybe_foreign_type_to_java_string(mer_type,
    maybe(foreign_type_and_assertions)) = string.

:- func exported_builtin_type_to_c_string(builtin_type) = string.
:- func exported_builtin_type_to_csharp_string(builtin_type) = string.
:- func exported_builtin_type_to_java_string(builtin_type) = string.

%-----------------------------------------------------------------------------%

    % Find the current target backend from the module_info, and given
    % a foreign_type_body, return the name of the foreign language type
    % the identity of any user-defined unify/compare predicates, and the
    % assertions applicable to that backend.
    %
:- pred foreign_type_body_to_exported_type(module_info::in,
    foreign_type_body::in, sym_name::out, maybe_canonical::out,
    foreign_type_assertions::out) is det.

    % Does the foreign_type_body contain a definition usable
    % when compiling to the given target.
    %
:- pred have_foreign_type_for_backend(compilation_target::in,
    foreign_type_body::in, bool::out) is det.

    % Does the implementation of the given foreign type body on
    % the current backend use a user-defined comparison predicate.
    %
:- pred foreign_type_body_has_user_defined_eq_comp_pred(module_info::in,
    foreign_type_body::in, noncanonical::out) is semidet.

%-----------------------------------------------------------------------------%

    % Filter the decls for the given foreign language.
    % The first return value is the list of matches, the second is
    % the list of mis-matches.
    %
:- pred filter_decls(foreign_language::in, list(foreign_decl_code)::in,
    list(foreign_decl_code)::out, list(foreign_decl_code)::out) is det.

    % Filter the bodys for the given foreign language.
    % The first return value is the list of matches, the second is
    % the list of mis-matches.
    %
:- pred filter_bodys(foreign_language::in, list(foreign_body_code)::in,
    list(foreign_body_code)::out, list(foreign_body_code)::out) is det.

    % Filter the foreign exports for the given foreign language.
    % The first return value is the list of matches, the second is
    % the list of mis-matches.
    %
:- pred filter_exports(foreign_language::in,
    list(pragma_exported_proc)::in,
    list(pragma_exported_proc)::out, list(pragma_exported_proc)::out)
    is det.

%-----------------------------------------------------------------------------%

    % foreign_code_to_identifiers(Lang, Code, Identifiers):
    %
    % Break up Code into words that meet the rules for identifiers.
    % Some of these may actually be language keywords.
    %
:- pred foreign_code_to_identifiers(foreign_language::in, string::in,
    list(string)::out) is det.

%-----------------------------------------------------------------------------%

    % The name of the #define which can be used to guard declarations with
    % to prevent entities being declared twice.
    %
:- func decl_guard(sym_name) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.

:- import_module char.
:- import_module cord.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

is_this_a_foreign_type(ModuleInfo, Type) = MaybeForeignTypeAssertions :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    ( if
        type_to_ctor(Type, TypeCtor),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn)
    then
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_foreign_type(ForeignTypeBody),
            foreign_type_body_to_exported_type(ModuleInfo, ForeignTypeBody,
                ForeignTypeName, _, Assertions),
            MaybeForeignTypeAssertions =
                yes(foreign_type_and_assertions(ForeignTypeName, Assertions))
        ;
            ( TypeBody = hlds_du_type(_)
            ; TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_solver_type(_)
            ; TypeBody = hlds_abstract_type(_)
            ),
            MaybeForeignTypeAssertions = no
        )
    else
        MaybeForeignTypeAssertions = no
    ).

exported_type_to_string(ModuleInfo, Lang, Type) = String :-
    MaybeForeignType = is_this_a_foreign_type(ModuleInfo, Type),
    String = maybe_foreign_type_to_string(Lang, Type, MaybeForeignType).

exported_type_to_c_string(ModuleInfo, Type) = String :-
    MaybeForeignType = is_this_a_foreign_type(ModuleInfo, Type),
    String = maybe_foreign_type_to_c_string(Type, MaybeForeignType).

maybe_foreign_type_to_string(Lang, Type, MaybeForeignType) = String :-
    (
        Lang = lang_c,
        String = maybe_foreign_type_to_c_string(Type, MaybeForeignType)
    ;
        Lang = lang_csharp,
        String = maybe_foreign_type_to_csharp_string(Type, MaybeForeignType)
    ;
        Lang = lang_java,
        String = maybe_foreign_type_to_java_string(Type, MaybeForeignType)
    ).

maybe_foreign_type_to_c_string(Type, MaybeForeignType) = String :-
    (
        MaybeForeignType = yes(ForeignTypeAndAssertions),
        String = foreign_type_to_c_string(ForeignTypeAndAssertions)
    ;
        MaybeForeignType = no,
        % With --high-level-code, the value we return here should agree
        % with what happens is generated (indirectly) through
        % mercury_type_to_mlds_type.
        %
        % XXX I don't think this is yet true in all cases. -zs
        %
        % It is possible that in some cases, the right type name may depend
        % on whether --high-level-code is set.
        (
            Type = builtin_type(BuiltinType),
            String = exported_builtin_type_to_c_string(BuiltinType)
        ;
            Type = tuple_type(_, _),
            String = "MR_Tuple"
        ;
            % XXX Is MR_Word the right thing for any of these kinds of
            % types for high level code, with or without high level data?
            ( Type = defined_type(_, _, _)
            ; Type = higher_order_type(_, _, _, _)
            ; Type = apply_n_type(_, _, _)
            ),
            String = "MR_Word"
        ;
            Type = type_variable(_, _),
            String = "MR_Word"
        ;
            Type = kinded_type(_, _),
            unexpected($pred, "kinded type")
        )
    ).

foreign_type_to_c_string(ForeignTypeAndAssertions) = String :-
    ForeignTypeAndAssertions = foreign_type_and_assertions(ForeignType, _),
    (
        ForeignType = unqualified(String)
    ;
        ForeignType = qualified(_, _),
        unexpected($pred, "qualified C type")
    ).

maybe_foreign_type_to_csharp_string(Type, MaybeForeignType) = String :-
    (
        MaybeForeignType = yes(foreign_type_and_assertions(ForeignType, _)),
        String = sym_name_to_string(ForeignType)
    ;
        MaybeForeignType = no,
        (
            Type = builtin_type(BuiltinType),
            String = exported_builtin_type_to_csharp_string(BuiltinType)
        ;
            ( Type = tuple_type(_, _)
            ; Type = defined_type(_, _, _)
            ; Type = higher_order_type(_, _, _, _)
            ; Type = apply_n_type(_, _, _)
            ; Type = type_variable(_, _)
            ; Type = kinded_type(_, _)
            ),
            % This is here so we can share some code between C/C#/Java
            % backends. This is not the correct type to use in general.
            String = "object"
        )
    ).

maybe_foreign_type_to_java_string(Type, MaybeForeignType) = String :-
    (
        MaybeForeignType = yes(foreign_type_and_assertions(ForeignType, _)),
        String = sym_name_to_string(ForeignType)
    ;
        MaybeForeignType = no,
        (
            Type = builtin_type(BuiltinType),
            String = exported_builtin_type_to_java_string(BuiltinType)
        ;
            ( Type = tuple_type(_, _)
            ; Type = defined_type(_, _, _)
            ; Type = higher_order_type(_, _, _, _)
            ; Type = apply_n_type(_, _, _)
            ; Type = type_variable(_, _)
            ; Type = kinded_type(_, _)
            ),
            % This is here so we can share some code between C/C#/Java
            % backends. This is not the correct type to use in general.
            String = "java.lang.Object"
        )
    ).

exported_builtin_type_to_c_string(BuiltinType) = CTypeName :-
    (
        BuiltinType = builtin_type_int(IntType),
        (
            IntType = int_type_int,
            CTypeName = "MR_Integer"
        ;
            IntType = int_type_uint,
            CTypeName = "MR_Unsigned"
        ;
            IntType = int_type_int8,
            CTypeName = "int8_t"
        ;
            IntType = int_type_uint8,
            CTypeName = "uint8_t"
        ;
            IntType = int_type_int16,
            CTypeName = "int16_t"
        ;
            IntType = int_type_uint16,
            CTypeName = "uint16_t"
        ;
            IntType = int_type_int32,
            CTypeName = "int32_t"
        ;
            IntType = int_type_uint32,
            CTypeName = "uint32_t"
        ;
            IntType = int_type_int64,
            CTypeName = "int64_t"
        ;
            IntType = int_type_uint64,
            CTypeName = "uint64_t"
        )
    ;
        BuiltinType = builtin_type_float,
        CTypeName = "MR_Float"
    ;
        BuiltinType = builtin_type_string,
        CTypeName = "MR_String"
    ;
        BuiltinType = builtin_type_char,
        CTypeName = "MR_Char"
    ).

exported_builtin_type_to_csharp_string(BuiltinType) = CsharpTypeName :-
    (
        BuiltinType = builtin_type_int(IntType),
        (
            IntType = int_type_int,
            CsharpTypeName = "int"
        ;
            IntType = int_type_uint,
            CsharpTypeName = "uint"
        ;
            IntType = int_type_int8,
            CsharpTypeName = "sbyte"
        ;
            IntType = int_type_uint8,
            CsharpTypeName = "byte"
        ;
            IntType = int_type_int16,
            CsharpTypeName = "short"
        ;
            IntType = int_type_uint16,
            CsharpTypeName = "ushort"
        ;
            IntType = int_type_int32,
            CsharpTypeName = "int"
        ;
            IntType = int_type_uint32,
            CsharpTypeName = "uint"
        ;
            IntType = int_type_int64,
            CsharpTypeName = "long"
        ;
            IntType = int_type_uint64,
            CsharpTypeName = "ulong"
        )
    ;
        BuiltinType = builtin_type_float,
        CsharpTypeName = "double"
    ;
        BuiltinType = builtin_type_string,
        CsharpTypeName = "string"
    ;
        BuiltinType = builtin_type_char,
        CsharpTypeName = "char"
    ).

exported_builtin_type_to_java_string(BuiltinType) = JavaTypeName :-
    (
        BuiltinType = builtin_type_int(IntType),
        (
            IntType = int_type_int,
            JavaTypeName = "int"
        ;
            IntType = int_type_uint,
            JavaTypeName = "int"
        ;
            IntType= int_type_int8,
            JavaTypeName = "byte"
        ;
            IntType = int_type_uint8,
            JavaTypeName = "byte"
        ;
            IntType = int_type_int16,
            JavaTypeName = "short"
        ;
            IntType = int_type_uint16,
            JavaTypeName = "short"
        ;
            IntType = int_type_int32,
            JavaTypeName = "int"
        ;
            IntType = int_type_uint32,
            JavaTypeName = "int"
        ;
            IntType = int_type_int64,
            JavaTypeName = "long"
        ;
            IntType = int_type_uint64,
            JavaTypeName = "long"
        )
    ;
        BuiltinType = builtin_type_float,
        JavaTypeName = "double"
    ;
        BuiltinType = builtin_type_string,
        JavaTypeName = "java.lang.String"
    ;
        BuiltinType = builtin_type_char,
        JavaTypeName = "char"
    ).

%-----------------------------------------------------------------------------%

foreign_type_body_to_exported_type(ModuleInfo, ForeignTypeBody, Name,
        MaybeUserEqComp, Assertions) :-
    % The body of this function is very similar to the function
    % foreign_type_to_mlds_type in mlds.m.
    % Any changes here may require changes there as well.

    ForeignTypeBody = foreign_type_body(MaybeC, MaybeJava, MaybeCSharp),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        (
            MaybeC = yes(Data),
            Data = type_details_foreign(c_type(NameStr), MaybeUserEqComp,
                Assertions),
            Name = unqualified(NameStr)
        ;
            MaybeC = no,
            unexpected($pred, "no C type")
        )
    ;
        Target = target_csharp,
        (
            MaybeCSharp = yes(Data),
            Data = type_details_foreign(csharp_type(NameStr),
                MaybeUserEqComp, Assertions),
            Name = unqualified(NameStr)
        ;
            MaybeCSharp = no,
            unexpected($pred, "no C# type")
        )
    ;
        Target = target_java,
        (
            MaybeJava = yes(Data),
            Data = type_details_foreign(java_type(NameStr), MaybeUserEqComp,
                Assertions),
            Name = unqualified(NameStr)
        ;
            MaybeJava = no,
            unexpected($pred, "no Java type")
        )
    ).

have_foreign_type_for_backend(Target, ForeignTypeBody, Have) :-
    (
        Target = target_c,
        Have = ( if ForeignTypeBody ^ c = yes(_) then yes else no )
    ;
        Target = target_java,
        Have = ( if ForeignTypeBody ^ java = yes(_) then yes else no )
    ;
        Target = target_csharp,
        Have = ( if ForeignTypeBody ^ csharp = yes(_) then yes else no )
    ).

foreign_type_body_has_user_defined_eq_comp_pred(ModuleInfo, Body,
        NonCanonical) :-
    foreign_type_body_to_exported_type(ModuleInfo, Body, _,
        MaybeCanonical, _),
    MaybeCanonical = noncanon(NonCanonical).

%-----------------------------------------------------------------------------%

filter_decls(WantedLang, Decls0, LangDecls, NotLangDecls) :-
    IsWanted =
        ( pred(foreign_decl_code(Lang, _, _, _)::in) is semidet :-
            WantedLang = Lang
        ),
    list.filter(IsWanted, Decls0, LangDecls, NotLangDecls).

filter_bodys(WantedLang, Bodys0, LangBodys, NotLangBodys) :-
    IsWanted =
        ( pred(foreign_body_code(Lang, _, _)::in) is semidet :-
            WantedLang = Lang
        ),
    list.filter(IsWanted, Bodys0, LangBodys, NotLangBodys).

filter_exports(WantedLang, Exports0, LangExports, NotLangExports) :-
    IsWanted =
        ( pred(pragma_exported_proc(Lang, _, _, _, _)::in) is semidet :-
            WantedLang = Lang
        ),
    list.filter(IsWanted, Exports0, LangExports, NotLangExports).

%-----------------------------------------------------------------------------%

foreign_code_to_identifiers(Lang, Code, Identifiers) :-
    string.to_char_list(Code, Chars),
    % This one arm switch ensures that we will get a warning
    % if and when we add another target language (which may have
    % different rules for what is a comment).
    (
        ( Lang = lang_c
        ; Lang = lang_java
        ; Lang = lang_csharp
        ),
        % We use cords to ensure tail recursion in the loop over Code,
        % because it may be big. We do not do try to ensure tail recursion
        % for loops over identifiers, since (for cultural reasons)
        % they will effectively never be long enough for this to be a problem.
        foreign_code_to_c_j_cs_identifiers_loop(Chars,
            cord.init, IdentifierCord),
        Identifiers = cord.list(IdentifierCord)
    ).

:- pred foreign_code_to_c_j_cs_identifiers_loop(list(char)::in,
    cord(string)::in, cord(string)::out) is det.

foreign_code_to_c_j_cs_identifiers_loop(Chars0, !IdentifierCord) :-
    get_next_c_j_cs_identifier(Chars0, IdentifierChars, Chars1),
    (
        IdentifierChars = []
        % There are no identifiers left.
    ;
        IdentifierChars = [_ | _],
        string.from_char_list(IdentifierChars, Identifier),
        cord.snoc(Identifier, !IdentifierCord),
        foreign_code_to_c_j_cs_identifiers_loop(Chars1, !IdentifierCord)
    ).

:- pred get_next_c_j_cs_identifier(list(char)::in,
    list(char)::out, list(char)::out) is det.

get_next_c_j_cs_identifier([], [], []).
get_next_c_j_cs_identifier([Char0 | Chars0], IdentifierChars, LeftOverChars) :-
    ( if char.is_alnum_or_underscore(Char0) then
        get_rest_of_identifier(Chars0, TailIdentifierChars, LeftOverChars),
        IdentifierChars = [Char0 | TailIdentifierChars]
    else if Char0 = ('/') then
        (
            Chars0 = [],
            IdentifierChars = [],
            LeftOverChars = []
        ;
            Chars0 = [Char1 | Chars1],
            ( if Char1 = ('/') then
                ignore_rest_of_line(Chars1, Chars2),
                get_next_c_j_cs_identifier(Chars2,
                    IdentifierChars, LeftOverChars)
            else if Char1 = ('*') then
                ignore_rest_of_slash_star_comment(Chars1, Chars2),
                get_next_c_j_cs_identifier(Chars2,
                    IdentifierChars, LeftOverChars)
            else
                % Ignore Char0, since it does not start a comment, and
                % cannot be part of an identifier.
                get_next_c_j_cs_identifier(Chars0,
                    IdentifierChars, LeftOverChars)
            )
        )
    else
        % Ignore Char0, since it does not start a comment, and
        % cannot be part of an identifier.
        get_next_c_j_cs_identifier(Chars0, IdentifierChars, LeftOverChars)
    ).

:- pred ignore_rest_of_slash_star_comment(list(char)::in,
    list(char)::out) is det.

ignore_rest_of_slash_star_comment([], []).
ignore_rest_of_slash_star_comment([Char0 | Chars0], LeftOverChars) :-
    ( if
        Char0 = ('*'),
        Chars0 = [Char1 | Chars1],
        Char1 = ('/')
    then
        LeftOverChars = Chars1
    else
        ignore_rest_of_slash_star_comment(Chars0, LeftOverChars)
    ).

:- pred ignore_rest_of_line(list(char)::in,
    list(char)::out) is det.

ignore_rest_of_line([], []).
ignore_rest_of_line([Char0 | Chars0], LeftOverChars) :-
    ( if Char0 = ('\n') then
        LeftOverChars = Chars0
    else
        ignore_rest_of_line(Chars0, LeftOverChars)
    ).

:- pred get_rest_of_identifier(list(char)::in, list(char)::out,
    list(char)::out) is det.

get_rest_of_identifier([], [], []).
get_rest_of_identifier([Char0 | Chars0], IdentifierChars, LeftOverChars) :-
    ( if char.is_alnum_or_underscore(Char0) then
        % There may be more characters in the identifier.
        get_rest_of_identifier(Chars0, TailIdentifierChars, LeftOverChars),
        IdentifierChars = [Char0 | TailIdentifierChars]
    else
        % The word is finished.
        IdentifierChars = [],
        LeftOverChars = Chars0
    ).

%-----------------------------------------------------------------------------%

decl_guard(ModuleName) = UppercaseModuleName ++ "_DECL_GUARD" :-
    MangledModuleName = sym_name_mangle(ModuleName),
    string.to_upper(MangledModuleName, UppercaseModuleName).

%-----------------------------------------------------------------------------%
:- end_module backend_libs.foreign.
%-----------------------------------------------------------------------------%
