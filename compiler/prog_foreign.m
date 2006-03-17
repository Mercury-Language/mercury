%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module defines predicate for interfacing with foreign languages.
% that are necessary for the frontend of the compiler to construct
% the list of items.  The predicates in this module should not depend
% on the HLDS in any way.  The predicates for interfacing with foreign
% languages that do depend on the HLDS are defined in foreign.m.
%
% This module also contains the parts of the name mangler that are used
% by the frontend of the compiler.
%
% Warning: any changes to the name mangling algorithms implemented in this
% module may also require changes to extras/dynamic_linking/name_mangle.m,
% profiler/demangle.m, util/mdemangle.c and compiler/name_mangle.m.
%
% Main authors: trd, dgj.
% This code was originally part of the foreign module and was moved here.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_foreign.
:- interface.

:- import_module libs.globals.
:- import_module parse_tree.prog_data.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module list.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type foreign_decl_info == list(foreign_decl_code).
                                % in reverse order
:- type foreign_body_info == list(foreign_body_code).
                                % in reverse order

:- type foreign_decl_code
    --->    foreign_decl_code(
                fdecl_lang          :: foreign_language,
                fdecl_is_local      :: foreign_decl_is_local,
                fdecl_code          :: string,
                fdecl_context       :: prog_context
            ).

:- type foreign_body_code
    --->    foreign_body_code(
                fbody_lang          :: foreign_language,
                fbody_code          :: string,
                fbody_context       :: prog_context
            ).

:- type foreign_export_defns == list(foreign_export).
:- type foreign_export_decls
    --->    foreign_export_decls(
                fexp_decls_info     :: foreign_decl_info,
                fexp_decls_list     :: list(foreign_export_decl)
            ).

:- type foreign_export_decl
    --->    foreign_export_decl(
                fexp_decl_lang      :: foreign_language,
                                    % Language of the export.

                fexp_decl_ret_type  :: string,
                                    % Return type.

                fexp_decl_func_name :: string,
                                    % Function name.

                fexp_decl_arg_decls :: string
                                    % Argument declarations.
            ).

    % Some code from a `pragma foreign_code' declaration that is not
    % associated with a given procedure.
    %
:- type user_foreign_code
    --->    user_foreign_code(
                foreign_language,   % language of this code
                string,             % code
                term.context        % source code location
            ).

    % The code for `pragma export' is generated directly as strings
    % by export.m.
    %
:- type foreign_export  ==  string.

%-----------------------------------------------------------------------------%

    % foreign_import_module_name(ForeignImport)
    %
    % returns the module name which represents the ForeignImport.
    %
    % For instance for the foreign_import_module representing
    %   :- foreign_import_module("MC++", module)
    % would return the module_name
    %   unqualified("module__cpp_code")
    %
:- func foreign_import_module_name(foreign_import_module) = module_name.

    % foreign_import_module_name(ForeignImport, CurrentModule)
    %
    % returns the module name needed to refer to ForeignImport from the
    % CurrentModule.
    %
:- func foreign_import_module_name(foreign_import_module, module_name) =
    module_name.

    % Sub-type of foreign_language for languages for which
    % we generate external files for foreign code.
    %
:- inst lang_gen_ext_file
    --->    c
    ;       managed_cplusplus
    ;       csharp.

    % The module name used for this foreign language.
    % Not all foreign languages generate external modules
    % so this function only succeeds for those that do.
    %
:- func foreign_language_module_name(module_name, foreign_language) =
    module_name.
:- mode foreign_language_module_name(in, in) = out is semidet.
:- mode foreign_language_module_name(in, in(lang_gen_ext_file)) = out is det.

    % The file extension used for this foreign language (including the dot).
    % Not all foreign languages generate external files,
    % so this function only succeeds for those that do.
    %
:- func foreign_language_file_extension(foreign_language) = string.
:- mode foreign_language_file_extension(in) = out is semidet.
:- mode foreign_language_file_extension(in(lang_gen_ext_file)) = out is det.

    % It is possible that more than one foreign language could be used to
    % implement a particular piece of code.
    % Therefore, foreign languages have an order of preference, from most
    % preferred to least perferred.
    % prefer_foreign_language(Globals, Target, Lang1, Lang2) returns the
    % yes if Lang2 is preferred over Lang1.
    %
    % Otherwise it will return no.
    %
:- func prefer_foreign_language(globals, compilation_target,
    foreign_language, foreign_language) = bool.

    % The `multi' mode returns all supported foreign languages.
    %
:- pred foreign_language(foreign_language).
:- mode foreign_language(in) is det.
:- mode foreign_language(out) is multi.

:- func foreign_type_language(foreign_language_type) = foreign_language.

%-----------------------------------------------------------------------------%
%
% The following are the parts of the name mangler that are needed by
% the compiler frontend so that it can write out makefile fragments.

    % Returns the name of the initialization function for a given module.
    %
:- func make_init_name(module_name) = string.

    % Mangle a possibly module-qualified Mercury symbol name
    % into a C identifier.
    %
:- func sym_name_mangle(sym_name) = string.

    % Mangle an arbitrary name into a C etc identifier.
    %
:- func name_mangle(string) = string.

    % Produces a string of the form Module__Name.
    %
:- func qualify_name(string, string) = string.

:- func convert_to_valid_c_identifier(string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.modules.

:- import_module char.
:- import_module int.

%-----------------------------------------------------------------------------%

foreign_import_module_name(ImportModule) = ModuleName :-
    ImportModule = foreign_import_module(Lang, ForeignImportModule, _),
    (
        Lang = c,
        ModuleName = ForeignImportModule
    ;
        Lang = il,
        ModuleName = ForeignImportModule
    ;
        Lang = java,
        ModuleName = ForeignImportModule
    ;
        Lang = managed_cplusplus,
        ModuleName = foreign_language_module_name(ForeignImportModule,
                Lang)
    ;
        Lang = csharp,
        ModuleName = foreign_language_module_name(ForeignImportModule, Lang)
    ).

foreign_import_module_name(ModuleForeignImported, CurrentModule) =
        ImportedForeignCodeModuleName :-
    ModuleForeignImported = foreign_import_module(Lang, _, _),
    ImportedForeignCodeModuleName1 = ModuleForeignImported ^
        foreign_import_module_name,
    (
        Lang = c,
        ImportedForeignCodeModuleName = ImportedForeignCodeModuleName1
    ;
        Lang = il,
        ImportedForeignCodeModuleName = handle_std_library(
            CurrentModule, ImportedForeignCodeModuleName1)
    ;
        Lang = managed_cplusplus,
        ImportedForeignCodeModuleName = handle_std_library(
            CurrentModule, ImportedForeignCodeModuleName1)
    ;
        Lang = csharp,
        ImportedForeignCodeModuleName = handle_std_library(
            CurrentModule, ImportedForeignCodeModuleName1)
    ;
        Lang = java,
        ImportedForeignCodeModuleName = handle_std_library(
            CurrentModule, ImportedForeignCodeModuleName1)
    ).

    % On the il backend, we need to refer to the module "mercury" when
    % referencing a std library module when we are not actually building
    % the std library.
    %
:- func handle_std_library(module_name, module_name) = module_name.

handle_std_library(CurrentModule, ModuleName0) = ModuleName :-
    (
        mercury_std_library_module_name(ModuleName0),
        \+ mercury_std_library_module_name(CurrentModule)
    ->
        ModuleName = unqualified("mercury")
    ;
        ModuleName = ModuleName0
    ).

%-----------------------------------------------------------------------------%

foreign_language_module_name(ModuleName, Lang) = FullyQualifiedModuleName :-
    % Only succeed if this language generates external files.
    _ = foreign_language_file_extension(Lang),

    Ending = "__" ++ simple_foreign_language_string(Lang) ++ "_code",
    (
        ModuleName = unqualified(Name),
        FullyQualifiedModuleName = unqualified(Name ++ Ending)
    ;
        ModuleName = qualified(Module, Name),
        FullyQualifiedModuleName = qualified(Module, Name ++ Ending)
    ).

%-----------------------------------------------------------------------------%

foreign_language_file_extension(c) = ".c".
foreign_language_file_extension(managed_cplusplus) = ".cpp".
foreign_language_file_extension(csharp) = ".cs".
foreign_language_file_extension(java) = ".java".
foreign_language_file_extension(il) = _ :-
    fail.

%-----------------------------------------------------------------------------%

    % Currently we don't use the globals to compare foreign language
    % interfaces, but if we added appropriate options we might want
    % to do this later.
    %
prefer_foreign_language(_Globals, c, Lang1, Lang2) =
    % When compiling to C, C is always preferred over any other language.
    ( Lang2 = c, not Lang1 = c ->
        yes
    ;
        no
    ).

prefer_foreign_language(_Globals, asm, Lang1, Lang2) =
    % When compiling to asm, C is always preferred over any other language.
    ( Lang2 = c, not Lang1 = c ->
        yes
    ;
        no
    ).

prefer_foreign_language(_Globals, il, Lang1, Lang2) = Comp :-
    % Whe compiling to il, first we prefer il, then csharp, then
    % managed_cplusplus, after that we don't care.
    PreferredList = [il, csharp, managed_cplusplus],

    FindLangPriority = (func(L) = X :-
        ( list.nth_member_search(PreferredList, L, X0) ->
            X = X0
        ;
            X = list.length(PreferredList) + 1
        )),
    N1 = FindLangPriority(Lang1),
    N2 = FindLangPriority(Lang2),
    ( N2 < N1 ->
        Comp = yes
    ;
        Comp = no
    ).

prefer_foreign_language(_Globals, java, _Lang1, _Lang2) = no.
    % Nothing useful to do here, but when we add Java as a foreign language,
    % we should add it here.

%-----------------------------------------------------------------------------%

foreign_language(c).
foreign_language(java).
foreign_language(csharp).
foreign_language(managed_cplusplus).
foreign_language(il).

%-----------------------------------------------------------------------------%

foreign_type_language(il(_)) = il.
foreign_type_language(c(_)) = c.
foreign_type_language(java(_)) = java.

%-----------------------------------------------------------------------------%

make_init_name(ModuleName) = InitName :-
    MangledModuleName = sym_name_mangle(ModuleName),
    InitName = "mercury__" ++ MangledModuleName ++ "__".

sym_name_mangle(unqualified(Name)) =
    name_mangle(Name).
sym_name_mangle(qualified(ModuleName, PlainName)) = MangledName :-
    MangledModuleName = sym_name_mangle(ModuleName),
    MangledPlainName = name_mangle(PlainName),
    MangledName = qualify_name(MangledModuleName, MangledPlainName).

name_mangle(Name) = MangledName :-
    % Warning: any changes to the name mangling algorithm here may also
    % require changes to extras/dynamic_linking/name_mangle.m,
    % profiler/demangle.m, util/mdemangle.c and compiler/name_mangle.m.

    ( string.is_alnum_or_underscore(Name) ->
        % Any names that start with `f_' are changed so that they start with
        % `f__', so that we can use names starting with `f_' (followed by
        % anything except an underscore) without fear of name collisions.

        ( string.append("f_", Suffix, Name) ->
            MangledName = "f__" ++ Suffix
        ;
            MangledName = Name
        )
    ;
        MangledName = convert_to_valid_c_identifier(Name)
    ).

qualify_name(Module0, Name0) = Name :-
    string.append_list([Module0, "__", Name0], Name).

convert_to_valid_c_identifier(String) = Name :-
    ( name_conversion_table(String, Name0) ->
        Name = Name0
    ;
        Name = "f" ++ convert_to_valid_c_identifier_2(String)
    ).

    % A table used to convert Mercury functors into C identifiers.
    % Feel free to add any new translations you want. The C identifiers
    % should start with "f_", to avoid introducing name clashes. If the functor
    % name is not found in the table, then we use a fall-back method which
    % produces ugly names.
    %
:- pred name_conversion_table(string::in, string::out) is semidet.

name_conversion_table("\\=", "f_not_equal").
name_conversion_table(">=", "f_greater_or_equal").
name_conversion_table("=<", "f_less_or_equal").
name_conversion_table("=", "f_equal").
name_conversion_table("<", "f_less_than").
name_conversion_table(">", "f_greater_than").
name_conversion_table("-", "f_minus").
name_conversion_table("+", "f_plus").
name_conversion_table("*", "f_times").
name_conversion_table("/", "f_slash").
name_conversion_table(",", "f_comma").
name_conversion_table(";", "f_semicolon").
name_conversion_table("!", "f_cut").
name_conversion_table("{}", "f_tuple").
name_conversion_table("[|]", "f_cons").
name_conversion_table("[]", "f_nil").

    % This is the fall-back method. Given a string, produce a C identifier
    % for that string by concatenating the decimal expansions of the character
    % codes in the string, separated by underlines. The C identifier will
    % start with "f_"; this predicate constructs everything except the initial
    % "f".
    %
    % For example, given the input "\n\t" we return "_10_8".
    %
:- func convert_to_valid_c_identifier_2(string) = string.

convert_to_valid_c_identifier_2(String) = Name :-
    ( string.first_char(String, Char, Rest) ->
        % XXX This will cause ABI incompatibilities between compilers which are
        % built in grades that have different character representations.
        char.to_int(Char, Code),
        string.int_to_string(Code, CodeString),
        string.append("_", CodeString, ThisCharString),
        Name0 = convert_to_valid_c_identifier_2(Rest),
        string.append(ThisCharString, Name0, Name)
    ;
        % String is the empty string
        Name = String
    ).

%-----------------------------------------------------------------------------%
:- end_module prog_foreign.
%-----------------------------------------------------------------------------%
