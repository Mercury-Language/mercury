%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2008, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main authors: trd, dgj.
%
% This module defines predicate for interfacing with foreign languages.
% that are necessary for the frontend of the compiler to construct
% the parse tree. The predicates in this module should not depend on the HLDS
% in any way. The predicates for interfacing with foreign languages that
% *do* depend on the HLDS are defined in foreign.m.
%
% This module also contains the parts of the name mangler that are used
% by the frontend of the compiler.
%
% Warning: any changes to the name mangling algorithms implemented in this
% module may also require changes to profiler/demangle.m, util/mdemangle.c and
% compiler/name_mangle.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_foreign.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module bool.
:- import_module cord.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type foreign_decl_codes == cord(foreign_decl_code).
:- type foreign_body_codes == cord(foreign_body_code).

:- type foreign_decl_code
    --->    foreign_decl_code(
                fdecl_lang          :: foreign_language,
                fdecl_is_local      :: foreign_decl_is_local,
                fdecl_code          :: foreign_literal_or_include,
                fdecl_context       :: prog_context
            ).

:- type foreign_body_code
    --->    foreign_body_code(
                fbody_lang          :: foreign_language,
                fbody_code          :: foreign_literal_or_include,
                fbody_context       :: prog_context
            ).

:- type foreign_export_defns == list(foreign_export_defn).

:- type foreign_export_defn
    --->    foreign_export_defn(
                % The code for `pragma foreign_export' is generated directly
                % as strings by export.m.
                string
            ).

:- type foreign_export_decls
    --->    foreign_export_decls(
                fexp_decls_codes    :: list(foreign_decl_code),
                fexp_decls_list     :: list(foreign_export_decl)
            ).

:- type foreign_export_decl
    --->    foreign_export_decl(
                % Language of the export.
                fexp_decl_lang      :: foreign_language,

                % Return type.
                fexp_decl_ret_type  :: string,

                % Function name.
                fexp_decl_func_name :: string,

                % Argument declarations.
                fexp_decl_arg_decls :: string
            ).

%-----------------------------------------------------------------------------%

:- pred foreign_decl_code_is_for_lang(foreign_language::in,
    foreign_decl_code::in) is semidet.
:- pred foreign_body_code_is_for_lang(foreign_language::in,
    foreign_body_code::in) is semidet.

    % foreign_import_module_name(ForeignImport)
    %
    % returns the module name which represents the ForeignImport.
    %
    % For instance for the foreign_import_module representing
    %   :- foreign_import_module("C#", module)
    % would return the module_name
    %   unqualified("module__csharp_code")
    %
:- func fim_spec_module_name(fim_spec) = module_name.

    % foreign_import_module_name_from_module(ForeignImport, CurrentModule)
    %
    % returns the module name needed to refer to ForeignImport from the
    % CurrentModule.
    %
:- func fim_spec_module_name_from_module(fim_spec, module_name) = module_name.

    % Sub-type of foreign_language for languages for which
    % we generate external files for foreign code.
    %
:- inst lang_gen_ext_file for globals.foreign_language/0
    --->    lang_c
    ;       lang_csharp.

    % The module name used for this foreign language.
    % Not all foreign languages generate external modules,
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
    % implement a particular piece of code. Therefore, foreign languages
    % have an order of preference, from most preferred to least perferred.
    % prefer_foreign_language(Globals, Target, Lang1, Lang2) returns
    % `yes' if Lang2 is preferred over Lang1; otherwise, it will return no.
    %
:- func prefer_foreign_language(globals, compilation_target,
    foreign_language, foreign_language) = bool.

    % Return all supported foreign languages.
    %
:- func all_foreign_languages = list(foreign_language).

:- func foreign_type_language(generic_language_foreign_type)
    = foreign_language.

%-----------------------------------------------------------------------------%
%
% The following are the parts of the name mangler that are needed by
% the compiler frontend so that it can write out Makefile fragments.

    % Returns the name of the initialization function for a given module.
    %
:- func make_init_name(module_name) = string.

    % Mangle a possibly module-qualified Mercury symbol name
    % into a C identifier.
    %
:- func sym_name_mangle(sym_name) = string.

    % Mangle an arbitrary name into a C etc identifier.
    % Initial digits are allowed.
    %
:- func name_mangle(string) = string.

    % Mangle an arbitrary name into a C etc identifier.
    % The resulting identifier will not begin with a digit.
    %
:- func name_mangle_no_leading_digit(string) = string.

    % Produces a string of the form Module__Name.
    %
:- func qualify_name(string, string) = string.

:- func convert_to_valid_c_identifier(string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.file_names.

:- import_module char.
:- import_module solutions.
:- import_module string.

%-----------------------------------------------------------------------------%

foreign_decl_code_is_for_lang(Lang, DeclCode) :-
    Lang = DeclCode ^ fdecl_lang.

foreign_body_code_is_for_lang(Lang, BodyCode) :-
    Lang = BodyCode ^ fbody_lang.

fim_spec_module_name(FIMSpec) = ModuleName :-
    FIMSpec = fim_spec(Lang, ForeignImportModule),
    (
        Lang = lang_c,
        ModuleName = ForeignImportModule
    ;
        Lang = lang_java,
        ModuleName = ForeignImportModule
    ;
        Lang = lang_erlang,
        ModuleName = ForeignImportModule
    ;
        Lang = lang_csharp,
        ModuleName = foreign_language_module_name(ForeignImportModule, Lang)
    ).

fim_spec_module_name_from_module(ModuleFIMSpec, CurrentModule) =
        ImportedForeignCodeModuleName :-
    ModuleFIMSpec = fim_spec(Lang, _),
    ImportedForeignCodeModuleName1 = fim_spec_module_name(ModuleFIMSpec),
    (
        Lang = lang_c,
        ImportedForeignCodeModuleName = ImportedForeignCodeModuleName1
    ;
        Lang = lang_csharp,
        ImportedForeignCodeModuleName = handle_std_library(CurrentModule,
            ImportedForeignCodeModuleName1)
    ;
        Lang = lang_java,
        ImportedForeignCodeModuleName = handle_std_library(CurrentModule,
            ImportedForeignCodeModuleName1)
    ;
        Lang = lang_erlang,
        ImportedForeignCodeModuleName = handle_std_library(CurrentModule,
            ImportedForeignCodeModuleName1)
    ).

    % On the il backend, we need to refer to the module "mercury" when
    % referencing a std library module when we are not actually building
    % the std library.
    % XXX Obsolete comment.
    %
:- func handle_std_library(module_name, module_name) = module_name.

handle_std_library(CurrentModule, ModuleName0) = ModuleName :-
    ( if
        mercury_std_library_module_name(ModuleName0),
        not mercury_std_library_module_name(CurrentModule)
    then
        ModuleName = unqualified("mercury")
    else
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

foreign_language_file_extension(lang_c) = ".c".
foreign_language_file_extension(lang_csharp) = ".cs".
foreign_language_file_extension(lang_java) = ".java".

%-----------------------------------------------------------------------------%

prefer_foreign_language(_Globals, Target, Lang1, Lang2) = Prefer :-
    % Currently we don't use the globals to compare foreign language
    % interfaces, but if we added appropriate options we might want
    % to do this later.
    (
        Target = target_c,
        % When compiling to C, C is always preferred over any other language.
        ( if Lang2 = lang_c, not Lang1 = lang_c then
            Prefer = yes
        else
            Prefer = no
        )
    ;
        Target = target_csharp,
        Prefer = no
    ;
        Target = target_java,
        % Nothing useful to do here, but when we add Java as a
        % foreign language, we should add it here.
        % XXX Obsolete comment.
        Prefer = no
    ;
        Target = target_erlang,
        % Nothing useful to do here, but when we add Erlang as a
        % foreign language, we should add it here.
        % XXX Obsolete comment.
        Prefer = no
    ).

%-----------------------------------------------------------------------------%

all_foreign_languages = Langs :-
    GetLangs = (pred(Lang::out) is multi :- valid_foreign_language(Lang)),
    solutions(GetLangs, Langs).

:- pred valid_foreign_language(foreign_language).
:- mode valid_foreign_language(in) is det.
:- mode valid_foreign_language(out) is multi.

valid_foreign_language(lang_c).
valid_foreign_language(lang_java).
valid_foreign_language(lang_csharp).
valid_foreign_language(lang_erlang).

%-----------------------------------------------------------------------------%

foreign_type_language(c(_)) = lang_c.
foreign_type_language(java(_)) = lang_java.
foreign_type_language(csharp(_)) = lang_csharp.
foreign_type_language(erlang(_)) = lang_erlang.

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

name_mangle(Name) = name_mangle_2(yes, Name).

name_mangle_no_leading_digit(Name) = name_mangle_2(no, Name).

:- func name_mangle_2(bool, string) = string.

name_mangle_2(AllowLeadingDigit, Name) = MangledName :-
    % Warning: any changes to the name mangling algorithm here may also
    % require changes to profiler/demangle.m, util/mdemangle.c,
    % compiler/name_mangle.m and library/rtti_implementation.m.

    ( if
        string.is_all_alnum_or_underscore(Name),
        (
            AllowLeadingDigit = yes
        ;
            AllowLeadingDigit = no,
            % If the mangled name may be used at the start of a symbol then
            % leading digits are invalid.
            string.index(Name, 0, FirstChar),
            not char.is_digit(FirstChar)
        )
    then
        % Any names that start with `f_' are changed so that they start with
        % `f__', so that we can use names starting with `f_' (followed by
        % anything except an underscore) without fear of name collisions.

        ( if string.append("f_", Suffix, Name) then
            MangledName = "f__" ++ Suffix
        else
            MangledName = Name
        )
    else
        MangledName = convert_to_valid_c_identifier(Name)
    ).

qualify_name(Module0, Name0) = Name :-
    string.append_list([Module0, "__", Name0], Name).

convert_to_valid_c_identifier(String) = Name :-
    ( if name_conversion_table(String, Name0) then
        Name = Name0
    else
        Name = "f" ++ convert_to_valid_c_identifier_2(String)
    ).

    % A table used to convert Mercury functors into C identifiers.
    % Feel free to add any new translations you want. The C identifiers
    % should start with "f_", to avoid introducing name clashes. If the functor
    % name is not found in the table, then we use a fall-back method which
    % produces ugly names.
    %
    % Additions to this table should be reflected in rtti_implementation.m,
    % in the ML_name_mangle() method.
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
    ( if string.first_char(String, Char, Rest) then
        % XXX This will cause ABI incompatibilities between compilers which are
        % built in grades that have different character representations.
        char.to_int(Char, Code),
        string.int_to_string(Code, CodeString),
        string.append("_", CodeString, ThisCharString),
        Name0 = convert_to_valid_c_identifier_2(Rest),
        string.append(ThisCharString, Name0, Name)
    else
        % String is the empty string.
        Name = String
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_foreign.
%-----------------------------------------------------------------------------%
