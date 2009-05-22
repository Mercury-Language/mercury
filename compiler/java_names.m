%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: java_names.m.
% Main authors: juliensf, mjwybrow, wangp.
%
% This module contains utility routines related to naming things in Java,
% which are also required in the frontend.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.java_names.
:- interface.

:- import_module mdbcomp.prim_data.

:- import_module bool.

%-----------------------------------------------------------------------------%

    % For the Java back-end, we need to distinguish between module qualifiers
    % and type qualifiers, because type names get the case of their initial
    % letter inverted (i.e. lowercase => uppercase).
    %
    % This duplicates mlds_qual_kind so as not to introduce unwanted
    % dependencies in either direction.
    %
:- type java_qual_kind
    --->    module_qual
    ;       type_qual.

    % Java doesn't allow a fully-qualified class to have the same name as a
    % package.  Our workaround is to name package components with trailing
    % underscores, e.g. `mammal_.primate_.chimp' where `chimp' is a class.
    % This is enabled with `package_name_mangling'.
    %
    % The packages `mercury' and `mercury.runtime' are named without
    % underscores simply because there is existing handwritten code already
    % using those names.
    %
:- type package_name_mangling
    --->    package_name_mangling
    ;       no_package_name_mangling.

    % Mangle a name so that it is suitable for Java.
    %
:- pred mangle_sym_name_for_java(sym_name::in, java_qual_kind::in,
    string::in, package_name_mangling::in, string::out) is det.

    % Returns yes iff the given package is one provided by the Mercury
    % implementation, `mercury' or `mercury.runtime'.
    %
:- func is_mercury_provided_java_package(sym_name) = bool.

    % Used in module_name_to_file_name to derive file names for Java files.
    % Returns a module name which each component mangled.
    %
:- func java_module_name(module_name) = module_name.

    % Return the given module name with an outermost "mercury" qualifier,
    % if it is not already present.
    %
:- func enforce_outermost_mercury_qualifier(module_name) = module_name.

    % If the given name conficts with a reserved Java word we must add a
    % prefix to it to avoid compilation errors.
    %
:- func valid_java_symbol_name(string) = string.

    % Succeeds iff the given string matches a reserved word in Java.
    %
:- pred java_is_keyword(string::in) is semidet.

    % Invert the case of the first letter of the string.
    %
:- func flip_initial_case(string) = string.

    % Invert the case of the first letter of the last component of
    % a (possibly) qualified name.
    %
:- func flip_initial_case_of_final_part(sym_name) = sym_name.

    % The package containing the Mercury standard library.
    %
:- func mercury_std_library_package_name = sym_name.

    % The package containing the Mercury Java runtime classes.
    %
:- func mercury_runtime_package_name = sym_name.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_foreign.   % for name_mangle

:- import_module char.
:- import_module string.

%-----------------------------------------------------------------------------%

mangle_sym_name_for_java(SymName, QualKind, QualifierOp,
        PackageNameMangling, JavaSafeName) :-
    mangle_sym_name_for_java_2(SymName, QualKind, PackageNameMangling,
        MangledSymName),
    JavaSafeName = sym_name_to_string_sep(MangledSymName, QualifierOp).

:- pred mangle_sym_name_for_java_2(sym_name::in, java_qual_kind::in,
    package_name_mangling::in, sym_name::out) is det.

mangle_sym_name_for_java_2(SymName, QualKind, PackageNameMangling,
        MangledSymName) :-
    (
        SymName = unqualified(Name),
        JavaSafeName = java_safe_name_component(QualKind, Name),
        MangledSymName = unqualified(JavaSafeName)
    ;
        SymName = qualified(ModuleName0, PlainName),
        mangle_sym_name_for_java_2(ModuleName0, module_qual,
            PackageNameMangling, MangledModuleName0),
        (
            PackageNameMangling = package_name_mangling,
            MercuryProvided = is_mercury_provided_java_package(ModuleName0),
            (
                MercuryProvided = yes,
                MangledModuleName = MangledModuleName0
            ;
                MercuryProvided = no,
                MangledModuleName = append_underscore_sym_name(
                    MangledModuleName0)
            )
        ;
            PackageNameMangling = no_package_name_mangling,
            MangledModuleName = MangledModuleName0
        ),
        JavaSafePlainName = java_safe_name_component(QualKind, PlainName),
        MangledSymName = qualified(MangledModuleName, JavaSafePlainName)
    ).

:- func java_safe_name_component(java_qual_kind, string) = string.

java_safe_name_component(QualKind, Name) = JavaSafeName :-
    (
        QualKind = module_qual,
        FlippedName = Name
    ;
        QualKind = type_qual,
        FlippedName = flip_initial_case(Name)
    ),
    MangledName = name_mangle(FlippedName),
    JavaSafeName = valid_java_symbol_name(MangledName).

is_mercury_provided_java_package(ModuleName) = MercuryProvided :-
    ( ModuleName = mercury_std_library_package_name ->
        MercuryProvided = yes
    ; ModuleName = mercury_runtime_package_name ->
        MercuryProvided = yes
    ;
        MercuryProvided = no
    ).

:- func append_underscore_sym_name(sym_name) = sym_name.

append_underscore_sym_name(SymName0) = SymName :-
    (
        SymName0 = unqualified(Name),
        SymName = unqualified(Name ++ "_")
    ;
        SymName0 = qualified(ModuleSymName, Name),
        SymName = qualified(ModuleSymName, Name ++ "_")
    ).

%-----------------------------------------------------------------------------%

java_module_name(ModuleName) = JavaModuleName :-
    % Put a "mercury" prefix on the module name if it doesn't already have one,
    % so that even unqualified module names to end up in a Java package.
    % Java doesn't allow packaged classes to import types from the default
    % unnamed package.  We don't do this earlier so as not to disturb other
    % MLDS backends.
    QualModuleName = enforce_outermost_mercury_qualifier(ModuleName),
    mangle_sym_name_for_java_2(QualModuleName, module_qual,
        package_name_mangling, JavaModuleName).

enforce_outermost_mercury_qualifier(ModuleName) = QualModuleName :-
    ( outermost_qualifier(ModuleName) = "mercury" ->
        QualModuleName = ModuleName
    ;
        QualModuleName = add_outermost_qualifier("mercury", ModuleName)
    ).

%-----------------------------------------------------------------------------%

valid_java_symbol_name(SymName) = ValidSymName :-
    Prefix = "mr_",
    ( java_is_keyword(SymName) ->
        % This is a reserved Java word, add the above prefix.
        ValidSymName = Prefix ++ SymName
    ; string.append(Prefix, Suffix, SymName) ->
        % This name already contains the prefix we are adding to
        % variables to avoid conficts, so add an additional '_'.
        ValidSymName = Prefix ++ "_" ++ Suffix
    ;
        % Normal name; do nothing.
        ValidSymName = SymName
    ).

%-----------------------------------------------------------------------------%

java_is_keyword("abstract").
java_is_keyword("boolean").
java_is_keyword("break").
java_is_keyword("byte").
java_is_keyword("case").
java_is_keyword("catch").
java_is_keyword("char").
java_is_keyword("class").
java_is_keyword("const").
java_is_keyword("continue").
java_is_keyword("default").
java_is_keyword("do").
java_is_keyword("double").
java_is_keyword("else").
java_is_keyword("enum").
java_is_keyword("extends").
java_is_keyword("false").
java_is_keyword("final").
java_is_keyword("finally").
java_is_keyword("float").
java_is_keyword("for").
java_is_keyword("goto").
java_is_keyword("if").
java_is_keyword("implements").
java_is_keyword("import").
java_is_keyword("instanceof").
java_is_keyword("int").
java_is_keyword("interface").
java_is_keyword("long").
java_is_keyword("native").
java_is_keyword("new").
java_is_keyword("null").
java_is_keyword("package").
java_is_keyword("private").
java_is_keyword("protected").
java_is_keyword("public").
java_is_keyword("return").
java_is_keyword("short").
java_is_keyword("static").
java_is_keyword("strictfp").
java_is_keyword("super").
java_is_keyword("switch").
java_is_keyword("synchronized").
java_is_keyword("this").
java_is_keyword("throw").
java_is_keyword("throws").
java_is_keyword("transient").
java_is_keyword("true").
java_is_keyword("try").
java_is_keyword("void").
java_is_keyword("volatile").
java_is_keyword("while").

%-----------------------------------------------------------------------------%

flip_initial_case(S0) = S :-
    ( string.first_char(S0, First0, Rest) ->
        ( char.is_upper(First0) ->
            First = char.to_lower(First0)
        ; char.is_lower(First0) ->
            First = char.to_upper(First0)
        ;
            First = First0
        ),
        string.first_char(S, First, Rest)
    ;
        S = S0
    ).

flip_initial_case_of_final_part(unqualified(Name)) =
    unqualified(flip_initial_case(Name)).
flip_initial_case_of_final_part(qualified(Qual, Name)) =
    qualified(Qual, flip_initial_case(Name)).

%-----------------------------------------------------------------------------%

mercury_std_library_package_name = unqualified("mercury").

mercury_runtime_package_name = qualified(unqualified("mercury"), "runtime").

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "java_names.m".

%-----------------------------------------------------------------------------%
:- end_module java_names.
%-----------------------------------------------------------------------------%
