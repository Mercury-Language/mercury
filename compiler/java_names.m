%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: java_names.m.
% Main authors: juliensf, mjwybrow, wangp.
%
% This module contains utility routines related to naming things in Java/C#
% which are also required in the frontend.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.java_names.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

%-----------------------------------------------------------------------------%

    % For the C# and Java back-ends, we need to distinguish between module
    % qualifiers and type qualifiers, because type names get the case of their
    % initial letter inverted (i.e. lowercase => uppercase).
    %
    % This duplicates mlds_qual_kind so as not to introduce unwanted
    % dependencies in either direction.
    %
:- type csj_qual_kind
    --->    module_qual
    ;       type_qual.

    % Mangle a name so that it is suitable for Java.
    %
:- pred mangle_sym_name_for_java(sym_name::in, csj_qual_kind::in,
    string::in, string::out) is det.

    % If the given name conflicts with a reserved Java word,
    % add a prefix to it to avoid compilation errors.
    %
:- func make_valid_java_symbol_name(string) = string.

    % Succeeds iff the given string matches a reserved word in Java.
    %
:- pred is_java_keyword(string::in) is semidet.

    % The package containing the Mercury Java runtime classes.
    %
:- func java_mercury_runtime_package_name = sym_name.

%-----------------------------------------------------------------------------%

    % Mangle a name so that it is suitable for C#.
    %
:- pred mangle_sym_name_for_csharp(sym_name::in, csj_qual_kind::in,
    string::in, string::out) is det.

    % If the given name conflicts with a reserved C# word,
    % add a prefix to it to avoid compilation errors.
    %
:- func make_valid_csharp_symbol_name(string) = string.

    % Succeeds iff the given string matches a reserved word in C#.
    %
:- pred is_csharp_keyword(string::in) is semidet.

    % The package containing the Mercury C# runtime classes.
    %
:- func csharp_mercury_runtime_package_name = sym_name.

%-----------------------------------------------------------------------------%

    % Invert the case of the first letter of the string.
    %
:- func flip_initial_case(string) = string.

    % Invert the case of the first letter of the last component of
    % a (possibly) qualified name.
    %
:- func flip_initial_case_of_final_part(sym_name) = sym_name.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_foreign.   % for name_mangle

:- import_module char.
:- import_module string.

%-----------------------------------------------------------------------------%
%
% Java naming.
%

mangle_sym_name_for_java(SymName0, QualKind, QualifierOp, JavaSafeName) :-
    % Modules in the Mercury standard library get a `mercury' prefix when
    % mapped to MLDS module names. Since we place all Java classes inside a
    % `jmercury' package, the extra prefix is just redundant so we remove it.
    ( if strip_outermost_qualifier(SymName0, "mercury", StrippedSymName) then
        SymName = StrippedSymName
    else
        SymName = SymName0
    ),
    mangle_sym_name_for_java_2(SymName, QualKind, MangledSymName),
    JavaSafeName = sym_name_to_string_sep(MangledSymName, QualifierOp).

:- pred mangle_sym_name_for_java_2(sym_name::in, csj_qual_kind::in,
    sym_name::out) is det.

mangle_sym_name_for_java_2(SymName, QualKind, MangledSymName) :-
    (
        SymName = unqualified(Name),
        JavaSafeName = java_safe_name_component(QualKind, Name),
        MangledSymName = unqualified(JavaSafeName)
    ;
        SymName = qualified(ModuleName0, PlainName),
        mangle_sym_name_for_java_2(ModuleName0, module_qual,
            MangledModuleName),
        JavaSafePlainName = java_safe_name_component(QualKind, PlainName),
        MangledSymName = qualified(MangledModuleName, JavaSafePlainName)
    ).

:- func java_safe_name_component(csj_qual_kind, string) = string.

java_safe_name_component(QualKind, Name) = JavaSafeName :-
    MangledName = name_mangle_no_leading_digit(Name),
    (
        QualKind = module_qual,
        MaybeFlippedName = MangledName
    ;
        QualKind = type_qual,
        MaybeFlippedName = flip_initial_case(MangledName)
    ),
    JavaSafeName = make_valid_java_symbol_name(MaybeFlippedName).

make_valid_java_symbol_name(SymName) = ValidSymName :-
    Prefix = "mr_",
    ( if is_java_keyword(SymName) then
        % This is a reserved Java word, add the above prefix.
        ValidSymName = Prefix ++ SymName
    else if string.append(Prefix, Suffix, SymName) then
        % This name already contains the prefix we are adding to
        % variables to avoid conflicts, so add an additional '_'.
        ValidSymName = Prefix ++ "_" ++ Suffix
    else
        % Normal name; do nothing.
        ValidSymName = SymName
    ).

is_java_keyword("abstract").
is_java_keyword("boolean").
is_java_keyword("break").
is_java_keyword("byte").
is_java_keyword("case").
is_java_keyword("catch").
is_java_keyword("char").
is_java_keyword("class").
is_java_keyword("const").
is_java_keyword("continue").
is_java_keyword("default").
is_java_keyword("do").
is_java_keyword("double").
is_java_keyword("else").
is_java_keyword("enum").
is_java_keyword("extends").
is_java_keyword("false").
is_java_keyword("final").
is_java_keyword("finally").
is_java_keyword("float").
is_java_keyword("for").
is_java_keyword("goto").
is_java_keyword("if").
is_java_keyword("implements").
is_java_keyword("import").
is_java_keyword("instanceof").
is_java_keyword("int").
is_java_keyword("interface").
is_java_keyword("long").
is_java_keyword("native").
is_java_keyword("new").
is_java_keyword("null").
is_java_keyword("package").
is_java_keyword("private").
is_java_keyword("protected").
is_java_keyword("public").
is_java_keyword("return").
is_java_keyword("short").
is_java_keyword("static").
is_java_keyword("strictfp").
is_java_keyword("super").
is_java_keyword("switch").
is_java_keyword("synchronized").
is_java_keyword("this").
is_java_keyword("throw").
is_java_keyword("throws").
is_java_keyword("transient").
is_java_keyword("true").
is_java_keyword("try").
is_java_keyword("void").
is_java_keyword("volatile").
is_java_keyword("while").

java_mercury_runtime_package_name =
    qualified(unqualified("jmercury"), "runtime").

%-----------------------------------------------------------------------------%
%
% C# naming.
%

% XXX Reduce code duplication between C# and Java routines.

mangle_sym_name_for_csharp(SymName, QualKind, QualifierOp, SafeName) :-
    mangle_sym_name_for_csharp_2(SymName, QualKind, MangledSymName),
    SafeName = sym_name_to_string_sep(MangledSymName, QualifierOp).

:- pred mangle_sym_name_for_csharp_2(sym_name::in, csj_qual_kind::in,
    sym_name::out) is det.

mangle_sym_name_for_csharp_2(SymName, QualKind, MangledSymName) :-
    (
        SymName = unqualified(Name),
        SafeName = csharp_safe_name_component(QualKind, Name),
        MangledSymName = unqualified(SafeName)
    ;
        SymName = qualified(ModuleName0, PlainName),
        mangle_sym_name_for_csharp_2(ModuleName0, module_qual,
            MangledModuleName),
        SafePlainName = csharp_safe_name_component(QualKind, PlainName),
        MangledSymName = qualified(MangledModuleName, SafePlainName)
    ).

:- func csharp_safe_name_component(csj_qual_kind, string) = string.

csharp_safe_name_component(QualKind, Name) = SafeName :-
    MangledName = name_mangle_no_leading_digit(Name),
    (
        QualKind = module_qual,
        FlippedName = MangledName
    ;
        QualKind = type_qual,
        FlippedName = flip_initial_case(MangledName)
    ),
    SafeName = make_valid_csharp_symbol_name(FlippedName).

make_valid_csharp_symbol_name(SymName) = ValidSymName :-
    Prefix = "mr_",
    ( if is_csharp_keyword(SymName) then
        % This is a reserved word, add the above prefix.
        ValidSymName = Prefix ++ SymName
    else if string.append(Prefix, Suffix, SymName) then
        % This name already contains the prefix we are adding to
        % variables to avoid conflicts, so add an additional '_'.
        ValidSymName = Prefix ++ "_" ++ Suffix
    else
        % Normal name; do nothing.
        ValidSymName = SymName
    ).

is_csharp_keyword("abstract").
is_csharp_keyword("as").
is_csharp_keyword("base").
is_csharp_keyword("bool").
is_csharp_keyword("break").
is_csharp_keyword("byte").
is_csharp_keyword("case").
is_csharp_keyword("catch").
is_csharp_keyword("char").
is_csharp_keyword("checked").
is_csharp_keyword("class").
is_csharp_keyword("const").
is_csharp_keyword("continue").
is_csharp_keyword("decimal").
is_csharp_keyword("default").
is_csharp_keyword("delegate").
is_csharp_keyword("do").
is_csharp_keyword("double").
is_csharp_keyword("else").
is_csharp_keyword("enum").
is_csharp_keyword("event").
is_csharp_keyword("explicit").
is_csharp_keyword("extern").
is_csharp_keyword("false").
is_csharp_keyword("finally").
is_csharp_keyword("fixed").
is_csharp_keyword("float").
is_csharp_keyword("for").
is_csharp_keyword("foreach").
is_csharp_keyword("goto").
is_csharp_keyword("if").
is_csharp_keyword("implicit").
is_csharp_keyword("in").
is_csharp_keyword("int").
is_csharp_keyword("interface").
is_csharp_keyword("internal").
is_csharp_keyword("is").
is_csharp_keyword("lock").
is_csharp_keyword("long").
is_csharp_keyword("namespace").
is_csharp_keyword("new").
is_csharp_keyword("null").
is_csharp_keyword("object").
is_csharp_keyword("operator").
is_csharp_keyword("out").
is_csharp_keyword("override").
is_csharp_keyword("params").
is_csharp_keyword("private").
is_csharp_keyword("protected").
is_csharp_keyword("public").
is_csharp_keyword("readonly").
is_csharp_keyword("ref").
is_csharp_keyword("return").
is_csharp_keyword("sbyte").
is_csharp_keyword("sealed").
is_csharp_keyword("short").
is_csharp_keyword("sizeof").
is_csharp_keyword("stackalloc").
is_csharp_keyword("static").
is_csharp_keyword("string").
is_csharp_keyword("struct").
is_csharp_keyword("switch").
is_csharp_keyword("this").
is_csharp_keyword("throw").
is_csharp_keyword("true").
is_csharp_keyword("try").
is_csharp_keyword("typeof").
is_csharp_keyword("uint").
is_csharp_keyword("ulong").
is_csharp_keyword("unchecked").
is_csharp_keyword("unsafe").
is_csharp_keyword("ushort").
is_csharp_keyword("using").
is_csharp_keyword("virtual").
is_csharp_keyword("volatile").
is_csharp_keyword("void").
is_csharp_keyword("while").

csharp_mercury_runtime_package_name =
    qualified(unqualified("mercury"), "runtime").

%-----------------------------------------------------------------------------%

flip_initial_case(S0) = S :-
    ( if string.first_char(S0, First0, Rest) then
        ( if char.is_upper(First0) then
            First = char.to_lower(First0)
        else if char.is_lower(First0) then
            First = char.to_upper(First0)
        else
            First = First0
        ),
        string.first_char(S, First, Rest)
    else
        S = S0
    ).

flip_initial_case_of_final_part(unqualified(Name)) =
    unqualified(flip_initial_case(Name)).
flip_initial_case_of_final_part(qualified(Qual, Name)) =
    qualified(Qual, flip_initial_case(Name)).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.java_names.
%-----------------------------------------------------------------------------%
