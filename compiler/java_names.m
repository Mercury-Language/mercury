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

    % If the given name conficts with a reserved Java word we must add a
    % prefix to it to avoid compilation errors.
    %
:- func valid_java_symbol_name(string) = string.

    % Succeeds iff the given string matches a reserved word in Java.
    %
:- pred java_is_keyword(string::in) is semidet.

    % The package containing the Mercury Java runtime classes.
    %
:- func java_mercury_runtime_package_name = sym_name.

%-----------------------------------------------------------------------------%

    % Mangle a name so that it is suitable for C#.
    %
:- pred mangle_sym_name_for_csharp(sym_name::in, csj_qual_kind::in,
    string::in, string::out) is det.

    % If the given name conficts with a reserved C# word we must add a
    % prefix to it to avoid compilation errors.
    %
:- func valid_csharp_symbol_name(string) = string.

    % Succeeds iff the given string matches a reserved word in C#.
    %
:- pred csharp_is_keyword(string::in) is semidet.

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
% Java naming
%

mangle_sym_name_for_java(SymName0, QualKind, QualifierOp, JavaSafeName) :-
    % Modules in the Mercury standard library get a `mercury' prefix when
    % mapped to MLDS module names.  Since we place all Java classes inside a
    % `jmercury' package, the extra prefix is just redundant so we remove it.
    ( strip_outermost_qualifier(SymName0, "mercury", StrippedSymName) ->
        SymName = StrippedSymName
    ;
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
        FlippedName = MangledName
    ;
        QualKind = type_qual,
        FlippedName = flip_initial_case(MangledName)
    ),
    JavaSafeName = valid_java_symbol_name(FlippedName).

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

java_mercury_runtime_package_name =
    qualified(unqualified("jmercury"), "runtime").

%-----------------------------------------------------------------------------%
%
% C# naming
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
    SafeName = valid_csharp_symbol_name(FlippedName).

valid_csharp_symbol_name(SymName) = ValidSymName :-
    Prefix = "mr_",
    ( csharp_is_keyword(SymName) ->
        % This is a reserved word, add the above prefix.
        ValidSymName = Prefix ++ SymName
    ; string.append(Prefix, Suffix, SymName) ->
        % This name already contains the prefix we are adding to
        % variables to avoid conficts, so add an additional '_'.
        ValidSymName = Prefix ++ "_" ++ Suffix
    ;
        % Normal name; do nothing.
        ValidSymName = SymName
    ).

csharp_is_keyword("abstract").
csharp_is_keyword("as").
csharp_is_keyword("base").
csharp_is_keyword("bool").
csharp_is_keyword("break").
csharp_is_keyword("byte").
csharp_is_keyword("case").
csharp_is_keyword("catch").
csharp_is_keyword("char").
csharp_is_keyword("checked").
csharp_is_keyword("class").
csharp_is_keyword("const").
csharp_is_keyword("continue").
csharp_is_keyword("decimal").
csharp_is_keyword("default").
csharp_is_keyword("delegate").
csharp_is_keyword("do").
csharp_is_keyword("double").
csharp_is_keyword("else").
csharp_is_keyword("enum").
csharp_is_keyword("event").
csharp_is_keyword("explicit").
csharp_is_keyword("extern").
csharp_is_keyword("false").
csharp_is_keyword("finally").
csharp_is_keyword("fixed").
csharp_is_keyword("float").
csharp_is_keyword("for").
csharp_is_keyword("foreach").
csharp_is_keyword("goto").
csharp_is_keyword("if").
csharp_is_keyword("implicit").
csharp_is_keyword("in").
csharp_is_keyword("int").
csharp_is_keyword("interface").
csharp_is_keyword("internal").
csharp_is_keyword("is").
csharp_is_keyword("lock").
csharp_is_keyword("long").
csharp_is_keyword("namespace").
csharp_is_keyword("new").
csharp_is_keyword("null").
csharp_is_keyword("object").
csharp_is_keyword("operator").
csharp_is_keyword("out").
csharp_is_keyword("override").
csharp_is_keyword("params").
csharp_is_keyword("private").
csharp_is_keyword("protected").
csharp_is_keyword("public").
csharp_is_keyword("readonly").
csharp_is_keyword("ref").
csharp_is_keyword("return").
csharp_is_keyword("sbyte").
csharp_is_keyword("sealed").
csharp_is_keyword("short").
csharp_is_keyword("sizeof").
csharp_is_keyword("stackalloc").
csharp_is_keyword("static").
csharp_is_keyword("string").
csharp_is_keyword("struct").
csharp_is_keyword("switch").
csharp_is_keyword("this").
csharp_is_keyword("throw").
csharp_is_keyword("true").
csharp_is_keyword("try").
csharp_is_keyword("typeof").
csharp_is_keyword("uint").
csharp_is_keyword("ulong").
csharp_is_keyword("unchecked").
csharp_is_keyword("unsafe").
csharp_is_keyword("ushort").
csharp_is_keyword("using").
csharp_is_keyword("virtual").
csharp_is_keyword("volatile").
csharp_is_keyword("void").
csharp_is_keyword("while").

csharp_mercury_runtime_package_name =
    qualified(unqualified("mercury"), "runtime").

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
:- end_module parse_tree.java_names.
%-----------------------------------------------------------------------------%
