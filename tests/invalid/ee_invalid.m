%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test error messages for invalid foreign_export_enum pragmas.
%
:- module ee_invalid.
:- interface.

:- type foo(T)
    --->    foo(T).

:- type bar == int.

:- implementation.
:- type baz
    --->    baz.
:- pragma foreign_type("C",    baz, "int").
:- pragma foreign_type("Java", baz, "int").
:- pragma foreign_type("C#",   baz, "int").
:- type alphabet
    --->    abc
    ;       def
    ;       ghi
    ;       jkl
    ;       mno
    ;       pqr
    ;       stu
    ;       vwx
    ;       yz.

:- type strange_names
    --->    '!@THIS'
    ;       '#$WON''T'
    ;       '%^WORK'.

    % foreign_export_enum pragma for atomic type.
    %
:- pragma foreign_export_enum("C", int/0).

    % foreign export_enum pragma for undefined type.
    %
:- pragma foreign_export_enum("C", undefined_type/0).

    % foreign_export_enum pragma for non-enumeration d.u. type.
    %
:- pragma foreign_export_enum("C", foo/1).

    % foreign_export_enum pragma for equivalence type.
    %
:- pragma foreign_export_enum("C", bar/0).

    % foreign_export_enum pragma for foreign_type (with default
    % Mercury definition.)
    % We should reject these if the language of the foreign_type
    % and foreign_export_enum pragmas is the same.
    %
:- pragma foreign_export_enum("C", baz/0).

    % foreign_export_enum pragma where the override list refers to a
    % constructor that is not a member of the type.
    %
:- pragma foreign_export_enum("C", alphabet/0,
    [], [abc - "ABC", deg - "DEF"]).

    % Make sure we do something sensible with module-qualifiers
    % in the override list.
:- pragma foreign_export_enum("C", alphabet/0,
    [], [ee_invalid.abc - "ABC", foo.def - "DEF"]).

    % Check that the the mappings from Mercury -> Foreign names are a bijection.
    %
:- pragma foreign_export_enum("C", alphabet/0,
    [prefix("FOO")], [abc - "ABC", deg - "ABC"]).
:- pragma foreign_export_enum("C", alphabet/0,
    [prefix("BAR")], [def - "abc"]).
:- pragma foreign_export_enum("C", alphabet/0,
    [prefix("BAZ")], [abc - "ABC", abc - "DEF"]).

    % Emit an error when the default mapping for a language fails and
    % the user has not specified an alternative.
    %
:- pragma foreign_export_enum("C", strange_names/0).
