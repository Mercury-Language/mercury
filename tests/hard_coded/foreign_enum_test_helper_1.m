%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_enum_test_helper_1.
:- interface.

:- type instrument.

:- type ingredient
    --->    eggs
    ;       sugar
    ;       flour
    ;       milk.

:- func my_instrument = instrument.

:- implementation.

:- type instrument
    --->    violin
    ;       piano
    ;       xylophone.

:- type foo
    --->    foo
    ;       bar
    ;       baz.

my_instrument = piano.

    % This should end up in the .int file.
    %
:- pragma foreign_enum("C", ingredient/0, [
    eggs  - "EGGS",
    sugar - "SUGAR",
    flour - "FLOUR",
    milk  - "MILK"
]).

    % As should this.
    % XXX not currently supported in Java grade.
%:- pragma foreign_enum("Java", ingredient/0, [
%    eggs  - "Ingredient.EGGS",
%    sugar - "Ingredient.SUGAR",
%    flour - "Ingredient.FLOUR",
%    milk  - "Ingredient.MILK"
%]).

    % This shouldn't since the type is not exported.
    %
:- pragma foreign_enum("C", foo/0, [
    foo - "3",
    bar - "4",
    baz - "5"
]).

    % This shouldn't since the type is abstract.
    %
:- pragma foreign_enum("C", instrument/0, [
    violin    - "100",
    piano     - "200",
    xylophone - "300"
]).

:- pragma foreign_decl("C",
"
#define EGGS    10
#define SUGAR   20
#define FLOUR   30
#define MILK    40
").
