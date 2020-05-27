%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module test_repn_sub.
:- interface.

:- import_module bool.

    % A direct dummy type.
:- type d
    --->    d_f.

    % An enum type.
:- type animal
    --->    ant         % 0
    ;       bat         % 1
    ;       cat         % 2
    ;       dog         % 3
    ;       eel         % 4
    ;       fox         % 5
    ;       gnu         % 6
    ;       hog         % 7
    ;       ibis        % 8
    ;       jay         % 9
    ;       kea         % 10
    ;       lark        % 11
    ;       moa         % 12
    ;       newt        % 13
    ;       owl         % 14
    ;       pug.        % 15

:- type cats
    --->    house_cat
    ;       panther
    ;       lion
    ;       tiger.

    % A notag type.
:- type nt(T)
    --->    nt_f(T).

:- type notag_for_nonc_only
    --->    ntfnco_f(animal).
:- pragma foreign_type("C", notag_for_nonc_only, "void *",
    [can_pass_as_mercury_type]).

    % A word_aligned type.
:- type wa
    --->    wa_f(
                int,
                int
            ).

:- type foreign_wa.
:- pragma foreign_type("C", foreign_wa, "void *",
    [can_pass_as_mercury_type, word_aligned_pointer]).

:- type ft1
    --->    ft_f1
    ;       ft_f2(int).
:- pragma foreign_type("C", ft1, "void *",
    [can_pass_as_mercury_type, word_aligned_pointer]).
:- pragma foreign_type("C#", ft1, "void *",
    [can_pass_as_mercury_type]).

    % A mini-type, i.e. a structure type whose size can be made
    % less than the size of a word.
:- type mini
    --->    mini_f(
                bool,
                nt(int8),
                uint8
            ).

    % A type that is equivalent to a packable type.
:- type animal_eqv == animal.

:- type eqv(T) == T.

:- implementation.

    % A type that is equivalent to a packable type, but the equivalence
    % is not visible from outside this module.
:- type hidden_animal_eqv == animal.

:- pragma foreign_enum("C#", animal/0,
    [
        ant     - "ANIMAL_ANT",
        bat     - "ANIMAL_BAT",
        cat     - "ANIMAL_CAT",
        dog     - "ANIMAL_DOG",
        eel     - "ANIMAL_EEL",
        fox     - "ANIMAL_FOX",
        gnu     - "ANIMAL_GNU",
        hog     - "ANIMAL_HOG",
        ibis    - "ANIMAL_IBIS",
        jay     - "ANIMAL_JAY",
        kea     - "ANIMAL_KEA",
        lark    - "ANIMAL_LARK",
        moa     - "ANIMAL_MOA",
        newt    - "ANIMAL_NEWT",
        owl     - "ANIMAL_OWL",
        pug     - "ANIMAL_PUG"
    ]).
