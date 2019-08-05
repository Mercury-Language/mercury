%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for Mantis bug #480.
%
% Versions of the Mercury compiler between 2019 Jun 30 and 2019 Aug 5
% could not compile the correct code of to_bool below. The reason was that
% (a) the construction of Struct leaves it with a unique inst for its
% top level cell, and (b) a fix for github issue 64 applied on Jun 30
% prevented cse_detection.m from pulling the "Struct = struct(Foo, _Bar)"
% unifications out of the disjunction, preventing the compiler from
% recognizing the disjunction as a switch on Foo. The fix was intending
% to avoid problems with unique insts inside the *arguments* of the
% structure, but it incorrectly paid attention to the inst of the
% *top level memory cell* as well.
%
%---------------------------------------------------------------------------%

:- module bug480.
:- interface.

:- import_module bool.
:- import_module maybe.

:- type struct
    --->    struct(
                foo :: maybe(int),
                bar :: int
            ).

:- func to_bool(struct) = bool.

:- implementation.

to_bool(Struct0) = Bool :-
    Struct0 = struct(Foo0, Bar0),
    Struct = struct(Foo0, Bar0),
    (
        Struct = struct(Foo, _Bar),
        Foo = yes(_),
        Bool = yes
    ;
        Struct = struct(Foo, _Bar),
        Foo = no,
        Bool = no
    ).
