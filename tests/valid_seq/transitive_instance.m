%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module transitive_instance.

:- interface.

:- type t.

:- implementation.

:- import_module transitive_instance.sub2.

:- type t
    --->    some [T] c(T) => c2(T).

:- module transitive_instance.sub1.

    :- interface.

    :- typeclass c1(T) where [].

    :- instance c1(int).

    :- implementation.

    :- instance c1(int) where [].

:- end_module transitive_instance.sub1.

:- module transitive_instance.sub2.

    :- interface.

    :- import_module transitive_instance.sub1.

    :- typeclass c2(T) <= c1(T) where [].

    :- instance c2(int).

    :- implementation.

    :- instance c2(int) where [].

:- end_module transitive_instance.sub2.
