%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_ctor_arg.
:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- type nonempty_list(T) =< list(T)
    --->    [T | list(T)].                      % ok

:- type fruit(O)
    --->    apple
    ;       orange(list(O))
    ;       lemon(list(foo_bar)).

:- type citrus(O) =< fruit(O)
    --->    orange(nonempty_list(foo))          % bad
    ;       lemon(nonempty_list(foo)).          % ok

:- type foo_bar
    --->    foo
    ;       bar.

:- type foo =< foo_bar
    --->    foo.

%---------------------------------------------------------------------------%

:- type pair(T)
    --->    pair(T, T).

:- type bad_pair1(T, U) =< pair(U)
    --->    pair(T, U).                         % bad

:- type bad_pair2 =< pair(citrus(foo))
    --->    pair(citrus(foo), fruit(foo)).      % bad

%---------------------------------------------------------------------------%


