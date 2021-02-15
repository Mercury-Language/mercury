%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_submodule.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- implementation.

%---------------------------------------------------------------------------%

:- module child.
:- interface.

:- type citrus =< fruit     % ok
    --->    orange
    ;       lemon.

:- type citrus2 =< fruit2   % bad
    --->    orange
    ;       lemon.

:- end_module child.

%---------------------------------------------------------------------------%

:- end_module subtype_submodule.
