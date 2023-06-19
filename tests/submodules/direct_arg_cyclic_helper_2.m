%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module direct_arg_cyclic_helper_2.

:- interface.

:- import_module direct_arg_cyclic_helper_1.

:- type maybe_grapheme
    --->    no_grapheme
    ;       yes_grapheme(grapheme)
    where direct_arg is [yes_grapheme/1].
