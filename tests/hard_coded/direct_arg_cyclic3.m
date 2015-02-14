%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module direct_arg_cyclic3.

:- interface.

:- import_module direct_arg_cyclic2.

:- type maybe_grapheme
    --->    no_grapheme
    ;       yes_grapheme(grapheme)
    where direct_arg is [yes_grapheme/1].
