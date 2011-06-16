:- module direct_arg_cyclic2.

:- interface.

:- import_module direct_arg_cyclic3.

:- type grapheme
    --->    grapheme(int, int, int, int).

:- func okay(int) = maybe_grapheme.

:- implementation.

okay(_) = no_grapheme.
