:- module type_with_no_defn.

:- interface.

:- type alpha.

:- type beta == int.

:- type gamma.

:- implementation.

:- type baz.	% This is redundant but we allow it.

:- type beta.

:- type gamma == float.

:- type delta == string.
