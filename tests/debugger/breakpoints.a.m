:- module breakpoints__a.

:- interface.

:- include_module breakpoints__a__testmod.
:- import_module breakpoints__a__testmod.

:- func afunc = int.

:- implementation.

afunc = 42.
