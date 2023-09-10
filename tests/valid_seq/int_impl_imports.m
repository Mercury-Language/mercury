%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module int_impl_imports.
:- interface.

:- use_module int_impl_imports_helper_1.

:- type foo == int_impl_imports_helper_1.bar.

:- implementation.

:- import_module int_impl_imports_helper_1.
