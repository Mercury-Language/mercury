:- module int_impl_imports.
:- interface.

:- use_module int_impl_imports_2.

:- type foo == int_impl_imports_2.bar.

:- implementation.

:- import_module int_impl_imports_2.
