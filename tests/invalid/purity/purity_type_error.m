% Check that impurity errors are still reported correctly
% if there are type errors in the module.
:- module purity_type_error.

:- interface.

:- impure pred warn(list(int)::out) is det.

:- pred type_error(int::out) is det.

:- implementation.

:- import_module list, string.

warn(List) :-
	append([1,2,3], [4,5,6], List).

type_error(1.0).
