:- module intermod_nested2.

:- interface.

	:- module intermod_nested2__bar.

	:- interface.

	:- type bar ---> bar.

	:- end_module intermod_nested2__bar.

:- implementation.

:- import_module int.

% Check that `exported_to_submodules' functions are declared
% in the `.opt' file.
:- func foo = int.

foo = 1.

