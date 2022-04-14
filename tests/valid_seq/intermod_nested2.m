%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_nested2.

:- interface.

    :- module intermod_nested2.bar.

    :- interface.

    :- type bar
        --->    bar.

    :- end_module intermod_nested2.bar.

:- implementation.

:- import_module int.

% Check that `exported_to_submodules' functions are declared
% in the `.opt' file.
:- func foo = int.

foo = 1.
