%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module to_submods_opt_helper_1.

:- interface.

% Export something to shut up a warning.
:- type t
    --->    f.

%---------------------------------------------------------------------------%
    :- module to_submods_opt_helper_1.to_submods_opt_helper_2.

    :- interface.

    :- type bar
        --->    bar.

    :- end_module
    to_submods_opt_helper_1.to_submods_opt_helper_2.
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

% Check that `exported_to_submodules' functions are declared
% in the `.opt' file.
:- func foo = int.

foo = 1.
