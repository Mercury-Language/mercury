% Check that inter-module optimization works with nested modules.
%
% The compiler of 12/11/1999 did not read the `.int0' files
% for parent modules of a module for which a `.opt' file was
% read, resulting in undefined symbol errors.
:- module intermod_nested_module.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.
:- import_module intermod_nested_module2, intermod_nested_module2__sub_module.

main -->
        { bar(3, X) },
        write(X),
        nl.

