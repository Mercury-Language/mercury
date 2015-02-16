%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Compile with: --debug -O0 --optimize-saved-vars-const

:- module exists_cast_bug.

:- interface.

:- type version_store(S).

:- some [S] func new = version_store(S).

:- implementation.

:- import_module version_array.

:- type version_store(S)  ---> version_store(version_array(unit)).
:- type unit ---> unit.

new = Result :-
    Result = version_store(VA) `with_type` version_store(unit),
    VA = version_array.init(256, unit).
