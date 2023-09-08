%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module empty_submodule.
:- interface.

:- type t1
    --->    t1.

:- implementation.

:- module sub.
% :- interface.
% 
% :- type t2
%     --->    t2.
:- end_module sub.

:- end_module empty_submodule.
