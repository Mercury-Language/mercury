%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test for error messages produced by syntax errors in inst definitions.
%

:- module bad_inst_defn.
:- interface.

:- type mylist(T) ---> [] ; [T | mylist(T)].

:- inst test1(A) ---> [] ; [B | mylistskel(A)].

:- inst test2(A) ---> [] ; [B | mylistskel(C)].

:- inst test3(A, A) ---> [] ; [A | mylistskel(A)].

:- inst test4(A, A, B, B) ---> [] ; [A | mylistskel(B)].
