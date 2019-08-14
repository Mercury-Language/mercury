%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module func_class.

% Class constraints on functions were being written out before the
% determinism annotation in interface files.
:- module func_class1.
:- interface.

:- typeclass tc(T) where [].
:- func f(T) = int is det <= tc(T).

:- implementation.
f(_) = 1.

:- end_module func_class1.

% Read in the faulty interface file.
:- module func_class2.
:- implementation.
:- import_module func_class.func_class1.
:- end_module func_class2.
