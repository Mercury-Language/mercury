%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Test the error messages about exporting pragmas.
%---------------------------------------------------------------------------%

:- module pragma_export.
:- interface.

% This should get an error. Declarative pragmas such as "obsolete" may appear
% in interfaces, but *only* if the predicate they refer to is also there.
:- pragma obsolete(pred(memoproc2/3)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pred memoproc2(int::in, int::in, int::out) is det.

memoproc2(A, B, A - B).
