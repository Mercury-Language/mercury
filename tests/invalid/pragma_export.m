%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Test the error messages about exporting pragmas.
%---------------------------------------------------------------------------%

:- module pragma_export.
:- interface.

:- pred memoproc1(int::in, int::in, int::out) is det.
% These should get an error, because these are implementation pragmas,
% and those should never be exported.
:- pragma memo(memoproc1(in, in, out)).
:- pragma consider_used(pred(memoproc2/3)).

% This should get an error. Declarative pragmas such as "obsolete" may appear
% in interfaces, but *only* if the predicate they refer to is also there.
:- pragma obsolete(pred(memoproc2/3)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

memoproc1(A, B, A + B).

:- pred memoproc2(int::in, int::in, int::out) is det.

memoproc2(A, B, A - B).
