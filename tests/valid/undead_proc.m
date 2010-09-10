:- module undead_proc.

:- interface.

% no exported preds!

:- implementation.

:- pred p(int::out) is det.

:- pragma foreign_export("C", p(out), "p").

p(42).
