:- module undead_proc.

:- interface.

% no exported preds!

:- implementation.

:- pred p(int::out) is det.

:- pragma export(p(out), "p").

p(42).
