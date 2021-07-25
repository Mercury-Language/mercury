%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module propositional.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util.

main(!IO) :-
    ( if a, b then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred a is semidet.
:- pred b is semidet.
:- pred c is semidet.
:- pred d is semidet.
:- pred e is semidet.
:- pred f is semidet.
:- pred g is semidet.
:- pred h is semidet.
:- pred i is semidet.

a :- e.
a :- c.
b :- f.
b :- g.
c :- semidet_succeed.
c :- d.
d :- semidet_succeed.
e :- semidet_fail.
f :- h.
f :- i.
g :- semidet_succeed.
h :- semidet_fail.
i :- semidet_succeed.
