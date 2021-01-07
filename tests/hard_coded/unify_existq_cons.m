%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module unify_existq_cons.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module enum.
:- import_module char.
:- import_module int.

:- type tc
    --->    some [T] tc(T) => enum(T).

main(!IO) :-
    ( if
        ( p('new tc'('a'))
        ; p('new tc'(2))
        )
    then
        io.write_string("test failed\n", !IO)
    else
        io.write_string("test succeeded\n", !IO)
    ).

:- pred p(tc::in) is semidet.

% Mode analysis must treat the headvar unification here as a construction
% followed by a var-var unification. If it treats it as a deconstruction
% the argument unifications will be ill-typed.
p('new tc'(1)).
