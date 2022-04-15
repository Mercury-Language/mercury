%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module glob_test_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module reference.

%----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include ""c_reference.h""
").

:- pragma promise_pure(main/2).

main(!IO) :-
    ( if
        impure update(trailed_global, 2),
        semidet_fail
    then
        io.write_string("I didn't expect that to succeed!\n", !IO)
    else
        semipure value(trailed_global, Value),
        io.write_int(Value, !IO),
        io.nl(!IO)
    ).

:- func trailed_global = reference(int).
:- pragma memo(trailed_global/0).
:- pragma promise_pure(trailed_global/0).

trailed_global = Ref :-
    impure new_reference(1, Ref).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
