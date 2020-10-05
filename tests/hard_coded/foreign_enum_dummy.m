%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that the dummy type optimisation is disabled for foreign enumerations.
%

:- module foreign_enum_dummy.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.

main(!IO) :-
    ( if test(foo) then
        io.write_string("Success.\n", !IO)
    else
        io.write_string("Failure.\n", !IO)
    ).

:- pred test(foo::in) is semidet.
:- pragma foreign_proc("C",
    test(FOO::in),
    [will_not_call_mercury, promise_pure],
"
    if (FOO == 561) {
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").
:- pragma foreign_proc("C#",
    test(FOO::in),
    [will_not_call_mercury, promise_pure],
"
    if ((int) FOO == 561) {
        SUCCESS_INDICATOR = true;
    } else {
        SUCCESS_INDICATOR = false;
    }
").

:- type foo
    --->    foo.
:- pragma foreign_enum("C", foo/0, [foo - "561"]).
:- pragma foreign_enum("C#", foo/0, [foo - "561"]).
