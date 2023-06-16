%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that foreign_export_enum for dummy types works.
%

:- module ee_dummy.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.

:- type dummy_type
    --->    dummy_type.

:- type poly_dummy_type(T)
    --->    poly_dummy_type.

:- pragma foreign_export_enum("C", dummy_type/0, [prefix("FOO_")]).
:- pragma foreign_export_enum("C", poly_dummy_type/1, [prefix("BAR_")]).

:- pragma foreign_export_enum("C#", dummy_type/0, [prefix("FOO_")]).
:- pragma foreign_export_enum("C#", poly_dummy_type/1, [prefix("BAR_")]).

:- pragma foreign_export_enum("Java", dummy_type/0, [prefix("FOO_")]).
:- pragma foreign_export_enum("Java", poly_dummy_type/1, [prefix("BAR_")]).

main(!IO) :-
    check_dummy_type(dummy_type, DummyTypeSucceeded, !IO),
    (
        DummyTypeSucceeded = yes,
        io.write_string("FOO_dummy_type exists.\n", !IO)
    ;
        DummyTypeSucceeded = no,
        io.write_string("FOO_dummy_type does not exist\n", !IO)
    ),
    check_poly_dummy_type(poly_dummy_type, PolyDummyTypeSucceeded, !IO),
    (
        PolyDummyTypeSucceeded = yes,
        io.write_string("BAR_poly_dummy_type exists.\n", !IO)
    ;
        PolyDummyTypeSucceeded = no,
        io.write_string("BAR_poly_dummy_type does not exist\n", !IO)
    ).

:- pred check_dummy_type(dummy_type::in, bool::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    check_dummy_type(X::in, Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Result = (X == FOO_dummy_type) ? MR_YES : MR_NO;
    IO = IO0;
").
:- pragma foreign_proc("C#",
    check_dummy_type(X::in, Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Result = (X == FOO_dummy_type) ? mr_bool.YES : mr_bool.NO;
    IO = IO0;
").
:- pragma foreign_proc("Java",
    check_dummy_type(X::in, Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Result = (X == FOO_dummy_type) ? bool.YES : bool.NO;
    IO = IO0;
").

:- pred check_poly_dummy_type(poly_dummy_type(dummy_type)::in, bool::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    check_poly_dummy_type(X::in, Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Result = (X == BAR_poly_dummy_type) ? MR_YES : MR_NO;
    IO = IO0;
").
:- pragma foreign_proc("C#",
    check_poly_dummy_type(X::in, Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Result = (X == BAR_poly_dummy_type) ? mr_bool.YES : mr_bool.NO;
    IO = IO0;
").
:- pragma foreign_proc("Java",
    check_poly_dummy_type(X::in, Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Result = (X == BAR_poly_dummy_type) ? bool.YES : bool.NO;
    IO = IO0;
").
