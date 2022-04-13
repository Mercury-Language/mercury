%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module user_compare.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    compare(Result, foo(1), foo(2)),
    io.write_line(Result, !IO),
    ( if unify(foo(1), foo(1)) then
        io.write_string("succeeded\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),
    ( if foreign(1) = foreign(1) then
        io.write_string("succeeded\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ),
    ( if foreign(2) = foreign(3) then
        io.write_string("failed\n", !IO)
    else
        io.write_string("succeeded\n", !IO)
    ),
    compare(Result2, foreign(3), foreign(2)),
    io.write_line(Result2, !IO).

:- type foo
    --->    foo(int)
    where comparison is compare_foo.

    % Reverse the comparison of the integers.
    %
:- pred compare_foo(comparison_result::uo, foo::in, foo::in) is det.

compare_foo(Res, Foo1, Foo2) :-
    promise_equivalent_solutions [Res] (
        Foo1 = foo(Int1),
        Foo2 = foo(Int2),
        compare(Res, Int2, Int1)
    ).

:- type foreign.
:- pragma foreign_type(c, foreign, "int") where
     equality is foreign_equals, comparison is foreign_compare.
:- pragma foreign_type("C#", foreign, "int") where
     equality is foreign_equals, comparison is foreign_compare.
:- pragma foreign_type("Java", foreign, "Integer") where
     equality is foreign_equals, comparison is foreign_compare.

:- pred foreign_equals(foreign::in, foreign::in) is semidet.
:- pragma foreign_proc(c,
    foreign_equals(Foreign1::in, Foreign2::in),
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = (Foreign1 == Foreign2);
").
:- pragma foreign_proc("C#",
    foreign_equals(Foreign1::in, Foreign2::in),
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = (Foreign1 == Foreign2);
").
:- pragma foreign_proc("Java",
    foreign_equals(Foreign1::in, Foreign2::in),
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = (Foreign1 == Foreign2);
").

:- pred foreign_compare `with_type` compare(foreign) `with_inst` compare.

foreign_compare(Result, Foreign1, Foreign2) :-
    foreign_compare_2(Result0, Foreign1, Foreign2),
    Result = ( Result0 < 0 -> (<) ; Result0 = 0 -> (=) ; (>) ).

    % Reverse the comparison of the integers.
    %
:- pred foreign_compare_2(int::out, foreign::in, foreign::in) is det.
:- pragma foreign_proc(c,
    foreign_compare_2(Result::out, Foreign1::in, Foreign2::in),
    [will_not_call_mercury, promise_pure],
"
    Result = (Foreign1 < Foreign2 ? 1 : (Foreign1 == Foreign2 ? 0 : -1));
").
:- pragma foreign_proc("C#",
    foreign_compare_2(Result::out, Foreign1::in, Foreign2::in),
    [will_not_call_mercury, promise_pure],
"
    Result = (Foreign1 < Foreign2 ? 1 : (Foreign1 == Foreign2 ? 0 : -1));
").
:- pragma foreign_proc("Java",
    foreign_compare_2(Result::out, Foreign1::in, Foreign2::in),
    [will_not_call_mercury, promise_pure],
"
    Result = (Foreign1 < Foreign2 ? 1 : (Foreign1 == Foreign2 ? 0 : -1));
").

:- func foreign(int) = foreign.
:- pragma foreign_proc(c,
    foreign(Int::in) = (Foreign::out),
    [will_not_call_mercury, promise_pure],
"
    Foreign = Int;
").
:- pragma foreign_proc("C#",
    foreign(Int::in) = (Foreign::out),
    [will_not_call_mercury, promise_pure],
"
    Foreign = Int;
").
:- pragma foreign_proc("Java",
    foreign(Int::in) = (Foreign::out),
    [will_not_call_mercury, promise_pure],
"
    Foreign = Int;
").
