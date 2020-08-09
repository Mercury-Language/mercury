%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_user_sharing_2.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- type myarray(T).

:- pred p_no_sharing(io::di, io::uo) is det.
:- pred p_unknown_sharing(T::in, T::out) is det.
:- pred p_sharing(int::in, T::in, myarray(T)::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("C", myarray(T), "MR_Word").
:- pragma foreign_type("C#", myarray(T), "int").
:- pragma foreign_type("Java", myarray(T), "Integer").

:- pragma foreign_proc("C",
    p_no_sharing(IO0::di, IO::uo),
    [promise_pure, no_sharing],
"
    IO = IO0;
").

:- pragma foreign_proc("C#",
    p_no_sharing(IO0::di, IO::uo),
    [promise_pure, no_sharing],
"
    IO = IO0;
").

:- pragma foreign_proc("Java",
    p_no_sharing(IO0::di, IO::uo),
    [promise_pure, no_sharing],
"
    IO = IO0;
").

%---------------------%

:- pragma foreign_proc("C",
    p_unknown_sharing(T0::in, T::out),
    [promise_pure, unknown_sharing],
"
    T = T0;
").

:- pragma foreign_proc("C#",
    p_unknown_sharing(T0::in, T::out),
    [promise_pure, unknown_sharing],
"
    T = T0;
").

:- pragma foreign_proc("Java",
    p_unknown_sharing(T0::in, T::out),
    [promise_pure, unknown_sharing],
"
    T = T0;
").

%---------------------%

:- pragma foreign_proc("C",
    p_sharing(_Size::in, _Item::in, Array::uo),
    [promise_pure,
        sharing(yes(int, T, myarray(T)), [cel(Item, []) - cel(Array, [T])])],
"
    /* dummy */
    Array = 0;
").

:- pragma foreign_proc("C#",
    p_sharing(_Size::in, _Item::in, Array::uo),
    [promise_pure,
        sharing(yes(int, T, myarray(T)), [cel(Item, []) - cel(Array, [T])])],
"
    /* dummy */
    Array = 0;
").

:- pragma foreign_proc("Java",
    p_sharing(_Size::in, _Item::in, Array::uo),
    [promise_pure,
        sharing(yes(int, T, myarray(T)), [cel(Item, []) - cel(Array, [T])])],
"
    /* dummy */
    Array = 0;
").

%---------------------------------------------------------------------------%
