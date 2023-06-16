%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_may_export_body_helper_1.

:- interface.

:- pred plus(int::in, int::in, int::out) is det.

:- pred cannot_export_plus(int::in, int::in, int::out) is det.

:- implementation.

plus(X, Y, Z) :-
    % This call should be inlined.
    cannot_export_plus(X, Y, Z).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", local, "
    typedef MR_Integer MyInt;
").
:- pragma foreign_decl("C#", local, "
    using System.Collections; // for ArrayList
").
:- pragma foreign_decl("Java", local, "
    import java.util.ArrayList;
").

:- pragma inline(cannot_export_plus/3).

:- pragma foreign_proc("C",
    cannot_export_plus(X::in, Y::in, Z::out),
    [will_not_call_mercury, promise_pure, may_not_export_body],
"
    // Refers to local type MyInt.
    Z = (MyInt) (X + Y);
").

:- pragma foreign_proc("C#",
    cannot_export_plus(X::in, Y::in, Z::out),
    [will_not_call_mercury, promise_pure, may_not_export_body],
"
    // Uses ArrayList without namespace prefix.
    ArrayList arr = new ArrayList();
    arr.Add(X + Y);
    Z = (int) arr[0];
").

:- pragma foreign_proc("Java",
    cannot_export_plus(X::in, Y::in, Z::out),
    [will_not_call_mercury, promise_pure, may_not_export_body],
"
    // Uses ArrayList without package prefix.
    ArrayList<Integer> arr = new ArrayList();
    arr.add(X + Y);
    Z = arr.get(0);
").
