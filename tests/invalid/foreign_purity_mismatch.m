%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .err_exp{,2,3} files are for C, Java and C# grades respectively.
%---------------------------------------------------------------------------%

:- module foreign_purity_mismatch.
:- interface.

:- pred pure_with_impure(string::in) is det.
:- pred pure_with_semipure(string::in) is det.

:- semipure pred semipure_with_impure(string::in) is det.
:- semipure pred semipure_with_pure(string::in) is det.

    % This one was particularly bad since the compiler was
    % optimising away the foreign_proc goal(!).
    %
:- impure pred impure_with_pure(string::in) is det.
:- impure pred impure_with_semipure(string::in) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pure_with_impure(S::in),
    [will_not_call_mercury],
"
    // S
").
:- pragma foreign_proc("Java",
    pure_with_impure(S::in),
    [will_not_call_mercury],
"
    // S
").
:- pragma foreign_proc("C#",
    pure_with_impure(S::in),
    [will_not_call_mercury],
"
    // S
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pure_with_semipure(S::in),
    [will_not_call_mercury, promise_semipure],
"
    // S
").
:- pragma foreign_proc("Java",
    pure_with_semipure(S::in),
    [will_not_call_mercury, promise_semipure],
"
    // S
").
:- pragma foreign_proc("C#",
    pure_with_semipure(S::in),
    [will_not_call_mercury, promise_semipure],
"
    // S
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    semipure_with_impure(S::in),
    [will_not_call_mercury],
"
    // S
").
:- pragma foreign_proc("Java",
    semipure_with_impure(S::in),
    [will_not_call_mercury],
"
    // S
").
:- pragma foreign_proc("C#",
    semipure_with_impure(S::in),
    [will_not_call_mercury],
"
    // S
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    semipure_with_pure(S::in),
    [will_not_call_mercury, promise_pure],
"
    // S
").
:- pragma foreign_proc("Java",
    semipure_with_pure(S::in),
    [will_not_call_mercury, promise_pure],
"
    // S
").
:- pragma foreign_proc("C#",
    semipure_with_pure(S::in),
    [will_not_call_mercury, promise_pure],
"
    // S
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    impure_with_pure(S::in),
    [will_not_call_mercury, promise_pure],
"
    // S
").
:- pragma foreign_proc("Java",
    impure_with_pure(S::in),
    [will_not_call_mercury, promise_pure],
"
    // S
").
:- pragma foreign_proc("C#",
    impure_with_pure(S::in),
    [will_not_call_mercury, promise_pure],
"
    // S
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    impure_with_semipure(S::in),
    [will_not_call_mercury, promise_semipure],
"
    // S
").
:- pragma foreign_proc("Java",
    impure_with_semipure(S::in),
    [will_not_call_mercury, promise_semipure],
"
    // S
").
:- pragma foreign_proc("C#",
    impure_with_semipure(S::in),
    [will_not_call_mercury, promise_semipure],
"
    // S
").

%---------------------------------------------------------------------------%
