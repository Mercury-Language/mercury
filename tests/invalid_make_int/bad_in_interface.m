%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the effect of the presence in the interface section
% of declarations that should occur only in the implementation section.
%

:- module bad_in_interface.

:- interface.

:- pred ext_foo(int::in, int::out) is det.
:- func ext_bar(int) = int.

:- type ft_t
    --->    ft_a
    ;       ft_b.

:- pred ft_foo(ft_t::in, int::out) is det.
:- func ft_bar(ft_t) = int.
:- pred ft_baz(ft_t::in, int::out) is det.
:- func ft_baz(ft_t) = int.

:- pred fp_foo(int::in, int::out) is det.
:- func fp_bar(int) = int.

% :- implementation.    Missing section marker.

:- pragma external_pred(ext_foo/2).
:- pragma external_func(ext_bar/1).

:- pragma fact_table(pred(ft_foo/2), "fact_table_ft_foo").
:- pragma fact_table(func(ft_bar/1), "fact_table_ft_bar").
:- pragma fact_table(ft_baz/2,       "fact_table_ft_baz").   % Ambiguity error.

:- pragma foreign_proc("C",
    fp_foo(In::in, Out::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Out = In;
").

:- pragma foreign_proc("C",
    fp_bar(In::in) = (Out::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Out = In;
").

:- pragma foreign_enum("C",    ft_t/0, [ft_a - "300", ft_b - "400"]).
:- pragma foreign_enum("Java", ft_t/0, [ft_a - "300", ft_b - "400"]).
:- pragma foreign_enum("C#",   ft_t/0, [ft_a - "300", ft_b - "400"]).
