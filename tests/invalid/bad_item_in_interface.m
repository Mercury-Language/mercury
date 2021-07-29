%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .err_exp2 is for grades that don't support pragma memo.

:- module bad_item_in_interface.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    hello(!IO).

:- pred hello(io::di, io::uo) is det.

% If this section marker is commented out, this module should compile cleanly.
% That way, we know that we get all the resulting error messages purely
% because the presence of the following items in the interface section,
% instead of the implementation section.

:- interface.

hello(!IO) :-
    io.write_string("Hello, world.\n", !IO).

:- pragma foreign_decl("C",
"
    extern int example_x;
").

:- pragma foreign_code("C",
"
    int example_x = 0;
").

:- pragma foreign_export("C", hello(di, uo), "hello_for_c").

:- type e
    --->    e1
    ;       e2.

:- pragma foreign_export_enum("C", e/0, [prefix("PREFIX_")]).

:- pragma inline(hello/2).
:- pragma no_inline(main/2).
:- pragma consider_used(hello/2).
:- pragma no_determinism_warning(hello/2).

:- pred ft_example(int::in, int::out) is semidet.
:- pragma fact_table(ft_example/2, "ft_examples").
:- pragma memo(ft_example/2).
:- impure pred imp1(int::out) is det.
:- impure pred imp2(int::out) is det.
:- pragma external_pred(imp1/1).
:- pragma external_pred(imp2/1).
:- pragma promise_pure(imp1/1).
:- pragma promise_semipure(imp2/1).
:- pragma promise_equivalent_clauses(hello/2).
:- pragma require_feature_set([memo]).

:- pragma foreign_proc("C",
    hello(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    printf(""Hello, world.\\n"");
").

:- mutable(x, int, 0, ground, [untrailed]).
:- initialise hello/2.
:- finalise hello/2.
