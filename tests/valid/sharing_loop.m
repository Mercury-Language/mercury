% Regression test.  The structure sharing analysis wasn't able to reach a
% fixpoint analysing this module with --structure-sharing-widening set to
% certain values.

:- module sharing_loop.
:- interface.

:- type elds_expr
    --->    elds_term(var)
    ;       elds_fun(var)
    ;       elds_case_expr(var)
    ;       elds_try(elds_catch).

:- type elds_catch
    --->    elds_catch(var, elds_expr). 

:- type var
    --->    var(int).

:- type renaming
    --->    no
    ;       yes(var).

:- pred erl_rename_vars_in_expr(renaming::in,
    elds_expr::in, elds_expr::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

erl_rename_vars_in_expr(Subn, Expr0, Expr) :-
    (
        Expr0 = elds_term(Term0),
        erl_rename_vars_in_term(Subn, Term0, Term),
        Expr = elds_term(Term)
    ;
        Expr0 = elds_fun(Clause0),
        erl_rename_vars_in_term(Subn, Clause0, Clause),
        Expr = elds_fun(Clause)
    ;
        Expr0 = elds_case_expr(Cases),
        Expr = elds_case_expr(Cases)
    ;
        Expr0 = elds_try(Catch0),
        erl_rename_vars_in_catch(Subn, Catch0, Catch),
        Expr = elds_try(Catch)
    ).

:- pred erl_rename_vars_in_term(renaming::in,
    var::in, var::out) is det.

erl_rename_vars_in_term(no, Var, Var).
erl_rename_vars_in_term(yes(Var), _, Var).

:- pred erl_rename_vars_in_catch(renaming::in,
    elds_catch::in, elds_catch::out) is det.

erl_rename_vars_in_catch(Subn, Catch0, Catch) :-
    Catch0 = elds_catch(Pattern, Expr0),
    erl_rename_vars_in_expr(Subn, Expr0, Expr),
    Catch = elds_catch(Pattern, Expr).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
