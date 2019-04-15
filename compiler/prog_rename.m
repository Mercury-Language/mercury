%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% The predicates here are similar to the "apply_variable_renaming" family of
% predicates in library/term.m, but they allow the caller to specify that all
% variables in the data structure being updated must appear in the renaming.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_rename.

:- interface.

:- import_module parse_tree.set_of_var.

:- import_module list.
:- import_module map.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%

:- type must_rename
    --->    must_rename
    ;       need_not_rename.

:- inst must_rename for must_rename/0
    --->    must_rename.
:- inst need_not_rename for must_rename/0
    --->    need_not_rename.

:- pred rename_vars_in_term(must_rename, map(var(V), var(V)),
    term(V), term(V)).
:- mode rename_vars_in_term(in(must_rename), in, in, out) is det.
:- mode rename_vars_in_term(in(need_not_rename), in, in, out) is det.
:- mode rename_vars_in_term(in, in, in, out) is det.

:- pred rename_vars_in_term_list(must_rename, map(var(V), var(V)),
    list(term(V)), list(term(V))).
:- mode rename_vars_in_term_list(in(must_rename), in, in, out) is det.
:- mode rename_vars_in_term_list(in(need_not_rename), in, in, out) is det.
:- mode rename_vars_in_term_list(in, in, in, out) is det.

:- pred rename_vars_in_var_set(must_rename, map(var(V), var(V)),
    set(var(V)), set(var(V))).
:- mode rename_vars_in_var_set(in(must_rename), in, in, out) is det.
:- mode rename_vars_in_var_set(in(need_not_rename), in, in, out) is det.
:- mode rename_vars_in_var_set(in, in, in, out) is det.

:- pred rename_vars_in_set_of_var(must_rename, map(var(V), var(V)),
    set_of_var(V), set_of_var(V)).
:- mode rename_vars_in_set_of_var(in(must_rename), in, in, out) is det.
:- mode rename_vars_in_set_of_var(in(need_not_rename), in, in, out) is det.
:- mode rename_vars_in_set_of_var(in, in, in, out) is det.

:- pred rename_var_list(must_rename, map(var(V), var(V)),
    list(var(V)), list(var(V))).
:- mode rename_var_list(in(must_rename), in, in, out) is det.
:- mode rename_var_list(in(need_not_rename), in, in, out) is det.
:- mode rename_var_list(in, in, in, out) is det.

:- pred rename_var(must_rename, map(var(V), var(V)),
    var(V), var(V)).
:- mode rename_var(in(must_rename), in, in, out) is det.
:- mode rename_var(in(need_not_rename), in, in, out) is det.
:- mode rename_var(in, in, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

rename_vars_in_term(Must, Renaming, Term0, Term) :-
    (
        Term0 = variable(Var0, Context),
        rename_var(Must, Renaming, Var0, Var),
        Term = variable(Var, Context)
    ;
        Term0 = functor(ConsId, Args0, Context),
        % The mutual non-tail recursion between rename_vars_in_term and
        % rename_vars_in_term_list means that when given a large Term0,
        % this predicate may need a LOT of stack frames, and may even run
        % the program out of stack.
        %
        % To try to prevent this in as many cases as possible (though
        % unfortunately not all), we handle the first three arguments
        % directly. The most common kind of very large term is a very
        % long list, and with this approach, we use only one stack frame
        % per list element, not two.
        (
            Args0 = [],
            Args = []
        ;
            Args0 = [Arg1Term0],
            rename_vars_in_term(Must, Renaming, Arg1Term0, Arg1Term),
            Args = [Arg1Term]
        ;
            Args0 = [Arg1Term0, Arg2Term0],
            rename_vars_in_term(Must, Renaming, Arg1Term0, Arg1Term),
            rename_vars_in_term(Must, Renaming, Arg2Term0, Arg2Term),
            Args = [Arg1Term, Arg2Term]
        ;
            Args0 = [Arg1Term0, Arg2Term0, Arg3Term0 | OtherArgTerms0],
            rename_vars_in_term(Must, Renaming, Arg1Term0, Arg1Term),
            rename_vars_in_term(Must, Renaming, Arg2Term0, Arg2Term),
            rename_vars_in_term(Must, Renaming, Arg3Term0, Arg3Term),
            rename_vars_in_term_list(Must, Renaming,
                OtherArgTerms0, OtherArgTerms),
            Args = [Arg1Term, Arg2Term, Arg3Term | OtherArgTerms]
        ),
        Term = functor(ConsId, Args, Context)
    ).

rename_vars_in_term_list(_Must, _Renaming, [], []).
rename_vars_in_term_list(Must, Renaming, [Term0 | Terms0], [Term | Terms]) :-
    rename_vars_in_term(Must, Renaming, Term0, Term),
    rename_vars_in_term_list(Must, Renaming, Terms0, Terms).

rename_vars_in_var_set(Must, Renaming, Vars0, Vars) :-
    set.to_sorted_list(Vars0, VarsList0),
    rename_var_list(Must, Renaming, VarsList0, VarsList),
    set.list_to_set(VarsList, Vars).

rename_vars_in_set_of_var(Must, Renaming, Vars0, Vars) :-
    set_of_var.to_sorted_list(Vars0, VarsList0),
    rename_var_list(Must, Renaming, VarsList0, VarsList),
    set_of_var.list_to_set(VarsList, Vars).

rename_var_list(_Must, _Renaming, [], []).
rename_var_list(Must, Renaming, [Var0 | Vars0], [Var | Vars]) :-
    rename_var(Must, Renaming, Var0, Var),
    rename_var_list(Must, Renaming, Vars0, Vars).

rename_var(Must, Renaming, Var0, Var) :-
    ( if map.search(Renaming, Var0, VarPrime) then
        Var = VarPrime
    else
        (
            Must = need_not_rename,
            Var = Var0
        ;
            Must = must_rename,
            term.var_to_int(Var0, Var0Int),
            string.format("rename_var: no substitute for var %i",
                [i(Var0Int)], Msg),
            unexpected($pred, Msg)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_rename.
%---------------------------------------------------------------------------%
