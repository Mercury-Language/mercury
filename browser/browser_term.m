%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2012 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: browser_term.m.
%
% This module defines a type to represent both natural and synthetic terms
% for use by the browser.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.browser_term.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module univ.

%---------------------------------------------------------------------------%

:- type browser_term
    --->    plain_term(
                % We are browsing a plain term.
                univ
            )
    ;       synthetic_term(
                % We are browsing a synthetic term, such as a predicate name
                % applied to a list of arguments.

                string,
                % What we should print as the functor.

                list(univ),
                % The arguments.

                maybe(univ)
                % If yes, the synthetic term represents a function call,
                % and the argument inside the yes() is the return value.
            ).

    % This predicate converts a term represented as univ to a browser term.
    %
:- func univ_to_browser_term(univ) = browser_term.

    % This predicate converts a plain term from the representation used
    % in the trace directory to a browser term.
    %
:- func plain_term_to_browser_term(T) = browser_term.

    % This predicate converts a synthetic term from the representation used
    % in the trace directory (as a list of arguments, the last of which
    % represents the return value for function calls) to the representation
    % used in the browser directory, in which a function call's return
    % value is stored separately from the other arguments.
    %
    % The reason why the trace directory does not use the latter representation
    % is that it would require C code to construct values of type maybe(T).
    %
:- func synthetic_term_to_browser_term(string, list(univ), bool)
    = browser_term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_export("C", plain_term_to_browser_term(in) = out,
    "ML_BROWSE_plain_term_to_browser_term").
:- pragma foreign_export("C", univ_to_browser_term(in) = out,
    "ML_BROWSE_univ_to_browser_term").
:- pragma foreign_export("C", synthetic_term_to_browser_term(in, in, in) = out,
    "ML_BROWSE_synthetic_term_to_browser_term").

univ_to_browser_term(Univ) = plain_term(Univ).

plain_term_to_browser_term(Term) = plain_term(univ(Term)).

synthetic_term_to_browser_term(FunctorString, Args, IsFunc) = BrowserTerm :-
    (
        IsFunc = no,
        BrowserTerm = synthetic_term(FunctorString, Args, no)
    ;
        IsFunc = yes,
        list.det_split_last(Args, FuncArgs, Return),
        BrowserTerm = synthetic_term(FunctorString, FuncArgs, yes(Return))
    ).
