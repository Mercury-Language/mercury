%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000,2003-2009,2011-2012 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_context.m.
% Main author: fjh.
% Stability: medium.
%
% This file contains a type, term_context, that describes where each part
% of a term occurs in a file, and the operations associated with it.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term_context.
:- interface.

%---------------------------------------------------------------------------%

:- type term_context
    --->    context(
                context_filename    :: string,
                context_linenumber  :: int
            ).

    % Initialize the term context when reading in (or otherwise constructing)
    % a term.
    %
:- func context_init(string, int) = term_context.

    % Return a dummy term context.
    %
:- func dummy_context = term_context.

    % Is the given context a dummy context, as returned by dummy_context_init?
    %
:- pred is_dummy_context(term_context::in) is semidet.

    % Given a term context, return the source file.
    %
:- func context_file(term_context) = string.

    % Given a term context, return the source line number.
    %
:- func context_line(term_context) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

context_init(File, LineNumber) = context(File, LineNumber).

dummy_context = context("", 0).

is_dummy_context(Context) :-
    Context = dummy_context.

context_file(context(FileName, _)) = FileName.

context_line(context(_, LineNumber)) = LineNumber.

%---------------------------------------------------------------------------%
:- end_module term_context.
%---------------------------------------------------------------------------%
