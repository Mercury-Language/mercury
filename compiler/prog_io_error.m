%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
:- module parse_tree.prog_io_error.
:- interface.

% This type gets its own module because its definition should be replaced
% with one that contains significantly more detail, and that type will need
% its own utility predicates.
%
% The definition I (zs) am thinking of is a record that has one boolean
% for every different kind of error we can encounter when reading in
% the contents of a Mercury source file (.m, .int, .int2 etc).
% 
% Different parts of the compiler care about different subsets of these
% errors. Each will need its own utility predicate that tests for the
% kinds of errors it cares about, and ignores the rest.

:- type module_error
    --->    no_module_errors        % no errors
    ;       some_module_errors      % some syntax errors
    ;       fatal_module_errors.    % couldn't open the file

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_error.
%-----------------------------------------------------------------------------%
