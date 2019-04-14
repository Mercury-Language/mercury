%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module parse_tree.parse_error.
:- interface.

:- import_module set.

    % This type represents the kinds of errors that can happen
    % when we (attempt to) read in a Mercury module (which will be
    % either a source file, or an interface file).
    %
:- type read_module_error
    --->    rme_could_not_open_file
            % We could not open the specified file.

    ;       rme_unexpected_module_name
            % The file starts with a module declaration for a module
            % other than the one we expected.

    ;       rme_no_module_decl_at_start
            % The file does not start with a module declaration at all.

    ;       rme_no_section_decl_at_start
            % The module does not start with either an interface or an
            % implementation section marker.

    ;       rme_end_module_not_at_end_of_src
            % The source code of a module has at least one term
            % after the end_module marker for the main module.

    ;       rme_unexpected_term_in_int_or_opt
            % The interface or optimization file of a module has at least one
            % term that is not expected in such a file.

    ;       rme_could_not_read_term
    ;       rme_could_not_parse_item
            % When we attempted to read an item from the file, we got a failure
            % either in the first stage of parsing (reading in a term), or
            % the second stage of parsing (converting that term to an item).
            % Since the rest of the compiler should not care whether parsing
            % is done in one or two stages, it should treat both these errors
            % the same. We distinguish them only for completeness.

    ;       rme_bad_submodule_start
            % We encountered a declaration for the starting of a submodule,
            % but the name of the new submodule is not that can be immediately
            % nested inside the current module.
            % NOTE: This error can happen only for SUBmodules of the main
            % module in the file. For the main module itself, we would generate
            % the rme_unexpected_module_name error.

    ;       rme_bad_module_end.
            % We encountered a declaration for the ending of a module,
            % but the name of the ended module is not the name of the
            % until-then-current module.
            % NOTE: This error can happen both for the main module of the
            % file and for its submodules.

    % This type represents the set of errors that were encountered
    % during an attempt to read in a Mercury module (source file or
    % interface file).
    %
    % There are two kinds of tests that code will typically perform
    % on values of this type.
    %
    % 1. Test whether the set is nonempty. (Are there any errors?)
    % 2. Intersect the set with fatal_read_module_errors, and test whether
    %    the resulting set is nonempty. (Are there any FATAL errors?)
    %
    % In some places, we want just the first test, in some other places,
    % we want just the second, and in yet a third set of places, we want both.
    %
:- type read_module_errors == set(read_module_error).

    % The set of errors that should stop the compiler from further processing
    % of the read-in module.
    %
:- func fatal_read_module_errors = set(read_module_error).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

fatal_read_module_errors = FatalErrors :-
    % 1. We obviously cannot process the module if we could not read it.
    %
    % 2. If the module contains some bad ":- module" declarations, then
    %    the identity of the module that contains any items following
    %    such declarations will be in doubt. The compiler can guess the
    %    identity of the module intended by the programmer only by chance,
    %    and any mistake will typically lead to an avalanche of misleading
    %    errors.
    %
    % 3. If the module contains some bad ":- end_module" declarations,
    %    then the same is true for any items following those declarations.
    %
    % Items follow most ":- module" declarations, but not most ":- end_module"
    % declarations, and if no items follow a bad ":- end_module" declaration,
    % then the badness of the declaration has no further consequences.
    % It would therefore be worthwhile making a distinction between bad
    % ":- end_module" declarations that have items following them, and those
    % that don't. Unfortunately, since each item is processed independently,
    % that is not trivial to do.

    FatalErrors = set.list_to_set([rme_could_not_open_file,
        rme_bad_submodule_start, rme_bad_module_end]).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_error.
%---------------------------------------------------------------------------%
