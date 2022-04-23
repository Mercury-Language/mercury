%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module parse_tree.parse_error.
:- interface.

:- import_module parse_tree.error_util.

:- import_module list.
:- import_module set.

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
    %
:- type fatal_read_module_error
    --->    frme_could_not_open_file
            % We could not open the specified file.

    ;       frme_could_not_read_file
            % We could open the specified file, but could not read its
            % contents.

    ;       frme_bad_submodule_start
            % We encountered a declaration for the start of a submodule,
            % but the name of the new submodule is not that can be immediately
            % nested inside the current module.
            % NOTE: This error can happen only for submodules of the main
            % module in the file. For the main module itself, we would generate
            % the rme_unexpected_module_name error.

    ;       frme_bad_module_end.
            % We encountered a declaration for the end of a module,
            % but the name of the ended module is not the name of the
            % until-then-current module.
            % NOTE: This error can happen both for the main module of the
            % file and for its submodules.

    % This type represents the kinds of nonfatal errors that can happen
    % when we read in a Mercury module (which will be either a source file,
    % or an interface file).
    %
:- type nonfatal_read_module_error
    --->    rme_unexpected_module_name
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

    ;       rme_nec.
            % A read module error that is Not Elsewhere Classified, i.e.
            % is not one of the error kinds listed above.
            %
            % Before the change away from read_module_errors being just a set
            % of read_module_error values, we used to not include these errors
            % in the set at all, which could lead to an empty set of
            % read_module_error values being paired with a nonempty list
            % of severity_error error_specs.

    % This type represents the set of errors that were encountered
    % during an attempt to read in a Mercury module's source file,
    % interface file, or optimization file.
    %
    % There are two kinds of tests that code will typically perform
    % on values of this type.
    %
    % 1. Are there any errors?
    % 2. Are there any FATAL errors?
    %
    % You can use there_are_no_errors and there_are_some_errors for
    % the first kind of test, and a direct invocation of an emptiness test
    % on the rm_fatal_errors field for the second kind.
    %
:- type read_module_errors
    --->    read_module_errors(
                % The fatal errors we have encountered, and their messages.
                % All these error_specs should have severity_error.
                rm_fatal_errors         :: set(fatal_read_module_error),
                rm_fatal_error_specs    :: list(error_spec),

                % The nonfatal errors we have encountered, and their messages.
                % All these error_specs should have severity_error.
                rm_nonfatal_errors      :: set(nonfatal_read_module_error),
                rm_nonfatal_error_specs :: list(error_spec),

                % The warnings we have encountered. All these should have
                % severity levels *below* severity_error.
                rm_warning_specs        :: list(error_spec)
            ).

%---------------------%

    % Return a structure that records no errors (so far).
    %
:- func init_read_module_errors = read_module_errors.

%---------------------%

    % Add a fatal error, and its message.
    %
:- pred add_fatal_error(fatal_read_module_error::in, list(error_spec)::in,
    read_module_errors::in, read_module_errors::out) is det.

    % Add a nonfatal error, and its message.
    %
:- pred add_nonfatal_error(nonfatal_read_module_error::in,
    list(error_spec)::in,
    read_module_errors::in, read_module_errors::out) is det.

    % If there are any error_specs in the input list,
    % record them as representing a not-elsewhere-classified nonfatal error.
    %
:- pred add_any_nec_errors(list(error_spec)::in,
    read_module_errors::in, read_module_errors::out) is det.

    % Add some warning messages.
    %
:- pred add_warning(list(error_spec)::in,
    read_module_errors::in, read_module_errors::out) is det.

%---------------------%

    % Succeed if the read_module_errors structure records zero errors.
    % (Though it may contain warnings.)
    %
:- pred there_are_no_errors(read_module_errors::in) is semidet.

    % Succeed if the read_module_errors structure records at least one error.
    %
:- pred there_are_some_errors(read_module_errors::in) is semidet.

%---------------------%

    % Return all the error_specs in the argument, regardless of severity.
    %
:- func get_read_module_specs(read_module_errors) = list(error_spec).

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

init_read_module_errors = read_module_errors(set.init, [], set.init, [], []).

%---------------------%

add_fatal_error(Error, Specs, Errors0, Errors) :-
    Errors0 = read_module_errors(FatalErrors0, FatalSpecs0,
        NonFatalErrors, NonFatalSpecs, WarningSpecs),
    set.insert(Error, FatalErrors0, FatalErrors),
    FatalSpecs = Specs ++  FatalSpecs0,
    Errors = read_module_errors(FatalErrors, FatalSpecs,
        NonFatalErrors, NonFatalSpecs, WarningSpecs).

add_nonfatal_error(Error, Specs, Errors0, Errors) :-
    Errors0 = read_module_errors(FatalErrors, FatalSpecs,
        NonFatalErrors0, NonFatalSpecs0, WarningSpecs),
    set.insert(Error, NonFatalErrors0, NonFatalErrors),
    NonFatalSpecs = Specs ++ NonFatalSpecs0,
    Errors = read_module_errors(FatalErrors, FatalSpecs,
        NonFatalErrors, NonFatalSpecs, WarningSpecs).

add_any_nec_errors(Specs, !Errors) :-
    (
        Specs = []
    ;
        Specs = [_ | _],
        add_nonfatal_error(rme_nec, Specs, !Errors)
    ).

add_warning(Specs, Errors0, Errors) :-
    Errors0 = read_module_errors(FatalErrors, FatalSpecs,
        NonFatalErrors, NonFatalSpecs, WarningSpecs0),
    WarningSpecs = Specs ++ WarningSpecs0,
    Errors = read_module_errors(FatalErrors, FatalSpecs,
        NonFatalErrors, NonFatalSpecs, WarningSpecs).

%---------------------%

there_are_no_errors(Errors) :-
    Errors = read_module_errors(FatalErrors, _, NonFatalErrors, _, _),
    set.is_empty(FatalErrors),
    set.is_empty(NonFatalErrors).

there_are_some_errors(Errors) :-
    Errors = read_module_errors(FatalErrors, _, NonFatalErrors, _, _),
    ( set.is_non_empty(FatalErrors)
    ; set.is_non_empty(NonFatalErrors)
    ).

%---------------------%

get_read_module_specs(Errors) = Specs :-
    Errors = read_module_errors(_FatalErrors, FatalSpecs,
        _NonFatalErrors, NonFatalSpecs, WarningSpecs),
    Specs = FatalSpecs ++ NonFatalSpecs ++ WarningSpecs.

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_error.
%---------------------------------------------------------------------------%
