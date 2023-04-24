%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001, 2004-2006, 2008-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: exclude.m.
% Author: zs.
%
% This module handles contour exclusion.
%
% When asking for the list of callers of a procedure, the user may ask for
% contour exclusion to be applied. This means that instead of deriving the
% displayed information from the immediate callers of the specified procedure,
% the system uses the nearest ancestors of the specified procedure that are not
% excluded. Contour exclusion draws a line through the program, with the
% excluded procedures below the line and the non-excluded procedures above;
% and when looking at the callers of a procedure, the system uses the nearest
% ancestor that is above the line.
%
% The read_exclude_file procedure reads in the file that specifies the set of
% excluded procedures. The exclusion file should consist of a sequence of
% lines, and each line should contain two words. The first word should be
% either "all" or "internal"; the second should the name of a module.
% If the first word is "all", then all procedures in the named module are
% excluded; if the first word is "internal", then all internal (non-exported)
% procedures in the named module are excluded. The first argument of
% read_exclude_file specifies the name of the exclusion file; the second is the
% main profiler data structure, which read_exclude_file uses to verify the
% existence of the named modules. Read_exclude_file returns an abstract data
% structure representing the contents of the file, provided that the file
% exists and has the proper structure.
%
% The only thing the caller can do with that abstract data structure is give it
% back to apply_contour_exclusion. This predicate applies contour exclusion by
% following the ancestors of the supplied call_site_dynamic_ptr until it
% arrives at a procedure which is not excluded.

%---------------------------------------------------------------------------%

:- module exclude.
:- interface.

:- import_module profile.

:- import_module io.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type exclude_file
    --->    exclude_file(
                exclude_filename        :: string,
                exclude_file_contents   :: exclude_contents
            ).

:- type exclude_contents
    --->    no_exclude_file
    ;       unreadable_exclude_file(
                exclude_syntax_error    :: string
            )
    ;       readable_exclude_file(
                exclude_specs           :: excluded_modules,
                exclude_maybe_error     :: maybe(string)
            ).

:- type excluded_modules.

:- pred read_exclude_file(string::in, map(string, module_data)::in,
    exclude_file::out, io::di, io::uo) is det.

:- func apply_contour_exclusion(deep, excluded_modules, call_site_dynamic_ptr)
    = call_site_dynamic_ptr.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

:- type excluded_modules == set(exclude_spec).

:- type exclude_spec
    --->    exclude_spec(
                string,         % the name of a module
                exclusion_type  % which procedures in the module
                                % are excluded
           ).

:- type exclusion_type
    --->    exclude_all_procedures
            % Exclude all procedures in the named module.
    ;       exclude_internal_procedures.
            % Exclude all procedures in the named module, except those
            % which are exported from the module.

%---------------------------------------------------------------------------%

read_exclude_file(FileName, ModuleDataMap, ExcludeFile, !IO) :-
    io.open_input(FileName, MaybeStream, !IO),
    (
        MaybeStream = ok(InputStream),
        read_exclude_lines(FileName, InputStream, [], MaybeSpecs, !IO),
        io.close_input(InputStream, !IO),
        (
            MaybeSpecs = ok(Specs),
            validate_exclude_lines(FileName, Specs, ModuleDataMap,
                ExcludeContents)
        ;
            MaybeSpecs = error(Msg),
            ExcludeContents = unreadable_exclude_file(Msg)
        )
    ;
        MaybeStream = error(_),
        % If we cannot open the file, simply return `no' as an indication
        % that there is no exclude file there, at least not a readable one.
        ExcludeContents = no_exclude_file
    ),
    ExcludeFile = exclude_file(FileName, ExcludeContents).

:- pred read_exclude_lines(string::in, io.text_input_stream::in,
    list(exclude_spec)::in, maybe_error(list(exclude_spec))::out,
    io::di, io::uo) is det.

read_exclude_lines(FileName, InputStream, RevSpecs0, Res, !IO) :-
    io.read_line_as_string(InputStream, Res0, !IO),
    (
        Res0 = ok(Line0),
        ( if string.remove_suffix(Line0, "\n", LinePrime) then
            Line = LinePrime
        else
            Line = Line0
        ),
        ( if
            Words = string.words_separator(char.is_whitespace, Line),
            Words = [Scope, ModuleName],
            (
                Scope = "all",
                ExclType = exclude_all_procedures
            ;
                Scope = "internal",
                ExclType = exclude_internal_procedures
            )
        then
            Spec = exclude_spec(ModuleName, ExclType),
            RevSpecs1 = [Spec | RevSpecs0],
            read_exclude_lines(FileName, InputStream, RevSpecs1, Res, !IO)
        else
            Msg = string.format("file %s contains badly formatted line: %s",
                [s(FileName), s(Line)]),
            Res = error(Msg)
        )
    ;
        Res0 = eof,
        Res = ok(RevSpecs0)
    ;
        Res0 = error(Err),
        io.error_message(Err, Msg),
        Res = error(Msg)
    ).

:- pred validate_exclude_lines(string::in, list(exclude_spec)::in,
    map(string, module_data)::in, exclude_contents::out) is det.

validate_exclude_lines(FileName, Specs, ModuleDataMap, ExcludeContents) :-
    list.filter(has_valid_module_name(ModuleDataMap), Specs,
        ValidSpecs, InvalidSpecs),
    set.list_to_set(ValidSpecs, ModuleSpecs),
    (
        InvalidSpecs = [],
        MaybeErrorMsg = no,
        ExcludeContents = readable_exclude_file(ModuleSpecs, MaybeErrorMsg)
    ;
        InvalidSpecs = [_ | _],
        (
            ValidSpecs = [_ | _],
            InvalidModuleNames = list.map(spec_to_module_name, InvalidSpecs),
            BadNames = string.join_list(", ", InvalidModuleNames),
            Msg1 = string.format(
                "Warning: %s contains unrecognized module names: %s.",
                [s(FileName), s(BadNames)]),
            Msg2 = "These modules either do not exist " ++
                "or have no deep profiled procedures.",
            Msg = Msg1 ++ Msg2,
            MaybeErrorMsg = yes(Msg),
            ExcludeContents = readable_exclude_file(ModuleSpecs, MaybeErrorMsg)
        ;
            ValidSpecs = [],
            Msg1 = string.format(
                "Error: file %s contains only unrecognized module names.",
                [s(FileName)]),
            Msg2 = "These modules either do not exist " ++
                "or have no deep profiled procedures.",
            Msg = Msg1 ++ Msg2,
            ExcludeContents = unreadable_exclude_file(Msg)
        )
    ).

:- pred has_valid_module_name(map(string, module_data)::in, exclude_spec::in)
    is semidet.

has_valid_module_name(ModuleDataMap, Spec) :-
    map.search(ModuleDataMap, spec_to_module_name(Spec), _).

:- func spec_to_module_name(exclude_spec) = string.

spec_to_module_name(exclude_spec(ModuleName, _)) = ModuleName.

%---------------------------------------------------------------------------%

apply_contour_exclusion(Deep, ExcludedSpecs, CSDPtr0) = CSDPtr :-
    ( if valid_call_site_dynamic_ptr(Deep, CSDPtr0) then
        deep_lookup_call_site_dynamics(Deep, CSDPtr0, CSD),
        PDPtr = CSD ^ csd_caller,
        deep_lookup_proc_dynamics(Deep, PDPtr, PD),
        PSPtr = PD ^ pd_proc_static,
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        ModuleName = PS ^ ps_decl_module,
        ( if
            set.member(ExclSpec, ExcludedSpecs),
            ExclSpec = exclude_spec(ModuleName, ExclType),
            (
                ExclType = exclude_all_procedures
            ;
                ExclType = exclude_internal_procedures,
                PS ^ ps_in_interface = no
            )
        then
            deep_lookup_clique_index(Deep, PDPtr, CliquePtr),
            deep_lookup_clique_parents(Deep, CliquePtr, EntryCSDPtr),
            CSDPtr = apply_contour_exclusion(Deep, ExcludedSpecs, EntryCSDPtr)
        else
            CSDPtr = CSDPtr0
        )
    else
        CSDPtr = CSDPtr0
    ).

%---------------------------------------------------------------------------%
:- end_module exclude.
%---------------------------------------------------------------------------%
