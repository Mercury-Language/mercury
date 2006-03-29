%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
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

%-----------------------------------------------------------------------------%

:- module exclude.

:- interface.

:- import_module profile.

:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type exclude_file.

:- pred read_exclude_file(string::in, deep::in, maybe_error(exclude_file)::out,
    io::di, io::uo) is det.

:- func apply_contour_exclusion(deep, exclude_file, call_site_dynamic_ptr)
    = call_site_dynamic_ptr.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type exclude_file == set(exclude_spec).

:- type exclude_spec
    --->    exclude_spec(
                string,         % the name of a module
                exclusion_type  % which procedures in the module
                                % are excluded
           ).

:- type exclusion_type
    --->    all_procedures      % Exclude all procedures in the
                                % named module.
    ;       internal_procedures.    % Exclude all procedures in the
                                    % named module, except those
                                    % which are exported from the
                                    % module.

%-----------------------------------------------------------------------------%

read_exclude_file(FileName, Deep, Res, !IO) :-
    io.open_input(FileName, Res0, !IO),
    (
        Res0 = ok(InputStream),
        read_exclude_lines(FileName, InputStream, [], Res1, !IO),
        io.close_input(InputStream, !IO),
        (
            Res1 = ok(Specs),
            validate_exclude_lines(FileName, Specs, Deep, Res)
        ;
            Res1 = error(Msg),
            Res = error(Msg)
        )
    ;
        Res0 = error(Err),
        io.error_message(Err, Msg),
        Res = error(Msg)
    ).

:- pred read_exclude_lines(string::in, io.input_stream::in,
    list(exclude_spec)::in, maybe_error(list(exclude_spec))::out,
    io::di, io::uo) is det.

read_exclude_lines(FileName, InputStream, RevSpecs0, Res, !IO) :-
    io.read_line_as_string(InputStream, Res0, !IO),
    (
        Res0 = ok(Line0),
        ( string.remove_suffix(Line0, "\n", LinePrime) ->
            Line = LinePrime
        ;
            Line = Line0
        ),
        (
            Words = string.words(char.is_whitespace, Line),
            Words = [Scope, ModuleName],
            (
                Scope = "all",
                ExclType = all_procedures
            ;
                Scope = "internal",
                ExclType = internal_procedures
            )
        ->
            Spec = exclude_spec(ModuleName, ExclType),
            RevSpecs1 = [Spec | RevSpecs0],
            read_exclude_lines(FileName, InputStream, RevSpecs1, Res, !IO)
        ;
            Msg = string.format(
                "file %s contains badly formatted line: %s",
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

:- pred validate_exclude_lines(string::in, list(exclude_spec)::in, deep::in,
    maybe_error(set(exclude_spec))::out) is det.

validate_exclude_lines(FileName, Specs, Deep, Res) :-
    list.filter(has_valid_module_name(Deep), Specs, ValidSpecs, InvalidSpecs),
    (
        InvalidSpecs = [],
        set.list_to_set(ValidSpecs, ModuleSpecSet),
        Res = ok(ModuleSpecSet)
    ;
        InvalidSpecs = [_ | _],
        InvalidModuleNames = list.map(spec_to_module_name, InvalidSpecs),
        BadNames = string.join_list(", ", InvalidModuleNames),
        Msg = string.format("file %s contains bad module names: %s",
            [s(FileName), s(BadNames)]),
        Res = error(Msg)
    ).

:- pred has_valid_module_name(deep::in, exclude_spec::in) is semidet.

has_valid_module_name(Deep, Spec) :-
    map.search(Deep ^ module_data, spec_to_module_name(Spec), _).

:- func spec_to_module_name(exclude_spec) = string.

spec_to_module_name(exclude_spec(ModuleName, _)) = ModuleName.

%-----------------------------------------------------------------------------%

apply_contour_exclusion(Deep, ExcludedSpecs, CSDPtr0) = CSDPtr :-
    ( valid_call_site_dynamic_ptr(Deep, CSDPtr0) ->
        deep_lookup_call_site_dynamics(Deep, CSDPtr0, CSD),
        PDPtr = CSD ^ csd_caller,
        deep_lookup_proc_dynamics(Deep, PDPtr, PD),
        PSPtr = PD ^ pd_proc_static,
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        ModuleName = PS ^ ps_decl_module,
        (
            set.member(ExclSpec, ExcludedSpecs),
            ExclSpec = exclude_spec(ModuleName, ExclType),
            (
                ExclType = all_procedures
            ;
                ExclType = internal_procedures,
                PS ^ ps_in_interface = no
            )
        ->
            deep_lookup_clique_index(Deep, PDPtr, CliquePtr),
            deep_lookup_clique_parents(Deep, CliquePtr, EntryCSDPtr),
            CSDPtr = apply_contour_exclusion(Deep, ExcludedSpecs, EntryCSDPtr)
        ;
            CSDPtr = CSDPtr0
        )
    ;
        CSDPtr = CSDPtr0
    ).

%-----------------------------------------------------------------------------%
:- end_module exclude.
%-----------------------------------------------------------------------------%
