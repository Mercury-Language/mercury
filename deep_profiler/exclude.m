%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: zs.
%
% XXX

:- module exclude.

:- interface.

:- import_module profile.
:- import_module std_util, io, set.

:- type exclude_spec
	--->	all_procedures(string)		% Exclude all procedures in the
						% named module.
	;	internal_procedures(string).	% Exclude all procedures in the 
						% named module, except those
						% which are exported from the
						% module.
					

:- pred read_exclude_file(string::in, deep::in,
	maybe_error(set(exclude_spec))::out,
	io__state::di, io__state::uo) is det.

:- func apply_contour_exclusion(deep, set(exclude_spec), call_site_dynamic_ptr)
	= call_site_dynamic_ptr.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, string, list, map, require.

read_exclude_file(FileName, Deep, Res) -->
	io__open_input(FileName, Res0),
	(
		{ Res0 = ok(InputStream) },
		read_exclude_lines(FileName, InputStream, [], Res1),
		io__close_input(InputStream),
		(
			{ Res1 = ok(Specs) },
			{ validate_exclude_lines(FileName, Specs, Deep, Res) }
		;
			{ Res1 = error(Msg) },
			{ Res = error(Msg) }
		)
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		{ Res = error(Msg) }
	).

:- pred read_exclude_lines(string::in, io__input_stream::in,
	list(exclude_spec)::in, maybe_error(list(exclude_spec))::out,
	io__state::di, io__state::uo) is det.

read_exclude_lines(FileName, InputStream, RevSpecs0, Res) -->
	io__read_line_as_string(InputStream, Res0),
	(
		{ Res0 = ok(Line0) },
		{ string__remove_suffix(Line0, "\n", LinePrime) ->
			Line = LinePrime
		;
			Line = Line0
		},
		(
			{ Words = string__words(char__is_whitespace, Line) },
			{ Words = [Scope, ModuleName] },
			{
				Scope = "all",
				Spec = all_procedures(ModuleName)
			;
				Scope = "internal",
				Spec = internal_procedures(ModuleName)
			}
		->
			{ RevSpecs1 = [Spec | RevSpecs0] },
			read_exclude_lines(FileName, InputStream, RevSpecs1,
				Res)
		;
			{ Msg = string__format(
				"file %s contains badly formatted line: %s",
				[s(FileName), s(Line)]) },
			{ Res = error(Msg) }
		)
	;
		{ Res0 = eof },
		{ Res = ok(RevSpecs0) }
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		{ Res = error(Msg) }
	).

:- pred validate_exclude_lines(string::in, list(exclude_spec)::in, deep::in,
	maybe_error(set(exclude_spec))::out) is det.

validate_exclude_lines(FileName, Specs, Deep, Res) :-
	list__filter(has_valid_module_name(Deep), Specs,
		ValidSpecs, InvalidSpecs),
	( InvalidSpecs = [] ->
		set__list_to_set(ValidSpecs, ModuleSpecSet),
		Res = ok(ModuleSpecSet)
	;
		InvalidModuleNames = list__map(spec_to_module_name,
			InvalidSpecs),
		BadNames = string__join_list(", ", InvalidModuleNames),
		Msg = string__format("file %s contains bad module names: %s",
			[s(FileName), s(BadNames)]),
		Res = error(Msg)
	).

:- pred has_valid_module_name(deep::in, exclude_spec::in) is semidet.

has_valid_module_name(Deep, Spec) :-
	map__search(Deep ^ module_data, spec_to_module_name(Spec), _).

:- func spec_to_module_name(exclude_spec) = string.

spec_to_module_name(all_procedures(ModuleName)) = ModuleName.
spec_to_module_name(internal_procedures(ModuleName)) = ModuleName.

%-----------------------------------------------------------------------------%

apply_contour_exclusion(Deep, ExcludedSpec, CSDPtr0) = CSDPtr :-
	( valid_call_site_dynamic_ptr(Deep, CSDPtr0) ->
		deep_lookup_call_site_dynamics(Deep, CSDPtr0, CSD),
		PDPtr = CSD ^ csd_caller,
		deep_lookup_proc_dynamics(Deep, PDPtr, PD),
		PSPtr = PD ^ pd_proc_static,
		deep_lookup_proc_statics(Deep, PSPtr, PS),
		ModuleName = PS ^ ps_decl_module,
		(
			set__member(all_procedures(ModuleName),
				ExcludedSpec)
		->
			deep_lookup_clique_index(Deep, PDPtr, CliquePtr),
			deep_lookup_clique_parents(Deep, CliquePtr,
				EntryCSDPtr),
			CSDPtr = apply_contour_exclusion(Deep, ExcludedSpec,
				EntryCSDPtr)
		;
			set__member(internal_procedures(ModuleName),
				ExcludedSpec),
			PS ^ ps_in_interface = no
		->
			deep_lookup_clique_index(Deep, PDPtr, CliquePtr),
			deep_lookup_clique_parents(Deep, CliquePtr,
				EntryCSDPtr),
			CSDPtr = apply_contour_exclusion(Deep, ExcludedSpec,
				EntryCSDPtr)
		;
			CSDPtr = CSDPtr0
		)
	;
		CSDPtr = CSDPtr0
	).

