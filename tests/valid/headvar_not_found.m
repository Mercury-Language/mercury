:- module headvar_not_found.

:- interface.
:- import_module list.

:- type (mode).
:- type module_info.

:- pred inputs_precede_outputs(list(mode), module_info).
:- mode inputs_precede_outputs(in, in) is semidet.

:- implementation.

:- pred mode_is_input(module_info::in, (mode)::in) is semidet.
:- external(mode_is_input/2).

% succeed iff all the inputs in the list of modes precede the outputs

inputs_precede_outputs([], _).
inputs_precede_outputs([Mode | Modes], ModuleInfo) :-
	( mode_is_input(ModuleInfo, Mode) ->
		inputs_precede_outputs(Modes, ModuleInfo)
	;
		\+ (
			list__member(OtherMode, Modes),
			mode_is_input(ModuleInfo, OtherMode)
		)
	).
