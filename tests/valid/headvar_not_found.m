%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module headvar_not_found.

:- interface.
:- import_module list.

:- type xmode.
:- type module_info.

    % Succeed iff all the inputs in the list of modes precede the outputs.
    %
:- pred inputs_precede_outputs(list(xmode)::in, module_info::in) is semidet.

:- implementation.

% We need dummy definitions of these types.
:- type xmode
    --->    xmode(int, int).
:- type module_info
    --->    module_info(int, int).

inputs_precede_outputs([], _).
inputs_precede_outputs([Mode | Modes], ModuleInfo) :-
    ( if mode_is_input(ModuleInfo, Mode) then
        inputs_precede_outputs(Modes, ModuleInfo)
    else
        not (
            list.member(OtherMode, Modes),
            mode_is_input(ModuleInfo, OtherMode)
        )
    ).

:- pred mode_is_input(module_info::in, xmode::in) is semidet.

mode_is_input(_, _) :-
    semidet_true.
