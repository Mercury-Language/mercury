%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module disabled_warning.
:- interface.

:- pred p1(string::in, int::in, string::out) is det.
:- pred p2(string::in, int::in, string::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

p1(FormatStr, N, Str) :-
    disable_warning [unknown_format_calls] (
        % The warning for this call should be disabled by the scope.
        string.format(FormatStr, [i(N)], Str)
    ).

p2(FormatStr, N, Str) :-
    % We should get a warning for this call.
    string.format(FormatStr, [i(N)], Str).
