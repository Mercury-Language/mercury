%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests the readability of the error message we generate
% when the actual and expected types are both higher order, but differ
% in their arity.
%

:- module ho_type_arity_bug.

:- interface.

:- import_module char.
:- import_module list.
:- import_module unit.

:- type option
    --->    option_a
    ;       option_b.

:- type option_ops_userdata(OptionType, UserDataType)
    --->    option_ops_userdata(
                pred(char, OptionType),                     % short_option
                pred(string, OptionType),                   % long_option
                pred(OptionType, unit,                      % special handler
                    list(OptionType), list(OptionType),
                    UserDataType, UserDataType)
            ).

:- pred p(option_ops_userdata(option, unit)::out) is det.

:- implementation.

p(OOU) :-
    OOU = option_ops_userdata(p_short, p_long, p_special).

:- pred p_short(char::in, option::out) is semidet.

p_short('a', option_a).
p_short('b', option_b).

:- pred p_long(string::in, option::out) is semidet.

p_long("a", option_a).
p_long("b", option_b).

:- pred p_special(option::in, unit::in, list(option)::in, list(option)::out)
    is semidet.

p_special(option_a, _, !Options).
