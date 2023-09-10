%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test. The compiler aborted when making the optimisation interface
% file for this module when --intermod-unused-args is enabled.
%
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%     Key Type: term.var(parse_tree.prog_data.tvar_type)
%     Key Value: var(1)
%     Value Type: hlds.hlds_rtti.type_info_locn
%
% This test was originally called intermod_us_type_spec.
%

:- module unused_args_type_spec.
:- interface.

:- type tt(T)
    --->    tt.

:- typeclass tc(T) where [].
:- instance tc(tt(_)).

:- func myfunc(T) = int <= tc(T).
:- pragma type_spec(myfunc/1, T = tt(_)).

:- implementation.

:- instance tc(tt(_)) where [].

myfunc(_) = 0.
