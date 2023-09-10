%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug563.
:- interface.

:- import_module bug563_helper_1.

:- func get_prop(props) = int.

:- implementation.

get_prop(Props) = Props ^ prop_foo.
