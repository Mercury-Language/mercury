:- module bug563.
:- interface.

:- import_module bug563_helper_1.

:- func get_prop(props) = int.

:- implementation.

get_prop(Props) = Props ^ prop_foo.
