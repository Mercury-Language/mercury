% This module is invalid, because there is no `include_module' declaration
% for it in parent.m.

:- module parent:undeclared_child.
:- interface.
:- import_module io.

:- type foo ---> bar ; baz(int).

:- pred hello(io__state::di, io__state::uo) is det.

:- implementation.

hello --> io__write_string("parent:undeclared_child:hello\n").
