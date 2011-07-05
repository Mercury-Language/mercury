%-----------------------------------------------------------------------------%

:- module where_abstract_enum.
:- interface.

:- type abs1
    --->    abs1
    ;       abs2
    ;       abs3.

:- type abs2 where type_is_abstract_enum.

:- type abs3 where blah.

:- implementation.

:- type abs1 where type_is_abstract_enum(3).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
