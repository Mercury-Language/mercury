%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_pack_helper_1.
:- interface.

% sub_abs_enum should occupy 3 bits in subtype_pack.struct without needing
% to export its base type.
% :- type abs_enum.

:- type sub_abs_enum.

:- type sub_abs_dummy.

:- type sub_abs_notag.

:- func make_sub_abs_enum = sub_abs_enum.

:- func make_sub_abs_dummy = sub_abs_dummy.

:- func make_sub_abs_notag = sub_abs_notag.

%---------------------------------------------------------------------------%

:- implementation.

:- type abs_enum
    --->    ant
    ;       bat
    ;       cat
    ;       dog
    ;       eel.

:- type abs_enum2 =< abs_enum
    --->    ant
    ;       bat
    ;       cat
    ;       dog.

:- type eqv_enum == abs_enum2.
:- type eqv_eqv_enum == eqv_enum.

:- type sub_abs_enum =< eqv_eqv_enum
    --->    bat
    ;       cat
    ;       dog.

:- type abs_dummy
    --->    dummy.

:- type sub_abs_dummy =< abs_dummy
    --->    dummy.

:- type abs_notag
    --->    abs_notag(int).

:- type sub_abs_notag =< abs_notag
    --->    abs_notag(int).

make_sub_abs_enum = dog.

make_sub_abs_dummy = dummy.

make_sub_abs_notag = abs_notag(42).
