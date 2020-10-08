%-----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module bug521_sub.
:- interface.

% :- import_module list.  % Missing module import causes the error

:- type thing
    --->    thing_a
    ;       thing_b.

:- func get_things = list(thing).

%-----------------------------------------------------------------------------%

:- implementation.

get_things = [thing_a, thing_b].
