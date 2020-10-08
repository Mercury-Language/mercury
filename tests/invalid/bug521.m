%-----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module bug521.
:- interface.

:- import_module bug521_sub.
:- import_module list.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- type list_wrapper(C)
    --->    list_wrapper(list(C)).

:- pred get_wrapped_things(list_wrapper(thing)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

main(!IO).

get_wrapped_things(X) :-
    X = list_wrapper(get_things).
