%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests whether we generate spurious warnings such as this:
%
% bad_singleton_warning.m:031: In clause for predicate
% bad_singleton_warning.m:031:   `bad_singleton_warning.is_it_there_str'/3:
% bad_singleton_warning.m:031:   warning: variable `Value' occurs only once in
% bad_singleton_warning.m:031:   this scope.

:- module bad_singleton_warning.
:- interface.

:- import_module map.

:- pred is_it_there_test(map(int, string)::in, int::in) is semidet.

:- pred is_it_there_str(map(int, string)::in, int::in, string::out) is det.

:- implementation.

is_it_there_test(Map, Key) :-
    % Test whether we get a warning on an ordinary quantification.
    some [Value] (
        map.search(Map, Key, Value)
    ).

is_it_there_str(Map, Key, Str) :-
    % Test whether we get a warning on a special quantification
    % on the condition of an if-then-else. (This is treated separately
    % by the compiler because it quantifies over the "conjunction" formed
    % by the condition and the then-part.)
    ( if
        some [Value] (
            map.search(Map, Key, Value)
        )
    then
        Str = "there"
    else
        Str = "not there"
    ).
