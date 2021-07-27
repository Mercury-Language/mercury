%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% dupelim.m did not handle mkword_hole case leading to:
%
% predicate `ll_backend.dupelim.process_elim_labels'/11:
% Unexpected: blocks with same standard form don't antiunify

:- module dupelim_mkword_hole.
:- interface.

:- import_module bool.
:- import_module io.

:- type foo
    --->    foo.

:- type value
    --->    object(object).

:- type object
    --->    some [T] object(T).

:- pred p1(foo::in, object::in, value::in, bool::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.

:- type thing
    --->    f1
    ;       f2
    ;       f3
    ;       f4
    ;       f5
    ;       f6
    ;       f7
    ;       f8(int, int)
    ;       f9(int, int, int)
    ;       struct(struct). % direct arg functor

:- type struct
    --->    struct(string, int).

%---------------------------------------------------------------------------%

p1(Foo, X, object(Y), Res, !IO) :-
    ( if get_thing(X, struct(_)) then
        p2(Foo, X, Y, Res, !IO)
    else
        unexpected($module, $pred)
    ).

:- pred p2(foo::in, object::in, object::in, bool::out, io::di, io::uo) is det.

p2(Foo, X, Y, Res, !IO) :-
    ( if get_thing(X, struct(_)) then
        p3(Foo, X, Z),
        ( if Z = [] then
            Res = no
        else if get_thing(Y, struct(_)) then
            Res = yes
        else
            Res = no
        )
    else if get_thing(X, struct(_)) then
        ( if get_thing(Y, struct(_)) then
            Res = yes
        else
            Res = no
        )
    else
        Res = no
    ).

:- pred p3(foo::in, object::in, list(int)::out) is det.

p3(Foo, X, Z) :-
    p4(Foo, X, Y),
    (
        Y = [],
        Z = []
    ;
        Y = [_ | _],
        Z = [1]
    ).

:- pragma no_inline(p4/3).
:- pred p4(foo, object, list(int)).
:- mode p4(in, in, out) is det.

p4(_, _, []).

:- pred get_thing(object, T).
:- mode get_thing(in, out) is semidet.

get_thing(object(T0), T) :-
    dynamic_cast(T0, T).
