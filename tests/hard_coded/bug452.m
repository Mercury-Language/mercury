%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This program used to get a compiler abort in hlc.gc.
% This happened because find_int_lookup_switch_params in switch_util.m
% said that the inner switch on Thing in classify/2 below needed a bit vector
% check. It said so because the inner switch does not cover Thing = nothing.
% However, it does not *need* to cover Thing = nothing, because the inner
% switch is reached only in the arm of the outer switch that is not taken
% when Thing = nothing.
%
% In general, cannot_fail switches should not need either bit vectors
% or range checks. The compiler abort happensed because the MLDS code generator
% had sanity checks (assertions) to this effect. The LLDS code generator
% just silently generated an unnecessary bit vector check, which would execute
% incorrect code if the check failed; however, for these cannot_fail switches,
% the check could never fail.
%

:- module bug452.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    list.foldl(test_thing, [thing1, thing2, nothing, thing3, thing4], !IO).

:- pred test_thing(thing::in, io::di, io::uo) is det.

test_thing(Thing, !IO) :-
    list.foldl(test_category(Thing), [cat1, cat2, nocat], !IO),
    io.nl(!IO).

:- pred test_category(thing::in, category::in, io::di, io::uo) is det.

test_category(Thing, Cat0, !IO) :-
    Cat = classify(Cat0, Thing),
    io.write(Thing, !IO),
    io.write_string(" ", !IO),
    io.write(Cat0, !IO),
    io.write_string(" -> ", !IO),
    io.write_line(Cat, !IO).

:- type thing
    --->    thing1
    ;       thing2
    ;       nothing
    ;       thing3
    ;       thing4.

:- type category
    --->    cat1
    ;       cat2
    ;       nocat.

:- func classify(category, thing) = category.
:- pragma no_inline(classify/2).

classify(Cat0, Thing) = Cat :-
    (
        ( Thing = thing1
        ; Thing = thing2
        ; Thing = thing3
        ; Thing = thing4
        ),
        (
            ( Thing = thing1
            ; Thing = thing2
            ),
            Cat = cat1
        ;
            ( Thing = thing3
            ; Thing = thing4
            ),
            Cat = cat2
        )
    ;
        Thing = nothing,
        Cat = Cat0
    ).
