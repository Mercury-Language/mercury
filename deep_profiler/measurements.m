%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module defines the data structures that store deep profiling
% measurements and the operations on them.
%
%-----------------------------------------------------------------------------%

:- module measurements.

:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type own_prof_info.
:- type inherit_prof_info.

:- func calls(own_prof_info) = int.
:- func exits(own_prof_info) = int.
:- func fails(own_prof_info) = int.
:- func redos(own_prof_info) = int.
:- func excps(own_prof_info) = int.
:- func quanta(own_prof_info) = int.
:- func allocs(own_prof_info) = int.
:- func words(own_prof_info) = int.

:- func zero_own_prof_info = own_prof_info.

:- func inherit_quanta(inherit_prof_info) = int.
:- func inherit_allocs(inherit_prof_info) = int.
:- func inherit_words(inherit_prof_info) = int.

:- func zero_inherit_prof_info = inherit_prof_info.

:- func add_inherit_to_inherit(inherit_prof_info, inherit_prof_info)
    = inherit_prof_info.
:- func add_own_to_inherit(own_prof_info, inherit_prof_info)
    = inherit_prof_info.
:- func subtract_own_from_inherit(own_prof_info, inherit_prof_info)
    = inherit_prof_info.
:- func subtract_inherit_from_inherit(inherit_prof_info, inherit_prof_info)
    = inherit_prof_info.
:- func add_inherit_to_own(inherit_prof_info, own_prof_info) = own_prof_info.
:- func add_own_to_own(own_prof_info, own_prof_info) = own_prof_info.

:- func sum_own_infos(list(own_prof_info)) = own_prof_info.
:- func sum_inherit_infos(list(inherit_prof_info)) = inherit_prof_info.

:- func compress_profile(int, int, int, int, int, int, int) = own_prof_info.
:- func compress_profile(own_prof_info) = own_prof_info.

:- pred decompress_profile(own_prof_info::in, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out) is det.

:- func own_to_string(own_prof_info) = string.

    % Tests if this profiling information represents an entity in the
    % program that was inactive during the profiling run, e.g. a module
    % or procedure that has had no calls made to it.
    %
:- pred is_inactive(own_prof_info::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type own_prof_info
    --->    all(int, int, int, int, int, int, int)
                % exits, fails, redos, excps, quanta, allocs, words
                % implicit calls = exits + fails + excps - redos
    ;       det(int, int, int, int)
                % exits, quanta, allocs, words;
                % implicit fails == redos == excps == 0
                % implicit calls == exits
    ;       fast_det(int, int, int)
                % exits, allocs, words;
                % implicit fails == redos == excps == 0
                % implicit calls == exits
                % implicit quanta == 0
    ;       fast_nomem_semi(int, int).
                % exits, fails
                % implicit redos == excps == 0
                % implicit calls == exits + fails
                % implicit quanta == 0
                % implicit allocs == words == 0

:- type inherit_prof_info
    --->    inherit_prof_info(
                int,        % quanta
                int,        % allocs
                int         % words
            ).

calls(fast_nomem_semi(Exits, Fails)) = Exits + Fails.
exits(fast_nomem_semi(Exits, _)) = Exits.
fails(fast_nomem_semi(_, Fails)) = Fails.
redos(fast_nomem_semi(_, _)) = 0.
excps(fast_nomem_semi(_, _)) = 0.
quanta(fast_nomem_semi(_, _)) = 0.
allocs(fast_nomem_semi(_, _)) = 0.
words(fast_nomem_semi(_, _)) = 0.

calls(fast_det(Exits, _, _)) = Exits.
exits(fast_det(Exits, _, _)) = Exits.
fails(fast_det(_, _, _)) = 0.
redos(fast_det(_, _, _)) = 0.
excps(fast_det(_, _, _)) = 0.
quanta(fast_det(_, _, _)) = 0.
allocs(fast_det(_, Allocs, _)) = Allocs.
words(fast_det(_, _, Words)) = Words.

calls(det(Exits, _, _, _)) = Exits.
exits(det(Exits, _, _, _)) = Exits.
fails(det(_, _, _, _)) = 0.
redos(det(_, _, _, _)) = 0.
excps(det(_, _, _, _)) = 0.
quanta(det(_, Quanta, _, _)) = Quanta.
allocs(det(_, _, Allocs, _)) = Allocs.
words(det(_, _, _, Words)) = Words.

calls(all(Exits, Fails, Redos, Excps, _, _, _)) = Exits + Fails + Excps - Redos.
exits(all(Exits, _, _, _, _, _, _)) = Exits.
fails(all(_, Fails, _, _, _, _, _)) = Fails.
redos(all(_, _, Redos, _, _, _, _)) = Redos.
excps(all(_, _, _, Excps, _, _, _)) = Excps.
quanta(all(_, _, _, _, Quanta, _, _)) = Quanta.
allocs(all(_, _, _, _, _, Allocs, _)) = Allocs.
words(all(_, _, _, _, _, _, Words)) = Words.

zero_own_prof_info = fast_nomem_semi(0, 0).

inherit_quanta(inherit_prof_info(Quanta, _, _)) = Quanta.
inherit_allocs(inherit_prof_info(_, Allocs, _)) = Allocs.
inherit_words(inherit_prof_info(_, _, Words)) = Words.

zero_inherit_prof_info = inherit_prof_info(0, 0, 0).

add_inherit_to_inherit(PI1, PI2) = SumPI :-
    Quanta = inherit_quanta(PI1) + inherit_quanta(PI2),
    Allocs = inherit_allocs(PI1) + inherit_allocs(PI2),
    Words = inherit_words(PI1) + inherit_words(PI2),
    SumPI = inherit_prof_info(Quanta, Allocs, Words).

add_own_to_inherit(PI1, PI2) = SumPI :-
    Quanta = quanta(PI1) + inherit_quanta(PI2),
    Allocs = allocs(PI1) + inherit_allocs(PI2),
    Words = words(PI1) + inherit_words(PI2),
    SumPI = inherit_prof_info(Quanta, Allocs, Words).

subtract_own_from_inherit(PI1, PI2) = SumPI :-
    Quanta = inherit_quanta(PI2) - quanta(PI1),
    Allocs = inherit_allocs(PI2) - allocs(PI1),
    Words = inherit_words(PI2) - words(PI1),
    SumPI = inherit_prof_info(Quanta, Allocs, Words).

subtract_inherit_from_inherit(PI1, PI2) = SumPI :-
    Quanta = inherit_quanta(PI2) - inherit_quanta(PI1),
    Allocs = inherit_allocs(PI2) - inherit_allocs(PI1),
    Words = inherit_words(PI2) - inherit_words(PI1),
    SumPI = inherit_prof_info(Quanta, Allocs, Words).

add_inherit_to_own(PI1, PI2) = SumPI :-
    Exits = exits(PI2),
    Fails = fails(PI2),
    Redos = redos(PI2),
    Excps = excps(PI2),
    Quanta = inherit_quanta(PI1) + quanta(PI2),
    Allocs = inherit_allocs(PI1) + allocs(PI2),
    Words = inherit_words(PI1) + words(PI2),
    SumPI = compress_profile(Exits, Fails, Redos, Excps,
        Quanta, Allocs, Words).

add_own_to_own(PI1, PI2) = SumPI :-
    Exits = exits(PI1) + exits(PI2),
    Fails = fails(PI1) + fails(PI2),
    Redos = redos(PI1) + redos(PI2),
    Excps = excps(PI1) + excps(PI2),
    Quanta = quanta(PI1) + quanta(PI2),
    Allocs = allocs(PI1) + allocs(PI2),
    Words = words(PI1) + words(PI2),
    SumPI = compress_profile(Exits, Fails, Redos, Excps,
        Quanta, Allocs, Words).

sum_own_infos(Owns) =
    list.foldl(add_own_to_own, Owns, zero_own_prof_info).

sum_inherit_infos(Inherits) =
    list.foldl(add_inherit_to_inherit, Inherits, zero_inherit_prof_info).

compress_profile(Exits, Fails, Redos, Excps, Quanta, Allocs, Words) = PI :-
    (
        Redos = 0,
        Excps = 0,
        Quanta = 0,
        Allocs = 0,
        Words = 0
    ->
        PI = fast_nomem_semi(Exits, Fails)
    ;
        Fails = 0,
        Redos = 0,
        Excps = 0
    ->
        ( Quanta = 0 ->
            PI = fast_det(Exits, Allocs, Words)
        ;
            PI = det(Exits, Quanta, Allocs, Words)
        )
    ;
        PI = all(Exits, Fails, Redos, Excps, Quanta, Allocs, Words)
    ).

compress_profile(PI0) = PI :-
    (
        PI0 = all(Exits, Fails, Redos, Excps, Quanta, Allocs, Words),
        (
            Redos = 0,
            Excps = 0,
            Quanta = 0,
            Allocs = 0,
            Words = 0
        ->
            PI = fast_nomem_semi(Exits, Fails)
        ;
            Fails = 0,
            Redos = 0,
            Excps = 0
        ->
            ( Quanta = 0 ->
                PI = fast_det(Exits, Allocs, Words)
            ;
                PI = det(Exits, Quanta, Allocs, Words)
            )
        ;
            PI = PI0
        )
    ;
        PI0 = det(Exits, Quanta, Allocs, Words),
        ( Allocs = 0, Words = 0 ->
            PI = fast_nomem_semi(Exits, 0)
        ; Quanta = 0 ->
            PI = fast_det(Exits, Allocs, Words)
        ;
            PI = PI0
        )
    ;
        PI0 = fast_det(Exits, Allocs, Words),
        ( Allocs = 0, Words = 0 ->
            PI = fast_nomem_semi(Exits, 0)
        ;
            PI = PI0
        )
    ;
        PI0 = fast_nomem_semi(_, _),
        PI = PI0
    ).

%-----------------------------------------------------------------------------%

decompress_profile(Own, Calls, Exits, Fails, Redos, Excps,
        Quanta, Allocs, Words) :-
    (
        Own = all(Exits, Fails, Redos, Excps, Quanta, Allocs, Words),
        Calls = Exits + Fails + Redos
    ;
        Own = det(Exits, Quanta, Allocs, Words),
        Calls = Exits,
        Fails = 0,
        Redos = 0,
        Excps = 0
    ;
        Own = fast_det(Exits, Allocs, Words),
        Calls = Exits,
        Fails = 0,
        Redos = 0,
        Excps = 0,
        Quanta = 0
    ;
        Own = fast_nomem_semi(Exits, Fails),
        Calls = Exits + Fails,
        Redos = 0,
        Excps = 0,
        Quanta = 0,
        Allocs = 0,
        Words = 0
    ).

own_to_string(all(Exits, Fails, Redos, Excps, Quanta, Allocs, Words)) =
    "all(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Fails) ++ ", " ++
    string.int_to_string(Redos) ++ ", " ++
    string.int_to_string(Excps) ++ ", " ++
    string.int_to_string(Quanta) ++ ", " ++
    string.int_to_string(Allocs) ++ ", " ++
    string.int_to_string(Words) ++
    ")".
own_to_string(det(Exits, Quanta, Allocs, Words)) =
    "det(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Quanta) ++ ", " ++
    string.int_to_string(Allocs) ++ ", " ++
    string.int_to_string(Words) ++
    ")".
own_to_string(fast_det(Exits, Allocs, Words)) =
    "fast_det(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Allocs) ++ ", " ++
    string.int_to_string(Words) ++
    ")".
own_to_string(fast_nomem_semi(Exits, Fails)) =
    "fast_det(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Fails) ++
    ")".

is_inactive(all(0, 0, 0, 0, _, _, _)).
is_inactive(det(0, _, _, _)).
is_inactive(fast_det(0, _, _)).
is_inactive(fast_nomem_semi(0, 0)).

%----------------------------------------------------------------------------%
:- end_module measurements.
%----------------------------------------------------------------------------%
