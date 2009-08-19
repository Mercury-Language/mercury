%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2004-2006, 2008-2009 The University of Melbourne.
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
:- func callseqs(own_prof_info) = int.
:- func allocs(own_prof_info) = int.
:- func words(own_prof_info) = int.

:- func zero_own_prof_info = own_prof_info.

:- pred is_zero_own_prof_info(own_prof_info::in) is semidet.

:- func inherit_quanta(inherit_prof_info) = int.
:- func inherit_callseqs(inherit_prof_info) = int.
:- func inherit_allocs(inherit_prof_info) = int.
:- func inherit_words(inherit_prof_info) = int.

:- func zero_inherit_prof_info = inherit_prof_info.

:- pred is_zero_inherit_prof_info(inherit_prof_info::in) is semidet.

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

:- func compress_profile(int, int, int, int, int, int, int, int)
    = own_prof_info.
:- func compress_profile(own_prof_info) = own_prof_info.

:- pred decompress_profile(own_prof_info::in, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out, int::out) is det.

:- func own_to_string(own_prof_info) = string.

:- type is_active
    --->    is_active
    ;       is_not_active.

    % Tests if this profiling information represents an entity in the program
    % that was inactive during the profiling run, e.g. a module or procedure
    % that has had no calls made to it.
    %
:- func compute_is_active(own_prof_info) = is_active.

%-----------------------------------------------------------------------------%

:- type proc_cost_csq.

:- type cs_cost_csq.

    % build_proc_cost_csq(NRCalls, RCalls, TotalCost) = ParentCost.
    %
    % NRCalls: the number of non-recursive calls into this context.
    % RCalls: the number of recursive (inc mutually-recursive) calls into this
    % context.
    %
:- func build_proc_cost_csq(int, int, int) = proc_cost_csq.

    % build_cs_cost_csq(Calls, TotalCost) = CallSiteCost.
    %
:- func build_cs_cost_csq(int, float) = cs_cost_csq.
    
    % build_cs_cost_csq_percall(Calls, PercallCost) = CallSiteCost.
    %
:- func build_cs_cost_csq_percall(float, float) = cs_cost_csq.

    % Call site cost structure that has a zero cost and zero calls.
    %
:- func zero_cs_cost = cs_cost_csq.

    % Retrieve the total cost of this context.
    %
:- func proc_cost_get_total(proc_cost_csq) = float.

    % Retrive the number of calls made to this procedure.
    %
:- func proc_cost_get_calls_total(proc_cost_csq) = int.
:- func proc_cost_get_calls_nonrec(proc_cost_csq) = int.
:- func proc_cost_get_calls_rec(proc_cost_csq) = int.

    % Retrieve the total cost of a call site.
    %
:- func cs_cost_get_total(cs_cost_csq) = float.

    % Retrieve the per-call cost of a call site.
    % Note that this may throw an exception if the number of calls is zero.
    %
:- func cs_cost_get_percall(cs_cost_csq) = float.

    % Retrive the number of calls made from this call site.
    %
:- func cs_cost_get_calls(cs_cost_csq) = float.

    % Convert a call site cost to a proc cost.
    %
:- pred cs_cost_to_proc_cost(cs_cost_csq::in, int::in, 
    proc_cost_csq::out) is det.

:- func cs_cost_per_proc_call(cs_cost_csq, proc_cost_csq) = cs_cost_csq.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module string.

%----------------------------------------------------------------------------%

:- type own_prof_info
    --->    own_prof_all(
                opa_exits               :: int,
                opa_fails               :: int,
                opa_redos               :: int,
                opa_excps               :: int,
                opa_quanta              :: int,
                opa_callseqs            :: int,
                opa_allocs              :: int,
                opa_words               :: int
            )
            % implicit calls = exits + fails + excps - redos

    ;       own_prof_det(
                opd_exits               :: int,
                opd_quanta              :: int,
                opd_callseqs            :: int,
                opd_allocs              :: int,
                opd_words               :: int
            )
            % implicit fails == redos == excps == 0
            % implicit calls == exits

    ;       own_prof_fast_det(
                opfd_exits              :: int,
                opfd_callseqs           :: int,
                opfd_allocs             :: int,
                opfd_words              :: int
            )
            % implicit fails == redos == excps == 0
            % implicit calls == exits
            % implicit quanta == 0

    ;       own_prof_fast_nomem_semi(
                opfns_exits             :: int,
                opfns_fails             :: int,
                opfns_callseqs          :: int
            ).
            % implicit redos == excps == 0
            % implicit calls == exits + fails
            % implicit quanta == 0
            % implicit allocs == words == 0

:- type inherit_prof_info
    --->    inherit_prof_info(
                ipo_quanta              :: int,
                ipo_callseqs            :: int,
                ipo_allocs              :: int,
                ipo_words               :: int
            ).

calls(own_prof_fast_nomem_semi(Exits, Fails, _)) = Exits + Fails.
calls(own_prof_fast_det(Exits, _, _, _)) = Exits.
calls(own_prof_det(Exits, _, _, _, _)) = Exits.
calls(own_prof_all(Exits, Fails, Redos, Excps, _, _, _, _)) =
    Exits + Fails + Excps - Redos.

exits(own_prof_fast_nomem_semi(Exits, _, _)) = Exits.
exits(own_prof_fast_det(Exits, _, _, _)) = Exits.
exits(own_prof_det(Exits, _, _, _, _)) = Exits.
exits(own_prof_all(Exits, _, _, _, _, _, _, _)) = Exits.

fails(own_prof_fast_nomem_semi(_, Fails, _)) = Fails.
fails(own_prof_fast_det(_, _, _, _)) = 0.
fails(own_prof_det(_, _, _, _, _)) = 0.
fails(own_prof_all(_, Fails, _, _, _, _, _, _)) = Fails.

redos(own_prof_fast_nomem_semi(_, _, _)) = 0.
redos(own_prof_fast_det(_, _, _, _)) = 0.
redos(own_prof_det(_, _, _, _, _)) = 0.
redos(own_prof_all(_, _, Redos, _, _, _, _, _)) = Redos.

excps(own_prof_fast_nomem_semi(_, _, _)) = 0.
excps(own_prof_fast_det(_, _, _, _)) = 0.
excps(own_prof_det(_, _, _, _, _)) = 0.
excps(own_prof_all(_, _, _, Excps, _, _, _, _)) = Excps.

quanta(own_prof_fast_nomem_semi(_, _, _)) = 0.
quanta(own_prof_fast_det(_, _, _, _)) = 0.
quanta(own_prof_det(_, Quanta, _, _, _)) = Quanta.
quanta(own_prof_all(_, _, _, _, Quanta, _, _, _)) = Quanta.

callseqs(own_prof_fast_nomem_semi(_, _, CallSeqs)) = CallSeqs.
callseqs(own_prof_fast_det(_, CallSeqs, _, _)) = CallSeqs.
callseqs(own_prof_det(_, _, CallSeqs, _, _)) = CallSeqs.
callseqs(own_prof_all(_, _, _, _, _, CallSeqs, _, _)) = CallSeqs.

allocs(own_prof_fast_nomem_semi(_, _, _)) = 0.
allocs(own_prof_fast_det(_, _, Allocs, _)) = Allocs.
allocs(own_prof_det(_, _, _, Allocs, _)) = Allocs.
allocs(own_prof_all(_, _, _, _, _, _, Allocs, _)) = Allocs.

words(own_prof_fast_nomem_semi(_, _, _)) = 0.
words(own_prof_fast_det(_, _, _, Words)) = Words.
words(own_prof_det(_, _, _, _, Words)) = Words.
words(own_prof_all(_, _, _, _, _, _, _, Words)) = Words.

zero_own_prof_info = own_prof_fast_nomem_semi(0, 0, 0).

is_zero_own_prof_info(own_prof_all(0, 0, 0, 0, 0, 0, 0, 0)).
is_zero_own_prof_info(own_prof_det(0, 0, 0, 0, 0)).
is_zero_own_prof_info(own_prof_fast_det(0, 0, 0, 0)).
is_zero_own_prof_info(own_prof_fast_nomem_semi(0, 0, 0)).

inherit_quanta(inherit_prof_info(Quanta, _, _, _)) = Quanta.
inherit_callseqs(inherit_prof_info(_, CallSeqs, _, _)) = CallSeqs.
inherit_allocs(inherit_prof_info(_, _, Allocs, _)) = Allocs.
inherit_words(inherit_prof_info(_, _, _, Words)) = Words.

zero_inherit_prof_info = inherit_prof_info(0, 0, 0, 0).

is_zero_inherit_prof_info(inherit_prof_info(0, 0, 0, 0)).

add_inherit_to_inherit(PI1, PI2) = SumPI :-
    Quanta = inherit_quanta(PI1) + inherit_quanta(PI2),
    CallSeqs = inherit_callseqs(PI1) + inherit_callseqs(PI2),
    Allocs = inherit_allocs(PI1) + inherit_allocs(PI2),
    Words = inherit_words(PI1) + inherit_words(PI2),
    SumPI = inherit_prof_info(Quanta, CallSeqs, Allocs, Words).

add_own_to_inherit(PI1, PI2) = SumPI :-
    Quanta = quanta(PI1) + inherit_quanta(PI2),
    CallSeqs = callseqs(PI1) + inherit_callseqs(PI2),
    Allocs = allocs(PI1) + inherit_allocs(PI2),
    Words = words(PI1) + inherit_words(PI2),
    SumPI = inherit_prof_info(Quanta, CallSeqs, Allocs, Words).

subtract_own_from_inherit(PI1, PI2) = SumPI :-
    Quanta = inherit_quanta(PI2) - quanta(PI1),
    CallSeqs = inherit_callseqs(PI2) - callseqs(PI1),
    Allocs = inherit_allocs(PI2) - allocs(PI1),
    Words = inherit_words(PI2) - words(PI1),
    SumPI = inherit_prof_info(Quanta, CallSeqs, Allocs, Words).

subtract_inherit_from_inherit(PI1, PI2) = SumPI :-
    Quanta = inherit_quanta(PI2) - inherit_quanta(PI1),
    CallSeqs = inherit_callseqs(PI2) - inherit_callseqs(PI1),
    Allocs = inherit_allocs(PI2) - inherit_allocs(PI1),
    Words = inherit_words(PI2) - inherit_words(PI1),
    SumPI = inherit_prof_info(Quanta, CallSeqs, Allocs, Words).

add_inherit_to_own(PI1, PI2) = SumPI :-
    Exits = exits(PI2),
    Fails = fails(PI2),
    Redos = redos(PI2),
    Excps = excps(PI2),
    Quanta = inherit_quanta(PI1) + quanta(PI2),
    CallSeqs = inherit_callseqs(PI1) + callseqs(PI2),
    Allocs = inherit_allocs(PI1) + allocs(PI2),
    Words = inherit_words(PI1) + words(PI2),
    SumPI = compress_profile(Exits, Fails, Redos, Excps,
        Quanta, CallSeqs, Allocs, Words).

add_own_to_own(PI1, PI2) = SumPI :-
    Exits = exits(PI1) + exits(PI2),
    Fails = fails(PI1) + fails(PI2),
    Redos = redos(PI1) + redos(PI2),
    Excps = excps(PI1) + excps(PI2),
    Quanta = quanta(PI1) + quanta(PI2),
    CallSeqs = callseqs(PI1) + callseqs(PI2),
    Allocs = allocs(PI1) + allocs(PI2),
    Words = words(PI1) + words(PI2),
    SumPI = compress_profile(Exits, Fails, Redos, Excps,
        Quanta, CallSeqs, Allocs, Words).

sum_own_infos(Owns) =
    list.foldl(add_own_to_own, Owns, zero_own_prof_info).

sum_inherit_infos(Inherits) =
    list.foldl(add_inherit_to_inherit, Inherits, zero_inherit_prof_info).

compress_profile(Exits, Fails, Redos, Excps, Quanta, CallSeqs, Allocs, Words)
        = PI :-
    (
        Redos = 0,
        Excps = 0,
        Quanta = 0,
        Allocs = 0,
        Words = 0
    ->
        PI = own_prof_fast_nomem_semi(Exits, Fails, CallSeqs)
    ;
        Fails = 0,
        Redos = 0,
        Excps = 0
    ->
        ( Quanta = 0 ->
            PI = own_prof_fast_det(Exits, CallSeqs, Allocs, Words)
        ;
            PI = own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words)
        )
    ;
        PI = own_prof_all(Exits, Fails, Redos, Excps, Quanta, CallSeqs,
            Allocs, Words)
    ).

compress_profile(PI0) = PI :-
    (
        PI0 = own_prof_all(Exits, Fails, Redos, Excps, Quanta, CallSeqs,
            Allocs, Words),
        (
            Redos = 0,
            Excps = 0,
            Quanta = 0,
            Allocs = 0,
            Words = 0
        ->
            PI = own_prof_fast_nomem_semi(Exits, Fails, CallSeqs)
        ;
            Fails = 0,
            Redos = 0,
            Excps = 0
        ->
            ( Quanta = 0 ->
                PI = own_prof_fast_det(Exits, CallSeqs, Allocs, Words)
            ;
                PI = own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words)
            )
        ;
            PI = PI0
        )
    ;
        PI0 = own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words),
        ( Allocs = 0, Words = 0 ->
            PI = own_prof_fast_nomem_semi(Exits, 0, CallSeqs)
        ; Quanta = 0 ->
            PI = own_prof_fast_det(Exits, CallSeqs, Allocs, Words)
        ;
            PI = PI0
        )
    ;
        PI0 = own_prof_fast_det(Exits, CallSeqs, Allocs, Words),
        ( Allocs = 0, Words = 0 ->
            PI = own_prof_fast_nomem_semi(Exits, 0, CallSeqs)
        ;
            PI = PI0
        )
    ;
        PI0 = own_prof_fast_nomem_semi(_, _, _),
        PI = PI0
    ).

%-----------------------------------------------------------------------------%

decompress_profile(Own, Calls, Exits, Fails, Redos, Excps, Quanta, CallSeqs,
        Allocs, Words) :-
    (
        Own = own_prof_all(Exits, Fails, Redos, Excps, Quanta, CallSeqs,
            Allocs, Words),
        Calls = Exits + Fails - Redos
    ;
        Own = own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words),
        Calls = Exits,
        Fails = 0,
        Redos = 0,
        Excps = 0
    ;
        Own = own_prof_fast_det(Exits, CallSeqs, Allocs, Words),
        Calls = Exits,
        Fails = 0,
        Redos = 0,
        Excps = 0,
        Quanta = 0
    ;
        Own = own_prof_fast_nomem_semi(Exits, Fails, CallSeqs),
        Calls = Exits + Fails,
        Redos = 0,
        Excps = 0,
        Quanta = 0,
        Allocs = 0,
        Words = 0
    ).

own_to_string(own_prof_all(Exits, Fails, Redos, Excps, Quanta, CallSeqs,
        Allocs, Words)) =
    "all(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Fails) ++ ", " ++
    string.int_to_string(Redos) ++ ", " ++
    string.int_to_string(Excps) ++ ", " ++
    string.int_to_string(Quanta) ++ ", " ++
    string.int_to_string(CallSeqs) ++ ", " ++
    string.int_to_string(Allocs) ++ ", " ++
    string.int_to_string(Words) ++
    ")".
own_to_string(own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words)) =
    "det(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Quanta) ++ ", " ++
    string.int_to_string(CallSeqs) ++ ", " ++
    string.int_to_string(Allocs) ++ ", " ++
    string.int_to_string(Words) ++
    ")".
own_to_string(own_prof_fast_det(Exits, CallSeqs, Allocs, Words)) =
    "fast_det(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(CallSeqs) ++ ", " ++
    string.int_to_string(Allocs) ++ ", " ++
    string.int_to_string(Words) ++
    ")".
own_to_string(own_prof_fast_nomem_semi(Exits, Fails, CallSeqs)) =
    "fast_det(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Fails) ++ ", " ++
    string.int_to_string(CallSeqs) ++
    ")".

compute_is_active(Own) = IsActive :-
    (
        ( Own = own_prof_all(0, 0, 0, 0, _, _, _, _)
        ; Own = own_prof_det(0, _, _, _, _)
        ; Own = own_prof_fast_det(0, _, _, _)
        ; Own = own_prof_fast_nomem_semi(0, 0, _)
        )
    ->
        IsActive = is_not_active
    ;
        IsActive = is_active
    ).

%----------------------------------------------------------------------------%

:- type proc_cost_csq
    --->    proc_cost_csq(
                pcc_nr_calls        :: int,
                    % The number of non-recursive calls into this context.  For
                    % example if this is a clique this is the number of calls
                    % made from the parent' clique to this one.
                
                pcc_r_calls         :: int,
                    % The number of recursive calls into this context.  This
                    % includes mutual recursion.
                
                pcc_csq             :: cost 
                    % The number of callseq counts per call,
            ).

:- type cs_cost_csq
    --->    cs_cost_csq(
                cscc_calls          :: float,
                    % The number of calls (per parent invocation) through this
                    % call site.
                
                cscc_csq_cost       :: cost 
                    % The cost of the call site per call.
            ).

%----------------------------------------------------------------------------%

build_proc_cost_csq(NonRecursiveCalls, RecursiveCalls, TotalCost) = 
    proc_cost_csq(NonRecursiveCalls, RecursiveCalls, 
        cost_total(float(TotalCost))).

proc_cost_get_total(proc_cost_csq(NRCalls, RCalls, Cost)) =
    cost_get_total(float(NRCalls + RCalls), Cost).

proc_cost_get_calls_total(proc_cost_csq(NRCalls, RCalls, _)) = 
    NRCalls + RCalls.

proc_cost_get_calls_nonrec(proc_cost_csq(NRCalls, _, _)) = NRCalls.

proc_cost_get_calls_rec(proc_cost_csq(_, RCalls, _)) = RCalls.

%----------------------------------------------------------------------------%

build_cs_cost_csq(Calls, TotalCost) = 
    cs_cost_csq(float(Calls), cost_total(TotalCost)).

build_cs_cost_csq_percall(Calls, PercallCost) =
    cs_cost_csq(Calls, cost_per_call(PercallCost)).

zero_cs_cost =
    % Build this using the percall structure so that if a percall cost is ever
    % retrived we don't have to divide by zero.  This is only a partial
    % solution.
    build_cs_cost_csq_percall(0.0, 0.0).

cs_cost_get_total(cs_cost_csq(Calls, Cost)) = 
    cost_get_total(Calls, Cost).

cs_cost_get_percall(cs_cost_csq(Calls, Cost)) =
    cost_get_percall(Calls, Cost).

cs_cost_get_calls(cs_cost_csq(Calls, _)) = Calls.

%----------------------------------------------------------------------------%

cs_cost_to_proc_cost(cs_cost_csq(CSCalls, CSCost), TotalCalls, 
        proc_cost_csq(NRCalls, RCalls, PCost)) :-
    NRCalls = round_to_int(CSCalls),
    RCalls = TotalCalls - round_to_int(CSCalls),
    % The negative one represents the cost of the callsite itsself.
    PCost = cost_total(cost_get_total(CSCalls, CSCost) - 1.0 * CSCalls).

cs_cost_per_proc_call(cs_cost_csq(CSCalls0, CSCost0), ParentCost) = 
        cs_cost_csq(CSCalls, CSCost) :-
    TotalParentCalls = proc_cost_get_calls_nonrec(ParentCost),
    CSCalls = CSCalls0 / float(TotalParentCalls),
    CSCost = CSCost0 / TotalParentCalls.

%----------------------------------------------------------------------------%

:- type cost
    --->    cost_per_call(float)
    ;       cost_total(float).

:- func cost_get_total(float, cost) = float.

cost_get_total(_, cost_total(Total)) = Total.
cost_get_total(Calls, cost_per_call(PC)) = Calls * PC.

:- func cost_get_percall(float, cost) = float.

cost_get_percall(Calls, cost_total(Total)) = Total / Calls.
cost_get_percall(_, cost_per_call(PC)) = PC.

:- func (cost) / (int) = cost.

Cost0 / Denom = Cost :-
    (
        Cost0 = cost_total(Total),
        Cost = cost_total(Total / float(Denom))
    ;
        Cost0 = cost_per_call(Percall),
        Cost = cost_per_call(Percall / float(Denom))
    ).

%----------------------------------------------------------------------------%
:- end_module measurements.
%----------------------------------------------------------------------------%
