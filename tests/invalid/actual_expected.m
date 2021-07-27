%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module actual_expected.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

main(!IO) :-
    find_compulsory_lvals([], no, no, no, MaybeCompulsoryLvals),
    io.write(MaybeCompulsoryLvals, !IO).

:- type instruction
    --->    llcall(int, int)
    ;       goto(code_addr)
    ;       livevals(set(lval)).

:- type lval
    --->    reg(int).

:- type rval
    --->    rvalconst(int).

:- type code_addr
    --->    code_addr_label(label)
    ;       code_addr_do_fail.

:- type label
    --->    label(string).

:- type maybe_compulsory_lvals
    --->    known(set(lval))
    ;       unknown_must_assume_all.

:- type livemap == map(label, set(lval)).

:- pred find_compulsory_lvals(list(instruction)::in, maybe(livemap)::in,
    maybe(label)::in, bool::in, maybe_compulsory_lvals::out) is det.

find_compulsory_lvals([], MaybeLiveMap, MaybeFallThrough,
        _PrevLivevals, MaybeCompulsoryLvals) :-
    (
        MaybeFallThrough = yes(FallThrough),
        (
            MaybeLiveMap = yes(LiveMap),
            map.lookup(LiveMap, FallThrough, CompulsoryLvals),
            MaybeCompulsoryLvals = known(CompulsoryLvals)
        ;
            MaybeLiveMap = no,
            MaybeCompulsoryLvals = unknown_must_assume_all
        )
    ;
        MaybeFallThrough = no,
        MaybeCompulsoryLvals = unknown_must_assume_all
    ).
find_compulsory_lvals([Instr | Instrs], MaybeLiveMap, MaybeFallThrough,
        PrevLivevals, !:MaybeCompulsoryLvals) :-
    ( if
        Instr = livevals(LiveLvals)
    then
        find_compulsory_lvals(Instrs, MaybeLiveMap, MaybeFallThrough,
            yes, !:MaybeCompulsoryLvals),
        union_maybe_compulsory_lvals(LiveLvals, !MaybeCompulsoryLvals)
    else if
        Instr = llcall(_, _)
    then
        require(unify(PrevLivevals, yes),
            "find_compulsory_lvals: call without livevals"),
        % The livevals instruction will include all the live lvals
        % in MaybeCompulsoryLvals after we return.
        !:MaybeCompulsoryLvals = known(set.init)
    else if
        Instr = goto(_Target)
    then
        % The livevals instruction will include all the live lvals
        % in MaybeCompulsoryLvals after we return.
        !:MaybeCompulsoryLvals = known(set.init)
    else
        possible_targets(Instr, Labels, NonLabelCodeAddrs),
        (
            NonLabelCodeAddrs = [],
            (
                Labels = [],
                % Optimize the common case
                find_compulsory_lvals(Instrs, MaybeLiveMap, MaybeFallThrough,
                    no, !:MaybeCompulsoryLvals)
            ;
                Labels = [_ | _],
                (
                    MaybeLiveMap = yes(LiveMap),
                    list.map(map.lookup(LiveMap), Labels, LabelsLiveLvals),
                    AllLabelsLiveLvals = set.union_list(LabelsLiveLvals),
                    find_compulsory_lvals(Instrs, LiveMap,
                        MaybeFallThrough, no, !:MaybeCompulsoryLvals),
                    union_maybe_compulsory_lvals(AllLabelsLiveLvals,
                        !MaybeCompulsoryLvals)
                ;
                    MaybeLiveMap = no,
                    !:MaybeCompulsoryLvals = unknown_must_assume_all
                )
            )
        ;
            NonLabelCodeAddrs = [_ | _],
            !:MaybeCompulsoryLvals = unknown_must_assume_all
        )
    ).

:- pred union_maybe_compulsory_lvals(set(lval)::in,
    maybe_compulsory_lvals::in, maybe_compulsory_lvals::out) is det.

union_maybe_compulsory_lvals(New, !MaybeCompulsoryLvals) :-
    (
        !.MaybeCompulsoryLvals = known(OldCompulsoryLvals),
        set.union(New, OldCompulsoryLvals, AllCompulsoryLvals),
        !:MaybeCompulsoryLvals = known(AllCompulsoryLvals)
    ;
        !.MaybeCompulsoryLvals = unknown_must_assume_all
    ).

:- pred possible_targets(instruction::in, list(label)::out,
    list(code_addr)::out) is det.

possible_targets(llcall(_, _), [], []).
possible_targets(goto(code_addr_label(Label)), [Label], []).
possible_targets(goto(code_addr_do_fail), [], [code_addr_do_fail]).
