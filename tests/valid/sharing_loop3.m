%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for structure sharing analysis with
% --structure-sharing-widening set to certain values.

:- module sharing_loop3.
:- interface.

:- type instrmap == mymap(label, instruction).
:- type mymap(T, U).

:- pred short_labels_rval(instrmap::in, rval::in, rval::out) is det.

:- type instruction
    --->    llds_instr(instr, string).

:- type instr
    --->    label(label)
    ;       goto.

:- type label
    --->    internal_label(string).

:- type code_addr
    --->    code_label(label).

:- type lval
    --->    succip_slot(rval)
    ;       mem_ref(rval).

:- type rval
    --->    lval(lval)
    ;       const(rval_const)
    ;       binop(rval, rval)
    ;       mem_addr(int).

:- type rval_const
    --->    llconst_code_addr(code_addr).

%---------------------------------------------------------------------------%

:- implementation.

:- pred short_label(instrmap::in, label::in, label::out) is det.

short_label(Instrmap, Label0, Label) :-
    ( if search(Instrmap, Label0, Instr0) then
        final_dest(Instrmap, Label0, Label, Instr0, _Instr)
    else
        Label = Label0
    ).

:- pred final_dest(instrmap::in, label::in, label::out, instruction::in,
    instruction::out) is det.

final_dest(Instrmap, SrcLabel, DestLabel, SrcInstr, DestInstr) :-
    ( if
        SrcInstr = llds_instr(SrcUinstr, _Comment),
        SrcUinstr = label(TargetLabel),
        search(Instrmap, TargetLabel, TargetInstr)
    then
        final_dest(Instrmap,
            TargetLabel, DestLabel, TargetInstr, DestInstr)
    else
        DestLabel = SrcLabel,
        DestInstr = SrcInstr
    ).

short_labels_rval(Instrmap, lval(Lval0), lval(Lval)) :-
    short_labels_lval(Instrmap, Lval0, Lval).
short_labels_rval(Instrmap, const(Const0), const(Const)) :-
    short_labels_const(Instrmap, Const0, Const).
short_labels_rval(Instrmap, binop(LRval0, RRval0),
        binop(LRval, RRval)) :-
    short_labels_rval(Instrmap, LRval0, LRval),
    short_labels_rval(Instrmap, RRval0, RRval).
short_labels_rval(_, mem_addr(MemRef), mem_addr(MemRef)).

:- pred short_labels_const(instrmap::in,
    rval_const::in, rval_const::out) is det.

short_labels_const(Instrmap, llconst_code_addr(CodeAddr0),
        llconst_code_addr(CodeAddr)) :-
    CodeAddr0 = code_label(Label0),
    short_label(Instrmap, Label0, Label),
    CodeAddr = code_label(Label).

:- pred short_labels_lval(instrmap::in, lval::in, lval::out) is det.

short_labels_lval(Instrmap, succip_slot(Rval0), succip_slot(Rval)) :-
    short_labels_rval(Instrmap, Rval0, Rval).
short_labels_lval(Instrmap, mem_ref(Rval0), mem_ref(Rval)) :-
    short_labels_rval(Instrmap, Rval0, Rval).

%---------------------------------------------------------------------------%

:- type mymap(K, V)
    --->    empty
    ;       two(K, V, mymap(K, V), mymap(K, V))
    ;       three(K, V, K, V, mymap(K, V), mymap(K, V), mymap(K, V))
    ;       four(K, V, K, V, K, V, mymap(K, V), mymap(K, V),
                mymap(K, V), mymap(K, V)).

:- pred search(mymap(K, V)::in, K::in, V::out) is semidet.

search(T, K, V) :-
    (
        T = empty,
        fail
    ;
        T = two(K0, V0, T0, T1),
        compare(Result, K, K0),
        (
            Result = (<),
            search(T0, K, V)
        ;
            Result = (=),
            V = V0
        ;
            Result = (>),
            search(T1, K, V)
        )
    ;
        T = three(K0, V0, K1, V1, T0, T1, T2),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            search(T0, K, V)
        ;
            Result0 = (=),
            V = V0
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                search(T1, K, V)
            ;
                Result1 = (=),
                V = V1
            ;
                Result1 = (>),
                search(T2, K, V)
            )
        )
    ;
        T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                search(T0, K, V)
            ;
                Result0 = (=),
                V = V0
            ;
                Result0 = (>),
                search(T1, K, V)
            )
        ;
            Result1 = (=),
            V = V1
        ;
            Result1 = (>),
            compare(Result2, K, K2),
            (
                Result2 = (<),
                search(T2, K, V)
            ;
                Result2 = (=),
                V = V2
            ;
                Result2 = (>),
                search(T3, K, V)
            )
        )
    ).
