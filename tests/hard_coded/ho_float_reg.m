%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the float_reg.m pass.

:- module ho_float_reg.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module solutions.

%---------------------------------------------------------------------------%

main(!IO) :-
    P1 = foo(1.5),
    P2 = foo(1.5, 2.5),

    begin("plain call", !IO),
    some [Res] (
        foo(1.5, 2.5, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("higher-order call", !IO),
    some [Res] (
        P1(2.5, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("docall", !IO),
    some [Res] (
        docall(P2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("docall_inline", !IO),
    some [Res] (
        docall_inline(P2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("get_docall", !IO),
    some [DoCall, Res] (
        get_docall(DoCall),
        DoCall(P2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("get_docall2", !IO),
    some [DoCall1, DoCall2, Res] (
        get_docall2(DoCall2),
        DoCall2(DoCall1),
        DoCall1(P2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("get_docall3", !IO),
    some [DoCall1, DoCall2, DoCall3, Res] (
        get_docall3(DoCall3),
        DoCall3(DoCall2),
        DoCall2(DoCall1),
        DoCall1(P2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("docall_foreign_poly", !IO),
    some [Res] (
        docall_foreign_poly(P2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("docall_foreign_float", !IO),
    some [Res] (
        docall_foreign_float(P2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    some [SPoly, SMono, Res1, Res2, Res3] (
        SPoly = struct_poly(P2),
        SMono = struct_mono(foo(1.5, 2.5)) : struct(float),

        begin("docall_struct_poly_generic_args", !IO),
        docall_struct_poly_generic_args(SPoly, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO),

        begin("docall_struct_poly_float_args", !IO),
        docall_struct_poly_float_args(SPoly, 3.5, Res2),
        io.write_float(Res2, !IO),
        io.nl(!IO),

        begin("docall_struct_mono", !IO),
        docall_struct_mono(SMono, 3.5, Res3),
        io.write_float(Res3, !IO),
        io.nl(!IO)
    ),

    begin("docall_struct_both", !IO),
    some [S, Res1, Res2] (
        S = struct_both(P2, P2),
        docall_struct_both(S, 3.5, Res1, 3.5, Res2),
        io.write_float(Res1, !IO),
        io.nl(!IO),
        io.write_float(Res2, !IO),
        io.nl(!IO)
    ),

    some [T, Res1, Res2] (
        T = foo(1.5, 2.5) - "foo",

        begin("docall_pair_poly", !IO),
        docall_pair_poly(T, 3.5, Res1),
        io.write_float(Res1, !IO),
        io.nl(!IO),

        begin("docall_pair_float", !IO),
        docall_pair_float(T, 3.5, Res2),
        io.write_float(Res2, !IO),
        io.nl(!IO)
    ),

    some [T, Res1, Res2, Res3] (
        T = {foo(1.5, 2.5)},

        begin("docall_tuple_poly", !IO),
        docall_tuple_poly(T, 3.5, Res1),
        io.write_float(Res1, !IO),
        io.nl(!IO),

        begin("docall_tuple_float", !IO),
        docall_tuple_float(T, 3.5, Res2),
        io.write_float(Res2, !IO),
        io.nl(!IO),

        begin("docall_tuple2_float", !IO),
        docall_tuple2_float({T}, 3.5, Res3),
        io.write_float(Res3, !IO),
        io.nl(!IO)
    ),

    begin("get_struct_pred_switch", !IO),
    some [S, Pred, Res] (
        S = struct_poly(P2),
        get_struct_pred_switch(S, Pred),
        Pred(3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("get_struct_pred_disj", !IO),
    some [S, Res] (
        S = struct_poly(P2),
        unsorted_solutions(
            (pred(R::out) is multi :-
                get_struct_pred_disj(S, P),
                P(3.5, R)
            ), Res),
        io.write(Res, !IO),
        io.nl(!IO)
    ),

    begin("get_struct_pred_ite", !IO),
    some [S, Pred, Res] (
        S = struct_poly(P2),
        get_struct_pred_ite(S, Pred),
        Pred(3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    some [Res] (
        begin("meth1", !IO),
        meth1(1.5, 2.5, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    some [Res] (
        begin("meth2", !IO),
        meth2(1.5, 2.5, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    some [Res] (
        begin("meth3", !IO),
        meth3(P2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    some [Res] (
        begin("meth4", !IO),
        meth4(P1, 2.5, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    some [Res] (
        begin("meth5", !IO),
        meth5(DoCall),
        DoCall(P2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("cast_inst", !IO),
    some [S1, S2, Res] (
        S1 = struct_poly(P2),
        cast_inst(S1, S2),
        docall_struct_poly_float_args(S2, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    begin("lost_inst", !IO),
    some [S1, S2, S3, S2_Cast, Res] (
        S1 = struct_poly(P2),
        lost_inst(S1, S2),
        cast_inst(S2, S3),
        docall_struct_mono(S3, 3.5, Res),
        io.write_float(Res, !IO),
        io.nl(!IO)
    ),

    some [T, Res] (
        begin("map_recursive_type", !IO),
        T = foo(1.5, 2.5),
        map_recursive_type(cons(T, cons(T, cons(T, nil))), 3.5, Res),
        io.write(Res, !IO),
        io.nl(!IO)
    ).

%---------------------------------------------------------------------------%

:- pred begin(string::in, io::di, io::uo) is det.

begin(Message, !IO) :-
    io.nl(!IO),
    io.write_string(Message, !IO),
    io.nl(!IO),
    clear_float_regs(!IO).

:- pred clear_float_regs(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    clear_float_regs(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#if defined(MR_f)
    MR_f(1) = 0.0;
    MR_f(2) = 0.0;
    MR_f(3) = 0.0;
    MR_f(4) = 0.0;
    MR_f(5) = 0.0;
    MR_f(6) = 0.0;
    MR_f(7) = 0.0;
    MR_f(8) = 0.0;
    MR_f(9) = 0.0;
    MR_f(10) = 0.0;
#endif
").

clear_float_regs(!IO).

%---------------------------------------------------------------------------%

:- pred foo(float::in, float::in, float::in, float::out) is det.

foo(A, B, C, X) :-
    X = A + B*B + C*C*C.

%---------------------------------------------------------------------------%

:- pred docall(pred(T, T)::in(pred(in, out) is det), T::in, T::out) is det.
:- pragma no_inline(docall/3).

docall(P, X, Y) :-
    P(X, Y).

:- pred docall_inline(pred(T, T)::in(pred(in, out) is det), T::in, T::out)
    is det.
:- pragma inline(docall_inline/3).

docall_inline(P, X, Y) :-
    P(X, Y).

%---------------------------------------------------------------------------%

:- pred get_docall(pred(pred(T1, T1), T1, T1)).
:- mode get_docall(out(pred(pred(in, out) is det, in, out) is det)) is det.

:- pragma no_inline(get_docall/1).

get_docall(docall).

:- pred get_docall2(pred(pred(pred(T1, T1), T1, T1))).
:- mode get_docall2(out(pred(out(pred((pred(in, out) is det), in, out) is det))
    is det)) is det.
:- pragma no_inline(get_docall2/1).

get_docall2(get_docall).

:- pred get_docall3(pred(pred(pred(pred(T1, T1), T1, T1)))).
:- mode get_docall3(out(pred(out(pred(out(pred((pred(in, out) is det), in, out)
    is det)) is det)) is det)) is det.
:- pragma no_inline(get_docall3/1).

get_docall3(get_docall2).

%---------------------------------------------------------------------------%

:- pred docall_foreign_poly(pred(T, T)::in(pred(in, out) is det),
    T::in, T::out) is det.

docall_foreign_poly(P, X, Y) :- % for non-C backends
    P(X, Y).

:- pragma foreign_proc("C",
    docall_foreign_poly(P::in(pred(in, out) is det), X::in, Y::out),
    [may_call_mercury, promise_pure, thread_safe],
"
    EXPORT_docall_foreign_2(TypeInfo_for_T, P, X, &Y);
").

:- pred docall_foreign_2(pred(T, T)::in(pred(in, out) is det), T::in, T::out)
    is det.
:- pragma foreign_export("C",
    docall_foreign_2(in(pred(in, out) is det), in, out),
    "EXPORT_docall_foreign_2").

docall_foreign_2(P, X, Y) :-
    P(X, Y).

%---------------------------------------------------------------------------%

:- pred docall_foreign_float(pred(float, float)::in(pred(in, out) is det),
    float::in, float::out) is det.

:- pragma foreign_proc("C",
    docall_foreign_float(P::in(pred(in, out) is det), X::in, Y::out),
    [may_call_mercury, promise_pure, thread_safe],
"
    EXPORT_docall_foreignf_2(P, X, &Y);
").

docall_foreign_float(P, X, Y) :- % for non-C backends
    P(X, Y).

:- pred docall_foreign_float_2(pred(float, float)::in(pred(in, out) is det),
    float::in, float::out) is det.

:- pragma foreign_export("C",
    docall_foreign_float_2(in(pred(in, out) is det), in, out),
    "EXPORT_docall_foreignf_2").

docall_foreign_float_2(P, X, Y) :-
    P(X, Y).

%---------------------------------------------------------------------------%

:- type struct(T)
    --->    struct_poly(pred(T, T))
    ;       struct_mono(pred(float, float)).

:- inst struct
    --->    struct_poly(pred(in, out) is det)
    ;       struct_mono(pred(in, out) is det).

:- pred docall_struct_poly_generic_args(struct(T)::in(struct), T::in, T::out)
    is det.
:- pragma no_inline(docall_struct_poly_generic_args/3).

docall_struct_poly_generic_args(S, X, Y) :-
    (
        S = struct_poly(P),
        P(X, Y)
    ;
        S = struct_mono(_),
        unexpected($module, $pred)
    ).

:- pred docall_struct_poly_float_args(struct(float)::in(struct),
    float::in, float::out) is det.
:- pragma no_inline(docall_struct_poly_float_args/3).

docall_struct_poly_float_args(S, X, Y) :-
    (
        S = struct_poly(P),
        P(X, Y)
    ;
        S = struct_mono(_),
        unexpected($module, $pred)
    ).

:- pred docall_struct_mono(struct(T)::in(struct), float::in, float::out)
    is det.
:- pragma no_inline(docall_struct_mono/3).

docall_struct_mono(S, X, Y) :-
    (
        S = struct_mono(P),
        P(X, Y)
    ;
        S = struct_poly(_),
        unexpected($module, $pred)
    ).

%---------------------------------------------------------------------------%

:- type struct_both(T)
    --->    struct_both(pred(T, T), pred(float, float)).

:- inst struct_both
    --->    struct_both(pred(in, out) is det, pred(in, out) is det).

:- pred docall_struct_both(struct_both(T)::in(struct_both), T::in, T::out,
    float::in, float::out) is det.
:- pragma no_inline(docall_struct_both/5).

docall_struct_both(struct_both(P, Q), !X, !Y) :-
    P(!X),
    Q(!Y).

%---------------------------------------------------------------------------%

:- inst pairpred
    --->    (pred(in, out) is det) - ground.

:- pred docall_pair_poly(pair(pred(T, T), U)::in(pairpred), T::in, T::out)
    is det.
:- pragma no_inline(docall_pair_poly/3).

docall_pair_poly(P - _, X, Y) :-
    P(X, Y).

:- pred docall_pair_float(pair(pred(float, float), string)::in(pairpred),
    float::in, float::out) is det.
:- pragma no_inline(docall_pair_float/3).

docall_pair_float(P - _, X, Y) :-
    P(X, Y).

%---------------------------------------------------------------------------%

:- inst tuplepred
    --->    { pred(in, out) is det }.

:- inst tupletuplepred
    --->    { tuplepred }.

:- pred docall_tuple_poly({ pred(T, T) }::in(tuplepred), T::in, T::out) is det.
:- pragma no_inline(docall_tuple_poly/3).

docall_tuple_poly({P}, X, Y) :-
    P(X, Y).

:- pred docall_tuple_float({ pred(float, float) }::in(tuplepred),
    float::in, float::out) is det.
:- pragma no_inline(docall_tuple_float/3).

docall_tuple_float({P}, X, Y) :-
    P(X, Y).

:- pred docall_tuple2_float({{ pred(float, float) }}::in(tupletuplepred),
    float::in, float::out) is det.
:- pragma no_inline(docall_tuple2_float/3).

docall_tuple2_float({{P}}, X, Y) :-
    P(X, Y).

%---------------------------------------------------------------------------%

    % Test inst merging in switches.
    %
:- pred get_struct_pred_switch(struct(float)::in(struct),
    pred(float, float)::out(pred(in, out) is det)) is det.
:- pragma no_inline(get_struct_pred_switch/2).

get_struct_pred_switch(struct_poly(P), P).
get_struct_pred_switch(struct_mono(P), P).

    % Test inst merging in disjunctions.
    %
:- pred get_struct_pred_disj(struct(float)::in(struct),
    pred(float, float)::out(pred(in, out) is det)) is multi.
:- pragma no_inline(get_struct_pred_disj/2).

get_struct_pred_disj(struct_poly(P), P).
get_struct_pred_disj(struct_mono(P), P).
get_struct_pred_disj(_, foo(1.5, 2.5)).
get_struct_pred_disj(_, foo(1.5, 2.5)).

    % Test insts merging in if-then-elses.
    %
:- pred get_struct_pred_ite(struct(float)::in(struct),
    pred(float, float)::out(pred(in, out) is det)) is det.
:- pragma no_inline(get_struct_pred_ite/2).

get_struct_pred_ite(S, P) :-
    ( if S = struct_poly(P0) then
        P = P0
    else if S = struct_mono(P0) then
        P = P0
    else
        P = foo(-1.5, -2.5) % dummy
    ).

%---------------------------------------------------------------------------%

:- typeclass tc(T) where [
    pred meth1(T::in, T::in, T::in, T::out) is det,
    pred meth2(T::in, T::in, T::in, T::out) is det,
    pred meth3(pred(T, T)::in(pred(in, out) is det), T::in, T::out) is det,
    pred meth4(pred(float, T, T)::in(pred(in, in, out) is det), float::in,
        T::in, T::out) is det,
    pred meth5(pred(pred(T, T), T, T)::out(pred(pred(in, out) is det, in, out)
        is det)) is det
].

:- instance tc(float) where [
    pred(meth1/4) is foo,
    ( meth2(A, B, C, D) :-
        foo(A, B, C, D)
    ),
    ( meth3(P, C, D) :-
        P(C, D)
    ),
    ( meth4(P, B, C, D) :-
        P(B, C, D)
    ),
    pred(meth5/1) is get_docall
].

%---------------------------------------------------------------------------%

    % Check we are able to introduce wrappers after losing the inst
    % then "recovering" it.
    %
:- pred lost_inst(struct(float)::in, struct(float)::out) is det.
:- pragma no_inline(lost_inst/2).

lost_inst(S0, S) :-
    cast_inst(S0, S1),
    (
        S1 = struct_poly(P),
        S = struct_mono(P)  % wrapper here
    ;
        S1 = struct_mono(P),
        S = struct_mono(P)
    ).

:- pred cast_inst(struct(float)::in, struct(float)::out(struct)) is det.

:- pragma foreign_proc("C",
    cast_inst(S0::in, S::out(struct)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = S0;
").
:- pragma foreign_proc("C#",
    cast_inst(S0::in, S::out(struct)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = S0;
").
:- pragma foreign_proc("Java",
    cast_inst(S0::in, S::out(struct)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = S0;
").

%---------------------------------------------------------------------------%

:- type recursive_type(T)
    --->    nil
    ;       cons(pred(T, T), recursive_type(T)).

:- inst recursive_inst
    --->    nil
    ;       cons(pred(in, out) is det, recursive_inst).

:- pred map_recursive_type(recursive_type(T)::in(recursive_inst),
    T::in, list(T)::out) is det.

map_recursive_type(nil, _, []).
map_recursive_type(cons(P, Ps), X, [Y | Ys]) :-
    P(X, Y),
    map_recursive_type(Ps, X, Ys).

%---------------------------------------------------------------------------%

:- type existstruct(T)
    --->    some [U] existstruct(pred(T, T), U).

:- inst existstruct
    --->    existstruct(pred(in, out) is det, ground).

% XXX mode checking fails
%
% :- pred docall_existstruct(existstruct(T)::in(existstruct), T::in, T::out)
%     is det.
% :- pragma no_inline(docall_existstruct/3).
%
% docall_existstruct(existstruct(P, _), X, Y) :-
%     P(X, Y).
