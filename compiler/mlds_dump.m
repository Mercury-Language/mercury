%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module dumps out MLDS code in a form designed to help debug
% MLDS code generation and optimization, by making MLDS code fragments
% as easy to understand as possible. To this end, the output generated
% by this module
%
% - intentionally omits details that are only rarely relevant during
%   such debugging, and
%
% - presents the remaining details in a very direct, simple and unambiguous
%   format that is not constrained by the syntax of any actual programming
%   language.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_dump.
:- interface.

:- import_module ml_backend.mlds.

:- import_module io.

:- pred dump_mlds_stmt(int::in, mlds_stmt::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

:- type strcord == cord(string).

:- func strcord_to_string(strcord) = string.

strcord_to_string(Cord) = string.append_list(cord.to_list(Cord)).

%---------------------------------------------------------------------------%

dump_mlds_stmt(Indent, Stmt, !IO) :-
    Cord = mlds_stmt_to_strcord(Indent, Stmt),
    io.write_string(strcord_to_string(Cord), !IO).

%---------------------------------------------------------------------------%

:- func mlds_stmts_to_strcord(int, list(mlds_stmt)) = strcord.

mlds_stmts_to_strcord(_Indent, []) = cord.init.
mlds_stmts_to_strcord(Indent, [Stmt | Stmts]) =
    mlds_stmt_to_strcord(Indent, Stmt) ++
    mlds_stmts_to_strcord(Indent, Stmts).

:- func mlds_stmt_to_strcord(int, mlds_stmt) = strcord.

mlds_stmt_to_strcord(Indent, Stmt) = Cord :-
    (
        Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, _Context),
        (
            LocalVarDefns = [],
            LocalVarsCord = cord.init
        ;
            LocalVarDefns = [_ | _],
            LocalVarsCord =
                mlds_local_var_defns_to_strcord(Indent + 1, LocalVarDefns) ++
                nl_strcord
        ),
        (
            FuncDefns = [],
            FuncsCord = cord.init
        ;
            FuncDefns = [_ | _],
            FuncsCord =
                mlds_function_defns_to_strcord(Indent + 1, FuncDefns) ++
                nl_strcord
        ),
        Cord =
            indent_strcord(Indent) ++ strcord("block start\n") ++
            LocalVarsCord ++
            FuncsCord ++
            mlds_stmts_to_strcord(Indent + 1, SubStmts) ++
            indent_strcord(Indent) ++ strcord("block end\n")
    ;
        Stmt = ml_stmt_while(Kind, Rval, SubStmt, LocalLoopVars, _Context),
        (
            LocalLoopVars = [],
            LocalLoopVarsCord = cord.init
        ;
            LocalLoopVars = [_ | _],
            LocalLoopVarsCord =
                indent_strcord(Indent) ++ strcord("loop local vars ") ++
                strcord(string.join_list(", ",
                    list.map(ml_local_var_name_to_string, LocalLoopVars))) ++
                nl_strcord
        ),
        (
            Kind = may_loop_zero_times,
            Cord =
                indent_strcord(Indent) ++ strcord("while ") ++
                    mlds_rval_to_strcord(Rval) ++ nl_strcord ++
                mlds_stmt_to_strcord(Indent + 1, SubStmt) ++
                indent_strcord(Indent) ++ strcord("end while\n") ++
                LocalLoopVarsCord
        ;
            Kind = loop_at_least_once,
            Cord =
                indent_strcord(Indent) ++ strcord("do\n") ++
                mlds_stmt_to_strcord(Indent + 1, SubStmt) ++
                indent_strcord(Indent) ++ strcord("while ") ++
                    mlds_rval_to_strcord(Rval) ++ nl_strcord ++
                LocalLoopVarsCord
        )
    ;
        Stmt = ml_stmt_if_then_else(Cond, Then, MaybeElse, _Context),
        (
            MaybeElse = yes(Else),
            Cord =
                indent_strcord(Indent) ++ strcord("if ") ++
                    mlds_rval_to_strcord(Cond) ++ nl_strcord ++
                indent_strcord(Indent) ++ strcord("then\n") ++
                mlds_stmt_to_strcord(Indent + 1, Then) ++
                indent_strcord(Indent) ++ strcord("else\n") ++
                mlds_stmt_to_strcord(Indent + 1, Else) ++
                indent_strcord(Indent) ++ strcord("end if\n")
        ;
            MaybeElse = no,
            Cord =
                indent_strcord(Indent) ++ strcord("if ") ++
                    mlds_rval_to_strcord(Cond) ++ nl_strcord ++
                indent_strcord(Indent) ++ strcord("then\n") ++
                mlds_stmt_to_strcord(Indent + 1, Then) ++
                indent_strcord(Indent) ++ strcord("end if\n")
        )
    ;
        Stmt = ml_stmt_switch(Type, Rval, Range, Cases, Default, _Context),
        (
            Range = mlds_switch_range_unknown,
            RangeCord = cord.init
        ;
            Range = mlds_switch_range(Min, Max),
            RangeCord =
                strcord(" from ") ++ intcord(Min) ++
                strcord(" to ") ++ intcord(Max)
        ),
        Cord =
            indent_strcord(Indent) ++ strcord("switch ") ++
                strcord("(") ++ mlds_rval_to_strcord(Rval) ++ strcord(")") ++
                mlds_type_to_strcord(Type) ++ RangeCord ++ nl_strcord ++
            cord_list_to_cord(
                list.map(mlds_switch_case_to_strcord(Indent + 1), Cases)) ++
            mlds_switch_default_to_strcord(Indent + 1, Default) ++
            indent_strcord(Indent) ++ strcord("end switch\n")
    ;
        Stmt = ml_stmt_label(Label, _Context),
        Cord =
            indent_strcord(Indent) ++ strcord("label ") ++ strcord(Label) ++
                nl_strcord
    ;
        Stmt = ml_stmt_goto(Target, _Context),
        Cord =
            indent_strcord(Indent) ++ strcord("goto ") ++
                mlds_goto_target_to_strcord(Target) ++ nl_strcord
    ;
        Stmt = ml_stmt_computed_goto(Rval, Labels, _Context),
        Cord =
            indent_strcord(Indent) ++ strcord("computed goto ") ++
                strcord("(") ++ mlds_rval_to_strcord(Rval) ++ strcord(")") ++
                nl_strcord ++
                mlds_computed_goto_labels_to_strcord(Indent, 0, Labels)
    ;
        Stmt = ml_stmt_call(_Sig, FuncRval, ArgRvals, RetLvals, TailCall,
            _Context),
        (
            TailCall = no_return_call,
            CallCord = strcord("no_return_call ")
        ;
            TailCall = tail_call,
            CallCord = strcord("tail_call ")
        ;
            TailCall = ordinary_call,
            CallCord = strcord("call ")
        ),
        (
            ArgRvals = [],
            ArgsCord = cord.init
        ;
            ArgRvals = [HeadArgRval | TailArgRvals],
            ArgsCord = strcord("(") ++
                mlds_rvals_to_strcord(HeadArgRval, TailArgRvals) ++
                strcord(")")
        ),
        (
            RetLvals = [],
            RetCord = cord.init
        ;
            RetLvals = [HeadRetLval | TailRetLvals],
            RetCord = strcord(" -> (") ++
                mlds_lvals_to_strcord(HeadRetLval, TailRetLvals) ++
                strcord(")")
        ),
        Cord =
            indent_strcord(Indent) ++ CallCord ++
                mlds_rval_to_strcord(FuncRval) ++ ArgsCord ++ RetCord
    ;
        Stmt = ml_stmt_return(Rvals, _Context),
        (
            Rvals = [],
            Cord =
                indent_strcord(Indent) ++ strcord("return")
        ;
            Rvals = [HeadRval | TailRvals],
            Cord =
                indent_strcord(Indent) ++ strcord("return ") ++
                    mlds_rvals_to_strcord(HeadRval, TailRvals)
        )
    ;
        Stmt = ml_stmt_try_commit(RefLval, BodyStmt, HandlerStmt, _Context),
        Cord =
            indent_strcord(Indent) ++ strcord("try_commit ") ++
                mlds_lval_to_strcord(RefLval) ++ nl_strcord ++
            indent_strcord(Indent) ++ strcord("stmt to try\n") ++
            mlds_stmt_to_strcord(Indent + 1, BodyStmt) ++
            indent_strcord(Indent) ++ strcord("commit handler stmt\n") ++
            mlds_stmt_to_strcord(Indent + 1, HandlerStmt) ++
            indent_strcord(Indent) ++ strcord("end_try_commit\n")
    ;
        Stmt = ml_stmt_do_commit(RefRval, _Context),
        Cord =
            indent_strcord(Indent) ++ strcord("do_commit ") ++
                mlds_rval_to_strcord(RefRval)
    ;
        Stmt = ml_stmt_atomic(AtomicStmt, _Context),
        Cord = mlds_atomic_stmt_to_strcord(Indent, AtomicStmt)
    ).

:- func mlds_switch_case_to_strcord(int, mlds_switch_case) = strcord.

mlds_switch_case_to_strcord(Indent, Case) = Cord :-
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt),
    Cord =
        mlds_case_match_cond_to_strcord(Indent, FirstCond) ++
        cord_list_to_cord(
            list.map(mlds_case_match_cond_to_strcord(Indent), LaterConds)) ++
        mlds_stmt_to_strcord(Indent + 1, Stmt).

:- func mlds_case_match_cond_to_strcord(int, mlds_case_match_cond) = strcord.

mlds_case_match_cond_to_strcord(Indent, Cond) = Cord :-
    (
        Cond = match_value(Rval),
        Cord =
            indent_strcord(Indent) ++ strcord("match value ") ++
                mlds_rval_to_strcord(Rval) ++ nl_strcord
    ;
        Cond = match_range(MinRval, MaxRval),
        Cord =
            indent_strcord(Indent) ++ strcord("match range ") ++
                mlds_rval_to_strcord(MinRval) ++ strcord(" to ") ++
                mlds_rval_to_strcord(MaxRval) ++ nl_strcord
    ).

:- func mlds_switch_default_to_strcord(int, mlds_switch_default) = strcord.

mlds_switch_default_to_strcord(Indent, Default) = Cord :-
    (
        Default = default_is_unreachable,
        Cord = indent_strcord(Indent) ++ strcord("default is unreachable\n")
    ;
        Default = default_do_nothing,
        Cord = indent_strcord(Indent) ++ strcord("default is do nothing\n")
    ;
        Default = default_case(Stmt),
        Cord =
            indent_strcord(Indent) ++ strcord("default statement:\n") ++
            mlds_stmt_to_strcord(Indent + 1, Stmt)
    ).

:- func mlds_goto_target_to_strcord(mlds_goto_target) = strcord.

mlds_goto_target_to_strcord(Target) = Cord :-
    (
        Target = goto_label(Label),
        Cord = strcord(Label)
    ;
        Target = goto_break_switch,
        Cord = strcord("break_switch")
    ;
        Target = goto_break_loop,
        Cord = strcord("break_loop")
    ;
        Target = goto_continue_loop,
        Cord = strcord("continue_loop")
    ).

:- func mlds_computed_goto_labels_to_strcord(int, int, list(mlds_label))
    = strcord.

mlds_computed_goto_labels_to_strcord(_Indent, _Index, []) = cord.init.
mlds_computed_goto_labels_to_strcord(Indent, Index, [Label | Labels]) =
    indent_strcord(Indent) ++ intcord(Index) ++ strcord(": ") ++
        strcord(Label) ++ nl_strcord ++
    mlds_computed_goto_labels_to_strcord(Indent, Index + 1, Labels).

:- func mlds_atomic_stmt_to_strcord(int, mlds_atomic_statement) = strcord.

mlds_atomic_stmt_to_strcord(Indent, AtomicStmt) = Cord :-
    (
        AtomicStmt = comment(Comment0),
        string.replace_all(Comment0, "\n", " ", Comment),
        Cord = indent_strcord(Indent) ++ strcord(Comment) ++ nl_strcord
    ;
        AtomicStmt = gc_check,
        Cord = indent_strcord(Indent) ++ strcord("gc_check\n")
    ;
        AtomicStmt = assign(Lval, Rval),
        Cord =
            indent_strcord(Indent) ++ mlds_lval_to_strcord(Lval) ++
                strcord(" := ") ++ mlds_rval_to_strcord(Rval) ++ nl_strcord
    ;
        AtomicStmt = assign_if_in_heap(Lval, Rval),
        Cord =
            indent_strcord(Indent) ++ mlds_lval_to_strcord(Lval) ++
                strcord(" ?:= ") ++ mlds_rval_to_strcord(Rval) ++ nl_strcord
    ;
        AtomicStmt = delete_object(Rval),
        Cord =
            indent_strcord(Indent) ++ strcord("delete object ") ++
                mlds_rval_to_strcord(Rval) ++ nl_strcord
    ;
        AtomicStmt = new_object(Target, Ptag, ExplicitSecTag, Type,
            MaybeSize, _MaybeCtorName, _ArgRvalsTypes, _MayUseAtomic,
            _MaybeAllocId),
        (
            ExplicitSecTag = no,
            SecTagCord = strcord("no explicit sectag")
        ;
            ExplicitSecTag = yes,
            SecTagCord = strcord("explicit sectag")
        ),
        (
            MaybeSize = no,
            SizeCord = strcord("no size")
        ;
            MaybeSize = yes(Size),
            SizeCord = mlds_rval_to_strcord(Size) ++ strcord(" words")
        ),
        Cord =
            indent_strcord(Indent) ++ mlds_lval_to_strcord(Target) ++
                strcord(" := new object(ptag ") ++ intcord(Ptag) ++
                comma_cord ++ SecTagCord ++ comma_cord ++ SizeCord ++
                comma_cord ++ mlds_type_to_strcord(Type) ++ strcord(")") ++
                nl_strcord
    ;
        AtomicStmt = mark_hp(Lval),
        Cord =
            indent_strcord(Indent) ++ strcord("mark_hp ") ++
                mlds_lval_to_strcord(Lval) ++ nl_strcord
    ;
        AtomicStmt = restore_hp(Rval),
        Cord =
            indent_strcord(Indent) ++ strcord("restore_hp ") ++
                mlds_rval_to_strcord(Rval) ++ nl_strcord
    ;
        AtomicStmt = trail_op(TrailOp),
        (
            TrailOp = store_ticket(Lval),
            Cord =
                indent_strcord(Indent) ++ strcord("store_ticket ") ++
                    mlds_lval_to_strcord(Lval) ++ nl_strcord
        ;
            TrailOp = reset_ticket(Rval, Reason),
            (
                Reason = undo,
                ReasonCord = strcord("undo ")
            ;
                Reason = commit,
                ReasonCord = strcord("commit ")
            ;
                Reason = solve,
                ReasonCord = strcord("solve ")
            ;
                Reason = exception,
                ReasonCord = strcord("exception ")
            ;
                Reason = gc,
                ReasonCord = strcord("gc ")
            ),
            Cord =
                indent_strcord(Indent) ++ strcord("reset_ticket for ") ++
                    ReasonCord ++ mlds_rval_to_strcord(Rval) ++ nl_strcord
        ;
            TrailOp = discard_ticket,
            Cord =
                indent_strcord(Indent) ++ strcord("discard_ticket") ++
                    nl_strcord
        ;
            TrailOp = prune_ticket,
            Cord =
                indent_strcord(Indent) ++ strcord("prune_ticket") ++
                    nl_strcord
        ;
            TrailOp = mark_ticket_stack(Lval),
            Cord =
                indent_strcord(Indent) ++ strcord("mark_ticket_stack ") ++
                    mlds_lval_to_strcord(Lval) ++ nl_strcord
        ;
            TrailOp = prune_tickets_to(Rval),
            Cord =
                indent_strcord(Indent) ++ strcord("prune_tickets_to ") ++
                    mlds_rval_to_strcord(Rval) ++ nl_strcord
        )
    ;
        AtomicStmt = inline_target_code(_Lang, Components),
        Cord =
            indent_strcord(Indent) ++ strcord("inline_target_code\n") ++
            cord_list_to_cord(
                list.map(target_code_component_to_strcord(Indent + 1),
                    Components)) ++
            indent_strcord(Indent) ++ strcord("end inline_target_code\n")
    ;
        AtomicStmt = outline_foreign_proc(_Lang, OutlineArgs, ReturnLvals,
            Code),
        (
            ReturnLvals = [],
            ReturnCord =
                indent_strcord(Indent + 1) ++ strcord("no return lvals\n")
        ;
            ReturnLvals = [HeadLval | TailLvals],
            ReturnCord =
                indent_strcord(Indent + 1) ++ strcord("return lvals ") ++
                    mlds_lvals_to_strcord(HeadLval, TailLvals) ++ nl_strcord
        ),
        Cord =
            indent_strcord(Indent) ++ strcord("inline_target_code\n") ++
            cord_list_to_cord(
                list.map(outline_arg_to_strcord(Indent + 1),
                    OutlineArgs)) ++
            ReturnCord ++
            indent_strcord(Indent + 1) ++ strcord("code\n") ++
            strcord(Code) ++
            indent_strcord(Indent) ++ strcord("end inline_target_code\n")
    ).

:- func target_code_component_to_strcord(int, target_code_component) = strcord.

target_code_component_to_strcord(Indent, Component) = Cord :-
    (
        Component = user_target_code(Code, _Context),
        Cord =
            indent_strcord(Indent) ++ strcord("user_target_code") ++
                nl_strcord ++
            strcord(Code) ++ nl_strcord
    ;
        Component = raw_target_code(Code),
        Cord =
            indent_strcord(Indent) ++ strcord("raw_target_code") ++
                nl_strcord ++
            strcord(Code) ++ nl_strcord
    ;
        Component = target_code_input(Rval),
        Cord =
            indent_strcord(Indent) ++ strcord("input ") ++
            mlds_rval_to_strcord(Rval) ++ nl_strcord
    ;
        Component = target_code_output(Lval),
        Cord =
            indent_strcord(Indent) ++ strcord("output ") ++
            mlds_lval_to_strcord(Lval) ++ nl_strcord
    ;
        Component = target_code_type(Type),
        Cord =
            indent_strcord(Indent) ++ strcord("type ") ++
            mlds_type_to_strcord(Type) ++ nl_strcord
    ;
        Component = target_code_function_name(_QualFuncName),
        Cord = cord.init
    ;
        Component = target_code_alloc_id(_AllocId),
        Cord = cord.init
    ).

:- func outline_arg_to_strcord(int, outline_arg) = strcord.

outline_arg_to_strcord(Indent, OutlineArg) = Cord :-
    (
        OutlineArg = ola_in(Type, Name, Rval),
        Cord =
            indent_strcord(Indent) ++ strcord("input ") ++
                mlds_type_to_strcord(Type) ++ strcord(" ") ++
                strcord(Name) ++ strcord(" <= ") ++
                mlds_rval_to_strcord(Rval) ++ nl_strcord
    ;
        OutlineArg = ola_out(Type, Name, Lval),
        Cord =
            indent_strcord(Indent) ++ strcord("output ") ++
                mlds_type_to_strcord(Type) ++ strcord(" ") ++
                strcord(Name) ++ strcord(" => ") ++
                mlds_lval_to_strcord(Lval) ++ nl_strcord
    ;
        OutlineArg = ola_unused,
        Cord =
            indent_strcord(Indent) ++ strcord("unused\n")
    ).

%---------------------------------------------------------------------------%

:- func mlds_lvals_to_strcord(mlds_lval, list(mlds_lval)) = strcord.

mlds_lvals_to_strcord(HeadLval, TailLvals) = Cord :-
    (
        TailLvals = [],
        Cord = mlds_lval_to_strcord(HeadLval)
    ;
        TailLvals = [HeadTailLval | TailTailLvals],
        Cord = mlds_lval_to_strcord(HeadLval) ++ comma_cord ++
            mlds_lvals_to_strcord(HeadTailLval, TailTailLvals)
    ).

:- func mlds_lval_to_strcord(mlds_lval) = strcord.

mlds_lval_to_strcord(Lval) = Cord :-
    (
        Lval = ml_local_var(LocalVar, _Type),
        Cord = strcord(ml_local_var_name_to_string(LocalVar))
    ;
        Lval = ml_global_var(QualGlobalVar, _Type),
        QualGlobalVar = qual_global_var_name(_ModuleName, GlobalVar),
        (
            GlobalVar = gvn_rtti_var(_RttiId),
            Cord = strcord("rtti_id")
        ;
            GlobalVar = gvn_tabling_var(_, _),
            Cord = strcord("tabling_var")
        ;
            GlobalVar = gvn_const_var(GlobalConstVar, SeqNum),
            (
                GlobalConstVar = mgcv_const_var,
                GlobalCord = strcord("global const var ")
            ;
                GlobalConstVar = mgcv_float,
                GlobalCord = strcord("global float var ")
            ;
                GlobalConstVar = mgcv_int64,
                GlobalCord = strcord("global int64 var ")
            ;
                GlobalConstVar = mgcv_uint64,
                GlobalCord = strcord("global uint64 var ")
            ;
                GlobalConstVar = mgcv_closure_layout,
                GlobalCord = strcord("global closure layout ")
            ;
                GlobalConstVar = mgcv_typevar_vector,
                GlobalCord = strcord("global typevar vector ")
            ;
                GlobalConstVar = mgcv_bit_vector,
                GlobalCord = strcord("global bit vector ")
            ),
            Cord = GlobalCord ++ intcord(SeqNum)
        ;
            GlobalVar = gvn_dummy_var,
            Cord = strcord("dummy_var")
        )
    ;
        Lval = ml_target_global_var_ref(GlobalVarRef),
        GlobalVarRef = env_var_ref(EnvVar),
        Cord = strcord("env_var_ref(") ++ strcord(EnvVar) ++ strcord(")")
    ;
        Lval = ml_mem_ref(AddrRval, _Type),
        Cord = strcord("mem_ref(") ++ mlds_rval_to_strcord(AddrRval) ++
            strcord(")")
    ;
        Lval = ml_field(MaybePtag, AddrRval, FieldId, _FieldType, _PtrType),
        (
            MaybePtag = no,
            PtagCord = strcord("ptag unknown")
        ;
            MaybePtag = yes(Ptag),
            PtagCord = strcord("ptag " ) ++ intcord(Ptag)
        ),
        (
            FieldId = ml_field_offset(OffsetRval),
            FieldCord = mlds_rval_to_strcord(OffsetRval)
        ;
            FieldId = ml_field_named(QualVarName, _FieldIdType),
            QualVarName = qual_field_var_name(_ModuleName, _QualKind,
                FieldVarName),
            FieldCord = mlds_field_var_name_to_strcord(FieldVarName)
        ),
        Cord =
            strcord("field(") ++ PtagCord ++ comma_cord ++
            mlds_rval_to_strcord(AddrRval) ++ comma_cord ++
            FieldCord ++ strcord(")")
    ).

%---------------------------------------------------------------------------%

:- func mlds_rvals_to_strcord(mlds_rval, list(mlds_rval)) = strcord.

mlds_rvals_to_strcord(HeadRval, TailRvals) = Cord :-
    (
        TailRvals = [],
        Cord = mlds_rval_to_strcord(HeadRval)
    ;
        TailRvals = [HeadTailRval | TailTailRvals],
        Cord = mlds_rval_to_strcord(HeadRval) ++ comma_cord ++
            mlds_rvals_to_strcord(HeadTailRval, TailTailRvals)
    ).

:- func mlds_rval_to_strcord(mlds_rval) = strcord.

mlds_rval_to_strcord(Rval) = Cord :-
    (
        Rval = ml_lval(Lval),
        Cord = mlds_lval_to_strcord(Lval)
    ;
        Rval = ml_mkword(Ptag, SubRval),
        Cord = strcord("mkword(") ++ intcord(Ptag) ++ comma_cord ++
            mlds_rval_to_strcord(SubRval) ++ strcord(")")
    ;
        Rval = ml_const(RvalConst),
        Cord = mlds_rval_const_to_strcord(RvalConst)
    ;
        Rval = ml_box(FromType, SubRval),
        Cord = strcord("box(from ") ++ mlds_type_to_strcord(FromType) ++
            comma_cord ++ mlds_rval_to_strcord(SubRval) ++ strcord(")")
    ;
        Rval = ml_unbox(ToType, SubRval),
        Cord = strcord("unbox(to ") ++ mlds_type_to_strcord(ToType) ++
            comma_cord ++ mlds_rval_to_strcord(SubRval) ++ strcord(")")
    ;
        Rval = ml_cast(ToType, SubRval),
        Cord = strcord("cast(to ") ++ mlds_type_to_strcord(ToType) ++
            comma_cord ++ mlds_rval_to_strcord(SubRval) ++ strcord(")")
    ;
        Rval = ml_unop(UnOp, SubRvalA),
        Cord = unop_to_strcord(UnOp) ++ strcord("(") ++
            mlds_rval_to_strcord(SubRvalA) ++ strcord(")")
    ;
        Rval = ml_binop(BinOp, SubRvalA, SubRvalB),
        Cord = binop_to_strcord(BinOp) ++ strcord("(") ++
            mlds_rval_to_strcord(SubRvalA) ++ comma_cord ++
            mlds_rval_to_strcord(SubRvalB) ++ strcord(")")
    ;
        Rval = ml_mem_addr(SubLval),
        Cord = strcord("addr_of(") ++ mlds_lval_to_strcord(SubLval) ++
            strcord(")")
    ;
        Rval = ml_scalar_common(ScalarCommon),
        Cord = strcord("scalar_common(") ++
            mlds_scalar_common_to_strcord(ScalarCommon) ++ strcord(")")
    ;
        Rval = ml_scalar_common_addr(ScalarCommon),
        Cord = strcord("scalar_common_addr(") ++
            mlds_scalar_common_to_strcord(ScalarCommon) ++ strcord(")")
    ;
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval),
        Cord = strcord("vector_common_row_addr(") ++
            mlds_vector_common_to_strcord(VectorCommon) ++ comma_cord ++
            mlds_rval_to_strcord(RowRval) ++ strcord(")")
    ;
        Rval = ml_self(_Type),
        Cord = strcord("self")
    ).

:- func mlds_rval_const_to_strcord(mlds_rval_const) = strcord.

mlds_rval_const_to_strcord(Const) = Cord :-
    (
        Const = mlconst_true,
        Cord = strcord("true")
    ;
        Const = mlconst_false,
        Cord = strcord("false")
    ;
        Const = mlconst_int(Int),
        Cord = strcord(string.int_to_string(Int))
    ;
        Const = mlconst_uint(Uint),
        Cord = strcord(string.uint_to_string(Uint))
    ;
        Const = mlconst_int8(Int8),
        Cord = strcord(string.int8_to_string(Int8))
    ;
        Const = mlconst_uint8(Uint8),
        Cord = strcord(string.uint8_to_string(Uint8))
    ;
        Const = mlconst_int16(Int16),
        Cord = strcord(string.int16_to_string(Int16))
    ;
        Const = mlconst_uint16(Uint16),
        Cord = strcord(string.uint16_to_string(Uint16))
    ;
        Const = mlconst_int32(Int32),
        Cord = strcord(string.int32_to_string(Int32))
    ;
        Const = mlconst_uint32(Uint32),
        Cord = strcord(string.uint32_to_string(Uint32))
    ;
        Const = mlconst_int64(Int64),
        Cord = strcord(string.int64_to_string(Int64))
    ;
        Const = mlconst_uint64(Uint64),
        Cord = strcord(string.uint64_to_string(Uint64))
    ;
        Const = mlconst_enum(N, Type),
        Cord = strcord("enum(") ++ mlds_type_to_strcord(Type) ++ comma_cord ++
            intcord(N) ++ strcord(")")
    ;
        Const = mlconst_char(Char),
        Cord = strcord("char ") ++ intcord(Char)
    ;
        Const = mlconst_float(Float),
        Cord = strcord(string.float_to_string(Float))
    ;
        Const = mlconst_string(Str),
        Cord = strcord("""") ++ strcord(Str) ++ strcord("""")
    ;
        Const = mlconst_multi_string(_MultiStr),
        Cord = strcord("multi_string")
    ;
        Const = mlconst_foreign(_Lang, Str, _Type),
        Cord = strcord("foreign ") ++ strcord(Str)
    ;
        Const = mlconst_named_const(_TargetPrefixes, Str),
        Cord = strcord("named_const ") ++ strcord(Str)
    ;
        Const = mlconst_code_addr(CodeAddr),
        CodeAddr = mlds_code_addr(QualFuncLabel, _Signature),
        QualFuncLabel = qual_func_label(_ModuleName, FuncLabel),
        FuncLabelCord = mlds_func_label_to_strcord(FuncLabel),
        Cord = strcord("&") ++ FuncLabelCord
    ;
        Const = mlconst_data_addr_local_var(LocalVar),
        Cord = strcord("&") ++ strcord(ml_local_var_name_to_string(LocalVar))
    ;
        Const = mlconst_data_addr_global_var(_ModuleName, _GlobalVar),
        Cord = strcord("&global_var")
    ;
        Const = mlconst_data_addr_rtti(_ModuleName, _RttiId),
        Cord = strcord("&rtti")
    ;
        Const = mlconst_data_addr_tabling(_QualProcLabel, _TablingStructId),
        Cord = strcord("&tabling")
    ;
        Const = mlconst_null(_Type),
        Cord = strcord("null")
    ).

:- func mlds_scalar_common_to_strcord(mlds_scalar_common) = strcord.

mlds_scalar_common_to_strcord(ScalarCommon) = Cord :-
    ScalarCommon = ml_scalar_common(_ModuleName, _Type, TypeNum, RowNum),
    TypeNum = ml_scalar_common_type_num(TypeNumInt),
    Cord = strcord("scalar_common(type ") ++ intcord(TypeNumInt) ++
        comma_cord ++ strcord("row ") ++ intcord(RowNum) ++ strcord(")").

:- func mlds_vector_common_to_strcord(mlds_vector_common) = strcord.

mlds_vector_common_to_strcord(VectorCommon) = Cord :-
    VectorCommon = ml_vector_common(_ModuleName, _Type, TypeNum,
        StartRowNum, NumRows),
    TypeNum = ml_vector_common_type_num(TypeNumInt),
    Cord = strcord("vector_common(type ") ++ intcord(TypeNumInt) ++
        comma_cord ++ strcord("start row ") ++ intcord(StartRowNum) ++
        comma_cord ++ strcord("num rows ") ++ intcord(NumRows) ++ strcord(")").

:- func unop_to_strcord(unary_op) = strcord.

unop_to_strcord(UnOp) = Cord :-
    (
        UnOp = mktag,
        Cord = strcord("mktag")
    ;
        UnOp = tag,
        Cord = strcord("tag")
    ;
        UnOp = unmktag,
        Cord = strcord("unmktag")
    ;
        UnOp = strip_tag,
        Cord = strcord("strip_tag")
    ;
        UnOp = mkbody,
        Cord = strcord("mkbody")
    ;
        UnOp = unmkbody,
        Cord = strcord("unmkbody")
    ;
        UnOp = bitwise_complement(IntType),
        Cord = strcord("bitwise_complement<") ++
            int_type_to_strcord(IntType) ++ strcord(">")
    ;
        UnOp = logical_not,
        Cord = strcord("logical_not")
    ;
        UnOp = hash_string,
        Cord = strcord("hash_string1")
    ;
        UnOp = hash_string2,
        Cord = strcord("hash_string2")
    ;
        UnOp = hash_string3,
        Cord = strcord("hash_string3")
    ;
        UnOp = hash_string4,
        Cord = strcord("hash_string4")
    ;
        UnOp = hash_string5,
        Cord = strcord("hash_string5")
    ;
        UnOp = hash_string6,
        Cord = strcord("hash_string6")
    ;
        UnOp = dword_float_get_word0,
        Cord = strcord("float_get_word0")
    ;
        UnOp = dword_float_get_word1,
        Cord = strcord("float_get_word1")
    ;
        UnOp = dword_int64_get_word0,
        Cord = strcord("int64_get_word0")
    ;
        UnOp = dword_int64_get_word1,
        Cord = strcord("int64_get_word1")
    ;
        UnOp = dword_uint64_get_word0,
        Cord = strcord("uint64_get_word0")
    ;
        UnOp = dword_uint64_get_word1,
        Cord = strcord("uint64_get_word1")
    ).

:- func binop_to_strcord(binary_op) = strcord.

binop_to_strcord(BinOp) = Cord :-
    (
        ( BinOp = int_add(IntType), OpStr = "add"
        ; BinOp = int_sub(IntType), OpStr = "sub"
        ; BinOp = int_mul(IntType), OpStr = "mul"
        ; BinOp = int_div(IntType), OpStr = "div"
        ; BinOp = int_mod(IntType), OpStr = "mod"
        ; BinOp = unchecked_left_shift(IntType),  OpStr = "raw_left_shift"
        ; BinOp = unchecked_right_shift(IntType), OpStr = "raw_right_shift"
        ; BinOp = bitwise_and(IntType), OpStr = "bitwise_and"
        ; BinOp = bitwise_or(IntType),  OpStr = "bitwise_or"
        ; BinOp = bitwise_xor(IntType), OpStr = "bitwise_xor"
        ; BinOp = eq(IntType),      OpStr = "eq"
        ; BinOp = ne(IntType),      OpStr = "ne"
        ; BinOp = int_lt(IntType),  OpStr = "lt"
        ; BinOp = int_gt(IntType),  OpStr = "gt"
        ; BinOp = int_le(IntType),  OpStr = "le"
        ; BinOp = int_ge(IntType),  OpStr = "ge"
        ),
        Cord = strcord(OpStr) ++
            strcord("<") ++ int_type_to_strcord(IntType) ++ strcord(">")
    ;
        BinOp = unsigned_le,
        Cord = strcord("unsigned_le")
    ;
        BinOp = logical_and,
        Cord = strcord("logical_and")
    ;
        BinOp = logical_or,
        Cord = strcord("logical_or")
    ;
        BinOp = body,
        Cord = strcord("body")
    ;
        BinOp = string_unsafe_index_code_unit,
        Cord = strcord("string_raw_index_cu")
    ;
        BinOp = array_index(_Type),
        Cord = strcord("array_index")
    ;
        BinOp = offset_str_eq(Offset),
        Cord = strcord("offset_str_eq") ++
            strcord("<") ++ intcord(Offset) ++ strcord(">")
    ;
        BinOp = str_eq,
        Cord = strcord("str_eq")
    ;
        BinOp = str_ne,
        Cord = strcord("str_ne")
    ;
        BinOp = str_lt,
        Cord = strcord("str_lt")
    ;
        BinOp = str_gt,
        Cord = strcord("str_gt")
    ;
        BinOp = str_le,
        Cord = strcord("str_le")
    ;
        BinOp = str_ge,
        Cord = strcord("str_ge")
    ;
        BinOp = str_cmp,
        Cord = strcord("str_cmp")
    ;
        BinOp = float_plus,
        Cord = strcord("float_add")
    ;
        BinOp = float_minus,
        Cord = strcord("float_sub")
    ;
        BinOp = float_times,
        Cord = strcord("float_mul")
    ;
        BinOp = float_divide,
        Cord = strcord("float_div")
    ;
        BinOp = float_eq,
        Cord = strcord("float_eq")
    ;
        BinOp = float_ne,
        Cord = strcord("float_ne")
    ;
        BinOp = float_lt,
        Cord = strcord("float_lt")
    ;
        BinOp = float_gt,
        Cord = strcord("float_gt")
    ;
        BinOp = float_le,
        Cord = strcord("float_le")
    ;
        BinOp = float_ge,
        Cord = strcord("float_ge")
    ;
        BinOp = float_from_dword,
        Cord = strcord("float_from_dword")
    ;
        BinOp = int64_from_dword,
        Cord = strcord("int64_from_dword")
    ;
        BinOp = uint64_from_dword,
        Cord = strcord("uint64_from_dword")
    ;
        BinOp = pointer_equal_conservative,
        Cord = strcord("ptr_eq")
    ;
        BinOp = compound_eq,
        Cord = strcord("compound_eq")
    ;
        BinOp = compound_lt,
        Cord = strcord("compound_lt")
    ).

:- func int_type_to_strcord(int_type) = strcord.

int_type_to_strcord(IntType) = strcord(Str) :-
    ( IntType = int_type_int,    Str = "int"
    ; IntType = int_type_uint,   Str = "uint"
    ; IntType = int_type_int8,   Str = "int8"
    ; IntType = int_type_uint8,  Str = "uint8"
    ; IntType = int_type_int16,  Str = "int16"
    ; IntType = int_type_uint16, Str = "uint16"
    ; IntType = int_type_int32,  Str = "int32"
    ; IntType = int_type_uint32, Str = "uint32"
    ; IntType = int_type_int64,  Str = "int64"
    ; IntType = int_type_uint64, Str = "uint64"
    ).

%---------------------------------------------------------------------------%

:- func mlds_types_to_strcord(mlds_type, list(mlds_type)) = strcord.

mlds_types_to_strcord(HeadType, TailTypes) = Cord :-
    (
        TailTypes = [],
        Cord = mlds_type_to_strcord(HeadType)
    ;
        TailTypes = [HeadTailType | TailTailTypes],
        Cord = mlds_type_to_strcord(HeadType) ++ comma_cord ++
            mlds_types_to_strcord(HeadTailType, TailTailTypes)
    ).

:- func mercury_types_to_strcord(mer_type, list(mer_type)) = strcord.

mercury_types_to_strcord(HeadType, TailTypes) = Cord :-
    (
        TailTypes = [],
        Cord = mercury_type_to_strcord(HeadType)
    ;
        TailTypes = [HeadTailType | TailTailTypes],
        Cord = mercury_type_to_strcord(HeadType) ++ comma_cord ++
            mercury_types_to_strcord(HeadTailType, TailTailTypes)
    ).

%---------------------------------------------------------------------------%

:- func mlds_type_to_strcord(mlds_type) = strcord.

mlds_type_to_strcord(MLDS_Type) = Cord :-
    (
        MLDS_Type = mercury_type(MerType, _ForeignType, _CtorCat),
        Cord = mercury_type_to_strcord(MerType)
    ;
        MLDS_Type = mlds_mercury_array_type(MLDS_ElementType),
        Cord = strcord("mercury_array(") ++
            mlds_type_to_strcord(MLDS_ElementType) ++ strcord(")")
    ;
        MLDS_Type = mlds_array_type(MLDS_ElementType),
        Cord = strcord("array(") ++
            mlds_type_to_strcord(MLDS_ElementType) ++ strcord(")")
    ;
        MLDS_Type = mlds_mostly_generic_array_type(MLDS_ElementTypes),
        (
            MLDS_ElementTypes = [],
            Cord = strcord("mostly_generic_array()")
        ;
            MLDS_ElementTypes = [HeadType | TailTypes],
            Cord = strcord("mostly_generic_array(") ++
                mlds_types_to_strcord(HeadType, TailTypes) ++ strcord(")")
        )
    ;
        MLDS_Type = mlds_cont_type(MLDS_ReturnTypes),
        (
            MLDS_ReturnTypes = [],
            Cord = strcord("cont_type(return: <>)")
        ;
            MLDS_ReturnTypes = [HeadType | TailTypes],
            Cord = strcord("cont_type(return: <") ++
                mlds_types_to_strcord(HeadType, TailTypes) ++ strcord(">)")
        )
    ;
        MLDS_Type = mlds_commit_type,
        Cord = strcord("commit_type")
    ;
        MLDS_Type = mlds_native_bool_type,
        Cord = strcord("native bool")
    ;
        MLDS_Type = mlds_native_int_type,
        Cord = strcord("native int")
    ;
        MLDS_Type = mlds_native_uint_type,
        Cord = strcord("native uint")
    ;
        MLDS_Type = mlds_native_float_type,
        Cord = strcord("native float")
    ;
        MLDS_Type = mlds_native_char_type,
        Cord = strcord("native char")
    ;
        MLDS_Type = mlds_foreign_type(ForeignType),
        (
            ForeignType = c(c_type(TypeName)),
            Cord = strcord("c_type(") ++ strcord(TypeName) ++ strcord(")")
        ;
            ForeignType = java(java_type(TypeName)),
            Cord = strcord("java_type(") ++ strcord(TypeName) ++ strcord(")")
        ;
            ForeignType = csharp(csharp_type(TypeName)),
            Cord = strcord("csharp_type(") ++ strcord(TypeName) ++ strcord(")")
        ;
            ForeignType = erlang(erlang_type),
            Cord = strcord("erlang_type")
        )
    ;
        MLDS_Type = mlds_class_type(ClassId),
        ClassId = mlds_class_id(QualClassName, Arity, Kind),
        QualClassName = qual_class_name(_ModuleName, _QualKind, ClassName),
        (
            Kind = mlds_class,
            KindStr = "class"
        ;
            Kind = mlds_interface,
            KindStr = "interface"
        ;
            Kind = mlds_struct,
            KindStr = "class_struct"
        ;
            Kind = mlds_enum,
            KindStr = "class_enum"
        ),
        Cord = strcord(KindStr) ++ strcord("(") ++
            strcord(ClassName) ++ strcord("/") ++
            intcord(Arity) ++ strcord(")")
    ;
        MLDS_Type = mlds_ptr_type(PointedToType),
        Cord = strcord("ptr_to(") ++ mlds_type_to_strcord(PointedToType) ++
            strcord(")")
    ;
        MLDS_Type = mlds_func_type(FuncParams),
        Cord = strcord("func_type(") ++
            mlds_func_params_to_strcord(FuncParams) ++ strcord(")")
    ;
        MLDS_Type = mlds_generic_type,
        Cord = strcord("generic")
    ;
        MLDS_Type = mlds_generic_env_ptr_type,
        Cord = strcord("generic_env_ptr")
    ;
        MLDS_Type = mlds_type_info_type,
        Cord = strcord("type_info")
    ;
        MLDS_Type = mlds_pseudo_type_info_type,
        Cord = strcord("pseudo_type_info")
    ;
        MLDS_Type = mlds_rtti_type(_),
        Cord = strcord("rtti_type")
    ;
        MLDS_Type = mlds_tabling_type(_),
        Cord = strcord("tabling_type")
    ;
        MLDS_Type = mlds_unknown_type,
        Cord = strcord("unknown_type")
    ).

:- func mlds_func_params_to_strcord(mlds_func_params) = strcord.

mlds_func_params_to_strcord(Params) = Cord :-
    Params = mlds_func_params(Args, ReturnTypes),
    (
        Args = [],
        ArgsCord = cord.init
    ;
        Args = [HeadArg | TailArgs],
        ArgsCord =
            strcord("(") ++
            mlds_arguments_to_strcord(HeadArg, TailArgs) ++
            strcord(")")
    ),
    (
        ReturnTypes = [],
        ReturnCord = cord.init
    ;
        ReturnTypes = [HeadReturnType | TailReturnTypes],
        ReturnCord =
            strcord("->(") ++
            mlds_types_to_strcord(HeadReturnType, TailReturnTypes) ++
            strcord(")")
    ),
    Cord = ArgsCord ++ ReturnCord.

:- func mercury_type_to_strcord(mer_type) = strcord.

mercury_type_to_strcord(MerType) = Cord :-
    (
        MerType = type_variable(TypeVar, _Kind),
        Cord = strcord("type_var_") ++ intcord(var_to_int(TypeVar))
    ;
        MerType = defined_type(TypeCtorSymName, ArgTypes, _Kind),
        TypeCtorName = unqualify_name(TypeCtorSymName),
        (
            ArgTypes = [],
            Cord = strcord(TypeCtorName)
        ;
            ArgTypes = [HeadArgType | TailArgTypes],
            Cord = strcord(TypeCtorName) ++ strcord("(") ++
                mercury_types_to_strcord(HeadArgType, TailArgTypes) ++
                strcord(")")
        )
    ;
        MerType = builtin_type(BuiltinType),
        (
            BuiltinType = builtin_type_int(IntType),
            Cord = int_type_to_strcord(IntType)
        ;
            BuiltinType = builtin_type_float,
            Cord = strcord("float")
        ;
            BuiltinType = builtin_type_string,
            Cord = strcord("string")
        ;
            BuiltinType = builtin_type_char,
            Cord = strcord("char")
        )
    ;
        MerType = tuple_type(ArgTypes, _Kind),
        (
            ArgTypes = [],
            Cord = strcord("{}")
        ;
            ArgTypes = [HeadArgType | TailArgTypes],
            Cord = strcord("{") ++
                mercury_types_to_strcord(HeadArgType, TailArgTypes) ++
                strcord("}")
        )
    ;
        MerType = higher_order_type(PorF, ArgTypes, _HoInstInfo, Purity,
            _LambdaEvalMethod),
        (
            Purity = purity_pure,
            PurityCord = cord.init
        ;
            Purity = purity_semipure,
            PurityCord = strcord("semipure_")
        ;
            Purity = purity_impure,
            PurityCord = strcord("impure_")
        ),
        (
            PorF = pf_function,
            PorFCord = strcord("func")
        ;
            PorF = pf_predicate,
            PorFCord = strcord("pred")
        ),
        (
            ArgTypes = [],
            ArgCord = cord.init
        ;
            ArgTypes = [HeadArgType | TailArgTypes],
            ArgCord = strcord("(") ++
                mercury_types_to_strcord(HeadArgType, TailArgTypes) ++
                strcord(")")
        ),
        Cord = PurityCord ++ PorFCord ++ ArgCord
    ;
        MerType = apply_n_type(TypeVar, ArgTypes, _Kind),
        (
            ArgTypes = [],
            Cord = strcord("apply_tvar_") ++ intcord(var_to_int(TypeVar))
        ;
            ArgTypes = [HeadArgType | TailArgTypes],
            Cord = strcord("apply_tvar_") ++ intcord(var_to_int(TypeVar)) ++
                strcord("(") ++
                mercury_types_to_strcord(HeadArgType, TailArgTypes) ++
                strcord(")")
        )
    ;
        MerType = kinded_type(SubType, _Kind),
        Cord = mercury_type_to_strcord(SubType)
    ).

%---------------------------------------------------------------------------%

:- func mlds_field_var_name_to_strcord(mlds_field_var_name) = strcord.

mlds_field_var_name_to_strcord(FieldVarName) = Cord :-
    (
        FieldVarName = fvn_global_data_field(TypeNum, FieldNum),
        Str = string.format("global data field <type %d, field %d>",
            [i(TypeNum), i(FieldNum)]),
        Cord = strcord(Str)
    ;
        FieldVarName = fvn_du_ctor_field_hld(FieldName),
        Cord = strcord("du ctor field hld ") ++ strcord(FieldName)
    ;
        FieldVarName = fvn_mr_value,
        Cord = strcord("mr_value")
    ;
        FieldVarName = fvn_data_tag,
        Cord = strcord("data_tag")
    ;
        FieldVarName = fvn_enum_const(ConstName),
        Cord = strcord("enum_const ") ++ strcord(ConstName)
    ;
        FieldVarName = fvn_sectag_const(ConstName),
        Cord = strcord("sectag_const ") ++ strcord(ConstName)
    ;
        FieldVarName = fvn_ptr_num,
        Cord = strcord("ptr_num")
    ;
        FieldVarName = fvn_env_field_from_local_var(LocalVar),
        Cord = strcord("env field local var ") ++
            strcord(ml_local_var_name_to_string(LocalVar))
    ;
        FieldVarName = fvn_base_class(BaseNum),
        Cord = strcord("base class ") ++ intcord(BaseNum)
    ;
        FieldVarName = fvn_prev,
        Cord = strcord("prev")
    ;
        FieldVarName = fvn_trace,
        Cord = strcord("trace")
    ).

%---------------------------------------------------------------------------%

:- func mlds_local_var_defns_to_strcord(int, list(mlds_local_var_defn))
    = strcord.

mlds_local_var_defns_to_strcord(_Indent, []) = cord.init.
mlds_local_var_defns_to_strcord(Indent, [Defn | Defns]) =
    mlds_local_var_defn_to_strcord(Indent, Defn) ++
    mlds_local_var_defns_to_strcord(Indent, Defns).

:- func mlds_local_var_defn_to_strcord(int, mlds_local_var_defn) = strcord.

mlds_local_var_defn_to_strcord(Indent, LocalVarDefn) = Cord :-
    LocalVarDefn = mlds_local_var_defn(VarName, _Context, Type, Init, _Gc),
    (
        Init = no_initializer,
        InitCord = cord.init
    ;
        Init = init_obj(Rval),
        InitCord = strcord(" init_obj ") ++ mlds_rval_to_strcord(Rval)
    ;
        Init = init_struct(_Type, _Inits),
        InitCord = strcord(" init_struct(...)")
    ;
        Init = init_array(_Inits),
        InitCord = strcord(" init_array(...)")
    ),
    Cord = indent_strcord(Indent) ++ strcord("local ") ++
        strcord(ml_local_var_name_to_string(VarName)) ++ comma_cord ++
        mlds_type_to_strcord(Type) ++ InitCord ++ nl_strcord.

%---------------------------------------------------------------------------%

:- func mlds_function_defns_to_strcord(int, list(mlds_function_defn))
    = strcord.

mlds_function_defns_to_strcord(_Indent, []) = cord.init.
mlds_function_defns_to_strcord(Indent, [Defn | Defns]) =
    mlds_function_defn_to_strcord(Indent, Defn) ++
    mlds_function_defns_to_strcord(Indent, Defns).

:- func mlds_function_defn_to_strcord(int, mlds_function_defn) = strcord.

mlds_function_defn_to_strcord(Indent, FuncDefn) = Cord :-
    FuncDefn = mlds_function_defn(FuncName, _Context, _Flags, _OrigProc,
        Params, Body, _EnvVars, _TailRec),
    (
        FuncName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncNameCord = mlds_func_label_to_strcord(FuncLabel)
    ;
        FuncName = mlds_function_export(FuncNameStr),
        FuncNameCord = strcord(FuncNameStr)
    ),
    (
        Body = body_external,
        BodyCord = indent_strcord(Indent + 1) ++ strcord("external")
    ;
        Body = body_defined_here(Stmt),
        BodyCord = mlds_stmt_to_strcord(Indent + 1, Stmt)
    ),
    Cord =
        indent_strcord(Indent) ++ strcord("func ") ++ FuncNameCord ++
            mlds_func_params_to_strcord(Params) ++ nl_strcord ++
        indent_strcord(Indent) ++ strcord("begin") ++ nl_strcord ++
        BodyCord ++
        indent_strcord(Indent) ++ strcord("end") ++ nl_strcord.

:- func mlds_arguments_to_strcord(mlds_argument, list(mlds_argument))
    = strcord.

mlds_arguments_to_strcord(HeadArg, TailArgs) = Cord :-
    (
        TailArgs = [],
        Cord = mlds_argument_to_strcord(HeadArg)
    ;
        TailArgs = [HeadTailArg | TailTailArgs],
        Cord = mlds_argument_to_strcord(HeadArg) ++ comma_cord ++
            mlds_arguments_to_strcord(HeadTailArg, TailTailArgs)
    ).

:- func mlds_argument_to_strcord(mlds_argument) = strcord.

mlds_argument_to_strcord(Arg) = Cord :-
    Arg = mlds_argument(LocalVarName, Type, _Gc),
    Cord = mlds_type_to_strcord(Type) ++ strcord(" ") ++
        strcord(ml_local_var_name_to_string(LocalVarName)).

:- func mlds_func_label_to_strcord(mlds_func_label) = strcord.

mlds_func_label_to_strcord(FuncLabel) = Cord :-
    FuncLabel = mlds_func_label(ProcLabel, MaybeAuxFuncId),
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    (
        PredLabel = mlds_user_pred_label(PorF, _MaybeModuleName,
            PredName, Arity, _CodeModel, _NonDefaultModeFunction),
        (
            PorF = pf_function,
            PorFCord = strcord("func_")
        ;
            PorF = pf_predicate,
            PorFCord = strcord("pred_")
        ),
        PredCord = PorFCord ++ strcord(PredName) ++
            strcord("/") ++ intcord(Arity)
    ;
        PredLabel = mlds_special_pred_label(PredName, _MaybeModuleName,
            TypeName, TypeArity),
        PredCord = strcord(PredName) ++ strcord("_for_") ++
            strcord(TypeName) ++ strcord("/") ++ intcord(TypeArity)
    ),
    ProcCord = strcord("-") ++ intcord(proc_id_to_int(ProcId)),
    (
        MaybeAuxFuncId = proc_func,
        AuxCord = cord.init
    ;
        MaybeAuxFuncId = proc_aux_func(SeqNum),
        AuxCord = strcord("$aux_") ++ intcord(SeqNum)
    ;
        MaybeAuxFuncId = gc_trace_for_proc_func,
        AuxCord = strcord("$gc")
    ;
        MaybeAuxFuncId = gc_trace_for_proc_aux_func(SeqNum),
        AuxCord = strcord("$gc_aux_") ++ intcord(SeqNum)
    ),
    Cord = PredCord ++ ProcCord ++ AuxCord.

%---------------------------------------------------------------------------%

:- func comma_cord = strcord.

comma_cord = strcord(", ").

:- func strcord(string) = strcord.

strcord(Str) = cord.singleton(Str).

:- func intcord(int) = strcord.

intcord(N) = cord.singleton(string.int_to_string(N)).

:- func indent_strcord(int) = strcord.

indent_strcord(Indent) = Cord :-
    ( if
        ( Indent = 0, CordPrime = cord.init
        ; Indent = 1, CordPrime = cord.singleton("  ")
        ; Indent = 2, CordPrime = cord.singleton("    ")
        ; Indent = 3, CordPrime = cord.singleton("      ")
        ; Indent = 4, CordPrime = cord.singleton("        ")
        ; Indent = 5, CordPrime = cord.singleton("          ")
        )
    then
        Cord = CordPrime
    else if
        Indent > 0
    then
        Half = Indent / 2,
        Cord = indent_strcord(Half) ++ indent_strcord(Indent - Half)
    else
        unexpected($pred, "negative Indent")
    ).

:- func nl_strcord = strcord.

nl_strcord = cord.singleton("\n").

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_dump.
%---------------------------------------------------------------------------%
