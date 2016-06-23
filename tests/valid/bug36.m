% Compiling with:
% mmc --grade hlc.gc -O0 --no-optimize-dead-procs
%
% using rotd-2008-01-14 produces:
%
% Software Error: map.lookup: key not found
%    Key Type: parse_tree.prog_data.type_ctor
%    Key Value: type_ctor(qualified(unqualified("table_statistics"), "proc_table_statistics"), 0)
%    Value Type: hlds.hlds_data.hlds_type_defn
%
:- module bug36.
:- interface.

:- type ti_expr
    --->    ti_expr(
                raw_ti_expr :: raw_ti_expr
            ).

:- type raw_ti_expr
    --->    raw_ti_expr(
                base_ti_expr_tail :: base_ti_expr_tail
            ).

:- type base_ti_expr_tail
    --->    bte_bool
    ;       bte_set_of(ti_expr).

:- func bool_ti_expr = ti_expr.

:- implementation.

:- func base_ti_expr_tail_to_ti_expr(base_ti_expr_tail) = ti_expr.

base_ti_expr_tail_to_ti_expr(BaseTIExprTail) = TIExpr :-
    RawTIExpr = raw_ti_expr(BaseTIExprTail),
    TIExpr = ti_expr(RawTIExpr).

:- pragma memo(bool_ti_expr/0).

bool_ti_expr = base_ti_expr_tail_to_ti_expr(bte_bool).
