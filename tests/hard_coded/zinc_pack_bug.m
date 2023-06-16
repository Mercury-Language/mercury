%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-2011-07-09 generates code that seg. faults when executed.

:- module zinc_pack_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type type_inst
    --->    ti_par_bottom
    ;       ti_par_int
    ;       ti_var_int
    ;       ti_unknown.

:- type symbol
    --->    sym_variable(
                type_inst        :: type_inst,
                is_defined       :: is_defined,
                variable_kind    :: variable_kind
            ).

:- type is_defined
    --->    defined
    ;       undefined.

:- type variable_kind
    --->    global_var
    ;       type_constraint_var
    ;       let_var
    ;       generator_var
    ;       pred_arg
    ;       func_arg
    ;       ann_arg.

main(!IO) :-
    NewSym = sym_variable(ti_par_int, undefined, global_var),
    io.write_line(NewSym, !IO).
