%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test. The structure sharing analysis wasn't able to reach a
% fixpoint analysing this module with --structure-sharing-widening set to
% certain values.

:- module sharing_loop2.
:- interface.

:- type lval
    --->    field(rval)
    ;       mem_ref(rval).

:- type rval
    --->    lval(lval)
    ;       const(data_name)
    ;       binop(rval, rval)
    ;       mem_addr(mem_ref).

:- type mem_ref
    --->    heap_ref(rval, rval).

:- type data_name
    --->    scalar_common_ref(type_num)
    ;       vector_common_ref(type_num, int).

:- type type_num
    --->    type_num(int).

:- type static_cell_remap_info.

:- func remap_lval(static_cell_remap_info, lval) = lval.

%---------------------------------------------------------------------------%

:- implementation.

:- type static_cell_remap_info
    --->    static_cell_remap_info(
                cell_type_num_remap,
                data_name
            ).

:- type cell_type_num_remap
    --->    cell_type_num_remap(type_num, type_num).

%---------------------------------------------------------------------------%

remap_lval(Remap, Lval0) = Lval :-
    (
        Lval0 = field(Rval0),
        Rval = remap_rval(Remap, Rval0),
        Lval = field(Rval)
    ;
        Lval0 = mem_ref(Rval0),
        Rval = remap_rval(Remap, Rval0),
        Lval = mem_ref(Rval)
    ).

:- func remap_rval(static_cell_remap_info, rval) = rval.

remap_rval(Remap, Rval0) = Rval :-
    (
        Rval0 = lval(Lval0),
        Lval = remap_lval(Remap, Lval0),
        Rval = lval(Lval)
    ;
        Rval0 = const(DataName0),
        DataName = remap_data_name(Remap, DataName0),
        Rval = const(DataName)
    ;
        Rval0 = binop(A0, B0),
        A = remap_rval(Remap, A0),
        B = remap_rval(Remap, B0),
        Rval = binop(A, B)
    ;
        Rval0 = mem_addr(MemRef0),
        MemRef = remap_mem_ref(Remap, MemRef0),
        Rval = mem_addr(MemRef)
    ).

:- func remap_data_name(static_cell_remap_info, data_name) = data_name.

remap_data_name(Remap, DataName0) = DataName :-
    Remap = static_cell_remap_info(TypeNumRemap, ScalarCellGroupRemap),
    (
        DataName0 = scalar_common_ref(TypeNum0),
        ( if TypeNumRemap = cell_type_num_remap(TypeNum0, _) then
            ScalarCellGroupRemap = DataName
        else
            DataName = DataName0
        )
    ;
        DataName0 = vector_common_ref(TypeNum0, Offset),
        ( if TypeNumRemap = cell_type_num_remap(TypeNum0, TypeNum) then
            DataName = vector_common_ref(TypeNum, Offset)
        else
            DataName = DataName0
        )
    ).

:- func remap_mem_ref(static_cell_remap_info, mem_ref) = mem_ref.

remap_mem_ref(Remap, MemRef0) = MemRef :-
    MemRef0 = heap_ref(Ptr0, FieldNum),
    Ptr = remap_rval(Remap, Ptr0),
    MemRef = heap_ref(Ptr, FieldNum).
