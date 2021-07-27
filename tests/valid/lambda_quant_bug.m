%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test: a bug in quantification caused a software abort with the
% message "assoc_list__from_corresponding_lists: different lengths".
% 
% A cursory examination showed that the compiler thought that "Proj"
% was local to the switch on "MProj" after the first simplification pass.

:- module lambda_quant_bug.
:- interface.

:- import_module bool.
:- import_module io.

:- type cl_result
    --->    ok(int)
    ;       error(int).

:- pred all_reports(bool::in, cl_result::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

all_reports(MProj, MTuples, !IO) :-
    get_structure(MTemp),
    (
        MTemp = ok(_),
        (
            MProj = yes,
            list__map(find, [], Proj)
        ;
            MProj = no,
            list__map(find, [], Proj)
        ),
        list.map_foldl(adjust_tuple(Proj), [], _, !IO),
        MTuples = ok(42)
    ;
        MTemp = error(Err),
        MTuples = error(Err)
    ).

:- pragma no_inline(adjust_tuple/5).
:- pred adjust_tuple(list(int), int, int, io, io).
:- mode adjust_tuple(in, in, out, di, uo) is det.

adjust_tuple(_, _, 42) --> [].

:- pred get_structure(cl_result :: out) is det.
:- pragma no_inline(get_structure/1).

get_structure(error(42)).

:- pred find(int::in, int::out) is det.
:- pragma no_inline(find/2).

find(_, 42).
