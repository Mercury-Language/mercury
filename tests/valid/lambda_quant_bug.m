/*

Regression test: a bug in quantification caused a software abort with the
message "assoc_list__from_corresponding_lists: different lengths".

A cursory examination showed that the compiler thought that "Proj"
was local to the switch on "MProj" after the first simplification 
pass.
*/

:- module lambda_quant_bug.
:- interface.
:- import_module io, bool.

:- type cl_result ---> ok(int) ; error(int).

:- pred all_reports(bool, cl_result, io__state, io__state).
:- mode all_reports(in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int, string, list.

all_reports(MProj, MTuples) -->
        { get_structure(MTemp) },
        (
                { MTemp = ok(_) },
                (
                        { MProj = yes },
                        { list__map(find, [], Proj) }
                ;
                        { MProj = no },
                        { list__map(find, [], Proj) }
                ),
                list__map_foldl(adjust_tuple(Proj), [], _),
                { MTuples = ok(42) }
        ;
                { MTemp = error(Err) },
                { MTuples = error(Err) }
        ).

:- pragma no_inline(adjust_tuple/5).
:- pred adjust_tuple(list(int), int, int, io__state, io__state).
:- mode adjust_tuple(in, in, out, di, uo) is det.

adjust_tuple(_, _, 42) --> [].

:- pragma no_inline(get_structure/1).
:- pred get_structure(cl_result :: out) is det.

get_structure(error(42)).

:- pragma no_inline(find/2).
:- pred find(int, int).
:- mode find(in, out) is det.

find(_, 42).
