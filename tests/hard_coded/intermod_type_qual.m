:- module intermod_type_qual.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, intermod_type_qual2.

main --> p([1]).

