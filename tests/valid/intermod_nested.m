:- module intermod_nested.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module intermod_nested2.

main --> [].

