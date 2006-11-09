:- module stream_format.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module stream.
:- import_module string.

main(!IO) :-
     io.stdout_stream(Stdout, !IO),
     stream.format(Stdout, "%s%d%c%f\n",
          [s("foo"), i(561), c('a'), f(3.141)], !IO).
