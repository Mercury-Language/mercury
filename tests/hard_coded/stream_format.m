%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module stream_format.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module stream.
:- import_module stream.string_writer.
:- import_module string.

main(!IO) :-
     io.stdout_stream(Stdout, !IO),
     stream.string_writer.format(Stdout, "%s %d %c %f %u\n",
          [s("foo"), i(561), c('a'), f(3.141), u(1111u)], !IO).
