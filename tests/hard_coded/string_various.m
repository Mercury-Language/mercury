:- module string_various.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string, char.

main(!IO) :-
  io__write_string(remove_suffix_if_present(".gz", "myfile"), !IO),
  io__nl(!IO),
  io__write_string(remove_suffix_if_present(".gz", "myfile.gz"), !IO),
  io__nl(!IO),
  io__write_string(remove_suffix_if_present(".gz", "myfile.gz.gz"), !IO),
  io__nl(!IO),
  true.
