% This is a regression test for a bug in construct.get_functor/5,
% where it was not handling equivalence types properly.

:- module construct_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, std_util, require, string.
:- import_module construct_bug_submodule.

main(!IO) :-
  FileName = "temp.term",

  call(
    (pred(!.S::in, !:S::out) is det:-
      count(["A"], !S),
      count(["A","A2"], !S),
      count(["B"], !S),
      count(["B","B1"], !S),
      count(["B","B2"], !S),
      count(["B","B3"], !S),
      count(["C"], !S),
      count(["C","C1"], !S)
    ), construct_bug_submodule__init, Map),

  io__open_binary_output(FileName, Result, !IO),
  (
  if (Result = ok(Temp___ORDIE___Out___OutStream) )
  then (OutStream = Temp___ORDIE___Out___OutStream, true)
  else error( "Failed to write to '"++FileName++"'.")
  ),
  io__write_binary(OutStream, Map, !IO),
  close_binary_output(OutStream, !IO),

  io__write_string("Saved the map to file: "++FileName++"\n", !IO),

  io__open_binary_input(FileName, Result2, !IO),
  (
  if (Result2 = ok(Temp___ORDIE___Out___InStream) )
  then (InStream = Temp___ORDIE___Out___InStream, true)
  else error( "Failed to open '"++FileName++"'.")
  ),
  io__read_binary(InStream, MayDataTerm, !IO),
  io__close_binary_input(InStream, !IO),
  (
    MayDataTerm = ok(ReadMap`with_type`stat)
  ; MayDataTerm = eof,
    error("Unexpected end of file: '"++FileName++"'.")
  ; MayDataTerm = error(E),
    error("Error reading term from: '"++FileName++"': "
      ++io__error_message(E)++".")
  ),

  io__write_string("Loaded the map from file: "++FileName++"\n", !IO),
  
  (
  if Map = ReadMap
  then
    io__write_string("The representations are identical.\n", !IO)
  else
    io__write_string("The representations are different.\n", !IO)
  ),

  io__remove_file(FileName, RmResult, !IO),
  ( RmResult = ok
  ; RmResult = error(E2),
    error("Error deleting file '"++FileName++"': "
      ++io__error_message(E2)++".")
  ).

