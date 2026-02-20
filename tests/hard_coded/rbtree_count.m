%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module rbtree_count.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module list.
:- import_module pair.
:- import_module rbtree.
:- import_module string.

main(!IO) :-
  rbtree.init(Empty : rbtree(int, string)),
  Singleton = rbtree.singleton(1, "One"),
  ThreeElems = rbtree.from_assoc_list([1 - "One", 2 - "Two", 3 - "Three"]),

  io.format("count(Empty) = %d\n", [i(count(Empty))], !IO),
  io.format("count(Singleton) = %d\n", [i(count(Singleton))], !IO),
  io.format("counter(ThreeElems) = %d\n", [i(count(ThreeElems))], !IO),

  io.format("ucount(Empty) = %u\n", [u(ucount(Empty))], !IO),
  io.format("ucount(Singleton) = %u\n", [u(ucount(Singleton))], !IO),
  io.format("ucounter(ThreeElems) = %u\n", [u(ucount(ThreeElems))], !IO).

%---------------------------------------------------------------------------%
:- end_module rbtree_count.
%---------------------------------------------------------------------------%
