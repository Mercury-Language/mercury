:- module bag_various.
% basic test of some bag predicates
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bag, string, list, int, solutions, bool.

main(!IO) :-
  Bag = bag.from_list([1,1,1,2,3,3,4]),
  dump("bag.to_list: ", []++bag.to_list(Bag), !IO),
  dump("bag.to_assoc_list: ", []++bag.to_assoc_list(Bag), !IO),
  dump("bag.count: ", 0+bag.count(Bag), !IO),
  dump("bag.count_unique: ", 0+bag.count_unique(Bag), !IO),
  (
  if bag.member(4, Bag)
  then dump("bag.member(4): ", yes, !IO)
  else dump("bag.member(4): ", no, !IO)
  ),
  (
  if bag.member(5, Bag)
  then dump("bag.member(5): ", yes, !IO)
  else dump("bag.member(5): ", no, !IO)
  ),
  unsorted_solutions(
    (pred(O::out) is nondet:-
      bag.member(M, Bag, Rest),
      O = {M, []++to_list(Rest)}
    ), Sols),
  dump("unsorted_solutions(bag.member/3): ", Sols, !IO),
  true.

:- pred dump(string::in, T::in, io::di, io::uo) is det.
dump(Msg, T, !IO) :-
  io__write_string(Msg, !IO),
  io__write(T, !IO),
  io__nl(!IO).

