/*
$ mc bug2
bug2.m:009: Warning: incorrect module name in `:- module' declaration.
bug2.m:034: In `error(in) = out':
bug2.m:034:   warning: determinism declaration could be tighter.
bug2.m:034:   Declared `det', inferred `erroneous'.
Software error: variable V_32 not found
*/
:- module error_func.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module list, int, require, benchmarking, string.

:- func first_denomination(int) = int.
first_denomination(Kinds_of_coins) =
  (if (Kinds_of_coins = 1) then
    1
  else if (Kinds_of_coins = 2) then
    5
  else if (Kinds_of_coins = 3) then
    10
  else if (Kinds_of_coins = 4) then
    25
  else if (Kinds_of_coins = 5) then
    50
  else
    error("wrong kind of coin")
  ).

:- func error(string) = _.
error(S) = _ :-
  error(S).

:- func cc_tail(int, int, int) = int.
cc_tail(Amount, Kinds_of_coins, Count) =
  (if (Amount = 0) then
    Count + 1
  else if (Amount < 0 ; Kinds_of_coins = 0) then
    Count
  else
    cc_tail(Amount - first_denomination(Kinds_of_coins),
                   Kinds_of_coins,
                   cc_tail(Amount, Kinds_of_coins - 1, Count))
  ).

:- func count_change(int) = int.
count_change(Amount) =
  cc_tail(Amount, 5, 0).

:- pred do_count_change(int::in, int::out) is det.
do_count_change(Amount, Result) :- Result = count_change(Amount).

main -->
  io__command_line_arguments(Args),
  (if { Args = [] } then
      { N = 350 },
      { do_count_change(N, Answer) },
      io__format("Answer = %d\n", [i(Answer)])
  else if { Args = [Num] } then
    (if { string__to_int(Num, N), N >= 0 } then
      { benchmark_det(do_count_change, N, Answer, 1, Time) },
      { string__format("Answer = %d, Time = %d milliseconds\n",
		[i(Answer),i(Time)], Message) },
      io__write_string(Message)
    else
      io__write_string("invalid argument\n")
    )
  else
    io__write_string("wrong number of arguments\n")
  ).
