% This is part of the construct_bug test case.
:- module construct_bug_submodule.

:- interface.

:- import_module string, list, int, assoc_list.

:- type stat.
:- type statkey == list(string).

:- func init = (stat::out) is det.
:- func blank = (stat::out) is det.

:- pred count(statkey, stat, stat).
:- mode count(in,      in,   out) is det.

:- pred count(int, statkey, stat, stat).
:- mode count(in,  in,      in,   out) is det.

% Same as count/4, just reversed the first two args
:- pred count_more(statkey, int, stat, stat).
:- mode count_more(in,  in,      in,   out) is det.

:- func count(statkey, stat) = stat.
:- mode count(in, in) = out is det.

:- func count(int, statkey, stat) = stat.
:- mode count(in, in, in) = out is det.

:- func union(stat, stat) = stat.
% Sums the information in two stat stores.

:- func to_assoc_list(stat) = assoc_list(statkey, int).

% :- import_module pprint.

% :- func to_doc(stat::in) = (doc::out) is det.
:- func plain_to_string(stat::in) = (string::out) is det.
:- func to_string(stat::in) = (string::out) is det.
:- func subs_to_string(stat::in) = (string::out) is det.


%% Other useful predicates for evaluating something

:- func minavgmax_string(list(int)::in) = (string::out) is det.

:- implementation.

:- import_module bag.
:- import_module std_util.

:- type stat == bag(list(string)).

init = bag__init.
blank = construct_bug_submodule__init.

count(Elem, InStat, OutStat) :-
  bag__insert(InStat, Elem, OutStat).
count(Count, Elem, InStat, OutStat) :-
  bag__insert_list(InStat, list__duplicate(Count, Elem), OutStat).

count_more(Elem, Count, InStat, OutStat) :-
  count(Count, Elem, InStat, OutStat).

count(Elem, InStat) = OutStat :-
  count(Elem, InStat, OutStat).

count(Count, Elem, InStat) = OutStat :-
  count(Count, Elem, InStat, OutStat).

union(A, B) = bag__union(A, B).
to_assoc_list(Stat) = bag__to_assoc_list(Stat).

to_string(Stat) =
  "% Base Counts:\n"
  ++ plain_to_string(Stat)
  ++ "% Subtotals:\n"
  ++ plain_to_string(calc_subtotals(Stat)).
subs_to_string(Stat) =
  plain_to_string(calc_subtotals(Stat)).

plain_to_string(Stat) = Out :-
  Counts = bag__to_assoc_list(Stat),
  list__sort(comparator(Stat,0), Counts, CountsS),
  list__map(
    (pred((Name-Count)::in, Line::out) is det :-
      Line = string__int_to_string(Count)++"\t"++join_list("-", Name)
    ), CountsS, Lines),
  Out = join_list("\n", Lines)++"\n"
  .

:- pred comparator(stat, int, pair(statkey, int), pair(statkey, int), comparison_result).
:- mode comparator(in, in, in, in, out) is det.

comparator(Stats, Level, (ADescr-ANum), (BDescr-BNum), Out) :-
  if take(Level, ADescr, ALevel)
  then
    (
    if take(Level, BDescr, BLevel)
    then
      (
      if count_value(Stats, ALevel) < count_value(Stats, BLevel)
      then Out = (>)
      else
        if count_value(Stats, ALevel) > count_value(Stats, BLevel)
        then Out = (<)
        else
          % same value
          if ALevel = BLevel
          then comparator(Stats, Level+1, (ADescr-ANum), (BDescr-BNum), Out)
          else
            if compare((<), ALevel, BLevel)
            then Out = (>)
            else Out = (<)
      )
    else
      Out = (>)
    )
  else
    if take(Level, BDescr, _BLevel)
    then Out = (<)
    else Out = (=).

:- func calc_subtotals(stat::in) = (stat::out) is det.
calc_subtotals(Stat) = OutStat :-
  Counts = bag__to_assoc_list(Stat),
  list__foldl(
    (pred((Name - Count)::in, InBag::in, OutBag::out) is det :-
      aggregate(substarts(Name), count(Count), InBag, OutBag)
    ), Counts, Stat, OutStat).

/*
 ** buggy pprint, do not use.
to_string(Stat) = Str :-
  Str = pprint__to_string(150, construct_bug_submodule__to_doc(Stat)).

to_doc(Stat) = 
  text("% Base Counts:\n")
  `<>` plain_to_doc(Stat)
  `</>`
  text("% Subtotals:\n")
  `<>` subtotals_to_doc(Stat).

:- func plain_to_doc(stat::in) = (doc::out) is det.
:- func subtotals_to_doc(stat::in) = (doc::out) is det.

plain_to_doc(Stat) = Out :-
  Counts = bag__to_assoc_list(Stat),
  Out = 
    % text("% Statistiky") `</>`
    separated(
      (func((Name-Count)::in) = (Doc::out) is det :-
        Doc = to_doc(Count) `<>` text("\t")
          `<>` text(join("-", Name))
      ), line, Counts) 
    `<>` line.

subtotals_to_doc(Stat) = Out :-
  Counts = bag__to_assoc_list(Stat),
  list__foldl(
    (pred((Name - Count)::in, InBag::in, OutBag::out) is det :-
      aggregate(substarts(Name), count(Count), InBag, OutBag)
    ), Counts, Stat, Subtotals),
  Out = plain_to_doc(Subtotals).
*/
:- pred substarts(list(T)::in, list(T)::out) is multi.

substarts([], []).
substarts([_LastElem], []).
substarts([Elem1, Elem2|Rest], Out) :-
  Out = []
  ;
  substarts([Elem2|Rest], TOut),
  Out = [Elem1|TOut].


:- import_module float.

minavgmax_string(List) = int_to_string(length(List))++":"++Min++"/"++Avg++"/"++Max :-
  List = [],
    Min = "-", Avg = "-", Max = "-"
  ;
  List = [H|Tail],
    list__foldl3(
      (pred(N::in, MinSoFar::in, NewMin::out, MaxSoFar::in, NewMax::out, SumSoFar::in, NewSum::out) is det:-
        NewSum = SumSoFar + N,
        NewMin = (if N < MinSoFar then N else MinSoFar),
        NewMax = (if N > MaxSoFar then N else MaxSoFar)
      ), Tail, H, MinN, H, MaxN, H, SumN),
    Min = int_to_string(MinN),
    Max = int_to_string(MaxN),
    Prec = float(10),
    Avg = float_to_string(float(round_to_int(
                      (float(SumN)/float(length(List)))*Prec
                      ))/Prec)
  .
