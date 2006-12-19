% This condensed version of part of a program by Ondrej Bojar is a
% regression test.
%
% Versions of the compiler before 16 Dec 2006 aborted when generating code
% for the optimize predicate. The problem was that inlining, when it inlined
% nbestlist_to_data in optimize, did not adjust the variable numbers of the
% variables quantified by the trace goal in nbestlist_to_data. As a result,
% quantification thought that a variable (AllCandsC) computed before the
% trace goal was in fact quantified by the trace goal, which meant that it
% was not in the nonlocal set of the trace goal, which in turn meant that
% it was not in the nonlocal set of the goal that generated it, which lead
% to the deletion of that goal. After that, the presence of a consumer of
% AllCandsC without a generator guaranteed trouble.

:- module mert.
% bleu-based minimum error rate training, using Philipp Koehn's code
:- interface.

:- import_module float, list.

:- type weights == list(float).

:- type randomize --->
          no_randomize
        ; randomize(
            mins :: list(float),
            maxs :: list(float),
            iters :: int
          ).

:- func optimize(scorednbestlist, randomize, weights) = weights.
% given an nbestlist, reference translations, starting weights and possibly
% a randomization requirement, optimize weights to get highest bleu

:- implementation.

:- import_module string, int.

:- pragma foreign_decl("C", "
#include ""mert.koehn.h""
").

:- pragma foreign_code("C", "
#include ""mert.koehn.c""
  /* all the C functions */
").

%% Creating data for MERTing:

:- type c_candidate. % C-implemented, represents one sentence
:- pragma foreign_type("C", c_candidate, "candidate_t *",
    [stable, can_pass_as_mercury_type]).

:- type data. % C-implemented, represents the whole nbestlist
:- pragma foreign_type("C", data, "data_t *",
    [stable, can_pass_as_mercury_type]).

:- type feats == list(float). % score breakdown

:- func new_c_candidate(feats, bleucomps) = c_candidate.
% create a new candidate

new_c_candidate(F, C) = new_c_candidate(length(F), F, length(C), C).

:- func new_c_candidate(int::in, feats::in, int::in, bleucomps::in) = (c_candidate::uo) is det.

:- pragma foreign_proc("C",
new_c_candidate(NFeats::in, Feats::in, NComps::in, Comps::in) = (C::uo),
  [promise_pure, will_not_call_mercury, thread_safe], "
  C = MR_GC_malloc(sizeof(candidate_t));
  C->features = MR_GC_malloc(NFeats*sizeof(float));
  C->comps = MR_GC_malloc(NComps*sizeof(int));
  C->m = 0.0;
  C->b = 0.0;
  /* copy list to array */
  float *p = C->features;
  while (!MR_list_is_empty(Feats)) {
    *p = MR_list_head(Feats);
    p++;
    Feats = MR_list_tail(Feats);
  }
  /* copy list to array */
  int *q = C->comps;
  while (!MR_list_is_empty(Comps)) {
    *q = MR_list_head(Comps);
    q++;
    Comps = MR_list_tail(Feats);
  }
").

:- func nbestlist_to_data(scorednbestlist) = data.
  % construct data (using GC_malloc)
nbestlist_to_data(NBL) = OutData :-
  Lengths = list__map(length, NBL),
    % how many candidates do we have per sentence
  AllCands = list__condense(NBL),
  AllCandsC = list__map(
    func(scored_candidate(Scores, BLEUComps))
      = new_c_candidate(Scores, BLEUComps),
    AllCands),
  trace[io(!IO)]debugstr("nbestlist_to_data: lengths, allcands: ", {0+length(Lengths), 0+length(AllCandsC)}, !IO),
  OutData = new_c_data(length(Lengths), Lengths, length(AllCandsC), AllCandsC).

:- func new_c_data(int, list(int), int, list(c_candidate)) = data.
:- mode new_c_data(in, in, in, in) = uo is det.
:- pragma foreign_proc("C",
new_c_data(NSents::in, CandsPerSent::in, TotNCands::in, AllCands::in) = (D::uo),
  [promise_pure, will_not_call_mercury, thread_safe], "
  D = MR_GC_malloc(sizeof(data_t));
  D->sents_max = NSents;
  D->sents_n = NSents;
  D->cands_n = MR_GC_malloc(sizeof(int)*D->sents_max);
  /* copy list to array: CandsPerSent */
  int *q = D->cands_n;
  while (!MR_list_is_empty(CandsPerSent)) {
    *q = MR_list_head(CandsPerSent);
    q++;
    CandsPerSent = MR_list_tail(CandsPerSent);
  }

  /* create master array for candidates and then set data->sents
     to point into it */
  candidate_t *MasterCands;
  MasterCands = MR_GC_malloc(TotNCands * sizeof(candidate_t));
  /* copy list to array: MasterCands */
  candidate_t *p = MasterCands;
  while (!MR_list_is_empty(AllCands)) {
    candidate_t *thisC = (candidate_t*) MR_list_head(AllCands);
    *p = *thisC; /* copy the contents */
    p++;
    AllCands = MR_list_tail(AllCands);
  }

  D->sents = MR_GC_malloc(D->sents_n * sizeof(candidate_t *));
  int CandsSoFar = 0;
  int sent_i;
  for (sent_i=0; sent_i<D->sents_n; sent_i++) {
    D->sents[sent_i] = MasterCands+CandsSoFar;
    CandsSoFar += D->cands_n[sent_i];
  }

  /* GC will collect the masterarray 'MasterCands' */
").

:- type point. % C-implemented, represents the whole nbestlist
:- pragma foreign_type("C", point, "point_t *",
    [stable, can_pass_as_mercury_type]).

optimize(NBL, Rand, InW) = OutW :-
  Data = nbestlist_to_data(NBL),
  (
  Rand = no_randomize,
    OutWPoint = optimize_koehn(Data, construct_point(InW))
  ;
  Rand = randomize(Mins, Maxs, Iters),
    MinsPoint = construct_point(Mins),
    MaxsPoint = construct_point(Maxs),
    BestSoFar = optimize_koehn(Data, construct_point(InW)),
      % one iteration from current weights
    OutWPoint = optimize_random(Data, BestSoFar, MinsPoint, MaxsPoint, Iters),
      % plus some random seeds
    % now deconstruct mins and maxs
    _ = deconstruct_point(MinsPoint),
    _ = deconstruct_point(MaxsPoint)
  ),
  OutW = deconstruct_point(OutWPoint).

:- func optimize_random(data, point, point, point, int) = point.
   % warning, destructively modifies the BestSoFar!
:- pragma foreign_proc("C",
optimize_random(Data::in, BestSoFar::in, Min::in, Max::in, Iter::in) = (Out::out),
  [promise_pure, will_not_call_mercury, thread_safe], "
  int i;
  point_t *newpoint;
  for (i=0; i<Iter; i++) {
    newpoint = random_point(Min, Max);
    newpoint = optimize_koehn(Data, newpoint);
    if (newpoint->score > BestSoFar->score)
      point_copy_contents(newpoint, BestSoFar);
    point_delete(newpoint);
  }
  Out = BestSoFar;
").

:- func optimize_koehn(data, point) = point.
  % destructively replace contents of point doing one iteration of optimization

:- pragma foreign_proc("C",
optimize_koehn(Data::in, In::in) = (Out::out),
  [promise_pure, will_not_call_mercury, thread_safe], "
  Out = optimize_koehn(Data, In);
").

:- func construct_point(list(float)) = point.
  % construct a new point, uses plain malloc!

:- func deconstruct_point(point) = list(float).
  % delete point representation and return the contents as a list

:- pragma foreign_proc("C",
construct_point(List::in) = (Point::out),
  [promise_pure, will_not_call_mercury, thread_safe], "
  int i;

  Point = new_point();
  for(i=0; i<dim; i++) {
    if (MR_list_is_empty(List)) {
      fprintf(stderr, ""Failed to construct point, expected %i dimensions!\\n"", dim);
      exit(1);
    }
    Point->weights[i] = MR_list_head(List);
    List = MR_list_tail(List);
  }
").

:- pragma foreign_proc("C",
deconstruct_point(Point::in) = (List::out),
  [promise_pure, will_not_call_mercury, thread_safe], "
  MR_Word Tail;
  float v;
  int i;
  Tail = MR_list_empty();
  for(i=dim; i<=0; i--) {
    v = Point->weights[i];
    Tail = MR_list_cons(v, Tail);
  }
  point_delete(Point);
  List = Tail;
").

:- type bleucomps == list(int).

:- type nbestlist == list(list(candidate)).
:- type candidate ---> candidate(
          words :: list(string), % the sentence itself
          scores :: list(float)
        ).

:- type scorednbestlist == list(list(scored_candidate)).
:- type scored_candidate ---> scored_candidate(
          scores2 :: list(float),
          bleucomps :: bleucomps
        ).

:- func score_nbestlist(refs, nbestlist) = scorednbestlist.
% score nbestlist (do not complete BLEU scores, just collect the breakdowns)
% also removes duplicates from nbestlist

score_nbestlist(_, _) = [].

:- type word == string.

:- type refs ---> refs(
          refcount :: int,
          sentcount :: int,
          lengths :: version_array(list(int)), % sent->reference->length
          seen_ngrams :: version_array2d(approved_per_sent) % n->sent->approved
        ).

:- type ngram == list(word).
:- type approved_per_sent == map(ngram, int).
          % how many times was each ngram seen

:- import_module map.
:- import_module version_array.
:- import_module version_array2d.
:- import_module io.

:- pred debugstr(string::in, T::in, io::di, io::uo) is det.

debugstr(Msg, Data, !IO) :-
	io__stderr_stream(E, !IO),
	io__write_string(E, Msg, !IO),
	io__write(E, Data, !IO),
	io__nl(E, !IO).
