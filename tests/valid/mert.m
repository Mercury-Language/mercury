%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
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
%
% bleu-based minimum error rate training, using Philipp Koehn's code

:- module mert.
:- interface.

:- import_module float.
:- import_module list.

:- type weights == list(float).

:- type randomize
    --->    no_randomize
    ;       randomize(
                mins    :: list(float),
                maxs    :: list(float),
                iters   :: int
            ).

:- type scorednbestlist.
:- func optimize(scorednbestlist, randomize, weights) = weights.
% given an nbestlist, reference translations, starting weights and possibly
% a randomization requirement, optimize weights to get highest bleu

:- implementation.

:- import_module int.
:- import_module io.
:- import_module map.
:- import_module string.
:- import_module version_array.
:- import_module version_array2d.

%% Creating data for MERTing:

:- type c_candidate. % C-implemented, represents one sentence
:- pragma foreign_type("C", c_candidate, "void *",
    [stable, can_pass_as_mercury_type]).
:- pragma foreign_type("Java", c_candidate, "Object").
:- pragma foreign_type("C#", c_candidate, "object").

:- type data. % C-implemented, represents the whole nbestlist
:- pragma foreign_type("C", data, "void *",
    [stable, can_pass_as_mercury_type]).
:- pragma foreign_type("Java", data, "Object").
:- pragma foreign_type("C#", data, "object").

:- type feats == list(float). % score breakdown

:- func new_c_candidate(feats, bleucomps) = c_candidate.

new_c_candidate(F, C) = new_c_candidate(length(F), F, length(C), C).

:- func new_c_candidate(int::in, feats::in, int::in, bleucomps::in) =
    (c_candidate::uo) is det.

:- pragma foreign_proc("C",
    new_c_candidate(NFeats::in, Feats::in, NComps::in, Comps::in) = (C::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // NFeats, Feats, NComps, Comps, C
").
:- pragma foreign_proc("Java",
    new_c_candidate(NFeats::in, Feats::in, NComps::in, Comps::in) = (C::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // NFeats, Feats, NComps, Comps, C
    C = null;
").
:- pragma foreign_proc("C#",
    new_c_candidate(NFeats::in, Feats::in, NComps::in, Comps::in) = (C::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // NFeats, Feats, NComps, Comps, C
    C = null;
").

:- func nbestlist_to_data(scorednbestlist) = data.

  % construct data (using GC_malloc)
nbestlist_to_data(NBL) = OutData :-
    Lengths = list.map(length, NBL),
    % how many candidates do we have per sentence
    AllCands = list.condense(NBL),
    AllCandsC = list.map(
        ( func(scored_candidate(Scores, BLEUComps))
            = new_c_candidate(Scores, BLEUComps)
        ),
        AllCands),
    trace [io(!IO)] (
        debugstr("nbestlist_to_data: lengths, allcands: ",
            {0+length(Lengths), 0+length(AllCandsC)}, !IO)
    ),
    OutData = new_c_data(length(Lengths), Lengths,
        length(AllCandsC), AllCandsC).

:- func new_c_data(int, list(int), int, list(c_candidate)) = data.
:- mode new_c_data(in, in, in, in) = uo is det.
:- pragma foreign_proc("C",
    new_c_data(NSents::in, CandsPerSent::in, TotNCands::in, AllCands::in)
        = (D::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // NSents, CandsPerSent, TotNCands, AllCands, D
").
:- pragma foreign_proc("Java",
    new_c_data(NSents::in, CandsPerSent::in, TotNCands::in, AllCands::in)
        = (D::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // NSents, CandsPerSent, TotNCands, AllCands
    D = null;
").
:- pragma foreign_proc("C#",
    new_c_data(NSents::in, CandsPerSent::in, TotNCands::in, AllCands::in)
        = (D::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // NSents, CandsPerSent, TotNCands, AllCands
    D = null;
").

:- type point. % C-implemented, represents the whole nbestlist
:- pragma foreign_type("C", point, "void *",
    [stable, can_pass_as_mercury_type]).
:- pragma foreign_type("Java", point, "Object").
:- pragma foreign_type("C#", point, "Object").

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
        OutWPoint = optimize_random(Data, BestSoFar, MinsPoint, MaxsPoint,
            Iters),
        % plus some random seeds
        % now deconstruct mins and maxs
        _ = deconstruct_point(MinsPoint),
        _ = deconstruct_point(MaxsPoint)
    ),
    OutW = deconstruct_point(OutWPoint).

:- func optimize_random(data, point, point, point, int) = point.
   % warning, destructively modifies the BestSoFar!
:- pragma foreign_proc("C",
    optimize_random(Data::in, BestSoFar::in, Min::in, Max::in, Iter::in)
        = (Out::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // Data, BestSoFar, Min, Max, Iter, Out
").
:- pragma foreign_proc("Java",
    optimize_random(Data::in, BestSoFar::in, Min::in, Max::in, Iter::in)
        = (Out::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // Data, BestSoFar, Min, Max, Iter
    Out = null;
").
:- pragma foreign_proc("C#",
    optimize_random(Data::in, BestSoFar::in, Min::in, Max::in, Iter::in)
        = (Out::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // Data, BestSoFar, Min, Max, Iter
    Out = null;
").

:- func optimize_koehn(data, point) = point.
  % destructively replace contents of point doing one iteration of optimization

:- pragma foreign_proc("C",
    optimize_koehn(Data::in, In::in) = (Out::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // Data, In, Out
").
:- pragma foreign_proc("Java",
    optimize_koehn(Data::in, In::in) = (Out::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // Data, In, Out
    Out = null;
").
:- pragma foreign_proc("C#",
    optimize_koehn(Data::in, In::in) = (Out::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // Data, In, Out
    Out = null;
").

:- func construct_point(list(float)) = point.
  % construct a new point, uses plain malloc!

:- func deconstruct_point(point) = list(float).
  % delete point representation and return the contents as a list

:- pragma foreign_proc("C",
    construct_point(List::in) = (Point::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // List, Point
").
:- pragma foreign_proc("Java",
    construct_point(List::in) = (Point::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // List, Point
    Point = null;
").
:- pragma foreign_proc("C#",
    construct_point(List::in) = (Point::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // List, Point
    Point = null;
").

:- pragma foreign_proc("C",
    deconstruct_point(Point::in) = (List::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // Point, List
").
:- pragma foreign_proc("Java",
    deconstruct_point(Point::in) = (List::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // Point, List
    List = null;
").
:- pragma foreign_proc("C#",
    deconstruct_point(Point::in) = (List::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    // Point, List
    List = null;
").

:- type bleucomps == list(int).

:- type nbestlist == list(list(candidate)).
:- type candidate
    --->    candidate(
                words       :: list(string), % the sentence itself
                scores      :: list(float)
            ).

:- type scorednbestlist == list(list(scored_candidate)).
:- type scored_candidate
    --->    scored_candidate(
                scores2     :: list(float),
                bleucomps   :: bleucomps
            ).

:- func score_nbestlist(refs, nbestlist) = scorednbestlist.
% score nbestlist (do not complete BLEU scores, just collect the breakdowns)
% also removes duplicates from nbestlist

score_nbestlist(_, _) = [].

:- type word == string.

:- type refs
    --->    refs(
                refcount    :: int,
                sentcount   :: int,
                lengths     :: version_array(list(int)),
                                % sent->reference->length
                seen_ngrams :: version_array2d(approved_per_sent)
                                % n->sent->approved
            ).

:- type ngram == list(word).
:- type approved_per_sent == map(ngram, int).
          % how many times was each ngram seen

:- pred debugstr(string::in, T::in, io::di, io::uo) is det.

debugstr(Msg, Data, !IO) :-
    io.stderr_stream(E, !IO),
    io.write_string(E, Msg, !IO),
    io.write_line(E, Data, !IO).
