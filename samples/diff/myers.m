%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1998, 2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: myers.m.
% Main author: bromage.
% 
% TO DO: We should implement the big-snake heuristic (a.k.a.
%   --speed-large-files).
%
% ALSO TO DO: Gene Myers et al have since produced another algorithm
%   which takes O(NP) time where P is the number of deletions in
%   the edit script.  If the `too expensive' heuristic can be
%   retro-fitted onto that algorithm easily enough, we should try
%   out this algorithm and see how fast it runs.  In theory, we
%   should be looking at about a 2x speedup.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module myers.
:- interface.

:- import_module difftype.

:- import_module array.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pred diff_by_myers(array(int)::in, array(int)::in, diff::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals.
:- import_module options.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

% The basic algorithm is described in:
%   "An O(ND) Difference Algorithm and its Variations", Eugene Myers,
%   Algorithmica Vol. 1 No. 2, 1986, pp. 251-266.
%
% This uses the variation in section 4b.

diff_by_myers(FileX, FileY, Diff, !IO) :-
    globals.io_lookup_bool_option(minimal, Minimal, !IO),
    array.size(FileX, SizeX),
    array.size(FileY, SizeY),
    SizeMax = SizeX + SizeY + 3,
    DOffset = SizeY + 1,

    % If we don't insist on --minimal, calculate the
    % approximate square root of the input size for
    % the "too expensive" heuristic.  The effect of
    % this is to limit the amount of work to about
    % O(n ** (1.5 log n)) at the expense of finding a
    % possibly non-minimal diff.

    (
        Minimal = yes,
        Heur = none
    ;
        Minimal = no,
        int.log2(SizeMax, SizeLog2),
        int.max(minimum_too_expensive, 1 << (SizeLog2 // 2),
                SizeHeuristic),
        Heur = too_expensive(SizeHeuristic)
    ),

        % Fill the arrays with nondescript numbers which
        % the algorithm shouldn't produce.  (For debugging
        % purposes.)
    array.init(SizeMax, -65537, Fwd),
    array.init(SizeMax, -65537, Bwd),
    myers.bsearch(DOffset, FileX, FileY, 0, SizeX, 0, SizeY,
        Heur, Fwd, _, Bwd, _, [], Diff).

    % XXX This lower bound is a guess.  Need to do some measurements
    %     to see if it's good or not.
:- func minimum_too_expensive = int.

minimum_too_expensive = 256.

:- pred myers.bsearch(int::in, array(int)::in, array(int)::in, int::in,
    int::in, int::in, int::in, heur::in,
    array(int)::array_di, array(int)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    diff::in, diff::out) is det.

myers.bsearch(DOffset, FileX, FileY, Xlow0, Xhigh0, Ylow0, Yhigh0, Heur,
            !Fwd, !Bwd, !Diff) :-
    scan_forward(FileX, FileY, Xhigh0, Yhigh0, Xlow0, Xlow, Ylow0, Ylow),
    scan_backward(FileX, FileY, Xlow, Ylow, Xhigh0, Xhigh, Yhigh0, Yhigh),
    (
        ( Xlow >= Xhigh
        ; Ylow >= Yhigh
        )
    ->
        add_edit(Xlow - Xhigh, Ylow - Yhigh, !Diff)
    ;
        find_middle(DOffset, FileX, FileY, Xlow, Xhigh, Ylow, Yhigh, Heur,
            !Fwd, !Bwd, Xmid, Ymid, Cost, LeftHeur - RightHeur),
        (
            Cost > 0
        ->
            myers.bsearch(DOffset, FileX, FileY, Xmid, Xhigh, Ymid, Yhigh,
                LeftHeur, !Fwd, !Bwd, !Diff),
            myers.bsearch(DOffset, FileX, FileY, Xlow, Xmid, Ylow, Ymid,
                RightHeur, !Fwd, !Bwd, !Diff)
        ;
            error("myers.bsearch")
        )
    ).

:- type myers_constants
    --->    constants(
            int,        % DOffset
            array(int), % X
            array(int), % Y
            int,        % Xlow
            int,        % Xhigh
            int,        % Ylow
            int,        % Yhigh
            int,        % Dmin
            int,        % Dmax
            bool,       % DeltaOdd
            heur        % "Too expensive" heuristic.
        ).

:- type heur
    --->    too_expensive(int)
    ;       none.

    % The best part about this algorithm is: We don't actually
    % need to find the middle of the diff.  We only have to find
    % an estimate to it.  If we don't find the exact middle,
    % we will have a correct diff, but it won't necessarily be
    % minimal.
:- pred myers.find_middle(int, array(int), array(int), pos, pos, pos, pos,
        heur,
        array(int), array(int), array(int), array(int),
        pos, pos, int, pair(heur)).
:- mode myers.find_middle(in, in, in, in, in, in, in, in,
        array_di, array_uo, array_di, array_uo,
        out, out, out, out) is det.

myers.find_middle(DOffset, FileX, FileY, Xlow, Xhigh, Ylow, Yhigh, Heur,
        Fwd0, Fwd, Bwd0, Bwd, Xmid, Ymid, Cost, HeurReq) :-

    Dmin = Xlow - Yhigh,
    Dmax = Xhigh - Ylow,

    Fmid = Xlow - Ylow,
    array.set(Fwd0, Fmid + DOffset, Xlow, Fwd1),
    Bmid = Xhigh - Yhigh,
    array.set(Bwd0, Bmid + DOffset, Xhigh, Bwd1),

    ( 1 = (Fmid - Bmid) /\ 1 ->
        DeltaOdd = yes
    ;
        DeltaOdd = no
    ),

    Constants = constants(
        DOffset, FileX, FileY, Xlow, Xhigh, Ylow, Yhigh,
        Dmin, Dmax, DeltaOdd, Heur
    ),

    myers.find_middle_2(Constants, Fwd1, Fwd, Bwd1, Bwd,
        Fmid, Fmid, Bmid, Bmid, 1, Cost, Xmid - Ymid, HeurReq).


:- pred myers.find_middle_2(myers_constants,
        array(int), array(int), array(int), array(int),
        int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers.find_middle_2(in, array_di, array_uo, array_di, array_uo,
        in, in, in, in, in, out, out, out) is det.

myers.find_middle_2(Constants, Fwd0, Fwd, Bwd0, Bwd,
        Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq) :-
    Constants = constants(DOffset, _, _, _, _, _, _, Dmin, Dmax, _, _),
    ( Fmin > Dmin ->
        Fmin1 = Fmin - 1,
        array.set(Fwd0, Fmin1 + DOffset - 1, -1, Fwd1)
    ;
        Fmin1 = Fmin + 1,
        Fwd1 = Fwd0
    ),
    ( Fmax < Dmax ->
        Fmax1 = Fmax + 1,
        array.set(Fwd1, Fmax1 + DOffset + 1, -1, Fwd2)
    ;
        Fmax1 = Fmax - 1,
        Fwd2 = Fwd1
    ),
    myers.find_forward_reaching_path(Constants, Fwd2, Fwd, Bwd0, Bwd,
        Fmin1, Fmax1, Bmin, Bmax, Fmax1, Cost0, Cost, Mid, HeurReq).


:- pred myers.find_forward_reaching_path(myers_constants,
        array(int), array(int), array(int), array(int),
        int, int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers.find_forward_reaching_path(in, array_di, array_uo,
        array_di, array_uo, in, in, in, in, in, in, out, out, out)
                is det.

myers.find_forward_reaching_path(Constants, Fwd0, Fwd, Bwd0, Bwd,
        Fmin, Fmax, Bmin, Bmax, SearchCost, Cost0, Cost, Mid,
        HeurReq) :-
    ( SearchCost < Fmin ->
        Constants = constants(DOffset, _, _, _, _, _, _, Dmin, Dmax, _,
                    _),
        int.max_int(MaxInt),
        ( Bmin > Dmin ->
            Bmin1 = Bmin - 1,
            array.set(Bwd0, Bmin1 + DOffset - 1, MaxInt, Bwd1)
        ;
            Bmin1 = Bmin + 1,
            Bwd1 = Bwd0
        ),
        ( Bmax < Dmax ->
            Bmax1 = Bmax + 1,
            array.set(Bwd1, Bmax1 + DOffset + 1, MaxInt, Bwd2)
        ;
            Bmax1 = Bmax - 1,
            Bwd2 = Bwd1
        ),
        myers.find_backward_reaching_path(Constants,
            Fwd0, Fwd, Bwd2, Bwd, Fmin, Fmax, Bmin1, Bmax1,
            Bmax1, Cost0, Cost, Mid, HeurReq)
    ;
        Constants = constants(DOffset, _, _, _, _, _, _, _, _, _, _),
        array.lookup(Fwd0, SearchCost + DOffset - 1, Tlo),
        array.lookup(Fwd0, SearchCost + DOffset + 1, Thi),
        ( Tlo >= Thi ->
            X0 = Tlo + 1
        ;
            X0 = Thi
        ),
        Y0 = X0 - SearchCost,
        Constants = constants(_, FileX, FileY, _, Xhigh, _, Yhigh,
            _, _, _, _),
        myers.scan_forward(FileX, FileY, Xhigh, Yhigh, X0, X, Y0, Y),
        array.set(Fwd0, SearchCost + DOffset, X, Fwd1),

        Constants = constants(_, _, _, _, _, _, _, _, _, DeltaOdd, _),
        (
            DeltaOdd = yes,
            Bmin =< SearchCost,
            SearchCost =< Bmax,
            array.lookup(Bwd0, SearchCost + DOffset, BB),
            BB =< X
        ->
            Mid = X - Y,
            Cost = 2 * Cost0 + 1,
            Fwd = Fwd1,
            Bwd = Bwd0,
            HeurReq = none - none
        ;
            myers.find_forward_reaching_path(Constants,
                Fwd1, Fwd, Bwd0, Bwd, Fmin, Fmax, Bmin, Bmax,
                SearchCost - 2, Cost0, Cost, Mid, HeurReq)
        )
    ).


:- pred myers.find_backward_reaching_path(myers_constants,
        array(int), array(int), array(int), array(int),
        int, int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers.find_backward_reaching_path(in, array_di, array_uo,
        array_di, array_uo, in, in, in, in, in, in,
        out, out, out) is det.

myers.find_backward_reaching_path(Constants, Fwd0, Fwd, Bwd0, Bwd,
        Fmin, Fmax, Bmin, Bmax, SearchCost, Cost0, Cost, Mid,
        HeurReq) :-
    ( SearchCost < Bmin ->
        myers.try_heuristics(Constants, Fwd0, Fwd, Bwd0, Bwd,
            Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq)
    ;
        Constants = constants(DOffset, _, _, _, _, _, _, _, _, _, _),
        array.lookup(Bwd0, SearchCost + DOffset - 1, Tlo),
        array.lookup(Bwd0, SearchCost + DOffset + 1, Thi),
        ( Tlo < Thi ->
            X0 = Tlo
        ;
            X0 = Thi - 1
        ),
        Y0 = X0 - SearchCost,
        Constants = constants(_, FileX, FileY, Xlow, _, Ylow, _,
            _, _, _, _),
        myers.scan_backward(FileX, FileY, Xlow, Ylow, X0, X, Y0, Y),
        array.set(Bwd0, SearchCost + DOffset, X, Bwd1),

        Constants = constants(_, _, _, _, _, _, _, _, _, DeltaOdd, _),
        (
            DeltaOdd = no,
            Fmin =< SearchCost,
            SearchCost =< Fmax,
            array.lookup(Fwd0, SearchCost + DOffset, FF),
            X =< FF
        ->
            Mid = X - Y,
            Cost = 2 * Cost0,
            Fwd = Fwd0,
            Bwd = Bwd1,
            HeurReq = none - none
        ;
            myers.find_backward_reaching_path(Constants,
                Fwd0, Fwd, Bwd1, Bwd, Fmin, Fmax, Bmin, Bmax,
                SearchCost - 2, Cost0, Cost, Mid, HeurReq)
        )
    ).


    % Try applying some heuristics to see if we can avoid some work.
:- pred myers.try_heuristics(myers_constants,
        array(int), array(int), array(int), array(int),
        int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers.try_heuristics(in, array_di, array_uo,
        array_di, array_uo, in, in, in, in, in, out, out, out) is det.

myers.try_heuristics(Constants, Fwd0, Fwd, Bwd0, Bwd,
        Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq) :-
    Constants = constants(_, _, _, _, _, _, _, _, _, _, Heur),
    (
        Heur = too_expensive(Cutoff),
        Cost0 >= Cutoff
    ->
            % If we've done too much work, stop here.
        Fwd = Fwd0, Bwd = Bwd0,
        myers.too_expensive_heuristic(Constants, Fwd, Bwd,
            Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq)
    ;
        % Can't apply heuristic, so try looking for a diff of size
        % Cost0 + 1.

        myers.find_middle_2(Constants, Fwd0, Fwd, Bwd0, Bwd,
            Fmin, Fmax, Bmin, Bmax, Cost0 + 1, Cost, Mid, HeurReq)
    ).

%-----------------------------------------------------------------------------%

    % We've done too much work, so make our best guess.
:- pred myers.too_expensive_heuristic(myers_constants, array(int), array(int),
        int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers.too_expensive_heuristic(in, array_ui, array_ui,
        in, in, in, in, in, out, out, out) is det.

myers.too_expensive_heuristic(Constants, Fwd, Bwd,
        Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq) :-
    % Find the best diagonal that we can, take the end of
    % that diagonal as the "middle".  Do not apply the
    % heuristic recursively to that best diagonal.

    Constants = constants(DOffset, _, _, Xlow, Xhigh, Ylow, Yhigh,
            _, _, _, Heur),

        % Find the best forward diagonal.
    myers.find_best_forward_diagonal(Fmax, Fmin, Fwd,
            Xhigh, Yhigh, DOffset, -1, FXYBest, 0, FXBest),

        % Find the best backward diagonal.
    int.max_int(MaxInt),
    myers.find_best_backward_diagonal(Bmax, Bmin, Bwd,
            Xlow, Ylow, DOffset, MaxInt, BXYBest, 0, BXBest),

        % Choose which of these diagonals is the better one
        % and return that as the "middle" point.
    (
        FXYBest - (Xhigh + Yhigh) < (Xlow + Ylow) - BXYBest
    ->
        Xmid = FXBest,
        Ymid = FXYBest - FXBest,
        HeurReq = none - Heur
    ;
        Xmid = BXBest,
        Ymid = BXYBest - BXBest,
        HeurReq = Heur - none
    ),
    Mid = Xmid - Ymid,
    Cost = 2 * Cost0 - 1.

:- pred myers.find_best_forward_diagonal(int, int, array(int), int, int, int,
            int, int, int, int).
:- mode myers.find_best_forward_diagonal(in, in, array_ui, in, in, in,
            in, out, in, out) is det.

myers.find_best_forward_diagonal(D, Fmin, Fwd, Xhigh, Yhigh, DOffset,
            FXYBest0, FXYBest, FXBest0, FXBest) :-
    ( D < Fmin ->
        FXYBest = FXYBest0,
        FXBest = FXBest0
    ;
        array.lookup(Fwd, D + DOffset, X0),
        int.min(Xhigh, X0, X1),
        Y0 = X1 - D,

        ( Yhigh < Y0 ->
            X = Yhigh + D,
            Y = Yhigh
        ;
            X = X1,
            Y = Y0
        ),

        NewFXY = X + Y,
        ( FXYBest0 < NewFXY ->
            myers.find_best_forward_diagonal(D - 2, Fmin, Fwd,
                Xhigh, Yhigh, DOffset, NewFXY, FXYBest,
                X, FXBest)
        ;
            myers.find_best_forward_diagonal(D - 2, Fmin, Fwd,
                Xhigh, Yhigh, DOffset, FXYBest0, FXYBest,
                FXBest0, FXBest)
        )
    ).

:- pred myers.find_best_backward_diagonal(int, int, array(int), int, int, int,
            int, int, int, int).
:- mode myers.find_best_backward_diagonal(in, in, array_ui, in, in, in,
            in, out, in, out) is det.

myers.find_best_backward_diagonal(D, Bmin, Bwd, Xlow, Ylow, DOffset,
            BXYBest0, BXYBest, BXBest0, BXBest) :-
    ( D < Bmin ->
        BXYBest = BXYBest0,
        BXBest = BXBest0
    ;
        array.lookup(Bwd, D + DOffset, X0),
        int.max(Xlow, X0, X1),
        Y0 = X1 - D,

        ( Y0 < Ylow ->
            X = Ylow + D,
            Y = Ylow
        ;
            X = X1,
            Y = Y0
        ),

        NewBXY = X + Y,
        ( NewBXY < BXYBest0 ->
            myers.find_best_backward_diagonal(D - 2, Bmin, Bwd,
                Xlow, Ylow, DOffset, NewBXY, BXYBest,
                X, BXBest)
        ;
            myers.find_best_backward_diagonal(D - 2, Bmin, Bwd,
                Xlow, Ylow, DOffset, BXYBest0, BXYBest,
                BXBest0, BXBest)
        )
    ).

%-----------------------------------------------------------------------------%

    % Travel forwards along a snake.
:- pred myers.scan_forward(array(int), array(int), int, int,
        int, int, int, int).
:- mode myers.scan_forward(in, in, in, in, in, out, in, out) is det.

myers.scan_forward(FileX, FileY, Xhigh, Yhigh, Xlow0, Xlow, Ylow0, Ylow) :-
    (
        Xlow0 < Xhigh,
        Ylow0 < Yhigh,
        array.lookup(FileX, Xlow0, Line),
        array.lookup(FileY, Ylow0, Line)
    ->
        myers.scan_forward(FileX, FileY, Xhigh, Yhigh,
            Xlow0 + 1, Xlow, Ylow0 + 1, Ylow)
    ;
        Xlow = Xlow0, Ylow = Ylow0
    ).


    % Travel backwards along a snake.
:- pred myers.scan_backward(array(int), array(int), int, int,
        int, int, int, int).
:- mode myers.scan_backward(in, in, in, in, in, out, in, out) is det.

myers.scan_backward(FileX, FileY, Xlow, Ylow, Xhigh0, Xhigh, Yhigh0, Yhigh) :-
    (
        Xhigh0 > Xlow,
        Yhigh0 > Ylow,
        array.lookup(FileX, Xhigh0 - 1, Line),
        array.lookup(FileY, Yhigh0 - 1, Line)
    ->
        myers.scan_backward(FileX, FileY, Xlow, Ylow,
            Xhigh0 - 1, Xhigh, Yhigh0 - 1, Yhigh)
    ;
        Xhigh = Xhigh0, Yhigh = Yhigh0
    ).

%-----------------------------------------------------------------------------%
:- end_module myers.
%-----------------------------------------------------------------------------%
