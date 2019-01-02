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
% --speed-large-files).
%
% ALSO TO DO: Gene Myers et al have since produced another algorithm
% which takes O(NP) time where P is the number of deletions in the edit script.
% If the `too expensive' heuristic can be retro-fitted onto that algorithm
% easily enough, we should try out this algorithm and see how fast it runs.
% In theory, we should be looking at about a 2x speedup.
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

    % If we don't insist on --minimal, calculate the approximate square root of
    % the input size for the "too expensive" heuristic. The effect of this is
    % to limit the amount of work to about O(n ** (1.5 log n)) at the expense
    % of finding a possibly non-minimal diff.

    (
        Minimal = yes,
        Heur = none
    ;
        Minimal = no,
        int.log2(SizeMax, SizeLog2),
        int.max(minimum_too_expensive, 1 << (SizeLog2 // 2), SizeHeuristic),
        Heur = too_expensive(SizeHeuristic)
    ),

    % Fill the arrays with nondescript numbers which
    % the algorithm shouldn't produce. (For debugging purposes.)
    array.init(SizeMax, -65537, Fwd),
    array.init(SizeMax, -65537, Bwd),
    myers.bsearch(DOffset, FileX, FileY, 0, SizeX, 0, SizeY,
        Heur, Fwd, _, Bwd, _, [], Diff).

    % XXX This lower bound is a guess. Need to do some measurements
    % to see if it's good or not.
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
    ( if
        ( Xlow >= Xhigh
        ; Ylow >= Yhigh
        )
    then
        add_edit(Xlow - Xhigh, Ylow - Yhigh, !Diff)
    else
        find_middle(DOffset, FileX, FileY, Xlow, Xhigh, Ylow, Yhigh, Heur,
            !Fwd, !Bwd, Xmid, Ymid, Cost, LeftHeur - RightHeur),
        ( if
            Cost > 0
        then
            myers.bsearch(DOffset, FileX, FileY, Xmid, Xhigh, Ymid, Yhigh,
                LeftHeur, !Fwd, !Bwd, !Diff),
            myers.bsearch(DOffset, FileX, FileY, Xlow, Xmid, Ylow, Ymid,
                RightHeur, !Fwd, !Bwd, !Diff)
        else
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

    % The best part about this algorithm is: We don't actually need to find the
    % middle of the diff. We only have to find an estimate to it. If we don't
    % find the exact middle, we will have a correct diff, but it won't
    % necessarily be minimal.
    %
:- pred find_middle(int::in, array(int)::in, array(int)::in, pos::in, pos::in,
    pos::in, pos::in, heur::in, array(int)::array_di, array(int)::array_uo,
    array(int)::array_di, array(int)::array_uo, pos::out, pos::out, int::out,
    pair(heur)::out) is det.

find_middle(DOffset, FileX, FileY, Xlow, Xhigh, Ylow, Yhigh, Heur, !Fwd,
        !Bwd, Xmid, Ymid, Cost, HeurReq) :-

    Dmin = Xlow - Yhigh,
    Dmax = Xhigh - Ylow,

    Fmid = Xlow - Ylow,
    array.set(Fmid + DOffset, Xlow, !Fwd),
    Bmid = Xhigh - Yhigh,
    array.set(Bmid + DOffset, Xhigh, !Bwd),

    ( if 1 = (Fmid - Bmid) /\ 1 then
        DeltaOdd = yes
    else
        DeltaOdd = no
    ),

    Constants = constants(
        DOffset, FileX, FileY, Xlow, Xhigh, Ylow, Yhigh,
        Dmin, Dmax, DeltaOdd, Heur
    ),

    find_middle_2(Constants, !Fwd, !Bwd, Fmid, Fmid, Bmid, Bmid, 1, Cost,
        Xmid - Ymid, HeurReq).

:- pred find_middle_2(myers_constants::in,
    array(int)::array_di, array(int)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::in, int::in, int::in, int::in,
    int::out, pair(pos)::out, pair(heur)::out) is det.

find_middle_2(Constants, !Fwd, !Bwd, Fmin, Fmax, Bmin, Bmax, !Cost,
        Mid, HeurReq) :-
    Constants = constants(DOffset, _, _, _, _, _, _, Dmin, Dmax, _, _),
    ( if Fmin > Dmin then
        Fmin1 = Fmin - 1,
        array.set(Fmin1 + DOffset - 1, -1, !Fwd)
    else
        Fmin1 = Fmin + 1
    ),
    ( if Fmax < Dmax then
        Fmax1 = Fmax + 1,
        array.set(Fmax1 + DOffset + 1, -1, !Fwd)
    else
        Fmax1 = Fmax - 1
    ),
    find_forward_reaching_path(Constants, !Fwd, !Bwd, Fmin1, Fmax1,
        Bmin, Bmax, Fmax1, !Cost, Mid, HeurReq).

:- pred find_forward_reaching_path(myers_constants::in,
    array(int)::array_di, array(int)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::in, int::in, int::in, int::in, int::in, int::out,
    pair(pos)::out, pair(heur)::out) is det.

find_forward_reaching_path(Constants, !Fwd, !Bwd, Fmin, Fmax, Bmin, Bmax,
        SearchCost, !Cost, Mid, HeurReq) :-
    ( if SearchCost < Fmin then
        Constants = constants(DOffset, _, _, _, _, _, _, Dmin, Dmax, _, _),
        int.max_int(MaxInt),
        ( if Bmin > Dmin then
            Bmin1 = Bmin - 1,
            array.set(Bmin1 + DOffset - 1, MaxInt, !Bwd)
        else
            Bmin1 = Bmin + 1
        ),
        ( if Bmax < Dmax then
            Bmax1 = Bmax + 1,
            array.set(Bmax1 + DOffset + 1, MaxInt, !Bwd)
        else
            Bmax1 = Bmax - 1
        ),
        find_backward_reaching_path(Constants, !Fwd, !Bwd, Fmin, Fmax,
            Bmin1, Bmax1, Bmax1, !Cost, Mid, HeurReq)
    else
        Constants = constants(DOffset, _, _, _, _, _, _, _, _, _, _),
        array.lookup(!.Fwd, SearchCost + DOffset - 1, Tlo),
        array.lookup(!.Fwd, SearchCost + DOffset + 1, Thi),
        ( if Tlo >= Thi then
            X0 = Tlo + 1
        else
            X0 = Thi
        ),
        Y0 = X0 - SearchCost,
        Constants = constants(_, FileX, FileY, _, Xhigh, _, Yhigh, _, _, _, _),
        scan_forward(FileX, FileY, Xhigh, Yhigh, X0, X, Y0, Y),
        array.set(SearchCost + DOffset, X, !Fwd),

        Constants = constants(_, _, _, _, _, _, _, _, _, DeltaOdd, _),
        ( if
            DeltaOdd = yes,
            Bmin =< SearchCost,
            SearchCost =< Bmax,
            array.lookup(!.Bwd, SearchCost + DOffset, BB),
            BB =< X
        then
            Mid = X - Y,
            !:Cost = 2 * !.Cost + 1,
            HeurReq = none - none
        else
            find_forward_reaching_path(Constants, !Fwd, !Bwd, Fmin, Fmax,
                Bmin, Bmax, SearchCost - 2, !Cost, Mid, HeurReq)
        )
    ).

:- pred find_backward_reaching_path(myers_constants::in,
    array(int)::array_di, array(int)::array_uo,
    array(int)::array_di, array(int)::array_uo, int::in, int::in, int::in,
    int::in, int::in, int::in, int::out, pair(pos)::out, pair(heur)::out)
    is det.

find_backward_reaching_path(Constants, !Fwd, !Bwd, Fmin, Fmax, Bmin, Bmax,
        SearchCost, !Cost, Mid, HeurReq) :-
    ( if SearchCost < Bmin then
        try_heuristics(Constants, !Fwd, !Bwd, Fmin, Fmax, Bmin, Bmax, !Cost,
            Mid, HeurReq)
    else
        Constants = constants(DOffset, _, _, _, _, _, _, _, _, _, _),
        array.lookup(!.Bwd, SearchCost + DOffset - 1, Tlo),
        array.lookup(!.Bwd, SearchCost + DOffset + 1, Thi),
        ( if Tlo < Thi then
            X0 = Tlo
        else
            X0 = Thi - 1
        ),
        Y0 = X0 - SearchCost,
        Constants = constants(_, FileX, FileY, Xlow, _, Ylow, _, _, _, _, _),
        scan_backward(FileX, FileY, Xlow, Ylow, X0, X, Y0, Y),
        array.set(SearchCost + DOffset, X, !Bwd),

        Constants = constants(_, _, _, _, _, _, _, _, _, DeltaOdd, _),
        ( if
            DeltaOdd = no,
            Fmin =< SearchCost,
            SearchCost =< Fmax,
            array.lookup(!.Fwd, SearchCost + DOffset, FF),
            X =< FF
        then
            Mid = X - Y,
            !:Cost = 2 * !.Cost,
            HeurReq = none - none
        else
            find_backward_reaching_path(Constants, !Fwd, !Bwd, Fmin, Fmax,
                Bmin, Bmax, SearchCost - 2, !Cost, Mid, HeurReq)
        )
    ).

    % Try applying some heuristics to see if we can avoid some work.
    %
:- pred try_heuristics(myers_constants::in,
    array(int)::array_di, array(int)::array_uo,
    array(int)::array_di, array(int)::array_uo,
    int::in, int::in, int::in, int::in, int::in, int::out,
    pair(pos)::out, pair(heur)::out) is det.

try_heuristics(Constants, !Fwd, !Bwd, Fmin, Fmax, Bmin, Bmax, !Cost,
        Mid, HeurReq) :-
    Constants = constants(_, _, _, _, _, _, _, _, _, _, Heur),
    ( if
        Heur = too_expensive(Cutoff),
        !.Cost >= Cutoff
    then
        % If we've done too much work, stop here.
        too_expensive_heuristic(Constants, !.Fwd, !.Bwd, Fmin, Fmax,
            Bmin, Bmax, !Cost, Mid, HeurReq)
    else
        % Can't apply heuristic, so try looking for a diff of size Cost0 + 1.
        !:Cost = !.Cost + 1,
        find_middle_2(Constants, !Fwd, !Bwd, Fmin, Fmax, Bmin, Bmax, !Cost,
            Mid, HeurReq)
    ).

%-----------------------------------------------------------------------------%

    % We've done too much work, so make our best guess.
:- pred too_expensive_heuristic(myers_constants::in, array(int)::array_ui,
    array(int)::array_ui, int::in, int::in, int::in, int::in,
    int::in, int::out, pair(pos)::out, pair(heur)::out) is det.

too_expensive_heuristic(Constants, Fwd, Bwd, Fmin, Fmax, Bmin, Bmax, !Cost,
        Mid, HeurReq) :-
    % Find the best diagonal that we can, take the end of that diagonal as the
    % "middle". Do not apply the heuristic recursively to that best diagonal.

    Constants = constants(DOffset, _, _, Xlow, Xhigh, Ylow, Yhigh, _, _, _,
        Heur),

    % Find the best forward diagonal.
    find_best_forward_diagonal(Fmax, Fmin, Fwd, Xhigh, Yhigh, DOffset, -1,
        FXYBest, 0, FXBest),

    % Find the best backward diagonal.
    int.max_int(MaxInt),
    find_best_backward_diagonal(Bmax, Bmin, Bwd, Xlow, Ylow, DOffset, MaxInt,
        BXYBest, 0, BXBest),

    % Choose which of these diagonals is the better one
    % and return that as the "middle" point.
    ( if
        FXYBest - (Xhigh + Yhigh) < (Xlow + Ylow) - BXYBest
    then
        Xmid = FXBest,
        Ymid = FXYBest - FXBest,
        HeurReq = none - Heur
    else
        Xmid = BXBest,
        Ymid = BXYBest - BXBest,
        HeurReq = Heur - none
    ),
    Mid = Xmid - Ymid,
    !:Cost = 2 * !.Cost - 1.

:- pred find_best_forward_diagonal(int::in, int::in, array(int)::array_ui,
    int::in, int::in, int::in, int::in, int::out, int::in, int::out) is det.

find_best_forward_diagonal(D, Fmin, Fwd, Xhigh, Yhigh, DOffset,
        !FXYBest, !FXBest) :-
    ( if D < Fmin then
        true
    else
        array.lookup(Fwd, D + DOffset, X0),
        int.min(Xhigh, X0, X1),
        Y0 = X1 - D,

        ( if Yhigh < Y0 then
            X = Yhigh + D,
            Y = Yhigh
        else
            X = X1,
            Y = Y0
        ),

        NewFXY = X + Y,
        ( if !.FXYBest < NewFXY then
            find_best_forward_diagonal(D - 2, Fmin, Fwd, Xhigh, Yhigh,
                DOffset, NewFXY, !:FXYBest, X, !:FXBest)
        else
            find_best_forward_diagonal(D - 2, Fmin, Fwd, Xhigh, Yhigh,
                DOffset, !FXYBest, !FXBest)
        )
    ).

:- pred find_best_backward_diagonal(int::in, int::in, array(int)::array_ui,
    int::in, int::in, int::in, int::in, int::out, int::in, int::out) is det.

find_best_backward_diagonal(D, Bmin, Bwd, Xlow, Ylow, DOffset, !BXYBest,
        !BXBest) :-
    ( if D < Bmin then
        true
    else
        array.lookup(Bwd, D + DOffset, X0),
        int.max(Xlow, X0, X1),
        Y0 = X1 - D,

        ( if Y0 < Ylow then
            X = Ylow + D,
            Y = Ylow
        else
            X = X1,
            Y = Y0
        ),

        NewBXY = X + Y,
        ( if NewBXY < !.BXYBest then
            find_best_backward_diagonal(D - 2, Bmin, Bwd, Xlow, Ylow, DOffset,
                NewBXY, !:BXYBest, X, !:BXBest)
        else
            find_best_backward_diagonal(D - 2, Bmin, Bwd, Xlow, Ylow, DOffset,
                !BXYBest, !BXBest)
        )
    ).

%-----------------------------------------------------------------------------%

    % Travel forwards along a snake.
:- pred scan_forward(array(int)::in, array(int)::in, int::in, int::in,
    int::in, int::out, int::in, int::out) is det.

scan_forward(FileX, FileY, Xhigh, Yhigh, !Xlow, !Ylow) :-
    ( if
        !.Xlow < Xhigh,
        !.Ylow < Yhigh,
        array.lookup(FileX, !.Xlow, Line),
        array.lookup(FileY, !.Ylow, Line)
    then
        !:Xlow = !.Xlow + 1,
        !:Ylow = !.Ylow + 1,
        scan_forward(FileX, FileY, Xhigh, Yhigh, !Xlow, !Ylow)
    else
        true
    ).

    % Travel backwards along a snake.
    %
:- pred scan_backward(array(int)::in, array(int)::in, int::in, int::in,
    int::in, int::out, int::in, int::out) is det.

scan_backward(FileX, FileY, Xlow, Ylow, !Xhigh, !Yhigh) :-
    ( if
        !.Xhigh > Xlow,
        !.Yhigh > Ylow,
        array.lookup(FileX, !.Xhigh - 1, Line),
        array.lookup(FileY, !.Yhigh - 1, Line)
    then
        !:Xhigh = !.Xhigh - 1,
        !:Yhigh = !.Yhigh - 1,
        scan_backward(FileX, FileY, Xlow, Ylow, !Xhigh, !Yhigh)
    else
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module myers.
%-----------------------------------------------------------------------------%
