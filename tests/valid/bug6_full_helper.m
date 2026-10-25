%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A icfp string data structure supporting fast (logarithmic) concatenation,
% splitting, and length computation.
%
%-----------------------------------------------------------------------------%

:- module bug6_full_helper.
:- interface.

:- import_module int.
:- import_module list.

:- type icfp ---> i ; c ; f ; p.

:- type icfps == list(icfp).

:- type dna.

:- type dnas == list(dna).

:- func empty_dna = dna.

    % Convert a list of icfps into a dna.
    %
:- func dna(icfps) = dna.

    % Convert a dna into a list of icfps.
    %
:- func dna_to_icfps(dna) = icfps.

    % Append an icfp to a dna.
    %
:- func append_icfp(dna, icfp) = dna.

    % Prepend an icfp to a dna.
    %
:- func prepend_icfp(icfp, dna) = dna.

    % Append two dnas.
    %
:- func append_dnas(dna, dna) = dna.

    % The number of icfps in a dna.
    %
:- func dna_length(dna) = int.

    % Take/drop a prefix of a dna.
    %
:- func take_dna_prefix(int, dna) = dna.
:- func drop_dna_prefix(int, dna) = dna.

    % sub_dna(F, M, N) = take_dna_prefix(N - M, drop_dna_prefix(M, F))
    %
:- func sub_dna(dna, int, int) = dna.

    % Remove the first icfp from a list of dnas and return the dnas minus
    % that icfp. Fails if the list is empty.
    %
:- pred dna_head_tail(icfp::out, dnas::in, dnas::out)
    is semidet.

    % Condense a list of dnas into a single dna.
    %
:- func condense_dnas(dnas) = dna.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module pair.

    % ICFPs in a tree appear in left-to-right order.
    % c_trees are always perfectly balanced.
    %
:- type tree
    --->    l(icfp)
    ;       br(tree, tree).

    % Trees in a dna appear in right-to-left order,
    % in increasing order of depth. Adjacent trees may not
    % have the same depth. Each tree is paired with its depth.
    %
:- type dna == list(depth_tree).

:- type depth_tree == pair(depth, tree).

:- type depth == int.

%-----------------------------------------------------------------------------%

empty_dna = [].

%-----------------------------------------------------------------------------%

dna(ICFPs) = list.foldl(func(C, F) = append_icfp(F, C), ICFPs, []).

%-----------------------------------------------------------------------------%

dna_to_icfps(F) = list.foldl(cons_depth_tree_icfps, F, []).

:- func cons_depth_tree_icfps(depth_tree, icfps) = icfps.

cons_depth_tree_icfps(_D - T, ICFPs) = cons_tree_icfps(T, ICFPs).

:- func cons_tree_icfps(tree, icfps) = icfps.

cons_tree_icfps(l(ICFP), ICFPs) =
    [ICFP | ICFPs].

cons_tree_icfps(br(Tl, Tr), ICFPs) =
    cons_tree_icfps(Tl, cons_tree_icfps(Tr, ICFPs)).

%-----------------------------------------------------------------------------%

append_icfp(F, ICFP) = append_depth_tree(F, 0 - l(ICFP)).

%-----------------------------------------------------------------------------%

prepend_icfp(ICFP, F) = append_dnas([0 - l(ICFP)], F).

%-----------------------------------------------------------------------------%

    % append_depth_tree(F, DT) appends depth_tree DT to dna F.
    % The depth of DT may be no larger than the smallest (first) depth_tree
    % in F. The result is a well-formed dna.
    %
:- func append_depth_tree(dna, depth_tree) = dna.

append_depth_tree(F, D - Tr) =
    ( if F = [D - Tl | F0] then
        append_depth_tree(F0, (D + 1) - br(Tl, Tr))
      else
        [D - Tr | F]
    ).

%-----------------------------------------------------------------------------%

append_dnas(Fl, Fr) = list.foldr(append_dnas_2, Fr, Fl).

:- func append_dnas_2(depth_tree, dna) = dna.

append_dnas_2(Dr - Tr, []) = [Dr - Tr].

append_dnas_2(Dr - Tr, F0 @ [Dl - _Tl | _]) = F :-
    ( if Dr > Dl, Tr = br(Trl, Trr) then
        F1 = append_dnas_2((Dr - 1) - Trl, F0),
        F  = append_dnas_2((Dr - 1) - Trr, F1)
      else
        F  = append_depth_tree(F0, Dr - Tr)
    ).

%-----------------------------------------------------------------------------%

dna_length(F) =
    list.foldl(func(D - _T, N) = depth_tree_length(D) + N, F, 0).

:- func depth_tree_length(depth) = int.

depth_tree_length(D) = int.unchecked_left_shift(1, D).

%-----------------------------------------------------------------------------%

take_dna_prefix(N, F0) = F :-
    take_2(N, _, F0, F).

    % Take complete trees from the dna up to the point where we need to split
    % an individual tree to make the remainder of the prefix.
    %
:- pred take_2(int::in, int::out, dna::in, dna::out) is det.

take_2(N, N, [], []).

take_2(N0, N, [D - T | F0], F) :-
    take_2(N0, N1, F0, F1),
    take_3(D, T, N1, N, F1, F).

    % Add parts of a tree to a dna to make up the remainder of the prefix.
    %
:- pred take_3(depth::in, tree::in, int::in, int::out,
    dna::in, dna::out) is det.

take_3(D, T, N0, N, F0, F) :-
    ( if N0 =< 0 then
        N = N0,
        F = F0
      else if L = depth_tree_length(D), L =< N0 then
        N = N0 - L,
        F = [D - T | F0]
      else if T = br(Tl, Tr) then
        D1 = D - 1,
        take_3(D1, Tl, N0, N1, F0, F1),
        take_3(D1, Tr, N1, N,  F1, F)
      else
        throw("dna.take_3")
    ).

%-----------------------------------------------------------------------------%

drop_dna_prefix(N, F0) = F :-
    drop_2(N, _, F0, F).

    % Drop complete trees from the dna up to the point where we need to split
    % an individual tree to make the remainder of the drop.
    %
:- pred drop_2(int::in, int::out, dna::in, dna::out) is det.

drop_2(N, N, [], []).

drop_2(N0, N, [D - T | F0], F) :-
    drop_2(N0, N1, F0, F1),
    drop_3(D, T, N1, N, F1, F).

    % Add parts of a tree to a dna to make up the remainder of the drop.
    %
:- pred drop_3(depth::in, tree::in, int::in, int::out,
        dna::in, dna::out) is det.

drop_3(D, T, N0, N, F0, F) :-
    ( if N0 =< 0 then
        N = N0,
        F = append_depth_tree(F0, D - T)
      else if L = depth_tree_length(D), L =< N0 then
        N = N0 - L,
        F = F0
      else if T = br(Tl, Tr) then
        drop_3(D - 1, Tl, N0, N1, F0, F1),
        drop_3(D - 1, Tr, N1, N,  F1, F)
      else
        throw("bug6_full_helper.drop_3")
    ).

%-----------------------------------------------------------------------------%

sub_dna(F, M, N) =
    take_dna_prefix(N - M, drop_dna_prefix(M, F)).

%-----------------------------------------------------------------------------%

dna_head_tail(ICFP, [[] | Fs0], Fs) :-
    dna_head_tail(ICFP, Fs0, Fs).

dna_head_tail(ICFP, [[_ - l(ICFP)] | Fs], Fs).

dna_head_tail(ICFP, [[D - br(Tl, Tr)] | Fs0], Fs) :-
    dna_head_tail(ICFP, [[(D - 1) - Tl], [(D - 1) - Tr] | Fs0], Fs).

dna_head_tail(ICFP, [[DTr, DTl | F] | Fs0], Fs) :-
    dna_head_tail(ICFP, [[DTl | F], [DTr] | Fs0], Fs).

%-----------------------------------------------------------------------------%

condense_dnas(Fs) =
    list.foldl(func(Fa, Fb) = append_dnas(Fb, Fa), Fs, []).

%-----------------------------------------------------------------------------%
