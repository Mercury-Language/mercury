%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This program causes the compiler (rotd-2016-06-09) to abort with the
% following:
%
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%  Key Type: int
%  Key Value: 1
%  Value Type: hlds.hlds_pred.proc_info
%
%---------------------------------------------------------------------------%

:- module bug410.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module random.
:- import_module solutions.
:- import_module string.
:- import_module time.

%---------------------------------------------------------------------------%

main(!IO) :-
    Gen = gen(0),
    make_titles(Gen, [], _ : list(string)).

%---------------------------------------------------------------------------%

:- type generator == pred(maybe(string), list(string), list(string)).
:- inst generator == (pred(out, in, out) is det).

    % There was a closing parenthesis missing here.
    % ---------------------------------------+
    %                                        v
% :- pred make_titles(generator::in(generator,
%   list(string)::in, list(string)::out) is det.

make_titles(GenPred, !Used) :-
    GenPred(_MaybeTitle, !Used),
    make_titles(GenPred, !Used).

:- pred gen(int::in, bool::out, list(string)::in, list(string)::out) is det.

gen(_, MaybeTitle, !Used) :-
    MaybeTitle = yes.

%---------------------------------------------------------------------------%
:- end_module bug410.
%---------------------------------------------------------------------------%
