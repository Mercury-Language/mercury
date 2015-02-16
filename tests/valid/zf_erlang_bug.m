% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% This module results in the following error message from the Erlang compiler
% when compiled in the erlang grade:
%
%    Mercury/erls/zf_erlang_bug.erl:64:
%    variable 'STATE_VARIABLE_IO_1_12' unsafe in 'case' (line 43)
%
% This was with Mercury rotd-2009-06-02 (+ a few of the diffs posted that day).
% To reproduce:
%
%   mmc --grade erlang --make zf_erlang_bug.beam
%
% This test case is a cut-down version of the module zinc_frontend from
% rotd-2009-05-31 of the MiniZinc distribution.
%
%---------------------------------------------------------------------------%

:- module zf_erlang_bug.
:- interface.

:- import_module list.
:- import_module io.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type writer(T) == ( pred( T, io, io)        ).
:- inst writer    == ( pred(in, di, uo) is det ).

:- pred do_io_stage(list(string), writer(A), A, io, io).
:- mode do_io_stage(in, in(writer), in, di, uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

do_io_stage(StageNames, PreDumper, Input, !IO) :-
    ( if member("foo", StageNames) then
        PreDumper(Input, !IO)
      else
        true
    ),
    ( if member("bar", StageNames) then
        PreDumper(Input, !IO)
      else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module zf_erlang_bug.
%---------------------------------------------------------------------------%
