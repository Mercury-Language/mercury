%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
% util.m
% Ralph Becket <rbeck@microsoft.com>
% Thu Nov  9 15:14:21 GMT 2000
%---------------------------------------------------------------------------%

:- module util.

:- interface.

:- import_module int.

:- type code == int.

:- type byte == code.

:- func first_new_code = code.

:- func clear_code = code.

:- func empty_code = code.

:- func max_code = code.

:- func initial_bpc = int.

:- func update_bpc(code, int) = int.

:- func int `lshift` int = int.

:- func int `rshift` int = int.

:- func ratio(int, int) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

first_new_code = 257.

clear_code = 256.

empty_code = -1.

max_code = 65535.

initial_bpc = 9.

%---------------------------------------------------------------------------%

update_bpc(N, BPC) = BPC + ( if N >= (1 `lshift` BPC) then 1 else 0 ).

%---------------------------------------------------------------------------%

X `lshift` N = unchecked_left_shift(X, N).

X `rshift` N = unchecked_right_shift(X, N).

%---------------------------------------------------------------------------%

ratio(BitsIn, BitsOut) = (BitsIn - BitsOut) `rshift` 11.

%---------------------------------------------------------------------------%
