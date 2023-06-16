%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module trans_intermod_user_equality_helper_1.
:- interface.

:- import_module trans_intermod_user_equality_helper_2.

:- type bar == foo.

:- pred make_bar(int::in, int::in, bar::out) is det.

:- pred use_bar(bar::in, int::out) is cc_multi.

:- implementation.

make_bar(M, N, ctor1(M, N)).

use_bar(ctor1(_, N), N).
use_bar(ctor2(_, N), N).
