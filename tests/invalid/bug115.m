%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The unquoted name below caused a segmentation fault in rotd-2009-11-05.
%
%---------------------------------------------------------------------------%

:- module bug115.
:- interface.

:- pred unquoted_dèja(int::in) is semidet.

:- pred 'quoted_dèja'(int::in) is semidet.

% comment dèja

:- implementation.

unquoted_dèja(5).

'quoted_dèja'(5).
