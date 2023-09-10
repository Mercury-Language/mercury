%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test to ensure that terms are properly quoted in the .opt file.
%

:- module opt_file_quote_helper_1.
:- interface.
:- import_module char.

:- func '*'(func(X) = Y, func(Y) = Z, X) = Z.
:- mode '*'(func(in) = out is det, func(in) = out is det, in) = out is det.

:- func dollar = char.
:- func hash = char.

:- func '$' = int.
:- func '#' = int.

:- pred '$' is det.
:- pred '#' is det.

:- pred '$$' is semidet.
:- pred '##' is semidet.

:- implementation.

'*'(F, G, X) = G(F(X)).

dollar = '$'.
hash = '#'.

'$' = 42.
'#' = 43.

'$'.
'#'.

'$$' :- '##'.
'##' :- '$$'.
