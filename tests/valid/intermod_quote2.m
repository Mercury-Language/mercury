% Regression test to ensure that terms are properly quoted in the intermodule
% .opt file.
:- module intermod_quote2.
:- interface.

:- func '*'(func(X) = Y, func(Y) = Z, X) = Z.
:- mode '*'(func(in) = out is det, func(in) = out is det, in) = out is det.

:- implementation.

'*'(F, G, X) = G(F(X)).
