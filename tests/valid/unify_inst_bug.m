:- module unify_inst_bug.
:- interface.

:- type t(T) ---> t ; t(t(t(T))).
:- inst t(T) ---> t ; t(t(t(T))).

:- mode m :: in(t(ground)).

:- pred p(t(int)::m, t(int)::m) is semidet.

:- implementation.

% XXX The following code attempts to abstractly unify the inst t(ground)
% with itself.  Since types have been propagated into the modes, each level
% of the inst tree contains a new type that has never been referred to before
% (e.g., t(int), t(t(int)), t(t(t(int))), etc).  Therefore the abstract
% unification never terminates.

p(X, X).

