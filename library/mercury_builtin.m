%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: mercury_builtin.nl.
% Main author: fjh.

% This file is automatically imported into every module.
% It is intended for things that are part of the language,
% but which are implemented just as normal user-level code
% rather than with special coding in the compiler.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mercury_builtin.
:- interface.

%-----------------------------------------------------------------------------%

% TYPES.

% The types `character', `int', `float', and `string',
% and the types `pred', `pred(T)', `pred(T1, T2)', `pred(T1, T2, T3)', ...
% are builtin and are implemented using special code in the
% type-checker.  (XXX TODO: report an error for attempts to redefine
% these types.)

%-----------------------------------------------------------------------------%

% INSTS.

% The standard insts `free', `ground', and `bound(...)' are builtin
% and are implemented using special code in the parser and mode-checker.

% Unique insts.  Currently aliased to standard insts, since unique insts
% aren't implemented yet.  Note that `bound_unique' is aliased to `bound'
% in the parser (prog_io.nl), because it couldn't be done here.

:- inst free_unique = free.
:- inst ground_unique = ground.
:- inst dead = ground.

%-----------------------------------------------------------------------------%

% MODES.

% The standard modes.

:- mode unused :: free -> free.
:- mode output :: free -> ground.
:- mode input :: ground -> ground.

:- mode in :: input.
:- mode out :: output.

% Unique modes.  Currently aliased to standard modes, since unique modes
% aren't implemented yet.

:- mode di :: input.
:- mode uo :: output.
:- mode ui :: input.

% Higher-order predicate modes.
% This needs to be builtin - the following is just a temporary hack.

:- mode pred_call :: input.
:- mode pred_call(_, _) :: input.
:- mode pred_call(_, _, _, _) :: input.
:- mode pred_call(_, _, _, _, _, _) :: input.

%-----------------------------------------------------------------------------%

% PREDICATES.

% Inequality.

:- pred T \= T.
:- mode input \= input.

% The call/N family.  Note that the compiler (make_hlds.nl) will transform
% goals which are not atoms (e.g. goals which are free variables) into
% calls to call/1.

:- pred call(pred).
:- mode call(pred_call).

% Logical connectives.
% We need to implement mode segments before we can give these predicates
% useful modes.

:- pred '=>'((pred)::pred_call, (pred)::pred_call).
:- pred '<='((pred)::pred_call, (pred)::pred_call).
:- pred '<=>'((pred)::pred_call, (pred)::pred_call).

%-----------------------------------------------------------------------------%

:- implementation.

/*
:- external("NU-Prolog", (\=)/2).
:- external("NU-Prolog", call/1).
:- external("NU-Prolog", (=>)/2).
:- external("NU-Prolog", (<=)/2).
:- external("NU-Prolog", (<=>)/2).
*/

:- end_module mercury_builtin.

%-----------------------------------------------------------------------------%
