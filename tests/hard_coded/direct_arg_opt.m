%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% With intermodule optimisation enabled, the body of indirect_new_object is
% inlined into main/2, including the construction:
%
%   yes_object(Ob : test_object) : maybe_object
%
% Because the type `test_object' is opt-imported (but not imported) from
% direct_arg_opt_helper_1.direct_arg_opt_helper_2.m, the compiler depends on
% information from direct_arg_opt_helper_1.direct_arg_opt_helper_2.opt
% to state which constructors of maybe_object need to use the direct-arg
% representation.
%
% The problem was that that information was missing from the .opt file
% because maybe_object is defined in the interface section of its module,
% i.e. it was already exported.
%

:- module direct_arg_opt.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module direct_arg_opt_helper_1.
% The bug we are testing for only occurred if
% direct_arg_opt_helper_1.direct_arg_opt_helper_2 is NOT imported directly.

main(!IO) :-
    indirect_new_object(Ob),
    indirect_check_object(Ob, !IO).
