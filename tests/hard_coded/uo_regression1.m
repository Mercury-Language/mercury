%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for Mantis bug 94. Versions of the compiler before
% 22 Sep 2009 used to generate a spurious mode error for init_message,
% because the term being returned, being in a from_ground_term_construct scope,
% was inferred to be ground, not unique.

:- module uo_regression1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- type status
    --->    status_ok
    ;       status_error.

    % Must have more fields than --from-ground-term-threshold.
:- type response
    --->    response(
                response_status             :: status,
                response_error_message      :: maybe(string),
                response_sol_set            :: maybe(string),
                response_tbox_answer        :: maybe(string),
                response_custom_response    :: maybe(string)
            ).

:- func init_message = (response::uo) is det.

init_message = response(status_ok, no, no, no, no).

main(!IO) :-
    io.write_line(init_message, !IO).
