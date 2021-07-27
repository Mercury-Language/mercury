%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The "dvisor" Benchmark.
% Part of the DPPD Library.
%
% General Description:
% 
% This is a benchmark developed by Thomas Horvath. It can be fully unfolded
% and contains no builtins or negations.

:- module advisor.

:- interface.

:- pred advisor is semidet.

:- implementation.

:- import_module advisor_impl.
:- import_module run.

advisor :-
    what_to_do_today_may(sunny, Program1),
    use(Program1),
    what_to_do_today_may(Weather1, enjoy_yourself_at_home),
    use(Weather1),
    what_to_do_today_may(foggy, Program2),
    use(Program2),
    what_to_do_today_may(Weather2, wash_your_car),
    use(Weather2).
    % type error !
    % what_to_do_today_may(nice, wash_your_car).

% The partial deduction query
% 
% :- what_to_do_today(first_of_may, _weather, _program).
% 
% The renamed run-time queries
% 
% :- what_to_do_today__1(sunny, _program).
% :- what_to_do_today__1(_wheather, enjoy_yourself_at_home).
% :- what_to_do_today__1(foggy, _program).
% :- what_to_do_today__1(_wheather, wash_your_car).
% :- what_to_do_today__1(nice, wash_your_car)
% 
% Example solution
% 
% what_to_do_today__1(sunny, go_out_to_the_nature).
% what_to_do_today__1(sunny, visit_the_golf_club).
% what_to_do_today__1(sunny, wash_your_car).
% what_to_do_today__1(sunny, it_is_fun_to_learn_Japanese).
% what_to_do_today__1(rainy, go_out_to_the_town).
% what_to_do_today__1(rainy, visit_the_bridge_club).
% what_to_do_today__1(rainy, enjoy_yourself_at_home).
% what_to_do_today__1(rainy, it_is_fun_to_learn_Japanese).
% what_to_do_today__1(foggy, go_out_to_the_town).
% what_to_do_today__1(foggy, visit_the_bridge_club).
% what_to_do_today__1(foggy, enjoy_yourself_at_home).
% what_to_do_today__1(foggy, it_is_fun_to_learn_Japanese).
% what_to_do_today__1(windy, go_out_to_the_town).
% what_to_do_today__1(windy, visit_the_bridge_club).
% what_to_do_today__1(windy, enjoy_yourself_at_home).
% what_to_do_today__1(windy, it_is_fun_to_learn_Japanese).
% 
% Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
