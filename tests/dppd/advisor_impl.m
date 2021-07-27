%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module advisor_impl.

:- interface.

:- type weather
    --->    sunny
    ;       rainy
    ;       foggy
    ;       windy.

:- type activity
    --->    go_to_work
    ;       go_out_to_the_nature
    ;       visit_the_golf_club
    ;       wash_your_car
    ;       go_out_to_the_town
    ;       visit_the_bridge_club
    ;       enjoy_yourself_at_home
    ;       it_is_fun_to_learn_Japanese
    ;       you_had_better_stay_in_bed.

:- pred what_to_do_today_may(weather, activity).
:- mode what_to_do_today_may(in, in) is semidet.
:- mode what_to_do_today_may(in, out) is nondet.
:- mode what_to_do_today_may(out, in) is nondet.
:- mode what_to_do_today_may(out, out) is nondet.

:- implementation.

what_to_do_today_may(W, A) :-
    what_to_do_today(first_of_may, W, A).

:- type day
    --->    monday
    ;       tuesday
    ;       wednesday
    ;       thursday
    ;       friday
    ;       saturday
    ;       sunday
    ;       eastern
    ;       first_of_may
    ;       christmas
    ;       new_years_day
    ;       friday_the_13th.

:- type day_type
    --->    workday
    ;       weekend
    ;       feastday
    ;       badday.

:- type weather_type
    --->    nice
    ;       nasty.

:- pred what_to_do_today(day, weather, activity).
:- mode what_to_do_today(in, in, out) is nondet.
:- mode what_to_do_today(out, in, in) is nondet.
:- mode what_to_do_today(in, in, in) is semidet.
:- mode what_to_do_today(in, out, out) is nondet.

what_to_do_today(Today, Weather, Program):-
    kind_of_day(Today, Daykind),
    kind_of_weather(Weather, Weatherkind),
    proposal(Daykind, Weatherkind, Program).

:- pred kind_of_day(day, day_type).
:- mode kind_of_day(in, out) is det.
:- mode kind_of_day(out, in) is multi.
:- mode kind_of_day(out, out) is multi.

kind_of_day(monday, workday).
kind_of_day(tuesday, workday).
kind_of_day(wednesday, workday).
kind_of_day(thursday, workday).
kind_of_day(friday, workday).
kind_of_day(saturday, weekend).
kind_of_day(sunday, weekend).
kind_of_day(eastern, feastday).
kind_of_day(first_of_may, feastday).
kind_of_day(christmas, feastday).
kind_of_day(new_years_day, badday).
kind_of_day(friday_the_13th, badday).

:- pred kind_of_weather(weather, weather_type).
:- mode kind_of_weather(in, out) is det.
:- mode kind_of_weather(out, in) is multi.
:- mode kind_of_weather(out, out) is multi.

kind_of_weather(sunny, nice).
kind_of_weather(rainy, nasty).
kind_of_weather(foggy, nasty).
kind_of_weather(windy, nasty).

:- pred proposal(day_type, weather_type, activity).
:- mode proposal(in, in, out) is nondet.
:- mode proposal(out, in, in) is nondet.

proposal(workday, _, go_to_work).
proposal(weekend, nice, go_out_to_the_nature).
proposal(weekend, nice, visit_the_golf_club).
proposal(weekend, nice, wash_your_car).
proposal(weekend, nasty, go_out_to_the_town).
proposal(weekend, nasty, visit_the_bridge_club).
proposal(weekend, nasty, enjoy_yourself_at_home).
proposal(weekend, _, it_is_fun_to_learn_Japanese).
proposal(badday, _, you_had_better_stay_in_bed).
proposal(feastday, Weather, Program) :-
    proposal(weekend, Weather, Program).
