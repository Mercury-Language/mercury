%% Simple test for various things related to "crimes" database.

:- module aditi_error_bug2.

:- interface.

:- import_module aditi.
:- import_module aditi_error_bug3.

:- pred murders(aditi.state,city,us_state,year,count).
:- mode murders(aditi.aditi_mui,out,out,out,out) is nondet.
:- pragma aditi(assaults/5).

:- pred assaults(aditi.state,city,us_state,year,count).
:- mode assaults(aditi.aditi_mui,out,out,out,out) is nondet.
:- pragma aditi(murders/5).

:- pred crime(aditi.state,crime_type,city,us_state,year,count).
:- mode crime(aditi.aditi_mui,out,out,out,out,out) is nondet.
:- pragma aditi(crime/6).

:- pred crimes(
        aditi.state::aditi.aditi_mui,
        city::out,      % city name
        us_state::out,  % state name
        year::out,      % year
        count::out,     % crime index total
        count::out,     % modified crime index total
        count::out,     % murders
        count::out,     % rapes
        count::out,     % robberies
        count::out,     % aggravated assaults
        count::out,     % burglaries
        count::out,     % larceny thefts
        count::out,     % MV thefts
        count::out      % arsons
        ) is nondet.
:- pragma base_relation(crimes/14).

:- implementation.

murders(DB,City,State,Year,Number) :-
        crimes(DB,City,State,Year,_,_,Number,_,_,_,_,_,_,_).

assaults(DB,City,State,Year,Number) :-
        crimes(DB,City,State,Year,_,_,_,_,_,Number,_,_,_,_).

crime(DB,murder,City,State,Year,Number) :-
        murders(DB,City,State,Year,Number).
crime(DB,assault,City,State,Year,Number) :-
        assaults(DB,City,State,Year,Number).

