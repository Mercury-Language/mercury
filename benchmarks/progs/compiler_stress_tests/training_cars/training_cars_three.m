:- module training_cars_three.

% synopsis: Describes the facts that capture the characteristics
%           of cars pulling up to gas stations, and their purchases.
% author:   Douglas M. Auclair (DMA)
% date:     March 21, 2006

:- interface.

:- import_module assoc_list.
:- import_module string.

:- type class
	--->	class(string).

:- type attributes
	--->	attributes(assoc_list(string, string.poly_type)).

:- pred car(int::in, class::out, attributes::out) is semidet.

:- implementation.

:- import_module std_util, pair, list.

% automatically generated facts follow:

car(0, class("fill up only"), attributes(["state/province" - s("WA"), "tag" - s("IPD050"), "year" - i(2004), "month" - i(12), "day" - i(26), "hour" - i(15), "minute" - i(42), "station" - s("mobil"), "fill-ups" - i(4), "record history (days)" - i(27), "weekdays" - i(3), "weekends" - i(1), "preferred day" - s("tuesday"), "day preference %" - i(25), "mean time of visit (minutes after midnight)" - i(195), "standard deviation from mean" - i(293), "different locations" - i(2), "preferred station" - s("mobil"), "preferred station %" - i(50), "% pay cashier (instead of at pump)" - i(25), "payment method" - s("credit"), "preferred pump" - s("17LG"), "preferred pump %" - i(25)])).
car(1, class("fill up only"), attributes(["state/province" - s("MM"), "tag" - s("88434BH"), "year" - i(2005), "month" - i(1), "day" - i(5), "hour" - i(17), "minute" - i(25), "station" - s("mobil"), "fill-ups" - i(122), "record history (days)" - i(145), "weekdays" - i(108), "weekends" - i(14), "preferred day" - s("thursday"), "day preference %" - i(22), "mean time of visit (minutes after midnight)" - i(9), "standard deviation from mean" - i(104), "different locations" - i(2), "preferred station" - s("mobil"), "preferred station %" - i(84), "% pay cashier (instead of at pump)" - i(3), "payment method" - s("credit"), "preferred pump" - s("07HG"), "preferred pump %" - i(31)])).
car(2, class("fill up only"), attributes(["state/province" - s("MM"), "tag" - s("6DNI590"), "year" - i(2004), "month" - i(11), "day" - i(5), "hour" - i(16), "minute" - i(22), "station" - s("mobil"), "fill-ups" - i(18), "record history (days)" - i(146), "weekdays" - i(15), "weekends" - i(3), "preferred day" - s("wednesday"), "day preference %" - i(22), "mean time of visit (minutes after midnight)" - i(66), "standard deviation from mean" - i(266), "different locations" - i(2), "preferred station" - s("mobil"), "preferred station %" - i(55), "% pay cashier (instead of at pump)" - i(11), "payment method" - s("credit"), "preferred pump" - s("17YH"), "preferred pump %" - i(44)])).
