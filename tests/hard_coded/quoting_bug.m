:- module quoting_bug.
:- interface.
:- type token ---> '?' ; ('+') ; (*) ; && ; += ; -= .
:- inst '?' ---> '?' ; ('+') ; (*) ; && ; += ; -= .
