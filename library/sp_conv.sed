# \
This sed script is used to convert from NU-Prolog to Sicstus Prolog \
It does three things: delete the `:- module' line, \
expand backslash escapes, and replace the use \
of `^' for xor in calls to is/2 with `#'. \

/ is /s/\^/#/g
/^:- *module/d
/^[ 	]*%/s/.*//
/\\\\/s//\\/g
/\\a/s///g
/\\b/s///g
/\\r/s///g
/\\f/s///g
/\\t/s//	/g
/\\n/s//\
/g
/\\v/s///g
