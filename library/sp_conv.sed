# this sed script is used to convert from NU-Prolog to Sicstus Prolog
/^:- *module/d
/^[ 	]*%/s/.*//
/\\\\/s//\\/g
/\\t/s//	/g
/\\b/s///g
/\\n/s//\
/g
