#!/bin/sh
if [  $# != 1 ]
then
	echo "usage: `basename $0` module-name" 1>&2
	exit 1
fi

file="@LIBDIR@/$1.int"

if [ -r $file ]
then
	${PAGER:-more} $file
else
	echo "`basename $0`: no interface file for $1." 1>&2
fi
