#!/bin/sh
#---------------------------------------------------------------------------#
# vim: ts=4 sw=4 expandtab ft=sh
#---------------------------------------------------------------------------#
# Copyright (C) 2020 The Mercury team.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#
#
# list_cmd.sh <source-file> <first-line> <last-line> <mark-line>
#

SCRIPT='
NR >= first_line && NR <= last_line {
    if (NR == mark_line) {
        print "*" NR, $0;
    } else {
        print " " NR, $0;
    }
}' 

exec awk \
    -v first_line="$2" \
    -v last_line="$3" \
    -v mark_line="$4" \
    "${SCRIPT}" \
    "$1"

#-----------------------------------------------------------------------------#
