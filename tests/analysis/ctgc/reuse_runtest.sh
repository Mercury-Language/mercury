#!/bin/sh
# vim: sts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test the structure reuse analysis with the intermodule analysis framework.

. ../common.sh

set -e
set -x

rm -rf Mercury

: Step 1

cat reuse_m3.m.ctgc > reuse_m3.m
$MMCMAKE reuse_m1.analyse --analysis-repeat 0 || failed

check_result reuse_m1 "reuse.*main.*\[\].*no_reuse"
check_result reuse_m2 "reuse.*fiddle2.*\[\].*no_reuse"
check_result reuse_m3 "reuse.*fiddle3.*\[\].*reuse_condition"
check_statuses "optimal.suboptimal.optimal."
check_no_requests

: Step 2

$MMCMAKE reuse_m1.analyse --analysis-repeat 1 || failed

check_result reuse_m1 "_reuse.*main.*\[\].*no_reuse"
check_result reuse_m2 "_reuse.*fiddle2.*\[\].*reuse_condition"
check_result reuse_m3 "_reuse.*fiddle3.*\[\].*reuse_condition"
check_statuses "optimal.optimal.optimal."
check_request reuse_m2 "reuse_m1.*_reuse.*fiddle2.*\[1\]"
check_imdg reuse_m2 "reuse_m1.*reuse.*fiddle2.*\[\]"
check_imdg reuse_m3 "reuse_m2.*reuse.*fiddle3.*\[\]"

: Step 3

$MMCMAKE reuse_m1.analyse --analysis-repeat 1 || failed

check_result reuse_m1 "_reuse.*main.*\[\].*no_reuse"
check_result reuse_m2 "_reuse.*fiddle2.*\[\].*reuse_condition"
check_result reuse_m2 "_reuse.*fiddle2.*\[1\].*no_reuse"
check_result reuse_m3 "_reuse.*fiddle3.*\[\].*reuse_condition"
check_result reuse_m3 "_reuse.*fiddle3.*\[1\].*reuse_condition"
check_statuses "optimal.suboptimal.optimal."
check_no_requests
check_imdg reuse_m2 "reuse_m1.*reuse.*fiddle2.*\[\]"
check_imdg reuse_m3 "reuse_m2.*reuse.*fiddle3.*\[\]"

: Step 4

$MMCMAKE reuse_m1.analyse --analysis-repeat 1 || failed

check_result reuse_m1 "_reuse.*main.*\[\].*uncond"
check_result reuse_m2 "_reuse.*fiddle2.*\[\].*reuse_condition"
check_result reuse_m2 "_reuse.*fiddle2.*\[1\].*reuse_condition"
check_result reuse_m3 "_reuse.*fiddle3.*\[\].*reuse_condition"
check_result reuse_m3 "_reuse.*fiddle3.*\[1\].*reuse_condition"
check_statuses "optimal.optimal.optimal."
check_no_requests
check_imdg reuse_m2 "reuse_m1.*reuse.*fiddle2.*\[\]"
check_imdg reuse_m2 "reuse_m1.*reuse.*fiddle2.*\[1\]"
check_imdg reuse_m3 "reuse_m2.*reuse.*fiddle3.*\[\]"
check_imdg reuse_m3 "reuse_m2.*reuse.*fiddle3.*\[1\]"

: Step 5

sleep 1
cat reuse_m3.m.no_ctgc > reuse_m3.m
$MMCMAKE reuse_m1.analyse --analysis-repeat 1 || failed

check_result reuse_m1 "_reuse.*main.*\[\].*no_reuse"
check_result reuse_m2 "_reuse.*fiddle2.*\[\].*no_reuse"
check_result reuse_m2 "_reuse.*fiddle2.*\[1\].*no_reuse"
check_result reuse_m3 "_reuse.*fiddle3.*\[\].*no_reuse"
check_result reuse_m3 "_reuse.*fiddle3.*\[1\].*no_reuse"
check_statuses "optimal.optimal.optimal."
check_no_requests
check_imdg reuse_m2 "reuse_m1.*reuse.*fiddle2.*\[\]"
check_imdg reuse_m3 "reuse_m2.*reuse.*fiddle3.*\[\]"

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
