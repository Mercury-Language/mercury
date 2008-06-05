#!/bin/sh
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test unused argument optimisation with intermodule analysis framework.

source ../common.sh

set -e
set -x

rm -rf Mercury

: Step 1

# Start without unused arguments in aaa2.

cat ua_m1.m.no_unused_args > ua_m1.m
$MMCMAKE ua_m1.analyse --analysis-repeat 0 || failed

check_result ua_m1 'aaa1.*"", ""'
check_result ua_m1 'aaa2.*"", ""'
check_result ua_m2 'bbb.*"", ""'
check_result ua_m3 'ccc.*"", ""'
check_statuses 'suboptimal.suboptimal.optimal.'
check_no_requests

: Step 2

$MMCMAKE ua_m1.analyse --analysis-repeat 1 || failed

check_result ua_m1 'aaa1.*"", ""'
check_result ua_m1 'aaa2.*"", ""'
check_result ua_m2 'bbb.*"", ""'
check_result ua_m3 'ccc.*"", ""'
check_statuses 'optimal.optimal.optimal.'
check_no_requests
check_imdg ua_m2 'ua_m1.*unused_args.*bbb'
check_imdg ua_m3 'ua_m2.*unused_args.*ccc'
check_imdg ua_m1 'ua_m3.*unused_args.*aaa2'

: Step 3

# Everything is fully analysed.
# Now make aaa2 have one unused argument.

sleep 1
cat ua_m1.m.unused_args > ua_m1.m
$MMCMAKE ua_m1.analyse --analysis-repeat 0 || failed

check_result ua_m1 'aaa1.*"", ""'
check_result ua_m1 'aaa2.*"", "1"'
check_result ua_m2 'bbb.*"", ""'
check_result ua_m3 'ccc.*"", ""'
check_statuses 'optimal.optimal.suboptimal.'
check_no_requests
check_imdg ua_m2 'ua_m1.*unused_args.*bbb'
check_imdg ua_m3 'ua_m2.*unused_args.*ccc'
check_imdg ua_m1 'ua_m3.*unused_args.*aaa2'

: Step 4

$MMCMAKE ua_m1.analyse --analysis-repeat 1 || failed

check_result ua_m1 'aaa1.*"", ""'
check_result ua_m1 'aaa2.*"", "1"'
check_result ua_m2 'bbb.*"", ""'
check_result ua_m3 'ccc.*"", "1"'
check_statuses 'optimal.suboptimal.optimal.'
check_no_requests

: Step 5

$MMCMAKE ua_m1.analyse --analysis-repeat 2 || failed

check_result ua_m1 'aaa1.*"", "1"'
check_result ua_m1 'aaa2.*"", "1"'
check_result ua_m2 'bbb.*"", "1"'
check_result ua_m3 'ccc.*"", "1"'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Step 6

# The unused argument is fully propagated.
# Make the argument used again and watch everything recover.

sleep 1
cat ua_m1.m.no_unused_args > ua_m1.m
$MMCMAKE ua_m1.analyse --analysis-repeat 0 || failed

check_result ua_m1 'aaa1.*"", ""'
check_result ua_m1 'aaa2.*"", ""'
check_result ua_m2 'bbb.*"", ""'
check_result ua_m3 'ccc.*"", ""'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
