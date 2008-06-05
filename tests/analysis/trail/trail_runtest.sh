#!/bin/sh
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test trail usage analysis with the intermodule analysis framework.

source ../common.sh

set -e
set -x

rm -rf Mercury

: Step 1

cat trail_m1.m.no_trail > trail_m1.m
$MMCMAKE trail_m1.analyse --analysis-repeat 0 || failed

check_result trail_m1 'aaa1.*may_modify_trail'
check_result trail_m1 'aaa2.*will_not_modify_trail'
check_result trail_m2 'bbb.*may_modify_trail'
check_result trail_m3 'ccc.*will_not_modify_trail'
check_statuses 'suboptimal.suboptimal.optimal.'
check_no_requests

: Step 2

$MMCMAKE trail_m1.analyse --analysis-repeat 2 || failed

check_result trail_m1 'aaa1.*will_not_modify_trail'
check_result trail_m1 'aaa2.*will_not_modify_trail'
check_result trail_m2 'bbb.*will_not_modify_trail'
check_result trail_m3 'ccc.*will_not_modify_trail'
check_statuses 'optimal.optimal.optimal.'
check_no_requests
check_imdg trail_m2 'trail_m1.*trail.*bbb'
check_imdg trail_m3 'trail_m2.*trail.*ccc'
check_imdg trail_m1 'trail_m3.*trail.*aaa2'

: Step 3

sleep 1
cat trail_m1.m.trail > trail_m1.m
$MMCMAKE trail_m1.analyse --analysis-repeat 0 || failed

check_result trail_m1 'aaa1.*may_modify_trail'
check_result trail_m1 'aaa2.*may_modify_trail'
check_result trail_m2 'bbb.*may_modify_trail'
check_result trail_m3 'ccc.*may_modify_trail'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Step 4

sleep 1
cat trail_m1.m.no_trail > trail_m1.m
$MMCMAKE trail_m1.analyse --analysis-repeat 3 || failed

check_result trail_m1 'aaa1.*will_not_modify_trail'
check_result trail_m1 'aaa2.*will_not_modify_trail'
check_result trail_m2 'bbb.*will_not_modify_trail'
check_result trail_m3 'ccc.*will_not_modify_trail'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
