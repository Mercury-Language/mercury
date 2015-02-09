#!/bin/sh
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test exception analysis with intermodule analysis framework.

. ../analysis/common.sh

set -e
set -x

rm -rf Mercury

: Step 1

cat excp_m1.m.no_exception > excp_m1.m
$MMCMAKE excp_m1.analyse --analysis-repeat 0 || failed

check_result excp_m1 'exception.*aaa.*may_throw_user_exception'
check_result excp_m1 'exception.*aaa2.*will_not_throw'
check_result excp_m2 'exception.*bbb.*may_throw_user_exception'
check_result excp_m3 'exception.*ccc.*will_not_throw'
check_statuses 'suboptimal.suboptimal.optimal.'
check_no_requests

: Step 2

$MMCMAKE excp_m1.analyse --analysis-repeat 3 || failed

check_result excp_m1 'exception.*aaa.*will_not_throw'
check_result excp_m1 'exception.*aaa2.*will_not_throw'
check_result excp_m2 'exception.*bbb.*will_not_throw'
check_result excp_m3 'exception.*ccc.*will_not_throw'
check_statuses 'optimal.optimal.optimal.'
check_no_requests
check_imdg excp_m2 'excp_m1.*exception.*bbb'
check_imdg excp_m3 'excp_m2.*exception.*ccc'
check_imdg excp_m1 'excp_m3.*exception.*aaa'

: Step 3

sleep 1
cat excp_m1.m.exception > excp_m1.m
$MMCMAKE excp_m1.analyse --analysis-repeat 0 || failed

check_result excp_m1 'exception.*aaa.*may_throw_user_exception'
check_result excp_m1 'exception.*aaa2.*may_throw_user_exception'
check_result excp_m2 'exception.*bbb.*may_throw_user_exception'
check_result excp_m3 'exception.*ccc.*may_throw_user_exception'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Step 4

sleep 1
cat excp_m1.m.no_exception > excp_m1.m
$MMCMAKE excp_m1.analyse --analysis-repeat 3 || failed

check_result excp_m1 'exception.*aaa.*will_not_throw'
check_result excp_m1 'exception.*aaa2.*will_not_throw'
check_result excp_m2 'exception.*bbb.*will_not_throw'
check_result excp_m3 'exception.*ccc.*will_not_throw'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
