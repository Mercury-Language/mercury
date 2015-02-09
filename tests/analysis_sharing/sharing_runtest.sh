#!/bin/sh
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test structure sharing analysis with intermodule analysis framework.

. ../analysis/common.sh

set -e
set -x

rm -rf Mercury

: Step 1

cat sharing_m1.m.no_share > sharing_m1.m
$MMCMAKE sharing_m1.analyse --analysis-repeat 0 || failed

check_result sharing_m1 'sharing.*aaa.* t,'
check_result sharing_m1 'sharing.*aaa2.* b,'
check_result sharing_m2 'sharing.*bbb.* t,'
check_result sharing_m3 'sharing.*ccc.* b,'
check_statuses 'suboptimal.suboptimal.optimal.'
check_no_requests

: Step 2

$MMCMAKE sharing_m1.analyse --analysis-repeat 3 || failed

check_result sharing_m1 'sharing.*aaa.* b,'
check_result sharing_m1 'sharing.*aaa2.* b,'
check_result sharing_m2 'sharing.*bbb.* b,'
check_result sharing_m3 'sharing.*ccc.* b,'
check_statuses 'optimal.optimal.optimal.'
check_no_requests
check_imdg sharing_m2 'sharing_m1.*sharing.*bbb'
check_imdg sharing_m3 'sharing_m2.*sharing.*ccc'
check_imdg sharing_m1 'sharing_m3.*sharing.*aaa2'

: Step 3

sleep 1
cat sharing_m1.m.share > sharing_m1.m
$MMCMAKE sharing_m1.analyse --analysis-repeat 0 || failed

check_result sharing_m1 'sharing.*aaa.*cel.*\[\].*cel.*\[\]'
check_result sharing_m1 'sharing.*aaa2.*cel.*\[\].*cel.*\[\]'
check_result sharing_m2 'sharing.*bbb.*cel.*\[\].*cel.*\[\]'
check_result sharing_m3 'sharing.*ccc.*cel.*\[\].*cel.*\[\]'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Step 4

sleep 1
cat sharing_m1.m.no_share > sharing_m1.m
$MMCMAKE sharing_m1.analyse --analysis-repeat 3 || failed

check_result sharing_m1 'sharing.*aaa.* b,'
check_result sharing_m1 'sharing.*aaa2.* b,'
check_result sharing_m2 'sharing.*bbb.* b,'
check_result sharing_m3 'sharing.*ccc.* b,'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
