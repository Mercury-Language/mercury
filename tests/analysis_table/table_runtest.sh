#!/bin/sh
# vim: ft=sh ts=8 sts=4 sw=4 et
#-----------------------------------------------------------------------------#
# Test --analyse-mm-tabling with the intermodule analysis framework.

. ../analysis/common.sh

set -e
set -x

rm -rf Mercury

: Step 1

cat table_m1.m.no_tabling > table_m1.m
$MMCMAKE table_m1.analyse --analysis-repeat 0 || failed

check_result table_m1 'tabling.*aaa.*mm_tabled_may_call'
check_result table_m1 'tabling.*aaa2.*mm_tabled_will_not_call'
check_result table_m2 'tabling.*bbb.*mm_tabled_may_call'
check_result table_m3 'tabling.*ccc.*mm_tabled_will_not_call'
check_statuses 'suboptimal.suboptimal.optimal.'
check_no_requests

: Step 2

$MMCMAKE table_m1.analyse --analysis-repeat 2 || failed

check_result table_m1 'tabling.*aaa.*mm_tabled_will_not_call'
check_result table_m1 'tabling.*aaa2.*mm_tabled_will_not_call'
check_result table_m2 'tabling.*bbb.*mm_tabled_will_not_call'
check_result table_m3 'tabling.*ccc.*mm_tabled_will_not_call'
check_statuses 'optimal.optimal.optimal.'
check_no_requests
check_imdg table_m2 'table_m1.*tabling.*bbb'
check_imdg table_m3 'table_m2.*tabling.*ccc'
check_imdg table_m1 'table_m3.*tabling.*aaa'

: Step 3

sleep 1
cat table_m1.m.tabling > table_m1.m
$MMCMAKE table_m1.analyse --analysis-repeat 0 || failed

check_result table_m1 'tabling.*aaa.*mm_tabled_may_call'
check_result table_m1 'tabling.*aaa2.*mm_tabled_may_call'
check_result table_m2 'tabling.*bbb.*mm_tabled_may_call'
check_result table_m3 'tabling.*ccc.*mm_tabled_may_call'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Step 4

sleep 1
cat table_m1.m.no_tabling > table_m1.m
$MMCMAKE table_m1.analyse --analysis-repeat 3 || failed

check_result table_m1 'tabling.*aaa.*mm_tabled_will_not_call'
check_result table_m1 'tabling.*aaa2.*mm_tabled_will_not_call'
check_result table_m2 'tabling.*bbb.*mm_tabled_will_not_call'
check_result table_m3 'tabling.*ccc.*mm_tabled_will_not_call'
check_statuses 'optimal.optimal.optimal.'
check_no_requests

: Succeeded

exit 0

#-----------------------------------------------------------------------------#
