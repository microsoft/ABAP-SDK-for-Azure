
DATA gv_ext_cnt   TYPE i.
DATA gv_del_cnt_zrest_monitor    TYPE i.
DATA gv_del_cnt_zrest_mon_header   TYPE i.
DATA gv_del_cnt_zrest_mon_trace   TYPE i.
DATA gv_del_cnt_zrest_monitorlog   TYPE i.
DATA gv_del_cnt_zrest_mo_payload   TYPE i.
DATA gv_del_cnt_zrest_retries   TYPE i.

DATA: gt_zrest_monitor TYPE STANDARD TABLE OF zrest_monitor.

DATA: lw_monitor     TYPE zrest_monitor.
