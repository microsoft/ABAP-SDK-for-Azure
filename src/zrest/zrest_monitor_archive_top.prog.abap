*&---------------------------------------------------------------------*
*&  Include           ZREST_MONITOR_ARCHIVE_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
*03/08/2019 | WIBRADFO | 1673104| SMTK905163 | Initial Version         *
*----------------------------------------------------------------------*

data: lt_monitor     type table of zrest_monitor.
data: lw_monitor     type zrest_monitor.
data: lw_mon_header  type zrest_mon_header.
data: lw_mon_trace   type zrest_mon_trace.
data: lt_mon_payload type table of zrest_mo_payload.
data: lw_monitorlog  type zrest_monitorlog.
data: lv_heading     type boolean.
data: lv_count type i.
data: lw_zrest_mo_payload type zrest_mo_payload.
data: lt_zrest_mo_payload type table of zrest_mo_payload.

data:
  lt_zrest_mon_header type table of zrest_mon_header,
  lt_zrest_mon_trace  type table of zrest_mon_trace,
  lt_zrest_monitorlog type table of zrest_monitorlog.

data:
  lw_zrest_mon_header type zrest_mon_header,
  lw_zrest_mon_trace  type zrest_mon_trace,
  lw_zrest_monitorlog type zrest_monitorlog.
