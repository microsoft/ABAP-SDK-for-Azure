*&---------------------------------------------------------------------*
*&  Include           ZREST_SCHED_BATCH_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
*Feb' 2018  | V-MYAL   | 3019845| MP2K905030  | Initial development    *
*05/17/2018 | V-MYAL   | 3454161| MP2K905442 | More enhancements       *
*----------------------------------------------------------------------*
TABLES: zrest_monitor,    "Monitor
        zrest_config.     "Configure Interface URI Destination

TYPES: gty_zmessageid TYPE RANGE OF zrest_monitor-zmessageid.

DATA: gt_zmessageid   TYPE STANDARD TABLE OF zmid,
      gs_zmessageid   TYPE zmid,
      gv_batch_jobcnt TYPE i,
      gv_rec_cnt      TYPE i,
      gv_rc           TYPE sy-subrc,
      gv_msg_cnt      TYPE i,
      gtr_zmessageid  TYPE gty_zmessageid,
      gr_zmessageid   TYPE LINE OF gty_zmessageid.

DATA: gv_jobno(03)    TYPE n,
      gv_jobname      TYPE btcjob,
      gv_targetserver TYPE btcsrvname.

TYPES: BEGIN OF ty_zmid,
         batch_num  TYPE i,
         zmessageid TYPE zmid.
TYPES: END OF ty_zmid.

DATA: gt_zmid_batch TYPE STANDARD TABLE OF ty_zmid,
      gs_zmid_batch TYPE ty_zmid,
      gv_index      TYPE i,
      gv_batch_idx  LIKE sy-tabix.
