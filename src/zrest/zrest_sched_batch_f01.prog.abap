*&---------------------------------------------------------------------*
*&  Include           ZREST_SCHED_BATCH_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
*Feb' 2018  | V-MYAL   | 3019845| MP2K905030  | Initial development    *
*05/17/2018 | V-MYAL   | 3454161| MP2K905442 | More enhancements       *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data CHANGING pv_job_count TYPE i
                       pv_rec_cnt TYPE i
                       pv_rc TYPE sy-subrc.

  SELECT zmessageid INTO TABLE gt_zmessageid FROM zrest_monitor
         WHERE zmessageid IN s_messag
          AND    zuser            IN  s_user
         AND    zexedate         IN  s_exedat
         AND    zexetime         IN  s_exetim
         AND    zretrydate       IN  s_retdat
         AND    zretrytime       IN  s_rettim
         AND    submit_date      IN  s_subdat
         AND    submit_time      IN  s_subtim
         AND    ( httpstatus LT 200 OR httpstatus GE 300 )
         AND    interface_id     IN  s_id
         AND    calling_program  IN  s_callpr
         AND    calling_method   IN  s_callme
         AND    businessid       IN  s_busine
         AND    status = ''.

  IF sy-subrc NE 0.
    pv_rc = 1.
  ELSE.

    DESCRIBE TABLE gt_zmessageid LINES pv_rec_cnt.

***Begin of  MP2K905442
*    pv_job_count = round( val = pv_rec_cnt / p_batch dec = 0 ).
*
*    IF pv_job_count = 0.
*      pv_job_count = 1.
*    ENDIF.

    pv_job_count = round( val = pv_rec_cnt / p_batch dec = 0  mode = '1' ).

***End of  MP2K905442

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_job .

  DATA: lv_jobcnt       TYPE tbtcjob-jobcount,
        ls_print_params TYPE pri_params.

  ADD 1 TO gv_jobno.

  lv_jobcnt = gv_jobno.

***Target Server
  CLEAR gv_targetserver.
  IF NOT p_bsname IS INITIAL.
    gv_targetserver = p_bsname.
  ENDIF.

***Build Job Name

  CONCATENATE 'REST_SCHED_SUB'
              sy-datum
              sy-uzeit
              '_'
              gv_jobno
       INTO   gv_jobname.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = gv_jobname
    IMPORTING
      jobcount         = lv_jobcnt
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE i026(bt) WITH gv_jobname .
  ELSE.
*    SUBMIT zrest_scheduler WITH s_messag IN gtr_zmessageid           "MP2K905442
     SUBMIT zrest_scheduler_telemetry WITH s_messag IN gtr_zmessageid "MP2K905442
                       TO SAP-SPOOL
                    SPOOL PARAMETERS ls_print_params
                  WITHOUT SPOOL DYNPRO
                  VIA JOB gv_jobname NUMBER lv_jobcnt
                  AND RETURN.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_jobcnt
          jobname              = gv_jobname
          strtimmed            = 'X'
          targetserver         = gv_targetserver
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE s000(55) WITH gv_msg_cnt text-004 gv_jobno gv_jobname.
      ENDIF.

    ELSE.

    ENDIF.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_BATCH_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_BSNAME  text
*----------------------------------------------------------------------*
FORM f4_batch_server  CHANGING pv_bsname TYPE bpsrvgrp.

  DATA: lt_list       TYPE TABLE OF msxxlist,
        ls_list       TYPE msxxlist,
        lt_tsrvgrp    TYPE STANDARD TABLE OF tsrvgrp,
        ls_tsrvgrp    TYPE tsrvgrp,
        lv_sel_server LIKE tbtcjob-execserver.

  DATA BEGIN OF lt_sys_list OCCURS 10.
          INCLUDE STRUCTURE btctgtsrvr.
  DATA END OF lt_sys_list.

  DATA BEGIN OF field_tbl OCCURS 10.
          INCLUDE STRUCTURE help_value.
  DATA END OF field_tbl.

  CALL FUNCTION 'TH_SERVER_LIST'
    EXPORTING
      services       = '08'
    TABLES
      list           = lt_list
    EXCEPTIONS
      no_server_list = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

***Get custom server groups if any
  SELECT * FROM tsrvgrp INTO TABLE lt_tsrvgrp.


  LOOP AT lt_list INTO ls_list.
    lt_sys_list-srvname = ls_list-name.
    APPEND lt_sys_list.
  ENDLOOP.

  LOOP AT lt_tsrvgrp INTO ls_tsrvgrp.
    lt_sys_list-srvname = ls_tsrvgrp-grpname.
    APPEND lt_sys_list.
  ENDLOOP.

  FREE field_tbl.
  field_tbl-tabname    = 'BTCTGTSRVR'.
  field_tbl-fieldname  = 'SRVNAME'.
  field_tbl-selectflag = 'X'.
  APPEND field_tbl.

* read batch server
  CALL FUNCTION 'F4TOOL_F4FUNCTION_BRIDGE'
    EXPORTING
      tabname        = field_tbl-tabname
      fieldname      = field_tbl-fieldname
      display_only   = space
    IMPORTING
      selected_value = lv_sel_server
    TABLES
      value_tab      = lt_sys_list
      fields_tab     = field_tbl.

  pv_bsname = lv_sel_server.

ENDFORM.
