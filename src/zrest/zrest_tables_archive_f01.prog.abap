
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM extract_data .
  DATA t_ref      TYPE REF TO data.
  DATA t_ref_all  TYPE REF TO data.

  DATA lv_cnt     TYPE i.
  DATA cur1       TYPE cursor.
  DATA lv_pack    TYPE i.

  MESSAGE 'Extract Table data'(013) TYPE 'S'.

* //Test mode - collect total count to display
  OPEN CURSOR WITH HOLD cur1 FOR
  SELECT * FROM zrest_monitor
    WHERE zmessageid   IN s_mesgid
      AND zexedate     IN s_exedat
      AND zexetime     IN s_exetim
      AND interface_id IN s_interf
      AND zuser        IN s_zuser.

  DO.
    FETCH NEXT CURSOR cur1 INTO TABLE gt_zrest_monitor PACKAGE SIZE p_pack.
    IF sy-subrc IS INITIAL.

*     //collect total record and package
      DESCRIBE TABLE gt_zrest_monitor LINES lv_cnt.
      gv_ext_cnt = gv_ext_cnt + lv_cnt.
      lv_pack = lv_pack + 1.

      PERFORM f_delete_data.   "Delete table date

    ELSE.
      CLOSE CURSOR cur1.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " EXTRACT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_delete_data .
  DATA lv_line TYPE i.
  DATA: lt_zrest_mon_header TYPE STANDARD TABLE OF zrest_mon_header.
  DATA: lt_zrest_mon_trace TYPE STANDARD TABLE OF zrest_mon_trace.
  DATA: lt_zrest_monitorlog TYPE STANDARD TABLE OF zrest_monitorlog.
  DATA: lt_zrest_mo_payload TYPE STANDARD TABLE OF zrest_mo_payload.
  DATA: lt_zrest_retries TYPE STANDARD TABLE OF zrest_retries.

  IF gt_zrest_monitor IS NOT INITIAL.
*Delete zrest_mon_header
    SELECT * FROM zrest_mon_header
      INTO TABLE lt_zrest_mon_header
      FOR ALL ENTRIES IN gt_zrest_monitor
      WHERE messageid = gt_zrest_monitor-zmessageid.
    IF sy-subrc EQ 0 AND lt_zrest_mon_header IS NOT INITIAL.
      IF p_test IS INITIAL.
        DELETE zrest_mon_header FROM TABLE lt_zrest_mon_header.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'DB_COMMIT'.
        ENDIF.
      ENDIF.
      DESCRIBE TABLE lt_zrest_mon_header LINES lv_line.
      gv_del_cnt_zrest_mon_header = gv_del_cnt_zrest_mon_header + lv_line.
    ENDIF.
*Delete zrest_mon_trace
    SELECT * FROM zrest_mon_trace
      INTO TABLE lt_zrest_mon_trace
      FOR ALL ENTRIES IN gt_zrest_monitor
      WHERE zmessageid = gt_zrest_monitor-zmessageid.
    IF sy-subrc EQ 0 AND lt_zrest_mon_trace IS NOT INITIAL.
      IF p_test IS INITIAL.
        DELETE zrest_mon_trace FROM TABLE lt_zrest_mon_trace.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'DB_COMMIT'.
        ENDIF.
      ENDIF.
      DESCRIBE TABLE lt_zrest_mon_trace LINES lv_line.
      gv_del_cnt_zrest_mon_trace = gv_del_cnt_zrest_mon_trace + lv_line.
    ENDIF.
*Delete zrest_monitorlog
    SELECT * FROM zrest_monitorlog
      INTO TABLE lt_zrest_monitorlog
      FOR ALL ENTRIES IN gt_zrest_monitor
      WHERE zmessageid = gt_zrest_monitor-zmessageid.
    IF sy-subrc EQ 0 AND lt_zrest_monitorlog IS NOT INITIAL.
      IF p_test IS INITIAL.
        DELETE zrest_monitorlog FROM TABLE lt_zrest_monitorlog.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'DB_COMMIT'.
        ENDIF.
      ENDIF.
      DESCRIBE TABLE lt_zrest_monitorlog LINES lv_line.
      gv_del_cnt_zrest_monitorlog = gv_del_cnt_zrest_monitorlog + lv_line.
    ENDIF.
*Delete zrest_mo_payload
    SELECT * FROM zrest_mo_payload
      INTO TABLE lt_zrest_mo_payload
      FOR ALL ENTRIES IN gt_zrest_monitor
      WHERE messageid = gt_zrest_monitor-zmessageid.
    IF sy-subrc EQ 0 AND lt_zrest_mo_payload IS NOT INITIAL.
      IF p_test IS INITIAL.
        DELETE zrest_mo_payload FROM TABLE lt_zrest_mo_payload.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'DB_COMMIT'.
        ENDIF.
      ENDIF.
      DESCRIBE TABLE lt_zrest_mo_payload LINES lv_line.
      gv_del_cnt_zrest_mo_payload = gv_del_cnt_zrest_mo_payload + lv_line.
    ENDIF.
*Delete zrest_retries
    SELECT * FROM zrest_retries
      INTO TABLE lt_zrest_retries
      FOR ALL ENTRIES IN gt_zrest_monitor
      WHERE zmessageid = gt_zrest_monitor-zmessageid.
    IF sy-subrc EQ 0 AND lt_zrest_retries IS NOT INITIAL.
      IF p_test IS INITIAL.
        DELETE zrest_retries FROM TABLE lt_zrest_retries.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'DB_COMMIT'.
        ENDIF.
      ENDIF.
      DESCRIBE TABLE lt_zrest_retries LINES lv_line.
      gv_del_cnt_zrest_retries = gv_del_cnt_zrest_retries + lv_line.
    ENDIF.
*Delete zrest_monitor
    IF p_test IS INITIAL.
      DELETE zrest_monitor FROM TABLE gt_zrest_monitor.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'DB_COMMIT'.
      ENDIF.
    ENDIF.
    DESCRIBE TABLE gt_zrest_monitor LINES lv_line.
    gv_del_cnt_zrest_monitor = gv_del_cnt_zrest_monitor + lv_line.
  ENDIF.

ENDFORM.                    " F_DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  DISP_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disp_output .

  IF p_test IS NOT INITIAL.
    MESSAGE s000(55) WITH  'TEST RUN, No records will be deleted..'(025).
  ENDIF.
  MESSAGE s000(55) WITH 'Total records selected from zrest_monitor-'(018) gv_ext_cnt.
  MESSAGE s000(55) WITH 'Total records deleted from zrest_monitor-'(019) gv_del_cnt_zrest_monitor.
  MESSAGE s000(55) WITH 'Total records deleted from zrest_mon_header-'(020) gv_del_cnt_zrest_mon_header.
  MESSAGE s000(55) WITH 'Total records deleted from zrest_mon_trace-'(021) gv_del_cnt_zrest_mon_trace.
  MESSAGE s000(55) WITH 'Total records deleted from zrest_monitorlog-'(022) gv_del_cnt_zrest_monitorlog.
  MESSAGE s000(55) WITH 'Total records deleted from zrest_mo_payload-'(023) gv_del_cnt_zrest_mo_payload.
  MESSAGE s000(55) WITH 'Total records deleted from zrest_retries-'(024) gv_del_cnt_zrest_retries.

ENDFORM.                    " DISP_OUTPUT
