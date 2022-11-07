*&---------------------------------------------------------------------*
*&  Include           ZREST_MONITOR_ARCHIVE_F01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
*03/08/2019 | WIBRADFO | 1673104| SMTK905163 | Initial Version         *
*----------------------------------------------------------------------*
*-----------|----------|--------|------------|-------------------------*
*09/28/2022 | V-ASHOKM1 |       | SMTK907899 | Fixing Virtual Forge    *
*                                              Errors                  *
*----------------------------------------------------------------------*

FORM extract_data.

*----------------------------------------------------------------------*
* Called by    : ZREST_MONITOR_ARCHIVE                                 *
* Description  : The purpose of this report is to archive older entries*
*                of table ZREST_MONITOR                                *
*                This form reads the ZREST_MONITOR database entries    *
*                depending on the screen parameter input.              *
*----------------------------------------------------------------------*

  SELECT * FROM zrest_monitor INTO TABLE lt_monitor WHERE zmessageid   IN s_mesgid
                                                      AND zexedate     IN s_exedat
                                                      AND zexetime     IN s_exetim
                                                      AND interface_id IN s_interf
                                                      AND businessid   IN s_busnid
                                                      AND zuser        IN s_zuser.
  DESCRIBE TABLE lt_monitor LINES sy-dbcnt.
  MESSAGE s000(55) WITH  sy-dbcnt 'Monitor records selected'(019).

ENDFORM.



FORM archive_data.

*----------------------------------------------------------------------*
* Called by    : ZREST_MONITOR_ARCHIVE                                 *
* Description  : The purpose of this report is to archive older entries*
*                of table ZREST_MONITOR                                *
*                This form marks records for deletion or deletes them  *
*                depending on the screen parameter input.              *
*----------------------------------------------------------------------*

  IF  p_mark IS NOT INITIAL.

    MESSAGE s000(55) WITH 'Marking records for deletion'(018).

    LOOP AT lt_monitor INTO lw_monitor .

*      select single * from zrest_monitor into lw_monitor where zmessageid = lw_monitor-zmessageid.
*
*      if sy-subrc = 0.

      lw_monitor-zdelete = 'X'.
      lw_monitor-deleteuser = sy-uname.
      lw_monitor-deletedate = sy-datum.
      lw_monitor-deletetime = sy-uzeit.

      MODIFY zrest_monitor FROM lw_monitor." WHERE messageid = sel_row-zmessageid.

      IF sy-subrc =  0.
        ADD 1 TO lv_count.
        IF lv_heading = abap_false.
          WRITE:    / 'Record marked for deletion:-'(001).
          SKIP.
          lv_heading = abap_true.
        ENDIF.

        WRITE:    / lw_monitor-zmessageid      ,
                    lw_monitor-zuser           ,
                    lw_monitor-zexedate        ,
                    lw_monitor-zexetime        ,
                    lw_monitor-status+00(5)             ,
                    lw_monitor-httpstatus        ,
                    lw_monitor-host+00(20)              ,
                    lw_monitor-uri+00(20)               ,
                    lw_monitor-interface_id       ,
                    lw_monitor-calling_program+00(20)      ,
                    lw_monitor-calling_method+00(20)       ,
                    lw_monitor-businessid          .

      ENDIF.

*      endif.


    ENDLOOP.
    MESSAGE s000(55) WITH lv_count 'Records marked for deletion'(020).
  ELSE.

    MESSAGE s000(55) WITH 'Deleting records'(021).
    " Begin of changes by V-ASHOKM1 SMTK907899

    IF lt_monitor IS NOT INITIAL.
      SELECT * FROM zrest_mon_trace INTO TABLE @DATA(lt_mon_trace)
         FOR ALL ENTRIES IN @lt_monitor
         WHERE zmessageid = @lt_monitor-zmessageid.
      IF sy-subrc = 0.
        SORT  lt_mon_trace BY zmessageid.
      ENDIF.
    ENDIF.
    " End of changes by V-ASHOKM1 SMTK907899


    LOOP AT lt_monitor INTO lw_monitor WHERE zdelete IS NOT INITIAL.

*      SELECT SINGLE * FROM zrest_monitor INTO lw_monitor WHERE zmessageid = lw_monitor-zmessageid.
*      IF sy-subrc = 0.
      DELETE zrest_monitor FROM lw_monitor.
*      ENDIF.

      IF sy-subrc = 0.
        ADD 1 TO lv_count.

        IF lv_heading = abap_false.
          WRITE:    / 'ZREST_MONITOR Record deleted:-'(002).
          SKIP.
          lv_heading = abap_true.
        ENDIF.
        WRITE:    / lw_monitor-zmessageid      ,
                    lw_monitor-zuser           ,
                    lw_monitor-zexedate        ,
                    lw_monitor-zexetime        ,
                    lw_monitor-interface_id       ,
                    lw_monitor-calling_program+00(20) ,
                    lw_monitor-businessid          .
      ENDIF.

      SELECT * FROM zrest_mon_header INTO lw_mon_header WHERE messageid = lw_monitor-zmessageid.
        DELETE zrest_mon_header FROM lw_mon_header.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE s000(55) WITH 'ZREST_MON_HEADER delete'(008)
                                'Return code:'(007)
                                 sy-subrc.
        ENDIF.

      ENDSELECT.

      IF sy-subrc = 0.
        WRITE:    / 'ZREST_MON_HEADER deleted'(003).
      ENDIF.
      " Begin of changes by V-ASHOKM1 -- SMTK907899

*      SELECT SINGLE * FROM zrest_mon_trace INTO lw_mon_trace WHERE zmessageid = lw_monitor-zmessageid.
*
*      IF sy-subrc = 0.
*        DELETE zrest_mon_trace FROM lw_mon_trace.
*      ENDIF.
*
*      IF sy-subrc = 0.
*        WRITE:    / 'ZREST_MON_TRACE deleted'(004).
*      ENDIF.
      " End of changes by V-ASHOKM1 -- SMTK907899

     "Begin of changes by V-ASHOKM1 ++ SMTK907899
      lw_mon_trace = VALUE #( lt_mon_trace[ zmessageid = lw_monitor-zmessageid ] OPTIONAL ).
      IF lw_mon_trace IS NOT INITIAL.
        DELETE zrest_mon_trace FROM  lw_mon_trace.
        IF sy-subrc = 0.
          WRITE:    / 'ZREST_MON_TRACE deleted'(004).
        ENDIF.
      ENDIF.

    "End of changes by V-ASHOKM1 ++ SMTK907899


      SELECT * FROM zrest_monitorlog INTO lw_monitorlog WHERE zmessageid = lw_monitor-zmessageid.
        DELETE zrest_monitorlog FROM lw_monitorlog.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE s000(55) WITH 'ZREST_MONITORLOG delete'(006)
                                'Return code:'(007)
                                 sy-subrc.
        ENDIF.

      ENDSELECT.

      IF sy-subrc = 0.
        WRITE:    / 'ZREST_MONITORLOG deleted'(005).
      ENDIF.

      SELECT * FROM zrest_mo_payload INTO TABLE lt_mon_payload WHERE messageid = lw_monitor-zmessageid.
      DELETE zrest_mo_payload FROM TABLE lt_mon_payload.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE s000(55) WITH 'ZREST_MO_PAYLOAD delete'(010)
                              'Return code:'(007)
                               sy-subrc.
      ELSE.
        WRITE:    / 'ZREST_MO_PAYLOAD deleted'(009).
      ENDIF.

    ENDLOOP.
    MESSAGE s000(55) WITH lv_count 'Monitor records deleted'(017).


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_ORPHANS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_orphans .


  CLEAR lv_count.

  IF  p_mark IS INITIAL.

    MESSAGE s000(55) WITH 'Orphan management begins'(016).


    SELECT * INTO TABLE lt_zrest_mo_payload FROM zrest_mo_payload AS zrest_mo_payload
             WHERE NOT EXISTS (
      SELECT zrest_monitor~zmessageid FROM zrest_monitor AS zrest_monitor
       WHERE zrest_monitor~zmessageid = zrest_mo_payload~messageid ). " < - - - -  ANTI JOIN


    DELETE zrest_mo_payload FROM TABLE lt_zrest_mo_payload.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(55) WITH 'ZREST_MO_PAYLOAD delete'(010)
                            'Return code:'(007)
                             sy-subrc.
    ENDIF.
*
    LOOP AT lt_zrest_mo_payload INTO lw_zrest_mo_payload.
      WRITE: / 'Orphan ZREST_MO_PAYLOAD'(015), lw_zrest_mo_payload-messageid, 'deleted'(011).
      ADD 1 TO lv_count.
    ENDLOOP.





    SELECT * INTO TABLE lt_zrest_mon_header FROM zrest_mon_header AS zrest_mon_header
              WHERE NOT EXISTS (
       SELECT zrest_monitor~zmessageid FROM zrest_monitor AS zrest_monitor
        WHERE zrest_monitor~zmessageid = zrest_mon_header~messageid ). " < - - - -  ANTI JOIN

    DELETE zrest_mon_header FROM TABLE lt_zrest_mon_header.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(55) WITH 'ZREST_MON_HEADER delete'(008)
                            'Return code:'(007)
                             sy-subrc.
    ENDIF.
*
    LOOP AT lt_zrest_mon_header INTO lw_zrest_mon_header.
      WRITE: / 'Orphan ZREST_MON_HEADER'(014), lw_zrest_mon_header-messageid, 'deleted'(011).
      ADD 1 TO lv_count.
    ENDLOOP.





    SELECT * INTO TABLE lt_zrest_mon_trace FROM zrest_mon_trace AS zrest_mon_trace
              WHERE NOT EXISTS (
       SELECT zrest_monitor~zmessageid FROM zrest_monitor AS zrest_monitor
        WHERE zrest_monitor~zmessageid = zrest_mon_trace~zmessageid ). " < - - - -  ANTI JOIN

    DELETE zrest_mon_trace FROM TABLE lt_zrest_mon_trace.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(55) WITH 'ZREST_MON_TRACE delete'(009)
                            'Return code:'(007)
                             sy-subrc.
    ENDIF.
*
    LOOP AT lt_zrest_mon_trace INTO lw_zrest_mon_trace.
      WRITE: / 'Orphan ZREST_MON_TRACE'(013), lw_zrest_mon_trace-zmessageid, 'deleted'(011).
      ADD 1 TO lv_count.
    ENDLOOP.





    SELECT * INTO TABLE lt_zrest_monitorlog FROM zrest_monitorlog AS zrest_monitorlog
              WHERE NOT EXISTS (
       SELECT zrest_monitor~zmessageid FROM zrest_monitor AS zrest_monitor
        WHERE zrest_monitor~zmessageid = zrest_monitorlog~zmessageid ). " < - - - -  ANTI JOIN

    DELETE zrest_monitorlog FROM TABLE lt_zrest_monitorlog.
*
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(55) WITH 'ZREST_MONITORLOG delete'(006)
                            'Return code:'(007)
                             sy-subrc.
    ENDIF.
*
    LOOP AT lt_zrest_monitorlog INTO lw_zrest_monitorlog.
      WRITE: / 'Orphan ZREST_MONITORLOG'(010), lw_zrest_monitorlog-zmessageid, 'deleted'(011).
      ADD 1 TO lv_count.
    ENDLOOP.

    MESSAGE s000(55) WITH lv_count 'Orphans managed'(012).

  ENDIF.

ENDFORM.
