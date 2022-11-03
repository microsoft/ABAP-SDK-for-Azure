************************************************************************
* Program Title: Z_SD_TABLE_ARCHIVE                                    *
* Created by   : Ashutosh Pattanaik                Date: 01/02/2019    *
*                                                                      *
* Description  : Archive SD tables                                     *
* Module name  : SD                                                    *
*                                                                      *
*&---------------------------------------------------------------------*
*                       Modification History                           *
*&---------------------------------------------------------------------*
* Date      | USER ID  |  VSTF   | Transport  | Remarks                *
*&---------------------------------------------------------------------*
*01/02/2019 | V-ASPATT | 4138913 | MS2K977228 | Initial Version        *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_INIT
*&---------------------------------------------------------------------*
*       Initilization
*----------------------------------------------------------------------*
FORM f_init .

  CONCATENATE icon_variants 'Check Variants'(016)
               INTO s_var SEPARATED BY space.

  gv_strtdt = sy-datum.
ENDFORM.                    " F_INIT

*&---------------------------------------------------------------------*
*&      Form  F_SCREEN
*&---------------------------------------------------------------------*
*       Screen change
*----------------------------------------------------------------------*
FORM f_screen .

  DATA lw_screen TYPE screen.

  LOOP AT SCREEN INTO lw_screen.
    CASE lw_screen-name.
      WHEN  'P_DTFLD'  OR 'P_RETEN' OR
            'P_FIELD1' OR 'P_VAL1'  OR
            'P_FIELD2' OR 'P_VAL2'  OR
            'P_FIELD3' OR 'P_VAL3'  OR
            'P_FIELD4' OR 'P_VAL4'  OR
            'P_FIELD5' OR 'P_VAL5'.

        lw_screen-input = 2.
        MODIFY SCREEN FROM lw_screen.

      WHEN OTHERS.
*        do nothing
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " F_SCREEN

*&---------------------------------------------------------------------*
*&      Form  F_LIST_TABLE
*&---------------------------------------------------------------------*
*       Table List Box
*----------------------------------------------------------------------*
FORM f_list_table .

  DATA lt_list TYPE vrm_values.
  DATA lv_name TYPE vrm_id.

  FIELD-SYMBOLS <lf_archtab> TYPE zsd_archtab.
  FIELD-SYMBOLS <lf_list> TYPE vrm_value.

  SELECT * FROM zsd_archtab INTO TABLE gt_archtab.
  IF sy-subrc IS INITIAL.

    LOOP AT gt_archtab ASSIGNING <lf_archtab>.
      APPEND INITIAL LINE TO lt_list ASSIGNING <lf_list>.
      <lf_list>-key   = <lf_archtab>-tabname.
      <lf_list>-text  = <lf_archtab>-tabname.
    ENDLOOP.
  ENDIF.

  SORT lt_list BY key.
  DELETE ADJACENT DUPLICATES FROM lt_list COMPARING key.
  lv_name = 'P_TABLE'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = lv_name
      values          = lt_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE text-021 TYPE 'E'.  "Error in builidng list box
  ENDIF.


ENDFORM.                    " F_LIST_TABLE

*&---------------------------------------------------------------------*
*&      Form  F_LIST_VARIANT
*&---------------------------------------------------------------------*
*       Build Variant Lst Box
*----------------------------------------------------------------------*
FORM f_list_variant .

  DATA lt_list TYPE vrm_values.
  DATA lv_name TYPE vrm_id.

  FIELD-SYMBOLS <lf_archtab> TYPE zsd_archtab.
  FIELD-SYMBOLS <lf_list> TYPE vrm_value.

  CLEAR lt_list.
  LOOP AT gt_archtab ASSIGNING <lf_archtab> WHERE tabname EQ p_table.

    APPEND INITIAL LINE TO lt_list ASSIGNING <lf_list>.
    <lf_list>-key   = <lf_list>-text = <lf_archtab>-variant.
  ENDLOOP.

  lv_name = 'P_VARI'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = lv_name
      values          = lt_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE text-021 TYPE 'E'.  "Error in builidng list box
  ENDIF.

ENDFORM.                    " F_LIST_VARIANT

*&---------------------------------------------------------------------*
*&      Form  F_DISP_VARIANT
*&---------------------------------------------------------------------*
*       Display Variant
*----------------------------------------------------------------------*
FORM f_disp_variant.

  IF sy-ucomm EQ 'SCR'.
    CALL SELECTION-SCREEN '0100' STARTING AT 10 10.
  ENDIF.

  PERFORM f_populate_field.

ENDFORM.                    " F_DISP_VARIANT

*&---------------------------------------------------------------------*
*&      Form  F_POPULATE_FIELD
*&---------------------------------------------------------------------*
*       Populate other fields
*----------------------------------------------------------------------*
FORM f_populate_field .
  FIELD-SYMBOLS <lf_archtab> TYPE zsd_archtab.

  READ TABLE gt_archtab ASSIGNING <lf_archtab>
      WITH KEY tabname = p_table variant = p_vari.

  IF sy-subrc IS INITIAL.
    p_dtfld   = <lf_archtab>-datefield.
    p_reten   = <lf_archtab>-retention.

    p_field1  = <lf_archtab>-fieldname1.
    p_val1    = <lf_archtab>-fieldval1.

    p_field2  = <lf_archtab>-fieldname2.
    p_val2    = <lf_archtab>-fieldval2.

    p_field3  = <lf_archtab>-fieldname3.
    p_val3    = <lf_archtab>-fieldval3.

    p_field4  = <lf_archtab>-fieldname4.
    p_val4    = <lf_archtab>-fieldval4.

    p_field5  = <lf_archtab>-fieldname5.
    p_val5    = <lf_archtab>-fieldval5.

  ELSE.
    CLEAR:  p_dtfld, p_reten,
            p_field1, p_field2, p_field3, p_field4, p_field5,
            p_val1, p_val2, p_val3, p_val4,  p_val5.
  ENDIF.
ENDFORM.                    " F_POPULATE_FIELD

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_SELECTION
*&---------------------------------------------------------------------*
*       Validate Selection Variant
*----------------------------------------------------------------------*
FORM f_validate_selection .

  READ TABLE gt_archtab TRANSPORTING NO FIELDS
    WITH KEY tabname = p_table variant = p_vari.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Error in reading Config data'(023) TYPE 'E'.
  ENDIF.
ENDFORM.                    " F_VALIDATE_SELECTION

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_LOG
*&---------------------------------------------------------------------*
*       Create App Log
*----------------------------------------------------------------------*
FORM f_create_log .

  CHECK p_test IS INITIAL.

  CLEAR gw_log.
  gw_log-object     = 'ZSD_TABLE_ARCHIVE'.
  gw_log-subobject  = 'JOBLOG'.
  gw_log-aldate     = sy-datum.
  gw_log-altime     = sy-uzeit.
  gw_log-aluser     = sy-uname.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = gw_log
    IMPORTING
      e_log_handle            = gw_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Error in creating application log'(024) TYPE 'E'.
  ENDIF.

  gw_msg-msgv1 = 'Table name -'(030).
  gw_msg-msgv2 = p_table.
  PERFORM f_log_add.

  gw_msg-msgv1 = 'Variant name -'(031).
  gw_msg-msgv2 = p_vari.
  PERFORM f_log_add.

  IF p_over IS NOT INITIAL.
    gw_msg-msgv1 = 'Overwrite retention period -'(032).
    gw_msg-msgv2 = p_oreten.
    PERFORM f_log_add.
  ENDIF.

ENDFORM.                    " F_CREATE_LOG

*&---------------------------------------------------------------------*
*&      Form  F_GET_JOBINFO
*&---------------------------------------------------------------------*
*       Get Background Job info
*----------------------------------------------------------------------*
FORM f_get_jobinfo .

  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      jobcount        = gv_jobcount
      jobname         = gv_jobname
      stepcount       = gv_stepcount
    EXCEPTIONS
      no_runtime_info = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Error on getting Job info'(022) TYPE 'E'.
  ENDIF.

  gw_msg-msgv1 = 'Job Name -'(027).
  gw_msg-msgv2 = gv_jobname.
  PERFORM f_log_add.

  gw_msg-msgv1 = 'Job Count -'(028).
  gw_msg-msgv2 = gv_jobcount.
  PERFORM f_log_add.

  gw_msg-msgv1 = 'Step Count -'(029).
  gw_msg-msgv2 = gv_stepcount.
  PERFORM f_log_add.

ENDFORM.                    " F_GET_JOBINFO

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       Get table data to be deleted
*----------------------------------------------------------------------*
FORM f_get_data .

  DATA t_ref      TYPE REF TO data.
  DATA t_ref_all  TYPE REF TO data.

  DATA lv_cnt     TYPE i.
  DATA cur1       TYPE cursor.
  DATA lv_pack    TYPE i.

* //Build the dynamic table
  CREATE DATA t_ref TYPE TABLE OF (p_table).
  ASSIGN t_ref->* TO <gf_table>.

  CREATE DATA t_ref_all TYPE TABLE OF (p_table).
  ASSIGN t_ref_all->* TO <gf_table_all>.

  MESSAGE 'Extract Table data'(013) TYPE 'S'.

* //test mode - collect total count to display
  IF p_test EQ abap_true.
    SELECT COUNT( * ) INTO gv_ext_cnt
      FROM (p_table)
      WHERE (gv_where).

* //Select table data with defined package and delete
  ELSE.
    OPEN CURSOR WITH HOLD cur1 FOR
    SELECT * FROM (p_table)
      WHERE (gv_where).

    DO.
      FETCH NEXT CURSOR cur1 INTO TABLE <gf_table> PACKAGE SIZE p_pack.
      IF sy-subrc IS INITIAL.

*     //collect total record and package
        DESCRIBE TABLE <gf_table> LINES lv_cnt.
        gv_ext_cnt = gv_ext_cnt + lv_cnt.
        lv_pack = lv_pack + 1.

        IF p_down IS NOT INITIAL.
          APPEND LINES OF <gf_table> TO <gf_table_all>.
        ENDIF.

        PERFORM f_delete_data.   "Delete table date

      ELSE.
        CLOSE CURSOR cur1.
        EXIT.
      ENDIF.

    ENDDO.
  ENDIF.

ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_DELETE_data
*&---------------------------------------------------------------------*
*       Delete Table data
*----------------------------------------------------------------------*
FORM f_delete_data .

  DATA lv_line TYPE i.

  IF <gf_table> IS NOT INITIAL.

    DELETE (p_table) FROM TABLE <gf_table>.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'DB_COMMIT'.

      DESCRIBE TABLE <gf_table> LINES lv_line.
      MESSAGE s999(zeas) WITH 'Table data deleted'(015) lv_line.
      gv_del_cnt = gv_del_cnt + lv_line.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_DELETE_data

*&---------------------------------------------------------------------*
*&      Form  F_DISP_OUTPUT
*&---------------------------------------------------------------------*
*       Output Display
*----------------------------------------------------------------------*
FORM f_disp_output .

  DATA lv_date TYPE char10.

  WRITE gv_enddt TO lv_date.
  WRITE:/ 'Data extracted up to -'(017), lv_date.
  WRITE:/ 'Total records selected -'(018), gv_ext_cnt.
  WRITE:/ 'Total records deleted -'(019), gv_del_cnt.

ENDFORM.                    " F_DISP_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_WHERE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_build_where .

  DATA lv_date_c  TYPE char8.

  FIELD-SYMBOLS <lf_archtab> TYPE zsd_archtab.

  READ TABLE gt_archtab ASSIGNING <lf_archtab>
    WITH KEY tabname = p_table variant = p_vari.
  IF sy-subrc IS INITIAL.

*   //For overwrite retenction
    IF p_over IS NOT INITIAL.
      gv_enddt = gv_strtdt - p_oreten.

    ELSEIF <lf_archtab>-retention IS NOT INITIAL.
      gv_enddt = gv_strtdt - <lf_archtab>-retention.
    ENDIF.

    lv_date_c   = gv_enddt.

    CONCATENATE <lf_archtab>-datefield 'LE' '''' INTO gv_where SEPARATED BY space.
    CONCATENATE gv_where lv_date_c '''' INTO gv_where.

    IF <lf_archtab>-fieldname1 IS NOT INITIAL.
      CONCATENATE gv_where 'AND' <lf_archtab>-fieldname1 'EQ' '''' INTO gv_where SEPARATED BY space.
      CONCATENATE gv_where <lf_archtab>-fieldval1 '''' INTO gv_where.
    ENDIF.

    IF <lf_archtab>-fieldname2 IS NOT INITIAL.
      CONCATENATE gv_where 'AND' <lf_archtab>-fieldname2 'EQ' '''' INTO gv_where SEPARATED BY space.
      CONCATENATE gv_where <lf_archtab>-fieldval2 '''' INTO gv_where.
    ENDIF.

    IF <lf_archtab>-fieldname3 IS NOT INITIAL.
      CONCATENATE gv_where 'AND' <lf_archtab>-fieldname3 'EQ' '''' INTO gv_where SEPARATED BY space.
      CONCATENATE gv_where <lf_archtab>-fieldval3 '''' INTO gv_where.
    ENDIF.

    IF <lf_archtab>-fieldname4 IS NOT INITIAL.
      CONCATENATE gv_where 'AND' <lf_archtab>-fieldname4 'EQ' '''' INTO gv_where SEPARATED BY space.
      CONCATENATE gv_where <lf_archtab>-fieldval4 '''' INTO gv_where.
    ENDIF.

    IF <lf_archtab>-fieldname5 IS NOT INITIAL.
      CONCATENATE gv_where 'AND' <lf_archtab>-fieldname5 'EQ' '''' INTO gv_where SEPARATED BY space.
      CONCATENATE gv_where <lf_archtab>-fieldval5 '''' INTO gv_where.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_BUILD_WHERE

*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD
*&---------------------------------------------------------------------*
*       Download file
*----------------------------------------------------------------------*
FORM f_download .

  DATA lcl_file_handler TYPE REF TO zcl_file_handler.
  DATA lt_file_header   TYPE REF TO data.
  DATA lt_file_content  TYPE REF TO data.
  DATA lv_filename      TYPE string.
  DATA lv_fileshare     TYPE string.

  DATA lt_header TYPE STANDARD TABLE OF string.
  FIELD-SYMBOLS <lf_header> TYPE string.

  FIELD-SYMBOLS <lf_data> TYPE ANY TABLE.

  DATA lt_dd03l TYPE STANDARD TABLE OF dd03l.
  FIELD-SYMBOLS <lf_dd03l> TYPE dd03l.

  SELECT * FROM dd03l INTO TABLE lt_dd03l WHERE tabname = p_table.
  IF sy-subrc IS INITIAL.
    SORT lt_dd03l BY position.
  ENDIF.

  LOOP AT lt_dd03l ASSIGNING <lf_dd03l>.

    APPEND INITIAL LINE TO lt_header ASSIGNING <lf_header>.
    <lf_header> = <lf_dd03l>-fieldname.
  ENDLOOP.

  IF lcl_file_handler IS NOT BOUND.
    CREATE OBJECT lcl_file_handler.
  ENDIF.

  CONCATENATE p_table '_' sy-datum sy-uzeit '.txt' INTO lv_filename.
  lv_fileshare = p_path.

  GET REFERENCE OF <gf_table_all> INTO lt_file_content.
  GET REFERENCE OF lt_header INTO lt_file_header.

  CALL METHOD lcl_file_handler->create_file
    EXPORTING
      zin_data                  = lt_file_content
      zin_filename              = lv_filename
      zin_fileshare             = lv_fileshare
      zin_header                = lt_file_header
*     zin_encoding              = 'ANSI'
    EXCEPTIONS
      output_file_open_error    = 1
      no_write_authorization    = 2
      file_already_exist        = 3
      error_in_external_command = 4
      file_does_not_exist       = 5
      invalid_encoding_value    = 6
      no_header_allowed         = 7
      OTHERS                    = 8.

  IF sy-subrc IS INITIAL.
    MESSAGE 'File downloaded Successfully'(008) TYPE 'S'.

    gw_msg-msgv1 = 'File downloaded'(033).
    CONCATENATE lv_fileshare lv_filename INTO gw_msg-msgv2.
    PERFORM f_log_add.

  ELSE.
    MESSAGE 'Error in writing file'(009) TYPE 'E'.
  ENDIF.

ENDFORM.                    " F_DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  f_log_add
*&---------------------------------------------------------------------*
*       Add App Log
*----------------------------------------------------------------------*
FORM f_log_add.

  CHECK p_test IS INITIAL.

  IF gw_msg-msgty IS INITIAL.
    gw_msg-msgty = 'I'.
  ENDIF.

  IF gw_msg-msgid IS INITIAL.
    gw_msg-msgid = 'ZEAS'.
  ENDIF.

  IF gw_msg-msgno IS INITIAL.
    gw_msg-msgno = '999'.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle     = gw_log_handle
      i_s_msg          = gw_msg
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE 'Error in adding Application Log'(025) TYPE 'E'.
  ENDIF.

  CLEAR gw_msg.
ENDFORM.                    "f_log_add

*&---------------------------------------------------------------------*
*&      Form  f_log_save
*&---------------------------------------------------------------------*
*       Save Application Log
*----------------------------------------------------------------------*
FORM f_log_save.

  DATA lt_lgnm        TYPE bal_t_lgnm.
  DATA lt_log_handle  TYPE bal_t_logh.

  CHECK p_test IS INITIAL.

  gw_msg-msgv1 = 'Data extracted up to -'(017).
  gw_msg-msgv2 = gv_enddt.
  PERFORM f_log_add.

  gw_msg-msgv1 = 'Total records selected -'(018).
  gw_msg-msgv2 = gv_ext_cnt.
  PERFORM f_log_add.

  gw_msg-msgv1 = 'Total records deleted -'(019).
  gw_msg-msgv2 = gv_del_cnt.
  PERFORM f_log_add.


*  APPEND gw_log_handle TO lt_log_handle.
  INSERT gw_log_handle INTO lt_log_handle. " VF recommended changes

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = lt_log_handle
    IMPORTING
      e_new_lognumbers = lt_lgnm
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    "Begin Of Change v-srdasgupta ++
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e000(zvf_zrest) WITH DATA(lv_string) raise log_not_found.
        WHEN 2.
         MESSAGE e000(zvf_rest) WITH DATA(lv_string1) raise save_not_allowed.
          WHEN OTHERS.
            WRITE : / e002(zfv_rest).
      ENDCASE.
    "End Of Change v-srdasgupta ++
  ENDIF.

ENDFORM.                    "f_log_save
