*&---------------------------------------------------------------------*
*& Report  ZADF_AZUREOPENAI_DEMO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zadf_azureopenai_dev_demo.

DATA : ls_payload TYPE zadf_azopenai_compopr_req.

TABLES sscrfields.
DATA:
  lt_headers       TYPE tihttpnvp,
  wa_headers       TYPE LINE OF tihttpnvp,
  lv_string        TYPE string,
  lv_response      TYPE string,
  cx_interface     TYPE REF TO zcx_interace_config_missing,
  cx_http          TYPE REF TO zcx_http_client_failed,
  cx_adf_service   TYPE REF TO zcx_adf_service,
  oref_azureopenai TYPE REF TO zcl_adf_service_azureopenai,
  oref             TYPE REF TO zcl_adf_service,
  filter           TYPE zbusinessid,
  lv_http_status   TYPE i,
  lo_json          TYPE REF TO cl_trex_json_serializer.


TYPES : BEGIN OF ty_dumps,
          datum TYPE sydatum,
          uzeit TYPE syuzeit,
          ahost TYPE snap_syinst,
          uname TYPE syuname,
          seqno TYPE sychar03,
          xhold TYPE s380xhold,
          flist TYPE sychar200,
        END OF ty_dumps.

DATA:
  lt_tab_file        TYPE TABLE OF char255,
  lv_length          TYPE i,
  ls_xstring         TYPE xstring,
  ls_xstring1        TYPE xstring,
  lv_base64          TYPE string,
  lv_file_type       TYPE char200,
  lv_filename        TYPE string,
  ls_file_table      TYPE file_table,
  lv_msg             TYPE string,
  lt_file_table      TYPE filetable,
  lv_rc              TYPE i,
  lr_resp            TYPE REF TO data,
  lv_file            TYPE string,
  lv_file_content    TYPE string,
  o_output_container TYPE REF TO cl_gui_custom_container,
  o_ai_output        TYPE REF TO cl_gui_textedit,
  lt_dumps           TYPE STANDARD TABLE OF ty_dumps.


DATA :lv_comp_res  TYPE string.
FIELD-SYMBOLS: <data_tab>       TYPE ANY TABLE.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-003.
PARAMETERS : p_aaireq TYPE char250_d.
PARAMETERS: p_file TYPE rlgrap-filename.
SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
SELECTION-SCREEN: BEGIN OF LINE,
     PUSHBUTTON 2(10)  text-010 USER-COMMAND  cli1,
     PUSHBUTTON 14(12) text-020 USER-COMMAND  cli2,
     PUSHBUTTON 28(10) text-030 USER-COMMAND  cli3,
     PUSHBUTTON 40(15) text-040 USER-COMMAND  cli4,
   END OF LINE.

*   SELECTION-SCREEN: BEGIN OF LINE,
*     PUSHBUTTON 2(10)  text-020  USER-COMMAND cli1,
*     PUSHBUTTON 16(10) text-020 USER-COMMAND cli2,
*     PUSHBUTTON 22(10) but3 USER-COMMAND cli3,
*     PUSHBUTTON 28(10) text-040 USER-COMMAND cli4,
*   END OF LINE.

SELECTION-SCREEN : END OF BLOCK b2.

AT SELECTION-SCREEN.

*  MESSAGE i888(sabapdocu) WITH text-001 sscrfields-ucomm.
  CASE sscrfields-ucomm.
    WHEN 'CLI1'.
      DATA(lv_date) = sy-datum - 2.
      SELECT
               datum,
              uzeit,
              ahost,
              uname,
              seqno,
              xhold,
              flist
              UP TO 10 ROWS
         FROM snap_beg INTO TABLE @lt_dumps WHERE datum = @sy-datum.
    WHEN 'CLI2'.
    WHEN 'CLI3'.
    WHEN 'CLI4'.
  ENDCASE.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

* Upload text File and convert into Internal Table.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select a file'
      default_extension       = '*.txt'
      file_filter             = '*TXT'
      multiselection          = abap_false
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc = 0.
    READ TABLE lt_file_table INTO ls_file_table INDEX 1.
    IF sy-subrc IS INITIAL.
      p_file = ls_file_table-filename.
      lv_filename = ls_file_table-filename.
    ENDIF.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.

    MESSAGE lv_msg TYPE 'I'.
  ENDIF.

START-OF-SELECTION.

  IF lv_filename IS INITIAL.
    lv_filename = p_file.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_filename
      filetype                = 'ASC'
    IMPORTING
      filelength              = lv_length
    CHANGING
      data_tab                = lt_tab_file
    EXCEPTIONS
      file_open_error         = 01
      file_read_error         = 02
      no_batch                = 03
      gui_refuse_filetransfer = 04
      invalid_type            = 05
      no_authority            = 06
      unknown_error           = 07
      bad_data_format         = 08
      header_not_allowed      = 09
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18.
  IF sy-subrc = 0.

* Convert file content into string
    LOOP AT lt_tab_file INTO lv_file.
      CONCATENATE lv_file_content lv_file  cl_abap_char_utilities=>newline  INTO lv_file_content.
    ENDLOOP.

    CONDENSE lv_file_content.
  ENDIF.

  IF  lt_dumps IS NOT INITIAL.
    ls_payload-max_tokens = 3000.
    CONCATENATE LINES OF lt_dumps INTO lv_file_content SEPARATED BY cl_abap_char_utilities=>newline.
*    LOOP AT lt_dumps INTO lv_file.
*      CONCATENATE lv_file_content lv_file  cl_abap_char_utilities=>newline  INTO lv_file_content.
*    ENDLOOP.
  ENDIF.

  TRY.
**Calling Factory method to instantiate AZUREOPENAI client
      oref = zcl_adf_service_factory=>create( iv_interface_id = 'ZVIK_AZAI'
                                              iv_business_identifier = filter ).
      oref_azureopenai ?= oref.

      CONCATENATE p_aaireq cl_abap_char_utilities=>newline lv_file_content INTO lv_file_content.

      ls_payload-prompt  = lv_file_content.


      oref_azureopenai->set_compopr_req_body( EXPORTING im_azopenai_reqbody = ls_payload
                                              RECEIVING rv_xstring      = DATA(lv_pxstring) ).

**Sending Converted SAP data to Azure AZUREOPENAI
      CALL METHOD oref_azureopenai->send
        EXPORTING
          request        = lv_pxstring  "Input XSTRING of Prompt
          it_headers     = lt_headers  "Header attributes
        IMPORTING
          response       = lv_response       "Response from AZUREOPENAI
          ev_http_status = lv_http_status.   "Status

      /ui2/cl_json=>deserialize(
       EXPORTING
          json             =  lv_response
        CHANGING
          data           =  lr_resp ).

      IF lr_resp IS BOUND.
        ASSIGN lr_resp->* TO FIELD-SYMBOL(<resp>).
        ASSIGN COMPONENT 'CHOICES' OF STRUCTURE <resp> TO FIELD-SYMBOL(<results>).
        IF <results> IS ASSIGNED.
          ASSIGN <results>->* TO <data_tab>.
          LOOP AT <data_tab> ASSIGNING FIELD-SYMBOL(<structure>).
            ASSIGN <structure>->* TO <resp>.
            ASSIGN COMPONENT 'TEXT' OF STRUCTURE <resp> TO FIELD-SYMBOL(<field>).
            IF <field> IS ASSIGNED.
              ASSIGN <field>->* TO FIELD-SYMBOL(<resp1>).
            ENDIF.
            UNASSIGN: <field>.
          ENDLOOP.
        ELSE.
          ASSIGN COMPONENT 'ERROR' OF STRUCTURE <resp> TO <results>.
          ASSIGN <results>->* TO FIELD-SYMBOL(<data>).
          IF <results> IS ASSIGNED .
            ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <data> TO <field>.
            ASSIGN <field>->* TO <resp1>.
          ENDIF.
        ENDIF.

        lv_file_content = <resp1>.

        CALL SCREEN 9001 STARTING AT 10 50  ENDING AT 100 150.

*        cl_demo_output=>write( <resp1> ).
*        cl_demo_output=>display( ).

        CLEAR : lv_file_content,lt_tab_file.
        FREE : oref_azureopenai,oref.
      ENDIF.

    CATCH zcx_interace_config_missing INTO cx_interface.
      lv_string = cx_interface->get_text( ).
      MESSAGE lv_string TYPE 'E'.
    CATCH zcx_http_client_failed INTO cx_http .
      lv_string = cx_http->get_text( ).
      MESSAGE lv_string TYPE 'E'.
    CATCH zcx_adf_service INTO cx_adf_service.
      lv_string = cx_adf_service->get_text( ).
      MESSAGE lv_string TYPE 'E'.

  ENDTRY.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  IF  sy-ucomm =  'E' OR sy-ucomm = 'ECAN' OR sy-ucomm = 'ONLI' OR sy-ucomm = 'F7'.
    LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS '1000'.
*  SET TITLEBAR 'xxx'.

  CREATE OBJECT o_output_container
    EXPORTING
      container_name              = 'AIOUTPUT'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  CREATE OBJECT o_ai_output
    EXPORTING
      parent                     = o_output_container
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true
    EXCEPTIONS
      OTHERS                     = 1.

  CALL METHOD o_ai_output->set_textstream(
    EXPORTING
      text = lv_file_content ).

ENDMODULE.
