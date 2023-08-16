class ZCL_REST_UTILITY_CLASS definition
  public
  final
  create public .

public section.

  constants GC_I type CHAR1 value 'I' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods DOWNLOAD_PAYLOAD_FILE
    importing
      !XSTRING type XSTRING
      !MESSAGE_ID type ZMID
    raising
      ZCX_HTTP_CLIENT_FAILED .
  methods RETRY
    importing
      !MESSAGE_ID type ZMID
      !METHOD type CHAR20
      !FROM_SCHEDULER type CHAR1 optional
    raising
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  class-methods GET_HTTP_DESCRIPTION
    importing
      !CODE type INT4
    returning
      value(DESCRIPTION) type STRING .
  class-methods SHOW_PAYLOAD
    importing
      !MESSAGE_ID type ZMID
      !RESPONSE type C optional
    raising
      ZCX_HTTP_CLIENT_FAILED .
  class-methods GET_CONFIG_DATA
    importing
      !INTERFACE_ID type ZINTERFACE_ID
      !METHOD type CHAR20
    returning
      value(CONFIG_DATA) type ZREST_SRTUCT_CONFIG .
  class-methods GET_STATIC_HEADERS
    importing
      !INTERFACE_ID type ZINTERFACE_ID
    returning
      value(STATIC_HEADERS) type TIHTTPNVP .
  class-methods SHOW_SUBMITTED_HEADERS
    importing
      !MESSAGE_ID type ZMID
    raising
      ZCX_HTTP_CLIENT_FAILED .
  class-methods UNPROCESSED_DATA
    importing
      !INTERFACE_ID type ZTT_INTERFACE_ID optional
    returning
      value(RESULT) type ZRT_PAYLOAD .
  class-methods RESET_ALL_DATA
    importing
      !INTERFACE_ID type ZTT_INTERFACE_ID optional
    raising
      ZCX_HTTP_CLIENT_FAILED .
  class-methods CHECK_OBFUSCATION_NEEDED
    importing
      !INETRFACE_IN type ZINTERFACE_ID
    returning
      value(RESULT) type ABAP_BOOL .
  methods RETRY_LIMIT_EXCEEDED
    exporting
      !ET_RETRY_REPORT type ZTT_REST_RETRY_LIMI .
  methods SEND_EMAIL
    importing
      !IV_RECEPIENT type AD_SMTPADR
      !IV_SUBJECT type SO_OBJ_DES
      !IT_BODY type SOLI_TAB
    exporting
      !EW_RETURN type BAPIRET2
      !EV_RESULT type OS_BOOLEAN .
  class-methods CHECK_AUTHORITY
    raising
      ZCX_HTTP_CLIENT_FAILED .
  class-methods GET_GLOBAL_PARAMS
    returning
      value(GLOBAL_PARAMS) type ZREST_GLOBAL .
  class-methods RETRY_LOG
    importing
      !MESSAGE_ID type ZMID
      !RESPONSE type C optional
    raising
      ZCX_HTTP_CLIENT_FAILED .
  class-methods WRITE_APPLICATION_LOG
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
      !IV_EXTNUMBER type BALNREXT
      !IT_MESSAGE type ZRT_APPLOG_MESSAGE .
protected section.
private section.

  data REST_HANDLER type ref to ZCL_REST_FRAMEWORK .
  data HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  data RESPONSE type ref to IF_REST_ENTITY .
  data REQUEST type ref to IF_REST_ENTITY .
  data GV_CLIENT type STRING .
  data GV_BODY type STRING .
  class-data PAYLOAD type ZRT_PAYLOAD .
  class-data MONITOR type ZREST_MONITOR .
  class-data METHODNAME type CHAR5 .
  class-data INTERFACE_NAME type ZINTERFACE_ID .
  class-data IT_ZOBFUSCATE type ZTOBFUSCATE .
  constants GC_TYPE_AAD type STRING value 'type=aad' ##NO_TEXT.
  constants GC_TYPE type STRING value 'type=master' ##NO_TEXT.
  constants GC_VERSION type STRING value 'ver=1.0' ##NO_TEXT.
  constants GC_SEPARATOR type STRING value '&' ##NO_TEXT.
  constants GC_SIG type STRING value 'sig=' ##NO_TEXT.

  class-methods GET_DB_DATA
    importing
      !MESSAGE_ID type ZMID .
  class-methods CHECK_MESSAGEID
    importing
      !MESSAGE_ID type ZMID .
  class-methods OBFUSCATE
    importing
      !INPUT type XSTRING
    returning
      value(RESULT) type XSTRING .
ENDCLASS.



CLASS ZCL_REST_UTILITY_CLASS IMPLEMENTATION.


  METHOD check_authority.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04|28|2016|V-DEVEER  |2163894 | DGDK903413 | Authorization Check
*----------------------------------------------------------------------*


* Added for VSTF# 2163894 | DGDK903413
    DATA:lv_textid TYPE scx_t100key.
*    Uncomment when needed
    AUTHORITY-CHECK OBJECT 'ZREST_AUTH'
    ID 'ZBOOLEAN' FIELD 'X'.
    IF sy-subrc NE 0.
      lv_textid-msgid = 'Z_FI_MDG'.
      lv_textid-msgno = '057'.
      RAISE EXCEPTION TYPE zcx_http_client_failed EXPORTING textid = lv_textid.
    ENDIF.
* End of changes for 2163894 | DGDK903413

  ENDMETHOD.


  method CHECK_MESSAGEID.
    IF message_id IS INITIAL .
      EXIT.
    ENDIF.
  endmethod.


  METHOD check_obfuscation_needed.
    SELECT  * FROM zobfuscate INTO table it_zobfuscate
                                    WHERE inetrface EQ inetrface_in.
    IF sy-subrc EQ 0.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  method CONSTRUCTOR.

  endmethod.


  METHOD download_payload_file.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04|28|2016|V-DEVEER  |2163894 | DGDK903413 | Authorization Check     *
* 09|29|2022|V-ASHOKM1 |        | SMTK907895 | Fixing VF Errors        *
*----------------------------------------------------------------------*
    TYPES : BEGIN OF ty_string,
              line TYPE string,
            END OF ty_string.

    DATA : wa_payload TYPE zrest_mo_payload,
           wa_string  TYPE ty_string,
           it_string  TYPE STANDARD TABLE OF ty_string,
           output     TYPE xstring.

*   Check if user has the authority to call.
*   Begin of changes for VSTF# 2163894 | DGDK903413
*    TRY.
*        check_authority( ).
*      CATCH zcx_http_client_failed INTO DATA(lv_textid).
*        DATA(lv_text2) = lv_textid->if_t100_message~t100key.
*        RAISE EXCEPTION TYPE zcx_http_client_failed EXPORTING textid = lv_text2.
*    ENDTRY.
*   End of changes for VSTF 2163894 | DGDK903413

    SELECT SINGLE * FROM zrest_mo_payload INTO wa_payload WHERE messageid EQ message_id.
    IF sy-subrc EQ 0.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'ZREST_AUTH'
        ID 'ZBOOLEAN' FIELD 'X'.
    IF sy-subrc NE 0.
      IF check_obfuscation_needed( inetrface_in = wa_payload-interface_id ) EQ abap_true.
*        DATA(output) = obfuscate( input = wa_payload-payload ).  v-jobpau
        output = obfuscate( input = wa_payload-payload ).
        wa_payload-payload = output.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring  = wa_payload-payload
        im_encoding = 'UTF-8'
      IMPORTING
        ex_string   = wa_string-line.

    APPEND wa_string TO it_string.
    CLEAR wa_string.

    DATA gv_filename TYPE string.
    DELETE it_string WHERE line IS INITIAL.
    IF it_string IS INITIAL.
      MESSAGE 'Payload not found' TYPE 'I'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'GUI_FILE_SAVE_DIALOG'
*     EXPORTING
*       WINDOW_TITLE            =
*       DEFAULT_EXTENSION       =
*       DEFAULT_FILE_NAME       =
*       WITH_ENCODING           =
*       FILE_FILTER             =
*       INITIAL_DIRECTORY       =
*       DEFAULT_ENCODING        =
      IMPORTING
*       FILENAME =
*       PATH     =
        fullpath = gv_filename
*       USER_ACTION             =
*       FILE_ENCODING           =
      .

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
*       BIN_FILESIZE            =
        filename                = gv_filename
        filetype                = 'ASC'
*       APPEND                  = ' '
*       WRITE_FIELD_SEPARATOR   = ' '
*       HEADER                  = '00'
*       TRUNC_TRAILING_BLANKS   = ' '
*       WRITE_LF                = 'X'
*       COL_SELECT              = ' '
*       COL_SELECT_MASK         = ' '
*       DAT_MODE                = ' '
*       CONFIRM_OVERWRITE       = ' '
*       NO_AUTH_CHECK           = ' '
*       CODEPAGE                = ' '
*       IGNORE_CERR             = ABAP_TRUE
*       REPLACEMENT             = '#'
*       WRITE_BOM               = ' '
*       TRUNC_TRAILING_BLANKS_EOL       = 'X'
*       WK1_N_FORMAT            = ' '
*       WK1_N_SIZE              = ' '
*       WK1_T_FORMAT            = ' '
*       WK1_T_SIZE              = ' '
*       WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*       SHOW_TRANSFER_STATUS    = ABAP_TRUE
*       VIRUS_SCAN_PROFILE      = '/SCET/GUI_DOWNLOAD'
* IMPORTING
*       FILELENGTH              =
      TABLES
        data_tab                = it_string
*       FIELDNAMES              =
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.

      MESSAGE e005(zvf_zrest). " Error while downloading the file " Added by V-ASHOKM1 for Fixing VF Errors  SMTK907895

    ENDIF.

  ENDMETHOD.


  METHOD get_config_data.
    SELECT SINGLE * FROM zrest_config INTO CORRESPONDING FIELDS OF config_data WHERE interface_id EQ interface_id. " and method eq method.
    SELECT SINGLE method max_retry
      INTO (config_data-method, config_data-max_retry)
      FROM zrest_conf_misc
      WHERE interface_id EQ interface_id AND method EQ method.
  ENDMETHOD.


  METHOD get_db_data.
    CALL METHOD z_restcall_from_db=>get_calldata_fromdb
      EXPORTING
        message_id = message_id
      IMPORTING
        payload    = payload
        monitor    = monitor.
  ENDMETHOD.


  METHOD get_global_params.
    DATA it_global TYPE TABLE OF zrest_global.
    SELECT * FROM zrest_global INTO TABLE it_global.
    READ TABLE it_global INTO global_params INDEX 1.
  ENDMETHOD.


  METHOD get_http_description.
    CASE code.
      WHEN '100'. description = 'Continue'.
      WHEN '101'. description = 'Switching Protocols'.
      WHEN '200'. description = 'OK'.
      WHEN '201'. description = 'Created'.
      WHEN '202'. description = 'Accepted'.
      WHEN '203'. description = 'Non-Authoritative Information'.
      WHEN '204'. description = 'No Content'.
      WHEN '205'. description = 'Reset Content'.
      WHEN '206'. description = 'Partial Content'.
      WHEN '300'. description = 'Multiple Choices'.
      WHEN '301'. description = 'Moved Permanently'.
      WHEN '302'. description = 'Found'.
      WHEN '303'. description = 'See Other'.
      WHEN '304'. description = 'Not Modified'.
      WHEN '305'. description = 'Use Proxy'.
      WHEN '307'. description = 'Temporary Redirect'.
      WHEN '400'. description = 'Bad Request'.
      WHEN '401'. description = 'Unauthorized'.
      WHEN '402'. description = 'Payment Required'.
      WHEN '403'. description = 'Forbidden'.
      WHEN '404'. description = 'Not Found'.
      WHEN '405'. description = 'Method Not Allowed'.
      WHEN '406'. description = 'Not Acceptable'.
      WHEN '407'. description = 'Proxy Authentication Required'.
      WHEN '408'. description = 'Request Timeout'.
      WHEN '409'. description = 'Conflict'.
      WHEN '410'. description = 'Gone'.
      WHEN '411'. description = 'Length Required'.
      WHEN '412'. description = 'Precondition Failed'.
      WHEN '413'. description = 'Request Entity Too Large'.
      WHEN '414'. description = 'Request-URI Too Long'.
      WHEN '415'. description = 'Unsupported Media Type'.
      WHEN '416'. description = 'Requested Range Not Satisfiable'.
      WHEN '417'. description = 'Expectation Failed'.
      WHEN '500'. description = 'Internal Server Error'.
      WHEN '501'. description = 'Not Implemented'.
      WHEN '502'. description = 'Bad Gateway'.
      WHEN '503'. description = 'Service Unavailable'.
      WHEN '504'. description = 'Gateway Timeout'.
      WHEN '505'. description = 'HTTP Version Not Supported'.

      WHEN'505'.
      WHEN OTHERS.
        description = ''.
    ENDCASE.

  ENDMETHOD.


  METHOD get_static_headers.
    DATA: wa_ihttpnvp TYPE ihttpnvp,
          lt_headers type TABLE OF  zrest_conf_head,
          wa_headers type zrest_conf_head
    .
*    SELECT * FROM zrest_conf_head INTO TABLE @DATA(lt_headers) WHERE interface_id EQ @interface_id. v-jobpau
    SELECT * FROM zrest_conf_head INTO TABLE lt_headers WHERE interface_id EQ interface_id.
    IF sy-subrc EQ 0.
*      LOOP AT lt_headers INTO DATA(wa_headers). v-jobpau
      LOOP AT lt_headers INTO wa_headers.
        wa_ihttpnvp-name = wa_headers-name.
        wa_ihttpnvp-value = wa_headers-value.
        append wa_ihttpnvp to static_headers.
        clear  wa_ihttpnvp.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD obfuscate.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 09|29|2022|V-ASHOKM1 |        | SMTK907895 | Fixing VF Errors        *
*----------------------------------------------------------------------*
************************************************************************************
    CONSTANTS : c_obfuscating_symbol TYPE c VALUE '*'.
    FIELD-SYMBOLS : <fs>           TYPE any,
                    <fchar_string> TYPE any.
    DATA : converted        TYPE REF TO cl_abap_conv_in_ce,
           regex            TYPE REF TO cl_abap_regex,
           matcher          TYPE REF TO cl_abap_matcher,
           obfuscate_word   TYPE REF TO data,
           lchar_string     TYPE REF TO data,
           regex_result_tab TYPE        match_result_tab,
           regex_result     TYPE        match_result,
           st_obfuscate     LIKE LINE OF it_zobfuscate,
           lv_string        TYPE string,
           fieldtag(50)     TYPE c,
           length_of_word   TYPE i.
************************************************************************************
*   Convert to string                                                              *
************************************************************************************
* Check if the obfuscation is needed and input is not initial
    CHECK : it_zobfuscate IS NOT INITIAL ,
            input IS NOT INITIAL.
*   if yes , convert the xstring to string for the processing
    TRY.
        CALL METHOD cl_abap_conv_in_ce=>create
          EXPORTING
*           encoding    = 'DEFAULT'
*           endian      =
*           replacement = '#'
*           ignore_cerr = ABAP_FALSE
            input = input
          RECEIVING
            conv  = converted.
*--Begin of changes by V-ASHOKM1 ++  SMTK907895
*     CATCH cx_parameter_invalid_range .       Commented by V-ASHOKM1 SMTK907895
      CATCH cx_parameter_invalid_range INTO DATA(lr_invrange).
        DATA(lv_text) = lr_invrange->get_text( ).
        IF lv_text IS NOT INITIAL.
          MESSAGE lv_text TYPE 'E'.
        ENDIF.
*      CATCH cx_sy_codepage_converter_init .   Commented by V-ASHOKM1 SMTK907895
      CATCH cx_sy_codepage_converter_init INTO DATA(lr_converter).
        DATA(lv_text1) = lr_converter->get_text( ).
        IF lv_text1 IS NOT INITIAL.
          MESSAGE lv_text1 TYPE 'E'.
        ENDIF.
*  End of changes by V-ASHOKM1 ++ SMTK907895
    ENDTRY.
    TRY.
        CALL METHOD converted->read
          IMPORTING
            data = lv_string.

*--Begin of changes by V-ASHOKM1 ++ SMTK907895
      CATCH cx_sy_conversion_codepage INTO DATA(lr_codepage) .
        DATA(lv_text2) = lr_codepage->get_text( ).
        IF lv_text2 IS NOT INITIAL.
          MESSAGE lv_text2 TYPE 'E'.
        ENDIF.

*      CATCH cx_sy_codepage_converter_init .     Commented by V-ASHOKM1 SMTK907895
      CATCH cx_sy_codepage_converter_init INTO lr_converter.
        lv_text1 = lr_converter->get_text( ).
        IF lv_text1 IS NOT INITIAL.
          MESSAGE lv_text1 TYPE 'E'.
        ENDIF.

*      CATCH cx_parameter_invalid_type.          Commented by V-ASHOKM1 SMTK907895
      CATCH cx_parameter_invalid_type  INTO DATA(lr_invtype) .
        DATA(lv_text3) = lr_invtype->get_text( ).
        IF lv_text3 IS NOT INITIAL.
          MESSAGE lv_text3 TYPE 'E'.
        ENDIF..

*      CATCH cx_parameter_invalid_range . Commented by V-ASHOKM1 SMTK907895
      CATCH cx_parameter_invalid_range INTO lr_invrange.
        lv_text = lr_invrange->get_text( ).
        IF lv_text IS NOT INITIAL.
          MESSAGE lv_text TYPE 'E'.
        ENDIF.
*  End of changes by V-ASHOKM1 ++ SMTK907895
    ENDTRY.
************************************************************************************
*   Loop at the fields which needed to be obfuscated                               *
************************************************************************************
    LOOP AT it_zobfuscate INTO st_obfuscate.
      CREATE OBJECT : regex EXPORTING pattern = st_obfuscate-fieldtag ignore_case = abap_true,
                      matcher EXPORTING regex = regex text = lv_string.
      CREATE DATA obfuscate_word TYPE c LENGTH st_obfuscate-length.
      length_of_word = strlen( lv_string ).
      CREATE DATA lchar_string TYPE c LENGTH length_of_word.
      regex_result_tab = matcher->find_all( ).
      length_of_word =  strlen( st_obfuscate-fieldtag ).
      ASSIGN obfuscate_word->* TO <fs>.
      ASSIGN lchar_string->* TO <fchar_string>.
      <fchar_string> = lv_string.

      DO st_obfuscate-length TIMES.
        sy-index = sy-index - 1.
        <fs>+sy-index(1) = '*'.
      ENDDO.
*     Repace the words in all string
      LOOP AT regex_result_tab INTO regex_result.
        length_of_word =  strlen( st_obfuscate-fieldtag ).
        length_of_word = length_of_word + regex_result-offset.
        <fchar_string>+length_of_word(st_obfuscate-length) = <fs> .
      ENDLOOP.
      CLEAR : st_obfuscate,length_of_word,regex_result_tab,regex_result.
      lv_string = <fchar_string>.
      UNASSIGN : <fs> , <fchar_string>.
    ENDLOOP.
************************************************************************************
*   Make it string                                                                 *
************************************************************************************
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_string
*       MIMETYPE       = ' '
*       ENCODING       =
      IMPORTING
        buffer = result
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
* Begin of Changes by V-ASHOKM1 ++ SMTK907859
      MESSAGE e003(zvf_zrest). " XSTRING Conversion error
* End of Changes by V-ASHOKM1 ++ SMTK907859
    ENDIF.


  ENDMETHOD.


  METHOD reset_all_data.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04|28|2016|V-DEVEER  |2163894 | DGDK903413 | Authorization Check     *
* 03|13|2019|V-SRKURA  |4364787 | DGDK910813 | REST Framework Changes  *
* 09|28|2022|V-ASHOKM1 |        | SMTK907895 | Fixing VF Errors        *
*----------------------------------------------------------------------*
**************************************************************************************
    DATA  : it_monitor TYPE STANDARD TABLE OF zrest_monitor,
            it_payload TYPE STANDARD TABLE OF zrest_mo_payload,
            it_headers TYPE STANDARD TABLE OF zrest_mon_header,
            it_global  TYPE STANDARD TABLE OF zrest_global,
            del_date   TYPE sy-datum,
            seconds    TYPE i.
    DATA: lr_interface_id TYPE RANGE OF zinterface_id,   "Added by V-SRKURA 4364787/DGDK910813
          ls_interface_id TYPE zts_interface_id,         "Added by V-SRKURA 4364787/DGDK910813
          l_interface_id  LIKE LINE OF lr_interface_id.  "Added by V-SRKURA 4364787/DGDK910813
**************************************************************************************
* Go back 30 days in time . All the message before this date would need to           *
* be cleanesed.By default 30 days...                                                 *
**************************************************************************************
    DATA(wa_global) = zcl_rest_utility_class=>get_global_params( ).
*    seconds = -1 * 2592000.
*   seconds = -1 * wa_global-message_retention  * 24  * 60.          "Commented by V-SRKURA 4364787/DGDK910813
    seconds = -1 * wa_global-message_retention  * 24  * 60 * 60.     "Added by V-SRKURA 4364787/DGDK910813
    TRY.
        CALL METHOD cl_abap_tstmp=>td_add
          EXPORTING
            date     = sy-datum
            time     = sy-uzeit
            secs     = seconds
          IMPORTING
            res_date = del_date.
*  Begin of changes by V-ASHOKM1 ++ SMTK907895
*      CATCH cx_parameter_invalid_type .  Commented by V-ASHOKM1 SMTK907895
      CATCH cx_parameter_invalid_type INTO DATA(lr_invtype).
        DATA(lv_text) = lr_invtype->get_text( ).
        IF lv_text IS NOT INITIAL.
          MESSAGE lv_text TYPE 'E'.
        ENDIF.

*      CATCH cx_parameter_invalid_range . Commented by V-ASHOKM1 SMTK907895
      CATCH cx_parameter_invalid_range INTO DATA(lr_invrange).
        DATA(lv_text1) = lr_invrange->get_text( ).
        IF lv_text1 IS NOT INITIAL.
          MESSAGE lv_text1 TYPE 'E'.
        ENDIF.
*  End of changes by V-ASHOKM1 ++ SMTK907895
    ENDTRY.
**************************************************************************************
*   Begin of changes VSTF # 2163894 | DGDK903413
    TRY.
        check_authority( ).
      CATCH zcx_http_client_failed INTO DATA(lv_textid).
        DATA(lv_text2) = lv_textid->if_t100_message~t100key.
        RAISE EXCEPTION TYPE zcx_http_client_failed EXPORTING textid = lv_text2.
    ENDTRY.
*  End of changes VSTF # 2163894 | DGDK903413
**************************************************************************************
*--Begin of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
    LOOP AT interface_id INTO ls_interface_id.
      l_interface_id-sign = 'I'.
      l_interface_id-option = 'EQ'.
      l_interface_id-low = ls_interface_id-interface_id.
      APPEND l_interface_id TO lr_interface_id.
    ENDLOOP.
*--End of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
*   SELECT * FROM zrest_monitor INTO TABLE it_monitor                   "Commented by V-SRKURA 4364787/DGDK910813
    SELECT * FROM zrest_monitor PACKAGE SIZE 5000 INTO TABLE it_monitor  "Added by V-SRKURA 4364787/DGDK910813
                                     WHERE zcompdate < del_date
                                     AND   httpstatus BETWEEN 200 AND 300
                                     AND   interface_id IN lr_interface_id. "Added by V-SRKURA 4364787/DGDK910813
      IF ( NOT it_monitor IS INITIAL ) AND ( sy-subrc EQ 0 ). "VF remediations by KRDASH
        SELECT * FROM zrest_mo_payload INTO TABLE it_payload
                              FOR ALL ENTRIES IN it_monitor WHERE messageid EQ it_monitor-zmessageid.
        SELECT * FROM     zrest_mon_header  INTO TABLE it_headers
                              FOR ALL ENTRIES IN it_monitor WHERE messageid EQ it_monitor-zmessageid.
      ENDIF.
**************************************************************************************
*    Delete the data from the table                                                  *
**************************************************************************************
      IF it_monitor IS NOT INITIAL.
        DELETE : zrest_monitor    FROM TABLE it_monitor,
                 zrest_mo_payload FROM TABLE it_payload,
                 zrest_mon_header FROM TABLE  it_headers.
      ENDIF.
      CLEAR: it_monitor,it_payload,it_headers.                           "Added by V-SRKURA 4364787/DGDK910813
    ENDSELECT.                                                         "Added by V-SRKURA 4364787/DGDK910813
  ENDMETHOD.


  METHOD retry.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04|28|2016|V-DEVEER  |2163894 | DGDK903413 | Authorization Check
*----------------------------------------------------------------------*
* 05|05|2016|V-DEVEER  |2163894 | DGDK903444 | SIT Testing issues
*----------------------------------------------------------------------*
* 10|18|2017|V-LAUPPA  |2636456 | DG2K902770 | Performace improvement
*----------------------------------------------------------------------*
* 11|14|2017|V-SANMOH  |3960458 | DG2K904406 | ZREST Retry bug fix
*----------------------------------------------------------------------*
* 03|14|2019|V-SRKURA  |4364787 | DGDK910813 | REST Framework changes
*----------------------------------------------------------------------*
* 04|09|2019| V-ASPATT |4360996 | MS2K981536 |Submit Date and time     *
*                                             calling program          *
*----------------------------------------------------------------------*
* 07|19|2019|KRDASH    |4812204 | DGDK911539 | Reprocessing logic for
*                                              Azure Services
*----------------------------------------------------------------------*
* 04|28|2020|KRDASH    |5566653 | SMTK906003 | Reprocessing logic for
*                                              AAD and Blob service
*----------------------------------------------------------------------*
* 03|01|2022|SANJUKUM  |8222481 | SMTK907382  | Reprocessing logic for
*                                               Managed Identity
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 19|04|2023| VBANSAL  |8222481 | SMTK90841  | Reprocessing Logic MI/AAD token gen.
*----------------------------------------------------------------------*

    DATA : lv_string        TYPE string,
* Start of changes by KRDASH DGDK911539 4812204
           lw_zadf_config   TYPE zadf_config,
           lv_interfacetype TYPE zazure_dest,
           lv_interfaceid   TYPE zinterface_id,
           lo_adf_reprocess TYPE REF TO zcl_adf_service_reprocess,
           lv_sas_token     TYPE string,
           lv_sas_date      TYPE string,
* End of changes by KRDASH DGDK911539 4812204
           lv_method        TYPE char20.
*--Begin of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
    DATA: lv_app_id       TYPE string,
          lv_interface_id TYPE string,
          lv_response     TYPE string.
* Begin of changes by KRDASH SMTK906003
    DATA: lv_low           TYPE c LENGTH 80,
          lv_interface_aad TYPE zinterface_id.
* End of changes by KRDASH SMTK906003

    CONSTANTS:lc_app_id                      TYPE rvari_vnam    VALUE 'ZCUST_CR_EIS_APP_ID',
              lc_interface_id                TYPE rvari_vnam    VALUE 'ZRETRY_INTERFACE_WITH_AAD',
              lc_interface_aad               TYPE zinterface_id VALUE 'CUSTCR_AAD',
* Begin of changes by KRDASH DGDK911539 4812204
              lc_zadf_reprocess_interface_id TYPE ztvarvc-varname VALUE 'ZADF_REPROCESS_INTERFACE_ID',
              lc_servicebus                  TYPE zadf_config-interface_type VALUE 'SERVICEBUS',
              lc_service_cosmosdb            TYPE zazure_dest VALUE 'COSMOSDB',
              lc_aad                         TYPE zazure_dest VALUE 'AAD',
              lc_msi                         TYPE zazure_dest VALUE 'MI',
              lc_blob                        TYPE zadf_config-interface_type VALUE 'BLOB', "Added by KRDASH SMTK906003
              lc_auth1                       TYPE ihttpnvp-name VALUE 'Authorization',
              lc_date                        TYPE ihttpnvp-name VALUE 'x-ms-date',
* End of changes by KRDASH DGDK911539 4812204
              lc_auth                        TYPE string        VALUE 'authorization'.
*--End of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813

    DATA:lv_textid2 TYPE scx_t100key.
*-----------------------check message id ------------------------------*
    check_messageid( EXPORTING message_id = message_id ).
*-----------------------get db data -----------------------------------*
    get_db_data( EXPORTING message_id = message_id ).
* ------------------------get the latest payloadv----------------------*
*   Sort the retry num , so that latest one is picked for reprocessing
    SORT payload BY retry_num DESCENDING.
    READ TABLE payload INTO DATA(wa_paylod) INDEX 1.
* ---------------------------------------------------------------------*
    interface_name = wa_paylod-interface_id.
* ---------------------------------------------------------------------*
*-----------------------Get the config ------------------------------*
* Begin of changes V-LAUPPA
    DATA: wa_config TYPE zrest_srtuct_config.
    CALL METHOD zcl_rest_utility_class=>get_config_data
      EXPORTING
        interface_id = interface_name
        method       = wa_paylod-method
      RECEIVING
        config_data  = wa_config.
* End of changes V-LAUPPA
*-----------------------Retry possible ? ------------------------------*
    IF wa_config-max_retry EQ 0.
      MESSAGE 'Retry not possible'(008) TYPE 'I'.   " Added the messsage for VSTF 2163894 / DGDK903444
      EXIT.
    ELSE.
*      IF wa_paylod-retry_num EQ  wa_config-max_retry."(-) VSO# 3960458 / DG2K904406
      IF wa_paylod-retry_num GE  wa_config-max_retry. "(+) VSO# 3960458 / DG2K904406
        IF from_scheduler EQ 'X'. "v-lauppa
          MESSAGE 'Maximum number of retry attempts reached'(009) TYPE 'I'. " Added the message for VSTF 2163894 / DGDK903444
          EXIT.
        ENDIF."v-lauppa
      ENDIF.
    ENDIF.
*----------------------------------------------------------------------*
* Begin of change for the exponential retries.
    IF from_scheduler EQ 'X'.
      DATA(wa_global) = zcl_rest_utility_class=>get_global_params( ).

*     3 9 27 81 243 243 243 ...........
      DATA : base       TYPE i VALUE 3,
             next_retry TYPE p DECIMALS 0,
             seconds    TYPE p DECIMALS 0.
*     Get the next retrial
      IF wa_paylod-retry_num EQ space.
        wa_paylod-retry_num = 0.
      ELSE.
      ENDIF.
      next_retry = base ** wa_paylod-retry_num .
*     Check the date and time difference between the first trail and now
      TRY.
          CALL METHOD cl_abap_tstmp=>td_subtract
            EXPORTING
              date1    = sy-datum
              time1    = sy-uzeit
              date2    = monitor-zexedate
              time2    = monitor-zexetime
            IMPORTING
              res_secs = seconds.
        CATCH cx_parameter_invalid_type INTO DATA(lx_invalid_type).
          DATA(lv_param_type_ex) = lx_invalid_type->get_text( ).
        CATCH cx_parameter_invalid_range INTO DATA(lx_invalid_range).
          DATA(lv_param_range_ex) = lx_invalid_range->get_text( ).
      ENDTRY.
*       Check if enough time has elapsed
      next_retry = 60 * next_retry.
*       Set the max. repetitive interval to be retried ( break from exponential ).
      IF next_retry > ( wa_global-linear_retry_duration * 60 * 60 ).
        next_retry = wa_global-linear_retry_duration * 60 * 60.
      ENDIF.
      IF ( seconds < next_retry ).
        EXIT.
      ENDIF.
    ENDIF.
*    end of change.
*------------------------------------- --------------------------------*

    CLEAR: lv_interface_id, lv_interface_aad.
    CLEAR : lv_low.

* Check entry for Azure SDK in zadf_sdk_retry for retry mechanism
    SELECT SINGLE  *
               FROM zadf_sdk_retry
               INTO  @DATA(ls_sdk_retry)
               WHERE interface_id = @wa_paylod-interface_id.
    IF sy-subrc EQ 0 AND ls_sdk_retry-retry_flag IS NOT INITIAL.
*Reprocessing logic for Azure services Getting Auth Token.
      lv_interfacetype  = ls_sdk_retry-interface_type.
      CREATE OBJECT lo_adf_reprocess.
      CALL METHOD lo_adf_reprocess->sdk_rest_retry
        EXPORTING
          iv_rest_retry = ls_sdk_retry
          iv_url        = CONV #( wa_paylod-uri )
        IMPORTING
          rv_token      = lv_sas_token
          rv_date       = lv_sas_date
          rv_aad_token  = DATA(lv_aad_auth)
          rv_url        = DATA(lv_uri).

      IF lv_interfacetype EQ lc_servicebus.
        wa_paylod-uri = lv_uri.
      ENDIF.

* Else follow the ZTVRAVC approch for reprocessing AAD
    ELSE.

      CLEAR: lv_interface_id, lv_interface_aad.
      SELECT SINGLE low                                     " Given current reprocessing App Interface
             high                                           " Fetch corresponding AAD Interface for a given app Interface
             FROM ztvarvc
             INTO ( lv_interface_id, lv_interface_aad )
             WHERE varname = lc_interface_id
               AND low     = wa_paylod-interface_id.
      IF sy-subrc EQ 0.
* Begin OF changes by KRDASH SMTK906003
        CLEAR : lv_low.
        SELECT SINGLE low,                                  " Client ID
               high                                         " Resource
               FROM ztvarvc
               INTO ( @lv_low, @DATA(lv_resource) )
               WHERE varname = @lv_interface_id.
        IF sy-subrc EQ 0 AND lv_resource IS NOT INITIAL AND lv_interface_aad IS NOT INITIAL.
          lv_app_id = lv_low.
* Begin of Change SANJUKUM_SMTK907382
          CREATE OBJECT lo_adf_reprocess.
          CALL METHOD lo_adf_reprocess->get_aad_token
            EXPORTING
              iv_interface_id = lv_interface_aad
              iv_client_id    = lv_app_id
              iv_resource     = lv_resource
            IMPORTING
              ev_aad_token    = lv_aad_auth.
* End of Change SANJUKUM_SMTK907382
        ENDIF.
      ENDIF.
    ENDIF.
**End of changes by KRDASH DGDK911539 4812204

    TRY .
        lv_method = wa_config-method.
        CREATE OBJECT rest_handler
          EXPORTING
            interface_name      = interface_name
            business_identifier = wa_paylod-businessid
            method              = lv_method.
      CATCH zcx_http_client_failed INTO DATA(cx_http_failed).
        RAISE EXCEPTION cx_http_failed.
      CATCH zcx_interace_config_missing INTO DATA(cx_config_missing).
        RAISE EXCEPTION cx_config_missing.
    ENDTRY.
*   Set the format in binary as it's already converted.
    IF wa_paylod-payload IS NOT INITIAL.
      rest_handler->zif_rest_framework~set_binary_body( wa_paylod-payload ).
    ENDIF.

*   //Begin of code by Ashutosh - to retain the calling method VSO#4360996 TR#MS2K981536
*    rest_handler->set_callingmethod('RETRY').
    IF monitor-calling_method IS NOT INITIAL.
      rest_handler->set_callingmethod( monitor-calling_method ).
    ELSE.
      rest_handler->set_callingmethod('RETRY').
    ENDIF.
*   //up to here VSO#4360996 TR#MS2K981536

*   Append the headers to the message
    CLEAR lv_string.
    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring  = wa_paylod-programheaders
        im_encoding = 'UTF-8'
      IMPORTING
        ex_string   = lv_string.

    DATA:wa_header TYPE ihttpnvp.
    SPLIT lv_string AT '|' INTO TABLE DATA(result_tab) IN CHARACTER MODE.
    LOOP AT result_tab INTO DATA(wa_result_tab).
      SPLIT wa_result_tab AT ':' INTO wa_header-name wa_header-value.
      CASE wa_header-name.
        WHEN lc_auth OR lc_auth1.                         " authorization or Auhorization Header
          IF lv_aad_auth IS NOT INITIAL .                 " AAD Auth token
            DATA(lv_value) = lv_aad_auth.
*          ELSEIF lv_final_token IS NOT INITIAL."lv_interfacetype EQ lc_servicebus AND lv_final_token IS NOT INITIAL.      " ServiceBus Token
*            lv_value = lv_final_token.
*          ELSEIF lv_mi_token IS NOT INITIAL.              " MI Token
*            lv_value = lv_mi_token.
          ELSEIF lv_sas_token IS NOT INITIAL.
            lv_value = lv_sas_token.
          ELSE.
            lv_value =  wa_header-value.
          ENDIF.
        WHEN lc_date.                                     " x-ms-date Header
*          IF lv_interfacetype EQ lc_service_cosmosdb AND lv_sas_date IS NOT INITIAL.       " CosmosDB SAS date
          IF lv_sas_date IS NOT INITIAL.
            lv_value = lv_sas_date.
          ELSE.
            lv_value =  wa_header-value.
          ENDIF.
        WHEN OTHERS.
          lv_value =  wa_header-value.
      ENDCASE.
* Add headers
      rest_handler->set_request_header( iv_name = wa_header-name  iv_value = lv_value  ).
    ENDLOOP.

*   //Begin of code by Ashutosh - to retain the calling program VSO#4360996 TR#MS2K981536
*    rest_handler->set_callingprogram('ZCL_REST_UTILITY_CLASS').
    IF monitor-calling_program IS NOT INITIAL.
      rest_handler->set_callingprogram( monitor-calling_program ).
    ELSE.
      rest_handler->set_callingprogram('ZCL_REST_UTILITY_CLASS').
    ENDIF.

*   //Retain Submit date and time
    IF monitor-submit_date IS NOT INITIAL.
      rest_handler->keep_submit_params( im_submit_date = monitor-submit_date im_submit_time = monitor-submit_time ).
    ENDIF.
*   //up to here VSO#4360996 TR#MS2K981536

    lv_string = wa_paylod-uri.

* Begin of Change SANJUKUM_SMTK907382
* Reprocessing logic for Azure Blob Service
    IF ( lv_interfacetype EQ lc_blob ) AND ( lv_sas_token IS NOT INITIAL ).
* End of Change SANJUKUM_SMTK907382
      rest_handler->set_uri( lv_sas_token ).
    ELSEIF lv_interfacetype EQ lc_servicebus.
* Ignore URI aadition as already taken care earlier
    ELSEIF lv_string IS NOT INITIAL.
      rest_handler->set_uri( lv_string ).
    ENDIF.
*   Retry
    response = rest_handler->zif_rest_framework~execute( io_entity  = request
                                                         async      = abap_false
                                                         is_retry   = abap_true
                                                         messageid  = message_id
                                                         retry_count = wa_paylod-retry_num ).
    rest_handler->close( ).
    FREE rest_handler.
    FREE lo_adf_reprocess.
    CLEAR: lv_aad_auth, lv_app_id.
  ENDMETHOD.


  METHOD retry_limit_exceeded.

    DATA:it_data            TYPE TABLE OF zrest_monitor,
         it_retrynum        TYPE TABLE OF zrest_conf_misc,
         lw_retrynum        TYPE zrest_conf_misc,
         wa_data            TYPE zrest_monitor,
         wa_retrynum        TYPE zrest_conf_misc,
         lt_lines           TYPE TABLE OF tline,
         lw_lines           TYPE tline,
         lt_body            TYPE soli_tab,
         lt_body_email      TYPE soli_tab,
         lt_body_tmp        TYPE soli_tab,
         lw_body_tmp        TYPE soli,
         lv_subject         TYPE so_obj_des,
         lv_recepient       TYPE adr6-smtp_addr,
         lt_zrt_monitor     TYPE zrt_monitor,
         lt_zrt_monitor_tmp TYPE zrt_monitor,
         lt_monitor_final   TYPE zrt_monitor,
         lw_monitor_final   TYPE zrest_monitor,
         lw_return          TYPE bapiret2,
         lv_result          TYPE os_boolean,
         lt_retry_report    TYPE TABLE OF zrest_retry_limi,
         lw_retry_report    TYPE zrest_retry_limi,
         lv_body            TYPE char255,
         lv_messageid       TYPE char50,
         lv_tabix           TYPE sytabix.

    TYPES: BEGIN OF lty_email_data,
             email_id      TYPE ad_smtpadr,
             mail_body_txt TYPE tdobname,
             max_retry     TYPE zq_counte,
             retry_method  TYPE zretry_method.
            INCLUDE        TYPE zrest_monitor.
    TYPES: END OF lty_email_data.

    DATA: lw_email_data TYPE lty_email_data,
          lt_email_data TYPE TABLE OF lty_email_data.

    FIELD-SYMBOLS: <lfs_zrt_monitor>  TYPE zrest_monitor,
                   <lfs_body>         TYPE soli,
                   <lfs_retry_report> TYPE zrest_retry_limi.

    CONSTANTS: lc_object     TYPE balobj_d  VALUE 'Z_REST_LOG',
               lc_object_sub TYPE balsubobj VALUE 'Z_REST_SUB'.

    DATA: lt_alog   TYPE zrt_applog_message,
          lw_alog   TYPE zrest_applog_message,
          lv_extnum TYPE balnrext,
          lv_jobcnt TYPE tbtcm-jobcount,
          lv_jobnme TYPE tbtcm-jobname.

    CLEAR: lv_messageid, lt_alog, lv_extnum,
           lv_jobcnt, lv_jobnme, lw_alog.

    CLEAR: lt_retry_report.
    SELECT * FROM zrest_monitor
      INTO TABLE it_data
      WHERE rertynum > 0
      AND ( httpstatus <= 200 OR httpstatus >= 300 )
      AND mail_sent = ' '.
*
    IF it_data IS NOT INITIAL.
      SELECT * FROM zrest_conf_misc
        INTO TABLE it_retrynum
        FOR ALL ENTRIES IN it_data
        WHERE interface_id =  it_data-interface_id.
      IF sy-subrc = 0.
        SORT it_retrynum BY interface_id method.
        LOOP AT it_data INTO wa_data.
          READ TABLE it_retrynum INTO wa_retrynum WITH KEY interface_id = wa_data-interface_id
                                                           method = wa_data-method BINARY SEARCH.
          IF sy-subrc EQ 0.
            IF  wa_retrynum-max_retry <= wa_data-rertynum.
              APPEND wa_data TO lt_zrt_monitor.
*// Join 2 tables for sending Email
              MOVE-CORRESPONDING wa_data TO lw_email_data.
              lw_email_data-max_retry     = wa_retrynum-max_retry.
              lw_email_data-email_id      = wa_retrynum-email_id.
              lw_email_data-mail_body_txt = wa_retrynum-mail_body_txt.
              lw_email_data-retry_method  = wa_retrynum-retry_method.
              APPEND lw_email_data TO lt_email_data.
              CLEAR wa_data.
            ENDIF.
*// Config Table is not maintained
          ELSE.
            lw_retry_report-zmessageid   = wa_data-zmessageid.
            lw_retry_report-zuser        = wa_data-zuser.
            lw_retry_report-zexedate     = wa_data-zexedate.
            lw_retry_report-zexetime     = wa_data-zexetime.
            lw_retry_report-destination  = wa_data-destination.
            lw_retry_report-interface_id = wa_data-interface_id.
            lw_retry_report-method       = wa_data-method.
            lw_retry_report-message      = 'Config Table is not maintained'(008).

            APPEND lw_retry_report TO et_retry_report.
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE 'No Data Found in Config Table'(010) TYPE 'I'.
        RETURN.
      ENDIF.

      SORT lt_zrt_monitor BY zmessageid.
*// Get standard Text for the Email Subject from SO10
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'ST'
          language                = sy-langu
          name                    = 'ZREST_RETRY_EMAIL_BODY'
          object                  = 'TEXT'
        TABLES
          lines                   = lt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        MESSAGE 'Mail Body is not maintained'(008) TYPE 'I'.
        RETURN.
      ELSE.

        LOOP AT lt_lines INTO lw_lines.
          APPEND lw_lines-tdline TO lt_body.
        ENDLOOP.
      ENDIF.
      lv_subject    = 'HTTP Calls: Maximum Retry has been reached'.

*// Sort based on Email and Consolidate all the failures in a single email

      SORT lt_email_data BY email_id interface_id.
      LOOP AT lt_email_data INTO lw_email_data.
*// Prepare the header of the email.
        AT NEW email_id.
*// Find the standard Text from the Configuration Table when maintained
          IF lw_email_data-mail_body_txt IS NOT INITIAL.
            CLEAR: lt_body_email.
            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = 'ST'
                language                = sy-langu
                name                    = lw_email_data-mail_body_txt
                object                  = 'TEXT'
              TABLES
                lines                   = lt_lines
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.
            IF sy-subrc <> 0.
              LOOP AT lt_lines INTO lw_lines.
                APPEND lw_lines-tdline TO lt_body_email.
              ENDLOOP.
            ENDIF.
          ENDIF.
*// Prepare Tabular Display of Interface and messages.
*// Set Email Body Table header border and Color
          lw_body_tmp-line =    '<head>'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    '<style type="text/css">'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    '<!--'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    'table{background-color:#FFF;border-collapse:collapse;}'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    'td.Data{background-'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    'color:#FFF;border:1px solid black;padding:3px;}'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    'td.Heading{background-color:Blue;text-'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    'align:center;border:1px solidblack;padding:3px;}'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    '-->'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    '</style>'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =    '<head>'.
          APPEND lw_body_tmp TO lt_body_tmp.
*// Prepare Table Header Data
          lw_body_tmp-line = '<TABLE class="Data">'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line =  '<tr>'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = '<td class="Heading">'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = 'Interface Id:'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = '</td>'.
          APPEND lw_body_tmp TO lt_body_tmp.

          lw_body_tmp-line = '<td class="Heading">'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = 'Message Id:'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = '</td>'.
          APPEND lw_body_tmp TO lt_body_tmp.

          lw_body_tmp-line = '<td class="Heading">'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = 'Method:'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = '</td>'.
          APPEND lw_body_tmp TO lt_body_tmp.

          lw_body_tmp-line = '<td class="Heading">'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = 'Retry Number'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = '</td>'.
          APPEND lw_body_tmp TO lt_body_tmp.
          lw_body_tmp-line = '</tr>'.
          APPEND lw_body_tmp TO lt_body_tmp.
        ENDAT.
*// Prepare Data Records of the table
        lw_body_tmp-line = '/tr>'.
        lw_body_tmp-line = '<td class="Data">'.
        APPEND lw_body_tmp TO lt_body_tmp.
        lw_body_tmp-line = lw_email_data-interface_id.
        APPEND lw_body_tmp TO lt_body_tmp.
        lw_body_tmp-line = '</td>'.
        APPEND lw_body_tmp TO lt_body_tmp.

        lw_body_tmp-line = '<td class="Data">'.
        APPEND lw_body_tmp TO lt_body_tmp.
        lw_body_tmp-line = lw_email_data-zmessageid.
        APPEND lw_body_tmp TO lt_body_tmp.
        lw_body_tmp-line = '</td>'.
        APPEND lw_body_tmp TO lt_body_tmp.

        lw_body_tmp-line = '<td class="Data">'.
        APPEND lw_body_tmp TO lt_body_tmp.
        lw_body_tmp-line = lw_email_data-method.
        APPEND lw_body_tmp TO lt_body_tmp.
        lw_body_tmp-line = '</td>'.
        APPEND lw_body_tmp TO lt_body_tmp.

        lw_body_tmp-line = '<td class="Data">'.
        APPEND lw_body_tmp TO lt_body_tmp.
        lw_body_tmp-line = lw_email_data-max_retry.
        APPEND lw_body_tmp TO lt_body_tmp.
        lw_body_tmp-line = '</td>'.
        APPEND lw_body_tmp TO lt_body_tmp.
        lw_body_tmp-line = '</tr>'.
        APPEND lw_body_tmp TO lt_body_tmp.

        READ TABLE lt_zrt_monitor ASSIGNING  <lfs_zrt_monitor>
                                  WITH KEY zmessageid = lw_email_data-zmessageid
                                  BINARY SEARCH.
        IF sy-subrc = 0.
          <lfs_zrt_monitor>-mail_sent = 'X'.
          APPEND <lfs_zrt_monitor> TO lt_zrt_monitor_tmp.
        ENDIF.
*// Prepare Success Log Template
        lw_retry_report-zmessageid   = lw_email_data-zmessageid.
        lw_retry_report-zuser        = lw_email_data-zuser.
        lw_retry_report-zexedate     = lw_email_data-zexedate.
        lw_retry_report-zexetime     = lw_email_data-zexetime.
        lw_retry_report-destination  = lw_email_data-destination.
        lw_retry_report-interface_id = lw_email_data-interface_id.
        lw_retry_report-method       = lw_email_data-method.
        lw_retry_report-max_retry    = lw_email_data-max_retry.
        lw_retry_report-message      = 'Success. Email Sent'(004).

        APPEND lw_retry_report TO lt_retry_report.

        AT END OF email_id.
*// Close the table of Email Body
          lw_body_tmp-line = '</TABLE>'.
          APPEND lw_body_tmp TO lt_body_tmp.
*// Use the SO10 Template and insert extra lines for Mail Body
          IF lt_body_email IS INITIAL.
*// Refer standard template only if Config table is not maintained with TEXT Name
            lt_body_email = lt_body.
          ENDIF.

          READ TABLE lt_body_email ASSIGNING <lfs_body> WITH KEY line = '&LW_INTERFACE&'.
          IF sy-subrc = 0.
            lv_tabix = sy-tabix.
            INSERT LINES OF lt_body_tmp INTO lt_body_email INDEX lv_tabix.
            DELETE lt_body_email WHERE line = '&LW_INTERFACE&'.
          ENDIF.
          lv_recepient = lw_email_data-email_id.

*// Call the email send Method
          CALL METHOD me->send_email
            EXPORTING
              iv_recepient = lv_recepient
              iv_subject   = lv_subject
              it_body      = lt_body_email
            IMPORTING
              ew_return    = lw_return
              ev_result    = lv_result.

          IF lv_result IS NOT INITIAL.
*// Collect to final table to Update Email Sent Flag.
            APPEND LINES OF lt_zrt_monitor_tmp TO lt_monitor_final.
            APPEND LINES OF lt_retry_report TO et_retry_report.
*// Update Success Log
          ELSEIF lw_return IS NOT INITIAL.
*// Send Mail returned Error
            CONCATENATE lw_return-type lw_return-id  lw_return-number lw_return-message_v1
            INTO lw_retry_report-message SEPARATED BY space.
*// Update Error Message
            LOOP AT lt_retry_report ASSIGNING  <lfs_retry_report>.
              <lfs_retry_report>-message  =   lw_retry_report-message.
            ENDLOOP.
            APPEND LINES OF lt_retry_report TO et_retry_report.
*// No Exception is returned from Send Mail
          ELSE.
            LOOP AT lt_retry_report ASSIGNING  <lfs_retry_report>.
              <lfs_retry_report>-message  =   'Error. Email Not Sent'(005).
            ENDLOOP.
            APPEND LINES OF lt_retry_report TO et_retry_report.
          ENDIF.
*// Clear attributes for Next Iteration
          CLEAR: lt_zrt_monitor_tmp, lt_retry_report, lt_body_email, lv_recepient, lw_retry_report,
                 lt_body_tmp.
        ENDAT.

      ENDLOOP.
*// Update the table to mark Email sent
      DELETE lt_monitor_final WHERE mail_sent IS INITIAL.
*// Set Table Lock
      LOOP AT lt_monitor_final INTO lw_monitor_final.
        CALL FUNCTION 'ENQUEUE_EZ_ZREST_MONITOR'
          EXPORTING
            mandt          = sy-mandt
            zmessageid     = lw_monitor_final-zmessageid
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          DELETE lt_monitor_final WHERE zmessageid =  lw_monitor_final-zmessageid.
*// If locking failed write to Application log and do not proceed
          IF sy-batch IS NOT INITIAL.
            CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
              IMPORTING
                jobcount        = lv_jobcnt
                jobname         = lv_jobnme
              EXCEPTIONS
                no_runtime_info = 1
                OTHERS          = 2.
            IF sy-subrc = 0.
              CONCATENATE lv_jobcnt lv_jobnme INTO lv_extnum SEPARATED BY '_'.
            ENDIF.
          ELSE.
            lv_extnum = sy-cprog.
          ENDIF.
          lv_messageid =  'Table not updated due to lock'(015).

          lw_alog-zmsgty = 'E'.
          lw_alog-zmsgv1 = lv_messageid.
          lw_alog-zmsgv2 = lw_monitor_final-zmessageid.
          lw_alog-zmsgv2 = 'ZREST_MONITOR'.
          APPEND lw_alog TO lt_alog.

*// Call the application Log
          CALL METHOD zcl_rest_utility_class=>write_application_log
            EXPORTING
              iv_object    = lc_object
              iv_subobject = lc_object_sub
              iv_extnumber = lv_extnum
              it_message   = lt_alog.
        ENDIF.

      ENDLOOP.

      IF lt_monitor_final IS NOT INITIAL.
        MODIFY zrest_monitor FROM TABLE lt_monitor_final.
        IF sy-subrc = 0.
          MESSAGE 'Table ZREST_MONITOR updated Successfully'(011) TYPE 'S'.
        ELSE.
          MESSAGE 'Table ZREST_MONITOR not updated'(012) TYPE 'I'.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'No Messages Found'(013) TYPE 'I'.
    ENDIF.

    LOOP AT lt_monitor_final INTO lw_monitor_final.
      CALL FUNCTION 'DEQUEUE_EZ_ZREST_MONITOR'
        EXPORTING
          mandt      = sy-mandt
          zmessageid = lw_monitor_final-zmessageid.
    ENDLOOP.

  ENDMETHOD.


  METHOD retry_log.
    DATA lv_string1 TYPE string.
    DATA lv_string2 TYPE string.
    DATA lv_string3 TYPE string.
    DATA: lv_string     TYPE string,
          lv_retry_date TYPE char10,
          lv_retry_time TYPE char8,
          lt_retries type TABLE OF zrest_retries,
          wa_retries type zrest_retries.
*    SELECT * FROM zrest_retries INTO TABLE @DATA(lt_retries) v-jobpau
*      WHERE zmessageid = @message_id.
    SELECT * FROM zrest_retries INTO TABLE lt_retries
      WHERE zmessageid = message_id.
    IF sy-subrc EQ 0.
      SORT lt_retries BY retry_num DESCENDING.
    ENDIF.

*    LOOP AT lt_retries INTO DATA(wa_retries).  v-jobpau
     LOOP AT lt_retries INTO wa_retries.
      AT FIRST.
        lv_string =
       '<htm1 lang="EN">' &&
       '<head>' &&
       '<meta name="Demo" content="Test">' &&
       '<style type="text/css">' &&
       'span.h1 {font-size: 100%; color:#000080; ' &&
       'font-weight:bold;}' &&
       '</style>' &&
       '</head>' &&
       '<body>' &&
       '<p><span class="h1"> <pre>              Message ID                       Count      Date            Time</pre> </span></p>' &&
       '</body>' &&
       '</html>'.
      ENDAT.

      lv_string1 = message_id.
      lv_string2 = wa_retries-retry_num.
      WRITE wa_retries-retrydate TO lv_retry_date MM/DD/YYYY.
      WRITE wa_retries-retrytime TO lv_retry_time USING EDIT MASK '__:__:__'.
      CONCATENATE '<pre>'
                  lv_string1
                  lv_string2
                  lv_retry_date
                  lv_retry_time
                  '</pre>'
             INTO lv_string3 SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
      CONCATENATE lv_string
                  lv_string3
                  INTO lv_string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      CLEAR: lv_string1, lv_string2, lv_string3, wa_retries.
    ENDLOOP.


    CALL METHOD cl_abap_browser=>show_html(
      EXPORTING
        title       = 'Retry Log'
        html_string = lv_string ).
  ENDMETHOD.


  METHOD send_email.
    DATA:wa_input TYPE zrest_monitor.
    DATA: lo_send_request    TYPE REF TO cl_bcs.
    DATA: ip_subject       TYPE string,
          lv_subject       TYPE so_obj_des,
          lv_recepient     TYPE adr6-smtp_addr,
          lt_body          TYPE soli_tab,
          lo_recipient     TYPE REF TO if_recipient_bcs,
          ls_content       TYPE LINE OF soli_tab,
          lo_document      TYPE REF TO cl_document_bcs,
          lx_bcs_exception TYPE REF TO cx_bcs,
          lo_sender        TYPE REF TO if_sender_bcs,
          lv_result        TYPE os_boolean.

    lv_subject    = iv_subject.
    lv_recepient  = iv_recepient.
    lt_body       = it_body.

    TRY.
*     -------- create persistent send request ------------------------
        lo_send_request = cl_bcs=>create_persistent( ).

        lo_document = cl_document_bcs=>create_document( i_type    = 'HTM'
                                                        i_text    = lt_body
                                                        i_subject = lv_subject ).

        lo_send_request->set_document( lo_document ).

*//     Create recipient and add to send request
        lo_recipient = cl_cam_address_bcs=>create_internet_address(
                                           lv_recepient ).
        lo_send_request->add_recipient( i_recipient = lo_recipient ).

*//     ---------- send document ---------------------------------------
        lo_send_request->set_send_immediately('X').
        CALL METHOD lo_send_request->send(
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result              = lv_result ).
        IF lv_result = 'X'.
*          COMMIT WORK.
          ev_result = lv_result.
        ENDIF.
      CATCH cx_bcs INTO lx_bcs_exception.
        ew_return-id         = lx_bcs_exception->msgid.
        ew_return-number     = lx_bcs_exception->msgno.
        ew_return-type       = lx_bcs_exception->msgty.
        ew_return-message_v1 = lx_bcs_exception->msgv1.
        ew_return-message_v2 = lx_bcs_exception->msgv2.
        ew_return-message_v3 = lx_bcs_exception->msgv3.
        ew_return-message_v4 = lx_bcs_exception->msgv4.
        ew_return-message    = lx_bcs_exception->error_text.
    ENDTRY.
  ENDMETHOD.


  METHOD show_payload.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04|28|2016|V-DEVEER  |2163894 | DGDK903413 | Authorization Check
* 06|28|2019|V_ANYALL  |4668163 | SMTK905370 | Sync MS1
*----------------------------------------------------------------------*
    DATA : wa_payload TYPE zrest_mo_payload,
           it_payload TYPE STANDARD TABLE OF zrest_mo_payload,
           wa_monitor TYPE zrest_monitor.
    DATA : lv_content LIKE wa_payload-payload,

           output     TYPE xstring,
           l_exception_error TYPE REF TO cx_st_error,
           l_exception_rt_error TYPE REF TO cx_xslt_runtime_error,
           lv_content_cpy LIKE wa_payload-payload.

*   Check if user has the authority to call.

*   Begin of changes VSTF # 2163894 | DGDK903413
*    TRY.
*        check_authority( ).
*      CATCH zcx_http_client_failed INTO DATA(lv_textid).
*        DATA(lv_text2) = lv_textid->if_t100_message~t100key.
*        RAISE EXCEPTION TYPE zcx_http_client_failed EXPORTING textid = lv_text2.
*    ENDTRY.
*   End of changes VSTF # 2163894 | DGDK903413
*   Select the response data .
    SELECT * FROM zrest_mo_payload INTO TABLE it_payload WHERE messageid EQ message_id.
    IF sy-subrc EQ 0.
    ELSE.
      EXIT.
    ENDIF.
*   Based on the response indicator ..set the payload
*    IF response EQ abap_true.
*      CLEAR lv_content.
*      LOOP AT it_payload INTO wa_payload.
*        CHECK NOT wa_payload-content_type_res CS 'text/html'.
*        CONCATENATE wa_payload-response
*                    lv_content
*               INTO lv_content IN BYTE MODE.
*      ENDLOOP.
*      IF wa_payload-content_type_res CS 'application/json' .
*        CALL METHOD cl_abap_browser=>show_html( EXPORTING html_xstring = lv_content ).
*      ELSEIF lv_content IS INITIAL.
*        SELECT SINGLE status INTO @DATA(lv_text) FROM zrest_monitor WHERE zmessageid = @wa_payload-messageid.
*        MESSAGE lv_text TYPE 'I'.
*        EXIT.
*      ELSE.
*        CALL METHOD cl_abap_browser=>show_xml( EXPORTING xml_xstring = lv_content ).
*      ENDIF.
*    ELSE.
*      CLEAR lv_content.
*      LOOP AT it_payload INTO wa_payload.
*        CONCATENATE wa_payload-payload
*                    lv_content INTO
*                    lv_content IN BYTE MODE.
*      ENDLOOP.
*      IF  wa_payload-content_type_req CS 'application/json'.
*        CALL METHOD cl_abap_browser=>show_html( EXPORTING html_xstring = lv_content ).
*      ELSE.
*        CALL METHOD cl_abap_browser=>show_xml( EXPORTING xml_xstring = lv_content ).
*      ENDIF.
*    ENDIF.
*Commented for VSTF # 2163894 | DGDK903413
*    IF sy-uname EQ 'SAPURANA' OR sy-uname = 'TST-ZREST' or sy-uname = 'V-DEVEER'.
*      IF check_obfuscation_needed( inetrface_in = wa_payload-interface_id ) EQ abap_true.
*        DATA(output) = obfuscate( input = lv_content ).
*        lv_content = output.
*      ENDIF.
*    ENDIF.
*  End of changes for VSTF # 2163894 | DGDK903413
*   if its json..call up the HTML editor
*    IF wa_payload-content_type_res CS 'application/json' OR
*       wa_payload-content_type_req CS 'application/json'.
*      CALL METHOD cl_abap_browser=>show_html( EXPORTING html_xstring = lv_content ).
*    ELSE.
*      CALL METHOD cl_abap_browser=>show_xml( EXPORTING xml_xstring = lv_content ).
*    ENDIF.

*   Based on the response indicator ..set the payload
    IF response EQ abap_true.
      CLEAR lv_content.
      LOOP AT it_payload INTO wa_payload.
        CONCATENATE wa_payload-response
                    lv_content
               INTO lv_content IN BYTE MODE.
      ENDLOOP.
      AUTHORITY-CHECK OBJECT 'ZREST_AUTH'
      ID 'ZBOOLEAN' FIELD 'X'.
      IF sy-subrc NE 0.
        IF check_obfuscation_needed( inetrface_in = wa_payload-interface_id ) EQ abap_true.
*          DATA(output) = obfuscate( input = lv_content ). V-jobpau
          output = obfuscate( input = lv_content ).
          lv_content = output.
        ENDIF.
      ENDIF.
      CALL METHOD cl_abap_browser=>show_html( EXPORTING html_xstring = lv_content ).
    ELSE.
      CLEAR lv_content.
      LOOP AT it_payload INTO wa_payload.
        CONCATENATE wa_payload-payload
                    lv_content INTO
                    lv_content IN BYTE MODE.
      ENDLOOP.
      AUTHORITY-CHECK OBJECT 'ZREST_AUTH'
    ID 'ZBOOLEAN' FIELD 'X'.
      IF sy-subrc NE 0.
        IF check_obfuscation_needed( inetrface_in = wa_payload-interface_id ) EQ abap_true.
          output = obfuscate( input = lv_content ).
          lv_content = output.
        ENDIF.
      ENDIF.

*   This bit transforms a JSON string into a lovely hierarchical format
      lv_content_cpy = lv_content.


      TRY.
          CALL TRANSFORMATION sjson2html  SOURCE XML lv_content RESULT XML lv_content.
        CATCH: cx_st_error           INTO l_exception_error.
          lv_content = lv_content_cpy.
        CATCH: cx_xslt_runtime_error INTO l_exception_rt_error.
          lv_content = lv_content_cpy.
      ENDTRY.




      CALL METHOD cl_abap_browser=>show_html( EXPORTING html_xstring = lv_content ).
    ENDIF.
    ENDMETHOD.


  METHOD show_submitted_headers.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04|28|2016|V-DEVEER  |2163894 | DGDK903413 | Authorization Check
*----------------------------------------------------------------------*
* 04|28|2020| KRDASH   |5566653 | SMTK906003 | Correction to show all
*                                              request/response headers
*                                              in ZREST_UTIL monitor
*----------------------------------------------------------------------*
    DATA :  wa_monitor TYPE zrest_monitor,
            lcl_object TYPE REF TO zcl_rest_framework,
            lv_string  TYPE string,
            lx_string  TYPE xstring.
    CONSTANTS : c_comma TYPE c VALUE ','.
    TYPES : BEGIN OF ty_string,
              line TYPE string,
            END OF ty_string.

    DATA : wa_payload TYPE zrest_mo_payload,
           it_payload TYPE STANDARD TABLE OF zrest_mo_payload,
           wa_string  TYPE ty_string,
           it_string  TYPE STANDARD TABLE OF ty_string,
           output     TYPE xstring.
*   Check if user has the authority to call.
*   Begin of changes VSTF # 2163894 | DGDK903413
*    TRY.
*        check_authority( ).
*      CATCH zcx_http_client_failed INTO DATA(lv_textid).
*        DATA(lv_text2) = lv_textid->if_t100_message~t100key.
*        RAISE EXCEPTION TYPE zcx_http_client_failed EXPORTING textid = lv_text2.
*    ENDTRY.
*   End of changes VSTF # 2163894 | DGDK903413

*   Get all the headers called till date.
    SELECT * FROM zrest_mo_payload INTO TABLE it_payload WHERE messageid EQ message_id.
    IF sy-subrc <> 0.
    ELSE.
      CLEAR lv_string.
      LOOP AT it_payload INTO wa_payload.
*        Commented for VSTF # 2163894 | DGDK903413
        AUTHORITY-CHECK OBJECT 'ZREST_AUTH'
        ID 'ZBOOLEAN' FIELD 'X'.
        IF sy-subrc NE 0.
*          READ TABLE it_payload INTO wa_payload INDEX 1. "Commented by KRDASH SMTK906003
          IF check_obfuscation_needed( inetrface_in = wa_payload-interface_id ) EQ abap_true.
*            data(output) = obfuscate( input = wa_payload-headers ). v-jobpau
            output = obfuscate( input = wa_payload-headers ).
            wa_payload-headers = output.
          ENDIF.
        ENDIF.
*       End of changes for VSTF # 2163894 | DGDK903413
        CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
          EXPORTING
            im_xstring  = wa_payload-headers
            im_encoding = 'UTF-8'
          IMPORTING
            ex_string   = wa_string-line.
        CONCATENATE wa_string-line lv_string INTO lv_string.
      ENDLOOP.
*    Check if some fields needs to obfuscated.
      DATA lv_string1 TYPE string.
      CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
        EXPORTING
          im_xstring  = wa_payload-programheaders
          im_encoding = 'UTF-8'
        IMPORTING
          ex_string   = lv_string1.

      CONCATENATE lv_string
                  'Headers added by program'
                  lv_string1
                  INTO lv_string SEPARATED BY cl_abap_char_utilities=>cr_lf.

      CALL METHOD cl_abap_browser=>show_html(
        EXPORTING
          html_string = lv_string ).

    ENDIF.

  ENDMETHOD.


  METHOD unprocessed_data.
* Get the data waiting to be processed
*  SELECT * FROM zrest_mo_payload INTO TABLE result WHERE status EQ 0.
*    v-javeda MS2K948826
    TYPES: BEGIN OF ty_range,
                     sign(1) TYPE c,
                     option(2) TYPE c,
                     low          TYPE int4,
                     high         TYPE  int4,
                 END   OF ty_range.

    TYPES: ty_ranges TYPE TABLE OF ty_range.

    DATA: lw_range TYPE  ty_range,
          lr_ranges TYPE  ty_ranges.

    lw_range-sign   = 'I'.
    lw_range-option = 'NB'.
    lw_range-low    = '200'.
    lw_range-high   = '299'.
    APPEND lw_range TO lr_ranges.
**v-javeda -MS2K948826
* Begin of changes v-jobpau
    DATA: lt_monitor TYPE TABLE OF zrest_monitor.
    SELECT * FROM zrest_monitor
      INTO TABLE lt_monitor ". v-javeda - MS2K948543
*      where  ( httpstatus LT 200 OR httpstatus GE 300 )"AND httpstatus GE 300 )  "v-javeda - MS2K948543 - for performance
     WHERE   httpstatus IN lr_ranges. "v-javeda MS2K948826
*      AND zdelete NE 'X'. "v-javeda - MS2K948543 - for not preocessing deleted records
    IF sy-subrc = 0.
      DELETE lt_monitor WHERE zdelete EQ 'X'."( httpstatus GE 200 AND httpstatus LT 300 ).v-javeda - MS2K948543
* End of changes v-jobpau

* Select all the entries where retrial is still an option
      IF lt_monitor IS NOT INITIAL.
        SELECT * FROM zrest_mo_payload INTO TABLE result
                 FOR ALL ENTRIES IN lt_monitor
                 WHERE messageid = lt_monitor-zmessageid.
* Delete where status is valid-succss
        DELETE result WHERE ( status GE 200 AND status LT 300 ).
* Begin of changes v-jobpau
        SORT result BY messageid retry_num DESCENDING.
* Pick only the latest record if it is failed
        DELETE ADJACENT DUPLICATES FROM result COMPARING messageid.
* End of changes v-jobpau

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD write_application_log.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date     | USER ID       | VSTF#  | Transport  | Description         *
*----------|---------------|--------|------------|---------------------*
*10/05/21  | V-PRALAV      |   -    | SMTK906965 | Changing message    *
*                                                  type E to I         *
*09|29|2022| V-ASHOKM1     |        | SMTK907895 | Fixing VF Errors    *
*----------------------------------------------------------------------*
*********************************************************************
*                   Variable Declaration                            *
*********************************************************************
    DATA: lw_log        TYPE bal_s_log,
          lw_msg        TYPE bal_s_msg,
          lv_log_handle TYPE balloghndl,
          lw_message    TYPE zrest_applog_message.

*********************************************************************
*                   Constants Declaration                           *
*********************************************************************
    CONSTANTS: lc_bl TYPE char2      VALUE 'BL',
               lc_no TYPE char4      VALUE '0001',
               lc_e  TYPE char1      VALUE 'E',
               lc_s  TYPE char1      VALUE 'S',
               lc_b  TYPE char1      VALUE 'B',
               lc_i  TYPE char1      VALUE 'I'. "(+)V-PRALAV TR# SMTK906965

*// Populate the Data for the Hierarchial update
    lw_log-extnumber  = iv_extnumber.
    lw_log-object     = iv_object.
    lw_log-subobject  = iv_subobject.
    lw_log-aldate     = sy-datum.
    lw_log-altime     = sy-uzeit.
    lw_log-aluser     = sy-uname.
    lw_log-altcode    = sy-tcode.
    lw_log-alprog     = sy-repid.

*// Calling Function Module to Create the Application Log
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = lw_log
      IMPORTING
        e_log_handle            = lv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc EQ 0.

      LOOP AT it_message INTO lw_message.

        IF lw_message-zmsgid IS NOT INITIAL.
          lw_msg-msgid = lw_message-zmsgid.
        ELSE.
          lw_msg-msgid = lc_bl.     "'BL'.
        ENDIF.
        IF lw_message-zmsgno IS NOT INITIAL.
          lw_msg-msgno = lw_message-zmsgno.
        ELSE.
          lw_msg-msgno = lc_no.     "'0001'.
        ENDIF.

        lw_msg-msgty = lw_message-zmsgty.
        lw_msg-msgv1 = lw_message-zmsgv1.
        lw_msg-msgv2 = lw_message-zmsgv2.
        lw_msg-msgv3 = lw_message-zmsgv3.
        lw_msg-msgv4 = lw_message-zmsgv4.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle     = lv_log_handle
            i_s_msg          = lw_msg
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
*-Begin of changes by V-ASHOKM1 ++ SMTK907895
        IF sy-subrc <> 0.
          MESSAGE e004(zvf_zrest).  "Error in adding the Application Log Messages
        ENDIF.
*-End of changes by V-ASHOKM1 ++ SMTK907895
        CLEAR:  lw_msg-msgv1,
                lw_msg-msgv2,
                lw_msg-msgv3,
                lw_msg-msgv4.

      ENDLOOP.


*// FM to save the Application Log Messages
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_save_all       = 'X'
        EXCEPTIONS
          log_not_found    = 1
          save_not_allowed = 2
          numbering_error  = 3
          OTHERS           = 4.
      IF sy-subrc EQ 0.

*// 'Messages posted to Application Log Successfully'
        MESSAGE text-002 TYPE lc_s. "'S'.
      ENDIF.
    ELSE.

*// 'Application Log Creation Failed'
*    MESSAGE text-001 TYPE lc_e DISPLAY LIKE lc_s.  "(-)V-PRALAV TR# SMTK906965
      MESSAGE text-001 TYPE lc_i DISPLAY LIKE lc_s.  "(+)V-PRALAV TR# SMTK906965
    ENDIF.
  ENDMETHOD.
ENDCLASS.
