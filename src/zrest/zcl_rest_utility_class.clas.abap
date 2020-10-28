CLASS zcl_rest_utility_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .
    CLASS-METHODS download_payload_file
      IMPORTING
        !xstring    TYPE xstring
        !message_id TYPE zmid
      RAISING
        zcx_http_client_failed .
    METHODS retry
      IMPORTING
        !message_id     TYPE zmid
        !method         TYPE zinterface_method
        !from_scheduler TYPE char1 OPTIONAL
      RAISING
        zcx_interace_config_missing
        zcx_http_client_failed .
    CLASS-METHODS get_http_description
      IMPORTING
        !code              TYPE int4
      RETURNING
        VALUE(description) TYPE string .
    CLASS-METHODS show_payload
      IMPORTING
        !message_id TYPE zmid
        !response   TYPE c OPTIONAL
      RAISING
        zcx_http_client_failed .
    CLASS-METHODS get_config_data
      IMPORTING
        !interface_id      TYPE zinterface_id
        !method            TYPE zinterface_method
      RETURNING
        VALUE(config_data) TYPE zrest_srtuct_config .
    CLASS-METHODS get_static_headers
      IMPORTING
        !interface_id         TYPE zinterface_id
      RETURNING
        VALUE(static_headers) TYPE tihttpnvp .
    CLASS-METHODS show_submitted_headers
      IMPORTING
        !message_id TYPE zmid
      RAISING
        zcx_http_client_failed .
    CLASS-METHODS unprocessed_data
      RETURNING
        VALUE(result) TYPE zrt_payload .
    CLASS-METHODS reset_all_data
      RAISING
        zcx_http_client_failed .
    CLASS-METHODS check_obfuscation_needed
      IMPORTING
        !inetrface_in TYPE zinterface_id
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS retry_limit_exceeded
      EXPORTING
        !et_retry_report TYPE ztt_rest_retry_limi .
    METHODS send_email
      IMPORTING
        !iv_recepient TYPE ad_smtpadr
        !iv_subject   TYPE so_obj_des
        !it_body      TYPE soli_tab
      EXPORTING
        !ew_return    TYPE bapiret2
        !ev_result    TYPE os_boolean .
    CLASS-METHODS check_authority
      RAISING
        zcx_http_client_failed .
    CLASS-METHODS get_global_params
      RETURNING
        VALUE(global_params) TYPE zrest_global .
    CLASS-METHODS retry_log
      IMPORTING
        !message_id TYPE zmid
        !response   TYPE c OPTIONAL
      RAISING
        zcx_http_client_failed .
    CLASS-METHODS write_application_log
      IMPORTING
        !iv_object    TYPE balobj_d
        !iv_subobject TYPE balsubobj
        !iv_extnumber TYPE balnrext
        !it_message   TYPE zrt_applog_message .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA rest_handler TYPE REF TO zcl_rest_framework .
    DATA response TYPE REF TO if_rest_entity .
    DATA request TYPE REF TO if_rest_entity .
    CLASS-DATA payload TYPE zrt_payload .
    CLASS-DATA monitor TYPE zrest_monitor .
    CLASS-DATA interface_name TYPE zinterface_id .
    CLASS-DATA it_zobfuscate TYPE ztobfuscate .

    CLASS-METHODS get_db_data
      IMPORTING
        !message_id TYPE zmid .
    CLASS-METHODS check_messageid
      IMPORTING
        !message_id TYPE zmid .
    CLASS-METHODS obfuscate
      IMPORTING
        !input        TYPE xstring
      RETURNING
        VALUE(result) TYPE xstring .
ENDCLASS.



CLASS zcl_rest_utility_class IMPLEMENTATION.


  METHOD check_authority.

    DATA:lv_textid TYPE scx_t100key.

    AUTHORITY-CHECK OBJECT 'ZREST_AUTH'

    ID 'ZBOOLEAN' FIELD 'X'.
    IF sy-subrc NE 0.
      lv_textid-msgid = 'Z_FI_MDG'.
      lv_textid-msgno = '057'.
      RAISE EXCEPTION TYPE zcx_http_client_failed EXPORTING textid = lv_textid.
    ENDIF.

  ENDMETHOD.


  METHOD check_messageid.
    IF message_id IS INITIAL .
      EXIT.
    ENDIF.
  ENDMETHOD.


  METHOD check_obfuscation_needed.
    SELECT  * FROM zobfuscate INTO TABLE it_zobfuscate
                                    WHERE inetrface EQ inetrface_in.
    IF sy-subrc EQ 0.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.

  ENDMETHOD.


  METHOD download_payload_file.

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

    AUTHORITY-CHECK OBJECT 'ZREST_AUTH'
        ID 'ZBOOLEAN' FIELD 'X'.
    IF sy-subrc NE 0 AND check_obfuscation_needed( inetrface_in = wa_payload-interface_id ) EQ abap_true.
      output = obfuscate( input = wa_payload-payload ).
      wa_payload-payload = output.
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
      IMPORTING
        fullpath = gv_filename.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = gv_filename
        filetype                = 'ASC'
      TABLES
        data_tab                = it_string
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
* Implement suitable error handling here
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
    z_restcall_from_db=>get_calldata_fromdb(
      EXPORTING
        message_id = message_id
      IMPORTING
        payload    = payload
        monitor    = monitor ).
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
      WHEN OTHERS.
        description = ''.
    ENDCASE.

  ENDMETHOD.


  METHOD get_static_headers.
    DATA: wa_ihttpnvp TYPE ihttpnvp,
          lt_headers  TYPE TABLE OF  zrest_conf_head,
          wa_headers  TYPE zrest_conf_head.

    SELECT * FROM zrest_conf_head INTO TABLE lt_headers WHERE interface_id EQ interface_id.

    IF sy-subrc EQ 0.
      LOOP AT lt_headers INTO wa_headers.
        wa_ihttpnvp-name = wa_headers-name.
        wa_ihttpnvp-value = wa_headers-value.
        APPEND wa_ihttpnvp TO static_headers.
        CLEAR  wa_ihttpnvp.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD obfuscate.
************************************************************************************
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
           length_of_word   TYPE i.
************************************************************************************
*   Convert to string                                                              *
************************************************************************************
* Check if the obfuscation is needed and input is not initial
    CHECK : it_zobfuscate IS NOT INITIAL ,
            input IS NOT INITIAL.
*   if yes , convert the xstring to string for the processing
    TRY.
        converted = cl_abap_conv_in_ce=>create(
            input = input ).
      CATCH cx_parameter_invalid_range .
      CATCH cx_sy_codepage_converter_init .
    ENDTRY.
    TRY.
        converted->read(
          IMPORTING
            data = lv_string ).
      CATCH cx_sy_conversion_codepage .
      CATCH cx_sy_codepage_converter_init .
      CATCH cx_parameter_invalid_type .
      CATCH cx_parameter_invalid_range .
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
      IMPORTING
        buffer = result
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDMETHOD.


  METHOD reset_all_data.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04|28|2016|V-DEVEER  |2163894 | DGDK903413 | Authorization Check
*----------------------------------------------------------------------*
**************************************************************************************
    DATA  : it_monitor TYPE STANDARD TABLE OF zrest_monitor,
            it_payload TYPE STANDARD TABLE OF zrest_mo_payload,
            it_headers TYPE STANDARD TABLE OF zrest_mon_header,
            del_date   TYPE sy-datum,
            seconds    TYPE i,
            lv_textid  TYPE REF TO zcx_http_client_failed,
            lv_text2   TYPE scx_t100key.

    CREATE OBJECT lv_textid.
**************************************************************************************
* Go back 30 days in time . All the message before this date would need to           *
* be cleanesed.By default 30 days...                                                 *
**************************************************************************************
    DATA: wa_global TYPE zrest_global.
    wa_global = zcl_rest_utility_class=>get_global_params( ).
    seconds = -1 * wa_global-message_retention  * 24  * 60.
    TRY.
        cl_abap_tstmp=>td_add(
          EXPORTING
            date     = sy-datum
            time     = sy-uzeit
            secs     = seconds
          IMPORTING
            res_date = del_date ).
      CATCH cx_parameter_invalid_type .
      CATCH cx_parameter_invalid_range .
    ENDTRY.
**************************************************************************************
*   Begin of changes VSTF # 2163894 | DGDK903413
    TRY.
        check_authority( ).

      CATCH zcx_http_client_failed INTO lv_textid.
        lv_text2 = lv_textid->if_t100_message~t100key.
        RAISE EXCEPTION TYPE zcx_http_client_failed
          EXPORTING
            textid = lv_text2.
    ENDTRY.
*  End of changes VSTF # 2163894 | DGDK903413
**************************************************************************************
    SELECT * FROM zrest_monitor INTO TABLE it_monitor
                                     WHERE zcompdate < del_date
                                     AND   httpstatus BETWEEN 200 AND 300 .
    IF sy-subrc EQ 0.
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
* 12|08|2016|V-JAVEDA  |2163894 | MS2K948543 | Enhance delete function
*----------------------------------------------------------------------*
* 12|16|2016|V-JAVEDA  |2163894 | MS2K948920 | Performance improve
*----------------------------------------------------------------------*
*12/20/2016| V-JAVEDA  |2278065  | MS2K948978 |Clearing prog header *
*----------------------------------------------------------------------*

    DATA : lv_string               TYPE string,
           wa_paylod               TYPE zrest_mo_payload,
           lv_text2                TYPE scx_t100key,
           lv_textid               TYPE REF TO zcx_http_client_failed,
           lv_rfc_destination      TYPE zrest_config-destination,
           lv_srtfd                TYPE zadf_con_indx-srtfd,
           lw_indx                 TYPE zadf_con_indx,
           lt_enveloped_data       TYPE TABLE OF ssfbin,
           lv_cert_string          TYPE xstring,
           lt_recipients           TYPE TABLE OF ssfinfo,
           lw_recipient            TYPE ssfinfo,
           lt_input_data           TYPE TABLE OF ssfbin,
           lv_env_data_len         TYPE i,
           lv_env_len_total        TYPE i,
           lv_subject              TYPE string,
           lw_enveloped_data       TYPE ssfbin,
           lv_len_output           TYPE i,
           lv_len_input            TYPE i,
           lv_decoded_str          TYPE string,
           lv_applic               TYPE rfcdisplay-sslapplic,
           lv_psename              TYPE ssfpsename,
           lv_profilename          TYPE localfile,
           lv_profile              TYPE ssfparms-pab,
           lv_current_timestamp    TYPE timestampl,
           lv_date_adf             TYPE datum,
           lv_time_adf             TYPE uzeit,
           lv_seconds_adf          TYPE p,
           lv_input_seconds_adf    TYPE p,
           lv_expiry_time_adf      TYPE string,
           lv_new_expiry_adf       TYPE string,
           lv_format               TYPE i,
           lv_string_to_sign       TYPE string,
           lv_encoded_base_address TYPE string,
           lv_body_xstring         TYPE xstring,
           lv_sign                 TYPE string,
           lv_final_token          TYPE string,
           lv_decoded              TYPE xstring,
           lo_conv                 TYPE REF TO cl_abap_conv_out_ce,
           lv_sas_key              TYPE string,
           lw_zadf_config          TYPE zadf_config,
           lv_zone                 TYPE sy-zonlo,
           lv_baseaddress          TYPE string,
           http_method             TYPE zinterface_method.
    CONSTANTS: lc_i          TYPE c VALUE 'I',
               lc_servicebus TYPE zadf_config-interface_type VALUE 'SERVICEBUS',
               lc_eventhub   TYPE zadf_config-interface_type VALUE 'EVENTHUB',
               lc_auth       TYPE ihttpnvp-name VALUE 'Authorization'.
    CREATE OBJECT lv_textid.
*-----------------------check message id ------------------------------*
    check_messageid( message_id = message_id ).
*-----------------------get db data -----------------------------------*
    get_db_data( message_id = message_id ).
* ------------------------get the latest payloadv----------------------*
*   Sort the retry num , so that latest one is picked for reprocessing
    SORT payload BY retry_num DESCENDING.

*    READ TABLE payload INTO DATA(wa_paylod) INDEX 1. v-jobpau
    CLEAR wa_paylod.  "v-javeda | MS2K948978
    READ TABLE payload INTO wa_paylod INDEX 1.
* ---------------------------------------------------------------------*
    interface_name = wa_paylod-interface_id.
    http_method = wa_paylod-method.
* ---------------------------------------------------------------------*
*   Begin of changes for 2163894 | DGDK903413
    TRY.
*        To be uncommented later - sapurana - 6/24
*        check_authority( ).
*        AUTHORITY-CHECK OBJECT 'ZREST_RETR'
*        ID 'ZBOOLEAN' FIELD 'X'.
*        IF sy-subrc NE 0.
*          lv_textid2-msgid = 'Z_FI_MDG'.
*          lv_textid2-msgno = '057'.
*          RAISE EXCEPTION TYPE zcx_http_client_failed EXPORTING textid = lv_textid2.
*        ENDIF.
* End of changes for 2163894 | DGDK903413

      CATCH zcx_http_client_failed INTO lv_textid.
        lv_text2 = lv_textid->if_t100_message~t100key.
        RAISE EXCEPTION TYPE zcx_http_client_failed
          EXPORTING
            textid = lv_text2.
    ENDTRY.
*   End of changes for 2163894 | DGDK903413
*-----------------------Get the config ------------------------------*
    DATA: wa_config TYPE zrest_srtuct_config.

    wa_config = zcl_rest_utility_class=>get_config_data(
        interface_id = interface_name
        method       = http_method ).



*-----------------------Retry possible ? ------------------------------*
    IF wa_config-max_retry EQ 0.
      MESSAGE 'Retry not possible'(001) TYPE 'I'.   " Added the messsage for VSTF 2163894 / DGDK903444
      EXIT.
    ELSEIF wa_paylod-retry_num EQ wa_config-max_retry AND from_scheduler EQ 'X'. "v-javeda - MS2K948543
      MESSAGE 'Maximum number of retry attempts reached'(002) TYPE 'I'. " Added the message for VSTF 2163894 / DGDK903444
      EXIT.
    ENDIF.
*----------------------------------------------------------------------*
*    begin of change for the exponential retries.
    IF from_scheduler EQ 'X'.
      DATA: wa_global TYPE zrest_global.
      wa_global = zcl_rest_utility_class=>get_global_params( ).

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
          cl_abap_tstmp=>td_subtract(
            EXPORTING
              date1    = sy-datum
              time1    = sy-uzeit
              date2    = monitor-zexedate
              time2    = monitor-zexetime
            IMPORTING
              res_secs = seconds ).
        CATCH cx_parameter_invalid_type .
        CATCH cx_parameter_invalid_range .
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
    DATA: cx_http_failed    TYPE REF TO zcx_http_client_failed,
          cx_config_missing TYPE REF TO zcx_interace_config_missing.
    CREATE OBJECT: cx_http_failed,
                   cx_config_missing.
    TRY .
        CREATE OBJECT rest_handler
          EXPORTING
            interface_name      = interface_name
            business_identifier = wa_paylod-businessid
            method              = wa_config-method.

      CATCH zcx_http_client_failed INTO cx_http_failed.

        RAISE EXCEPTION cx_http_failed.
      CATCH zcx_interace_config_missing INTO cx_config_missing.
        RAISE EXCEPTION cx_config_missing.
    ENDTRY.
*   Set the format in binary as it's already converted.
    IF wa_paylod-payload IS NOT INITIAL.
      rest_handler->zif_rest_framework~set_binary_body( wa_paylod-payload ).
    ENDIF.
    rest_handler->set_callingmethod( 'RETRY' ).
*   Append the headers to the message
    CLEAR lv_string.
    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring  = wa_paylod-programheaders
        im_encoding = 'UTF-8'
      IMPORTING
        ex_string   = lv_string.

    DATA:wa_header     TYPE ihttpnvp,
         result_tab    TYPE TABLE OF string,
         wa_result_tab TYPE string.
    SPLIT lv_string AT '|' INTO TABLE result_tab IN CHARACTER MODE.
*****
**Regenerating SAS key token with new expiry time
    CLEAR lw_zadf_config.
    SELECT SINGLE * FROM zadf_config
             INTO lw_zadf_config
             WHERE interface_id EQ interface_name.
    IF sy-subrc EQ 0.
      CASE lw_zadf_config-interface_type.
        WHEN lc_servicebus OR lc_eventhub. "Azure ServiceBus or eventhub
          lv_srtfd = interface_name.
*Import internal table as a cluster from INDX for decoding SAS Primary key
          IMPORT tab  = lt_enveloped_data[]
                 FROM DATABASE zadf_con_indx(zd)
                 TO lw_indx
                 ID lv_srtfd.
          IF NOT lt_enveloped_data[] IS INITIAL.
            CLEAR lv_rfc_destination.
            SELECT SINGLE destination FROM zrest_config
                                      INTO lv_rfc_destination
                                      WHERE interface_id EQ interface_name.
            IF NOT lv_rfc_destination IS INITIAL .
              CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
                EXPORTING
                  destination             = lv_rfc_destination
                  authority_check         = ' '
                IMPORTING
                  sslapplic               = lv_applic
                EXCEPTIONS
                  authority_not_available = 1
                  destination_not_exist   = 2
                  information_failure     = 3
                  internal_failure        = 4
                  no_http_destination     = 5
                  OTHERS                  = 6.
              IF sy-subrc NE 0.
                MESSAGE TEXT-008 TYPE lc_i.
              ELSE.
                CALL FUNCTION 'SSFPSE_FILENAME'
                  EXPORTING
                    mandt         = sy-mandt
                    context       = 'SSLC'
                    applic        = lv_applic
                  IMPORTING
                    psename       = lv_psename
                  EXCEPTIONS
                    pse_not_found = 1
                    OTHERS        = 2.
                IF NOT lv_psename IS INITIAL.
                  lv_profile = lv_psename.
                  CALL FUNCTION 'SSFC_GET_CERTIFICATE'
                    EXPORTING
                      profile               = lv_profile
                    IMPORTING
                      certificate           = lv_cert_string
                    EXCEPTIONS
                      ssf_krn_error         = 1
                      ssf_krn_nomemory      = 2
                      ssf_krn_nossflib      = 3
                      ssf_krn_invalid_par   = 4
                      ssf_krn_nocertificate = 5
                      OTHERS                = 6.
                  IF sy-subrc NE 0.
**Adding complete profile path for reading certificate instance
                    lv_profile = lv_profilename.
                    CALL FUNCTION 'SSFC_GET_CERTIFICATE'
                      EXPORTING
                        profile               = lv_profile
                      IMPORTING
                        certificate           = lv_cert_string
                      EXCEPTIONS
                        ssf_krn_error         = 1
                        ssf_krn_nomemory      = 2
                        ssf_krn_nossflib      = 3
                        ssf_krn_invalid_par   = 4
                        ssf_krn_nocertificate = 5
                        OTHERS                = 6.
                    IF sy-subrc NE 0.
                      MESSAGE TEXT-007 TYPE lc_i.
                    ENDIF.
                  ELSE.
                    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
                      EXPORTING
                        certificate         = lv_cert_string
                      IMPORTING
                        subject             = lv_subject
                      EXCEPTIONS
                        ssf_krn_error       = 1
                        ssf_krn_nomemory    = 2
                        ssf_krn_nossflib    = 3
                        ssf_krn_invalid_par = 4
                        OTHERS              = 5.
                    IF sy-subrc NE 0.
                      MESSAGE TEXT-006 TYPE lc_i.
                    ELSE.
                      lw_recipient-id      = lv_subject.
                      lw_recipient-profile = lv_profile.
                      APPEND lw_recipient TO lt_recipients.
                      LOOP AT lt_enveloped_data INTO lw_enveloped_data.
                        lv_env_data_len = xstrlen( lw_enveloped_data-bindata ).
                        lv_env_len_total = lv_env_len_total + lv_env_data_len.
                        CLEAR lw_enveloped_data.
                      ENDLOOP.
                      CALL FUNCTION 'SSF_KRN_DEVELOPE'
                        EXPORTING
                          ssftoolkit                   = 'SAPSECULIB'
                          str_format                   = 'PKCS7'
                          ostr_enveloped_data_l        = lv_env_len_total
                        IMPORTING
                          ostr_output_data_l           = lv_len_input
                        TABLES
                          ostr_enveloped_data          = lt_enveloped_data
                          recipient                    = lt_recipients
                          ostr_output_data             = lt_input_data
                        EXCEPTIONS
                          ssf_krn_error                = 1
                          ssf_krn_noop                 = 2
                          ssf_krn_nomemory             = 3
                          ssf_krn_opinv                = 4
                          ssf_krn_nossflib             = 5
                          ssf_krn_recipient_error      = 6
                          ssf_krn_input_data_error     = 7
                          ssf_krn_invalid_par          = 8
                          ssf_krn_invalid_parlen       = 9
                          ssf_fb_input_parameter_error = 10
                          OTHERS                       = 11.
                      IF sy-subrc NE 0.
                        MESSAGE TEXT-005 TYPE lc_i.
                      ELSE.
                        IF NOT lt_input_data[] IS INITIAL.
                          CALL FUNCTION 'SCMS_BINARY_TO_STRING'
                            EXPORTING
                              input_length  = lv_len_input
                            IMPORTING
                              text_buffer   = lv_decoded_str
                              output_length = lv_len_output
                            TABLES
                              binary_tab    = lt_input_data
                            EXCEPTIONS
                              failed        = 1
                              OTHERS        = 2.
                          IF lv_decoded_str IS INITIAL.
                            MESSAGE TEXT-004 TYPE lc_i.
                          ELSE.
                            lv_sas_key = lv_decoded_str.
                          ENDIF.
                        ELSE.
                          MESSAGE TEXT-003 TYPE lc_i.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSE.
              MESSAGE TEXT-002 TYPE lc_i.
            ENDIF.
          ELSE.
            MESSAGE TEXT-001 TYPE lc_i.
          ENDIF.
**Calculating Epoch time for SAS Token
          IF NOT lv_sas_key IS INITIAL.
*Get the current timestamp
            GET TIME STAMP FIELD  lv_current_timestamp .
*Get the time difference
            CONVERT TIME STAMP lv_current_timestamp TIME ZONE lv_zone INTO DATE lv_date_adf TIME lv_time_adf.
            TRY.
                CALL METHOD cl_abap_tstmp=>td_subtract
                  EXPORTING
                    date1    = lv_date_adf
                    time1    = lv_time_adf
                    date2    = '19700101'
                    time2    = '000000'
                  IMPORTING
                    res_secs = lv_seconds_adf.
* Add expiry time in seconds
                lv_input_seconds_adf =  900 . "Setting expiry time as 15 mins
                lv_seconds_adf = lv_seconds_adf + lv_input_seconds_adf.
                lv_expiry_time_adf = lv_seconds_adf.
                CONDENSE lv_expiry_time_adf.
              CATCH cx_parameter_invalid_type.
              CATCH cx_parameter_invalid_range .
            ENDTRY.
**Generating SAS token with new expiry time
            IF NOT lv_expiry_time_adf IS INITIAL.
              lv_baseaddress = lw_zadf_config-uri.
              lv_format = 18.
              lv_encoded_base_address = escape( val = lv_baseaddress format = lv_format  ).
              CONCATENATE lv_encoded_base_address  cl_abap_char_utilities=>newline lv_expiry_time_adf INTO lv_string_to_sign.

              lo_conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
              lo_conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = lv_body_xstring ).

              lo_conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
              lo_conv->convert( EXPORTING data = lv_sas_key IMPORTING buffer = lv_decoded ).
              TRY.
                  CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
                    EXPORTING
                      if_algorithm     = 'sha-256'
                      if_key           = lv_decoded
                      if_data          = lv_body_xstring
                      if_length        = 0
                    IMPORTING
                      ef_hmacb64string = lv_sign.
                CATCH cx_abap_message_digest.
              ENDTRY.
              lv_new_expiry_adf = lv_expiry_time_adf.
              CONDENSE lv_new_expiry_adf.
              IF NOT lv_sign IS INITIAL.
                DATA wa_policy TYPE zadf_ehub_policy.
                SELECT SINGLE * FROM zadf_ehub_policy INTO wa_policy WHERE interface_id EQ interface_name.
                lv_sign = escape( val = lv_sign format = lv_format  ).
                IF lw_zadf_config-interface_type eq lc_servicebus. "Servicebus signature string
                   CONCATENATE 'SharedAccessSignature sig=' lv_encoded_base_address  '&sig=' lv_sign '&se=' lv_new_expiry_adf '&skn='
                   wa_policy-policy INTO lv_final_token.
                ELSEIF lw_zadf_config-interface_type eq lc_eventhub. "Eventhub signature string
                   CONCATENATE 'SharedAccessSignature sr=' lv_encoded_base_address  '&sig=' lv_sign '&se=' lv_new_expiry_adf '&skn='
                   wa_policy-policy INTO lv_final_token.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDIF.
    LOOP AT result_tab INTO wa_result_tab.
      SPLIT wa_result_tab AT ':' INTO wa_header-name wa_header-value.
**Replacing Authorization header value with newly generated SAS Token
      IF ( lw_zadf_config-interface_type EQ lc_servicebus OR lw_zadf_config-interface_type EQ lc_eventhub ) AND
         ( wa_header-name EQ lc_auth ) AND
         ( NOT lv_final_token IS INITIAL ).
        rest_handler->set_request_header( iv_name = wa_header-name  iv_value = lv_final_token ).
      ELSE.
        rest_handler->set_request_header( iv_name = wa_header-name  iv_value = wa_header-value  ).
      ENDIF.
    ENDLOOP.
    rest_handler->set_callingprogram( 'ZCL_REST_UTILITY_CLASS' ).
    lv_string = wa_paylod-uri.
    rest_handler->set_uri( lv_string ).
*   Retry
    response = rest_handler->zif_rest_framework~execute( io_entity  = request
                                                         async      = abap_false
                                                         is_retry   = abap_true
                                                         messageid  = message_id
                                                         retry_count = wa_paylod-retry_num ).

    FREE rest_handler.  "v-javeda -    MS2K948920
  ENDMETHOD.


  METHOD retry_limit_exceeded.

    DATA:it_data            TYPE TABLE OF zrest_monitor,
         it_retrynum        TYPE TABLE OF zrest_conf_misc,
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
          send_email(
            EXPORTING
              iv_recepient = lv_recepient
              iv_subject   = lv_subject
              it_body      = lt_body_email
            IMPORTING
              ew_return    = lw_return
              ev_result    = lv_result ).

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
          lt_retries    TYPE TABLE OF zrest_retries,
          wa_retries    TYPE zrest_retries.

    SELECT * FROM zrest_retries INTO TABLE lt_retries
      WHERE zmessageid = message_id.
    IF sy-subrc EQ 0.
      SORT lt_retries BY retry_num DESCENDING.
    ENDIF.

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
        title       = 'Retry Log'
        html_string = lv_string ).
  ENDMETHOD.


  METHOD send_email.
    DATA: lo_send_request    TYPE REF TO cl_bcs.
    DATA: lv_subject       TYPE so_obj_des,
          lv_recepient     TYPE adr6-smtp_addr,
          lt_body          TYPE soli_tab,
          lo_recipient     TYPE REF TO if_recipient_bcs,
          lo_document      TYPE REF TO cl_document_bcs,
          lx_bcs_exception TYPE REF TO cx_bcs,
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
        lo_send_request->set_send_immediately( 'X' ).
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
*----------------------------------------------------------------------*
    DATA : wa_payload TYPE zrest_mo_payload,
           it_payload TYPE STANDARD TABLE OF zrest_mo_payload.
    DATA : lv_content LIKE wa_payload-payload,
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
*   Select the response data .
    SELECT * FROM zrest_mo_payload INTO TABLE it_payload WHERE messageid EQ message_id.
    IF sy-subrc EQ 0.
    ELSE.
      EXIT.
    ENDIF.

*   Based on the response indicator ..set the payload
    CLEAR lv_content.
    IF response EQ abap_true.
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
    ELSE.
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

      DATA: l_exception_error TYPE REF TO cx_st_error.
      DATA: l_exception_rt_error TYPE REF TO cx_xslt_runtime_error.
      DATA: lv_content_cpy LIKE wa_payload-payload.

      lv_content_cpy = lv_content.

      TRY.
          CALL TRANSFORMATION sjson2html  SOURCE XML lv_content RESULT XML lv_content.
        CATCH: cx_st_error           INTO l_exception_error.
          lv_content = lv_content_cpy.
        CATCH: cx_xslt_runtime_error INTO l_exception_rt_error.
          lv_content = lv_content_cpy.
      ENDTRY.

    ENDIF.

    CALL METHOD cl_abap_browser=>show_html( html_xstring = lv_content ).

  ENDMETHOD.


  METHOD show_submitted_headers.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04|28|2016|V-DEVEER  |2163894 | DGDK903413 | Authorization Check
*----------------------------------------------------------------------*
    DATA : lv_string  TYPE string.
    TYPES : BEGIN OF ty_string,
              line TYPE string,
            END OF ty_string.

    DATA : wa_payload TYPE zrest_mo_payload,
           it_payload TYPE STANDARD TABLE OF zrest_mo_payload,
           wa_string  TYPE ty_string,
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
          READ TABLE it_payload INTO wa_payload INDEX 1.
          IF check_obfuscation_needed( inetrface_in = wa_payload-interface_id ) EQ abap_true.
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
          html_string = lv_string ).

    ENDIF.

  ENDMETHOD.


  METHOD unprocessed_data.
* Get the data waiting to be processed
    TYPES: BEGIN OF ty_range,
             sign(1)   TYPE c,
             option(2) TYPE c,
             low       TYPE int4,
             high      TYPE  int4,
           END   OF ty_range.

    TYPES: ty_ranges TYPE TABLE OF ty_range.

    DATA: lw_range  TYPE  ty_range,
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
     WHERE   httpstatus IN lr_ranges. "v-javeda MS2K948826
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
               lc_s  TYPE char1      VALUE 'S'.

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
        MESSAGE TEXT-002 TYPE lc_s. "'S'.
      ENDIF.
    ELSE.

*// 'Application Log Creation Failed'
      MESSAGE TEXT-001 TYPE lc_e DISPLAY LIKE lc_s.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
