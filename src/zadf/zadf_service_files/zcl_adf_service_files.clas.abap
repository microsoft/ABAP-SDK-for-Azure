class ZCL_ADF_SERVICE_FILES definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public .

public section.

  methods CREATE_FILE
    importing
      !IV_FOLDER type STRING optional
      !IV_FILE type STRING
      !IV_FILE_LENGTH type I
      value(IV_REQUEST) type XSTRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      value(EV_RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE .
  methods SET_FILE_STORAGE_ACCOUNT
    importing
      !IV_STORAGE_ACCOUNT type STRING
      !IV_FILE_SHARE type STRING .
  methods SET_FILE_ADDITIONAL_ATTRIBUTES
    importing
      !IV_PERMISSION type STRING default 'rwdlc'
      !IV_IDENTIFIER type STRING optional
      !IV_IP type STRING optional
      !IV_SIGNED_SERVICE type STRING optional
      !IV_SIGNED_RESOURCE_TYPE type STRING optional
      !IV_SIGNED_PROTOCOL type STRING optional
      !IV_VERSION type STRING default '2015-04-05'
      !IV_EXPIRY_TIME type STRING optional .
  methods PUT_FILE
    importing
      value(IV_FILE_LENGTH) type I
      value(IV_REQUEST) type XSTRING
      !IV_FOLDER type STRING optional
      !IV_FILE type STRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EV_HTTP_STATUS type I
      !EV_RESPONSE type STRING
    raising
      ZCX_ADF_SERVICE .
  methods GET_FILE
    importing
      !IV_FOLDER_NAME type STRING optional
      !IV_FILE_NAME type STRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EV_HTTP_STATUS type I
      !EV_RESPONSE type XSTRING
    raising
      ZCX_ADF_SERVICE .
  methods CREATE_DIRECTORY
    importing
      !IV_PARENT_DIR_PATH type STRING optional
      !IV_DIR_NAME type STRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EV_RESPONSE type STRING
      !EV_HTTP_STATUS type I
    raising
      ZCX_ADF_SERVICE .
  methods LIST_DIRECTORIES_FILES
    importing
      !IV_DIR_PATH type STRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EV_HTTP_STATUS type I
      !ET_FILE_LIST type ZADF_T_FILE_LIST
    raising
      ZCX_ADF_SERVICE .
  methods DELETE_FILE
    importing
      !IV_DIR_PATH type STRING optional
      !IV_FILE_NAME type STRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EV_HTTP_STATUS type I
      !EV_RESPONSE type STRING
    raising
      ZCX_ADF_SERVICE .
  PROTECTED SECTION.

    METHODS get_sas_token
         REDEFINITION .
private section.

  data GV_IDENTIFIER type STRING .
  data GV_IP type STRING .
  data GV_VERSION type STRING .
  data GV_PERMISSION type STRING value 'rwdlc' ##NO_TEXT.
  data GV_START_UTC_TIME type STRING .
  data GV_EXPIRY_UTC_TIME type STRING .
  data GV_FOLDER type STRING .
  data GV_SIGNED_SERVICE type STRING value 'f' ##NO_TEXT.
  data GV_SIGNED_RESOURCE_TYPE type STRING value 'sco' ##NO_TEXT.
  data GV_SIGNED_PROTOCOL type STRING value 'https' ##NO_TEXT.
  constants GC_SEP_SLASH type CHAR1 value '/' ##NO_TEXT.
  constants GC_FILE type STRING value 'file' ##NO_TEXT.
  constants GC_UTC_ZONE type TZNZONE value 'UTC' ##NO_TEXT.
  constants GC_SEP_HYPHEN type CHAR1 value '-' ##NO_TEXT.
  constants GC_SEP_COLON type CHAR1 value ':' ##NO_TEXT.
  data GV_FILE_SHARE type STRING .
  data GV_STORAGE_ACCOUNT type STRING .

  methods STRING_TO_SIGN
    returning
      value(RV_STRING_TO_SIGN) type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_ADF_SERVICE_FILES .
  methods VALIDATE_ATTRIBUTES
    raising
      ZCX_ADF_SERVICE_FILES .
  methods SET_EXPIRY_UTC_TIME
    raising
      ZCX_ADF_SERVICE .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_FILES IMPLEMENTATION.


  METHOD create_directory.

    DATA:lv_parent_dir TYPE string,
         lv_dir        TYPE string.

    IF go_rest_api IS BOUND.

      TRY.
          DATA(lv_token) = get_sas_token( iv_baseaddress = gv_uri ).
        CATCH zcx_adf_service INTO DATA(lcx_adf_service).
          DATA(lv_msg) =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.

*    Set URI
      lv_dir =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_dir_name ).

      IF iv_parent_dir_path IS INITIAL.
        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash
                    lv_dir lv_token INTO DATA(lv_uri).
      ELSE.
        lv_parent_dir =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_parent_dir_path ).

        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash lv_parent_dir gc_sep_slash
                    lv_dir lv_token INTO lv_uri.
      ENDIF.

      CONCATENATE lv_uri '&restype=directory' INTO lv_uri.

      go_rest_api->set_uri( uri = lv_uri ).

**** Set Headers
      DATA(lt_headers) = VALUE tihttpnvp( ( name = 'x-ms-version'        value = gv_version )
                                          ( name = 'x-ms-date'           value = gv_start_utc_time )
                                          ( name = 'Content-Length'      value = '0' ) ).

      IF it_headers[] IS NOT INITIAL.
        APPEND LINES OF it_headers TO lt_headers.
      ENDIF.

      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = lt_headers[] ).

***Rest API call to get response from Azure Destination
**** Get reference
      DATA(lo_response) = go_rest_api->zif_rest_framework~execute( async = gv_asynchronous
                                                                is_retry = gv_is_try ).

      ev_http_status = go_rest_api->get_status( ).

      IF lo_response IS BOUND.
        ev_response = lo_response->get_string_data( ).
        go_rest_api->close( ).
      ELSE.
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service_blob=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_file.

    DATA:lv_file   TYPE string,
         lv_folder TYPE string.

    IF go_rest_api IS BOUND.

      TRY .
          DATA(token) = get_sas_token( iv_baseaddress =  gv_uri ).
        CATCH zcx_adf_service INTO DATA(lcx_adf_service).
          DATA(lv_msg) =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.


      lv_file =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_file ).

      IF iv_folder IS INITIAL.
        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash lv_file token INTO DATA(lv_uri).
      ELSE.
        lv_folder =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_folder ).

        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash
                    lv_folder gc_sep_slash lv_file token INTO lv_uri.
      ENDIF.

      go_rest_api->set_uri( uri = lv_uri ).

      DATA(lt_headers) = VALUE tihttpnvp( ( name = 'x-ms-version'        value = gv_version )
                                          ( name = 'x-ms-date'           value = gv_start_utc_time )
                                          ( name = 'x-ms-type'           value = 'file' )
                                          ( name = 'x-ms-content-length' value = iv_file_length )
                                          ( name = 'Content-Length'      value = '0' )
                                          ( name = 'x-ms-content-type'   value = 'application/octet-stream' ) ).

      IF it_headers[] IS NOT INITIAL.
        APPEND LINES OF it_headers TO lt_headers.
      ENDIF.

      go_rest_api->set_request_headers( it_header_fields = lt_headers[] ).

      DATA(lo_response) = go_rest_api->zif_rest_framework~execute( async = gv_asynchronous
                                                                is_retry = gv_is_try ).

      ev_http_status = go_rest_api->get_status( ).

      IF lo_response IS BOUND.
        ev_response = lo_response->get_string_data( ).
        go_rest_api->close( ).
      ELSE.
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD delete_file.
    DATA:lv_file   TYPE string,
         lv_folder TYPE string.

    IF go_rest_api IS BOUND.

**** Get Authorization
      TRY.
          DATA(token) = get_sas_token( iv_baseaddress = gv_uri ).
        CATCH zcx_adf_service INTO DATA(lcx_adf_service).
          DATA(lv_msg) =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.

*    Set URI
      lv_file =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_file_name ).

      IF iv_dir_path IS INITIAL.
        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash lv_file token INTO DATA(lv_uri).
      ELSE.
        lv_folder =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_dir_path ).

        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash
                    lv_folder gc_sep_slash lv_file token INTO lv_uri.
      ENDIF.

      go_rest_api->set_uri( uri = lv_uri ).

**** Set Headers
      DATA(lt_headers) = VALUE tihttpnvp( ( name = 'x-ms-version'        value = gv_version )
                                          ( name = 'x-ms-date'           value = gv_start_utc_time ) ).

      IF it_headers[] IS NOT INITIAL.
        APPEND LINES OF it_headers TO lt_headers.
      ENDIF.

      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = lt_headers[] ).


***Rest API call to get response from Azure Destination
**** Get reference
      DATA(lo_response) = go_rest_api->zif_rest_framework~execute( async = gv_asynchronous
                                                                is_retry = gv_is_try ).

      ev_http_status = go_rest_api->get_status( ).

      IF lo_response IS BOUND.
        ev_response = lo_response->get_string_data( ).
        go_rest_api->close( ).
      ELSE.
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service_blob=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_file.

    DATA:lv_file   TYPE string,
         lv_folder TYPE string.

    IF go_rest_api IS BOUND.

**** Get Authorization
      TRY.
          DATA(token) = get_sas_token( iv_baseaddress = gv_uri ).
        CATCH zcx_adf_service INTO DATA(lcx_adf_service).
          DATA(lv_msg) =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.

*    Set URI
      lv_file =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_file_name ).

      IF iv_folder_name IS INITIAL.
        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash lv_file token INTO DATA(lv_uri).
      ELSE.
        lv_folder =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_folder_name ).

        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash
                    lv_folder gc_sep_slash lv_file token INTO lv_uri.
      ENDIF.

      go_rest_api->set_uri( uri = lv_uri ).

**** Set Headers
      DATA(lt_headers) = VALUE tihttpnvp( ( name = 'x-ms-version'        value = gv_version )
                                          ( name = 'x-ms-date'           value = gv_start_utc_time ) ).

      IF it_headers[] IS NOT INITIAL.
        APPEND LINES OF it_headers TO lt_headers.
      ENDIF.

      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = lt_headers[] ).


***Rest API call to get response from Azure Destination
**** Get reference
      DATA(lo_response) = go_rest_api->zif_rest_framework~execute( async = gv_asynchronous
                                                                is_retry = gv_is_try ).

      ev_http_status = go_rest_api->get_status( ).

      IF lo_response IS BOUND.
        ev_response = lo_response->get_binary_data( ).
        go_rest_api->close( ).
      ELSE.
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service_blob=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_sas_token.
    DATA : body_xstring      TYPE xstring,
           sign              TYPE string,
           final_token       TYPE string,
           lv_decoded_xstr   TYPE xstring,
           conv              TYPE REF TO cl_abap_conv_out_ce,
           lv_sas_key        TYPE string,
           lv_format         TYPE i,
           lv_string_to_sign TYPE string.

    CONSTANTS:lc_equal(1) VALUE '=',
              lc_q(1)     VALUE '?',
              lc_amp(1)   VALUE '&',
              lc_sv       TYPE string VALUE 'sv',
              lc_ss(2)    VALUE 'ss',
              lc_srt(3)   VALUE 'srt',
              lc_sp(2)    VALUE 'sp',
              lc_st(2)    VALUE 'st',
              lc_se(2)    VALUE 'se',
              lc_spr(3)   VALUE 'spr',
              lc_sig(3)   VALUE 'sig'.

    conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

*   Calling String to sign
    TRY.
        CALL METHOD me->string_to_sign
          RECEIVING
            rv_string_to_sign = lv_string_to_sign.
      CATCH zcx_adf_service_files INTO DATA(lx_adf_files).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid = zcx_adf_service=>file_validation_failed
            text   = lx_adf_files->get_text( ).
    ENDTRY.

    conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = body_xstring ).

    DEFINE encrypt_key.
      decode_sign( receiving rv_secret = lv_sas_key ).

      IF gv_sas_key IS INITIAL.
        lv_sas_key = read_ssf_key( ).
      ELSE.
        lv_sas_key = read_key( ).
      ENDIF.

      CALL FUNCTION 'SSFC_BASE64_DECODE'
        EXPORTING
          b64data                  = lv_sas_key
        IMPORTING
          bindata                  = lv_decoded_xstr
        EXCEPTIONS
          ssf_krn_error            = 1
          ssf_krn_noop             = 2
          ssf_krn_nomemory         = 3
          ssf_krn_opinv            = 4
          ssf_krn_input_data_error = 5
          ssf_krn_invalid_par      = 6
          ssf_krn_invalid_parlen   = 7
          OTHERS                   = 8.
      IF sy-subrc <> 0.
*       Implement suitable error handling here
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>error_in_sas_key_encryption
            interface_id = gv_interface_id.
      ENDIF.

      CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
        EXPORTING
          if_algorithm     = 'sha-256'
          if_key           = lv_decoded_xstr "decoded
          if_data          = body_xstring
          if_length        = 0
        IMPORTING
          ef_hmacb64string = sign.
      CLEAR : lv_sas_key,lv_decoded_xstr.
    END-OF-DEFINITION.

    encrypt_key.

    IF NOT sign IS INITIAL.
      lv_format = 18.
      sign = escape( val = sign format = lv_format ).
      gv_start_utc_time = escape( val = gv_start_utc_time format = lv_format ).
      gv_expiry_utc_time = escape( val = gv_expiry_utc_time format = lv_format ).

      IF NOT gv_expiry_utc_time IS INITIAL.
        CONCATENATE lc_q
                    lc_amp lc_sv  lc_equal gv_version
                    lc_amp lc_ss  lc_equal gv_signed_service
                    lc_amp lc_srt lc_equal gv_signed_resource_type
                    lc_amp lc_sp  lc_equal gv_permission
                    lc_amp lc_st  lc_equal gv_start_utc_time
                    lc_amp lc_se  lc_equal gv_expiry_utc_time
                    lc_amp lc_spr lc_equal gv_signed_protocol
                    lc_amp lc_sig lc_equal sign
                    INTO final_token.

        CONCATENATE rv_sas_token final_token INTO rv_sas_token.

      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>sas_key_not_generated
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


  METHOD list_directories_files.

    DATA:lv_dir_path TYPE string,
         lv_xdata    TYPE xstring.

    DATA: lt_xml_info  TYPE TABLE OF smum_xmltb,
          ls_file_list TYPE zadf_file_type,
          lt_return    TYPE TABLE OF bapiret2.

    IF go_rest_api IS BOUND.

**** Get Authorization
      TRY.
          DATA(token) = get_sas_token( iv_baseaddress = gv_uri ).
        CATCH zcx_adf_service INTO DATA(lcx_adf_service).
          DATA(lv_msg) =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.

*    Set URI
      lv_dir_path =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_dir_path ).

      CONCATENATE gc_sep_slash gv_file_share gc_sep_slash
                  lv_dir_path token INTO DATA(lv_uri).

      CONCATENATE lv_uri '&restype=directory&comp=list' INTO lv_uri.

      go_rest_api->set_uri( uri = lv_uri ).

**** Set Headers
      DATA(lt_headers) = VALUE tihttpnvp( ( name = 'x-ms-version'        value = gv_version )
                                          ( name = 'x-ms-date'           value = gv_start_utc_time ) ).

      IF it_headers[] IS NOT INITIAL.
        APPEND LINES OF it_headers TO lt_headers.
      ENDIF.

      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = lt_headers[] ).


***Rest API call to get response from Azure Destination
**** Get reference
      DATA(lo_response) = go_rest_api->zif_rest_framework~execute( async = gv_asynchronous
                                                                is_retry = gv_is_try ).

      ev_http_status = go_rest_api->get_status( ).

      IF lo_response IS BOUND.
        DATA(lv_response) = lo_response->get_string_data( ).

        IF ev_http_status = 200.
          CALL TRANSFORMATION zadf_service_files_list
            SOURCE XML lv_response
              RESULT XML  lv_xdata.

          CALL FUNCTION 'SMUM_XML_PARSE'
            EXPORTING
              xml_input = lv_xdata
            TABLES
              xml_table = lt_xml_info
              return    = lt_return
            EXCEPTIONS
              OTHERS    = 0.
          IF sy-subrc EQ 0.
            DELETE lt_xml_info WHERE type NE 'V'.
          ENDIF.

          LOOP AT lt_xml_info INTO DATA(ls_xml_info).
            ls_file_list-name = ls_xml_info-cvalue.
            IF ls_xml_info-cname = 'File'.
              ls_file_list-type = 'F'.
            ELSE.
              ls_file_list-type = 'D'.
            ENDIF.
            APPEND ls_file_list TO et_file_list.
            CLEAR ls_file_list.
          ENDLOOP.
        ENDIF.

        go_rest_api->close( ).
      ELSE.
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service_blob=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD put_file.

    DATA:lv_file   TYPE string,
         lv_folder TYPE string.

    IF go_rest_api IS BOUND.
      TRY .
          DATA(token) = get_sas_token( iv_baseaddress =  gv_uri ).
        CATCH zcx_adf_service INTO DATA(lcx_adf_service).
          DATA(lv_msg) =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.

      lv_file =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_file ).

      IF iv_folder IS INITIAL.
        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash lv_file token INTO DATA(lv_uri).
      ELSE.
        lv_folder =  cl_http_utility=>if_http_utility~escape_url( unescaped = iv_folder ).

        CONCATENATE gc_sep_slash gv_file_share gc_sep_slash
                    lv_folder gc_sep_slash lv_file token INTO lv_uri.
      ENDIF.

      CONCATENATE lv_uri '&comp=range' INTO lv_uri.

      go_rest_api->set_uri( uri = lv_uri ).

      DATA(lv_range) = iv_file_length - 1.

      DATA(lv_range_c) = 'bytes=0-' && lv_range.

      DATA(lt_headers) = VALUE tihttpnvp( ( name = 'x-ms-version'   value = gv_version )
                                          ( name = 'x-ms-date'      value = gv_start_utc_time )
                                          ( name = 'x-ms-write'     value = 'update' )
                                          ( name = 'x-ms-range'     value = lv_range_c )
                                          ( name = 'Content-Length' value = iv_file_length ) ).

      IF it_headers[] IS NOT INITIAL.
        APPEND LINES OF it_headers TO lt_headers.
      ENDIF.

      go_rest_api->set_request_headers( it_header_fields = lt_headers[] ).

      go_rest_api->set_binary_body( body = iv_request ).

      DATA(lo_response) = go_rest_api->zif_rest_framework~execute( async = gv_asynchronous
                                                                is_retry = gv_is_try ).

      ev_http_status = go_rest_api->get_status( ).

      IF lo_response IS BOUND.
        ev_response = lo_response->get_string_data( ).
        go_rest_api->close( ).
      ELSE.
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.

    CLEAR go_rest_api.
  ENDMETHOD.


  METHOD set_expiry_utc_time.
    DATA : lv_current_timestamp TYPE timestamp,
           lv_date              TYPE sy-datum,
           lv_time_out          TYPE timestamp,
           lv_total_sec(16)     TYPE p,
           lv_time              TYPE sy-uzeit.

*   Get the current timestamp in UTC
    GET TIME STAMP FIELD  lv_current_timestamp .

    CONVERT TIME STAMP lv_current_timestamp TIME ZONE gc_utc_zone INTO DATE lv_date TIME lv_time.

    CONCATENATE lv_date+0(4) gc_sep_hyphen lv_date+4(2) gc_sep_hyphen lv_date+6(2)
                'T' lv_time+0(2) gc_sep_colon lv_time+2(2) gc_sep_colon lv_time+4(2) 'Z'
                INTO gv_start_utc_time.

    lv_total_sec = ( ( gv_expiry_hour * 60 ) * 60 ) + ( gv_expiry_min * 60 ) + ( gv_expiry_sec ).

    IF NOT lv_total_sec IS INITIAL.
      CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
        EXPORTING
          timestamp_in    = lv_current_timestamp
          timezone        = 'UTC'
          duration        = lv_total_sec
          unit            = 'S'
        IMPORTING
          timestamp_out   = lv_time_out
        EXCEPTIONS
          timestamp_error = 1
          OTHERS          = 2.

      IF sy-subrc <> 0.
*       Implement suitable error handling here
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>expiry_utc_time_not_set
            interface_id = gv_interface_id.
      ENDIF.
      CLEAR: lv_date,lv_time.

      CONVERT TIME STAMP lv_time_out TIME ZONE gc_utc_zone INTO DATE lv_date TIME lv_time.

      CONCATENATE lv_date+0(4) gc_sep_hyphen lv_date+4(2) gc_sep_hyphen lv_date+6(2)
                  'T' lv_time+0(2) gc_sep_colon lv_time+2(2) gc_sep_colon lv_time+4(2) 'Z'
                  INTO gv_expiry_utc_time.

      IF gv_expiry_utc_time IS INITIAL.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>expiry_utc_time_not_set
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_file_additional_attributes.
* Setting the Global Attributes for Blob Details.
    IF iv_permission IS NOT INITIAL.
      gv_permission = iv_permission.
    ENDIF.
    IF iv_expiry_time IS NOT INITIAL.
      gv_expiry_utc_time = iv_expiry_time.
    ENDIF.
    IF iv_identifier IS NOT INITIAL.
      gv_identifier = iv_identifier.
    ENDIF.
    IF iv_ip IS NOT INITIAL.
      gv_ip = iv_ip.
    ENDIF.
    IF iv_version IS NOT INITIAL.
      gv_version = iv_version.
    ENDIF.
    IF iv_signed_service IS NOT INITIAL.
      gv_signed_service = iv_signed_service.
    ENDIF.
    IF iv_signed_resource_type IS NOT INITIAL.
      gv_signed_resource_type = iv_signed_resource_type.
    ENDIF.
    IF iv_signed_protocol IS NOT INITIAL.
      gv_signed_protocol = iv_signed_protocol.
    ENDIF.
  ENDMETHOD.


  METHOD set_file_storage_account.
    gv_storage_account = iv_storage_account.
    gv_file_share      = iv_file_share.
  ENDMETHOD.


  METHOD string_to_sign.
    DATA : lv_canonical_str TYPE string,
           lv_msg           TYPE string,
           lcx_adf_service  TYPE REF TO zcx_adf_service.
    CLEAR: gv_string_to_sign, gv_start_utc_time.

*   Validate File attributes
    CALL METHOD me->validate_attributes.

*  if the exception is raised, it is passed on to this method.
    TRY.
        IF gv_expiry_utc_time IS INITIAL.
          set_expiry_utc_time( ).
        ENDIF.
      CATCH zcx_adf_service INTO lcx_adf_service.
        lv_msg =  lcx_adf_service->get_text( ).
        MESSAGE lv_msg TYPE 'I'.
    ENDTRY.

    CONCATENATE gv_storage_account      cl_abap_char_utilities=>newline
                gv_permission           cl_abap_char_utilities=>newline
                gv_signed_service       cl_abap_char_utilities=>newline
                gv_signed_resource_type cl_abap_char_utilities=>newline
                gv_start_utc_time       cl_abap_char_utilities=>newline
                gv_expiry_utc_time      cl_abap_char_utilities=>newline
                gv_ip                   cl_abap_char_utilities=>newline
                gv_signed_protocol      cl_abap_char_utilities=>newline
                gv_version              cl_abap_char_utilities=>newline INTO rv_string_to_sign.

    IF  rv_string_to_sign IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>string_to_sign_not_generated
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


  METHOD validate_attributes.
*   Validate Mandatory Attributes for Azure Files
*   Check Storage Account
    IF gv_storage_account IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service_files
        EXPORTING
          textid       = zcx_adf_service_files=>storage_account_not_set
          interface_id = gv_interface_id.
    ENDIF.

*   Check File Share
    IF gv_file_share IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service_files
        EXPORTING
          textid       = zcx_adf_service_files=>file_share_not_set
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
