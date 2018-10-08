CLASS zcl_adf_service_blob DEFINITION
  PUBLIC
  INHERITING FROM zcl_adf_service
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_adf_service_factory.

  PUBLIC SECTION.

    METHODS string_to_sign
      IMPORTING
        !iv_permisson             TYPE string DEFAULT 'racwdl'
        !iv_expiry_time           TYPE string OPTIONAL
        VALUE(iv_storage_account) TYPE string
        VALUE(iv_container)       TYPE string
        VALUE(iv_blob_name)       TYPE string
        VALUE(iv_blob_type)       TYPE char1 DEFAULT 'A'
        !iv_identifier            TYPE string OPTIONAL
        !iv_ip                    TYPE string OPTIONAL
        !iv_protocol              TYPE string OPTIONAL
        !iv_version               TYPE string DEFAULT '2016-05-31'
        !iv_rscc                  TYPE string OPTIONAL
        !iv_rscd                  TYPE string OPTIONAL
        !iv_rsce                  TYPE string OPTIONAL
        !iv_rsct                  TYPE string OPTIONAL
      RAISING
        zcx_adf_service .
    METHODS create_append_blob
      EXPORTING
        VALUE(ev_response)    TYPE string
        VALUE(ev_http_status) TYPE i
      RAISING
        zcx_adf_service .

    METHODS send
        REDEFINITION .
  PROTECTED SECTION.

    METHODS get_sas_token
        REDEFINITION .
  PRIVATE SECTION.

    CONSTANTS gc_utc_zone TYPE tznzone VALUE 'UTC'.         "#EC NOTEXT
    CONSTANTS gc_sep_hyphen TYPE char1 VALUE '-'.           "#EC NOTEXT
    CONSTANTS gc_sep_colon TYPE char1 VALUE ':'.            "#EC NOTEXT
    DATA gv_start_utc_time TYPE string .
    DATA gv_expiry_utc_time TYPE string .
    DATA gv_permisson TYPE string VALUE 'racwdl'.           "#EC NOTEXT
    DATA gv_blob_name TYPE string .
    DATA gv_container_name TYPE string .
    DATA gv_blob_type TYPE string .
    DATA gc_block_blob TYPE string VALUE 'BlockBlob'.       "#EC NOTEXT
    DATA gc_append_blob TYPE string VALUE 'AppendBlob'.     "#EC NOTEXT
    DATA gc_a TYPE char1 VALUE 'A'.                         "#EC NOTEXT
    DATA gc_b TYPE char1 VALUE 'B'.                         "#EC NOTEXT
    DATA gv_sas_token TYPE string .
    CONSTANTS gc_comp_appendblock TYPE string VALUE '&comp=appendblock'. "#EC NOTEXT
    CONSTANTS gc_sep_slash TYPE char1 VALUE '/'.            "#EC NOTEXT
    CONSTANTS gc_blob TYPE string VALUE 'blob'.             "#EC NOTEXT
    DATA gv_storage_account TYPE string .

    METHODS set_expiry_utc_time
      RAISING
        zcx_adf_service .
ENDCLASS.



CLASS zcl_adf_service_blob IMPLEMENTATION.


  METHOD create_append_blob.
    DATA : lo_response     TYPE REF TO if_rest_entity,
           lo_request      TYPE REF TO if_rest_entity,
           lv_sas_token    TYPE string,
           lv_msg          TYPE string,
           lcx_adf_service TYPE REF TO zcx_adf_service.
    IF go_rest_api IS BOUND.
      IF gv_sas_token IS INITIAL.
        TRY.
            get_sas_token( EXPORTING iv_baseaddress = gv_uri
                           RECEIVING rv_sas_token  = lv_sas_token ).
            gv_sas_token = lv_sas_token.
          CATCH zcx_adf_service INTO lcx_adf_service.
            lv_msg =  lcx_adf_service->get_text( ).
            MESSAGE lv_msg TYPE 'I'.
        ENDTRY.
      ENDIF.
      go_rest_api->zif_rest_framework~set_uri( lv_sas_token ).
      add_request_header( iv_name = 'x-ms-blob-type' iv_value = gc_append_blob ).
      add_request_header( iv_name = 'Content-Length' iv_value = '0' ).
      add_request_header( iv_name = 'Content-Type'   iv_value = 'text/plain' ).
**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
      ev_http_status = go_rest_api->get_status( ).
      IF lo_response IS BOUND.
        ev_response = lo_response->get_string_data( ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>error_in_append_blob_creation
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_sas_token.
    DATA : body_xstring    TYPE xstring,
           sign            TYPE string,
           final_token     TYPE string,
           lv_decoded_xstr TYPE xstring,
           conv            TYPE REF TO cl_abap_conv_out_ce,
           lv_sas_key      TYPE string,
           lv_format       TYPE i.
    conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    conv->convert( EXPORTING data = gv_string_to_sign IMPORTING buffer = body_xstring ).
    DEFINE encrypt_key.
      decode_sign( RECEIVING rv_secret = lv_sas_key ).
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
* Implement suitable error handling here
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
        CONCATENATE '?sv=' gv_service_version '&sr=c' '&sig=' sign '&st=' gv_start_utc_time
        '&se=' gv_expiry_utc_time '&sp=' gv_permisson INTO final_token.
        CONCATENATE '/' gv_container_name '/' gv_blob_name INTO rv_sas_token.
        CONCATENATE rv_sas_token final_token INTO rv_sas_token.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>sas_key_not_generated
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


  METHOD send.
    DATA : lo_response     TYPE REF TO if_rest_entity,
           lo_request      TYPE REF TO if_rest_entity,
           lv_sas_token    TYPE string,
           lv_msg          TYPE string,
           lcx_adf_service TYPE REF TO zcx_adf_service.
    IF go_rest_api IS BOUND.
      IF gv_sas_token IS INITIAL.
        TRY.
            get_sas_token( EXPORTING iv_baseaddress = gv_uri
                           RECEIVING rv_sas_token  = lv_sas_token ).
            gv_sas_token = lv_sas_token.
          CATCH zcx_adf_service INTO lcx_adf_service.
            lv_msg =  lcx_adf_service->get_text( ).
            MESSAGE lv_msg TYPE 'I'.
        ENDTRY.
        CASE gv_blob_type.
          WHEN gc_append_blob.
            CONCATENATE gv_sas_token gc_comp_appendblock INTO gv_sas_token.
            go_rest_api->zif_rest_framework~set_uri( gv_sas_token ).
          WHEN gc_block_blob.
            go_rest_api->zif_rest_framework~set_uri( gv_sas_token ).
        ENDCASE.
      ELSE.
        IF NOT gv_sas_token CS gc_comp_appendblock AND
           gv_blob_type EQ gc_append_blob.
          CONCATENATE gv_sas_token gc_comp_appendblock INTO gv_sas_token.
        ENDIF.
        go_rest_api->zif_rest_framework~set_uri( gv_sas_token ).
      ENDIF.
      CASE gv_blob_type.
        WHEN gc_block_blob.
          add_request_header( iv_name = 'x-ms-blob-type' iv_value = gc_block_blob ).
          add_request_header( iv_name = 'Content-Type' iv_value = 'text/plain' ).
        WHEN gc_append_blob.
          add_request_header( iv_name = 'x-ms-version' iv_value = '2016-05-31' ).
          add_request_header( iv_name = 'Content-Length' iv_value = '1048' ).
      ENDCASE.
      go_rest_api->zif_rest_framework~set_binary_body( request ).
**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
      ev_http_status = go_rest_api->get_status( ).
      IF lo_response IS BOUND.
        response = lo_response->get_string_data( ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_expiry_utc_time.
    DATA : lv_current_timestamp TYPE timestamp,
           lv_date              TYPE sy-datum,
           lv_time_out          TYPE timestamp,
           lv_total_sec(16)     TYPE p,
           lv_time              TYPE sy-uzeit.
*Get the current timestamp in UTC
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
* Implement suitable error handling here
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


  METHOD string_to_sign.
    DATA : lv_canonical_str TYPE string,
           lv_msg           TYPE string,
           lcx_adf_service  TYPE REF TO zcx_adf_service.
    CLEAR: gv_string_to_sign, gv_expiry_utc_time,gv_start_utc_time,gv_blob_name,gv_container_name.
    IF iv_expiry_time IS INITIAL.
      TRY.
          set_expiry_utc_time( ).
        CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.
    ELSE.
      gv_expiry_utc_time = iv_expiry_time.
    ENDIF.
    IF iv_blob_type EQ gc_a.
      gv_blob_type = gc_append_blob.
    ELSEIF iv_blob_type EQ gc_b.
      gv_blob_type = gc_block_blob.
    ENDIF.
    CONCATENATE gc_sep_slash gc_blob gc_sep_slash iv_storage_account
                gc_sep_slash iv_container INTO lv_canonical_str.
    gv_storage_account = iv_storage_account.
    gv_container_name  = iv_container.
    gv_blob_name       = iv_blob_name.
    gv_permisson       = iv_permisson.
    gv_service_version = iv_version.
    CONCATENATE  iv_permisson cl_abap_char_utilities=>newline gv_start_utc_time cl_abap_char_utilities=>newline
    gv_expiry_utc_time cl_abap_char_utilities=>newline lv_canonical_str cl_abap_char_utilities=>newline iv_identifier
    cl_abap_char_utilities=>newline iv_ip cl_abap_char_utilities=>newline iv_protocol
    cl_abap_char_utilities=>newline iv_version cl_abap_char_utilities=>newline iv_rscc
    cl_abap_char_utilities=>newline iv_rscd cl_abap_char_utilities=>newline iv_rsce
    cl_abap_char_utilities=>newline iv_rsct cl_abap_char_utilities=>newline INTO gv_string_to_sign.
    IF  gv_string_to_sign IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>string_to_sign_not_generated
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
