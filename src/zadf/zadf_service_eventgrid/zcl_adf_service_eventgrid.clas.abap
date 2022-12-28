class ZCL_ADF_SERVICE_EVENTGRID definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public

  global friends ZCL_ADF_SERVICE_REPROCESS .

public section.

  methods SET_EVENTGRID_SCHEMA
    importing
      !IT_EGRID_SCHEMA type ZADF_TT_EGRID_SCHEMA
    returning
      value(RV_XSTRING) type XSTRING
    raising
      ZCX_ADF_SERVICE .

  methods SEND
    redefinition .
protected section.

  methods GET_SAS_TOKEN
    redefinition .
private section.

  data GT_EGRID_SCHEMA type ZADF_TT_EGRID_SCHEMA .
  constants GC_UTC_ZONE type TZNZONE value 'UTC' ##NO_TEXT.
  constants GC_SEP_HYPHEN type CHAR1 value '-' ##NO_TEXT.
  constants GC_SEP_COLON type CHAR1 value ':' ##NO_TEXT.
  data GV_START_UTC_TIME type STRING .
  data GV_EXPIRY_UTC_TIME type STRING .

  methods SET_EXPIRY_UTC_TIME
    raising
      ZCX_ADF_SERVICE .
  methods CONVERT_TSTAMP_UTC
    importing
      !IM_TIMESTAMP type TIMESTAMP
    returning
      value(RE_UTCTIME) type CHAR100 .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_EVENTGRID IMPLEMENTATION.


  METHOD convert_tstamp_utc.

    CONVERT TIME STAMP im_timestamp TIME ZONE gc_utc_zone INTO DATE DATA(lv_date) TIME DATA(lv_time).
    CONCATENATE lv_date+0(4) gc_sep_hyphen lv_date+4(2) gc_sep_hyphen lv_date+6(2)
                'T' lv_time+0(2) gc_sep_colon lv_time+2(2) gc_sep_colon lv_time+4(2) 'Z'
                INTO re_utctime.
  ENDMETHOD.


  METHOD get_sas_token.
    DATA : lv_string_to_sign    TYPE string,
           encoded_base_address TYPE string,
           body_xstring         TYPE xstring,
           sign                 TYPE string,
           final_token          TYPE string,
           decoded              TYPE xstring,
           conv                 TYPE REF TO cl_abap_conv_out_ce,
           conv_in              TYPE REF TO cl_abap_conv_in_ce,
           lv_decoded_xstr      TYPE xstring,
           format               TYPE i,
           new_expiry           TYPE string,
           lv_sas_key           TYPE string,
           lv_expiry_time       TYPE string,
           lv_baseurl           TYPE string,
           lv_encodedexptime    TYPE string.

    set_expiry_utc_time( ).

    format = 18.
    CONCATENATE 'https://' iv_baseaddress INTO lv_baseurl.

    encoded_base_address = escape( val = lv_baseurl format = format  ).

    lv_encodedexptime    = escape( val = gv_expiry_utc_time format = format  ).

    CONCATENATE 'r=' encoded_base_address '&e=' lv_encodedexptime INTO lv_string_to_sign.

    conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = body_xstring ).

* Encript using SAS key, URL and  Exp Time.
    DEFINE encrypt_key.

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
*    Implement suitable error handling here
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>error_in_sas_key_encryption
            interface_id = gv_interface_id.
      ENDIF.

      CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
        EXPORTING
          if_algorithm     = 'sha-256'
          if_key           = lv_decoded_xstr
          if_data          = body_xstring
          if_length        = 0
        IMPORTING
          ef_hmacb64string = sign.
      CLEAR : lv_sas_key, decoded.
    END-OF-DEFINITION.
    encrypt_key.

* Build Final Token
    IF NOT sign IS INITIAL.
      sign = escape( val = sign format = format  ).
      CONCATENATE 'SharedAccessSignature r=' encoded_base_address '&e=' lv_encodedexptime '&s=' sign INTO final_token.
      rv_sas_token = final_token.
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
           lv_expiry       TYPE string,
           lv_sas_token    TYPE string,
           lv_msg          TYPE string,
           lcx_adf_service TYPE REF TO zcx_adf_service,
           lt_headers1     TYPE tihttpnvp.

    IF go_rest_api IS BOUND.
* Read token from headers for Managed Identity/AAD
      IF line_exists( it_headers[ name = gc_mi_auth ] ).
        DATA(lv_processing_method) = gc_mi_auth.
        lv_sas_token = it_headers[ name = gc_mi_auth ]-value.
      ENDIF.

      TRY.
          CASE lv_processing_method.
            WHEN gc_mi_auth.
            WHEN OTHERS.
              get_sas_token( EXPORTING iv_baseaddress = gv_uri
                             RECEIVING rv_sas_token  = lv_sas_token ).
          ENDCASE.
        CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.

*   Add custom headers.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).
* Add provided SAS token/generated token via Account Key to the headers
      add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ). "lv_sas_token ).

      go_rest_api->zif_rest_framework~set_binary_body( request ).
      IF NOT it_headers[] IS INITIAL.
        go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = it_headers[] ).
      ENDIF.

**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = abap_false ).
      ev_http_status = go_rest_api->get_status( ).
      IF lo_response IS BOUND.
        response = lo_response->get_string_data( ).
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


  METHOD set_eventgrid_schema.

    DATA : ls_ploaddata TYPE string .

    gt_egrid_schema = it_egrid_schema.

    LOOP AT gt_egrid_schema ASSIGNING FIELD-SYMBOL(<lfs_schema>).

* Convert Timestamp in UTC format  (Ex:2017-02-08T12:32:21Z)
      IF <lfs_schema>-eventtime IS INITIAL.
        GET TIME STAMP FIELD  DATA(lv_current_timestamp) .
        convert_tstamp_utc( EXPORTING im_timestamp = lv_current_timestamp
                            RECEIVING re_utctime   = <lfs_schema>-eventtime ).

      ELSE.
        CONDENSE <lfs_schema>-eventtime.
        convert_tstamp_utc( EXPORTING im_timestamp = CONV #( <lfs_schema>-eventtime )
                          RECEIVING re_utctime   = <lfs_schema>-eventtime ).
      ENDIF.

      IF <lfs_schema>-metadataversion IS INITIAL.
        <lfs_schema>-metadataversion  = '1'.
      ENDIF.
    ENDLOOP.

* Convert Data into json format
    /ui2/cl_json=>serialize(
       EXPORTING
         data             = gt_egrid_schema
         pretty_name      = 'X'
       RECEIVING
         r_json           = DATA(lv_jsondata) ).

*Convert string data to Xstring format
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_jsondata
      IMPORTING
        buffer = rv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
       RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>error_con_xstring
            interface_id = gv_interface_id.
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
* Convert Expiary Time into Eventgrid timezone format.
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
ENDCLASS.
