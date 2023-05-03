class ZCL_ADF_SERVICE_COSMOSDB definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public

  global friends ZCL_ADF_SERVICE_REPROCESS .

public section.

  methods SET_PARAMETERS
    importing
      value(IV_HTTP_VERB) type STRING
      value(IV_RESOURCE_TYPE) type STRING
      value(IV_RESOURCE_LINK) type STRING
      value(IV_PARTITION_KEY_VAL) type STRING optional .

  methods SEND
    redefinition .
protected section.

  methods GET_AAD_TOKEN
    importing
      value(IV_BASEADDRESS) type STRING
      !IV_AAD_TOKEN type STRING
    returning
      value(RV_AAD_ENCODED_TOKEN) type STRING
    raising
      ZCX_ADF_SERVICE .

  methods GET_SAS_TOKEN
    redefinition .
private section.

  data GV_HTTP_VERB type STRING .
  data GV_RESOURCE_TYPE type STRING .
  data GV_RESOURCE_LINK type STRING .
  constants GC_TYPE_AAD type STRING value 'type=aad' ##NO_TEXT.
  constants GC_TYPE type STRING value 'type=master' ##NO_TEXT.
  constants GC_VERSION type STRING value 'ver=1.0' ##NO_TEXT.
  constants GC_SEPARATOR type STRING value '&' ##NO_TEXT.
  constants GC_SIG type STRING value 'sig=' ##NO_TEXT.
  data GV_HTTP_DATE type STRING .
  data GV_PARTITION_KEY_VAL type STRING .

  methods GET_RFC7231_TIME
    returning
      value(RV_TIMESTAMP) type STRING .
  methods GENERATE_STRING_TO_SIGN
    returning
      value(RV_STRING_TO_SIGN) type STRING
    raising
      ZCX_ADF_SERVICE .
  methods VALIDATE_PARAMETERS
    raising
      ZCX_ADF_SERVICE .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_COSMOSDB IMPLEMENTATION.


METHOD generate_string_to_sign.
  DATA : lv_http_date TYPE string.
**Getting UTC timestamp
  me->get_rfc7231_time( RECEIVING rv_timestamp = gv_http_date ).
**Validation of parameters
  me->validate_parameters( ).
  lv_http_date = gv_http_date.
  TRANSLATE : lv_http_date      TO LOWER CASE,
              gv_http_verb      TO LOWER CASE,
              gv_resource_type  TO LOWER CASE.
**Constructing String to Sign
  CONCATENATE gv_http_verb cl_abap_char_utilities=>newline
              gv_resource_type cl_abap_char_utilities=>newline
              gv_resource_link cl_abap_char_utilities=>newline
              lv_http_date cl_abap_char_utilities=>newline
              cl_abap_char_utilities=>newline INTO rv_string_to_sign.
ENDMETHOD.


  METHOD get_aad_token.
    DATA :
          lv_authstring     TYPE string.

    IF NOT iv_aad_token IS INITIAL.
** Construct AuthString
      CONCATENATE gc_type_aad gc_separator gc_version gc_separator gc_sig iv_aad_token INTO lv_authstring.
** Generate AuthToken
      rv_aad_encoded_token = cl_http_utility=>escape_url( lv_authstring ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>auth_token_not_generated
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


METHOD get_rfc7231_time.
  CONSTANTS : lc_com      TYPE c VALUE ',',
              lc_gmt      TYPE char3 VALUE 'GMT',
              lc_col      TYPE c VALUE ':',
              lc_utc_zone TYPE tznzone VALUE 'UTC'.
  DATA : lv_day_name TYPE char4,
         lv_date     TYPE char12,
         lv_day      TYPE sc_day_txt,
         lv_time     TYPE string,
         lv_tstmp    TYPE timestamp,
         lv_tstmp_s  TYPE string,
         lv_tstmp_t  TYPE sy-uzeit,
         lv_datum    TYPE datum.
  FIELD-SYMBOLS <fs_date> TYPE any.
** Get current timestamp
  GET TIME STAMP FIELD lv_tstmp.
  CONVERT TIME STAMP lv_tstmp TIME ZONE lc_utc_zone INTO DATE lv_datum TIME lv_tstmp_t.
  lv_tstmp_s = lv_tstmp.
  lv_datum = lv_tstmp_s(8).
**  Convert date format
  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      input  = lv_datum
    IMPORTING
      output = lv_date.
**  get the day of the month
  CALL FUNCTION 'DATE_COMPUTE_DAY_ENHANCED'
    EXPORTING
      date    = lv_datum
    IMPORTING
      weekday = lv_day.
** get time separated by :
  CONCATENATE lv_tstmp_s+8(2)  lv_tstmp_s+10(2)  lv_tstmp_s+12(2)  INTO lv_time SEPARATED BY lc_col.
** Capitalise the first letter of month and day
  ASSIGN lv_day(1) TO <fs_date>.
  IF <fs_date> IS ASSIGNED.
    TRANSLATE <fs_date> TO UPPER CASE.
    UNASSIGN <fs_date>.
  ENDIF.

  ASSIGN lv_date(1) TO <fs_date>.
  IF <fs_date> IS ASSIGNED.
    TRANSLATE <fs_date> TO UPPER CASE.
    UNASSIGN <fs_date>.
  ENDIF.

  ASSIGN lv_date+1(2) TO <fs_date>.
  IF <fs_date> IS ASSIGNED.
    TRANSLATE <fs_date> TO LOWER CASE.
    UNASSIGN <fs_date>.
  ENDIF.

  ASSIGN lv_day+1(2) TO <fs_date>.
  IF <fs_date> IS ASSIGNED.
    TRANSLATE <fs_date> TO LOWER CASE.
    UNASSIGN <fs_date>.
  ENDIF.
** Add ',' to day
  CONCATENATE lv_day(3) lc_com INTO lv_day_name.
  CONDENSE lv_day_name.
** make timestamp in RFC7231 format
*  CONCATENATE lv_day_name
**              lv_date+4(2)
**              lv_date(3)
*              lv_date(2)
*              lv_date+3(3)
*              lv_date+7(4)
*              lv_time
*              lc_gmt
*           INTO rv_timestamp SEPARATED BY space.
  CONCATENATE lv_day_name
            lv_date+4(2)
            lv_date(3)
            lv_date+7(4)
            lv_time
            lc_gmt
         INTO rv_timestamp SEPARATED BY space.

ENDMETHOD.


  METHOD get_sas_token.
    DATA :lv_sign_utf       TYPE xstring,
          lv_string_to_sign TYPE string,
          lv_hash_sig       TYPE string,
          lv_authstring     TYPE string,
          lv_key            TYPE string,
          lv_key_xstr       TYPE xstring,
          lo_conv           TYPE REF TO cl_abap_conv_out_ce,
          lv_sas_key        TYPE string.
** generate String To Sign
    me->generate_string_to_sign( RECEIVING rv_string_to_sign = lv_string_to_sign ).
** Convert string to UTF-8 format
    lo_conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    lo_conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = lv_sign_utf ).
    DEFINE encrypt_signature.
*      decode_sign( receiving rv_secret = lv_key ).
      IF gv_sas_key IS INITIAL.
       lv_sas_key = read_ssf_key( ).
      ELSE.
         lv_key = read_key( ).
      ENDIF.
** Decode Key from base64
       call function 'SSFC_BASE64_DECODE'
         exporting
           b64data                  = lv_key
         importing
           bindata                  = lv_key_xstr
         exceptions
           ssf_krn_error            = 1
           ssf_krn_noop             = 2
           ssf_krn_nomemory         = 3
           ssf_krn_opinv            = 4
           ssf_krn_input_data_error = 5
           ssf_krn_invalid_par      = 6
           ssf_krn_invalid_parlen   = 7
           others                   = 8.
       if sy-subrc <> 0.
         raise exception type zcx_adf_service
          exporting
           textid       = zcx_adf_service=>key_decoding_failed
           interface_id = gv_interface_id.
       endif.
** Encode the signature with the Key in SHA 256 format
       call method cl_abap_hmac=>calculate_hmac_for_raw
         exporting
           if_algorithm     = 'SHA-256'
           if_key           = lv_key_xstr
           if_data          = lv_sign_utf
           if_length        = 0
         importing
           ef_hmacb64string = lv_hash_sig.
       clear: lv_key_xstr, lv_key.
    END-OF-DEFINITION.
** call macro
    encrypt_signature.
    IF NOT lv_hash_sig IS INITIAL.
** Construct AuthString
      CONCATENATE gc_type gc_separator gc_version gc_separator gc_sig lv_hash_sig INTO lv_authstring.
** Generate AuthToken
      rv_sas_token = cl_http_utility=>escape_url( lv_authstring ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>auth_token_not_generated
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


METHOD send.
  CONSTANTS : lc_separator TYPE c VALUE '/'.
  DATA : lo_response     TYPE REF TO if_rest_entity,
         lo_request      TYPE REF TO if_rest_entity,
         lv_msg          TYPE string,
         lv_path_prefix  TYPE string,
         lcx_adf_service TYPE REF TO zcx_adf_service,
         lv_auth_token   TYPE string,
         lt_header       TYPE tihttpnvp.
  IF go_rest_api IS BOUND.

* Read token from headers for Managed Identity/AAD
    lt_header = it_headers.
    IF line_exists( it_headers[ name = gc_mi_auth ] ).
      DATA(lv_processing_method) = gc_mi_auth.
      lv_auth_token = it_headers[ name = gc_mi_auth ]-value.
      DELETE lt_header WHERE name = gc_mi_auth.
    ENDIF.

    TRY.
        CASE lv_processing_method.
          WHEN gc_mi_auth.
            CALL METHOD me->get_aad_token
              EXPORTING
                iv_baseaddress       = gv_uri
                iv_aad_token         = lv_auth_token
              RECEIVING
                rv_aad_encoded_token = lv_auth_token.
          WHEN OTHERS.
            get_sas_token( EXPORTING iv_baseaddress = gv_uri
                           RECEIVING rv_sas_token  = lv_auth_token ).
        ENDCASE.
      CATCH zcx_adf_service INTO lcx_adf_service.
        lv_msg =  lcx_adf_service->get_text( ).
        MESSAGE lv_msg TYPE 'I'.
    ENDTRY.
*   Set URI
    CONCATENATE lc_separator gv_resource_link lc_separator gv_resource_type INTO lv_path_prefix.
    go_rest_api->zif_rest_framework~set_uri( uri = lv_path_prefix ).

** Fill in header values
    IF NOT gv_partition_key_val IS INITIAL.
      add_request_header( iv_name = 'x-ms-documentdb-partitionkey' iv_value = gv_partition_key_val ).
    ENDIF.
    add_request_header( iv_name = 'x-ms-version' iv_value = '2018-12-31').
    add_request_header( iv_name = 'x-ms-documentdb-is-upsert' iv_value = 'true').
    IF gv_http_date IS NOT INITIAL.
      add_request_header( iv_name = 'x-ms-date' iv_value = gv_http_date ).
    ENDIF.
    add_request_header( iv_name = 'Accept' iv_value = 'application/json').

    add_request_header( iv_name = 'Authorization' iv_value = lv_auth_token ).

    IF NOT lt_header[] IS INITIAL.
      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = lt_header[] ).
    ENDIF.
    go_rest_api->zif_rest_framework~set_binary_body( request ).
**Rest API call to get response from Azure Destination
    lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
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


  METHOD set_parameters.
**HTTP verb, such as GET, POST, or PUT.
    gv_http_verb        = iv_http_verb.
** Eg. "dbs", "colls", "docs".
    gv_resource_type    = iv_resource_type.
**dbs/<data base name>/colls/<collection name>
    gv_resource_link    = iv_resource_link.
**E.g for partition key /country property of Json payload ,
**partition key value would be ["<country value>"]
    IF NOT iv_partition_key_val IS INITIAL.
      CONCATENATE '["' iv_partition_key_val '"]' INTO gv_partition_key_val.
    ENDIF.
  ENDMETHOD.


  METHOD validate_parameters.
    IF gv_http_date IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>http_date_not_found
          interface_id = gv_interface_id.
    ENDIF.
**HTTP verb, such as GET, POST, or PUT.
    IF gv_http_verb IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>http_verb_not_found
          interface_id = gv_interface_id.
    ENDIF.
** Eg. "dbs", "colls", "docs".
    IF gv_resource_type IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>resource_type_not_found
          interface_id = gv_interface_id.
    ENDIF.
**dbs/<data base name>/colls/<collection name>
    IF gv_resource_link IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>resource_link_not_found
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
