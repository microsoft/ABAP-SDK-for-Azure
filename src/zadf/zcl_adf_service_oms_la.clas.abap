class ZCL_ADF_SERVICE_OMS_LA definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public .

public section.

  methods SET_WORKSPACE_ID
    importing
      value(IV_WORKSPACE_ID) type STRING .
  methods SET_LOG_TYPE
    importing
      !IV_LOG_TYPE type STRING .

  methods SEND
    redefinition .
protected section.
private section.

  data GV_WORKSPACE_ID type STRING .
  data GV_TIMESTAMP type STRING .
  data GV_AUTH_HEAD type STRING .
  data GV_LOG_TYPE type STRING value 'Sample' ##NO_TEXT.

  methods GENERATE_SIGNATURE
    importing
      value(IV_JSON_LEN) type I
    returning
      value(EV_AUTH_HEAD) type STRING
    raising
      ZCX_ADF_SERVICE .
  methods GET_RFC1123_TIME
    exporting
      value(EV_TIMESTAMP) type STRING .
  methods GENERATE_SIGN_STRING
    importing
      value(IV_VERB) type STRING default 'POST'
      value(IV_LENGTH) type I
      value(IV_TIMESTAMP) type STRING
    exporting
      value(EV_SIGN_TEXT) type STRING .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_OMS_LA IMPLEMENTATION.


  METHOD generate_signature.

    CONSTANTS : lc_key   TYPE string VALUE 'SharedKey'.

    DATA lo_conv TYPE REF TO cl_abap_conv_out_ce.

    DATA :lv_sign_utf  TYPE xstring,
          lv_sign      TYPE string,
          lv_signature TYPE string,
          lv_header    TYPE string,
          lv_key_enc   TYPE string,
          lv_key       TYPE xstring.

** get time in RFC1123 format
    CLEAR gv_timestamp.
    CALL METHOD me->get_rfc1123_time
      IMPORTING
        ev_timestamp = gv_timestamp.

** Build signature string
    CALL METHOD me->generate_sign_string
      EXPORTING
        iv_verb      = 'POST'
        iv_length    = iv_json_len
        iv_timestamp = gv_timestamp
      IMPORTING
        ev_sign_text = lv_sign.

** Convert string to UTF-8 format
    lo_conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    lo_conv->convert( EXPORTING data = lv_sign IMPORTING buffer = lv_sign_utf ).

    DEFINE encrypt_signature.
      decode_sign( receiving rv_secret = lv_key_enc ).
** Decode Key from base64
      call function 'SSFC_BASE64_DECODE'
        exporting
          b64data                  = lv_key_enc
        importing
          bindata                  = lv_key
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
** Encode the signature with the Key in SHA 56 format
      call method cl_abap_hmac=>calculate_hmac_for_raw
        exporting
          if_algorithm     = 'sha-256'
          if_key           = lv_key
          if_data          = lv_sign_utf
          if_length        = 0
        importing
          ef_hmacb64string = lv_signature.
      clear lv_key.
    END-OF-DEFINITION.
** call macro
    encrypt_signature.
** make prefix to signature
    CONCATENATE lc_key  gv_workspace_id INTO lv_header SEPARATED BY space.

** Construct header
    CONCATENATE lv_header ':' lv_signature INTO ev_auth_head.
  ENDMETHOD.


  METHOD generate_sign_string.

    CONSTANTS : lc_date TYPE char10 VALUE 'x-ms-date:',
                lc_app  TYPE char16 VALUE 'application/json',
                lc_log  TYPE char9 VALUE '/api/logs'.
    DATA lv_len TYPE string.

** move int length to string
    lv_len = iv_length.
    CONDENSE lv_len.
** Build token
    CONCATENATE iv_verb
                cl_abap_char_utilities=>newline
                lv_len
                cl_abap_char_utilities=>newline
                lc_app
                cl_abap_char_utilities=>newline
                lc_date iv_timestamp
                cl_abap_char_utilities=>newline
                lc_log INTO ev_sign_text.


  ENDMETHOD.


  METHOD get_rfc1123_time.

    CONSTANTS : lc_com TYPE c VALUE ',',
                lc_gmt TYPE char3 VALUE 'GMT',
                lc_col TYPE c VALUE ':'.

    DATA : lv_day_name TYPE char4,
           lv_date     TYPE char12,
           lv_day      TYPE sc_day_txt,
           lv_time     TYPE string,
           lv_tstmp    TYPE timestamp,
           lv_tstmp_s  TYPE string,
           lv_datum    TYPE datum.

    FIELD-SYMBOLS <fs_date> TYPE any.


** Get current GMT timestamp
    GET TIME STAMP FIELD lv_tstmp.
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

** make timestamp in RFC1123
    CONCATENATE lv_day_name
                lv_date+4(2)
                lv_date(3)
                lv_date+7(4)
                lv_time
                lc_gmt
             INTO ev_timestamp SEPARATED BY space.
  ENDMETHOD.


  METHOD send.
    DATA :  lo_response     TYPE REF TO if_rest_entity,
            lo_request      TYPE REF TO if_rest_entity,
            lv_expiry       TYPE string,
            lv_sas_token    TYPE string,
            lv_msg          TYPE string,
            lcx_adf_service TYPE REF TO zcx_adf_service,
            lv_json_len     TYPE i,
            lw_header       TYPE ihttpnvp.

    IF go_rest_api IS BOUND.
** Calculate the JSON length
      lv_json_len = xstrlen( request ).
      TRY.
**  Check if workspace ID exist
          IF gv_workspace_id IS INITIAL.
            RAISE EXCEPTION TYPE zcx_adf_service
              EXPORTING
                textid       = zcx_adf_service=>workspace_id_not_found
                interface_id = gv_interface_id.
          ENDIF.
** Get Auth header
          gv_auth_head = generate_signature( iv_json_len  = lv_json_len  ).
        CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.

*   Set URI   /api/logs?api-version=2016-04-01
      go_rest_api->zif_rest_framework~set_uri( '/api/logs?api-version=2016-04-01' ).

** Fill in header values.
      add_request_header( iv_name = 'Log-Type' iv_value = gv_log_type ).
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/json' ).
      add_request_header( iv_name = 'x-ms-date' iv_value = gv_timestamp ).
      add_request_header( iv_name = 'Authorization' iv_value = gv_auth_head ).
**  Get time generated
      CLEAR lw_header.
      READ TABLE it_headers INTO lw_header WITH KEY name = 'TIME_GENERATED'.
      IF sy-subrc IS INITIAL.
        TRANSLATE lw_header-value TO LOWER CASE.
        add_request_header( iv_name = 'time-generated-field' iv_value = lw_header-value ).
      ENDIF.
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


  METHOD set_log_type.
**  Set log type in global variable
    gv_log_type = iv_log_type.
  ENDMETHOD.


  METHOD set_workspace_id.
** Assign workspace id to global attribute
    gv_workspace_id = iv_workspace_id.
  ENDMETHOD.
ENDCLASS.
