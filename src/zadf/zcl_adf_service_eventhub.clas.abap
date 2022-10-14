class ZCL_ADF_SERVICE_EVENTHUB definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public

  global friends ZCL_ADF_SERVICE_REPROCESS .

public section.

  methods CREATE_CONSUMER_GROUP
    importing
      value(IV_CONSUMER_GROUP) type STRING
    exporting
      value(RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE .
  methods GET_LIST_CONSUMER_GROUP
    exporting
      value(RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE .
  methods GET_CONSUMER_GROUP_DETAILS
    importing
      value(IV_CONSUMER_GROUP) type STRING
    exporting
      value(RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE .
  methods SEND_BATCH_EVENTS
    importing
      value(REQUEST) type XSTRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      value(RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE .
  methods DELETE_CONSUMER_GROUP
    importing
      value(IV_CONSUMER_GROUP) type STRING
    exporting
      value(RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I .

  methods SEND
    redefinition .
protected section.

  methods GET_SAS_TOKEN
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ADF_SERVICE_EVENTHUB IMPLEMENTATION.


METHOD create_consumer_group.
  DATA :    lo_response     TYPE REF TO if_rest_entity,
            lo_request      TYPE REF TO if_rest_entity,
            lv_expiry       TYPE string,
            lv_sas_token    TYPE string,
            lv_msg          TYPE string,
            lcx_adf_service TYPE REF TO zcx_adf_service,
            lv_path_prefix  TYPE string,
            lv_host         TYPE rfcdisplay-rfchost,
            lv_host_s       TYPE string,
            lt_lines        TYPE STANDARD TABLE OF tline,
            lw_lines        TYPE tline,
            lv_body         TYPE string.
  IF go_rest_api IS BOUND.
    TRY.
        get_sas_token( EXPORTING iv_baseaddress = gv_uri
                       RECEIVING rv_sas_token  = lv_sas_token ).
      CATCH zcx_adf_service INTO lcx_adf_service.
        lv_msg =  lcx_adf_service->get_text( ).
        MESSAGE lv_msg TYPE 'I'.
    ENDTRY.
*Passing new consumer group name in URI along with query parameters
    CONCATENATE '/' iv_consumer_group '?api-version=2014-01' INTO lv_path_prefix.
    IF NOT lv_path_prefix IS INITIAL.
      go_rest_api->zif_rest_framework~set_uri( lv_path_prefix ).
    ENDIF.
    lv_host_s = gv_host.
*   Add header attributes in Rest call
    add_request_header( iv_name = 'Content-Type' iv_value = 'application/xml; charset=utf-8' ).
    add_request_header( iv_name = 'Host' iv_value = lv_host_s ).
    add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ).
**Reading xml body from text ID
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = 'E'
        name                    = 'ZADF_EVENTHUB_CREATE_CONSUMER'
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
    ENDIF.
    IF NOT lt_lines IS INITIAL.
      LOOP AT lt_lines INTO lw_lines.
        CONCATENATE lv_body  lw_lines-TDLINE INTO lv_body.
        CLEAR: lw_lines.
      ENDLOOP.
      go_rest_api->zif_rest_framework~set_string_body( lv_body ).
    ENDIF.
**Rest API call to get response from Azure Destination
    lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
    ev_http_status = go_rest_api->get_status( ).
    go_rest_api->close( ).
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


  METHOD delete_consumer_group.
 DATA :  lo_response     TYPE REF TO if_rest_entity,
          lo_request      TYPE REF TO if_rest_entity,
          lv_expiry       TYPE string,
          lv_sas_token    TYPE string,
          lv_msg          TYPE string,
          lcx_adf_service TYPE REF TO zcx_adf_service,
          lv_path_prefix  TYPE string,
          lv_host         TYPE rfcdisplay-rfchost,
          lv_host_s       TYPE string,
          lt_lines        TYPE STANDARD TABLE OF tline,
          lw_lines        TYPE tline,
          lv_body         TYPE string.
  IF go_rest_api IS BOUND.
    TRY.
        get_sas_token( EXPORTING iv_baseaddress = gv_uri
                       RECEIVING rv_sas_token  = lv_sas_token ).
      CATCH zcx_adf_service INTO lcx_adf_service.
        lv_msg =  lcx_adf_service->get_text( ).
        MESSAGE lv_msg TYPE 'I'.
    ENDTRY.
**Passing existing consumer group name in URI along with query parameters
    CONCATENATE '/' iv_consumer_group '?api-version=2014-01' INTO lv_path_prefix.
    IF NOT lv_path_prefix IS INITIAL.
      go_rest_api->zif_rest_framework~set_uri( lv_path_prefix ).
    ENDIF.
    lv_host_s = gv_host.
**Add header attributes in Rest call
    add_request_header( iv_name = 'Content-Type' iv_value = 'application/xml; charset=utf-8' ).
    add_request_header( iv_name = 'Host' iv_value = lv_host_s ).
    add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ).
**Reading xml body from text ID
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = 'E'
        name                    = 'ZADF_EVENTHUB_DELETE_CONSUMER'
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
    ENDIF.
    IF NOT lt_lines IS INITIAL.
      LOOP AT lt_lines INTO lw_lines.
        CONCATENATE lv_body  lw_lines-tdline INTO lv_body.
        CLEAR: lw_lines.
      ENDLOOP.
      go_rest_api->zif_rest_framework~set_string_body( lv_body ).
    ENDIF.
**Rest API call to get response from Azure Destination
    lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
    ev_http_status = go_rest_api->get_status( ).
    go_rest_api->close( ).
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


  METHOD get_consumer_group_details.
 DATA :      lo_response     TYPE REF TO if_rest_entity,
              lo_request      TYPE REF TO if_rest_entity,
              lv_expiry       TYPE string,
              lv_sas_token    TYPE string,
              lv_msg          TYPE string,
              lv_path_prefix  type string,
              lcx_adf_service TYPE REF TO zcx_adf_service,
              lv_host         TYPE rfcdisplay-rfchost,
              lv_host_s       TYPE string.
  IF go_rest_api IS BOUND.
    TRY.
        get_sas_token( EXPORTING iv_baseaddress = gv_uri
                       RECEIVING rv_sas_token  = lv_sas_token ).
      CATCH zcx_adf_service INTO lcx_adf_service.
        lv_msg =  lcx_adf_service->get_text( ).
        MESSAGE lv_msg TYPE 'I'.
    ENDTRY.
*Passing existing consumer group name in URI
    CONCATENATE '/' iv_consumer_group INTO lv_path_prefix.
    IF NOT lv_path_prefix IS INITIAL.
      go_rest_api->zif_rest_framework~set_uri( lv_path_prefix ).
    ENDIF.
    lv_host_s = gv_host.
**Add header attributes in REST call.
    add_request_header( iv_name = 'Content-Type' iv_value = 'application/xml; charset=utf-8' ).
    add_request_header( iv_name = 'Host' iv_value = lv_host_s ).
    add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ).

**Rest API call to get response from Azure Destination
    lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
    ev_http_status = go_rest_api->get_status( ).
    go_rest_api->close( ).
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


  METHOD get_list_consumer_group.
  DATA : lo_response     TYPE REF TO if_rest_entity,
         lo_request      TYPE REF TO if_rest_entity,
         lv_expiry       TYPE string,
         lv_sas_token    TYPE string,
         lv_msg          TYPE string,
         lcx_adf_service TYPE REF TO zcx_adf_service.

  IF go_rest_api IS BOUND.
    TRY.
        get_sas_token( EXPORTING iv_baseaddress = gv_uri
                       RECEIVING rv_sas_token  = lv_sas_token ).
      CATCH zcx_adf_service INTO lcx_adf_service.
        lv_msg =  lcx_adf_service->get_text( ).
        MESSAGE lv_msg TYPE 'I'.
    ENDTRY.
*---Add Header details
    add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ).

*---Rest API call to get response from Azure Destination
    lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
    ev_http_status = go_rest_api->get_status( ).
    go_rest_api->close( ).
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


METHOD get_sas_token.
  DATA : lv_string_to_sign    TYPE string,
         encoded_base_address TYPE string,
         body_xstring         TYPE xstring,
         sign                 TYPE string,
         final_token          TYPE string,
         decoded              TYPE xstring,
         conv                 TYPE REF TO cl_abap_conv_out_ce,
         conv_in              TYPE REF TO cl_abap_conv_in_ce,
         format               TYPE i,
         new_expiry           TYPE string,
         lv_sas_key           TYPE string,
         lv_expiry_time       TYPE string,
         lv_policy            type zadf_ehub_policy-policy,
         lx_static_check      TYPE REF TO cx_static_check.
  get_epoch_time( RECEIVING rv_expiry_time =  lv_expiry_time ).
  format = 18.
  encoded_base_address = escape( val = iv_baseaddress format = format  ).
  CONCATENATE encoded_base_address  cl_abap_char_utilities=>newline lv_expiry_time INTO lv_string_to_sign.

  conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
  conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = body_xstring ).
  DEFINE encrypt_key.
*    decode_sign( receiving rv_secret = lv_sas_key ).
    IF gv_sas_key IS INITIAL.
     lv_sas_key = read_ssf_key( ).
   ELSE.
       lv_sas_key = read_key( ).
   ENDIF.
   conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
   conv->convert( exporting data = lv_sas_key importing buffer = decoded ).

   call method cl_abap_hmac=>calculate_hmac_for_raw
     exporting
       if_algorithm     = 'sha-256'
       if_key           = decoded
       if_data          = body_xstring
       if_length        = 0
     importing
       ef_hmacb64string = sign.
   clear : lv_sas_key, decoded.
  END-OF-DEFINITION.
  encrypt_key.
  new_expiry = lv_expiry_time.
  CONDENSE new_expiry.
  IF NOT sign IS INITIAL.
    DATA wa_policy TYPE zadf_ehub_policy.
    SELECT SINGLE * FROM zadf_ehub_policy INTO wa_policy WHERE interface_id EQ  gv_interface_id.
    IF sy-subrc EQ 0.
      lv_policy = wa_policy-policy.
      sign = escape( val = sign format = format  ).
      CONCATENATE 'SharedAccessSignature sr=' encoded_base_address  '&sig=' sign '&se=' new_expiry '&skn=' wa_policy-policy INTO final_token.
      rv_sas_token = final_token.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>policy_not_maintained
          interface_id = gv_interface_id.
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
         lv_expiry       TYPE string,
         lv_sas_token    TYPE string,
         lv_msg          TYPE string,
         lcx_adf_service TYPE REF TO zcx_adf_service,
         lt_headers1     TYPE tihttpnvp.

  IF go_rest_api IS BOUND.
    lt_headers1[] = it_headers[].
* Read token from headers for Managed Identity
    IF line_exists( lt_headers1[ name = gc_mi_auth ] ).
      DATA(lv_processing_method) = gc_mi_auth.
      DATA(lv_aad_token) = lt_headers1[ 1 ]-value.
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

*   Set the path prefix from the headers instead of creating many RFC
    DATA  : wa_headers TYPE ihttpnvp, lt_headers LIKE it_headers.
    lt_headers[] = it_headers[].
    LOOP AT lt_headers INTO wa_headers.
      IF wa_headers-name = 'path_prefix'.
        go_rest_api->zif_rest_framework~set_uri( wa_headers-value ).
      ENDIF.
    ENDLOOP.
    DELETE lt_headers WHERE name EQ 'path_prefix'.

*   Add custom headers.
    add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).
* Add Managed Identity token to the headers
    IF lv_processing_method EQ gc_mi_auth.
      add_request_header( iv_name = 'Authorization' iv_value = lv_aad_token ).
    ELSE.
* Add SAS token to the headers
      add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ).
    ENDIF.
    go_rest_api->zif_rest_framework~set_binary_body( request ).
    IF NOT it_headers[] IS INITIAL.
      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = lt_headers[] ).
    ENDIF.
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


  METHOD send_batch_events.
    DATA :   lo_response     TYPE REF TO if_rest_entity,
             lo_request      TYPE REF TO if_rest_entity,
             lv_expiry       TYPE string,
             lv_sas_token    TYPE string,
             lv_msg          TYPE string,
             lcx_adf_service TYPE REF TO zcx_adf_service.
    IF go_rest_api IS BOUND.
      TRY.
          get_sas_token( EXPORTING iv_baseaddress = gv_uri
                         RECEIVING rv_sas_token  = lv_sas_token ).
        CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.
*   Set the path prefix from the headers instead of creating many RFC
      DATA  : wa_headers TYPE ihttpnvp, lt_headers LIKE it_headers.
      lt_headers[] = it_headers[].
      LOOP AT lt_headers INTO wa_headers.
        IF wa_headers-name = 'path_prefix'.
          go_rest_api->zif_rest_framework~set_uri( wa_headers-value ).
        ENDIF.
      ENDLOOP.
      DELETE lt_headers WHERE name EQ 'path_prefix'.
**Add header attributes in REST call.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/vnd.microsoft.servicebus.json' ).
      add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ).
      go_rest_api->zif_rest_framework~set_binary_body( request ).
**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
      ev_http_status = go_rest_api->get_status( ).
      go_rest_api->close( ).
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
ENDCLASS.
