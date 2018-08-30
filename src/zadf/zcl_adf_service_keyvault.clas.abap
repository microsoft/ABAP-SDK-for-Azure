class ZCL_ADF_SERVICE_KEYVAULT definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public .

public section.

  methods GET_KV_DETAILS
    importing
      !IV_KV_INTERFACE_ID type ZINTERFACE_ID
      !IV_CLIENT_ID type STRING
      !IV_RESOURCE type STRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EV_KEY type STRING
      !EV_RESPONSE type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
protected section.
private section.

  data GV_KV_INTERFACE type ZINTERFACE_ID .
  data GV_CLIENT_ID type STRING .
  data GV_RESOURCE type STRING .
  data GV_TOKEN type STRING .
  data GV_RESPONSE type STRING .

  methods GET_AAD_TOKEN
    returning
      value(RV_AAD_TOKEN) type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods GET_KEY_FROM_KV
    returning
      value(RV_KV_KEY) type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_KEYVAULT IMPLEMENTATION.


METHOD get_aad_token.
  DATA : lo_request         TYPE REF TO if_rest_entity,
         lo_response        TYPE REF TO if_rest_entity,
         lv_response_data   TYPE string,
         lt_response_fields TYPE tihttpnvp,
         lv_token           TYPE string,
         ls_response_fields TYPE ihttpnvp,
         reader1            TYPE REF TO if_sxml_reader,
         form_data_helper   TYPE REF TO cl_rest_form_data,
         it_params          TYPE tihttpnvp,
         wa_params          TYPE ihttpnvp,
         lv_mediatype       TYPE string,
         lv_secret          TYPE string,
         lo_http_client     TYPE REF TO if_http_client,
         lv_content_type    TYPE string,
         lw_headers         TYPE ihttpnvp,
         lv_http_status     TYPE i.
  DEFINE set_headers.
    lv_mediatype = if_rest_media_type=>gc_appl_www_form_url_encoded.
    create object form_data_helper
      exporting
        io_entity = lo_request.
    wa_params-name = 'resource'.
    wa_params-value =  gv_resource .
    append wa_params to it_params.
    clear wa_params.
    wa_params-name = 'client_id'.
    wa_params-value =  gv_client_id .
    append wa_params to it_params.
    clear wa_params.
    decode_sign( receiving rv_secret = lv_secret ).
    wa_params-name = 'client_secret'.
    wa_params-value = lv_secret.
    append wa_params to it_params.
    clear wa_params.
    wa_params-name = 'grant_type'.
    wa_params-value = 'client_credentials'.
    append wa_params to it_params.
    clear wa_params.
    go_rest_api->set_request_header( iv_name = 'Content-Type'  iv_value = lv_mediatype ).
    go_rest_api->set_string_body( cl_http_utility=>fields_to_string( it_params ) ) .
    clear: lv_secret, it_params,gt_headers.
  END-OF-DEFINITION.
  IF go_rest_api IS BOUND.
    set_headers .
    lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = abap_false is_retry = abap_false ).
    lv_http_status = go_rest_api->get_status( ).
    IF lo_response IS BOUND.
      lv_response_data = lo_response->get_string_data( ).
      lo_http_client = go_rest_api->get_http_client( ).
      IF lo_http_client IS BOUND.
        lv_content_type = lo_http_client->response->get_content_type( ).
        go_rest_api->close( ).
      ENDIF.
      IF lv_http_status EQ '200'.
        IF lv_content_type CP `text/plain*` OR
           lv_content_type   CP `text/javascript*` OR
           lv_content_type   CP `application/x-www-form-urlencoded*`.
          lt_response_fields = urlencoded_to_http_fields( iv_response_data = lv_response_data ).
        ELSE.
          lt_response_fields = json_to_http_fields( iv_response_data = lv_response_data ).
        ENDIF.
        CLEAR ls_response_fields.
        READ TABLE lt_response_fields INTO ls_response_fields
                                      WITH KEY name = 'access_token'.
        IF sy-subrc EQ 0.
          lv_token = ls_response_fields-value.
        ELSE.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid       = zcx_adf_service=>aad_token_not_found
              interface_id = gv_interface_id.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>error_restapi_response
            interface_id = gv_interface_id.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>restapi_response_not_found
          interface_id = gv_interface_id.
    ENDIF.
    rv_aad_token = lv_token.
  ENDIF.
ENDMETHOD.


METHOD get_key_from_kv.
  DATA : rest_handler       TYPE REF TO zcl_rest_framework,
         go_response        TYPE REF TO if_rest_entity,
         go_request         TYPE REF TO if_rest_entity,
         gv_client          TYPE string,
         gv_body            TYPE string,
         gv_langu           TYPE langu,
         lv_key             TYPE string,
         lv_content_type    TYPE string,
         lo_http_client     TYPE REF TO if_http_client,
         lv_requestid       TYPE string,
         lcx_interface      TYPE REF TO zcx_interace_config_missing,
         lcx_http           TYPE REF TO zcx_http_client_failed,
         lv_response_data   TYPE string,
         lt_response_fields TYPE tihttpnvp,
         lv_token           TYPE string,
         ls_response_fields TYPE ihttpnvp,
         form_data_helper   TYPE REF TO cl_rest_form_data,
         it_params          TYPE tihttpnvp,
         wa_params          TYPE ihttpnvp,
         lv_mediatype       TYPE string,
         lv_http_status     TYPE i.
  CREATE OBJECT lcx_interface.
  CREATE OBJECT lcx_http.
  TRY .
      CREATE OBJECT rest_handler
        EXPORTING
          interface_name      = gv_kv_interface               "Mandatory
          business_identifier = 'KeyVaultAuth'
          method              = 'GET'.    "For troubleshooting
    CATCH zcx_interace_config_missing INTO lcx_interface.
      RAISE EXCEPTION lcx_interface.
    CATCH zcx_http_client_failed INTO lcx_http .
      RAISE EXCEPTION lcx_http.
  ENDTRY.
*Optional - To help developer understand the origin of call
  rest_handler->set_callingmethod('GET_KEY_FROM_KV').
*Optional - To help developer understand the origin of call
  rest_handler->set_callingprogram('ZCL_ADF_SERVICE_KEYVAULT').
  rest_handler->zif_rest_framework~set_uri( '?api-version=2016-10-01' ).
************************************************************************
  CONCATENATE 'Bearer' gv_token INTO lv_token SEPARATED BY space.
  rest_handler->zif_rest_framework~set_request_header( iv_name = 'Authorization' iv_value = lv_token ).
************************************************************************
  go_response = rest_handler->zif_rest_framework~execute( io_entity = go_request async = abap_false is_retry = abap_false ).
  lv_http_status = rest_handler->get_status( ).
************************************************************************
  IF go_response IS BOUND.
    lv_response_data = go_response->get_string_data( ).
    gv_response = lv_response_data.
    lo_http_client = rest_handler->get_http_client( ).
    IF lo_http_client IS BOUND.
      lv_content_type = lo_http_client->response->get_content_type( ).
      go_rest_api->close( ).
    ENDIF.
    IF lv_http_status EQ '201' OR lv_http_status EQ '200'.
      IF lv_content_type CP `text/plain*` OR
         lv_content_type   CP `text/javascript*` OR
         lv_content_type   CP `application/x-www-form-urlencoded*`.
        lt_response_fields = urlencoded_to_http_fields( iv_response_data = lv_response_data ).
      ELSE.
        lt_response_fields = json_to_http_fields( iv_response_data = lv_response_data ).
      ENDIF.
      CLEAR ls_response_fields.
      READ TABLE lt_response_fields INTO ls_response_fields
                                    WITH KEY name = 'value'.
      IF sy-subrc EQ 0.
        lv_key = ls_response_fields-value.
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>kv_secret_not_found
            interface_id = gv_kv_interface.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>error_restapi_response
          interface_id = gv_kv_interface.
    ENDIF.
  ELSE.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>restapi_response_not_found
        interface_id = gv_kv_interface.
  ENDIF.
  rv_kv_key = lv_key.
ENDMETHOD.


METHOD get_kv_details.
  DATA : lv_kv_key    TYPE string.
  gv_kv_interface     = iv_kv_interface_id.
  gv_client_id        = iv_client_id.
  gv_resource         = iv_resource.
  gt_headers = it_headers.
  get_aad_token( RECEIVING rv_aad_token = gv_token ).
  IF NOT gv_token IS INITIAL.
    get_key_from_kv( RECEIVING rv_kv_key = lv_kv_key ).
  ELSE.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>error_aad_token
        interface_id = gv_interface_id.
  ENDIF.
  ev_key = lv_kv_key.
  ev_response = gv_response.
  CLEAR: gv_token, gt_headers.
ENDMETHOD.
ENDCLASS.
