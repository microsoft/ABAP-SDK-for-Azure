class ZCL_ADF_SERVICE_AAD definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public

  global friends ZCL_ADF_SERVICE_REPROCESS .

public section.

  methods GET_AAD_TOKEN
    importing
      value(IV_CLIENT_ID) type STRING
      value(IV_RESOURCE) type STRING
    exporting
      value(EV_AAD_TOKEN) type STRING
      value(EV_RESPONSE) type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods GET_AAD_TOKEN_MSI
    exporting
      value(EV_AAD_TOKEN) type STRING
      value(EV_RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
protected section.
private section.

  data GV_RESPONSE type STRING .

  methods GET_AAD_TOKEN_CLNT_CRED
    importing
      value(IV_CLIENT_ID) type STRING
      value(IV_RESOURCE) type STRING
    exporting
      value(EV_AAD_TOKEN) type STRING
      value(EV_RESPONSE) type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_AAD IMPLEMENTATION.


METHOD get_aad_token.
* Switch to managed identities/ AAD authentication
* based on the configuration in table ZADF_MI_CONFIG
  DATA:
    lv_switch_to_mi TYPE boolean,
    lv_http_status  TYPE i.

  CALL METHOD me->check_switch_to_mi
    EXPORTING
      iv_interface_id = gv_interface_id
    IMPORTING
      ev_switch_to_mi = lv_switch_to_mi.
* Switching to exising fuctionality of getting AAD token
  IF lv_switch_to_mi EQ abap_false.
    CALL METHOD me->get_aad_token_clnt_cred
      EXPORTING
        iv_client_id = iv_client_id
        iv_resource  = iv_resource
      IMPORTING
        ev_aad_token = ev_aad_token
        ev_response  = ev_response.
  ELSE.
* Switching managed identites based authentication.
        CALL METHOD me->get_aad_token_msi
          IMPORTING
            ev_aad_token   = ev_aad_token
            ev_response    = ev_response
            ev_http_status = lv_http_status.
  ENDIF.
ENDMETHOD.


  METHOD get_aad_token_clnt_cred.
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
           lv_http_status     TYPE i,
           lv_msg             TYPE string,
           lcx_adf_service    TYPE REF TO zcx_adf_service.
    DEFINE set_headers.
************************************************************************
      lv_mediatype = if_rest_media_type=>gc_appl_www_form_url_encoded.
      CREATE OBJECT form_data_helper
        EXPORTING
          io_entity = lo_request.
      wa_params-name = 'resource'.
      wa_params-value =  iv_resource .
      APPEND wa_params TO it_params.
      CLEAR wa_params.
      wa_params-name = 'client_id'.
      wa_params-value =  iv_client_id .
      APPEND wa_params TO it_params.
      CLEAR wa_params.
*    decode_sign( receiving rv_secret = lv_secret ).
      TRY.
        IF gv_sas_key IS INITIAL.
          lv_secret = read_ssf_key( ).
        ELSE.
          lv_secret = read_key( ).
        ENDIF.
      CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.
      wa_params-name = 'client_secret'.
      wa_params-value = lv_secret.
      APPEND wa_params TO it_params.
      CLEAR wa_params.
      wa_params-name = 'grant_type'.
      wa_params-value = 'client_credentials'.
      APPEND wa_params TO it_params.
      CLEAR wa_params.
      go_rest_api->set_request_header( iv_name = 'Content-Type'  iv_value = lv_mediatype ).
      go_rest_api->set_string_body( cl_http_utility=>fields_to_string( it_params ) ) .
      CLEAR : lv_secret, it_params.
    END-OF-DEFINITION.
    IF go_rest_api IS BOUND.
      set_headers .
************************************************************************
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = abap_false is_retry = abap_false ).
      lv_http_status = go_rest_api->get_status( ).
************************************************************************
      IF lo_response IS BOUND.
        lv_response_data = lo_response->get_string_data( ).
        ev_response = lv_response_data.
        go_rest_api->close( ).
        IF lv_http_status EQ '200'.
          lt_response_fields = json_to_http_fields( iv_response_data = lv_response_data ).
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
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      ev_aad_token = lv_token.
    ENDIF.
  ENDMETHOD.


  METHOD get_aad_token_msi.
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
           lv_http_status     TYPE i.

    IF go_rest_api IS BOUND.
      go_rest_api->set_request_header( iv_name = 'Metadata' iv_value = 'true' ).
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = abap_false is_retry = abap_false ).
      lv_http_status = go_rest_api->get_status( ).
      ev_http_status = lv_http_status.
      IF lo_response IS BOUND.
        lv_response_data = lo_response->get_string_data( ).
        ev_response = lv_response_data.
        go_rest_api->close( ).
        IF lv_http_status EQ '200'.
          lt_response_fields = json_to_http_fields( iv_response_data = lv_response_data ).
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
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      ev_aad_token = lv_token.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
