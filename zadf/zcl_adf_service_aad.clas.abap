class ZCL_ADF_SERVICE_AAD definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public .

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
protected section.
private section.

  data GV_RESPONSE type STRING .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_AAD IMPLEMENTATION.


METHOD get_aad_token.
  DATA : lo_request           TYPE REF TO if_rest_entity,
        lo_response           TYPE REF TO if_rest_entity,
        lv_response_data      TYPE string,
        lt_response_fields    TYPE tihttpnvp,
        lv_token              TYPE string,
        ls_response_fields    TYPE ihttpnvp,
        reader1               TYPE REF TO if_sxml_reader,
        form_data_helper      TYPE REF TO cl_rest_form_data,
        it_params             TYPE tihttpnvp,
        wa_params             TYPE ihttpnvp,
        lv_mediatype          TYPE string,
        lv_secret             TYPE string,
        lv_http_status        TYPE i.
  DEFINE set_headers.
************************************************************************
    lv_mediatype = if_rest_media_type=>gc_appl_www_form_url_encoded.
    create object form_data_helper
      exporting
        io_entity = lo_request.
    wa_params-name = 'resource'.
    wa_params-value =  iv_resource .
    append wa_params to it_params.
    clear wa_params.
    wa_params-name = 'client_id'.
    wa_params-value =  iv_client_id .
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
    clear : lv_secret, it_params.
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
      IF lv_http_status EQ '200'.
*        IF lv_mediatype CP `text/plain*` OR
*         lv_mediatype CP `text/javascript*` OR
*         lv_mediatype CP `application/x-www-form-urlencoded*`.
*          lt_response_fields = urlencoded_to_http_fields( iv_response_data = lv_response_data ).
*        ELSE.
          lt_response_fields = json_to_http_fields( iv_response_data = lv_response_data ).
*        ENDIF.
        CLEAR ls_response_fields.
        READ TABLE lt_response_fields INTO ls_response_fields
                                      WITH KEY name = 'access_token'.
        IF sy-subrc EQ 0.
          lv_token = ls_response_fields-value.
        ELSE.
**Raise Exception
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid       = zcx_adf_service=>aad_token_not_found
              interface_id = gv_interface_id.
        ENDIF.
        go_rest_api->close( ).
      ELSE.
**Raise Exception
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
    ev_aad_token = lv_token.
  ENDIF.
ENDMETHOD.
ENDCLASS.
