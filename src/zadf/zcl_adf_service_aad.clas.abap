CLASS zcl_adf_service_aad DEFINITION
  PUBLIC
  INHERITING FROM zcl_adf_service
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_adf_service_factory.

  PUBLIC SECTION.

    METHODS get_aad_token
      IMPORTING
        VALUE(iv_client_id) TYPE string
        VALUE(iv_resource)  TYPE string
      EXPORTING
        VALUE(ev_aad_token) TYPE string
        VALUE(ev_response)  TYPE string
      RAISING
        zcx_adf_service
        zcx_interace_config_missing
        zcx_http_client_failed .
  PROTECTED SECTION.
  PRIVATE SECTION.

    ENDCLASS.



CLASS zcl_adf_service_aad IMPLEMENTATION.


  METHOD get_aad_token.
    DATA : lo_request         TYPE REF TO if_rest_entity,
           lo_response        TYPE REF TO if_rest_entity,
           lv_response_data   TYPE string,
           lt_response_fields TYPE tihttpnvp,
           lv_token           TYPE string,
           ls_response_fields TYPE ihttpnvp,
           form_data_helper   TYPE REF TO cl_rest_form_data,
           it_params          TYPE tihttpnvp,
           wa_params          TYPE ihttpnvp,
           lv_mediatype       TYPE string,
           lv_secret          TYPE string,
           lv_http_status     TYPE i.
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
      decode_sign( RECEIVING rv_secret = lv_secret ).
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
