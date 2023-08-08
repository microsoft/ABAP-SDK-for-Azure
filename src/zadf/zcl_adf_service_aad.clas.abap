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
    importing
      !IV_TOKENGEN_FLAG type CHAR01 optional
    exporting
      value(EV_AAD_TOKEN) type STRING
      value(EV_RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods GET_AAD_FCI_TOKEN
    importing
      value(IV_ASSERTION_TOKEN) type STRING
      value(IV_CLIENT_ID) type STRING
      value(IV_SCOPE) type STRING
    exporting
      value(EV_ACCESS_TOKEN) type STRING
      value(EV_RESPONSE) type STRING
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
  methods MI_TOKEN_STORE
    importing
      !IT_MI_RESPONSE type TIHTTPNVP .
  methods MI_TOKEN_GET
    importing
      !IM_INTERFACE_ID type ZINTERFACE_ID
    returning
      value(RV_TOKEN) type STRINGVAL .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_AAD IMPLEMENTATION.


  METHOD get_aad_fci_token.
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

************************************************************************
    lv_mediatype = if_rest_media_type=>gc_appl_www_form_url_encoded.

    CREATE OBJECT form_data_helper
      EXPORTING
        io_entity = lo_request.

    wa_params-name  =  'scope'.
    wa_params-value =  cl_http_utility=>escape_url( iv_scope ).
    APPEND wa_params TO it_params.
    CLEAR wa_params.

    wa_params-name  = 'client_assertion_type'.
    wa_params-value = cl_http_utility=>escape_url( 'urn:ietf:params:oauth:client-assertion-type:jwt-bearer' ) .
    APPEND wa_params TO it_params.
    CLEAR wa_params.

    wa_params-name  = 'client_assertion'.
    wa_params-value =  iv_assertion_token .
    APPEND wa_params TO it_params.
    CLEAR wa_params.

**    decode_sign( receiving rv_secret = lv_secret ).
*    TRY.
*        IF gv_sas_key IS INITIAL.
*          lv_secret = read_ssf_key( ).
*        ELSE.
*          lv_secret = read_key( ).
*        ENDIF.
*      CATCH zcx_adf_service INTO lcx_adf_service.
*        lv_msg =  lcx_adf_service->get_text( ).
*        MESSAGE lv_msg TYPE 'I'.
*    ENDTRY.

*    wa_params-name = 'client_secret'.
*    wa_params-value = lv_secret.
*    APPEND wa_params TO it_params.
*    CLEAR wa_params.

    wa_params-name = 'grant_type'.
    wa_params-value = 'client_credentials'.
    APPEND wa_params TO it_params.
    CLEAR wa_params.

    go_rest_api->set_request_header( iv_name = 'Content-Type'  iv_value = lv_mediatype ).
    go_rest_api->set_string_body( cl_http_utility=>fields_to_string( it_params ) ) .
    CLEAR : lv_secret, it_params.

    IF go_rest_api IS BOUND.
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
      ev_access_token = lv_token.
    ENDIF.
  ENDMETHOD.


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

* Insert Begin VBANSAL- Fetch stored Token
      mi_token_get( EXPORTING im_interface_id = gv_interface_id RECEIVING rv_token = DATA(lv_mi_token) ).
* Insert END VBANSAL- Fetch stored Token
      IF lv_mi_token IS INITIAL OR iv_tokengen_flag = 'X' .
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
* Insert Begin VBANSAL- Store MI Token
            mi_token_store( EXPORTING it_mi_response  = lt_response_fields ).
* Insert End VBANSAL- Store MI Token

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
      ELSE.
        ev_aad_token = lv_mi_token.
        go_rest_api->close( ).
        RETURN.
      ENDIF.
      ev_aad_token = lv_token.
    ENDIF.
*  ENDIF.
  ENDMETHOD.


  METHOD mi_token_get.

* This method use the old MI token stored in table zadf_token_exp
* To Avoid Token fetch load on Virtual Machine. Since it has limit of 20 request per second
* https://learn.microsoft.com/en-us/azure/virtual-machines/instance-metadata-service?tabs=windows
    DATA : lv_clientid   TYPE zadf_client_id_short,
           lv_resourceid TYPE zadf_resource_short.
    IF  im_interface_id IS NOT INITIAL.
      SELECT SINGLE interface_id,
              resource_id,
              client_id
              FROM zadf_mi_config
              INTO @DATA(ls_mi_config)
              WHERE interface_id = @im_interface_id.
      IF sy-subrc = 0.
        CLEAR : lv_clientid,lv_resourceid.
        lv_clientid = ls_mi_config-client_id.
        lv_resourceid = ls_mi_config-resource_id.

        SELECT SINGLE client_id,
               resource_id,
               interface_id,
               token,
               expire_in,
               expires_on
               FROM zadf_token_exp
               INTO @DATA(ls_token_exp)
                WHERE client_id  = @lv_clientid AND resource_id  = @lv_resourceid.
        IF sy-subrc <> 0.
          CLEAR ls_token_exp.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ls_token_exp-token IS NOT INITIAL.
      GET TIME STAMP FIELD DATA(lv_curr_stamp).

      CALL METHOD cl_http_utility=>if_http_utility~decode_base64
        EXPORTING
          encoded = ls_token_exp-token
        RECEIVING
          decoded = ls_token_exp-token.

      CALL METHOD cl_abap_tstmp=>subtract
        EXPORTING
          tstmp1 = ls_token_exp-expires_on
          tstmp2 = lv_curr_stamp
        RECEIVING
          r_secs = DATA(lv_diff).

      IF lv_diff > 300.
        rv_token  = ls_token_exp-token.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD mi_token_store.
* This method use to store MI token in table zadf_token_exp
* To Avoid Token fetch load on Virtual Machine. Since it has limit of 20 request per second
* https://learn.microsoft.com/en-us/azure/virtual-machines/instance-metadata-service?tabs=windows
    DATA ls_token_exp TYPE zadf_token_exp.

    IF it_mi_response[] IS NOT INITIAL.
      ls_token_exp-client_id        =  VALUE #( it_mi_response[ name = 'client_id' ]-value OPTIONAL ).
      ls_token_exp-resource_id      =  VALUE #( it_mi_response[ name = 'resource' ]-value  OPTIONAL ).
      ls_token_exp-token            =  VALUE #( it_mi_response[ name = 'access_token' ]-value  OPTIONAL ).
      ls_token_exp-expire_in        =  VALUE #( it_mi_response[ name = 'expires_in' ]-value  OPTIONAL ).
*      DATA(lv_unixtime)             =  VALUE #( it_mi_response[ name = 'expires_on' ]-value  OPTIONAL ).

      IF ls_token_exp-token IS NOT INITIAL.
        CALL METHOD cl_http_utility=>if_http_utility~encode_base64
          EXPORTING
            unencoded = ls_token_exp-token
          RECEIVING
            encoded   = ls_token_exp-token.
      ENDIF.

      IF ls_token_exp-expire_in IS NOT INITIAL.
        GET TIME STAMP FIELD DATA(lv_timestamp). "" UTC time stamp
        TRY.
            CALL METHOD cl_abap_tstmp=>add
              EXPORTING
                tstmp   = lv_timestamp
                secs    = CONV #( ls_token_exp-expire_in )
              RECEIVING
                r_tstmp = DATA(lv_ltimestamp).
          CATCH cx_parameter_invalid_range.
            RETURN.
          CATCH cx_parameter_invalid_type.
            RETURN.
        ENDTRY.
        ls_token_exp-expires_on    = lv_ltimestamp.
      ENDIF.

      ls_token_exp-curr_date     = sy-datum.
      ls_token_exp-curr_time     = sy-uzeit.
      ls_token_exp-uname         = sy-uname.
      ls_token_exp-interface_id  = gv_interface_id.

      IF ls_token_exp-client_id IS NOT INITIAL AND ls_token_exp-resource_id IS NOT INITIAL AND ls_token_exp-token IS NOT INITIAL.
        MODIFY zadf_token_exp FROM ls_token_exp.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
