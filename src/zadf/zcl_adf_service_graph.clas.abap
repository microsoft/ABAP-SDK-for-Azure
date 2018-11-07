CLASS zcl_adf_service_graph DEFINITION
  PUBLIC
  INHERITING FROM zcl_adf_service
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS create_user
      IMPORTING
        !request              TYPE xstring
        VALUE(iv_aad_token)   TYPE string
      EXPORTING
        VALUE(response)       TYPE string
        VALUE(ev_http_status) TYPE i
      RAISING
        zcx_adf_service
        zcx_adf_service_graph .
    METHODS update_user
      IMPORTING
        !iv_user_object_id    TYPE string
        !request              TYPE xstring
        VALUE(iv_aad_token)   TYPE string
      EXPORTING
        VALUE(response)       TYPE string
        VALUE(ev_http_status) TYPE i
      RAISING
        zcx_adf_service
        zcx_adf_service_graph .
    METHODS get_users
      IMPORTING
        VALUE(iv_aad_token)   TYPE string
      EXPORTING
        VALUE(response)       TYPE string
        VALUE(ev_http_status) TYPE i
      RAISING
        zcx_adf_service .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADF_SERVICE_GRAPH IMPLEMENTATION.


  METHOD create_user.

    DATA: lo_response     TYPE REF TO if_rest_entity,
          lo_request      TYPE REF TO if_rest_entity,
          lv_expiry       TYPE string,
          lv_sas_token    TYPE string,
          lv_msg          TYPE string,
          lv_path_prefix  TYPE string,
          lcx_adf_service TYPE REF TO zcx_adf_service,
          lv_host         TYPE rfcdisplay-rfchost,
          lv_host_s       TYPE string.

    IF go_rest_api IS BOUND.

      lv_path_prefix = '/users?api-version=1.6'.
      IF NOT lv_path_prefix IS INITIAL.
        go_rest_api->zif_rest_framework~set_uri( lv_path_prefix ).
      ENDIF.
      lv_host_s = gv_host.
**Add header attributes in REST call.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).
      add_request_header( iv_name = 'Host' iv_value = lv_host_s ).
      add_request_header( iv_name = 'Authorization' iv_value = |Bearer | && iv_aad_token ).

      go_rest_api->zif_rest_framework~set_binary_body( request ).
**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute(
        io_entity = lo_request
        async     = gv_asynchronous
        is_retry  = gv_is_try
      ).
      ev_http_status = go_rest_api->get_status( ).
      IF lo_response IS BOUND.
        response = lo_response->get_string_data( ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.

      IF ev_http_status <> 201. " Created
        DATA(lt_errors) = json_to_http_fields( iv_response_data = response ).
        READ TABLE lt_errors ASSIGNING FIELD-SYMBOL(<fs_error>) INDEX 1.
        RAISE EXCEPTION TYPE zcx_adf_service_graph
          EXPORTING
            textid        = zcx_adf_service_graph=>error_during_user_creation
            error_message = <fs_error>-value.
      ENDIF.

      go_rest_api->close( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_users.

    DATA: lo_response    TYPE REF TO if_rest_entity,
          lo_request     TYPE REF TO if_rest_entity,
          lv_path_prefix TYPE string,
          lv_host_s      TYPE string.

    IF go_rest_api IS BOUND.

      lv_path_prefix = '/users?api-version=1.6'.
      IF NOT lv_path_prefix IS INITIAL.
        go_rest_api->zif_rest_framework~set_uri( lv_path_prefix ).
      ENDIF.
      lv_host_s = gv_host.
**Add header attributes in REST call.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).
      add_request_header( iv_name = 'Host' iv_value = lv_host_s ).
      add_request_header( iv_name = 'Authorization' iv_value = |Bearer | && iv_aad_token ).

**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute(
        io_entity = lo_request
        async     = gv_asynchronous
        is_retry  = gv_is_try
      ).
      ev_http_status = go_rest_api->get_status( ).
      IF lo_response IS BOUND.
        response = lo_response->get_string_data( ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      go_rest_api->close( ).
    ENDIF.
  ENDMETHOD.


  METHOD update_user.

    DATA: lo_response     TYPE REF TO if_rest_entity,
          lo_request      TYPE REF TO if_rest_entity,
          lv_expiry       TYPE string,
          lv_sas_token    TYPE string,
          lv_msg          TYPE string,
          lv_path_prefix  TYPE string,
          lcx_adf_service TYPE REF TO zcx_adf_service,
          lv_host         TYPE rfcdisplay-rfchost,
          lv_host_s       TYPE string.

    IF go_rest_api IS BOUND.

      lv_path_prefix = '/users/' && iv_user_object_id && '?api-version=1.6'.
      IF NOT lv_path_prefix IS INITIAL.
        go_rest_api->zif_rest_framework~set_uri( lv_path_prefix ).
      ENDIF.
      lv_host_s = gv_host.
**Add header attributes in REST call.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).
      add_request_header( iv_name = 'Authorization' iv_value = |Bearer | && iv_aad_token ).

      go_rest_api->zif_rest_framework~set_binary_body( request ).
**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute(
        io_entity = lo_request
        async     = gv_asynchronous
        is_retry  = gv_is_try
      ).
      ev_http_status = go_rest_api->get_status( ).
      IF lo_response IS BOUND.
        response = lo_response->get_string_data( ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.

      IF ev_http_status <> 201. " Created
        DATA(lt_errors) = json_to_http_fields( iv_response_data = response ).
        READ TABLE lt_errors ASSIGNING FIELD-SYMBOL(<fs_error>) INDEX 1.
        RAISE EXCEPTION TYPE zcx_adf_service_graph
          EXPORTING
            textid        = zcx_adf_service_graph=>error_during_user_creation
            error_message = <fs_error>-value.
      ENDIF.

      go_rest_api->close( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
