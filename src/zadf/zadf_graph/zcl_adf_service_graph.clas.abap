CLASS zcl_adf_service_graph DEFINITION

  PUBLIC

  INHERITING FROM zcl_adf_service

  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_adf_service_graph.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_adf_service_graph IMPLEMENTATION.


  METHOD zif_adf_service_graph~create_calendar_event.
    DATA: lo_response              TYPE REF TO if_rest_entity,
          lo_request               TYPE REF TO if_rest_entity,
          lv_expiry                TYPE string,
          lv_sas_token             TYPE string,
          lv_msg                   TYPE string,
          lv_path_prefix           TYPE string,
          lcx_adf_service          TYPE REF TO zcx_adf_service,
          lv_host                  TYPE rfcdisplay-rfchost,
          lv_host_s                TYPE string,
          lv_http_events           TYPE i,
          lv_result_calendar_event TYPE  zif_adf_service_graph~calendar_event.

    IF go_rest_api IS BOUND.

      lv_path_prefix = 'users/04131c86-58f1-4b6c-be8a-696a4f5b522d/calendar/events'.

      IF NOT lv_path_prefix IS INITIAL.
        go_rest_api->zif_rest_framework~set_uri( lv_path_prefix ).
      ENDIF.
      lv_host_s = gv_host.
**Add header attributes in REST call.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).
      add_request_header( iv_name = 'Host' iv_value = lv_host_s ).
      add_request_header( iv_name = 'Authorization' iv_value = |Bearer | && iv_aad_token ).

      DATA(lv_body_json) = /ui2/cl_json=>serialize( data = iv_calendar_event compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).


      go_rest_api->zif_rest_framework~set_binary_body( CONV #( lv_body_json ) ).

**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute(
        io_entity = lo_request
        async     = gv_asynchronous
        is_retry  = gv_is_try
      ).
      ev_http_status = go_rest_api->get_status( ).
      IF lo_response IS BOUND.
        DATA(lo_response_string) = lo_response->get_string_data( ).

        /ui2/cl_json=>deserialize(
                        EXPORTING
                          json = lo_response_string   " Data to serialize
                          pretty_name = abap_true    " Pretty Print property names
                        CHANGING
                          data = lv_result_calendar_event
                      ).
        response = lv_result_calendar_event.
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.

      IF ev_http_status <> 201. " Created
        DATA(lt_errors) = json_to_http_fields( iv_response_data = lo_response_string ).
        READ TABLE lt_errors ASSIGNING FIELD-SYMBOL(<fs_error>) INDEX 1.
        RAISE EXCEPTION TYPE zcx_adf_service_graph.
        "      EXPORTING
        "       textid        = '1'
        "      error_message = <fs_error>-value.
      ENDIF.

      go_rest_api->close( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_adf_service_graph~get_events.

    DATA: lo_response        TYPE REF TO if_rest_entity,
          lo_request         TYPE REF TO if_rest_entity,
          lv_path_prefix     TYPE string,
          lv_host_s          TYPE string,
          lt_calendar_events TYPE  zif_adf_service_graph~calendar_events.

    IF go_rest_api IS BOUND.

      lv_path_prefix = '/users/04131c86-58f1-4b6c-be8a-696a4f5b522d/calendar/events'.
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
        DATA(response) = lo_response->get_string_data( ).
        /ui2/cl_json=>deserialize(
                            EXPORTING
                              json = response   " Data to serialize
                            "  pretty_name = abap_true    " Pretty Print property names
                            CHANGING
                              data = lt_calendar_events
                          ).
        rt_calendar_events = lt_calendar_events.
      ELSE.

        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      go_rest_api->close( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_adf_service_graph~get_users.

    TYPES: BEGIN OF response,
             value TYPE  zif_adf_service_graph~users,
           END OF response.
    DATA: lo_response    TYPE REF TO if_rest_entity,
          lo_request     TYPE REF TO if_rest_entity,
          lv_path_prefix TYPE string,
          lv_host_s      TYPE string,
          ls_response    TYPE response.



    IF go_rest_api IS BOUND.

      lv_path_prefix = '/users'.
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
        DATA(response) = lo_response->get_string_data( ).
        /ui2/cl_json=>deserialize(
                            EXPORTING
                              json = response   " Data to serialize

                            "  pretty_name = abap_true    " Pretty Print property names
                            CHANGING
                              data = ls_response
                          ).
        rt_users = ls_response-value.
      ELSE.

        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      go_rest_api->close( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
