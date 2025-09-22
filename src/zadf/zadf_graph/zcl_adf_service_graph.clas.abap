CLASS zcl_adf_service_graph DEFINITION
  PUBLIC
  INHERITING FROM zcl_adf_service
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_adf_service_graph .
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
          lv_result_calendar_event TYPE  zif_adf_service_graph~calendar_event,
          lv_body_xstring          TYPE xstring.

    IF go_rest_api IS BOUND.

      DATA(lv_calendar_event) = iv_calendar_event.

      " Get id for user
      lv_path_prefix = |/users/{ iv_calendar_event-organizer-emailaddress-address }/calendar/events|.


      go_rest_api->zif_rest_framework~set_uri( lv_path_prefix ).

      lv_host_s = gv_host.
**Add header attributes in REST call.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).
      add_request_header( iv_name = 'Host' iv_value = lv_host_s ).
      add_request_header( iv_name = 'Authorization' iv_value = |Bearer | && iv_aad_token ).

      DATA(lv_body_json) = /ui2/cl_json=>serialize( data = iv_calendar_event compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv_body_json
        IMPORTING
          buffer = lv_body_xstring
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>error_con_xstring
            interface_id = gv_interface_id.
      ENDIF.
      go_rest_api->zif_rest_framework~set_binary_body( lv_body_xstring ).

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

*        RAISE EXCEPTION TYPE zcx_adf_service
*          EXPORTING
*            textid = zcx_adf_service=>general_exception
*            text   = <fs_error>-value.

      ENDIF.

      go_rest_api->close( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_adf_service_graph~get_events.

    TYPES: BEGIN OF response,
             value TYPE  zif_adf_service_graph~calendar_events,
           END OF response.

    DATA: lo_response        TYPE REF TO if_rest_entity,
          lo_request         TYPE REF TO if_rest_entity,
          lv_path_prefix     TYPE string,
          lv_host_s          TYPE string,
          lt_calendar_events TYPE  zif_adf_service_graph~calendar_events,
          lt_response        TYPE response.

    IF go_rest_api IS BOUND.

      lv_path_prefix = |/users/{ iv_userprincipaltoken }/calendar/events|.
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
                              data = lt_response
                          ).
        rt_calendar_events = lt_response-value.
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
        IF ev_http_status = 400.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid = zcx_adf_service=>general_exception
              text   = response.
        ELSE.
          /ui2/cl_json=>deserialize(
                              EXPORTING
                                json = response   " Data to serialize
                              "  pretty_name = abap_true    " Pretty Print property names
                              CHANGING
                                data = ls_response
                            ).
          rt_users = ls_response-value.
        ENDIF.
      ELSE.

        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      go_rest_api->close( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_adf_service_graph~get_spsite_by_name.

    "response  zif_adf_service_graph~sharepointsite,

    DATA: lo_response    TYPE REF TO if_rest_entity,
          lo_request     TYPE REF TO if_rest_entity,
          lv_path_prefix TYPE string,
          lv_host_s      TYPE string,
          ls_response    TYPE zif_adf_service_graph~sharepointsite.

    IF go_rest_api IS BOUND.
      lv_path_prefix =  |/sites/{ iv_hostname }:/sites/{ iv_site }|.
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
        IF ev_http_status = 400.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid = zcx_adf_service=>general_exception
              text   = response.
        ELSE.
          /ui2/cl_json=>deserialize(
                              EXPORTING
                                json = response   " Data to serialize
                              "  pretty_name = abap_true    " Pretty Print property names
                              CHANGING
                                data = ls_response
                            ).
          rt_sp_site = ls_response.
        ENDIF.
      ELSE.

        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      go_rest_api->close( ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_adf_service_graph~get_splists.

    TYPES: BEGIN OF response,
             value TYPE  zif_adf_service_graph~sharepointlists,
           END OF response.

    DATA: lo_response    TYPE REF TO if_rest_entity,
          lo_request     TYPE REF TO if_rest_entity,
          lv_path_prefix TYPE string,
          lv_host_s      TYPE string,
          ls_response    TYPE response.

    IF go_rest_api IS BOUND.
      lv_path_prefix =  |/sites/{ iv_site_id }/lists|.
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
        DATA(lv_response) = lo_response->get_string_data( ).
        IF ev_http_status = 400.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid = zcx_adf_service=>general_exception
              text   = lv_response.
        ELSE.
          /ui2/cl_json=>deserialize(
                              EXPORTING
                                json = lv_response   " Data to serialize
                              "  pretty_name = abap_true    " Pretty Print property names
                              CHANGING
                                data = ls_response
                            ).
          rt_sp_lists = ls_response-value.
        ENDIF.
      ELSE.

        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      go_rest_api->close( ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_adf_service_graph~get_splistitems.

    TYPES: BEGIN OF response,
             value TYPE  zif_adf_service_graph~sharepointlistitems,
           END OF response.

    DATA: lo_response    TYPE REF TO if_rest_entity,
          lo_request     TYPE REF TO if_rest_entity,
          lv_path_prefix TYPE string,
          lv_host_s      TYPE string,
          ls_response    TYPE response.

    IF go_rest_api IS BOUND.
      lv_path_prefix =  |/sites/{ iv_site_id }/lists/{ iv_list_id }/items?expand=fields|.
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
        DATA(lv_response) = lo_response->get_string_data( ).
        IF ev_http_status = 400.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid = zcx_adf_service=>general_exception
              text   = lv_response.
        ELSE.
          /ui2/cl_json=>deserialize(
                              EXPORTING
                                json = lv_response   " Data to serialize
                              "  pretty_name = abap_true    " Pretty Print property names
                              CHANGING
                                data = ls_response
                            ).

          rt_sp_items = ls_response-value.
        ENDIF.
      ELSE.

        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      go_rest_api->close( ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_adf_service_graph~get_spdrives.

    TYPES: BEGIN OF response,
             value TYPE  zif_adf_service_graph~sharepointdrives,
           END OF response.

    DATA: lo_response    TYPE REF TO if_rest_entity,
          lo_request     TYPE REF TO if_rest_entity,
          lv_path_prefix TYPE string,
          lv_host_s      TYPE string,
          ls_response    TYPE response.

    IF go_rest_api IS BOUND.
      lv_path_prefix =  |/sites/{ iv_site_id }/drives|.
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
        DATA(lv_response) = lo_response->get_string_data( ).
        IF ev_http_status = 400.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid = zcx_adf_service=>general_exception
              text   = lv_response.
        ELSE.
          /ui2/cl_json=>deserialize(
                              EXPORTING
                                json = lv_response   " Data to serialize
                              "  pretty_name = abap_true    " Pretty Print property names
                              CHANGING
                                data = ls_response
                            ).

          rt_drives = ls_response-value.
        ENDIF.
      ELSE.

        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      go_rest_api->close( ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_adf_service_graph~upload_file_to_sharepoint.

    DATA: lo_response    TYPE REF TO if_rest_entity,
          lo_request     TYPE REF TO if_rest_entity,
          lv_path_prefix TYPE string,
          lv_host_s      TYPE string,
          ls_response    TYPE response.

    IF go_rest_api IS BOUND.
      lv_path_prefix =  |/sites/{ iv_site_id }/drives/{ iv_drive_id }/root:/{ iv_filename }:/content|.
      IF NOT lv_path_prefix IS INITIAL.
        go_rest_api->zif_rest_framework~set_uri( lv_path_prefix ).
      ENDIF.
      lv_host_s = gv_host.
**Add header attributes in REST call.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/octet-stream' ).
      add_request_header( iv_name = 'Host' iv_value = lv_host_s ).
      add_request_header( iv_name = 'Authorization' iv_value = |Bearer | && iv_aad_token ).

      go_rest_api->zif_rest_framework~set_binary_body( iv_binary ).

**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute(
        io_entity = lo_request
        async     = gv_asynchronous
        is_retry  = gv_is_try
      ).

      ev_http_status = go_rest_api->get_status( ).

      IF lo_response IS BOUND.
        DATA(lv_response) = lo_response->get_string_data( ).
        IF ev_http_status = 400.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid = zcx_adf_service=>general_exception
              text   = lv_response.
        ELSE.
          ev_response = lv_response.
        ENDIF.
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
