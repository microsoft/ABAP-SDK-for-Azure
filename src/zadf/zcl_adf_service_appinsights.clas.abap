class ZCL_ADF_SERVICE_APPINSIGHTS definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public .

public section.

  methods SEND
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ADF_SERVICE_APPINSIGHTS IMPLEMENTATION.


METHOD send.
  DATA :  lo_response     TYPE REF TO if_rest_entity,
          lo_request      TYPE REF TO if_rest_entity,
          lv_expiry       TYPE string,
          lv_sas_token    TYPE string,
          lv_msg          TYPE string,
          lcx_adf_service TYPE REF TO zcx_adf_service,
          lw_headers      TYPE ihttpnvp,
          lt_headers      LIKE it_headers.

  IF go_rest_api IS BOUND.

*   Set the path prefix from the headers instead of creating many RFC
    lt_headers[] = it_headers[].
*   Add custom headers.
    add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).
*    add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ).
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
ENDCLASS.
