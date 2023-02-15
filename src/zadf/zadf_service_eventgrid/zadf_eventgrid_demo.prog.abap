*&---------------------------------------------------------------------*
*& Report  ZADF_EVENTGRID_DEMO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zadf_eventgrid_demo.

DATA : lt_payload TYPE TABLE OF zadf_str_egrid_schema,
       ls_payload TYPE  zadf_str_egrid_schema.

DATA:
  lt_headers     TYPE tihttpnvp,
  wa_headers     TYPE LINE OF tihttpnvp,
  lv_string      TYPE string,
  lv_response    TYPE string,
  cx_interface   TYPE REF TO zcx_interace_config_missing,
  cx_http        TYPE REF TO zcx_http_client_failed,
  cx_adf_service TYPE REF TO zcx_adf_service,
  oref_eventgrid TYPE REF TO zcl_adf_service_eventgrid,
  oref           TYPE REF TO zcl_adf_service,
  filter         TYPE zbusinessid,
  lv_http_status TYPE i,
  lo_json        TYPE REF TO cl_trex_json_serializer,
  lv1_string     TYPE string,
  lv_xstring     TYPE xstring.

TRY.
**Calling Factory method to instantiate eventgrid client
    oref = zcl_adf_service_factory=>create( iv_interface_id = 'DEMO_EGRID'
                                            iv_business_identifier = filter ).
    oref_eventgrid ?= oref.

**Setting Expiry time
    CALL METHOD oref_eventgrid->add_expiry_time
      EXPORTING
        iv_expiry_hour = 0
        iv_expiry_min  = 120
        iv_expiry_sec  = 0.

*Sample data population for sending it to Azure eventgrid
    SELECT  objectclas, objectid
         FROM cdhdr UP TO 2 ROWS
         INTO TABLE @DATA(lt_data).
    IF sy-subrc EQ 0.

* Convert Data into json format
      /ui2/cl_json=>serialize(
         EXPORTING
           data             = lt_data
           pretty_name      = 'X'
         RECEIVING
           r_json           = lv1_string ).

      GET TIME STAMP FIELD  DATA(lv_current_timestamp) .

      ls_payload = VALUE #(
                  topic            = ''                         " Optional (but if included, must match the Event Grid topic Azure Resource Manager ID exactly.
                                                                " If not included, Event Grid will stamp onto the event)
                  id               = '9932432499'               " Mendatory ( Unique identifier for the event) Like Billing Doc number created in SAP.
                  eventtype        = 'SAP.BILLING DOC.CREATED'  " Mandatory(One of the registered event types for this event source)
                                                                " Like SAP.BILLING DOC.CREATED
                  subject          = 'SAP/SALES/Billing DOC'    " Mandatory( Publisher-defined path to the event subject) Like
                                                                " 'SAP/SALES/Billing DOC'
                  "eventtime        = lv_current_timestamp      " Mandatory( The time the event is generated based on the provider's UTC time.
                  data             = lv1_string                " Optional ( Like Billing data in Json string )
             ) .

      APPEND ls_payload TO lt_payload.

      oref_eventgrid->set_eventgrid_schema_json( EXPORTING it_egrid_schema = lt_payload
                                            RECEIVING rv_xstring      = DATA(lv_pxstring) ).

**Sending Converted SAP data to Azure eventgrid
      CALL METHOD oref_eventgrid->send
        EXPORTING
          request        = lv_pxstring  "Input XSTRING of SAP Business Event data
          it_headers     = lt_headers  "Header attributes
        IMPORTING
          response       = lv_response       "Response from eventgrid
          ev_http_status = lv_http_status.   "Status


      IF lv_http_status NE '201' AND
         lv_http_status NE '200'.
        MESSAGE 'SAP data not sent to Azure eventgrid' TYPE 'E'.
      ELSE.
        MESSAGE 'SAP data sent to Azure eventgrid' TYPE 'I'.
      ENDIF.
    ELSE.
      MESSAGE 'No data in SFLIFHT' TYPE 'E'.
    ENDIF.

  CATCH zcx_interace_config_missing INTO cx_interface.
    lv_string = cx_interface->get_text( ).
    MESSAGE lv_string TYPE 'E'.
  CATCH zcx_http_client_failed INTO cx_http .
    lv_string = cx_http->get_text( ).
    MESSAGE lv_string TYPE 'E'.
  CATCH zcx_adf_service INTO cx_adf_service.
    lv_string = cx_adf_service->get_text( ).
    MESSAGE lv_string TYPE 'E'.

ENDTRY.
