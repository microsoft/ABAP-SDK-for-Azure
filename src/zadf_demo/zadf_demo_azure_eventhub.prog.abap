*&---------------------------------------------------------------------*
*& Report  ZADF_DEMO_AZURE_EVENTHUB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zadf_demo_azure_eventhub.

CONSTANTS: gc_interface TYPE zinterface_id VALUE 'HACK_CL'.

TYPES: BEGIN OF lty_data,
         carrid    TYPE     s_carr_id,
         connid    TYPE    s_conn_id,
         fldate    TYPE    s_date,
         planetype TYPE    s_planetye,
       END OF lty_data.

DATA: it_headers     TYPE tihttpnvp,
      wa_headers     TYPE LINE OF tihttpnvp,
      lv_string      TYPE string,
      lv_response    TYPE string,
      cx_interface   TYPE REF TO zcx_interace_config_missing,
      cx_http        TYPE REF TO zcx_http_client_failed,
      cx_adf_service TYPE REF TO zcx_adf_service,
      oref_eventhub  TYPE REF TO zcl_adf_service_eventhub,
      oref           TYPE REF TO zcl_adf_service,
      filter         TYPE zbusinessid,
      lv_http_status TYPE i,
      lo_json        TYPE REF TO cl_trex_json_serializer,
      lv1_string     TYPE string,
      lv_xstring     TYPE xstring,
      it_data        TYPE STANDARD TABLE OF lty_data.

SELECTION-SCREEN begin of BLOCK bl1 with frame.
PARAMETERS: p_ehifid type zinterface_id DEFAULT gc_interface.
SELECTION-SCREEN end of BLOCK bl1.

START-OF-SELECTION.

*Sample data population for sending it to Azure eventhub
SELECT  carrid connid fldate planetype
        FROM sflight UP TO 10 ROWS
        INTO TABLE it_data.

IF sy-subrc EQ 0.

  TRY.
**Calling Factory method to instantiate eventhub client

      oref = zcl_adf_service_factory=>create( iv_interface_id        = p_ehifid
                                              iv_business_identifier = filter ).
      oref_eventhub ?= oref.

**Setting Expiry time
      CALL METHOD oref_eventhub->add_expiry_time
        EXPORTING
          iv_expiry_hour = 0
          iv_expiry_min  = 15
          iv_expiry_sec  = 0.

      CREATE OBJECT lo_json
        EXPORTING
          data = it_data.
      lo_json->serialize( ).
      lv1_string  = lo_json->get_data( ).


*Convert input string data to Xstring format
      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv1_string
        IMPORTING
          buffer = lv_xstring
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
      ENDIF.

**Sending Converted SAP data to Azure Eventhub
      CALL METHOD oref_eventhub->send
        EXPORTING
          request        = lv_xstring  "Input XSTRING of SAP Business Event data
          it_headers     = it_headers  "Header attributes
        IMPORTING
          response       = lv_response       "Response from EventHub
          ev_http_status = lv_http_status.   "Status

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

  IF lv_http_status NE '201' AND
     lv_http_status NE '200'.
    MESSAGE 'SAP data not sent to Azure EventHub' TYPE 'E'.
  ELSE.
    MESSAGE 'SAP data sent to Azure EventHub' TYPE 'I'.
  ENDIF.
ELSE.
  MESSAGE 'No data in SFLIFHT' TYPE 'E'.
ENDIF.
