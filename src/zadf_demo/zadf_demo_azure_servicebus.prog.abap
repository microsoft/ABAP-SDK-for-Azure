*&---------------------------------------------------------------------*
*& Report  ZADF_DEMO_AZURE_SERVICEBUS
*&
*&---------------------------------------------------------------------*
**---------------Prerequisite------------------------------------------*
** Basic Configuration Steps similar to Azure eventhub implementation  *
** guide in Github                                                     *
**---------------------------------------------------------------------*
** 1.Create your service Bus namespace in  Azure portal link           *
** Refer Document:                                                     *
** https://docs.microsoft.com/en-us/azure/                             *
** service-bus-messaging/service-bus-create-namespace-portal           *
** Choose pricing Tier as 'Standard' for availing topics service       *
** 2. Create Topic                                                     *
** 3. Create Access policy under your topics                           *
** 4. Add Subscription under your topics                               *
** 5. Double Click on Policy created in above steps and                *
** Copy 'Primary key' or 'Secondary key'                               *
** for configuration in table ZADF_CONFIG through below steps          *
** 5.1 Configure RFC destination of type 'G' with target host          *
** <Servicebus namespace>.servicebus.windows.net                       *
** Path Prefix : /<topics name>/messages                               *
** 5.2 Table entry for ZREST_CONFIG for interface ID e.g 'DEMO_SB'     *
** 5.3 Table entry for ZREST_CONF_MISC for interface ID e.g 'DEMO_SB'  *
** with method as 'POST' along with other field values                 *
** 5.4 Table entry for ZADF_CONFIG with base URI value as blank and    *
** Enable processing flag as blank , type as 'Azure Servicebus  '      *
** and SAS key as 'primary key' or 'Secondary key', Service call type  *
** as 'S'                                                              *
** 5.5 Maintain your Policy name for interface id in table             *
** 'ZADF_EHUB_POLICY'                                                  *
*&---------------------------------------------------------------------*
REPORT ZADF_DEMO_AZURE_SERVICEBUS.
CONSTANTS: gc_interface TYPE zinterface_id VALUE 'DEMO_SB1'.

TYPES: BEGIN OF lty_data,
         carrid    TYPE    s_carr_id,
         connid    TYPE    s_conn_id,
         fldate    TYPE    s_date,
         planetype TYPE    s_planetye,
       END OF lty_data.

DATA:       it_headers       TYPE tihttpnvp,
            wa_headers       TYPE LINE OF tihttpnvp,
            lv_string        TYPE string,
            lv_response      TYPE string,
            cx_interface     TYPE REF TO zcx_interace_config_missing,
            cx_http          TYPE REF TO zcx_http_client_failed,
            cx_adf_service   TYPE REF TO zcx_adf_service,
            oref_servicebus  TYPE REF TO zcl_adf_service_servicebus,
            oref             TYPE REF TO zcl_adf_service,
            filter           TYPE zbusinessid,
            lv_http_status   TYPE i,
            lo_json          TYPE REF TO cl_trex_json_serializer,
            lv1_string       TYPE string,
            lv_xstring       TYPE xstring,
            it_data          TYPE STANDARD TABLE OF lty_data.

*Sample data population for sending it to Azure Service Bus
SELECT  carrid connid fldate planetype
        FROM sflight UP TO 10 ROWS
        INTO TABLE it_data.

IF sy-subrc EQ 0.

  TRY.
**Calling Factory method to instantiate Service Bus client

      oref = zcl_adf_service_factory=>create( iv_interface_id = gc_interface
                                              iv_business_identifier = filter ).
      oref_servicebus ?= oref.

**Setting Expiry time
      CALL METHOD oref_servicebus->add_expiry_time
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
    CLEAR it_headers.
    wa_headers-name = 'BrokerProperties'.
    wa_headers-value = '{"Label":"SFLIGHTData"}'.
    APPEND wa_headers TO it_headers.
    CLEAR  wa_headers.
**Sending Converted SAP data to Azure Servicebus
      CALL METHOD oref_servicebus->send
        EXPORTING
          request        = lv_xstring  "Input XSTRING of SAP Business data
          it_headers     = it_headers  "Header attributes
        IMPORTING
          response       = lv_response       "Response from Service Bus
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
    MESSAGE 'SAP data not sent to Azure Service Bus' TYPE 'E'.
  ELSE.
    MESSAGE 'SAP data sent to Azure Service Bus' TYPE 'I'.
  ENDIF.
ELSE.
  MESSAGE 'No data in SFLIFHT' TYPE 'E'.
ENDIF.
