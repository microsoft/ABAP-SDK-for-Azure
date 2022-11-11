*&---------------------------------------------------------------------*
*& Report  ZADF_DEMO_AZURE_SERVICEBUS
*&
*&---------------------------------------------------------------------*
**---------------Prerequisite------------------------------------------*
** Basic Configuration Steps similar to Azure eventhub implementation  *
** guide in Github                                                     *
**---------------------------------------------------------------------*
*-----------Steps for access to ServiceBus via SAS keys----------------*
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
**-----------Steps for access via Azure Active directory---------------*
** 1.Follow above steps i.e. 1 to 4 to create ServiceBus namespace     *
** 2.Register an App in Azure Active Directory under App registerations*
** 3.Now, assign relevant roles to this AAD app under Access Control   *
**   (IAM) in ServiceBus namespace.                                    *
** 4.1 Configure 2 RFC destinations of type 'G' with below target host:*
** 4.2 Service Bus destination                                         *
**    Target Host : <Servicebus namespace>.servicebus.windows.net      *
**    Path Prefix : /<topics name>/messages                            *
**  4.3 AAD Destination                                                *
**     Target Host : login.microsoftonline.com                         *
**     Path Prefix : /<AAD_Tenant_ID>/oauth2/token                     *
** 5.1 Table entry for ZREST_CONFIG for AAD interface ID e.g 'AAD_SBUS'*
** 5.2 Table entry for ZREST_CONFIG for Service Bus interface ID       *
**  e.g 'DEMO_SB'                                                      *
** 5.3 Maintain 2 Table entries for ZREST_CONF_MISC for above interface*
**  each with method as 'POST' along with other field values           *
** 5.4 Maintain 2 Table entries for ZADF_CONFIG with base URI value as *
**     as blank,Enable processing as blank,Service call type as 'S'    *
**  i) In case of AAD Interface ID                                     *
**        Interface Type as 'Azure Active Directory'                   *
*         SAS key as Registered AAD App Secret                         *
**  ii) In case of ServiceBus Interface ID                             *
**       Interface Type as 'Azure Servicebus'                          *
**       SAS Key as blank                                              *
** 5.5 Maintain your Policy name for interface id in table             *
** 'ZADF_EHUB_POLICY'                                                  *
*&---------------------------------------------------------------------*
REPORT zadf_demo_azure_servicebus.
CONSTANTS: gc_interface TYPE zinterface_id VALUE 'DEMO_SB1'.

TYPES: BEGIN OF lty_data,
         carrid    TYPE    s_carr_id,
         connid    TYPE    s_conn_id,
         fldate    TYPE    s_date,
         planetype TYPE    s_planetye,
       END OF lty_data.

DATA: it_headers      TYPE tihttpnvp,
      wa_headers      TYPE LINE OF tihttpnvp,
      lv_string       TYPE string,
      lv_response     TYPE string,
      cx_interface    TYPE REF TO zcx_interace_config_missing,
      cx_http         TYPE REF TO zcx_http_client_failed,
      cx_adf_service  TYPE REF TO zcx_adf_service,
      oref_servicebus TYPE REF TO zcl_adf_service_servicebus,
      lo_ref_aad      TYPE REF TO zcl_adf_service_aad,
      oref            TYPE REF TO zcl_adf_service,
      filter          TYPE zbusinessid,
      lv_http_status  TYPE i,
      lo_json         TYPE REF TO cl_trex_json_serializer,
      lv1_string      TYPE string,
      lv_xstring      TYPE xstring,
      it_data         TYPE STANDARD TABLE OF lty_data.

* Call below methods in order to fetch AAD token
TRY.
    CALL METHOD zcl_adf_service_factory=>create
      EXPORTING
        iv_interface_id        = 'AAD_SBUS'         " AAD Interface
        iv_business_identifier = 'AAD_TOKEN_IDENT'
      RECEIVING
        ro_service             = DATA(lo_oref).
  CATCH zcx_adf_service zcx_interace_config_missing zcx_http_client_failed.
ENDTRY.

lo_ref_aad ?=  lo_oref.

* Generate the Bearer token
IF lo_ref_aad IS BOUND.
  TRY.
      CALL METHOD lo_ref_aad->get_aad_token
        EXPORTING
          iv_client_id = 'Input_AAD_ClientId'                     " Input AAD client ID registered in Azure
          iv_resource  = 'https://servicebus.azure.net'           " Input Service Bus resource
        IMPORTING
          ev_aad_token = DATA(lv_aad_token)
          ev_response  = DATA(lv_add_response).
    CATCH zcx_adf_service .
    CATCH zcx_interace_config_missing.
    CATCH zcx_http_client_failed .
  ENDTRY.

  CLEAR it_headers.
* Add token to headers
  it_headers = VALUE #( ( name = 'Authorization' value = lv_aad_token ) ).
ENDIF.

*Sample data population for sending it to Azure Service Bus
SELECT carrid connid fldate planetype
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
