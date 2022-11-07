
*&---------------------------------------------------------------------*
*& Report  ZADF_DEMO_AZURE_KV
*&
*&---------------------------------------------------------------------*
*& Demo program to retrieve secret from Azure key vault( Azure KV)
*& In real world scenario, you can use ABAP SDK library in case
*& it is required to get Key/secrets/passwords/Connection strings etc.
*& from Azure KV in SAP.
*&---------------------------------------------------------------------*
REPORT zadf_demo_azure_kv.

DATA: oref             TYPE REF TO zcl_adf_service,
      oref1            TYPE REF TO zcl_adf_service_keyvault,
      gv_interface_aad TYPE zinterface_id VALUE 'DEMO_AAD',
      gv_message_bid   TYPE zbusinessid   VALUE  'TEST_AAD',
      gv_interface_akv TYPE zinterface_id VALUE 'DEMO_AKV',
      gv_string        TYPE string,
      gv_response      TYPE string,
      cx_adf_service   TYPE REF TO zcx_adf_service,
      cx_interface     TYPE REF TO zcx_interace_config_missing,
      gv_filter        TYPE zbusinessid VALUE 'AAD_KV_KEY',
      cx_http          TYPE REF TO zcx_http_client_failed,
      gv_key           TYPE string,
      gw_headers       TYPE ihttpnvp,
      gt_headers       TYPE tihttpnvp.

TRY.
**Calling Factory method to instantiate AAD client
    oref = zcl_adf_service_factory=>create( iv_interface_id = gv_interface_aad  "AAD interface id
                                           iv_business_identifier = gv_message_bid ).
    oref1 ?=  oref.

**Getting Key from Keyvault
    CALL METHOD oref1->get_kv_details
      EXPORTING
        iv_kv_interface_id = gv_interface_akv "KV interface ID
        iv_client_id       = '*******'    " Input your AAD client id
        iv_resource        = 'https://vault.azure.net'
        it_headers         = gt_headers
      IMPORTING
        ev_key             = gv_key
        ev_response        = gv_response.
  CATCH zcx_interace_config_missing INTO cx_interface.
    gv_string = cx_adf_service->get_text( ).
    MESSAGE gv_string TYPE 'E'.
  CATCH zcx_http_client_failed INTO cx_http .
    gv_string = cx_adf_service->get_text( ).
    MESSAGE gv_string TYPE 'E'.
  CATCH zcx_adf_service INTO cx_adf_service.
    gv_string = cx_adf_service->get_text( ).
    MESSAGE gv_string TYPE 'E'.
ENDTRY.

IF gv_key IS INITIAL.
  WRITE:  'Key is not retrieved'.
ELSE.
  WRITE: 'Key is:', gv_key.
ENDIF.
