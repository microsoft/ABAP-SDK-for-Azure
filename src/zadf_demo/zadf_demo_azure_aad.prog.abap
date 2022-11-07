
*&---------------------------------------------------------------------*
*& Report  ZADF_DEMO_AZURE_AAD
*&
*&---------------------------------------------------------------------*
*& This is a demo program to get token from Azure Active Directory(AAD).
*& For demo purpose we have considered Azure Key vault as an external application.
*& In real world scenario, you can use ABAP SDK library in case your
*& Web App/API requires AAD token for authentication purpose.
*&---------------------------------------------------------------------*
REPORT zadf_demo_azure_aad.

DATA:  gv_interface_aad TYPE zinterface_id VALUE 'DEMO_AAD',
       gv_message_bid   TYPE zbusinessid   VALUE  'TEST_AAD',
       gv_string        TYPE string,
       oref             TYPE REF TO zcl_adf_service,
       oref_aad         TYPE REF TO zcl_adf_service_aad,
       cx_adf_service   TYPE REF TO zcx_adf_service,
       cx_interface     TYPE REF TO zcx_interace_config_missing,
       cx_http          TYPE REF TO zcx_http_client_failed,
       gv_aad_token     TYPE string,
       gv_response      TYPE string.

**Calling Factory method to instantiate AAD client
oref = zcl_adf_service_factory=>create(
                                     iv_interface_id =  gv_interface_aad
                                     iv_business_identifier = gv_message_bid ).
oref_aad ?=  oref.

TRY.
    CALL METHOD oref_aad->get_aad_token
      EXPORTING
        iv_client_id = '************' " Input client id as per implementation guide for AAD
        iv_resource  = 'https://vault.azure.net'  "Resource for Azure Key vault application
      IMPORTING
        ev_aad_token = gv_aad_token
        ev_response  = gv_response.
  CATCH zcx_interace_config_missing INTO cx_interface.
    gv_string = cx_interface->get_text( ).
    MESSAGE gv_string TYPE 'E'.
  CATCH zcx_http_client_failed INTO cx_http .
    gv_string = cx_http->get_text( ).
    MESSAGE gv_string TYPE 'E'.
  CATCH zcx_adf_service INTO cx_adf_service.
    gv_string = cx_adf_service->get_text( ).
    MESSAGE gv_string TYPE 'E'.

ENDTRY.

IF NOT gv_aad_token IS INITIAL.
  WRITE: 'AAD token retrived successfully for Authentication of Azure Key vault as an Application'. NEW-LINE.
  CONCATENATE 'Bearer' space gv_aad_token INTO gv_aad_token RESPECTING BLANKS.
  WRITE: 'AAD Token :', gv_aad_token.
ENDIF.
