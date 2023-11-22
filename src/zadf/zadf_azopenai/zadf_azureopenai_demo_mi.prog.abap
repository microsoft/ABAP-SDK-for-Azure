*&---------------------------------------------------------------------*
*& Report  ZADF_AZUREOPENAI_DEMO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zadf_azureopenai_demo_mi.

DATA : ls_payload TYPE zadf_azopenai_compopr_req.

DATA:
  lt_headers       TYPE tihttpnvp,
  wa_headers       TYPE LINE OF tihttpnvp,
  lv_string        TYPE string,
  lv_response      TYPE string,
  cx_interface     TYPE REF TO zcx_interace_config_missing,
  cx_http          TYPE REF TO zcx_http_client_failed,
  cx_adf_service   TYPE REF TO zcx_adf_service,
  oref_azureopenai TYPE REF TO zcl_adf_service_azureopenai,
  oref             TYPE REF TO zcl_adf_service,
  filter           TYPE zbusinessid,
  lv_http_status   TYPE i.


PARAMETERS : p_aaireq TYPE char250_d.


DATA:
  lo_oref    TYPE REF TO zcl_adf_service,
  lo_ref_aad TYPE REF TO zcl_adf_service_aad.



TRY.
    CALL METHOD zcl_adf_service_factory=>create
      EXPORTING
        iv_interface_id        = 'VIK_MI_CDB' " Generate the token
        iv_business_identifier = 'AI_TOKEN'
      RECEIVING
        ro_service             = lo_oref.
  CATCH zcx_adf_service .
  CATCH zcx_interace_config_missing .
  CATCH zcx_http_client_failed .
ENDTRY.

lo_ref_aad ?=  lo_oref.

IF lo_ref_aad IS BOUND.

  TRY.
      CALL METHOD lo_ref_aad->get_aad_token_msi
        IMPORTING
          ev_aad_token = DATA(lv_aad_token)
          ev_response  = lv_response.

    CATCH zcx_adf_service .
    CATCH zcx_interace_config_missing.
    CATCH zcx_http_client_failed .
  ENDTRY.

ENDIF.


TRY.

    DATA: it_headers TYPE tihttpnvp.

    wa_headers-name = 'Authorization'.
*      CONCATENATE 'Bearer'  lv_aad_token INTO wa_headers-value SEPARATED BY space.
    CONCATENATE 'Bearer'  lv_aad_token INTO wa_headers-value SEPARATED BY space.
    APPEND wa_headers TO it_headers.

**Calling Factory method to instantiate AZUREOPENAI client
    oref = zcl_adf_service_factory=>create( iv_interface_id = 'ZVIK_AZAI'
                                            iv_business_identifier = filter ).
    oref_azureopenai ?= oref.

    ls_payload-prompt  = p_aaireq.

    oref_azureopenai->set_compopr_req_body( EXPORTING im_azopenai_reqbody = ls_payload
                                            RECEIVING rv_xstring      = DATA(lv_pxstring) ).

**Sending Converted SAP data to Azure AZUREOPENAI
    CALL METHOD oref_azureopenai->send
      EXPORTING
        request        = lv_pxstring  "Input XSTRING of SAP Business Event data
        it_headers     = lt_headers  "Header attributes
      IMPORTING
        response       = lv_response       "Response from AZUREOPENAI
        ev_http_status = lv_http_status.   "Status

    DATA ls_comp_response  TYPE zadf_azopenai_compopr_res.

    /ui2/cl_json=>deserialize(
     EXPORTING
        json             =  lv_response
      CHANGING
        data           =  ls_comp_response ).

    DATA lv_comp_res  TYPE string.

    LOOP AT ls_comp_response-choices ASSIGNING FIELD-SYMBOL(<lfs_data>).
      lv_comp_res = <lfs_data>-text.
*       WRITE : <lfs_data>-text.
    ENDLOOP.


    CALL METHOD cl_http_utility=>if_http_utility~escape_html
      EXPORTING
        unescaped = lv_comp_res
*       keep_num_char_ref = ABAP_UNDEFINED
      RECEIVING
        escaped   = DATA(lv_escaped).

    cl_demo_output=>write( lv_comp_res ).
    cl_demo_output=>display( ).


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
