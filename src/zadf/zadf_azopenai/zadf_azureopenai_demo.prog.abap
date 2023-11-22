*&---------------------------------------------------------------------*
*& Report  ZADF_AZUREOPENAI_DEMO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zadf_azureopenai_demo.

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

TRY.
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
        request        = lv_pxstring  "Input XSTRING of Promt
        it_headers     = lt_headers  "Header attributes
      IMPORTING
        response       = lv_response       "Response from AZUREOPENAI
        ev_http_status = lv_http_status.   "Status

*    DATA ls_comp_response  TYPE zadf_azopenai_compopr_res.

    DATA lr_resp      TYPE REF TO data.

    /ui2/cl_json=>deserialize(
     EXPORTING
        json             =  lv_response
      CHANGING
        data           =  lr_resp ).

    DATA lv_comp_res  TYPE string.

    FIELD-SYMBOLS: <data_tab>       TYPE ANY TABLE.

    IF lr_resp IS BOUND.
      ASSIGN lr_resp->* TO FIELD-SYMBOL(<resp>).
      ASSIGN COMPONENT 'CHOICES' OF STRUCTURE <resp> TO FIELD-SYMBOL(<results>).
      ASSIGN <results>->* TO <data_tab>.

      LOOP AT <data_tab> ASSIGNING FIELD-SYMBOL(<structure>).
        ASSIGN <structure>->* TO <resp>.
        ASSIGN COMPONENT 'TEXT' OF STRUCTURE <resp> TO FIELD-SYMBOL(<field>).
        IF <field> IS ASSIGNED.
          ASSIGN <field>->* TO FIELD-SYMBOL(<resp1>).
          cl_demo_output=>write( <resp1> ).
          cl_demo_output=>display( ).
        ENDIF.
        UNASSIGN: <field>.
      ENDLOOP.
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
