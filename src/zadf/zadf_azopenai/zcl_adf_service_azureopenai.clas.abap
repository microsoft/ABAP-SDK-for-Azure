class ZCL_ADF_SERVICE_AZUREOPENAI definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public .

public section.

  methods SET_COMPOPR_REQ_BODY
    importing
      !IM_AZOPENAI_REQBODY type ZADF_AZOPENAI_COMPOPR_REQ
      !IM_SET_COMPLETION_OPVER type CHAR250_D default '/COMPLETIONS?API-VERSION=2022-12-01'
    returning
      value(RV_XSTRING) type XSTRING
    raising
      ZCX_ADF_SERVICE .

  methods SEND
    redefinition .
protected section.

  data GS_AZUREOPENAI_SCHEMA type ZADF_AZOPENAI_COMPOPR_REQ .
  data GV_URI_STRING type STRING .

  methods GET_SAS_TOKEN
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ADF_SERVICE_AZUREOPENAI IMPLEMENTATION.


  METHOD get_sas_token.
    DATA : lv_string_to_sign    TYPE string,
           encoded_base_address TYPE string,
           lv_authstring         TYPE string,
           sign                 TYPE string,
           final_token          TYPE string,
           decoded              TYPE xstring,
           conv                 TYPE REF TO cl_abap_conv_out_ce,
           conv_in              TYPE REF TO cl_abap_conv_in_ce,
           lv_decoded_xstr      TYPE xstring,
           format               TYPE i,
           new_expiry           TYPE string,
           lv_sas_key           TYPE string,
           lv_expiry_time       TYPE string,
           lv_baseurl           TYPE string,
           lv_encodedexptime    TYPE string.

    format = 18.
* Encript using SAS key, URL and  Exp Time.
    DEFINE encrypt_key.

      IF gv_sas_key IS INITIAL.
        lv_sas_key = read_ssf_key( ).
      ELSE.
        lv_sas_key = read_key( ).
      ENDIF.
      lv_authstring  = lv_sas_key.

      CLEAR : lv_sas_key, decoded.
    END-OF-DEFINITION.
    encrypt_key.

* Build Final Token
    IF NOT lv_authstring IS INITIAL.
      sign =  cl_http_utility=>escape_url( lv_authstring ).
      rv_sas_token = sign.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>sas_key_not_generated
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


  METHOD send.
    DATA : lo_response     TYPE REF TO if_rest_entity,
           lo_request      TYPE REF TO if_rest_entity,
           lv_expiry       TYPE string,
           lv_sas_token    TYPE string,
           lv_msg          TYPE string,
           lcx_adf_service TYPE REF TO zcx_adf_service,
           lt_headers1     TYPE tihttpnvp.

    IF go_rest_api IS BOUND.

* Read token from headers for Managed Identity/AAD
      IF line_exists( it_headers[ name = gc_mi_auth ] ).
        DATA(lv_processing_method) = gc_mi_auth.
        lv_sas_token = it_headers[ name = gc_mi_auth ]-value.
      ENDIF.

      TRY.
          CASE lv_processing_method.
            WHEN gc_mi_auth.
*              add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ). "lv_sas_token ).

            WHEN OTHERS.
              get_sas_token( EXPORTING iv_baseaddress = gv_uri
                             RECEIVING rv_sas_token  = lv_sas_token ).
* Add provided SAS token/generated token via Account Key to the headers
              add_request_header( iv_name = 'api-key' iv_value = lv_sas_token ). "lv_sas_token ).
          ENDCASE.
        CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.

*   Add custom headers.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/json' ).

      go_rest_api->zif_rest_framework~set_binary_body( request ).
      IF NOT it_headers[] IS INITIAL.
        go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = it_headers[] ).
      ENDIF.
      IF gv_uri_string IS NOT INITIAL.
        go_rest_api->zif_rest_framework~set_uri( gv_uri_string ).
      ENDIF.

**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = abap_false ).
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


  METHOD SET_COMPOPR_REQ_BODY.

    gs_azureopenai_schema = im_azopenai_reqbody.

    IF gs_azureopenai_schema-max_tokens IS INITIAL .
      gs_azureopenai_schema-max_tokens  = 1000.
    ENDIF.

    IF gs_azureopenai_schema-user IS INITIAL.
      gs_azureopenai_schema-user = sy-uname.
    ENDIF.

    IF gs_azureopenai_schema-n IS INITIAL .
      gs_azureopenai_schema-n = 1.
    ENDIF.

    IF  im_set_completion_opver IS NOT INITIAL.
      gv_uri_string = im_set_completion_opver.
      TRANSLATE gv_uri_string TO LOWER CASE.
    ENDIF.

* Convert Data into json format
    /ui2/cl_json=>serialize(
       EXPORTING
         data             =  gs_azureopenai_schema
         pretty_name      = 'X'
*         EXPAND_INCLUDES  = space
       RECEIVING
         r_json           = DATA(lv_jsondata) ).

    REPLACE ALL OCCURRENCES OF 'maxTokens' IN lv_jsondata WITH 'max_tokens'.


*Convert string data to Xstring format
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_jsondata
      IMPORTING
        buffer = rv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>error_con_xstring
          interface_id = gv_interface_id.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
