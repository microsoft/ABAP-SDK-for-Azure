CLASS zcl_adf_service_azureopenai DEFINITION
  PUBLIC
  INHERITING FROM zcl_adf_service
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_chatcompletion_message,
        role    TYPE string,
        content TYPE string,
      END OF ty_chatcompletion_message .
    TYPES:
      tty_chatcompletion_messages TYPE STANDARD TABLE OF ty_chatcompletion_message WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_chatcompletion_input,
        messages   TYPE tty_chatcompletion_messages,
        max_tokens TYPE i,
        user       TYPE string,
        n          TYPE i,
      END OF ty_chatcompletion_input .

    METHODS set_compopr_req_body
      IMPORTING
        !im_azopenai_reqbody     TYPE zadf_azopenai_compopr_req OPTIONAL
        !im_chatcomp_reqbody     TYPE ty_chatcompletion_input OPTIONAL
        !im_set_completion_opver TYPE char250_d DEFAULT '/COMPLETIONS?API-VERSION=2022-12-01'
      RETURNING
        VALUE(rv_xstring)        TYPE xstring
      RAISING
        zcx_adf_service .
    METHODS set_chatcomp_req_body
      IMPORTING
        !im_azopenai_reqbody     TYPE ty_chatcompletion_input OPTIONAL
        !im_set_completion_opver TYPE char250_d DEFAULT '/CHAT/COMPLETIONS?API-VERSION=2023-03-15-preview'
      RETURNING
        VALUE(rv_xstring)        TYPE xstring
      RAISING
        zcx_adf_service .

    METHODS send
        REDEFINITION .
protected section.

  data GS_AZUREOPENAI_SCHEMA type ZADF_AZOPENAI_COMPOPR_REQ .
  data GV_URI_STRING type STRING .
  data GS_CHATCOMP_SCHEMA type TY_CHATCOMPLETION_INPUT .

  methods GET_SAS_TOKEN
    redefinition .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADF_SERVICE_AZUREOPENAI IMPLEMENTATION.


  METHOD get_sas_token.
    DATA :
           lv_authstring        TYPE string,
           sign                 TYPE string,
           format               TYPE i,
           lv_sas_key           TYPE string.

    format = 18.
* Encript using SAS key, URL and  Exp Time.
    DEFINE encrypt_key.

      IF gv_sas_key IS INITIAL.
        lv_sas_key = read_ssf_key( ).
      ELSE.
        lv_sas_key = read_key( ).
      ENDIF.
      lv_authstring  = lv_sas_key.

      CLEAR : lv_sas_key.
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


  METHOD set_chatcomp_req_body.

    gs_chatcomp_schema = im_azopenai_reqbody.

    IF gs_chatcomp_schema-max_tokens IS INITIAL .
      gs_chatcomp_schema-max_tokens  = 300.
    ENDIF.

    IF gs_chatcomp_schema-user IS INITIAL.
      gs_chatcomp_schema-user = sy-uname.
    ENDIF.

    IF gs_chatcomp_schema-n IS INITIAL .
      gs_chatcomp_schema-n = 1.
    ENDIF.

    IF  im_set_completion_opver IS NOT INITIAL.
      gv_uri_string = im_set_completion_opver.
      TRANSLATE gv_uri_string TO LOWER CASE.
    ENDIF.

* Convert Data into json format
    /ui2/cl_json=>serialize(
       EXPORTING
         data             =  gs_chatcomp_schema
         pretty_name      = 'L'
*         EXPAND_INCLUDES  = space
       RECEIVING
         r_json           = DATA(lv_jsondata) ).


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


  METHOD set_compopr_req_body.

    IF im_chatcomp_reqbody IS NOT INITIAL.
      set_chatcomp_req_body( EXPORTING im_azopenai_reqbody = im_chatcomp_reqbody
                                        im_set_completion_opver = im_set_completion_opver
                             RECEIVING  rv_xstring      = rv_xstring  ).
      RETURN.
    ENDIF.

    gs_azureopenai_schema = im_azopenai_reqbody.

    IF gs_azureopenai_schema-max_tokens IS INITIAL .
      gs_azureopenai_schema-max_tokens  = 300.
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
         pretty_name      = 'L'
*         EXPAND_INCLUDES  = space
       RECEIVING
         r_json           = DATA(lv_jsondata) ).

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
