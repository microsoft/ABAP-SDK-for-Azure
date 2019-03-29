*&---------------------------------------------------------------------*
*& Report ZADF_DEMO_AZURE_GRAPH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zadf_demo_azure_graph.

CONSTANTS: gc_token_interface TYPE zinterface_id VALUE 'ADB2CTOKEN',
           gc_graph_get       TYPE zinterface_id VALUE 'ADB2CGR_GE',
           gc_graph_post      TYPE zinterface_id VALUE 'ADB2CGR_PO',
           gc_graph_patch     TYPE zinterface_id VALUE 'ADB2CGR_PA'.

DATA: filter          TYPE zbusinessid,
      lv_client_id    TYPE string,
      lv_request      TYPE xstring,
      lv_resource     TYPE string VALUE '',
      lt_headers      TYPE tihttpnvp,
      cx_interface    TYPE REF TO zcx_interace_config_missing,
      cx_http         TYPE REF TO zcx_http_client_failed,
      cx_adf_service  TYPE REF TO zcx_adf_service,
      cx_graph        TYPE REF TO zcx_adf_service_graph,
      oref_aad_token  TYPE REF TO zcl_adf_service_aad,
      oref_graph      TYPE REF TO zcl_adf_service_graph,
      oref_graph_post TYPE REF TO zcl_adf_service_graph.

PARAMETERS:
  p_token  RADIOBUTTON GROUP rg1,
  p_readal RADIOBUTTON GROUP rg1 DEFAULT 'X',
  p_create RADIOBUTTON GROUP rg1.

TRY.
    DATA(oref) = zcl_adf_service_factory=>create( iv_interface_id        = gc_token_interface
                                                  iv_business_identifier = filter ).

    oref_aad_token ?= oref.

    DATA: lv_description TYPE rfcdoc_d.

    " To avoid storing the client id in this report
    " I've used the description of the destination
    " to save the client id.
    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination = 'MS_AZURE_GRAPH'
      IMPORTING
        description = lv_description.

    lv_client_id = lv_description.

    oref_aad_token->get_aad_token(
      EXPORTING
        iv_client_id                = lv_client_id
        iv_resource                 = lv_resource
      IMPORTING
        ev_aad_token                = DATA(lv_aad_token)
        ev_response                 = DATA(lv_response)
    ).

    CASE abap_true.
      WHEN p_token.
        WRITE: / 'Token :', lv_aad_token.
      WHEN p_create.

        oref = zcl_adf_service_factory=>create( iv_interface_id        = gc_graph_post
                                                iv_business_identifier = filter ).

        oref_graph_post ?= oref.

        DATA: ls_user TYPE zadf_service_graph_user.

        ls_user-account_enabled = abap_true.
        APPEND INITIAL LINE TO ls_user-sign_in_names ASSIGNING FIELD-SYMBOL(<fs_sign_in_name>).
        <fs_sign_in_name>-type  = 'emailAddress'.
        <fs_sign_in_name>-value = 'joeconsumer4@gmail.com'.
        ls_user-creation_type = 'LocalAccount'.
        ls_user-display_name = 'Joe Consumer'.
        ls_user-given_name = 'Joe'.
        ls_user-surname = 'Consumer'.
        ls_user-mail_nickname = 'joec4'.
        ls_user-password_profile-password = 'P@ssword!'.
        ls_user-password_profile-force_change_password_next_log = abap_false.
        ls_user-password_policies = 'DisablePasswordExpiration'.

        DATA(lv_json) = /ui2/cl_json=>serialize(
                          EXPORTING
                            data        = ls_user    " Data to serialize
                            pretty_name = abap_true    " Pretty Print property names
                        ).

        REPLACE FIRST OCCURRENCE OF 'forceChangePasswordNextLog' IN lv_json WITH 'forceChangePasswordNextLogin'.

        " Convert input string data to Xstring format
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = lv_json
          IMPORTING
            buffer = lv_request
          EXCEPTIONS
            failed = 1
            OTHERS = 2.
        IF sy-subrc <> 0.
        ENDIF.

        oref_graph_post->create_user(
          EXPORTING
            request         = lv_request
            iv_aad_token    = lv_aad_token
          IMPORTING
            response        = DATA(lv_user_response)
            ev_http_status  = DATA(lv_http_status)
        ).

        WRITE: / 'Response: ',    lv_user_response.
        WRITE: / 'HTTP Status: ', lv_http_status.

        " cl_demo_output=>display_json( lv_user_response ).

        /ui2/cl_json=>deserialize(
          EXPORTING
            json          = lv_user_response
            pretty_name   = abap_true
          CHANGING
            data          = ls_user    " Data to serialize
        ).
        WRITE: / 'Object ID: ', ls_user-object_id.

      WHEN p_readal.
        oref = zcl_adf_service_factory=>create( iv_interface_id        = gc_graph_get
                                                iv_business_identifier = filter ).

        oref_graph ?= oref.

        oref_graph->get_users(
          EXPORTING
            iv_aad_token    = lv_aad_token
          IMPORTING
            response        = lv_user_response
            ev_http_status  = lv_http_status
        ).

        WRITE: / 'HTTP Status: ', lv_http_status.
        cl_demo_output=>display_json( lv_user_response ).
    ENDCASE.
  CATCH zcx_adf_service_graph INTO cx_graph.
    DATA(lv_string) = cx_graph->get_text( ).
    MESSAGE lv_string TYPE 'E'.
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
