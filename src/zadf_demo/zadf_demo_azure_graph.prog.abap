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
      oref_graph_post TYPE REF TO zcl_adf_service_graph,
      lt_smtp         TYPE  bapiadsmtp_t,
      lt_return       TYPE bapiret2_t,
      lv_http_status  TYPE i,
      ls_end_ts       TYPE timestamp,
      ls_address      TYPE bapiaddr3,
      lv_description  TYPE rfcdoc_d.

PARAMETERS:
  p_token RADIOBUTTON GROUP rg1,
  p_readc RADIOBUTTON GROUP rg1 DEFAULT 'X',
  p_readu RADIOBUTTON GROUP rg1,
  p_creau RADIOBUTTON GROUP rg1,
  p_c_id  TYPE string.

TRY.
    DATA(oref) = zcl_adf_service_factory=>create( iv_interface_id        = gc_token_interface
                                                  iv_business_identifier = filter ).

    oref_aad_token ?= oref.

    " If parameter is initia then use the method that can also be used in background calls
    IF p_c_id IS INITIAL.
      " To avoid storing the client id in this report
      " I've used the description of the destination
      " to save the client id.
      CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
        EXPORTING
          destination = 'MS_AZURE_GRAPH'
        IMPORTING
          description = lv_description.

      lv_client_id = lv_description.
    ELSE.
      lv_client_id = p_c_id.
    ENDIF.

    oref_aad_token->get_aad_token(
      EXPORTING
        iv_client_id                = lv_client_id " Input client id as per implementation guide for AAD
        iv_resource                 = 'https://graph.microsoft.com' "lv_resource
      IMPORTING
        ev_aad_token                = DATA(lv_aad_token)
        ev_response                 = DATA(lv_response)
    ).

    CASE abap_true.
      WHEN p_token.
        WRITE: / 'Token :', lv_aad_token.

      WHEN p_readu.
        oref = zcl_adf_service_factory=>create( iv_interface_id        = gc_graph_get
                                                iv_business_identifier = filter ).

        oref_graph ?= oref.


        DATA(lt_users) = oref_graph->zif_adf_service_graph~get_users(
          EXPORTING
            iv_aad_token    = lv_aad_token
          IMPORTING
            ev_http_status  = lv_http_status
        ).

        DATA(lv_json_result) = /ui2/cl_json=>serialize( data = lt_users pretty_name = abap_true ).

        WRITE: / 'HTTP Status: ', lv_http_status.
        cl_demo_output=>display_json( lv_json_result ).
      WHEN p_creau.
*        oref = zcl_adf_service_factory=>create( iv_interface_id        = gc_graph_post
*                                                iv_business_identifier = filter ).
*
*        oref_graph_post ?= oref.
*
*        DATA: ls_user TYPE zadf_service_graph_user.
*
*        ls_user-account_enabled = abap_true.
*        APPEND INITIAL LINE TO ls_user-sign_in_names ASSIGNING FIELD-SYMBOL(<fs_sign_in_name>).
*        <fs_sign_in_name>-type            = 'emailAddress'.
*        <fs_sign_in_name>-value           = 'joeconsumer4@gmail.com'.
*        ls_user-creation_type             = 'LocalAccount'.
*        ls_user-display_name              = 'Joe Consumer'.
*        ls_user-given_name                = 'Joe'.
*        ls_user-surname                   = 'Consumer'.
*        ls_user-mail_nickname             = 'joec4'.
*        ls_user-password_profile-password = 'P@ssword!'.
*        ls_user-password_policies         = 'DisablePasswordExpiration'.
*        ls_user-password_profile-force_change_password_next_log = abap_false.
*
*        DATA(lv_json) = /ui2/cl_json=>serialize(
*                            data        = ls_user    " Data to serialize
*                            pretty_name = abap_true    " Pretty Print property names
*                        ).
*
*        REPLACE FIRST OCCURRENCE OF 'forceChangePasswordNextLog' IN lv_json WITH 'forceChangePasswordNextLogin'.
*
*        " Convert input string data to Xstring format
*        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*          EXPORTING
*            text   = lv_json
*          IMPORTING
*            buffer = lv_request
*          EXCEPTIONS
*            failed = 1
*            OTHERS = 2.
*        IF sy-subrc <> 0.
*        ENDIF.
*
*        oref_graph_post->create_user(
*          EXPORTING
*            request         = lv_request
*            iv_aad_token    = lv_aad_token
*          IMPORTING
*            response        = DATA(lv_user_response)
*            ev_http_status  = DATA(lv_http_status)
*        ).
*
*        WRITE: / 'Response: ',    lv_user_response.
*        WRITE: / 'HTTP Status: ', lv_http_status.
*
*        " cl_demo_output=>display_json( lv_user_response ).
*
*        /ui2/cl_json=>deserialize(
*          EXPORTING
*            json          = lv_user_response
*            pretty_name   = abap_true
*          CHANGING
*            data          = ls_user    " Data to serialize
*        ).
*        WRITE: / 'Object ID: ', ls_user-object_id.

      WHEN p_readc.

        " Get email of current user
        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = sy-uname                " User Name
          IMPORTING
            address  = ls_address
          TABLES
            return   = lt_return
            addsmtp  = lt_smtp.                " E-Mail Addresses BAPI Structure

        " In demo program assume we only have one and only one email address
        DATA(ls_smtp) = lt_smtp[ 1 ].

        oref = zcl_adf_service_factory=>create( iv_interface_id        = gc_graph_get
                                                  iv_business_identifier = filter ).

        oref_graph ?= oref.


        DATA(lt_events) = oref_graph->zif_adf_service_graph~get_events(
          EXPORTING
          iv_userprincipaltoken = CONV #( ls_smtp-e_mail )
            iv_aad_token    = lv_aad_token
          IMPORTING
            ev_http_status  = lv_http_status
        ).

        lv_json_result = /ui2/cl_json=>serialize( data = lt_events pretty_name = abap_true ).

        WRITE: / 'HTTP Status: ', lv_http_status.
        cl_demo_output=>display_json( lv_json_result ).
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
