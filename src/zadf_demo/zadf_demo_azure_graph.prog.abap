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
      lv_http_status  TYPE i.

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
        iv_client_id                = '************' " Input client id as per implementation guide for AAD
        iv_resource                 = 'https://graph.microsoft.com' "lv_resource
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

        DATA: ls_calendar_event TYPE zif_adf_service_graph=>calendar_event.

        " Get email of current user
        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = sy-uname                " User Name
          TABLES
            return   = lt_return
            addsmtp  = lt_smtp.                " E-Mail Addresses BAPI Structure

        .

        " In demo program assume we only have one and only one email address
        DATA(ls_smtp) = lt_smtp[ 1 ].

        ls_calendar_event-subject = 'Test event'.
        ls_calendar_event-organizer-emailaddress = ls_smtp-e_mail.

        ls_calendar_event-start-datetime = sy-timlo.
        ls_calendar_event-start-timezone = sy-zonlo.

        ls_calendar_event-end-datetime = sy-timlo.
        ls_calendar_event-end-timezone = sy-zonlo.



        DATA(ls_response_calendar_event) = oref_graph_post->zif_adf_service_graph~create_calendar_event(
                EXPORTING
                  iv_calendar_event = ls_calendar_event
                  iv_aad_token    = lv_aad_token
                IMPORTING
                  ev_http_status  = lv_http_status
              ).

** Deserialize into JSON
        DATA(lv_body_json) = CONV xstring( /ui2/cl_json=>serialize(
                               EXPORTING
                                 data        = ls_response_calendar_event    " Data to serialize
                                 pretty_name = abap_true    " Pretty Print property names
                             ) ).

        WRITE: / 'Response: ',    lv_body_json.
        WRITE: / 'HTTP Status: ', lv_http_status.

        " cl_demo_output=>display_json( lv_user_response ).



      WHEN p_readal.
        oref = zcl_adf_service_factory=>create( iv_interface_id        = gc_graph_get
                                                iv_business_identifier = filter ).

        oref_graph ?= oref.


        data(lt_users) = oref_graph->zif_adf_service_graph~get_users(
          EXPORTING
            iv_aad_token    = lv_aad_token
          IMPORTING
            ev_http_status  = lv_http_status
        ).

        data(lv_json_result) = /ui2/cl_json=>serialize( data = lt_users pretty_name = abap_true ).

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
