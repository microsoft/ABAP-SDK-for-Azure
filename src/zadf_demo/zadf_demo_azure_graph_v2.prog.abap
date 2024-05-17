*&---------------------------------------------------------------------*
*& Report zadf_demo_azure_graph_v2
*&---------------------------------------------------------------------*
*& This report shows how to use MS Graph with Azure AAD Token V2.0
*& to access information about SharePoint Objects like Sites, lists,
*& list items and files.
*& It also shows how to upload a file to a document library
*& of SharePoint Online site collection
*&---------------------------------------------------------------------*
REPORT zadf_demo_azure_graph_v2.

DATA: filter          TYPE zbusinessid,
      oref_aad_token  TYPE REF TO zcl_adf_service_aad,
      oref_graph      TYPE REF TO zcl_adf_service_graph,
      oref_graph_post TYPE REF TO zcl_adf_service_graph,
      lv_client_id    TYPE string,
      lv_request      TYPE xstring,
      lv_resource     TYPE string VALUE '',
      lt_headers      TYPE tihttpnvp,
      cx_interface    TYPE REF TO zcx_interace_config_missing,
      cx_http         TYPE REF TO zcx_http_client_failed,
      cx_adf_service  TYPE REF TO zcx_adf_service,
      cx_graph        TYPE REF TO zcx_adf_service_graph,
      lv_http_status  TYPE i.

DATA: o365_graph TYPE REF TO zcl_o365_service.

TRY.
    " get access token by calling V2.0 oauth service
    DATA(oref) = zcl_adf_service_factory=>create( iv_interface_id        = 'TOKEN_V2'
                                                  iv_business_identifier = filter ).
    oref_aad_token ?= oref.

    oref_aad_token->get_aad_token(
      EXPORTING
        iv_client_id = '<enter client it here>'
        iv_resource  = ''
        iv_scope = 'https://graph.microsoft.com/.default'
      IMPORTING
        ev_aad_token                = DATA(lv_aad_token)
        ev_response                 = DATA(lv_response)
).

    " create rest connection
    oref = zcl_adf_service_factory=>create( iv_interface_id        = 'GRAPH_GET'
                                            iv_business_identifier = filter ).
    oref_graph ?= oref.

    " get SharePoint site id
    DATA(ls_SPSite) = oref_graph->zif_adf_service_graph~get_spsite_by_name(
      EXPORTING
        iv_aad_token    = lv_aad_token
        iv_hostname = '<your host>.sharepoint.com'
        iv_site = '<my site>'
       IMPORTING
        ev_http_status  = lv_http_status
    ).

    " rebuild connection for next query. This is necessary as methods always close connection when done
    oref = zcl_adf_service_factory=>create( iv_interface_id        = 'GRAPH_GET'
                                            iv_business_identifier = filter ).
    oref_graph ?= oref.

    " fetch all lists of given site
    DATA(lt_lists) = oref_graph->zif_adf_service_graph~get_splists(
                       EXPORTING
                         iv_aad_token   = lv_aad_token
                         iv_site_id     = ls_spsite-id
                       IMPORTING
                         ev_http_status = lv_http_status
                     ).

    " rebuild connection for next query. This is necessary as methods always close connection when done
    oref = zcl_adf_service_factory=>create( iv_interface_id        = 'GRAPH_GET'
                                            iv_business_identifier = filter ).
    oref_graph ?= oref.

    " fetch list items of a list
    DATA(lt_items) = oref_graph->zif_adf_service_graph~get_splistitems(
                       EXPORTING
                         iv_aad_token   = lv_aad_token
                         iv_site_id     = ls_spsite-id
                         iv_list_id     = lt_lists[ 4 ]-id
                       IMPORTING
                         ev_http_status = lv_http_status
                     ).

    " rebuild connection for next query. This is necessary as methods always close connection when done
    oref = zcl_adf_service_factory=>create( iv_interface_id        = 'GRAPH_GET'
                                            iv_business_identifier = filter ).
    oref_graph ?= oref.

    " fetch all SharePoint Drives aka Document Libraries
    DATA(lt_drives) = oref_graph->zif_adf_service_graph~get_spdrives(
                        EXPORTING
                          iv_aad_token   = lv_aad_token
                          iv_site_id     = ls_spsite-id
                        IMPORTING
                          ev_http_status = lv_http_status
                      ).


    " rebuild connection for next query. This is necessary as methods always close connection when done
    " But this time we want a PUT request
    oref = zcl_adf_service_factory=>create( iv_interface_id        = 'GRAPH_PUT'
                                            iv_business_identifier = filter ).
    oref_graph ?= oref.

    " read data from file
    DATA: lt_bin_data TYPE w3mimetabtype.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = 'C:\Temp\Mappe1.xlsx'
        filetype                = 'BIN'
*    has_field_separator     = space
*    header_length           = 0
*    read_by_line            = 'X'
*    dat_mode                = space
*    codepage                =
*    ignore_cerr             = abap_true
*    replacement             = '#'
*    virus_scan_profile      =
      IMPORTING
        filelength              = DATA(lv_file_length)
*    header                  =
      CHANGING
        data_tab                = lt_bin_data
*    isscanperformed         = space
*  EXCEPTIONS
*    file_open_error         = 1
*    file_read_error         = 2
*    no_batch                = 3
*    gui_refuse_filetransfer = 4
*    invalid_type            = 5
*    no_authority            = 6
*    unknown_error           = 7
*    bad_data_format         = 8
*    header_not_allowed      = 9
*    separator_not_allowed   = 10
*    header_too_long         = 11
*    unknown_dp_error        = 12
*    access_denied           = 13
*    dp_out_of_memory        = 14
*    disk_full               = 15
*    dp_timeout              = 16
*    not_supported_by_gui    = 17
*    error_no_gui            = 18
*    others                  = 19
    ).

    DATA(lv_xstring) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_bin_data iv_size = lv_file_length ).


    " upload file to SharePoint
    DATA(response) = oref_graph->zif_adf_service_graph~upload_file_to_sharepoint(
                       EXPORTING
                         iv_aad_token   = lv_aad_token
                         iv_site_id     = ls_spsite-id
                         iv_drive_id    = lt_drives[ name = 'Documents' ]-id
                         iv_filename    = 'MySapTest.xlsx'
                         iv_binary      = lv_xstring
                       IMPORTING
                         ev_http_status = lv_http_status
                     ).
    WRITE: / 'HTTP Status: ', lv_http_status.
    "DATA(lv_json_result) = /ui2/cl_json=>serialize( data = ls_spsite pretty_name = abap_true ).
    cl_demo_output=>display_json( lt_items[ 1 ]-fields ).


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
