class ZCL_O365_SERVICE_SHAREPOINT definition
  public
  inheriting from ZCL_O365_SERVICE
  final
  create public .

public section.

  methods SET_FILE_PARAMETERS
    importing
      value(IV_FILENAME) type LOCALFILE optional
      value(IV_FILETYPE) type CHAR10 optional
      value(IV_FOLDER_NAME) type CHAR200 optional
    raising
      ZCX_O365_SERVICE
      ZCX_O365_SERVICE_SHAREPOINT .
  methods READ_FILE_DATA
    importing
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      value(RESPONSE) type XSTRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_O365_SERVICE .
  methods SET_HEADER_ATTRIBUTES
    importing
      value(REQUEST) type XSTRING
      value(IT_HEADERS) type TIHTTPNVP .
  methods DELETE_FILE_FROM_FOLDER
    importing
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      value(RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_O365_SERVICE .
  methods CREATE_FOLDER
    importing
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      value(RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I .
  methods READ_ALL_FILE_FROM_FOLDER
    importing
      !IV_FOLDER_NAME type CHAR200
    exporting
      !GT_DATA type STANDARD TABLE .

  methods SEND
    redefinition .
protected section.
private section.

  data GV_TOKEN type STRING .
  constants GC_URI_SP type STRING value '/GetFolderByServerRelativeUrl(' ##NO_TEXT.
  constants GC_FILES_URI_NAME type STRING value '/Files/add(url=' ##NO_TEXT.
  constants GV_OVERWRITE type STRING value 'overwrite=true)' ##NO_TEXT.
  constants GC_I type CHAR1 value 'I' ##NO_TEXT.
  data GV_BUSINESSID type ZBUSINESSID .
  constants GC_SEPARATOR_1 type CHAR1 value '\' ##NO_TEXT.
  constants GC_SEPARATOR type CHAR1 value '/' ##NO_TEXT.
  constants GC_FOL_VALUE type STRING value 'true' ##NO_TEXT.
  constants GC_FOL_REQ1 type STRING value '{ "__metadata": { "type": "SP.Folder" }, "ServerRelativeUrl": "' ##NO_TEXT.
  constants GC_FOL_REQ2 type STRING value '" }' ##NO_TEXT.
  constants GC_FIELD type STRING value 'Edm.Boolean' ##NO_TEXT.
  constants GC_FOLDER type CHAR10 value '/folders' ##NO_TEXT.

  methods SET_URI_PATH .
  methods GET_AAD_TOKEN
    exporting
      value(EV_AAD_TOKEN) type STRING
      value(EV_RESPONSE) type STRING
    raising
      ZCX_O365_SERVICE
      ZCX_O365_SERVICE_SHAREPOINT .
  methods SEND_HEADER_EXCEL
    importing
      !REQUEST type XSTRING
      !IT_HEADERS type TIHTTPNVP
    exporting
      !EO_REQUEST type ref to IF_REST_ENTITY .
  methods SEND_HEADER_TXT
    importing
      !REQUEST type XSTRING
      !IT_HEADERS type TIHTTPNVP
    exporting
      !EO_REQUEST type ref to IF_REST_ENTITY .
  methods SEND_HEADER_CSV
    importing
      !REQUEST type XSTRING
      !IT_HEADERS type TIHTTPNVP
    exporting
      !EO_REQUEST type ref to IF_REST_ENTITY
    raising
      ZCX_O365_SERVICE_SHAREPOINT
      ZCX_O365_SERVICE .
  methods SET_HEADER_PARAMETERS
    importing
      value(REQUEST) type XSTRING
      value(IT_HEADERS) type TIHTTPNVP
    raising
      ZCX_O365_SERVICE
      ZCX_O365_SERVICE_SHAREPOINT .
  methods VALIDATE_PARAMETERS
    raising
      ZCX_O365_SERVICE .
  methods SET_URI_PATH_READ .
  methods SET_URI_PATH_FOR_DELETE .
  methods SET_URI_PATH_FOLDER .
  methods SET_BUSINESS_IDENTIFIER
    importing
      !IV_BUSINESS_IDENTIFIER type ZBUSINESSID .
  methods SEND_HEADER_PDF
    importing
      !REQUEST type XSTRING
      !IT_HEADERS type TIHTTPNVP
    exporting
      !EO_REQUEST type ref to IF_REST_ENTITY .
  methods SEND_HEADER_FOLDER
    importing
      !REQUEST type XSTRING
      !IT_HEADERS type TIHTTPNVP
    exporting
      !EO_REQUEST type ref to IF_REST_ENTITY .
ENDCLASS.



CLASS ZCL_O365_SERVICE_SHAREPOINT IMPLEMENTATION.


  METHOD create_folder.

    DATA: go_response        TYPE REF TO   if_rest_entity,
          go_request         TYPE REF TO   if_rest_entity,
          lv_response        TYPE          string,
          lv_msg             TYPE          string,
          lv_request         TYPE          xstring,
          lcx_o365_service   TYPE REF TO   zcx_o365_service,
          lt_string          TYPE TABLE OF string,
          lv_request_fol     TYPE          string,
          lv_string          TYPE          string,
          lv_folder          TYPE          string,
          lc_adf_service     TYPE REF TO   zcl_adf_service,
          lc_aad             TYPE REF TO   zcl_adf_service_aad,
          gt_response_fields TYPE tihttpnvp.

    IF gv_folder_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>folder_name_not_found
          interface_id = gv_interface_id.
    ENDIF.
**** Get Authorization
    TRY.
        CALL METHOD get_aad_token
          IMPORTING
            ev_aad_token = gv_token
            ev_response  = lv_response.
      CATCH zcx_o365_service INTO lcx_o365_service.
        lv_msg =  lcx_o365_service->get_text( ).
        MESSAGE lv_msg TYPE gc_i.
    ENDTRY.

    me->get_rest_api_ref( EXPORTING iv_business_identifier = gv_businessid ).

    CALL METHOD set_header_parameters(
      EXPORTING
        request    = lv_request
        it_headers = it_headers ).

    go_rest_api1->set_callingmethod('CREATE_FOLDER').

    me->set_uri_path_folder( ).

    SPLIT gv_folder_name AT gc_separator_1 INTO TABLE lt_string.
    if ( sy-subrc <> 0 ).

        lv_string = gv_folder_name.
        APPEND lv_string to lt_string.

      endif.

    LOOP AT lt_string INTO lv_string.

      CONCATENATE lv_folder lv_string gc_separator INTO lv_folder .
      CONCATENATE gc_fol_req1 lv_folder  gc_fol_req2 INTO lv_request_fol .

      go_rest_api1->zif_rest_framework~set_string_body( lv_request_fol ).

      go_response = go_rest_api1->zif_rest_framework~execute( io_entity = go_request
                                                         async     = abap_false
                                                         is_retry  = abap_false ).
      ev_http_status = go_rest_api1->get_status( ).

      IF go_response IS BOUND.
        response = go_response->get_string_data( ).

        IF ( response IS NOT INITIAL AND ( ev_http_status = 200 OR ev_http_status = 201 )  ).

          gt_response_fields = me->json_to_http_fields( iv_response_data = response ).

          READ TABLE gt_response_fields TRANSPORTING NO FIELDS
            WITH KEY name = gc_field
                     value = gc_fol_value.

          IF ( sy-subrc <> 0 ).
            go_rest_api1->close( ).
            RAISE EXCEPTION TYPE zcx_o365_service
              EXPORTING
                textid       = zcx_o365_service=>error_in_folder_creation
                interface_id = gv_interface_id.
          ENDIF.
        ENDIF.
      ELSE.
        go_rest_api1->close( ).
        RAISE EXCEPTION TYPE zcx_o365_service
          EXPORTING
            textid       = zcx_o365_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
      CLEAR : lv_string.
*      WAIT UP TO 1 SECONDS.
    ENDLOOP.
    go_rest_api1->close( ).

  ENDMETHOD.


  METHOD delete_file_from_folder.

    DATA: go_response      TYPE REF TO if_rest_entity,
          go_request       TYPE REF TO if_rest_entity,
          cx_interface     TYPE REF TO zcx_interace_config_missing,
          cx_http          TYPE REF TO zcx_http_client_failed,
          lo_http_client   TYPE REF TO if_http_client,
          lv_response      TYPE string,
          lv_msg           TYPE string,
          lcx_o365_service TYPE REF TO zcx_o365_service,
          lv_request       TYPE xstring.

**** Get Authorization
    me->validate_parameters( ).
    TRY.
        CALL METHOD get_aad_token
          IMPORTING
            ev_aad_token = gv_token
            ev_response  = lv_response.
      CATCH zcx_o365_service INTO lcx_o365_service.
        lv_msg =  lcx_o365_service->get_text( ).
        MESSAGE lv_msg TYPE gc_i.
    ENDTRY.

    me->get_rest_api_ref( iv_business_identifier = ' ' ).
    CALL METHOD set_header_attributes(
      EXPORTING
        request    = lv_request
        it_headers = it_headers ).
    CREATE OBJECT cx_interface.
    CREATE OBJECT cx_http.
    go_rest_api1->set_callingmethod('DELETE_FILE_FROM_FOLDER').
    me->set_uri_path_for_delete( ).

    go_response = go_rest_api1->zif_rest_framework~execute( io_entity = go_request
                                                            async     = abap_false
                                                            is_retry  = abap_false ).
    ev_http_status = go_rest_api1->get_status( ).

    IF go_response IS BOUND.
      response = go_response->get_string_data( ).
      go_rest_api1->close( ).
    ELSE.
      go_rest_api1->close( ).
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>restapi_response_not_found
          interface_id = gv_interface_id.
    ENDIF.

  ENDMETHOD.


METHOD get_aad_token.
  DATA: lo_ref           TYPE REF TO zcl_adf_service,
        lv_xstring       TYPE xstring,
        lv_response      TYPE string,
        lv_http_status   TYPE i,
        it_headers       TYPE tihttpnvp,
        lv_msg           TYPE string,
        wa_headers       TYPE LINE OF tihttpnvp,
        lcx_interface    TYPE REF TO zcx_interace_config_missing,
        lcx_http         TYPE REF TO zcx_http_client_failed,
        lcx_o365_service TYPE REF TO zcx_o365_service,
        lcx_adf_service  TYPE REF TO zcx_adf_service,
        lo_ref_aad       TYPE REF TO zcl_adf_service_aad.
  TRY.
      lo_ref = zcl_adf_service_factory=>create( iv_interface_id = gv_interface_id_aad
                                                 iv_business_identifier = ' ' ).
      lo_ref_aad ?= lo_ref.
* gv_client_id ----> ClientID@TenantID
* gv_resource  ----> resource/SiteDomain@TenantID
      CALL METHOD lo_ref_aad->get_aad_token
        EXPORTING
          iv_client_id = gv_client_id
          iv_resource  = gv_resource
        IMPORTING
          ev_aad_token = ev_aad_token
          ev_response  = ev_response.
    CATCH zcx_adf_service INTO lcx_adf_service.
      lv_msg =  lcx_adf_service->get_text( ).
      MESSAGE lv_msg TYPE gc_i.
    CATCH zcx_interace_config_missing INTO lcx_interface.
      lv_msg =  lcx_interface->get_text( ).
      MESSAGE lv_msg TYPE gc_i.
    CATCH zcx_http_client_failed INTO lcx_http .
      lv_msg =  lcx_http->get_text( ).
      MESSAGE lv_msg TYPE gc_i.
  ENDTRY.
  IF ev_aad_token IS INITIAL.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>error_aad_token
        interface_id = gv_interface_id.
  ELSE.
    gv_token = ev_aad_token.
  ENDIF.
  CLEAR : lo_ref_aad , lo_ref.
ENDMETHOD.


  METHOD read_all_file_from_folder.
    DATA: response_string  TYPE string,
          response         TYPE xstring,
          go_response      TYPE REF TO if_rest_entity,
          go_request       TYPE REF TO if_rest_entity,
          ev_http_status   TYPE i,
          lv_request       TYPE xstring,
          lt_data          TYPE tihttpnvp,
          lcx_o365_service TYPE REF TO zcx_o365_service,
          lv_response      TYPE string,
          lv_msg           TYPE string,
          lv_requestid     TYPE string,
          lt_headers       TYPE tihttpnvp,
          ls_header        TYPE ihttpnvp.

    TRY.
        CALL METHOD get_aad_token
          IMPORTING
            ev_aad_token = gv_token
            ev_response  = lv_response.
      CATCH zcx_o365_service INTO lcx_o365_service.
        lv_msg =  lcx_o365_service->get_text( ).
        MESSAGE lv_msg TYPE gc_i.
    ENDTRY.

    me->get_rest_api_ref( iv_business_identifier = gv_businessid ).

    ls_header-name = 'Accept'.
    ls_header-value = 'application/json;odata=verbose'.
    APPEND ls_header TO lt_headers.

    CALL METHOD set_header_attributes(
      EXPORTING
        request    = lv_request
        it_headers = lt_headers ).

    go_rest_api1->set_callingmethod('READ_ALL_FILE_FROM_FOLDER').

    CONCATENATE gc_uri_sp gc_sep1
                iv_folder_name gc_sep1  gc_sep2
                '/Files' INTO lv_requestid.


    go_rest_api1->set_uri( lv_requestid ).


    go_response = go_rest_api1->zif_rest_framework~execute( io_entity = go_request
                                                           async     = abap_false
                                                           is_retry  = abap_false ).

    ev_http_status = go_rest_api1->get_status( ).

    IF go_response IS BOUND.
      response = go_response->get_binary_data( ).

      response_string = go_response->get_string_data( ).

      CALL METHOD me->json_to_http_fields
        EXPORTING
          iv_response_data = response_string
        RECEIVING
          et_fields        = lt_data.

      IF lt_data IS NOT INITIAL.
        DELETE lt_data WHERE name <> 'Name'.
        IF sy-subrc EQ 0.
          gt_data = lt_data.
        ENDIF.
      ENDIF.
    ENDIF.
    go_rest_api1->close( ).
  ENDMETHOD.


  METHOD read_file_data.

    DATA: go_response      TYPE REF TO if_rest_entity,
          go_request       TYPE REF TO if_rest_entity,
          lo_http_client   TYPE REF TO if_http_client,
          lv_response      TYPE string,
          lv_msg           TYPE string,
          lcx_o365_service TYPE REF TO zcx_o365_service,
          lv_request       TYPE xstring.

**** Get Authorization
    me->validate_parameters( ).
    TRY.
        CALL METHOD get_aad_token
          IMPORTING
            ev_aad_token = gv_token
            ev_response  = lv_response.
      CATCH zcx_o365_service INTO lcx_o365_service.
        lv_msg =  lcx_o365_service->get_text( ).
        MESSAGE lv_msg TYPE gc_i.
    ENDTRY.

    me->get_rest_api_ref( iv_business_identifier = ' ' ).
    CALL METHOD set_header_attributes(
      EXPORTING
        request    = lv_request
        it_headers = it_headers ).

    go_rest_api1->set_callingmethod('READ_FILE_DATA').
    me->set_uri_path_read( ).
    go_response = go_rest_api1->zif_rest_framework~execute( io_entity = go_request
                                                            async     = abap_false
                                                            is_retry  = abap_false ).
    ev_http_status = go_rest_api1->get_status( ).
    IF go_response IS BOUND.
      response = go_response->get_binary_data( ).
      go_rest_api1->close( ).
    ELSE.
      go_rest_api1->close( ).
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>restapi_response_not_found
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


METHOD send.
  DATA: go_response      TYPE REF TO if_rest_entity,
        go_request       TYPE REF TO if_rest_entity,
        lv_response      TYPE string,
        lv_msg           TYPE string,
        lcx_o365_service TYPE REF TO zcx_o365_service.

**** Get Authorization
  me->validate_parameters( ).
  TRY.
      CALL METHOD get_aad_token
        IMPORTING
          ev_aad_token = gv_token
          ev_response  = lv_response.
    CATCH zcx_o365_service INTO lcx_o365_service.
      lv_msg =  lcx_o365_service->get_text( ).
      MESSAGE lv_msg TYPE gc_i.
  ENDTRY.

  me->get_rest_api_ref( EXPORTING iv_business_identifier = gv_businessid ).

  CALL METHOD set_header_parameters(
    EXPORTING
      request    = request
      it_headers = it_headers ).
  go_rest_api1->set_callingmethod('SEND').
  me->set_uri_path( ).
  go_rest_api1->zif_rest_framework~set_binary_body( request ).
  go_response = go_rest_api1->zif_rest_framework~execute( io_entity = go_request
                                                         async     = abap_false
                                                         is_retry  = abap_false ).
  ev_http_status = go_rest_api1->get_status( ).
  IF go_response IS BOUND.
    response = go_response->get_string_data( ).
    go_rest_api1->close( ).
  ELSE.
    go_rest_api1->close( ).
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>restapi_response_not_found
        interface_id = gv_interface_id.
  ENDIF.
ENDMETHOD.


METHOD SEND_HEADER_CSV.
  DATA: lw_headers   TYPE ihttpnvp.

  CONCATENATE gc_bearer gv_token INTO gv_token SEPARATED BY space.
  go_rest_api1->set_request_header( iv_name = gc_auth iv_value = gv_token ).
  go_rest_api1->set_request_header( iv_name = gc_content
                                    iv_value = gc_csv ).
  IF it_headers[] IS NOT INITIAL.
    go_rest_api1->set_request_headers( it_header_fields = it_headers[] ).
  ENDIF.
ENDMETHOD.


METHOD SEND_HEADER_EXCEL.
  DATA: lw_headers   TYPE ihttpnvp.
  CONCATENATE gc_bearer gv_token INTO gv_token SEPARATED BY space.
  go_rest_api1->set_request_header( iv_name  = gc_auth iv_value = gv_token ).
  go_rest_api1->set_request_header( iv_name  = gc_content
                                    iv_value = gc_xls ).
  "'application/x-www-form-urlencoded' ).

  IF it_headers[] IS NOT INITIAL.
    go_rest_api1->set_request_headers( it_header_fields = it_headers[] ).
  ENDIF.

ENDMETHOD.


  METHOD send_header_folder.

    DATA: lw_headers   TYPE ihttpnvp.
    CONCATENATE gc_bearer gv_token INTO gv_token SEPARATED BY space.
    go_rest_api1->set_request_header( iv_name  = gc_auth iv_value = gv_token ).
    go_rest_api1->set_request_header( iv_name  = gc_content
                                      iv_value = gc_fol ).
    IF it_headers[] IS NOT INITIAL.
      go_rest_api1->set_request_headers( it_header_fields = it_headers[] ).
    ENDIF.

  ENDMETHOD.


  method SEND_HEADER_PDF.
 DATA: lw_headers   TYPE ihttpnvp.

  CONCATENATE gc_bearer gv_token INTO gv_token SEPARATED BY space.
  go_rest_api1->set_request_header( iv_name = gc_auth iv_value = gv_token ).
  go_rest_api1->set_request_header( iv_name = gc_content
                                    iv_value = gc_pdf ). " multipart/form-data

  IF it_headers[] IS NOT INITIAL.
    go_rest_api1->set_request_headers( it_header_fields = it_headers[] ).
  ENDIF.
  endmethod.


METHOD SEND_HEADER_TXT.
  DATA: lw_headers   TYPE ihttpnvp.

  CONCATENATE gc_bearer gv_token INTO gv_token SEPARATED BY space.
  go_rest_api1->set_request_header( iv_name = gc_auth iv_value = gv_token ).
  go_rest_api1->set_request_header( iv_name = gc_content
                                    iv_value = gc_text ).

  IF it_headers[] IS NOT INITIAL.
    go_rest_api1->set_request_headers( it_header_fields = it_headers[] ).
  ENDIF.
ENDMETHOD.


  method SET_BUSINESS_IDENTIFIER.
gv_businessid = iv_business_identifier.

  endmethod.


METHOD set_file_parameters.
  DATA:ls_sharepoint_info TYPE zo365_config.
  SELECT SINGLE *
    FROM zo365_config
    INTO ls_sharepoint_info
   WHERE interface_id = gv_interface_id.
  IF sy-subrc IS INITIAL.
* Set Global Parameters
    gv_client_id         = ls_sharepoint_info-client_id.
    gv_resource          = ls_sharepoint_info-resource_id.
**********************************************************************
* AAD Interface ID
**********************************************************************
    gv_interface_id_aad  = ls_sharepoint_info-aad_interface_id.
**********************************************************************
  ELSE.
* Error handling
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>sp_config_details_not_found
        interface_id = gv_interface_id.
  ENDIF.
  gv_file_name         = iv_filename.    " File Name
  gv_file_type         = iv_filetype.    " File Type
  gv_folder_name       = iv_folder_name. " Folder Name in the SharePoint
  TRANSLATE gv_file_type TO UPPER CASE.
ENDMETHOD.


  method SET_HEADER_ATTRIBUTES.
    DATA: lw_headers   TYPE ihttpnvp.


    CONCATENATE gc_bearer gv_token INTO gv_token SEPARATED BY space.
    go_rest_api1->set_request_header( iv_name  = gc_auth iv_value = gv_token ).

    IF it_headers[] IS NOT INITIAL.
      go_rest_api1->set_request_headers( it_header_fields = it_headers[] ).
    ENDIF.
  endmethod.


METHOD set_header_parameters.

  CASE gv_file_type.
    WHEN 'XLS' OR 'XLSX'.
      CALL METHOD send_header_excel(
        EXPORTING
          request    = request
          it_headers = it_headers ).
    WHEN 'TXT'.
      CALL METHOD send_header_txt(
        EXPORTING
          request    = request
          it_headers = it_headers ).
    WHEN 'CSV'.
      CALL METHOD send_header_csv(
        EXPORTING
          request    = request
          it_headers = it_headers ).
    WHEN 'PDF'.
      CALL METHOD send_header_pdf(
        EXPORTING
          request    = request
          it_headers = it_headers ).
    WHEN 'FOL'.
      CALL METHOD send_header_folder(
        EXPORTING
          request    = request
          it_headers = it_headers ).
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>file_type_not_handled
          interface_id = gv_interface_id.
  ENDCASE.


ENDMETHOD.


METHOD set_uri_path.
  DATA  : lv_requestid TYPE string.

  CONCATENATE gc_uri_sp gc_sep1
              gv_folder_name gc_sep1  gc_sep2 gc_files_uri_name gc_sep1 gv_file_name gc_dot gv_file_type
              gc_sep1 gc_sep gv_overwrite lv_requestid
              INTO lv_requestid.

  go_rest_api1->set_uri( lv_requestid ).

ENDMETHOD.


METHOD set_uri_path_folder.

  data : lv_requestid TYPE string.
  lv_requestid = gc_folder.
  go_rest_api1->set_uri( lv_requestid ).


ENDMETHOD.


  METHOD SET_URI_PATH_FOR_DELETE.
    DATA  : lv_requestid TYPE string.
    CONCATENATE gc_uri_sp gc_sep1
*                '/Shared%20Documents/'"Comment when moving to MSS
                gv_folder_name gc_sep1  gc_sep2
                gc_files gc_sep1 gv_file_name gc_dot gv_file_type
                gc_sep1 gc_sep2 INTO lv_requestid.

    go_rest_api1->set_uri( lv_requestid ).

  ENDMETHOD.


  method SET_URI_PATH_READ.
 DATA  : lv_requestid TYPE string.
    CONCATENATE gc_uri_sp gc_sep1
                gv_folder_name gc_sep1  gc_sep2
                gc_files gc_sep1 gv_file_name gc_dot gv_file_type
                gc_sep1 gc_sep2 gc_value INTO lv_requestid.

    go_rest_api1->set_uri( lv_requestid ).
  endmethod.


METHOD validate_parameters.

  IF gv_file_name IS INITIAL.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>file_name_not_found
        interface_id = gv_interface_id.
  ENDIF.
  IF gv_file_type IS INITIAL.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>file_type_not_found
        interface_id = gv_interface_id.
  ENDIF.
  IF gv_folder_name IS INITIAL.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>folder_name_not_found
        interface_id = gv_interface_id.
  ENDIF.
ENDMETHOD.
ENDCLASS.
