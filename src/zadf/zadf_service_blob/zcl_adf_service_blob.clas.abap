class ZCL_ADF_SERVICE_BLOB definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public

  global friends ZCL_ADF_SERVICE_REPROCESS .

public section.

  methods CREATE_APPEND_BLOB
    exporting
      value(EV_RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE .
  methods CONV_PDF_TO_XSTRING
    importing
      value(IV_FILENAME) type STRING default SPACE
    exporting
      value(ES_XSTRING) type XSTRING .
  methods SET_BLOB_ADDITIONAL_ATTRIBUTES
    importing
      !IV_PERMISSION type STRING default 'racwdl'
      !IV_IDENTIFIER type STRING optional
      !IV_IP type STRING optional
      !IV_PROTOCOL type STRING optional
      !IV_VERSION type STRING default '2016-05-31'
      !IV_RSCC type STRING optional
      !IV_RSCD type STRING optional
      !IV_RSCE type STRING optional
      !IV_RSCT type STRING optional
      !IV_EXPIRY_TIME type STRING optional .
  methods SET_BLOB_NAME_TYPE
    importing
      value(IV_BLOB_NAME) type STRING
      value(IV_BLOB_TYPE) type STRING default 'A' .
  methods SET_STORAGE_ACCOUNT_CONTAINER
    importing
      value(IV_STORAGE_ACCOUNT) type STRING
      value(IV_CONTAINER_NAME) type STRING
      value(IV_FOLDER) type STRING optional .
  methods GET_BLOB
    importing
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      value(RESPONSE) type XSTRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE
      ZCX_HTTP_CLIENT_FAILED
      ZCX_INTERACE_CONFIG_MISSING .
  methods DELETE_BLOB
    importing
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      value(RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_ADF_SERVICE
      ZCX_HTTP_CLIENT_FAILED
      ZCX_INTERACE_CONFIG_MISSING .
  methods SET_URI_PARAMETERS_BLOB
    importing
      !IV_TIMEOUT type STRING optional .
  methods SET_FILE_DETAILS
    importing
      value(IV_FILENAME) type SDBAH-ACTID
      value(IV_FILETYPE) type CHAR200 .
  methods DECODE_ADF_SAS_KEY
    returning
      value(RV_KEY) type STRING
    raising
      ZCX_ADF_SERVICE .

  methods SEND
    redefinition .
protected section.

  methods GET_SAS_TOKEN
    redefinition .
private section.

  constants GC_UTC_ZONE type TZNZONE value 'UTC' ##NO_TEXT.
  constants GC_SEP_HYPHEN type CHAR1 value '-' ##NO_TEXT.
  constants GC_SEP_COLON type CHAR1 value ':' ##NO_TEXT.
  data GV_START_UTC_TIME type STRING .
  data GV_EXPIRY_UTC_TIME type STRING .
  data GV_PERMISSON type STRING value 'racwdl' ##NO_TEXT.
  data GV_BLOB_NAME type STRING .
  data GV_CONTAINER_NAME type STRING .
  data GV_BLOB_TYPE type STRING .
  data GC_BLOCK_BLOB type STRING value 'BlockBlob' ##NO_TEXT.
  data GC_APPEND_BLOB type STRING value 'AppendBlob' ##NO_TEXT.
  data GC_A type CHAR1 value 'A' ##NO_TEXT.
  data GC_B type CHAR1 value 'B' ##NO_TEXT.
  data GV_SAS_TOKEN type STRING .
  constants GC_COMP_APPENDBLOCK type STRING value '&comp=appendblock' ##NO_TEXT.
  constants GC_SEP_SLASH type CHAR1 value '/' ##NO_TEXT.
  constants GC_BLOB type STRING value 'blob' ##NO_TEXT.
  data GV_STORAGE_ACCOUNT type STRING .
  data GV_FILE_TYPE type CHAR200 .
  data GV_FILENAME type SDBAH-ACTID .
  data GV_IDENTIFIER type STRING .
  data GV_IP type STRING .
  data GV_PROTOCOL type STRING .
  data GV_VERSION type STRING .
  data GV_RSCC type STRING .
  data GV_RSCD type STRING .
  data GV_RSCE type STRING .
  data GV_RSCT type STRING .
  data GV_PERMISSION type STRING value 'racwdl' ##NO_TEXT.
  data GV_FOLDER type STRING .
  constants GC_TIMEOUT type STRING value '&timeout=' ##NO_TEXT.
  data GV_TIMEOUT type STRING .
  constants GV_CONTAINER type STRING value '&restype=container' ##NO_TEXT.
  constants GV_LIST type STRING value '&comp=list' ##NO_TEXT.
  data GV_URI_STRING type STRING .

  methods STRING_TO_SIGN
    returning
      value(RV_STRING_TO_SIGN) type STRING
    raising
      ZCX_ADF_SERVICE .
  methods SET_EXPIRY_UTC_TIME
    raising
      ZCX_ADF_SERVICE .
  methods SEND_PDF
    importing
      value(REQUEST) type XSTRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EO_REQUEST type ref to IF_REST_ENTITY
    raising
      ZCX_ADF_SERVICE .
  methods SET_SAS_TOKEN
    raising
      ZCX_ADF_SERVICE
      ZCX_HTTP_CLIENT_FAILED .
  methods SEND_CSV
    importing
      value(REQUEST) type XSTRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EO_REQUEST type ref to IF_REST_ENTITY
    raising
      ZCX_ADF_SERVICE .
  methods SEND_TEXT
    importing
      !REQUEST type XSTRING
      !IT_HEADERS type TIHTTPNVP
    raising
      ZCX_ADF_SERVICE .
  methods VALIDATE_ATTRIBUTES
    raising
      ZCX_ADF_SERVICE .
  methods GENERATE_SAS_TOKEN
    raising
      ZCX_ADF_SERVICE .
  methods SEND_XML
    importing
      value(REQUEST) type XSTRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EO_REQUEST type ref to IF_REST_ENTITY
    raising
      ZCX_ADF_SERVICE .
  methods SET_AAD_TOKEN .
  methods SET_SERVICE_SAS_TOKEN
    importing
      value(IV_SERVICE_SAS_TOKEN) type STRING .
  methods SEND_JSON
    importing
      value(REQUEST) type XSTRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      !EO_REQUEST type ref to IF_REST_ENTITY .
  methods SET_SAS_TOKEN_RD
    importing
      value(IV_SAS_TOKEN) type STRING .
  methods SET_MI_TOKEN_RD .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_BLOB IMPLEMENTATION.


  METHOD conv_pdf_to_xstring.
    DATA:
      lt_tab_file  TYPE TABLE OF char255,
      lv_length    TYPE i,
      ls_xstring   TYPE xstring,
      ls_xstring1  TYPE xstring,
      lv_base64    TYPE string,
      lv_filepath  TYPE dbmsgora-filename,
      lv_filename  TYPE sdbah-actid,
      lv_file_type TYPE sdbad-funct.

**** Converting PDF to BIN format
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = iv_filename
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      CHANGING
        data_tab                = lt_tab_file
      EXCEPTIONS
        file_open_error         = 01
        file_read_error         = 02
        no_batch                = 03
        gui_refuse_filetransfer = 04
        invalid_type            = 05
        no_authority            = 06
        unknown_error           = 07
        bad_data_format         = 08
        header_not_allowed      = 09
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18.
    IF sy-subrc = 0.                         "(+)VSO#5995767/TR#MS2K9A03
**** PDF conversion to XSTRING
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = lv_length
        IMPORTING
          buffer       = ls_xstring
        TABLES
          binary_tab   = lt_tab_file
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc = 0.
        es_xstring = ls_xstring.
      ENDIF.
    ENDIF.
**** Get File type
    lv_filepath = iv_filename.

    CALL FUNCTION 'SPLIT_FILENAME'
      EXPORTING
        long_filename  = lv_filepath
      IMPORTING
        pure_filename  = lv_filename
        pure_extension = lv_file_type.

    TRANSLATE lv_file_type TO UPPER CASE.

    gv_filename = lv_filename.
    gv_file_type = lv_file_type.

    CLEAR: lv_length,lt_tab_file,ls_xstring,lv_base64,ls_xstring1,lv_filename,lv_file_type.
  ENDMETHOD.


METHOD create_append_blob.
  DATA :   lo_response     TYPE REF TO if_rest_entity,
           lo_request      TYPE REF TO if_rest_entity,
           lv_expiry       TYPE string,
           lv_sas_token    TYPE string,
           lv_msg          TYPE string,
           lcx_adf_service TYPE REF TO zcx_adf_service.
  IF go_rest_api IS BOUND.
    IF gv_sas_token IS INITIAL.
      TRY.
          get_sas_token( EXPORTING iv_baseaddress = gv_uri
                         RECEIVING rv_sas_token  = lv_sas_token ).
          gv_sas_token = lv_sas_token.
        CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.
    ENDIF.
    go_rest_api->zif_rest_framework~set_uri( lv_sas_token ).
    add_request_header( iv_name = 'x-ms-blob-type' iv_value = gc_append_blob ).
    add_request_header( iv_name = 'Content-Length' iv_value = '0' ).
    add_request_header( iv_name = 'Content-Type'   iv_value = 'text/plain' ).
**Rest API call to get response from Azure Destination
    lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
    ev_http_status = go_rest_api->get_status( ).
    IF lo_response IS BOUND.
      ev_response = lo_response->get_string_data( ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>error_in_append_blob_creation
          interface_id = gv_interface_id.
    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD decode_adf_sas_key.

    rv_key = read_key( ).

  ENDMETHOD.


  METHOD delete_blob.
**** Declarations
    DATA: lo_response     TYPE REF TO if_rest_entity,
          lo_request      TYPE REF TO if_rest_entity,
          lv_expiry       TYPE string,
          lv_sas_token    TYPE string,
          lv_msg          TYPE string,
          lcx_adf_service TYPE REF TO zcx_adf_service,
          lv_file_type    TYPE char200,
          lo_get_file     TYPE REF TO cl_rest_multipart_form_data,
          lt_headers      TYPE tihttpnvp.

    IF go_rest_api IS BOUND.
* Check if the Authentication Type is SAS Token based
      IF line_exists( it_headers[ name = gc_sas ] ).
        DATA(lv_blob_sas) = it_headers[ name = gc_sas ]-value.
        DATA(lv_processing_method) = gc_sas.
* Check if the Authentication Type is Managed IDentity based
      ELSEIF line_exists( it_headers[ name = gc_mi_auth ] ).
        lv_processing_method = gc_mi_auth.
      ENDIF.

* Set the token the based on the Authentication type
      CASE lv_processing_method.
        WHEN gc_sas.
* Set the pre-configured SAS Token to request
          set_sas_token_rd(
                          EXPORTING
                            iv_sas_token = lv_blob_sas ).
        WHEN gc_mi_auth.
* MI Token already added to Request header
          set_mi_token_rd( ).
        WHEN OTHERS.
          generate_sas_token( ).
      ENDCASE.

      IF NOT it_headers[] IS INITIAL.
        APPEND LINES OF it_headers TO lt_headers.
* Exclude SAS token header
        IF line_exists( lt_headers[ name = gc_sas ] ).
          DATA(lv_tabix) = sy-tabix.
          DELETE lt_headers INDEX lv_tabix.
        ENDIF.
      ENDIF.

      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = lt_headers[] ).
**** Rest API call to get response from Azure Destination
**** Get reference
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
      ev_http_status = go_rest_api->get_status( ).
      IF lo_response IS BOUND.
        response = lo_response->get_binary_data( ).
        go_rest_api->close( ).
      ELSE.
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service_blob=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD generate_sas_token.
**** Declarations
    DATA : lv_sas_token    TYPE string,
           lcx_adf_service TYPE REF TO zcx_adf_service,
           lv_msg          TYPE string.
**** Generate SAS Token
    IF gv_sas_token IS INITIAL.
      TRY.
          get_sas_token( EXPORTING iv_baseaddress = gv_uri
                         RECEIVING rv_sas_token  = lv_sas_token ).
          gv_sas_token = lv_sas_token.
        CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.
**** Set URI prameter
      IF gv_timeout IS NOT INITIAL.
        CONCATENATE gv_sas_token gc_timeout gv_timeout INTO gv_sas_token.
      ENDIF.
      go_rest_api->zif_rest_framework~set_uri( gv_sas_token ).
    ENDIF.
    CLEAR:lv_sas_token,lv_msg.
  ENDMETHOD.


  METHOD get_blob.
    DATA: lo_response     TYPE REF TO if_rest_entity,
          lo_request      TYPE REF TO if_rest_entity,
          lv_expiry       TYPE string,
          lv_sas_token    TYPE string,
          lv_msg          TYPE string,
          lcx_adf_service TYPE REF TO zcx_adf_service,
          lv_file_type    TYPE char200,
          lo_get_file     TYPE REF TO cl_rest_multipart_form_data,
          lt_headers      TYPE tihttpnvp.

    IF go_rest_api IS BOUND.

* Check if the Authentication Type is SAS Token based
      IF line_exists( it_headers[ name = gc_sas ] ).
        DATA(lv_blob_sas) = it_headers[ name = gc_sas ]-value.
        DATA(lv_processing_method) = gc_sas.
* Check if the Authentication Type is Managed IDentity based
      ELSEIF line_exists( it_headers[ name = gc_mi_auth ] ).
        lv_processing_method = gc_mi_auth.
      ENDIF.

* Set the token the based on the Authentication type
      CASE lv_processing_method.
        WHEN gc_sas.
* Set the pre-configured SAS Token to request
          set_sas_token_rd(
                EXPORTING
                  iv_sas_token = lv_blob_sas ).
        WHEN gc_mi_auth.
* Token already added to Request header
          set_mi_token_rd( ).
        WHEN OTHERS.
          generate_sas_token( ).
      ENDCASE.

      IF NOT it_headers[] IS INITIAL.
        APPEND LINES OF it_headers TO lt_headers.
* Exclude SAS token header
        IF line_exists( lt_headers[ name = gc_sas ] ).
          DATA(lv_tabix) = sy-tabix.
          DELETE lt_headers INDEX lv_tabix.
        ENDIF.
      ENDIF.

      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = lt_headers[] ).
**** Rest API call to get response from Azure Destination
**** Get reference
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request
                                                             async = gv_asynchronous
                                                             is_retry = gv_is_try ).
      ev_http_status = go_rest_api->get_status( ).
      IF lo_response IS BOUND.
        response = lo_response->get_binary_data( ).
        go_rest_api->close( ).
      ELSE.
        go_rest_api->close( ).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service_blob=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD get_sas_token.
  DATA : body_xstring      TYPE xstring,
         sign              TYPE string,
         final_token       TYPE string,
         lv_decoded_xstr   TYPE xstring,
         conv              TYPE REF TO cl_abap_conv_out_ce,
         lv_sas_key        TYPE string,
         lv_format         TYPE i,
         lx_static_check   TYPE REF TO cx_static_check,
         lv_string_to_sign TYPE string.
  conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

*  Calling String to sign
  CALL METHOD me->string_to_sign
    RECEIVING
      rv_string_to_sign = lv_string_to_sign.
  conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = body_xstring ).
  DEFINE encrypt_key.
*    decode_sign( RECEIVING rv_secret = lv_sas_key ).
    IF gv_sas_key IS INITIAL.
      lv_sas_key = read_ssf_key( ).
    ELSE.
      lv_sas_key = read_key( ).
    ENDIF.
    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data                  = lv_sas_key
      IMPORTING
        bindata                  = lv_decoded_xstr
      EXCEPTIONS
        ssf_krn_error            = 1
        ssf_krn_noop             = 2
        ssf_krn_nomemory         = 3
        ssf_krn_opinv            = 4
        ssf_krn_input_data_error = 5
        ssf_krn_invalid_par      = 6
        ssf_krn_invalid_parlen   = 7
        OTHERS                   = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>error_in_sas_key_encryption
          interface_id = gv_interface_id.
    ENDIF.
    CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
      EXPORTING
        if_algorithm     = 'sha-256'
        if_key           = lv_decoded_xstr "decoded
        if_data          = body_xstring
        if_length        = 0
      IMPORTING
        ef_hmacb64string = sign.
    CLEAR : lv_sas_key,lv_decoded_xstr.
  END-OF-DEFINITION.
  encrypt_key.
  IF NOT sign IS INITIAL.
    lv_format = 18.
    sign = escape( val = sign format = lv_format ).
    gv_start_utc_time = escape( val = gv_start_utc_time format = lv_format ).
    gv_expiry_utc_time = escape( val = gv_expiry_utc_time format = lv_format ).
    IF NOT gv_expiry_utc_time IS INITIAL.
      CONCATENATE '?sv=' gv_service_version '&sr=c' '&sig=' sign '&st=' gv_start_utc_time
      '&se=' gv_expiry_utc_time '&sp=' gv_permisson INTO final_token.
      IF gv_folder IS NOT INITIAL.
        CONCATENATE '/' gv_container_name '/' gv_folder '/' gv_blob_name INTO rv_sas_token.
      ELSE.
        CONCATENATE '/' gv_container_name '/' gv_blob_name INTO rv_sas_token.
      ENDIF.
      CONCATENATE rv_sas_token final_token INTO rv_sas_token.
    ENDIF.
  ELSE.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>sas_key_not_generated
        interface_id = gv_interface_id.
  ENDIF.
ENDMETHOD.


METHOD send.
  DATA : lo_response          TYPE REF TO if_rest_entity,
         lo_request           TYPE REF TO if_rest_entity,
         lv_expiry            TYPE string,
         lv_sas_token         TYPE string,
         lv_msg               TYPE string,
         lcx_adf_service      TYPE REF TO zcx_adf_service,
         lv_file_type         TYPE char200,
         ls_headers           TYPE ihttpnvp,
         lv_processing_method TYPE string,
         lv_token             TYPE string,
         lv_tabix             TYPE sy-tabix,
         lt_headers           TYPE tihttpnvp.

  IF go_rest_api IS BOUND.

    lt_headers[] = it_headers[].

    CLEAR ls_headers.
    READ TABLE lt_headers INTO ls_headers WITH KEY name = gc_sas.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix.
      lv_processing_method = gc_sas.
      lv_token = ls_headers-value.
* Deleting from the headers, as this won't be used in the service call for SASTOKEN scenario
      DELETE lt_headers INDEX lv_tabix.
    ELSE.
      CLEAR ls_headers.
      READ TABLE lt_headers INTO ls_headers WITH KEY name = gc_mi_auth.
      IF sy-subrc EQ 0.
        lv_processing_method = gc_mi_auth.
      ENDIF.
    ENDIF.

    CASE lv_processing_method.
* Applicable for SAS Token
      WHEN gc_sas.
        CALL METHOD me->set_service_sas_token
          EXPORTING
            iv_service_sas_token = lv_token.
      WHEN gc_mi_auth.
* Applicable for MI and AAD.
        set_aad_token( ).
      WHEN OTHERS.
*Applicable for Account Key
        set_sas_token( ).
    ENDCASE.

**** Send file to Blob
*Below file wise case will work for Account Key. for MI/AAD the file header needs to be set from Calling program.
    CASE gv_file_type.
      WHEN 'PDF'.
        CALL METHOD send_pdf(
          EXPORTING
            request    = request
            it_headers = lt_headers
          IMPORTING
            eo_request = lo_request ).
      WHEN 'CSV'.
        CALL METHOD send_csv(
          EXPORTING
            request    = request
            it_headers = lt_headers
          IMPORTING
            eo_request = lo_request ).
      WHEN 'TXT'.
        CALL METHOD send_text(
          EXPORTING
            request    = request
            it_headers = lt_headers ).
*BOC-KRANKAM-08/20/2020
      WHEN 'JSON'.
        CALL METHOD send_json(
          EXPORTING
            request    = request
            it_headers = lt_headers
          IMPORTING
            eo_request = lo_request ).
*EOC-KRANKAM-08/20/2020
      WHEN 'XML'.
        CALL METHOD send_xml(
          EXPORTING
            request    = request
            it_headers = lt_headers
          IMPORTING
            eo_request = lo_request ).
    ENDCASE.

    IF NOT lt_headers[] IS INITIAL.
      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = lt_headers[] ).
    ENDIF.
    go_rest_api->zif_rest_framework~set_binary_body( request ).
** Rest API call to get response from Azure Destination
    lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
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


METHOD send_csv.
**** Declarations
  DATA: lo_post_file TYPE REF TO cl_rest_multipart_form_data,
        lw_headers   TYPE ihttpnvp,
        lv_filename  TYPE string,
        lo_http_client  TYPE REF TO if_http_client,
        lo_request      TYPE REF TO if_rest_entity.

  CONSTANTS: lc_value TYPE string VALUE 'attachment; filename = '.
  CASE gv_blob_type.
**** Block Blob
    WHEN gc_block_blob.
      CONCATENATE gv_filename gv_file_type INTO lv_filename SEPARATED BY '.'.
      CONCATENATE lc_value lv_filename INTO lv_filename SEPARATED BY space.
**** Get reference
      go_rest_api->get_http_client( RECEIVING result = lo_http_client ).
**** Set Headers
      lo_http_client->request->set_content_type( content_type = 'text/csv').
      add_request_header( iv_name = 'x-ms-blob-type' iv_value = gc_block_blob ).
      add_request_header( iv_name = 'x-ms-blob-content-disposition'
                          iv_value = lv_filename ).
**** Get reference
      go_rest_api->get_request( RECEIVING result = lo_request ).

      eo_request = lo_request.
**** Append Blob
    WHEN gc_append_blob.
      add_request_header( iv_name = 'x-ms-version' iv_value = '2016-05-31' ).
**Validation logic for content-length header
      CLEAR lw_headers.
      READ TABLE it_headers INTO lw_headers
                            WITH KEY name = 'Content-Length'.
      IF sy-subrc EQ 0.
        IF NOT lw_headers-value IS INITIAL.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid       = zcx_adf_service=>content_length_missing
              interface_id = gv_interface_id.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>content_length_header_missing
            interface_id = gv_interface_id.
      ENDIF.
  ENDCASE.
ENDMETHOD.                    "send_csv


METHOD send_json.
**** Declarations
  DATA: lo_post_file   TYPE REF TO cl_rest_multipart_form_data,
        lw_headers     TYPE ihttpnvp,
        lv_filename    TYPE string,
        lo_http_client TYPE REF TO if_http_client,
        lo_request     TYPE REF TO if_rest_entity.

  CONSTANTS: lc_value TYPE string VALUE 'attachment; filename = '.
  CASE gv_blob_type.
**** Block Blob
    WHEN gc_block_blob.
      CONCATENATE gv_filename gv_file_type INTO lv_filename SEPARATED BY '.'.
      CONCATENATE lc_value lv_filename INTO lv_filename SEPARATED BY space.
**** Get reference
      go_rest_api->get_http_client( RECEIVING result = lo_http_client ).
**** Set Headers
      lo_http_client->request->set_content_type( content_type = 'json').
      add_request_header( iv_name = 'x-ms-blob-type' iv_value = gc_block_blob ).
      add_request_header( iv_name = 'x-ms-blob-content-disposition'
                          iv_value = lv_filename ).
**** Get reference
      go_rest_api->get_request( RECEIVING result = lo_request ).

      eo_request = lo_request.
**** Append Blob
    WHEN gc_append_blob.
      add_request_header( iv_name = 'x-ms-version' iv_value = '2016-05-31' ).
**Validation logic for content-length header
      CLEAR lw_headers.
      READ TABLE it_headers INTO lw_headers
                            WITH KEY name = 'Content-Length'.
      IF sy-subrc EQ 0.
        IF NOT lw_headers-value IS INITIAL.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid       = zcx_adf_service=>content_length_missing
              interface_id = gv_interface_id.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>content_length_header_missing
            interface_id = gv_interface_id.
      ENDIF.
  ENDCASE.
ENDMETHOD.


METHOD send_pdf.
**** Declarations
  DATA: lo_post_file TYPE REF TO cl_rest_multipart_form_data,
        lw_headers   TYPE ihttpnvp,
        lv_filename  TYPE string.

  CONSTANTS: lc_value TYPE string VALUE 'attachment; filename = '.

  CASE gv_blob_type.
**** Block Blob
    WHEN gc_block_blob.

      CONCATENATE gv_filename gv_file_type INTO lv_filename SEPARATED BY '.'.
      CONCATENATE lc_value lv_filename INTO lv_filename SEPARATED BY space.
**** Get reference
      go_rest_api->get_http_client( RECEIVING result = DATA(lo_http_client) ).
**** Set Headers
      lo_http_client->request->set_content_type( content_type = 'multipart/form-data').
      add_request_header( iv_name = 'x-ms-blob-type' iv_value = gc_block_blob ).
      add_request_header( iv_name = 'x-ms-version' iv_value = '2017-11-09' ).
      add_request_header( iv_name = 'x-ms-blob-content-disposition' iv_value = lv_filename ).
**** Get reference
      go_rest_api->get_request( RECEIVING result = DATA(lo_request) ).

      CREATE OBJECT lo_post_file
        EXPORTING
          io_entity = lo_request.

      CALL METHOD lo_post_file->set_file
        EXPORTING
          iv_name     = 'PDF_File'
          iv_filename = 'file.pdf'
          iv_type     = 'PDF'
          iv_data     = request.

      CALL METHOD lo_post_file->if_rest_entity_provider~write_to
        EXPORTING
          io_entity = lo_request.

      eo_request = lo_request.
**** Append Blob
    WHEN gc_append_blob.
      add_request_header( iv_name = 'x-ms-version' iv_value = '2016-05-31' ).
**Validation logic for content-length header
      CLEAR lw_headers.
      READ TABLE it_headers INTO lw_headers
      WITH KEY name = 'Content-Length'.
      IF sy-subrc EQ 0.
        IF NOT lw_headers-value IS INITIAL.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid       = zcx_adf_service=>content_length_missing
              interface_id = gv_interface_id.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>content_length_header_missing
            interface_id = gv_interface_id.
      ENDIF.
  ENDCASE.
ENDMETHOD.


  METHOD send_text.
**** declarations
    DATA: lo_post_file TYPE REF TO cl_rest_multipart_form_data,
          lw_headers   TYPE ihttpnvp,
          lv_filename  TYPE string.

    CONSTANTS: lc_value TYPE string VALUE 'attachment; filename = '.

    CASE gv_blob_type.
**** Block Blob
      WHEN gc_block_blob.

        CONCATENATE gv_filename gv_file_type INTO lv_filename SEPARATED BY '.'.
        CONCATENATE lc_value lv_filename INTO lv_filename SEPARATED BY space.
**** Set Headers
        add_request_header( iv_name = 'x-ms-blob-type' iv_value = gc_block_blob ).
        add_request_header( iv_name = 'Content-Type' iv_value = 'text/plain' ).
        add_request_header( iv_name = 'x-ms-blob-content-disposition' iv_value = lv_filename ).
**** Append Blob
      WHEN gc_append_blob.
        add_request_header( iv_name = 'x-ms-version' iv_value = '2016-05-31' ).
**Validation logic for content-length header
        CLEAR lw_headers.
        READ TABLE it_headers INTO lw_headers
        WITH KEY name = 'Content-Length'.
        IF sy-subrc EQ 0.
          IF lw_headers-value IS INITIAL.
            RAISE EXCEPTION TYPE zcx_adf_service
              EXPORTING
                textid       = zcx_adf_service=>content_length_missing
                interface_id = gv_interface_id.
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid       = zcx_adf_service=>content_length_header_missing
              interface_id = gv_interface_id.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD send_xml.
**** Declarations
  DATA: lo_post_file   TYPE REF TO cl_rest_multipart_form_data,
        lw_headers     TYPE ihttpnvp,
        lv_filename    TYPE string,
        lo_http_client TYPE REF TO if_http_client,
        lo_request     TYPE REF TO if_rest_entity.

  CONSTANTS: lc_value TYPE string VALUE 'attachment; filename = '.
  CASE gv_blob_type.
**** Block Blob
    WHEN gc_block_blob.
      CONCATENATE gv_filename gv_file_type INTO lv_filename SEPARATED BY '.'.
      CONCATENATE lc_value lv_filename INTO lv_filename SEPARATED BY space.
**** Get reference
      go_rest_api->get_http_client( RECEIVING result = lo_http_client ).
**** Set Headers
      lo_http_client->request->set_content_type( content_type = 'application/xml; charset=utf-8').
      add_request_header( iv_name = 'x-ms-blob-type' iv_value = gc_block_blob ).
      add_request_header( iv_name = 'x-ms-blob-content-disposition'
                          iv_value = lv_filename ).
**** Get reference
      go_rest_api->get_request( RECEIVING result = lo_request ).

      eo_request = lo_request.
**** Append Blob
    WHEN gc_append_blob.
      add_request_header( iv_name = 'x-ms-version' iv_value = '2016-05-31' ).
**Validation logic for content-length header
      CLEAR lw_headers.
      READ TABLE it_headers INTO lw_headers
                            WITH KEY name = 'Content-Length'.
      IF sy-subrc EQ 0.
        IF NOT lw_headers-value IS INITIAL.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid       = zcx_adf_service=>content_length_missing
              interface_id = gv_interface_id.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>content_length_header_missing
            interface_id = gv_interface_id.
      ENDIF.
  ENDCASE.
ENDMETHOD.


  METHOD set_aad_token.
    IF gv_uri_string IS INITIAL.
      add_request_header( iv_name = 'x-ms-version' iv_value = '2017-11-09' ).

      IF gv_folder IS NOT INITIAL.
        CONCATENATE '/' gv_container_name '/' gv_folder '/' gv_blob_name INTO gv_uri_string.
      ELSE.
        CONCATENATE '/' gv_container_name '/' gv_blob_name INTO gv_uri_string.
      ENDIF.

      CASE gv_blob_type.
        WHEN gc_append_blob.
          CONCATENATE gv_uri_string gc_comp_appendblock INTO gv_uri_string.
          go_rest_api->zif_rest_framework~set_uri( gv_uri_string ).
        WHEN gc_b OR gc_block_blob.
          go_rest_api->zif_rest_framework~set_uri( gv_uri_string ).
          add_request_header( iv_name = 'x-ms-blob-type' iv_value = 'BlockBlob' ).
      ENDCASE.

    ELSE.
      IF NOT gv_uri_string CS gc_comp_appendblock AND
         gv_blob_type EQ gc_append_blob.
        CONCATENATE gv_uri_string gc_comp_appendblock INTO gv_uri_string.
      ENDIF.
      go_rest_api->zif_rest_framework~set_uri( gv_uri_string ).
    ENDIF.

  ENDMETHOD.


METHOD set_blob_additional_attributes.

* Setting the Global Attributes for Blob Details.
  IF iv_permission IS NOT INITIAL.
    gv_permission = iv_permission.
  ENDIF.
  IF iv_version IS NOT INITIAL.
    gv_service_version = iv_version.
  ENDIF.
  IF iv_expiry_time IS NOT INITIAL.
    gv_expiry_utc_time = iv_expiry_time.
  ENDIF.
  IF iv_identifier IS NOT INITIAL.
    gv_identifier = iv_identifier.
  ENDIF.
  IF iv_ip IS NOT INITIAL.
    gv_ip = iv_ip.
  ENDIF.
  IF iv_protocol IS NOT INITIAL.
    gv_protocol = iv_protocol.
  ENDIF.
  IF iv_version IS NOT INITIAL.
    gv_version = iv_version.
  ENDIF.
  IF iv_rscc IS NOT INITIAL.
    gv_rscc = iv_rscc.
  ENDIF.
  IF iv_rscd IS NOT INITIAL.
    gv_rscd = iv_rscd.
  ENDIF.
  IF iv_rsce IS NOT INITIAL.
    gv_rsce = iv_rsce.
  ENDIF.
  IF iv_rsct IS NOT INITIAL.
    gv_rsct = iv_rsct.
  ENDIF.
ENDMETHOD.


METHOD SET_BLOB_NAME_TYPE.
  IF iv_blob_name IS NOT INITIAL.
    gv_blob_name = iv_blob_name.
  ENDIF.
  IF iv_blob_type IS NOT INITIAL.
    gv_blob_type = iv_blob_type.
  ENDIF.
ENDMETHOD.


METHOD set_expiry_utc_time.
  DATA : lv_current_timestamp TYPE timestamp,
         lv_date              TYPE sy-datum,
         lv_time_out          TYPE timestamp,
         lv_total_sec(16)     TYPE p,
         lv_time              TYPE sy-uzeit.
*Get the current timestamp in UTC
  GET TIME STAMP FIELD  lv_current_timestamp .
  CONVERT TIME STAMP lv_current_timestamp TIME ZONE gc_utc_zone INTO DATE lv_date TIME lv_time.
  CONCATENATE lv_date+0(4) gc_sep_hyphen lv_date+4(2) gc_sep_hyphen lv_date+6(2)
              'T' lv_time+0(2) gc_sep_colon lv_time+2(2) gc_sep_colon lv_time+4(2) 'Z'
              INTO gv_start_utc_time.
  lv_total_sec = ( ( gv_expiry_hour * 60 ) * 60 ) + ( gv_expiry_min * 60 ) + ( gv_expiry_sec ).

  IF NOT lv_total_sec IS INITIAL.
    CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
      EXPORTING
        timestamp_in    = lv_current_timestamp
        timezone        = 'UTC'
        duration        = lv_total_sec
        unit            = 'S'
      IMPORTING
        timestamp_out   = lv_time_out
      EXCEPTIONS
        timestamp_error = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>expiry_utc_time_not_set
          interface_id = gv_interface_id.
    ENDIF.
    CLEAR: lv_date,lv_time.
    CONVERT TIME STAMP lv_time_out TIME ZONE gc_utc_zone INTO DATE lv_date TIME lv_time.
    CONCATENATE lv_date+0(4) gc_sep_hyphen lv_date+4(2) gc_sep_hyphen lv_date+6(2)
                'T' lv_time+0(2) gc_sep_colon lv_time+2(2) gc_sep_colon lv_time+4(2) 'Z'
                INTO gv_expiry_utc_time.
    IF gv_expiry_utc_time IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>expiry_utc_time_not_set
          interface_id = gv_interface_id.
    ENDIF.
  ENDIF.
ENDMETHOD.


  METHOD set_file_details.
    gv_filename = iv_filename.
    gv_file_type = iv_filetype.

    TRANSLATE gv_file_type TO UPPER CASE.

  ENDMETHOD.


  METHOD SET_MI_TOKEN_RD.
* Build the Path for the Azure blob service
    IF gv_folder IS NOT INITIAL.
      gv_uri_string = |/{ gv_container_name }/{ gv_folder }/{ gv_blob_name }|.
    ELSE.
      gv_uri_string = |/{ gv_container_name }/{ gv_blob_name }|.
    ENDIF.
* Assign the required version for Oauth Bearer access token
   add_request_header( iv_name = 'x-ms-version' iv_value = '2017-11-09' ).

* Add the SAS token to the Request
    go_rest_api->zif_rest_framework~set_uri( gv_uri_string ).
  ENDMETHOD.


METHOD set_sas_token.
  DATA : lv_sas_token    TYPE string,
         lcx_adf_service TYPE REF TO zcx_adf_service,
         lv_msg          TYPE string.
  IF go_rest_api IS BOUND.
    IF gv_sas_token IS INITIAL.
      TRY.
          get_sas_token( EXPORTING iv_baseaddress = gv_uri
                         RECEIVING rv_sas_token  = lv_sas_token ).
          gv_sas_token = lv_sas_token.
        CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.
**** Set URI prameter
      IF gv_timeout IS NOT INITIAL.
        CONCATENATE gv_sas_token gc_timeout gv_timeout INTO gv_sas_token.
      ENDIF.
      CASE gv_blob_type.
        WHEN gc_append_blob.
          CONCATENATE gv_sas_token gc_comp_appendblock INTO gv_sas_token.
          go_rest_api->zif_rest_framework~set_uri( gv_sas_token ).
        WHEN gc_block_blob or gc_b.
          go_rest_api->zif_rest_framework~set_uri( gv_sas_token ).
          add_request_header( iv_name = 'x-ms-blob-type' iv_value = 'BlockBlob' ).
      ENDCASE.
    ELSE.
**validation logic for URI parameter while sending data to existing append blob
      IF NOT gv_sas_token CS gc_comp_appendblock AND
         gv_blob_type EQ gc_append_blob.
        CONCATENATE gv_sas_token gc_comp_appendblock INTO gv_sas_token.
      ENDIF.
      go_rest_api->zif_rest_framework~set_uri( gv_sas_token ).
    ENDIF.
  ENDIF.
  CLEAR:lv_sas_token,lv_msg.
ENDMETHOD.


  METHOD SET_SAS_TOKEN_RD.
* Build the Path for the Azure blob service
    IF gv_folder IS NOT INITIAL.
      gv_uri_string = |/{ gv_container_name }/{ gv_folder }/{ gv_blob_name }|.
    ELSE.
      gv_uri_string = |/{ gv_container_name }/{ gv_blob_name }|.
    ENDIF.
* Generate the URI containing SAS Token
    IF iv_sas_token IS NOT INITIAL.
      gv_uri_string = |{ gv_uri_string }?{ iv_sas_token }|.
    ENDIF.
* Add the SAS token to the Request
    go_rest_api->zif_rest_framework~set_uri( gv_uri_string ).
  ENDMETHOD.


  METHOD set_service_sas_token.

    DATA: lv_service_sas_token TYPE string.

    lv_service_sas_token = iv_service_sas_token.

    IF gv_uri_string IS INITIAL.
      add_request_header( iv_name = 'x-ms-version' iv_value = '2017-11-09' ).

      IF gv_folder IS NOT INITIAL.
        CONCATENATE '/' gv_container_name '/' gv_folder '/' gv_blob_name INTO gv_uri_string.
      ELSE.
        CONCATENATE '/' gv_container_name '/' gv_blob_name INTO gv_uri_string.
      ENDIF.

      IF lv_service_sas_token IS NOT INITIAL.
        CONCATENATE gv_uri_string '?' lv_service_sas_token  INTO gv_uri_string.
      ENDIF.

      CASE gv_blob_type.
        WHEN gc_append_blob.
          CONCATENATE gv_uri_string gc_comp_appendblock INTO gv_uri_string.
          go_rest_api->zif_rest_framework~set_uri( gv_uri_string ).
        WHEN gc_b OR gc_block_blob.
          go_rest_api->zif_rest_framework~set_uri( gv_uri_string ).
          add_request_header( iv_name = 'x-ms-blob-type' iv_value = 'BlockBlob' ).
      ENDCASE.

    ELSE.
      IF NOT gv_uri_string CS gc_comp_appendblock AND
         gv_blob_type EQ gc_append_blob.
        CONCATENATE gv_uri_string gc_comp_appendblock INTO gv_uri_string.
      ENDIF.
      go_rest_api->zif_rest_framework~set_uri( gv_uri_string ).
    ENDIF.
  ENDMETHOD.


METHOD set_storage_account_container.
  IF iv_storage_account IS NOT INITIAL.
    gv_storage_account  = iv_storage_account.
  ENDIF.
  IF iv_container_name IS NOT INITIAL.
    gv_container_name   = iv_container_name.
  ENDIF.
  IF iv_folder IS NOT INITIAL.
    gv_folder   = iv_folder.
  ENDIF.
ENDMETHOD.


  METHOD set_uri_parameters_blob.
    IF iv_timeout IS NOT INITIAL.
      gv_timeout = iv_timeout.
    ENDIF.
  ENDMETHOD.


METHOD string_to_sign.
  DATA : lv_canonical_str TYPE string,
         lv_msg           TYPE string,
         lcx_adf_service  TYPE REF TO zcx_adf_service.
  CLEAR: gv_string_to_sign, gv_start_utc_time.

* Validate blob attributes
  CALL METHOD me->validate_attributes.
* if the exception is raised, it is passed on to this method.
  TRY.
      IF gv_expiry_utc_time IS INITIAL.
        set_expiry_utc_time( ).
      ENDIF.
    CATCH zcx_adf_service INTO lcx_adf_service.
      lv_msg =  lcx_adf_service->get_text( ).
      MESSAGE lv_msg TYPE 'I'.
  ENDTRY.

*  Getting the Blob Type from Global Attributes
  IF gv_blob_type EQ gc_a.
    gv_blob_type = gc_append_blob.
  ELSEIF gv_blob_type EQ gc_b.
    gv_blob_type = gc_block_blob.
  ENDIF.

  CONCATENATE gc_sep_slash gc_blob gc_sep_slash gv_storage_account
              gc_sep_slash gv_container_name INTO lv_canonical_str.

* using the global attributes set in set method
  CONCATENATE  gv_permission cl_abap_char_utilities=>newline gv_start_utc_time cl_abap_char_utilities=>newline
  gv_expiry_utc_time cl_abap_char_utilities=>newline lv_canonical_str cl_abap_char_utilities=>newline gv_identifier
  cl_abap_char_utilities=>newline gv_ip cl_abap_char_utilities=>newline gv_protocol
  cl_abap_char_utilities=>newline gv_version cl_abap_char_utilities=>newline gv_rscc
  cl_abap_char_utilities=>newline gv_rscd cl_abap_char_utilities=>newline gv_rsce
  cl_abap_char_utilities=>newline gv_rsct cl_abap_char_utilities=>newline INTO rv_string_to_sign. "gv_string_to_sign.
  IF  rv_string_to_sign IS INITIAL.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>string_to_sign_not_generated
        interface_id = gv_interface_id.
  ENDIF.
ENDMETHOD.


METHOD VALIDATE_ATTRIBUTES.
*  Check if mandatory attributes are present
*  if not display error message/raise exception
  IF gv_container_name IS INITIAL.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>container_name_not_set
        interface_id = gv_interface_id.
  ELSEIF gv_storage_account IS INITIAL.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>storage_account_not_set
        interface_id = gv_interface_id.
  ELSEIF gv_blob_name IS INITIAL.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>blob_details_not_set
        interface_id = gv_interface_id.
  ENDIF.
ENDMETHOD.
ENDCLASS.
