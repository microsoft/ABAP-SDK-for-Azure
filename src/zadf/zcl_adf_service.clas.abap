CLASS zcl_adf_service DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS send
      IMPORTING
        VALUE(request)        TYPE xstring
        !it_headers           TYPE tihttpnvp OPTIONAL
      EXPORTING
        VALUE(response)       TYPE string
        VALUE(ev_http_status) TYPE i
      RAISING
        zcx_adf_service .
    METHODS constructor
      IMPORTING
        VALUE(iv_interface_id)        TYPE zinterface_id
        VALUE(iv_service_id)          TYPE zazure_dest
        VALUE(iv_business_identifier) TYPE zbusinessid OPTIONAL
      RAISING
        zcx_adf_service
        zcx_http_client_failed
        zcx_interace_config_missing .
    METHODS format_data
      IMPORTING
        VALUE(iv_format_type) TYPE zadf_format_type
        VALUE(it_data)        TYPE data
      RETURNING
        VALUE(rv_string)      TYPE string
      RAISING
        zcx_adf_service .
    METHODS format_data_to_xml
      IMPORTING
        VALUE(it_data)   TYPE data
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS format_data_to_json
      IMPORTING
        VALUE(it_data)   TYPE data
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS add_expiry_time
      IMPORTING
        VALUE(iv_expiry_hour) TYPE i
        VALUE(iv_expiry_min)  TYPE i DEFAULT 30
        VALUE(iv_expiry_sec)  TYPE i
      RAISING
        zcx_adf_service .
    METHODS urlencoded_to_http_fields
      IMPORTING
        !iv_response_data TYPE string
      RETURNING
        VALUE(et_fields)  TYPE tihttpnvp
      RAISING
        zcx_adf_service .
    METHODS json_to_http_fields
      IMPORTING
        !iv_response_data TYPE string
      RETURNING
        VALUE(et_fields)  TYPE tihttpnvp
      RAISING
        zcx_adf_service .
    METHODS get_message_id
      RETURNING
        VALUE(rv_message_id) TYPE guid_16 .
  PROTECTED SECTION.

    CONSTANTS gc_error TYPE char1 VALUE 'E' ##NO_TEXT.
    DATA gv_interface_id TYPE zinterface_id .
    DATA gv_sas_key TYPE string .
    DATA gv_uri TYPE string .
    DATA go_rest_api TYPE REF TO zcl_rest_framework .
    TYPE-POOLS abap .
    DATA gv_asynchronous TYPE abap_bool .
    DATA gv_is_try TYPE abap_bool .
    DATA gv_expiry_hour TYPE i .
    DATA gv_expiry_min TYPE i .
    DATA gv_expiry_sec TYPE i .
    DATA gv_string_to_sign TYPE string .
    DATA gv_service_version TYPE string .
    DATA gv_rfc_destination TYPE rfcdest .
    DATA gv_path_prefix TYPE string .
    DATA gv_host TYPE string .
    DATA gt_headers TYPE tihttpnvp .

    METHODS add_request_header
      IMPORTING
        VALUE(iv_name)  TYPE string
        VALUE(iv_value) TYPE string .
    METHODS get_sas_token
      IMPORTING
        VALUE(iv_baseaddress) TYPE string
      RETURNING
        VALUE(rv_sas_token)   TYPE string
      RAISING
        zcx_adf_service .
    METHODS get_epoch_time
      RETURNING
        VALUE(rv_expiry_time) TYPE string
      RAISING
        zcx_adf_service .
    METHODS decode_sign
      RETURNING
        VALUE(rv_secret) TYPE string
      RAISING
        zcx_adf_service .
    METHODS get_target_host
      IMPORTING
        !iv_destination     TYPE rfcdes-rfcdest
        !iv_authority_check TYPE rfcdisplay-rfctrace OPTIONAL
        !iv_bypass_buff     TYPE char1 OPTIONAL
      EXPORTING
        !ev_server          TYPE rfcdisplay-rfchost
        !ev_path_prefix     TYPE string
      RAISING
        zcx_adf_service .
  PRIVATE SECTION.

    CONSTANTS gc_format_xml TYPE zadf_format_type VALUE 'XML' ##NO_TEXT.
    CONSTANTS gc_format_json TYPE zadf_format_type VALUE 'JSON' ##NO_TEXT.
    CONSTANTS gc_asynchronous TYPE char1 VALUE 'A' ##NO_TEXT.
    CONSTANTS gc_synchronous TYPE char1 VALUE 'S' ##NO_TEXT.
    DATA gv_destination TYPE zazure_dest .
    DATA gv_service_id TYPE zazure_dest .

    METHODS get_interface_details
      RAISING
        zcx_adf_service .
    METHODS get_rest_api_ref
      IMPORTING
        !iv_business_identifier TYPE zbusinessid OPTIONAL
      RAISING
        zcx_adf_service
        zcx_http_client_failed
        zcx_interace_config_missing .
ENDCLASS.



CLASS zcl_adf_service IMPLEMENTATION.


  METHOD add_expiry_time.
    CLEAR: gv_expiry_hour,gv_expiry_min,gv_expiry_sec.
    gv_expiry_hour = iv_expiry_hour.
    gv_expiry_min  = iv_expiry_min.
    gv_expiry_sec  = iv_expiry_sec.
    IF ( gv_expiry_hour IS INITIAL ) AND
       ( gv_expiry_min IS INITIAL )  AND
       ( gv_expiry_sec IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>expiry_time_not_set
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


  METHOD add_request_header.
    go_rest_api->zif_rest_framework~set_request_header( iv_name = iv_name iv_value = iv_value ).
  ENDMETHOD.


  METHOD constructor.
    gv_interface_id = iv_interface_id.
    gv_service_id   = iv_service_id.
    get_interface_details( ).
    get_rest_api_ref( EXPORTING iv_business_identifier = iv_business_identifier ).
  ENDMETHOD.


  METHOD decode_sign.
*This Include is to encrypt secrets with SSL client ID from STRUST     *
*Framework Author : Krishna Chandra Dash                               *
*                   Sasidhar Puranam                                   *
*----------------------------------------------------------------------*
    DATA : lv_rfc_destination TYPE zrest_config-destination,
           lv_srtfd           TYPE zadf_con_indx-srtfd,
           lw_indx            TYPE zadf_con_indx,
           lt_enveloped_data  TYPE TABLE OF ssfbin,
           lv_cert_string     TYPE xstring,
           lt_recipients      TYPE TABLE OF ssfinfo,
           lw_recipient       TYPE ssfinfo,
           lt_input_data      TYPE TABLE OF ssfbin,
           lw_input_data      TYPE ssfbin,
           lv_env_data_len    TYPE i,
           lv_env_len_total   TYPE i,
           lv_subject         TYPE string,
           lw_enveloped_data  TYPE ssfbin,
           lv_xstr_input      TYPE xstring,
           lv_len_output      TYPE i,
           lv_len_input       TYPE i,
           lt_decoded_bin     TYPE TABLE OF x,
           lv_decoded_str     TYPE string,
           lv_applic          TYPE rfcdisplay-sslapplic,
           lv_psename         TYPE ssfpsename,
           lv_profilename     TYPE localfile,
           lv_profile         TYPE ssfparms-pab.
    lv_srtfd = gv_interface_id.
    DEFINE decode_key.
*Import internal table as a cluster from INDX
      IMPORT tab  = lt_enveloped_data[]
             FROM DATABASE zadf_con_indx(zd)
             TO lw_indx
             ID lv_srtfd.
      IF NOT lt_enveloped_data[] IS INITIAL.
        CLEAR lv_rfc_destination.
        SELECT SINGLE destination FROM zrest_config
                                  INTO lv_rfc_destination
                                  WHERE interface_id EQ gv_interface_id.
        IF NOT lv_rfc_destination IS INITIAL .
          CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
            EXPORTING
              destination             = lv_rfc_destination
              authority_check         = ' '
            IMPORTING
              sslapplic               = lv_applic
            EXCEPTIONS
              authority_not_available = 1
              destination_not_exist   = 2
              information_failure     = 3
              internal_failure        = 4
              no_http_destination     = 5
              OTHERS                  = 6.
          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE zcx_adf_service
              EXPORTING
                textid       = zcx_adf_service=>read_error_rfc_destination
                interface_id = gv_interface_id.
          ENDIF.
          CALL FUNCTION 'SSFPSE_FILENAME'
            EXPORTING
              mandt         = sy-mandt
              context       = 'SSLC'
              applic        = lv_applic
            IMPORTING
              psename       = lv_psename
              profile       = lv_profilename
            EXCEPTIONS
              pse_not_found = 1
              OTHERS        = 2.
          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE zcx_adf_service
              EXPORTING
                textid       = zcx_adf_service=>read_error_pse_filename
                interface_id = gv_interface_id.
          ENDIF.
          IF NOT lv_psename IS INITIAL.
            lv_profile = lv_psename.
            CALL FUNCTION 'SSFC_GET_CERTIFICATE'
              EXPORTING
                profile               = lv_profile
              IMPORTING
                certificate           = lv_cert_string
              EXCEPTIONS
                ssf_krn_error         = 1
                ssf_krn_nomemory      = 2
                ssf_krn_nossflib      = 3
                ssf_krn_invalid_par   = 4
                ssf_krn_nocertificate = 5
                OTHERS                = 6.
            IF sy-subrc NE 0.
**Addinng complete profile path for reading certificate instance
              lv_profile = lv_profilename.
              CALL FUNCTION 'SSFC_GET_CERTIFICATE'
                EXPORTING
                  profile               = lv_profile
                IMPORTING
                  certificate           = lv_cert_string
                EXCEPTIONS
                  ssf_krn_error         = 1
                  ssf_krn_nomemory      = 2
                  ssf_krn_nossflib      = 3
                  ssf_krn_invalid_par   = 4
                  ssf_krn_nocertificate = 5
                  OTHERS                = 6.
              IF sy-subrc NE 0.
**Raise Exception
                RAISE EXCEPTION TYPE zcx_adf_service
                  EXPORTING
                    textid       = zcx_adf_service=>error_get_certificate_instance
                    interface_id = gv_interface_id.
              ENDIF.
            ENDIF.
            CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
              EXPORTING
                certificate         = lv_cert_string
              IMPORTING
                subject             = lv_subject
              EXCEPTIONS
                ssf_krn_error       = 1
                ssf_krn_nomemory    = 2
                ssf_krn_nossflib    = 3
                ssf_krn_invalid_par = 4
                OTHERS              = 5.
            IF sy-subrc NE 0.
**Raise Exception
              RAISE EXCEPTION TYPE zcx_adf_service
                EXPORTING
                  textid       = zcx_adf_service=>error_attributes_certificate
                  interface_id = gv_interface_id.
            ENDIF.
            lw_recipient-id      = lv_subject.
            lw_recipient-profile = lv_profile.
            APPEND lw_recipient TO lt_recipients.
            LOOP AT lt_enveloped_data INTO lw_enveloped_data.
              lv_env_data_len = xstrlen( lw_enveloped_data-bindata ).
              lv_env_len_total = lv_env_len_total + lv_env_data_len.
              CLEAR lw_enveloped_data.
            ENDLOOP.
            CALL FUNCTION 'SSF_KRN_DEVELOPE'
              EXPORTING
                ssftoolkit                   = 'SAPSECULIB'
                str_format                   = 'PKCS7'
*           B_OUTDEC                     = 'X'
*           IO_SPEC                      = 'T'
                ostr_enveloped_data_l        = lv_env_len_total
              IMPORTING
                ostr_output_data_l           = lv_len_input
*           CRC                          =
              TABLES
                ostr_enveloped_data          = lt_enveloped_data
                recipient                    = lt_recipients
                ostr_output_data             = lt_input_data
              EXCEPTIONS
                ssf_krn_error                = 1
                ssf_krn_noop                 = 2
                ssf_krn_nomemory             = 3
                ssf_krn_opinv                = 4
                ssf_krn_nossflib             = 5
                ssf_krn_recipient_error      = 6
                ssf_krn_input_data_error     = 7
                ssf_krn_invalid_par          = 8
                ssf_krn_invalid_parlen       = 9
                ssf_fb_input_parameter_error = 10
                OTHERS                       = 11.
            IF sy-subrc NE 0.
**Raise Exception
              RAISE EXCEPTION TYPE zcx_adf_service
                EXPORTING
                  textid       = zcx_adf_service=>error_decode_sas_key
                  interface_id = gv_interface_id.
            ENDIF.
            IF NOT lt_input_data[] IS INITIAL.
              CALL FUNCTION 'SCMS_BINARY_TO_STRING'
                EXPORTING
                  input_length  = lv_len_input
                IMPORTING
                  text_buffer   = lv_decoded_str
                  output_length = lv_len_output
                TABLES
                  binary_tab    = lt_input_data
                EXCEPTIONS
                  failed        = 1
                  OTHERS        = 2.
              IF lv_decoded_str IS INITIAL.
**Raise exception
                RAISE EXCEPTION TYPE zcx_adf_service
                  EXPORTING
                    textid       = zcx_adf_service=>error_con_saskey_string
                    interface_id = gv_interface_id.
              ELSE.
                rv_secret = lv_decoded_str.
              ENDIF.
            ELSE.
**Raise exception
              RAISE EXCEPTION TYPE zcx_adf_service
                EXPORTING
                  textid       = zcx_adf_service=>error_read_encoded_saskey
                  interface_id = gv_interface_id.
            ENDIF.
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid       = zcx_adf_service=>rfc_destination_not_maintained
              interface_id = gv_interface_id.
        ENDIF.
      ELSE.
**Raise Exception
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>error_import_sas_key
            interface_id = gv_interface_id.
      ENDIF.
      IF rv_secret IS INITIAL.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>sas_key_not_maintained
            interface_id = gv_interface_id.
      ENDIF.
    END-OF-DEFINITION.
    decode_key.
  ENDMETHOD.


  METHOD format_data.
    CASE iv_format_type.
      WHEN gc_format_xml.
        rv_string = format_data_to_xml( it_data ).
      WHEN gc_format_json.
        rv_string = format_data_to_json( it_data ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid      = zcx_adf_service=>format_not_supported
            format_type = iv_format_type.
    ENDCASE.
  ENDMETHOD.


  METHOD format_data_to_json.

    DATA : lo_json TYPE REF TO cl_trex_json_serializer.

    CREATE OBJECT lo_json
      EXPORTING
        data = it_data.
    lo_json->serialize( ).
    rv_string  = lo_json->get_data( ).
  ENDMETHOD.


  METHOD format_data_to_xml.
    CALL TRANSFORMATION ('ID') SOURCE itab = it_data RESULT XML rv_string.
  ENDMETHOD.


  METHOD get_epoch_time.
    DATA : lv_start_timestamp   TYPE timestampl,
           lv_current_timestamp TYPE timestampl,
           lv_seconds           TYPE p,
           lv_input_seconds     TYPE p,
           zcx_adf_service      TYPE REF TO zcx_adf_service.
    DATA: date TYPE datum,
          time TYPE uzeit.
*Get the current timestamp
    GET TIME STAMP FIELD  lv_current_timestamp .
*Get the time difference
    DATA zone TYPE sy-zonlo.
    CONVERT TIME STAMP lv_current_timestamp TIME ZONE zone INTO DATE date TIME time.
    TRY.
        CALL METHOD cl_abap_tstmp=>td_subtract
          EXPORTING
            date1    = date
            time1    = time
            date2    = '19700101'
            time2    = '000000'
          IMPORTING
            res_secs = lv_seconds.
* Add expiry time in seconds
        lv_input_seconds = ( ( gv_expiry_hour * 60 ) * 60  ) + ( gv_expiry_min * 60 ) +
                           ( gv_expiry_sec ).
        lv_seconds = lv_seconds + lv_input_seconds.
        rv_expiry_time = lv_seconds.
        CONDENSE rv_expiry_time.
      CATCH cx_parameter_invalid_type.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>parameter_invalid_type
            interface_id = gv_interface_id.
      CATCH cx_parameter_invalid_range .
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>parameter_invalid_range
            interface_id = gv_interface_id.
    ENDTRY.
  ENDMETHOD.


  METHOD get_interface_details.

    DATA : ls_config      TYPE zadf_config,
           lv_host        TYPE rfcdisplay-rfchost,
           lv_host_s      TYPE string,
           lv_path_prefix TYPE string.

    SELECT SINGLE * FROM zadf_config
           INTO ls_config
           WHERE interface_id = gv_interface_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>interface_not_available
          interface_id = gv_interface_id.
    ENDIF.

    SELECT SINGLE destination FROM zrest_config INTO gv_rfc_destination
         WHERE interface_id = gv_interface_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>interface_not_available
          interface_id = gv_interface_id.
    ENDIF.

**Getting Target host of RFC destination
    get_target_host( EXPORTING iv_destination = gv_rfc_destination IMPORTING  ev_server = lv_host
                                                                    ev_path_prefix = lv_path_prefix ).
    gv_host = lv_host.
    gv_path_prefix = lv_path_prefix.
    lv_host_s = lv_host.
    CONCATENATE lv_host_s lv_path_prefix INTO gv_uri.
    IF gv_uri IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>uri_not_maintained
          interface_id = gv_interface_id.
    ENDIF.
    IF ls_config-service_type IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>call_type_not_maintained
          interface_id = gv_interface_id.
    ELSE.
      IF ls_config-service_type EQ gc_synchronous.
        gv_asynchronous = abap_false.
      ELSEIF ls_config-service_type EQ gc_asynchronous.
        gv_asynchronous = abap_true.
      ENDIF.
    ENDIF.
    gv_is_try = ls_config-is_try.
    gv_sas_key = ls_config-sas_key. "Added by KRDASH
  ENDMETHOD.


  METHOD get_message_id.

    rv_message_id = go_rest_api->get_guid( ).

  ENDMETHOD.


  METHOD get_rest_api_ref.
    DATA : lcx_interface TYPE REF TO zcx_interace_config_missing,
           lcx_http      TYPE REF TO zcx_http_client_failed,
           lv_method     TYPE char20.
    IF go_rest_api IS INITIAL.
      CLEAR lv_method.
      SELECT SINGLE method FROM zrest_conf_misc
                           INTO lv_method
                           WHERE interface_id EQ gv_interface_id.
      IF lv_method IS INITIAL.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>method_not_maintained
            interface_id = gv_interface_id.
      ENDIF.
      TRY.
          CREATE OBJECT go_rest_api
            EXPORTING
              interface_name      = gv_interface_id       "Mandatory
              business_identifier = iv_business_identifier
              method              = lv_method.               "For troubleshooting
        CATCH zcx_interace_config_missing INTO lcx_interface.
          RAISE EXCEPTION lcx_interface.
        CATCH zcx_http_client_failed INTO lcx_http .
          RAISE EXCEPTION lcx_http.
      ENDTRY.
      IF go_rest_api IS BOUND.
*Optional - To help developer understand the origin of call
        go_rest_api->set_callingmethod( EXPORTING zimethodname = 'SEND' ).
*Optional - To help developer understand the origin of call
        go_rest_api->set_callingprogram( EXPORTING ziclassname = 'ZCL_ADF_SERVICE' ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>error_rest_api_instance
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_sas_token.
    DATA : lv_string_to_sign    TYPE string,
           encoded_base_address TYPE string,
           body_xstring         TYPE xstring,
           sign                 TYPE string,
           final_token          TYPE string,
           decoded              TYPE xstring,
           conv                 TYPE REF TO cl_abap_conv_out_ce,
           conv_in              TYPE REF TO cl_abap_conv_in_ce,
           format               TYPE i,
           new_expiry           TYPE string,
           lv_sas_key           TYPE string,
           lv_expiry_time       TYPE string.
    get_epoch_time( RECEIVING rv_expiry_time =  lv_expiry_time ).
    format = 18.
    encoded_base_address = escape( val = iv_baseaddress format = format  ).
    CONCATENATE encoded_base_address  cl_abap_char_utilities=>newline lv_expiry_time INTO lv_string_to_sign.

    conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = body_xstring ).
    DEFINE encrypt_key.
      decode_sign( RECEIVING rv_secret = lv_sas_key ).
      conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
      conv->convert( EXPORTING data = lv_sas_key IMPORTING buffer = decoded ).

      CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
        EXPORTING
          if_algorithm     = 'sha-256'
          if_key           = decoded
          if_data          = body_xstring
          if_length        = 0
        IMPORTING
          ef_hmacb64string = sign.
      CLEAR : lv_sas_key, decoded.
    END-OF-DEFINITION.
    encrypt_key.
    new_expiry = lv_expiry_time.
    CONDENSE new_expiry.
    IF NOT sign IS INITIAL.
      sign = escape( val = sign format = format  ).
      CONCATENATE 'SharedAccessSignature sr=' encoded_base_address  '&sig=' sign '&se=' new_expiry '&skn=' 'edit' INTO final_token.
      rv_sas_token = final_token.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>sas_key_not_generated
          interface_id = gv_interface_id.
    ENDIF.
<<<<<<< HEAD
  ENDMETHOD.


  METHOD get_target_host.
    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
=======
  ENDIF.
ENDMETHOD.


METHOD get_sas_token.
  DATA : lv_string_to_sign    TYPE string,
         encoded_base_address TYPE string,
         body_xstring         TYPE xstring,
         sign                 TYPE string,
         final_token          TYPE string,
         decoded              TYPE xstring,
         conv                 TYPE REF TO cl_abap_conv_out_ce,
         conv_in              TYPE REF TO cl_abap_conv_in_ce,
         new_expiry           TYPE string,
         lv_sas_key           TYPE string,
         lv_expiry_time       TYPE string.
  get_epoch_time( RECEIVING rv_expiry_time =  lv_expiry_time ).
  encoded_base_address = escape( val = iv_baseaddress format = cl_abap_format=>e_uri_full ).
  CONCATENATE encoded_base_address  cl_abap_char_utilities=>newline lv_expiry_time INTO lv_string_to_sign.

  conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
  conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = body_xstring ).
  DEFINE encrypt_key.
    decode_sign( RECEIVING rv_secret = lv_sas_key ).
    conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    conv->convert( EXPORTING data = lv_sas_key IMPORTING buffer = decoded ).

    CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
      EXPORTING
        if_algorithm     = 'sha-256'
        if_key           = decoded
        if_data          = body_xstring
        if_length        = 0
      IMPORTING
        ef_hmacb64string = sign.
    CLEAR : lv_sas_key, decoded.
  END-OF-DEFINITION.
  encrypt_key.
  new_expiry = lv_expiry_time.
  CONDENSE new_expiry.
  IF NOT sign IS INITIAL.
    sign = escape( val = sign format = cl_abap_format=>e_uri_full ).
    CONCATENATE 'SharedAccessSignature sr=' encoded_base_address  '&sig=' sign '&se=' new_expiry '&skn=' 'edit' INTO final_token.
    rv_sas_token = final_token.
  ELSE.
    RAISE EXCEPTION TYPE zcx_adf_service
>>>>>>> pr/34
      EXPORTING
        destination             = iv_destination
        authority_check         = iv_authority_check
        bypass_buf              = iv_bypass_buff
      IMPORTING
        server                  = ev_server
        path_prefix             = ev_path_prefix
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>read_error_rfc_destination
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


  METHOD json_to_http_fields.
    DATA: ls_fields       TYPE ihttpnvp,
          l_node          TYPE REF TO if_sxml_node,
          l_error         TYPE string,
          lr_open_element TYPE REF TO  if_sxml_open_element,
          lt_attributes   TYPE if_sxml_attribute=>attributes,
          ls_attribute    LIKE LINE OF lt_attributes,
          lr_value_node   TYPE REF TO if_sxml_value_node,
          json            TYPE xstring,
          reader          TYPE REF TO if_sxml_reader,
          parse_error     TYPE REF TO cx_sxml_parse_error.
    TRY .
        json = cl_abap_codepage=>convert_to( iv_response_data ).
        reader = cl_sxml_string_reader=>create( json ).
        " after parse, the json response should look like following

        DO.
          CLEAR ls_fields.
          l_node = reader->read_next_node( ).

          IF l_node IS INITIAL.
            EXIT.
          ENDIF.
          CASE l_node->type.
            WHEN if_sxml_node=>co_nt_element_open.
              lr_open_element ?= l_node.
              lt_attributes  = lr_open_element->get_attributes( ).
              IF lt_attributes IS NOT INITIAL.
                " get name
                READ TABLE lt_attributes INDEX 1 INTO ls_attribute.
                ls_fields-name = ls_attribute->get_value( ).
                " get value
                l_node = reader->read_next_node( ).
                IF l_node->type = if_sxml_node=>co_nt_value..
                  lr_value_node ?= l_node .
                  ls_fields-value = lr_value_node->get_value( ).
                  " add field into the result table
                  APPEND ls_fields TO et_fields.
                ENDIF.
              ENDIF.
            WHEN OTHERS.
              " do nothing
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error INTO parse_error.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>parse_error
            interface_id = gv_interface_id.
    ENDTRY.
  ENDMETHOD.


  METHOD send.
    DATA : lo_response     TYPE REF TO if_rest_entity,
           lo_request      TYPE REF TO if_rest_entity,
           lv_expiry       TYPE string,
           lv_sas_token    TYPE string,
           lv_msg          TYPE string,
           lcx_adf_service TYPE REF TO zcx_adf_service.
    IF go_rest_api IS BOUND.
      TRY.
          get_sas_token( EXPORTING iv_baseaddress = gv_uri
                         RECEIVING rv_sas_token  = lv_sas_token ).
        CATCH zcx_adf_service INTO lcx_adf_service.
          lv_msg =  lcx_adf_service->get_text( ).
          MESSAGE lv_msg TYPE 'I'.
      ENDTRY.
      add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).
      add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ).
      go_rest_api->zif_rest_framework~set_binary_body( request ).
      IF NOT it_headers[] IS INITIAL.
        go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = it_headers[] ).
      ENDIF.
**Rest API call to get response from Azure Destination
      lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
      ev_http_status = go_rest_api->get_status( ).
      go_rest_api->close( ).
      IF lo_response IS BOUND.
        response = lo_response->get_string_data( ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>restapi_response_not_found
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD urlencoded_to_http_fields.
    DATA: lt_params         TYPE STANDARD TABLE OF string,
          ls_response_field LIKE LINE OF et_fields.
    FIELD-SYMBOLS:  <l_param> LIKE LINE OF lt_params.
    SPLIT iv_response_data AT `&` INTO TABLE lt_params.
    LOOP AT lt_params ASSIGNING <l_param>.
      SPLIT <l_param> AT `=` INTO ls_response_field-name
                                  ls_response_field-value.
      APPEND ls_response_field TO et_fields.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
