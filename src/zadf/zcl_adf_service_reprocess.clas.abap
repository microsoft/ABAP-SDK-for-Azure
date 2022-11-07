class ZCL_ADF_SERVICE_REPROCESS definition
  public
  final
  create public .

public section.

  constants GC_AAD type CHAR5 value 'AAD' ##NO_TEXT.
  constants GC_MSI type CHAR3 value 'MSI' ##NO_TEXT.
  constants GC_SERVICE_COSMOSDB type ZAZURE_DEST value 'COSMOSDB' ##NO_TEXT.
  constants GC_I type CHAR1 value 'I' ##NO_TEXT.
  constants GC_SERVICE_BLOB type ZADF_CONFIG-INTERFACE_TYPE value 'BLOB' ##NO_TEXT.
  constants GC_SERVICEBUS type ZAZURE_DEST value 'SERVICEBUS' ##NO_TEXT.

  methods GENERATE_AUTH_TOKEN
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
      !IV_URI type ZURI optional
      value(IV_INTERFACE_TYPE) type ZAZURE_DEST
    exporting
      value(RV_TOKEN) type STRING
      value(RV_DATE) type STRING
      value(RV_AAD_TOKEN) type STRING
      value(RV_MI_TOKEN) type STRING
      value(RV_SBUS_TOKEN) type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods GET_TOKEN_SERVICEBUS
    importing
      value(IV_INTERFACE_NAME) type ZINTERFACE_ID
      value(IV_URI) type ZURI optional
    exporting
      value(EV_FINAL_TOKEN) type STRING
    raising
      ZCX_ADF_SERVICE .
  methods GET_AAD_TOKEN
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
      value(IV_CLIENT_ID) type STRING
      value(IV_RESOURCE) type TVARV_VAL optional
    exporting
      value(EV_AAD_TOKEN) type STRING .
protected section.
private section.

  methods GET_TOKEN_MI
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
      value(IV_URI) type ZURI
    exporting
      value(RV_MI_TOKEN) type STRING
      value(RV_DATE) type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods AUTH_INTERFACE_TYPE
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
    changing
      value(CV_INTERFACE_TYPE) type ZAZURE_DEST .
  methods GET_TOKEN_COSMOSDB
    importing
      !IV_INTERFACE_ID type ZINTERFACE_ID
      !IV_URI type ZURI optional
    exporting
      value(RV_TOKEN) type STRING
      value(RV_DATE) type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods CALL_STACK_CHECK
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods GET_TOKEN_BLOB
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
      value(IV_URI) type ZURI
    exporting
      value(RV_SAS_TOKEN) type STRING
      value(RV_DATE) type STRING
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
ENDCLASS.



CLASS ZCL_ADF_SERVICE_REPROCESS IMPLEMENTATION.


  METHOD auth_interface_type.
* Check the type of Interface(Azure AD/Managed Identity)
    SELECT SINGLE interface_id
                  FROM zadf_mi_config
                  INTO @DATA(lw_mi_config)
                  WHERE interface_id = @iv_interface_id.
    IF sy-subrc EQ 0.
      cv_interface_type = gc_msi.
    ELSE.
      cv_interface_type = cv_interface_type.
    ENDIF.
  ENDMETHOD.


METHOD call_stack_check.
  CONSTANTS : lc_zrest_util     TYPE sy-tcode VALUE 'ZREST_UTIL',
              lc_mainprog       TYPE abap_callstack_line-mainprogram VALUE 'ZREST_SCHEDULER',
              lc_blockty        TYPE abap_callstack_line-blocktype VALUE 'EVENT',
              lc_blockname      TYPE abap_callstack_line-blockname VALUE 'START-OF-SELECTION'.
DEFINE call_stack.
  IF sy-tcode NE lc_zrest_util.
    DATA : lt_abap_stack TYPE abap_callstack,
           lt_syst_stack TYPE sys_callst.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack    = lt_abap_stack
        et_callstack = lt_syst_stack.
    READ TABLE lt_abap_stack TRANSPORTING NO FIELDS
    WITH KEY mainprogram = lc_mainprog
             blocktype   = lc_blockty
             blockname   = lc_blockname.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>execution_terminated
          interface_id = space.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.
  call_stack.
ENDMETHOD.


METHOD generate_auth_token.
  DEFINE gen_auth.
    me->call_stack_check( ).
    DATA(lv_interface_type) = iv_interface_type.
    auth_interface_type( EXPORTING
                           iv_interface_id = iv_interface_id
                         CHANGING
                           cv_interface_type = lv_interface_type ).

*    CASE iv_interface_type.
    CASE lv_interface_type.
      WHEN gc_service_cosmosdb. " Cosmos DB Service
        me->get_token_cosmosdb( EXPORTING iv_interface_id = iv_interface_id
                                          iv_uri          = iv_uri
                                IMPORTING rv_token = rv_token
                                          rv_date  = rv_date ).
      WHEN gc_service_blob. "Blob Service
        CALL METHOD me->get_token_blob
          EXPORTING
            iv_interface_id = iv_interface_id
            iv_uri          = iv_uri
          IMPORTING
            rv_sas_token    = rv_token.
* Begin of Change SANJUKUM_SMTK907382
      WHEN gc_msi.
            CALL METHOD me->get_token_mi
          EXPORTING
            iv_interface_id = iv_interface_id
            iv_uri          = iv_uri
          IMPORTING
            rv_mi_token    = rv_mi_token.
      WHEN gc_servicebus.
          CALL METHOD me->get_token_servicebus
              EXPORTING
                iv_interface_name = iv_interface_id
                iv_uri            = iv_uri
              IMPORTING
                ev_final_token    = rv_sbus_token.
* End of Change SANJUKUM_SMTK907382
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>interface_type_not_maintained
            interface_id = iv_interface_id.
    ENDCASE.
  END-OF-DEFINITION.
  gen_auth.
ENDMETHOD.


  METHOD get_aad_token.
    DATA: lo_ref_aad TYPE REF TO zcl_adf_service_aad.

* Generate Reference to AAD
    TRY.
        lo_ref_aad ?= zcl_adf_service_factory=>create( iv_interface_id =  iv_interface_id
                                                       iv_business_identifier = 'RETRY' ).
* Generate Token
        CALL METHOD lo_ref_aad->get_aad_token
          EXPORTING
            iv_client_id = iv_client_id
            iv_resource  = CONV #( iv_resource )
          IMPORTING
            ev_aad_token = DATA(lv_aad_token)
            ev_response  = DATA(lv_response).

        CONCATENATE 'Bearer' space lv_aad_token INTO ev_aad_token RESPECTING BLANKS.
      CATCH zcx_interace_config_missing INTO DATA(cx_interface_aad).
        DATA(lv_string1) = cx_interface_aad->get_text( ).
        MESSAGE lv_string1 TYPE gc_i.
        RETURN.
      CATCH zcx_http_client_failed INTO DATA(cx_http_aad) .
        lv_string1 = cx_http_aad->get_text( ).
        MESSAGE lv_string1 TYPE gc_i.
        RETURN.
      CATCH zcx_adf_service INTO DATA(cx_adf_service).
        lv_string1 = cx_adf_service->get_text( ).
        MESSAGE lv_string1 TYPE gc_i.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD get_token_blob.

    DATA : lo_ref_super           TYPE REF TO zcl_adf_service,
           lo_ref_blob            TYPE REF TO zcl_adf_service_blob,
           lv_uri                 TYPE string,
           lv_auth_token          TYPE string,
           lv_business_identifier TYPE zbusinessid,
           lv_temp                TYPE string,
           lv_temp1               TYPE string,
           lv_container_name      TYPE string,
           lv_storage_account     TYPE string,
           lv_blob_name           TYPE string,
           lv_server              TYPE rfcdisplay-rfchost,
           lv_rfc_destination     TYPE rfcdest.

    SELECT SINGLE destination FROM zrest_config
                              INTO lv_rfc_destination
                              WHERE interface_id EQ iv_interface_id.
    IF NOT lv_rfc_destination IS INITIAL .
      CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
        EXPORTING
          destination             = lv_rfc_destination
          authority_check         = ' '
        IMPORTING
          server                  = lv_server
        EXCEPTIONS
          authority_not_available = 1
          destination_not_exist   = 2
          information_failure     = 3
          internal_failure        = 4
          no_http_destination     = 5
          OTHERS                  = 6.
      IF sy-subrc EQ 0.
        lo_ref_super = zcl_adf_service_factory=>create( iv_interface_id =  iv_interface_id ).
        lo_ref_blob ?= lo_ref_super.
        CLEAR lo_ref_blob->go_rest_api.
        lv_temp = lv_server.
        SPLIT lv_temp AT '.' INTO lv_storage_account lv_temp.
        CLEAR lv_temp.
        SPLIT iv_uri AT '/' INTO lv_temp lv_container_name lv_temp1.
        SPLIT lv_temp1 AT '?' INTO lv_blob_name lv_temp1.
        CLEAR: lv_temp, lv_temp1.

* Begin of SANJUKUM_SMTK907382
* Setting Expiry time
        CALL METHOD lo_ref_blob->add_expiry_time
          EXPORTING
            iv_expiry_hour = 0
            iv_expiry_min  = 15
            iv_expiry_sec  = 0.
* Setting additional attributes
        CALL METHOD lo_ref_blob->set_blob_additional_attributes.
* End of SANJUKUM_SMTK907382
        CALL METHOD lo_ref_blob->set_blob_name_type
          EXPORTING
            iv_blob_name = lv_blob_name.
        CALL METHOD lo_ref_blob->set_storage_account_container
          EXPORTING
            iv_storage_account = lv_storage_account
            iv_container_name  = lv_container_name.

        lo_ref_blob->get_sas_token( EXPORTING iv_baseaddress = lo_ref_blob->gv_uri
                                    RECEIVING rv_sas_token  = rv_sas_token ).

        CLEAR: lo_ref_blob, lo_ref_super.
      ELSE.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>rfc_destination_not_maintained
            interface_id = iv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD get_token_cosmosdb.
  CONSTANTS : lc_separator TYPE c VALUE '/'.
  DATA : lo_ref_super     TYPE REF TO zcl_adf_service,
         lo_ref_cosmosdb  TYPE REF TO zcl_adf_service_cosmosdb,
         lv_resource_type TYPE string,
         lv_resource_link TYPE string,
         lv_temp          TYPE string,
         lv_dbs           TYPE string,
         lv_db_name       TYPE string,
         lv_colls         TYPE string,
         lv_coll_name     TYPE string.
  IF NOT iv_uri IS INITIAL.
    lv_temp = iv_uri.
    SPLIT lv_temp+1 AT lc_separator
                    INTO lv_dbs lv_db_name lv_colls lv_coll_name lv_resource_type.
    CLEAR lv_temp.
    CONCATENATE lv_dbs lv_db_name lv_colls lv_coll_name INTO lv_resource_link
    SEPARATED BY lc_separator.
**Instantiating factory class for Cosmosdb
    lo_ref_super = zcl_adf_service_factory=>create( iv_interface_id =  iv_interface_id ).
    lo_ref_cosmosdb ?= lo_ref_super.
    CLEAR lo_ref_cosmosdb->go_rest_api.
**Setting cosmosdb parameters
    lo_ref_cosmosdb->set_parameters( EXPORTING iv_http_verb         = 'post'
                                               iv_resource_type     = lv_resource_type
                                               iv_resource_link     = lv_resource_link ).
**generating token
    lo_ref_cosmosdb->get_sas_token( EXPORTING iv_baseaddress = lo_ref_cosmosdb->gv_uri
                                    RECEIVING rv_sas_token   = rv_token ).
    rv_date = lo_ref_cosmosdb->gv_http_date.
    CLEAR: lo_ref_cosmosdb, lo_ref_super.
  ELSE.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>path_prefix_not_maintained
        interface_id = iv_interface_id.
  ENDIF.
ENDMETHOD.


  METHOD get_token_mi.
    DATA: lo_adf     TYPE REF TO zcl_adf_service,
          lo_aad_ref TYPE REF TO zcl_adf_service_aad.

* Calling Factory method to instantiate AAD client
    lo_adf = zcl_adf_service_factory=>create( iv_interface_id =  iv_interface_id
                                              iv_business_identifier = 'MI_TOKEN_RETRY' ).
    lo_aad_ref ?= lo_adf.
* Fetch the MI Token
    lo_aad_ref->get_aad_token_msi(
      IMPORTING
        ev_aad_token  = rv_mi_token
        ev_response  = DATA(lv_response)
        ev_http_status = DATA(http_status)
      ).
  ENDMETHOD.


  METHOD get_token_servicebus.
* Variables Delcarations
    DATA: lv_srtfd                TYPE zadf_con_indx-srtfd,
          lv_applic               TYPE rfcdisplay-sslapplic,
          lv_psename              TYPE ssfpsename,
          lv_cert_string          TYPE xstring,
          lv_string_to_sign       TYPE string,
          lv_body_xstring         TYPE xstring,
          lv_profile              TYPE ssfparms-pab,
          lv_encoded_base_address TYPE string,
          lv_seconds_adf          TYPE p,
          lv_input_seconds_adf    TYPE p,
          lv_expiry_time_adf      TYPE string,
          lv_new_expiry_adf       TYPE string,
          lv_env_len_total        TYPE i,
          lv_len_output           TYPE i,
          lv_len_input            TYPE i,
          lv_decoded              TYPE xstring,
          lv_subject              TYPE string,
          lv_decoded_str          TYPE string,
          lv_current_timestamp    TYPE timestampl,
          lv_zone                 TYPE sy-zonlo,
          lw_indx                 TYPE zadf_con_indx,
          lt_enveloped_data       TYPE TABLE OF ssfbin,
          lt_recipients           TYPE TABLE OF ssfinfo,
          lt_input_data           TYPE TABLE OF ssfbin,
          lw_recipient            TYPE ssfinfo.

* Constants Declarations
    CONSTANTS : lc_i              TYPE char1 VALUE 'I'.

    lv_srtfd = iv_interface_name.
*Import internal table as a cluster from INDX for decoding SAS Primary key
    IMPORT tab  = lt_enveloped_data[]
           FROM DATABASE zadf_con_indx(zd)
           TO lw_indx
           ID lv_srtfd.
    IF NOT lt_enveloped_data[] IS INITIAL.
      SELECT SINGLE destination
             FROM zrest_config
             INTO @DATA(lv_rfc_destination)
             WHERE interface_id EQ @iv_interface_name.
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
          MESSAGE text-008 TYPE lc_i.
        ELSE.
          CALL FUNCTION 'SSFPSE_FILENAME'
            EXPORTING
              mandt         = sy-mandt
              context       = 'SSLC'
              applic        = lv_applic
            IMPORTING
              psename       = lv_psename
            EXCEPTIONS
              pse_not_found = 1
              OTHERS        = 2.
          IF sy-subrc EQ 0 AND lv_psename IS NOT INITIAL.
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
              MESSAGE text-007 TYPE lc_i.
            ELSE.
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
                MESSAGE text-006 TYPE lc_i.
              ELSE.
                lw_recipient-id      = lv_subject.
                lw_recipient-profile = lv_profile.
                APPEND lw_recipient TO lt_recipients.
                LOOP AT lt_enveloped_data INTO DATA(lw_enveloped_data).
                  DATA(lv_env_data_len) = xstrlen( lw_enveloped_data-bindata ).
                  lv_env_len_total = lv_env_len_total + lv_env_data_len.
                  CLEAR lw_enveloped_data.
                ENDLOOP.
                CALL FUNCTION 'SSF_KRN_DEVELOPE'
                  EXPORTING
                    ssftoolkit                   = 'SAPSECULIB'
                    str_format                   = 'PKCS7'
                    ostr_enveloped_data_l        = lv_env_len_total
                  IMPORTING
                    ostr_output_data_l           = lv_len_input
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
                  MESSAGE text-005 TYPE lc_i.
                ELSE.
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
                    IF sy-subrc EQ 0 AND lv_decoded_str IS INITIAL.
                      MESSAGE text-004 TYPE lc_i.
                    ELSE.
                      DATA(lv_sas_key) = lv_decoded_str.
                    ENDIF.
                  ELSE.
                    MESSAGE text-003 TYPE lc_i.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE text-002 TYPE lc_i.
      ENDIF.
    ELSE.
      MESSAGE text-001 TYPE lc_i.
    ENDIF.
**Calculating Epoch time for SAS Token
    IF NOT lv_sas_key IS INITIAL.
*Get the current timestamp
      GET TIME STAMP FIELD  lv_current_timestamp .
*Get the time difference
      CONVERT TIME STAMP lv_current_timestamp TIME ZONE lv_zone INTO DATE DATA(lv_date_adf) TIME DATA(lv_time_adf).
      TRY.
          CALL METHOD cl_abap_tstmp=>td_subtract
            EXPORTING
              date1    = lv_date_adf
              time1    = lv_time_adf
              date2    = '19700101'
              time2    = '000000'
            IMPORTING
              res_secs = lv_seconds_adf.
* Add expiry time in seconds
          lv_input_seconds_adf = 900 . "Setting expiry time as 30 mins
          lv_seconds_adf = lv_seconds_adf + lv_input_seconds_adf.
          lv_expiry_time_adf = lv_seconds_adf.
          CONDENSE lv_expiry_time_adf.
        CATCH cx_parameter_invalid_type INTO DATA(lx_param_inv_type).
          DATA(lv_inv_ex) = lx_param_inv_type->get_text( ).
        CATCH cx_parameter_invalid_range INTO DATA(lx_param_invalid_range).
          DATA(lv_str_ex) = lx_param_invalid_range->get_text( ).
      ENDTRY.
**Generating SAS token with new expiry time
      IF NOT lv_expiry_time_adf IS INITIAL.
        DATA(lv_baseaddress) = iv_uri.
        DATA(lv_format) = 18.
        lv_encoded_base_address = escape( val = lv_baseaddress format = lv_format  ).
        CONCATENATE lv_encoded_base_address  cl_abap_char_utilities=>newline lv_expiry_time_adf INTO lv_string_to_sign.

        DATA(lo_conv) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
        lo_conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = lv_body_xstring ).

        lo_conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
        lo_conv->convert( EXPORTING data = lv_sas_key IMPORTING buffer = lv_decoded ).
        TRY.
            CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
              EXPORTING
                if_algorithm     = 'sha-256'
                if_key           = lv_decoded
                if_data          = lv_body_xstring
                if_length        = 0
              IMPORTING
                ef_hmacb64string = DATA(lv_sign).
          CATCH cx_abap_message_digest INTO DATA(lx_abap_msg_digest).
            DATA(lv_msg_digest) = lx_abap_msg_digest->get_text( ).
        ENDTRY.
        lv_new_expiry_adf = lv_expiry_time_adf.
        CONDENSE lv_new_expiry_adf.
* Begin of Change SANJUKUM_SMTK907382
* Fetch the policy name
        SELECT SINGLE interface_id,
               policy
               FROM zadf_ehub_policy
               INTO @DATA(lwa_adf_policy)
               WHERE interface_id EQ @iv_interface_name.
        IF sy-subrc EQ 0.
          DATA(lv_policy) = lwa_adf_policy-policy.
        ENDIF.
* End of Change SANJUKUM_SMTK907382
        IF NOT lv_sign IS INITIAL.
          lv_sign = escape( val = lv_sign format = lv_format  ).
* Begin of Change SANJUKUM_SMTK907382
          IF lv_policy IS NOT INITIAL.
            ev_final_token = |SharedAccessSignature sig={ lv_sign }&se={ lv_new_expiry_adf }&skn={ lv_policy }&sr={ lv_encoded_base_address }|.
          ELSE.
            RAISE EXCEPTION TYPE zcx_adf_service
              EXPORTING
                textid       = zcx_adf_service=>policy_not_maintained
                interface_id = iv_interface_name.
* End of Change SANJUKUM_SMTK907382
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
