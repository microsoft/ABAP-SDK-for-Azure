class ZCL_ADF_MANAGE_ACCESS_KEYS definition
  public
  final
  create private .

public section.

  class-methods GET_INSTANCE
    returning
      value(RO_REF) type ref to ZCL_ADF_MANAGE_ACCESS_KEYS
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
  methods SET_ENCRYPT_CERT_APPLIC
    importing
      value(IV_APPLIC) type SSFAPPLSSL .
  methods SET_DECRYPT_CERT_APPLIC
    importing
      value(IV_APPLIC) type SSFAPPLSSL .
  methods DECRYPT
    importing
      value(IT_ENCRYPTED_DATA) type SAML2_PSE_BIN_DATA_T
    returning
      value(RV_SECRET) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
  methods ENCRYPT
    importing
      value(IV_SECRET) type STRING
    returning
      value(RT_ENCRYPTED_DATA) type SAML2_PSE_BIN_DATA_T
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
  class-methods READ_APPLIC_FROM_DESTINATION
    importing
      !IV_DESTINATION type RFCDEST
    returning
      value(RV_APPLIC) type SSFAPPLSSL
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
protected section.
private section.

  data GV_ENCRYPT_CERT_APPLIC type SSFAPPLSSL .
  data GV_DECRYPT_CERT_APPLIC type SSFAPPLSSL .

  class-methods CHECK_CALL_STACK
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
  methods GET_ENCRYPT_CERT_APPLIC
    returning
      value(RV_APPLIC) type SSFAPPLSSL
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
  methods GET_DECRYPT_CERT_APPLIC
    returning
      value(RV_APPLIC) type SSFAPPLSSL
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
  methods GET_PSE_FROM_APPLIC
    importing
      value(IV_APPLIC) type SSFAPPLSSL
    exporting
      value(RV_PSENAME) type SSFPSENAME
      value(RV_PROFILE) type LOCALFILE
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
  methods GET_CERT_FROM_PSE
    importing
      value(IV_PSENAME) type SSFPSENAME
      value(IV_PROFILE) type LOCALFILE
    returning
      value(RV_CERT) type XSTRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
  methods PARSE_CERTIFICATE
    importing
      value(IV_CERT) type XSTRING
    returning
      value(RV_SUBJECT) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS .
ENDCLASS.



CLASS ZCL_ADF_MANAGE_ACCESS_KEYS IMPLEMENTATION.


  METHOD check_call_stack.

    DATA: lt_abap_stack TYPE  abap_callstack,
          ls_abap_stack TYPE  abap_callstack_line,
          lt_sys_stack  TYPE  sys_callst,
          lv_allowed    TYPE  boolean.

    lv_allowed = abap_false.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack    = lt_abap_stack
        et_callstack = lt_sys_stack.

    LOOP AT lt_abap_stack INTO ls_abap_stack
            WHERE mainprogram EQ 'SAPLZSSF_FG' OR
                  mainprogram EQ 'ZCL_ADF_MANAGE_INTERFACE_KEY==CP' OR
                  mainprogram EQ 'ZADF_VALIDATE_AND_ADJUST_KEYS' OR
                  mainprogram EQ 'ZSSF_VALIDATE_AND_ADJUST_KEYS' OR
                ( mainprogram EQ 'ZCL_SSF_UTILITY===============CP' AND blockname EQ 'DECODE_SIGN' ).
      IF sy-subrc = 0.
        lv_allowed = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_allowed NE abap_true.
      RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
        EXPORTING
          textid = zcx_adf_manage_access_keys=>zcx_adf_start_program_no_auth.
    ENDIF.
  ENDMETHOD.


  METHOD decrypt.

    DATA : lt_enveloped_data TYPE TABLE OF ssfbin,
           lv_cert_string    TYPE xstring,
           lt_recipients     TYPE TABLE OF ssfinfo,
           ls_recipient      TYPE ssfinfo,
           lt_input_data     TYPE TABLE OF ssfbin,
           lv_env_data_len   TYPE i,
           lv_env_len_total  TYPE i,
           lv_subject        TYPE string,
           ls_enveloped_data TYPE ssfbin,
           lv_xstr_input     TYPE xstring,
           lv_len_output     TYPE i,
           lv_len_input      TYPE i,
           lt_decoded_bin    TYPE TABLE OF x,
           lv_decoded_str    TYPE string,
           lv_applic         TYPE rfcdisplay-sslapplic,
           lv_psename        TYPE ssfpsename,
           lv_profilename    TYPE localfile,
           lv_profile        TYPE ssfparms-pab.

*    check_debugger( ).
    lt_enveloped_data = it_encrypted_data.

    IF lt_enveloped_data[] IS NOT INITIAL.
      lv_applic = get_decrypt_cert_applic( ).

      CALL METHOD me->get_pse_from_applic
        EXPORTING
          iv_applic  = lv_applic
        IMPORTING
          rv_psename = lv_psename
          rv_profile = lv_profilename.

      lv_cert_string = get_cert_from_pse( iv_psename = lv_psename
                                          iv_profile = lv_profilename ).
      lv_subject = parse_certificate( lv_cert_string ).
      lv_profile = lv_psename.

      ls_recipient-id      = lv_subject.
      ls_recipient-profile = lv_profile.
      APPEND ls_recipient TO lt_recipients.
      CLEAR ls_recipient.
      LOOP AT lt_enveloped_data INTO ls_enveloped_data.
        lv_env_data_len = xstrlen( ls_enveloped_data-bindata ).
        lv_env_len_total = lv_env_len_total + lv_env_data_len.
        CLEAR ls_enveloped_data.
      ENDLOOP.

      CALL FUNCTION 'SSF_KRN_DEVELOPE'
        EXPORTING
          ssftoolkit                   = 'SAPSECULIB'
          str_format                   = 'PKCS7'
*         B_OUTDEC                     = 'X'
*         IO_SPEC                      = 'T'
          ostr_enveloped_data_l        = lv_env_len_total
        IMPORTING
          ostr_output_data_l           = lv_len_input
*         CRC                          =
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
      IF sy-subrc NE 0 OR lt_input_data[] IS INITIAL.
        RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
          EXPORTING
            textid = zcx_adf_manage_access_keys=>zcx_adf_decrypt_error.
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
        IF sy-subrc <> 0 OR lv_decoded_str IS INITIAL.
          RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
            EXPORTING
              textid = zcx_adf_manage_access_keys=>zcx_adf_string_conv_error.
        ENDIF.

        rv_secret = lv_decoded_str.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD encrypt.

    CONSTANTS: lc_e TYPE c VALUE 'E'.

    DATA : lv_rfc_dest            TYPE zrest_config-destination,
           lv_srtfd               TYPE zadf_con_indx-srtfd,
           lw_indx                TYPE zadf_con_indx,
           lt_enveloped_data      TYPE TABLE OF ssfbin,
           lv_cert_string         TYPE xstring,
           lt_recipients          TYPE TABLE OF ssfinfo,
           ls_recipient           TYPE ssfinfo,
           lt_input_data          TYPE TABLE OF ssfbin,
           ls_input_data          TYPE ssfbin,
           lv_env_data_len        TYPE i,
           lv_env_len_out         TYPE i,
           lv_subject             TYPE string,
           lw_enveloped_data      TYPE ssfbin,
           lv_text                TYPE string,
           l_env_str_pab_password TYPE ssfparms-pabpw,
           lv_xstring             TYPE xstring,
           lv_applic              TYPE rfcdisplay-sslapplic,
           lv_psename             TYPE ssfpsename,
           lv_profilename         TYPE localfile,
           lv_profile             TYPE ssfparms-pab.

*    check_debugger( ).
    IF iv_secret IS NOT INITIAL.

      lv_applic = get_encrypt_cert_applic( ).
      CALL METHOD me->get_pse_from_applic
        EXPORTING
          iv_applic  = lv_applic
        IMPORTING
          rv_psename = lv_psename
          rv_profile = lv_profilename.

      lv_cert_string = get_cert_from_pse( iv_psename = lv_psename
                                          iv_profile = lv_profilename ).
      lv_subject = parse_certificate( lv_cert_string ).
      lv_profile = lv_psename.

      ls_recipient-id = lv_subject.
      APPEND ls_recipient TO lt_recipients.

      lv_text = iv_secret.
      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv_text
        IMPORTING
          buffer = lv_xstring
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
          EXPORTING
            textid = zcx_adf_manage_access_keys=>zcx_adf_xstring_conv_error.
      ENDIF.

      IF NOT lv_xstring IS INITIAL.
        lv_env_data_len = xstrlen( lv_xstring ).
        ls_input_data-bindata = lv_xstring.
        APPEND ls_input_data TO lt_input_data.

        l_env_str_pab_password = space.
        CALL FUNCTION 'SSF_KRN_ENVELOPE'
          EXPORTING
            ssftoolkit                   = 'SAPSECULIB'
            str_format                   = 'PKCS7'
            ostr_input_data_l            = lv_env_data_len
            str_pab                      = lv_profile
            str_pab_password             = l_env_str_pab_password
          IMPORTING
            ostr_enveloped_data_l        = lv_env_len_out
          TABLES
            ostr_input_data              = lt_input_data
            recipient_list               = lt_recipients
            ostr_enveloped_data          = lt_enveloped_data
          EXCEPTIONS
            ssf_krn_error                = 1
            ssf_krn_noop                 = 2
            ssf_krn_nomemory             = 3
            ssf_krn_opinv                = 4
            ssf_krn_nossflib             = 5
            ssf_krn_recipient_list_error = 6
            ssf_krn_input_data_error     = 7
            ssf_krn_invalid_par          = 8
            ssf_krn_invalid_parlen       = 9
            ssf_fb_input_parameter_error = 10
            OTHERS                       = 11.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
            EXPORTING
              textid = zcx_adf_manage_access_keys=>zcx_adf_encrypt_error.
        ENDIF.
        rt_encrypted_data = lt_enveloped_data.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_cert_from_pse.

    DATA : lv_profile TYPE ssfpab.

    lv_profile = iv_psename.
    CALL FUNCTION 'SSFC_GET_CERTIFICATE'
      EXPORTING
        profile               = lv_profile
      IMPORTING
        certificate           = rv_cert
      EXCEPTIONS
        ssf_krn_error         = 1
        ssf_krn_nomemory      = 2
        ssf_krn_nossflib      = 3
        ssf_krn_invalid_par   = 4
        ssf_krn_nocertificate = 5
        OTHERS                = 6.
    IF sy-subrc NE 0.
**Adding complete profile path for reading certificate instance
      lv_profile = iv_profile.
      CALL FUNCTION 'SSFC_GET_CERTIFICATE'
        EXPORTING
          profile               = lv_profile
        IMPORTING
          certificate           = rv_cert
        EXCEPTIONS
          ssf_krn_error         = 1
          ssf_krn_nomemory      = 2
          ssf_krn_nossflib      = 3
          ssf_krn_invalid_par   = 4
          ssf_krn_nocertificate = 5
          OTHERS                = 6.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
          EXPORTING
            textid = zcx_adf_manage_access_keys=>zcx_adf_cert_not_found
            iv_pse = iv_psename.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_decrypt_cert_applic.

    IF gv_decrypt_cert_applic IS NOT INITIAL.
      rv_applic = gv_decrypt_cert_applic.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
        EXPORTING
          textid = zcx_adf_manage_access_keys=>zcx_adf_applic_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD get_encrypt_cert_applic.

    IF gv_encrypt_cert_applic IS NOT INITIAL.
      rv_applic = gv_encrypt_cert_applic.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
        EXPORTING
          textid = zcx_adf_manage_access_keys=>zcx_adf_applic_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    CALL METHOD zcl_adf_manage_access_keys=>check_call_stack( ).

    CREATE OBJECT ro_ref.

  ENDMETHOD.


  METHOD get_pse_from_applic.


    CALL FUNCTION 'SSFPSE_FILENAME'
      EXPORTING
        mandt         = sy-mandt
        context       = 'SSLC'
        applic        = iv_applic
      IMPORTING
        psename       = rv_psename
        profile       = rv_profile
      EXCEPTIONS
        pse_not_found = 1
        OTHERS        = 2.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
        EXPORTING
          textid    = zcx_adf_manage_access_keys=>zcx_adf_pse_not_found
          iv_applic = iv_applic.
    ENDIF.

  ENDMETHOD.


  METHOD parse_certificate.

    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
      EXPORTING
        certificate         = iv_cert
      IMPORTING
        subject             = rv_subject
      EXCEPTIONS
        ssf_krn_error       = 1
        ssf_krn_nomemory    = 2
        ssf_krn_nossflib    = 3
        ssf_krn_invalid_par = 4
        OTHERS              = 5.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
        EXPORTING
          textid = zcx_adf_manage_access_keys=>zcx_adf_cert_parse_error.
    ENDIF.

  ENDMETHOD.


  METHOD read_applic_from_destination.
    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = iv_destination
        authority_check         = ' '
      IMPORTING
        sslapplic               = rv_applic
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_adf_manage_access_keys
        EXPORTING
          textid         = zcx_adf_manage_access_keys=>zcx_adf_read_dest_applic_err
          iv_destination = iv_destination.
    ENDIF.
  ENDMETHOD.


  METHOD set_decrypt_cert_applic.

    gv_decrypt_cert_applic = iv_applic.

  ENDMETHOD.


  METHOD set_encrypt_cert_applic.

    gv_encrypt_cert_applic = iv_applic.

  ENDMETHOD.
ENDCLASS.
