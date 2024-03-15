class ZCL_SSF_UTILITY definition
  public
  final
  create public .

public section.

  class-data GC_SECONDARY_APPLIC type SSFAPPLSSL value 'ZADFS' ##NO_TEXT.
  class-data GC_PRIMARY_APPLIC type SSFAPPLSSL value 'ZADFP' ##NO_TEXT.

  methods DECODE_SIGN
    importing
      value(IV_INTERFACEID) type ZINTERFACE_ID
    returning
      value(RV_KEY) type STRING
    raising
      ZCX_SSF_UTILITY .
  methods READ_KEY
    importing
      value(IV_INTERFACEID) type ZINTERFACE_ID
    returning
      value(RV_KEY) type STRING .
  methods INSERT_KEY
    importing
      value(IV_KEY) type STRING
      value(IV_INTERFACEID) type ZINTERFACE_ID
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_SSF_UTILITY .
protected section.
private section.

  data GV_INTERFACE_ID type ZINTERFACE_ID .
  data GO_ACCESS_KEY type ref to ZCL_ADF_MANAGE_ACCESS_KEYS .
  data GS_ADF_CONFIG type ZADF_CONFIG .
  data GS_SSF_DATA type ZSSF_DATA .
  data GV_NEW type BOOLEAN .
  data GV_APPLIC type SSFAPPLSSL .

  methods SET_GLOBAL_PARAMETERS
    importing
      value(IV_INTERFACEID) type ZINTERFACE_ID .
  methods READ_KEY_WITH_PRIMARY
    returning
      value(RV_KEY) type STRING .
  methods READ_KEY_WITH_SECONDARY
    returning
      value(RV_KEY) type STRING .
  methods READ_KEY_WITH_DEFAULT_PSE
    returning
      value(RV_KEY) type STRING .
  methods GET_APPLIC_FROM_ZTVARVC .
  methods INSERT_KEY_WITH_PRIMARY
    importing
      !IV_KEY type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_SSF_UTILITY .
  methods INSERT_KEY_WITH_SECONDARY
    importing
      value(IV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_SSF_UTILITY .
  methods INSERT_KEY_WITH_DEFAULT_PSE
    importing
      value(IV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_SSF_UTILITY .
ENDCLASS.



CLASS ZCL_SSF_UTILITY IMPLEMENTATION.


METHOD decode_sign.
**----------------------------------------------------------------------*
**This method is to decode secrets with SSL client ID from STRUST       *
**Framework Author : Krishna Chandra Dash                               *
**                   Sasidhar puranam                                   *
**                   Pramisha Ladha                                     *
**----------------------------------------------------------------------*
** Date      | USER ID  |Transport   | Remarks                          *
**-----------|----------|------------|------------|---------------------*
**13.10.2017 | KRDASH   |            | Initial Development              *
**11.02.2019 | PRLADHA  |            | Adding Primary and Sec PSEs      *
**----------------------------------------------------------------------*
  CONSTANTS : lc_profile TYPE ztvarvc-varname VALUE 'SSL_CLIENT_ID'.
  DATA : lv_srtfd          TYPE zssf_con_indx-srtfd,
         lw_indx           TYPE zssf_con_indx,
         lt_enveloped_data TYPE TABLE OF ssfbin,
         lv_cert_string    TYPE xstring,
         lt_recipients     TYPE TABLE OF ssfinfo,
         lw_recipient      TYPE ssfinfo,
         lt_input_data     TYPE TABLE OF ssfbin,
         lw_input_data     TYPE ssfbin,
         lv_env_data_len   TYPE i,
         lv_env_len_total  TYPE i,
         lv_subject        TYPE string,
         lw_enveloped_data TYPE ssfbin,
         lv_xstr_input     TYPE xstring,
         lv_len_output     TYPE i,
         lv_len_input      TYPE i,
         lt_decoded_bin    TYPE TABLE OF x,
         lv_decoded_str    TYPE string,
         lv_applic         TYPE rfcdisplay-sslapplic,
         lv_psename        TYPE ssfpsename,
         lv_profilename    TYPE localfile,
         lv_profile        TYPE ssfparms-pab.
  DATA: t_abap_stack  TYPE  abap_callstack,
        t_sys_stack   TYPE  sys_callst,
        lx_access_key TYPE REF TO zcx_adf_manage_access_keys,
        lv_msg        TYPE string.
*
  DEFINE decode.
    TRY.
      rv_key = read_key( iv_interfaceid ).
    CATCH zcx_adf_manage_access_keys INTO lx_access_key.
      lv_msg = lx_access_key->get_text( ).
        RAISE EXCEPTION TYPE zcx_ssf_utility
          EXPORTING
            textid = zcx_ssf_utility=>zcx_adf_manage_key_exception
            text   = lv_msg.
    ENDTRY.
*
*    lv_srtfd = iv_interfaceid.
**Import internal table as a cluster from INDX
*    import tab  = lt_enveloped_data[]
*           from database zssf_con_indx(ze)
*           to lw_indx
*           id lv_srtfd.
*    if not lt_enveloped_data[] is initial.
*      clear lv_applic.
*      select single low  from ztvarvc
*                         into lv_applic
*                         where varname eq lc_profile.
*      if ( sy-subrc eq 0 ) and ( not lv_applic is initial ) .
*        call function 'SSFPSE_FILENAME'
*          exporting
*            mandt         = sy-mandt
*            context       = 'SSLC'
*            applic        = lv_applic
*          importing
*            psename       = lv_psename
*            profile       = lv_profilename
*          exceptions
*            pse_not_found = 1
*            others        = 2.
*        if sy-subrc ne 0.
*          raise exception type zcx_ssf_utility
*            exporting
*              textid       = zcx_ssf_utility=>read_error_pse_filename
*              interface_id = iv_interfaceid.
*        endif.
*        if not lv_psename is initial.
*          lv_profile = lv_profilename. "lv_psename.
*          call function 'SSFC_GET_CERTIFICATE'
*            exporting
*              profile               = lv_profile
*            importing
*              certificate           = lv_cert_string
*            exceptions
*              ssf_krn_error         = 1
*              ssf_krn_nomemory      = 2
*              ssf_krn_nossflib      = 3
*              ssf_krn_invalid_par   = 4
*              ssf_krn_nocertificate = 5
*              others                = 6.
*          if sy-subrc ne 0.
***Raise Exception
*            raise exception type zcx_ssf_utility
*              exporting
*                textid       = zcx_ssf_utility=>error_get_certificate_instance
*                interface_id = iv_interfaceid.
*          endif.
*          call function 'SSFC_PARSE_CERTIFICATE'
*            exporting
*              certificate         = lv_cert_string
*            importing
*              subject             = lv_subject
*            exceptions
*              ssf_krn_error       = 1
*              ssf_krn_nomemory    = 2
*              ssf_krn_nossflib    = 3
*              ssf_krn_invalid_par = 4
*              others              = 5.
*          if sy-subrc ne 0.
***Raise Exception
*            raise exception type zcx_ssf_utility
*              exporting
*                textid       = zcx_ssf_utility=>error_attributes_certificate
*                interface_id = iv_interfaceid.
*          endif.
*          lw_recipient-id      = lv_subject.
*          lw_recipient-profile = lv_profile.
*          append lw_recipient to lt_recipients.
*          loop at lt_enveloped_data into lw_enveloped_data.
*            lv_env_data_len = xstrlen( lw_enveloped_data-bindata ).
*            lv_env_len_total = lv_env_len_total + lv_env_data_len.
*            clear lw_enveloped_data.
*          endloop.
*          call function 'SSF_KRN_DEVELOPE'
*            exporting
*              ssftoolkit                   = 'SAPSECULIB'
*              str_format                   = 'PKCS7'
**           B_OUTDEC                     = 'X'
**           IO_SPEC                      = 'T'
*              ostr_enveloped_data_l        = lv_env_len_total
*            importing
*              ostr_output_data_l           = lv_len_input
**           CRC                          =
*            tables
*              ostr_enveloped_data          = lt_enveloped_data
*              recipient                    = lt_recipients
*              ostr_output_data             = lt_input_data
*            exceptions
*              ssf_krn_error                = 1
*              ssf_krn_noop                 = 2
*              ssf_krn_nomemory             = 3
*              ssf_krn_opinv                = 4
*              ssf_krn_nossflib             = 5
*              ssf_krn_recipient_error      = 6
*              ssf_krn_input_data_error     = 7
*              ssf_krn_invalid_par          = 8
*              ssf_krn_invalid_parlen       = 9
*              ssf_fb_input_parameter_error = 10
*              others                       = 11.
*          if sy-subrc ne 0.
***Raise Exception
*            raise exception type zcx_ssf_utility
*              exporting
*                textid       = zcx_ssf_utility=>error_decode_key
*                interface_id = iv_interfaceid.
*          endif.
*          if not lt_input_data[] is initial.
*            call function 'SCMS_BINARY_TO_STRING'
*              exporting
*                input_length  = lv_len_input
*              importing
*                text_buffer   = lv_decoded_str
*                output_length = lv_len_output
*              tables
*                binary_tab    = lt_input_data
*              exceptions
*                failed        = 1
*                others        = 2.
*            if lv_decoded_str is initial.
***Raise exception
*              raise exception type zcx_ssf_utility
*                exporting
*                  textid       = zcx_ssf_utility=>error_con_key_string
*                  interface_id = iv_interfaceid.
*            else.
*              rv_key = lv_decoded_str.
*            endif.
*          else.
***Raise exception
*            raise exception type zcx_ssf_utility
*              exporting
*                textid       = zcx_ssf_utility=>error_read_encoded_key
*                interface_id = iv_interfaceid.
*          endif.
*        endif.
*      else.
*        raise exception type zcx_ssf_utility
*          exporting
*            textid       = zcx_ssf_utility=>ssl_clientid_not_maintained
*            interface_id = iv_interfaceid.
*      endif.
*    else.
***Raise Exception
*      raise exception type zcx_ssf_utility
*        exporting
*          textid       = zcx_ssf_utility=>error_import_key
*          interface_id = iv_interfaceid.
*    endif.
    IF rv_key IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ssf_utility
        EXPORTING
          textid       = zcx_ssf_utility=>key_not_maintained
          interface_id = iv_interfaceid.
    ENDIF.
  END-OF-DEFINITION.
**Authority Check To be done
****************************************************************************
**                       Begin of Authority check                          *
****************************************************************************
  DATA : it_registered_programs TYPE STANDARD TABLE OF zregister,
         wa_register            TYPE zregister,
         lv_found               TYPE abap_bool.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
*      EXPORTING
*        max_level = '2'
    IMPORTING
      callstack    = t_abap_stack
      et_callstack = t_sys_stack.
*   Get the registered programs from table in SAP.
*  SELECT * FROM zregister INTO TABLE it_registered_programs.
*  CLEAR wa_register.
*  READ TABLE it_registered_programs INTO wa_register
*                                    WITH KEY objnam        = sy-cprog "Current calling main program
*                                             interface_id  = iv_interfaceid. "Interface Id
  CLEAR wa_register.
  SELECT SINGLE * FROM zregister
                  INTO wa_register
                  WHERE objnam EQ sy-cprog "Current calling main program
                  AND   interface_id EQ iv_interfaceid. "Interface Id
  IF sy-subrc EQ 0.
    READ TABLE t_abap_stack TRANSPORTING NO FIELDS
                            WITH KEY mainprogram = wa_register-objnam. "Current calling main program
    IF sy-subrc EQ 0.
      lv_found = abap_true.
    ENDIF.
  ENDIF.
  IF lv_found EQ abap_false.
    RAISE EXCEPTION TYPE zcx_ssf_utility
      EXPORTING
        textid       = zcx_ssf_utility=>program_not_authorized
        interface_id = iv_interfaceid.
  ENDIF.
***************************************************************************
*                       End of Authroity check                            *
***************************************************************************
**Calling macro to decrypt key with SSL client ID
  decode.
ENDMETHOD.


  METHOD get_applic_from_ztvarvc.
    DATA : lv_applic         TYPE ssfapplssl.

    CONSTANTS: lc_profile TYPE ztvarvc-varname VALUE 'SSL_CLIENT_ID'.

    SELECT SINGLE low FROM ztvarvc
                  INTO lv_applic
                  WHERE varname EQ lc_profile.
    IF sy-subrc = 0.
      gv_applic = lv_applic.
    ENDIF.
  ENDMETHOD.


  METHOD insert_key.
    DATA: lv_switch TYPE char1.
    CONSTANTS : lc_switch TYPE ztvarvc-varname VALUE 'ZADF_NEW_PSE_SWITCH'.
    gv_interface_id = iv_interfaceid.
    go_access_key = zcl_adf_manage_access_keys=>get_instance( ).

    SELECT SINGLE low  FROM ztvarvc
                          INTO lv_switch
                          WHERE varname EQ lc_switch.
    IF ( sy-subrc EQ 0 ) AND ( lv_switch EQ 'X' ).
      insert_key_with_primary( iv_key ).
      insert_key_with_secondary( iv_key ).
    ELSE.
      insert_key_with_default_pse( iv_key ).
    ENDIF.
  ENDMETHOD.


  METHOD insert_key_with_default_pse.
    DATA : lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zssf_con_indx-srtfd,
           lv_indx           TYPE zssf_con_indx.

    get_applic_from_ztvarvc( ).
    go_access_key->set_encrypt_cert_applic( gv_applic ).
    lt_enveloped_data = go_access_key->encrypt( iv_key ).

    IF NOT lt_enveloped_data[] IS INITIAL.

      lv_indx-aedat = sy-datum.
      lv_indx-usera = sy-uname.
      lv_indx-pgmid = sy-repid.
      lv_srtfd = gv_interface_id.

      EXPORT tab = lt_enveloped_data[]
      TO DATABASE zssf_con_indx(ze)
      FROM lv_indx
      ID lv_srtfd.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_ssf_utility
          EXPORTING
            textid       = zcx_ssf_utility=>zcx_ssf_con_update_failed
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD insert_key_with_primary.
    DATA : lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zssf_con_indx-srtfd,
           lv_indx           TYPE zssf_con_indx.

    go_access_key->set_encrypt_cert_applic( gc_primary_applic ).
    lt_enveloped_data = go_access_key->encrypt( iv_key ).

    IF NOT lt_enveloped_data[] IS INITIAL.

      lv_indx-aedat = sy-datum.
      lv_indx-usera = sy-uname.
      lv_indx-pgmid = sy-repid.
      lv_srtfd = gv_interface_id.

      EXPORT tab = lt_enveloped_data[]
      TO DATABASE zssf_con_indx(pr)
      FROM lv_indx
      ID lv_srtfd.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_ssf_utility
          EXPORTING
            textid       = zcx_ssf_utility=>zcx_ssf_con_update_failed
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD insert_key_with_secondary.
    DATA : lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zssf_con_indx-srtfd,
           lv_indx           TYPE zssf_con_indx.

    go_access_key->set_encrypt_cert_applic( gc_secondary_applic ).
    lt_enveloped_data = go_access_key->encrypt( iv_key ).

    IF NOT lt_enveloped_data[] IS INITIAL.

      lv_indx-aedat = sy-datum.
      lv_indx-usera = sy-uname.
      lv_indx-pgmid = sy-repid.
      lv_srtfd = gv_interface_id.

      EXPORT tab = lt_enveloped_data[]
      TO DATABASE zssf_con_indx(bu)
      FROM lv_indx
      ID lv_srtfd.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_ssf_utility
          EXPORTING
            textid       = zcx_ssf_utility=>zcx_ssf_con_update_failed
            interface_id = gv_interface_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD read_key.

    DATA : lx_ref    TYPE REF TO zcx_ssf_utility,
           lv_switch TYPE char1.

    CONSTANTS : lc_switch TYPE ztvarvc-varname VALUE 'ZADF_NEW_PSE_SWITCH'.

    set_global_parameters( iv_interfaceid ).

    SELECT SINGLE low  FROM ztvarvc
                          INTO lv_switch
                          WHERE varname EQ lc_switch.
    IF ( sy-subrc EQ 0 ) AND ( lv_switch EQ 'X' ).
      TRY.
          rv_key = read_key_with_primary( ).
        CATCH zcx_ssf_utility INTO lx_ref.
          rv_key = read_key_with_secondary( ).
      ENDTRY.
    ELSE.
      rv_key = read_key_with_default_pse( ).
    ENDIF.

  ENDMETHOD.


  METHOD read_key_with_default_pse.
    DATA : lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zssf_con_indx-srtfd,
           lv_indx           TYPE zssf_con_indx.

    lv_srtfd = gs_ssf_data-interface_id.
    IMPORT tab  = lt_enveloped_data[]
      FROM DATABASE zssf_con_indx(ze)
      TO lv_indx
      ID lv_srtfd.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ssf_utility
        EXPORTING
          textid       = zcx_ssf_utility=>zcx_ssf_import_failed
          interface_id = gs_ssf_data-interface_id.
    ENDIF.
    get_applic_from_ztvarvc( ).
    IF gv_applic IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ssf_utility
        EXPORTING
          textid       = zcx_ssf_utility=>read_error_pse_filename
          interface_id = gv_interface_id.
    ENDIF.
    go_access_key->set_decrypt_cert_applic( gv_applic ).
    rv_key = go_access_key->decrypt( lt_enveloped_data ).
  ENDMETHOD.


  METHOD read_key_with_primary.
    DATA : lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zssf_con_indx-srtfd,
           lv_indx           TYPE zssf_con_indx.

    lv_srtfd = gs_ssf_data-interface_id.
    IMPORT tab  = lt_enveloped_data[]
      FROM DATABASE zssf_con_indx(pr)
      TO lv_indx
      ID lv_srtfd.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ssf_utility
        EXPORTING
          textid       = zcx_ssf_utility=>zcx_ssf_import_failed
          interface_id = gs_ssf_data-interface_id.
    ENDIF.

    go_access_key->set_decrypt_cert_applic( gc_primary_applic ).
    rv_key = go_access_key->decrypt( lt_enveloped_data ).
  ENDMETHOD.


  METHOD read_key_with_secondary.
    DATA : lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zssf_con_indx-srtfd,
           lv_indx           TYPE zssf_con_indx.

    lv_srtfd = gs_ssf_data-interface_id.
    IMPORT tab  = lt_enveloped_data[]
      FROM DATABASE zssf_con_indx(bu)
      TO lv_indx
      ID lv_srtfd.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ssf_utility
        EXPORTING
          textid       = zcx_ssf_utility=>zcx_ssf_import_failed
          interface_id = gs_ssf_data-interface_id.
    ENDIF.

    go_access_key->set_decrypt_cert_applic( gc_secondary_applic ).
    rv_key = go_access_key->decrypt( lt_enveloped_data ).
  ENDMETHOD.


  METHOD set_global_parameters.
    gv_interface_id = iv_interfaceid.
*  gv_new = iv_new.
    go_access_key = zcl_adf_manage_access_keys=>get_instance( ).

    SELECT SINGLE * FROM zssf_data INTO gs_ssf_data
      WHERE interface_id = gv_interface_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ssf_utility
        EXPORTING
          textid       = zcx_ssf_utility=>zcx_ssf_config_not_found
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
