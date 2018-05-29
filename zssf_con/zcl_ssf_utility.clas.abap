class ZCL_SSF_UTILITY definition
  public
  final
  create public .

public section.

  methods DECODE_SIGN
    importing
      value(IV_INTERFACEID) type ZINTERFACE_ID
    returning
      value(RV_KEY) type STRING
    raising
      ZCX_SSF_UTILITY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SSF_UTILITY IMPLEMENTATION.


METHOD decode_sign.
*----------------------------------------------------------------------*
*This method is to decode secrets with SSL client ID from STRUST       *
*Framework Author : Krishna Chandra Dash                               *
*                   Sasidhar puranam                                   *
*----------------------------------------------------------------------*
* Date      | USER ID  |Transport   | Remarks                          *
*-----------|----------|------------|------------|---------------------*
*13.10.2017 | KRDASH   |            | Initial Development              *
*----------------------------------------------------------------------*
  CONSTANTS : lc_profile TYPE tvarvc-name VALUE 'SSL_CLIENT_ID'.
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
  DATA: t_abap_stack TYPE  abap_callstack,
        t_sys_stack  TYPE  sys_callst.

  DEFINE decode.

    lv_srtfd = iv_interfaceid.
*Import internal table as a cluster from INDX
    import tab  = lt_enveloped_data[]
           from database zssf_con_indx(ze)
           to lw_indx
           id lv_srtfd.
    if not lt_enveloped_data[] is initial.
      clear lv_applic.
      select single low  from tvarvc
                         into lv_applic
                         where name eq lc_profile.
      if ( sy-subrc eq 0 ) and ( not lv_applic is initial ) .
        call function 'SSFPSE_FILENAME'
          exporting
            mandt         = sy-mandt
            context       = 'SSLC'
            applic        = lv_applic
          importing
            psename       = lv_psename
            profile       = lv_profilename
          exceptions
            pse_not_found = 1
            others        = 2.
        if sy-subrc ne 0.
          raise exception type zcx_ssf_utility
            exporting
              textid       = zcx_ssf_utility=>read_error_pse_filename
              interface_id = iv_interfaceid.
        endif.
        else.
         call function 'SSFPSE_FILENAME'
          exporting
            mandt         = sy-mandt
            context       = 'SSLC'
            applic        = 'DFAULT'
          importing
            psename       = lv_psename
            profile       = lv_profilename
          exceptions
            pse_not_found = 1
            others        = 2.
        if sy-subrc ne 0.
          raise exception type zcx_ssf_utility
            exporting
              textid       = zcx_ssf_utility=>read_error_pse_filename
              interface_id = iv_interfaceid.
        endif.

        endif.
        if not lv_psename is initial.
          lv_profile = lv_profilename. "lv_psename.
          call function 'SSFC_GET_CERTIFICATE'
            exporting
              profile               = lv_profile
            importing
              certificate           = lv_cert_string
            exceptions
              ssf_krn_error         = 1
              ssf_krn_nomemory      = 2
              ssf_krn_nossflib      = 3
              ssf_krn_invalid_par   = 4
              ssf_krn_nocertificate = 5
              others                = 6.
          if sy-subrc ne 0.
**Raise Exception
            raise exception type zcx_ssf_utility
              exporting
                textid       = zcx_ssf_utility=>error_get_certificate_instance
                interface_id = iv_interfaceid.
          endif.
          call function 'SSFC_PARSE_CERTIFICATE'
            exporting
              certificate         = lv_cert_string
            importing
              subject             = lv_subject
            exceptions
              ssf_krn_error       = 1
              ssf_krn_nomemory    = 2
              ssf_krn_nossflib    = 3
              ssf_krn_invalid_par = 4
              others              = 5.
          if sy-subrc ne 0.
**Raise Exception
            raise exception type zcx_ssf_utility
              exporting
                textid       = zcx_ssf_utility=>error_attributes_certificate
                interface_id = iv_interfaceid.
          endif.
          lw_recipient-id      = lv_subject.
          lw_recipient-profile = lv_profile.
          append lw_recipient to lt_recipients.
          loop at lt_enveloped_data into lw_enveloped_data.
            lv_env_data_len = xstrlen( lw_enveloped_data-bindata ).
            lv_env_len_total = lv_env_len_total + lv_env_data_len.
            clear lw_enveloped_data.
          endloop.
          call function 'SSF_KRN_DEVELOPE'
            exporting
              ssftoolkit                   = 'SAPSECULIB'
              str_format                   = 'PKCS7'
*           B_OUTDEC                     = 'X'
*           IO_SPEC                      = 'T'
              ostr_enveloped_data_l        = lv_env_len_total
            importing
              ostr_output_data_l           = lv_len_input
*           CRC                          =
            tables
              ostr_enveloped_data          = lt_enveloped_data
              recipient                    = lt_recipients
              ostr_output_data             = lt_input_data
            exceptions
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
              others                       = 11.
          if sy-subrc ne 0.
**Raise Exception
            raise exception type zcx_ssf_utility
              exporting
                textid       = zcx_ssf_utility=>error_decode_key
                interface_id = iv_interfaceid.
          endif.
          if not lt_input_data[] is initial.
            call function 'SCMS_BINARY_TO_STRING'
              exporting
                input_length  = lv_len_input
              importing
                text_buffer   = lv_decoded_str
                output_length = lv_len_output
              tables
                binary_tab    = lt_input_data
              exceptions
                failed        = 1
                others        = 2.
            if lv_decoded_str is initial.
**Raise exception
              raise exception type zcx_ssf_utility
                exporting
                  textid       = zcx_ssf_utility=>error_con_key_string
                  interface_id = iv_interfaceid.
            else.
              rv_key = lv_decoded_str.
            endif.
          else.
**Raise exception
            raise exception type zcx_ssf_utility
              exporting
                textid       = zcx_ssf_utility=>error_read_encoded_key
                interface_id = iv_interfaceid.
          endif.
        endif.
*      else.
*        raise exception type zcx_ssf_utility
*          exporting
*            textid       = zcx_ssf_utility=>ssl_clientid_not_maintained
*            interface_id = iv_interfaceid.
*      endif.
    else.
**Raise Exception
      raise exception type zcx_ssf_utility
        exporting
          textid       = zcx_ssf_utility=>error_import_key
          interface_id = iv_interfaceid.
    endif.
    if rv_key is initial.
      raise exception type zcx_ssf_utility
        exporting
          textid       = zcx_ssf_utility=>key_not_maintained
          interface_id = iv_interfaceid.
    endif.
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
ENDCLASS.
