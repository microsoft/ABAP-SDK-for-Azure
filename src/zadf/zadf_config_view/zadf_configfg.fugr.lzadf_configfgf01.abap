*----------------------------------------------------------------------*
***INCLUDE LZADF_V_CONFIGFGF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_ENCRYPT_DATA
*&---------------------------------------------------------------------*
*       Encrypt SAS Key
*----------------------------------------------------------------------*
FORM f_encrypt_data USING pw_zrest LIKE zrest_config
                          p_sas_key TYPE string.
  CONSTANTS: lc_e     TYPE c VALUE 'E',
             lc_zadfp TYPE ssfapplssl VALUE 'ZADFP'.
  DATA : lv_rfc_dest            TYPE zrest_config-destination,
         lv_srtfd               TYPE zadf_con_indx-srtfd,
         lw_indx                TYPE zadf_con_indx,
         lt_enveloped_data      TYPE TABLE OF ssfbin,
         lv_cert_string         TYPE xstring,
         lt_recipients          TYPE TABLE OF ssfinfo,
         lw_recipient           TYPE ssfinfo,
         lt_input_data          TYPE TABLE OF ssfbin,
         lw_input_data          TYPE ssfbin,
         lv_env_data_len        TYPE i,
         lv_env_len_out         TYPE i,
         lv_subject             TYPE string,
         lw_enveloped_data      TYPE ssfbin,
         lv_text                TYPE string,
         l_env_str_pab_password TYPE ssfparms-pabpw,
         lv_xstring             TYPE xstring,
         lv_applic              TYPE rfcdisplay-sslapplic,
         lv_psename             TYPE ssfpsename,
**Start of changes by KRDASH MS2K960975
         lv_profilename         TYPE localfile,
**End of chnages by KRDASH MS2K960975
         lv_profile             TYPE ssfparms-pab.
  IF NOT p_sas_key IS INITIAL.
    lv_applic = lc_zadfp. "Encrypting using primary certificate
    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = pw_zrest-destination
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
      MESSAGE TEXT-005 TYPE lc_e.
    ENDIF.
    IF NOT lv_applic IS INITIAL.
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
        MESSAGE TEXT-006 TYPE lc_e.
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
**Start of changes by KRDASH MS2K960975
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
**End of changes by KRDASH MS2K960975
            MESSAGE TEXT-007 TYPE lc_e.
          ENDIF.
        ENDIF. "Added by KRDASH MS2K960975
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
          MESSAGE TEXT-008 TYPE lc_e.
        ENDIF.
        lw_recipient-id = lv_subject.
        APPEND lw_recipient TO lt_recipients.
        CLEAR lw_recipient.
        lv_text = p_sas_key.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = lv_text
          IMPORTING
            buffer = lv_xstring
          EXCEPTIONS
            failed = 1
            OTHERS = 2.
        IF sy-subrc NE 0.
          MESSAGE TEXT-009 TYPE lc_e.
        ENDIF.
        IF NOT lv_xstring IS INITIAL.
          lv_env_data_len = xstrlen( lv_xstring ).
          lw_input_data-bindata = lv_xstring.
          APPEND lw_input_data TO lt_input_data.
          CLEAR lw_input_data.
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
            MESSAGE TEXT-010 TYPE lc_e.
          ELSE.
            IF NOT lt_enveloped_data[] IS INITIAL.
              CLEAR lw_indx.
              lw_indx-aedat = sy-datum.
              lw_indx-usera = sy-uname.
              lw_indx-pgmid = sy-repid.
              lv_srtfd = pw_zrest-interface_id.
              EXPORT tab = lt_enveloped_data[]
              TO DATABASE zadf_con_indx(zd)
              FROM lw_indx
              ID lv_srtfd.
              IF sy-subrc EQ 0.
                CLEAR zadf_v_config-sas_key.
                zadf_v_config-sas_key = '*****'.
              ELSE.
                MESSAGE TEXT-011 TYPE lc_e.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*    MESSAGE  text-003 TYPE lc_e. "Commented by KRDASH
  ENDIF.
ENDFORM.                    " F_ENCRYPT_DATA
*&---------------------------------------------------------------------*
*&      Form  f_create_entry
*&---------------------------------------------------------------------*
*  Export encrypted data to INDX table
*----------------------------------------------------------------------*
FORM f_create_entry.

  DATA : lo_interface_key TYPE REF TO zcl_adf_manage_interface_key,
         lx_interface_key TYPE REF TO zcx_adf_manage_interface_key,
         lx_access_key    TYPE REF TO zcx_adf_manage_access_keys,
         lv_key           TYPE string,
         lv_message       TYPE string.

  TRY.
      CREATE OBJECT lo_interface_key
        EXPORTING
          iv_interface_id = zadf_v_config-interface_id.

      lv_key = zadf_v_config-sas_key.
      IF lv_key IS NOT INITIAL.
        lo_interface_key->insert_key( lv_key ).
        CLEAR zadf_v_config-sas_key.
        zadf_v_config-sas_key = '*****'.
      ENDIF.

    CATCH zcx_adf_manage_interface_key INTO lx_interface_key.
      lv_message = lx_interface_key->get_text( ).
      MESSAGE lv_message TYPE 'E'.
    CATCH zcx_adf_manage_access_keys INTO lx_access_key.
      lv_message = lx_access_key->get_text( ).
      MESSAGE lv_message TYPE 'E'.

  ENDTRY.

ENDFORM.                    "f_create_entry
*&---------------------------------------------------------------------*
*&      Form  f_before_save
*&---------------------------------------------------------------------*
*       Validation with DB to restrict any change in SAS_KEY
*----------------------------------------------------------------------*
FORM f_before_save.
  RETURN.
ENDFORM.                    "f_before_save

FORM f_change_entry.
  DATA : lv_key TYPE string.

  IF status-action EQ 'U'.
* Fetch the details from ZADF_V_CONFIG table to identify if the change in SAS Key is required
    SELECT interface_id,
           sas_key
           FROM zadf_config
           INTO TABLE @DATA(lt_zadf_v_config)
           WHERE interface_id = @zadf_v_config-interface_id.
* Check if event is triggered for SAS Key update
    IF sy-subrc = 0 AND line_exists( lt_zadf_v_config[ 1 ] ) AND lt_zadf_v_config[ 1 ]-sas_key NE zadf_v_config-sas_key.
      lv_key = zadf_v_config-sas_key.
    ELSE.
** No Key update during modification of the rest of fields
      CLEAR lv_key.
    ENDIF.

    TRY.
* Update the SAS Key
        IF lv_key IS NOT INITIAL.
          DATA(lo_interface_key) = NEW  zcl_adf_manage_interface_key( zadf_v_config-interface_id ).
          lo_interface_key->insert_key( lv_key ).
          CLEAR zadf_v_config-sas_key.
          zadf_v_config-sas_key = '*****'.
        ENDIF.
      CATCH zcx_adf_manage_interface_key INTO DATA(lx_interface_key).
        DATA(lv_message) = lx_interface_key->get_text( ).
        MESSAGE lv_message TYPE 'E'.
      CATCH zcx_adf_manage_access_keys INTO DATA(lx_access_key).
        lv_message = lx_access_key->get_text( ).
        MESSAGE lv_message TYPE 'E'.
    ENDTRY.
  ENDIF.
ENDFORM.
