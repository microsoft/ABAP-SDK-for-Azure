*----------------------------------------------------------------------*
***INCLUDE LZSSF_FGF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  f_before_save
*&---------------------------------------------------------------------*
*       Validation with DB to restrict any change in KEY
*----------------------------------------------------------------------*
FORM f_before_save.
  TYPES: BEGIN OF lty_zssf,
           interface_id TYPE zssf_data-interface_id,
           zkey         TYPE zssf_data-zkey,
         END OF lty_zssf.
  DATA : lw_extract(4096) TYPE c,
         lt_zssf_data     TYPE STANDARD TABLE OF lty_zssf,
         lw_zssf_data     TYPE lty_zssf.
  IF status-action NE 'A'.
    SELECT interface_id zkey FROM zssf_data
                            INTO TABLE lt_zssf_data.
    IF sy-subrc EQ 0.
      IF NOT extract[] IS INITIAL.
        LOOP AT extract INTO lw_extract.
          CLEAR lw_zssf_data.
          READ TABLE lt_zssf_data INTO lw_zssf_data
                                 WITH KEY interface_id = lw_extract+3(10).
          IF sy-subrc EQ 0.
            IF lw_zssf_data-zkey NE lw_extract+13(255).
              MESSAGE TEXT-004 TYPE 'E'.
            ENDIF.
          ENDIF.
          CLEAR lw_extract.
        ENDLOOP.
      ELSE.
        sy-subrc = 0.
      ENDIF.
    ELSE.
      sy-subrc = 0.
    ENDIF.
  ENDIF.
ENDFORM.                    "f_before_save
*----------------------------------------------------------------------*
* Include Name : LZSSF_FGF01                                           *
*This Include is to encrypt secrets with SSL client ID from STRUST     *
*Framework Author : Krishna Chandra Dash                               *
*                   Sasidhar Puranam                                   *
*                   Pramisha Ladha                                     *
*----------------------------------------------------------------------*
* Date      | USER ID  |Transport   | Remarks                          *
*-----------|----------|------------|------------|---------------------*
*13.10.2017 | KRDASH   |            | Initial Development              *
*06.02.2020 | PRLADHA  |DG2K906097  | Modularization and Backup PSE    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  f_create_entry
*&---------------------------------------------------------------------*
*  Export encrypted data to INDX table
*----------------------------------------------------------------------*
FORM f_create_entry.

  DATA : lo_interface_key TYPE REF TO zcl_ssf_utility,
         lx_interface_key TYPE REF TO zcx_ssf_utility,
         lx_access_key    TYPE REF TO zcx_adf_manage_access_keys,
         lv_key           TYPE string,
         lv_message       TYPE string.

  TRY.

      CREATE OBJECT lo_interface_key.
      lv_key = zssf_data-zkey.
      IF zssf_data-zkey IS NOT INITIAL.
        CALL METHOD lo_interface_key->insert_key
          EXPORTING
            iv_key         = lv_key
            iv_interfaceid = zssf_data-interface_id.

        CLEAR zssf_data-zkey.
        zssf_data-zkey = '*****'.
      ELSE.
        MESSAGE TEXT-001 TYPE 'E'. "Entry required for Key
      ENDIF.
    CATCH zcx_ssf_utility INTO lx_interface_key.
      lv_message = lx_interface_key->get_text( ).
      MESSAGE lv_message TYPE 'E'.
    CATCH zcx_adf_manage_access_keys INTO lx_access_key.
      lv_message = lx_access_key->get_text( ).
      MESSAGE lv_message TYPE 'E'.

  ENDTRY.

ENDFORM.                    "f_create_entry
*&---------------------------------------------------------------------*
*&      Form  F_ENCRYPT_DATA
*&---------------------------------------------------------------------*
*       Encrypt Key
*----------------------------------------------------------------------*
FORM f_encrypt_data  USING pw_zssf_data LIKE zssf_data
                           pw_applic  TYPE strustssl-applic.
  CONSTANTS: lc_e TYPE c VALUE 'E'.
  DATA : lv_srtfd               TYPE zssf_con_indx-srtfd,
         lw_indx                TYPE zssf_con_indx,
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
         lv_profilename         TYPE localfile,
         lv_profile             TYPE ssfparms-pab.
  IF NOT pw_applic IS INITIAL.
    lv_applic = pw_applic.
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
      lv_profile = lv_profilename ."lv_psename.
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
        MESSAGE TEXT-007 TYPE lc_e.
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
        MESSAGE TEXT-008 TYPE lc_e.
      ENDIF.
      lw_recipient-id = lv_subject.
      APPEND lw_recipient TO lt_recipients.
      CLEAR lw_recipient.
      lv_text = pw_zssf_data-zkey.
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
            lv_srtfd = pw_zssf_data-interface_id.
            EXPORT tab = lt_enveloped_data[]
            TO DATABASE zssf_con_indx(ze)
            FROM lw_indx
            ID lv_srtfd.
            IF sy-subrc EQ 0.
              CLEAR zssf_data-zkey.
              zssf_data-zkey = '*****'.
            ELSE.
              MESSAGE TEXT-011 TYPE lc_e.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_ENCRYPT_DATA
