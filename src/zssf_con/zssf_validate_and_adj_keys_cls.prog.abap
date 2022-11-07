*&---------------------------------------------------------------------*
*&  Include           ZSSF_VALIDATE_AND_ADJ_KEYS_CLS
*&---------------------------------------------------------------------*
CLASS lcl_ssf_validate_and_adj_keys DEFINITION.

  PUBLIC SECTION.

    METHODS constructor .
    METHODS read_config_tables .
    METHODS check_key
      IMPORTING
        VALUE(iv_applic)       TYPE ssfapplssl
        VALUE(iv_interface_id) TYPE zinterface_id .
    METHODS check_access_keys .
    METHODS adjust_access_keys .
    METHODS adjust_key
      IMPORTING
        VALUE(iv_applic)       TYPE ssfapplssl
        VALUE(iv_interface_id) TYPE zinterface_id .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS lcl_ssf_validate_and_adj_keys IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADF_VALIDATE_AND_ADJ_KEYS->ADJUST_ACCESS_KEYS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD adjust_access_keys.
    DATA : lx_access_keys TYPE REF TO zcx_adf_manage_access_keys.

    LOOP AT lt_ssf_data INTO ls_ssf_data.

      CALL METHOD me->adjust_key
        EXPORTING
          iv_applic       = gv_applic
          iv_interface_id = ls_ssf_data-interface_id.

      WRITE : / '--------------------------------------------------------------------------'.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADF_VALIDATE_AND_ADJ_KEYS->ADJUST_KEY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_APPLIC                      TYPE        SSFAPPLSSL
* | [--->] IV_INTERFACE_ID                TYPE        ZINTERFACE_ID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD adjust_key.
    DATA : lx_ref            TYPE REF TO zcx_adf_manage_access_keys,
           lv_key            TYPE string,
           lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zssf_con_indx-srtfd,
           lv_indx           TYPE zssf_con_indx,
           lv_error_msg      TYPE string.

    CLEAR lv_message.
    lv_srtfd = ls_ssf_data-interface_id.
    IF p_ft EQ abap_false.
      IF p_prim EQ abap_true.
        IMPORT tab  = lt_enveloped_data[]
          FROM DATABASE zssf_con_indx(bu)
          TO lv_indx
          ID lv_srtfd.
        IF sy-subrc <> 0.
          CONCATENATE TEXT-003 iv_interface_id TEXT-010 INTO lv_message
                      SEPARATED BY space.
          WRITE : / lv_message COLOR COL_NEGATIVE.
        ELSE.
          TRY.
              lo_keys->set_decrypt_cert_applic( zcl_ssf_utility=>gc_secondary_applic ).
              lv_key = lo_keys->decrypt( lt_enveloped_data ).

              lo_keys->set_encrypt_cert_applic( zcl_ssf_utility=>gc_primary_applic ). "iv_applic ).
              lt_enveloped_data = lo_keys->encrypt( lv_key ).

              IF NOT lt_enveloped_data[] IS INITIAL.
                CLEAR lv_indx.
                lv_indx-aedat = sy-datum.
                lv_indx-usera = sy-uname.
                lv_indx-pgmid = sy-repid.
                lv_srtfd = iv_interface_id.
                EXPORT tab = lt_enveloped_data[]
                TO DATABASE zssf_con_indx(pr)
                FROM lv_indx
                ID lv_srtfd.
              ENDIF.

              CONCATENATE TEXT-003 iv_interface_id TEXT-008 INTO lv_message
                          SEPARATED BY space.
              WRITE : / lv_message COLOR COL_POSITIVE.

            CATCH zcx_adf_manage_access_keys INTO lx_ref.
              lv_error_msg = lx_ref->get_text( ).
              CONCATENATE TEXT-003 iv_interface_id TEXT-010 INTO lv_message
                          SEPARATED BY space.
              WRITE : / lv_message COLOR COL_NEGATIVE.
              WRITE : /  lv_error_msg COLOR COL_NEGATIVE.
          ENDTRY.
        ENDIF.

      ELSEIF p_sec EQ abap_true.
        IMPORT tab  = lt_enveloped_data[]
          FROM DATABASE zssf_con_indx(pr)
          TO lv_indx
          ID lv_srtfd.
        IF sy-subrc <> 0.
          CONCATENATE TEXT-003 iv_interface_id TEXT-011 INTO lv_message
                      SEPARATED BY space.
          WRITE : / lv_message COLOR COL_NEGATIVE.
        ELSE.
          TRY.
              lo_keys->set_decrypt_cert_applic( zcl_ssf_utility=>gc_primary_applic ). "iv_applic ).
              lv_key = lo_keys->decrypt( lt_enveloped_data ).

              lo_keys->set_encrypt_cert_applic( zcl_ssf_utility=>gc_secondary_applic ).
              lt_enveloped_data = lo_keys->encrypt( lv_key ).

              IF NOT lt_enveloped_data[] IS INITIAL.
                CLEAR lv_indx.
                lv_indx-aedat = sy-datum.
                lv_indx-usera = sy-uname.
                lv_indx-pgmid = sy-repid.
                lv_srtfd = iv_interface_id.
                EXPORT tab = lt_enveloped_data[]
                TO DATABASE zssf_con_indx(bu)
                FROM lv_indx
                ID lv_srtfd.
              ENDIF.

              CONCATENATE TEXT-003 iv_interface_id TEXT-009 INTO lv_message
                          SEPARATED BY space.
              WRITE : / lv_message COLOR COL_POSITIVE.

            CATCH zcx_adf_manage_access_keys INTO lx_ref.
              lv_error_msg = lx_ref->get_text( ).
              CONCATENATE TEXT-003 iv_interface_id TEXT-011 INTO lv_message
                          SEPARATED BY space.
              WRITE : / lv_message COLOR COL_NEGATIVE.
              WRITE : /  lv_error_msg COLOR COL_NEGATIVE.
          ENDTRY.
        ENDIF.
      ENDIF.
    ELSEIF p_ft EQ abap_true.

      IMPORT tab  = lt_enveloped_data[]
        FROM DATABASE zssf_con_indx(ze)
        TO lv_indx
        ID lv_srtfd.
      IF sy-subrc <> 0.
        CONCATENATE TEXT-003 iv_interface_id TEXT-016 INTO lv_message
                    SEPARATED BY space.
        WRITE : / lv_message COLOR COL_NEGATIVE.
      ELSE.
        TRY.
            lo_keys->set_decrypt_cert_applic( iv_applic ).
            lv_key = lo_keys->decrypt( lt_enveloped_data ).

            lo_keys->set_encrypt_cert_applic( zcl_ssf_utility=>gc_primary_applic ).
            lt_enveloped_data = lo_keys->encrypt( lv_key ).

            IF NOT lt_enveloped_data[] IS INITIAL.
              CLEAR lv_indx.
              lv_indx-aedat = sy-datum.
              lv_indx-usera = sy-uname.
              lv_indx-pgmid = sy-repid.
              lv_srtfd = iv_interface_id.
              EXPORT tab = lt_enveloped_data[]
              TO DATABASE zssf_con_indx(pr)
              FROM lv_indx
              ID lv_srtfd.
            ENDIF.

            CONCATENATE TEXT-003 iv_interface_id TEXT-008 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_POSITIVE.

            lo_keys->set_encrypt_cert_applic( zcl_ssf_utility=>gc_secondary_applic ).
            lt_enveloped_data = lo_keys->encrypt( lv_key ).

            IF NOT lt_enveloped_data[] IS INITIAL.
              CLEAR lv_indx.
              lv_indx-aedat = sy-datum.
              lv_indx-usera = sy-uname.
              lv_indx-pgmid = sy-repid.
              lv_srtfd = iv_interface_id.
              EXPORT tab = lt_enveloped_data[]
              TO DATABASE zssf_con_indx(bu)
              FROM lv_indx
              ID lv_srtfd.
            ENDIF.

            CONCATENATE TEXT-003 iv_interface_id TEXT-009 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_POSITIVE.

          CATCH zcx_adf_manage_access_keys INTO lx_ref.
            lv_error_msg = lx_ref->get_text( ).
            CONCATENATE TEXT-003 iv_interface_id TEXT-018 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_NEGATIVE.
            WRITE : /  lv_error_msg COLOR COL_NEGATIVE.
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADF_VALIDATE_AND_ADJ_KEYS->CHECK_ACCESS_KEYS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_access_keys.
    DATA : lx_access_keys TYPE REF TO zcx_adf_manage_access_keys.

    LOOP AT lt_ssf_data INTO ls_ssf_data.

      CALL METHOD me->check_key
        EXPORTING
          iv_applic       = gv_applic
          iv_interface_id = ls_ssf_data-interface_id.

      WRITE : / '--------------------------------------------------------------------------'.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADF_VALIDATE_AND_ADJ_KEYS->CHECK_KEY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_APPLIC                      TYPE        SSFAPPLSSL
* | [--->] IV_INTERFACE_ID                TYPE        ZINTERFACE_ID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_key.
    DATA : lx_ref            TYPE REF TO zcx_adf_manage_access_keys,
           lv_key            TYPE string,
           lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zssf_con_indx-srtfd,
           lv_indx           TYPE zssf_con_indx,
           lv_error_msg      TYPE string.


    CLEAR lv_message.
    lv_srtfd = ls_ssf_data-interface_id.

    IF p_ft EQ abap_true.
      IMPORT tab  = lt_enveloped_data[]
        FROM DATABASE zssf_con_indx(ze)
        TO lv_indx
        ID lv_srtfd.
      IF sy-subrc <> 0.
        CONCATENATE TEXT-003 iv_interface_id TEXT-016 INTO lv_message
                      SEPARATED BY space.
        WRITE : / lv_message COLOR COL_NEGATIVE.
      ELSE.
        TRY.
            lo_keys->set_decrypt_cert_applic( iv_applic ).
            lv_key = lo_keys->decrypt( lt_enveloped_data ).

            CONCATENATE TEXT-003 iv_interface_id TEXT-019 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_POSITIVE.
          CATCH zcx_adf_manage_access_keys INTO lx_ref.
            lv_error_msg = lx_ref->get_text( ).
            CONCATENATE TEXT-003 iv_interface_id TEXT-016 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_NEGATIVE.
            WRITE : /  lv_error_msg COLOR COL_NEGATIVE.
        ENDTRY.
      ENDIF.
    ELSEIF p_ft EQ abap_false AND p_prim EQ abap_true.
      IMPORT tab  = lt_enveloped_data[]
        FROM DATABASE zssf_con_indx(pr)
        TO lv_indx
        ID lv_srtfd.
      IF sy-subrc <> 0.
        CONCATENATE TEXT-003 iv_interface_id TEXT-006 INTO lv_message
                      SEPARATED BY space.
        WRITE : / lv_message COLOR COL_NEGATIVE.
      ELSE.
        TRY.
            lo_keys->set_decrypt_cert_applic( zcl_ssf_utility=>gc_primary_applic ).
            lv_key = lo_keys->decrypt( lt_enveloped_data ).

            CONCATENATE TEXT-003 iv_interface_id TEXT-004 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_POSITIVE.

          CATCH zcx_adf_manage_access_keys INTO lx_ref.
            lv_error_msg = lx_ref->get_text( ).
            CONCATENATE TEXT-003 iv_interface_id TEXT-006 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_POSITIVE.
            WRITE : /  lv_error_msg COLOR COL_NEGATIVE.
        ENDTRY.
      ENDIF.
    ELSEIF p_ft EQ abap_false AND p_sec EQ abap_true.
      IMPORT tab  = lt_enveloped_data[]
        FROM DATABASE zssf_con_indx(bu)
        TO lv_indx
        ID lv_srtfd.
      IF sy-subrc <> 0.
        CONCATENATE TEXT-003 iv_interface_id TEXT-007 INTO lv_message
                      SEPARATED BY space.
        WRITE : / lv_message COLOR COL_NEGATIVE.
      ELSE.
        TRY.
            lo_keys->set_decrypt_cert_applic( zcl_ssf_utility=>gc_secondary_applic ).
            lv_key = lo_keys->decrypt( lt_enveloped_data ).

            CONCATENATE TEXT-003 iv_interface_id TEXT-005 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_POSITIVE.

          CATCH zcx_adf_manage_access_keys INTO lx_ref.
            lv_error_msg = lx_ref->get_text( ).
            CONCATENATE TEXT-003 iv_interface_id TEXT-007 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_POSITIVE.
            WRITE : /  lv_error_msg COLOR COL_NEGATIVE.
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADF_VALIDATE_AND_ADJ_KEYS->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    lo_keys = zcl_adf_manage_access_keys=>get_instance( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADF_VALIDATE_AND_ADJ_KEYS->READ_CONFIG_TABLES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_config_tables.
    CONSTANTS: lc_profile TYPE ztvarvc-varname VALUE 'SSL_CLIENT_ID'.

    IF p_all EQ abap_true.
      SELECT * FROM zssf_data INTO TABLE lt_ssf_data.
    ELSE.
      SELECT * FROM zssf_data INTO TABLE lt_ssf_data
        WHERE zkey IN s_intf.
    ENDIF.
    IF p_ft EQ abap_true.
      SELECT SINGLE low FROM ztvarvc
                        INTO gv_applic
                        WHERE varname EQ lc_profile.
      IF sy-subrc <> 0.
        WRITE : / TEXT-012 COLOR COL_NEGATIVE.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
