*&---------------------------------------------------------------------*
*&  Include           ZADF_VALIDATE_AND_ADJ_KEYS_CLS
*&---------------------------------------------------------------------*
CLASS lcl_adf_validate_and_adj_keys DEFINITION.

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
    METHODS submit_ssf_adj_keys_program .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS lcl_adf_validate_and_adj_keys IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADF_VALIDATE_AND_ADJ_KEYS->ADJUST_ACCESS_KEYS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD adjust_access_keys.
    DATA : lx_access_keys TYPE REF TO zcx_adf_manage_access_keys.

    LOOP AT lt_adf_config INTO ls_adf_config.
      READ TABLE lt_rest_config INTO ls_rest_config
           WITH KEY interface_id = ls_adf_config-interface_id.
      IF sy-subrc = 0.
        CLEAR lv_applic.
        IF p_ft EQ abap_true.
          TRY.
              lv_applic = zcl_adf_manage_access_keys=>read_applic_from_destination(
                                                      ls_rest_config-destination ).
            CATCH zcx_adf_manage_access_keys INTO lx_access_keys.
*          IF p_ft EQ abap_true.
              CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-016 INTO lv_message
                        SEPARATED BY space.
*          ELSEIF p_ft EQ abap_false AND p_prim EQ abap_true.
*            CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-006 INTO lv_message
*                                SEPARATED BY space.
*          ELSE.
*            CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-007 INTO lv_message
*                              SEPARATED BY space.
*          ENDIF.
              WRITE : / lv_message COLOR COL_NEGATIVE.

              CONCATENATE TEXT-012 ls_rest_config-destination INTO lv_message
                          SEPARATED BY space.
              WRITE : / lv_message COLOR COL_NEGATIVE.
*            WRITE : / '--------------------------------------------------------------------------'.
          ENDTRY.
        ENDIF.

        CALL METHOD me->adjust_key
          EXPORTING
            iv_applic       = lv_applic
            iv_interface_id = ls_rest_config-interface_id.

      ELSE.
        IF p_ft EQ abap_true.
          CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-016 INTO lv_message
                    SEPARATED BY space.
        ELSEIF p_ft EQ abap_false AND p_prim EQ abap_true.
          CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-006 INTO lv_message
                              SEPARATED BY space.
        ELSE.
          CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-007 INTO lv_message
                            SEPARATED BY space.
        ENDIF.
        WRITE : / lv_message COLOR COL_NEGATIVE.

        CONCATENATE TEXT-014 ls_adf_config-interface_id INTO lv_message
                    SEPARATED BY space.
        WRITE : / lv_message COLOR COL_NEGATIVE.
      ENDIF.
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
           lv_srtfd          TYPE zadf_con_indx-srtfd,
           lv_indx           TYPE zadf_con_indx,
           lv_error_msg      TYPE string.

    CLEAR lv_message.
    IF p_ft EQ abap_false.
      IF p_prim EQ abap_true.

        lv_srtfd = ls_rest_config-interface_id.
        IMPORT tab  = lt_enveloped_data[]
          FROM DATABASE zadf_con_indx(bu)
          TO lv_indx
          ID lv_srtfd.
        IF sy-subrc <> 0.
          CONCATENATE TEXT-003 iv_interface_id TEXT-010 INTO lv_message
                      SEPARATED BY space.
          WRITE : / lv_message COLOR COL_NEGATIVE.
        ELSE.
          TRY.
              lo_keys->set_decrypt_cert_applic( zcl_adf_manage_interface_key=>gc_secondary_applic ).
              lv_key = lo_keys->decrypt( lt_enveloped_data ).

              lo_keys->set_encrypt_cert_applic( zcl_adf_manage_interface_key=>gc_primary_applic ). "iv_applic ).
              lt_enveloped_data = lo_keys->encrypt( lv_key ).

              IF NOT lt_enveloped_data[] IS INITIAL.
                CLEAR lv_indx.
                lv_indx-aedat = sy-datum.
                lv_indx-usera = sy-uname.
                lv_indx-pgmid = sy-repid.
                lv_srtfd = iv_interface_id.
                EXPORT tab = lt_enveloped_data[]
                TO DATABASE zadf_con_indx(pr)
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
        lv_srtfd = ls_rest_config-interface_id.
        IMPORT tab  = lt_enveloped_data[]
          FROM DATABASE zadf_con_indx(pr)
          TO lv_indx
          ID lv_srtfd.
        IF sy-subrc <> 0.
          CONCATENATE TEXT-003 iv_interface_id TEXT-011 INTO lv_message
                      SEPARATED BY space.
          WRITE : / lv_message COLOR COL_NEGATIVE.
        ELSE.
          TRY.
              lo_keys->set_decrypt_cert_applic( zcl_adf_manage_interface_key=>gc_primary_applic ). "iv_applic ).
              lv_key = lo_keys->decrypt( lt_enveloped_data ).

              lo_keys->set_encrypt_cert_applic( zcl_adf_manage_interface_key=>gc_secondary_applic ).
              lt_enveloped_data = lo_keys->encrypt( lv_key ).

              IF NOT lt_enveloped_data[] IS INITIAL.
                CLEAR lv_indx.
                lv_indx-aedat = sy-datum.
                lv_indx-usera = sy-uname.
                lv_indx-pgmid = sy-repid.
                lv_srtfd = iv_interface_id.
                EXPORT tab = lt_enveloped_data[]
                TO DATABASE zadf_con_indx(bu)
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
*    IF p_prim EQ abap_true.
      lv_srtfd = ls_rest_config-interface_id.
      IMPORT tab  = lt_enveloped_data[]
        FROM DATABASE zadf_con_indx(zd)
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

            lo_keys->set_encrypt_cert_applic( zcl_adf_manage_interface_key=>gc_primary_applic ).
            lt_enveloped_data = lo_keys->encrypt( lv_key ).

            IF NOT lt_enveloped_data[] IS INITIAL.
              CLEAR lv_indx.
              lv_indx-aedat = sy-datum.
              lv_indx-usera = sy-uname.
              lv_indx-pgmid = sy-repid.
              lv_srtfd = iv_interface_id.
              EXPORT tab = lt_enveloped_data[]
              TO DATABASE zadf_con_indx(pr)
              FROM lv_indx
              ID lv_srtfd.
            ENDIF.

            CONCATENATE TEXT-003 iv_interface_id TEXT-008 INTO lv_message
                        SEPARATED BY space.
            WRITE : / lv_message COLOR COL_POSITIVE.

            lo_keys->set_encrypt_cert_applic( zcl_adf_manage_interface_key=>gc_secondary_applic ).
            lt_enveloped_data = lo_keys->encrypt( lv_key ).

            IF NOT lt_enveloped_data[] IS INITIAL.
              CLEAR lv_indx.
              lv_indx-aedat = sy-datum.
              lv_indx-usera = sy-uname.
              lv_indx-pgmid = sy-repid.
              lv_srtfd = iv_interface_id.
              EXPORT tab = lt_enveloped_data[]
              TO DATABASE zadf_con_indx(bu)
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

    LOOP AT lt_adf_config INTO ls_adf_config.
      READ TABLE lt_rest_config INTO ls_rest_config
           WITH KEY interface_id = ls_adf_config-interface_id.
      IF sy-subrc = 0.
        CLEAR lv_applic.
        IF p_ft EQ abap_true.
          TRY.
              lv_applic = zcl_adf_manage_access_keys=>read_applic_from_destination(
                                                      ls_rest_config-destination ).
            CATCH zcx_adf_manage_access_keys INTO lx_access_keys.
*          IF p_ft EQ abap_true.
              CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-016 INTO lv_message
                        SEPARATED BY space.
*          ELSEIF p_ft EQ abap_false AND p_prim EQ abap_true.
*            CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-006 INTO lv_message
*                                SEPARATED BY space.
*          ELSE.
*            CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-007 INTO lv_message
*                              SEPARATED BY space.
*          ENDIF.
              WRITE : / lv_message COLOR COL_NEGATIVE.
              CONCATENATE TEXT-012 ls_rest_config-destination INTO lv_message
                          SEPARATED BY space.
              WRITE : / lv_message COLOR COL_NEGATIVE.
              WRITE : / '--------------------------------------------------------------------------'.
          ENDTRY.
        ENDIF.

        CALL METHOD me->check_key
          EXPORTING
            iv_applic       = lv_applic
            iv_interface_id = ls_rest_config-interface_id.


      ELSE.
        IF p_ft EQ abap_true.
          CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-016 INTO lv_message
                    SEPARATED BY space.
        ELSEIF p_ft EQ abap_false AND p_prim EQ abap_true.
          CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-006 INTO lv_message
                              SEPARATED BY space.
        ELSE.
          CONCATENATE TEXT-003 ls_adf_config-interface_id TEXT-007 INTO lv_message
                            SEPARATED BY space.
        ENDIF.
        WRITE : / lv_message COLOR COL_NEGATIVE.
        CONCATENATE TEXT-014 ls_adf_config-interface_id INTO lv_message
                    SEPARATED BY space.
        WRITE : / lv_message COLOR COL_NEGATIVE.
      ENDIF.
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
           lv_srtfd          TYPE zadf_con_indx-srtfd,
           lv_indx           TYPE zadf_con_indx,
           lv_error_msg      TYPE string.


    CLEAR lv_message.
    IF p_ft EQ abap_true.

      lv_srtfd = ls_rest_config-interface_id.
      IMPORT tab  = lt_enveloped_data[]
        FROM DATABASE zadf_con_indx(zd)
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
      lv_srtfd = ls_rest_config-interface_id.
      IMPORT tab  = lt_enveloped_data[]
        FROM DATABASE zadf_con_indx(pr)
        TO lv_indx
        ID lv_srtfd.
      IF sy-subrc <> 0.
        CONCATENATE TEXT-003 iv_interface_id TEXT-006 INTO lv_message
                      SEPARATED BY space.
        WRITE : / lv_message COLOR COL_NEGATIVE.
      ELSE.
        TRY.
            lo_keys->set_decrypt_cert_applic( zcl_adf_manage_interface_key=>gc_primary_applic ).
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
      lv_srtfd = ls_rest_config-interface_id.
      IMPORT tab  = lt_enveloped_data[]
        FROM DATABASE zadf_con_indx(bu)
        TO lv_indx
        ID lv_srtfd.
      IF sy-subrc <> 0.
        CONCATENATE TEXT-003 iv_interface_id TEXT-007 INTO lv_message
                      SEPARATED BY space.
        WRITE : / lv_message COLOR COL_NEGATIVE.
      ELSE.
        TRY.
            lo_keys->set_decrypt_cert_applic( zcl_adf_manage_interface_key=>gc_secondary_applic ).
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
   IF p_all EQ abap_true.
      SELECT * FROM zadf_config
               INTO TABLE lt_adf_config.
    ELSE.
      SELECT * FROM zadf_config
               INTO TABLE lt_adf_config
               WHERE interface_id IN s_intf.
    ENDIF.

    LOOP AT lt_adf_config INTO DATA(ls_adf) WHERE sas_key IS INITIAL.
      IF p_chk EQ abap_true.
        DATA(lv_str) = |{ text-003 } { ls_adf-interface_id } { text-020 }|.
      ELSEIF p_adj EQ abap_true.
        lv_str = |{ text-003 } { ls_adf-interface_id } { text-021 }|.
      ENDIF.
      WRITE /: lv_str COLOR COL_POSITIVE.
    ENDLOOP.

* Remove records with no key in ZADF_CONFIG table
    DELETE lt_adf_config WHERE sas_key IS INITIAL.

    IF lt_adf_config IS NOT INITIAL.
* Fetch the destination for the remaining interfaces
      SELECT * FROM zrest_config
        INTO TABLE lt_rest_config
        FOR ALL ENTRIES IN lt_adf_config
        WHERE interface_id = lt_adf_config-interface_id.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADF_VALIDATE_AND_ADJ_KEYS->SUBMIT_SSF_ADJ_KEYS_PROGRAM
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD submit_ssf_adj_keys_program.
* local data declerations
    DATA : lv_job_num  TYPE tbtcjob-jobcount,
           lv_jobname  TYPE btcjob,
           lv_jb_count TYPE  tbtcm-jobcount,
           lv_jb_name  TYPE  tbtcm-jobname.

* For Job Name
    CONCATENATE 'ZSSF_ADJUST_KEYS' sy-datum sy-uzeit
           INTO lv_jobname SEPARATED BY '_' .
* Condense Job name
    CONDENSE lv_jobname .

* Calling JOB OPEN Function Module
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_jobname
      IMPORTING
        jobcount         = lv_job_num
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc = 0.
* Submit Program for adjust keys
      SUBMIT zssf_validate_and_adjust_keys
                                   VIA JOB lv_jobname NUMBER lv_job_num
                                   WITH p_ft EQ p_ft
                                   WITH p_chk EQ p_chk
                                   WITH p_adj EQ p_adj
                                   WITH p_prim EQ p_prim
                                   WITH p_sec EQ p_sec
                                   WITH p_all EQ p_all
                                   WITH s_intf IN s_intf
                                   AND RETURN .
      IF sy-subrc EQ 0.
* Call the Function Module JOB CLOSE
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lv_job_num
            jobname              = lv_jobname
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.
        IF sy-subrc = 0.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDIF.
    ENDIF.
* Clearing the Variables
    CLEAR : lv_job_num , lv_jobname .
  ENDMETHOD.
ENDCLASS.
