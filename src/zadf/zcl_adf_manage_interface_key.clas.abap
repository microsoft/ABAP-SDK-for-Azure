class ZCL_ADF_MANAGE_INTERFACE_KEY definition
  public
  final
  create public .

public section.

  class-data GC_SECONDARY_APPLIC type SSFAPPLSSL value 'ZADFS' ##NO_TEXT.
  class-data GC_PRIMARY_APPLIC type SSFAPPLSSL value 'ZADFP' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_ADF_MANAGE_INTERFACE_KEY .
  methods READ_KEY
    returning
      value(RV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_ADF_MANAGE_INTERFACE_KEY .
  methods INSERT_KEY
    importing
      value(IV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_ADF_MANAGE_INTERFACE_KEY .
  PROTECTED SECTION.
private section.

  data GV_INTERFACE_ID type ZINTERFACE_ID .
  data GO_ACCESS_KEY type ref to ZCL_ADF_MANAGE_ACCESS_KEYS .
  data GS_ADF_CONFIG type ZADF_CONFIG .
  data GS_REST_CONFIG type ZREST_CONFIG .
  data GV_NEW type BOOLEAN .

  methods READ_CONFIG_TABLES
    raising
      ZCX_ADF_MANAGE_INTERFACE_KEY .
  methods READ_KEY_WITH_PRIMARY
    returning
      value(RV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_ADF_MANAGE_INTERFACE_KEY .
  methods READ_KEY_WITH_SECONDARY
    returning
      value(RV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_ADF_MANAGE_INTERFACE_KEY .
  methods INSERT_KEY_WITH_PRIMARY
    importing
      value(IV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_ADF_MANAGE_INTERFACE_KEY .
  methods INSERT_KEY_WITH_SECONDARY
    importing
      value(IV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_ADF_MANAGE_INTERFACE_KEY .
  methods INSERT_KEY_WITH_DEFAULT_PSE
    importing
      value(IV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_ADF_MANAGE_INTERFACE_KEY .
  methods READ_KEY_WITH_DEFAULT_PSE
    returning
      value(RV_KEY) type STRING
    raising
      ZCX_ADF_MANAGE_ACCESS_KEYS
      ZCX_ADF_MANAGE_INTERFACE_KEY .
ENDCLASS.



CLASS ZCL_ADF_MANAGE_INTERFACE_KEY IMPLEMENTATION.


  METHOD constructor.

    gv_interface_id = iv_interface_id.
    go_access_key = zcl_adf_manage_access_keys=>get_instance( ).

    read_config_tables( ).

  ENDMETHOD.


  METHOD insert_key.
    DATA:
      lv_switch TYPE char1.

    CONSTANTS:
      lc_switch TYPE ztvarvc-varname VALUE 'ZADF_NEW_PSE_SWITCH'.

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

    DATA : lx_ref            TYPE REF TO zcx_adf_manage_access_keys,
           lv_key            TYPE string,
           lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zadf_con_indx-srtfd,
           lv_indx           TYPE zadf_con_indx.

    DATA : lv_applic         TYPE ssfapplssl.

    lv_applic = zcl_adf_manage_access_keys=>read_applic_from_destination(
                                            gs_rest_config-destination ).
    go_access_key->set_encrypt_cert_applic( lv_applic ).
    lt_enveloped_data = go_access_key->encrypt( iv_key ).

    IF NOT lt_enveloped_data[] IS INITIAL.

      lv_indx-aedat = sy-datum.
      lv_indx-usera = sy-uname.
      lv_indx-pgmid = sy-repid.
      lv_srtfd = gs_rest_config-interface_id.

      EXPORT tab = lt_enveloped_data[]
      TO DATABASE zadf_con_indx(zd)
      FROM lv_indx
      ID lv_srtfd.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_adf_manage_interface_key
          EXPORTING
            textid       = zcx_adf_manage_interface_key=>zcx_adf_con_update_failed
            interface_id = gs_rest_config-interface_id.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD insert_key_with_primary.
    DATA : lx_ref            TYPE REF TO zcx_adf_manage_access_keys,
           lv_key            TYPE string,
           lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zadf_con_indx-srtfd,
           lv_indx           TYPE zadf_con_indx.

    go_access_key->set_encrypt_cert_applic( gc_primary_applic ).
    lt_enveloped_data = go_access_key->encrypt( iv_key ).

    IF NOT lt_enveloped_data[] IS INITIAL.

      lv_indx-aedat = sy-datum.
      lv_indx-usera = sy-uname.
      lv_indx-pgmid = sy-repid.
      lv_srtfd = gs_rest_config-interface_id.

      EXPORT tab = lt_enveloped_data[]
      TO DATABASE zadf_con_indx(pr)
      FROM lv_indx
      ID lv_srtfd.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_adf_manage_interface_key
          EXPORTING
            textid       = zcx_adf_manage_interface_key=>zcx_adf_con_update_failed
            interface_id = gs_rest_config-interface_id.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD insert_key_with_secondary.

    DATA : lx_ref            TYPE REF TO zcx_adf_manage_access_keys,
           lv_key            TYPE string,
           lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zadf_con_indx-srtfd,
           lv_indx           TYPE zadf_con_indx.

    go_access_key->set_encrypt_cert_applic( gc_secondary_applic ).
    lt_enveloped_data = go_access_key->encrypt( iv_key ).

    IF NOT lt_enveloped_data[] IS INITIAL.

      lv_indx-aedat = sy-datum.
      lv_indx-usera = sy-uname.
      lv_indx-pgmid = sy-repid.
      lv_srtfd = gs_rest_config-interface_id.

      EXPORT tab = lt_enveloped_data[]
      TO DATABASE zadf_con_indx(bu)
      FROM lv_indx
      ID lv_srtfd.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_adf_manage_interface_key
          EXPORTING
            textid       = zcx_adf_manage_interface_key=>zcx_adf_con_update_failed
            interface_id = gs_rest_config-interface_id.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD read_config_tables.

    SELECT SINGLE * FROM zrest_config INTO gs_rest_config
       WHERE interface_id = gv_interface_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adf_manage_interface_key
        EXPORTING
          textid       = zcx_adf_manage_interface_key=>zcx_adf_rest_config_not_found
          interface_id = gv_interface_id.
    ENDIF.

  ENDMETHOD.


  METHOD read_key.

    DATA : lx_ref    TYPE REF TO zcx_adf_manage_interface_key,
           lv_switch TYPE char1.
    CONSTANTS : lc_switch TYPE ztvarvc-varname VALUE 'ZADF_NEW_PSE_SWITCH'.

    SELECT SINGLE low  FROM ztvarvc
                          INTO lv_switch
                          WHERE varname EQ lc_switch.
    IF sy-subrc EQ 0 AND lv_switch EQ 'X'.
      TRY.
          rv_key = read_key_with_primary( ).
        CATCH zcx_adf_manage_interface_key INTO lx_ref.
          rv_key = read_key_with_secondary( ).
      ENDTRY.
    ELSE.
      rv_key = read_key_with_default_pse( ).
    ENDIF.
  ENDMETHOD.


  METHOD read_key_with_default_pse.
    DATA : lx_ref            TYPE REF TO zcx_adf_manage_access_keys,
           lv_key            TYPE string,
           lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zadf_con_indx-srtfd,
           lv_indx           TYPE zadf_con_indx.

    DATA : lv_applic         TYPE ssfapplssl.

    lv_applic = zcl_adf_manage_access_keys=>read_applic_from_destination(
                                            gs_rest_config-destination ).

    lv_srtfd = gs_rest_config-interface_id.
    IMPORT tab  = lt_enveloped_data[]
      FROM DATABASE zadf_con_indx(zd)
      TO lv_indx
      ID lv_srtfd.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adf_manage_interface_key
        EXPORTING
          textid       = zcx_adf_manage_interface_key=>zcx_adf_import_failed
          interface_id = gs_rest_config-interface_id.
    ENDIF.

    go_access_key->set_decrypt_cert_applic( lv_applic ).
    rv_key = go_access_key->decrypt( lt_enveloped_data ).
  ENDMETHOD.


  METHOD read_key_with_primary.

    DATA : lx_ref            TYPE REF TO zcx_adf_manage_access_keys,
           lv_key            TYPE string,
           lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zadf_con_indx-srtfd,
           lv_indx           TYPE zadf_con_indx.

    lv_srtfd = gs_rest_config-interface_id.
    IMPORT tab  = lt_enveloped_data[]
      FROM DATABASE zadf_con_indx(pr)
      TO lv_indx
      ID lv_srtfd.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adf_manage_interface_key
        EXPORTING
          textid       = zcx_adf_manage_interface_key=>zcx_adf_import_failed
          interface_id = gs_rest_config-interface_id.
    ENDIF.

    go_access_key->set_decrypt_cert_applic( gc_primary_applic ).
    rv_key = go_access_key->decrypt( lt_enveloped_data ).

  ENDMETHOD.


  METHOD read_key_with_secondary.

    DATA : lx_ref            TYPE REF TO zcx_adf_manage_access_keys,
           lv_key            TYPE string,
           lt_enveloped_data TYPE saml2_pse_bin_data_t,
           lv_srtfd          TYPE zadf_con_indx-srtfd,
           lv_indx           TYPE zadf_con_indx.

    lv_srtfd = gs_rest_config-interface_id.
    IMPORT tab  = lt_enveloped_data[]
      FROM DATABASE zadf_con_indx(bu)
      TO lv_indx
      ID lv_srtfd.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adf_manage_interface_key
        EXPORTING
          textid       = zcx_adf_manage_interface_key=>zcx_adf_import_failed
          interface_id = gs_rest_config-interface_id.
    ENDIF.

    go_access_key->set_decrypt_cert_applic( gc_secondary_applic ).
    rv_key = go_access_key->decrypt( lt_enveloped_data ).

  ENDMETHOD.
ENDCLASS.
