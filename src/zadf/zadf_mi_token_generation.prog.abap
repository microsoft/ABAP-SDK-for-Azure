*&---------------------------------------------------------------------*
*& Report  ZADF_MI_TOKEN_GENERATION
**---------------------------------------------------------------------------------------------*
* Programmer                                                      Vikas Bansal            *
*----------------------------------------------------------------------------------------------*
* Program developed to Generate the Managed Identity Token Manually
* In Case SL's face issue with the stored MI token or want to generate new MSI token for Interface ID.
*----------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 01|06|2023|VIKASBANSAL | SMTK908732 |  MI Token Generaion
*----------------------------------------------------------------------*

REPORT zadf_mi_token_generation.

DATA:
  lo_oref    TYPE REF TO zcl_adf_service,
  lo_ref_aad TYPE REF TO zcl_adf_service_aad,
  ls_mi_conf TYPE zadf_mi_config.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_intid FOR ls_mi_conf-interface_id NO INTERVALS.
SELECTION-SCREEN END OF BLOCK bl1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_intid-low.

  SELECT * FROM zadf_mi_config INTO TABLE @DATA(lt_midata).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'INTERFACE_ID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'S_INTID'
      value_org       = 'S'
    TABLES
      value_tab       = lt_midata[]
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE TEXT-003 TYPE 'E'.
      WHEN 2.
        MESSAGE TEXT-004 TYPE 'E'.
      WHEN OTHERS.
        MESSAGE TEXT-005 TYPE 'E'.
    ENDCASE.
  ENDIF.


START-OF-SELECTION.

  IF s_intid[] IS INITIAL.
    MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SELECT interface_id FROM zadf_mi_config
                  INTO TABLE @DATA(lt_mi_tab)
                   WHERE interface_id IN @s_intid.
  IF sy-subrc = 0.
    LOOP AT lt_mi_tab ASSIGNING FIELD-SYMBOL(<lfs_mi>).
      TRY.
*     create service
          CALL METHOD zcl_adf_service_factory=>create
            EXPORTING
              iv_interface_id        = <lfs_mi>-interface_id
              iv_business_identifier = 'MI_TOKEN_GEN'
            RECEIVING
              ro_service             = lo_oref.
        CATCH zcx_adf_service INTO DATA(lo_cx_adf).
          DATA(lv_string) = lo_cx_adf->get_text( ).
        CATCH zcx_interace_config_missing INTO DATA(lo_cx_conf) .
          lv_string = lo_cx_conf->get_text( ).
        CATCH zcx_http_client_failed INTO DATA(lo_cx_clint).
          lv_string = lo_cx_clint->get_text( ).
      ENDTRY.

      lo_ref_aad ?=  lo_oref.

      IF lo_ref_aad IS BOUND AND lv_string IS INITIAL.
        TRY.
*       Generate MSI Token
            CALL METHOD lo_ref_aad->get_aad_token_msi
              EXPORTING
                iv_tokengen_flag = 'X'
              IMPORTING
                ev_aad_token     = DATA(lv_aad_token)
                ev_response      = DATA(lv_response).
          CATCH zcx_interace_config_missing INTO DATA(lo_cx_interface).
            lv_string = lo_cx_interface->get_text( ).
          CATCH zcx_http_client_failed INTO DATA(lo_cx_http) .
            lv_string = lo_cx_http->get_text( ).
          CATCH zcx_adf_service INTO DATA(lo_cx_adf_service).
            lv_string = lo_cx_adf_service->get_text( ).
        ENDTRY.
      ENDIF.

      IF lv_aad_token IS NOT INITIAL.
        WRITE / TEXT-006 && <lfs_mi>-interface_id .
      ELSE.
        WRITE /  TEXT-007  && space && lv_string .
      ENDIF.

      CLEAR : lv_string,lv_aad_token.
      FREE : lo_ref_aad,lo_ref_aad.
    ENDLOOP.
  ELSE.
    MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
