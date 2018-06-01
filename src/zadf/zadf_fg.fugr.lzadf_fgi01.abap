*----------------------------------------------------------------------*
***INCLUDE LZADF_FGI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_SASKEY  INPUT
*&---------------------------------------------------------------------*
*       Validate Sas_key
*----------------------------------------------------------------------*
MODULE validate_saskey INPUT.
  DATA : lv_sas_key TYPE zadf_config-sas_key.
  IF ( status-action EQ 'A' ) AND
   ( function EQ 'SAVE' ) AND
   ( extract[] NE total[] ).
**Do nothing
  ELSE.
    SELECT SINGLE sas_key FROM zadf_config
                          INTO lv_sas_key
                          WHERE interface_id EQ zadf_config-interface_id.
    IF sy-subrc EQ 0.
      IF lv_sas_key NE zadf_config-sas_key.
        MESSAGE text-004 TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " VALIDATE_SASKEY  INPUT
