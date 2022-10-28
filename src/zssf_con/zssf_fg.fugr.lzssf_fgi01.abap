*----------------------------------------------------------------------*
***INCLUDE LZSSF_FGI01.
*----------------------------------------------------------------------*
MODULE validate_key INPUT.
  DATA : lv_key TYPE zssf_data-zkey.
  IF ( status-action EQ 'A' ) AND
   ( function EQ 'SAVE' ) AND
   ( extract[] NE total[] ).
**Do nothing
  ELSE.
    CLEAR lv_key.
    SELECT SINGLE zkey FROM zssf_data
                      INTO lv_key
                      WHERE interface_id EQ zssf_data-interface_id.
    IF sy-subrc EQ 0.
      IF lv_key NE zssf_data-zkey.
        MESSAGE text-004 TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " VALIDATE_KEY  INPUT
