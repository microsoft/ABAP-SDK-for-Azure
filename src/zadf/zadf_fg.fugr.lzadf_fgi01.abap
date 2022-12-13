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
  ENDIF.

ENDMODULE.                 " VALIDATE_SASKEY  INPUT
*&---------------------------------------------------------------------*
*&      Module  HIDE_ISTRYCOL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE hide_istrycol OUTPUT.
  tctrl_zadf_config-cols[ 6 ]-invisible = 'X'.
ENDMODULE.
