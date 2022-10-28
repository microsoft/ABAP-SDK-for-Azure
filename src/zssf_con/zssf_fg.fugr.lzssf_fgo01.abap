*----------------------------------------------------------------------*
***INCLUDE LZSSF_FGO01.
*----------------------------------------------------------------------*

*{   INSERT         DGDK908495                                        1
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CHANGE_PWD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_change_pwd OUTPUT.
   LOOP AT SCREEN.
    IF screen-name EQ 'ZSSF_DATA-ZKEY'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.
*}   INSERT
