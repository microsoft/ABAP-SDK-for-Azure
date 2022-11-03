*&---------------------------------------------------------------------*
*&  Include  ZSSF_VALIDATE_AND_ADJ_KEYS_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-015.
PARAMETERS : p_ft AS CHECKBOX USER-COMMAND ftm  .
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_chk TYPE c RADIOBUTTON GROUP rg1 MODIF ID rg1,
             p_adj TYPE c RADIOBUTTON GROUP rg1 MODIF ID rg1.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE TEXT-002.
PARAMETERS : p_prim   TYPE c RADIOBUTTON GROUP rg2 MODIF ID rg2,
             p_sec TYPE c RADIOBUTTON GROUP rg2 MODIF ID rg2.
SELECTION-SCREEN END OF BLOCK b11.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN ULINE.

PARAMETERS : p_all AS CHECKBOX USER-COMMAND usr1 MODIF ID all.
SELECT-OPTIONS : s_intf FOR zssf_data-interface_id MODIF ID inf.

AT SELECTION-SCREEN OUTPUT.

  IF p_all EQ abap_true.
    CLEAR : s_intf, s_intf[].
    LOOP AT SCREEN.
      IF screen-group1 = 'INF'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'INF'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_ft EQ abap_true .
    LOOP AT SCREEN.
      IF screen-group1 = 'RG2'.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE .
    LOOP AT SCREEN.
      IF screen-group1 = 'RG2'.
        screen-invisible = 0 .
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF .
