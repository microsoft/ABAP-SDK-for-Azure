class ZCL_IM_ADF_EDIT_AUTH definition
  public
  final
  create public .

public section.

  interfaces IF_EX_CTS_CURRENT_PROJECT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ADF_EDIT_AUTH IMPLEMENTATION.


  METHOD if_ex_cts_current_project~get_current_project.


*    TYPES : BEGIN OF yt_tadir,
*              obj_name TYPE tadir-obj_name,
*            END OF yt_tadir.
*
*    DATA : ls_objects TYPE ko200,
*           ls_keys    TYPE e071k,
*           lt_tadir   TYPE TABLE OF yt_tadir.
*
*    READ TABLE it_objects INTO ls_objects INDEX 1.
*    IF sy-subrc = 0.
*      SELECT obj_name FROM tadir
*         INTO CORRESPONDING FIELDS OF TABLE lt_tadir
*         WHERE devclass = 'ZADF' OR devclass = 'ZSSF_CON'.
*      SORT lt_tadir.
*      READ TABLE lt_tadir WITH KEY obj_name = ls_objects-obj_name
*           BINARY SEARCH
*           TRANSPORTING NO FIELDS.
*      IF sy-subrc = 0.
*        AUTHORITY-CHECK OBJECT 'ZEDITAUTH'
*            ID 'P_ACTION' FIELD 'X'.
*        IF sy-subrc <> 0.
**     Hardcoded alias for testing. to be removed post testing.
*          IF sy-uname <> 'NAKURMAD' OR
*             sy-uname <> 'KRDASH' OR
*             sy-uname <> 'CHANDBS' OR
*             sy-uname <> 'TAMANDAL' OR
*             sy-uname <> 'MANOJKK'.
*            MESSAGE 'No Change Authorization for ZADF Objects' TYPE 'E'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
