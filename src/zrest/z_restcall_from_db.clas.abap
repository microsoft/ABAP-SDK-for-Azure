class Z_RESTCALL_FROM_DB definition
  public
  create public .

public section.

  class-methods GET_CALLDATA_FROMDB
    importing
      !MESSAGE_ID type ZMID
    exporting
      !PAYLOAD type ZRT_PAYLOAD
      !MONITOR type ZREST_MONITOR .
protected section.
private section.
ENDCLASS.



CLASS Z_RESTCALL_FROM_DB IMPLEMENTATION.


  METHOD get_calldata_fromdb.

*   Check if message exists
    SELECT SINGLE * FROM zrest_monitor INTO monitor WHERE zmessageid EQ message_id.
    IF sy-subrc EQ 0.
*     Get the data to reload
      REFRESH payload."v-javeda | MS2K948978
      SELECT * FROM zrest_mo_payload INTO TABLE payload WHERE messageid EQ message_id .
      IF sy-subrc EQ 0.
      ENDIF.
    ELSE.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
