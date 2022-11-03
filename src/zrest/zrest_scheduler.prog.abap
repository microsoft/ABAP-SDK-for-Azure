REPORT zrest_scheduler.
*----------------------------------------------------------------------------------------------*
* Programmer                                                      Sasidhar Puranam             *
*----------------------------------------------------------------------------------------------*
* REST Scheduler reads all messages which are not 200 OK and resubmits them. The number of times
* retried maintained is controlled by config
*----------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04|28|2016|V-DEVEER  |2163894 | DGDK903413 | Authorization Check
* 10|18|2017|V-LAUPPA  |2636456 | DGDK907421 | Performace improvement
* 03|13|2019|V-SRKURA  |4364787 | DGDK910813 | REST Framework changes
* 08|24|2022|V-GORAJESH|        | SMTK907903 | Fixing VF Errors        *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------------------------------*
* SELECTION SCREEN
*-----------------------------------------------------------------------------------------------*
TABLES zrest_config.
*--Begin of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-002.
PARAMETERS: p_retry  TYPE char1 RADIOBUTTON GROUP rad DEFAULT 'X',
            p_delete TYPE char1 RADIOBUTTON GROUP rad.
SELECTION-SCREEN: END   OF BLOCK blk1.
*--End of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
SELECTION-SCREEN: BEGIN OF BLOCK blk0 WITH FRAME TITLE text-001 .
SELECT-OPTIONS: s_id FOR zrest_config-interface_id.
SELECTION-SCREEN: END   OF BLOCK blk0.
DATA: lt_interface_id TYPE ztt_interface_id,                 "Added by V-SRKURA 4364787/DGDK910813
      ls_interface_id TYPE zts_interface_id.                 "Added by V-SRKURA 4364787/DGDK910813
*----------------------------------------------------------------------------------------------*
CLASS lcl_process_data DEFINITION.
  PUBLIC SECTION.
    METHODS cleanse_records.
    METHODS get_process.
ENDCLASS.
*----------------------------------------------------------------------------------------------*
CLASS lcl_process_data IMPLEMENTATION.
*----------------------------------------------------------------------------------------------*
* Process the ones which are waiting in 0 state in the payload table                           *
*----------------------------------------------------------------------------------------------*
  METHOD get_process.
    DATA : rest_utility TYPE REF TO zcl_rest_utility_class.
*--Begin of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
*   DATA(it_unprocessed_data) = zcl_rest_utility_class=>unprocessed_data( ).
    DATA(it_unprocessed_data) = zcl_rest_utility_class=>unprocessed_data(
                                EXPORTING interface_id = lt_interface_id  ).
*--End of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
    DELETE it_unprocessed_data WHERE interface_id NOT IN s_id.

* Check if table is initial , if yes exit.
    IF it_unprocessed_data IS INITIAL.
      WRITE:/10 '*******************************************************************************'.
      WRITE:/10 '*************Nothing to be processed              *****************************'.
      WRITE:/10 '*******************************************************************************'.
      EXIT.
    ELSE.
*   Loop at unprocessed_data and retry.
      LOOP AT it_unprocessed_data INTO DATA(wa_unprocessed_data).
*   Create utility object
        CREATE OBJECT rest_utility.
*   Call the retry method and set async to false , retry to true.
        TRY .
            CALL METHOD rest_utility->retry(
              EXPORTING
                message_id     = wa_unprocessed_data-messageid
                method         = wa_unprocessed_data-method
                from_scheduler = 'X' ).
          CATCH zcx_interace_config_missing .
            WRITE:/10 '*******************************************************************************'.
            WRITE:/10 'Processed Message ID' , 30 wa_unprocessed_data-messageid  , 65 'Failed'.
            WRITE:/10 '*******************************************************************************'.
          CATCH zcx_http_client_failed .
            WRITE:/10 '*******************************************************************************'.
            WRITE:/10 'Processed Message ID' , 30 wa_unprocessed_data-messageid  , 65 'Failed'.
            WRITE:/10 '*******************************************************************************'.
        ENDTRY.
*   Clear the object reference
        FREE rest_utility.
*        WRITE:/10 '*******************************************************************************'.
*        WRITE:/10 'Processed Message ID' , 30 wa_unprocessed_data-messageid  , 65 'Failed'.
*        WRITE:/10 '*******************************************************************************'.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
*----------------------------------------------------------------------------------------------*
* Apply the retention period  - 30 days for succesful and errors stay forever !                *
*----------------------------------------------------------------------------------------------*
  METHOD cleanse_records.
    TRY.
*--Begin of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
*       zcl_rest_utility_class=>reset_all_data( ).
        zcl_rest_utility_class=>reset_all_data(
                                EXPORTING interface_id = lt_interface_id ).
*--End of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
*    Changes for Authorization VSTF # 2163894 | DGDK903413

*Begin of Changes by V-gorajesh 08/24/2022 SMTK907903
*      CATCH zcx_http_client_failed. "commented by V-gorajesh 08/16/2019
      CATCH zcx_http_client_failed into data(lv_ref).
      data(lv_msgtxt) = lv_ref->GET_TEXT( ).
      IF NOT lv_msgtxt IS INITIAL.
      WRITE:/10 lv_msgtxt.
      ENDIF.
*End of Changes by V-gorajesh 08/24/2022 SMTK907903
    ENDTRY.
*    End of changes VSTF # 2163894 | DGDK903413
    WRITE:/10 '*******************************************************************************'.
    WRITE:/10 '*************Messages older than 30 days cleansed******************************'.
    WRITE:/10 '*******************************************************************************'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------------------------------*
  DATA lc_process_object TYPE REF TO lcl_process_data.
  CREATE OBJECT lc_process_object.
*----------------------------------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------------------------------*
*--Begin of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
  LOOP AT s_id.
    ls_interface_id = s_id-low.
    APPEND ls_interface_id TO lt_interface_id.
    CLEAR ls_interface_id.
  ENDLOOP.
*--End of Changes by V-SRKURA 03/13/2019 4364787/DGDK910813
  IF p_delete = abap_true.                     "Added by V-SRKURA 4364787/DGDK910813
* Cleanse the records older than 30 days
    lc_process_object->cleanse_records( ).
  ELSEIF p_retry = abap_true.                  "Added by V-SRKURA 4364787/DGDK910813
* Reprocess the error and records which are waiting to be executed.
    lc_process_object->get_process( ).
  ENDIF.                                       "Added by V-SRKURA 4364787/DGDK910813
