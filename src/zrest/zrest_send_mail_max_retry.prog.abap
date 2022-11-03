*&---------------------------------------------------------------------*
*& Report  ZREST_SEND_MAIL_MAX_RETRY
*&
*&----------------------------------------------------------------------*
*& Report   ZREST_SEND_MAIL_MAX_RETRY                                   *
*& Date     06/20/2016                                                  *
*& Purpose: Send Email if the HTTP Call interface has reached Max  Retry*
*************************************************************************
*                           MODIFICATION LOG                            *
*************************************************************************
*&  Transport    Author       Date        VSTF     Description          *
*   MS2K947391  v-javeda    11/16/2016  2236131    Sel screen addition  *
*   MS2K949218  v-javeda    12/23/2016  2236131    version capture to   *
*                                                      sync             *
*   SMTK907897  V-ASHOKM1   09/28/2022            Fixing VF Errors      *
*************************************************************************
REPORT zrest_send_mail_max_retry.

DATA: lo_rest        TYPE REF TO zcl_rest_utility_class,
      lt_zrt_monitor TYPE zrt_monitor,
      lt_retry_email TYPE ztt_rest_retry_limi,
      lw_retry_email TYPE zrest_retry_limi.

SELECTION-SCREEN: BEGIN OF BLOCK blk0 WITH FRAME TITLE text-001 .
SELECT-OPTIONS: s_date FOR sy-datum NO INTERVALS NO-EXTENSION  DEFAULT sy-datum,
                s_time FOR sy-uzeit NO INTERVALS NO-EXTENSION  DEFAULT sy-uzeit.
SELECTION-SCREEN: END   OF BLOCK blk0.

CREATE OBJECT lo_rest .

START-OF-SELECTION.

  TRY.
      CALL METHOD lo_rest->retry_limit_exceeded
        IMPORTING
          et_retry_report = lt_retry_email.
*    Begin of changes by V-ASHOKM1 ++  SMTK907897
    CATCH zcx_rest INTO DATA(lo_zcxrest).
      DATA(lv_text) = lo_zcxrest->get_text( ).
      IF lv_text IS NOT INITIAL.
        MESSAGE lv_text TYPE 'E'.
      ENDIF.
*    End of changes by V-ASHOKM1 ++   SMTK907897

  ENDTRY.
  IF lt_retry_email IS NOT INITIAL.
    COMMIT WORK.
    WRITE AT: /5'INTERFACE_ID', 20'METHOD', 30'MAX_RETRY', 40'ZMESSAGEID', 90'ZUSER',
               100'ZEXEDATE', 115'ZEXETIME', 125'DESTINATION', 155'MESSAGE'.
    SKIP 1.
    LOOP AT lt_retry_email INTO lw_retry_email.
      WRITE AT: /5 lw_retry_email-interface_id,
                20 lw_retry_email-method,
                30 lw_retry_email-max_retry,
                40 lw_retry_email-zmessageid,
                90 lw_retry_email-zuser,
                100 lw_retry_email-zexedate,
                115 lw_retry_email-zexetime,
                125 lw_retry_email-destination,
                155 lw_retry_email-message.

    ENDLOOP.
  ELSE.
    WRITE AT: /5'No Records are found'(001).
  ENDIF.
