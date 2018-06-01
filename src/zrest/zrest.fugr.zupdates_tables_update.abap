FUNCTION ZUPDATES_TABLES_UPDATE.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PAYLOAD) TYPE  ZREST_MO_PAYLOAD
*"     VALUE(MONITOR) TYPE  ZREST_MONITOR
*"     VALUE(RETRY) TYPE  ZREST_RETRIES
*"----------------------------------------------------------------------
* Update the database for further processign and analysis
*"----------------------------------------------------------------------*
* Update the payload table
  IF  payload IS NOT INITIAL.
    MODIFY zrest_mo_payload FROM payload.
    clear lwa_payload.
  ENDIF.
* Update the log table
  IF monitor IS NOT INITIAL.
    MODIFY zrest_monitor FROM monitor.
    clear gwa_log.
  ENDIF.
* Retry log
  IF monitor IS NOT INITIAL.
    MODIFY zrest_retries FROM retry.
  ENDIF.
ENDFUNCTION.
