*&---------------------------------------------------------------------*
*&  Include  ZSSF_VALIDATE_AND_ADJ_KEYS_TOP
*&---------------------------------------------------------------------*

TABLES : zssf_data.

DATA: lt_ssf_data TYPE STANDARD TABLE OF zssf_data,
      ls_ssf_data TYPE zssf_data,
      lv_applic   TYPE ssfapplssl,
      lv_success  TYPE boolean,
      lv_message  TYPE string,
      gv_applic   TYPE ssfapplssl.

DATA : lo_keys TYPE REF TO zcl_adf_manage_access_keys.

CLASS lcl_ssf_validate_and_adj_keys DEFINITION DEFERRED.
DATA : go_adj_keys TYPE REF TO lcl_ssf_validate_and_adj_keys.
