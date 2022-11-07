*&---------------------------------------------------------------------*
*&  Include  ZADF_VALIDATE_AND_ADJ_KEYS_TOP
*&---------------------------------------------------------------------*

TABLES : zadf_config.

DATA: lt_adf_config  TYPE STANDARD TABLE OF zadf_config,
      ls_adf_config  TYPE zadf_config,
      lt_rest_config TYPE STANDARD TABLE OF zrest_config,
      ls_rest_config TYPE zrest_config,
      lv_applic      TYPE ssfapplssl,
      lv_success     TYPE boolean,
      lv_message     TYPE string.

DATA : lo_keys TYPE REF TO zcl_adf_manage_access_keys.

CLASS lcl_adf_validate_and_adj_keys DEFINITION DEFERRED.
DATA : go_adj_keys TYPE REF TO lcl_adf_validate_and_adj_keys.
