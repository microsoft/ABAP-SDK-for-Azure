*&---------------------------------------------------------------------*
*& Report ZADF_ROTATE_SSL_CERT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zadf_validate_and_adjust_keys.

* Include for Global variables
INCLUDE zadf_validate_and_adj_keys_top .
* Include for selection screen
INCLUDE zadf_validate_and_adj_keys_sel .
* Include for class
INCLUDE zadf_validate_and_adj_keys_cls.

INITIALIZATION.
  CREATE OBJECT go_adj_keys.
* start of selection
START-OF-SELECTION.

* get the config tables
  go_adj_keys->read_config_tables( ).

* check the radio button "check keys"
  IF p_chk EQ abap_true.
* Check Access Keys
    go_adj_keys->check_access_keys( ).
  ELSEIF p_adj EQ abap_true.
* Adjust Access Keys
    go_adj_keys->adjust_access_keys( ).
  ENDIF.

* Submit program
  go_adj_keys->submit_ssf_adj_keys_program( ).
