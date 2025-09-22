FUNCTION-POOL zrest                      MESSAGE-ID sv.

* INCLUDE LZRESTD...                         " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzrestt00                               . "view rel. data dcl.

DATA : gwa_log TYPE zrest_monitor.
DATA : tab                    TYPE tihttpnvp,
       request_header_string  TYPE string,
       response_header_string TYPE string,
       lv_string TYPE string.
DATA : object TYPE REF TO  zcl_rest_framework.
DATA : lwa_payload TYPE zrest_mo_payload.
DATA :  gt_log TYPE STANDARD TABLE OF zrest_monitor.
DATA : lv_content_type_req TYPE string.
DATA : lv_content_type_res TYPE string.
DATA : retry_log TYPE zrest_retries.
