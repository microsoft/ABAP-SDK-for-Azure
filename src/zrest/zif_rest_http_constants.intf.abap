INTERFACE zif_rest_http_constants
  PUBLIC .

  CONSTANTS c_http_method_head    TYPE zinterface_method VALUE 'HEAD' ##NO_TEXT.
  CONSTANTS c_http_method_options TYPE zinterface_method VALUE 'OPTIONS' ##NO_TEXT.
  CONSTANTS c_http_method_get     TYPE zinterface_method VALUE 'GET' ##NO_TEXT.
  CONSTANTS c_http_method_put     TYPE zinterface_method VALUE 'PUT' ##NO_TEXT.
  CONSTANTS c_http_method_post    TYPE zinterface_method VALUE 'POST' ##NO_TEXT.
  CONSTANTS c_http_method_delete  TYPE zinterface_method VALUE 'DELETE' ##NO_TEXT.

ENDINTERFACE.
