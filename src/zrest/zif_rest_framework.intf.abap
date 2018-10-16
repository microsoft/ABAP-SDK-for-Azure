INTERFACE zif_rest_framework
  PUBLIC .


  METHODS execute
    IMPORTING
      !method         TYPE zinterface_method DEFAULT zif_rest_http_constants=>c_http_method_get
      !io_entity      TYPE REF TO if_rest_entity OPTIONAL
      !async          TYPE abap_bool
      !is_retry       TYPE char1
      !messageid      TYPE guid_16 OPTIONAL
      !retry_count    TYPE zq_counte OPTIONAL
    RETURNING
      VALUE(response) TYPE REF TO if_rest_entity .
  METHODS set_string_body
    IMPORTING
      !body TYPE string .
  METHODS set_binary_body
    IMPORTING
      !body TYPE xstring .
  METHODS set_uri
    IMPORTING
      !uri TYPE string .
  METHODS set_request_header
    IMPORTING
      !iv_name  TYPE string
      !iv_value TYPE string .
  METHODS set_request_headers
    IMPORTING
      !it_header_fields TYPE tihttpnvp .

ENDINTERFACE.
