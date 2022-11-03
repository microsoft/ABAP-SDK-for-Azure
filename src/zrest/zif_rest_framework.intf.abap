interface ZIF_REST_FRAMEWORK
  public .


  methods EXECUTE
    importing
      !METHOD type CHAR20 default 'GET'
      !IO_ENTITY type ref to IF_REST_ENTITY optional
      !ASYNC type ABAP_BOOL
      !IS_RETRY type CHAR1
      !MESSAGEID type GUID_16 optional
      !RETRY_COUNT type ZQ_COUNTE optional
    returning
      value(RESPONSE) type ref to IF_REST_ENTITY .
  methods SET_STRING_BODY
    importing
      !BODY type STRING .
  methods SET_BINARY_BODY
    importing
      !BODY type XSTRING .
  methods SET_URI
    importing
      !URI type STRING .
  methods SET_REQUEST_HEADER
    importing
      !IV_NAME type STRING
      !IV_VALUE type STRING .
  methods SET_REQUEST_HEADERS
    importing
      !IT_HEADER_FIELDS type TIHTTPNVP .

endinterface.
