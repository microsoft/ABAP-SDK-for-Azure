class ZCX_ADF_SERVICE_BLOB definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants RESTAPI_RESPONSE_NOT_FOUND type SOTR_CONC value '000D3A3BFDAD1EDCADC77D22B8ED5070' ##NO_TEXT.
  data INTERFACE_ID type ZINTERFACE_ID read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !INTERFACE_ID type ZINTERFACE_ID optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ADF_SERVICE_BLOB IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->INTERFACE_ID = INTERFACE_ID .
  endmethod.
ENDCLASS.
