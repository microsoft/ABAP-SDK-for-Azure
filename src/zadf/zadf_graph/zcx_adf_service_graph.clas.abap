class ZCX_ADF_SERVICE_GRAPH definition
  public
  inheriting from ZCX_ADF_SERVICE
  final
  create public .

public section.

  constants GENERAL_EXCEPTION type SOTR_CONC value '6805CA2480681EDAA1C7B394C05580BC' ##NO_TEXT.
  data ERROR_RESPONSE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !FORMAT_TYPE type ZADF_FORMAT_TYPE optional
      !INTERFACE_ID type ZINTERFACE_ID optional
      !ERROR_RESPONSE type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ADF_SERVICE_GRAPH IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
FORMAT_TYPE = FORMAT_TYPE
INTERFACE_ID = INTERFACE_ID
.
me->ERROR_RESPONSE = ERROR_RESPONSE .
  endmethod.
ENDCLASS.
