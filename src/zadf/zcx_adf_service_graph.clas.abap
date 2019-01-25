class ZCX_ADF_SERVICE_GRAPH definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants ERROR_DURING_USER_CREATION type SOTR_CONC value '02FBA84C29D01EE8B8B31E2281D2DC61' ##NO_TEXT.
  data ERROR_MESSAGE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !ERROR_MESSAGE type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ADF_SERVICE_GRAPH IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->ERROR_MESSAGE = ERROR_MESSAGE .
  endmethod.
ENDCLASS.
