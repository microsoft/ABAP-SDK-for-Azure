class ZCX_ADF_SERVICE_GRAPH definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants GENERAL_EXCEPTION type SOTR_CONC value '000D3A3BFDAD1EDDA1EDDDAC25FF70A6' ##NO_TEXT.
  data ERROR_RESPONSE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !ERROR_RESPONSE type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ADF_SERVICE_GRAPH IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->ERROR_RESPONSE = ERROR_RESPONSE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
