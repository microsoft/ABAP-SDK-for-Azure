class ZCX_ADF_SERVICE_FILES definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants STORAGE_ACCOUNT_NOT_SET type SOTR_CONC value '000D3A3BFDAD1EDDA7809F710AB270AB' ##NO_TEXT.
  constants FILE_SHARE_NOT_SET type SOTR_CONC value '000D3A3BFDAD1EDDA7809F710AB290AB' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ADF_SERVICE_FILES IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
  endmethod.
ENDCLASS.
