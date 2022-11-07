class ZCX_ADF_SERVICE_FILES definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants STORAGE_ACCOUNT_NOT_SET type SOTR_CONC value '000D3AE393151EDABC9E4EDC6ECF1001' ##NO_TEXT.
  constants FILE_SHARE_NOT_SET type SOTR_CONC value '000D3A3BFDAD1EDCA2CE2E055D019067' ##NO_TEXT.
  data FORMAT_TYPE type ZADF_FORMAT_TYPE .
  data INTERFACE_ID type ZINTERFACE_ID .
  data TEXT type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !FORMAT_TYPE type ZADF_FORMAT_TYPE optional
      !INTERFACE_ID type ZINTERFACE_ID optional
      !TEXT type STRING optional .
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
me->FORMAT_TYPE = FORMAT_TYPE .
me->INTERFACE_ID = INTERFACE_ID .
me->TEXT = TEXT .
  endmethod.
ENDCLASS.
