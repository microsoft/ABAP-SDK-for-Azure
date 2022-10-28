class ZCX_ADF_MANAGE_ACCESS_KEYS definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_ADF_NO_AUTHORIZATION,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_NO_AUTHORIZATION .
  constants:
    begin of ZCX_ADF_START_PROGRAM_NO_AUTH,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_START_PROGRAM_NO_AUTH .
  constants:
    begin of ZCX_ADF_ABORT_IN_DEBUG,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_ABORT_IN_DEBUG .
  constants:
    begin of ZCX_ADF_PSE_NOT_FOUND,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'IV_APPLIC',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_PSE_NOT_FOUND .
  constants:
    begin of ZCX_ADF_CERT_NOT_FOUND,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'IV_PSE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_CERT_NOT_FOUND .
  constants:
    begin of ZCX_ADF_CERT_PARSE_ERROR,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_CERT_PARSE_ERROR .
  constants:
    begin of ZCX_ADF_XSTRING_CONV_ERROR,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_XSTRING_CONV_ERROR .
  constants:
    begin of ZCX_ADF_ENCRYPT_ERROR,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_ENCRYPT_ERROR .
  constants:
    begin of ZCX_ADF_DECRYPT_ERROR,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_DECRYPT_ERROR .
  constants:
    begin of ZCX_ADF_STRING_CONV_ERROR,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_STRING_CONV_ERROR .
  constants:
    begin of ZCX_ADF_APPLIC_NOT_FOUND,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_APPLIC_NOT_FOUND .
  constants:
    begin of ZCX_ADF_READ_DEST_APPLIC_ERR,
      msgid type symsgid value 'ZADF_SEC',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'IV_DESTINATION',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_ADF_READ_DEST_APPLIC_ERR .
  data IV_APPLIC type SSFAPPLSSL .
  data IV_PSE type SSFPSENAME .
  data IV_DESTINATION type RFCDEST .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !IV_APPLIC type SSFAPPLSSL optional
      !IV_PSE type SSFPSENAME optional
      !IV_DESTINATION type RFCDEST optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ADF_MANAGE_ACCESS_KEYS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->IV_APPLIC = IV_APPLIC .
me->IV_PSE = IV_PSE .
me->IV_DESTINATION = IV_DESTINATION .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
