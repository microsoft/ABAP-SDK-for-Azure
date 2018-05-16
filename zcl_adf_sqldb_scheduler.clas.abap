class ZCL_ADF_SQLDB_SCHEDULER definition
  public
  final
  create public .

public section.

  data GV_DBCON type DBCON_NAME .
  data GV_TABNAME type TABNAME .

  methods CONSTRUCTOR
    importing
      value(IV_DBCON) type DBCON_NAME
      value(IV_TABNAME) type TABNAME .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ADF_SQLDB_SCHEDULER IMPLEMENTATION.


METHOD constructor.

  gv_dbcon = iv_dbcon.
  gv_tabname = iv_tabname.

ENDMETHOD.
ENDCLASS.
