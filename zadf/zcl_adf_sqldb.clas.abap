class ZCL_ADF_SQLDB definition
  public
  create public .

public section.

  data GV_DBCON type DBCON_NAME .
  data GO_DBCON type ref to CL_SQL_CONNECTION .

  methods CONSTRUCTOR
    importing
      value(IV_DBCON) type DBCON_NAME
    raising
      CX_SQL_EXCEPTION
      ZCX_ADF_SQLDB .
  methods INSERT_TABLE
    importing
      value(IV_TABNAME) type TABNAME
      value(IT_DATA) type ANY TABLE
    raising
      CX_SQL_EXCEPTION
      ZCX_ADF_SQLDB .
  methods INSERT_ROW
    importing
      value(IV_TABNAME) type TABNAME
      value(IV_DATA) type DATA
    raising
      CX_SQL_EXCEPTION
      ZCX_ADF_SQLDB .
  methods DELETE_TABLE
    importing
      value(IV_TABNAME) type TABNAME
      value(IT_DATA) type ANY TABLE
    raising
      CX_SQL_EXCEPTION
      ZCX_ADF_SQLDB .
  methods DELETE_ROW
    importing
      value(IV_TABNAME) type TABNAME
      value(IV_DATA) type DATA
    raising
      CX_SQL_EXCEPTION
      ZCX_ADF_SQLDB .
  methods MODIFY_TABLE
    importing
      value(IV_TABNAME) type TABNAME
      value(IT_DATA) type ANY TABLE
    raising
      CX_SQL_EXCEPTION
      ZCX_ADF_SQLDB .
  methods MODIFY_ROW
    importing
      value(IV_TABNAME) type TABNAME
      value(IV_DATA) type DATA
    raising
      CX_SQL_EXCEPTION
      ZCX_ADF_SQLDB .
  methods COMMIT_DB
    raising
      CX_SQL_EXCEPTION
      ZCX_ADF_SQLDB .
  methods GENERATE_CREATE_STATEMENT
    importing
      value(IV_TABNAME) type TABNAME
    returning
      value(RV_STATEMENT) type STRING .
  methods INSERT_MULTIPLE_ROWS
    importing
      value(IV_TABNAME) type TABNAME
      value(IT_DATA) type ANY TABLE
    returning
      value(RV_ROWS) type INT4 .
  methods DROP_TABLE
    importing
      value(IV_TABNAME) type TABNAME .
  methods CREATE_TABLE
    importing
      value(IV_TABNAME) type TABNAME .
protected section.
private section.

  methods CHECK_CONNECTION
    raising
      CX_SQL_EXCEPTION
      ZCX_ADF_SQLDB .
  methods OPEN_CONNECTION .
  methods CLOSE_CONNECTION .
  methods GENERATE_INSERT_STATEMENT
    importing
      value(IV_TABNAME) type TABNAME
    returning
      value(RV_STATEMENT) type STRING .
  methods GET_DDIC_FIELDS
    importing
      value(IV_TABNAME) type TABNAME
    returning
      value(RT_FIELDS) type ZADF_TT_DDFIELD .
  methods UPDATE_MULTIPLE_ROWS .
  methods SELECT_MULTIPLE_ROWS .
  methods DELETE_MULTIPLE_ROWS .
  methods INSERT_SINGLE_ROW .
  methods SELECT_SINGLE_ROW .
  methods UPDATE_SINGLE_ROW .
ENDCLASS.



CLASS ZCL_ADF_SQLDB IMPLEMENTATION.


METHOD check_connection.

  open_connection( ).
  close_connection( ).

ENDMETHOD.


METHOD close_connection.

  DATA : lv_dbcon TYPE dbcon_name.

  lv_dbcon = go_dbcon->get_con_name( ).
  CHECK lv_dbcon <> cl_sql_connection=>c_default_connection.
  go_dbcon->close( ).

ENDMETHOD.


method COMMIT_DB.
endmethod.


METHOD constructor.

  gv_dbcon = iv_dbcon.
*  check_connection( ).

ENDMETHOD.


METHOD create_table.

  DATA: lo_sqlerr_ref TYPE REF TO cx_sql_exception,
        lv_stmt       TYPE string,
        lo_stmt_ref   TYPE REF TO cl_sql_statement.

  IF go_dbcon IS NOT BOUND.
    open_connection( ).
  ENDIF.

  lo_stmt_ref = go_dbcon->create_statement( ).
  lv_stmt = generate_create_statement( iv_tabname = iv_tabname ).
  lo_stmt_ref->execute_ddl( lv_stmt ).

ENDMETHOD.


method DELETE_MULTIPLE_ROWS.
endmethod.


METHOD delete_row.

  INSERT (iv_tabname) CLIENT SPECIFIED CONNECTION (gv_dbcon)
         FROM iv_data.

ENDMETHOD.


METHOD delete_table.

  DELETE (iv_tabname) CLIENT SPECIFIED CONNECTION (gv_dbcon)
          FROM TABLE it_data.

ENDMETHOD.


METHOD drop_table.

  DATA: lv_stmt     TYPE string,
        lv_stmt_ref TYPE REF TO cl_sql_statement.

  IF go_dbcon IS NOT BOUND.
    open_connection( ).
  ENDIF.

* create a statement object
  lv_stmt_ref = go_dbcon->create_statement( ).

* create the statement string
  CONCATENATE 'drop table' iv_tabname
    INTO lv_stmt SEPARATED BY space.

* execute the DDL command
  lv_stmt_ref->execute_ddl( lv_stmt ).


ENDMETHOD.


METHOD generate_create_statement.

  DATA : lt_ddflds TYPE zadf_tt_ddfield,
         ls_ddflds TYPE ddfield,
         lv_stmt   TYPE string,
         lv_strlen TYPE i,
         lt_dbflds TYPE TABLE OF dbftype,
         ls_dbflds TYPE dbftype,
         lv_field  TYPE string,
         lv_type   TYPE string,
         lv_length TYPE string,
         lv_nullable TYPE string.

  lt_ddflds = get_ddic_fields( iv_tabname ).

  CALL FUNCTION 'DB_MAP_DDFIELDS'
    EXPORTING
      dbsys        = sy-dbsys
      with_check   = ' '
      with_string  = 'X'
      nullable     = 'X'
      tabname      = ''
    TABLES
      dbftypes     = lt_dbflds
      ddfields     = lt_ddflds
    EXCEPTIONS
      not_mappable = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_dbflds INTO ls_dbflds.
    CONCATENATE '[' ls_dbflds-name ']' INTO lv_field.
    SPLIT ls_dbflds-string AT '(' INTO lv_type lv_length.
    CONCATENATE '[' lv_type ']' '(' lv_length INTO lv_type.
    IF ls_dbflds-nullable IS INITIAL.
      lv_nullable = 'NOT NULL'.
    ELSE.
      lv_nullable = 'NULL'.
    ENDIF.
    CONCATENATE lv_stmt lv_field lv_type lv_nullable ','
                INTO lv_stmt SEPARATED BY space.
  ENDLOOP.

  lv_strlen = strlen( lv_stmt ).
  lv_strlen = lv_strlen - 1.
  lv_stmt = lv_stmt+0(lv_strlen).

  CONCATENATE '(' lv_stmt ')' INTO lv_stmt.
  CONCATENATE 'CREATE TABLE' iv_tabname lv_stmt
              INTO lv_stmt SEPARATED BY space.

  rv_statement = lv_stmt.

ENDMETHOD.


METHOD generate_insert_statement.

  DATA : lt_ddflds TYPE zadf_tt_ddfield,
         ls_ddflds TYPE ddfield,
         lv_stmt   TYPE string,
         lv_strlen TYPE i.

  lt_ddflds = get_ddic_fields( iv_tabname ).

  LOOP AT lt_ddflds INTO ls_ddflds.
    CONCATENATE lv_stmt '?,' INTO lv_stmt
                SEPARATED BY space.
  ENDLOOP.
  lv_strlen = strlen( lv_stmt ).
  lv_strlen = lv_strlen - 1.
  lv_stmt = lv_stmt+0(lv_strlen).

  CONCATENATE '(' lv_stmt ')' INTO lv_stmt.
  CONCATENATE 'INSERT INTO' iv_tabname 'VALUES' lv_stmt
              INTO lv_stmt SEPARATED BY space.

  rv_statement = lv_stmt.

ENDMETHOD.


METHOD get_ddic_fields.

  DATA: lv_tabname        TYPE tabname,
        lv_dbsys          TYPE sy-dbsys,
        lt_tabflds        TYPE TABLE OF ddfield.


  lv_dbsys = 'MSS'.
  lv_tabname = iv_tabname.
  TRANSLATE lv_tabname TO UPPER CASE.

  CALL FUNCTION 'DD_NAMETAB_TO_DDFIELDS'
    EXPORTING
      keyfields = 'X'
      nullable  = 'X'
      tabname   = lv_tabname
    TABLES
      ddfields  = lt_tabflds.

  rt_fields = lt_tabflds.

ENDMETHOD.


METHOD insert_multiple_rows.

  DATA : lv_stmt  TYPE string,
         lo_stmt_ref TYPE REF TO cl_sql_statement,
         lo_table TYPE REF TO data.

  IF go_dbcon IS NOT BOUND.
    open_connection( ).
  ENDIF.

  lo_stmt_ref = go_dbcon->create_statement( ).

  GET REFERENCE OF it_data INTO lo_table.
  lo_stmt_ref->set_param_table( lo_table ).

  lv_stmt = generate_insert_statement( iv_tabname ).
  rv_rows = lo_stmt_ref->execute_update( lv_stmt ).

ENDMETHOD.


METHOD insert_row.

  INSERT (iv_tabname) CLIENT SPECIFIED CONNECTION (gv_dbcon)
                      FROM iv_data.

ENDMETHOD.


method INSERT_SINGLE_ROW.
endmethod.


METHOD insert_table.

  INSERT (iv_tabname) CLIENT SPECIFIED CONNECTION (gv_dbcon)
         FROM TABLE it_data.

ENDMETHOD.


METHOD modify_row.

  MODIFY (iv_tabname) CLIENT SPECIFIED CONNECTION (gv_dbcon)
         FROM iv_data.

ENDMETHOD.


METHOD modify_table.

  MODIFY (iv_tabname) CLIENT SPECIFIED CONNECTION (gv_dbcon)
         FROM TABLE it_data.

ENDMETHOD.


METHOD open_connection.

  IF gv_dbcon IS INITIAL.
    CREATE OBJECT go_dbcon.
  ELSE.
    go_dbcon = cl_sql_connection=>get_connection( gv_dbcon ).
  ENDIF.

ENDMETHOD.


method SELECT_MULTIPLE_ROWS.
endmethod.


method SELECT_SINGLE_ROW.
endmethod.


method UPDATE_MULTIPLE_ROWS.
endmethod.


method UPDATE_SINGLE_ROW.
endmethod.
ENDCLASS.
