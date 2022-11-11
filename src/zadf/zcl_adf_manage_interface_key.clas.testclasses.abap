CLASS lcl_adf_manage_interface_key_t DEFINITION DEFERRED.
CLASS zcl_adf_manage_interface_key DEFINITION LOCAL FRIENDS lcl_adf_manage_interface_key_t.

CLASS lcl_adf_manage_interface_key_t DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_adf_manage_interface_key.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: insert_key FOR TESTING.
    METHODS: insert_key_with_default_pse FOR TESTING.
    METHODS: insert_key_with_primary FOR TESTING.
    METHODS: insert_key_with_secondary FOR TESTING.
    METHODS: read_config_tables FOR TESTING.
    METHODS: read_key FOR TESTING.
    METHODS: read_key_with_default_pse FOR TESTING.
    METHODS: read_key_with_primary FOR TESTING.
    METHODS: read_key_with_secondary FOR TESTING.
ENDCLASS.       "lcl_Adf_Manage_Interface_Key_T


CLASS lcl_adf_manage_interface_key_t IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.

*    data iv_Interface_Id type zinterface_Id.
*
*    create object f_Cut
*      EXPORTING
*        IV_INTERFACE_ID = iv_Interface_Id.
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD insert_key.

*    data iv_Key type string.
*
*    f_Cut->insert_Key( iv_Key ).

  ENDMETHOD.


  METHOD insert_key_with_default_pse.
*
*    data iv_Key type string.
*
*    f_Cut->insert_Key_With_Default_Pse( iv_Key ).

  ENDMETHOD.


  METHOD insert_key_with_primary.

*    data iv_Key type string.
*
*    f_Cut->insert_Key_With_Primary( iv_Key ).

  ENDMETHOD.


  METHOD insert_key_with_secondary.

*    data iv_Key type string.
*
*    f_Cut->insert_Key_With_Secondary( iv_Key ).

  ENDMETHOD.


  METHOD read_config_tables.


*    f_Cut->read_Config_Tables(  ).

  ENDMETHOD.


  METHOD read_key.

*    data rv_Key type string.
*
*    rv_Key = f_Cut->read_Key(  ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Key
*      exp   = rv_Key          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Key'
**     level =
*    ).
  ENDMETHOD.


  METHOD read_key_with_default_pse.

*    data rv_Key type string.
*
*    rv_Key = f_Cut->read_Key_With_Default_Pse(  ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Key
*      exp   = rv_Key          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Key'
**     level =
*    ).
  ENDMETHOD.


  METHOD read_key_with_primary.

*    data rv_Key type string.
*
*    rv_Key = f_Cut->read_Key_With_Primary(  ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Key
*      exp   = rv_Key          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Key'
**     level =
*    ).
  ENDMETHOD.


  METHOD read_key_with_secondary.
*
*    data rv_Key type string.
*
*    rv_Key = f_Cut->read_Key_With_Secondary(  ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Key
*      exp   = rv_Key          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Key'
**     level =
*    ).
  ENDMETHOD.




ENDCLASS.
