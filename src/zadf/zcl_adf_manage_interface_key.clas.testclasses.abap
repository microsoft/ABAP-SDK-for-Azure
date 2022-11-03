class lcl_Adf_Manage_Interface_Key_T definition deferred.
class zcl_Adf_Manage_Interface_Key definition local friends lcl_Adf_Manage_Interface_Key_T.

class lcl_Adf_Manage_Interface_Key_T definition for testing
  duration short
  risk level harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Adf_Manage_Interface_Key_T
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ADF_MANAGE_INTERFACE_KEY
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  private section.
    data:
      f_Cut type ref to zcl_Adf_Manage_Interface_Key.  "class under test

    class-methods: class_Setup.
    class-methods: class_Teardown.
    methods: setup.
    methods: teardown.
    methods: insert_Key for testing.
    methods: insert_Key_With_Default_Pse for testing.
    methods: insert_Key_With_Primary for testing.
    methods: insert_Key_With_Secondary for testing.
    methods: read_Config_Tables for testing.
    methods: read_Key for testing.
    methods: read_Key_With_Default_Pse for testing.
    methods: read_Key_With_Primary for testing.
    methods: read_Key_With_Secondary for testing.
endclass.       "lcl_Adf_Manage_Interface_Key_T


class lcl_Adf_Manage_Interface_Key_T implementation.

  method class_Setup.



  endmethod.


  method class_Teardown.



  endmethod.


  method setup.

*    data iv_Interface_Id type zinterface_Id.
*
*    create object f_Cut
*      EXPORTING
*        IV_INTERFACE_ID = iv_Interface_Id.
  endmethod.


  method teardown.



  endmethod.


  method insert_Key.

*    data iv_Key type string.
*
*    f_Cut->insert_Key( iv_Key ).

  endmethod.


  method insert_Key_With_Default_Pse.
*
*    data iv_Key type string.
*
*    f_Cut->insert_Key_With_Default_Pse( iv_Key ).

  endmethod.


  method insert_Key_With_Primary.

*    data iv_Key type string.
*
*    f_Cut->insert_Key_With_Primary( iv_Key ).

  endmethod.


  method insert_Key_With_Secondary.

*    data iv_Key type string.
*
*    f_Cut->insert_Key_With_Secondary( iv_Key ).

  endmethod.


  method read_Config_Tables.


*    f_Cut->read_Config_Tables(  ).

  endmethod.


  method read_Key.

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
  endmethod.


  method read_Key_With_Default_Pse.

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
  endmethod.


  method read_Key_With_Primary.

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
  endmethod.


  method read_Key_With_Secondary.
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
  endmethod.




endclass.
