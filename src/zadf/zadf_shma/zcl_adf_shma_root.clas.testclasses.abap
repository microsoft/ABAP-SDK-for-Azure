*"* use this source file for your ABAP unit test classes
class zcl_Adf_Shma_Root_Tst definition deferred.
class zcl_Adf_Shma_Root definition local friends zcl_Adf_Shma_Root_Tst.

class zcl_Adf_Shma_Root_Tst definition for testing
  duration short
  risk level harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>zcl_Adf_Shma_Root_Tst
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ADF_SHMA_ROOT
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
      f_Cut type ref to zcl_Adf_Shma_Root.  "class under test

    class-methods: class_Setup.
    class-methods: class_Teardown.
    methods: setup.
    methods: teardown.
    methods: build for testing.
    methods: call_Stack_Check for testing.
    methods: get_Sas_Key for testing.
    methods: set_Sas_Key for testing.
endclass.       "zcl_Adf_Shma_Root_Tst


class zcl_Adf_Shma_Root_Tst implementation.

  method class_Setup.



  endmethod.


  method class_Teardown.



  endmethod.


  method setup.


    create object f_Cut.
  endmethod.


  method teardown.



  endmethod.


  method build.

    data inst_Name type shm_Inst_Name.
    data invocation_Mode type shm_Constr_Invocation_Mode.

    zcl_Adf_Shma_Root=>if_Shm_Build_Instance~build(
*       INST_NAME = inst_Name
*       INVOCATION_MODE = invocation_Mode
    ).

  endmethod.


  method call_Stack_Check.


*    f_Cut->call_Stack_Check(  ).

  endmethod.


  method get_Sas_Key.

    data iv_Interface_Id type zinterface_Id.
    data ev_Sas_Key type zsas_Azure.

*    ev_Sas_Key = f_Cut->get_Sas_Key( iv_Interface_Id ).

    cl_Abap_Unit_Assert=>assert_Equals(
      act   = ev_Sas_Key
      exp   = ev_Sas_Key          "<--- please adapt expected value
    " msg   = 'Testing value ev_Sas_Key'
*     level =
    ).
  endmethod.


  method set_Sas_Key.

    data is_Adf_Config type zadf_Config.

*    f_Cut->set_Sas_Key( is_Adf_Config ).

  endmethod.




endclass.
