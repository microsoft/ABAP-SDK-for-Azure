
class lcl_O365_Service_Factory_T definition for testing
  duration short
  risk level harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_O365_Service_Factory_T
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_O365_SERVICE_FACTORY
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
      f_Cut type ref to zcl_O365_Service_Factory.  "class under test

    class-methods: class_Setup.
    class-methods: class_Teardown.
    methods: setup.
    methods: teardown.
    methods: create for testing.
endclass.       "lcl_O365_Service_Factory_T


class lcl_O365_Service_Factory_T implementation.

  method class_Setup.



  endmethod.


  method class_Teardown.



  endmethod.


  method setup.


*    create object f_Cut.
  endmethod.


  method teardown.



  endmethod.


  method create.

*    data iv_Interface_Id type zinterface_Id.
*    data ro_Service type ref to zcl_O365_Service.
*
*    ro_Service = zcl_O365_Service_Factory=>create( iv_Interface_Id ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = ro_Service
*      exp   = ro_Service          "<--- please adapt expected value
*    " msg   = 'Testing value ro_Service'
**     level =
*    ).
  endmethod.




endclass.
