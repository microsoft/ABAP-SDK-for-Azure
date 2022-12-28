class lcl_Adf_Service_Reprocess_T definition deferred.
class zcl_Adf_Service_Reprocess definition local friends lcl_Adf_Service_Reprocess_T.

class lcl_Adf_Service_Reprocess_T definition for testing
  duration short
  risk level harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Adf_Service_Reprocess_T
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ADF_SERVICE_REPROCESS
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
      f_Cut type ref to zcl_Adf_Service_Reprocess.  "class under test

    class-methods: class_Setup.
    class-methods: class_Teardown.
    methods: setup.
    methods: teardown.
    methods: call_Stack_Check for testing.
    methods: generate_Auth_Token for testing.
    methods: get_Token_Blob for testing.
    methods: get_Token_Cosmosdb for testing.
endclass.       "lcl_Adf_Service_Reprocess_T


class lcl_Adf_Service_Reprocess_T implementation.

  method class_Setup.



  endmethod.


  method class_Teardown.



  endmethod.


  method setup.


*    create object f_Cut.
  endmethod.


  method teardown.



  endmethod.


  method call_Stack_Check.


*    f_Cut->call_Stack_Check(  ).

  endmethod.


  method generate_Auth_Token.
*
*    data iv_Interface_Id type zinterface_Id.
*    data iv_Uri type zuri.
*    data iv_Interface_Type type zazure_Dest.
*    data rv_Token type string.
*    data rv_Date type string.
*
*    f_Cut->generate_Auth_Token(
*      EXPORTING
*        IV_INTERFACE_ID = iv_Interface_Id
**       IV_URI = iv_Uri
*        IV_INTERFACE_TYPE = iv_Interface_Type
**     IMPORTING
**       RV_TOKEN = rv_Token
**       RV_DATE = rv_Date
*    ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Token
*      exp   = rv_Token          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Token'
**     level =
*    ).
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Date
*      exp   = rv_Date          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Date'
**     level =
*    ).
  endmethod.


  method get_Token_Blob.

*    data iv_Interface_Id type zinterface_Id.
*    data iv_Uri type zuri.
*    data rv_Sas_Token type string.
*    data rv_Date type string.
*
*    f_Cut->get_Token_Blob(
*      EXPORTING
*        IV_INTERFACE_ID = iv_Interface_Id
*        IV_URI = iv_Uri
**     IMPORTING
**       RV_SAS_TOKEN = rv_Sas_Token
**       RV_DATE = rv_Date
*    ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Sas_Token
*      exp   = rv_Sas_Token          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Sas_Token'
**     level =
*    ).
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Date
*      exp   = rv_Date          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Date'
**     level =
*    ).
  endmethod.


  method get_Token_Cosmosdb.
*    data iv_Interface_Id type zinterface_Id.
*    data iv_Uri type zuri.
*    data rv_Token type string.
*    data rv_Date type string.
*
*    f_Cut->get_Token_Cosmosdb(
*      EXPORTING
*        IV_INTERFACE_ID = iv_Interface_Id
**       IV_URI = iv_Uri
**     IMPORTING
**       RV_TOKEN = rv_Token
**       RV_DATE = rv_Date
*    ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Token
*      exp   = rv_Token          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Token'
**     level =
*    ).
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = rv_Date
*      exp   = rv_Date          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Date'
**     level =
*    ).

  endmethod.




endclass.
