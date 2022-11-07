class lcl_O365_Service_Sharepoint_T definition deferred.
class zcl_O365_Service_Sharepoint definition local friends lcl_O365_Service_Sharepoint_T.

class lcl_O365_Service_Sharepoint_T definition for testing
  duration short
  risk level harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_O365_Service_Sharepoint_T
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_O365_SERVICE_SHAREPOINT
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
      f_Cut type ref to zcl_O365_Service_Sharepoint.  "class under test

    class-methods: class_Setup.
    class-methods: class_Teardown.
    methods: setup.
    methods: teardown.
    methods: get_Aad_Token for testing.
    methods: send for testing.
    methods: send_Header_Csv for testing.
    methods: send_Header_Excel for testing.
    methods: send_Header_Txt for testing.
    methods: set_File_Parameters for testing.
    methods: set_Header_Parameters for testing.
    methods: set_Uri_Path for testing.
    methods: validate_Parameters for testing.
endclass.       "lcl_O365_Service_Sharepoint_T


class lcl_O365_Service_Sharepoint_T implementation.

  method class_Setup.



  endmethod.


  method class_Teardown.



  endmethod.


  method setup.

*    data iv_Interface_Id type zinterface_Id.
*    data iv_Service_Id type zazure_Dest.
*    data iv_Business_Identifier type zbusinessid.
*
*    create object f_Cut
*      EXPORTING
*        IV_INTERFACE_ID = iv_Interface_Id
**       IV_SERVICE_ID = iv_Service_Id
**       IV_BUSINESS_IDENTIFIER = iv_Business_Identifier
*   .
  endmethod.


  method teardown.



  endmethod.


  method get_Aad_Token.
*
*    data ev_Aad_Token type string.
*    data ev_Response type string.
*
*    f_Cut->get_Aad_Token(
**     IMPORTING
**       EV_AAD_TOKEN = ev_Aad_Token
**       EV_RESPONSE = ev_Response
*    ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = ev_Aad_Token
*      exp   = ev_Aad_Token          "<--- please adapt expected value
*    " msg   = 'Testing value ev_Aad_Token'
**     level =
*    ).
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = ev_Response
*      exp   = ev_Response          "<--- please adapt expected value
*    " msg   = 'Testing value ev_Response'
**     level =
*    ).
  endmethod.


  method send.

*    data request type xstring.
*    data it_Headers type tihttpnvp.
*    data response type string.
*    data ev_Http_Status type i.
*
*    f_Cut->send(
*      EXPORTING
*        REQUEST = request
**       IT_HEADERS = it_Headers
**     IMPORTING
**       RESPONSE = response
**       EV_HTTP_STATUS = ev_Http_Status
*    ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = response
*      exp   = response          "<--- please adapt expected value
*    " msg   = 'Testing value response'
**     level =
*    ).
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = ev_Http_Status
*      exp   = ev_Http_Status          "<--- please adapt expected value
*    " msg   = 'Testing value ev_Http_Status'
**     level =
*    ).
  endmethod.


  method send_Header_Csv.

*    data request type xstring.
*    data it_Headers type tihttpnvp.
*    data eo_Request type ref to if_Rest_Entity.
*
*    f_Cut->send_Header_Csv(
*      EXPORTING
*        REQUEST = request
*        IT_HEADERS = it_Headers
**     IMPORTING
**       EO_REQUEST = eo_Request
*    ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = eo_Request
*      exp   = eo_Request          "<--- please adapt expected value
*    " msg   = 'Testing value eo_Request'
**     level =
*    ).
  endmethod.


  method send_Header_Excel.

*    data request type xstring.
*    data it_Headers type tihttpnvp.
*    data eo_Request type ref to if_Rest_Entity.
*
*    f_Cut->send_Header_Excel(
*      EXPORTING
*        REQUEST = request
*        IT_HEADERS = it_Headers
**     IMPORTING
**       EO_REQUEST = eo_Request
*    ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = eo_Request
*      exp   = eo_Request          "<--- please adapt expected value
*    " msg   = 'Testing value eo_Request'
**     level =
*    ).
  endmethod.


  method send_Header_Txt.

*    data request type xstring.
*    data it_Headers type tihttpnvp.
*    data eo_Request type ref to if_Rest_Entity.
*
*    f_Cut->send_Header_Txt(
*      EXPORTING
*        REQUEST = request
*        IT_HEADERS = it_Headers
**     IMPORTING
**       EO_REQUEST = eo_Request
*    ).
*
*    cl_Abap_Unit_Assert=>assert_Equals(
*      act   = eo_Request
*      exp   = eo_Request          "<--- please adapt expected value
*    " msg   = 'Testing value eo_Request'
**     level =
*    ).
  endmethod.


  method set_File_Parameters.

*    data iv_Filename type sdba_Actid.
*    data iv_Filetype type char10.
*    data iv_Folder_Name type char200.
*
*    f_Cut->set_File_Parameters(
**       IV_FILENAME = iv_Filename
**       IV_FILETYPE = iv_Filetype
**       IV_FOLDER_NAME = iv_Folder_Name
*    ).

  endmethod.


  method set_Header_Parameters.

*    data request type xstring.
*    data it_Headers type tihttpnvp.
*
*    f_Cut->set_Header_Parameters(
*        REQUEST = request
*        IT_HEADERS = it_Headers ).

  endmethod.


  method set_Uri_Path.


*    f_Cut->set_Uri_Path(  ).

  endmethod.


  method validate_Parameters.


*    f_Cut->validate_Parameters(  ).

  endmethod.




endclass.
