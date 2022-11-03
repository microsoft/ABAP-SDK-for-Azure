
CLASS lcl_adf_service_drf DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Adf_Service_Drf
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ADF_DRF_AZURE
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
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_adf_drf_azure.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: execute_outbound_call FOR TESTING.
ENDCLASS.       "lcl_Adf_Service_Drf


CLASS lcl_adf_service_drf IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.


    CREATE OBJECT f_cut.
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD execute_outbound_call.

    DATA is_ai_header TYPE zadf_stru_ai_head.
    DATA it_ai_info TYPE zadf_tt_ai_info.
    DATA im_calling_program TYPE dbglprog.
    DATA im_unique_id TYPE char50.
    DATA im_interface_id TYPE zinterface_id.
    DATA im_businessid TYPE zbusinessid.
    DATA et_status TYPE bapiret2_t.
    DATA ev_success TYPE char1.

    zcl_adf_drf_azure=>execute_outbound_call(
      EXPORTING
        is_ai_header = is_ai_header
        it_ai_info = it_ai_info
        im_calling_program = im_calling_program
*       IM_UNIQUE_ID = im_Unique_Id
        im_interface_id = im_interface_id
        im_businessid = im_businessid
*     IMPORTING
*       ET_STATUS = et_Status
*       EV_SUCCESS = ev_Success
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = et_status
      exp   = et_status          "<--- please adapt expected value
    " msg   = 'Testing value et_Status'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = ev_success
      exp   = ev_success          "<--- please adapt expected value
    " msg   = 'Testing value ev_Success'
*     level =
    ).
  ENDMETHOD.




ENDCLASS.
