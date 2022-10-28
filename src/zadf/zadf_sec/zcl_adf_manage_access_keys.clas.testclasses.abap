*"* use this source file for your ABAP unit test classes
CLASS lcl_adf_manage_access_keys_tst DEFINITION DEFERRED.
CLASS zcl_adf_manage_access_keys DEFINITION LOCAL FRIENDS lcl_adf_manage_access_keys_tst.

CLASS lcl_adf_manage_access_keys_tst DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Adf_Manage_Access_Keys_Tst
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_ADF_MANAGE_ACCESS_KEYS
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
      f_cut TYPE REF TO zcl_adf_manage_access_keys.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: check_call_stack FOR TESTING.
    METHODS: decrypt FOR TESTING.
    METHODS: encrypt FOR TESTING.
    METHODS: get_cert_from_pse FOR TESTING.
    METHODS: get_decrypt_cert_applic FOR TESTING.
    METHODS: get_encrypt_cert_applic FOR TESTING.
    METHODS: get_instance FOR TESTING.
    METHODS: get_pse_from_applic FOR TESTING.
    METHODS: parse_certificate FOR TESTING.
    METHODS: read_applic_from_destination FOR TESTING.
    METHODS: set_decrypt_cert_applic FOR TESTING.
    METHODS: set_encrypt_cert_applic FOR TESTING.
ENDCLASS.       "lcl_Adf_Manage_Access_Keys_Tst


CLASS lcl_adf_manage_access_keys_tst IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.


    CREATE OBJECT f_cut.
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD check_call_stack.


*    zcl_Adf_Manage_Access_Keys=>check_Call_Stack(  ).

  ENDMETHOD.


  METHOD decrypt.

    DATA it_encrypted_data TYPE saml2_pse_bin_data_t.
    DATA rv_secret TYPE string.
    DATA lv_indx TYPE zadf_con_indx.
    DATA lv_srtfd TYPE zadf_con_indx-srtfd.

    lv_indx-aedat = sy-datum.
    lv_indx-usera = sy-uname.
    lv_indx-pgmid = sy-repid.
    lv_srtfd = 'MDG_SB_PD'.

    IMPORT tab  = it_encrypted_data[]
      FROM DATABASE zadf_con_indx(zd)
      TO lv_indx
      ID lv_srtfd.

*    rv_secret = f_cut->decrypt( it_encrypted_data ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_secret
      exp   = rv_secret          "<--- please adapt expected value
    " msg   = 'Testing value rv_Secret'
*     level =
    ).
  ENDMETHOD.


  METHOD encrypt.

    DATA iv_secret TYPE string.
    DATA rt_encrypted_data TYPE saml2_pse_bin_data_t.

*    rt_encrypted_data = f_cut->encrypt( iv_secret ).

    cl_abap_unit_assert=>assert_equals(
      act   = rt_encrypted_data
      exp   = rt_encrypted_data          "<--- please adapt expected value
    " msg   = 'Testing value rt_Encrypted_Data'
*     level =
    ).
  ENDMETHOD.


  METHOD get_cert_from_pse.

    DATA iv_psename TYPE ssfpsename.
    DATA iv_profile TYPE localfile.
    DATA rv_cert TYPE xstring.

*    rv_cert = f_cut->get_cert_from_pse(
*        iv_psename = iv_psename
*        iv_profile = iv_profile ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_cert
      exp   = rv_cert          "<--- please adapt expected value
    " msg   = 'Testing value rv_Cert'
*     level =
    ).
  ENDMETHOD.


  METHOD get_decrypt_cert_applic.

    DATA rv_applic TYPE ssfapplssl.

*    rv_applic = f_cut->get_decrypt_cert_applic(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_applic
      exp   = rv_applic          "<--- please adapt expected value
    " msg   = 'Testing value rv_Applic'
*     level =
    ).
  ENDMETHOD.


  METHOD get_encrypt_cert_applic.

    DATA rv_applic TYPE ssfapplssl.

*    rv_applic = f_cut->get_encrypt_cert_applic(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_applic
      exp   = rv_applic          "<--- please adapt expected value
    " msg   = 'Testing value rv_Applic'
*     level =
    ).
  ENDMETHOD.


  METHOD get_instance.

    DATA ro_ref TYPE REF TO zcl_adf_manage_access_keys.

*    ro_ref = zcl_adf_manage_access_keys=>get_instance(  ).

    cl_abap_unit_assert=>assert_equals(
      act   = ro_ref
      exp   = ro_ref          "<--- please adapt expected value
    " msg   = 'Testing value ro_Ref'
*     level =
    ).
  ENDMETHOD.


  METHOD get_pse_from_applic.

    DATA iv_applic TYPE ssfapplssl.
    DATA rv_psename TYPE ssfpsename.
    DATA rv_profile TYPE localfile.

    f_cut->get_pse_from_applic(
      EXPORTING
        iv_applic = 'ZADFP'
*     IMPORTING
*       RV_PSENAME = rv_Psename
*       RV_PROFILE = rv_Profile
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_psename
      exp   = rv_psename          "<--- please adapt expected value
    " msg   = 'Testing value rv_Psename'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = rv_profile
      exp   = rv_profile          "<--- please adapt expected value
    " msg   = 'Testing value rv_Profile'
*     level =
    ).
  ENDMETHOD.


  METHOD parse_certificate.

    DATA iv_cert TYPE xstring.
    DATA rv_subject TYPE string.

*    rv_subject = f_cut->parse_certificate( iv_cert ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_subject
      exp   = rv_subject          "<--- please adapt expected value
    " msg   = 'Testing value rv_Subject'
*     level =
    ).
  ENDMETHOD.


  METHOD read_applic_from_destination.

    DATA iv_destination TYPE rfcdest.
    DATA rv_applic TYPE ssfapplssl.

*    rv_applic = zcl_adf_manage_access_keys=>read_applic_from_destination( iv_destination ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_applic
      exp   = rv_applic          "<--- please adapt expected value
    " msg   = 'Testing value rv_Applic'
*     level =
    ).
  ENDMETHOD.


  METHOD set_decrypt_cert_applic.

    DATA iv_applic TYPE ssfapplssl.
    iv_applic = 'ZADFP'.

    f_cut->set_decrypt_cert_applic( iv_applic ).

  ENDMETHOD.


  METHOD set_encrypt_cert_applic.

    DATA iv_applic TYPE ssfapplssl.
    iv_applic = 'ZADFP'.
    f_cut->set_encrypt_cert_applic( iv_applic ).

  ENDMETHOD.




ENDCLASS.
