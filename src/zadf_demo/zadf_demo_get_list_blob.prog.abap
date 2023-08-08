*&---------------------------------------------------------------------*
*& Report ZPPAL_TEST_BLOB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zadf_demo_get_list_blob.

DATA: lv_blob_name TYPE string,
      lv_st_acc    TYPE string,
      lv_container TYPE string,
      lv_folder    TYPE string,
      lt_listtable TYPE zadf_xmltb_t
      .

CONSTANTS lc_property   TYPE string VALUE 'Name'.
DATA: lt_return   TYPE bapiret2_t,
      lt_data     TYPE STANDARD TABLE OF smum_xmltb,
      lv_tempflag TYPE c.

DATA :
  r_obj           TYPE REF TO zcl_adf_service,
  gcx_adf_service TYPE REF TO zcx_adf_service,
  gcx_interface   TYPE REF TO zcx_interace_config_missing,
  gcx_http        TYPE REF TO zcx_http_client_failed,
  r_obj1          TYPE REF TO zcl_adf_service_blob,
  gv_response     TYPE xstring,
  gv_msg          TYPE string,
  gv_http_status  TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_int_id TYPE zinterface_id,
            p_st_acc TYPE char50,
            p_cont   TYPE char50.

SELECTION-SCREEN END OF BLOCK b1.

lv_st_acc = p_st_acc.
lv_container = p_cont.
lv_blob_name = 'Dummy.csv'.
*lv_folder = 'Test_Folder/Test_Folder_Sub'.

TRY.
* Calling Factory method to instantiate Blob Storage Client
    r_obj = zcl_adf_service_factory=>create( iv_interface_id = p_int_id iv_business_identifier = space ).
    r_obj1 ?=  r_obj.

* Setting Expiry time
    CALL METHOD r_obj1->add_expiry_time
      EXPORTING
        iv_expiry_hour = 0
        iv_expiry_min  = 15
        iv_expiry_sec  = 0.

* Setting Blob storage attributes
    CALL METHOD r_obj1->set_storage_account_container
      EXPORTING
        iv_storage_account = lv_st_acc      "Storage account namespace should already exist in Azure as prerequisite
        iv_container_name  = lv_container  "Blob container name should already exist in your storage account as prerequisite
        iv_folder          = lv_folder.     "Folder name (Optional)

* Setting Blob Dummy File attributes
    CALL METHOD r_obj1->set_blob_name_type
      EXPORTING
        iv_blob_name = lv_blob_name "Input blob name
        iv_blob_type = 'B'.

* Setting additional attributes
    CALL METHOD r_obj1->set_blob_additional_attributes.

    CALL METHOD r_obj1->get_list_blob
      IMPORTING
        response       = gv_response
        ev_http_status = gv_http_status.

    CALL METHOD r_obj1->conv_xml_to_inttab
      EXPORTING
        iv_xml_string = gv_response
        iv_property   = lc_property
      IMPORTING
        et_xml_data   = lt_data
        et_return     = lt_return.
  CATCH cx_root INTO DATA(lx_root)                  ##NEEDED ##CATCH_ALL.
    DATA(lv_str) = lx_root->if_message~get_text( )   ##NEEDED.
    MESSAGE:lv_str TYPE 'E'.
ENDTRY.
IF lt_data IS NOT INITIAL.
  cl_demo_output=>display_data( lt_data[] ).
  MESSAGE 'BLOB list fetched successfully' TYPE 'S'.
ENDIF.
