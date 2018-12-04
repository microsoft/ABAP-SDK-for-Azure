*&---------------------------------------------------------------------*
*& Report  ZADF_DEMO_AZURE_BLOB
*&---------------------------------------------------------------------*
*& Sample program for sending SAP business data to Azure blob Storage  *
*&---------------------------------------------------------------------*
**---------------Prerequisite------------------------------------------*
** Basic Configuration Steps similar to Azure eventhub implementation  *
** guide in Github                                                     *
**---------------------------------------------------------------------*
** 1.Create your storage account in Azure portal link                  *
** https://docs.microsoft.com/en-us/azure/storage/blobs                *
** /storage-quickstart-blobs-portal                                    *
** 2.Configure 'CORS' for blob service under settings section of your  *
** storage account                                                     *
** Allowed origin as '*', Allowed method as 'PUT', Allowed Headers as  *
** '*' , Exposed header as '*' as Optional                             *
** 3.Copy key1 or Key2 from 'Access keys' under settings section of your
** storage account for configuration in table ZADF_CONFIG in below steps
** 4.Add container under Blobs service of your storage account         *
** 5.Configure RFC destination of type 'G' with target host            *
** <Storage account>.blob.core.windows.net                             *
** 6.Table entry for ZREST_CONFIG for interface ID e.g 'DEMOBLOB'      *
** 7.Table entry for ZREST_CONF_MISC for interface ID e.g 'DEMOBLOB'   *
** with method as 'PUT' along with other field values                  *
** 8.Table entry for ZADF_CONFIG with base URI value as blank and      *
** Enable processing flag as blank , type as 'Azure blob service'      *
** and SAS key as Key1 or key2, Service call type as 'S'               *
*&---------------------------------------------------------------------*
REPORT zadf_demo_azure_blob.

CONSTANTS: gc_error     TYPE c VALUE 'E',
           gc_interface TYPE zinterface_id VALUE 'DEMO_BLOB'.
TYPES: BEGIN OF lty_data,
         carrid    TYPE    s_carr_id,
         connid    TYPE    s_conn_id,
         fldate    TYPE    s_date,
         planetype TYPE    s_planetye,
       END OF lty_data.
DATA : gt_data          TYPE STANDARD TABLE OF lty_data,
       gw_data          TYPE lty_data,
       gv_xstr          TYPE xstring,
       gv_string        TYPE string,
       gv_response      TYPE string,
       r_obj            TYPE REF TO zcl_adf_service,
       gcx_adf_service  TYPE REF TO zcx_adf_service,
       gcx_interface    TYPE REF TO zcx_interace_config_missing,
       gcx_http         TYPE REF TO zcx_http_client_failed,
       r_obj1           TYPE REF TO zcl_adf_service_blob,
       gv_response_blob TYPE string,
       gv_msg           TYPE string,
       gv_http_status   TYPE i.

START-OF-SELECTION.
**Fecthing data from SFLIGHT Table
  PERFORM f_fetch_data.
  TRY.
**Calling Factory method to instantiate Blob Storage Client
      r_obj = zcl_adf_service_factory=>create( iv_interface_id = gc_interface iv_business_identifier = space ).
**Setting Expiry time
      CALL METHOD r_obj->add_expiry_time
        EXPORTING
          iv_expiry_hour = 0
          iv_expiry_min  = 15
          iv_expiry_sec  = 0.
      r_obj1 ?=  r_obj.
**Forming String-to-sign for SAS token generation
      CALL METHOD r_obj1->string_to_sign
        EXPORTING
          iv_storage_account = 'Storage account name'  "Storage account namespace should already exist in Azure as prerequisite
          iv_container       = 'Blob Container name'   "Blob container name should already exist in your storage account as prerequisite
          iv_blob_name       = 'xxxxxx'                "Input blob name e.g newblob1 ( provide a new name)
                                                       "which would be created successfully after executing this sample program
          iv_blob_type       = 'B' .                   "Type of Blob e.g 'B' for BlockBlob
**Sending Converted SAP business data to BLOB Storage in Azure
      CALL METHOD r_obj->send
        EXPORTING
          request        = gv_xstr             "Input XSTRING of SAP Business data
        IMPORTING
          response       = gv_response         "Response from Azure blob service
          ev_http_status = gv_http_status.     "Http status
    CATCH zcx_adf_service INTO gcx_adf_service.
      gv_msg =  gcx_adf_service->get_text( ).
      MESSAGE gv_msg TYPE gc_error.
    CATCH zcx_interace_config_missing INTO gcx_interface.
      gv_msg =  gcx_interface->get_text( ).
      MESSAGE gv_msg TYPE gc_error.
    CATCH zcx_http_client_failed INTO gcx_http .
      gv_msg =  gcx_http->get_text( ).
      MESSAGE gv_msg TYPE gc_error.
  ENDTRY.
  IF gv_http_status EQ '201'.
    WRITE : /'Blob created in Azure successfully'.
  ELSE.
    WRITE: /'Error in blob creation process'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_FETCH_DATA
*&---------------------------------------------------------------------*
*       Fetch SFLIGHT Table Data
*----------------------------------------------------------------------*
FORM f_fetch_data .
*Sample data population for sending it to Azure Blob storage
  SELECT carrid connid fldate planetype
            FROM sflight UP TO 10 ROWS
  INTO TABLE gt_data.
  LOOP AT gt_data INTO gw_data.
    CONCATENATE gv_string gw_data-carrid cl_abap_char_utilities=>horizontal_tab
    gw_data-connid cl_abap_char_utilities=>horizontal_tab
    gw_data-fldate cl_abap_char_utilities=>horizontal_tab
    gw_data-planetype cl_abap_char_utilities=>newline INTO gv_string.
    CLEAR gw_data.
  ENDLOOP.
**Converting data string to xtring
  IF NOT gv_string IS INITIAL.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = gv_string
      IMPORTING
        buffer = gv_xstr
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ELSE.
    MESSAGE 'No data found in Table : SFLIGHT' TYPE gc_error.
  ENDIF.
ENDFORM.
