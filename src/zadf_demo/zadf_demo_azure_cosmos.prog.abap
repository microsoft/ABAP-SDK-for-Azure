*&---------------------------------------------------------------------*
*& Report  ZADF_DEMO_AZURE_COSMOSDB
*&
*&---------------------------------------------------------------------*
**---------------Prerequisite------------------------------------------*
** Destination configuration in SM59
** ZREST tables maintained via SM30
** Azure VNet connectivity from SAP to Cosmos ( in case of FW verify
** private endpoint )
**---------------------------------------------------------------------*
** 1.Create your CosmosDB instance *
** 2.Maintain CosmosDB primary key in ZADF_CONFIG table *
*&---------------------------------------------------------------------*
REPORT ZADF_DEMO_AZURE_COSMOS.
CONSTANTS: gc_interface TYPE zinterface_id VALUE 'DEMOCOSMOS'.

TYPES: BEGIN OF lty_data,
         id        TYPE    s_carr_id,
         carrid    TYPE    s_carr_id,
         connid    TYPE    s_conn_id,
         fldate    TYPE    s_date,
         planetype TYPE    s_planetye,
         SEATSMAX  TYPE    int4,
         SEATSOCC  TYPE    int4,
       END OF lty_data.

DATA: it_headers      TYPE tihttpnvp,
      wa_headers      TYPE LINE OF tihttpnvp,
      lv_string       TYPE string,
      lv_response     TYPE string,
      cx_interface    TYPE REF TO zcx_interace_config_missing,
      cx_http         TYPE REF TO zcx_http_client_failed,
      cx_adf_service  TYPE REF TO zcx_adf_service,
      oref_cosmos     TYPE REF TO zcl_adf_service_cosmosdb,
      oref            TYPE REF TO zcl_adf_service,
      filter          TYPE zbusinessid,
      lv_http_status  TYPE i,
      lo_json         TYPE REF TO cl_trex_json_serializer,
      lv1_string      TYPE string,
      lv_xstring      TYPE xstring,
      it_data         TYPE STANDARD TABLE OF lty_data,
      W_LEN           TYPE I.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME.
PARAMETER: p_sbifid TYPE zinterface_id DEFAULT gc_interface.
SELECTION-SCREEN END OF BLOCK bl1.

*Sample data population for sending it to Azure Cosmos
SELECT  connid carrid connid fldate planetype SEATSMAX SEATSOCC
        FROM sflight
        INTO TABLE it_data
        WHERE connid = 64 AND fldate = '20210813'.

IF sy-subrc EQ 0.

  TRY.
**Calling Factory method to instantiate Cosmos client

      oref = zcl_adf_service_factory=>create( iv_interface_id        = p_sbifid
                                              iv_business_identifier = filter ).
      oref_cosmos ?= oref.

**Setting Expiry time
      oref_cosmos->add_expiry_time( iv_expiry_hour = 0
                                        iv_expiry_min  = 15
                                        iv_expiry_sec  = 0 ).

      lv1_string = /ui2/cl_json=>serialize( data = it_data compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
**Cut array [] from json string for expected format for cosmos api request
      W_LEN = STRLEN( lv1_string ) - 2.
      lv1_string = lv1_string+1(W_LEN).

**Convert input string data to Xstring format
      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv1_string
        IMPORTING
          buffer = lv_xstring
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
      ENDIF.
**Send data to collection sflight in db saps4. Adjust to your needs
      oref_cosmos->set_parameters( iv_http_verb = 'POST'
                                   iv_resource_type = 'docs'
                                   iv_partition_key_val = '006'
                                   iv_resource_link = 'dbs/saps4/colls/sflight' ).


**Sending Converted SAP data to Azure CosmosDB
      oref_cosmos->send( EXPORTING request        = lv_xstring        "Input XSTRING of SAP Business data
                                       it_headers     = it_headers        "Header attributes
                             IMPORTING response       = lv_response       "Response from Cosmos
                                       ev_http_status = lv_http_status ). "Status

    CATCH zcx_interace_config_missing INTO cx_interface.
      lv_string = cx_interface->get_text( ).
      MESSAGE lv_string TYPE 'E'.
    CATCH zcx_http_client_failed INTO cx_http .
      lv_string = cx_http->get_text( ).
      MESSAGE lv_string TYPE 'E'.
    CATCH zcx_adf_service INTO cx_adf_service.
      lv_string = cx_adf_service->get_text( ).
      MESSAGE lv_string TYPE 'E'.
  ENDTRY.

  IF lv_http_status NE '201' AND
     lv_http_status NE '200'.
    MESSAGE 'SAP data not sent to Azure Cosmos' TYPE 'E'.
  ELSEIF lv_http_status EQ '401'.
    MESSAGE 'Call to Az Cosmos not authorized' TYPE 'E'.
  ELSE.
    MESSAGE 'SAP data sent to Azure Cosmos' TYPE 'I'.
  ENDIF.
ELSE.
  MESSAGE 'No data in SFLIGHT' TYPE 'E'.
ENDIF.
