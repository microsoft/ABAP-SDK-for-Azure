*&---------------------------------------------------------------------*
*& Report  ZADF_EVENTGRID_ANY_DEMO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zadf_eventgrid_any_demo.


DATA:
  lt_headers     TYPE tihttpnvp,
  wa_headers     TYPE LINE OF tihttpnvp,
  lv_string      TYPE string,
  lv_response    TYPE string,
  cx_interface   TYPE REF TO zcx_interace_config_missing,
  cx_http        TYPE REF TO zcx_http_client_failed,
  cx_adf_service TYPE REF TO zcx_adf_service,
  oref_eventgrid TYPE REF TO zcl_adf_service_eventgrid,
  oref           TYPE REF TO zcl_adf_service,
  filter         TYPE zbusinessid,
  lv_http_status TYPE i,
  lo_json        TYPE REF TO cl_trex_json_serializer,
  lv1_string     TYPE string,
  lv_xstring     TYPE xstring.


TYPES : BEGIN OF ty_egrid_schema1 ,
          topic           TYPE  zadf_egrid_topic,
          subject         TYPE  zadf_egrid_subj,
          eventtype       TYPE  zadf_egrid_typ,
          eventtime       TYPE  zadf_egrid_etime,
          id              TYPE  zadf_egrid_id,
          dataversion     TYPE  zadf_egrid_dversion,
          metadataversion TYPE  zadf_egrid_mdversion,
          data           TYPE  REF TO data,
        END OF  ty_egrid_schema1.


DATA : ls_egridschema TYPE ty_egrid_schema1,
     lt_egridschema TYPE STANDARD TABLE OF ty_egrid_schema1.

FIELD-SYMBOLS : <lfs_data>     TYPE STANDARD TABLE,
                <lfs_data_tmp> TYPE STANDARD TABLE.

TRY.
**Calling Factory method to instantiate eventgrid client
    oref = zcl_adf_service_factory=>create( iv_interface_id = 'DEMO_EGRID'
                                            iv_business_identifier = filter ).
    oref_eventgrid ?= oref.

**Setting Expiry time
    CALL METHOD oref_eventgrid->add_expiry_time
      EXPORTING
        iv_expiry_hour = 0
        iv_expiry_min  = 120
        iv_expiry_sec  = 0.

*Sample data population for sending it to Azure eventgrid
    SELECT  objectclas, objectid
         FROM cdhdr UP TO 2 ROWS
         INTO TABLE @DATA(lt_data).
    IF sy-subrc EQ 0.

      GET TIME STAMP FIELD  DATA(lv_current_timestamp) .

      ls_egridschema = VALUE #(
                  topic            = ''                         " Optional (but if included, must match the Event Grid topic Azure Resource Manager ID exactly.
                                                                " If not included, Event Grid will stamp onto the event)
                  id               = '9932432499'               " Mendatory ( Unique identifier for the event) Like Billing Doc number created in SAP.
                  eventtype        = 'SAP.BILLING DOC.CREATED'  " Mandatory(One of the registered event types for this event source)
                                                                " Like SAP.BILLING DOC.CREATED
                 subject          = 'SAP/SALES/Billing DOC'    " Mandatory( Publisher-defined path to the event subject) Like
                                                                " 'SAP/SALES/Billing DOC'
                  "eventtime        = lv_current_timestamp      " Mandatory( The time the event is generated based on the provider's UTC time.
*                  data             =                          " Optional ( Like Billing data in Json string )
             ) .

* Dynamically assign intenal Table Data to payload
      ASSIGN lt_data TO <lfs_data>.
      CREATE DATA ls_egridschema-data LIKE <lfs_data>.

      ASSIGN ls_egridschema-data->* TO <lfs_data_tmp>.
      <lfs_data_tmp>   = <lfs_data>.

      APPEND ls_egridschema TO lt_egridschema.

      oref_eventgrid->set_eventgrid_schema( EXPORTING it_egrid_schema = lt_egridschema
                                            RECEIVING rv_xstring      = DATA(lv_pxstring) ).

**Sending Converted SAP data to Azure eventgrid
      CALL METHOD oref_eventgrid->send
        EXPORTING
          request        = lv_pxstring  "Input XSTRING of SAP Business Event data
          it_headers     = lt_headers  "Header attributes
        IMPORTING
          response       = lv_response       "Response from eventgrid
          ev_http_status = lv_http_status.   "Status


      IF lv_http_status NE '201' AND
         lv_http_status NE '200'.
        MESSAGE 'SAP data not sent to Azure eventgrid' TYPE 'E'.
      ELSE.
        MESSAGE 'SAP data sent to Azure eventgrid' TYPE 'I'.
      ENDIF.
    ELSE.
      MESSAGE 'No data in SFLIFHT' TYPE 'E'.
    ENDIF.

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
