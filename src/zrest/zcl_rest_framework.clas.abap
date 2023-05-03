class ZCL_REST_FRAMEWORK definition
  public
  final
  create public .

public section.

  interfaces ZIF_REST_FRAMEWORK .
  interfaces IF_SERIALIZABLE_OBJECT .

  aliases EXECUTE
    for ZIF_REST_FRAMEWORK~EXECUTE .
  aliases SET_BINARY_BODY
    for ZIF_REST_FRAMEWORK~SET_BINARY_BODY .
  aliases SET_REQUEST_HEADER
    for ZIF_REST_FRAMEWORK~SET_REQUEST_HEADER .
  aliases SET_REQUEST_HEADERS
    for ZIF_REST_FRAMEWORK~SET_REQUEST_HEADERS .
  aliases SET_STRING_BODY
    for ZIF_REST_FRAMEWORK~SET_STRING_BODY .
  aliases SET_URI
    for ZIF_REST_FRAMEWORK~SET_URI .

  methods CONSTRUCTOR
    importing
      !INTERFACE_NAME type ZINTERFACE_ID
      !BUSINESS_IDENTIFIER type ZBUSINESSID optional
      !METHOD type CHAR20
      !LOGDATA_IN_UPDATETASK type CHAR1 optional
    raising
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods GET_CALLINGPROGRAM
    returning
      value(ZECLASSNAME) type ZCLASSNAME .
  methods GET_CALLINGMETHOD
    returning
      value(ZEMETHOD) type ZCLASSNAME .
  methods GET_STARTTIME
    returning
      value(RV_STARTTIME) type SY-UZEIT .
  methods GET_STARTDATE
    returning
      value(RV_STARTDATE) type SY-DATUM .
  methods GET_SUBMITTIME
    returning
      value(RV_SUBMITTIME) type SY-UZEIT .
  methods GET_SUBMITDATE
    returning
      value(RV_SUBMITDATE) type SY-DATUM .
  methods GET_ENDTIME
    returning
      value(RV_ENDTIME) type SY-UZEIT .
  methods GET_ENDATE
    returning
      value(RV_ENDDATE) type DATUM .
  methods GET_RESPONSE_HEADER
    importing
      !IV_NAME type STRING
    returning
      value(RV_VALUE) type STRING .
  methods GET_RESPONSE_HEADERS
    returning
      value(RT_HEADER_FIELDS) type TIHTTPNVP .
  methods SET_GUID
    returning
      value(GUID) type GUID_16 .
  methods GET_USER
    returning
      value(RV_USER) type SY-UNAME .
  methods GET_STATUS
    returning
      value(RV_STATUS) type I .
  methods GET_HTTP_CLIENT
    returning
      value(RESULT) type ref to IF_HTTP_CLIENT .
  methods GET_REST_CLIENT
    returning
      value(RESULT) type ref to CL_REST_HTTP_CLIENT .
  methods IS_RETRY
    returning
      value(RESULT) type ABAP_BOOL .
  methods GET_REQUEST
    returning
      value(RESULT) type ref to IF_REST_ENTITY .
  methods GET_GUID
    returning
      value(RESULT) type GUID_16 .
  methods GET_INTERFACE
    returning
      value(INTERFACE_NAME) type ZINTERFACE_ID .
  methods GET_BUSINESSID
    returning
      value(RESULT) type ZBUSINESSID .
  methods SET_CALLINGPROGRAM
    importing
      !ZICLASSNAME type ZCLASSNAME .
  methods SET_CALLINGMETHOD
    importing
      !ZIMETHODNAME type ZMETHODNAME .
  methods GET_URI
    returning
      value(RESULT) type ZURI .
  methods GET_RETRYNUM
    returning
      value(RESULT) type ZQ_COUNTE .
  methods GET_METHOD
    returning
      value(RESULT) type CHAR20 .
  methods GET_DURATION
    returning
      value(R_SECS) type TZNTSTMPL .
  methods GET_PROGRAM_HEADERS
    returning
      value(RESULT) type TIHTTPNVP .
  methods CLOSE .
  methods KEEP_SUBMIT_PARAMS
    importing
      !IM_SUBMIT_DATE type SY-DATUM
      !IM_SUBMIT_TIME type SY-UZEIT .
protected section.
private section.

*"* private components of class ZCL_REST_FRAMEWORK
*"* do not include other source files here!!!
*  types WA type ZREST_MON_TRACE .
*  types WA_EKKO type ZREST_MON_TRACE .
*  types ZREST_MON_TRACE type ZREST_MON_TRACE .

  constants CREATE_BY_DESTINATION type CHAR1 value 1 ##NO_TEXT.
  class-data GWA_HEADER type ZREST_MON_HEADER .
  class-data IT_LOG_HEADERS type ZRT_HEADER_LOG .
  data ERROR type C .
  class-data RESPONSE_HEADER_STRING type STRING .
  class-data REQUEST_HEADER_STRING type STRING .
  data CALLING_PROGRAM_NAME type ZCLASSNAME .
  data CALLING_METHOD_NAME type ZCLASSNAME .
  data REST_CLIENT type ref to CL_REST_HTTP_CLIENT .
  data HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  data SUBMIT_DATE type SY-DATUM .
  data SUBMIT_TIME type SY-UZEIT .
  data START_TIME type SY-UZEIT .
  data START_DATE type SY-DATUM .
  data END_TIME type SY-UZEIT .
  data END_DATE type SY-DATUM .
  data MESSAGE_ID type GUID_16 .
  data INDENTIFIER type STRING .
  data RETRY type ABAP_BOOL .
  data USER type UNAME .
  data GT_LOG type ZRT_MONITOR .
  data GWA_LOG type ZREST_MONITOR .
  constants HEAD type CHAR8 value 'HEAD' ##NO_TEXT.
  constants GET type CHAR8 value 'GET' ##NO_TEXT.
  constants DELETE type CHAR8 value 'DELETE' ##NO_TEXT.
  constants OPTIONS type CHAR8 value 'OPTIONS' ##NO_TEXT.
  constants POST type CHAR8 value 'POST' ##NO_TEXT.
  constants PUT type CHAR8 value 'PUT' ##NO_TEXT.
  data REQUEST type ref to IF_REST_ENTITY .
  data RESPONSE type ref to IF_REST_ENTITY .
  data HTTP_STATUS type I .
  class-data CONTENT_TYPE type ZCHAR75 .
  class-data INTERFACE type ZINTERFACE_ID .
  class-data BUSINESSID type ZBUSINESSID .
  class-data URI_FINAL type ZURI .
  class-data RETRY_CNT type ZQ_COUNTE .
  class-data METHOD_CALL type CHAR20 .
  class-data PRE_TIMESTAMP type TIMESTAMPL .
  class-data PRO_TIMESTAMP type TIMESTAMPL .
  class-data DURATION type TZNTSTMPL .
  class-data LOG_UPDATE_TASK type CHAR1 .
  class-data PROGRAM_HEADERS type TIHTTPNVP .

  methods SET_INTERFACE_ID
    importing
      !INTERFACE type ZINTERFACE_ID .
  methods LOG_END_PARAMS .
  methods LOG_START_PARAMS .
  methods LOG_SUBMIT_PARAMS .
  methods SET_USER .
  methods SET_ENDDATE
    importing
      !IV_ENDATE type DATUM .
  methods SET_ENDTIME
    importing
      !IV_ENDTIME type SY-UZEIT .
  methods SET_STARTDATE .
  methods SET_SUBMITDATE .
  methods SET_SUBMITTIME .
  methods SET_STARTTIME .
  methods CREATEHTTPCLIENT
    importing
      !INTERFACE type ZINTERFACE_ID
      !METHOD type CHAR20
    returning
      value(HTTP_CLIENT) type ref to IF_HTTP_CLIENT
    raising
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods SAVE_LOG .
  methods CREATERESTCLIENT
    importing
      !IV_HTTP_CLIENT type ref to IF_HTTP_CLIENT
    returning
      value(RV_REST_CLIENT) type ref to CL_REST_HTTP_CLIENT .
  methods CREATE_REQUEST_ENTITY
    importing
      !IV_MULTIPART type ABAP_BOOL default ABAP_FALSE
    returning
      value(RO_ENTITY) type ref to IF_REST_ENTITY .
  methods GET_RESPONSE_ENTITY
    returning
      value(RO_RESPONSE_ENTITY) type ref to IF_REST_ENTITY .
  methods CREATEHTTPCLIENT_BY_URL
    importing
      !IV_INTERFACE type ZINTERFACE_ID
      !IV_METHOD type CHAR20
      !IV_RESOURCE type STRING
      !IV_CLIENT_ID type STRING optional
    returning
      value(RV_HTTP_CLIENT) type ref to IF_HTTP_CLIENT
    raising
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
ENDCLASS.



CLASS ZCL_REST_FRAMEWORK IMPLEMENTATION.


  METHOD  close.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
*05/26/2016| KRDASH    |2165812 | MS2K955227 | Closing Http client     *
*                                              connection              *
*09/27/2022| V-ASHOKM1 |         | SMTK907895 | Fixing VF Errors       *
*----------------------------------------------------------------------*
    DATA :rest_exception TYPE REF TO cx_rest_client_exception.
    TRY.
        rest_client->if_rest_client~close( ).
      CATCH cx_rest_client_exception INTO rest_exception.
*      Begin of changes by V-ASHOKM1 ++ SMTK907893
        DATA(lv_text) = rest_exception->get_text( ).
        IF lv_text IS NOT INITIAL.
          MESSAGE lv_text TYPE 'E'.
        ENDIF.
*     End of changes by V-ASHOKM1 ++  SMTK907893
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
*   Create the unique guid . This is used across all the tables for storing
*   and retrieving the information related to REST calls
*   message_id = me->set_guid( ).
*   Set the calling interface.This needs to be provided by while creating instance
    DATA:
      cx_interface_missing  TYPE REF TO zcx_interace_config_missing,
      cx_http_client_failed TYPE REF TO zcx_http_client_failed.

    CREATE OBJECT:
      cx_interface_missing,
      cx_http_client_failed.

    interface = interface_name.
    businessid = business_identifier.
    method_call = method.
    log_update_task = logdata_in_updatetask.

*   Create the HTTP client and raise exception if interface is not found in the config
*   Propagate the error to the calling program
    TRY .
        CALL METHOD me->createhttpclient
          EXPORTING
            interface   = interface_name
            method      = method
          RECEIVING
            http_client = http_client.
      CATCH zcx_interace_config_missing INTO cx_interface_missing.
        RAISE EXCEPTION cx_interface_missing.
      CATCH zcx_http_client_failed INTO cx_http_client_failed.
        RAISE EXCEPTION cx_http_client_failed.
    ENDTRY.

*   Set the protocol method as HTTP 1.1--Hard code this..Not gonna change
    http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

*   Set the request
    create_request_entity( ).
  ENDMETHOD.


  METHOD  createhttpclient.

* Local data declarations
    DATA:
      lv_resource    TYPE string,
      lv_resource_ec TYPE string,
      lv_client_id   TYPE string,
      lw_mi_config   TYPE zadf_mi_config,
      wa_config      TYPE zrest_srtuct_config.
*   Start loggin the request
    set_interface_id( interface = interface ).
*   Check against the configuration table ZREST_CONFIG and see if there exists entry. if not
*   Raise an exception if either destination is missing or inteface Id is missing.There is no
*   point in proceeding further.Calling program would need to catch this exception !
    CALL METHOD zcl_rest_utility_class=>get_config_data
      EXPORTING
        interface_id = interface
        method       = method
      RECEIVING
        config_data  = wa_config.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_interace_config_missing.
    ELSEIF wa_config-destination IS INITIAL.
      RAISE EXCEPTION TYPE zcx_interace_config_missing.
    ENDIF.

* Switch to managed identities
    CLEAR: lv_resource, lv_client_id.
    SELECT SINGLE resource_id
                  client_id
                  FROM zadf_mi_config INTO (lv_resource, lv_client_id)
                  WHERE interface_id = interface.
    IF sy-subrc EQ 0.
      lv_resource    = lv_resource.
      lv_client_id   = lv_client_id.

      CALL METHOD me->createhttpclient_by_url
        EXPORTING
          iv_interface   = interface
          iv_method      = method
          iv_resource    = lv_resource
          iv_client_id   = lv_client_id
        RECEIVING
          rv_http_client = http_client.
    ELSE.
*   Create the http client by destination
      CALL METHOD cl_http_client=>create_by_destination
        EXPORTING
          destination              = wa_config-destination
        IMPORTING
          client                   = http_client
        EXCEPTIONS
          argument_not_found       = 1
          destination_not_found    = 2
          destination_no_authority = 3
          plugin_not_active        = 4
          internal_error           = 5
          OTHERS                   = 6.
      CASE sy-subrc.
        WHEN 1 OR 2 OR 3 OR 4 OR 5 OR 6.
          RAISE EXCEPTION TYPE zcx_http_client_failed.
      ENDCASE.
    ENDIF.
*   Go ahead with processign and create the http client
    http_client->propertytype_logon_popup = http_client->co_disabled.
    rest_client = createrestclient( iv_http_client =  http_client ).
  ENDMETHOD.


  METHOD createhttpclient_by_url.

    DATA:
      lw_adf_config  TYPE zadf_config,
      lv_url         TYPE string,
      lt_ranges_tab  TYPE rseloption,
      lwa_ranges_tab TYPE rsdsselopt,
      format         TYPE i,
      lv_resource    TYPE string,
      lv_client_id   TYPE string.

* Get the base url from the ZADF_CONFIG
    SELECT SINGLE * FROM zadf_config INTO lw_adf_config
                                    WHERE interface_id EQ iv_interface.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_interace_config_missing.
    ELSE.
      lv_url = lw_adf_config-uri.
    ENDIF.

* Add Client id to the URL if it is available
    IF iv_client_id IS NOT INITIAL.
      lv_client_id = iv_client_id.
      CONCATENATE '&client_id=' lv_client_id INTO lv_client_id.
      CONCATENATE lv_url lv_client_id INTO lv_url.
      CONDENSE lv_url NO-GAPS.
    ENDIF.

* Build the complete URL by combining the base URL from ZADF_CONFIG
* and the resource from importing varaible.
    IF iv_resource IS NOT INITIAL.
      format = 18.
      lv_resource = iv_resource.
      lv_resource = escape( val = lv_resource format = format ).
      CONCATENATE '&resource=' lv_resource INTO lv_resource.
      CONCATENATE lv_url lv_resource INTO lv_url.

*
*      IF sy-uname = 'VIKASBANSAL'.
*        DATA(lv_resource_tmp) =
*           '/subscriptions/cf814bf2-d425-4b7f-bba2-e815cef7cba7/resourceGroups/SAP_AUTOMATION/providers/Microsoft.DocumentDB/databaseAccounts/smt-test-mi-cosmos'.
*
*        DATA(lv_resourceid) = escape( val = lv_resource_tmp format = format ).
*        CONCATENATE lv_url '&msi_res_id=' lv_resourceid
*         INTO lv_url.
*      ENDIF.

      CONDENSE lv_url NO-GAPS.
    ENDIF.


* Create the HTTP client by URL
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = rv_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    CASE sy-subrc.
      WHEN 1 OR 2 OR 3 OR 4 OR 5 OR 6.
        RAISE EXCEPTION TYPE zcx_http_client_failed.
    ENDCASE.

  ENDMETHOD.


  METHOD  createrestclient.
    TRY .
        CREATE OBJECT rv_rest_client EXPORTING io_http_client = iv_http_client.
        gwa_log-status = 'E'.
        gwa_log-reason = 'REST Client Created' .
      CATCH cx_root.
        gwa_log-status = 'E'.
        gwa_log-reason = 'Failed to create REST Client' .
    ENDTRY.
  ENDMETHOD.


  METHOD  create_request_entity.
**To allow GET and DELETE request entity in REST call
    IF method_call EQ 'GET' or method_call eq 'DELETE'.
    ELSE.
      request = rest_client->if_rest_client~create_request_entity( iv_multipart ).
    ENDIF.
  ENDMETHOD.


  method GET_BUSINESSID.
    result = businessid.
  endmethod.


  METHOD  get_callingmethod.
    zemethod =  calling_method_name.
  ENDMETHOD.


  METHOD  get_callingprogram.
    zeclassname =  calling_program_name.
  ENDMETHOD.


  METHOD get_duration.

    r_secs = duration.
    r_secs = r_secs * -1000. "microseconds

  ENDMETHOD.


  METHOD  get_endate.
    rv_enddate =   end_date.
  ENDMETHOD.


  METHOD  get_endtime.
    rv_endtime =  end_time.
  ENDMETHOD.


  METHOD get_guid.
      result = message_id.
  ENDMETHOD.


  method GET_HTTP_CLIENT.
    result = http_client.
  endmethod.


  method GET_INTERFACE.
    interface_name = interface.
  endmethod.


  method GET_METHOD.
    result = method_call.
  endmethod.


  METHOD get_program_headers.
    result = program_headers.
    REFRESH program_headers. "v-javeda | MS2K948978
  ENDMETHOD.


  method GET_REQUEST.
    result = request.
  endmethod.


  METHOD  get_response_entity.
    ro_response_entity = me->rest_client->if_rest_client~get_response_entity( ).
    response  = ro_response_entity.
*    IF ro_response_entity IS BOUND.
*      gwa_log-status =  ro_response_entity->get_header_field( '~status_code' ).
*      gwa_log-reason  = ro_response_entity->get_header_field( '~status_reason' ).
*      gwa_log-content_length  = ro_response_entity->get_header_field( 'content-length' ).
*      gwa_log-location = ro_response_entity->get_header_field( 'location' ).
*      gwa_log-content_type = ro_response_entity->get_header_field( 'content-type' ).
*      gwa_log-response = ro_response_entity->get_string_data( ).
*
*      gwa_log-status =  'S'.
*      gwa_log-reason  = 'Response Recieved'.
*
*      wa_error_log-zmessageid   =  message_id.
*      counter = counter + 1.
*      wa_error_log-counter      = counter.
*      wa_error_log-zmessageclass = 'No Response Recieved'.
*      APPEND wa_error_log TO it_error_log.
*      CLEAR  wa_error_log.
*
*    ELSE.
*
*      gwa_log-status =  'E'.
*      gwa_log-reason  = 'No Response Recieved'.
*
*      wa_error_log-zmessageid   =  message_id.
*      counter = counter + 1.
*      wa_error_log-counter      = counter.
*      wa_error_log-zmessageclass = 'No Response Recieved'.
*      APPEND wa_error_log TO it_error_log.
*      CLEAR  wa_error_log.
*
*    ENDIF.
  ENDMETHOD.


  METHOD  get_response_header.
*    rv_value = me->rest_client->if_rest_client~get_response_header( EXPORTING iv_name = iv_name ). v-jobpau
CALL METHOD me->rest_client->if_rest_client~get_response_header
  EXPORTING
    iv_name  =  iv_name
  receiving
    rv_value =  rv_value
    .


  ENDMETHOD.


  METHOD  get_response_headers.
    rt_header_fields = me->rest_client->if_rest_client~get_response_headers(  ).
  ENDMETHOD.


  method GET_REST_CLIENT.
    result = rest_client.
  endmethod.


  method GET_RETRYNUM.
    result = retry_cnt.
  endmethod.


  METHOD  get_startdate.
    rv_startdate =  start_date.
  ENDMETHOD.


  METHOD  get_starttime.
    rv_starttime =  start_time.
  ENDMETHOD.


  METHOD  get_status.
    IF  rest_client IS BOUND.
      rv_status =  rest_client->if_rest_client~get_status( ).
    ENDIF.
  ENDMETHOD.


  METHOD  get_submitdate.
    rv_submitdate =  submit_date.
  ENDMETHOD.


  METHOD  get_submittime.
    rv_submittime =  submit_time.
  ENDMETHOD.


  method GET_URI.
    result = uri_final.
  endmethod.


  METHOD  get_user.
    rv_user =  user.
  ENDMETHOD.


  METHOD is_retry.
    IF retry EQ abap_true.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.


METHOD keep_submit_params.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04/09/2019|V-ASPATT  |4360996  | MS2K981536 |Submit Date and time    *
*----------------------------------------------------------------------*

* //Preserve Submit date and time after RETRY function
  submit_date = im_submit_date.
  submit_time = im_submit_time.
ENDMETHOD.


  METHOD log_end_params.
    set_enddate( iv_endate = sy-datum ).
    set_endtime( iv_endtime = sy-uzeit ).
  ENDMETHOD.


  METHOD log_start_params.
    set_user( ).
    set_startdate( ).
    set_starttime( ).
  ENDMETHOD.


  METHOD log_submit_params.
    set_submitdate( ).
    set_submittime( ).
  ENDMETHOD.


  METHOD  save_log.

    CALL FUNCTION 'ZSAVE_REST_LOG'
      EXPORTING
        framework_class = me
        update_task = log_update_task.
  ENDMETHOD.


  METHOD  set_callingmethod.
    calling_method_name = zimethodname.

  ENDMETHOD.


  METHOD  set_callingprogram.
    calling_program_name = ziclassname.
  ENDMETHOD.


  METHOD  set_enddate.
    end_date = iv_endate  .
  ENDMETHOD.


  METHOD  set_endtime.
    end_time = iv_endtime.
  ENDMETHOD.


  METHOD set_guid.
    IF retry EQ abap_false.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = guid.
      message_id = guid.
    ELSE.
    ENDIF.
  ENDMETHOD.


  method SET_INTERFACE_ID.
        gwa_log-interface_id = interface.
  endmethod.


  METHOD  set_startdate.
    start_date = sy-datum.
  ENDMETHOD.


  METHOD  set_starttime.
    start_time = sy-uzeit.
  ENDMETHOD.


  METHOD  set_submitdate.
    submit_date = sy-datum.
  ENDMETHOD.


  METHOD  set_submittime.
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
* 04/09/2019|V-ASPATT  |4360996  | MS2K981536 |Submit Date and time    *
*----------------------------------------------------------------------*
*    start_date = sy-datum.
    submit_time = sy-uzeit.  "Ashutosh on 04/05/2019
  ENDMETHOD.


  METHOD  set_user.
    user = sy-uname.
  ENDMETHOD.


  METHOD  zif_rest_framework~execute.

    DATA: lwa_config_data TYPE zrest_srtuct_config,
          rest_exception  TYPE REF TO cx_rest_client_exception.
    CREATE OBJECT rest_exception.
    GET TIME STAMP FIELD pre_timestamp.
*   Create the unique guid . This is used across all the tables for storing
*   and retrieving the information related to REST calls
    message_id = me->set_guid( ).
*   Is this a retry ?
    retry = is_retry.
*   Chaneg only for the retry sceario.
    IF is_retry( ) EQ abap_true.
      message_id = messageid.
      retry_cnt = retry_count + 1.
    ENDIF.
*   This method will execute GET,POST,PUT ...ased on the configuration set for the inteface. If Async is set , Request will
*   in the wating status till the async program flushes this out. Apart from executing the calls , this method will hold
*   the metrics of data
*   Get the configuration data for the inteface.
*    DATA(lwa_config_data) = zcl_rest_utility_class=>get_config_data( EXPORTING interface_id  =  interface method = method_call ). v-jobpau

    CALL METHOD zcl_rest_utility_class=>get_config_data
      EXPORTING
        interface_id = interface
        method       = method_call
      RECEIVING
        config_data  = lwa_config_data.

*   Get the static headers from the configuration
    rest_client->if_rest_client~set_request_headers( zcl_rest_utility_class=>get_static_headers( interface_id =  interface ) ).

*   Set the corelation id
    DATA lv_value TYPE string.
    lv_value = message_id.
    rest_client->if_rest_client~set_request_header( iv_name = 'id-sap-restfrmwrk' iv_value = lv_value ).
*   Set the basic log parameters
    log_start_params( ).
    gwa_log-method = method.
*   If it's Async , record and exit
    IF async EQ abap_false.
      TRY .
          CASE lwa_config_data-method.
            WHEN  head.
              rest_client->if_rest_resource~head( ).
            WHEN  get.
              rest_client->if_rest_resource~get( ).
            WHEN  delete.
              rest_client->if_rest_resource~delete( ).
            WHEN  options.
              rest_client->if_rest_resource~options( ).
            WHEN  post.
              rest_client->if_rest_resource~post(  request ).
            WHEN  put.
              rest_client->if_rest_resource~put(   request ).
          ENDCASE.
*Log End parameters
          log_end_params( ).
*        CATCH cx_rest_client_exception INTO data(rest_exception). v-jobpau
        CATCH cx_rest_client_exception INTO rest_exception.
          log_end_params( ).
          gwa_log-reason = rest_exception->get_text( ).
      ENDTRY.
*Read the respomse and set the appropriate reason
      response =  rest_client->if_rest_client~get_response_entity( ).
      IF response IS BOUND.
        gwa_log-reason = 'Endpoint Called' .
      ENDIF.
    ELSE.
*Log the submitted time
      log_submit_params( ).
      gwa_log-reason = 'Async-Waiting'.
    ENDIF.
*   duration = duration * -1000. " Convert to microseconds
    GET TIME STAMP FIELD pro_timestamp.
*    duration = cl_abap_tstmp=>subtract( exporting tstmp1 = pre_timestamp tstmp2 = pro_timestamp ). v-jobpau
*    TRY.
    CALL METHOD cl_abap_tstmp=>subtract
      EXPORTING
        tstmp1 = pre_timestamp
        tstmp2 = pro_timestamp
      RECEIVING
        r_secs = duration.
*     CATCH cx_parameter_invalid_range .
*     CATCH cx_parameter_invalid_type .
*    ENDTRY.


*   Save the log to database for further reporing
    me->save_log( ).
  ENDMETHOD.


  METHOD zif_rest_framework~set_binary_body.
    IF body IS NOT INITIAL.
        request->set_binary_data( body ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_rest_framework~set_request_header.
    DATA lv_tihttpnvp TYPE LINE OF tihttpnvp.
    lv_tihttpnvp-name = iv_name.
    lv_tihttpnvp-value = iv_value.
    APPEND lv_tihttpnvp TO program_headers.
    CLEAR lv_tihttpnvp.
    http_client->request->set_header_field( EXPORTING name = iv_name value = iv_value ).
  ENDMETHOD.


  METHOD zif_rest_framework~set_request_headers.
    DATA: lv_tihttpnvp TYPE LINE OF tihttpnvp,
          wa_modified_headers type IHTTPNVP.
*    LOOP AT it_header_fields INTO DATA(wa_modified_headers). v-jobpau
    LOOP AT it_header_fields INTO wa_modified_headers.
      lv_tihttpnvp-name  = wa_modified_headers-name.
      lv_tihttpnvp-value = wa_modified_headers-value.
      APPEND lv_tihttpnvp TO program_headers.
      CLEAR lv_tihttpnvp.
    ENDLOOP.
    http_client->request->set_header_fields( it_header_fields  ).
  ENDMETHOD.


  METHOD  zif_rest_framework~set_string_body.
    IF body IS NOT INITIAL.
         http_client->request->set_cdata( body ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_rest_framework~set_uri.
    CHECK uri IS NOT INITIAL.
    cl_http_utility=>set_request_uri(
        request = http_client->request    " HTTP Framework (iHTTP) HTTP Request
        uri     = uri                     " URI String (in the Form of /path?query-string)
           ).
    uri_final = uri.

  ENDMETHOD.
ENDCLASS.
