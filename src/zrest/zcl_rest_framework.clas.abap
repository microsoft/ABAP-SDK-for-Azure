CLASS zcl_rest_framework DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_rest_framework .
    INTERFACES if_serializable_object .

    ALIASES execute
      FOR zif_rest_framework~execute .
    ALIASES set_binary_body
      FOR zif_rest_framework~set_binary_body .
    ALIASES set_request_header
      FOR zif_rest_framework~set_request_header .
    ALIASES set_request_headers
      FOR zif_rest_framework~set_request_headers .
    ALIASES set_string_body
      FOR zif_rest_framework~set_string_body .
    ALIASES set_uri
      FOR zif_rest_framework~set_uri .

    METHODS constructor
      IMPORTING
        !interface_name        TYPE zinterface_id
        !business_identifier   TYPE zbusinessid OPTIONAL
        !method                TYPE zinterface_method
        !logdata_in_updatetask TYPE char1 OPTIONAL
      RAISING
        zcx_interace_config_missing
        zcx_http_client_failed .
    METHODS get_callingprogram
      RETURNING
        VALUE(zeclassname) TYPE zclassname .
    METHODS get_callingmethod
      RETURNING
        VALUE(zemethod) TYPE zclassname .
    METHODS get_starttime
      RETURNING
        VALUE(rv_starttime) TYPE sy-uzeit .
    METHODS get_startdate
      RETURNING
        VALUE(rv_startdate) TYPE sy-datum .
    METHODS get_submittime
      RETURNING
        VALUE(rv_submittime) TYPE sy-uzeit .
    METHODS get_submitdate
      RETURNING
        VALUE(rv_submitdate) TYPE sy-datum .
    METHODS get_endtime
      RETURNING
        VALUE(rv_endtime) TYPE sy-uzeit .
    METHODS get_endate
      RETURNING
        VALUE(rv_enddate) TYPE datum .
    METHODS get_response_header
      IMPORTING
        !iv_name        TYPE string
      RETURNING
        VALUE(rv_value) TYPE string .
    METHODS get_response_headers
      RETURNING
        VALUE(rt_header_fields) TYPE tihttpnvp .
    METHODS set_guid
      RETURNING
        VALUE(guid) TYPE guid_16 .
    METHODS get_user
      RETURNING
        VALUE(rv_user) TYPE sy-uname .
    METHODS get_status
      RETURNING
        VALUE(rv_status) TYPE i .
    METHODS get_http_client
      RETURNING
        VALUE(result) TYPE REF TO if_http_client .
    METHODS get_rest_client
      RETURNING
        VALUE(result) TYPE REF TO cl_rest_http_client .
    METHODS is_retry
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS get_request
      RETURNING
        VALUE(result) TYPE REF TO if_rest_entity .
    METHODS get_guid
      RETURNING
        VALUE(result) TYPE guid_16 .
    METHODS get_interface
      RETURNING
        VALUE(interface_name) TYPE zinterface_id .
    METHODS get_businessid
      RETURNING
        VALUE(result) TYPE zbusinessid .
    METHODS set_callingprogram
      IMPORTING
        !ziclassname TYPE zclassname .
    METHODS set_callingmethod
      IMPORTING
        !zimethodname TYPE zmethodname .
    METHODS get_uri
      RETURNING
        VALUE(result) TYPE zuri .
    METHODS get_retrynum
      RETURNING
        VALUE(result) TYPE zq_counte .
    METHODS get_method
      RETURNING
        VALUE(result) TYPE char20 .
    METHODS get_duration
      RETURNING
        VALUE(r_secs) TYPE tzntstmpl .
    METHODS get_program_headers
      RETURNING
        VALUE(result) TYPE tihttpnvp .
    METHODS close .
  PROTECTED SECTION.
  PRIVATE SECTION.

*"* private components of class ZCL_REST_FRAMEWORK
*"* do not include other source files here!!!
    DATA calling_program_name TYPE zclassname .
    DATA calling_method_name TYPE zclassname .
    DATA rest_client TYPE REF TO cl_rest_http_client .
    DATA http_client TYPE REF TO if_http_client .
    DATA submit_date TYPE sy-datum .
    DATA submit_time TYPE sy-uzeit .
    DATA start_time TYPE sy-uzeit .
    DATA start_date TYPE sy-datum .
    DATA end_time TYPE sy-uzeit .
    DATA end_date TYPE sy-datum .
    DATA message_id TYPE guid_16 .
    DATA retry TYPE abap_bool .
    DATA user TYPE uname .
    DATA gwa_log TYPE zrest_monitor .
    DATA request TYPE REF TO if_rest_entity .
    DATA response TYPE REF TO if_rest_entity .
    CLASS-DATA interface TYPE zinterface_id .
    CLASS-DATA businessid TYPE zbusinessid .
    CLASS-DATA uri_final TYPE zuri .
    CLASS-DATA retry_cnt TYPE zq_counte .
    CLASS-DATA method_call TYPE zinterface_method .
    CLASS-DATA pre_timestamp TYPE timestampl .
    CLASS-DATA pro_timestamp TYPE timestampl .
    CLASS-DATA duration TYPE tzntstmpl .
    CLASS-DATA log_update_task TYPE char1 .
    CLASS-DATA program_headers TYPE tihttpnvp .

    METHODS set_interface_id
      IMPORTING
        !interface TYPE zinterface_id .
    METHODS log_end_params .
    METHODS log_start_params .
    METHODS log_submit_params .
    METHODS set_user .
    METHODS set_enddate
      IMPORTING
        !iv_endate TYPE datum .
    METHODS set_endtime
      IMPORTING
        !iv_endtime TYPE sy-uzeit .
    METHODS set_startdate .
    METHODS set_submitdate .
    METHODS set_submittime .
    METHODS set_starttime .
    METHODS createhttpclient
      IMPORTING
        !interface         TYPE zinterface_id
        !method            TYPE zinterface_method
      RETURNING
        VALUE(http_client) TYPE REF TO if_http_client
      RAISING
        zcx_interace_config_missing
        zcx_http_client_failed .
    METHODS save_log .
    METHODS createrestclient
      IMPORTING
        !iv_http_client       TYPE REF TO if_http_client
      RETURNING
        VALUE(rv_rest_client) TYPE REF TO cl_rest_http_client .
    METHODS create_request_entity
      IMPORTING
        !iv_multipart    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_entity) TYPE REF TO if_rest_entity .
    METHODS get_response_entity
      RETURNING
        VALUE(ro_response_entity) TYPE REF TO if_rest_entity .
ENDCLASS.



CLASS zcl_rest_framework IMPLEMENTATION.


  METHOD  close.

    DATA: rest_exception TYPE REF TO cx_rest_client_exception.

    TRY.
        rest_client->if_rest_client~close( ).
      CATCH cx_rest_client_exception INTO rest_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
**   Create the unique guid . This is used across all the tables for storing
**   and retrieving the information related to REST calls
*    message_id = me->set_guid( ).
*   Set the calling interface.This needs to be provided by while creating instance
    DATA: cx_interface_missing  TYPE REF TO   zcx_interace_config_missing,
          cx_http_client_failed TYPE REF TO zcx_http_client_failed.

    CREATE OBJECT: cx_interface_missing,
                   cx_http_client_failed.

    interface = interface_name.
    businessid = business_identifier.
    method_call = method.
    log_update_task = logdata_in_updatetask .
*   Create the HTTP client and raise exception if interface is not found in the config
*   Propagate the error to the calling program
    TRY .

        http_client =  createhttpclient(
            interface   = interface_name
            method      = method ).

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
*   Start loggin the request
    set_interface_id( interface = interface ).
*   Check against the configuration table ZREST_CONFIG and see if there exists entry. if not
*   Raise an exception if either destination is missing or inteface Id is missing.There is no
*   point in proceeding further.Calling program would need to catch this exception !
    DATA: wa_config TYPE zrest_srtuct_config.
*    DATA(wa_config) = zcl_rest_utility_class=>get_config_data( exporting interface_id = interface  method = method ). v-jobpau
    wa_config = zcl_rest_utility_class=>get_config_data(
        interface_id = interface
        method       = method ).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_interace_config_missing.
    ELSEIF wa_config-destination IS INITIAL.
      RAISE EXCEPTION TYPE zcx_interace_config_missing.
    ENDIF.
*   Create the http client by destination
    cl_http_client=>create_by_destination(
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
        OTHERS                   = 6 ).
    CASE sy-subrc.
      WHEN 1 OR 2 OR 3 OR 4 OR 5 OR 6.
        RAISE EXCEPTION TYPE zcx_http_client_failed.
    ENDCASE.
*   Go ahead with processign and create the http client
    http_client->propertytype_logon_popup = http_client->co_disabled.
    rest_client = createrestclient( iv_http_client =  http_client ).
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
    IF method_call EQ zif_rest_http_constants=>c_http_method_get OR method_call EQ zif_rest_http_constants=>c_http_method_delete.
    ELSE.
      request = rest_client->if_rest_client~create_request_entity( iv_multipart ).
    ENDIF.
  ENDMETHOD.


  METHOD get_businessid.
    result = businessid.
  ENDMETHOD.


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


  METHOD get_http_client.
    result = http_client.
  ENDMETHOD.


  METHOD get_interface.
    interface_name = interface.
  ENDMETHOD.


  METHOD get_method.
    result = method_call.
  ENDMETHOD.


  METHOD get_program_headers.
    result = program_headers.
    REFRESH program_headers. "v-javeda | MS2K948978
  ENDMETHOD.


  METHOD get_request.
    result = request.
  ENDMETHOD.


  METHOD  get_response_entity.
    ro_response_entity = me->rest_client->if_rest_client~get_response_entity( ).
    response  = ro_response_entity.
  ENDMETHOD.


  METHOD  get_response_header.
    rv_value = rest_client->if_rest_client~get_response_header(
      EXPORTING
        iv_name  = iv_name ).
  ENDMETHOD.


  METHOD  get_response_headers.
    rt_header_fields = me->rest_client->if_rest_client~get_response_headers(  ).
  ENDMETHOD.


  METHOD get_rest_client.
    result = rest_client.
  ENDMETHOD.


  METHOD get_retrynum.
    result = retry_cnt.
  ENDMETHOD.


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


  METHOD get_uri.
    result = uri_final.
  ENDMETHOD.


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
        update_task     = log_update_task.
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
    DATA: uuid_16  TYPE sysuuid_x16.
    DATA: cx       TYPE REF TO cx_uuid_error.

    IF retry EQ abap_false.
      TRY.
          uuid_16 = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error INTO cx.
          uuid_16 = '0'.
      ENDTRY.

      message_id = uuid_16.
    ENDIF.

  ENDMETHOD.


  METHOD set_interface_id.
    gwa_log-interface_id = interface.
  ENDMETHOD.


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
    start_date = sy-datum.
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

    lwa_config_data = zcl_rest_utility_class=>get_config_data(
        interface_id = interface
        method       = method_call ).

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
            WHEN  zif_rest_http_constants=>c_http_method_head.
              rest_client->if_rest_resource~head( ).
            WHEN zif_rest_http_constants=>c_http_method_get.
              rest_client->if_rest_resource~get( ).
            WHEN  zif_rest_http_constants=>c_http_method_delete.
              rest_client->if_rest_resource~delete( ).
            WHEN zif_rest_http_constants=>c_http_method_options.
              rest_client->if_rest_resource~options( ).
            WHEN  zif_rest_http_constants=>c_http_method_post.
              rest_client->if_rest_resource~post(  request ).
            WHEN  zif_rest_http_constants=>c_http_method_put.
              rest_client->if_rest_resource~put(   request ).
          ENDCASE.

          log_end_params( ).
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
    duration = cl_abap_tstmp=>subtract(
        tstmp1 = pre_timestamp
        tstmp2 = pro_timestamp ).

*   Save the log to database for further reporing
    save_log( ).
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
    DATA: lv_tihttpnvp        TYPE LINE OF tihttpnvp,
          wa_modified_headers TYPE ihttpnvp.
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
        uri     = uri ).                  " URI String (in the Form of /path?query-string)

    uri_final = uri.
  ENDMETHOD.
ENDCLASS.
