*----------------------------------------------------------------------*
***INCLUDE LZRESTF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_REQUESTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_requests .
  object->get_http_client( )->request->if_http_entity~get_header_fields( CHANGING fields = tab ).
  DATA: wa_header_fields TYPE ihttpnvp.
*  LOOP AT tab  INTO DATA(wa_header_fields). v-jobpau
  LOOP AT tab  INTO wa_header_fields.
    AT FIRST.
      CONCATENATE '***********REQEST HEADERS**************'
      cl_abap_char_utilities=>cr_lf
      INTO request_header_string.
    ENDAT.
*   Get the host and targeted path
    IF wa_header_fields-name EQ 'host'.
      gwa_log-host = wa_header_fields-value.
    ENDIF.
*   Get path if set from headers
    IF wa_header_fields-name CS 'path'.
      gwa_log-uri = wa_header_fields-value.
    ENDIF.
    IF wa_header_fields-name EQ 'content-type'.
      lv_content_type_req = wa_header_fields-value.
    ENDIF.
    CLEAR lv_string.
    CONCATENATE wa_header_fields-name '-' wa_header_fields-value cl_abap_char_utilities=>cr_lf
    INTO lv_string.
    CONCATENATE request_header_string
    lv_string
    INTO request_header_string SEPARATED BY '|'.
  ENDLOOP.

  DATA: lv_string TYPE string,
        gv_string TYPE string,
        lt_program_headers TYPE tihttpnvp,
        wa_program_headers TYPE ihttpnvp.


  CALL METHOD object->get_program_headers
    RECEIVING
      result = lt_program_headers.
*  LOOP AT object->get_program_headers( ) INTO DATA(wa_program_headers).   v-jobpau
  LOOP AT lt_program_headers INTO wa_program_headers.

    CONCATENATE wa_program_headers-name ':' wa_program_headers-value INTO lv_string .
    CONCATENATE lv_string gv_string INTO gv_string SEPARATED BY '|'.
    CLEAR lv_string.
  ENDLOOP.
  PERFORM convert_to_xstring USING gv_string CHANGING lwa_payload-programheaders.


ENDFORM.                    "set_requests
*&---------------------------------------------------------------------*
*&      Form  SET_RESPONSES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_responses .
  DATA: lo_rest_http_client TYPE REF TO cl_rest_http_client,
        lt_header_fields TYPE tihttpnvp,
        wa_header_fields TYPE ihttpnvp.

  CALL METHOD object->get_rest_client
    RECEIVING
      result = lo_rest_http_client.

  lt_header_fields = lo_rest_http_client->if_rest_client~get_response_headers( ).

*  LOOP AT  object->get_rest_client( )->if_rest_client~get_response_headers( ) INTO data(wa_header_fields). v-jobpau
  LOOP AT lt_header_fields INTO  wa_header_fields.
    AT FIRST.
      CONCATENATE '***********RESPONSE HEADERS**************'
      cl_abap_char_utilities=>cr_lf
      INTO response_header_string.
    ENDAT.

    CLEAR lv_string.
    CONCATENATE wa_header_fields-name '-' wa_header_fields-value cl_abap_char_utilities=>cr_lf
    INTO lv_string.

    CONCATENATE response_header_string
    lv_string
    INTO response_header_string SEPARATED BY '|'.

    IF wa_header_fields-name EQ 'content-type'.
      lv_content_type_res = wa_header_fields-value.
    ENDIF.

  ENDLOOP.

  CONCATENATE request_header_string
  response_header_string
  INTO request_header_string.
  PERFORM convert_to_xstring USING request_header_string CHANGING lwa_payload-headers.

ENDFORM.                    "set_responses
*&---------------------------------------------------------------------*
*&      Form  SET_STRING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_string_data .

* Set the request data
  IF  object->get_request( ) IS BOUND AND object->get_request( )->get_string_data( ) IS NOT INITIAL.
    lv_string = object->get_request( )->get_string_data( ).
*   Convert to xstring
    PERFORM convert_to_xstring USING lv_string CHANGING lwa_payload-payload.
  ELSEIF  object->get_request( ) IS BOUND .
    lwa_payload-payload =  object->get_request( )->get_binary_data( ).
  ENDIF.
* Set the response data
  IF  object->get_rest_client( )->if_rest_client~get_response_entity( )->get_string_data( ) IS NOT INITIAL.
    CLEAR lv_string.
    lv_string = object->get_rest_client( )->if_rest_client~get_response_entity( )->get_string_data( ).
*   Convert string
    PERFORM convert_to_xstring USING lv_string CHANGING lwa_payload-response.
  ELSE.
    lwa_payload-response =  object->get_rest_client( )->if_rest_client~get_response_entity( )->get_binary_data( ).
  ENDIF.

*  if object->get_retrynum( ) gt 0.
*  lwa_payload-payload = space.
*  endif.

ENDFORM.                    "set_string_data
*&---------------------------------------------------------------------*
*&      Form  SET_PAYLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_payload.
  lwa_payload-mandt = sy-mandt.
  lwa_payload-content_type_res = lv_content_type_res.
  lwa_payload-content_type_req = lv_content_type_req.
  lwa_payload-messageid =  object->get_guid( ).
  lwa_payload-interface_id = object->get_interface( ).
  lwa_payload-status = object->get_status( ).
  lwa_payload-uri    = object->get_uri( ).
  lwa_payload-businessid  = object->get_businessid( ).
  lwa_payload-method = object->get_method( ).
  IF object->is_retry( ) EQ abap_true.
    lwa_payload-retry_num = object->get_retrynum( ).
  ENDIF.
ENDFORM.                    "set_payload
*&---------------------------------------------------------------------*
*&      Form  SET_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_log .
* Client , User and Message Id
  CLEAR gwa_log.
  gwa_log-mandt           = sy-mandt.
  gwa_log-zmessageid      = object->get_guid( ).
  gwa_log-zuser           = object->get_user( ).
  gwa_log-interface_id    = object->get_interface( ).
  gwa_log-businessid      = object->get_businessid( ).
* Execution Time and Date
  gwa_log-zexedate        = object->get_startdate( ).
  gwa_log-zexetime        = object->get_starttime( ).
* Calling Program and Callign Mehtod
  gwa_log-calling_method  = object->get_callingmethod( ).
  gwa_log-calling_program = object->get_callingprogram( ).
* Completed data and completed time
  gwa_log-zcompdate       = object->get_endate( ).
  gwa_log-zcomptime       = object->get_endtime( ).
* Set the HTTP Status
  gwa_log-httpstatus      = object->get_status( ).
* Set the submitted date and submitted time for the asyn calls
  gwa_log-submit_date     = object->get_submitdate( ).
  gwa_log-submit_time     = object->get_submittime( ).
  gwa_log-rertynum        = object->get_retrynum( ).
  gwa_log-uri             = object->get_uri( ).
  gwa_log-method          = object->get_method( ).
  gwa_log-zduration       = object->get_duration( ).
* Check if this retry
  IF object->is_retry( ) EQ abap_true.
    gwa_log-zretrydate = sy-datum.
    gwa_log-zretrytime = sy-uzeit.
  ENDIF.
* Get the description of the async calls.
  IF object->get_status( ) IS NOT INITIAL.
    gwa_log-httpstatus = object->get_status( ).
    gwa_log-status = zcl_rest_utility_class=>get_http_description( code = gwa_log-httpstatus ).
  ENDIF.
ENDFORM.                    "set_log
*&---------------------------------------------------------------------*
*&      Form  CONVERT_TO_XSTRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_REQUEST_HEADER_STRING  text
*      <--P_LWA_PAYLOAD_HEADERS  text
*----------------------------------------------------------------------*
FORM convert_to_xstring  USING    input TYPE string
                         CHANGING result TYPE xstring.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text     = input
*     MIMETYPE = ' '
*     ENCODING =
    IMPORTING
      buffer   = result
    EXCEPTIONS
      failed   = 1
      OTHERS   = 2.

ENDFORM.                    "convert_to_xstring
