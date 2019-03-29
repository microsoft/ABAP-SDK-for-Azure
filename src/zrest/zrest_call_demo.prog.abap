REPORT zrest_call_demo.
************************************************************************
*     Test program to make rest call using the frameworkl              *
************************************************************************
DATA : rest_handler TYPE REF TO zcl_rest_framework,
       go_response  TYPE REF TO if_rest_entity,
       go_request   TYPE REF TO if_rest_entity,
       gv_client    TYPE string,
       gv_body      TYPE string,
       gv_langu     TYPE langu,
       lv_requestid TYPE string.
************************************************************************
CONSTANTS: c_interface   TYPE zinterface_id VALUE '1VT_OUTBND'.
************************************************************************
DATA: cx_interface TYPE REF TO zcx_interace_config_missing.
CREATE OBJECT cx_interface.
DATA: cx_http TYPE REF TO zcx_http_client_failed.
DATA: v_string TYPE string.
CREATE OBJECT cx_http.
TRY .
    CREATE OBJECT rest_handler
      EXPORTING
        interface_name      = c_interface               "Mandatory
        business_identifier = 'Inv/SO/Deli-ID'
        method              = 'POST'.    "For troubleshooting
*  CATCH zcx_interace_config_missing INTO DATA(cx_interface). v-jobpau
  CATCH zcx_interace_config_missing INTO cx_interface.
    WRITE:/10 'Handle logic Here-Interface Missing'.
*  CATCH zcx_http_client_failed INTO DATA(cx_http) . v-jobpau
  CATCH zcx_http_client_failed INTO cx_http .
    WRITE:/10 'Handle logic Here-HTTP Failed'.
ENDTRY.
************************************************************************
*----------------------Build the query params +Set the Payload here----*
************************************************************************
*Optional - To help developer understand the origin of call
rest_handler->set_callingmethod( 'CALL DEMO' ).
*Optional - To help developer understand the origin of call
rest_handler->set_callingprogram( 'ZREST_CALL_DEMO' ).
rest_handler->set_request_header( iv_name = 'header1' iv_value = 'value for header1' ).
rest_handler->set_request_header( iv_name = 'header2' iv_value = 'value for header2' ).
************************************************************************
lv_requestid = 'Microsoft'.  "Vetting by Organization
CONCATENATE '/org1/' lv_requestid INTO lv_requestid.
rest_handler->set_uri( lv_requestid ).
*gv_body = '{"IndividualData":{"Email": "v-gugam@microsoft.com"},"OrganizationData":{"OrganizationName":"Microsoft corporation Ltd","Address": {"StateProvinceCode":"IN",'.
*CONCATENATE gv_body '"Country":"US","AddressLine1":"","City":"","PostalCode":""}},' into gv_body.
*CONCATENATE gv_body '"EmailOwnershipData":{"RecipientFirstName": "Marshall","RecipientLastName": "Jhonson","RejectUrl" : "http://www.Bing.com","SuccessUrl": "http://www.msn.com",' into gv_body.
*CONCATENATE gv_body '"ExpiredUrl": "http://www.google.com","ExpirationTime": 1440},"Parameters": {"TriggerMiniInvestigation": true,"ValidateEmailOwnership": true,"TriggerManualReview" : true }};' into gv_body.
************************************************************************
*In case of 'POST'
*gv_body = '<note><to>Tove</to><from>Jani</from><heading>Reminder</heading><body>Dont forget me this weekend!</body></note>'.
*rest_handler->zif_rest_framework~set_string_body( gv_body ).
************************************************************************
*------------------------read requests---------------------------------*
DATA : ts  TYPE timestampl,
       ts1 TYPE timestampl.
GET TIME STAMP FIELD ts.
go_response = rest_handler->zif_rest_framework~execute( io_entity = go_request async = abap_false is_retry = abap_false ).
************************************************************************
GET TIME STAMP FIELD ts1.
WRITE:/10 ts , 50 ts1.
DATA : r_secs TYPE tzntstmpl.
*TRY.
CALL METHOD cl_abap_tstmp=>subtract
  EXPORTING
    tstmp1 = ts1
    tstmp2 = ts
  RECEIVING
    r_secs = r_secs.
WRITE:/10 r_secs.
* CATCH cx_parameter_invalid_range .
* CATCH cx_parameter_invalid_type .
*ENDTRY.


IF go_response IS BOUND.
*  data(v_string) = go_response->get_string_data( ). v-jobpau
  v_string = go_response->get_string_data( ).
  WRITE:/10 v_string.
ENDIF.
