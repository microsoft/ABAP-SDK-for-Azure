class ZCL_ADF_SERVICE_SERVICEBUS definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public

  global friends ZCL_ADF_SERVICE_REPROCESS .

public section.

  methods SEND
    redefinition .
protected section.

  methods GET_SAS_TOKEN
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ADF_SERVICE_SERVICEBUS IMPLEMENTATION.


METHOD get_sas_token.
  DATA : lv_string_to_sign    TYPE string,
         encoded_base_address TYPE string,
         body_xstring         TYPE xstring,
         sign                 TYPE string,
         final_token          TYPE string,
         decoded              TYPE xstring,
         conv                 TYPE REF TO cl_abap_conv_out_ce,
         conv_in              TYPE REF TO cl_abap_conv_in_ce,
         format               TYPE i,
         new_expiry           TYPE string,
         lv_sas_key           TYPE string,
         lv_expiry_time       TYPE string.
  get_epoch_time( RECEIVING rv_expiry_time =  lv_expiry_time ).
  format = 18.
  encoded_base_address = escape( val = iv_baseaddress format = format  ).
  CONCATENATE encoded_base_address  cl_abap_char_utilities=>newline lv_expiry_time INTO lv_string_to_sign.

  conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
  conv->convert( EXPORTING data = lv_string_to_sign IMPORTING buffer = body_xstring ).
  DEFINE encrypt_key.
*    decode_sign( receiving rv_secret = lv_sas_key ).
    IF gv_sas_key IS INITIAL.
      lv_sas_key = read_ssf_key( ).
    ELSE.
        lv_sas_key = read_key( ).
    ENDIF.
    conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    conv->convert( exporting data = lv_sas_key importing buffer = decoded ).

    call method cl_abap_hmac=>calculate_hmac_for_raw
      exporting
        if_algorithm     = 'sha-256'
        if_key           = decoded
        if_data          = body_xstring
        if_length        = 0
      importing
        ef_hmacb64string = sign.
    clear: lv_sas_key, decoded.
  END-OF-DEFINITION.
  encrypt_key.
  new_expiry = lv_expiry_time.
  CONDENSE new_expiry.
*Soc Get policy detials - CR 67877 TR DG2K904448
* getting policy from ZADF_EHUB_POLICY table
  DATA: wa_policy TYPE zadf_ehub_policy,
        lv_policy TYPE zadf_policy.
  IF gv_interface_id IS NOT INITIAL.
    SELECT SINGLE * FROM zadf_ehub_policy INTO wa_policy WHERE interface_id EQ  gv_interface_id.
    IF sy-subrc EQ 0.
      lv_policy = wa_policy-policy.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>policy_not_maintained
          interface_id = gv_interface_id.
    ENDIF.
  ENDIF.
*Eoc CR 67877 TR DG2K904448
  IF NOT sign IS INITIAL.
    sign = escape( val = sign format = format  ).
* Commented below line to retrieve the policy from ZADF_EHUB_POLICY table.
*    CONCATENATE 'SharedAccessSignature sig=' sign '&se=' new_expiry '&skn=' 'RootManageSharedAccessKey' '&sr=' encoded_base_address INTO final_token.
    CONCATENATE 'SharedAccessSignature sig=' sign '&se=' new_expiry '&skn=' lv_policy '&sr=' encoded_base_address INTO final_token."CR 67877 TR DG2K904448
    rv_sas_token = final_token.
  ELSE.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>sas_key_not_generated
        interface_id = gv_interface_id.
  ENDIF.
  CLEAR: lv_policy, wa_policy. "CR 67877 TR DG2K904448
ENDMETHOD.


METHOD send.
  DATA : lo_response     TYPE REF TO if_rest_entity,
         lo_request      TYPE REF TO if_rest_entity,
         lv_expiry       TYPE string,
         lv_sas_token    TYPE string,
         lv_msg          TYPE string,
         lcx_adf_service TYPE REF TO zcx_adf_service.

  IF go_rest_api IS BOUND.
* Read token from headers for Managed Identity/AAD
    IF line_exists( it_headers[ name = gc_mi_auth ] ).
      DATA(lv_processing_method) = gc_mi_auth.
      DATA(lv_aad_token) = it_headers[ name = gc_mi_auth ]-value.
    ENDIF.

    TRY.
        CASE lv_processing_method.
* AAD/Managed Identity token
          WHEN gc_mi_auth.
            CLEAR lv_sas_token.
            lv_sas_token = lv_aad_token.
* SAS keys
          WHEN OTHERS.
            get_sas_token( EXPORTING iv_baseaddress = gv_uri
                           RECEIVING rv_sas_token  = lv_sas_token ).
        ENDCASE.
      CATCH zcx_adf_service INTO lcx_adf_service.
        lv_msg =  lcx_adf_service->get_text( ).
        MESSAGE lv_msg TYPE 'I'.
    ENDTRY.

    add_request_header( iv_name = 'Content-Type' iv_value = 'application/json; charset=utf-8' ).

* Add Managed Identity/AAD/SAS keys to the headers
    add_request_header( iv_name = 'Authorization' iv_value = lv_sas_token ).

    go_rest_api->zif_rest_framework~set_binary_body( request ).
    IF NOT it_headers[] IS INITIAL.
      go_rest_api->zif_rest_framework~set_request_headers( it_header_fields = it_headers[] ).
    ENDIF.
**Rest API call to get response from Azure Destination
    lo_response = go_rest_api->zif_rest_framework~execute( io_entity = lo_request async = gv_asynchronous is_retry = gv_is_try ).
    ev_http_status = go_rest_api->get_status( ).
    IF lo_response IS BOUND.
      response = lo_response->get_string_data( ).
      go_rest_api->close( ).
    ELSE.
      go_rest_api->close( ).
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>restapi_response_not_found
          interface_id = gv_interface_id.
    ENDIF.
  ENDIF.
ENDMETHOD.
ENDCLASS.
