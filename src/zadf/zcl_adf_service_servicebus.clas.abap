class ZCL_ADF_SERVICE_SERVICEBUS definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_adf_service_factory.

public section.
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
    decode_sign( receiving rv_secret = lv_sas_key ).
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
  IF NOT sign IS INITIAL.
    DATA wa_policy TYPE zadf_ehub_policy.
    SELECT SINGLE * FROM zadf_ehub_policy INTO wa_policy WHERE interface_id EQ  gv_interface_id.
    IF sy-subrc EQ 0.
      sign = escape( val = sign format = format  ).
      CONCATENATE 'SharedAccessSignature sig=' sign '&se=' new_expiry '&skn=' wa_policy-policy '&sr=' encoded_base_address INTO final_token.
      rv_sas_token = final_token.
    ENDIF.
  ELSE.
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_adf_service=>sas_key_not_generated
        interface_id = gv_interface_id.
  ENDIF.
ENDMETHOD.
ENDCLASS.
