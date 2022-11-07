class ZCL_ADF_SERVICE_SERVICEBUS definition
  public
  inheriting from ZCL_ADF_SERVICE
  final
  create public

  global friends ZCL_ADF_SERVICE_REPROCESS .

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
ENDCLASS.
