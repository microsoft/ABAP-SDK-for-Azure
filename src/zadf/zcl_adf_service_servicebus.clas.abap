CLASS zcl_adf_service_servicebus DEFINITION
  PUBLIC
  INHERITING FROM zcl_adf_service
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_adf_service_factory.



  PUBLIC SECTION.
  PROTECTED SECTION.



    METHODS get_sas_token
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.





CLASS zcl_adf_service_servicebus IMPLEMENTATION.




  METHOD get_sas_token.
    DATA : lv_string_to_sign    TYPE string,
           encoded_base_address TYPE string,
           body_xstring         TYPE xstring,
           sign                 TYPE string,
           final_token          TYPE string,
           decoded              TYPE xstring,
           conv                 TYPE REF TO cl_abap_conv_out_ce,
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
      decode_sign( RECEIVING rv_secret = lv_sas_key ).
      conv = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
      conv->convert( EXPORTING data = lv_sas_key IMPORTING buffer = decoded ).



      CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
        EXPORTING
          if_algorithm     = 'sha-256'
          if_key           = decoded
          if_data          = body_xstring
          if_length        = 0
        IMPORTING
          ef_hmacb64string = sign.
      CLEAR: lv_sas_key, decoded.
    END-OF-DEFINITION.
    encrypt_key.
    new_expiry = lv_expiry_time.
    CONDENSE new_expiry.
    IF NOT sign IS INITIAL.
      sign = escape( val = sign format = format  ).
      CONCATENATE 'SharedAccessSignature sig=' sign '&se=' new_expiry '&skn=' 'RootManageSharedAccessKey' '&sr=' encoded_base_address INTO final_token.
      rv_sas_token = final_token.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>sas_key_not_generated
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
