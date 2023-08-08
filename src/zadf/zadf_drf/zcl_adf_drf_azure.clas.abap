class ZCL_ADF_DRF_AZURE definition
  public
  final
  create public .

public section.

  class-methods EXECUTE_OUTBOUND_CALL
    importing
      !IS_AI_HEADER type ZADF_STRU_AI_HEAD
      !IT_AI_INFO type ZADF_TT_AI_INFO
      !IM_CALLING_PROGRAM type DBGLPROG
      !IM_UNIQUE_ID type CHAR50 optional
      !IM_INTERFACE_ID type ZINTERFACE_ID
      !IM_BUSINESSID type ZBUSINESSID
    exporting
      !ET_STATUS type BAPIRET2_T
      !EV_SUCCESS type CHAR1
    raising
      ZCX_ADF_SERVICE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ADF_DRF_AZURE IMPLEMENTATION.


METHOD execute_outbound_call.

**********************************************************************
* Local data declaration
**********************************************************************

  CONSTANTS:
    lc_sep       TYPE char1 VALUE '"',
    lc_sep_space TYPE char2 VALUE ': ',
    lc_space     TYPE char1 VALUE ' ',
    lc_sep_cama  TYPE char2 VALUE ','.


  DATA: ls_ai_info         TYPE zadf_stru_ai_info,
        lt_ai_info         TYPE zadf_tt_ai_info,
        lt_ai_send         TYPE zadf_tt_ai_info,
        oref_ai            TYPE REF TO zcl_adf_service_appinsights,
        oref               TYPE REF TO zcl_adf_service,
        cx_interface       TYPE REF TO zcx_interace_config_missing,
        cx_http            TYPE REF TO zcx_http_client_failed,
        cx_adf_service     TYPE REF TO zcx_adf_service,
        oref_utility       TYPE REF TO zcl_ssf_utility,
        lv_response        TYPE string,
        lv_http_status     TYPE i,
        ls_status          TYPE bapiret2,
        lv_part1           TYPE string,
        lv_part2           TYPE string,
        lv_head_string     TYPE string,
        it_headers         TYPE tihttpnvp,
        wa_headers         TYPE LINE OF tihttpnvp,
        lv_date_time_stamp TYPE string,
        lv_string          TYPE string,
        lv_xstring         TYPE xstring,
        lv_key             TYPE string,
        lv_inst_key        TYPE string.


  DEFINE fill_error.


    ls_status-type = &1.
    ls_status-id   = &2.
    ls_status-number  = &3.
    ls_status-message = &4.

    message id &2 type &1 number &3
                    into ls_status-message." WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ls_status-message_v1 = &5.
    ls_status-message_v2 = &6.
    ls_status-message_v3 = &7.
    ls_status-message_v4 = &8.
    append ls_status to et_status.
    clear ls_status.

  END-OF-DEFINITION.

  DEFINE fill_head_info.
    lv_part1 =   &1.
    lv_part2 =  lc_sep && &2 && lc_sep && ','.
    concatenate lv_part1 lv_part2 into lv_head_string separated by space.
    lv_string = lv_string && lv_head_string.

  END-OF-DEFINITION.

*******************************Get inst key********************
*Create an isntance of decrypting utility
  CREATE OBJECT oref_utility .
  TRY.
      CALL METHOD oref_utility->decode_sign
        EXPORTING
          iv_interfaceid = im_interface_id
        RECEIVING
          rv_key         = lv_key.
    CATCH zcx_ssf_utility .
**************Needs to handle*******************************************


      fill_error 'E' 'ZMSG_AZURE' '007' 'Error in recovering key' is_ai_header-service_line
                  is_ai_header-business_process is_ai_header-sub_process ''.
* Error in recovering key from key store & & & &
  ENDTRY.


  lv_inst_key = lv_key.

*********************************************


  lt_ai_info = it_ai_info.
  SORT lt_ai_info BY key.

  LOOP AT lt_ai_info INTO ls_ai_info.

    AT NEW key.
**********************************************************************
* Build header
**********************************************************************
*      lv_inst_key = ''

      lv_date_time_stamp = sy-datum+0(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2) && 'T' && sy-uzeit+0(2) && ':' && sy-uzeit+2(2) && ':'
                          && sy-uzeit+4(2) && '.' && '6359745Z' && lc_sep && lc_sep_cama .
      CONCATENATE '[{"time":"' lv_date_time_stamp INTO lv_string." SEPARATED BY space.

      lv_string = lv_string && '"iKey": ' && lc_sep.

      CONCATENATE lv_string  lv_inst_key INTO lv_string." SEPARATED BY space.

      lv_string = lv_string && lc_sep && lc_sep_cama.

      lv_string = lv_string &&
            '"name": "Microsoft.ApplicationInsights.' && lv_inst_key && '.Event",'  &&
            '"data": {' &&
                  ' "baseType": "EventData",' &&
                  ' "baseData": {' &&
                         '"ver": 2,' &&
                         '"name": "test",'
           &&         '  "properties": {'.


      fill_head_info '"User":'            sy-uname.

      fill_head_info '"Environment":'     sy-sysid.

      DATA: lv_text TYPE char25 VALUE 'SAP'.

      fill_head_info '"ServiceOffering":' lv_text."'" SAP",'.

      fill_head_info '"ServiceLine":'     is_ai_header-service_line.

      fill_head_info '"BusinessProcess":' is_ai_header-business_process.

      fill_head_info '"SubProcess":'      is_ai_header-sub_process.

      fill_head_info '"Program":'         im_calling_program.

      fill_head_info '"Key":'             ls_ai_info-key.

    ENDAT.
**********************************************************************
* Build Body
**********************************************************************
    lv_string = lv_string && lc_sep && ls_ai_info-field && lc_sep && lc_sep_space.
    lv_part2 =  lc_sep && ls_ai_info-value && lc_sep && ','.

    CONCATENATE lv_string lv_part2 INTO lv_string SEPARATED BY space.

    AT END OF key.
**********************************************************************
* Send data to Aication Insights for each unique Key rows
**********************************************************************
      lv_string = lv_string && '"Message Completed": "END"'.
      lv_string = lv_string &&
                              '},' &&
                              '"measurements": null              }       } }]'.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv_string
*         MIMETYPE = ' '
*         ENCODING =
        IMPORTING
          buffer = lv_xstring
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>error_in_conversion
            interface_id = im_interface_id.
      ENDIF.

      TRY.
**Calling Factory method to instantiate AI client

          oref = zcl_adf_service_factory=>create( iv_interface_id = im_interface_id
                                                  iv_business_identifier = im_businessid ).
          oref_ai ?= oref.

          CALL METHOD oref_ai->send
            EXPORTING
              request        = lv_xstring  "Input XSTRING of SAP Business AI
              it_headers     = it_headers  "Header attributes
            IMPORTING
              response       = lv_response       "Response from AI
              ev_http_status = lv_http_status.   "Status

        CATCH zcx_interace_config_missing INTO cx_interface.
          lv_string = cx_interface->get_text( ).

          fill_error 'E' 'ZMSG_AZURE' '006' lv_string is_ai_header-service_line
                      is_ai_header-business_process is_ai_header-sub_process ls_ai_info-key.

        CATCH zcx_http_client_failed INTO cx_http .
          lv_string = cx_http->get_text( ).

          fill_error 'E' 'ZMSG_AZURE' '006' lv_string is_ai_header-service_line
                      is_ai_header-business_process is_ai_header-sub_process ls_ai_info-key.
        CATCH zcx_adf_service INTO cx_adf_service.
          lv_string = cx_adf_service->get_text( ).

          fill_error 'E' 'ZMSG_AZURE' '006' lv_string is_ai_header-service_line
                      is_ai_header-business_process is_ai_header-sub_process ls_ai_info-key.
      ENDTRY.

      IF lv_http_status NE '201' AND
         lv_http_status NE '200'.
**********************************************************************
* Error
**********************************************************************
        fill_error 'E' 'ZMSG_AZURE' '004' ' ' is_ai_header-service_line
                    is_ai_header-business_process is_ai_header-sub_process ls_ai_info-key.

      ELSE.
**********************************************************************
* Success
**********************************************************************
        fill_error 'S' 'ZMSG_AZURE' '005' ' ' is_ai_header-service_line
                    is_ai_header-business_process is_ai_header-sub_process ls_ai_info-key.
      ENDIF.

      CLEAR: lv_xstring, lv_string, lv_response, lv_response, lv_part1, lv_part2.
    ENDAT.
  ENDLOOP.


ENDMETHOD.
ENDCLASS.
