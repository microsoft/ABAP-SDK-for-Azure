*&---------------------------------------------------------------------*
*&  Include           ZTEST_HTTP_REST_FO1
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZTEST_HTTP_REST_FO1
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0100 .

  DATA : lv_timestamp_start TYPE timestampl ,
         lv_timestamp_end  TYPE timestampl.

  CASE gv_okcode_100.

    WHEN 'EXIT' OR 'CANC' OR 'BACK ' .
      " Finish program
      LEAVE PROGRAM.

    WHEN 'GEN_PAYLOAD'.

      CLEAR : gv_body ,
              gv_msg,
              gv_json_gen_time ,
              gv_http_post_time ,
              gv_http_response_time,
              gv_http_response_status  ,
              gv_http_response_reason  ,
              gv_http_response_response  ,
              gv_http_response_content_len  ,
              gv_http_response_location  ,
              gv_http_response_content_type .

      GET TIME STAMP FIELD lv_timestamp_start .

**get the json string for the input payload structure
*      zcl_json_xml_builder=>build_json_string_v2(
*        EXPORTING
*          in_source_data   = gw_trace_payload
*        IMPORTING
*         out_json_string  = gv_body
**        out_json_xstring = out_json_xstring
*             ).

      GET TIME STAMP FIELD lv_timestamp_end.

      CALL METHOD cl_abap_tstmp=>subtract
        EXPORTING
          tstmp1 = lv_timestamp_end
          tstmp2 = lv_timestamp_start
        RECEIVING
          r_secs = gv_json_gen_time.

    WHEN 'POST_PAYLOAD' .

      IF gv_regen_http_objects IS NOT INITIAL .

        CLEAR : go_http_client , go_rest_client , go_request , go_response .

      ENDIF.

      IF gv_body IS INITIAL .

        gv_msg = 'JSON Payload is empty. Click on Generate Payload' .

      ENDIF.

      GET TIME STAMP FIELD lv_timestamp_start .

      TRY.

          IF go_http_client IS NOT BOUND .

            cl_http_client=>create_by_url(
                EXPORTING
                  url                = gv_url
                IMPORTING
                  client             = go_http_client
                EXCEPTIONS
                  argument_not_found = 1
                  plugin_not_active  = 2
                  internal_error     = 3
                  OTHERS             = 4 ).
            IF sy-subrc <> 0.
              " error handling
            ENDIF.

*    go_http_client->authenticate( username =  '' password = '' ).

            go_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.

            gv_client = sy-mandt.  "set the client

            go_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client  value = gv_client ).

*    go_http_client->propertytype_logon_popup = go_http_client->co_disabled.

            CREATE OBJECT go_rest_client
              EXPORTING
                io_http_client = go_http_client.

            go_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

            cl_http_utility=>set_request_uri(
                request = go_http_client->request    " HTTP Framework (iHTTP) HTTP Request
                uri     = gv_uri                     " URI String (in the Form of /path?query-string)
                   ).

* Set Payload or body ( JSON or XML)
            go_request = go_rest_client->if_rest_client~create_request_entity( ).

            go_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).

          ENDIF.

          IF go_request IS BOUND AND go_rest_client IS BOUND .

            go_request->set_string_data( gv_body ).

* POST
            go_rest_client->if_rest_resource~post( go_request ).

            GET TIME STAMP FIELD lv_timestamp_end.

            CALL METHOD cl_abap_tstmp=>subtract
              EXPORTING
                tstmp1 = lv_timestamp_end
                tstmp2 = lv_timestamp_start
              RECEIVING
                r_secs = gv_http_post_time.

            GET TIME STAMP FIELD lv_timestamp_start.

* Collect response
            go_response = go_rest_client->if_rest_client~get_response_entity( ).

            GET TIME STAMP FIELD lv_timestamp_end.

            CALL METHOD cl_abap_tstmp=>subtract
              EXPORTING
                tstmp1 = lv_timestamp_end
                tstmp2 = lv_timestamp_start
              RECEIVING
                r_secs = gv_http_response_time.

            IF go_response IS BOUND .

              gv_http_response_status = gv_status = go_response->get_header_field( '~status_code' ).

              gv_http_response_reason = go_response->get_header_field( '~status_reason' ).

              gv_http_response_content_len = go_response->get_header_field( 'content-length' ).

              gv_http_response_location = go_response->get_header_field( 'location' ).

              gv_http_response_content_type = go_response->get_header_field( 'content-type' ).

              gv_http_response_response = go_response->get_string_data( ).

            ENDIF.

          ENDIF.

        CATCH cx_rest_client_exception INTO go_exc_root .

          gv_msg = go_exc_root->get_text( ).
          WRITE: / gv_msg.

        CATCH cx_st_error INTO go_exc_root.

          gv_msg = go_exc_root->get_text( ).
          WRITE: / gv_msg.

      ENDTRY.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " USER_COMMAND_0100
