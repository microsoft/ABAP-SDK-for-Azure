*&---------------------------------------------------------------------*
*&  Include           ZTEST_HTTP_REST_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZTEST_HTTP_REST_TOP
*&---------------------------------------------------------------------*

*Define the trace payload  API structure .This can change..this is the current structure as per Kelly's email

      TYPES : BEGIN OF ty_properties ,

              functionmodule TYPE string ,
              developermode TYPE string,
              scenario TYPE string ,
              totalsaptime TYPE  string ,

              END OF ty_properties .

      TYPES : BEGIN OF ty_basedata ,

              ver TYPE  string ,
              name TYPE string ,

              properties TYPE ty_properties ,

              END OF ty_basedata .

      TYPES :  BEGIN OF ty_data ,

               basetype TYPE string ,
               basedata TYPE ty_basedata ,

               END OF ty_data .

      TYPES : BEGIN OF ty_trace_payload,

              name TYPE string ,
              time TYPE string ,
              ikey TYPE string ,
              data TYPE ty_data ,

              END OF ty_trace_payload .

      DATA: go_http_client     TYPE REF TO if_http_client,
            go_rest_client     TYPE REF TO cl_rest_http_client,
            gv_url             TYPE        string,
            gv_uri             TYPE        string,
            gv_client TYPE string ,
            http_status        TYPE        string,
            gv_body            TYPE        string ,
            gv_http_response_status         TYPE  string,
            gv_http_response_reason         TYPE  string,
            gv_http_response_response       TYPE  string,
            gv_http_response_content_len TYPE  string,
            gv_http_response_location       TYPE  string,
            gv_http_response_content_type   TYPE  string .

      DATA: go_exc_root TYPE REF TO cx_root.
      DATA: gv_msg TYPE string.

*data for building the payload
      DATA : gw_trace_payload TYPE ty_trace_payload ,
             gv_json_gen_time TYPE timestampl ,
             gv_http_post_time TYPE timestampl ,
             gv_http_response_time type timestampl .


      DATA : gv_okcode_100 TYPE sy-ucomm  ,
             gv_regen_http_objects type char1.

      DATA :
              gv_timestampl TYPE timestampl.

      DATA: go_json        TYPE REF TO cl_clb_parse_json,
            go_response    TYPE REF TO if_rest_entity,
            go_request     TYPE REF TO if_rest_entity,
            go_sql         TYPE REF TO cx_sy_open_sql_db,
            gv_status      TYPE  i.
