class ZCL_O365_SERVICE definition
  public
  abstract
  create public .

public section.

  constants URI_NOT_MAINTAINED type SOTR_CONC value 'AC162D8534601ED6BAC048BB654CB43F' ##NO_TEXT.
  constants GC_ASYNCHRONOUS type CHAR1 value 'A' ##NO_TEXT.
  constants GC_SEP type CHAR1 value ',' ##NO_TEXT.
  constants GC_SEP1 type CHAR2 value '''' ##NO_TEXT.
  constants GC_SEP2 type CHAR1 value  ')' ##NO_TEXT.
  constants GC_DOT type CHAR1 value '.' ##NO_TEXT.
  constants GC_AUTH type STRING value 'Authorization' ##NO_TEXT.
  constants GC_CONTENT type STRING value 'Content-Type' ##NO_TEXT.
  constants GC_TEXT type STRING value 'text/plain' ##NO_TEXT.
  constants GC_PDF type STRING value 'application/pdf' ##NO_TEXT.
  constants GC_CSV type STRING value 'text/csv' ##NO_TEXT.
  constants GC_XLS type STRING value 'application/vnd.ms-excel' ##NO_TEXT.
  constants GC_BEARER type CHAR6 value 'Bearer' ##NO_TEXT.
  constants GC_FILES type CHAR7 value '/Files(' ##NO_TEXT.
  constants GC_VALUE type CHAR7 value '/$value' ##NO_TEXT.
  constants GC_TAB type CHAR1 value CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB ##NO_TEXT.
  constants GC_REGEX type STRING value ',(?=(?:[^"]*$)|(?:[^"]*"[^"]*"[^"]*)*$)' ##NO_TEXT.
  constants GC_FOL type STRING value 'application/json;odata=verbose' ##NO_TEXT.
  constants GC_X type CHAR1 value 'X' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
      value(IV_SERVICE_ID) type ZAZURE_DEST optional
      value(IV_BUSINESS_IDENTIFIER) type ZBUSINESSID optional
    raising
      ZCX_O365_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
  methods SEND
  abstract
    importing
      value(REQUEST) type XSTRING
      !IT_HEADERS type TIHTTPNVP optional
    exporting
      value(RESPONSE) type STRING
      value(EV_HTTP_STATUS) type I
    raising
      ZCX_O365_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED
      ZCX_O365_SERVICE_SHAREPOINT .
  methods INT_TABLE_TO_XLS_XSTRING
    importing
      value(IT_DATA) type TABLE optional
    exporting
      value(EV_XSTRING) type XSTRING
    raising
      ZCX_O365_SERVICE
      CX_SALV_MSG .
  methods INT_TABLE_TO_TEXT_XSTRING
    importing
      value(IT_DATA) type TABLE optional
    exporting
      value(EV_XSTRING) type XSTRING
    raising
      ZCX_O365_SERVICE .
  methods INT_TABLE_TO_CSV_XSTRING
    importing
      value(IT_DATA) type TABLE optional
    exporting
      value(EV_XSTRING) type XSTRING
    raising
      ZCX_O365_SERVICE .
  methods XLS_XSTRING_TO_INT_TABLE
    importing
      value(RESPONSE) type XSTRING
    exporting
      !GT_DATA type STANDARD TABLE
    raising
      ZCX_O365_SERVICE .
  methods CSV_XSTRING_TO_INT_TABLE
    importing
      !RESPONSE type XSTRING
    exporting
      !GT_DATA type ANY TABLE
    raising
      ZCX_O365_SERVICE .
  methods TXT_XSTRING_TO_INT_TABLE
    importing
      !RESPONSE type XSTRING
    exporting
      !GT_DATA type ANY TABLE
    raising
      ZCX_O365_SERVICE .
  methods JSON_TO_HTTP_FIELDS
    importing
      !IV_RESPONSE_DATA type STRING optional
    returning
      value(ET_FIELDS) type TIHTTPNVP .
protected section.

  data GO_REST_API1 type ref to ZCL_REST_FRAMEWORK .
  data GV_INTERFACE_ID type ZINTERFACE_ID .
  data GV_ASYNCHRONOUS type ABAP_BOOL .
  data GV_IS_TRY type ABAP_BOOL .
  data GV_RFC_DESTINATION type RFCDEST .
  data GV_HOST type STRING .
  data GT_HEADERS type TIHTTPNVP .
  data GV_PATH_PREFIX type STRING .
  data GV_URI type STRING .
  data GV_FILE_NAME type LOCALFILE .
  data GV_FILE_TYPE type CHAR10 .
  data GV_FOLDER_NAME type CHAR100 .
  data GV_CLIENT_ID type STRING .
  data GV_RESOURCE type STRING .
  data GV_INTERFACE_ID_AAD type ZINTERFACE_ID .

  methods GET_REST_API_REF
    importing
      !IV_BUSINESS_IDENTIFIER type ZBUSINESSID
    raising
      ZCX_O365_SERVICE
      ZCX_HTTP_CLIENT_FAILED
      ZCX_INTERACE_CONFIG_MISSING .
  methods GET_TARGET_HOST
    importing
      !IV_DESTINATION type RFCDES-RFCDEST
      !IV_AUTHORITY_CHECK type RFCDISPLAY-RFCTRACE optional
      !IV_BYPASS_BUFF type CHAR1 optional
    exporting
      !EV_SERVER type RFCDISPLAY-RFCHOST
      !EV_PATH_PREFIX type STRING .
private section.

  data GV_SERVICE_ID type ZAZURE_DEST .
  constants GC_SYNCHRONOUS type CHAR1 value 'S' ##NO_TEXT.

  methods GET_INTERFACE_DETAILS
    raising
      ZCX_O365_SERVICE .
  methods CALL_STACK_CHECK
    raising
      ZCX_O365_SERVICE .
  methods ADD_REQUEST_HEADER
    importing
      value(IV_NAME) type STRING
      value(IV_VALUE) type STRING .
ENDCLASS.



CLASS ZCL_O365_SERVICE IMPLEMENTATION.


method ADD_REQUEST_HEADER.
   go_rest_api1->zif_rest_framework~set_request_header( iv_name = iv_name iv_value = iv_value ).
endmethod.


METHOD call_stack_check.
  DATA : lt_abap_stack TYPE abap_callstack,
         lt_syst_stack TYPE sys_callst.

  CONSTANTS: lc_prg     TYPE syrepid VALUE 'ZCL_O365_SERVICE_FACTORY======CP',
             lc_type(6) TYPE c VALUE 'METHOD',
             lc_name    TYPE string VALUE 'CREATE'.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack    = lt_abap_stack
      et_callstack = lt_syst_stack.
  READ TABLE lt_abap_stack TRANSPORTING NO FIELDS
             WITH KEY mainprogram = lc_prg
                      blocktype   = lc_type
                      blockname   = lc_name.
  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>execution_terminated
        interface_id = space.
  ENDIF.

ENDMETHOD.


METHOD constructor.

  call_stack_check( ).
  gv_interface_id = iv_interface_id. " SharePoint Interface ID Whcis is maintained in ZO365_CONFIG
  gv_service_id   = iv_service_id.
  get_interface_details( ).

ENDMETHOD.


  METHOD csv_xstring_to_int_table.
    DATA: lr_struct_descr TYPE REF TO cl_abap_tabledescr,
          lr_type_ref     TYPE REF TO cl_abap_structdescr,
          lr_elemdescr    TYPE REF TO cl_abap_elemdescr.

    DATA:lt_line    TYPE TABLE OF string,
         lt_line2   TYPE TABLE OF string,
         lt_content TYPE STANDARD TABLE OF tdline,
         lt_comp    TYPE cl_abap_structdescr=>component_table,
         ls_data    TYPE lvc_s_fcat,
         lt_data    TYPE lvc_t_fcat,
         lt_dytab   TYPE REF TO data,
         lt_dyline  TYPE REF TO data,
         lv_string  TYPE string,
         lv_data    TYPE string,
         lv_len     TYPE i,
         lv_fname   TYPE dfies.

    FIELD-SYMBOLS:<fs_dyn_table> TYPE STANDARD TABLE,
                  <fs_dyn_wa>    TYPE any,
                  <fs>           TYPE any.

    IF response IS NOT INITIAL.
      CLEAR lv_len.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = response
        IMPORTING
          output_length = lv_len
        TABLES
          binary_tab    = lt_content.

      IF lt_content IS NOT INITIAL.
        CALL FUNCTION 'SCMS_BINARY_TO_STRING'
          EXPORTING
            input_length = lv_len
          IMPORTING
            text_buffer  = lv_string
          TABLES
            binary_tab   = lt_content
          EXCEPTIONS
            failed       = 1
            OTHERS       = 2.

        IF sy-subrc IS INITIAL AND lv_string IS NOT INITIAL.
          SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_line.
*Create sructure of the output table
          TRY.
              lr_struct_descr ?= cl_abap_tabledescr=>describe_by_data( gt_data ).
              lr_type_ref     ?= lr_struct_descr->get_table_line_type( ).
              lt_comp          = lr_type_ref->get_components( ).
            CATCH cx_sy_move_cast_error.
              RAISE EXCEPTION TYPE zcx_o365_service
                EXPORTING
                  textid       = zcx_o365_service=>error_in_conversion
                  interface_id = gv_interface_id.
          ENDTRY.

          LOOP AT lt_comp INTO DATA(ls_comp) WHERE as_include IS INITIAL.
            lr_elemdescr     ?= lr_type_ref->get_component_type( ls_comp-name ).
            lv_fname          = lr_elemdescr->get_ddic_field( ).
            ls_data-fieldname = ls_comp-name.
            ls_data-datatype  = lv_fname-datatype.
            ls_data-inttype   = lv_fname-inttype.
            ls_data-intlen    = lv_fname-outputlen.
            ls_data-decimals  = lv_fname-decimals.
            APPEND ls_data TO lt_data.
            CLEAR: ls_data, ls_comp, lv_fname.
          ENDLOOP.

          IF lt_data IS  NOT INITIAL.
            CALL METHOD cl_alv_table_create=>create_dynamic_table
              EXPORTING
                it_fieldcatalog           = lt_data
              IMPORTING
                ep_table                  = lt_dytab
              EXCEPTIONS
                generate_subpool_dir_full = 1
                OTHERS                    = 2.
            IF sy-subrc IS INITIAL AND lt_dytab IS NOT INITIAL.
              ASSIGN lt_dytab->* TO <fs_dyn_table>.
              CREATE DATA lt_dyline LIKE LINE OF <fs_dyn_table>.
              ASSIGN lt_dyline->* TO <fs_dyn_wa>.
*Passing data to output table
              LOOP AT lt_line INTO DATA(ls_string).
                REPLACE ALL OCCURRENCES OF REGEX gc_regex IN ls_string WITH gc_tab.
                SPLIT ls_string AT gc_tab INTO TABLE lt_line2.
                LOOP AT lt_data INTO ls_data.
                  READ TABLE lt_line2 INTO DATA(ls_line2) INDEX sy-tabix.
                  IF sy-subrc IS INITIAL.
                    ASSIGN COMPONENT ls_data-fieldname OF STRUCTURE <fs_dyn_wa> TO <fs>.
                    <fs> = ls_line2.
                    UNASSIGN <fs>.
                  ENDIF.
                  CLEAR: ls_data,ls_line2.
                ENDLOOP.
                IF <fs_dyn_wa> IS ASSIGNED.
                  APPEND <fs_dyn_wa> TO <fs_dyn_table>.
                ENDIF.
                CLEAR: lt_line2, ls_string.
              ENDLOOP.
              IF <fs_dyn_table> IS ASSIGNED.
                gt_data = <fs_dyn_table>.
              ELSE.
                RAISE EXCEPTION TYPE zcx_o365_service
                  EXPORTING
                    textid       = zcx_o365_service=>error_in_conversion
                    interface_id = gv_interface_id.
              ENDIF.
            ELSEIF sy-subrc IS NOT INITIAL.
              RAISE EXCEPTION TYPE zcx_o365_service
                EXPORTING
                  textid       = zcx_o365_service=>error_in_conversion
                  interface_id = gv_interface_id.
            ENDIF.
          ELSE.
            RAISE EXCEPTION TYPE zcx_o365_service
              EXPORTING
                textid       = zcx_o365_service=>error_in_conversion
                interface_id = gv_interface_id.
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE zcx_o365_service
            EXPORTING
              textid       = zcx_o365_service=>xstring_to_string_error
              interface_id = gv_interface_id.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_o365_service
          EXPORTING
            textid       = zcx_o365_service=>xstring_to_string_error
            interface_id = gv_interface_id.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>no_input
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


METHOD get_interface_details.


  DATA : ls_config      TYPE zo365_config,
         lv_host        TYPE rfcdisplay-rfchost,
         lv_host_s      TYPE string,
         lv_path_prefix TYPE string.

* Check fro Interface id in O365 Config table
  SELECT SINGLE *
           FROM zo365_config
           INTO ls_config
          WHERE interface_id = gv_interface_id.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>interface_not_available
        interface_id = gv_interface_id.
  ENDIF.

  gv_client_id = ls_config-client_id.
  gv_resource = ls_config-resource_id.
  gv_interface_id_aad = ls_config-aad_interface_id.

* Get RFC Destination for Interface maintained in zo365_config
  SELECT SINGLE destination
           FROM zrest_config
           INTO gv_rfc_destination
          WHERE interface_id = gv_interface_id.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>interface_not_available
        interface_id = gv_interface_id.
  ENDIF.

**Getting Target host of RFC destination
  get_target_host( EXPORTING iv_destination = gv_rfc_destination
                   IMPORTING ev_server      = lv_host
                             ev_path_prefix = lv_path_prefix ).

  gv_host        = lv_host.
  gv_path_prefix = lv_path_prefix.
  lv_host_s      = lv_host.

  CONCATENATE lv_host_s lv_path_prefix INTO gv_uri.

  IF gv_uri IS INITIAL.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>uri_not_maintained
        interface_id = gv_interface_id.
  ENDIF.

  IF ls_config-service_type IS INITIAL.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>call_type_not_maintained
        interface_id = gv_interface_id.
  ELSE.
    IF ls_config-service_type EQ gc_synchronous.
      gv_asynchronous = abap_false.
    ELSEIF ls_config-service_type EQ gc_asynchronous.
      gv_asynchronous = abap_true.
    ENDIF.
  ENDIF.

  gv_is_try = ls_config-is_try.

ENDMETHOD.


METHOD get_rest_api_ref.

  DATA : lcx_interface TYPE REF TO zcx_interace_config_missing,
         lcx_http      TYPE REF TO zcx_http_client_failed,
         lv_method     TYPE char20.
  IF go_rest_api1 IS INITIAL.
    CLEAR lv_method.
    SELECT SINGLE method
             FROM zrest_conf_misc
             INTO lv_method
            WHERE interface_id EQ gv_interface_id.
    IF sy-subrc IS INITIAL AND lv_method IS INITIAL.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>method_not_maintained
          interface_id = gv_interface_id.
    ELSEIF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_o365_service=>interface_not_available
          interface_id = gv_interface_id.
    ENDIF.
    TRY.
        CREATE OBJECT go_rest_api1
          EXPORTING
            interface_name      = gv_interface_id       "Mandatory
            business_identifier = iv_business_identifier
            method              = lv_method.               "For troubleshooting
      CATCH zcx_interace_config_missing INTO lcx_interface.
        RAISE EXCEPTION lcx_interface.
      CATCH zcx_http_client_failed INTO lcx_http .
        RAISE EXCEPTION lcx_http.
    ENDTRY.
*Optional - To help developer understand the origin of call
    IF go_rest_api1 IS BOUND.
      go_rest_api1->set_callingmethod( EXPORTING zimethodname = 'SEND' ).
*Optional - To help developer understand the origin of call
      go_rest_api1->set_callingprogram( EXPORTING ziclassname = 'ZCL_O365_SERVICE' ).
    ELSE. " IF go_rest_api IS BOUND.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>error_rest_api_instance
          interface_id = gv_interface_id.
    ENDIF. "     IF go_rest_api IS BOUND.
  ENDIF. "  IF go_rest_api1 IS BOUND.
ENDMETHOD.


method GET_TARGET_HOST.

     CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
    EXPORTING
      destination             = iv_destination
      authority_check         = iv_authority_check
      bypass_buf              = iv_bypass_buff
    IMPORTING
      server                  = ev_server
      path_prefix             = ev_path_prefix
    EXCEPTIONS
      authority_not_available = 1
      destination_not_exist   = 2
      information_failure     = 3
      internal_failure        = 4
      no_http_destination     = 5
      OTHERS                  = 6.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    RAISE EXCEPTION TYPE zcx_adf_service
      EXPORTING
        textid       = zcx_o365_service=>read_error_rfc_destination
        interface_id = gv_interface_id.
  ENDIF.


endmethod.


METHOD int_table_to_csv_xstring.

  CONSTANTS : lc_sep    TYPE char1 VALUE ','.
  DATA: lt_data         TYPE REF TO data,
        lv_string       TYPE string.
  FIELD-SYMBOLS:
            <lft_tab>   TYPE ANY TABLE,
            <lfs_line>  TYPE any,
            <lfs_value> TYPE any.

  IF it_data IS NOT INITIAL.
    GET REFERENCE OF it_data INTO lt_data.
    ASSIGN lt_data->* TO <lft_tab>.
    IF <lft_tab> IS ASSIGNED.
      LOOP AT <lft_tab> ASSIGNING <lfs_line>.
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <lfs_line> TO <lfs_value>.
          IF sy-subrc IS NOT INITIAL.
            EXIT.
          ELSE.
            CONCATENATE lv_string <lfs_value> lc_sep INTO  lv_string.
          ENDIF.
        ENDDO.
* new Line
        CONCATENATE lv_string cl_abap_char_utilities=>newline  INTO lv_string.
      ENDLOOP.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv_string
        IMPORTING
          buffer = ev_xstring
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_o365_service
          EXPORTING
            textid       = zcx_o365_service=>error_con_xstring
            interface_id = gv_interface_id.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>empty_table_error
          interface_id = gv_interface_id.
    ENDIF. " IF <lft_tab> IS ASSIGNED.
  ELSE.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>empty_table_error
        interface_id = gv_interface_id.
  ENDIF. " iF ct_data IS NOT INITIAL.
ENDMETHOD.


METHOD int_table_to_text_xstring.

  DATA: lt_data        TYPE REF TO data,
        lv_string      TYPE string.

  FIELD-SYMBOLS:
            <lft_tab>   TYPE ANY TABLE,
            <lfs_line>  TYPE any,
            <lfs_value> TYPE any.

  IF it_data IS NOT INITIAL.
    GET REFERENCE OF it_data INTO lt_data.
    ASSIGN lt_data->* TO <lft_tab>.
    IF <lft_tab> IS ASSIGNED.
      LOOP AT <lft_tab> ASSIGNING <lfs_line>.
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <lfs_line> TO <lfs_value>.
          IF sy-subrc IS NOT INITIAL.
            EXIT.
          ELSE.
            CONCATENATE lv_string <lfs_value> cl_abap_char_utilities=>horizontal_tab INTO  lv_string.
          ENDIF.
        ENDDO.
* new Line
        CONCATENATE lv_string cl_abap_char_utilities=>newline  INTO lv_string.
      ENDLOOP.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv_string
        IMPORTING
          buffer = ev_xstring
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_o365_service
          EXPORTING
            textid       = zcx_o365_service=>error_con_xstring
            interface_id = gv_interface_id.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>empty_table_error
          interface_id = gv_interface_id.
    ENDIF. " IF <lft_tab> IS ASSIGNED.
  ELSE.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>empty_table_error
        interface_id = gv_interface_id.

  ENDIF. " iF ct_data IS NOT INITIAL.
ENDMETHOD.


METHOD int_table_to_xls_xstring.
  TYPE-POOLS: slis.

  DATA:  lt_fcat        TYPE lvc_t_fcat,
         lt_data        TYPE REF TO data,
         lv_flavour     TYPE string,
         lv_version     TYPE string,
         lcx_salv_msg   TYPE REF TO cx_salv_msg,
         lo_result_data TYPE REF TO cl_salv_ex_result_data_table,
         lo_columns     TYPE REF TO cl_salv_columns_table,
         lo_aggreg      TYPE REF TO cl_salv_aggregations,
         lo_salv_table  TYPE REF TO cl_salv_table,
         lv_file_type   TYPE salv_bs_constant.
  FIELD-SYMBOLS:
            <lft_tab>   TYPE ANY TABLE.

  IF it_data IS NOT INITIAL.
    GET REFERENCE OF it_data INTO lt_data.
    ASSIGN lt_data->* TO <lft_tab>.
    IF <lft_tab> IS ASSIGNED.
      lv_file_type = if_salv_bs_xml=>c_type_xlsx.
      TRY .
          cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = lo_salv_table
          CHANGING
            t_table      = <lft_tab> ).
        CATCH cx_salv_msg INTO lcx_salv_msg.
          RAISE EXCEPTION lcx_salv_msg.
      ENDTRY.
      "get colums & aggregation infor to create fieldcat
      lo_columns  = lo_salv_table->get_columns( ).
      lo_aggreg   = lo_salv_table->get_aggregations( ).
      lt_fcat     =  cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                    r_columns      = lo_columns
                                    r_aggregations = lo_aggreg ).
      IF cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_25 OR
         cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_26.
        lo_result_data = cl_salv_ex_util=>factory_result_data_table(
            r_data                      = lt_data
            t_fieldcatalog              = lt_fcat
        ).
        CASE cl_salv_bs_a_xml_base=>get_version( ).
          WHEN if_salv_bs_xml=>version_25.
            lv_version = if_salv_bs_xml=>version_25.
          WHEN if_salv_bs_xml=>version_26.
            lv_version = if_salv_bs_xml=>version_26.
        ENDCASE.
        lv_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export.
        "transformation of data to excel
        CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
          EXPORTING
            xml_type      = lv_file_type
            xml_version   = lv_version
            r_result_data = lo_result_data
            xml_flavour   = lv_flavour
            gui_type      = if_salv_bs_xml=>c_gui_type_gui
          IMPORTING
            xml           = ev_xstring.
        IF ev_xstring IS INITIAL.
          RAISE EXCEPTION TYPE zcx_o365_service
            EXPORTING
              textid       = zcx_o365_service=>string_to_xstring_error
              interface_id = gv_interface_id.
        ENDIF.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>empty_table_error
          interface_id = gv_interface_id.
    ENDIF. " IF <lft_tab> IS ASSIGNED.
  ELSE.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>empty_table_error
        interface_id = gv_interface_id.
  ENDIF. " IF ct_data IS NOT INITIAL.
ENDMETHOD.


  METHOD json_to_http_fields.

    DATA: ls_fields       TYPE ihttpnvp,
          l_node          TYPE REF TO if_sxml_node,
          l_error         TYPE string,
          lr_open_element TYPE REF TO  if_sxml_open_element,
          lt_attributes   TYPE if_sxml_attribute=>attributes,
          ls_attribute    LIKE LINE OF lt_attributes,
          lr_value_node   TYPE REF TO if_sxml_value_node,
          json            TYPE xstring,
          reader          TYPE REF TO if_sxml_reader,
          parse_error     TYPE REF TO cx_sxml_parse_error.
    TRY .
        json = cl_abap_codepage=>convert_to( iv_response_data ).
        reader = cl_sxml_string_reader=>create( json ).

        DO.
          CLEAR ls_fields.
          l_node = reader->read_next_node( ).

          IF l_node IS INITIAL.
            EXIT.
          ENDIF.
          CASE l_node->type.
            WHEN if_sxml_node=>co_nt_element_open.
              lr_open_element ?= l_node.
              lt_attributes  = lr_open_element->get_attributes( ).
              IF lt_attributes IS NOT INITIAL.
                " get name
                READ TABLE lt_attributes INDEX 1 INTO ls_attribute.
                ls_fields-name = ls_attribute->get_value( ).
                " get value
                l_node = reader->read_next_node( ).
                IF l_node->type = if_sxml_node=>co_nt_value..
                  lr_value_node ?= l_node .
                  ls_fields-value = lr_value_node->get_value( ).
                  " add field into the result table
                  APPEND ls_fields TO et_fields.
                ENDIF.
              ENDIF.
            WHEN OTHERS.
              " do nothing
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error INTO parse_error.
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid       = zcx_adf_service=>parse_error
            interface_id = gv_interface_id.
    ENDTRY.

  ENDMETHOD.


  METHOD txt_xstring_to_int_table.
    DATA:lr_struct_descr TYPE REF TO cl_abap_tabledescr,
         lr_type_ref     TYPE REF TO cl_abap_structdescr,
         lr_elemdescr    TYPE REF TO cl_abap_elemdescr.

    DATA:lt_line    TYPE TABLE OF string,
         lt_line2   TYPE TABLE OF string,
         lt_content TYPE STANDARD TABLE OF tdline,
         lt_comp    TYPE cl_abap_structdescr=>component_table,
         ls_data    TYPE lvc_s_fcat,
         lt_data    TYPE lvc_t_fcat,
         lt_dytab   TYPE REF TO data,
         lt_dyline  TYPE REF TO data,
         lv_string  TYPE string,
         lv_data    TYPE string,
         lv_len     TYPE i,
         lv_fname   TYPE dfies.

    FIELD-SYMBOLS:<fs_dyn_table> TYPE STANDARD TABLE,
                  <fs_dyn_wa>    TYPE any,
                  <fs>           TYPE any.

    IF response IS NOT INITIAL.
      CLEAR lv_len.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = response
        IMPORTING
          output_length = lv_len
        TABLES
          binary_tab    = lt_content.

      IF lt_content IS NOT INITIAL.
        CALL FUNCTION 'SCMS_BINARY_TO_STRING'
          EXPORTING
            input_length = lv_len
          IMPORTING
            text_buffer  = lv_string
          TABLES
            binary_tab   = lt_content
          EXCEPTIONS
            failed       = 1
            OTHERS       = 2.
        IF sy-subrc IS INITIAL AND lv_string IS NOT INITIAL.

          SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE lt_line.
*Create sructure of the output table
          TRY.
              lr_struct_descr ?= cl_abap_tabledescr=>describe_by_data( gt_data ).
              lr_type_ref     ?= lr_struct_descr->get_table_line_type( ).
              lt_comp          = lr_type_ref->get_components( ).
            CATCH cx_sy_move_cast_error .
              RAISE EXCEPTION TYPE zcx_o365_service
                EXPORTING
                  textid       = zcx_o365_service=>error_in_conversion
                  interface_id = gv_interface_id.
          ENDTRY.

          LOOP AT lt_comp INTO DATA(ls_comp) WHERE as_include IS INITIAL.
            lr_elemdescr     ?= lr_type_ref->get_component_type( ls_comp-name ).
            lv_fname          = lr_elemdescr->get_ddic_field( ).
            ls_data-fieldname = ls_comp-name.
            ls_data-datatype  = lv_fname-datatype.
            ls_data-inttype   = lv_fname-inttype.
            ls_data-intlen    = lv_fname-outputlen.
            ls_data-decimals  = lv_fname-decimals.
            APPEND ls_data TO lt_data.
            CLEAR: ls_data, ls_comp, lv_fname.
          ENDLOOP.

          IF sy-subrc IS INITIAL AND lt_data IS  NOT INITIAL.
            CALL METHOD cl_alv_table_create=>create_dynamic_table
              EXPORTING
                it_fieldcatalog           = lt_data
              IMPORTING
                ep_table                  = lt_dytab
              EXCEPTIONS
                generate_subpool_dir_full = 1
                OTHERS                    = 2.
            IF sy-subrc IS INITIAL AND lt_dytab IS NOT INITIAL.
              ASSIGN lt_dytab->* TO <fs_dyn_table>.
              CREATE DATA lt_dyline LIKE LINE OF <fs_dyn_table>.
              ASSIGN lt_dyline->* TO <fs_dyn_wa>.
*Passing data to output table
              LOOP AT lt_line INTO DATA(ls_string).
                REPLACE ALL OCCURRENCES OF  REGEX gc_regex IN ls_string WITH gc_tab.
                SPLIT ls_string AT gc_tab INTO TABLE lt_line2.
                LOOP AT lt_data INTO ls_data.
                  READ TABLE lt_line2 INTO DATA(ls_line2) INDEX sy-tabix.
                  IF sy-subrc IS INITIAL.
                    ASSIGN COMPONENT ls_data-fieldname OF STRUCTURE <fs_dyn_wa> TO <fs>.
                    <fs> = ls_line2.
                    UNASSIGN <fs>.
                  ENDIF.
                  CLEAR:ls_line2, ls_data.
                ENDLOOP.
                IF <fs_dyn_wa> IS ASSIGNED.
                  APPEND <fs_dyn_wa> TO <fs_dyn_table>.
                ENDIF.
                CLEAR: lt_line2, ls_string.
              ENDLOOP.
              IF <fs_dyn_table> IS ASSIGNED.
                gt_data = <fs_dyn_table>.
              ELSE.
                RAISE EXCEPTION TYPE zcx_o365_service
                  EXPORTING
                    textid       = zcx_o365_service=>error_in_conversion
                    interface_id = gv_interface_id.
              ENDIF.
            ELSEIF sy-subrc IS NOT INITIAL.
              RAISE EXCEPTION TYPE zcx_o365_service
                EXPORTING
                  textid       = zcx_o365_service=>error_in_conversion
                  interface_id = gv_interface_id.
            ENDIF.
          ELSE.
            RAISE EXCEPTION TYPE zcx_o365_service
              EXPORTING
                textid       = zcx_o365_service=>error_in_conversion
                interface_id = gv_interface_id.
          ENDIF.
        ELSE.
          RAISE EXCEPTION TYPE zcx_o365_service
            EXPORTING
              textid       = zcx_o365_service=>xstring_to_string_error
              interface_id = gv_interface_id.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_o365_service
          EXPORTING
            textid       = zcx_o365_service=>xstring_to_string_error
            interface_id = gv_interface_id.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>no_input
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.


  METHOD xls_xstring_to_int_table.
    DATA:lr_struct_descr TYPE REF TO cl_abap_tabledescr,
         lr_type_ref     TYPE REF TO cl_abap_structdescr,
         lr_elemdescr    TYPE REF TO cl_abap_elemdescr.

    FIELD-SYMBOLS:<lt_data>      TYPE STANDARD TABLE,
                  <fs_data>      TYPE any,
                  <fs_line>      TYPE any,
                  <fs_dyn_wa>    TYPE any,
                  <fs>           TYPE any,
                  <fs_dyn_table> TYPE STANDARD TABLE,
                  <fs_field>     TYPE any.

    DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet .

    DATA: lv_filename TYPE string,
          lv_fname    TYPE dfies.

    DATA:lt_line   TYPE TABLE OF string,
         lt_line2  TYPE TABLE OF string,
         ls_line   TYPE string,
         lt_comp   TYPE cl_abap_structdescr=>component_table,
         ls_data   TYPE lvc_s_fcat,
         lt_data   TYPE lvc_t_fcat,
         lt_dytab  TYPE REF TO data,
         lt_dyline TYPE REF TO data.


    IF response IS NOT INITIAL.
      TRY .
          lv_filename = gv_file_name.
          lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
                                  document_name = lv_filename
                                  xdocument     = response ) .
        CATCH cx_fdt_excel_core .
          RAISE EXCEPTION TYPE zcx_o365_service
            EXPORTING
              textid       = zcx_o365_service=>error_in_conversion
              interface_id = gv_interface_id.
      ENDTRY.

      lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
       IMPORTING
         worksheet_names = DATA(lt_worksheets) ).

      IF lt_worksheets IS NOT INITIAL.
        READ TABLE lt_worksheets INTO DATA(lv_worksheetname) INDEX 1.
        IF sy-subrc IS INITIAL.
          DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                                   lv_worksheetname ).
          "excel data in internal table
          ASSIGN lo_data_ref->* TO <lt_data>.
          CLEAR lv_worksheetname.
          IF <lt_data> IS NOT ASSIGNED.
            RAISE EXCEPTION TYPE zcx_o365_service
              EXPORTING
                textid       = zcx_o365_service=>empty_table_error
                interface_id = gv_interface_id.
          ENDIF.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_o365_service
          EXPORTING
            textid       = zcx_o365_service=>error_in_conversion
            interface_id = gv_interface_id.
      ENDIF.
*Create sructure of the output table
      TRY.
          lr_struct_descr ?= cl_abap_tabledescr=>describe_by_data( gt_data ).
          lr_type_ref     ?= lr_struct_descr->get_table_line_type( ).
          lt_comp          = lr_type_ref->get_components( ).
        CATCH cx_sy_move_cast_error .
          RAISE EXCEPTION TYPE zcx_o365_service
            EXPORTING
              textid       = zcx_o365_service=>error_in_conversion
              interface_id = gv_interface_id.
      ENDTRY.

      LOOP AT lt_comp INTO DATA(ls_comp) WHERE as_include IS INITIAL.
        lr_elemdescr      ?= lr_type_ref->get_component_type( ls_comp-name ).
        lv_fname          = lr_elemdescr->get_ddic_field( ).
        ls_data-fieldname = ls_comp-name.
        ls_data-datatype  = lv_fname-datatype.
        ls_data-inttype   = lv_fname-inttype.
        ls_data-intlen    = lv_fname-outputlen.
        ls_data-decimals  = lv_fname-decimals.
        APPEND ls_data TO lt_data.
        CLEAR: ls_data, ls_comp, lv_fname.
      ENDLOOP.

      IF lt_data IS  NOT INITIAL.
        DESCRIBE TABLE lt_data LINES DATA(lv_lines).
        CALL METHOD cl_alv_table_create=>create_dynamic_table
          EXPORTING
            it_fieldcatalog           = lt_data
          IMPORTING
            ep_table                  = lt_dytab
          EXCEPTIONS
            generate_subpool_dir_full = 1
            OTHERS                    = 2.
        IF sy-subrc IS INITIAL and lt_dytab IS NOT INITIAL.
          ASSIGN lt_dytab->* TO <fs_dyn_table>.
          CREATE DATA lt_dyline LIKE LINE OF <fs_dyn_table>.
          ASSIGN lt_dyline->* TO <fs_dyn_wa>.

          LOOP AT <lt_data> ASSIGNING <fs_data>.
            DO lv_lines TIMES.
              ASSIGN COMPONENT sy-index OF STRUCTURE <fs_data> TO <fs_field> .
              IF <fs_field> IS ASSIGNED .
                IF ls_line IS INITIAL.
                  ls_line = <fs_field>.
                ELSE.
                  CONCATENATE ls_line <fs_field> INTO ls_line SEPARATED BY gc_tab.
                ENDIF.
              ENDIF.
            ENDDO.
            APPEND ls_line TO lt_line.
            CLEAR ls_line.
          ENDLOOP.
*Passing the data to output table
          LOOP AT lt_line INTO ls_line.
            SPLIT ls_line AT gc_tab INTO TABLE lt_line2.
            LOOP AT lt_data INTO ls_data.
              READ TABLE lt_line2 INTO DATA(ls_line2) INDEX sy-tabix.
              IF sy-subrc IS INITIAL.
                ASSIGN COMPONENT ls_data-fieldname OF STRUCTURE <fs_dyn_wa> TO <fs>.
                <fs> = ls_line2.
                UNASSIGN <fs>.
              ENDIF.
              CLEAR: ls_data,ls_line2.
            ENDLOOP.
            IF <fs_dyn_wa> IS ASSIGNED.
              APPEND <fs_dyn_wa> TO <fs_dyn_table>.
            ENDIF.
            CLEAR:ls_line, lt_line2.
          ENDLOOP.

          IF <fs_dyn_table> IS ASSIGNED.
            gt_data = <fs_dyn_table>.
          ELSE.
            RAISE EXCEPTION TYPE zcx_o365_service
              EXPORTING
                textid       = zcx_o365_service=>error_in_conversion
                interface_id = gv_interface_id.
          ENDIF.
        ELSEIF sy-subrc IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_o365_service
            EXPORTING
              textid       = zcx_o365_service=>error_in_conversion
              interface_id = gv_interface_id.
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_o365_service
          EXPORTING
            textid       = zcx_o365_service=>error_in_conversion
            interface_id = gv_interface_id.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_o365_service
        EXPORTING
          textid       = zcx_o365_service=>no_input
          interface_id = gv_interface_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
