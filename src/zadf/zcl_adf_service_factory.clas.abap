CLASS zcl_adf_service_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_service_eventhub TYPE zazure_dest VALUE 'EVENTHUB'. "#EC NOTEXT
    CONSTANTS gc_service_blob TYPE zazure_dest VALUE 'BLOB'. "#EC NOTEXT
    CONSTANTS gc_service_docdb TYPE zazure_dest VALUE 'DOCUMENTDB'. "#EC NOTEXT
    CONSTANTS gc_service_servicebus TYPE zazure_dest VALUE 'SERVICEBUS'. "#EC NOTEXT
    CONSTANTS gc_service_aad TYPE zazure_dest VALUE 'AAD'.  "#EC NOTEXT
    CONSTANTS gc_service_keyvault TYPE zazure_dest VALUE 'KV'. "#EC NOTEXT
    CONSTANTS gc_service_graph TYPE zazure_dest VALUE 'GRAPH'. "#EC NOTEXT

    CLASS-METHODS create
      IMPORTING
        VALUE(iv_interface_id)        TYPE zinterface_id
        VALUE(iv_business_identifier) TYPE zbusinessid OPTIONAL
      RETURNING
        VALUE(ro_service)             TYPE REF TO zcl_adf_service
      RAISING
        zcx_adf_service
        zcx_interace_config_missing
        zcx_http_client_failed .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adf_service_factory IMPLEMENTATION.


  METHOD create.
    DATA : lv_interface_type TYPE zadf_config-interface_type.
    SELECT SINGLE interface_type FROM zadf_config
                                 INTO lv_interface_type
                                 WHERE interface_id EQ iv_interface_id.
    IF sy-subrc EQ 0.
      CASE lv_interface_type.
        WHEN gc_service_eventhub.
          CREATE OBJECT ro_service TYPE zcl_adf_service_eventhub
            EXPORTING
              iv_interface_id        = iv_interface_id
              iv_service_id          = lv_interface_type
              iv_business_identifier = iv_business_identifier.
        WHEN gc_service_blob.
          CREATE OBJECT ro_service TYPE zcl_adf_service_blob
            EXPORTING
              iv_interface_id        = iv_interface_id
              iv_service_id          = lv_interface_type
              iv_business_identifier = iv_business_identifier.
        WHEN gc_service_docdb.
          CREATE OBJECT ro_service TYPE zcl_adf_service_documentdb
            EXPORTING
              iv_interface_id        = iv_interface_id
              iv_service_id          = lv_interface_type
              iv_business_identifier = iv_business_identifier.
        WHEN gc_service_servicebus.
          CREATE OBJECT ro_service TYPE zcl_adf_service_servicebus
            EXPORTING
              iv_interface_id        = iv_interface_id
              iv_service_id          = lv_interface_type
              iv_business_identifier = iv_business_identifier.
        WHEN gc_service_aad.
          CREATE OBJECT ro_service TYPE zcl_adf_service_aad
            EXPORTING
              iv_interface_id        = iv_interface_id
              iv_service_id          = lv_interface_type
              iv_business_identifier = iv_business_identifier.
        WHEN gc_service_keyvault.
          CREATE OBJECT ro_service TYPE zcl_adf_service_keyvault
            EXPORTING
              iv_interface_id        = iv_interface_id
              iv_service_id          = lv_interface_type
              iv_business_identifier = iv_business_identifier.
        WHEN gc_service_graph.
          CREATE OBJECT ro_service TYPE zcl_adf_service_graph
            EXPORTING
              iv_interface_id        = iv_interface_id
              iv_service_id          = lv_interface_type
              iv_business_identifier = iv_business_identifier.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_adf_service
            EXPORTING
              textid       = zcx_adf_service=>interface_type_not_maintained
              interface_id = iv_interface_id.
      ENDCASE.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>interface_not_available
          interface_id = iv_interface_id.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
