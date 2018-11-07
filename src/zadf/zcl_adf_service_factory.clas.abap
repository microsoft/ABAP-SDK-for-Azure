class ZCL_ADF_SERVICE_FACTORY definition
  public
  final
  create public .

public section.

  constants GC_SERVICE_EVENTHUB type ZAZURE_DEST value 'EVENTHUB' ##NO_TEXT.
  constants GC_SERVICE_BLOB type ZAZURE_DEST value 'BLOB' ##NO_TEXT.
  constants GC_SERVICE_DOCDB type ZAZURE_DEST value 'DOCUMENTDB' ##NO_TEXT.
  constants GC_SERVICE_SERVICEBUS type ZAZURE_DEST value 'SERVICEBUS' ##NO_TEXT.
  constants GC_SERVICE_AAD type ZAZURE_DEST value 'AAD' ##NO_TEXT.
  constants GC_SERVICE_KEYVAULT type ZAZURE_DEST value 'KV' ##NO_TEXT.
  constants GC_SERVICE_GRAPH type ZAZURE_DEST value 'GRAPH' ##NO_TEXT.

  class-methods CREATE
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
      value(IV_BUSINESS_IDENTIFIER) type ZBUSINESSID optional
    returning
      value(RO_SERVICE) type ref to ZCL_ADF_SERVICE
    raising
      ZCX_ADF_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ADF_SERVICE_FACTORY IMPLEMENTATION.


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
      WHEN gc_service_graph .
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
