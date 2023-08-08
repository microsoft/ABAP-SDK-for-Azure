class ZCL_ADF_SERVICE_FACTORY definition
  public
  final
  create public .

public section.

  constants GC_SERVICE_EVENTHUB type ZAZURE_DEST value 'EVENTHUB' ##NO_TEXT.
  constants GC_SERVICE_BLOB type ZAZURE_DEST value 'BLOB' ##NO_TEXT.
  constants GC_SERVICE_SERVICEBUS type ZAZURE_DEST value 'SERVICEBUS' ##NO_TEXT.
  constants GC_SERVICE_EVENTGRID type ZAZURE_DEST value 'EVENTGRID' ##NO_TEXT.
  constants GC_SERVICE_AAD type ZAZURE_DEST value 'AAD' ##NO_TEXT.
  constants GC_SERVICE_KEYVAULT type ZAZURE_DEST value 'KV' ##NO_TEXT.
  constants GC_SERVICE_OMS_LA type ZAZURE_DEST value 'OMS_LA' ##NO_TEXT.
  constants GC_SERVICE_COSMOSDB type ZAZURE_DEST value 'COSMOSDB' ##NO_TEXT.
  constants GC_SERVICE_APPINSIGHTS type ZAZURE_DEST value 'APPINS' ##NO_TEXT.
  constants GC_SERVICE_FILES type ZAZURE_DEST value 'FILE' ##NO_TEXT.
  constants GC_SERVICE_MSI type ZAZURE_DEST value 'MSI' ##NO_TEXT.
  constants GC_GRAPH type ZAZURE_DEST value 'GRAPH' ##NO_TEXT.
  constants GC_SERVICE_AZOPENAI type ZAZURE_DEST value 'AZOPENAI' ##NO_TEXT.
  constants GC_SERVICE_AZTABLE type ZAZURE_DEST value 'AZTABLE' ##NO_TEXT.

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
      WHEN gc_service_cosmosdb.
        CREATE OBJECT ro_service TYPE zcl_adf_service_cosmosdb
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
* Insert Begin of VBANSAL- 06/12/2022 - Event Grid
      WHEN gc_service_eventgrid.
        CREATE OBJECT ro_service TYPE zcl_adf_service_eventgrid
          EXPORTING
            iv_interface_id        = iv_interface_id
            iv_service_id          = lv_interface_type
            iv_business_identifier = iv_business_identifier.
* Insert End of VBANSAL- 06/12/2022  - Event Grid

* Insert Begin of VBANSAL- 02/05/2023 - Azure Open AI
      WHEN gc_service_azopenai.
        CREATE OBJECT ro_service TYPE zcl_adf_service_azureopenai
          EXPORTING
            iv_interface_id        = iv_interface_id
            iv_service_id          = lv_interface_type
            iv_business_identifier = iv_business_identifier.
* Insert End of VBANSAL- 06/12/2022  - Azure Open AI

* Insert Begin of VBANSAL- 02/05/2023 - Azure Open AI
      WHEN gc_service_aztable.
        CREATE OBJECT ro_service TYPE zcl_adf_service_aztable
          EXPORTING
            iv_interface_id        = iv_interface_id
            iv_service_id          = lv_interface_type
            iv_business_identifier = iv_business_identifier.
* Insert End of VBANSAL- 06/12/2022  - Azure Open AI

      WHEN gc_service_keyvault.
        CREATE OBJECT ro_service TYPE zcl_adf_service_keyvault
          EXPORTING
            iv_interface_id        = iv_interface_id
            iv_service_id          = lv_interface_type
            iv_business_identifier = iv_business_identifier.
      WHEN gc_service_oms_la.
        CREATE OBJECT ro_service TYPE zcl_adf_service_oms_la
          EXPORTING
            iv_interface_id        = iv_interface_id
            iv_service_id          = lv_interface_type
            iv_business_identifier = iv_business_identifier.
      WHEN gc_service_appinsights.
        CREATE OBJECT ro_service TYPE zcl_adf_service_appinsights
          EXPORTING
            iv_interface_id        = iv_interface_id
            iv_service_id          = lv_interface_type
            iv_business_identifier = iv_business_identifier.
      WHEN gc_service_files.
        CREATE OBJECT ro_service TYPE zcl_adf_service_files
          EXPORTING
            iv_interface_id        = iv_interface_id
            iv_service_id          = lv_interface_type
            iv_business_identifier = iv_business_identifier.
      WHEN gc_service_aad OR gc_service_msi.                  " Adding logic to enable MSI
        CREATE OBJECT ro_service TYPE zcl_adf_service_aad
          EXPORTING
            iv_interface_id        = iv_interface_id
            iv_service_id          = lv_interface_type
            iv_business_identifier = iv_business_identifier.
      WHEN gc_graph.
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
