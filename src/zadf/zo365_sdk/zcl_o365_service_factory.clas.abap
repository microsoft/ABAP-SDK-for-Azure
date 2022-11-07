class ZCL_O365_SERVICE_FACTORY definition
  public
  final
  create public .

public section.

  constants GC_SERVICE_SHAREPOINT type ZAZURE_DEST value 'SHAREPOINT' ##NO_TEXT.

  class-methods CREATE
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
    returning
      value(RO_SERVICE) type ref to ZCL_O365_SERVICE
    raising
      ZCX_O365_SERVICE
      ZCX_INTERACE_CONFIG_MISSING
      ZCX_HTTP_CLIENT_FAILED
      ZCX_O365_SERVICE_SHAREPOINT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_O365_SERVICE_FACTORY IMPLEMENTATION.


METHOD create.

  DATA : lv_interface_type TYPE zo365_config-interface_type.

**********************************************************************
* Check for Interface in O365 Config table
**********************************************************************
  SELECT SINGLE interface_type FROM zo365_config
                               INTO lv_interface_type
                               WHERE interface_id EQ iv_interface_id.
  IF sy-subrc EQ 0.
**Factory class instantiation
    CASE lv_interface_type.
**Instantiate service class for Sharepoint
      WHEN gc_service_sharepoint.
        CREATE OBJECT ro_service TYPE zcl_o365_service_sharepoint
          EXPORTING
            iv_interface_id = iv_interface_id
            iv_service_id   = lv_interface_type.
**Exception handling
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_o365_service_sharepoint
          EXPORTING
            textid       = zcx_o365_service_sharepoint=>interface_type_not_maintained
            interface_id = iv_interface_id.
    ENDCASE.
  ELSE.
    RAISE EXCEPTION TYPE zcx_o365_service
      EXPORTING
        textid       = zcx_o365_service=>interface_not_available
        interface_id = iv_interface_id.
  ENDIF.
ENDMETHOD.
ENDCLASS.
