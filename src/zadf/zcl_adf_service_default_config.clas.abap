CLASS zcl_adf_service_default_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_adf_azure_defconfig.
    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF sty_impl_cache,
             service_id TYPE zazure_dest,
             classname  TYPE seoclsname,
           END OF sty_impl_cache.
    TYPES tty_impl_cache TYPE STANDARD TABLE OF sty_impl_cache.

    DATA m_impl_cache TYPE tty_impl_cache.
ENDCLASS.



CLASS zcl_adf_service_default_config IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS : <fs> like LINE OF m_impl_cache.
    APPEND INITIAL LINE TO m_impl_cache ASSIGNING <fs>.
    <fs>-service_id = zif_adf_azure_defconfig~gc_service_aad.
    <fs>-classname = 'ZCL_ADF_SERVICE_AAD'.

    APPEND INITIAL LINE TO m_impl_cache ASSIGNING <fs>.
    <fs>-service_id = zif_adf_azure_defconfig~gc_service_blob .
    <fs>-classname = 'ZCL_ADF_SERVICE_BLOB'.

    APPEND INITIAL LINE TO m_impl_cache ASSIGNING <fs>.
    <fs>-service_id = zif_adf_azure_defconfig~gc_service_docdb.
    <fs>-classname = 'ZCL_ADF_SERVICE_DOCUMENTDB'.

    APPEND INITIAL LINE TO m_impl_cache ASSIGNING <fs>.
    <fs>-service_id = zif_adf_azure_defconfig~gc_service_eventhub.
    <fs>-classname = 'ZCL_ADF_SERVICE_EVENTHUB'.

    APPEND INITIAL LINE TO m_impl_cache ASSIGNING <fs>.
    <fs>-service_id = zif_adf_azure_defconfig~gc_service_keyvault.
    <fs>-classname = 'ZCL_ADF_SERVICE_KEYVAULT'.

    APPEND INITIAL LINE TO m_impl_cache ASSIGNING <fs>.
    <fs>-service_id = zif_adf_azure_defconfig~gc_service_servicebus.
    <fs>-classname = 'ZCL_ADF_SERVICE_SERVICEBUS'.

    APPEND INITIAL LINE TO m_impl_cache ASSIGNING <fs>.
    <fs>-service_id = zif_adf_azure_defconfig~gc_service_cosmosdb.
    <fs>-classname = 'ZCL_ADF_SERVICE_COSMOSDB'.

  ENDMETHOD.


  METHOD zif_adf_azure_defconfig~get_classname.

  DATA : lw_impl TYPE sty_impl_cache.

  CLEAR lw_impl.
  READ TABLE m_impl_cache INTO lw_impl WITH KEY service_id = i_interface_type.
  IF SY-SUBRC IS INITIAL.
    r_classname = lw_impl-classname.
  ELSE.
    RAISE EXCEPTION TYPE zcx_adf_service
    EXPORTING
      textid = zcx_adf_service=>interface_type_not_maintained.
  ENDIF.


  ENDMETHOD.
ENDCLASS.
