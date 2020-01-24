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

    m_impl_cache = VALUE #( ( service_id = zif_adf_azure_defconfig~gc_service_aad         classname = 'ZCL_ADF_SERVICE_AAD')
                            ( service_id = zif_adf_azure_defconfig~gc_service_blob       classname = 'ZCL_ADF_SERVICE_BLOB')
                            ( service_id = zif_adf_azure_defconfig~gc_service_docdb      classname = 'ZCL_ADF_SERVICE_DOCUMENTDB')
                            ( service_id = zif_adf_azure_defconfig~gc_service_eventhub   classname = 'ZCL_ADF_SERVICE_EVENTHUB')
                            ( service_id = zif_adf_azure_defconfig~gc_service_keyvault   classname = 'ZCL_ADF_SERVICE_KEYVAULT')
                            ( service_id = zif_adf_azure_defconfig~gc_service_servicebus classname = 'ZCL_ADF_SERVICE_SERVICEBUS')
                           ).

  ENDMETHOD.


  METHOD zif_adf_azure_defconfig~get_classname.

    TRY.
        r_classname = m_impl_cache[ service_id = i_interface_type ]-classname.

      CATCH cx_sy_itab_line_not_found INTO DATA(cx).
        RAISE EXCEPTION TYPE zcx_adf_service
          EXPORTING
            textid = zcx_adf_service=>interface_type_not_maintained.
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
