CLASS zcl_adf_service_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    CLASS-METHODS inject_configuration
      IMPORTING i_ext_config TYPE REF TO zif_adf_azure_defconfig.

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
    CLASS-DATA m_service_config TYPE REF TO zif_adf_azure_defconfig.
ENDCLASS.



CLASS zcl_adf_service_factory IMPLEMENTATION.


  METHOD class_constructor.
    m_service_config = NEW zcl_adf_service_default_config( ).
  ENDMETHOD.

  METHOD create.
    DATA : interface_type TYPE zadf_config-interface_type.

    SELECT SINGLE interface_type FROM zadf_config
                                 INTO @interface_type
                                 WHERE interface_id = @iv_interface_id.
    IF sy-subrc = 0.

      DATA(classname) = m_service_config->get_classname( i_interface_type = interface_type ).

      CREATE OBJECT ro_service TYPE (classname)
      EXPORTING
            iv_interface_id        = iv_interface_id
            iv_service_id          = interface_type
            iv_business_identifier = iv_business_identifier.


    ELSE.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>interface_not_available
          interface_id = iv_interface_id.
    ENDIF.
  ENDMETHOD.

  METHOD inject_configuration.
    m_service_config = i_ext_config.
  ENDMETHOD.

ENDCLASS.
