class ZCL_ADF_SHMA_AREA definition
  public
  inheriting from CL_SHM_AREA
  final
  create private

  global friends CL_SHM_AREA .

public section.

  constants AREA_NAME type SHM_AREA_NAME value 'ZCL_ADF_SHMA_AREA' ##NO_TEXT.
  data ROOT type ref to ZCL_ADF_SHMA_ROOT read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_GENERATOR_VERSION
    returning
      value(GENERATOR_VERSION) type I .
  class-methods ATTACH_FOR_READ
    importing
      !INST_NAME type SHM_INST_NAME default CL_SHM_AREA=>DEFAULT_INSTANCE
    preferred parameter INST_NAME
    returning
      value(HANDLE) type ref to ZCL_ADF_SHMA_AREA
    raising
      CX_SHM_INCONSISTENT
      CX_SHM_NO_ACTIVE_VERSION
      CX_SHM_READ_LOCK_ACTIVE
      CX_SHM_EXCLUSIVE_LOCK_ACTIVE
      CX_SHM_PARAMETER_ERROR
      CX_SHM_CHANGE_LOCK_ACTIVE .
  class-methods ATTACH_FOR_WRITE
    importing
      !INST_NAME type SHM_INST_NAME default CL_SHM_AREA=>DEFAULT_INSTANCE
      !ATTACH_MODE type SHM_ATTACH_MODE default CL_SHM_AREA=>ATTACH_MODE_DEFAULT
      !WAIT_TIME type I default 0
    preferred parameter INST_NAME
    returning
      value(HANDLE) type ref to ZCL_ADF_SHMA_AREA
    raising
      CX_SHM_EXCLUSIVE_LOCK_ACTIVE
      CX_SHM_VERSION_LIMIT_EXCEEDED
      CX_SHM_CHANGE_LOCK_ACTIVE
      CX_SHM_PARAMETER_ERROR
      CX_SHM_PENDING_LOCK_REMOVED .
  class-methods ATTACH_FOR_UPDATE
    importing
      !INST_NAME type SHM_INST_NAME default CL_SHM_AREA=>DEFAULT_INSTANCE
      !ATTACH_MODE type SHM_ATTACH_MODE default CL_SHM_AREA=>ATTACH_MODE_DEFAULT
      !WAIT_TIME type I default 0
    preferred parameter INST_NAME
    returning
      value(HANDLE) type ref to ZCL_ADF_SHMA_AREA
    raising
      CX_SHM_INCONSISTENT
      CX_SHM_NO_ACTIVE_VERSION
      CX_SHM_EXCLUSIVE_LOCK_ACTIVE
      CX_SHM_VERSION_LIMIT_EXCEEDED
      CX_SHM_CHANGE_LOCK_ACTIVE
      CX_SHM_PARAMETER_ERROR
      CX_SHM_PENDING_LOCK_REMOVED .
  class-methods DETACH_AREA
    returning
      value(RC) type SHM_RC .
  class-methods INVALIDATE_INSTANCE
    importing
      !INST_NAME type SHM_INST_NAME default CL_SHM_AREA=>DEFAULT_INSTANCE
      !TERMINATE_CHANGER type ABAP_BOOL default ABAP_TRUE
    preferred parameter INST_NAME
    returning
      value(RC) type SHM_RC
    raising
      CX_SHM_PARAMETER_ERROR .
  class-methods INVALIDATE_AREA
    importing
      !TERMINATE_CHANGER type ABAP_BOOL default ABAP_TRUE
    returning
      value(RC) type SHM_RC
    raising
      CX_SHM_PARAMETER_ERROR .
  class-methods FREE_INSTANCE
    importing
      !INST_NAME type SHM_INST_NAME default CL_SHM_AREA=>DEFAULT_INSTANCE
      !TERMINATE_CHANGER type ABAP_BOOL default ABAP_TRUE
    preferred parameter INST_NAME
    returning
      value(RC) type SHM_RC
    raising
      CX_SHM_PARAMETER_ERROR .
  class-methods FREE_AREA
    importing
      !TERMINATE_CHANGER type ABAP_BOOL default ABAP_TRUE
    returning
      value(RC) type SHM_RC
    raising
      CX_SHM_PARAMETER_ERROR .
  class-methods GET_INSTANCE_INFOS
    importing
      !INST_NAME type SHM_INST_NAME optional
    returning
      value(INFOS) type SHM_INST_INFOS .
  class-methods BUILD
    importing
      !INST_NAME type SHM_INST_NAME default CL_SHM_AREA=>DEFAULT_INSTANCE
    raising
      CX_SHMA_NOT_CONFIGURED
      CX_SHMA_INCONSISTENT
      CX_SHM_BUILD_FAILED .
  methods SET_ROOT
    importing
      !ROOT type ref to ZCL_ADF_SHMA_ROOT
    raising
      CX_SHM_INITIAL_REFERENCE
      CX_SHM_WRONG_HANDLE .

  methods GET_ROOT
    redefinition .
protected section.
private section.

  constants _VERSION_ type I value 22 ##NO_TEXT.
  class-data _TRACE_SERVICE type ref to IF_SHM_TRACE .
  class-data _TRACE_ACTIVE type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants _TRANSACTIONAL type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants _CLIENT_DEPENDENT type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants _LIFE_CONTEXT type SHM_LIFE_CONTEXT value CL_SHM_AREA=>LIFE_CONTEXT_APPSERVER ##NO_TEXT.
ENDCLASS.



CLASS ZCL_ADF_SHMA_AREA IMPLEMENTATION.


  method ATTACH_FOR_READ.

  DATA:
    l_attributes       TYPE shma_attributes,
    l_root             TYPE REF TO object,
    l_cx               TYPE REF TO cx_root,
    l_client           TYPE shm_client,
    l_client_supplied  TYPE abap_bool. "#EC NEEDED

* check if tracing should be activated/de-activated
  IF  ( NOT _trace_service IS INITIAL ).
    TRY.
        _trace_active =
          cl_shm_service=>trace_is_variant_active(
            _trace_service->variant-def_name
          ).
      CATCH cx_root. "#EC NO_HANDLER
                     "#EC CATCH_ALL
    ENDTRY.
  ENDIF.


  IF _trace_active = abap_false OR
  _trace_service->variant-attach_for_read = abap_false.

*   >

    CREATE OBJECT handle.

    handle->client    = l_client.
    handle->inst_name = inst_name.

*   try sneak mode first
    handle->_attach_read71( EXPORTING area_name    = area_name
                                      sneak_mode   = abap_true
                                      life_context = _life_context
                            IMPORTING root         = l_root ).

    IF l_root IS INITIAL.
*     no root object returned, sneak mode was not successful.
*     -> read area properties from database and try again.
      cl_shm_service=>initialize(
        EXPORTING area_name       = handle->area_name
                  client          = l_client
        IMPORTING attributes      = l_attributes
      ).

      handle->properties = l_attributes-properties.
      handle->_attach_read71( EXPORTING area_name    = area_name
                                        sneak_mode   = abap_false
                                        life_context = _life_context
                              IMPORTING root         = l_root ).

    ENDIF.

    handle->root ?= l_root.
*   <

  ELSE.

    TRY.

*       >

        CREATE OBJECT handle.

        handle->client    = l_client.
        handle->inst_name = inst_name.

        handle->_attach_read71( EXPORTING area_name    = area_name
                                          sneak_mode   = abap_true
                                          life_context = _life_context
                                IMPORTING root         = l_root ).

        IF l_root IS INITIAL.
*         no root object returned, sneak mode was not successful.
*         -> read area properties from database and try again.
          cl_shm_service=>initialize(
            EXPORTING area_name       = handle->area_name
                      client          = l_client
            IMPORTING attributes      = l_attributes
          ).

          handle->properties = l_attributes-properties.
          handle->_attach_read71( EXPORTING area_name    = area_name
                                            sneak_mode   = abap_false
                                            life_context = _life_context
                                  IMPORTING root         = l_root ).

        ENDIF.
        handle->root ?= l_root.

*       <
        _trace_service->trin_attach_for_read(
          area_name = area_name
          inst_name = inst_name
          client    = l_client ).

      CLEANUP INTO l_cx.
        _trace_service->trcx_attach_for_read(
          area_name = area_name
          inst_name = inst_name
          client    = l_client
          cx        = l_cx
        ).
    ENDTRY.

  ENDIF.

  handle->inst_trace_service = _trace_service.
  handle->inst_trace_active  = _trace_active.

  endmethod.


  method ATTACH_FOR_UPDATE.

  DATA:
    l_attributes             TYPE shma_attributes,
    l_root                   TYPE REF TO object,
    l_cx                     TYPE REF TO cx_root,
    l_client                 TYPE shm_client,
    l_client_supplied        TYPE abap_bool, "#EC NEEDED
    l_wait_time              TYPE i,
    l_wait_time_per_loop     TYPE i,
    l_wait_time_per_loop_sec TYPE f.

  l_wait_time = wait_time.

* check if tracing should be activated/de-activated
  IF  ( NOT _trace_service IS INITIAL ).
    TRY.
        _trace_active =
          cl_shm_service=>trace_is_variant_active(
            _trace_service->variant-def_name
          ).
      CATCH cx_root. "#EC NO_HANDLER
                     "#EC CATCH_ALL
    ENDTRY.
  ENDIF.


  IF _trace_active = abap_false OR
  _trace_service->variant-attach_for_upd = abap_false.

*   >

    CREATE OBJECT handle.

    handle->client    = l_client.
    handle->inst_name = inst_name.

    cl_shm_service=>initialize(
      EXPORTING area_name    = handle->area_name
                client       = l_client
      IMPORTING attributes   = l_attributes
    ).

    handle->properties = l_attributes-properties.

    handle->_attach_update70(
      EXPORTING area_name = handle->area_name
                mode      = attach_mode
      IMPORTING root      = l_root
      CHANGING  wait_time = l_wait_time ).

    IF abap_true = l_attributes-properties-has_versions AND
       handle->_lock IS NOT INITIAL.
* we may need a second try in case of class constructors
      handle->_attach_update70(
        EXPORTING area_name = handle->area_name
                  mode      = attach_mode
        IMPORTING root      = l_root
        CHANGING  wait_time = l_wait_time ).
    ENDIF.

    IF attach_mode = cl_shm_area=>attach_mode_wait AND
       handle->_lock IS INITIAL.

      l_wait_time_per_loop = l_wait_time / 10.
* wait_time_per_loop should be at least 2 * SHMATTACHWRITE_MAXACTIVEWAIT
      IF l_wait_time_per_loop < 2000.
        l_wait_time_per_loop = 2000.
      ELSEIF l_wait_time_per_loop > 300000.
        l_wait_time_per_loop = 300000.
      ENDIF.

      l_wait_time_per_loop_sec = l_wait_time_per_loop / 1000.

      WHILE handle->_lock IS INITIAL.

        IF l_wait_time_per_loop > l_wait_time.
          l_wait_time_per_loop = l_wait_time.
          l_wait_time_per_loop_sec = l_wait_time_per_loop / 1000.
        ENDIF.

        WAIT UP TO l_wait_time_per_loop_sec SECONDS.
        l_wait_time = l_wait_time - l_wait_time_per_loop.

        handle->_attach_update70(
          EXPORTING area_name = handle->area_name
                    mode      = cl_shm_area=>attach_mode_wait_2nd_try
          IMPORTING root      = l_root
          CHANGING  wait_time = l_wait_time ).

        IF abap_true = l_attributes-properties-has_versions AND
           handle->_lock IS NOT INITIAL.
* we may need a second try in case of class constructors
          handle->_attach_update70(
            EXPORTING area_name = handle->area_name
                      mode      = cl_shm_area=>attach_mode_wait_2nd_try
            IMPORTING root      = l_root
            CHANGING  wait_time = l_wait_time ).
        ENDIF.

      ENDWHILE.

    ENDIF.

    handle->root ?= l_root.

*   <

  ELSE.

    TRY.

*       >

        CREATE OBJECT handle.

        handle->client    = l_client.
        handle->inst_name = inst_name.

        cl_shm_service=>initialize(
          EXPORTING area_name    = handle->area_name
                    client       = l_client
          IMPORTING attributes   = l_attributes
        ).

        handle->properties = l_attributes-properties.

        handle->_attach_update70(
          EXPORTING area_name = handle->area_name
                    mode      = attach_mode
          IMPORTING root      = l_root
          CHANGING  wait_time = l_wait_time ).

        IF abap_true = l_attributes-properties-has_versions AND
           handle->_lock IS NOT INITIAL.
* we may need a second try in case of class constructors
          handle->_attach_update70(
            EXPORTING area_name = handle->area_name
                      mode      = attach_mode
            IMPORTING root      = l_root
            CHANGING  wait_time = l_wait_time ).
        ENDIF.

        IF attach_mode = cl_shm_area=>attach_mode_wait AND
           handle->_lock IS INITIAL.

          l_wait_time_per_loop = l_wait_time / 10.
* wait_time_per_loop should be at least 2 * SHMATTACHWRITE_MAXACTIVEWAIT
          IF l_wait_time_per_loop < 2000.
            l_wait_time_per_loop = 2000.
          ELSEIF l_wait_time_per_loop > 300000.
            l_wait_time_per_loop = 300000.
          ENDIF.

          l_wait_time_per_loop_sec = l_wait_time_per_loop / 1000.

          WHILE handle->_lock IS INITIAL.

            IF l_wait_time_per_loop > l_wait_time.
              l_wait_time_per_loop = l_wait_time.
              l_wait_time_per_loop_sec = l_wait_time_per_loop / 1000.
            ENDIF.

            WAIT UP TO l_wait_time_per_loop_sec SECONDS.
            l_wait_time = l_wait_time - l_wait_time_per_loop.

            handle->_attach_update70(
              EXPORTING
                area_name = handle->area_name
                mode      = cl_shm_area=>attach_mode_wait_2nd_try
              IMPORTING
                root      = l_root
              CHANGING
                wait_time = l_wait_time ).

            IF abap_true = l_attributes-properties-has_versions AND
               handle->_lock IS NOT INITIAL.
* we may need a second try in case of class constructors
              handle->_attach_update70(
                EXPORTING
                  area_name = handle->area_name
                  mode      = cl_shm_area=>attach_mode_wait_2nd_try
                IMPORTING
                  root      = l_root
                CHANGING
                  wait_time = l_wait_time ).
            ENDIF.

          ENDWHILE.

        ENDIF.

        handle->root ?= l_root.

*       <
        _trace_service->trin_attach_for_update(
          area_name = area_name
          inst_name = inst_name
          client    = l_client
          mode      = attach_mode
          wait_time = wait_time
        ).

      CLEANUP INTO l_cx.
        _trace_service->trcx_attach_for_update(
          area_name = area_name
          inst_name = inst_name
          client    = l_client
          mode      = attach_mode
          wait_time = wait_time
          cx        = l_cx
        ).
    ENDTRY.

  ENDIF.

  handle->inst_trace_service = _trace_service.
  handle->inst_trace_active  = _trace_active.

  endmethod.


  method ATTACH_FOR_WRITE.

  DATA:
    l_attributes             TYPE shma_attributes,
    l_cx                     TYPE REF TO cx_root,
    l_client                 TYPE shm_client,
    l_client_supplied        TYPE abap_bool, "#EC NEEDED
    l_wait_time              TYPE i,
    l_wait_time_per_loop     TYPE i,
    l_wait_time_per_loop_sec TYPE f.

  l_wait_time = wait_time.

* check if tracing should be activated/de-activated
  IF  ( NOT _trace_service IS INITIAL ).
    TRY.
        _trace_active =
          cl_shm_service=>trace_is_variant_active(
            _trace_service->variant-def_name
          ).
      CATCH cx_root. "#EC NO_HANDLER
                     "#EC CATCH_ALL
    ENDTRY.
  ENDIF.


  IF _trace_active = abap_false OR
  _trace_service->variant-attach_for_write = abap_false.

*   >

    CREATE OBJECT handle.

    handle->client    = l_client.
    handle->inst_name = inst_name.

    cl_shm_service=>initialize(
      EXPORTING area_name    = handle->area_name
                client       = l_client
      IMPORTING attributes   = l_attributes
    ).

    handle->properties = l_attributes-properties.

    handle->_attach_write70(
      EXPORTING
        area_name = handle->area_name
        mode      = attach_mode
      CHANGING
        wait_time = l_wait_time ).

    IF attach_mode = cl_shm_area=>attach_mode_wait AND
       handle->_lock IS INITIAL.

      l_wait_time_per_loop = l_wait_time / 10.
* wait_time_per_loop should be at least 2 * SHMATTACHWRITE_MAXACTIVEWAIT
      IF l_wait_time_per_loop < 2000.
        l_wait_time_per_loop = 2000.
      ELSEIF l_wait_time_per_loop > 300000.
        l_wait_time_per_loop = 300000.
      ENDIF.

      l_wait_time_per_loop_sec = l_wait_time_per_loop / 1000.

      WHILE handle->_lock IS INITIAL.

        IF l_wait_time_per_loop > l_wait_time.
          l_wait_time_per_loop = l_wait_time.
          l_wait_time_per_loop_sec = l_wait_time_per_loop / 1000.
        ENDIF.

        WAIT UP TO l_wait_time_per_loop_sec SECONDS.
        l_wait_time = l_wait_time - l_wait_time_per_loop.

        handle->_attach_write70(
          EXPORTING
            area_name = handle->area_name
            mode      = cl_shm_area=>attach_mode_wait_2nd_try
          CHANGING
            wait_time = l_wait_time ).

      ENDWHILE.

    ENDIF.

*   <

  ELSE.

    TRY.

*     >

        CREATE OBJECT handle.

        handle->client    = l_client.
        handle->inst_name = inst_name.

        cl_shm_service=>initialize(
          EXPORTING area_name    = handle->area_name
                    client       = l_client
          IMPORTING attributes   = l_attributes
        ).

        handle->properties = l_attributes-properties.

        handle->_attach_write70(
          EXPORTING
            area_name = handle->area_name
            mode      = attach_mode
          CHANGING
            wait_time = l_wait_time ).

        IF attach_mode = cl_shm_area=>attach_mode_wait AND
           handle->_lock IS INITIAL.

          l_wait_time_per_loop = l_wait_time / 10.
* wait_time_per_loop should be at least 2 * SHMATTACHWRITE_MAXACTIVEWAIT
          IF l_wait_time_per_loop < 2000.
            l_wait_time_per_loop = 2000.
          ELSEIF l_wait_time_per_loop > 300000.
            l_wait_time_per_loop = 300000.
          ENDIF.

          l_wait_time_per_loop_sec = l_wait_time_per_loop / 1000.

          WHILE handle->_lock IS INITIAL.

            IF l_wait_time_per_loop > l_wait_time.
              l_wait_time_per_loop = l_wait_time.
              l_wait_time_per_loop_sec = l_wait_time_per_loop / 1000.
            ENDIF.

            WAIT UP TO l_wait_time_per_loop_sec SECONDS.
            l_wait_time = l_wait_time - l_wait_time_per_loop.

            handle->_attach_write70(
              EXPORTING
                area_name = handle->area_name
                mode      = cl_shm_area=>attach_mode_wait_2nd_try
              CHANGING
                wait_time = l_wait_time ).

          ENDWHILE.

        ENDIF.

*     <

        _trace_service->trin_attach_for_write(
          area_name = area_name
          inst_name = inst_name
          client    = l_client
          mode      = attach_mode
          wait_time = wait_time
        ).
      CLEANUP INTO l_cx.
        _trace_service->trcx_attach_for_write(
          area_name = area_name
          inst_name = inst_name
          client    = l_client
          mode      = attach_mode
          wait_time = wait_time
          cx        = l_cx
        ).
    ENDTRY.

  ENDIF.

  handle->inst_trace_service = _trace_service.
  handle->inst_trace_active  = _trace_active.

  endmethod.


  method BUILD.

  DATA:
    l_cls_name TYPE shm_auto_build_class_name,
    l_cx TYPE REF TO cx_root.

  IF _trace_active = abap_false OR
  _trace_service->variant-build = abap_false.

*   >
    l_cls_name =
      cl_shm_service=>get_auto_build_class_name( area_name ).

    CALL METHOD (l_cls_name)=>if_shm_build_instance~build
      EXPORTING
        inst_name = inst_name.
*   <

  ELSE.

    TRY.

*       >
        l_cls_name =
          cl_shm_service=>get_auto_build_class_name( area_name ).

        CALL METHOD (l_cls_name)=>if_shm_build_instance~build
          EXPORTING
            inst_name = inst_name.
*       <
        _trace_service->trin_build(
          area_name         = area_name
          inst_name         = inst_name
        ).

      CLEANUP INTO l_cx.
        _trace_service->trcx_build(
          area_name         = area_name
          inst_name         = inst_name
          cx                = l_cx
        ).
    ENDTRY.

  ENDIF.

  endmethod.


  method CLASS_CONSTRUCTOR.

* TRACE { DO NOT REMOVE THIS LINE !
  _trace_active = abap_false.
  TRY.
      _trace_service =
        cl_shm_service=>trace_get_service( area_name ).
      IF NOT _trace_service IS INITIAL.
        _trace_active =
          cl_shm_service=>trace_is_variant_active(
            _trace_service->variant-def_name
          ).
      ENDIF.
    CATCH cx_root. "#EC NO_HANDLER
                   "#EC CATCH_ALL
  ENDTRY.
* TRACE } DO NOT REMOVE THIS LINE !

  endmethod.


  method DETACH_AREA.

  DATA:
    l_client TYPE shm_client,
    l_client_supplied TYPE abap_bool VALUE abap_false.


* >
  rc = _detach_area71( area_name        = area_name
                       client           = l_client
                       client_supplied  = l_client_supplied
                       client_dependent = _client_dependent
                       life_context     = _life_context
       ).
* <

  IF _trace_active = abap_true.
    IF _trace_service->variant-detach_area = abap_true.
      _trace_service->trin_detach_area(
        area_name = area_name
        client    = l_client
        rc        = rc
      ).
    ENDIF.
  ENDIF.

  endmethod.


  method FREE_AREA.

  DATA:
    l_client TYPE shm_client,
    l_client_supplied TYPE abap_bool VALUE abap_false.

  CONSTANTS: affect_server TYPE shm_affect_server
             VALUE cl_shm_area=>affect_local_server.


* >
  rc = _free_area71( area_name         = area_name
                     client            = l_client
                     client_supplied   = l_client_supplied
                     client_dependent  = _client_dependent
                     transactional     = _transactional
                     terminate_changer = terminate_changer
                     affect_server     = affect_server
                     life_context      = _life_context ).
* <

  IF _trace_active = abap_true.
    IF _trace_service->variant-free_area = abap_true.
      _trace_service->trin_free_area(
      area_name         = area_name
      client            = l_client
      terminate_changer = terminate_changer
      affect_server     = affect_server
      rc                = rc
    ).
    ENDIF.
  ENDIF.

  endmethod.


  method FREE_INSTANCE.

  DATA:
    l_client TYPE shm_client,
    l_client_supplied TYPE abap_bool VALUE abap_false.

  CONSTANTS: affect_server TYPE shm_affect_server
             VALUE cl_shm_area=>affect_local_server.


* >
  rc = _free_instance71( area_name         = area_name
                         inst_name         = inst_name
                         client            = l_client
                         client_supplied   = l_client_supplied
                         client_dependent  = _client_dependent
                         transactional     = _transactional
                         terminate_changer = terminate_changer
                         affect_server     = affect_server
                         life_context      = _life_context ).
* <

  IF _trace_active = abap_true.
    IF _trace_service->variant-free_instance = abap_true.
      _trace_service->trin_free_instance(
        area_name         = area_name
        inst_name         = inst_name
        client            = l_client
        terminate_changer = terminate_changer
        affect_server     = affect_server
        rc                = rc
      ).
    ENDIF.
  ENDIF.

  endmethod.


  method GET_GENERATOR_VERSION.
  generator_version = _version_.
  endmethod.


  method GET_INSTANCE_INFOS.

  DATA:
    l_client             TYPE shm_client,
    l_client_supplied    TYPE abap_bool VALUE abap_false,
    l_inst_name_supplied TYPE abap_bool VALUE abap_false.


  IF inst_name IS SUPPLIED.
    l_inst_name_supplied = abap_true.
  ENDIF.

* >
  TRY.
      CALL METHOD ('_GET_INSTANCE_INFOS804')
        EXPORTING
          area_name          = area_name
          client             = l_client
          client_supplied    = l_client_supplied
          client_dependent   = _client_dependent
          life_context       = _life_context
          inst_name          = inst_name
          inst_name_supplied = l_inst_name_supplied
        RECEIVING
          infos              = infos.
    CATCH cx_sy_dyn_call_illegal_method.
*     New kernel and/or new basis SP missing -> use slow fallback
      infos = _get_instance_infos71(
                area_name        = area_name
                client           = l_client
                client_supplied  = l_client_supplied
                client_dependent = _client_dependent
                life_context     = _life_context
              ).
      IF abap_true = l_inst_name_supplied.
        DELETE infos WHERE name <> inst_name.
      ENDIF.
  ENDTRY.
* <

  IF _trace_active = abap_true.
    IF _trace_service->variant-get_instance_inf = abap_true.
      _trace_service->trin_get_instance_infos(
        area_name         = area_name
        client            = l_client
        infos             = infos
      ).
    ENDIF.
  ENDIF.

  endmethod.


  method GET_ROOT.

  DATA:
    l_cx        TYPE REF TO cx_root,
    l_area_name TYPE string,
    l_inst_name TYPE string,
    l_client    TYPE string.

  IF _trace_active = abap_false OR
  _trace_service->variant-get_root = abap_false.

*   >
    IF is_valid( ) = abap_false.
      l_area_name = me->area_name.
      l_inst_name = me->inst_name.
      l_client    = me->client.
      RAISE EXCEPTION TYPE cx_shm_already_detached
        EXPORTING
          area_name = l_area_name
          inst_name = l_inst_name
          client    = l_client.
    ENDIF.
    root = me->root.
*   <

  ELSE.

    TRY.

*       >
        IF is_valid( ) = abap_false.
          l_area_name = me->area_name.
          l_inst_name = me->inst_name.
          l_client    = me->client.
          RAISE EXCEPTION TYPE cx_shm_already_detached
            EXPORTING
              area_name = l_area_name
              inst_name = l_inst_name
              client    = l_client.
        ENDIF.
        root = me->root.
*       <

        _trace_service->trin_get_root(
          area_name = area_name
        ).

      CLEANUP INTO l_cx.
        _trace_service->trcx_get_root(
          area_name = area_name
          cx        = l_cx
        ).
    ENDTRY.

  ENDIF.

  endmethod.


  method INVALIDATE_AREA.

  DATA:
    l_client TYPE shm_client,
    l_client_supplied TYPE abap_bool VALUE abap_false.

  CONSTANTS: affect_server TYPE shm_affect_server
             VALUE cl_shm_area=>affect_local_server.


* >
  rc = _invalidate_area71( area_name         = area_name
                           client            = l_client
                           client_supplied   = l_client_supplied
                           client_dependent  = _client_dependent
                           transactional     = _transactional
                           terminate_changer = terminate_changer
                           affect_server     = affect_server
                           life_context      = _life_context ).
* <

  IF _trace_active = abap_true.
    IF _trace_service->variant-invalidate_area = abap_true.
      _trace_service->trin_invalidate_area(
        area_name         = area_name
        client            = l_client
        terminate_changer = terminate_changer
        affect_server     = affect_server
        rc                = rc
      ).
    ENDIF.
  ENDIF.

  endmethod.


  method INVALIDATE_INSTANCE.

  DATA:
    l_client TYPE shm_client,
    l_client_supplied TYPE abap_bool value abap_false.

  CONSTANTS: affect_server TYPE shm_affect_server
             VALUE cl_shm_area=>affect_local_server.


* >
  rc = _invalidate_instance71(
    area_name         = area_name
    inst_name         = inst_name
    client            = l_client
    client_supplied   = l_client_supplied
    client_dependent  = _client_dependent
    transactional     = _transactional
    terminate_changer = terminate_changer
    affect_server     = affect_server
    life_context      = _life_context
  ).
* <

  IF _trace_active = abap_true.
    IF _trace_service->variant-invalidate_inst = abap_true.
      _trace_service->trin_invalidate_instance(
        area_name         = area_name
        inst_name         = inst_name
        client            = l_client
        terminate_changer = terminate_changer
        affect_server     = affect_server
        rc                = rc
      ).
    ENDIF.
  ENDIF.

  endmethod.


  method SET_ROOT.

  DATA:
    l_cx TYPE REF TO cx_root.

  IF _trace_active = abap_false OR
  _trace_service->variant-set_root = abap_false.

*   >
    _set_root( root ).
    me->root = root.
*   <

  ELSE.

    TRY.

*       >
        _set_root( root ).
        me->root = root.
*       <
        _trace_service->trin_set_root(
          area_name         = area_name
          inst_name         = inst_name
          root              = root
        ).

      CLEANUP INTO l_cx.
        _trace_service->trcx_set_root(
          area_name         = area_name
          inst_name         = inst_name
          root              = root
          cx                = l_cx
        ).
    ENDTRY.

  ENDIF.

  endmethod.
ENDCLASS.
