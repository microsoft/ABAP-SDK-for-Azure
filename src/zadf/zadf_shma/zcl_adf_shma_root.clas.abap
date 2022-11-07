class ZCL_ADF_SHMA_ROOT definition
  public
  final
  create public
  shared memory enabled .

public section.

  interfaces IF_SHM_BUILD_INSTANCE .

  methods GET_SAS_KEY
    importing
      value(IV_INTERFACE_ID) type ZINTERFACE_ID
    returning
      value(EV_SAS_KEY) type ZSAS_AZURE
    raising
      ZCX_ADF_SERVICE .
  methods SET_SAS_KEY
    importing
      value(IS_ADF_CONFIG) type ZADF_CONFIG .
protected section.
private section.

  types:
    tt_zadf_config TYPE TABLE OF zadf_config .

  data GT_ADF_CONFIG type TT_ZADF_CONFIG .

  methods CALL_STACK_CHECK
    raising
      ZCX_ADF_SERVICE .
ENDCLASS.



CLASS ZCL_ADF_SHMA_ROOT IMPLEMENTATION.


  METHOD call_stack_check.

    DATA : lt_abap_stack TYPE abap_callstack,
           lt_syst_stack TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack    = lt_abap_stack
        et_callstack = lt_syst_stack.
    READ TABLE lt_abap_stack TRANSPORTING NO FIELDS
               WITH KEY mainprogram = 'ZCL_ADF_SERVICE===============CP'
                        blocktype = 'METHOD'
                        blockname = 'READ_KEY'.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_adf_service
        EXPORTING
          textid       = zcx_adf_service=>execution_terminated
          interface_id = space.
    ENDIF.

  ENDMETHOD.


  METHOD get_sas_key.

    DATA: ls_adf_config TYPE zadf_config.
    call_stack_check( ).
    READ TABLE gt_adf_config INTO ls_adf_config WITH KEY
    interface_id = iv_interface_id.
    IF sy-subrc IS NOT INITIAL.
      ev_sas_key = ls_adf_config-sas_key.
    ENDIF.
  ENDMETHOD.


  METHOD if_shm_build_instance~build.

*    DATA: area  TYPE REF TO zcl_adf_shma_area,
*          root  TYPE REF TO zcl_adf_shma_root,
*          excep TYPE REF TO cx_root.
*
*    DATA: ls_zadf_config TYPE zadf_config,
*          lt_fieldcat    TYPE lvc_t_fcat.
*
*    TRY.
*        area = zcl_adf_shma_area=>attach_for_write( ).
*
*      CATCH cx_shm_error INTO excep.
*        RAISE EXCEPTION TYPE cx_shm_build_failed
*          EXPORTING
*            previous = excep.
*    ENDTRY.
*
*    CREATE OBJECT root AREA HANDLE area.
*
*    CALL METHOD root->set_sas_key
*      EXPORTING
*        is_adf_config = ls_zadf_config.
*
*    area->set_root( root ).
*
*    area->detach_commit( ).

  ENDMETHOD.


  METHOD set_sas_key.

    FIELD-SYMBOLS : <ls_adf_config> TYPE zadf_config.

    READ TABLE gt_adf_config ASSIGNING <ls_adf_config>
         WITH KEY interface_id = is_adf_config-interface_id.
    IF sy-subrc = 0 AND <ls_adf_config> IS ASSIGNED.
      <ls_adf_config>-sas_key = is_adf_config-sas_key.
    ELSE.
      APPEND is_adf_config TO gt_adf_config.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
