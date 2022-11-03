*&---------------------------------------------------------------------*
*&  Include           ZREST_MONITOR_ARCHIVE_SEL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
*03/08/2019 | WIBRADFO | 1673104| SMTK905163 | Initial Version         *
*----------------------------------------------------------------------*


select-options: s_mesgid for lw_monitor-zmessageid  .
select-options: s_exedat for lw_monitor-zexedate     memory id  zexedate     .
select-options: s_exetim for lw_monitor-zexetime     memory id  zexetime     .
select-options: s_interf for lw_monitor-interface_id memory id  interface_id .
select-options: s_busnid for lw_monitor-businessid   memory id  businessid .
select-options: s_zuser  for lw_monitor-zuser        memory id  zuser        .

parameter: p_mark radiobutton group acti,
           p_del  radiobutton group acti.
