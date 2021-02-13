
SELECT-OPTIONS: s_mesgid FOR lw_monitor-zmessageid  .
SELECT-OPTIONS: s_exedat FOR lw_monitor-zexedate   OBLIGATORY  MEMORY ID  zexedate     .
SELECT-OPTIONS: s_exetim FOR lw_monitor-zexetime     MEMORY ID  zexetime     .
SELECT-OPTIONS: s_interf FOR lw_monitor-interface_id MEMORY ID  interface_id .
SELECT-OPTIONS: s_busnid FOR lw_monitor-businessid   MEMORY ID  businessid .
SELECT-OPTIONS: s_zuser  FOR lw_monitor-zuser        MEMORY ID  zuser        .


PARAMETERS: p_pack  TYPE i OBLIGATORY DEFAULT 10000,  "package
            p_test AS CHECKBOX DEFAULT abap_true.
