*&---------------------------------------------------------------------*
*&  Include           ZREST_SCHED_BATCH_SEL
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
*Feb' 2018  | V-MYAL   | 3019845| MP2K905030  | Initial development    *
*05/17/2018 | V-MYAL   | 3454161| MP2K905442 | More enhancements       *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_id FOR zrest_config-interface_id         MEMORY ID interface_id.
SELECT-OPTIONS: s_user     FOR zrest_monitor-zuser         MEMORY ID zuser    .
SELECT-OPTIONS: s_exedat   FOR zrest_monitor-zexedate      MEMORY ID zexedate  .
SELECT-OPTIONS: s_exetim   FOR zrest_monitor-zexetime      MEMORY ID zexetime  .
SELECT-OPTIONS: s_retdat   FOR zrest_monitor-zretrydate     .
SELECT-OPTIONS: s_rettim   FOR zrest_monitor-zretrytime     .
SELECT-OPTIONS: s_subdat   FOR zrest_monitor-submit_date    .
SELECT-OPTIONS: s_subtim   FOR zrest_monitor-submit_time    .
SELECT-OPTIONS: s_callpr   FOR zrest_monitor-calling_program  MEMORY ID calling_program.
SELECT-OPTIONS: s_callme   FOR zrest_monitor-calling_method   MEMORY ID calling_method .
SELECT-OPTIONS: s_busine   FOR zrest_monitor-businessid       MEMORY ID businessid     .
SELECT-OPTIONS: s_messag   FOR zrest_monitor-zmessageid.
PARAMETERS:     p_retain(03)   TYPE n DEFAULT 30.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_batch TYPE i DEFAULT 5 OBLIGATORY,            "MP2K905225
            p_jobs  TYPE i OBLIGATORY DEFAULT 3.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) text-005 MODIF ID m1.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_mjobs TYPE i OBLIGATORY DEFAULT 5.     "MP2K905442

PARAMETERS: p_bsname TYPE bpsrvgrp.   "batch server group name

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) text-006 MODIF ID m1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.


AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bsname.
  PERFORM f4_batch_server CHANGING p_bsname.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'M1'.
      screen-intensified = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
