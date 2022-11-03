************************************************************************
* Program Title: ZREST_TABLES_ARCHIVE                                  *
* Created by   : Krishan Ankam                     Date: 06/04/2019    *
*                                                                      *
* Description  : Archive ZREST* tables                                 *
* Module name  : ZREST tables are deleted for theselection screen date *
*&---------------------------------------------------------------------*
*                       Modification History                           *
*&---------------------------------------------------------------------*
* Date      | USER ID  |  VSTF   | Transport  | Remarks                *
*&---------------------------------------------------------------------*
*06/04/2019 | KRANKAM  | 4653851 | MS2K985166 | Initial Version        *
*&---------------------------------------------------------------------*
TABLES:  zrest_monitor."KRANKAM 10/23/2019
TYPES: BEGIN OF gty_zrest_monitor, "(+)v-pralav SMTK906666
         mandt type mandt,
         zmessageid TYPE zmid,
       END OF gty_zrest_monitor.

DATA gv_ext_cnt   TYPE i.
DATA gv_del_cnt_zrest_monitor    TYPE i.
DATA gv_del_cnt_zrest_mon_header   TYPE i.
DATA gv_del_cnt_zrest_mon_trace   TYPE i.
DATA gv_del_cnt_zrest_monitorlog   TYPE i.
DATA gv_del_cnt_zrest_mo_payload   TYPE i.
DATA gv_del_cnt_zrest_retries   TYPE i.

*DATA: gt_zrest_monitor TYPE STANDARD TABLE OF zrest_monitor. "(-)v-pralav SMTK906666
DATA: gt_zrest_monitor TYPE TABLE OF gty_zrest_monitor.       "(+)v-pralav SMTK906666

DATA: lw_monitor     TYPE zrest_monitor.
