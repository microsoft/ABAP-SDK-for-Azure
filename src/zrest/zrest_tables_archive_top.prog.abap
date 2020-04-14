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

DATA gv_ext_cnt   TYPE i.
DATA gv_del_cnt_zrest_monitor    TYPE i.
DATA gv_del_cnt_zrest_mon_header   TYPE i.
DATA gv_del_cnt_zrest_mon_trace   TYPE i.
DATA gv_del_cnt_zrest_monitorlog   TYPE i.
DATA gv_del_cnt_zrest_mo_payload   TYPE i.
DATA gv_del_cnt_zrest_retries   TYPE i.

DATA: gt_zrest_monitor TYPE STANDARD TABLE OF zrest_monitor.

DATA: lw_monitor     TYPE zrest_monitor.
