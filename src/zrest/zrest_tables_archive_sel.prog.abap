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
*                                  MS2K985296
*&---------------------------------------------------------------------*

SELECT-OPTIONS: s_mesgid FOR lw_monitor-zmessageid  .
SELECT-OPTIONS: s_exedat FOR lw_monitor-zexedate   OBLIGATORY  MEMORY ID  zexedate     .
SELECT-OPTIONS: s_exetim FOR lw_monitor-zexetime     MEMORY ID  zexetime     .
SELECT-OPTIONS: s_interf FOR lw_monitor-interface_id MEMORY ID  interface_id .
SELECT-OPTIONS: s_busnid FOR lw_monitor-businessid   MEMORY ID  businessid .
SELECT-OPTIONS: s_zuser  FOR lw_monitor-zuser        MEMORY ID  zuser        .


PARAMETER: p_pack  TYPE i OBLIGATORY DEFAULT 10000,  "package
           p_test AS CHECKBOX DEFAULT abap_true.
