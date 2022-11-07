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
*04/22/2021 | V-ARUNKH | 7286051 | SMTK906666 |Performance Optimization*
*&---------------------------------------------------------------------*
REPORT zrest_tables_archive.

INCLUDE zrest_tables_archive_top.
INCLUDE zrest_tables_archive_sel.
INCLUDE zrest_tables_archive_f01.

START-OF-SELECTION.
  PERFORM extract_data.
  PERFORM disp_output.
