*&---------------------------------------------------------------------*
*& Report  ZREST_MONITOR_ARCHIVE
*&
*&---------------------------------------------------------------------*

************************************************************************
*      Everdeen Program - ARCHIVE ZREST_MONITOR table                  *
*----------------------------------------------------------------------*
* Program Title: ZREST_MONITOR_ARCHIVE                                 *
* Created by   : Bill Bradford                       Date: 03/08/2019  *
*                                                                      *
* Description  : The purpose of this report is to clear older entries  *
*                of table ZREST_MONITOR                                *
*----------------------------------------------------------------------*
*                       Modification History                           *
*----------------------------------------------------------------------*
* Date      | USER ID  |  VSTF  | Transport  | Remarks                 *
*-----------|----------|--------|------------|-------------------------*
*03/08/2019 | WIBRADFO | 1673104| SMTK905163 | Initial Version         *
*----------------------------------------------------------------------*

report zrest_monitor_archive line-size 220.

include zrest_monitor_archive_top.
include zrest_monitor_archive_sel.
include zrest_monitor_archive_f01.

start-of-selection.

  perform extract_data.
  perform archive_data.
  perform handle_orphans.





*                       ============================
*                          E N D   O F   R E P O R T
*                       ============================
