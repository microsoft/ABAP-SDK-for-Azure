*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01/16/2023 at 21:47:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZADF_V_CONFIG...................................*
TABLES: ZADF_V_CONFIG, *ZADF_V_CONFIG. "view work areas
CONTROLS: TCTRL_ZADF_V_CONFIG
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZADF_V_CONFIG. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZADF_V_CONFIG.
* Table for entries selected to show on screen
DATA: BEGIN OF ZADF_V_CONFIG_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZADF_V_CONFIG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZADF_V_CONFIG_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZADF_V_CONFIG_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZADF_V_CONFIG.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZADF_V_CONFIG_TOTAL.

*...processing: ZADF_V_RESTDEST.................................*
TABLES: ZADF_V_RESTDEST, *ZADF_V_RESTDEST. "view work areas
CONTROLS: TCTRL_ZADF_V_RESTDEST
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZADF_V_RESTDEST. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZADF_V_RESTDEST.
* Table for entries selected to show on screen
DATA: BEGIN OF ZADF_V_RESTDEST_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZADF_V_RESTDEST.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZADF_V_RESTDEST_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZADF_V_RESTDEST_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZADF_V_RESTDEST.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZADF_V_RESTDEST_TOTAL.

*.........table declarations:.................................*
TABLES: ZADF_CONFIG                    .
TABLES: ZREST_CONFIG                   .
