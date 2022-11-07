*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 10/28/2022 at 01:36:17
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZGROUP_VALUES...................................*
DATA:  BEGIN OF STATUS_ZGROUP_VALUES                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGROUP_VALUES                 .
CONTROLS: TCTRL_ZGROUP_VALUES
            TYPE TABLEVIEW USING SCREEN '9101'.
*.........table declarations:.................................*
TABLES: *ZGROUP_VALUES                 .
TABLES: ZGROUP_VALUES                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
