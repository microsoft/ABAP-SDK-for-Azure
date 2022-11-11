*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11/10/2022 at 09:16:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTVARVC.........................................*
DATA:  BEGIN OF STATUS_ZTVARVC                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTVARVC                       .
CONTROLS: TCTRL_ZTVARVC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTVARVC                       .
TABLES: ZTVARVC                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
