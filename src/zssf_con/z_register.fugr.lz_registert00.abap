*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 10/09/2017 at 02:31:43
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZREGISTER.......................................*
DATA:  BEGIN OF STATUS_ZREGISTER                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREGISTER                     .
CONTROLS: TCTRL_ZREGISTER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZREGISTER                     .
TABLES: ZREGISTER                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
