*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 05/10/2017 at 02:12:25
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZADF_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZADF_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZADF_CONFIG                   .
CONTROLS: TCTRL_ZADF_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZADF_CONFIG                   .
TABLES: ZADF_CONFIG                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
