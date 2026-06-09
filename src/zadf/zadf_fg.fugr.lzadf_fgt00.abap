*---------------------------------------------------------------------*
*    view related data declarations
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
