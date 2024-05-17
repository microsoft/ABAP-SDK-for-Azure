*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZADF_MI_CONFIG..................................*
DATA:  BEGIN OF STATUS_ZADF_MI_CONFIG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZADF_MI_CONFIG                .
CONTROLS: TCTRL_ZADF_MI_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZADF_MI_CONFIG                .
TABLES: ZADF_MI_CONFIG                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
