*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZADF_SDK_RETRY..................................*
DATA:  BEGIN OF STATUS_ZADF_SDK_RETRY                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZADF_SDK_RETRY                .
CONTROLS: TCTRL_ZADF_SDK_RETRY
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZADF_SDK_RETRY                .
TABLES: ZADF_SDK_RETRY                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
