*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZO365_CONFIG....................................*
DATA:  BEGIN OF STATUS_ZO365_CONFIG                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZO365_CONFIG                  .
CONTROLS: TCTRL_ZO365_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZO365_CONFIG                  .
TABLES: ZO365_CONFIG                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
