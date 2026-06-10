*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSSF_DATA.......................................*
DATA:  BEGIN OF STATUS_ZSSF_DATA                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSSF_DATA                     .
CONTROLS: TCTRL_ZSSF_DATA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSSF_DATA                     .
TABLES: ZSSF_DATA                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
