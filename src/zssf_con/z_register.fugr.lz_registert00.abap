*---------------------------------------------------------------------*
*    view related data declarations
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
