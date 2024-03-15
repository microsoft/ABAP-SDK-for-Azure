*---------------------------------------------------------------------*
*    view related data declarations
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
