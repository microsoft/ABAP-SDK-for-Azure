*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGROUP_VALUES...................................*
DATA:  BEGIN OF STATUS_ZGROUP_VALUES                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGROUP_VALUES                 .
CONTROLS: TCTRL_ZGROUP_VALUES
            TYPE TABLEVIEW USING SCREEN '9101'.
*.........table declarations:.................................*
TABLES: *ZGROUP_VALUES                 .
TABLES: ZGROUP_VALUES                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
