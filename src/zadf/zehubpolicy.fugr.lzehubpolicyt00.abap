*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZADF_EHUB_POLICY................................*
DATA:  BEGIN OF STATUS_ZADF_EHUB_POLICY              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZADF_EHUB_POLICY              .
CONTROLS: TCTRL_ZADF_EHUB_POLICY
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZADF_EHUB_POLICY              .
TABLES: ZADF_EHUB_POLICY               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
