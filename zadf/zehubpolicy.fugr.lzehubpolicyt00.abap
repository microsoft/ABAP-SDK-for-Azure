*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11/28/2017 at 21:02:16
*   view maintenance generator version: #001407#
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
