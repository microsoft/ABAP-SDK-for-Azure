*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 08/29/2018 at 03:15:49
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZOBFUSCATE......................................*
DATA:  BEGIN OF STATUS_ZOBFUSCATE                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZOBFUSCATE                    .
CONTROLS: TCTRL_ZOBFUSCATE
            TYPE TABLEVIEW USING SCREEN '9050'.
*...processing: ZREST_CONFIG....................................*
DATA:  BEGIN OF STATUS_ZREST_CONFIG                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREST_CONFIG                  .
CONTROLS: TCTRL_ZREST_CONFIG
            TYPE TABLEVIEW USING SCREEN '9010'.
*...processing: ZREST_CONF_HEAD.................................*
DATA:  BEGIN OF STATUS_ZREST_CONF_HEAD               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREST_CONF_HEAD               .
CONTROLS: TCTRL_ZREST_CONF_HEAD
            TYPE TABLEVIEW USING SCREEN '9004'.
*...processing: ZREST_CONF_MISC.................................*
DATA:  BEGIN OF STATUS_ZREST_CONF_MISC               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREST_CONF_MISC               .
CONTROLS: TCTRL_ZREST_CONF_MISC
            TYPE TABLEVIEW USING SCREEN '9002'.
*...processing: ZREST_GLOBAL....................................*
DATA:  BEGIN OF STATUS_ZREST_GLOBAL                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREST_GLOBAL                  .
CONTROLS: TCTRL_ZREST_GLOBAL
            TYPE TABLEVIEW USING SCREEN '9089'.
*...processing: ZREST_MONITOR...................................*
DATA:  BEGIN OF STATUS_ZREST_MONITOR                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREST_MONITOR                 .
CONTROLS: TCTRL_ZREST_MONITOR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZOBFUSCATE                    .
TABLES: *ZREST_CONFIG                  .
TABLES: *ZREST_CONF_HEAD               .
TABLES: *ZREST_CONF_MISC               .
TABLES: *ZREST_GLOBAL                  .
TABLES: *ZREST_MONITOR                 .
TABLES: ZOBFUSCATE                     .
TABLES: ZREST_CONFIG                   .
TABLES: ZREST_CONF_HEAD                .
TABLES: ZREST_CONF_MISC                .
TABLES: ZREST_GLOBAL                   .
TABLES: ZREST_MONITOR                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
