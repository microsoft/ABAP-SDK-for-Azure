*---------------------------------------------------------------------*
*    program for:   VIEWFRAME_ZADF_V_SDK_RETRY
*---------------------------------------------------------------------*
FUNCTION VIEWFRAME_ZADF_V_SDK_RETRY    .

  DATA: ENQUEUE_PROCESSED TYPE C. "flag: view enqueued by VIEWFRAME_...

*-<<<-------------------------------------------------------------->>>>*
* Entrypoint after changing maintenance mode (show <--> update)        *
*-<<<-------------------------------------------------------------->>>>*
  DO.
*----------------------------------------------------------------------*
* Select data from database                                            *
*----------------------------------------------------------------------*
CALL FUNCTION 'VIEWPROC_ZADF_V_SDK_RETRY'
         EXPORTING
              FCODE          = READ
              VIEW_ACTION    = VIEW_ACTION
              VIEW_NAME      = VIEW_NAME
         TABLES
              EXCL_CUA_FUNCT = EXCL_CUA_FUNCT
EXTRACT = ZADF_V_SDK_RETRY_EXTRACT
TOTAL = ZADF_V_SDK_RETRY_TOTAL
              X_HEADER       = X_HEADER
              X_NAMTAB       = X_NAMTAB
              DBA_SELLIST    = DBA_SELLIST
              DPL_SELLIST    = DPL_SELLIST
              CORR_KEYTAB    = E071K_TAB
         EXCEPTIONS
              MISSING_CORR_NUMBER       = 1
              NO_VALUE_FOR_SUBSET_IDENT = 2.
    CASE SY-SUBRC.
      WHEN 1.
        RAISE MISSING_CORR_NUMBER.
      WHEN 2.
        RAISE NO_VALUE_FOR_SUBSET_IDENT.
    ENDCASE.
*-<<<-------------------------------------------------------------->>>>*
* Entrypoint after saving data into database                           *
* Entrypoint after refreshing selected entries from database           *
*-<<<-------------------------------------------------------------->>>>*
    DO.
*----------------------------------------------------------------------*
* Edit data                                                            *
*----------------------------------------------------------------------*
      DO.
CALL FUNCTION 'VIEWPROC_ZADF_V_SDK_RETRY'
             EXPORTING
                  FCODE           = EDIT
                  VIEW_ACTION     = MAINT_MODE
                  VIEW_NAME       = VIEW_NAME
                  CORR_NUMBER     = CORR_NUMBER
             IMPORTING
                  UCOMM           = FUNCTION
UPDATE_REQUIRED = STATUS_ZADF_V_SDK_RETRY-UPD_FLAG
             TABLES
                  EXCL_CUA_FUNCT  = EXCL_CUA_FUNCT
EXTRACT = ZADF_V_SDK_RETRY_EXTRACT
TOTAL = ZADF_V_SDK_RETRY_TOTAL
                  X_HEADER        = X_HEADER
                  X_NAMTAB        = X_NAMTAB
                  DBA_SELLIST     = DBA_SELLIST
                  DPL_SELLIST     = DPL_SELLIST
                  CORR_KEYTAB     = E071K_TAB
             EXCEPTIONS
                  MISSING_CORR_NUMBER       = 1
                  NO_VALUE_FOR_SUBSET_IDENT = 2.
        CASE SY-SUBRC.
          WHEN 1.
            IF MAINT_MODE EQ TRANSPORTIEREN AND VIEW_ACTION EQ AENDERN.
              MOVE VIEW_ACTION TO MAINT_MODE.
            ELSE.
              PERFORM BEFORE_LEAVING_FRAME_FUNCTION
                                         USING X_HEADER-FRM_BF_END.
              RAISE MISSING_CORR_NUMBER.
            ENDIF.
          WHEN 2.
            RAISE NO_VALUE_FOR_SUBSET_IDENT.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ENDDO.
*----------------------------------------------------------------------*
*  Handle usercommands...                                              *
*  ...at first handle commands which could cause loss of data          *
*----------------------------------------------------------------------*
      IF FUNCTION EQ BACK. FUNCTION = END. ENDIF.
      IF ( FUNCTION EQ SWITCH_TO_SHOW_MODE OR
           FUNCTION EQ GET_ANOTHER_VIEW    OR
           FUNCTION EQ SWITCH_TRANSP_TO_UPD_MODE OR
           FUNCTION EQ END ) AND
STATUS_ZADF_V_SDK_RETRY-UPD_FLAG NE SPACE.
        PERFORM BEENDEN.
        CASE SY-SUBRC.
          WHEN 0.
CALL FUNCTION 'VIEWPROC_ZADF_V_SDK_RETRY'
                  EXPORTING
                      FCODE           = SAVE
                      VIEW_ACTION     = MAINT_MODE
                      VIEW_NAME       = VIEW_NAME
                      CORR_NUMBER     = CORR_NUMBER
                  IMPORTING
UPDATE_REQUIRED = STATUS_ZADF_V_SDK_RETRY-UPD_FLAG
                  TABLES
                      EXCL_CUA_FUNCT  = EXCL_CUA_FUNCT
EXTRACT = ZADF_V_SDK_RETRY_EXTRACT
TOTAL = ZADF_V_SDK_RETRY_TOTAL
                      X_HEADER        = X_HEADER
                      X_NAMTAB        = X_NAMTAB
                      DBA_SELLIST     = DBA_SELLIST
                      DPL_SELLIST     = DPL_SELLIST
                      CORR_KEYTAB     = E071K_TAB
                  EXCEPTIONS
                      MISSING_CORR_NUMBER       = 1
                      NO_VALUE_FOR_SUBSET_IDENT = 2
                      SAVING_CORRECTION_FAILED  = 3.
            CASE SY-SUBRC.
              WHEN 0.
IF STATUS_ZADF_V_SDK_RETRY-UPD_FLAG EQ SPACE. EXIT. ENDIF.
              WHEN 1. RAISE MISSING_CORR_NUMBER.
              WHEN 2. RAISE NO_VALUE_FOR_SUBSET_IDENT.
              WHEN 3.
            ENDCASE.
          WHEN 8. EXIT.
          WHEN 12.
        ENDCASE.
*----------------------------------------------------------------------*
*  ...2nd: transport request                                           *
*----------------------------------------------------------------------*
      ELSEIF FUNCTION EQ TRANSPORT.
IF STATUS_ZADF_V_SDK_RETRY-UPD_FLAG NE SPACE.
          PERFORM TRANSPORTIEREN.
          CASE SY-SUBRC.
            WHEN 0.
CALL FUNCTION 'VIEWPROC_ZADF_V_SDK_RETRY'
                    EXPORTING
                        FCODE           = SAVE
                        VIEW_ACTION     = MAINT_MODE
                        VIEW_NAME       = VIEW_NAME
                        CORR_NUMBER     = CORR_NUMBER
                    IMPORTING
                              UPDATE_REQUIRED =
STATUS_ZADF_V_SDK_RETRY-UPD_FLAG
                    TABLES
                        EXCL_CUA_FUNCT  = EXCL_CUA_FUNCT
EXTRACT = ZADF_V_SDK_RETRY_EXTRACT
TOTAL = ZADF_V_SDK_RETRY_TOTAL
                        X_HEADER        = X_HEADER
                        X_NAMTAB        = X_NAMTAB
                        DBA_SELLIST     = DBA_SELLIST
                        DPL_SELLIST     = DPL_SELLIST
                        CORR_KEYTAB     = E071K_TAB
                    EXCEPTIONS
                        MISSING_CORR_NUMBER       = 1
                        NO_VALUE_FOR_SUBSET_IDENT = 2
                        SAVING_CORRECTION_FAILED  = 3.
              CASE SY-SUBRC.
                WHEN 0. MAINT_MODE = TRANSPORTIEREN.
                WHEN 1. RAISE MISSING_CORR_NUMBER.
                WHEN 2. RAISE NO_VALUE_FOR_SUBSET_IDENT.
                WHEN 3.
              ENDCASE.
            WHEN 8.
              EXIT.
            WHEN 12.
          ENDCASE.
        ELSE.
          MAINT_MODE = TRANSPORTIEREN.
        ENDIF.
*----------------------------------------------------------------------*
*  ...now reset or save requests                                       *
*----------------------------------------------------------------------*
      ELSEIF FUNCTION EQ RESET_LIST  OR
             FUNCTION EQ RESET_ENTRY OR
             FUNCTION EQ SAVE.
*----------------------------------------------------------------------*
*  Refresh selected entries from database or save data into database   *
*----------------------------------------------------------------------*
CALL FUNCTION 'VIEWPROC_ZADF_V_SDK_RETRY'
             EXPORTING
                  FCODE               = FUNCTION
                  VIEW_ACTION         = MAINT_MODE
                  VIEW_NAME           = VIEW_NAME
                  CORR_NUMBER         = CORR_NUMBER
             IMPORTING
UPDATE_REQUIRED = STATUS_ZADF_V_SDK_RETRY-UPD_FLAG
             TABLES
                  EXCL_CUA_FUNCT      = EXCL_CUA_FUNCT
EXTRACT = ZADF_V_SDK_RETRY_EXTRACT
TOTAL = ZADF_V_SDK_RETRY_TOTAL
                  X_HEADER            = X_HEADER
                  X_NAMTAB            = X_NAMTAB
                  DBA_SELLIST         = DBA_SELLIST
                  DPL_SELLIST         = DPL_SELLIST
                  CORR_KEYTAB         = E071K_TAB
             EXCEPTIONS
                  MISSING_CORR_NUMBER       = 1
                  NO_VALUE_FOR_SUBSET_IDENT = 2
                  SAVING_CORRECTION_FAILED  = 3.
        CASE SY-SUBRC.
          WHEN 1. RAISE MISSING_CORR_NUMBER.
          WHEN 2. RAISE NO_VALUE_FOR_SUBSET_IDENT.
          WHEN 3.
        ENDCASE.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
*----------------------------------------------------------------------*
*  ...now other commands...                                            *
*----------------------------------------------------------------------*
    CASE FUNCTION.
      WHEN SWITCH_TO_SHOW_MODE.
*     change maintenance mode from update to show
        PERFORM ENQUEUE USING 'D' X_HEADER-FRM_AF_ENQ.    "dequeue view
        CLEAR ENQUEUE_PROCESSED.
        VIEW_ACTION = ANZEIGEN.
      WHEN SWITCH_TO_UPDATE_MODE.
*     change maintenance mode from show to update
        PERFORM ENQUEUE USING 'E' X_HEADER-FRM_AF_ENQ.    "enqueue view
        IF SY-SUBRC EQ 0.
          MOVE 'X' TO ENQUEUE_PROCESSED.
          VIEW_ACTION = AENDERN.
        ENDIF.
      WHEN SWITCH_TRANSP_TO_UPD_MODE.
*     change maintenance mode from transport to update
        VIEW_ACTION = AENDERN.
      WHEN TRANSPORT.
*     change maintenance mode from update to transport
        VIEW_ACTION = TRANSPORTIEREN.
      WHEN OTHERS.
        IF ENQUEUE_PROCESSED NE SPACE.
          PERFORM ENQUEUE USING 'D' X_HEADER-FRM_AF_ENQ.  "dequeue view
        ENDIF.
        PERFORM BEFORE_LEAVING_FRAME_FUNCTION USING X_HEADER-FRM_BF_END.
        EXIT.
    ENDCASE.
  ENDDO.
ENDFUNCTION.
