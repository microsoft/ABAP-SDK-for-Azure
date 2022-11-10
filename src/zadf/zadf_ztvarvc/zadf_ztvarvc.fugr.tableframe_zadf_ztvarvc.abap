*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZADF_ZTVARVC
*   generation date: 11/10/2022 at 09:16:02
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZADF_ZTVARVC       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
