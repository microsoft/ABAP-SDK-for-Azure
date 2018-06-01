*----------------------------------------------------------------------*
***INCLUDE LZRESTF02 .
*----------------------------------------------------------------------*
*{   INSERT         SMTK903673                                        1
FORM authorization_check.

*    vim_auth_rc = 0.
*
*  TRY.
*      CALL METHOD zcl_rest_utility_class=>check_authority.
*    CATCH zcx_http_client_failed INTO DATA(lv_textid).
*      vim_auth_msgno  = lv_textid->if_t100_message~t100key-msgno.
*      vim_auth_msgid  = lv_textid->if_t100_message~t100key-msgid.
*      vim_auth_rc = 8.
*  ENDTRY.
ENDFORM.
*}   INSERT
