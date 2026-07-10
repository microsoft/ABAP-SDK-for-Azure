
PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zssf_data CURSOR nextline.
    MODULE liste_show_liste.
    MODULE screen_change_pwd.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zssf_data-interface_id .
      FIELD zssf_data-zkey .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zssf_data-interface_id .
      MODULE liste_update_liste.
    ENDCHAIN.
    CHAIN.
      FIELD zssf_data-zkey.
      MODULE validate_key ON CHAIN-REQUEST.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
