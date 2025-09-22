* VCL-Modifications
PROCESS BEFORE OUTPUT.

  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_zhcm_vfct_ss_cnt CURSOR nextline.
    MODULE liste_show_liste.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD zhcm_vfct_ss_cnt-cttyp .
      FIELD zhcm_vfct_ss_cnt-bukrs .
      FIELD zhcm_vfct_ss_cnt-parcial .
      FIELD zhcm_vfct_ss_cnt-modalidade .
      FIELD zhcm_vfct_ss_cnt-text .
      FIELD zhcm_vfct_ss_cnt-val_fct .
      FIELD zhcm_vfct_ss_cnt-val_ss .
      MODULE set_update_flag ON CHAIN-REQUEST.
      MODULE complete_zhcm_vfct_ss_cnt ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD zhcm_vfct_ss_cnt-cttyp .
      FIELD zhcm_vfct_ss_cnt-bukrs .
      FIELD zhcm_vfct_ss_cnt-parcial .
      FIELD zhcm_vfct_ss_cnt-modalidade.
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
*
PROCESS ON VALUE-REQUEST.
  FIELD  zhcm_vfct_ss_cnt-modalidade MODULE vcl_help_values.
