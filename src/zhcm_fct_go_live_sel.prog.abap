*&---------------------------------------------------------------------*
*& Include          ZHCM_FCT_GO_LIVE_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN : BEGIN OF BLOCK b_15 WITH FRAME TITLE TEXT-t10.
  PARAMETERS: p_env       TYPE flag MODIF ID env.
*    modificação SS
  PARAMETERS: r_mods  RADIOBUTTON GROUP rd1 DEFAULT 'X' USER-COMMAND u1.
  SELECTION-SCREEN : BEGIN OF BLOCK b_12 WITH FRAME.

    PARAMETERS: p_masnsm    TYPE massn MATCHCODE OBJECT hrpad_massn.
    PARAMETERS: p_masnsg    TYPE massg MATCHCODE OBJECT hrpad_massg.
    PARAMETERS: p_begda     TYPE begda.
    SELECT-OPTIONS: s_retsm FOR zhcm_vfct_lgart-lgart MATCHCODE OBJECT zhcm_shfct_lgart.
    SELECT-OPTIONS: s_diusm FOR zhcm_vfct_lgart-lgart MATCHCODE OBJECT zhcm_shfct_lgart.
  SELECTION-SCREEN : END OF BLOCK b_12.

*    modificação de contrato FCT
  PARAMETERS: r_mod  RADIOBUTTON GROUP rd1 .
  SELECTION-SCREEN : BEGIN OF BLOCK b_05 WITH FRAME.
    PARAMETERS: p_begdaf    TYPE begda.
    SELECT-OPTIONS: s_retm FOR zhcm_vfct_lgart-lgart MATCHCODE OBJECT zhcm_shfct_lgart.
    SELECT-OPTIONS: s_dium FOR zhcm_vfct_lgart-lgart MATCHCODE OBJECT zhcm_shfct_lgart.
  SELECTION-SCREEN : END OF BLOCK b_05.


*    consulta de log
  PARAMETERS: r_cons RADIOBUTTON GROUP rd1.
  SELECTION-SCREEN : BEGIN OF BLOCK b_10 WITH FRAME TITLE TEXT-t06.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(17) TEXT-t04.
      PARAMETERS: p_dati TYPE datum DEFAULT sy-datum.
      PARAMETERS: p_timi TYPE uzeit.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(17) TEXT-t05 MODIF ID mfg.
      PARAMETERS: p_datf TYPE datum DEFAULT sy-datum.
      PARAMETERS: p_timf TYPE uzeit DEFAULT '235959'.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN : END OF BLOCK b_10.

SELECTION-SCREEN : END OF BLOCK b_15.
