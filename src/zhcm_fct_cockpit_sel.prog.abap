*&---------------------------------------------------------------------*
*& Include          ZHCM_FCT_COCKPIT_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN : BEGIN OF BLOCK b_15 WITH FRAME TITLE TEXT-t10.
  PARAMETERS: r_com RADIOBUTTON GROUP rd6 DEFAULT 'X' USER-COMMAND u6 MODIF ID opc.
  SELECTION-SCREEN : BEGIN OF BLOCK b_01 WITH FRAME.
    PARAMETERS: r_fct RADIOBUTTON GROUP rd1 DEFAULT 'X' USER-COMMAND u1 MODIF ID mfr.

*  Opções FCT
    SELECTION-SCREEN : BEGIN OF BLOCK b_02 WITH FRAME.
      PARAMETERS: p_envfct TYPE flag MODIF ID env.
*    admitir colaborador
      PARAMETERS: r_admi RADIOBUTTON GROUP rd2 DEFAULT 'X' USER-COMMAND u2 MODIF ID mfc.
      SELECTION-SCREEN : BEGIN OF BLOCK b_03 WITH FRAME.
        SELECT-OPTIONS: s_massna FOR pa0000-massn MODIF ID ma.
        SELECT-OPTIONS: s_reta FOR zhcm_vfct_lgart-lgart MODIF ID ma MATCHCODE OBJECT zhcm_shfct_lgart.
        SELECT-OPTIONS: s_diua FOR zhcm_vfct_lgart-lgart MODIF ID ma MATCHCODE OBJECT zhcm_shfct_lgart.
      SELECTION-SCREEN : END OF BLOCK b_03.

*    modificação de contrato
      PARAMETERS: r_mod  RADIOBUTTON GROUP rd2 MODIF ID mfc.
      SELECTION-SCREEN : BEGIN OF BLOCK b_05 WITH FRAME.
        SELECT-OPTIONS: s_retm FOR zhcm_vfct_lgart-lgart MODIF ID mm MATCHCODE OBJECT zhcm_shfct_lgart.
        SELECT-OPTIONS: s_dium FOR zhcm_vfct_lgart-lgart MODIF ID mm MATCHCODE OBJECT zhcm_shfct_lgart.
        PARAMETERS: p_modnul AS CHECKBOX MODIF ID mm.
      SELECTION-SCREEN : END OF BLOCK b_05.

*    cessação de contrato
      PARAMETERS: r_cess RADIOBUTTON GROUP rd2 MODIF ID mfc.
      SELECTION-SCREEN : BEGIN OF BLOCK b_04 WITH FRAME.
        SELECT-OPTIONS: s_massnc FOR pa0000-massn MODIF ID mc.
        PARAMETERS: p_cesnul AS CHECKBOX MODIF ID mc.
        PARAMETERS: p_trcfct TYPE flag MODIF ID mc.
      SELECTION-SCREEN : END OF BLOCK b_04.

    SELECTION-SCREEN : END OF BLOCK b_02.

*  segurança social
    PARAMETERS: r_ss  RADIOBUTTON GROUP rd1 MODIF ID msr.

    SELECTION-SCREEN : BEGIN OF BLOCK b_06 WITH FRAME.
      PARAMETERS: p_envss TYPE flag MODIF ID mfj.
*    vinculo colaborador
      PARAMETERS: r_vincs RADIOBUTTON GROUP rd5 DEFAULT 'X' USER-COMMAND u5 MODIF ID mfs.
      SELECTION-SCREEN : BEGIN OF BLOCK b_11 WITH FRAME.
        SELECT-OPTIONS: s_massns FOR pa0000-massn MODIF ID mss.
        SELECT-OPTIONS: s_rets FOR zhcm_vfct_lgart-lgart MODIF ID mss MATCHCODE OBJECT zhcm_shfct_lgart.
        SELECT-OPTIONS: s_dius FOR zhcm_vfct_lgart-lgart MODIF ID mss MATCHCODE OBJECT zhcm_shfct_lgart.
        PARAMETERS: p_trass TYPE flag MODIF ID mss.
      SELECTION-SCREEN : END OF BLOCK b_11.

*    modificação SS
      PARAMETERS: r_mods  RADIOBUTTON GROUP rd5 MODIF ID mfs.
      SELECTION-SCREEN : BEGIN OF BLOCK b_12 WITH FRAME.
        SELECT-OPTIONS: s_masnsm FOR pa0000-massn MODIF ID msa.
        SELECT-OPTIONS: s_retsm FOR zhcm_vfct_lgart-lgart MODIF ID msa MATCHCODE OBJECT zhcm_shfct_lgart.
        SELECT-OPTIONS: s_diusm FOR zhcm_vfct_lgart-lgart MODIF ID msa MATCHCODE OBJECT zhcm_shfct_lgart.
      SELECTION-SCREEN : END OF BLOCK b_12.

*   cessação SS
      PARAMETERS: r_cesss  RADIOBUTTON GROUP rd5 MODIF ID mfs.
      SELECTION-SCREEN : BEGIN OF BLOCK b_13 WITH FRAME.
        SELECT-OPTIONS: s_masnsc FOR pa0000-massn MODIF ID msb.
        PARAMETERS: p_trcss TYPE flag MODIF ID msb.
      SELECTION-SCREEN : END OF BLOCK b_13.

*   consulta contrato SS
      PARAMETERS: r_consc  RADIOBUTTON GROUP rd5 MODIF ID mfs.
    SELECTION-SCREEN : END OF BLOCK b_06.

  SELECTION-SCREEN : END OF BLOCK b_01.

*  Admissões via ficheiro
  PARAMETERS: r_fcom RADIOBUTTON GROUP rd6.
  SELECTION-SCREEN : BEGIN OF BLOCK b_17 WITH FRAME.
*     FCT comunicação via ficheiro
    PARAMETERS: r_aff  RADIOBUTTON GROUP rd7 MODIF ID adc DEFAULT 'X' USER-COMMAND u7.
    SELECTION-SCREEN : BEGIN OF BLOCK b_18 WITH FRAME.
*     FCT comunicação via ficheiro online
      PARAMETERS: r_affo  RADIOBUTTON GROUP rd8 MODIF ID adf DEFAULT 'X' USER-COMMAND u8.
      PARAMETERS: p_fichfo TYPE rlgrap-filename MODIF ID adf.

*      FCT comunicação via ficheiro online background
      PARAMETERS: r_affb  RADIOBUTTON GROUP rd8 MODIF ID adf.
      PARAMETERS: p_fichfb TYPE rlgrap-filename MODIF ID adf.
    SELECTION-SCREEN : END OF BLOCK b_18.

*      SS comunicação via ficheiro
    PARAMETERS: r_afs  RADIOBUTTON GROUP rd7 MODIF ID adc.
    SELECTION-SCREEN : BEGIN OF BLOCK b_19 WITH FRAME.
*     SS comunicação via ficheiro online
      PARAMETERS: r_afso  RADIOBUTTON GROUP rd9 MODIF ID ads DEFAULT 'X' USER-COMMAND u9.
      PARAMETERS: p_fichso TYPE rlgrap-filename MODIF ID ads .

*      SS comunicação via ficheiro online background
      PARAMETERS: r_afsb  RADIOBUTTON GROUP rd9 MODIF ID ads.
      PARAMETERS: p_fichsb TYPE rlgrap-filename MODIF ID ads.
    SELECTION-SCREEN : END OF BLOCK b_19.


  SELECTION-SCREEN : END OF BLOCK b_17.

  PARAMETERS: r_frm RADIOBUTTON GROUP rd6.
  SELECTION-SCREEN : BEGIN OF BLOCK b_14 WITH FRAME.
*    parametrizações
    SELECTION-SCREEN: PUSHBUTTON 1(20) TEXT-t09 USER-COMMAND param MODIF ID mfo.
*    ferramentas
*    carregamento FCT para log
    PARAMETERS: r_fichfl  RADIOBUTTON GROUP rd3 DEFAULT 'X' USER-COMMAND u3 MODIF ID mff.
    SELECTION-SCREEN : BEGIN OF BLOCK b_07 WITH FRAME.
      SELECTION-SCREEN: PUSHBUTTON 29(19) TEXT-t08 USER-COMMAND but MODIF ID mfl.
      PARAMETERS: p_fichfl TYPE rlgrap-filename MODIF ID mfl.
    SELECTION-SCREEN : END OF BLOCK b_07.

*    carregamento SS para log
    PARAMETERS: r_fichsl  RADIOBUTTON GROUP rd3 MODIF ID mff.
    SELECTION-SCREEN : BEGIN OF BLOCK b_20 WITH FRAME.
      SELECTION-SCREEN: PUSHBUTTON 29(18) TEXT-t11 USER-COMMAND buv MODIF ID mfv.
      PARAMETERS: p_fichsl TYPE rlgrap-filename MODIF ID mfv.
    SELECTION-SCREEN : END OF BLOCK b_20.

*    consulta de log
    PARAMETERS: r_cons RADIOBUTTON GROUP rd3 MODIF ID mff.
    SELECTION-SCREEN : BEGIN OF BLOCK b_08 WITH FRAME.
      PARAMETERS: r_consf RADIOBUTTON GROUP rd4 DEFAULT 'X' USER-COMMAND u4 MODIF ID mfg.
      PARAMETERS: r_conss RADIOBUTTON GROUP rd4 MODIF ID mfg.
      PARAMETERS: r_const RADIOBUTTON GROUP rd4 MODIF ID mfg.

      SELECTION-SCREEN : BEGIN OF BLOCK b_10 WITH FRAME TITLE TEXT-t06.
        SELECTION-SCREEN BEGIN OF LINE.
          SELECTION-SCREEN COMMENT 1(17) TEXT-t04 MODIF ID mfg.
          PARAMETERS: p_dati TYPE datum MODIF ID mfg DEFAULT sy-datum.
          PARAMETERS: p_timi TYPE uzeit MODIF ID mfg.
        SELECTION-SCREEN END OF LINE.
        SELECTION-SCREEN BEGIN OF LINE.
          SELECTION-SCREEN COMMENT 1(17) TEXT-t05 MODIF ID mfg.
          PARAMETERS: p_datf TYPE datum MODIF ID mfg DEFAULT sy-datum.
          PARAMETERS: p_timf TYPE uzeit MODIF ID mfg DEFAULT '235959'.
        SELECTION-SCREEN END OF LINE.
      SELECTION-SCREEN : END OF BLOCK b_10.

    SELECTION-SCREEN : END OF BLOCK b_08.
  SELECTION-SCREEN : END OF BLOCK b_14.
SELECTION-SCREEN : END OF BLOCK b_15.
