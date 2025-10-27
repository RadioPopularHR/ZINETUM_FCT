interface ZHCM_FCT_BADI_ITF
  public .


  interfaces IF_BADI_INTERFACE .

  methods PREP_PREST_TRAB
    importing
      !IS_P0000 type P0000
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    exporting
      !EV_PREST_TRAB type ZHCM_FCT_SS_PREST_TRAB
    raising
      ZCX_HCM_FCT_COCKPIT .
  methods PREP_COM_DESEMP
    importing
      !IS_P0000 type P0000
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    exporting
      !EV_COM_DESMP type ZHCM_FCT_SS_COM_DESMP
    raising
      ZCX_HCM_FCT_COCKPIT .
  methods PREP_RUB_RET_DIU
    importing
      !IR_RET type HRPAY00_T_LGART_RANGE
      !IR_DIU type HRPAY00_T_LGART_RANGE
      !IS_P0008 type P0008
      !IS_P0001 type P0001
      !IS_P0007 type P0007
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_USA_IT0052 type FLAG optional
    exporting
      !EV_RETRIBUICAO type ZHCM_FCT_RTRBC
      !EV_DIUTURNIDADES type ZHCM_FCT_DIUTRND
    raising
      ZCX_HCM_FCT_COCKPIT .
  methods PREP_NIVEL_UTIL
    importing
      !IS_P0008 type P0008
      !IS_P0007 type P0007
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    exporting
      !EV_BSGRD type BSGRD .
  methods PREP_MODALIDADE_CONTRATO
    importing
      !IS_P0008 type P0008
      !IS_P0007 type P0007
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_ACAO_GLOBAL type ZHCM_FCT_ACAO
      !IV_TIPO_CONTRATO type CTTYP
      !IS_P0001 type P0001
    exporting
      !EV_CALC_ANUAL type ZHCM_FCT_CALC_ANUAL
    changing
      !CS_DADOS_FCT type ZHCM_TFCT_LOG optional
      !CS_DADOS_SS type ZHCM_TFCT_SS_LOG optional .
endinterface.
