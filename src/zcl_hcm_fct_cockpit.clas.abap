class ZCL_HCM_FCT_COCKPIT definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_ssnum_s,
             ssnum TYPE ppt_ssnum,
             pernr TYPE pernr_d,
             bukrs TYPE bukrs,
           END OF ty_ssnum_s .
  types:
    ty_ssnum_tt TYPE STANDARD TABLE OF ty_ssnum_s .

  constants AC_ACAO_ADMISSAO type ZHCM_FCT_ACAO value 'ADMI' ##NO_TEXT.
  constants AC_ACAO_CESSACAO type ZHCM_FCT_ACAO value 'CESS' ##NO_TEXT.
  constants AC_ACAO_CONSULTA type ZHCM_FCT_ACAO value 'CONS' ##NO_TEXT.
  constants AC_ACAO_FCT type ZHCM_FCT_ACAO value 'FCT' ##NO_TEXT.
  constants AC_ACAO_FCT_FICH_COM_ADMI type ZHCM_FCT_ACAO value 'FFCAD' ##NO_TEXT.
  constants AC_ACAO_FCT_FICH_COM_ADMI_BCK type ZHCM_FCT_ACAO value 'FFCADB' ##NO_TEXT.
  constants AC_ACAO_FCT_FICH_LOG type ZHCM_FCT_ACAO value 'FFL' ##NO_TEXT.
  constants AC_ACAO_MODIFICACAO type ZHCM_FCT_ACAO value 'MOD' ##NO_TEXT.
  constants AC_ACAO_SS type ZHCM_FCT_ACAO value 'SS' ##NO_TEXT.
  constants AC_ACAO_SS_ALT_PER type ZHCM_FCT_ACAO value 'SSP' ##NO_TEXT.
  constants AC_ACAO_SS_CESSACAO type ZHCM_FCT_ACAO value 'SSC' ##NO_TEXT.
  constants AC_ACAO_SS_CONS_CONT type ZHCM_FCT_ACAO value 'CONSC' ##NO_TEXT.
  constants AC_ACAO_SS_FICH_COM_VINC type ZHCM_FCT_ACAO value 'SFCAD' ##NO_TEXT.
  constants AC_ACAO_SS_FICH_COM_VINC_BCK type ZHCM_FCT_ACAO value 'SFCADB' ##NO_TEXT.
  constants AC_ACAO_SS_FICH_LOG type ZHCM_FCT_ACAO value 'SFL' ##NO_TEXT.
  constants AC_ACAO_SS_MODIFICACAO type ZHCM_FCT_ACAO value 'SSM' ##NO_TEXT.
  constants AC_ACAO_SS_VINCULO type ZHCM_FCT_ACAO value 'SSV' ##NO_TEXT.
  constants AC_COM_DESMP_NAO type ZHCM_FCT_SS_COM_DESMP value '0' ##NO_TEXT.
  constants AC_COM_DESMP_SIM type ZHCM_FCT_SS_COM_DESMP value '1' ##NO_TEXT.
  constants AC_INFTY_0000 type INFTY value '0000' ##NO_TEXT.
  constants AC_INFTY_0008 type INFTY value '0008' ##NO_TEXT.
  constants AC_INFTY_0052 type INFTY value '0052' ##NO_TEXT.
  constants AC_LOG_OBJ type BALOBJ_D value 'ZINETUMFCT' ##NO_TEXT.
  constants AC_LOG_SUB_OBJ_FCT type BALSUBOBJ value 'ZFCT' ##NO_TEXT.
  constants AC_LOG_SUB_OBJ_SS type BALSUBOBJ value 'ZSS' ##NO_TEXT.
  constants AC_PREST_TRAB_PRES type ZHCM_FCT_SS_PREST_TRAB value 'P' ##NO_TEXT.
  constants AC_PREST_TRAB_TELE type ZHCM_FCT_SS_PREST_TRAB value 'T' ##NO_TEXT.
  constants AC_PREST_TRAB_TELE_PAR type ZHCM_FCT_SS_PREST_TRAB value 'A' ##NO_TEXT.
  constants AV_IT0052_SUBTY type SUBTY value '0' ##NO_TEXT.

  class-methods OBTEM_DATA_INI_CONTRATO
    importing
      !IS_P0016 type P0016
    returning
      value(RV_CTBEG) type CTBEG .
  class-methods OBTEM_EXCEL_TEMPLATE
    importing
      !IV_R3_APPLICATION_NAME type CHAR20
      !IV_DESC type STRING .
  class-methods OBTEM_SISTEMA_IT0302
    returning
      value(RV_USA_IT0302) type FLAG .
  class-methods OBTEM_TEMPO_PARCIAL_CONF
    returning
      value(RV_TEMPO_PARCIAL_CONF) type RVARI_VAL_255 .
  methods ATUALIZA_REGISTO_ADMI
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods ATUALIZA_REGISTO_ALT_PER
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods ATUALIZA_REGISTO_CESS
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods ATUALIZA_REGISTO_CESS_VIN_SS
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods ATUALIZA_REGISTO_MOD
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods ATUALIZA_REGISTO_MOD_VIN_SS
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods ATUALIZA_REGISTO_VINC_SS
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods CONSTRUCTOR
    importing
      !IV_ACAO type ZHCM_FCT_ACAO optional .
  methods CONSULTA_LOG
    importing
      !IV_SUBOBJECT type BALSUBOBJ optional
      !IV_ALDATE_I type BALDATE
      !IV_ALTIME_I type BALTIME
      !IV_ALDATE_F type BALDATE
      !IV_ALTIME_F type BALTIME .
  methods ESCREVE_MSG_LOG
    importing
      !IS_CAMPOS_ADICIONAIS type ZHCM_SFCT_LOG_STRUCT optional
      !IV_TEXTO_LIVRE type BALTMSG optional .
  methods GUARDA_DADOS_ALV .
  methods GUARDA_LOG .
  methods INTEGRA_FCT_FICH_ADMI
    importing
      !IV_NOME_FICHEIRO type LOCALFILE
    exporting
      !EV_ERRO type FLAG .
  methods INTEGRA_FCT_FICH_ADMI_SERV
    importing
      !IV_NOME_FICHEIRO type LOCALFILE
    exporting
      !EV_ERRO type FLAG .
  methods INTEGRA_SS_FICH_VINC
    importing
      !IV_NOME_FICHEIRO type LOCALFILE
    exporting
      !EV_ERRO type FLAG .
  methods INTEGRA_SS_FICH_VINC_SERV
    importing
      !IV_NOME_FICHEIRO type LOCALFILE
    exporting
      !EV_ERRO type FLAG .
  methods MOSTRA_ALV .
  methods MOSTRA_LOG_ONLINE .
  methods PREPARA_ACAO
    importing
      !IV_ACAO type ZHCM_FCT_ACAO .
  methods PREP_ACAO_DESC .
  methods PREP_ANULACAO_FLAG
    importing
      !IV_ANUL_ACAO type FLAG .
  methods PREP_DADOS_ADMI
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_PERNR type PERSNO
      !IV_MODALIDADE type ZHCM_FCT_MODA
      !IV_CTBDT type ZHCM_FCT_CTBDT
      !IV_CTEDT type CTEDT
      !IS_P0008 type P0008
      !IR_RET type HRPAY00_T_LGART_RANGE
      !IR_DIU type HRPAY00_T_LGART_RANGE
      !IS_P0001 type P0001
      !IS_P0007 type P0007
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_USA_IT0052 type FLAG
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods PREP_DADOS_CESS
    importing
      !IS_P0000 type P0000 .
  methods PREP_DADOS_MOD
    importing
      !IS_P0008 type P0008
      !IR_RET type HRPAY00_T_LGART_RANGE
      !IR_DIU type HRPAY00_T_LGART_RANGE
      !IV_INIC_PERI_REND type DATUM
      !IV_CTBDT type ZHCM_FCT_CTBDT
      !IS_P0001 type P0001
      !IS_P0007 type P0007
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_USA_IT0052 type FLAG
    raising
      ZCX_HCM_FCT_COCKPIT .
  methods PREP_DADOS_VINC_SS
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_GBDAT type GBDAT optional
      !IV_MODALIDADE type ZHCM_FCT_MODA
      !IV_CTBDT type ZHCM_FCT_CTBDT
      !IV_CTEDT type CTEDT
      !IS_P0008 type P0008
      !IR_RET type HRPAY00_T_LGART_RANGE
      !IR_DIU type HRPAY00_T_LGART_RANGE
      !IV_PRCNP type PPT_PRCNP
      !IS_P0007 type P0007
      !IV_SSNUM_SUBS type ZPPT_SSNUM_SUB
      !IS_P0001 type P0001
      !IS_P0000 type P0000
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_USA_IT0052 type FLAG
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods PREP_DADOS_VINC_SS_ALT_PER
    importing
      !IV_SSNUM type PPT_SSNUM
      !IS_P0000 type P0000
      !IV_BUKRS type BUKRS
      !IR_RET type HRPAY00_T_LGART_RANGE
      !IR_DIU type HRPAY00_T_LGART_RANGE
      !IS_P0008 type P0008
      !IS_P0007 type P0007
      !IS_P0001 type P0001
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_USA_IT0052 type FLAG
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods PREP_DADOS_VINC_SS_CESS
    importing
      !IV_SSNUM type PPT_SSNUM
      !IS_P0000 type P0000
      !IV_BUKRS type BUKRS
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods PREP_DADOS_VINC_SS_MOD
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_MODALIDADE type ZHCM_FCT_MODA
      !IV_CTBDT type ZHCM_FCT_CTBDT
      !IV_CTEDT type CTEDT
      !IS_P0008 type P0008
      !IR_RET type HRPAY00_T_LGART_RANGE
      !IR_DIU type HRPAY00_T_LGART_RANGE
      !IV_PRCNP type PPT_PRCNP
      !IS_P0007 type P0007
      !IV_SSNUM_SUBS type ZPPT_SSNUM_SUB
      !IS_P0001 type P0001
      !IS_P0000 type P0000
      !IV_BEGDA type BEGDA
      !IV_ENDDA type ENDDA
      !IV_USA_IT0052 type FLAG
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods PREP_LOG .
  methods PREP_TRANSF_EMPRESA_FLAG
    importing
      !IV_TRANSF_EMPRESA type FLAG .
  methods PREP_ULTIMO_FCT
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_BUKRS type BUKRS .
  methods PREP_ULTIMO_SS
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_BUKRS type BUKRS .
  methods PROCESSA_SS_CONTRATOS
    importing
      !IV_BEGDA type BEGDA
      !IT_SSNUM type TY_SSNUM_TT
      !IV_ENDDA type ENDDA
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods VALIDA_DURACAO_CONTRATO
    importing
      !IS_P0016 type P0016
      !IV_SSNUM type PPT_SSNUM
      !IV_PERNR type PERSNO
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods VALIDA_MOTIVO_TRANSF
    importing
      !IS_P0000 type P0000
      !IV_BUKRS type BUKRS
    returning
      value(RV_OK) type FLAG .
  methods VALIDA_ULTIMO_ATIVO
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_BUKRS type BUKRS
      !IV_CTBDT type ZHCM_FCT_CTBDT optional
      !IV_MODNUL type FLAG optional
    returning
      value(RV_OK) type FLAG .
  methods VALIDA_ULTIMO_ATIVO_CESS
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_BUKRS type BUKRS
    returning
      value(RV_OK) type FLAG .
  methods VALIDA_ULTIMO_ATIVO_CESS_ANUL
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_BUKRS type BUKRS
    returning
      value(RV_OK) type FLAG .
  methods VALIDA_ULTIMO_ATIVO_SS
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_BUKRS type BUKRS
      !IV_CTBDT type ZHCM_FCT_CTBDT optional
      !IV_ALT_PER type FLAG optional
    returning
      value(RV_OK) type FLAG .
protected section.
private section.

  constants AC_LIGHT_GREEN type BALIMSGTY value '@08@' ##NO_TEXT.
  constants AC_LIGHT_OUT type BALIMSGTY value '@EB@' ##NO_TEXT.
  constants AC_LIGHT_RED type BALIMSGTY value '@0A@' ##NO_TEXT.
  constants AC_LIGHT_YELLOW type BALIMSGTY value '@09@' ##NO_TEXT.
  class-data AT_DADOS_FCT_ALV type ZHCM_TTFCT_LOG_ALV .
  class-data AT_DADOS_SS_ALV type ZHCM_TTFCT_SS_LOG_ALV .
  class-data AV_LOG_HANDLE type BALLOGHNDL .
  data AC_STATUS_ATIVO type ZHCM_FCT_STATUS value '1' ##NO_TEXT.
  data AC_STATUS_DESATIVO type ZHCM_FCT_STATUS value '0' ##NO_TEXT.
  data AO_DADOS_ALV type ref to CL_SALV_TABLE .
  data AS_DADOS_FCT type ZHCM_TFCT_LOG .
  data AS_DADOS_FCT_ULTIMO type ZHCM_TFCT_LOG .
  data AS_DADOS_SS type ZHCM_TFCT_SS_LOG .
  data AS_DADOS_SS_ALV_CONSC type ZHCM_SFCT_SS_LOG_ALV_CONSC .
  data AS_DADOS_SS_ULTIMO type ZHCM_TFCT_SS_LOG .
  data AT_DADOS_SS_ALV_CONSC type ZHCM_TTFCT_SS_LOG_ALV_CONSC .
  data AT_PORTAS_LOGICAS type ZHCM_TTFCT_PRTLOG .
  data AV_ACAO type ZHCM_FCT_ACAO .
  data AV_ACAO_DESC type ZHCM_FCT_ACAO_DESC .
  data AV_ACAO_GLOBAL type ZHCM_FCT_ACAO .
  data AV_ACAO_MIN type ZHCM_FCT_ACAO_MIN .
  data AV_ANUL_ACAO type FLAG .
  data AV_CALC_ANUAL type ZHCM_FCT_CALC_ANUAL .
  data AV_COMM_MSG type BALTMSG .
  data AV_CONS_CONTR_TIMEOUT type I .
  data AV_CONTRATO_DUR_MIN type I .
  data AV_FER_CALEND type HIDENT .
  data AV_ICON type BALIMSGTY .
  data AV_TRANSF_EMPRESA type FLAG .

  methods VALIDA_MOD_CTBDT
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_BUKRS type BUKRS
      !IV_CTBDT type ZHCM_FCT_CTBDT
    returning
      value(RV_EXISTS) type FLAG .
  methods VALIDA_MOD_CTBDT_SS
    importing
      !IV_SSNUM type PPT_SSNUM
      !IV_BUKRS type BUKRS
      !IV_CTBDT type ZHCM_FCT_CTBDT
    returning
      value(RV_EXISTS) type FLAG .
  methods ATUALIZACAO_MANUAL
    raising
      ZCX_HCM_FCT_COCKPIT .
  methods ATUALIZA_BD
    importing
      !IV_EXEC_FICHEIRO type FLAG optional
      !IV_ATU_MANUAL type ZHCM_FCT_ATU_MANUAL optional .
  methods ATUALIZA_BD_LOG_GERAL
    importing
      !IV_TEXTO_LIVRE type BALTMSG .
  methods ATUALIZA_BD_SS
    importing
      !IV_EXEC_FICHEIRO type FLAG optional
      !IV_ATU_MANUAL type ZHCM_FCT_ATU_MANUAL optional .
  methods ATUALIZA_BD_SS_LOG_GERAL
    importing
      !IV_TEXTO_LIVRE type BALTMSG .
  methods CALC_DIAS_TRAB_ANUAL
    returning
      value(RV_DIAS_UTEIS) type I .
  methods COMUNICA_DADOS .
  methods CONSULTA_CONTRATO .
  methods CONVERTE_DATA
    importing
      !IV_DATA type DATUM
    returning
      value(RV_DATA) type XSDDATETIME_ISO .
  methods CONVERTE_DATA_FICHEIRO
    importing
      !IV_DATA type CHAR50
    returning
      value(RV_DATA) type DATUM .
  methods GUARDA_DADOS_ALV_CONSC .
  methods LIMPA_LOG .
  methods MOSTRA_AVL_CONSC_POP .
  methods MOSTRA_LOG
    importing
      !IT_LOG_HANDLE type BAL_T_LOGH .
  methods OBTEM_CAMPOS_LOG
    returning
      value(RS_LOG) type ZHCM_SFCT_LOG_STRUCT .
  methods OBTEM_SS_CONTRATOS
    importing
      !IV_BEGDA type BEGDA
      !IT_SSNUM type TY_SSNUM_TT
      !IV_ENDDA type ENDDA
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods POPUP_ATU_MANUAL
    returning
      value(RV_ANSWER) type CHAR1 .
  methods POPUP_CONTRATO
    returning
      value(RV_CONTRATO) type ZHCM_FCT_CONTRATO .
  methods PREPARA_ACAO_GLOB_MIN .
  methods PREP_CALENDARIO
    importing
      !IS_P0001 type P0001
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods PREP_COLUNAS .
  methods PREP_COLUNAS_FCT_ADMIN
    changing
      !CO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  methods PREP_COLUNAS_FCT_CESS
    changing
      !CO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  methods PREP_COLUNAS_FCT_MOD
    changing
      !CO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  methods PREP_COLUNAS_SS_ALT_PER
    changing
      !CO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  methods PREP_COLUNAS_SS_CESS
    changing
      !CO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  methods PREP_COLUNAS_SS_MOD
    changing
      !CO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  methods PREP_COLUNAS_SS_VINC
    changing
      !CO_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  methods PREP_CONTR_TIMEOUT .
  methods PREP_DIAS_TRAB
    importing
      !IS_P0007 type P0007
      !IS_P0001 type P0001
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods PREP_DUR_MIN_CONTRATO .
  methods PREP_FUNC .
  methods PREP_FUNDAMENTACAO
    importing
      !IV_MOTIVO type ZHCM_FCT_SS_MOTIV .
  methods PREP_HORAS_TRAB
    importing
      !IS_P0007 type P0007 .
  methods PREP_LAYOUT .
  methods PREP_LOCAL_TRAB
    importing
      !IS_P0001 type P0001
      !IV_BEGDA type BEGDA .
  methods PREP_MOTIVO
    importing
      !IS_P0000 type P0000 .
  methods PREP_ORDENA .
  methods PREP_PFSTATUS .
  methods PREP_PORTAS_LOGICAS .
  methods PREP_TOP_OF_PAGE .
  methods REMOVE_REGISTO
    importing
      !IV_ATU_MANUAL type ZHCM_FCT_ATU_MANUAL optional .
  methods RESET_STAT_COM .
  methods USER_COMMAND
    for event ADDED_FUNCTION of CL_SALV_EVENTS_TABLE .
  methods VALIDA_CAMPOS_OBRG_FCT_ADMI
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
  methods VALIDA_CAMPOS_OBRG_SS_VINC
    raising
      resumable(ZCX_HCM_FCT_COCKPIT) .
ENDCLASS.



CLASS ZCL_HCM_FCT_COCKPIT IMPLEMENTATION.


METHOD atualizacao_manual.

  DATA: lv_texto_livre TYPE baltmsg.

*  se a resposta ao popup não é positiva
  IF popup_atu_manual( ) NE '1'.
    EXIT.
  ENDIF.

*  percorre todos os registos selecionados
  LOOP AT ao_dados_alv->get_selections( )->get_selected_rows( ) ASSIGNING FIELD-SYMBOL(<fs_rows>).
    TRY.

*        valida ação
        CASE av_acao_global.
*          para segurança social
          WHEN ac_acao_ss.
            CLEAR: as_dados_ss.

*            obtem dados da tabela de SS
            READ TABLE at_dados_ss_alv
            ASSIGNING FIELD-SYMBOL(<fs_dados_alv_ss>)
            INDEX <fs_rows>.

            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING <fs_dados_alv_ss> TO as_dados_ss ##ENH_OK.

*              obtem dados da ultima modificação
              prep_ultimo_ss( iv_ssnum  = as_dados_ss-ssnum
                              iv_bukrs  = as_dados_ss-bukrs ).

*              atualiza tabela de controlo
              atualiza_bd_ss( iv_atu_manual = abap_true ).

              MESSAGE s012(zinetum_fct) INTO lv_texto_livre.

*              atualiza tabele de log
              atualiza_bd_ss_log_geral( lv_texto_livre ).

              CLEAR: as_dados_ss.

            ENDIF.

*          para as FCT
          WHEN ac_acao_fct.

*            obtem dados da tabela FCT
            READ TABLE at_dados_fct_alv
            ASSIGNING FIELD-SYMBOL(<fs_dados_alv_fct>)
            INDEX <fs_rows>.

            IF sy-subrc EQ 0.

              MOVE-CORRESPONDING <fs_dados_alv_fct> TO as_dados_fct ##ENH_OK.

*              obtem dados da ultima modificação
              prep_ultimo_fct(  iv_ssnum  = as_dados_fct-ssnum
                                iv_bukrs  = as_dados_fct-bukrs ).

*              valida ação
              CASE av_acao.
*                para admissão FCT
                WHEN ac_acao_admissao.

*                  obtem o ID do contrato via popup
                  DATA(lv_contrato) = popup_contrato( ).

*                  se não foi indicado o contrato
                  IF lv_contrato IS INITIAL.
*                    continua para o próximo registo
                    CONTINUE.
                  ENDIF.

*                  guarda informação do contrato recebido pelo popup
                  <fs_dados_alv_fct>-contrato = as_dados_fct-contrato = lv_contrato.

                  atualiza_bd( iv_atu_manual = abap_true ).

*                para modificação FCT
                WHEN ac_acao_modificacao.

                  CASE av_anul_acao.

*                    criar período de rendimento
                    WHEN abap_false.

                      atualiza_bd( iv_atu_manual = abap_true ).

*                    anular período de rendimento
                    WHEN abap_true.

                      remove_registo( iv_atu_manual = abap_true ).

                  ENDCASE.

                WHEN OTHERS.
                  atualiza_bd( iv_atu_manual = abap_true ).

              ENDCASE.

              MESSAGE s012(zinetum_fct) INTO lv_texto_livre.

*              atualiza tabele de log
              atualiza_bd_log_geral( lv_texto_livre ).

              CLEAR: as_dados_fct.
            ENDIF.

        ENDCASE.
      CATCH zcx_hcm_fct_cockpit ##NO_HANDLER. " Fundos de compensação de trabalho

    ENDTRY.

*    guarda dados das comunicações
    IF <fs_dados_alv_ss> IS ASSIGNED.
      <fs_dados_alv_ss>-icon     = av_icon.
      <fs_dados_alv_ss>-comm_msg = av_comm_msg.

    ELSEIF <fs_dados_alv_fct> IS ASSIGNED.
      <fs_dados_alv_fct>-icon     = av_icon.
      <fs_dados_alv_fct>-comm_msg = av_comm_msg.
    ENDIF.

  ENDLOOP.

*  refresh aos dados da alv.
  ao_dados_alv->refresh( ).

ENDMETHOD.


METHOD atualiza_bd.

  DATA: lv_dummy  TYPE string ##NEEDED.

  GET TIME.

  as_dados_fct-seq        = as_dados_fct_ultimo-seq + 1.
  as_dados_fct-aedtm      = sy-datum.
  as_dados_fct-aenuhr     = sy-uzeit.
  as_dados_fct-uname      = sy-uname.
  as_dados_fct-atu_manual = iv_atu_manual.

  CONVERT DATE sy-datum
          TIME sy-uzeit
INTO TIME STAMP as_dados_fct-timestamp
     TIME ZONE sy-zonlo.

  MODIFY zhcm_tfct_log FROM as_dados_fct.

  IF sy-subrc EQ 0 AND iv_exec_ficheiro IS INITIAL AND iv_atu_manual IS INITIAL.
    MESSAGE s001(zinetum_fct) INTO lv_dummy.

*  execução por ficheiro
  ELSEIF sy-subrc EQ 0 AND iv_exec_ficheiro IS NOT INITIAL AND iv_atu_manual IS INITIAL.
    MESSAGE s003(zinetum_fct) INTO lv_dummy.

*  atualização manual
  ELSEIF sy-subrc EQ 0 AND iv_exec_ficheiro IS INITIAL AND iv_atu_manual IS NOT INITIAL.
    MESSAGE s012(zinetum_fct) INTO lv_dummy.

  ENDIF.

  escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

ENDMETHOD.


METHOD atualiza_bd_log_geral.

  DATA: lv_dummy         TYPE string ##NEEDED,
        ls_dados_fct_lcm TYPE zhcm_tfct_lcm.

  MOVE-CORRESPONDING as_dados_fct TO ls_dados_fct_lcm.

*  obtém último número de sequência
  SELECT MAX( seq )
    FROM zhcm_tfct_lcm
    INTO ls_dados_fct_lcm-seq
    WHERE ssnum EQ ls_dados_fct_lcm-ssnum  AND
          pernr EQ ls_dados_fct_lcm-pernr  AND
          bukrs EQ ls_dados_fct_lcm-bukrs.

  ls_dados_fct_lcm-seq    = ls_dados_fct_lcm-seq + 1.
  ls_dados_fct_lcm-aedtm  = sy-datum.
  ls_dados_fct_lcm-aenuhr = sy-uzeit.
  ls_dados_fct_lcm-uname  = sy-uname.
  ls_dados_fct_lcm-text   = iv_texto_livre.

  CONVERT DATE sy-datum
          TIME sy-uzeit
INTO TIME STAMP ls_dados_fct_lcm-timestamp
     TIME ZONE sy-zonlo.

  MODIFY zhcm_tfct_lcm FROM ls_dados_fct_lcm.

ENDMETHOD.


METHOD atualiza_bd_ss.

  DATA: lv_dummy  TYPE string ##NEEDED.

  GET TIME.

  as_dados_ss-seq         = as_dados_ss_ultimo-seq + 1.
  as_dados_ss-aedtm       = sy-datum.
  as_dados_ss-aenuhr      = sy-uzeit.
  as_dados_ss-uname       = sy-uname.
  as_dados_ss-atu_manual  = iv_atu_manual.

  CONVERT DATE sy-datum
          TIME sy-uzeit
INTO TIME STAMP as_dados_ss-timestamp
     TIME ZONE sy-zonlo.

  MODIFY zhcm_tfct_ss_log FROM as_dados_ss.

  IF sy-subrc EQ 0 AND iv_exec_ficheiro IS INITIAL AND iv_atu_manual IS INITIAL.
    MESSAGE s001(zinetum_fct) INTO lv_dummy.

*  execução por ficheiro
  ELSEIF sy-subrc EQ 0 AND iv_exec_ficheiro IS NOT INITIAL AND iv_atu_manual IS INITIAL.
    MESSAGE s003(zinetum_fct) INTO lv_dummy.

*  atualização manual
  ELSEIF sy-subrc EQ 0 AND iv_exec_ficheiro IS INITIAL AND iv_atu_manual IS NOT INITIAL.
    MESSAGE s012(zinetum_fct) INTO lv_dummy.

  ENDIF.

  escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

ENDMETHOD.


METHOD atualiza_bd_ss_log_geral.

  DATA: lv_dummy        TYPE string ##NEEDED,
        ls_dados_ss_lcm TYPE zhcm_tfct_ss_lcm.

  MOVE-CORRESPONDING as_dados_ss TO ls_dados_ss_lcm.

*  obtém último número de sequência
  SELECT MAX( seq )
    FROM zhcm_tfct_ss_lcm
    INTO ls_dados_ss_lcm-seq
    WHERE ssnum EQ ls_dados_ss_lcm-ssnum  AND
          pernr EQ ls_dados_ss_lcm-pernr  AND
          bukrs EQ ls_dados_ss_lcm-bukrs.

  ls_dados_ss_lcm-seq     = ls_dados_ss_lcm-seq + 1.
  ls_dados_ss_lcm-aedtm   = sy-datum.
  ls_dados_ss_lcm-aenuhr  = sy-uzeit.
  ls_dados_ss_lcm-uname   = sy-uname.
  ls_dados_ss_lcm-text    = iv_texto_livre.

  CONVERT DATE sy-datum
          TIME sy-uzeit
INTO TIME STAMP ls_dados_ss_lcm-timestamp
     TIME ZONE sy-zonlo.

  MODIFY zhcm_tfct_ss_lcm FROM ls_dados_ss_lcm.

ENDMETHOD.


METHOD atualiza_registo_admi.

  DATA: lv_dummy       TYPE string ##NEEDED,
        ls_input       TYPE zinetumfctadmitir_trabalhador2,
        ls_output      TYPE zinetumfctadmitir_trabalhador1,
        lv_texto_livre TYPE baltmsg.

  TRY.

*      obtem porta lógica para a empresa correspondente
      READ TABLE at_portas_logicas
      ASSIGNING FIELD-SYMBOL(<fs_porta_logica>)
      BINARY SEARCH
      WITH KEY bukrs  = as_dados_fct-bukrs.

      IF sy-subrc NE 0.
        MESSAGE e008(zinetum_fct) WITH as_dados_fct-bukrs INTO lv_dummy.

        escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
      ENDIF.

      DATA(lo_ws_admi) = NEW zinetumfctco_admitir_trabalhad( <fs_porta_logica>-admi ).

      ls_input-parameters-dados_admissao-niss                         = as_dados_fct-ssnum.
      ls_input-parameters-dados_admissao-modalidade_contrato_trabalho = as_dados_fct-modalidade.
      ls_input-parameters-dados_admissao-data_inicio_contrato         = converte_data( as_dados_fct-ctbdt ).
      ls_input-parameters-dados_admissao-data_fim_contrato            = converte_data( as_dados_fct-ctedt ).
      ls_input-parameters-dados_admissao-retribuicao                  = as_dados_fct-retribuicao.
      ls_input-parameters-dados_admissao-diuturnidades                = as_dados_fct-diuturnidades.

*      envia dados
      lo_ws_admi->admitir_trabalhador(  EXPORTING input  = ls_input
                                        IMPORTING output = ls_output ).

*      guarda informação do contrato recebido pelo webservice
      as_dados_fct-contrato = ls_output-parameters-return.

      IF as_dados_fct-contrato IS INITIAL.
        MESSAGE e002(zinetum_fct) INTO lv_dummy.

        escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

        atualiza_bd_log_geral( lv_texto_livre ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
      ENDIF.

      atualiza_bd( ).

      MESSAGE s001(zinetum_fct) INTO lv_texto_livre.
      atualiza_bd_log_geral( lv_texto_livre ).

    CATCH cx_ai_system_fault INTO DATA(lo_fault).         "
*      obtem erro recebido
      lv_texto_livre = lo_fault->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

*    erros de dados enviados
    CATCH zinetumfctcx_gfctexception INTO DATA(lo_gfct). " Proxy Class (generated)

*      obtem erro recebido
      lv_texto_livre = |{ lo_gfct->fault-code } { lo_gfct->fault-desc }|.

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_bs_soa_exception INTO DATA(lo_soa). " Erro de comunicação
*      obtem erro recebido
      lv_texto_livre = lo_soa->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDTRY.

ENDMETHOD.


METHOD atualiza_registo_alt_per.

  DATA: lv_dummy       TYPE string ##NEEDED,
        ls_input       TYPE zinetumfctregistar_periodo_re4,
        ls_output      TYPE zinetumfctregistar_periodo_re3,
        lv_texto_livre TYPE baltmsg.

  TRY.

*      obtem porta lógica para a empresa correspondente
      READ TABLE at_portas_logicas
      ASSIGNING FIELD-SYMBOL(<fs_porta_logica>)
      BINARY SEARCH
      WITH KEY bukrs  = as_dados_ss-bukrs.

      IF sy-subrc NE 0.
        MESSAGE e008(zinetum_fct) WITH as_dados_ss-bukrs INTO lv_dummy.

        escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
      ENDIF.

      DATA(lo_ws_vinc) = NEW zinetumfctco_registar_periodo( <fs_porta_logica>-ss_alt_per ).

      ls_input-parameters-niss_trabalhador  = as_dados_ss-ssnum.
      ls_input-parameters-inicio_periodo    = converte_data( as_dados_ss-ctbdt ).
      ls_input-parameters-remuneracao_base  = as_dados_ss-remun_base.
      ls_input-parameters-diuturnidades     = as_dados_ss-diuturnidades.

*      envia dados
      lo_ws_vinc->registar_periodo_rendimento(  EXPORTING input   = ls_input
                                                IMPORTING output  = ls_output ).

*      em caso de sucesso
      IF ls_output-result-codigo_resultado EQ '1'.
        atualiza_bd_ss( ).

        MESSAGE s001(zinetum_fct) INTO lv_texto_livre.
        atualiza_bd_ss_log_geral( lv_texto_livre ).

      ELSE.
*        obtem erro recebido
        lv_texto_livre = ls_output-result-mensagens_erro.

        escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                          iv_texto_livre        = lv_texto_livre ).

        atualiza_bd_ss_log_geral( lv_texto_livre ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

      ENDIF.

    CATCH cx_ai_system_fault INTO DATA(lo_fault).         "
*      obtem erro recebido
      lv_texto_livre = lo_fault->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

*    erros de dados enviados
    CATCH zinetumfctcx_registar_periodo INTO DATA(lo_gfct). " Proxy Class (generated)
*      obtem erro recebido
      lv_texto_livre = VALUE #( lo_gfct->registar_periodo_rendimento_ex-mensagens-erro[ 1 ] OPTIONAL ).
      lv_texto_livre = |{ lo_gfct->registar_periodo_rendimento_ex-codigo_resultado } { lv_texto_livre }|.

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_bs_soa_exception INTO DATA(lo_soa). " Erro de comunicação
*      obtem erro recebido
      lv_texto_livre = lo_soa->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_ai_application_fault INTO DATA(lo_appl).
*      obtem erro recebido
      lv_texto_livre = lo_appl->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDTRY.

ENDMETHOD.


METHOD atualiza_registo_cess.

  DATA: ls_input       TYPE zinetumfctcessar_contrato1,
        ls_output      TYPE zinetumfctcessar_contrato_res1 ##NEEDED,
        lv_texto_livre TYPE baltmsg.

  TRY.

*      obtem porta lógica para a empresa correspondente
      READ TABLE at_portas_logicas
      ASSIGNING FIELD-SYMBOL(<fs_porta_logica>)
      BINARY SEARCH
      WITH KEY bukrs  = as_dados_fct-bukrs.

      IF sy-subrc NE 0.
        MESSAGE e008(zinetum_fct) WITH as_dados_fct-bukrs INTO DATA(lv_dummy) ##NEEDED.

        escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
      ENDIF.


      DATA(lo_ws_cess) = NEW zinetumfctco_cessar_contrato( <fs_porta_logica>-cess ).

      ls_input-parameters-dados_contrato-identificador_contrato   = as_dados_fct-contrato.
      ls_input-parameters-dados_contrato-data_fim_contrato        = converte_data( as_dados_fct-ctedt ).
      ls_input-parameters-dados_contrato-motivo_cessacao_contrato = as_dados_fct-motivo_cess.

*      envia dados
      lo_ws_cess->cessar_contrato( EXPORTING input  = ls_input
                                   IMPORTING output = ls_output ).

      atualiza_bd( ).

      MESSAGE s001(zinetum_fct) INTO lv_texto_livre.
      atualiza_bd_log_geral( lv_texto_livre ).

    CATCH cx_ai_system_fault INTO DATA(lo_fault).         "
*      obtem erro recebido
      lv_texto_livre = lo_fault->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

*    erros de dados enviados
    CATCH zinetumfctcx_gfctexception INTO DATA(lo_gfct). " Proxy Class (generated)

*      obtem erro recebido
      lv_texto_livre = |{ lo_gfct->fault-code } { lo_gfct->fault-desc }|.

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_bs_soa_exception INTO DATA(lo_soa). " Erro de comunicação
*      obtem erro recebido
      lv_texto_livre = lo_soa->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

  ENDTRY.

ENDMETHOD.


METHOD atualiza_registo_cess_vin_ss.

  DATA: lv_dummy       TYPE string ##NEEDED,
        ls_input       TYPE zinetumfctcessar_vinculo_sei_1,
        ls_output      TYPE zinetumfctcessar_vinculo_sei_c,
        lv_texto_livre TYPE baltmsg.

  TRY.

*      obtem porta lógica para a empresa correspondente
      READ TABLE at_portas_logicas
      ASSIGNING FIELD-SYMBOL(<fs_porta_logica>)
      BINARY SEARCH
      WITH KEY bukrs  = as_dados_ss-bukrs.

      IF sy-subrc NE 0.
        MESSAGE e008(zinetum_fct) WITH as_dados_ss-bukrs INTO lv_dummy.

        escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
      ENDIF.

      DATA(lo_ws_vinc) = NEW zinetumfctco_cessar_vinculo_se( <fs_porta_logica>-ss_cess ).

      ls_input-parameters-niss_trabalhador        = as_dados_ss-ssnum.
      ls_input-parameters-data_fim_vinculo        = converte_data( as_dados_ss-ctedt ).
      ls_input-parameters-motivo_fim_vinculo      = as_dados_ss-motivo.
      ls_input-parameters-comunicacao_desemprego  = as_dados_ss-com_desmp.
      ls_input-parameters-fundamentacao           = as_dados_ss-fundmt.

*      envia dados
      lo_ws_vinc->cessar_vinculo( EXPORTING input   = ls_input
                                  IMPORTING output  = ls_output ).

*      em caso de sucesso
      IF ls_output-result-codigo_resultado EQ '1'.
        atualiza_bd_ss( ).

        MESSAGE s001(zinetum_fct) INTO lv_texto_livre.
        atualiza_bd_ss_log_geral( lv_texto_livre ).

      ELSE.
*        obtem erro recebido
        lv_texto_livre = ls_output-result-mensagens_erro.

        escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                          iv_texto_livre        = lv_texto_livre ).

        atualiza_bd_ss_log_geral( lv_texto_livre ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

      ENDIF.

    CATCH cx_ai_system_fault INTO DATA(lo_fault).         "
*      obtem erro recebido
      lv_texto_livre = lo_fault->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

*    erros de dados enviados
    CATCH zinetumfctcx_cessar_vinculo_ex INTO DATA(lo_gfct). " Proxy Class (generated)
*      obtem erro recebido
      lv_texto_livre = VALUE #( lo_gfct->cessar_vinculo_exception-mensagens-erro[ 1 ] OPTIONAL ).
      lv_texto_livre = |{ lo_gfct->cessar_vinculo_exception-codigo_resultado } { lv_texto_livre }|.

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_bs_soa_exception INTO DATA(lo_soa). " Erro de comunicação
*      obtem erro recebido
      lv_texto_livre = lo_soa->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_ai_application_fault INTO DATA(lo_appl).
*      obtem erro recebido
      lv_texto_livre = lo_appl->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDTRY.

ENDMETHOD.


METHOD atualiza_registo_mod.

  DATA: ls_input       TYPE zinetumfctcriar_periodo_rendi2,
        ls_output      TYPE zinetumfctcriar_periodo_rendi1 ##NEEDED,
        lv_texto_livre TYPE baltmsg.

  TRY.

*      obtem porta lógica para a empresa correspondente
      READ TABLE at_portas_logicas
      ASSIGNING FIELD-SYMBOL(<fs_porta_logica>)
      BINARY SEARCH
      WITH KEY bukrs  = as_dados_fct-bukrs.

      IF sy-subrc NE 0.
        MESSAGE e008(zinetum_fct) WITH as_dados_fct-bukrs INTO DATA(lv_dummy) ##NEEDED.

        escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
      ENDIF.


      DATA(lo_ws_mod) = NEW zinetumfctco_alterar_contrato( <fs_porta_logica>-mod ).

      ls_input-parameters-dados_alteracao-identificador_contrato          = as_dados_fct-contrato.
      ls_input-parameters-dados_alteracao-data_inicio_periodo_rendimento  = converte_data( as_dados_fct-begda_peri_rend ).
      ls_input-parameters-dados_alteracao-montante_retribuicao_base       = as_dados_fct-retribuicao.
      ls_input-parameters-dados_alteracao-montante_diuturnidades_mensais  = as_dados_fct-diuturnidades.

      CASE av_anul_acao.

*        criar período de rendimento
        WHEN abap_false.
*          envia dados
          lo_ws_mod->criar_periodo_rendimento(  EXPORTING input  = ls_input
                                                IMPORTING output = ls_output ).

          atualiza_bd( ).

          MESSAGE s001(zinetum_fct) INTO lv_texto_livre.
          atualiza_bd_log_geral( lv_texto_livre ).

*        anular período de rendimento
        WHEN abap_true.
*          envia dados
          lo_ws_mod->remover_periodo_rendimento(  EXPORTING input  = ls_input
                                                  IMPORTING output = ls_output ) ##ENH_OK.

          remove_registo( ).

          MESSAGE s001(zinetum_fct) INTO lv_texto_livre.
          atualiza_bd_log_geral( lv_texto_livre ).

      ENDCASE.


    CATCH cx_ai_system_fault INTO DATA(lo_fault).         "
*      obtem erro recebido
      lv_texto_livre = lo_fault->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

*    erros de dados enviados
    CATCH zinetumfctcx_gfctexception INTO DATA(lo_gfct). " Proxy Class (generated)

*      obtem erro recebido
      lv_texto_livre = |{ lo_gfct->fault-code } { lo_gfct->fault-desc }|.

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_bs_soa_exception INTO DATA(lo_soa). " Erro de comunicação
*      obtem erro recebido
      lv_texto_livre = lo_soa->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

  ENDTRY.

ENDMETHOD.


METHOD atualiza_registo_mod_vin_ss.

  DATA: lv_dummy       TYPE string ##NEEDED,
        ls_input       TYPE zinetumfctalterar_contrato_tr4,
        ls_output      TYPE zinetumfctalterar_contrato_tr3,
        lv_texto_livre TYPE baltmsg.

  TRY.

*      obtem porta lógica para a empresa correspondente
      READ TABLE at_portas_logicas
      ASSIGNING FIELD-SYMBOL(<fs_porta_logica>)
      BINARY SEARCH
      WITH KEY bukrs  = as_dados_ss-bukrs.

      IF sy-subrc NE 0.
        MESSAGE e008(zinetum_fct) WITH as_dados_ss-bukrs INTO lv_dummy.

        escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
      ENDIF.

      DATA(lo_ws_vinc) = NEW zinetumfctco_alterar_contrato1( <fs_porta_logica>-ss_mod ).

      ls_input-parameters-niss_trabalhador            = as_dados_ss-ssnum.
      ls_input-parameters-modalidade_contrato         = as_dados_ss-modalidade.
      ls_input-parameters-prestacao_trabalho          = as_dados_ss-prest_trab.
      ls_input-parameters-inicio_contrato             = converte_data( as_dados_ss-ctbdt ).
      ls_input-parameters-fim_contrato                = converte_data( as_dados_ss-ctedt ).
      ls_input-parameters-profissao                   = as_dados_ss-prcnp.
      ls_input-parameters-remuneracao_base            = as_dados_ss-remun_base.
      ls_input-parameters-diuturnidades               = as_dados_ss-diuturnidades.
      ls_input-parameters-percentagem_trabalho        = as_dados_ss-bsgrd.
      ls_input-parameters-horas_trabalho              = as_dados_ss-wostd.
      ls_input-parameters-dias_trabalho               = as_dados_ss-dias_trab.
      ls_input-parameters-motivo_contrato             = as_dados_ss-motivo.
      ls_input-parameters-niss_trabalhador_substituir = as_dados_ss-ssnum_subs.

*      envia dados
      lo_ws_vinc->alterar_contrato_trabalho(  EXPORTING input   = ls_input
                                              IMPORTING output  = ls_output ).

*      em caso de sucesso
      IF ls_output-result-codigo_resultado EQ '1'.
        atualiza_bd_ss( ).

        MESSAGE s001(zinetum_fct) INTO lv_texto_livre.
        atualiza_bd_ss_log_geral( lv_texto_livre ).

      ELSE.
*        obtem erro recebido
        lv_texto_livre = ls_output-result-mensagens_erro.

        escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                          iv_texto_livre        = lv_texto_livre ).

        atualiza_bd_ss_log_geral( lv_texto_livre ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

      ENDIF.

    CATCH cx_ai_system_fault INTO DATA(lo_fault).         "
*      obtem erro recebido
      lv_texto_livre = lo_fault->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

*    erros de dados enviados
    CATCH zinetumfctcx_alterar_contrato INTO DATA(lo_gfct). " Proxy Class (generated)
*      obtem erro recebido
      lv_texto_livre = VALUE #( lo_gfct->alterar_contrato_trabalho_exce-mensagens-erro[ 1 ] OPTIONAL ).
      lv_texto_livre = |{ lo_gfct->alterar_contrato_trabalho_exce-codigo_resultado } { lv_texto_livre }|.

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_bs_soa_exception INTO DATA(lo_soa). " Erro de comunicação
*      obtem erro recebido
      lv_texto_livre = lo_soa->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_ai_application_fault INTO DATA(lo_appl).
*      obtem erro recebido
      lv_texto_livre = lo_appl->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

  ENDTRY.

ENDMETHOD.


METHOD atualiza_registo_vinc_ss.

  DATA: lv_dummy       TYPE string ##NEEDED,
        ls_input       TYPE zinetumfctvinculo_sei_regista1,
        ls_output      TYPE zinetumfctvinculo_sei_registar,
        lv_texto_livre TYPE baltmsg.

  TRY.

*      obtem porta lógica para a empresa correspondente
      READ TABLE at_portas_logicas
      ASSIGNING FIELD-SYMBOL(<fs_porta_logica>)
      BINARY SEARCH
      WITH KEY bukrs  = as_dados_ss-bukrs.

      IF sy-subrc NE 0.
        MESSAGE e008(zinetum_fct) WITH as_dados_ss-bukrs INTO lv_dummy.

        escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
      ENDIF.

      DATA(lo_ws_vinc) = NEW zinetumfctco_vinculo_sei( <fs_porta_logica>-ss_vinc ).

      ls_input-parameters-niss_trabalhador            = as_dados_ss-ssnum.
      ls_input-parameters-data_nascimento             = converte_data( as_dados_ss-gbdat ).
      ls_input-parameters-modalidade_contrato         = as_dados_ss-modalidade.
      ls_input-parameters-prestacao_trabalho          = as_dados_ss-prest_trab.
      ls_input-parameters-inicio_contrato             = converte_data( as_dados_ss-ctbdt ).
      ls_input-parameters-fim_contrato                = converte_data( as_dados_ss-ctedt ).
      ls_input-parameters-profissao                   = as_dados_ss-prcnp.
      ls_input-parameters-remuneracao_base            = as_dados_ss-remun_base.
      ls_input-parameters-diuturnidades               = as_dados_ss-diuturnidades.
      ls_input-parameters-percentagem_trabalho        = as_dados_ss-bsgrd.
      ls_input-parameters-horas_trabalho              = as_dados_ss-wostd.
      ls_input-parameters-dias_trabalho               = as_dados_ss-dias_trab.
      ls_input-parameters-motivo_contrato             = as_dados_ss-motivo.
      ls_input-parameters-niss_trabalhador_substituir = as_dados_ss-ssnum_subs.
      ls_input-parameters-local_trabalho              = as_dados_ss-ssnsa.

*      envia dados
      lo_ws_vinc->registar_vinculo( EXPORTING input   = ls_input
                                    IMPORTING output  = ls_output ).

*      em caso de sucesso
      IF ls_output-result-codigo_resultado EQ '1'.
        atualiza_bd_ss( ).

        MESSAGE s001(zinetum_fct) INTO lv_texto_livre.
        atualiza_bd_ss_log_geral( lv_texto_livre ).

      ELSE.
*        obtem erro recebido
        lv_texto_livre = ls_output-result-mensagens_erro.

        escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                          iv_texto_livre        = lv_texto_livre ).

        atualiza_bd_ss_log_geral( lv_texto_livre ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

      ENDIF.

    CATCH cx_ai_system_fault INTO DATA(lo_fault).         "
*      obtem erro recebido
      lv_texto_livre = lo_fault->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

*    erros de dados enviados
    CATCH zinetumfctcx_vinculo_exception INTO DATA(lo_gfct). " Proxy Class (generated)
*      obtem erro recebido
      lv_texto_livre = VALUE #( lo_gfct->vinculo_exception-mensagens-erro[ 1 ] OPTIONAL ).
      lv_texto_livre = |{ lo_gfct->vinculo_exception-codigo_resultado } { lv_texto_livre }|.

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_bs_soa_exception INTO DATA(lo_soa). " Erro de comunicação
*      obtem erro recebido
      lv_texto_livre = lo_soa->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_ai_application_fault INTO DATA(lo_appl).
*      obtem erro recebido
      lv_texto_livre = lo_appl->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = obtem_campos_log( )
                        iv_texto_livre        = lv_texto_livre ).

      atualiza_bd_ss_log_geral( lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDTRY.

ENDMETHOD.


METHOD calc_dias_trab_anual.

  CALL FUNCTION 'HR_RO_WORKDAYS_IN_INTERVAL'
    EXPORTING
      begda   = as_dados_ss-ctbdt
      endda   = as_dados_ss-ctedt
      mofid   = av_fer_calend
    CHANGING
      wrkdays = rv_dias_uteis.

ENDMETHOD.


METHOD comunica_dados.

*  percorre todos os registos selecionados
  LOOP AT ao_dados_alv->get_selections( )->get_selected_rows( ) ASSIGNING FIELD-SYMBOL(<fs_rows>).
    TRY.

*        valida ação
        CASE av_acao_global.
*          para segurança social
          WHEN ac_acao_ss.
            CLEAR: as_dados_ss.

*            obtem dados da tabela de SS
            READ TABLE at_dados_ss_alv
            ASSIGNING FIELD-SYMBOL(<fs_dados_alv_ss>)
            INDEX <fs_rows>.

            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING <fs_dados_alv_ss> TO as_dados_ss ##ENH_OK.

*              obtem dados da ultima modificação
              prep_ultimo_ss( iv_ssnum  = as_dados_ss-ssnum
                              iv_bukrs  = as_dados_ss-bukrs ).

*              valida ação
              CASE <fs_dados_alv_ss>-acao.

*                vínculo do colaborador
                WHEN ac_acao_ss_vinculo.
                  atualiza_registo_vinc_ss( ).

*                modificação de vínculo
                WHEN ac_acao_ss_modificacao.
*                  envia dados e atualiza log
                  atualiza_registo_mod_vin_ss( ).

*                alteração de período de rendimento
                when ac_acao_ss_alt_per.
*                  envia dados e atualiza log
                  atualiza_registo_alt_per( ).

*                cessação de vínculo
                WHEN ac_acao_ss_cessacao.

*                  envia dados e atualiza log
                  atualiza_registo_cess_vin_ss( ).
              ENDCASE.

              CLEAR: as_dados_ss.

            ENDIF.

*          para as FCT
          WHEN ac_acao_fct.

*            obtem dados da tabela FCT
            READ TABLE at_dados_fct_alv
            ASSIGNING FIELD-SYMBOL(<fs_dados_alv_fct>)
            INDEX <fs_rows>.

            IF sy-subrc EQ 0.

              MOVE-CORRESPONDING <fs_dados_alv_fct> TO as_dados_fct ##ENH_OK.

*              obtem dados da ultima modificação
              prep_ultimo_fct(  iv_ssnum  = as_dados_fct-ssnum
                                iv_bukrs  = as_dados_fct-bukrs ).

*              valida ação
              CASE <fs_dados_alv_fct>-acao.
*                para admissão FCT
                WHEN ac_acao_admissao.
                  atualiza_registo_admi( ).

*                para cessação FCT
                WHEN ac_acao_cessacao.
                  atualiza_registo_cess( ).

*                para modificação FCT
                WHEN ac_acao_modificacao.
                  atualiza_registo_mod( ).

                WHEN OTHERS.
              ENDCASE.

              CLEAR: as_dados_fct.
            ENDIF.

        ENDCASE.
      CATCH zcx_hcm_fct_cockpit ##NO_HANDLER. " Fundos de compensação de trabalho

    ENDTRY.

*    guarda dados das comunicações
    IF <fs_dados_alv_ss> IS ASSIGNED.
      <fs_dados_alv_ss>-icon     = av_icon.
      <fs_dados_alv_ss>-comm_msg = av_comm_msg.

    ELSEIF <fs_dados_alv_fct> IS ASSIGNED.
      <fs_dados_alv_fct>-icon     = av_icon.
      <fs_dados_alv_fct>-comm_msg = av_comm_msg.
    ENDIF.

*    restaura campos de status de comunicação
    reset_stat_com( ).

  ENDLOOP.

*  refresh aos dados da alv.
  ao_dados_alv->refresh( ).

  MESSAGE s007(zinetum_fct).

ENDMETHOD.


METHOD constructor.

*  prepara ação
  prepara_acao( iv_acao ).

*  prepara configuração de portas lógicas
  prep_portas_logicas( ).

*  prepara duração mínima do contrato
  prep_dur_min_contrato( ).

*  restaura campos de status de comunicação
  reset_stat_com( ).

*  prepara timeout de comunicação
  prep_contr_timeout( ).

*  prepara descrição da ação
  prep_acao_desc( ).

ENDMETHOD.


METHOD consulta_contrato.

  DATA: lt_ssnum TYPE zcl_hcm_fct_cockpit=>ty_ssnum_tt,
        lv_endda TYPE endda,
        lv_begda TYPE begda.

  lv_endda = sy-datum.
  lv_begda = sy-datum - 90.

  CLEAR: at_dados_ss_alv_consc.

*  limpa log
  limpa_log( ).

*  percorre os registos selecionados
  LOOP AT ao_dados_alv->get_selections( )->get_selected_rows( ) ASSIGNING FIELD-SYMBOL(<fs_rows>).

    APPEND VALUE #( ssnum = at_dados_ss_alv[ <fs_rows> ]-ssnum pernr = at_dados_ss_alv[ <fs_rows> ]-pernr bukrs = at_dados_ss_alv[ <fs_rows> ]-bukrs ) TO lt_ssnum.

  ENDLOOP.

  SORT lt_ssnum BY pernr bukrs ASCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_ssnum.

  TRY.
      processa_ss_contratos(  iv_begda  = lv_begda      " Início da validade
                              iv_endda  = lv_endda      " Fim da validade
                              it_ssnum  = lt_ssnum ).


    CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho
*    indicar que se deve analisar o log
      MESSAGE s006(zinetum_fct) DISPLAY LIKE 'W'.
  ENDTRY.

*  mostra ALV popup
  mostra_avl_consc_pop( ).

ENDMETHOD.


METHOD consulta_log.

  DATA: lt_log_handle TYPE bal_t_logh,
        lt_log_header TYPE balhdr_t,
        ls_log_filter TYPE bal_s_lfil.

  ls_log_filter-object = VALUE #( ( sign = 'I' option = 'EQ'  low = ac_log_obj high = '' ) ).

  IF iv_subobject IS NOT INITIAL.
    ls_log_filter-subobject = VALUE #( ( sign = 'I' option = 'EQ'  low = iv_subobject high = '' ) ).
  ENDIF.

  ls_log_filter-aldate = VALUE #( ( sign = 'I' option = 'BT'  low = iv_aldate_i high = iv_aldate_f ) ).
  ls_log_filter-altime = VALUE #( ( sign = 'I' option = 'BT'  low = iv_altime_i high = iv_altime_f ) ).

  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_s_log_filter     = ls_log_filter
    IMPORTING
      e_t_log_header     = lt_log_header
    EXCEPTIONS
      log_not_found      = 1
      no_filter_criteria = 2.

  IF sy-subrc NE 0 AND sy-msgty IS NOT INITIAL.

    IF sy-msgno EQ 208 ##NUMBER_OK.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    RETURN.
  ENDIF.

  IF lt_log_header IS INITIAL.
    RETURN.
  ENDIF.

*  obtem handle da base de dados
  CALL FUNCTION 'BAL_DB_LOAD'
    EXPORTING
      i_t_log_header     = lt_log_header
    IMPORTING
      e_t_log_handle     = lt_log_handle
    EXCEPTIONS
      no_logs_specified  = 1
      log_not_found      = 2
      log_already_loaded = 3.

  IF sy-subrc NE 0 AND sy-msgty IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  exibi o log
  mostra_log( lt_log_handle ).

ENDMETHOD.


METHOD converte_data.

  CHECK iv_data IS NOT INITIAL.

  DATA(lv_ano) = iv_data(4).
  DATA(lv_mes) = iv_data+4(2).
  DATA(lv_dia) = iv_data+6(2).

  CASE av_acao_global.
    WHEN ac_acao_ss.
      rv_data = |{ lv_ano }-{ lv_mes }-{ lv_dia }|.
    WHEN ac_acao_fct.
      rv_data = |{ lv_ano }-{ lv_mes }-{ lv_dia }T00:00:00Z|.
  ENDCASE.

ENDMETHOD.


METHOD converte_data_ficheiro.

  DATA(lv_ano) = iv_data+6(4).
  DATA(lv_mes) = iv_data+3(2).
  DATA(lv_dia) = iv_data(2).

  rv_data = |{ lv_ano }{ lv_mes }{ lv_dia }|.

ENDMETHOD.


METHOD escreve_msg_log.

  DATA: ls_msg      TYPE bal_s_msg,
        ls_contexto TYPE bal_s_cont.

  IF iv_texto_livre IS INITIAL.

    IF sy-msgty IS INITIAL.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING sy TO ls_msg.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO av_comm_msg.

    CASE sy-msgty.
      WHEN 'S'.
        av_icon = ac_light_green.
      WHEN 'W'.
        av_icon = ac_light_yellow.
      WHEN 'E'.
        av_icon = ac_light_red.
    ENDCASE.

    ls_msg-context-value    = is_campos_adicionais.
    ls_msg-context-tabname  = 'ZHCM_SFCT_LOG_STRUCT'.

* Adicionar a mensagem ao log
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle  = av_log_handle
        i_s_msg       = ls_msg
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.
    IF sy-subrc NE 0 AND sy-msgty IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*  para texto livre
  ELSE.

    ls_contexto-value   = is_campos_adicionais.
    ls_contexto-tabname = 'ZHCM_SFCT_LOG_STRUCT'.

    av_icon     = ac_light_red.
    av_comm_msg = iv_texto_livre.

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = av_log_handle
        i_msgty          = 'E'
        i_text           = iv_texto_livre
        i_s_context      = ls_contexto
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3.
    IF sy-subrc NE 0 AND sy-msgty IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD guarda_dados_alv.

  DATA: ls_dados_fct_alv LIKE LINE OF at_dados_fct_alv,
        ls_dados_ss_alv  LIKE LINE OF at_dados_ss_alv.

  CASE av_acao_global.
    WHEN ac_acao_fct.
      IF as_dados_fct IS INITIAL.
        RETURN.
      ENDIF.

      MOVE-CORRESPONDING as_dados_fct TO ls_dados_fct_alv ##ENH_OK.

      ls_dados_fct_alv-icon     = av_icon.
      ls_dados_fct_alv-comm_msg = av_comm_msg.

      INSERT ls_dados_fct_alv INTO TABLE at_dados_fct_alv.

    WHEN ac_acao_ss.

      IF as_dados_ss IS INITIAL.
        RETURN.
      ENDIF.

      MOVE-CORRESPONDING as_dados_ss TO ls_dados_ss_alv ##ENH_OK.

      ls_dados_ss_alv-icon      = av_icon.
      ls_dados_ss_alv-comm_msg  = av_comm_msg.

      INSERT ls_dados_ss_alv INTO TABLE at_dados_ss_alv.

    WHEN OTHERS.
  ENDCASE.

*  restaura campos de status de comunicação
  reset_stat_com( ).

ENDMETHOD.


METHOD guarda_dados_alv_consc.

  INSERT as_dados_ss_alv_consc INTO TABLE at_dados_ss_alv_consc.

ENDMETHOD.


METHOD guarda_log.

  DATA: lt_log_handle  TYPE bal_t_logh.

  INSERT av_log_handle INTO TABLE lt_log_handle.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3.

  IF sy-subrc NE 0 AND sy-msgty IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD integra_fct_fich_admi.

  CONSTANTS: lc_bcol TYPE i VALUE 1,
             lc_brow TYPE i VALUE 2,
             lc_ecol TYPE i VALUE 14,
             lc_erow TYPE i VALUE 65536.

  DATA: lt_excel TYPE STANDARD TABLE OF alsmex_tabline,
        ls_excel LIKE LINE OF lt_excel.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = iv_nome_ficheiro
      i_begin_col             = lc_bcol
      i_begin_row             = lc_brow
      i_end_col               = lc_ecol
      i_end_row               = lc_erow
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc NE 0 AND sy-msgty IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_excel INTO ls_excel WHERE row > 0.

    CASE ls_excel-col.

*      Nº Beneficiário SS
      WHEN 1 ##NUMBER_OK.
        as_dados_fct-ssnum  = ls_excel-value.

*      Nº pessoal
      WHEN 2 ##NUMBER_OK.
        as_dados_fct-pernr = ls_excel-value.

*      Empresa
      WHEN 3 ##NUMBER_OK.
        as_dados_fct-bukrs = ls_excel-value.

*      Modalidade de contrato de trabalho
      WHEN 4 ##NUMBER_OK.
        as_dados_fct-modalidade = ls_excel-value.

*      Início de contrato
      WHEN 5 ##NUMBER_OK.
        as_dados_fct-ctbdt = converte_data_ficheiro( ls_excel-value ).

*      Fim do contrato
      WHEN 6 ##NUMBER_OK.
        as_dados_fct-ctedt = converte_data_ficheiro( ls_excel-value ).

*      Remuneração-base
      WHEN 7 ##NUMBER_OK.
        REPLACE ',' WITH '.' INTO ls_excel-value.
        as_dados_fct-retribuicao = ls_excel-value.

*      Diuturnidades
      WHEN 8 ##NUMBER_OK.
        REPLACE ',' WITH '.' INTO ls_excel-value.
        as_dados_fct-diuturnidades  = ls_excel-value.

*      Identificador de contrato
      WHEN 9 ##NUMBER_OK.
        as_dados_fct-contrato = ls_excel-value.

*      Status do colaborador
      WHEN 10 ##NUMBER_OK.
        as_dados_fct-status = ls_excel-value.

*      Motivo de contrato de trabalho SS
      WHEN 11 ##NUMBER_OK.
        as_dados_fct-motivo_cess = ls_excel-value.

*      Início período de rendimento
      WHEN 12 ##NUMBER_OK.
        as_dados_fct-begda_peri_rend = converte_data_ficheiro( ls_excel-value ).

    ENDCASE.
*
    AT END OF row ##LOOP_AT_OK.

*      obtem registo do colaborador
      prep_ultimo_fct(  iv_ssnum  = as_dados_fct-ssnum
                        iv_bukrs  = as_dados_fct-bukrs ).

      TRY.
          valida_campos_obrg_fct_admi( ).

*      verifica ação
          CASE av_acao.
*        comunicar dados
            WHEN ac_acao_fct_fich_com_admi.
              TRY.

                  atualiza_registo_admi( ).

                CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho
                  ev_erro = abap_true.

                  CLEAR: as_dados_fct.
              ENDTRY.

*        atualizar log
            WHEN ac_acao_fct_fich_log.
*          apenas atualiza a base de dados. não os comunica
              atualiza_bd( iv_exec_ficheiro = abap_true ).
            WHEN OTHERS.
          ENDCASE.

        CATCH zcx_hcm_fct_cockpit.
          ev_erro = abap_true.

          CLEAR: as_dados_fct.
      ENDTRY.

      CLEAR: as_dados_fct.
    ENDAT.

  ENDLOOP.

ENDMETHOD.


METHOD integra_fct_fich_admi_serv.

  DATA: lv_string TYPE string,
        lv_char50 TYPE char50,
        lv_index  TYPE i.

*  abre ficheiro para leitura
  OPEN DATASET iv_nome_ficheiro FOR INPUT IN TEXT MODE ENCODING UTF-8.

  IF sy-subrc EQ 0.

    DO.

      lv_index = sy-index.

*      lê linha
      READ DATASET iv_nome_ficheiro INTO lv_string.

      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

*      não lê o cabeçalho
      CHECK lv_index NE 1.

*      separa os valores
      SPLIT lv_string AT ';' INTO TABLE DATA(lt_dados).

*      percorre os valores
      LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).
        CASE sy-tabix.
*      Nº Beneficiário SS
          WHEN 1 ##NUMBER_OK.
            as_dados_fct-ssnum  = <fs_dados>.

*      Nº pessoal
          WHEN 2 ##NUMBER_OK.
            as_dados_fct-pernr = <fs_dados>.

*      Empresa
          WHEN 3 ##NUMBER_OK.
            as_dados_fct-bukrs = <fs_dados>.

*      Modalidade de contrato de trabalho
          WHEN 4 ##NUMBER_OK.
            as_dados_fct-modalidade = <fs_dados>.

*      Início de contrato
          WHEN 5 ##NUMBER_OK.
            CLEAR: lv_char50.

            lv_char50 = <fs_dados>.
            as_dados_fct-ctbdt = converte_data_ficheiro( lv_char50 ).

*      Fim do contrato
          WHEN 6 ##NUMBER_OK.
            CLEAR: lv_char50.

            lv_char50 = <fs_dados>.
            as_dados_fct-ctedt = converte_data_ficheiro( lv_char50 ).

*      Remuneração-base
          WHEN 7 ##NUMBER_OK.
            REPLACE ',' WITH '.' INTO <fs_dados>.
            as_dados_fct-retribuicao = <fs_dados>.

*      Diuturnidades
          WHEN 8 ##NUMBER_OK.
            REPLACE ',' WITH '.' INTO <fs_dados>.
            as_dados_fct-diuturnidades  = <fs_dados>.

*      Identificador de contrato
          WHEN 9 ##NUMBER_OK.
            as_dados_fct-contrato = <fs_dados>.

*      Status do colaborador
          WHEN 10 ##NUMBER_OK.
            as_dados_fct-status = <fs_dados>.

*      Motivo de contrato de trabalho SS
          WHEN 11 ##NUMBER_OK.
            as_dados_fct-motivo_cess = <fs_dados>.

*      Início período de rendimento
          WHEN 12 ##NUMBER_OK.
            CLEAR: lv_char50.

            lv_char50 = <fs_dados>.
            as_dados_fct-begda_peri_rend = converte_data_ficheiro( lv_char50 ).

        ENDCASE.
      ENDLOOP.

*      obtem registo do colaborador
      prep_ultimo_fct(  iv_ssnum  = as_dados_fct-ssnum
                        iv_bukrs  = as_dados_fct-bukrs ).

      TRY.

          valida_campos_obrg_fct_admi( ).

          atualiza_registo_admi( ).

        CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho
          ev_erro = abap_true.
      ENDTRY.

      CLEAR: as_dados_fct.

    ENDDO.
  ENDIF.

ENDMETHOD.


METHOD integra_ss_fich_vinc.

  CONSTANTS: lc_bcol TYPE i VALUE 1,
             lc_brow TYPE i VALUE 2,
             lc_ecol TYPE i VALUE 21,
             lc_erow TYPE i VALUE 65536.

  DATA: lt_excel TYPE STANDARD TABLE OF alsmex_tabline,
        ls_excel LIKE LINE OF lt_excel.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = iv_nome_ficheiro
      i_begin_col             = lc_bcol
      i_begin_row             = lc_brow
      i_end_col               = lc_ecol
      i_end_row               = lc_erow
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc NE 0 AND sy-msgty IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_excel INTO ls_excel WHERE row > 0.

    CASE ls_excel-col.

*      Nº Beneficiário SS
      WHEN 1 ##NUMBER_OK.
        as_dados_ss-ssnum  = ls_excel-value.

*      Nº pessoal
      WHEN 2 ##NUMBER_OK.
        as_dados_ss-pernr = ls_excel-value.

*      Empresa
      WHEN 3 ##NUMBER_OK.
        as_dados_ss-bukrs = ls_excel-value.

*      data de nascimento
      WHEN 4 ##NUMBER_OK.
        as_dados_ss-gbdat = converte_data_ficheiro( ls_excel-value ).

*      Modalidade de contrato de trabalho
      WHEN 5 ##NUMBER_OK.
        as_dados_ss-modalidade = ls_excel-value.

*      Prestação de trabalho
      WHEN 6 ##NUMBER_OK.
        as_dados_ss-prest_trab = ls_excel-value.

*      Início de contrato
      WHEN 7 ##NUMBER_OK.
        as_dados_ss-ctbdt = converte_data_ficheiro( ls_excel-value ).

*      Fim do contrato
      WHEN 8 ##NUMBER_OK.
        as_dados_ss-ctedt = converte_data_ficheiro( ls_excel-value ).

*      início de período de rendimeno
      WHEN 9 ##NUMBER_OK.
        as_dados_ss-ctbdt = converte_data_ficheiro( ls_excel-value ).

*      Remuneração-base
      WHEN 10 ##NUMBER_OK.
        REPLACE ',' WITH '.' INTO ls_excel-value.
        as_dados_ss-remun_base = ls_excel-value.

*      Diuturnidades
      WHEN 11 ##NUMBER_OK.
        REPLACE ',' WITH '.' INTO ls_excel-value.
        as_dados_ss-diuturnidades  = ls_excel-value.

*      Classificação de Profissões (CNP ou CPP)
      WHEN 12 ##NUMBER_OK.
        as_dados_ss-prcnp = ls_excel-value.

*      Nível de utilização da capacidade
      WHEN 13 ##NUMBER_OK.
        as_dados_ss-bsgrd = ls_excel-value.

*      Horas semanais/anuais
      WHEN 14 ##NUMBER_OK.
        as_dados_ss-wostd = ls_excel-value.

*      Dias de trabalho
      WHEN 15 ##NUMBER_OK.
        as_dados_ss-dias_trab = ls_excel-value.

*      Motivo de contrato de trabalho SS
      WHEN 16 ##NUMBER_OK.
        as_dados_ss-motivo = ls_excel-value.

*      Nº Beneficiário SS Substituto
      WHEN 17 ##NUMBER_OK.
        as_dados_ss-ssnum_subs = ls_excel-value.

*      Código subárea Segurança Social
      WHEN 18 ##NUMBER_OK.
        as_dados_ss-ssnsa = ls_excel-value.

*      Comunicação de desemprego
      WHEN 19 ##NUMBER_OK.
        as_dados_ss-com_desmp = ls_excel-value.

*      Status do colaborador
      WHEN 20 ##NUMBER_OK.
        as_dados_ss-status = ls_excel-value.

*      Fundamentação
      WHEN 21 ##NUMBER_OK.
        as_dados_ss-fundmt = ls_excel-value.

    ENDCASE.
*
    AT END OF row ##LOOP_AT_OK.

*      obtem registo do colaborador
      prep_ultimo_ss( iv_ssnum  = as_dados_ss-ssnum
                      iv_bukrs  = as_dados_ss-bukrs ).

      TRY.
          valida_campos_obrg_ss_vinc( ).

*      verifica ação
          CASE av_acao.
*        comunicar dados
            WHEN ac_acao_ss_fich_com_vinc.
              TRY.

                  atualiza_registo_vinc_ss( ).

                CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho
                  ev_erro = abap_true.

                  CLEAR: as_dados_ss.
              ENDTRY.

*        atualizar log
            WHEN ac_acao_ss_fich_log.
*          apenas atualiza a base de dados. não os comunica
              atualiza_bd_ss( iv_exec_ficheiro = abap_true ).
            WHEN OTHERS.
          ENDCASE.

        CATCH zcx_hcm_fct_cockpit.
          ev_erro = abap_true.

          CLEAR: as_dados_ss.
      ENDTRY.

      CLEAR: as_dados_ss.
    ENDAT.

  ENDLOOP.

ENDMETHOD.


METHOD integra_ss_fich_vinc_serv.

  DATA: lv_string TYPE string,
        lv_char50 TYPE char50,
        lv_index  TYPE i.

*  abre ficheiro para leitura
  OPEN DATASET iv_nome_ficheiro FOR INPUT IN TEXT MODE ENCODING UTF-8.

  IF sy-subrc EQ 0.

    DO.

      lv_index = sy-index.

*      lê linha
      READ DATASET iv_nome_ficheiro INTO lv_string.

      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

*      não lê o cabeçalho
      CHECK lv_index NE 1.

*      separa os valores
      SPLIT lv_string AT ';' INTO TABLE DATA(lt_dados).

*      percorre os valores
      LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).
        CASE sy-tabix.
*      Nº Beneficiário SS
          WHEN 1 ##NUMBER_OK.
            as_dados_ss-ssnum  = <fs_dados>.

*      Nº pessoal
          WHEN 2 ##NUMBER_OK.
            as_dados_ss-pernr = <fs_dados>.

*      Empresa
          WHEN 3 ##NUMBER_OK.
            as_dados_ss-bukrs = <fs_dados>.

*      data de nascimento
          WHEN 4 ##NUMBER_OK.
            CLEAR: lv_char50.

            lv_char50 = <fs_dados>.
            as_dados_ss-gbdat = converte_data_ficheiro( lv_char50 ).

*      Modalidade de contrato de trabalho
          WHEN 5 ##NUMBER_OK.
            as_dados_ss-modalidade = <fs_dados>.

*      Prestação de trabalho
          WHEN 6 ##NUMBER_OK.
            as_dados_ss-prest_trab = <fs_dados>.

*      Início de contrato
          WHEN 7 ##NUMBER_OK.
            CLEAR: lv_char50.

            lv_char50 = <fs_dados>.
            as_dados_ss-ctbdt = converte_data_ficheiro( lv_char50 ).

*      Fim do contrato
          WHEN 8 ##NUMBER_OK.
            CLEAR: lv_char50.

            lv_char50 = <fs_dados>.
            as_dados_ss-ctedt = converte_data_ficheiro( lv_char50 ).

*      início de período de rendimeno
          WHEN 9 ##NUMBER_OK.
            CLEAR: lv_char50.

            lv_char50 = <fs_dados>.
            as_dados_ss-ctbdt = converte_data_ficheiro( lv_char50 ).

*      Remuneração-base
          WHEN 10 ##NUMBER_OK.
            REPLACE ',' WITH '.' INTO <fs_dados>.
            as_dados_ss-remun_base = <fs_dados>.

*      Diuturnidades
          WHEN 11 ##NUMBER_OK.
            REPLACE ',' WITH '.' INTO <fs_dados>.
            as_dados_ss-diuturnidades  = <fs_dados>.

*      Classificação de Profissões (CNP ou CPP)
          WHEN 12 ##NUMBER_OK.
            as_dados_ss-prcnp = <fs_dados>.

*      Nível de utilização da capacidade
          WHEN 13 ##NUMBER_OK.
            as_dados_ss-bsgrd = <fs_dados>.

*      Horas semanais/anuais
          WHEN 14 ##NUMBER_OK.
            as_dados_ss-wostd = <fs_dados>.

*      Dias de trabalho
          WHEN 15 ##NUMBER_OK.
            as_dados_ss-dias_trab = <fs_dados>.

*      Motivo de contrato de trabalho SS
          WHEN 16 ##NUMBER_OK.
            as_dados_ss-motivo = <fs_dados>.

*      Nº Beneficiário SS Substituto
          WHEN 17 ##NUMBER_OK.
            as_dados_ss-ssnum_subs = <fs_dados>.

*      Código subárea Segurança Social
          WHEN 18 ##NUMBER_OK.
            as_dados_ss-ssnsa = <fs_dados>.

*      Comunicação de desemprego
          WHEN 19 ##NUMBER_OK.
            as_dados_ss-com_desmp = <fs_dados>.

*      Status do colaborador
          WHEN 20 ##NUMBER_OK.
            as_dados_ss-status = <fs_dados>.

*      Fundamentação
          WHEN 21 ##NUMBER_OK.
            as_dados_ss-fundmt = <fs_dados>.

        ENDCASE.
      ENDLOOP.

*      obtem registo do colaborador
      prep_ultimo_ss( iv_ssnum  = as_dados_ss-ssnum
                      iv_bukrs  = as_dados_ss-bukrs ).

      TRY.

          valida_campos_obrg_ss_vinc( ).

          atualiza_registo_vinc_ss( ).

        CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho
          ev_erro = abap_true.
      ENDTRY.

      CLEAR: as_dados_ss.

    ENDDO.
  ENDIF.

ENDMETHOD.


METHOD limpa_log.

*  se foram encontrados dados, limpa log
  CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
    EXPORTING
      i_log_handle  = av_log_handle
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMETHOD.


METHOD mostra_alv.

  TRY.

      CASE av_acao_global.
        WHEN ac_acao_fct.
          cl_salv_table=>factory( IMPORTING r_salv_table = ao_dados_alv
                                  CHANGING  t_table      = at_dados_fct_alv ).

        WHEN ac_acao_ss.

*          para ação SS específica
          CASE av_acao.

*            para consulta de contrato
            WHEN ac_acao_ss_cons_cont.

              cl_salv_table=>factory( IMPORTING r_salv_table = ao_dados_alv
                                      CHANGING  t_table      = at_dados_ss_alv_consc ).

*            para as outras açoes
            WHEN OTHERS.

              cl_salv_table=>factory( IMPORTING r_salv_table = ao_dados_alv
                                      CHANGING  t_table      = at_dados_ss_alv ).
          ENDCASE.
        WHEN OTHERS.
          RETURN.
      ENDCASE.

    CATCH cx_salv_msg ##NO_HANDLER.
  ENDTRY.

*  prepara funções a exibir
  prep_func( ).

*  prepara colunas
  prep_colunas( ).

*  prepara layout
  prep_layout( ).

*  listagem zebra
  ao_dados_alv->get_display_settings( )->set_striped_pattern( cl_salv_display_settings=>true ).

*  mode de seleção
  ao_dados_alv->get_selections( )->set_selection_mode( value = if_salv_c_selection_mode=>row_column ).

*  prepara PF-STATUS
  prep_pfstatus( ).

*  prepara texto para TOP OF PAGE
  prep_top_of_page( ).

*  prepara ordenação
  prep_ordena( ).

  DATA(lo_events) = ao_dados_alv->get_event( ).

  SET HANDLER user_command FOR lo_events. " método da classe que chama ALV com evento CL_SALV_EVENTS_TABLE->ADDED_FUNCTION

  ao_dados_alv->display( ).

ENDMETHOD.


METHOD mostra_avl_consc_pop.

  DATA: ls_key  TYPE salv_s_layout_key.

  IF at_dados_ss_alv_consc IS INITIAL.
    RETURN.
  ENDIF.

  cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_dados_alv)
                          CHANGING  t_table      = at_dados_ss_alv_consc ).

  DATA(lo_functions) = lo_dados_alv->get_functions( ).
  lo_functions->set_default( abap_true ).
  lo_functions->set_export_wordprocessor( ).
  lo_functions->set_export_localfile( ).
  lo_functions->set_export_spreadsheet( ).

*  opções de layout
  DATA(lo_layout) = lo_dados_alv->get_layout( ).

  ls_key-report = sy-cprog.
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  DATA(lo_display)  = lo_dados_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

  DATA(lo_columns) = lo_dados_alv->get_columns( ).
  lo_columns->set_optimize( abap_true ).
  lo_columns->set_key_fixation( value = abap_true ).

  lo_dados_alv->set_screen_popup(
    EXPORTING
      start_column = 1
      end_column   = 200
      start_line   = 1
      end_line     = 10
  ).

  lo_dados_alv->display( ).

ENDMETHOD.


METHOD mostra_log.

  DATA: ls_display_profile TYPE bal_s_prof,
        ls_fcat            TYPE bal_s_fcat.

  CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
    IMPORTING
      e_s_display_profile = ls_display_profile.

  CLEAR ls_fcat.
  ls_fcat-ref_table = 'ZHCM_SFCT_LOG_STRUCT'.
  ls_fcat-ref_field = 'SSNUM'.
  ls_fcat-col_pos = 100 ##NUMBER_OK.
  APPEND ls_fcat TO ls_display_profile-mess_fcat.

  CLEAR ls_fcat.
  ls_fcat-ref_table = 'ZHCM_SFCT_LOG_STRUCT'.
  ls_fcat-ref_field = 'PERNR'.
  ls_fcat-col_pos = 99 ##NUMBER_OK.
  APPEND ls_fcat TO ls_display_profile-mess_fcat.

  CLEAR ls_fcat.
  ls_fcat-ref_table = 'ZHCM_SFCT_LOG_STRUCT'.
  ls_fcat-ref_field = 'BUKRS'.
  ls_fcat-col_pos = 98 ##NUMBER_OK.
  APPEND ls_fcat TO ls_display_profile-mess_fcat.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_t_log_handle       = it_log_handle
      i_s_display_profile  = ls_display_profile
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4.

  IF sy-subrc NE 0 AND sy-msgty IS NOT INITIAL.

    IF sy-msgno EQ 223 ##NUMBER_OK.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD mostra_log_online.

  DATA: lt_log_handle TYPE bal_t_logh.

  INSERT av_log_handle INTO TABLE lt_log_handle.

  mostra_log( lt_log_handle ).

ENDMETHOD.


METHOD obtem_campos_log.

  IF as_dados_fct-pernr IS NOT INITIAL.
    MOVE-CORRESPONDING as_dados_fct TO rs_log ##ENH_OK.
  ELSEIF as_dados_ss-pernr IS NOT INITIAL.
    MOVE-CORRESPONDING as_dados_ss TO rs_log ##ENH_OK.
  ENDIF.

ENDMETHOD.


METHOD obtem_data_ini_contrato.

  rv_ctbeg = COND ctbeg(  WHEN is_p0016-eindt IS NOT INITIAL THEN is_p0016-eindt
                          ELSE is_p0016-begda ).

ENDMETHOD.


METHOD obtem_excel_template.

  DATA: lo_ref_template    TYPE REF TO cl_bds_document_set,
        lo_ref_container   TYPE REF TO cl_gui_custom_container ##NEEDED,
        lo_ref_control     TYPE REF TO i_oi_container_control,
        lo_ref_error       TYPE REF TO i_oi_error,
        lo_ref_document    TYPE REF TO i_oi_document_proxy,
        lo_ref_spreadsheet TYPE REF TO i_oi_spreadsheet,
        lv_retcode         TYPE soi_ret_string.

  DATA: lt_signature TYPE sbdst_signature,
        ls_signature TYPE bapisignat,
        lt_uri       TYPE sbdst_uri,
        ls_uri       TYPE bapiuri,
        lt_sheet     TYPE soi_sheets_table,
        ls_sheet     TYPE soi_sheets.

  DATA: lt_fields   TYPE STANDARD TABLE OF rfc_fields,
        lv_last_row TYPE i,
        lv_last_col TYPE i.

  DATA: lt_excel TYPE STANDARD TABLE OF zhcm_tfct_ss_log.

  DATA: lv_name TYPE char12.

  c_oi_container_control_creator=>get_container_control(  IMPORTING control = lo_ref_control
                                                                    retcode = lv_retcode ).

  IF lv_retcode NE c_oi_errors=>ret_ok.
    RETURN.
  ENDIF.

  lo_ref_control->init_control( EXPORTING r3_application_name      = iv_r3_application_name ##NO_TEXT
                                          inplace_enabled          = abap_true
                                          inplace_scroll_documents = abap_true
                                          parent                   = lo_ref_container
                                IMPORTING retcode                  = lv_retcode ).

  IF lv_retcode NE c_oi_errors=>ret_ok.
    RETURN.
  ENDIF.

  CREATE OBJECT lo_ref_template.

  ls_signature-prop_name  = 'DESCRIPTION'.
  ls_signature-prop_value = iv_desc ##NO_TEXT.

  APPEND ls_signature TO lt_signature.

  CLEAR lt_uri.

  lo_ref_template->get_with_url(  EXPORTING   classname       = 'SOFFICEINTEGRATION'
                                              classtype       = 'OT'
                                              object_key      = 'SOFFICEINTEGRATION'
                                  CHANGING    uris            = lt_uri
                                              signature       = lt_signature
                                  EXCEPTIONS  nothing_found   = 1
                                              error_kpro      = 2
                                              internal_error  = 3
                                              parameter_error = 4
                                              not_authorized  = 5
                                              not_allowed     = 6 ).

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CLEAR ls_uri.

  READ TABLE lt_uri INTO ls_uri INDEX 1.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  lo_ref_control->get_document_proxy( EXPORTING document_type  = 'Excel.Sheet'
                                      IMPORTING document_proxy = lo_ref_document
                                                retcode        = lv_retcode ).

  IF lv_retcode NE c_oi_errors=>ret_ok.
    RETURN.
  ENDIF.

  lo_ref_document->open_document( EXPORTING document_url = ls_uri-uri
                                            open_inplace = abap_true
                                  IMPORTING retcode      = lv_retcode ).

  IF lv_retcode NE c_oi_errors=>ret_ok.
    RETURN.
  ENDIF.

  FREE lo_ref_error.

  lo_ref_document->get_spreadsheet_interface( IMPORTING error           = lo_ref_error
                                                        sheet_interface = lo_ref_spreadsheet ).

  lo_ref_spreadsheet->get_sheets( IMPORTING sheets = lt_sheet
                                            error  = lo_ref_error ).

  IF lo_ref_error->error_code NE c_oi_errors=>ret_ok.
    RETURN.
  ENDIF.

  CLEAR ls_sheet.

  READ TABLE lt_sheet INTO ls_sheet INDEX 1.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  lo_ref_spreadsheet->select_sheet( EXPORTING name  = ls_sheet-sheet_name
                                    IMPORTING error = lo_ref_error ).

  IF lo_ref_error->error_code NE c_oi_errors=>ret_ok.
    RETURN.
  ENDIF.

  CLEAR lt_fields.

  CALL FUNCTION 'DP_GET_FIELDS_FROM_TABLE'
    TABLES
      data   = lt_excel
      fields = lt_fields.

  lv_last_row = lines( lt_excel ).

  lv_last_col = lines( lt_fields ).

  lo_ref_spreadsheet->set_selection(  left    = 1
                                      top     = 2
                                      rows    = lv_last_row
                                      columns = lv_last_col ).

  lv_name = iv_desc.

  lo_ref_spreadsheet->insert_range( columns = lv_last_col
                                    rows    = lv_last_row
                                    name    = lv_name ).

  lo_ref_spreadsheet->insert_one_table( data_table   = lt_excel[]
                                        fields_table = lt_fields
                                        rangename    = lv_name ).

  lo_ref_document->save_as( file_name = 'C:\text.xlsx' ) ##NO_TEXT.

  lo_ref_document->release_document(  IMPORTING retcode = lv_retcode ).

  FREE: lo_ref_spreadsheet,
        lo_ref_document.

  lo_ref_control->release_all_documents( ).

  lo_ref_control->destroy_control( ).

ENDMETHOD.


METHOD obtem_sistema_it0302.

*  obtem indicação se o sistema usa o IT0302 como default de medidas
  SELECT SINGLE low
    FROM tvarvc
    INTO @DATA(lv_low)
    WHERE name  EQ 'ZFCT_IT0302'  AND
          type  EQ 'P'            AND
          numb  EQ '0000'.

  CHECK lv_low IS NOT INITIAL.

  rv_usa_it0302 = abap_true.

ENDMETHOD.


METHOD obtem_ss_contratos.

  DATA: lv_dummy              TYPE string ##NEEDED,
        ls_input_pesq         TYPE zinetumfctcontrato_sei_pesqui1,
        ls_output_pesq        TYPE zinetumfctcontrato_sei_pesquis,
        ls_input_dados        TYPE zinetumfctcontrato_sei_get_da1,
        ls_output_dados       TYPE zinetumfctcontrato_sei_get_dad,
        ls_trab               TYPE LINE OF zinetumfcttrabalhadorlst_n_tab,
        lv_texto_livre        TYPE baltmsg,
        lv_bukrs              TYPE bukrs,
        ls_campos_adicionais  TYPE zhcm_sfct_log_struct,
        lv_cons_contr_timeout TYPE i.

  CHECK it_ssnum IS NOT INITIAL.

  TRY.

      MESSAGE e011(zinetum_fct) WITH it_ssnum[ 1 ]-bukrs INTO lv_dummy.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = lv_dummy.

      CLEAR: lv_dummy.

      ls_campos_adicionais-bukrs = it_ssnum[ 1 ]-bukrs.

*        se só tiver um registo
      IF lines( it_ssnum ) EQ 1.
        ls_campos_adicionais-pernr = it_ssnum[ 1 ]-pernr.
        ls_campos_adicionais-ssnum = it_ssnum[ 1 ]-ssnum.
      ENDIF.

*      obtem porta lógica para a empresa correspondente
      READ TABLE at_portas_logicas
      ASSIGNING FIELD-SYMBOL(<fs_porta_logica>)
      BINARY SEARCH
      WITH KEY bukrs  = it_ssnum[ 1 ]-bukrs.

      IF sy-subrc NE 0.
        MESSAGE e008(zinetum_fct) WITH it_ssnum[ 1 ]-bukrs INTO lv_dummy.

        escreve_msg_log( is_campos_adicionais = ls_campos_adicionais ).

        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

      ENDIF.

      DATA(lo_ws_vinc) = NEW zinetumfctco_contrato_sei( <fs_porta_logica>-ss_consc ).

      ls_input_pesq-parameters-data_inicio  = converte_data( iv_begda ).
      ls_input_pesq-parameters-data_fim     = converte_data( iv_endda ).

*      percorre todos os SSNUM
      LOOP AT it_ssnum ASSIGNING FIELD-SYMBOL(<fs_ssnum>).
        ls_trab = <fs_ssnum>-ssnum.
        INSERT ls_trab INTO TABLE ls_input_pesq-parameters-trabalhadores-niss_trabalhador.

      ENDLOOP.

      SORT ls_input_pesq-parameters-trabalhadores-niss_trabalhador.

      DELETE ADJACENT DUPLICATES FROM ls_input_pesq-parameters-trabalhadores-niss_trabalhador COMPARING ALL FIELDS.

      lo_ws_vinc->pesquisa_contratos( EXPORTING input  = ls_input_pesq
                                      IMPORTING output = ls_output_pesq ).

      ls_input_dados-parameters-chave = ls_output_pesq-result-chave.

      ls_output_dados-result-result-resultado-codigo_resultado = '0'.

      WHILE ls_output_dados-result-result-resultado-codigo_resultado EQ '0' AND lv_cons_contr_timeout LE av_cons_contr_timeout.

        WAIT UP TO 1 SECONDS.

        CLEAR: ls_output_dados.

        lo_ws_vinc->get_dados_contratos(  EXPORTING input  = ls_input_dados
                                          IMPORTING output = ls_output_dados ).
*        em caso de pedido em processamento
        IF ls_output_dados-result-result-resultado-codigo_resultado EQ '0'.
          lv_cons_contr_timeout = lv_cons_contr_timeout + 1.

*        em caso de erro
        ELSEIF ls_output_dados-result-result-resultado-codigo_resultado NE '1'.
*        obtem erro recebido
          lv_texto_livre = ls_output_dados-result-result-resultado-mensagens_erro.

          escreve_msg_log(  is_campos_adicionais  = ls_campos_adicionais
                            iv_texto_livre        = lv_texto_livre ).

          RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

        ENDIF.

      ENDWHILE.

*      percorre os registos obtidos
      LOOP AT ls_output_dados-result-result-contratos-contrato ASSIGNING FIELD-SYMBOL(<fs_contratos>).
        CLEAR: as_dados_ss_alv_consc.

        as_dados_ss_alv_consc-ssnum = <fs_contratos>-niss_trabalhador.

        CONDENSE as_dados_ss_alv_consc-ssnum.

        READ TABLE it_ssnum
        ASSIGNING <fs_ssnum>
        WITH KEY ssnum = as_dados_ss_alv_consc-ssnum.

        IF sy-subrc EQ 0.
          as_dados_ss_alv_consc-pernr = <fs_ssnum>-pernr.
        ENDIF.

        as_dados_ss_alv_consc-bukrs         = it_ssnum[ 1 ]-bukrs.
        as_dados_ss_alv_consc-modalidade    = <fs_contratos>-modalidade_contrato.
        as_dados_ss_alv_consc-prest_trab    = <fs_contratos>-prestacao_trabalho.
        as_dados_ss_alv_consc-ctbdt         = <fs_contratos>-inicio_informacao_contrato.
        as_dados_ss_alv_consc-ctedt         = <fs_contratos>-fim_informacao_contrato.
        as_dados_ss_alv_consc-remun_base    = <fs_contratos>-remuneracao_base.
        as_dados_ss_alv_consc-diuturnidades = <fs_contratos>-diuturnidades.
        as_dados_ss_alv_consc-bsgrd         = <fs_contratos>-percentagem_trabalho.
        as_dados_ss_alv_consc-prcnp         = <fs_contratos>-profissao.
        as_dados_ss_alv_consc-wostd         = <fs_contratos>-horas_trabalho.
        as_dados_ss_alv_consc-dias_trab     = <fs_contratos>-dias_trabalho.
        as_dados_ss_alv_consc-motivo        = <fs_contratos>-motivo_contrato.
        as_dados_ss_alv_consc-ssnum_subs    = <fs_contratos>-niss_trabalhador_substituir.

*        guarda dados para ALV consulta contratro
        guarda_dados_alv_consc( ).

      ENDLOOP.

    CATCH cx_ai_system_fault INTO DATA(lo_fault).         "
*      obtem erro recebido
      lv_texto_livre = lo_fault->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = ls_campos_adicionais
                        iv_texto_livre        = lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

*    erros de dados enviados
    CATCH zinetumfctcx_contrato_exceptio INTO DATA(lo_gfct). " Proxy Class (generated)
*      obtem erro recebido
      lv_texto_livre = lo_gfct->contrato_exception-message.

      escreve_msg_log(  is_campos_adicionais  = ls_campos_adicionais
                        iv_texto_livre        = lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_bs_soa_exception INTO DATA(lo_soa). " Erro de comunicação
*      obtem erro recebido
      lv_texto_livre = lo_soa->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = ls_campos_adicionais
                        iv_texto_livre        = lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

    CATCH cx_ai_application_fault INTO DATA(lo_appl).
*      obtem erro recebido
      lv_texto_livre = lo_appl->get_text( ).

      escreve_msg_log(  is_campos_adicionais  = ls_campos_adicionais
                        iv_texto_livre        = lv_texto_livre ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDTRY.

ENDMETHOD.


METHOD obtem_tempo_parcial_conf.

*  obtem configuração de configuração de tempo parcial
  SELECT SINGLE low
    FROM tvarvc
    INTO rv_tempo_parcial_conf
    WHERE name EQ 'ZFCT_TEMPO_PARCIAL'  AND
          type EQ 'P'.

ENDMETHOD.


METHOD popup_atu_manual.

  DATA: lv_title  TYPE char200,
        lv_quest  TYPE char200.

  lv_title = 'Confirmação de atualização manual'(013).
  lv_quest = 'Tem a certeza que deseja atualizar manualmente?'(014).

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = lv_title
      text_question         = lv_quest
      text_button_1         = 'Sim'(015)
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'Não'(016)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = 'X'
    IMPORTING
      answer                = rv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMETHOD.


METHOD popup_contrato.

  DATA: lv_title  TYPE char200,
        lt_fields TYPE STANDARD TABLE OF sval,
        ls_fields LIKE LINE OF lt_fields.

  DATA(lv_pernr) = |{ as_dados_fct-pernr ALPHA = OUT }|.

  ls_fields-tabname   = 'ZHCM_TFCT_LOG'.
  ls_fields-fieldname = 'CONTRATO'.

  INSERT ls_fields INTO TABLE lt_fields.

  MESSAGE s013(zinetum_fct) WITH lv_pernr as_dados_fct-ssnum INTO lv_title.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title = lv_title
    TABLES
      fields      = lt_fields.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  rv_contrato = VALUE #( lt_fields[ 1 ]-value OPTIONAL ).

ENDMETHOD.


METHOD prepara_acao.

  av_acao = iv_acao.

*  prepara acção global e minimalista
  prepara_acao_glob_min( ).

ENDMETHOD.


METHOD prepara_acao_glob_min.

*  valida acção para ação global da execução
  CASE av_acao.
    WHEN  ac_acao_admissao              OR
          ac_acao_cessacao              OR
          ac_acao_modificacao           OR
          ac_acao_fct_fich_log          OR
          ac_acao_fct_fich_com_admi     OR
          ac_acao_fct_fich_com_admi_bck.

      av_acao_global  = ac_acao_fct.
      av_acao_min     = av_acao.

    WHEN  ac_acao_ss_vinculo            OR
          ac_acao_ss_modificacao        OR
          ac_acao_ss_cessacao           OR
          ac_acao_ss_alt_per            OR
          ac_acao_ss_fich_log           OR
          ac_acao_ss_fich_com_vinc      OR
          ac_acao_ss_fich_com_vinc_bck  OR
          ac_acao_ss_cons_cont.

      av_acao_global  = ac_acao_ss.

      CASE av_acao.
        WHEN ac_acao_ss_vinculo.
          av_acao_min = ac_acao_admissao.

        WHEN ac_acao_ss_modificacao OR ac_acao_ss_alt_per.
          av_acao_min = ac_acao_modificacao.

        WHEN ac_acao_ss_cessacao.
          av_acao_min = ac_acao_cessacao.

        WHEN OTHERS.
      ENDCASE.

    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.


METHOD prep_acao_desc.

*  obtem descrição da ação
  SELECT SINGLE ddtext
    FROM dd07t
    INTO av_acao_desc
    WHERE domname EQ 'ZHCM_FCT_ACAO'  AND
          ddlanguage  EQ sy-langu     AND
          as4local    EQ 'A'          AND
          as4vers     EQ '0000'       AND
          domvalue_l  EQ av_acao.

ENDMETHOD.


METHOD prep_anulacao_flag.

  av_anul_acao = iv_anul_acao.

ENDMETHOD.


METHOD PREP_CALENDARIO.

  CLEAR: av_fer_calend.

*  obtem calendário de férias
  SELECT SINGLE mofid
    FROM t001p
    INTO av_fer_calend
    WHERE werks EQ is_p0001-werks AND
          btrtl EQ is_p0001-btrtl.

  IF av_fer_calend IS INITIAL.
    MESSAGE e005(zinetum_fct) INTO DATA(lv_dummy) ##NEEDED.

    escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

    RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDIF.

ENDMETHOD.


METHOD prep_colunas.

  DATA(lo_columns) = ao_dados_alv->get_columns( ).
  lo_columns->set_optimize( abap_true ).
  lo_columns->set_key_fixation( value = abap_true ).

*    valida ação
  CASE av_acao.

*      para admissão FCT
    WHEN ac_acao_admissao.
      prep_colunas_fct_admin( CHANGING  co_columns = lo_columns ).

*      para cessação FCT
    WHEN ac_acao_cessacao.
      prep_colunas_fct_cess( CHANGING  co_columns = lo_columns ).

*      para modificação FCT
    WHEN ac_acao_modificacao.
      prep_colunas_fct_mod( CHANGING  co_columns = lo_columns ).

*      para vínculo segurança social
    WHEN ac_acao_ss_vinculo.
      prep_colunas_ss_vinc( CHANGING co_columns = lo_columns ).

*      para modificação de vínculo segurança social ou alteração de período de rendimento segurança social
    WHEN ac_acao_ss_modificacao OR ac_acao_ss_alt_per.
      prep_colunas_ss_mod( CHANGING  co_columns = lo_columns ).

*      para cessação de vínculo segurança social
    WHEN ac_acao_ss_cessacao.
      prep_colunas_ss_cess( CHANGING  co_columns = lo_columns ).

    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.


METHOD prep_colunas_fct_admin.

  TRY.
      DATA(lo_column) = co_columns->get_column( columnname = 'MOTIVO_CESS' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'BEGDA_PERI_REND' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO_DESC' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found ##NO_HANDLER.
  ENDTRY.

ENDMETHOD.


METHOD prep_colunas_fct_cess.

  TRY.
      DATA(lo_column) = co_columns->get_column( columnname = 'MODALIDADE' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'CTBDT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'RETRIBUICAO' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'DIUTURNIDADES' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'CONTRATO' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'BEGDA_PERI_REND' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO_DESC' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found ##NO_HANDLER.
  ENDTRY.

ENDMETHOD.


METHOD PREP_COLUNAS_FCT_MOD.

  TRY.
      DATA(lo_column) = co_columns->get_column( columnname = 'MODALIDADE' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'CTEDT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'MOTIVO_CESS' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO_DESC' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found ##NO_HANDLER.
  ENDTRY.

ENDMETHOD.


METHOD PREP_COLUNAS_SS_ALT_PER.

  TRY.
      DATA(lo_column) = co_columns->get_column( columnname = 'GBDAT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'MODALIDADE' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'CTBDT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'CTEDT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'PRCNP' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'BSGRD' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'WOSTD' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'DIAS_TRAB' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'MOTIVO' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'SSNUM_SUBS' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'SSNSA' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'PREST_TRAB' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'COM_DESMP' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'FUNDMT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found ##NO_HANDLER.
  ENDTRY.

ENDMETHOD.


METHOD PREP_COLUNAS_SS_CESS.

  TRY.
      DATA(lo_column) = co_columns->get_column( columnname = 'GBDAT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'MODALIDADE' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'CTBDT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'BEGDA_MED' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'PRCNP' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'BSGRD' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'WOSTD' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'DIAS_TRAB' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'REMUN_BASE' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'DIUTURNIDADES' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'SSNUM_SUBS' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'SSNSA' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'PREST_TRAB' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO_DESC' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found ##NO_HANDLER.
  ENDTRY.

ENDMETHOD.


METHOD prep_colunas_ss_mod.

  TRY.
      DATA(lo_column) = co_columns->get_column( columnname = 'BEGDA_MED' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'GBDAT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'SSNSA' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'COM_DESMP' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'FUNDMT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found ##NO_HANDLER.
  ENDTRY.

ENDMETHOD.


METHOD PREP_COLUNAS_SS_VINC.

  TRY.
      DATA(lo_column) = co_columns->get_column( columnname = 'BEGDA_MED' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'COM_DESMP' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'FUNDMT' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = co_columns->get_column( columnname = 'ACAO_DESC' ).

      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found ##NO_HANDLER.
  ENDTRY.

ENDMETHOD.


METHOD prep_contr_timeout.

*  obtem timeout para comunicação de consulta de contrato
  SELECT SINGLE low
    FROM tvarvc
    INTO @DATA(lv_low_timeout)
    WHERE name  EQ 'ZFCT_CONS_CONTR_TIMEOUT'  AND
          type  EQ 'P'                        AND
          numb  EQ '0000'.

  av_cons_contr_timeout = lv_low_timeout.

  IF av_cons_contr_timeout IS INITIAL.
    av_cons_contr_timeout = 10.
  ENDIF.

ENDMETHOD.


METHOD prep_dados_admi.

  DATA lo_badi TYPE REF TO zhcm_fct_badi.

  TRY.

      CLEAR: as_dados_fct.

      as_dados_fct-ssnum      = iv_ssnum.
      as_dados_fct-pernr      = iv_pernr.
      as_dados_fct-ctbdt      = iv_ctbdt.
      as_dados_fct-ctedt      = iv_ctedt.
      as_dados_fct-status     = ac_status_ativo.
      as_dados_fct-bukrs      = is_p0001-bukrs.
      as_dados_fct-acao       = av_acao.
      as_dados_fct-acao_desc  = av_acao_desc.

*      lógica de valores de rúbricas de retribuição e diuturnidades
      GET BADI lo_badi.

      CALL BADI lo_badi->prep_rub_ret_diu
        EXPORTING
          ir_ret           = ir_ret
          ir_diu           = ir_diu
          is_p0008         = is_p0008                 " Reg.mestre HR infotipo 0008 (Remuneração base)
          is_p0001         = is_p0001                 " Registro mestre HR: infotipo 0001 (atrib.org.)
          is_p0007         = is_p0007                 " Registro mestre pessoal: infotipo 0007 (tempo trab.teórico)
          iv_begda         = iv_begda
          iv_endda         = iv_endda
          iv_usa_it0052    = iv_usa_it0052
        IMPORTING
          ev_retribuicao   = as_dados_fct-retribuicao
          ev_diuturnidades = as_dados_fct-diuturnidades.

*          lógica de modalidade de trabalho
      CALL BADI lo_badi->prep_modalidade_contrato
        EXPORTING
          is_p0008         = is_p0008                 " Reg.mestre HR infotipo 0008 (Remuneração base)
          is_p0007         = is_p0007                 " Registro mestre pessoal: infotipo 0007 (tempo trab.teórico)
          iv_begda         = iv_begda                 " Início da validade
          iv_endda         = iv_endda                 " Fim da validade
          iv_acao_global   = av_acao_global                 " Acção
          iv_tipo_contrato = iv_modalidade                 " Tipo de contrato
          is_p0001         = is_p0001                 " Registro mestre HR: infotipo 0001 (atrib.org.)
        IMPORTING
          ev_calc_anual    = av_calc_anual                 " Cálculo anual
        CHANGING
          cs_dados_fct     = as_dados_fct.

    CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho

      escreve_msg_log( is_campos_adicionais  = obtem_campos_log( ) ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

  ENDTRY.

ENDMETHOD.


METHOD prep_dados_cess.

  CLEAR: as_dados_fct.

  as_dados_fct-ssnum      = as_dados_fct_ultimo-ssnum.
  as_dados_fct-contrato   = as_dados_fct_ultimo-contrato.
  as_dados_fct-bukrs      = as_dados_fct_ultimo-bukrs.
  as_dados_fct-pernr      = is_p0000-pernr.
  as_dados_fct-acao       = av_acao.
  as_dados_fct-acao_desc  = av_acao_desc.

*  obtem motivo para cessação de contrato
  prep_motivo( is_p0000 = is_p0000 ).

*  para cessar contrato
  IF av_anul_acao IS INITIAL.

*    coloca status inativo
    as_dados_fct-status = ac_status_desativo.
    as_dados_fct-ctedt  = is_p0000-begda - 1.

*  para anular cessação
  ELSE.

*    coloca status ativo
    as_dados_fct-status = ac_status_ativo.
  ENDIF.

ENDMETHOD.


METHOD prep_dados_mod.

  DATA lo_badi TYPE REF TO zhcm_fct_badi.

  CLEAR: as_dados_fct.

  as_dados_fct-ssnum            = as_dados_fct_ultimo-ssnum.
  as_dados_fct-contrato         = as_dados_fct_ultimo-contrato.
  as_dados_fct-bukrs            = as_dados_fct_ultimo-bukrs.
  as_dados_fct-ctbdt            = iv_ctbdt.
  as_dados_fct-begda_peri_rend  = iv_inic_peri_rend.
  as_dados_fct-pernr            = is_p0008-pernr.
  as_dados_fct-status           = ac_status_ativo.
  as_dados_fct-acao             = av_acao.
  as_dados_fct-acao_desc        = av_acao_desc.

*  se for uma anulação
  IF av_anul_acao IS NOT INITIAL.
    RETURN.
  ENDIF.

  CLEAR: as_dados_fct-retribuicao, as_dados_fct-diuturnidades.

*  lógica de valores de rúbricas de retribuição e diuturnidades
  GET BADI lo_badi.

  TRY.

      CALL BADI lo_badi->prep_rub_ret_diu
        EXPORTING
          ir_ret           = ir_ret
          ir_diu           = ir_diu
          is_p0008         = is_p0008                 " Reg.mestre HR infotipo 0008 (Remuneração base)
          is_p0001         = is_p0001                 " Registro mestre HR: infotipo 0001 (atrib.org.)
          is_p0007         = is_p0007                 " Registro mestre pessoal: infotipo 0007 (tempo trab.teórico)
          iv_begda         = iv_begda
          iv_endda         = iv_endda
          iv_usa_it0052    = iv_usa_it0052
        IMPORTING
          ev_retribuicao   = as_dados_fct-retribuicao
          ev_diuturnidades = as_dados_fct-diuturnidades.

    CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho

      escreve_msg_log( is_campos_adicionais  = obtem_campos_log( ) ).

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

  ENDTRY.

ENDMETHOD.


METHOD prep_dados_vinc_ss.

  DATA lo_badi TYPE REF TO zhcm_fct_badi.

  TRY .

      CLEAR: as_dados_ss.

      as_dados_ss-ssnum       = iv_ssnum.
      as_dados_ss-gbdat       = iv_gbdat.
      as_dados_ss-ctbdt       = iv_ctbdt.
      as_dados_ss-pernr       = is_p0000-pernr.
      as_dados_ss-ctedt       = iv_ctedt.
      as_dados_ss-prcnp       = iv_prcnp.
      as_dados_ss-ssnum_subs  = iv_ssnum_subs.
      as_dados_ss-bukrs       = is_p0001-bukrs.
      as_dados_ss-status      = ac_status_ativo.
      as_dados_ss-acao        = av_acao.
      as_dados_ss-acao_desc   = av_acao_desc.

      prep_local_trab(  is_p0001 = is_p0001
                        iv_begda = iv_ctbdt ).

      GET BADI lo_badi.

      TRY.

*          lógica de prestação de trabalho
          CALL BADI lo_badi->prep_prest_trab
            EXPORTING
              is_p0000      = is_p0000                 " Registro mestre HR infotipo 0000 (Medidas)
              iv_begda      = iv_begda
              iv_endda      = iv_endda
            IMPORTING
              ev_prest_trab = as_dados_ss-prest_trab.  " Prestação de trabalho


*          lógica de valores de rúbricas de retribuição e diuturnidades
          CALL BADI lo_badi->prep_rub_ret_diu
            EXPORTING
              ir_ret           = ir_ret
              ir_diu           = ir_diu
              is_p0008         = is_p0008                 " Reg.mestre HR infotipo 0008 (Remuneração base)
              is_p0001         = is_p0001                 " Registro mestre HR: infotipo 0001 (atrib.org.)
              is_p0007         = is_p0007                 " Registro mestre pessoal: infotipo 0007 (tempo trab.teórico)
              iv_begda         = iv_begda
              iv_endda         = iv_endda
              iv_usa_it0052    = iv_usa_it0052
            IMPORTING
              ev_retribuicao   = as_dados_ss-remun_base
              ev_diuturnidades = as_dados_ss-diuturnidades.

*          lógica de modalidade de trabalho
          CALL BADI lo_badi->prep_modalidade_contrato
            EXPORTING
              is_p0008         = is_p0008                 " Reg.mestre HR infotipo 0008 (Remuneração base)
              is_p0007         = is_p0007                 " Registro mestre pessoal: infotipo 0007 (tempo trab.teórico)
              iv_begda         = iv_begda                 " Início da validade
              iv_endda         = iv_endda                 " Fim da validade
              iv_acao_global   = av_acao_global                 " Acção
              iv_tipo_contrato = iv_modalidade                 " Tipo de contrato
              is_p0001         = is_p0001                 " Registro mestre HR: infotipo 0001 (atrib.org.)
            IMPORTING
              ev_calc_anual    = av_calc_anual                 " Cálculo anual
            CHANGING
              cs_dados_ss      = as_dados_ss.

*        lógica do nível de atualização
          CALL BADI lo_badi->prep_nivel_util
            EXPORTING
              is_p0008 = is_p0008                 " Reg.mestre HR infotipo 0008 (Remuneração base)
              is_p0007 = is_p0007                 " Registro mestre pessoal: infotipo 0007 (tempo trab.teórico)
              iv_begda = iv_begda                 " Início da validade
              iv_endda = iv_endda                 " Fim da validade
            IMPORTING
              ev_bsgrd = as_dados_ss-bsgrd.       " Nível de utilização da capacidade

*          prepara dias de trabalho
*          tem de ser após a preparação da modalidade de trabalho porque usa o atributo av_calc_anual
          prep_dias_trab( is_p0007  = is_p0007
                          is_p0001  = is_p0001 ).

          prep_horas_trab( is_p0007 ).

*          obtem motivo do contrato
          prep_motivo(  is_p0000 = is_p0000 ).

        CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho

          escreve_msg_log( is_campos_adicionais  = obtem_campos_log( ) ).

          RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

      ENDTRY.

    CATCH zcx_hcm_fct_cockpit.

      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

  ENDTRY.

ENDMETHOD.


METHOD prep_dados_vinc_ss_alt_per.

  DATA lo_badi TYPE REF TO zhcm_fct_badi.

  CLEAR: as_dados_ss.

  as_dados_ss-ssnum     = iv_ssnum.
  as_dados_ss-status    = ac_status_ativo.
  as_dados_ss-ctbdt     = is_p0000-begda.
  as_dados_ss-pernr     = is_p0000-pernr.
  as_dados_ss-bukrs     = iv_bukrs.
  as_dados_ss-acao      = av_acao.
  as_dados_ss-acao_desc = av_acao_desc.

*  lógica de valores de rúbricas de retribuição e diuturnidades
  GET BADI lo_badi.

  TRY.

      CALL BADI lo_badi->prep_rub_ret_diu
        EXPORTING
          ir_ret           = ir_ret
          ir_diu           = ir_diu
          is_p0008         = is_p0008                 " Reg.mestre HR infotipo 0008 (Remuneração base)
          is_p0001         = is_p0001                 " Registro mestre HR: infotipo 0001 (atrib.org.)
          is_p0007         = is_p0007                 " Registro mestre pessoal: infotipo 0007 (tempo trab.teórico)
          iv_begda         = iv_begda
          iv_endda         = iv_endda
          iv_usa_it0052    = iv_usa_it0052
        IMPORTING
          ev_retribuicao   = as_dados_ss-remun_base
          ev_diuturnidades = as_dados_ss-diuturnidades.

    CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho

      escreve_msg_log( is_campos_adicionais  = obtem_campos_log( ) ).
      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

  ENDTRY.

ENDMETHOD.


METHOD prep_dados_vinc_ss_cess.

  DATA lo_badi TYPE REF TO zhcm_fct_badi.

  TRY .

      CLEAR: as_dados_ss.

      as_dados_ss-ssnum     = iv_ssnum.
      as_dados_ss-status    = ac_status_desativo.
      as_dados_ss-ctedt     = is_p0000-begda - 1.
      as_dados_ss-pernr     = is_p0000-pernr.
      as_dados_ss-bukrs     = iv_bukrs.
      as_dados_ss-acao      = av_acao.
      as_dados_ss-acao_desc = av_acao_desc.

*      obtem motivo do contrato
      prep_motivo(  is_p0000 = is_p0000 ).

*      lógica de prestação de trabalho
      GET BADI lo_badi.

      CALL BADI lo_badi->prep_com_desemp
        EXPORTING
          is_p0000     = is_p0000                 " Registro mestre HR infotipo 0000 (Medidas)
          iv_begda     = iv_begda
          iv_endda     = iv_endda
        IMPORTING
          ev_com_desmp = as_dados_ss-com_desmp.  " Prestação de trabalho

*      obtem fundamentacao
      prep_fundamentacao( iv_motivo = as_dados_ss-motivo ).

      CLEAR: as_dados_fct.

    CATCH zcx_hcm_fct_cockpit.
      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

  ENDTRY.

ENDMETHOD.


METHOD prep_dados_vinc_ss_mod.

  TRY.

*      prepara dados para vinculo sem data de nascimento
      prep_dados_vinc_ss( iv_ssnum      = iv_ssnum          " Nº Beneficiário SS
                          iv_modalidade = iv_modalidade     " Modalidade de contrato de trabalho
                          iv_ctbdt      = iv_ctbdt          " Início de contrato
                          iv_ctedt      = iv_ctedt          " Fim do contrato
                          is_p0008      = is_p0008          " Reg.mestre HR infotipo 0008 (Remuneração base)
                          ir_ret        = ir_ret
                          ir_diu        = ir_diu
                          iv_prcnp      = iv_prcnp          " Classificação de Profissões (CNP ou CPP)
                          is_p0007      = is_p0007
                          iv_ssnum_subs = iv_ssnum_subs     " Nº Beneficiário SS
                          is_p0000      = is_p0000
                          is_p0001      = is_p0001
                          iv_begda      = iv_begda
                          iv_endda      = iv_endda
                          iv_usa_it0052 = iv_usa_it0052 ).

*      limpa local de trabalho
      CLEAR: as_dados_ss-ssnsa.

    CATCH zcx_hcm_fct_cockpit.
      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDTRY.

ENDMETHOD.


METHOD prep_dias_trab.

*  se o cálculo for anual
  IF av_calc_anual IS NOT INITIAL.
    TRY .
        prep_calendario( is_p0001 ).

        as_dados_ss-dias_trab = calc_dias_trab_anual( ).

      CATCH zcx_hcm_fct_cockpit.
        RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
    ENDTRY.

*  se não for cálculo anual
  ELSE.
*  caso empregado a tempo parcial
    IF is_p0007-teilk EQ abap_true.
      as_dados_ss-dias_trab = is_p0007-mostd / 6 ##NUMBER_OK.
    ELSE.
      as_dados_ss-dias_trab = 30 ##NUMBER_OK.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD prep_dur_min_contrato.

*  obtem duração mínima de contrato
  SELECT SINGLE low
    FROM tvarvc
    INTO @DATA(lv_low)
    WHERE name  EQ 'ZFCT_CONTRATO_DUR'  AND
          type  EQ 'P'                  AND
          numb  EQ '0000'.

  av_contrato_dur_min = lv_low.

ENDMETHOD.


METHOD prep_func.

  ao_dados_alv->get_functions( )->set_default( abap_true ).
  ao_dados_alv->get_functions( )->set_export_wordprocessor( ).
  ao_dados_alv->get_functions( )->set_export_localfile( ).
  ao_dados_alv->get_functions( )->set_export_spreadsheet( ).

  TRY.

*      verifica ação global
      CASE av_acao_global.

*        para FCT
        WHEN ac_acao_fct.
          ao_dados_alv->get_functions( )->remove_function( name = '&CONSC' ).

        WHEN OTHERS.
      ENDCASE.

    CATCH cx_salv_not_found.
    CATCH cx_salv_wrong_call.
  ENDTRY.

ENDMETHOD.


  METHOD prep_fundamentacao.

*    só é necessária fundamentação para SIM na comunicação de desemprego
    IF as_dados_ss-com_desmp EQ ac_com_desmp_sim.
*    obtem texto de fundamentação válido à data de final de contrato
      SELECT fundt~fundmt
        UP TO 1 ROWS
        FROM zhcm_tfct_fundm AS fund
        INNER JOIN zhcm_tfct_fundmt AS fundt ON fundt~motivo      EQ fund~motivo AND
                                                fundt~valid_from  EQ fund~valid_from
        INTO as_dados_ss-fundmt
        WHERE fund~motivo     EQ iv_motivo          AND
              fund~valid_from LE as_dados_ss-ctedt  AND
              fund~valid_to   GE as_dados_ss-ctedt  AND
              fundt~spras     EQ sy-langu
        ORDER BY fundt~fundmt.
      ENDSELECT.
    ENDIF.

  ENDMETHOD.


METHOD prep_horas_trab.

*  se o cálculo for anual
  IF av_calc_anual IS NOT INITIAL.
*    multiplica Horas de trabalho diárias com dias de trabalho anuais calculadas
    as_dados_ss-wostd = is_p0007-arbst * as_dados_ss-dias_trab.
  ELSE.
    as_dados_ss-wostd = is_p0007-wostd.
  ENDIF.

ENDMETHOD.


METHOD prep_layout.

  DATA: ls_key  TYPE salv_s_layout_key.

  ls_key-report = sy-cprog.
  ao_dados_alv->get_layout( )->set_key( ls_key ).
  ao_dados_alv->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).

ENDMETHOD.


METHOD prep_local_trab.

*  obtem local de trabalho
  SELECT ssnsa
    UP TO 1 ROWS
    FROM t5p0e
    INTO as_dados_ss-ssnsa
    WHERE werks EQ is_p0001-werks AND
          btrtl EQ is_p0001-btrtl AND
          endda GE iv_begda
    ORDER BY PRIMARY KEY.
  ENDSELECT.

*  se não encontra dados
  IF sy-subrc NE 0.
*  obtem local de trabalho
    SELECT ssnsa
      UP TO 1 ROWS
      FROM t5p0p
      INTO as_dados_ss-ssnsa
      WHERE werks EQ is_p0001-werks AND
            btrtl EQ is_p0001-btrtl
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDIF.

ENDMETHOD.


METHOD prep_log.

  DATA:ls_bal_s_log  TYPE bal_s_log.

*  atributo estáticos para execuções multiplas
  CHECK av_log_handle IS INITIAL.

  ls_bal_s_log-object = ac_log_obj.

*  valida acção para diferentes subobjetos de log
  CASE av_acao_global.
    WHEN ac_acao_fct.
      ls_bal_s_log-subobject  = ac_log_sub_obj_fct.

    WHEN ac_acao_ss.
      ls_bal_s_log-subobject  = ac_log_sub_obj_ss.

    WHEN OTHERS.
  ENDCASE.

  ls_bal_s_log-aldate     = sy-datum.
  ls_bal_s_log-altime     = sy-uzeit.
  ls_bal_s_log-aluser     = sy-uname.
  ls_bal_s_log-aldate_del = sy-datum + 30 ##NUMBER_OK.
  ls_bal_s_log-del_before = abap_true.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_bal_s_log
    IMPORTING
      e_log_handle = av_log_handle
    EXCEPTIONS
      OTHERS       = 1.

  IF sy-subrc NE 0 AND sy-msgty IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD prep_motivo.

  CASE av_acao_global.
    WHEN ac_acao_fct.
*      obtem modalidade de contrato para FCT por ação e empresa
      SELECT SINGLE motivo_fct
        FROM zhcm_tfct_ss_moa
        INTO as_dados_fct-motivo_cess
        WHERE massn EQ is_p0000-massn AND
              massg EQ is_p0000-massg AND
              acao  EQ av_acao_min    AND
              bukrs EQ as_dados_fct-bukrs.

      IF sy-subrc NE 0.
*        obtem modalidade de contrato para FCT por ação e sem ser específico de empresa
        SELECT SINGLE motivo_fct
          FROM zhcm_tfct_ss_moa
          INTO as_dados_fct-motivo_cess
          WHERE massn EQ is_p0000-massn AND
                massg EQ is_p0000-massg AND
                acao  EQ av_acao_min    AND
                bukrs EQ ''.

*      se não encontra motivo e não é uma transferência de empresa
        IF sy-subrc NE 0 AND av_transf_empresa IS INITIAL.
*        obtem modalidade de contrato para FCT por empresa
          SELECT SINGLE motivo_fct
            FROM zhcm_tfct_ss_mot
            INTO as_dados_fct-motivo_cess
            WHERE massn EQ is_p0000-massn AND
                  massg EQ is_p0000-massg AND
                  bukrs EQ as_dados_fct-bukrs.

          IF sy-subrc NE 0.
*            obtem modalidade de contrato para FCT sem ser específico de empresa
            SELECT SINGLE motivo_fct
              FROM zhcm_tfct_ss_mot
              INTO as_dados_fct-motivo_cess
              WHERE massn EQ is_p0000-massn AND
                    massg EQ is_p0000-massg AND
                    bukrs EQ ''.
          ENDIF.

        ENDIF.

      ENDIF.

    WHEN ac_acao_ss.

*      obtem modalidade de contrato para SS por ação e por empresa
      SELECT SINGLE motivo_ss
        FROM zhcm_tfct_ss_moa
        INTO as_dados_ss-motivo
        WHERE massn EQ is_p0000-massn AND
              massg EQ is_p0000-massg AND
              acao  EQ av_acao_min    AND
              bukrs EQ as_dados_ss-bukrs.

      IF sy-subrc NE 0.
*        obtem modalidade de contrato para SS por ação e sem ser específico de empresa
        SELECT SINGLE motivo_ss
          FROM zhcm_tfct_ss_moa
          INTO as_dados_ss-motivo
          WHERE massn EQ is_p0000-massn AND
                massg EQ is_p0000-massg AND
                acao  EQ av_acao_min    AND
                bukrs EQ ''.

*        se não encontra motivo e não é uma transferência de empresa
        IF sy-subrc NE 0 AND av_transf_empresa IS INITIAL.
*           obtem modalidade de contrato para SS e por empresa
          SELECT SINGLE motivo_ss
            FROM zhcm_tfct_ss_mot
            INTO as_dados_ss-motivo
            WHERE massn EQ is_p0000-massn AND
                  massg EQ is_p0000-massg AND
                  bukrs EQ as_dados_ss-bukrs.

          IF sy-subrc NE 0.
*            obtem modalidade de contrato para SS sem ser específico de empresa
            SELECT SINGLE motivo_ss
              FROM zhcm_tfct_ss_mot
              INTO as_dados_ss-motivo
              WHERE massn EQ is_p0000-massn AND
                    massg EQ is_p0000-massg AND
                    bukrs EQ ''.
          ENDIF.

        ENDIF.
      ENDIF.

  ENDCASE.

ENDMETHOD.


METHOD PREP_ORDENA.

*  verifica ação específica
  CASE av_acao.
*    consulta contrato de trabalho SS
    WHEN ac_acao_ss_cons_cont.

      TRY.
*          ordenação crescente para o campo Nº Beneficiário SS
          ao_dados_alv->get_sorts( )->add_sort( columnname = 'SSNUM'
                                                sequence   = if_salv_c_sort=>sort_up ).

        CATCH cx_salv_not_found .
        CATCH cx_salv_existing .
        CATCH cx_salv_data_error .
      ENDTRY.

*    outras ações
    WHEN OTHERS.

  ENDCASE.

ENDMETHOD.


METHOD prep_pfstatus.

  DATA: lv_pfstatus TYPE sypfkey.

*  verifica ação global
  CASE av_acao_global.
*    para FCT
    WHEN ac_acao_fct.
      lv_pfstatus = 'STANDARD_FCT'.

*    para SS
    WHEN ac_acao_ss.
  CASE av_acao.
*    consulta contrato de trabalho SS
    WHEN ac_acao_ss_cons_cont.
      lv_pfstatus = 'STANDARD_SS_CONSC'.

*    outras ações
    WHEN OTHERS.
      lv_pfstatus = 'STANDARD_SS'.
  ENDCASE.
    WHEN OTHERS.
  ENDCASE.

  "Set pf-status
  ao_dados_alv->set_screen_status(  pfstatus      =  lv_pfstatus
                                    report        =  sy-cprog
                                    set_functions =  ao_dados_alv->c_functions_all ). " para botão de Excel sem erro

ENDMETHOD.


METHOD prep_portas_logicas.

*  obtem configuração das portas lógicas
  SELECT *
    FROM zhcm_tfct_prtlog
    INTO CORRESPONDING FIELDS OF TABLE at_portas_logicas
    WHERE sysid EQ sy-sysid
    ORDER BY PRIMARY KEY.

ENDMETHOD.


METHOD prep_top_of_page.

  DATA: lv_fct_cessacao   TYPE string,
        lv_fct_alteracao  TYPE string,
        lv_ss_admissao    TYPE string,
        lv_ss_cessacao    TYPE string,
        lv_nul            TYPE string,
        lv_transf_empresa TYPE string.

  lv_transf_empresa = 'Transferência de empresa'(005).

  DATA(lo_top) = NEW cl_salv_form_layout_grid( ).

  DATA(lo_label) = lo_top->create_label(  row     =  1  " natural Number
                                          column  =  1  ).

  lo_label->set_text( value = 'Ação'(003) ).

  DATA(lo_flow) = lo_top->create_flow(  row     = 2
                                        column  = 1 ).

  CASE av_acao.
*    FCT admissão
    WHEN ac_acao_admissao.
      lo_flow->create_text( text = 'FCT - Admitir colaborador'(002) ).

*    FCT alteração
    WHEN ac_acao_modificacao.

      lv_fct_alteracao = 'FCT - Alterar Contrato'(006).

      lv_nul = 'Anular modificação'(012).

*      se é uma anulação de alteração
      IF av_anul_acao IS NOT INITIAL.
        lv_fct_alteracao = |{ lv_fct_alteracao } - { lv_nul }|.
      ENDIF.

      lo_flow->create_text( text = lv_fct_alteracao ).

*    FCT cessação
    WHEN ac_acao_cessacao.

      lv_fct_cessacao = 'FCT - Cessar Contrato'(003).

      lv_nul = 'Anular cessação'(004).

*      se é uma anulação de contrato
      IF av_anul_acao IS NOT INITIAL.
        lv_fct_cessacao = |{ lv_fct_cessacao } - { lv_nul }|.
      ENDIF.

*      se é uma transferência de empresa
      IF av_transf_empresa IS NOT INITIAL.
        lv_fct_cessacao = |{ lv_fct_cessacao } - { lv_transf_empresa }|.
      ENDIF.

      lo_flow->create_text( text = lv_fct_cessacao ).

*    SS admissão
    WHEN ac_acao_ss_vinculo.

      lv_ss_admissao = 'SS - Vínculo colaborador'(007).

*      se é uma transferência de empresa
      IF av_transf_empresa IS NOT INITIAL.
        lv_ss_admissao = |{ lv_ss_admissao } - { lv_transf_empresa }|.
      ENDIF.

      lo_flow->create_text( text = lv_ss_admissao ).

*    SS alteração vínculo
    WHEN ac_acao_ss_modificacao.
      lo_flow->create_text( text = 'SS - Alterar vínculo'(008) ).

*    SS Adicionar período rendimento
    WHEN ac_acao_ss_alt_per.
      lo_flow->create_text( text = 'SS - Adicionar período rendimento'(009) ).

*    SS Cessar vínculo
    WHEN ac_acao_ss_cessacao.

      lv_ss_cessacao = 'SS - Cessar vínculo'(010).

*      se é uma transferência de empresa
      IF av_transf_empresa IS NOT INITIAL.
        lv_ss_cessacao = |{ lv_ss_cessacao } - { lv_transf_empresa }|.
      ENDIF.

      lo_flow->create_text( text = lv_ss_cessacao ).

*    SS Consulta contratos
    WHEN ac_acao_ss_cons_cont.
      lo_flow->create_text( text = 'SS - Consulta contratos'(011) ).

    WHEN OTHERS.
  ENDCASE.

  lo_label = lo_top->create_label(  row     = 3  " natural Number
                                    column  = 1  ).
  lo_label->set_text( value = '' ).

  lo_label = lo_top->create_label(  row     = 4  " natural Number
                                    column  = 1  ).

  lo_label->set_text( value = 'Número de registos identificados'(001) ).

  lo_flow = lo_top->create_flow(  row     = 5
                                  column  = 1 ).

  CASE av_acao_global.
*    para ação FCT global
    WHEN ac_acao_fct.
      lo_flow->create_text( text = lines( at_dados_fct_alv ) ).

*    para ação SS global
    WHEN ac_acao_ss.
*      para ação SS específica
      CASE av_acao.

*        para consulta de contrato
        WHEN ac_acao_ss_cons_cont.

          lo_flow->create_text( text = lines( at_dados_ss_alv_consc ) ).

*        para as outras açoes
        WHEN OTHERS.
          lo_flow->create_text( text = lines( at_dados_ss_alv ) ).
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

  ao_dados_alv->set_top_of_list( value =  lo_top ).

ENDMETHOD.


METHOD prep_transf_empresa_flag.

  av_transf_empresa = iv_transf_empresa.

ENDMETHOD.


METHOD prep_ultimo_fct.

  CLEAR: as_dados_fct_ultimo.

*  obtem ultimo registo
  SELECT *
    UP TO 1 ROWS
    FROM zhcm_tfct_log
    INTO as_dados_fct_ultimo
    WHERE ssnum EQ iv_ssnum AND
          bukrs EQ iv_bukrs
    ORDER BY seq DESCENDING.
  ENDSELECT.

ENDMETHOD.


METHOD PREP_ULTIMO_SS.

  CLEAR: as_dados_ss_ultimo.

*  obtem ultimo registo
  SELECT *
    UP TO 1 ROWS
    FROM zhcm_tfct_ss_log
    INTO as_dados_ss_ultimo
    WHERE ssnum EQ iv_ssnum AND
          bukrs eq iv_bukrs
    ORDER BY seq DESCENDING.
  ENDSELECT.

ENDMETHOD.


METHOD processa_ss_contratos.

  DATA: lt_ssnum TYPE ty_ssnum_tt,
        lv_bukrs TYPE bukrs,
        lv_erro  TYPE flag.


*      percorre todos os SSNUM
  LOOP AT it_ssnum ASSIGNING FIELD-SYMBOL(<fs_ssnum>).
*        obtem a primeira empresa
    IF sy-tabix EQ 1 .
      lv_bukrs = <fs_ssnum>-bukrs.
    ENDIF.

*        se é uma nova empresa
    IF <fs_ssnum>-bukrs NE lv_bukrs.
      TRY.
          obtem_ss_contratos( iv_begda  = iv_begda    " Início da validade
                              iv_endda  = iv_endda    " Fim da validade
                              it_ssnum  = lt_ssnum ).

        CATCH zcx_hcm_fct_cockpit.
          lv_erro = abap_true.

      ENDTRY.

      CLEAR: lt_ssnum.
      lv_bukrs = <fs_ssnum>-bukrs.

    ENDIF.

    INSERT <fs_ssnum> INTO TABLE lt_ssnum.

  ENDLOOP.

  TRY.

*      obtem a ultima entrada
      obtem_ss_contratos( iv_begda  = iv_begda    " Início da validade
                          iv_endda  = iv_endda    " Fim da validade
                          it_ssnum  = lt_ssnum ).

    CATCH zcx_hcm_fct_cockpit.
      lv_erro = abap_true.

  ENDTRY.

*  se houve algum erro e não foram encontrados dados nenhuns
  IF lv_erro IS NOT INITIAL AND at_dados_ss_alv_consc IS INITIAL.
    RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDIF.

ENDMETHOD.


METHOD remove_registo.

  DATA: lv_dummy  TYPE string ##NEEDED.

*  só para casos que já exista a criação de um período de rendimento
  CHECK as_dados_fct-seq GT 1.

  DELETE FROM zhcm_tfct_log
  WHERE ssnum EQ as_dados_fct-ssnum  AND
        seq   EQ as_dados_fct-seq.

  IF sy-subrc EQ 0 AND iv_atu_manual IS INITIAL.
    MESSAGE s001(zinetum_fct) INTO lv_dummy.

  ELSEIF sy-subrc EQ 0 AND iv_atu_manual IS NOT INITIAL.
    MESSAGE s012(zinetum_fct) INTO lv_dummy.
  ENDIF.

  escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

ENDMETHOD.


METHOD reset_stat_com.

  av_icon = ac_light_out.
  CLEAR: av_comm_msg.

ENDMETHOD.


METHOD user_command.

  CASE sy-ucomm.
    WHEN '&LOG'.
      mostra_log_online( ).

    WHEN '&COM'.
      comunica_dados( ).

    WHEN '&CONSC'.
      consulta_contrato( ).

    WHEN '&ATU_MAN'.
      atualizacao_manual( ).

    WHEN OTHERS.

  ENDCASE.

ENDMETHOD.


METHOD valida_campos_obrg_fct_admi.

*  valida Nº Beneficiário SS
  IF as_dados_fct-ssnum IS INITIAL.
    MESSAGE e009(zinetum_fct) WITH CAST cl_abap_elemdescr(  cl_abap_typedescr=>describe_by_data( as_dados_fct-ssnum ) )->get_ddic_field( )-scrtext_m INTO DATA(lv_dummy) ##NEEDED.

    escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

    RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDIF.

*  valida Empresa
  IF as_dados_fct-bukrs IS INITIAL.
    MESSAGE e009(zinetum_fct) WITH CAST cl_abap_elemdescr(  cl_abap_typedescr=>describe_by_data( as_dados_fct-bukrs ) )->get_ddic_field( )-scrtext_m INTO lv_dummy.

    escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

    RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDIF.

ENDMETHOD.


METHOD VALIDA_CAMPOS_OBRG_SS_VINC.

*  valida Nº Beneficiário SS
  IF as_dados_ss-ssnum IS INITIAL.
    MESSAGE e009(zinetum_fct) WITH CAST cl_abap_elemdescr(  cl_abap_typedescr=>describe_by_data( as_dados_ss-ssnum ) )->get_ddic_field( )-scrtext_m INTO DATA(lv_dummy) ##NEEDED.

    escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

    RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDIF.

*  valida Empresa
  IF as_dados_ss-bukrs IS INITIAL.
    MESSAGE e009(zinetum_fct) WITH CAST cl_abap_elemdescr(  cl_abap_typedescr=>describe_by_data( as_dados_ss-bukrs ) )->get_ddic_field( )-scrtext_m INTO lv_dummy.

    escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

    RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDIF.

ENDMETHOD.


METHOD valida_duracao_contrato.

  DATA: lv_dummy  TYPE string ##NEEDED.

  IF is_p0016-ctedt IS INITIAL.
    RETURN.
  ENDIF.

*  se a duração do contrato é menor que a duração configurada
  IF is_p0016-ctedt - is_p0016-begda LE av_contrato_dur_min.

    as_dados_fct-ssnum  = iv_ssnum.
    as_dados_fct-pernr  = iv_pernr.

    MESSAGE w004(zinetum_fct) INTO lv_dummy.

    escreve_msg_log( is_campos_adicionais = obtem_campos_log( ) ).

    CLEAR: as_dados_fct.

    RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDIF.

ENDMETHOD.


METHOD valida_mod_ctbdt.

*  obtem ultimo registo
  SELECT SINGLE 'X'
    FROM zhcm_tfct_log
    INTO @rv_exists
    WHERE ssnum EQ @iv_ssnum AND
          bukrs EQ @iv_bukrs AND
          ctbdt EQ @iv_ctbdt.

ENDMETHOD.


METHOD valida_mod_ctbdt_ss.

*  obtem ultimo registo
  SELECT SINGLE 'X'
    FROM zhcm_tfct_ss_log
    INTO @rv_exists
    WHERE ssnum EQ @iv_ssnum AND
          bukrs EQ @iv_bukrs AND
          ctbdt EQ @iv_ctbdt.

ENDMETHOD.


METHOD valida_motivo_transf.

  CASE av_acao_global.
    WHEN ac_acao_fct.
*      verifica se o motivo está configurado
      SELECT SINGLE 'X'
        FROM zhcm_tfct_ss_moa
        INTO @rv_ok
        WHERE massn EQ @is_p0000-massn AND
              massg EQ @is_p0000-massg AND
              acao  EQ @av_acao_min    AND
              bukrs EQ @iv_bukrs.

    WHEN ac_acao_ss.
*      verifica se o motivo está configurado
      SELECT SINGLE 'X'
        FROM zhcm_tfct_ss_moa
        INTO @rv_ok
        WHERE massn EQ @is_p0000-massn AND
              massg EQ @is_p0000-massg AND
              acao  EQ @av_acao_min    AND
              bukrs EQ @iv_bukrs.

  ENDCASE.

ENDMETHOD.


METHOD valida_ultimo_ativo.

  prep_ultimo_fct(  iv_ssnum  = iv_ssnum
                    iv_bukrs  = iv_bukrs ).

*  se for inativo
  IF as_dados_fct_ultimo-status NE ac_status_ativo.
    RETURN.
  ENDIF.

*  se ultima alteração da medida já foi enviada
  IF iv_modnul IS INITIAL AND as_dados_fct_ultimo-ctbdt EQ iv_ctbdt.
    RETURN.
  ENDIF.

*  valida alteração para a data de alteração de contrato
  IF valida_mod_ctbdt(  iv_ssnum  = iv_ssnum
                        iv_bukrs  = iv_bukrs
                        iv_ctbdt  = iv_ctbdt ) IS NOT INITIAL.
    RETURN.
  ENDIF.

  rv_ok = abap_true.

ENDMETHOD.


METHOD valida_ultimo_ativo_cess.

  prep_ultimo_fct(  iv_ssnum  = iv_ssnum
                    iv_bukrs  = iv_bukrs ).

  IF as_dados_fct_ultimo-status NE ac_status_ativo OR as_dados_fct_ultimo-status IS INITIAL.
    RETURN.
  ENDIF.

  rv_ok = abap_true.

ENDMETHOD.


METHOD valida_ultimo_ativo_cess_anul.

  prep_ultimo_fct(  iv_ssnum  = iv_ssnum
                    iv_bukrs  = iv_bukrs ).

  IF as_dados_fct_ultimo-status NE ac_status_desativo OR as_dados_fct_ultimo-status IS INITIAL.
    RETURN.
  ENDIF.

  rv_ok = abap_true.

ENDMETHOD.


METHOD valida_ultimo_ativo_ss.

  prep_ultimo_ss( iv_ssnum  = iv_ssnum
                  iv_bukrs  = iv_bukrs ).

*  se o status não está ativo ou se ultima alteração da medida já foi enviada
  IF as_dados_ss_ultimo-status NE ac_status_ativo OR ( iv_ctbdt IS NOT INITIAL AND as_dados_ss_ultimo-ctbdt EQ iv_ctbdt ).
    RETURN.
  ENDIF.

*  se for uma alteração de período de rendimento, valida se ultima alteração da medida já foi enviada
  IF iv_alt_per IS NOT INITIAL AND as_dados_ss_ultimo-ctbdt EQ iv_ctbdt.
    RETURN.
  ENDIF.

*  valida alteração para a data de alteração de contrato
  IF iv_ctbdt IS NOT INITIAL AND valida_mod_ctbdt_ss( iv_ssnum  = iv_ssnum
                                                      iv_bukrs  = iv_bukrs
                                                      iv_ctbdt  = iv_ctbdt ) IS NOT INITIAL.
    RETURN.
  ENDIF.

  rv_ok = abap_true.

ENDMETHOD.
ENDCLASS.
