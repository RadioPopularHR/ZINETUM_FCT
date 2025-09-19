*&---------------------------------------------------------------------*
*& Report ZHCM_FCT_GO_LIVE
*& Atualização de dados para GoLive
*& Author: João Paixão Silva
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcm_fct_go_live.

INCLUDE zhcm_fct_go_live_top.
INCLUDE zhcm_fct_go_live_sel.
INCLUDE zhcm_fct_go_live_frm.

INITIALIZATION.
  PERFORM inic_dados.

START-OF-SELECTION.

  DATA: lv_acao   TYPE zhcm_fct_acao                    ##NEEDED,
        lv_erro   TYPE flag                             ##NEEDED,
        lv_return TYPE flag                             ##NEEDED.

  DATA(lo_dados)  = NEW zcl_hcm_fct_cockpit( ) ##NEEDED.

  PERFORM prep_acao CHANGING lv_acao.

*  processa ações independentes de colaborador/BD lógica
  PERFORM processa_nao_colab  USING     lv_acao
                              CHANGING  lo_dados
                                        lv_return.

*  se for processadas ações
  IF lv_return IS NOT INITIAL.
*    termina o evento START-OF-SELECTION
    RETURN.
  ENDIF.

*caso seja para ler da base de dados lógica
GET pernr.

  lo_dados = NEW zcl_hcm_fct_cockpit( iv_acao = lv_acao ).

  lo_dados->prep_log( ).

*  valida ação
  CASE lv_acao.

*    para admissão FCT
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_modificacao.

      PERFORM processa_ss_mod_vinc  CHANGING  lo_dados
                                              lv_erro.

*    para modificação FCT
    WHEN zcl_hcm_fct_cockpit=>ac_acao_modificacao.
      PERFORM processa_fct_mod  CHANGING  lo_dados
                                          lv_erro.
  ENDCASE.

*    guarda os dados para serem apresentados no ALV
  lo_dados->guarda_dados_alv( ).

END-OF-SELECTION.

*  em caso de erros
  IF lv_erro IS NOT INITIAL.
*    indicar que se deve analisar o log
    MESSAGE s006(zinetum_fct) DISPLAY LIKE 'W'.
  ENDIF.

  lo_dados->mostra_alv( ).

*  execução em background
  IF sy-batch EQ abap_true.
    lo_dados->guarda_log( ).
  ENDIF.
