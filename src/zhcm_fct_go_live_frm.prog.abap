*&---------------------------------------------------------------------*
*& Include          ZHCM_FCT_GO_LIVE_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form inic_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM inic_dados .

  pnpbegda = sy-datum - 365 ##NUMBER_OK.
  pnpendda = sy-datum.

*  coloca as opções de retribuições com valor default a vazio
  PERFORM retribuicoes_vazio.

*  coloca as opções de diuturnidades com valor default a vazio
  PERFORM diuturnidades_vazio.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form retribuicoes_vazio
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM retribuicoes_vazio .

  APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO s_retsm[].
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO s_retm[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form diuturnidades_vazio
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM diuturnidades_vazio .

  APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO s_diusm[].
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO s_dium[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form prep_acao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ACAO
*&---------------------------------------------------------------------*
FORM prep_acao  CHANGING cv_acao TYPE zhcm_fct_acao.

  CASE abap_true.
    WHEN r_mods.
      cv_acao = zcl_hcm_fct_cockpit=>ac_acao_ss_modificacao.
    WHEN r_mod.
      cv_acao = zcl_hcm_fct_cockpit=>ac_acao_modificacao.
    WHEN r_cons.
      cv_acao = zcl_hcm_fct_cockpit=>ac_acao_consulta.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_nao_colab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ACAO
*&      <-- LO_DADOS
*&      <-- LV_RETURN
*&---------------------------------------------------------------------*
FORM processa_nao_colab  USING    iv_acao   TYPE zhcm_fct_acao
                         CHANGING co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                  cv_return TYPE flag.

*  consulta log
  IF iv_acao EQ zcl_hcm_fct_cockpit=>ac_acao_consulta.

    PERFORM processa_consulta CHANGING co_dados.

    cv_return = abap_true.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_consulta
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CO_DADOS
*&---------------------------------------------------------------------*
FORM processa_consulta  CHANGING co_dados TYPE REF TO zcl_hcm_fct_cockpit.

*    consulta log
  co_dados->consulta_log( iv_subobject  = zcl_hcm_fct_cockpit=>ac_log_sub_obj_ss  " Log de aplicação: subobjeto
                          iv_aldate_i   = p_dati        " Log de aplicação: data
                          iv_altime_i   = p_timi        " Log de aplicação: hora
                          iv_aldate_f   = p_datf
                          iv_altime_f   = p_timf ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_ss_mod_vinc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CO_DADOS
*&      <-- LV_ERRO
*&---------------------------------------------------------------------*
FORM processa_ss_mod_vinc  CHANGING co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                    cv_erro   TYPE flag.

  DATA: ls_p0000      TYPE p0000,
        lv_usa_it0052 TYPE flag,
        ls_p0008      TYPE p0008,
        lv_ctbdt      TYPE zhcm_fct_ctbdt.

*  obtém ultima medida
  rp_provide_from_last p0000 space pn-begda pn-endda.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Remuneração base
  rp_provide_from_last p0008 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Elementos do contrato
  rp_provide_from_last p0016 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Prof.Classificat. PT
  rp_provide_from_last p0337 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Atribuição organizacional
  rp_provide_from_last p0001 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Tempo de trabalho teórico
  rp_provide_from_last p0007 space pn-begda pn-endda.

  ls_p0000 = p0000.

  ls_p0000-massn  = p_masnsm.
  ls_p0000-massg  = p_masnsg.

*  só executa se tiver NISS
  IF p0332-ssnum IS INITIAL.
    RETURN.
  ENDIF.

*  se não foi indicada data de inicio de contrato
  IF p_begda IS NOT INITIAL.
    lv_ctbdt = p_begda.
  ELSE.
    lv_ctbdt = zcl_hcm_fct_cockpit=>obtem_data_ini_contrato( p0016 ).
  ENDIF.

*  se os dados de renumeração estão no IT0052
  IF p0052[] IS NOT INITIAL.
*  obtém ultimo registo de dados de Remuneração base
    rp_provide_from_last p0052 space ls_p0000-begda ls_p0000-begda.

    MOVE-CORRESPONDING p0052 TO ls_p0008.

    ls_p0008-bsgrd = p0008-bsgrd.

    lv_usa_it0052 = abap_true.

  ELSE.

    ls_p0008 = p0008.
  ENDIF.

  TRY .
*      prepara dados para vinculo
      co_dados->prep_dados_vinc_ss_mod( iv_ssnum      = p0332-ssnum
                                        iv_modalidade = p0016-cttyp
                                        iv_ctbdt      = lv_ctbdt
                                        iv_ctedt      = p0016-ctedt
                                        is_p0008      = p0008
                                        ir_ret        = CORRESPONDING #( s_retsm[] )
                                        ir_diu        = CORRESPONDING #( s_diusm[] )
                                        iv_prcnp      = p0337-prcnp
                                        is_p0007      = p0007
                                        iv_ssnum_subs = ''
                                        is_p0000      = ls_p0000
                                        is_p0001      = p0001
                                        iv_begda      = pn-begda
                                        iv_endda      = pn-endda
                                        iv_usa_it0052 = lv_usa_it0052 ).

      IF p_env IS NOT INITIAL.
*        envia dados e atualiza log
        co_dados->atualiza_registo_mod_vin_ss( ).
      ENDIF.

    CATCH zcx_hcm_fct_cockpit.
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_fct_mod
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LO_DADOS
*&      <-- CV_ERRO
*&---------------------------------------------------------------------*
FORM processa_fct_mod  CHANGING co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                cv_erro   TYPE flag.

  DATA: ls_p0000      TYPE p0000,
        lv_usa_it0052 TYPE flag,
        ls_p0008      TYPE p0008.

*  obtém ultima medida
  rp_provide_from_last p0000 space pn-begda pn-endda.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Remuneração base
  rp_provide_from_last p0008 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Atribuição organizacional
  rp_provide_from_last p0001 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Tempo de trabalho teórico
  rp_provide_from_last p0007 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Elementos do contrato
  rp_provide_from_last p0016 space pn-begda pn-endda.

  ls_p0000 = p0000.

*  só executa se tiver NISS
  IF p0332-ssnum IS INITIAL.
    RETURN.
  ENDIF.

*  obtem ultimo registo
  SELECT 'X'
    UP TO 1 ROWS
    FROM zhcm_tfct_log
    INTO @DATA(lv_dummy)
    WHERE ssnum EQ @p0332-ssnum AND
          bukrs EQ @p0001-bukrs
    ORDER BY seq DESCENDING.
  ENDSELECT.

*  se não existe
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

*  se os dados de renumeração estão no IT0052
  IF p0052[] IS NOT INITIAL.
*  obtém ultimo registo de dados de Remuneração base
    rp_provide_from_last p0052 space ls_p0000-begda ls_p0000-begda.

    MOVE-CORRESPONDING p0052 TO ls_p0008.

    ls_p0008-bsgrd = p0008-bsgrd.

    lv_usa_it0052 = abap_true.

  ELSE.

    ls_p0008 = p0008.
  ENDIF.


  co_dados->valida_ultimo_ativo( iv_ssnum  = p0332-ssnum
                                 iv_bukrs  = p0001-bukrs
                                 iv_ctbdt  = p_begdaf
                                 iv_modnul = abap_false ).

  TRY.
*      prepara dados para a modificação
      co_dados->prep_dados_mod( is_p0008          = p0008
                                ir_ret            = CORRESPONDING #( s_retm[] ) " rúbricas de retribuções
                                iv_ctbdt          = p_begdaf
                                ir_diu            = CORRESPONDING #( s_dium[] ) " rúbricas de diuturnidades
                                iv_inic_peri_rend = p0000-begda
                                is_p0001          = p0001
                                is_p0007          = p0007
                                iv_begda          = pn-begda
                                iv_endda          = pn-endda
                                iv_usa_it0052     = lv_usa_it0052 ).

      IF p_env IS NOT INITIAL.
*        envia dados e atualiza log
        co_dados->atualiza_registo_mod( ).
      ENDIF.

    CATCH zcx_hcm_fct_cockpit.
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
