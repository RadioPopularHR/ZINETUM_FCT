*&---------------------------------------------------------------------*
*& Report ZHCM_FCT_COCKPIT
*& Comunicação com webservices para atualizar registos dos colaboradores
*& Author: João Paixão Silva
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcm_fct_cockpit.

INCLUDE zhcm_fct_cockpit_top.
INCLUDE zhcm_fct_cockpit_sel.
INCLUDE zhcm_fct_cockpit_frm.

INITIALIZATION.
  PERFORM inic_dados.

AT SELECTION-SCREEN OUTPUT.
  PERFORM contrl_ecra.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fichfl.
  PERFORM obtem_nome_fich CHANGING p_fichfl.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fichsl.
  PERFORM obtem_nome_fich CHANGING p_fichsl.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fichfo.
  PERFORM obtem_nome_fich CHANGING p_fichfo.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fichso.
  PERFORM obtem_nome_fich CHANGING p_fichso.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fichfb.
  PERFORM obtem_nome_fichserv CHANGING p_fichfb.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fichsb.
  PERFORM obtem_nome_fichserv CHANGING p_fichsb.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
*  download de template Excel FCT
    WHEN 'BUT'.
      zcl_hcm_fct_cockpit=>obtem_excel_template(  iv_r3_application_name  = 'Carregamento_IDs_FCT'
                                                  iv_desc                 = 'Admissão FCT' )  ##NO_TEXT.

*  download de template Excel SS
    WHEN 'BUV'.
      zcl_hcm_fct_cockpit=>obtem_excel_template(  iv_r3_application_name  = 'Carregamento_IDs_SS'
                                                  iv_desc                 = 'Vínculo SS' ) ##NO_TEXT.

*  se for mostrar as parametrizações
    WHEN 'PARAM'.
      CALL TRANSACTION 'ZHCM_FCT_SPRO'.
  ENDCASE.

START-OF-SELECTION.

  DATA: lv_acao           TYPE zhcm_fct_acao                    ##NEEDED,
        lv_subty_niss_sub TYPE subty                            ##NEEDED,
        lv_erro           TYPE flag                             ##NEEDED,
        lt_p0000          TYPE STANDARD TABLE OF p0000          ##NEEDED,
        lt_ssnum          TYPE zcl_hcm_fct_cockpit=>ty_ssnum_tt ##NEEDED,
        lv_return         TYPE flag                             ##NEEDED,
        lv_usa_it0052     TYPE flag,
        lt_it_metadata    TYPE ty_it_metadata_tt,
        lt_p0008          TYPE STANDARD TABLE OF p0008.

  DATA(lo_dados)      = NEW zcl_hcm_fct_cockpit( ) ##NEEDED.
  DATA(lv_usa_it0302) = zcl_hcm_fct_cockpit=>obtem_sistema_it0302( ) ##NEEDED.

  PERFORM prep_acao CHANGING lv_acao.

  PERFORM prep_subty_niss_sub CHANGING lv_subty_niss_sub.

*  processa ações independentes de colaborador/BD lógica
  PERFORM processa_nao_colab  USING     lv_acao
                              CHANGING  lo_dados
                                        lv_erro
                                        lv_return.

*  se for processadas ações
  IF lv_return IS NOT INITIAL.
*    termina o evento START-OF-SELECTION
    RETURN.
  ENDIF.

*caso seja para ler da base de dados lógica
GET pernr.

  CLEAR: lt_it_metadata.

*  prepara dados caso o sistema use o IT0302 como default de medidas
  PERFORM prepara_p0000 USING     lv_usa_it0302
                        CHANGING  lt_p0000.

*  prepara a ordenação dos IT de medidas e alteração de rendimentos
  PERFORM prepara_metadata  USING     lv_acao
                                      lt_p0000
                            CHANGING  lt_it_metadata
                                      lv_usa_it0052
                                      lt_p0008.

*  percorre todas os registos
  LOOP AT lt_it_metadata ASSIGNING FIELD-SYMBOL(<fs_it_metadata>) ##NEEDED.

    lo_dados = NEW zcl_hcm_fct_cockpit( iv_acao = <fs_it_metadata>-acao ).

    lo_dados->prep_log( ).

    CASE <fs_it_metadata>-infty.

*      para alterações por medidas
      WHEN zcl_hcm_fct_cockpit=>ac_infty_0000.

*        obtem registo do IT0000
        READ TABLE lt_p0000
        ASSIGNING FIELD-SYMBOL(<fs_p0000>)
        WITH KEY begda = <fs_it_metadata>-begda.

        CHECK sy-subrc EQ 0.

*        processa colaborador
        PERFORM processo_colab  USING     lv_acao
                                          <fs_p0000>
                                          lv_subty_niss_sub
                                CHANGING  lt_ssnum
                                          lo_dados
                                          lv_erro
                                          lt_it_metadata.

*      para alterações manuais nos rendimentos
      WHEN zcl_hcm_fct_cockpit=>ac_infty_0008.

*        obtem registo do IT0008
        READ TABLE lt_p0008
        ASSIGNING FIELD-SYMBOL(<fs_p0008>)
        WITH KEY begda = <fs_it_metadata>-begda.

        CHECK sy-subrc EQ 0.

*        processa rendimentos do colaborador
        PERFORM processo_colab_rend USING     <fs_it_metadata>-acao
                                              <fs_p0008>
                                    CHANGING  lo_dados
                                              lv_erro.

      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

END-OF-SELECTION.

*  valida ação
  CASE lv_acao.
*    para consulta de contratos segurança social
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_cons_cont.
*      para consulta de contratos segurança social
      PERFORM consulta_ss_contrato  CHANGING  lt_ssnum
                                              lo_dados
                                              lv_erro.

    WHEN OTHERS.
  ENDCASE.

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
