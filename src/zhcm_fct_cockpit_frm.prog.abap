*&---------------------------------------------------------------------*
*& Include          ZHCM_FCT_COCKPIT_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form data_initialization
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM inic_dados .

  pnpbegda = sy-datum - 365 ##NUMBER_OK.
  pnpendda = sy-datum.

*  coloca as opções de medidas com valor default a vazio
  PERFORM medidas_vazio.

*  coloca as opções de retribuições com valor default a vazio
  PERFORM retribuicoes_vazio.

*  coloca as opções de diuturnidades com valor default a vazio
  PERFORM diuturnidades_vazio.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HIDE_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_gc_MOD_ID  text
*----------------------------------------------------------------------*
FORM hide_field  USING ir_mod_id TYPE ty_rg_mod_id.

  LOOP AT SCREEN.
    IF screen-group1 IN ir_mod_id.
      screen-active     = 0.
      screen-invisible  = 1.

      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form contrl_ecra
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM contrl_ecra.

  CASE abap_true.
*    Comunicações
    WHEN r_com.

      PERFORM contrl_ecra_com.

      CASE abap_true.
        WHEN r_fct.
          PERFORM contrl_ecra_fct.
        WHEN r_ss.
          PERFORM contrl_ecra_ss.
      ENDCASE.

*    Ferramentas
    WHEN r_frm.
      PERFORM contrl_ecra_frm.

*    Admissões via ficheiro
    WHEN r_fcom.
      PERFORM contrl_ecra_fcom.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_gc_MOD_ID  text
*----------------------------------------------------------------------*
FORM show_field  USING  ir_mod_id TYPE ty_rg_mod_id.

  LOOP AT SCREEN.
    IF screen-group1 IN ir_mod_id.
      screen-active     = 1.
      screen-invisible  = 0.

      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form contrl_ecra_fct
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM contrl_ecra_fct .

  DATA: lr_mod_id_show TYPE ty_rg_mod_id.
  DATA: lr_mod_id_hide TYPE ty_rg_mod_id.

*  adiciona campos a mostrar/esconder quando selecionada a área FCT
  PERFORM add_mod_id_fct  CHANGING  lr_mod_id_show
                                    lr_mod_id_hide.

  CASE abap_true.
*    Admitir colaborador
    WHEN r_admi.
*      adiciona campos a mostrar/esconder quando selecionada a opção Admitir colaborador da área FCT
      PERFORM add_mod_id_fct_admi CHANGING  lr_mod_id_show
                                            lr_mod_id_hide.

*    Cessar contrato
    WHEN r_cess.
*      adiciona campos a mostrar/esconder quando selecionada a opção Cessar contrato da área FCT
      PERFORM add_mod_id_fct_cess CHANGING  lr_mod_id_show
                                            lr_mod_id_hide.

*    Modificar contrato
    WHEN r_mod.
*      adiciona campos a mostrar/esconder quando selecionada a opção Modificar contrato da área FCT
      PERFORM add_mod_id_fct_mod CHANGING lr_mod_id_show
                                          lr_mod_id_hide.

  ENDCASE.

*  modifica o ecrã para os campos a mostrar/esconder
  PERFORM show_field USING lr_mod_id_show.
  PERFORM hide_field USING lr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form contrl_ecra_ss
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM contrl_ecra_ss .

  DATA: lr_mod_id_show TYPE ty_rg_mod_id.
  DATA: lr_mod_id_hide TYPE ty_rg_mod_id.

*      adiciona campos a mostrar/esconder quando selecionada a área SS
  PERFORM add_mod_id_ss CHANGING  lr_mod_id_show
                                  lr_mod_id_hide.

  CASE abap_true.

*    Vínculo colaborador
    WHEN r_vincs.
*      adiciona campos a mostrar/esconder quando selecionada a opção Vínculo colaborador da área SS
      PERFORM add_mod_id_ss_vincs CHANGING  lr_mod_id_show
                                            lr_mod_id_hide.

*    Alterar vínculo
    WHEN r_mods.
*      adiciona campos a mostrar/esconder quando selecionada a opção Alterar vínculo da área SS
      PERFORM add_mod_id_ss_mods CHANGING  lr_mod_id_show
                                            lr_mod_id_hide.

*    Cessar vínculo
    WHEN r_cesss.
*      adiciona campos a mostrar/esconder quando selecionada a opção Cessar vínculo da área SS
      PERFORM add_mod_id_ss_cesss CHANGING  lr_mod_id_show
                                            lr_mod_id_hide.

*    consulta contrato
    WHEN r_consc.
*      adiciona campos a mostrar/esconder quando selecionada a opção Consulta contrato da área SS
      PERFORM add_mod_id_ss_consc CHANGING  lr_mod_id_show
                                            lr_mod_id_hide.

  ENDCASE.

*  modifica o ecrã para os campos a mostrar/esconder
  PERFORM show_field USING lr_mod_id_show.
  PERFORM hide_field USING lr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form obtem_nome_fich
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FICH
*&---------------------------------------------------------------------*
FORM obtem_nome_fich  CHANGING cv_fich  TYPE localfile.

  DATA: lt_fich       TYPE filetable,
        ls_fich       TYPE LINE OF filetable,
        lv_rcode      TYPE i,
        lv_usera      TYPE i,
        lv_titutlopop TYPE string.

  CONSTANTS: lc_err      TYPE c VALUE 'E'.

  lv_titutlopop = 'Indicar nome do ficheiro a ler'(t07).

  CLEAR: ls_fich.

  cl_gui_frontend_services=>file_open_dialog( EXPORTING   window_title            = lv_titutlopop
                                                          default_extension       = 'XLSX'
                                                          file_filter             = 'Ficheiros do Excel (*.XLS)|*.XLS| Excel files (*.XLSX)|*.XLSX|' ##NO_TEXT
                                              CHANGING    file_table              = lt_fich
                                                          rc                      = lv_rcode
                                                          user_action             = lv_usera
                                              EXCEPTIONS  file_open_dialog_failed = 1
                                                          cntl_error              = 2
                                                          error_no_gui            = 3
                                                          not_supported_by_gui    = 4
                                                          OTHERS                  = 5 ).

  IF sy-subrc = 0.
    IF lv_usera = 0 AND lv_rcode >= 1.
      READ TABLE lt_fich INTO ls_fich INDEX 1.
      cv_fich = ls_fich-filename.
    ELSE.
      "A operação foi cancelada pelo usuário
      MESSAGE s643(bt) DISPLAY LIKE lc_err.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form contrl_ecra_frm
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM contrl_ecra_frm .

  DATA: lr_mod_id_show TYPE ty_rg_mod_id.
  DATA: lr_mod_id_hide TYPE ty_rg_mod_id.

*  adiciona campos a mostrar/esconder quando selecionada a área Ferramentas
  PERFORM add_mod_id_frm  CHANGING  lr_mod_id_show
                                    lr_mod_id_hide.

  CASE abap_true.
    WHEN r_fichfl.

*      adiciona campos a mostrar/esconder quando selecionada a opção Carregamento ID's de contrato FCT da área Ferramentas
      PERFORM add_mod_id_ss_fichfl CHANGING lr_mod_id_show
                                            lr_mod_id_hide.

    WHEN r_fichsl.

*      adiciona campos a mostrar/esconder quando selecionada a opção Carregamento ID's de vínculo SS da área Ferramentas
      PERFORM add_mod_id_ss_fichsl CHANGING lr_mod_id_show
                                            lr_mod_id_hide.

    WHEN r_cons.

*      adiciona campos a mostrar/esconder quando selecionada a opção Consulta Log da área Ferramentas
      PERFORM add_mod_id_ss_cons CHANGING lr_mod_id_show
                                          lr_mod_id_hide.

  ENDCASE.

*   modifica o ecrã para os campos a mostrar/esconder
  PERFORM show_field USING lr_mod_id_show.
  PERFORM hide_field USING lr_mod_id_hide.

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
*    para comunicações
    WHEN r_com.
      CASE abap_true.
*    ação para FCT
        WHEN r_fct.
          CASE abap_true.
*        admissão
            WHEN r_admi.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_admissao.

*        cessação
            WHEN r_cess.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_cessacao.

*        modificação
            WHEN r_mod.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_modificacao.

            WHEN OTHERS.
              CLEAR: cv_acao.
          ENDCASE.

*        ação para segurança social
        WHEN r_ss.
          CASE abap_true.
*            vínculo
            WHEN r_vincs.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_ss_vinculo.

*            alteração de vínculo
            WHEN r_mods.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_ss_modificacao.

*            cessação de vínculo
            WHEN r_cesss.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_ss_cessacao.

*            consulta contrato
            WHEN r_consc.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_ss_cons_cont.

          ENDCASE.
      ENDCASE.

*    para ferramentas
    WHEN r_frm.
      CASE abap_true.
*        ficheiro FCT para log
        WHEN r_fichfl.
          cv_acao = zcl_hcm_fct_cockpit=>ac_acao_fct_fich_log.

*        ficheiro SS para log
        WHEN r_fichsl.
          cv_acao = zcl_hcm_fct_cockpit=>ac_acao_ss_fich_log.

*        consulta de logs
        WHEN r_cons.
          cv_acao = zcl_hcm_fct_cockpit=>ac_acao_consulta.
        WHEN OTHERS.
      ENDCASE.

*    Admissões via ficheiro
    WHEN r_fcom.
      CASE abap_true.
*        área FCT
        WHEN r_aff.
          CASE abap_true.
*            execução online
            WHEN r_affo.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_fct_fich_com_admi.
*            execução background
            WHEN r_affb.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_fct_fich_com_admi_bck.

            WHEN OTHERS.
          ENDCASE.
*        área SS
        WHEN r_afs.
          CASE abap_true.
*            execução online
            WHEN r_afso.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_ss_fich_com_vinc.
*            execução background
            WHEN r_afsb.
              cv_acao = zcl_hcm_fct_cockpit=>ac_acao_ss_fich_com_vinc_bck.

            WHEN OTHERS.
          ENDCASE.
        WHEN OTHERS.

      ENDCASE.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_fct_admin
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LO_DADOS
*&---------------------------------------------------------------------*
FORM processa_fct_admi  USING     is_p0000  TYPE p0000
                        CHANGING  co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                  cv_erro   TYPE flag.
  DATA: lv_usa_it0052 TYPE flag,
        ls_p0008      TYPE p0008.

*  verifica se a medida está intervalo de medidas de admissão FCT
  CHECK is_p0000-massn IN s_massna.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Remuneração base
  rp_provide_from_last p0008 space is_p0000-begda is_p0000-begda.

*  se os dados de renumeração estão no IT0052
  IF p0052[] IS NOT INITIAL.
*  obtém ultimo registo de dados de Remuneração base
    rp_provide_from_last p0052 space is_p0000-begda is_p0000-begda.

    MOVE-CORRESPONDING p0052 TO ls_p0008.

    ls_p0008-bsgrd = p0008-bsgrd.

    lv_usa_it0052 = abap_true.

  ELSE.
    ls_p0008 = p0008.
  ENDIF.

*  obtém ultimo registo de dados de Elementos do contrato
  rp_provide_from_last p0016 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Atribuição organizacional
  rp_provide_from_last p0001 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Tempo de trabalho teórico
  rp_provide_from_last p0007 space is_p0000-begda is_p0000-begda.

*  só executa se tiver NISS
  IF p0332-ssnum IS INITIAL.
    RETURN.
  ENDIF.

*  só executa se não for um registo já validado
  IF co_dados->valida_ultimo_ativo( iv_ssnum  = p0332-ssnum
                                    iv_bukrs  = p0001-bukrs ) EQ abap_true.
    RETURN.
  ENDIF.

  TRY.
*  valida duração mínima do contrato
      co_dados->valida_duracao_contrato( is_p0016  = p0016
                                         iv_ssnum  = p0332-ssnum
                                         iv_pernr  = is_p0000-pernr ).

*      prepara dados para a admissão
      co_dados->prep_dados_admi( iv_ssnum       = p0332-ssnum
                                 iv_pernr       = is_p0000-pernr
                                 iv_modalidade  = p0016-cttyp
                                 iv_ctbdt       = zcl_hcm_fct_cockpit=>obtem_data_ini_contrato( p0016 )
                                 iv_ctedt       = p0016-ctedt
                                 is_p0008       = ls_p0008
                                 ir_ret         = CORRESPONDING #( s_reta[] ) " rúbricas de retribuções
                                 ir_diu         = CORRESPONDING #( s_diua[] ) " rúbricas de diuturnidades
                                 is_p0001       = p0001
                                 is_p0007       = p0007
                                 iv_begda       = is_p0000-begda
                                 iv_endda       = is_p0000-begda
                                 iv_usa_it0052  = lv_usa_it0052 ).

      IF p_envfct IS NOT INITIAL.
*       envia dados e atualiza logs
        co_dados->atualiza_registo_admi( ).
      ENDIF.

    CATCH zcx_hcm_fct_cockpit.
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_fct_cess
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LO_DADOS
*&---------------------------------------------------------------------*
FORM processa_fct_cess  USING     is_p0000  TYPE p0000
                        CHANGING  co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                  cv_erro   TYPE flag.

  co_dados->prep_anulacao_flag( iv_anul_acao = p_cesnul ).
  co_dados->prep_transf_empresa_flag( iv_transf_empresa = p_trcfct ).

*  verifica se a medida está intervalo de medidas de cessação FCT
  CHECK is_p0000-massn IN s_massnc.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Atribuição organizacional
  DATA(lv_datum) = is_p0000-begda - 1.
  rp_provide_from_last p0001 space lv_datum lv_datum.

*  para uma cessação, só executa se não for um registo já validado
  IF p_cesnul IS INITIAL AND co_dados->valida_ultimo_ativo_cess(  iv_ssnum  = p0332-ssnum
                                                                  iv_bukrs  = p0001-bukrs ) NE abap_true.
    RETURN.

*  para a anulação da cessação, só executa se não for um registo já validado
  ELSEIF p_cesnul IS NOT INITIAL AND co_dados->valida_ultimo_ativo_cess_anul( iv_ssnum  = p0332-ssnum
                                                                              iv_bukrs  = p0001-bukrs ) NE abap_true.
    RETURN.

  ENDIF.

*  para transferência de empresas, valida configuração de motivo
  IF p_trcfct IS NOT INITIAL AND co_dados->valida_motivo_transf(  is_p0000 = is_p0000
                                                                  iv_bukrs = p0001-bukrs ) NE abap_true.
    RETURN.
  ENDIF.

  TRY.
      co_dados->prep_dados_cess( is_p0000 = is_p0000 ).

      IF p_envfct IS NOT INITIAL.
*    envia dados e atualiza logs
        co_dados->atualiza_registo_cess( ).
      ENDIF.

    CATCH zcx_hcm_fct_cockpit.
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_consulta
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LO_DADOS
*&---------------------------------------------------------------------*
FORM processa_consulta  CHANGING co_dados TYPE REF TO zcl_hcm_fct_cockpit.

  DATA: lv_subobject  TYPE balsubobj.

*  valida subobjeto de log
  CASE abap_true.
    WHEN r_consf.
      lv_subobject = zcl_hcm_fct_cockpit=>ac_log_sub_obj_fct.
    WHEN r_conss.
      lv_subobject = zcl_hcm_fct_cockpit=>ac_log_sub_obj_ss.
    WHEN OTHERS.
  ENDCASE.

*    consulta log
  co_dados->consulta_log( iv_subobject  = lv_subobject  " Log de aplicação: subobjeto
                          iv_aldate_i   = p_dati        " Log de aplicação: data
                          iv_altime_i   = p_timi        " Log de aplicação: hora
                          iv_aldate_f   = p_datf
                          iv_altime_f   = p_timf ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_ss_vinc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LO_DADOS
*&---------------------------------------------------------------------*
FORM processa_ss_vinc USING     iv_subty_niss_sub TYPE subty
                                is_p0000          TYPE p0000
                      CHANGING  co_dados          TYPE REF TO zcl_hcm_fct_cockpit
                                cv_erro           TYPE flag.

  co_dados->prep_transf_empresa_flag( iv_transf_empresa = p_trass ).

  DATA: lv_niss_sub   TYPE zppt_ssnum_sub,
        lv_usa_it0052 TYPE flag,
        ls_p0008      TYPE p0008.

*  verifica se a medida está intervalo de medidas de criação de vínculo SS
  CHECK is_p0000-massn IN s_massns.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados pessoais
  rp_provide_from_last p0002 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Remuneração base
  rp_provide_from_last p0008 space is_p0000-begda is_p0000-begda.

*  se os dados de renumeração estão no IT0052
  IF p0052[] IS NOT INITIAL.
*  obtém ultimo registo de dados de Remuneração base
    rp_provide_from_last p0052 space is_p0000-begda is_p0000-begda.

    MOVE-CORRESPONDING p0052 TO ls_p0008.

    ls_p0008-bsgrd = p0008-bsgrd.

    lv_usa_it0052 = abap_true.

  ELSE.
    ls_p0008 = p0008.
  ENDIF.

*  obtém ultimo registo de dados de Elementos do contrato
  rp_provide_from_last p0016 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Prof.Classificat. PT
  rp_provide_from_last p0337 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Atribuição organizacional
  rp_provide_from_last p0001 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Tempo de trabalho teórico
  rp_provide_from_last p0007 space is_p0000-begda is_p0000-begda.

*  só executa se tiver NISS
  IF p0332-ssnum IS INITIAL.
    RETURN.
  ENDIF.

*  só executa se não for um registo já validado
  IF co_dados->valida_ultimo_ativo_ss(  iv_ssnum  = p0332-ssnum
                                        iv_bukrs  = p0001-bukrs ) EQ abap_true.
    RETURN.
  ENDIF.

*  para transferência de empresas, valida configuração de motivo
  IF p_trass IS NOT INITIAL AND co_dados->valida_motivo_transf( is_p0000 = is_p0000
                                                                iv_bukrs = p0001-bukrs ) NE abap_true.
    RETURN.
  ENDIF.

*  obtém registo de NISS Colaborador substituído -Vínculo SS de Comunicação
  PERFORM obtem_subty_niss_sub  USING     iv_subty_niss_sub
                                          is_p0000
                                CHANGING  lv_niss_sub.

  TRY .
*      prepara dados para vinculo
      co_dados->prep_dados_vinc_ss( iv_ssnum          = p0332-ssnum                                           " Nº Beneficiário SS
                                    iv_gbdat          = p0002-gbdat                                           " Data de nascimento
                                    iv_modalidade     = p0016-cttyp                                           " Modalidade de contrato de trabalho
                                    iv_ctbdt          = zcl_hcm_fct_cockpit=>obtem_data_ini_contrato( p0016 ) " Início de contrato
                                    iv_ctedt          = p0016-ctedt                                           " Fim do contrato
                                    is_p0008          = ls_p0008                                              " Reg.mestre HR infotipo 0008 (Remuneração base)
                                    ir_ret            = CORRESPONDING #( s_rets[] )                           " rúbricas de retribuções
                                    ir_diu            = CORRESPONDING #( s_dius[] )                           " rúbricas de diuturnidades
                                    iv_prcnp          = p0337-prcnp                                           " Classificação de Profissões (CNP ou CPP)
                                    is_p0007          = p0007
                                    iv_ssnum_subs     = lv_niss_sub                                           " Nº Beneficiário SS
                                    is_p0000          = is_p0000
                                    is_p0001          = p0001
                                    iv_begda          = is_p0000-begda
                                    iv_endda          = is_p0000-begda
                                    iv_usa_it0052     = lv_usa_it0052 ).

      IF p_envss IS NOT INITIAL.
*        envia dados e atualiza log
        co_dados->atualiza_registo_vinc_ss( ).
      ENDIF.

    CATCH zcx_hcm_fct_cockpit.
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_ficheiro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LO_DADOS
*&---------------------------------------------------------------------*
FORM processa_fct_fich_admi USING     iv_acao   TYPE zhcm_fct_acao
                            CHANGING  co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                      cv_erro   TYPE flag.

  DATA: lv_fich LIKE p_fichfl.

  CASE iv_acao.
*    para ficheiro em servidor
    WHEN zcl_hcm_fct_cockpit=>ac_acao_fct_fich_com_admi_bck.
      lv_fich = p_fichfb.

*    para ficheiro local de comunicação
    WHEN zcl_hcm_fct_cockpit=>ac_acao_fct_fich_com_admi.
      lv_fich = p_fichfo.

*     para ficheiro local de comunicação de log
    WHEN zcl_hcm_fct_cockpit=>ac_acao_fct_fich_log.
      lv_fich = p_fichfl.
  ENDCASE.

*  prepara log
  co_dados->prep_log( ).

*  lê o ficheiro
  co_dados->integra_fct_fich_admi(  EXPORTING iv_nome_ficheiro  = lv_fich
                                    IMPORTING ev_erro           = cv_erro ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form prep_subty_niss_sub
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_SUBTY_NISS_SUB
*&---------------------------------------------------------------------*
FORM prep_subty_niss_sub  CHANGING cv_subty_niss_sub TYPE subty.

*  obtem subtipo para SS substituo
  SELECT SINGLE low
    FROM tvarvc
    INTO @DATA(lv_low)
    WHERE name  EQ 'ZFCT_NISS_SUB'  AND
          type  EQ 'P'              AND
          numb  EQ '0000'.

  cv_subty_niss_sub = lv_low.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_ss_mod_vinc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SUBTY_NISS_SUB
*&      <-- LO_DADOS
*&      <-- LV_ERRO
*&---------------------------------------------------------------------*
FORM processa_ss_mod_vinc USING     iv_subty_niss_sub TYPE subty
                                    is_p0000          TYPE p0000
                          CHANGING  co_dados          TYPE REF TO zcl_hcm_fct_cockpit
                                    cv_erro           TYPE flag
                                    ct_it_metadata    TYPE ty_it_metadata_tt.

  DATA: lv_niss_sub   TYPE zppt_ssnum_sub,
        lv_usa_it0052 TYPE flag,
        ls_p0008      TYPE p0008.

*  verifica se a medida está intervalo de medidas de modificação de vículo SS
  CHECK is_p0000-massn IN s_masnsm.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Remuneração base
  rp_provide_from_last p0008 space is_p0000-begda is_p0000-begda.

*  se os dados de renumeração estão no IT0052
  IF p0052[] IS NOT INITIAL.
*  obtém ultimo registo de dados de Remuneração base
    rp_provide_from_last p0052 space is_p0000-begda is_p0000-begda.

    MOVE-CORRESPONDING p0052 TO ls_p0008.

    ls_p0008-bsgrd = p0008-bsgrd.

    lv_usa_it0052 = abap_true.

  ELSE.

    ls_p0008 = p0008.
  ENDIF.

*  obtém ultimo registo de dados de Elementos do contrato
  rp_provide_from_last p0016 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Prof.Classificat. PT
  rp_provide_from_last p0337 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Atribuição organizacional
  rp_provide_from_last p0001 space is_p0000-begda is_p0000-begda.

*  obtém ultimo registo de dados de Tempo de trabalho teórico
  rp_provide_from_last p0007 space is_p0000-begda is_p0000-begda.

*  só executa se tiver NISS
  IF p0332-ssnum IS INITIAL.
    RETURN.
  ENDIF.

*  só executa se for um registo já validado
  IF co_dados->valida_ultimo_ativo_ss(  iv_ssnum  = p0332-ssnum
                                        iv_bukrs  = p0001-bukrs
                                        iv_ctbdt  = is_p0000-begda ) NE abap_true.
    RETURN.
  ENDIF.

*  obtém registo de NISS Colaborador substituído -Vínculo SS de Comunicação
  PERFORM obtem_subty_niss_sub  USING     iv_subty_niss_sub
                                          is_p0000
                                CHANGING  lv_niss_sub.

  TRY .
*      prepara dados para vinculo
      co_dados->prep_dados_vinc_ss_mod( iv_ssnum      = p0332-ssnum
                                        iv_modalidade = p0016-cttyp
                                        iv_ctbdt      = is_p0000-begda
                                        iv_ctedt      = p0016-ctedt
                                        is_p0008      = ls_p0008
                                        ir_ret        = CORRESPONDING #( s_retsm[] )
                                        ir_diu        = CORRESPONDING #( s_diusm[] )
                                        iv_prcnp      = p0337-prcnp
                                        is_p0007      = p0007
                                        iv_ssnum_subs = lv_niss_sub
                                        is_p0000      = is_p0000
                                        is_p0001      = p0001
                                        iv_begda      = is_p0000-begda
                                        iv_endda      = is_p0000-begda
                                        iv_usa_it0052 = lv_usa_it0052 ).

*      remove o registo equivalente do IT0008
      PERFORM remove_it0008 USING     is_p0000-begda
                            CHANGING  ct_it_metadata.

      IF p_envss IS NOT INITIAL.
*        envia dados e atualiza log
        co_dados->atualiza_registo_mod_vin_ss( ).
      ENDIF.

    CATCH zcx_hcm_fct_cockpit.
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_ss_cess_vinc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SUBTY_NISS_SUB
*&      <-- LO_DADOS
*&      <-- LV_ERRO
*&---------------------------------------------------------------------*
FORM processa_ss_cess_vinc  USING     is_p0000  TYPE p0000
                            CHANGING  co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                      cv_erro   TYPE flag.

  co_dados->prep_transf_empresa_flag( iv_transf_empresa = p_trcss ).

*  verifica se a medida está intervalo de medidas de cessação de vículo SS
  CHECK is_p0000-massn IN s_masnsc.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space is_p0000-begda is_p0000-begda.

*  obtém registo de dados de Atribuição organizacional à data prévia à cessão
  DATA(lv_datum) = is_p0000-begda - 1.
  rp_provide_from_last p0001 space lv_datum lv_datum.

*  só executa se tiver NISS
  IF p0332-ssnum IS INITIAL.
    RETURN.
  ENDIF.

*  só executa se for um registo já validado
  IF co_dados->valida_ultimo_ativo_ss(  iv_ssnum  = p0332-ssnum
                                        iv_bukrs  = p0001-bukrs ) NE abap_true.
    RETURN.
  ENDIF.

*  para transferência de empresas, valida configuração de motivo
  IF p_trcss IS NOT INITIAL AND co_dados->valida_motivo_transf( is_p0000 = is_p0000
                                                                iv_bukrs = p0001-bukrs ) NE abap_true.
    RETURN.
  ENDIF.

  TRY .
*      prepara dados para vinculo
      co_dados->prep_dados_vinc_ss_cess(  iv_ssnum  = p0332-ssnum     " Nº Beneficiário SS
                                          iv_bukrs  = p0001-bukrs
                                          is_p0000  = is_p0000
                                          iv_begda  = is_p0000-begda
                                          iv_endda  = is_p0000-begda ).

      IF p_envss IS NOT INITIAL.
*        envia dados e atualiza log
        co_dados->atualiza_registo_cess_vin_ss( ).
      ENDIF.

    CATCH zcx_hcm_fct_cockpit.
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form contrl_ecra_com
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM contrl_ecra_com .

  DATA: lr_mod_id_show TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFR'
                      CHANGING  lr_mod_id_show.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MSR'
                      CHANGING  lr_mod_id_show.

*  modifica o ecrã para os campos a mostrar
  PERFORM show_field USING lr_mod_id_show.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form prepara_p0000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_USA_IT0302
*&      <-- LS_P0000
*&---------------------------------------------------------------------*
FORM prepara_p0000  USING    iv_usa_it0302  TYPE flag
                    CHANGING ct_p0000       TYPE p0000_tab.

  CLEAR: ct_p0000.

*  se for um sistema que usa o IT0302
  IF iv_usa_it0302  IS NOT INITIAL.

    MOVE-CORRESPONDING p0302[] TO ct_p0000[].

*  se não usa o IT0302
  ELSE.

    MOVE-CORRESPONDING p0000[] TO ct_p0000[].
  ENDIF.

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

  s_diua = VALUE #( sign = 'I' option = 'EQ' low = '' ).
  APPEND s_diua.

  s_dium = VALUE #( sign = 'I' option = 'EQ' low = '' ).
  APPEND s_dium.

  s_dius = VALUE #( sign = 'I' option = 'EQ' low = '' ).
  APPEND s_dius.

  s_diusm = VALUE #( sign = 'I' option = 'EQ' low = '' ).
  APPEND s_diusm.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form medidas_vazio
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM medidas_vazio .

  s_massna = VALUE #( sign = 'I' option = 'EQ' low = '' ).
  APPEND s_massna.

  s_massnc = VALUE #( sign = 'I' option = 'EQ' low = '' ).
  APPEND s_massnc.

  s_massns = VALUE #( sign = 'I' option = 'EQ' low = '' ).
  APPEND s_massns.

  s_masnsm = VALUE #( sign = 'I' option = 'EQ' low = '' ).
  APPEND s_masnsm.

  s_masnsc = VALUE #( sign = 'I' option = 'EQ' low = '' ).
  APPEND s_masnsc.

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

  APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO s_reta[].

  APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO s_retm[].

  APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO s_rets[].

  APPEND VALUE #( sign = 'I' option = 'EQ' low = '' ) TO s_retsm[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LR_MOD_ID_SHOW
*&---------------------------------------------------------------------*
FORM add_mod_id  USING    iv_mod_id TYPE char3
                 CHANGING cr_mod_id TYPE ty_rg_mod_id.

  APPEND VALUE #( sign = 'I' option = 'EQ' low = iv_mod_id ) TO cr_mod_id.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_fct
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_fct  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                              cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFC'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'ENV'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFF'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFL'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFG'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSB'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSP'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFO'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADF'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFV'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFJ'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_fct_admi
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_fct_admi  CHANGING  cr_mod_id_show  TYPE ty_rg_mod_id
                                    cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MA'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MM'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_fct_cess
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_fct_cess  CHANGING  cr_mod_id_show  TYPE ty_rg_mod_id
                                    cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MC'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MM'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_fct_mod
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_fct_mod  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                  cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MM'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MC'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                             cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MSS'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFS'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MM'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFF'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFL'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFG'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ENV'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFO'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADF'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFV'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_vincs
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_vincs  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                   cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MSS'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFJ'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSB'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSP'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_mods
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_mods  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                  cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MSA'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFJ'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSB'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSP'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_pers
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_pers  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                  cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MSP'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFJ'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSB'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSA'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_cesss
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_cesss  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                   cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MSB'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFJ'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSP'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_frm
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_frm  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                              cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFO'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MM'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ENV'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSP'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSB'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFR'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSR'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADF'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFJ'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_fichfl
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_fichfl  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                    cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFL'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFG'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFV'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_cons
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_cons  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                  cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFG'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFL'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFV'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form contrl_ecra_fcom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM contrl_ecra_fcom .

  DATA: lr_mod_id_show TYPE ty_rg_mod_id.
  DATA: lr_mod_id_hide TYPE ty_rg_mod_id.

*  adiciona campos a mostrar/esconder quando selecionada a área Admissões via ficheiro
  PERFORM add_mod_id_fcom CHANGING  lr_mod_id_show
                                    lr_mod_id_hide.

  CASE abap_true.
    WHEN r_aff.

*      adiciona campos a mostrar/esconder quando selecionada a opção Área FCT da área Admissões via ficheiro
      PERFORM add_mod_id_ss_aff CHANGING  lr_mod_id_show
                                          lr_mod_id_hide.

    WHEN r_afs.

*      adiciona campos a mostrar/esconder quando selecionada a opção Área SS da área Admissões via ficheiro
      PERFORM add_mod_id_ss_afs CHANGING  lr_mod_id_show
                                          lr_mod_id_hide.

  ENDCASE.

*   modifica o ecrã para os campos a mostrar/esconder
  PERFORM show_field USING lr_mod_id_show.
  PERFORM hide_field USING lr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_fcom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_fcom  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                               cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'ADC'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFO'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFL'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFF'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFG'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MM'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MC'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ENV'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSP'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSB'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFR'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSR'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFV'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFJ'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_aff
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_aff  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                 cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'ADF'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADS'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_afs
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_afs  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                 cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'ADS'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'ADF'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form obtem_nome_fichserv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FICHFB
*&---------------------------------------------------------------------*
FORM obtem_nome_fichserv  CHANGING cv_fich  TYPE localfile.

  CONSTANTS: cv_dpath  TYPE dxlpath VALUE '/usr/sap/'.

  DATA: ls_rfci   TYPE rfcsi,
        lv_ipath  TYPE dxlpath,
        lv_server TYPE msname2.

  CALL FUNCTION 'RFC_SYSTEM_INFO'
    IMPORTING
      rfcsi_export = ls_rfci.

  lv_server = ls_rfci-rfcdest.

* Application Server
* Let user choose file name
  CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
    EXPORTING
      i_location_flag = 'A'
      i_server        = lv_server
      i_path          = cv_dpath
    IMPORTING
      o_path          = lv_ipath.

  cv_fich = lv_ipath.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_fct_fich_serv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LO_DADOS
*&      <-- LV_ERRO
*&---------------------------------------------------------------------*
FORM processa_fct_fich_serv  CHANGING co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                      cv_erro   TYPE flag.

*  prepara log
  co_dados->prep_log( ).

*  lê o ficheiro
  co_dados->integra_fct_fich_admi_serv( EXPORTING iv_nome_ficheiro  = p_fichfb
                                        IMPORTING ev_erro           = cv_erro ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_fichsl
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_fichsl  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                    cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a mostrar
  PERFORM add_mod_id  USING     'MFV'
                      CHANGING  cr_mod_id_show.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFG'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFL'
                      CHANGING  cr_mod_id_hide.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_ss_fich_admi
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ACAO
*&      <-- LO_DADOS
*&      <-- LV_ERRO
*&---------------------------------------------------------------------*
FORM processa_ss_fich_admi  USING     iv_acao   TYPE zhcm_fct_acao
                            CHANGING  co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                      cv_erro   TYPE flag.

  DATA: lv_fich LIKE p_fichsl.

  CASE iv_acao.
*    para ficheiro em servidor
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_fich_com_vinc_bck.
      lv_fich = p_fichsb.

*    para ficheiro local de comunicação
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_fich_com_vinc.
      lv_fich = p_fichso.

*     para ficheiro local de comunicação de log
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_fich_log.
      lv_fich = p_fichsl.
  ENDCASE.

*  prepara log
  co_dados->prep_log( ).

*  lê o ficheiro
  co_dados->integra_ss_fich_vinc( EXPORTING iv_nome_ficheiro  = lv_fich
                                  IMPORTING ev_erro           = cv_erro ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_ss_fich_serv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LO_DADOS
*&      <-- LV_ERRO
*&---------------------------------------------------------------------*
FORM processa_ss_fich_serv  CHANGING  co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                      cv_erro   TYPE flag.

*  prepara log
  co_dados->prep_log( ).

*  lê o ficheiro
  co_dados->integra_ss_fich_vinc_serv(  EXPORTING iv_nome_ficheiro  = p_fichsb
                                        IMPORTING ev_erro           = cv_erro ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_mod_id_ss_consc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LR_MOD_ID_SHOW
*&      <-- LR_MOD_ID_HIDE
*&---------------------------------------------------------------------*
FORM add_mod_id_ss_consc  CHANGING cr_mod_id_show  TYPE ty_rg_mod_id
                                   cr_mod_id_hide  TYPE ty_rg_mod_id.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MFJ'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSB'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSS'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSA'
                      CHANGING  cr_mod_id_hide.

*  adiciona campos a esconder
  PERFORM add_mod_id  USING     'MSP'
                      CHANGING  cr_mod_id_hide.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_ss_const_vinc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_P0000
*&      <-- LO_DADOS
*&      <-- LV_ERRO
*&---------------------------------------------------------------------*
FORM processa_ss_const_vinc CHANGING ct_ssnum TYPE zcl_hcm_fct_cockpit=>ty_ssnum_tt.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space pn-begda pn-endda.

*  obtém ultimo registo de dados de Atribuição organizacional
  rp_provide_from_last p0001 space pn-begda pn-endda.

*  só executa se tiver NISS
  IF p0332-ssnum IS INITIAL.
    RETURN.
  ENDIF.

  APPEND VALUE #( ssnum = p0332-ssnum pernr = p0001-pernr bukrs = p0001-bukrs ) TO ct_ssnum.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form consulta_ss_contrato
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SSNUM
*&---------------------------------------------------------------------*
FORM consulta_ss_contrato CHANGING  ct_ssnum  TYPE zcl_hcm_fct_cockpit=>ty_ssnum_tt
                                    co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                    cv_erro   TYPE flag.

  SORT ct_ssnum BY bukrs pernr  ASCENDING.

  TRY.
      co_dados->processa_ss_contratos(  iv_begda  = pn-begda      " Início da validade
                                        iv_endda  = pn-endda      " Fim da validade
                                        it_ssnum  = ct_ssnum ).

    CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_nao_colab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ACAO
*&      <-- LV_RETURN
*&---------------------------------------------------------------------*
FORM processa_nao_colab  USING    iv_acao   TYPE zhcm_fct_acao
                         CHANGING co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                  cv_erro   TYPE flag
                                  cv_return TYPE flag.

*  não obtem os dados dos empregados para estas ações
  CASE iv_acao.
*    consulta
    WHEN zcl_hcm_fct_cockpit=>ac_acao_consulta.

      PERFORM processa_consulta CHANGING co_dados.

      cv_return = abap_true.

*      sai do programa
      RETURN.

*    integração de ficheiro FCT para log e comunicação online
    WHEN zcl_hcm_fct_cockpit=>ac_acao_fct_fich_log OR zcl_hcm_fct_cockpit=>ac_acao_fct_fich_com_admi.

      co_dados = NEW zcl_hcm_fct_cockpit( iv_acao = iv_acao ).

      PERFORM processa_fct_fich_admi  USING     iv_acao
                                      CHANGING  co_dados
                                                cv_erro.

      co_dados->mostra_log_online( ).

      cv_return = abap_true.

      RETURN.

*    integração de ficheiro FCT para ficheiro no servidor
    WHEN zcl_hcm_fct_cockpit=>ac_acao_fct_fich_com_admi_bck.

      co_dados = NEW zcl_hcm_fct_cockpit( iv_acao = iv_acao ).

      PERFORM processa_fct_fich_serv CHANGING co_dados
                                              cv_erro.

*      execução em background
      IF sy-batch EQ abap_true.
        co_dados->guarda_log( ).

      ELSE.
        co_dados->mostra_log_online( ).
      ENDIF.

      cv_return = abap_true.

*      sai do programa
      RETURN.

*    integração de ficheiro SS para log e comunicação online
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_fich_log OR zcl_hcm_fct_cockpit=>ac_acao_ss_fich_com_vinc.

      co_dados = NEW zcl_hcm_fct_cockpit( iv_acao = iv_acao ).

      PERFORM processa_ss_fich_admi USING     iv_acao
                                    CHANGING  co_dados
                                              cv_erro.

      co_dados->mostra_log_online( ).

      cv_return = abap_true.

*      sai do programa
      RETURN.

*    integração de ficheiro SS para comunicação servidor
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_fich_com_vinc_bck.

      co_dados = NEW zcl_hcm_fct_cockpit( iv_acao = iv_acao ).

      PERFORM processa_ss_fich_serv CHANGING  co_dados
                                              cv_erro.

      co_dados->mostra_log_online( ).

      cv_return = abap_true.

*      sai do programa
      RETURN.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processo_colab
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ACAO
*&      --> LS_P0000
*&      <-- LO_DADOS
*&      <-- LV_ERRO
*&---------------------------------------------------------------------*
FORM processo_colab USING     iv_acao           TYPE zhcm_fct_acao
                              is_p0000          TYPE p0000
                              iv_subty_niss_sub TYPE subty
                    CHANGING  ct_ssnum          TYPE zcl_hcm_fct_cockpit=>ty_ssnum_tt
                              co_dados          TYPE REF TO zcl_hcm_fct_cockpit
                              cv_erro           TYPE flag
                              ct_it_metadata    TYPE ty_it_metadata_tt.

*  para medidas com data de início dentro do intervalo indicado
  CHECK is_p0000-begda BETWEEN pn-begda AND pn-endda.

*  valida ação
  CASE iv_acao.

*    para admissão FCT
    WHEN zcl_hcm_fct_cockpit=>ac_acao_admissao.
      PERFORM processa_fct_admi USING     is_p0000
                                CHANGING  co_dados
                                          cv_erro.

*    para cessação FCT
    WHEN zcl_hcm_fct_cockpit=>ac_acao_cessacao.
      PERFORM processa_fct_cess USING     is_p0000
                                CHANGING  co_dados
                                          cv_erro.

*    para vínculo segurança social
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_vinculo.
      PERFORM processa_ss_vinc  USING     iv_subty_niss_sub
                                          is_p0000
                                CHANGING  co_dados
                                          cv_erro.

*    para modificação de vínculo segurança social
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_modificacao.
      PERFORM processa_ss_mod_vinc  USING     iv_subty_niss_sub
                                              is_p0000
                                    CHANGING  co_dados
                                              cv_erro
                                              ct_it_metadata.

*    para cessação de vínculo segurança social
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_cessacao.
      PERFORM processa_ss_cess_vinc USING     is_p0000
                                    CHANGING  co_dados
                                              cv_erro.

*    para consulta de contratos segurança social
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_cons_cont.
      PERFORM processa_ss_const_vinc  CHANGING  ct_ssnum.

*      não guarda os dados para serem apresentados em ALV
      RETURN.

    WHEN OTHERS.
      RETURN.
  ENDCASE.

*    guarda os dados para serem apresentados no ALV
  co_dados->guarda_dados_alv( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form obtem_subty_niss_sub
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_SUBTY_NISS_SUB
*&      <-- LV_NISS_SUB
*&---------------------------------------------------------------------*
FORM obtem_subty_niss_sub  USING    iv_subty_niss_sub TYPE subty
                                    is_p0000          TYPE p0000
                           CHANGING cv_niss_sub       TYPE zppt_ssnum_sub.

*  obtem número de colaborador substituto válido à data de input
*  LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<fs_p105>) WHERE subty EQ iv_subty_niss_sub AND begda LE is_p0000-begda AND endda GE is_p0000-begda.
*    EXIT.
*  ENDLOOP.

  LOOP AT p0185 ASSIGNING FIELD-SYMBOL(<fs_p185>) WHERE subty EQ iv_subty_niss_sub AND begda LE is_p0000-begda AND endda GE is_p0000-begda.
    EXIT.
  ENDLOOP.
  CHECK sy-subrc EQ 0.

*  obtém número SS válido à data de input
  SELECT ssnum
    UP TO 1 ROWS
    FROM pa0332
    INTO cv_niss_sub
    WHERE pernr EQ <fs_p185>-icnum  AND
          begda LE is_p0000-begda         AND
          endda GE is_p0000-begda
    ORDER BY PRIMARY KEY.
  ENDSELECT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form prepara_p0008
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_USA_IT0052
*&      <-- LT_P0008
*&---------------------------------------------------------------------*
FORM prepara_p0008  CHANGING cv_usa_it0052  TYPE flag
                             ct_p0008       TYPE p0008_tab.

*  se os dados de renumeração estão no IT0052 alterados no intervalo da execução
  SELECT *
    FROM pa0052
    INTO CORRESPONDING FIELDS OF TABLE ct_p0008
    WHERE pernr EQ p0001-pernr  AND
          sprps EQ abap_false   AND
          aedtm BETWEEN pn-begda AND pn-endda.

*  se existem dados
  IF ct_p0008 IS NOT INITIAL.
    cv_usa_it0052 = abap_true.

*  se não existem dados
  ELSE.
*     se os dados de renumeração estão no IT0008 alterados no intervalo da execução
    SELECT *
      FROM pa0008
      INTO CORRESPONDING FIELDS OF TABLE ct_p0008
      WHERE pernr EQ p0001-pernr  AND
            sprps EQ abap_false   AND
            aedtm BETWEEN pn-begda AND pn-endda.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form prepara_metadata
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ACAO
*&      --> LT_P0000
*&      --> LT_P0008
*&      <-- LT_IT_METADATA
*&---------------------------------------------------------------------*
FORM prepara_metadata  USING    iv_acao         TYPE zhcm_fct_acao
                                it_p0000        TYPE p0000_tab
                       CHANGING ct_it_metadata  TYPE ty_it_metadata_tt
                                cv_usa_it0052   TYPE flag
                                ct_p0008        TYPE p0008_tab.

*  percorre todas as medidas
  LOOP AT it_p0000  ASSIGNING FIELD-SYMBOL(<fs_p0000>).
    APPEND INITIAL LINE TO ct_it_metadata ASSIGNING FIELD-SYMBOL(<fs_it_metadata>).
    <fs_it_metadata>-acao   = iv_acao.
    <fs_it_metadata>-begda  = <fs_p0000>-begda.
    <fs_it_metadata>-infty  = zcl_hcm_fct_cockpit=>ac_infty_0000.
  ENDLOOP.

*  caso a ação seja uma modificação
  IF iv_acao EQ zcl_hcm_fct_cockpit=>ac_acao_ss_modificacao OR iv_acao EQ zcl_hcm_fct_cockpit=>ac_acao_modificacao.

*  prepara dados caso o sistema use o IT0052
    PERFORM prepara_p0008 CHANGING  cv_usa_it0052
                                    ct_p0008.

*    percorre todas os registos
    LOOP AT ct_p0008  ASSIGNING FIELD-SYMBOL(<fs_p0008>).
      APPEND INITIAL LINE TO ct_it_metadata ASSIGNING <fs_it_metadata>.

*      se for uma modificação da SS
      IF iv_acao EQ zcl_hcm_fct_cockpit=>ac_acao_ss_modificacao.
*        a ação deve ser Alteração de período de rendimento
        <fs_it_metadata>-acao = zcl_hcm_fct_cockpit=>ac_acao_ss_alt_per.
      ELSE.
        <fs_it_metadata>-acao   = iv_acao.
      ENDIF.
      <fs_it_metadata>-begda  = <fs_p0008>-begda.
      <fs_it_metadata>-infty  = zcl_hcm_fct_cockpit=>ac_infty_0008.
    ENDLOOP.
  ENDIF.

*  ordena as alterações de dados por ordem crescente
  SORT ct_it_metadata BY  begda infty ASCENDING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form remove_it0008
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IS_P0000_BEGDA
*&      <-- CT_IT_METADATA
*&---------------------------------------------------------------------*
FORM remove_it0008  USING     iv_begda        TYPE begda
                    CHANGING  ct_it_metadata  TYPE ty_it_metadata_tt.

  READ TABLE ct_it_metadata
  TRANSPORTING NO FIELDS
  WITH KEY  begda = iv_begda
            infty = zcl_hcm_fct_cockpit=>ac_infty_0008.

*  se encontrou o registo
  IF sy-subrc EQ 0.
*    elimina para não ser considerado no processamento posterior
    DELETE ct_it_metadata INDEX sy-tabix.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processo_colab_rend
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_ACAO
*&      --> <FS_P0008>
*&      <-- LO_DADOS
*&      <-- LV_ERRO
*&---------------------------------------------------------------------*
FORM processo_colab_rend  USING    iv_acao  TYPE zhcm_fct_acao
                                   is_p0008 TYPE p0008
                          CHANGING co_dados TYPE REF TO zcl_hcm_fct_cockpit
                                   cv_erro  TYPE flag.

*  valida ação
  CASE iv_acao.
*    para modificação FCT
    WHEN zcl_hcm_fct_cockpit=>ac_acao_modificacao.

*    para modificação FCT de rendimentos
      PERFORM processa_fct_mod_rend USING   is_p0008
                                  CHANGING  co_dados
                                            cv_erro.

*    para alteração de período de rendimento segurança social
    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss_alt_per.
      PERFORM processa_ss_alt_per_red USING     is_p0008
                                      CHANGING  co_dados
                                                cv_erro.

    WHEN OTHERS.
      RETURN.
  ENDCASE.

*  guarda os dados para serem apresentados no ALV
  co_dados->guarda_dados_alv( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_fct_mod_rend
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IS_P0008
*&      <-- CO_DADOS
*&      <-- CV_ERRO
*&---------------------------------------------------------------------*
FORM processa_fct_mod_rend  USING     is_p0008  TYPE p0008
                            CHANGING  co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                      cv_erro   TYPE flag.

  DATA: lv_usa_it0052 TYPE flag,
        ls_p0008      TYPE p0008.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space is_p0008-begda is_p0008-begda.

*  obtém ultimo registo de dados de Remuneração base
  rp_provide_from_last p0008 space is_p0008-begda is_p0008-begda.

*  se os dados de renumeração estão no IT0052
  IF p0052[] IS NOT INITIAL.
*  obtém ultimo registo de dados de Remuneração base
    rp_provide_from_last p0052 space is_p0008-begda is_p0008-begda.

    MOVE-CORRESPONDING p0052 TO ls_p0008.

    ls_p0008-bsgrd = p0008-bsgrd.

    lv_usa_it0052 = abap_true.

  ELSE.
    ls_p0008 = p0008.
  ENDIF.

*  obtém ultimo registo de dados de Atribuição organizacional
  rp_provide_from_last p0001 space is_p0008-begda is_p0008-begda.

*  obtém ultimo registo de dados de Tempo de trabalho teórico
  rp_provide_from_last p0007 space is_p0008-begda is_p0008-begda.

*  obtém ultimo registo de dados de Elementos do contrato
  rp_provide_from_last p0016 space is_p0008-begda is_p0008-begda.

*  só executa se não for um registo já validado
  IF co_dados->valida_ultimo_ativo( iv_ssnum  = p0332-ssnum
                                    iv_bukrs  = p0001-bukrs
                                    iv_ctbdt  = is_p0008-begda
                                    iv_modnul = p_modnul ) NE abap_true.
    RETURN.
  ENDIF.

  TRY.
*      prepara dados para a modificação
      co_dados->prep_dados_mod( is_p0008          = ls_p0008
                                ir_ret            = CORRESPONDING #( s_retm[] ) " rúbricas de retribuções
                                iv_ctbdt          = is_p0008-begda
                                ir_diu            = CORRESPONDING #( s_dium[] ) " rúbricas de diuturnidades
                                iv_inic_peri_rend = is_p0008-begda
                                is_p0001          = p0001
                                is_p0007          = p0007
                                iv_begda          = is_p0008-begda
                                iv_endda          = is_p0008-begda
                                iv_usa_it0052     = lv_usa_it0052 ).


      IF p_envfct IS NOT INITIAL.
*        envia dados e atualiza log
        co_dados->atualiza_registo_mod( ).
      ENDIF.

    CATCH zcx_hcm_fct_cockpit.
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form processa_ss_alt_per_red
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IS_P0008
*&      <-- CO_DADOS
*&      <-- CV_ERRO
*&---------------------------------------------------------------------*
FORM processa_ss_alt_per_red  USING     is_p0008  TYPE p0008
                              CHANGING  co_dados  TYPE REF TO zcl_hcm_fct_cockpit
                                        cv_erro   TYPE flag.

*  obtém ultimo registo de dados de segurança social
  rp_provide_from_last p0332 space is_p0008-begda is_p0008-begda.

*  obtém ultimo registo de dados de Atribuição organizacional
  rp_provide_from_last p0001 space is_p0008-begda is_p0008-begda.

*  obtém ultimo registo de dados de Remuneração base
  rp_provide_from_last p0008 space is_p0008-begda is_p0008-begda.

*  obtém ultimo registo de dados de Tempo de trabalho teórico
  rp_provide_from_last p0007 space is_p0008-begda is_p0008-begda.

*  só executa se tiver NISS
  IF p0332-ssnum IS INITIAL.
    RETURN.
  ENDIF.

*  só executa se for um registo já validado
  IF co_dados->valida_ultimo_ativo_ss(  iv_ssnum    = p0332-ssnum
                                        iv_bukrs    = p0001-bukrs
                                        iv_ctbdt    = is_p0008-begda
                                        iv_alt_per  = abap_true ) NE abap_true.
    RETURN.
  ENDIF.

  TRY .
      co_dados->prep_dados_vinc_ss_alt_per( EXPORTING iv_ssnum      = p0332-ssnum                  " Nº Beneficiário SS
                                                      is_p0000      = CORRESPONDING #( is_p0008 )  " Registro mestre HR infotipo 0000 (Medidas)
                                                      iv_bukrs      = p0001-bukrs                  " Empresa
                                                      ir_ret        = CORRESPONDING #( s_retsm[] ) " rúbricas de retribuções
                                                      ir_diu        = CORRESPONDING #( s_diusm[] ) " rúbricas de diuturnidades
                                                      is_p0008      = p0008
                                                      is_p0001      = p0001
                                                      is_p0007      = p0007
                                                      iv_begda      = is_p0008-begda
                                                      iv_endda      = is_p0008-begda
                                                      iv_usa_it0052 = abap_true ).                     " Reg.mestre HR infotipo 0008 (Remuneração base)

      IF p_envss IS NOT INITIAL.
*        envia dados e atualiza log
        co_dados->atualiza_registo_alt_per( ).
      ENDIF.

    CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho
      cv_erro = abap_true.
  ENDTRY.

ENDFORM.
