class ZHCM_FCT_CL_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces ZHCM_FCT_BADI_ITF .
protected section.
private section.

  constants CA_PREST_TRAB_TELE type ZHCM_FCT_SS_PREST_TRAB value 'T' ##NO_TEXT.
  constants CA_PREST_TRAB_TELE_PAR type ZHCM_FCT_SS_PREST_TRAB value 'A' ##NO_TEXT.
  constants CA_PREST_TRAB_PRES type ZHCM_FCT_SS_PREST_TRAB value 'P' ##NO_TEXT.
  constants CA_COM_DESMP_SIM type ZHCM_FCT_SS_COM_DESMP value '1' ##NO_TEXT.
  constants CA_COM_DESMP_NAO type ZHCM_FCT_SS_COM_DESMP value '0' ##NO_TEXT.

  methods PREP_RUB_RET_DIU_IT0008
    importing
      !IR_RET type HRPAY00_T_LGART_RANGE
      !IR_DIU type HRPAY00_T_LGART_RANGE
      !IS_P0008 type P0008
    exporting
      !EV_RETRIBUICAO type ZHCM_FCT_RTRBC
      !EV_DIUTURNIDADES type ZHCM_FCT_DIUTRND .
  methods PREP_RUB_RET_DIU_IND
    importing
      !IR_RET type HRPAY00_T_LGART_RANGE
      !IR_DIU type HRPAY00_T_LGART_RANGE
      !IS_P0008 type P0008
      !IS_P0001 type P0001
      !IS_P0007 type P0007
      !IV_USA_IT0052 type FLAG
    exporting
      !EV_NODATA type FLAG
      !EV_RETRIBUICAO type ZHCM_FCT_RTRBC
      !EV_DIUTURNIDADES type ZHCM_FCT_DIUTRND
    raising
      ZCX_HCM_FCT_COCKPIT .
  methods OBTEM_TEMPO_PARCIAL
    importing
      !IS_P0008 type P0008
      !IS_P0007 type P0007
    returning
      value(RV_PARCIAL) type ZHCM_FCT_PARCIAL .
ENDCLASS.



CLASS ZHCM_FCT_CL_BADI IMPLEMENTATION.


METHOD obtem_tempo_parcial.

  DATA(lv_conf) = zcl_hcm_fct_cockpit=>obtem_tempo_parcial_conf( ).

*  se está configurado para validação de percentagem e se a percentagem é menor que 100
  IF lv_conf EQ '1' AND is_p0008-bsgrd LT '100'.
    rv_parcial = abap_true.

*  se está configurado para validação de percentagem e se as horas são menores que 40
  ELSEIF lv_conf EQ '2' AND is_p0007-wostd LT '40'.
    rv_parcial = abap_true.
  ENDIF.

ENDMETHOD.


METHOD prep_rub_ret_diu_ind.

  DATA: lt_p0001 TYPE STANDARD TABLE OF p0001,
        lt_p0007 TYPE STANDARD TABLE OF p0007,
        lt_p0008 TYPE STANDARD TABLE OF p0008,
        lt_pbwla TYPE STANDARD TABLE OF pbwla,
        lt_p0052 TYPE STANDARD TABLE OF p0052,
        ls_p0052 LIKE LINE OF lt_p0052.

  INSERT is_p0001 INTO TABLE lt_p0001.
  INSERT is_p0007 INTO TABLE lt_p0007.
  INSERT is_p0008 INTO TABLE lt_p0008.

  IF iv_usa_it0052 IS NOT INITIAL.
    MOVE-CORRESPONDING is_p0008 TO ls_p0052.

    CALL FUNCTION 'RP_FILL_WAGE_TYPE_TABLE_EXT'
      EXPORTING
        appli                        = 'P'
        begda                        = is_p0001-begda
        endda                        = is_p0001-endda
        pernr                        = is_p0001-pernr
        infty                        = zcl_hcm_fct_cockpit=>ac_infty_0052
        subty                        = zcl_hcm_fct_cockpit=>av_it0052_subty
      TABLES
        pp0001                       = lt_p0001
        pp0007                       = lt_p0007
        pp0008                       = lt_p0008
        ppbwla                       = lt_pbwla
        pp0052                       = lt_p0052
      EXCEPTIONS
        error_at_indirect_evaluation = 1
        OTHERS                       = 2.

  ELSE.

    CALL FUNCTION 'RP_FILL_WAGE_TYPE_TABLE_EXT'
      EXPORTING
        appli                        = 'P'
        begda                        = is_p0001-begda
        endda                        = is_p0001-endda
        pernr                        = is_p0001-pernr
      TABLES
        pp0001                       = lt_p0001
        pp0007                       = lt_p0007
        pp0008                       = lt_p0008
        ppbwla                       = lt_pbwla
      EXCEPTIONS
        error_at_indirect_evaluation = 1
        OTHERS                       = 2.

  ENDIF.

  IF sy-subrc <> 0 AND sy-msgty IS NOT INITIAL.
* Implement suitable error handling here

    RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.

*  se não há dados
  ELSEIF lt_pbwla IS INITIAL.
    ev_nodata = abap_true.
    RETURN.
  ENDIF.

*  percorre todos os registos
  LOOP AT lt_pbwla ASSIGNING FIELD-SYMBOL(<fs_pbwla>).
*      se a rúbrica está parametrizada para retribuições
    IF <fs_pbwla>-lgart IN ir_ret.
      ev_retribuicao = ev_retribuicao + <fs_pbwla>-betrg.
    ENDIF.

*      se a rúbrica está parametrizada para diuturnidades
    IF <fs_pbwla>-lgart IN ir_diu.
      ev_diuturnidades = ev_diuturnidades + <fs_pbwla>-betrg.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD prep_rub_ret_diu_it0008.

  CONSTANTS: lc_lga TYPE char3 VALUE 'LGA',
             lc_bet TYPE char3 VALUE 'BET'.

  DATA: lo_strucdesc TYPE REF TO cl_abap_structdescr,
        ls_comp      TYPE abap_compdescr,
        lv_bet_char  TYPE char5.

  FIELD-SYMBOLS: <fs_lga> TYPE any,
                 <fs_bet> TYPE pad_amt7s.

*  obter os componentes da estrutura
  lo_strucdesc ?= cl_abap_typedescr=>describe_by_data( is_p0008 ).

*  percorre todos os campos da estrutura
  LOOP AT lo_strucdesc->components INTO ls_comp.

*    verifica as entradas para rúbricas
    IF ls_comp-name(3) EQ lc_lga.

*      atribui o nome da rúbrica
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_p0008  TO <fs_lga>.

      CHECK <fs_lga> IS ASSIGNED AND <fs_lga> IS NOT INITIAL.

      CONCATENATE lc_bet ls_comp-name+3(2) INTO lv_bet_char.

*      atribui o valor da rúbrica
      ASSIGN COMPONENT lv_bet_char OF STRUCTURE is_p0008  TO <fs_bet>.

      CHECK sy-subrc EQ 0.

*      se a rúbrica está parametrizada para retribuições
      IF <fs_lga> IN ir_ret.
        ev_retribuicao = ev_retribuicao + <fs_bet>.
      ENDIF.

*      se a rúbrica está parametrizada para diuturnidades
      IF <fs_lga> IN ir_diu.
        ev_diuturnidades = ev_diuturnidades + <fs_bet>.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD zhcm_fct_badi_itf~prep_com_desemp.

*  valor default
  ev_com_desmp  = zcl_hcm_fct_cockpit=>ac_com_desmp_nao.

ENDMETHOD.


METHOD zhcm_fct_badi_itf~prep_modalidade_contrato.

  DATA(lv_parcial) = obtem_tempo_parcial( is_p0008 = is_p0008
                                          is_p0007 = is_p0007 ).

  CASE iv_acao_global.
    WHEN zcl_hcm_fct_cockpit=>ac_acao_fct.
*      obtem modalidade de contrato para FCT específico para empresa
      SELECT modalidade
        UP TO 1 ROWS
        FROM zhcm_tfct_ss_cnt
        INTO cs_dados_fct-modalidade
        WHERE cttyp   EQ iv_tipo_contrato AND
              bukrs   EQ is_p0001-bukrs   AND
              parcial EQ lv_parcial       AND
              val_fct EQ abap_true
        ORDER BY modalidade.
      ENDSELECT.

*      se não encontra procura uma entrada global, sem empresa
      IF sy-subrc NE 0.
*      obtem modalidade de contrato para FCT global
        SELECT modalidade
          UP TO 1 ROWS
          FROM zhcm_tfct_ss_cnt
          INTO cs_dados_fct-modalidade
          WHERE cttyp   EQ iv_tipo_contrato AND
                bukrs   EQ ''               AND
                val_fct EQ abap_true        AND
                parcial EQ lv_parcial
          ORDER BY modalidade.
        ENDSELECT.
      ENDIF.


    WHEN zcl_hcm_fct_cockpit=>ac_acao_ss.
*      obtem modalidade de contrato para SS específico para empresa
      SELECT cnt~modalidade moda~calc_anual
        UP TO 1 ROWS
        FROM zhcm_tfct_ss_cnt AS cnt
        INNER JOIN zhcm_tfct_moda AS moda ON moda~modalidade EQ cnt~modalidade
        INTO (cs_dados_ss-modalidade, ev_calc_anual)
        WHERE cnt~cttyp   EQ iv_tipo_contrato AND
              cnt~bukrs   EQ is_p0001-bukrs   AND
              cnt~parcial EQ lv_parcial       AND
              cnt~val_ss  EQ abap_true
        ORDER BY cnt~modalidade moda~calc_anual.
      ENDSELECT.

*      se não encontra, procura uma entrada global, sem empresa
      IF sy-subrc NE 0.
*        obtem modalidade de contrato para SS global
        SELECT cnt~modalidade moda~calc_anual
          UP TO 1 ROWS
          FROM zhcm_tfct_ss_cnt AS cnt
          INNER JOIN zhcm_tfct_moda AS moda ON moda~modalidade EQ cnt~modalidade
          INTO (cs_dados_ss-modalidade, ev_calc_anual)
          WHERE cnt~cttyp   EQ iv_tipo_contrato AND
                cnt~bukrs   EQ ''               AND
                cnt~parcial EQ lv_parcial       AND
                cnt~val_ss  EQ abap_true
          ORDER BY cnt~modalidade moda~calc_anual.
        ENDSELECT.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMETHOD.


METHOD zhcm_fct_badi_itf~prep_nivel_util.

  ev_bsgrd = is_p0008-bsgrd.

ENDMETHOD.


METHOD zhcm_fct_badi_itf~prep_prest_trab.

*  valor default
  ev_prest_trab = zcl_hcm_fct_cockpit=>ac_prest_trab_pres.

ENDMETHOD.


METHOD zhcm_fct_badi_itf~prep_rub_ret_diu.

  TRY.
      prep_rub_ret_diu_ind( EXPORTING ir_ret            = ir_ret
                                      ir_diu            = ir_diu
                                      is_p0008          = is_p0008                 " Reg.mestre HR infotipo 0008 (Remuneração base)
                                      is_p0001          = is_p0001                 " Registro mestre HR: infotipo 0001 (atrib.org.)
                                      is_p0007          = is_p0007                 " Registro mestre pessoal: infotipo 0007 (tempo trab.teórico)
                                      iv_usa_it0052     = iv_usa_it0052
                            IMPORTING ev_nodata         = DATA(lv_nodata)
                                      ev_retribuicao    = ev_retribuicao
                                      ev_diuturnidades  = ev_diuturnidades ).       " Flag geral

*      se não obteve dados
      IF lv_nodata IS NOT INITIAL.
        prep_rub_ret_diu_it0008(  EXPORTING ir_ret            = ir_ret
                                            ir_diu            = ir_diu
                                            is_p0008          = is_p0008
                                  IMPORTING ev_retribuicao    = ev_retribuicao
                                            ev_diuturnidades  = ev_diuturnidades ).  " Reg.mestre HR infotipo 0008 (Remuneração base)
      ENDIF.

    CATCH zcx_hcm_fct_cockpit. " Fundos de compensação de trabalho
      RAISE EXCEPTION TYPE zcx_hcm_fct_cockpit.
  ENDTRY.

ENDMETHOD.
ENDCLASS.
