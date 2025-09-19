*&---------------------------------------------------------------------*
*&  Include           ZHR_CARREGAMENTO_MEDIDAS_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F4_GET_FILE
*&---------------------------------------------------------------------*
FORM f4_get_file  CHANGING p_file TYPE localfile.

  DATA:
    lt_filetable TYPE filetable,
    lv_rc        TYPE i.

  REFRESH: lt_filetable.
  CLEAR: lv_rc.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      multiselection          = abap_false
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
               DISPLAY LIKE 'E'
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

* Number of selected filed must be equal to one.
  CHECK lv_rc = 1.

* Access selected file
  DATA:
    ls_file TYPE file_table.

  READ TABLE lt_filetable INTO ls_file INDEX 1.
  CHECK sy-subrc = 0.

  p_file = ls_file-filename.


ENDFORM.                    " F4_GET_FILE
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_XLS
*&---------------------------------------------------------------------*
FORM upload_xls .

  DATA:
    lv_filename  LIKE  rlgrap-filename,
    lv_begin_row TYPE i,
    lv_end_col   TYPE i,
    lt_xls       TYPE TABLE OF alsmex_tabline
    .

* Verifica se foi seleccionado um processamento
  IF gv_selected_process IS INITIAL.
    MESSAGE 'Nenhum processamento seleccionado' TYPE 'S'.
    EXIT.
  ENDIF.

  lv_begin_row = gc_xls_header_lines + 1.

* Número de colunas a carregar
  CASE gv_selected_process.
    WHEN gc_comm_adm.lv_end_col = gc_xls_adm_cols.
    WHEN gc_comm_dem.lv_end_col = gc_xls_dem_cols.
    WHEN gc_comm_pos.lv_end_col = gc_xls_pos_cols.
* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
    WHEN gc_comm_aum.lv_end_col = gc_xls_aum_cols.
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------
  ENDCASE.

* Ficheiro
  lv_filename = p_file.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lv_filename
      i_begin_col             = 1
      i_begin_row             = lv_begin_row
      i_end_col               = lv_end_col
      i_end_row               = p_lines
    TABLES
      intern                  = lt_xls
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Converte ficheiro
* Número de colunas a carregar
  CASE gv_selected_process.
    WHEN gc_comm_adm.
      PERFORM conv_file_adm TABLES lt_xls.

    WHEN gc_comm_dem.
      PERFORM conv_file_dem TABLES lt_xls.

    WHEN gc_comm_pos.
      PERFORM conv_file_pos TABLES lt_xls.

* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
    WHEN gc_comm_aum.
      PERFORM conv_file_aum TABLES lt_xls.
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------
  ENDCASE.

ENDFORM.                    " UPLOAD_XLS
*&---------------------------------------------------------------------*
*&      Form  SET_ICON_INFO
*&---------------------------------------------------------------------*
FORM set_icon_info  USING  p_index TYPE syindex.

  CASE p_index.
    WHEN 1.

      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = icon_xls
          text   = 'Admissão'
*         info   = 'Medida Admissão'
          info   = 'Admissão'
        IMPORTING
          result = badm
        EXCEPTIONS
          OTHERS = 0.

    WHEN 2.

      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = icon_xls
          text   = 'Demissão'
*         info   = 'Medida Demissão'
          info   = 'Demissão'
        IMPORTING
          result = bdem
        EXCEPTIONS
          OTHERS = 0.

    WHEN 3.

      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = icon_xls
          text   = 'Medida Inicial'
*         info   = 'Medida Posição'
          info   = 'Posição'
        IMPORTING
          result = bpro
        EXCEPTIONS
          OTHERS = 0.

* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
    WHEN 4.

      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name   = icon_xls
          text   = 'Promoção'
*         info   = 'Aumento Salarial'
          info   = 'AS'
        IMPORTING
          result = baum
        EXCEPTIONS
          OTHERS = 0.
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------

  ENDCASE.

ENDFORM.                    " SET_ICON_INFO
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization .

* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* Comentado
** constroi botões
*  DO 3 TIMES.

* constroi botões
  DO 4 TIMES.
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------

    PERFORM set_icon_info USING sy-index.
  ENDDO.

* linhas por defeito
  p_lines = gc_xls_lines_default.

* informação por defeito
  ssc_com1 = '@MC@ Seleccione um processamento'.
  ssc_file = '@J2@ Ficheiro Excel:'.
  ssc_line = 'Número total de linhas do Excel a carregar'.

  CLEAR:
        ssc_com2,
        ssc_com3,
        ssc_com4,
        ssc_com5,
        ssc_com6,
        ssc_com7
        .

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
FORM at_selection_screen_output .

  LOOP AT SCREEN.
    IF screen-group1 = 'CB3'.
      screen-intensified = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " AT_SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM at_selection_screen .

  CASE sscrfields-ucomm.
    WHEN gc_comm_adm.

      PERFORM set_ssc_adm_texts.

      PERFORM set_selected_process USING sscrfields-ucomm.

    WHEN gc_comm_dem.

      PERFORM set_ssc_dem_texts.

      PERFORM set_selected_process USING sscrfields-ucomm.

    WHEN gc_comm_pos.

      PERFORM set_ssc_pos_texts.

      PERFORM set_selected_process USING sscrfields-ucomm.

* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
    WHEN gc_comm_aum.

      PERFORM set_ssc_aum_texts.

      PERFORM set_selected_process USING sscrfields-ucomm.

* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------

  ENDCASE.

ENDFORM.                    " AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SET_SELECTED_PROCESS
*&---------------------------------------------------------------------*
FORM set_selected_process  USING p_ucomm.
  gv_selected_process = p_ucomm.
ENDFORM.                    " SET_SELECTED_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SET_SSC_ADM_TEXTS
*&---------------------------------------------------------------------*
FORM set_ssc_adm_texts.

  DATA:
    lv_txt(3)
    .

* Linha 1
  ssc_com1 =
  '@LP@ Seleccionado o carregamento de Medidas de Admissão'.

* Linha 2
  ssc_com2 =
  '@DJ@ Formato Excel:'.

* Linha 3 - Linhas de cabeçalho
  lv_txt = gc_xls_header_lines.
  CONCATENATE '@HO@' lv_txt 'Linhas de cabeçalho a ignorar'
  INTO ssc_com3 SEPARATED BY space.

* Linha 4 - Coluna de início
  ssc_com4 = '@HO@ Início na coluna A'.

* Linha 5 - Formato Data
  ssc_com5 = '@HO@ Campos Data: DD.MM.AAAA'.

* Linha 6 - Formato valor
  ssc_com6 = '@HO@ Campos Valor: NNNNNNNN.DD'.

* Linha 7 - Colunas a carregar
  lv_txt = gc_xls_adm_cols.
  CONCATENATE '@HO@' lv_txt 'Total de colunas a carregar'
  INTO ssc_com7 SEPARATED BY space.

ENDFORM.                    " SET_SSC_ADM_TEXTS
*&---------------------------------------------------------------------*
*&      Form  SET_SSC_DEM_TEXTS
*&---------------------------------------------------------------------*
FORM set_ssc_dem_texts .

  DATA:
    lv_txt(3)
    .

* Linha 1
  ssc_com1 =
  '@LQ@ Seleccionado o carregamento de Medidas de Demissão'.

* Linha 2
  ssc_com2 =
  '@DJ@ Formato Excel:'.

* Linha 3 - Linhas de cabeçalho
  lv_txt = gc_xls_header_lines.
  CONCATENATE '@HO@' lv_txt 'Linhas de cabeçalho a ignorar'
  INTO ssc_com3 SEPARATED BY space.

* Linha 4 - Coluna de início
  ssc_com4 = '@HO@ Início na coluna A'.

* Linha 5 - Formato Data
  ssc_com5 = '@HO@ Campos Data: DD.MM.AAAA'.

* Linha 6 - Formato valor
  ssc_com6 = '@HO@ Campos Valor: NNNNNNNN.DD'.

* Linha 7 - Colunas a carregar
  lv_txt = gc_xls_dem_cols.
  CONCATENATE '@HO@' lv_txt 'Total de colunas a carregar'
  INTO ssc_com7 SEPARATED BY space.

ENDFORM.                    " SET_SSC_DEM_TEXTS
*&---------------------------------------------------------------------*
*&      Form  SET_SSC_POS_TEXTS
*&---------------------------------------------------------------------*
FORM set_ssc_pos_texts .

  DATA:
    lv_txt(3)
    .

* Linha 1
  ssc_com1 =
  '@L4@ Seleccionado o carregamento de Medidas Inicial'.

* Linha 2
  ssc_com2 =
  '@DJ@ Formato Excel:'.

* Linha 3 - Linhas de cabeçalho
  lv_txt = gc_xls_header_lines.
  CONCATENATE '@HO@' lv_txt 'Linhas de cabeçalho a ignorar'
  INTO ssc_com3 SEPARATED BY space.

* Linha 4 - Coluna de início
  ssc_com4 = '@HO@ Início na coluna A'.

* Linha 5 - Formato Data
  ssc_com5 = '@HO@ Campos Data: DD.MM.AAAA'.

* Linha 6 - Formato valor
  ssc_com6 = '@HO@ Campos Valor: NNNNNNNN.DD'.

* Linha 7 - Colunas a carregar
  lv_txt = gc_xls_pos_cols.
  CONCATENATE '@HO@' lv_txt 'Total de colunas a carregar'
  INTO ssc_com7 SEPARATED BY space.

ENDFORM.                    " SET_SSC_POS_TEXTS
* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
*&---------------------------------------------------------------------*
*&      Form  SET_SSC_AUM_TEXTS
*&---------------------------------------------------------------------*
FORM set_ssc_aum_texts .
  DATA: lv_txt(3).

* Linha 1
  ssc_com1 =
  '@93@ Seleccionado o carregamento da Medida de Aumento Salarial'.

* Linha 2
  ssc_com2 =
  '@DJ@ Formato Excel:'.

* Linha 3 - Linhas de cabeçalho
  lv_txt = gc_xls_header_lines.
  CONCATENATE '@HO@' lv_txt 'Linhas de cabeçalho a ignorar'
         INTO ssc_com3
         SEPARATED BY space.

* Linha 4 - Coluna de início
  ssc_com4 = '@HO@ Início na coluna A'.

* Linha 5 - Formato Data
  ssc_com5 = '@HO@ Campos Data: DD.MM.AAAA'.

* Linha 6 - Formato valor
  ssc_com6 = '@HO@ Campos Valor: NNNNNNNN.DD'.

* Linha 7 - Colunas a carregar
  lv_txt = gc_xls_aum_cols.
  CONCATENATE '@HO@' lv_txt 'Total de colunas a carregar'
         INTO ssc_com7
         SEPARATED BY space.
ENDFORM.                    " SET_SSC_AUM_TEXTS
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  CONV_FILE_ADM
*&---------------------------------------------------------------------*
FORM conv_file_adm
   TABLES pt_xls STRUCTURE alsmex_tabline.

  DATA:
    lt_admissao    LIKE gs_admissoes OCCURS 0,
    ls_admissao    LIKE gs_admissoes,
    ls_xls         TYPE alsmex_tabline,
    lv_lines       TYPE i,
    lv_row         TYPE i,
    lv_column      TYPE i,
    ls_fields      TYPE abap_compdescr,
* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* Comentado
*        lv_pernr      TYPE p0003-pernr,
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------
    lv_string(255).

* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
  DATA: lv_erro.
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------

  FIELD-SYMBOLS:
                 <lf_campo_admissao> TYPE any
               .

* Obtem o último registo, para obter o número de linhas
  DESCRIBE TABLE pt_xls LINES lv_lines.
  READ TABLE pt_xls INTO ls_xls
       INDEX lv_lines.
  lv_lines = ls_xls-row.

  CLEAR lv_row.

* Percorre as linhas
  DO lv_lines TIMES.

    CLEAR: ls_admissao, lv_column.

    ADD 1 TO lv_row.

* * * Preenche dados de linha
* Percorre os campos da estrutura,

    LOOP AT gt_fields_process INTO ls_fields.

      ADD 1 TO lv_column.

* Assigna campo da estrutura
      ASSIGN COMPONENT ls_fields-name OF STRUCTURE ls_admissao
      TO <lf_campo_admissao>.

      IF <lf_campo_admissao> IS ASSIGNED.

* Lê o campo de transferencia XLS
        READ TABLE pt_xls INTO ls_xls
           WITH KEY
           row = lv_row
           col = lv_column.

        IF sy-subrc = 0.

          PERFORM check_xls_value_field
            USING lv_row
                  lv_column
                  ls_fields-name
                  ls_fields-type_kind
                  ls_fields-decimals
                  ls_fields-length
         CHANGING ls_xls-value.

          IF ls_xls-value = gc_error.
            EXIT. " Do loop deste processamento
          ENDIF.

          <lf_campo_admissao> = ls_xls-value.
          UNASSIGN <lf_campo_admissao>.

* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* Validar conteúdos de campos do excel
          CLEAR: lv_erro.
          IF ls_xls-value IS NOT INITIAL.
            IF ls_fields-name EQ 'ANRED__P0002'
            OR ls_fields-name EQ 'GBLND__P0002'
            OR ls_fields-name EQ 'GESCH__P0002'
            OR ls_fields-name EQ 'ICTYP__P0185'
            OR ls_fields-name EQ 'KSTAR__P0027'
* >>> INI Inetum SAM EMP/GPS 7000123834 em 19.08.2021
            OR ls_fields-name EQ 'PSP01__P0027'
            OR ls_fields-name EQ 'GSBER__P0001'
* <<< END Inetum SAM EMP/GPS 7000123834 em 19.08.2021
            OR ls_fields-name EQ 'WSPSE__P0331'
            OR ls_fields-name EQ 'HSPSE__P0331'
            OR ls_fields-name EQ 'DESTA__P2006'
            OR ls_fields-name EQ 'MSTAT__P0331'.
              PERFORM valida_campos USING ls_fields-name
                                          ls_xls-value
                                          lv_erro
                                          lv_row
                                          lv_column
                                          ls_admissao-begda__p0000
                                          ls_admissao-famst__p0002.
              IF lv_erro IS NOT INITIAL.
                ls_xls-value = gc_error.
                EXIT.
              ENDIF.

* >>> INI Inetum SAM EMP/GPS 7000123834 em 19.08.2021
*              IF ls_fields-name EQ 'PSP01__P0027'.
*                ls_admissao-psp01__p0027 = ls_xls-value.
*              ENDIF.
* <<< END Inetum SAM EMP/GPS 7000123834 em 19.08.2021
            ENDIF.
          ENDIF.
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------

          IF ls_fields-name EQ 'PERNR__P0000'.
* Validar se o PERNR existe
            gv_pernr = ls_xls-value.
            SELECT SINGLE * INTO p0003
                            FROM pa0003
                            WHERE pernr EQ gv_pernr.
            IF sy-subrc EQ 0.
              CONCATENATE gv_pernr 'já existe criado' INTO lv_string
                                                      SEPARATED BY space.
              PERFORM set_error_line
                USING lv_row lv_column ''
                      lv_string.
              ls_xls-value = gc_error.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF NOT ls_xls-value = gc_error.
* Cria o registo
      APPEND ls_admissao TO lt_admissao.
      CLEAR ls_admissao.
    ENDIF.
  ENDDO.

  READ TABLE lt_admissao INDEX 1 TRANSPORTING NO FIELDS.
  CHECK sy-subrc = 0.
  APPEND LINES OF lt_admissao TO gt_admissoes.

  CLEAR: lv_lines.
  LOOP AT gt_admissoes INTO gs_admissoes.
    ADD 1 TO lv_lines.
    PERFORM generate_medidas USING lv_lines.
  ENDLOOP.

* INI - ROFF NG/SG 7000010088/5000003337
* XPRA: Conversion of Reference Personnel Numbers
  SUBMIT rpu46cx_central_person_online AND RETURN
         WITH mandt = sy-mandt.
* FIM - ROFF NG/SG 7000010088/5000003337

ENDFORM.                    " CONV_FILE_ADM
*&---------------------------------------------------------------------*
*&      Form  GENERATE_MEDIDAS
*&---------------------------------------------------------------------*
FORM generate_medidas USING p_tabix TYPE sy-tabix.

  DATA: lt_prop_values     TYPE ty_t_pprop,
        lt_prop_values_aux TYPE ty_t_pprop,
        ls_prop_values     LIKE LINE OF lt_prop_values,
        ls_modifkeys_lines TYPE i,
* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* Comentado
*        lv_pernr       TYPE p0000-pernr,
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------
        lv_begda           TYPE p0000-begda,
        lv_mstring(120),
        lv_mode            TYPE  c.

  DATA: return        LIKE bapireturn,
        return1       LIKE bapireturn1,
        hr_return     LIKE hrhrmm_msg,
        modified_keys LIKE pskey OCCURS 0 WITH HEADER LINE.

  CLEAR: lt_prop_values[], modified_keys[].

* Converte os dados para formato de entrada na HR_MAINTAIN_MASTERDATA
  CASE gv_selected_process.
    WHEN gc_comm_adm.
* >>> INI ROFF SAM EMP/GPS HR 7000070954 22.03.2019
      EXPORT p1 = sy-repid TO MEMORY ID 'ZZZ_CARREGAMENTO_MEDIDAS'.
* <<< END ROFF SAM EMP/GPS HR 7000070954 22.03.2019

      PERFORM set_param_adm CHANGING lt_prop_values.

* * * ADD ROFF SAM HR MGA/SG 7000027617 -->
      PERFORM set_adm_iban
        USING p_tabix
     CHANGING lt_prop_values.
* * * ADD ROFF SAM HR MGA/SG 7000027617 <--

    WHEN gc_comm_dem.
      PERFORM set_param_dem CHANGING lt_prop_values.
    WHEN gc_comm_pos.
      PERFORM set_param_pos CHANGING lt_prop_values.

* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
    WHEN gc_comm_aum.
      PERFORM set_param_aum CHANGING lt_prop_values.
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------
  ENDCASE.

  READ TABLE lt_prop_values INTO ls_prop_values INDEX 1.
  CHECK sy-subrc = 0.
  gv_pernr = ls_prop_values-fval.
  READ TABLE lt_prop_values INTO ls_prop_values INDEX 2.
  IF sy-subrc EQ 0.
    lv_begda = ls_prop_values-fval.
  ENDIF.

  lv_mode = '0'.

* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
  gv_begda = lv_begda.
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021

* FJHU

  lt_prop_values_aux[] = lt_prop_values[].

  IF gv_selected_process EQ gc_comm_adm.
    DELETE lt_prop_values_aux WHERE infty NE '0000'.   " FJHU
*                                and infty NE '0001'.
  ENDIF.
*lv_begda = '20230501'.
  CALL FUNCTION 'HR_MAINTAIN_MASTERDATA'
    EXPORTING
      pernr           = gv_pernr
*      massn           = 'Z1'
      actio           = 'INS'
      begda           = lv_begda
      endda           = '99991231'
*      WERKS           = '2100'
*      PERSG           = '2'
*      PERSK           = '2B'
*      PLANS           = '99999999'
      dialog_mode     = '0'   " lv_mode " '0' FJHU
*     luw_mode        = '2'
*     no_existence_check = 'X'
    IMPORTING
      return          = return
      return1         = return1
      hr_return       = hr_return
    TABLES
      proposed_values = lt_prop_values_aux
      modified_keys   = modified_keys.

  IF gv_selected_process EQ gc_comm_adm.

    DESCRIBE TABLE modified_keys LINES ls_modifkeys_lines.
    IF ls_modifkeys_lines GT 0.
      READ TABLE modified_keys INDEX 1.
      IF NOT modified_keys-pernr IS INITIAL.
*
        lt_prop_values_aux[] = lt_prop_values[].
        DELETE lt_prop_values_aux WHERE infty EQ '0000'.
*                                     or infty EQ '0001'.
*                              infty NE '0001'
*                              AND infty NE '0002'.

        gv_pernr = modified_keys-pernr.

        CALL FUNCTION 'HR_MAINTAIN_MASTERDATA'
          EXPORTING
            pernr           = gv_pernr
      massn           = 'Z1'
      actio           = 'INS'
      begda           = lv_begda
      endda           = '99991231'
      WERKS           = '2100'
      PERSG           = '2'
      PERSK           = '2B'
      PLANS           = '99999999'
            dialog_mode     = '0'   " lv_mode " '0'
*           luw_mode        = '2'
*           no_existence_check = 'X'
          IMPORTING
            return          = return
            return1         = return1
            hr_return       = hr_return
          TABLES
            proposed_values = lt_prop_values_aux
            modified_keys   = modified_keys.


        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

      ENDIF.
    ENDIF.
  ENDIF.

  IF return1 IS NOT INITIAL.
    lv_mstring = return1-message.
    CONDENSE lv_mstring.

    PERFORM set_error_line
      USING p_tabix 1 '' lv_mstring.

  ELSE.

    IF modified_keys[] IS NOT INITIAL.
* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
      IF gt_prop_values IS NOT INITIAL.
        PERFORM cartao_refeicao_it0009.
      ENDIF.
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021

      MESSAGE s000(zp) WITH 'Nº pessoal:' gv_pernr
                            'actualizado com sucesso!'.

      lv_mstring = 'actualizado com sucesso!'.

      WRITE: /
               '|', gv_pernr,
               '|', lv_mstring, AT 150 '|',
             / sy-uline(150).
    ENDIF.
  ENDIF.

* >>> INI ROFF SAM EMP/GPS HR 7000070954 22.03.2019
  FREE MEMORY ID 'ZZZ_CARREGAMENTO_MEDIDAS'.
* <<< END ROFF SAM EMP/GPS HR 7000070954 22.03.2019

ENDFORM.                    " GENERATE_MEDIDAS
*&---------------------------------------------------------------------*
*&      Form  SET_PARAM_ADM
*&---------------------------------------------------------------------*
FORM set_param_adm CHANGING p_prop_values TYPE ty_t_pprop.

  DATA:
    ls_pprop       TYPE pprop,
    ls_prop_values TYPE pprop,
    ls_fields      TYPE abap_compdescr,
    lv_plans       TYPE p0001-plans,
    lv_bukrs       TYPE p0001-bukrs,
    lv_werks       TYPE p0001-werks,
    ls_t500p       TYPE t500p,
    lv_flag.

  FIELD-SYMBOLS:
                 <lf_campo_admissao> TYPE any.

  REFRESH: p_prop_values.

  CLEAR: lv_flag.

  LOOP AT gt_fields_process INTO ls_fields.

* Converte para campos, formato interno
    PERFORM set_field_for_pprop
      USING ls_fields-name
   CHANGING ls_pprop-infty ls_pprop-fname.

* Obtém valor
    ASSIGN COMPONENT ls_fields-name
                  OF STRUCTURE gs_admissoes
                  TO <lf_campo_admissao>.

    ls_pprop-fval = <lf_campo_admissao>.

    APPEND ls_pprop TO p_prop_values.

    IF ls_pprop-infty      EQ '0001'.
*       ls_fields-name+0(5) eq 'PLANS'.      " FJHU
      IF lv_flag IS INITIAL.
*
        ls_pprop-infty = '0001'.
        ls_pprop-fname = 'P0001-BUKRS'.
        ls_pprop-fval  = lv_bukrs.
        APPEND ls_pprop TO p_prop_values.
*
        ls_pprop-infty = '0001'.
        ls_pprop-fname = 'P0001-werks'.
        ls_pprop-fval  = lv_werks.
        APPEND ls_pprop TO p_prop_values.
*
        ls_pprop-infty = '0001'.
        ls_pprop-fname = 'P0001-PLANS'.
        ls_pprop-fval  = lv_plans.
        APPEND ls_pprop TO p_prop_values.

        ls_pprop-infty = '0001'.
        ls_pprop-fname = 'PSYST-PLANS'.
        ls_pprop-fval  = lv_plans.
        APPEND ls_pprop TO p_prop_values.

        lv_flag = 'X'.
      ENDIF.
    ELSEIF ls_pprop-infty EQ '0000'.
      IF ls_pprop-fname EQ 'PSPAR-PLANS'.
        lv_plans = ls_pprop-fval.

        ls_pprop-infty = '0000'.
        ls_pprop-fname = 'PSYST-PLANS'.
        ls_pprop-fval  = lv_plans.
        APPEND ls_pprop TO p_prop_values.
      ENDIF.
*
      IF ls_pprop-fname EQ 'PSPAR-WERKS'.
*
        lv_werks = ls_pprop-fval.
        SELECT * FROM t500p INTO ls_t500p
                 WHERE persa EQ lv_werks.
        ENDSELECT.
        IF sy-subrc EQ 0.
          lv_bukrs = ls_t500p-bukrs.
        ENDIF.
      ENDIF.
*
      IF ls_pprop-fname EQ 'PSPAR-WERKS'.
*
        lv_werks = ls_pprop-fval.
      ENDIF.
*
    ELSEIF ls_pprop-infty EQ '0002'
       AND ls_pprop-fname EQ 'P0002-CNAME'.
      CLEAR: ls_prop_values.
      READ TABLE p_prop_values INTO ls_prop_values INDEX 2.
      IF sy-subrc EQ 0.
        ls_pprop-infty = '0002'.
        ls_pprop-fname = 'P0002-BEGDA'.
        ls_pprop-fval  = ls_prop_values-fval.
        APPEND ls_pprop TO p_prop_values.
      ENDIF.

    ELSEIF ls_pprop-infty EQ '0332'.
      ls_pprop-infty = '0332'.
      ls_pprop-fname = 'PSYST-NSELC'.
      ls_pprop-fval  = '0'.
      APPEND ls_pprop TO p_prop_values.

    ELSEIF ls_pprop-infty EQ '0105'.
      ls_pprop-infty = '0105'.
      ls_pprop-fname = 'P0105-SUBTY'.
      ls_pprop-fval  = '0010'.
      APPEND ls_pprop TO p_prop_values.

*      ls_pprop-infty = '0105'.
*      ls_pprop-fname = 'P0105-USRID_LONG'.
*      ls_pprop-fval = <lf_campo_admissao>.
*      APPEND ls_pprop TO p_prop_values.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " SET_PARAM_ADM
*&---------------------------------------------------------------------*
*&      Form  SET_PROCESS_FIELDS
*&---------------------------------------------------------------------*
FORM set_process_fields .

  DATA :
        lo_ref_table_des TYPE REF TO cl_abap_structdescr
        .

* Verifica se foi seleccionado um processamento
  IF gv_selected_process IS INITIAL.
    MESSAGE 'Nenhum processamento seleccionado' TYPE 'S'.
    EXIT.
  ENDIF.

  CASE gv_selected_process.
    WHEN gc_comm_adm.
      lo_ref_table_des ?=
      cl_abap_typedescr=>describe_by_data( gs_admissoes ).
    WHEN gc_comm_dem.
      lo_ref_table_des ?=
      cl_abap_typedescr=>describe_by_data( gs_demissoes ).
    WHEN gc_comm_pos.
      lo_ref_table_des ?=
      cl_abap_typedescr=>describe_by_data( gs_posicao ).
* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
    WHEN gc_comm_aum.
      lo_ref_table_des ?=
      cl_abap_typedescr=>describe_by_data( gs_aumento ).
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------
  ENDCASE.

  APPEND LINES OF lo_ref_table_des->components TO gt_fields_process.

ENDFORM.                    " SET_PROCESS_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_FOR_PPROP
*&---------------------------------------------------------------------*
FORM set_field_for_pprop  USING    p_name   TYPE abap_compname
                          CHANGING p_infty  TYPE infty
                                   p_fname  TYPE prop_fname.

  DATA:
    lt_split  TYPE TABLE OF string,
    ls_split  TYPE string,
    lv_linhas TYPE i,
    lv_aux    TYPE string
    .

  REFRESH lt_split.

  CLEAR:
         lt_split,
         ls_split,
         lv_linhas,
         lv_aux
       .

  SPLIT p_name AT gc_field_split INTO TABLE lt_split.

  DESCRIBE TABLE lt_split LINES lv_linhas.

* Verifica se é um campo de repetição
  READ TABLE lt_split INTO ls_split INDEX lv_linhas.
  IF ls_split CS gc_repc.
    DELETE lt_split INDEX lv_linhas. " Ignora a repetição

* recalcula o numero de linhas
    DESCRIBE TABLE lt_split LINES lv_linhas.

  ENDIF.

  CASE lv_linhas.
    WHEN 2.
      READ TABLE lt_split INTO ls_split INDEX 2.
      p_infty = ls_split+1.lv_aux = ls_split.

      READ TABLE lt_split INTO ls_split INDEX 1.
      CONCATENATE lv_aux ls_split INTO p_fname
      SEPARATED BY '-'.

    WHEN 3.
      READ TABLE lt_split INTO ls_split INDEX 3.
      p_infty = ls_split+1.

      READ TABLE lt_split INTO ls_split INDEX 2.
      p_fname = ls_split.

      READ TABLE lt_split INTO ls_split INDEX 1.
      CONCATENATE p_fname ls_split INTO p_fname
      SEPARATED BY '-'.

    WHEN OTHERS.
      " ERRO
  ENDCASE.


ENDFORM.                    " SET_FIELD_FOR_PPROP
*&---------------------------------------------------------------------*
*&      Form  CHECK_XLS_VALUE_FIELD
*&---------------------------------------------------------------------*
* Verfica se a conversão/assignação do campo é possível
*----------------------------------------------------------------------*
*      -->P_ROW           Linha
*      -->P_COLUMN        Coluna
*      -->P_FIELDNAME     Campo
*      -->P_TYPE          Tipo de dados
*      -->P_DECIMALS      Número decimais
*      -->P_LENGTH        Comprimento
*      <--P_VALUE         Valor / Erro
*----------------------------------------------------------------------*
FORM check_xls_value_field
                USING    p_row       TYPE i
                         p_column    TYPE i
                         p_fieldname TYPE abap_compname
                         p_type      TYPE abap_typekind
                         p_decimals  TYPE i
                         p_length    TYPE i
             CHANGING VALUE(p_value).

  FIELD-SYMBOLS: <lf_data_type> TYPE any.

  DATA:
    lv_aux    TYPE string,
    data_type TYPE REF TO cl_abap_elemdescr,
    wf_ref    TYPE REF TO data
    .

  CASE p_type.

**********************************************************************
      "         C
**********************************************************************
    WHEN 'C'.
      " Tipo C não contempla conversões, eventualmente trunca o campo
      EXIT.

**********************************************************************
      "         N
**********************************************************************
    WHEN 'N'.
      " Numérico
      IF NOT p_value CO '0987654321 '.
        PERFORM set_error_line
          USING p_row p_column p_fieldname
                'Campo não é numérico'.
        CLEAR p_value.p_value = gc_error.
        EXIT.
      ELSE.

        CALL METHOD cl_abap_elemdescr=>get_n
          EXPORTING
            p_length = p_length
          RECEIVING
            p_result = data_type.

      ENDIF.

**********************************************************************
      "         I
**********************************************************************
    WHEN 'I'.
      " Inteiro
      IF NOT p_value CO '0987654321 '.
        PERFORM set_error_line
          USING p_row p_column p_fieldname
                'Campo não é numérico'.
        CLEAR p_value.p_value = gc_error.
        EXIT.
      ELSE.

        CALL METHOD cl_abap_elemdescr=>get_i
          RECEIVING
            p_result = data_type.

      ENDIF.

**********************************************************************
      "         D
**********************************************************************
    WHEN 'D'.
      " Data
      IF NOT p_value CO '0987654321. '.
        PERFORM set_error_line
          USING p_row p_column p_fieldname
                'Campo não tem o formato data. DD.MM.AAAA'.
        CLEAR p_value.p_value = gc_error.
        EXIT.
      ELSE.
        CONDENSE p_value NO-GAPS.
        CONCATENATE p_value+6(4) p_value+3(2) p_value(2) INTO lv_aux.
        CLEAR p_value.
        p_value = lv_aux.
        EXIT.
      ENDIF.

**********************************************************************
      "         P
**********************************************************************
    WHEN 'P'.
      " Inteiro, Decimais
      IF NOT p_value CO '0987654321. '.
        PERFORM set_error_line
          USING p_row p_column p_fieldname
                'Campo não é decimal. NNNNNNN.DD'.
        CLEAR p_value.p_value = gc_error.
        EXIT.
      ELSE.

        CALL METHOD cl_abap_elemdescr=>get_p
          EXPORTING
            p_length   = p_length
            p_decimals = p_decimals
          RECEIVING
            p_result   = data_type.

      ENDIF.

**********************************************************************
      "         OUTROS
**********************************************************************
    WHEN OTHERS.
      " Não contemplado, F, T, X
      CLEAR p_value.p_value = gc_error.
      EXIT.
  ENDCASE.

* constroi campo mapeado
  CREATE DATA wf_ref TYPE HANDLE data_type.
  CHECK wf_ref IS BOUND.
  ASSIGN wf_ref->* TO <lf_data_type>.
  CHECK <lf_data_type> IS ASSIGNED.
  CONDENSE p_value NO-GAPS.
  IF p_type <> 'P'.
    UNPACK p_value TO <lf_data_type>.
  ELSE.
    <lf_data_type> = p_value.
  ENDIF.
  CLEAR p_value.
  p_value = <lf_data_type>.

ENDFORM.                    " CHECK_XLS_VALUE_FIELD
*&---------------------------------------------------------------------*
*&      Form  SET_ERROR_LINE
*&---------------------------------------------------------------------*
FORM set_error_line
              USING VALUE(p_row) VALUE(p_column)
                    VALUE(p_fieldname)
                    VALUE(p_error).

  DATA:
    ls_error   TYPE ty_error_log,
    lv_letters TYPE char26 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
    lv_aux     TYPE i,
    lv_offset1 TYPE i,
    lv_offset2 TYPE i,
    lv_letter1,
    lv_letter2
    .

* Linha
  ls_error-line = p_row.

* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* Comentado

** Coluna, converte em letra
*  lv_aux = p_column.
*
*  IF lv_aux > 26.
*    DO 99 TIMES. " Just in case
*      ADD 1 TO lv_offset1.
*      lv_aux = lv_aux - 26.
*
*      IF lv_aux <= 26.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    lv_offset1 = lv_offset1 - 1.
*    lv_offset2 = lv_aux - 1.
*
*    lv_letter1 = lv_letters+lv_offset1(1).
*    lv_letter2 = lv_letters+lv_offset2(1).
*
*    CONCATENATE lv_letter1 lv_letter2 INTO ls_error-column.
*
*  ELSE.
*    lv_offset1 = lv_aux - 1.
*    ls_error-column = lv_letters+lv_offset1(1).
*  ENDIF.

* Nº pessoal
  ls_error-pernr = gv_pernr.

  PERFORM converte_coluna USING p_column
                                ls_error-column.
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------


* Campo
  ls_error-field = p_fieldname.

* Erro
  ls_error-error = p_error.

  APPEND ls_error TO gt_error_log.

ENDFORM.                    " SET_ERROR_LINE
*&---------------------------------------------------------------------*
*&      Form  OUTPUT
*&---------------------------------------------------------------------*
FORM output .

  PERFORM error_log_xls.

ENDFORM.                    " OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ERROR_LOG_XLS
*&---------------------------------------------------------------------*
FORM error_log_xls .

  DATA:
        ls_error TYPE ty_error_log
      .

  READ TABLE gt_error_log INDEX 1 TRANSPORTING NO FIELDS.
  CHECK sy-subrc = 0.

* Cabeçalho
  WRITE:
        / sy-uline(150),
        / '|', 'Erros de conversão do carregamento EXCEL', AT 150 '|',
        / sy-uline(150),
        /
* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* Comentado
*          '|', 'Lin.',
          '|', 'Nºpessoa',
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------
          '|', 'Col.',
          '|', 'Erro', AT 150 '|',
        / sy-uline(150)
          .

  FORMAT COLOR COL_NEGATIVE.

  LOOP AT gt_error_log INTO ls_error.

    WRITE: /
* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* Comentado
*             '|', ls_error-line,
             '|', ls_error-pernr,
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------
             '|', ls_error-column,
             '|', ls_error-error, AT 150 '|',
           / sy-uline(150)
             .

  ENDLOOP.
  FORMAT COLOR OFF.

ENDFORM.                    " ERROR_LOG_xls
*&---------------------------------------------------------------------*
*&      Form  CONV_FILE_DEM
*&---------------------------------------------------------------------*
FORM conv_file_dem
   TABLES pt_xls STRUCTURE alsmex_tabline.

  DATA:
    lt_demissao    LIKE gs_demissoes OCCURS 0,
    ls_demissao    LIKE gs_demissoes,
    ls_xls         TYPE alsmex_tabline,
    lv_lines       TYPE i,
    lv_row         TYPE i,
    lv_column      TYPE i,
    ls_fields      TYPE abap_compdescr,
* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* Comentado
*        lv_pernr      TYPE p0003-pernr,
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------
    lv_begda       TYPE p0000-begda,
    lv_string(255).

  FIELD-SYMBOLS:
                 <lf_campo_demissao> TYPE any
               .

* Obtem o último registo, para obter o número de linhas
  DESCRIBE TABLE pt_xls LINES lv_lines.
  READ TABLE pt_xls INTO ls_xls
       INDEX lv_lines.
  lv_lines = ls_xls-row.

  CLEAR lv_row.

* Percorre as linhas
  DO lv_lines TIMES.

    CLEAR: ls_demissao, lv_column.

    ADD 1 TO lv_row.

* * * Preenche dados de linha
* Percorre os campos da estrutura,
    LOOP AT gt_fields_process INTO ls_fields.

      ADD 1 TO lv_column.

* Assigna campo da estrutura
      ASSIGN COMPONENT ls_fields-name OF STRUCTURE ls_demissao
      TO <lf_campo_demissao>.

      IF <lf_campo_demissao> IS ASSIGNED.

* Lê o campo de transferencia XLS
        READ TABLE pt_xls INTO ls_xls
           WITH KEY
           row = lv_row
           col = lv_column.

        IF sy-subrc = 0.

          PERFORM check_xls_value_field
            USING lv_row
                  lv_column
                  ls_fields-name
                  ls_fields-type_kind
                  ls_fields-decimals
                  ls_fields-length
         CHANGING ls_xls-value.

          IF ls_xls-value = gc_error.
            EXIT. " Do loop deste processamento
          ENDIF.

          <lf_campo_demissao> = ls_xls-value.
          UNASSIGN <lf_campo_demissao>.
        ENDIF.

        IF ls_fields-name EQ 'PERNR__P0000'.
* Validar se o PERNR existe
          gv_pernr = ls_xls-value.
          SELECT SINGLE * INTO p0003
                          FROM pa0003
                          WHERE pernr EQ gv_pernr.
          IF sy-subrc NE 0.
            CONCATENATE gv_pernr 'ainda não existe criado' INTO lv_string
                                                           SEPARATED BY space.
            PERFORM set_error_line
              USING lv_row lv_column ''
                    lv_string.
            ls_xls-value = gc_error.
            EXIT.
          ENDIF.
        ELSEIF ls_fields-name EQ 'BEGDA__P0000'.
* Validar se o PERNR já tem uma medida de demissão na data do ficheiro
          lv_begda = ls_xls-value.
          SELECT SINGLE * INTO p0000
                          FROM pa0000
                          WHERE pernr EQ gv_pernr
                            AND endda GE lv_begda
                            AND massn EQ '10'.
          IF sy-subrc EQ 0.
            CONCATENATE gv_pernr
                        'já tem uma medida de demissão à data de'
                        lv_begda INTO lv_string SEPARATED BY space.
            PERFORM set_error_line
              USING lv_row lv_column ''
                    lv_string.
            ls_xls-value = gc_error.
            EXIT.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

    IF NOT ls_xls-value = gc_error.
* Cria o registo
      APPEND ls_demissao TO lt_demissao.
      CLEAR ls_demissao.
    ENDIF.
  ENDDO.

  READ TABLE lt_demissao INDEX 1 TRANSPORTING NO FIELDS.
  CHECK sy-subrc = 0.
  APPEND LINES OF lt_demissao TO gt_demissoes.

  CLEAR: lv_lines.
  LOOP AT gt_demissoes INTO gs_demissoes.
    ADD 1 TO lv_lines.
    PERFORM generate_medidas USING lv_lines.
  ENDLOOP.

ENDFORM.                    " CONV_FILE_DEM
*&---------------------------------------------------------------------*
*&      Form  CONV_FILE_POS
*&---------------------------------------------------------------------*
FORM conv_file_pos
   TABLES pt_xls STRUCTURE alsmex_tabline.

  DATA:
    lt_posicao     LIKE gs_posicao OCCURS 0,
    ls_posicao     LIKE gs_posicao,
    ls_xls         TYPE alsmex_tabline,
    lv_lines       TYPE i,
    lv_row         TYPE i,
    lv_column      TYPE i,
    ls_fields      TYPE abap_compdescr,
* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* Comentado
*        lv_pernr      TYPE p0003-pernr,
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------
    lv_string(255).

  FIELD-SYMBOLS:
                 <lf_campo_posicao> TYPE any.

* Obtem o último registo, para obter o número de linhas
  DESCRIBE TABLE pt_xls LINES lv_lines.
  READ TABLE pt_xls INTO ls_xls
       INDEX lv_lines.
  lv_lines = ls_xls-row.

  CLEAR lv_row.

* Percorre as linhas
  DO lv_lines TIMES.

    CLEAR: ls_posicao, lv_column.

    ADD 1 TO lv_row.

* * * Preenche dados de linha
* Percorre os campos da estrutura,
    LOOP AT gt_fields_process INTO ls_fields.

      ADD 1 TO lv_column.

* Assigna campo da estrutura
      ASSIGN COMPONENT ls_fields-name OF STRUCTURE ls_posicao
      TO <lf_campo_posicao>.

      IF <lf_campo_posicao> IS ASSIGNED.

* Lê o campo de transferencia XLS
        READ TABLE pt_xls INTO ls_xls
           WITH KEY
           row = lv_row
           col = lv_column.

        IF sy-subrc = 0.

          PERFORM check_xls_value_field
            USING lv_row
                  lv_column
                  ls_fields-name
                  ls_fields-type_kind
                  ls_fields-decimals
                  ls_fields-length
         CHANGING ls_xls-value.

          IF ls_xls-value = gc_error.
            EXIT. " Do loop deste processamento
          ENDIF.

          <lf_campo_posicao> = ls_xls-value.
          UNASSIGN <lf_campo_posicao>.
        ENDIF.

        IF ls_fields-name EQ 'PERNR__P0000'.
* Validar se o PERNR existe
          gv_pernr = ls_xls-value.
          SELECT SINGLE * INTO p0003
                          FROM pa0003
                          WHERE pernr EQ gv_pernr.
          IF sy-subrc NE 0.
            CONCATENATE gv_pernr 'ainda não existe criado' INTO lv_string
                                                           SEPARATED BY space.
            PERFORM set_error_line
              USING lv_row lv_column ''
                    lv_string.
            ls_xls-value = gc_error.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF NOT ls_xls-value = gc_error.
* Cria o registo
      APPEND ls_posicao TO lt_posicao.
      CLEAR ls_posicao.
    ENDIF.
  ENDDO.

  READ TABLE lt_posicao INDEX 1 TRANSPORTING NO FIELDS.
  CHECK sy-subrc = 0.
  APPEND LINES OF lt_posicao TO gt_posicao.

  CLEAR: lv_lines.
  LOOP AT gt_posicao INTO gs_posicao.
    ADD 1 TO lv_lines.
    PERFORM generate_medidas USING lv_lines.
  ENDLOOP.

ENDFORM.                    " CONV_FILE_POS
* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
*&---------------------------------------------------------------------*
*&      Form  CONV_FILE_AUM
*&---------------------------------------------------------------------*
FORM conv_file_aum
   TABLES pt_xls STRUCTURE alsmex_tabline.

  DATA:
    lt_aumento     LIKE gs_aumento OCCURS 0,
    ls_aumento     LIKE gs_aumento,
    ls_xls         TYPE alsmex_tabline,
    lv_lines       TYPE i,
    lv_row         TYPE i,
    lv_column      TYPE i,
    ls_fields      TYPE abap_compdescr,
    lv_string(255),
    gs_pernr       LIKE pa0000-pernr,
    gs_begda       TYPE begda,
    gt_pa0001      TYPE pa0001,
    gs_pa0001      TYPE pa0001.

  FIELD-SYMBOLS:
                 <lf_campo_aumento> TYPE any.


* Obtem o último registo, para obter o número de linhas
  DESCRIBE TABLE pt_xls LINES lv_lines.
  READ TABLE pt_xls INTO ls_xls
       INDEX lv_lines.
  lv_lines = ls_xls-row.

  CLEAR lv_row.

* Percorre as linhas
  DO lv_lines TIMES.

    CLEAR: ls_aumento, lv_column.

    ADD 1 TO lv_row.


* * * Preenche dados de linha
* Percorre os campos da estrutura,
    LOOP AT gt_fields_process INTO ls_fields.

      ADD 1 TO lv_column.

* Assigna campo da estrutura
      ASSIGN COMPONENT ls_fields-name OF STRUCTURE ls_aumento
      TO <lf_campo_aumento>.

      IF <lf_campo_aumento> IS ASSIGNED.

* Lê o campo de transferencia XLS
        READ TABLE pt_xls INTO ls_xls
           WITH KEY
           row = lv_row
           col = lv_column.

        IF sy-subrc = 0.

          PERFORM check_xls_value_field
            USING lv_row
                  lv_column
                  ls_fields-name
                  ls_fields-type_kind
                  ls_fields-decimals
                  ls_fields-length
         CHANGING ls_xls-value.

          IF ls_xls-value = gc_error.
            EXIT. " Do loop deste processamento
          ENDIF.
* FJHU
* Obtem e guarda o número de colaborador
          IF ls_fields-name+0(5) EQ 'PERNR'.
            gs_pernr = ls_xls-value.
          ENDIF.

* Obtem e guarda a data de atualização e procura a informação do IT 0001
          IF ls_fields-name+0(5) EQ 'BEGDA'.
            gs_begda = ls_xls-value.
*
            SELECT * FROM pa0001 INTO gs_pa0001
                     WHERE pernr EQ gs_pernr
                       AND objps EQ space
                       AND begda LE gs_begda
                       AND endda GE gs_begda.
            ENDSELECT.
          ENDIF.
*
          <lf_campo_aumento> = ls_xls-value.
          UNASSIGN <lf_campo_aumento>.
        ENDIF.
*
* Verifica se se trata de campo do infotipo 0001 e atualiza
* com base na informação existente no sistema (não altera)
        IF ls_fields-name+7(5) EQ 'P0001'.
          CASE ls_fields-name+0(5).
            WHEN 'BTRTL'.
              ls_xls-value = gs_pa0001-btrtl.
            WHEN 'ANSVH'.
              ls_xls-value = gs_pa0001-ansvh.
            WHEN 'VDSK1'.
              ls_xls-value = gs_pa0001-vdsk1.
            WHEN 'ABKRS'.
              ls_xls-value = gs_pa0001-abkrs.
            WHEN 'CDOPE'.
              ls_xls-value = gs_pa0001-cdope.
            WHEN 'KOSTL'.
              ls_xls-value = gs_pa0001-kostl.
            WHEN 'ORGEH'.
              ls_xls-value = gs_pa0001-orgeh.
            WHEN 'PLANS'.
              ls_xls-value = gs_pa0001-plans.
            WHEN 'STELL'.
              ls_xls-value = gs_pa0001-stell.
            WHEN OTHERS.
          ENDCASE.
*
          <lf_campo_aumento> = ls_xls-value.
          UNASSIGN <lf_campo_aumento>.
        ENDIF.

        IF ls_fields-name EQ 'PERNR__P0000'.
* Validar se o PERNR existe
          gv_pernr = ls_xls-value.
          SELECT SINGLE * INTO p0003
                          FROM pa0003
                          WHERE pernr EQ gv_pernr.
          IF sy-subrc NE 0.
            CONCATENATE gv_pernr 'ainda não existe criado' INTO lv_string
                                                           SEPARATED BY space.
            PERFORM set_error_line
              USING lv_row lv_column ''
                    lv_string.
            ls_xls-value = gc_error.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF NOT ls_xls-value = gc_error.
* Cria o registo
      APPEND ls_aumento TO lt_aumento.
      CLEAR ls_aumento.
    ENDIF.
  ENDDO.

  READ TABLE lt_aumento INDEX 1 TRANSPORTING NO FIELDS.
  CHECK sy-subrc = 0.
  APPEND LINES OF lt_aumento TO gt_aumento.

  CLEAR: lv_lines.
  LOOP AT gt_aumento INTO gs_aumento.
    ADD 1 TO lv_lines.
    PERFORM generate_medidas USING lv_lines.
  ENDLOOP.

ENDFORM.                    " CONV_FILE_AUM
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  SET_PARAM_DEM
*&---------------------------------------------------------------------*
FORM set_param_dem CHANGING p_prop_values TYPE ty_t_pprop.

  DATA:
    ls_pprop  TYPE pprop,
    ls_fields TYPE abap_compdescr
    .

  FIELD-SYMBOLS:
                 <lf_campo_demissao> TYPE any
               .

  REFRESH p_prop_values.

  LOOP AT gt_fields_process INTO ls_fields.

* Converte para campos, formato interno
    PERFORM set_field_for_pprop
      USING ls_fields-name
   CHANGING ls_pprop-infty ls_pprop-fname.

* Obtém valor
    ASSIGN COMPONENT ls_fields-name
                  OF STRUCTURE gs_demissoes
                  TO <lf_campo_demissao>.

    ls_pprop-fval = <lf_campo_demissao>.

    APPEND ls_pprop TO p_prop_values.

    IF ls_pprop-infty EQ '0009'.
      ls_pprop-infty = '0009'.
      ls_pprop-fname = 'PSYST-ACTIO'.
      ls_pprop-fval  = 'LIS9'.
      APPEND ls_pprop TO p_prop_values.
    ENDIF.

    IF ls_pprop-infty EQ '0015'.
      ls_pprop-infty = '0015'.
      ls_pprop-fname = 'PSYST-ACTIO'.
      ls_pprop-fval  = 'LIS9'.
      APPEND ls_pprop TO p_prop_values.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " SET_PARAM_DEM
*&---------------------------------------------------------------------*
*&      Form  SET_PARAM_POS
*&---------------------------------------------------------------------*
FORM set_param_pos CHANGING p_prop_values TYPE ty_t_pprop.

  DATA:
    ls_pprop  TYPE pprop,
    ls_fields TYPE abap_compdescr,
    lv_plans  TYPE p0001-plans,
    lv_flag.

  FIELD-SYMBOLS:
                 <lf_campo_posicao> TYPE any
               .

  REFRESH p_prop_values.

*  LOOP AT gt_posicao INTO gs_posicao.

  LOOP AT gt_fields_process INTO ls_fields.

* Converte para campos, formato interno
    PERFORM set_field_for_pprop
      USING ls_fields-name
   CHANGING ls_pprop-infty ls_pprop-fname.

* Obtém valor
    ASSIGN COMPONENT ls_fields-name
                  OF STRUCTURE gs_posicao
                  TO <lf_campo_posicao>.

    ls_pprop-fval = <lf_campo_posicao>.

    APPEND ls_pprop TO p_prop_values.

    IF ls_pprop-infty EQ '0001'.
      IF lv_flag IS INITIAL.
        ls_pprop-infty = '0001'.
        ls_pprop-fname = 'P0001-PLANS'.
        ls_pprop-fval  = lv_plans.
        APPEND ls_pprop TO p_prop_values.

        ls_pprop-infty = '0001'.
        ls_pprop-fname = 'PSYST-PLANS'.
        ls_pprop-fval  = lv_plans.
        APPEND ls_pprop TO p_prop_values.

        lv_flag = 'X'.
      ENDIF.
    ELSEIF ls_pprop-infty EQ '0000'
       AND ls_pprop-fname EQ 'PSPAR-PLANS'.
      lv_plans = ls_pprop-fval.

      ls_pprop-infty = '0000'.
      ls_pprop-fname = 'PSYST-PLANS'.
      ls_pprop-fval  = lv_plans.
      APPEND ls_pprop TO p_prop_values.
    ENDIF.

  ENDLOOP.

*  ENDLOOP.

ENDFORM.                    " SET_PARAM_POS
* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
*&---------------------------------------------------------------------*
*&      Form  SET_PARAM_AUM
*&---------------------------------------------------------------------*
FORM set_param_aum CHANGING p_prop_values TYPE ty_t_pprop.

  DATA: ls_pprop  TYPE pprop,
        ls_fields TYPE abap_compdescr,
        lv_plans  TYPE p0001-plans,
        lv_flag.

  FIELD-SYMBOLS: <lf_campo_aumento> TYPE any.

  REFRESH p_prop_values.

  LOOP AT gt_fields_process INTO ls_fields.

* Converte para campos, formato interno
    PERFORM set_field_for_pprop
      USING ls_fields-name
   CHANGING ls_pprop-infty ls_pprop-fname.

* Obtém valor
    ASSIGN COMPONENT ls_fields-name
                  OF STRUCTURE gs_aumento
                  TO <lf_campo_aumento>.

    ls_pprop-fval = <lf_campo_aumento>.

    APPEND ls_pprop TO p_prop_values.
  ENDLOOP.

ENDFORM.                    " SET_PARAM_AUM
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_FIELDS_NAME  text
*      -->P_LV_ERRO  text
*----------------------------------------------------------------------*
FORM valida_campos USING VALUE(p_field)
* >>> INI Inetum SAM EMP/GPS 7000123834 em 19.08.2021
* Comentado
*                         value(p_value)
                               p_value
* <<< END Inetum SAM EMP/GPS 7000123834 em 19.08.2021
                               p_erro
                         VALUE(p_row)
                         VALUE(p_column)
                         VALUE(p_begda)
                         VALUE(p_famst).
  DATA: lv_flag, lv_column(4),
        lv_string    TYPE text120,
        lt_values    TYPE TABLE OF dd07v,
        ls_values    LIKE LINE OF lt_values,
        ls_t522      TYPE t522,
        ls_t005      TYPE t005,
        ls_t5r05     TYPE t5r05,
        ls_t591a     TYPE t591a,
        ls_t502t     TYPE t502t,
* >>> INI Inetum SAM EMP/GPS 7000123834 em 19.08.2021
        ls_tgsb      TYPE tgsb,
        ls_prps      TYPE prps.
* <<< END Inetum SAM EMP/GPS 7000123834 em 19.08.2021
  CLEAR: p_erro, lv_flag.

  PERFORM converte_coluna USING p_column
                                lv_column.
  CONDENSE lv_column.

  CASE p_field.
    WHEN 'ANRED__P0002'. " forma de tratamento L
      CLEAR: ls_t522.
      SELECT SINGLE * INTO ls_t522
                      FROM t522
                      WHERE anred EQ p_value.
      IF sy-subrc NE 0.
        p_erro = 'X'.
      ENDIF.

    WHEN 'GBLND__P0002'. " país de nascimento Q
      CLEAR: ls_t005.
      SELECT SINGLE * INTO ls_t005
                      FROM t005
                      WHERE land1 EQ p_value.
      IF sy-subrc NE 0.
        p_erro = 'X'.
      ENDIF.

    WHEN 'GESCH__P0002'. " género R
      CLEAR: lt_values[].
      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = 'GESCH'
        TABLES
          values_tab      = lt_values
        EXCEPTIONS
          no_values_found = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
      ELSE.
        LOOP AT lt_values INTO ls_values
                          WHERE domvalue_l EQ p_value.
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
          p_erro = 'X'.
        ENDIF.
      ENDIF.

    WHEN 'ICTYP__P0185'. " tipo de ID AA
      CLEAR: ls_t5r05.
      SELECT SINGLE * INTO ls_t5r05
                      FROM t5r05
                      WHERE molga EQ '19'
                        AND ictyp EQ p_value.
      IF sy-subrc NE 0.
        p_erro = 'X'.
      ENDIF.

    WHEN 'KSTAR__P0027'. " custos a distribuir AY
      CLEAR: ls_t591a.
      SELECT SINGLE * INTO ls_t591a
                      FROM t591a
                      WHERE infty EQ '0027'
                        AND subty EQ p_value.
      IF sy-subrc NE 0.
        p_erro = 'X'.
      ENDIF.

* >>> INI Inetum SAM EMP/GPS 7000123834 em 19.08.2021
    WHEN 'PSP01__P0027'.
      CLEAR: ls_prps.
      SELECT SINGLE * INTO ls_prps
                      FROM prps
                      WHERE posid EQ p_value.
      IF sy-subrc NE 0.
        p_erro = 'X'.
      ELSE.
        p_value = ls_prps-pspnr.
      ENDIF.

    WHEN 'GSBER__P0001'.
      CLEAR: ls_tgsb.
      SELECT SINGLE * INTO ls_tgsb
                      FROM tgsb
                      WHERE gsber EQ p_value.
      IF sy-subrc NE 0.
        p_erro = 'X'.
      ENDIF.
* <<< END Inetum SAM EMP/GPS 7000123834 em 19.08.2021

    WHEN 'WSPSE__P0331'  " cônj. trab. BM
      OR 'HSPSE__P0331'. " cônj. depen. BN
      IF p_value IS INITIAL
      OR p_value EQ 'X'.
      ELSE.
        p_erro = lv_flag = 'X'.

        CONCATENATE 'Conteúdo incorrecto na coluna' lv_column
                    '. Deverá colocar "X" ou a vazio.'
               INTO lv_string
               SEPARATED BY space.
* Qdo há erro no conteúdo do campo
        PERFORM set_error_line
          USING p_row p_column p_field
                lv_string.
      ENDIF.

    WHEN 'DESTA__P2006'. " data inicio dedud. BH
      IF p_value GE p_begda.
      ELSE.
        p_erro = lv_flag = 'X'.

        CONCATENATE 'A "Data inic. Ded.", coluna BH,'
                    'não pode ser inferior à data de admissão'
                    p_begda
                    '. Corrigir entrada.'
               INTO lv_string
               SEPARATED BY space.
* Qdo há erro no conteúdo do campo
        PERFORM set_error_line
          USING p_row p_column p_field
                lv_string.
      ENDIF.

    WHEN 'MSTAT__P0331'. " estado civil BL
      CLEAR: lt_values[].
      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = 'PPT_MSTAT'
        TABLES
          values_tab      = lt_values
        EXCEPTIONS
          no_values_found = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
      ELSE.
        LOOP AT lt_values INTO ls_values
                          WHERE domvalue_l EQ p_value.
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
          p_erro = lv_flag = 'X'.

          CONCATENATE 'Conteúdo incorrecto na coluna' lv_column
                      '. Corrigir entrada.'
                 INTO lv_string
                 SEPARATED BY space.
* Qdo há erro no conteúdo do campo
          PERFORM set_error_line
            USING p_row p_column p_field
                  lv_string.
        ELSE.
          IF ( p_value EQ 'S'
         AND   p_famst NE '0' )
          OR ( p_value EQ 'M'
         AND   p_famst NE '1' ).
            p_erro = lv_flag = 'X'.

            CONCATENATE 'Coluna BL (Chave estado civil) e'
                        'coluna U (Est.civil) não estão em concordância.'
                        'Corrigir entrada.'
                   INTO lv_string
                   SEPARATED BY space.
* Qdo há erro no conteúdo do campo
            PERFORM set_error_line
              USING p_row p_column p_field
                    lv_string.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'FAMST__P0002'. " estado civil U
      CLEAR: ls_t502t.
      SELECT SINGLE * INTO ls_t502t
                      FROM t502t
                      WHERE sprsl EQ sy-langu
                        AND famst EQ p_value.
      IF sy-subrc NE 0.
        p_erro = 'X'.
      ENDIF.
  ENDCASE.

  IF p_erro IS NOT INITIAL
 AND lv_flag IS INITIAL.
    CONCATENATE 'Conteúdo incorrecto na coluna' lv_column
                '. Corrigir entrada.'
           INTO lv_string
           SEPARATED BY space.
* Qdo há erro no conteúdo do campo
    PERFORM set_error_line
      USING p_row p_column p_field
            lv_string.
  ENDIF.

ENDFORM.                    " VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  CONVERTE_COLUNA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_COLUMN  text
*      -->P_LS_ERROR_COLUMN  text
*----------------------------------------------------------------------*
FORM converte_coluna  USING p_column
                            p_error_column.
  DATA: "ls_error   TYPE ty_error_log,
    lv_letters TYPE char26 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
    lv_aux     TYPE i,
    lv_offset1 TYPE i,
    lv_offset2 TYPE i,
    lv_letter1,
    lv_letter2.

  CHECK p_column > 0.

* Coluna, converte em letra
  lv_aux = p_column.

  IF lv_aux > 26.
    DO 99 TIMES. " Just in case
      ADD 1 TO lv_offset1.
      lv_aux = lv_aux - 26.

      IF lv_aux <= 26.
        EXIT.
      ENDIF.
    ENDDO.

    lv_offset1 = lv_offset1 - 1.
    lv_offset2 = lv_aux - 1.

    lv_letter1 = lv_letters+lv_offset1(1).
    lv_letter2 = lv_letters+lv_offset2(1).

    CONCATENATE lv_letter1 lv_letter2 INTO p_error_column.

  ELSE.
    lv_offset1     = lv_aux - 1.
    p_error_column = lv_letters+lv_offset1(1).
  ENDIF.

ENDFORM.                    " CONVERTE_COLUNA
*&---------------------------------------------------------------------*
*&      Form  SET_ADM_IBAN
*&---------------------------------------------------------------------*
FORM set_adm_iban
        USING p_tabix       TYPE sy-tabix
     CHANGING p_prop_values TYPE ty_t_pprop.

  DATA:
    ls_pprop        TYPE pprop,
    lv_fname1       TYPE string,
    lv_fname2       TYPE char5,
    ls_bankdata     TYPE hrpad_bankdata,
    lv_last_tabix   TYPE sy-tabix,
* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
    lv_first_tabix  TYPE sy-tabix,
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
    lv_mstring(120).
  .

  FIELD-SYMBOLS:
    <lf_dest> TYPE any.

* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
  CLEAR: lv_first_tabix.
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021

* Obtém valores infotipo 0009
  LOOP AT p_prop_values INTO ls_pprop
    WHERE infty = '0009'
* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
      AND fname(7) NE 'P0009_2'.

    IF lv_first_tabix IS INITIAL.
      lv_first_tabix = sy-tabix.
    ENDIF.
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021

    lv_last_tabix = sy-tabix.

* Campo
    CLEAR: lv_fname1, lv_fname2.
    SPLIT ls_pprop AT '-' INTO lv_fname1 lv_fname2.
    CHECK lv_fname2 IS NOT INITIAL.

* Assign
    ASSIGN COMPONENT lv_fname2 OF STRUCTURE ls_bankdata TO <lf_dest>.
    CHECK <lf_dest> IS ASSIGNED.

    <lf_dest> = ls_pprop-fval.
    UNASSIGN <lf_dest>.
  ENDLOOP.
* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
  IF sy-subrc EQ 0.
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
* Teremos de ir buscar a chave de país do banco, validamos o registo
    IF ls_bankdata-bankl IS NOT INITIAL.

      SELECT SINGLE banks INTO ls_bankdata-banks FROM bnka
       WHERE bankl = ls_bankdata-bankl.

      IF sy-subrc IS NOT INITIAL OR ls_bankdata-banks IS INITIAL.
        " Erro
        CONCATENATE 'IBAN: ERRO Chave Banco' ls_bankdata-bankl
        INTO lv_mstring SEPARATED BY space.
        PERFORM set_error_line
          USING p_tabix 0 '' lv_mstring.
      ENDIF.

    ELSE.
      " Erro
      lv_mstring = 'IBAN: ERRO Chave Banco não encontrada'.
      PERFORM set_error_line
        USING p_tabix 0 '' lv_mstring.
      EXIT.
    ENDIF.

    CALL METHOD cl_hrpad00_iban=>generate_iban_sepa
      CHANGING
        bank_data                     = ls_bankdata
      EXCEPTIONS
        conv_bank_data_to_iban_failed = 1
        country_not_available         = 2
        OTHERS                        = 3.
    IF sy-subrc <> 0.
      " Erro
      lv_mstring = 'IBAN: ERRO geração'.
      PERFORM set_error_line
        USING p_tabix 0 '' lv_mstring.
      EXIT.
    ENDIF.

* gera entrada
    CLEAR: ls_pprop.
    ls_pprop-infty = '0009'.
    ls_pprop-fname = 'P0009-IBAN'.
    ls_pprop-fval  = ls_bankdata-iban.

    ADD 1 TO lv_last_tabix.
    INSERT ls_pprop INTO p_prop_values INDEX lv_last_tabix.
* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
  ENDIF.

* Obtém valores infotipo 0009 - Cartão de refeição
  FIELD-SYMBOLS: <fs_pprop> TYPE LINE OF ty_t_pprop.

  REFRESH: gt_prop_values.

  CLEAR: ls_pprop.
  READ TABLE p_prop_values INTO ls_pprop
                           WITH KEY fname = 'P0009_2-BANKL'.
  IF ls_pprop-fval IS NOT INITIAL.
    CLEAR: ls_bankdata.
    LOOP AT p_prop_values ASSIGNING <fs_pprop>
      WHERE infty    EQ '0009'
        AND fname(7) EQ 'P0009_2'.

      lv_last_tabix = sy-tabix.

* Campo
      CLEAR: lv_fname1, lv_fname2.
      SPLIT <fs_pprop> AT '-' INTO lv_fname1 lv_fname2.

*    REPLACE 'P0009_2' WITH 'P0009' INTO <fs_pprop>-fname.
      MOVE-CORRESPONDING <fs_pprop> TO ls_pprop.
      APPEND ls_pprop TO gt_prop_values.

      CHECK lv_fname2 IS NOT INITIAL.
* Assign
      ASSIGN COMPONENT lv_fname2 OF STRUCTURE ls_bankdata TO <lf_dest>.
      CHECK <lf_dest> IS ASSIGNED.

      <lf_dest> = <fs_pprop>-fval.
      UNASSIGN <lf_dest>.
    ENDLOOP.
    IF sy-subrc EQ 0.
* Teremos de ir buscar a chave de país do banco, validamos o registo
      IF ls_bankdata-bankl IS NOT INITIAL.

        SELECT SINGLE banks INTO ls_bankdata-banks FROM bnka
         WHERE bankl = ls_bankdata-bankl.

        IF sy-subrc IS NOT INITIAL OR ls_bankdata-banks IS INITIAL.
          " Erro
          CONCATENATE 'IBAN: ERRO Chave Banco' ls_bankdata-bankl
          INTO lv_mstring SEPARATED BY space.
          PERFORM set_error_line
            USING p_tabix 0 '' lv_mstring.
        ENDIF.

      ELSE.
        " Erro
        lv_mstring = 'IBAN: ERRO Chave Banco não encontrada'.
        PERFORM set_error_line
          USING p_tabix 0 '' lv_mstring.
        EXIT.
      ENDIF.

      CALL METHOD cl_hrpad00_iban=>generate_iban_sepa
        CHANGING
          bank_data                     = ls_bankdata
        EXCEPTIONS
          conv_bank_data_to_iban_failed = 1
          country_not_available         = 2
          OTHERS                        = 3.
      IF sy-subrc <> 0.
        " Erro
        lv_mstring = 'IBAN: ERRO geração'.
        PERFORM set_error_line
          USING p_tabix 0 '' lv_mstring.
        EXIT.
      ENDIF.

* gera entrada
      CLEAR: ls_pprop.
      ls_pprop-infty = '0009'.
      ls_pprop-fname = 'P0009_2-IBAN'.
      ls_pprop-fval  = ls_bankdata-iban.

      ADD 1 TO lv_last_tabix.
      INSERT ls_pprop INTO p_prop_values INDEX lv_last_tabix.

      MOVE-CORRESPONDING ls_pprop TO ls_pprop.
      APPEND ls_pprop TO gt_prop_values.
    ENDIF.
  ENDIF.

  DELETE p_prop_values WHERE fname(7) EQ 'P0009_2'.
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
ENDFORM.                    " SET_ADM_IBAN
* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
*&---------------------------------------------------------------------*
*&      Form  CARTAO_REFEICAO_IT0009
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cartao_refeicao_it0009.

  DATA: ls_prop_values LIKE LINE OF gt_prop_values,
        ls_p0009       TYPE p0009,
        ls_return      LIKE bapireturn1,
        lv_key         LIKE bapipakey.

  CLEAR: ls_p0009.
  ls_p0009-pernr = gv_pernr.
  ls_p0009-begda = gv_begda.
  ls_p0009-endda = '99991231'.
  LOOP AT gt_prop_values INTO ls_prop_values.
    ls_p0009-infty = ls_prop_values-infty.
    IF     ls_prop_values-fname EQ 'P0009_2-BNKSA'.
      ls_p0009-subty = ls_prop_values-fval.
      ls_p0009-bnksa = ls_prop_values-fval.
    ELSEIF ls_prop_values-fname EQ 'P0009_2-BANKL'.
      ls_p0009-bankl = ls_prop_values-fval.
    ELSEIF ls_prop_values-fname EQ 'P0009_2-BANKN'.
      ls_p0009-bankn = ls_prop_values-fval.
    ELSEIF ls_prop_values-fname EQ 'P0009_2-BKONT'.
      ls_p0009-bkont = ls_prop_values-fval.
    ELSEIF ls_prop_values-fname EQ 'P0009_2-ZLSCH'.
      ls_p0009-zlsch = ls_prop_values-fval.
    ELSEIF ls_prop_values-fname EQ 'P0009_2-IBAN'.
      ls_p0009-iban = ls_prop_values-fval.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = ls_p0009-pernr
    IMPORTING
      return = ls_return.

  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      infty         = ls_p0009-infty
      number        = ls_p0009-pernr
      subtype       = ls_p0009-subty
      validityend   = ls_p0009-endda
      validitybegin = ls_p0009-begda
      record        = ls_p0009
      operation     = 'INS'
    IMPORTING
      return        = ls_return
      key           = lv_key.

  CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
    EXPORTING
      number = ls_p0009-pernr.

ENDFORM.                    " CARTAO_REFEIÇÃO_IT0009
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
