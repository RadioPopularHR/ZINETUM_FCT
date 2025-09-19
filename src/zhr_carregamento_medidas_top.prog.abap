*&---------------------------------------------------------------------*
*&  Include           ZHR_CARREGAMENTO_MEDIDAS_TOP
*&---------------------------------------------------------------------*

TABLES: sscrfields, t100.
TYPE-POOLS: icon, abap.

INFOTYPES: 0000, 0003.

* Definição tipos
" Suporte para carregamento de dados
TYPES:
      " PERNR
      BEGIN OF ty_pernr,
        pernr LIKE p0000-pernr,
        begda LIKE p0000-begda,
      END OF ty_pernr,
      " 0000 - Medidas
      BEGIN OF ty_p0000,
        massn LIKE p0000-massn,
        massg LIKE p0000-massg,
      END OF ty_p0000,
      " PSPAR - Barra de transferência p/módulos de diálogo HR infotipo
      BEGIN OF ty_pspar,
        plans LIKE pspar-plans,
        werks LIKE pspar-werks,
        persg LIKE pspar-persg,
        persk LIKE pspar-persk,
      END OF ty_pspar,
      BEGIN OF ty_pspard,
        werks LIKE pspar-werks,
        persg LIKE pspar-persg,
        persk LIKE pspar-persk,
      END OF ty_pspard,
      " 0001
      BEGIN OF ty_p0001,
*        bukrs LIKE p0001-bukrs,     " FJHU
*        werks LIKE p0001-werks,     " FJHU
        btrtl LIKE p0001-btrtl,
        ansvh LIKE p0001-ansvh,
        vdsk1 LIKE p0001-vdsk1,
        ABKRS like p0001-abkrs,   " Payroll Area
        CDOPE LIKE p0001-CDOPE,
      END OF ty_p0001,

      BEGIN OF ty_p0001_II,
*        bukrs LIKE p0001-bukrs,     " FJHU
*        werks LIKE p0001-werks,     " FJHU
        btrtl LIKE p0001-btrtl,
        ansvh LIKE p0001-ansvh,
        vdsk1 LIKE p0001-vdsk1,
        ABKRS like p0001-abkrs,   " Payroll Area
        CDOPE LIKE p0001-CDOPE,
        kostl like p0001-kostl,
        orgeh like p0001-orgeh,
        plans like p0001-plans,
        stell like p0001-stell,
      END OF ty_p0001_II,

      " 0002
      BEGIN OF ty_p0002,
        anred LIKE p0002-anred,
        cname LIKE p0002-cname,
        vorna LIKE p0002-vorna,
        nachn LIKE p0002-nachn,
        gbdat LIKE p0002-gbdat,
        gblnd LIKE p0002-gblnd,
        gesch LIKE p0002-gesch,
        sprsl LIKE p0002-sprsl,
        natio LIKE p0002-natio,
        famst LIKE p0002-famst,
      END OF ty_p0002,
      " 0006
      BEGIN OF ty_p0006,
        begda LIKE p0006-begda,     " FJHU
        name2 LIKE p0006-name2,
        stras LIKE p0006-stras,
        locat LIKE p0006-locat,
        pstlz LIKE p0006-pstlz,
        telnr LIKE p0006-telnr,
      END OF ty_p0006,
      " 0185
      BEGIN OF ty_p0185,
        ictyp LIKE p0185-ictyp,
        icnum LIKE p0185-icnum,
        expid LIKE p0185-expid,
      END OF ty_p0185,
      " 0007
      BEGIN OF ty_p0007,
        schkz LIKE p0007-schkz,
        kztim LIKE p0007-kztim,
      END OF ty_p0007,
      " 0008
      BEGIN OF ty_p0008,
        trfgr LIKE p0008-trfgr,
        trfst LIKE p0008-trfst,
        lga01 LIKE p0008-lga01,
        bet01 LIKE p0008-bet01,
        anz01 like p0008-anz01,
        lga02 LIKE p0008-lga02,
        bet02 LIKE p0008-bet02,
        anz02 like p0008-anz02,
        lga03 LIKE p0008-lga03,
        bet03 LIKE p0008-bet03,
        anz03 like p0008-anz03,
        lga04 LIKE p0008-lga04,
        bet04 LIKE p0008-bet04,
        anz04 like p0008-anz04,
        lga05 LIKE p0008-lga05,
        bet05 LIKE p0008-bet05,
        anz05 like p0008-anz05,
        lga06 LIKE p0008-lga06,
        bet06 LIKE p0008-bet06,
        anz06 like p0008-anz06,
      END OF ty_p0008,
       " 0337
      BEGIN OF ty_p0337,
        prcat LIKE p0337-prcat,
        prcod LIKE p0337-prcod,
        prcnp LIKE p0337-prcnp,
        prpar LIKE p0337-prpar,
        prgrp LIKE p0337-prgrp,
        prfun LIKE p0337-prfun,
        prsit LIKE p0337-prsit,
        ausgr LIKE p0337-ausgr,
        ctsit LIKE p0337-ctsit,
      END OF ty_p0337,
      " 0009
      BEGIN OF ty_p0009,
        bankl LIKE p0009-bankl,
        bankn LIKE p0009-bankn,
        bkont LIKE p0009-bkont,
        zlsch LIKE p0009-zlsch,
      END OF ty_p0009,
      BEGIN OF ty_p0009d,
        begda LIKE p0000-begda,
        bnksa LIKE p0009-bnksa,
        zlsch LIKE p0009-zlsch,
      END OF ty_p0009d,
      " 0027
      BEGIN OF ty_p0027,
        kstar LIKE p0027-kstar,
        kbu01 LIKE p0027-kbu01,
        kst01 LIKE p0027-kst01,
        kpr01 LIKE p0027-kpr01,
* >>> INI Inetum SAM EMP/GPS 7000123834 em 19.08.2021
        psp01 TYPE ps_posid,
* <<< END Inetum SAM EMP/GPS 7000123834 em 19.08.2021
      END OF ty_p0027,
      " 0016
      BEGIN OF ty_p0016,
        cttyp LIKE p0016-cttyp,
        eindt LIKE p0016-eindt,
        kondt LIKE p0016-kondt,
      END OF ty_p0016,
      " 2006
      BEGIN OF ty_p2006,
        ktart LIKE p2006-ktart,
        anzhl LIKE p2006-anzhl,
        desta LIKE p2006-desta,
        deend LIKE p2006-deend,
      END OF ty_p2006,
      " 0331
      BEGIN OF ty_p0331,
        finum LIKE p0331-finum,
        incat LIKE p0331-incat,
        mstat LIKE p0331-mstat,
        wspse LIKE p0331-wspse,
        hspse LIKE p0331-hspse,
        nrdep LIKE p0331-nrdep,
        ndeph LIKE p0331-ndeph,
      END OF ty_p0331,
      " 0332
      BEGIN OF ty_p0332,
        ssrgm LIKE p0332-ssrgm,
        cttyp LIKE p0332-cttyp,
        rgcod LIKE p0332-rgcod,
        ssnum LIKE p0332-ssnum,
        ssins LIKE p0332-ssins,
        sname LIKE p0332-sname,
        ctyru LIKE p0332-ctyru, "ROFF SAM-FB/SS-7000038325-05.06.2017
      END OF ty_p0332,
      " 0037
      BEGIN OF ty_p0037,
        vsart LIKE p0037-vsart,
        vsges LIKE p0037-vsges,
        vsnum LIKE p0037-vsnum,
      END OF ty_p0037,
      " 0015
      BEGIN OF ty_p0015,
        lgart LIKE p0015-lgart,
        betrg LIKE p0015-betrg,
        waers LIKE p0015-waers,
        anzhl LIKE p0015-anzhl,
        zeinh LIKE p0015-zeinh,
      END OF ty_p0015,
      " 0105
      BEGIN OF ty_p0105,
        usrid_long LIKE p0105-usrid_long,
*        usrid LIKE p0105-usrid_long,
      END OF ty_p0105,
* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
      " 0009 - Cartão refeição
      BEGIN OF ty_p0009_2,
        bnksa LIKE p0009-bnksa,
        bankl LIKE p0009-bankl,
        bankn LIKE p0009-bankn,
        bkont LIKE p0009-bkont,
        zlsch LIKE p0009-zlsch,
      END OF ty_p0009_2,
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
* IT 0015
       BEGIN OF ty_p0014,
        LGART LIKE p0014-LGART,
        BEGDA LIKE p0014-BEGDA,
        BETRG LIKE p0014-BETRG,
      END OF ty_p0014,
* IT0015
* IT 0015
       BEGIN OF ty_p0015_,
        LGART LIKE p0015-LGART,
        BEGDA LIKE p0015-BEGDA,
        BETRG LIKE p0015-BETRG,
      END OF ty_p0015_,
* IT0015


** --------------------------------------------------
** INI ROFF SAM HR SG/EMP 7000009687 em 10.03.2015
*      " 0334
*      BEGIN OF ty_p0334,
*        vagr1 LIKE p0334-vagr1,
*        regty LIKE p0334-regty,
*      END OF ty_p0334,
** END ROFF SAM HR SG/EMP 7000009687 em 10.03.2015
** --------------------------------------------------
      " Apenas delimitação
      BEGIN OF ty_endda,
        endda LIKE p0000-endda,
      END OF ty_endda
     .

TYPES: ty_t_pprop TYPE TABLE OF pprop.

TYPES:
      BEGIN OF ty_error_log,
* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
        pernr  TYPE p0000-pernr,
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------
        line   TYPE char04,
        column TYPE char04,
        field  TYPE prop_fname,
        error  TYPE text120,
      END OF ty_error_log
     .

* Suporte para Admissões
DATA:
       BEGIN OF gs_admissoes.
        INCLUDE TYPE ty_pernr AS pernr RENAMING WITH SUFFIX __p0000.
        INCLUDE TYPE ty_p0000 AS p0000 RENAMING WITH SUFFIX __p0000.
        INCLUDE TYPE ty_pspar AS pspar RENAMING WITH SUFFIX __pspar__p0000.
        INCLUDE TYPE ty_p0001 AS p0001 RENAMING WITH SUFFIX __p0001.
        INCLUDE TYPE ty_p0002 AS p0002 RENAMING WITH SUFFIX __p0002.
        INCLUDE TYPE ty_p0006 AS p0006 RENAMING WITH SUFFIX __p0006.
        INCLUDE TYPE ty_p0007 AS p0007 RENAMING WITH SUFFIX __p0007.
        INCLUDE TYPE ty_p0008 AS p0008 RENAMING WITH SUFFIX __p0008.
        INCLUDE TYPE ty_p0009 AS p0009 RENAMING WITH SUFFIX __p0009.
        INCLUDE TYPE ty_p0016 AS p0016 RENAMING WITH SUFFIX __p0016.
        INCLUDE TYPE ty_p0185 AS p0185 RENAMING WITH SUFFIX __p0185.
        INCLUDE TYPE ty_p0105 AS p0105 RENAMING WITH SUFFIX __p0105.
* 22
        INCLUDE TYPE ty_p0331 AS p0331 RENAMING WITH SUFFIX __p0331.
        INCLUDE TYPE ty_p0332 AS p0332 RENAMING WITH SUFFIX __p0332.
        INCLUDE TYPE ty_p0337 AS p0337 RENAMING WITH SUFFIX __p0337.
        INCLUDE TYPE ty_p0037 AS p0037 RENAMING WITH SUFFIX __p0037.
* 34
        INCLUDE TYPE ty_p2006 AS p2006 RENAMING WITH SUFFIX __p2006.
*        INCLUDE TYPE ty_p0027 AS p0027 RENAMING WITH SUFFIX __p0027.


* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
        INCLUDE TYPE ty_p0009_2 AS p0009_tab RENAMING WITH SUFFIX __p0009_2.
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021

** --------------------------------------------------
** INI ROFF SAM HR SG/EMP 7000009687 em 10.03.2015
*        INCLUDE TYPE ty_p0334 AS p0334 RENAMING WITH SUFFIX __p0334.
** END ROFF SAM HR SG/EMP 7000009687 em 10.03.2015
** --------------------------------------------------

DATA:  END OF gs_admissoes,
       gt_admissoes LIKE gs_admissoes OCCURS 0.

* Suporte para Demissões
DATA:
       BEGIN OF gs_demissoes.
        INCLUDE TYPE ty_pernr  AS pernr  RENAMING WITH SUFFIX __p0000.
        INCLUDE TYPE ty_p0000  AS p0000  RENAMING WITH SUFFIX __p0000.
        INCLUDE TYPE ty_pspard AS pspar  RENAMING WITH SUFFIX __pspar__p0000.
        INCLUDE TYPE ty_p0001  AS p0001  RENAMING WITH SUFFIX __p0001.
*        INCLUDE TYPE ty_p0009d AS p0009  RENAMING WITH SUFFIX __p0009.
DATA:  END OF gs_demissoes,
       gt_demissoes LIKE gs_demissoes OCCURS 0.

* Suporte para Posição / NOva Medida inicial
DATA:
       BEGIN OF gs_posicao. " Alteração de contrato
        INCLUDE TYPE ty_pernr AS pernr RENAMING WITH SUFFIX __p0000.
        INCLUDE TYPE ty_p0000 AS p0000 RENAMING WITH SUFFIX __p0000.
*        INCLUDE TYPE ty_pspar AS pspar RENAMING WITH SUFFIX __pspar__p0000.
*        INCLUDE TYPE ty_p0001 AS p0001 RENAMING WITH SUFFIX __p0001.
*        INCLUDE TYPE ty_p0027 AS p0027 RENAMING WITH SUFFIX __p0007.
*        INCLUDE TYPE ty_p0008 AS p0008 RENAMING WITH SUFFIX __p0008.
DATA:  END OF gs_posicao,
       gt_posicao LIKE gs_posicao OCCURS 0.

* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* Suporte para Aumento Salarial
DATA:
       BEGIN OF gs_aumento.  " Promoção
        INCLUDE TYPE ty_pernr AS pernr RENAMING WITH SUFFIX __p0000.
        INCLUDE TYPE ty_p0000 AS p0000 RENAMING WITH SUFFIX __p0000.
*        INCLUDE TYPE ty_p0001 AS p0001 RENAMING WITH SUFFIX __p0001.
        INCLUDE TYPE ty_p0001_ii AS p0001 RENAMING WITH SUFFIX __p0001.
*        INCLUDE TYPE ty_pspar AS pspar RENAMING WITH SUFFIX __pspar__p0000.
*        INCLUDE TYPE ty_p0007 AS p0007 RENAMING WITH SUFFIX __p0007.
        INCLUDE TYPE ty_p0008 AS p0008 RENAMING WITH SUFFIX __p0008.
*        INCLUDE TYPE ty_p0027 AS p0027 RENAMING WITH SUFFIX __p0027.
DATA:  END OF gs_aumento,
       gt_aumento LIKE gs_aumento OCCURS 0.
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------

* Declaração dados globais
DATA:
    " Identificação do tipo de processamento
     gv_selected_process TYPE char04,
    " Mapeamento de campos
     gt_fields_process   TYPE abap_compdescr_tab,
    " Log Erros
     gt_error_log        TYPE TABLE OF ty_error_log.

* -------------------------------------------------
* INI ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
DATA: gv_pernr TYPE p0003-pernr.
* END ROFF SAM HR SG/EMP 7000009687 em 03.02.2015
* -------------------------------------------------

* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
DATA: gv_begda TYPE begda.
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021


* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
DATA: gt_prop_values TYPE ty_t_pprop.
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021


* Constantes
CONSTANTS:
  " Comando botão: Carregamento medida admissão
           gc_comm_adm          TYPE char04
                   VALUE 'UADM',
  " Comando botão: Carregamento medida demissão
           gc_comm_dem          TYPE char04
                   VALUE 'UDEM',
  " Comando botão: Carregamento medida alteração posição
           gc_comm_pos          TYPE char04
                   VALUE 'UPRO',
* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
  " Comando botão: Carregamento medida aumento salarial
           gc_comm_aum          TYPE char04
                   VALUE 'UAUM',
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------
  " Separador de campos
           gc_field_split(2)    TYPE c
                   VALUE '__',
  " Identificador de Repetição
           gc_repc(10)         TYPE c
                   VALUE 'REPC',
  " Identificador de Erro
           gc_error(10)         TYPE c
                   VALUE '>#ERROR#<-',
  " Número de Linhas por defeito a carregar no Excel
           gc_xls_lines_default TYPE i
                   VALUE 200,
  " Número de Linhas no Excel a ignorar, cabeçalho
           gc_xls_header_lines  TYPE i
                   VALUE 2,
  " Número de Colunas no Excel de medidas de admissão
* >>> INI Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021
* Comentado
*           gc_xls_adm_cols      TYPE i
***** >>> INI Inetum SAM EMP/GPS 7000123834 em 19.08.2021
***** Comentado
****** --------------------------------------------------
****** INI ROFF SAM HR SG/EMP 7000009687 em 10.03.2015
******INI: ROFF SAM - FB/SS - 7000038325 - 05.06.2017
******                   VALUE 78,
*****                   VALUE 79,
******FIM: ROFF SAM - FB/SS - 7000038325 - 05.06.2017
****** END ROFF SAM HR SG/EMP 7000009687 em 10.03.2015
****** --------------------------------------------------
****                   VALUE 81,
***** <<< END Inetum SAM EMP/GPS 7000123834 em 19.08.2021

           gc_xls_adm_cols TYPE i VALUE 87,
* <<< END Inetum SAM EMP/GPS HR 7000131085 em 18.01.2021

  " Número de Colunas no Excel de medidas de demissão
           gc_xls_dem_cols      TYPE i
                   VALUE 12,
  " Número de Colunas no Excel de medidas de mudança de posição
           gc_xls_pos_cols      TYPE i
                   VALUE 21, "19.
* -------------------------------------------------
* INI ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
  " Número de Colunas no Excel de medidas de mudança de posição
           gc_xls_aum_cols      TYPE i
                   VALUE 27.
* END ROFF SAM HR SS/EMP 7000030725 em 03.11.2016
* -------------------------------------------------
