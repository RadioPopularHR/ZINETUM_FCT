*&---------------------------------------------------------------------*
*& Report  ZHR_CARREGAMENTO_MEDIDAS
*&
*&---------------------------------------------------------------------*
*& Autor  : Miguel Abrantes (ROFF SAM)
*& Data   : 13.10.2014
*& Desc.  : Carregamento de medidas de Admissão, Demissão e mudança
*&          posição a partir de um ficheiro XLS.
*&---------------------------------------------------------------------*
REPORT  zhr_carregamento_medidas LINE-SIZE 150.
**********************************************************************
***                    DECLARAÇÃO DE DADOS                         ***
**********************************************************************
INCLUDE zhr_carregamento_medidas_top.
**********************************************************************
***                    ECRÃ DE SELECÇÃO                            ***
**********************************************************************
INCLUDE zhr_carregamento_medidas_ssc.
**********************************************************************
***                    ROTINAS                                     ***
**********************************************************************
INCLUDE zhr_carregamento_medidas_frm.
**********************************************************************
***                    EVENTOS                                     ***
**********************************************************************
INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM at_selection_screen_output.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_get_file CHANGING p_file.

START-OF-SELECTION.
  PERFORM set_process_fields.

  PERFORM upload_xls.

END-OF-SELECTION.

  PERFORM output.
