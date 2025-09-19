*&---------------------------------------------------------------------*
*& Include          ZHCM_FCT_COCKPIT_TOP
*&---------------------------------------------------------------------*
INFOTYPES: 0001, 0002, 0008, 0000, 0016, 0332, 0337, 0007, 0105, 0185, 0302, 0052 ##NUMBER_OK.
TABLES: pernr, pa0000, zhcm_vfct_lgart.

TYPES: ty_rg_mod_id TYPE RANGE OF char3.

TYPES: BEGIN OF ty_it_metadata,
         begda TYPE begda,
         infty TYPE infty,
         acao  TYPE zhcm_fct_acao,
       END OF ty_it_metadata,
       ty_it_metadata_tt TYPE STANDARD TABLE OF ty_it_metadata.
