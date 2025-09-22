*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHCM_VFCT_SS_CNT................................*
TABLES: ZHCM_VFCT_SS_CNT, *ZHCM_VFCT_SS_CNT. "view work areas
CONTROLS: TCTRL_ZHCM_VFCT_SS_CNT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHCM_VFCT_SS_CNT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHCM_VFCT_SS_CNT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHCM_VFCT_SS_CNT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHCM_VFCT_SS_CNT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHCM_VFCT_SS_CNT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHCM_VFCT_SS_CNT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHCM_VFCT_SS_CNT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHCM_VFCT_SS_CNT_TOTAL.

*.........table declarations:.................................*
TABLES: ZHCM_TFCT_MODA                 .
TABLES: ZHCM_TFCT_MODAT                .
TABLES: ZHCM_TFCT_SS_CNT               .
