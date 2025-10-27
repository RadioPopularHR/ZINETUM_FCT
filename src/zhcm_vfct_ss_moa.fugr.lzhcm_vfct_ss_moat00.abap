*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHCM_VFCT_SS_MOA................................*
TABLES: ZHCM_VFCT_SS_MOA, *ZHCM_VFCT_SS_MOA. "view work areas
CONTROLS: TCTRL_ZHCM_VFCT_SS_MOA
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHCM_VFCT_SS_MOA. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHCM_VFCT_SS_MOA.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHCM_VFCT_SS_MOA_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHCM_VFCT_SS_MOA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHCM_VFCT_SS_MOA_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHCM_VFCT_SS_MOA_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHCM_VFCT_SS_MOA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHCM_VFCT_SS_MOA_TOTAL.

*.........table declarations:.................................*
TABLES: ZHCM_TFCT_MOTIV                .
TABLES: ZHCM_TFCT_MOTIVT               .
TABLES: ZHCM_TFCT_MOTVS                .
TABLES: ZHCM_TFCT_MOTVST               .
TABLES: ZHCM_TFCT_SS_MOA               .
