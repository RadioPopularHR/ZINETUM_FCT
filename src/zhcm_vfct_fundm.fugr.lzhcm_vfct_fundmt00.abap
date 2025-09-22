*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHCM_VFCT_FUNDM.................................*
TABLES: ZHCM_VFCT_FUNDM, *ZHCM_VFCT_FUNDM. "view work areas
CONTROLS: TCTRL_ZHCM_VFCT_FUNDM
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHCM_VFCT_FUNDM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHCM_VFCT_FUNDM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHCM_VFCT_FUNDM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHCM_VFCT_FUNDM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHCM_VFCT_FUNDM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHCM_VFCT_FUNDM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHCM_VFCT_FUNDM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHCM_VFCT_FUNDM_TOTAL.

*.........table declarations:.................................*
TABLES: ZHCM_TFCT_FUNDM                .
TABLES: ZHCM_TFCT_FUNDMT               .
