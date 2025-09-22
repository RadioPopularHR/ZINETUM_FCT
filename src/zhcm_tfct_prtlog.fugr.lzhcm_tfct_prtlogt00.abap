*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHCM_TFCT_PRTLOG................................*
DATA:  BEGIN OF STATUS_ZHCM_TFCT_PRTLOG              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHCM_TFCT_PRTLOG              .
CONTROLS: TCTRL_ZHCM_TFCT_PRTLOG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHCM_TFCT_PRTLOG              .
TABLES: ZHCM_TFCT_PRTLOG               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
